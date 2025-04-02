# tfl_scripts/lineplot.R
generate_tfl <- function(data, popdata, params) {
  # 参数验证
  required_params <- c("analysis_pop", "atoxgr", "atoxgrn", "treatment_group", "treatment_groupN", "aedecod")
  missing_params <- setdiff(required_params, names(params))
  if (length(missing_params) > 0) {
    stop(paste("缺少必要参数:", paste(missing_params, collapse = ", ")))
  }
  
  bigNdata <- popdata %>% 
    mutate(trtc = TRT01A,
           trtn = TRT01AN
    )
  
  # 数据筛选
  filtered_data <- data
  if (!is.null(params$filter_condition) && params$filter_condition != "") {
    filtered_data <- filtered_data %>% 
      filter(eval(parse(text = params$filter_condition)))
  }
  if (!is.null(params$analysis_pop) && params$analysis_pop != "") {
    filtered_data <- filtered_data %>% 
      filter(eval(parse(text = paste(params$analysis_pop,"%in% c('是','Y')") )))
    bigNdata <- bigNdata %>% 
      filter(eval(parse(text = paste(params$analysis_pop,"%in% c('是','Y')") )))
  }
  
  

  aedata <- filtered_data %>% 
    mutate(trtc = filtered_data[[params$treatment_group]],
           trtn = filtered_data[[params$treatment_groupN]],
           aegrade = filtered_data[[params$atoxgr]],
           aegraden = filtered_data[[params$atoxgrn]],
           aedecod = filtered_data[[params$aedecod]]) %>% 
    mutate(trtc = forcats::fct_reorder(trtc, trtn, min),
           aegrade = forcats::fct_reorder(aegrade, aegraden, min))
  
  aedata <- rbind(aedata,
                  mutate(aedata, trtn = 9))
  
  # graphfont <- 'KT'
  Xlabel <- c("")
  Ylabel <- c("不良事件发生率（%）")
  color = c("red", "blue", "grey")
  legendpos <- c(0.1,0.95)
  Annotpos <- 75
  


  bign <- bigNdata %>% 
    group_by(trtn) %>%
    summarise(bign=n() , .groups ="keep")

  ana1 <- aedata %>% 
    group_by(trtn, trtc, USUBJID, aedecod) %>%
    arrange(trtn, trtc, USUBJID, aedecod, aegraden, aegrade) %>%
    slice(n()) %>% #每个受试者每个PT取最严重的一条
    ungroup() %>%
    group_by(trtn, trtc, aedecod, aegraden, aegrade) %>%
    summarise(freq=n(),.groups = "keep")

  ord <- ana1 %>%
    filter(trtn == 9) %>%
    group_by(aedecod) %>%
    mutate(freqn = sum(freq)) %>%
    slice(n()) %>%
    arrange(desc(freqn)) %>%
    ungroup() %>%
    mutate(ord=row_number()) %>%
    select(aedecod ,ord ,freqn)

  ana2 = merge(ana1, ord, by = c("aedecod"))

  dummy = data.frame()
  for (i in seq(1, nlevels(aedata$trtc), by=1 )) {
    for (j in c(1,2,3) ) {
      dummy1 = ord %>%
        select(ord,aedecod) %>%
        mutate(trtn = i , aegraden = j)
      dummy = rbind(dummy ,dummy1)
    }
  }

  fina = right_join(ana2 ,dummy ,by = c("trtn" ,"aedecod" ,"ord" ,"aegraden")) %>%
    mutate(freq = if_else(is.na(freq),0,freq)) %>%
    select(-freqn) %>%
    left_join(bign, by = c("trtn")) %>%
    mutate(pct = 100*freq/bign) %>%
    mutate(order = ord + (trtn - 1)*(max(ord)+1) )%>%
    arrange(aedecod, trtn,aegrade)

  ordermiss = fina %>%
    group_by(trtn) %>%
    filter(trtn != 1) %>%
    arrange(order) %>%
    slice(1) %>%
    mutate(aedecod="" , pct = 0 ,freq = 0 ,aegraden=1 ,order = order-1 )

  final = rbind(fina,ordermiss) %>%
    arrange(order)%>%
    mutate(aegraden = factor(aegraden ,levels=c(3,2,1) ,ordered = T ) )  #定一个level，让3级显示在2级上方，2级显示在1级上方

  Annotation = final %>%
    distinct(trtn, ord, order) %>%
    filter(ord == ceiling(max(ord)/2)) %>%
    left_join(bign, by = c("trtn")) %>%
    mutate(GrpLabel = levels(aedata$trtc),
           nlabel = str_c("(N = ",as.character(bign),")"))

  # if (graphfont == "KT" ) {
  #   legendlabel <- c("三级及以上","二级","一级")
  # } else if (graphfont == "arialbd" ){
    legendlabel <- c(">=Grade 3","Grade 2","Grade 1")
  # }


  vline = data.frame()
  for (i in seq(2, nlevels(aedata$trtc), by=1 ) ) {
    vline1 = data.frame(x = ordermiss$order,
                        y = 100)
    vline = rbind(vline , vline1)
  }

  marginunit <- case_when(
    Annotpos <= 80 ~ c(0.4,0.1,0.2,0.5),
    Annotpos <= 100 ~ c(2,0.1,0.2,0.5),
    Annotpos <= 150 ~ c(3,0.1,0.2,0.5)
  )

  result <- ggplot() +
    geom_bar(data = final ,aes(x=factor(order) , y=pct ,fill = factor(aegraden)),
             position = "stack",
             stat = "identity" ,
             show.legend = TRUE)+
    geom_text(data = Annotation ,aes(x=order ,y=Annotpos ,label = GrpLabel ),
              na.rm = TRUE ,
              # family = "KT",
              fontface = "bold",
              size = 2,
              show.legend = FALSE) +
    geom_text(data = Annotation ,aes(x = order ,y=(Annotpos - 8) ,label = nlabel ),
              na.rm = TRUE ,
              # family = "KT",
              fontface = "bold",
              size = 2,
              show.legend = FALSE)+
    geom_vline(data = vline ,aes(xintercept = x) ,
               linetype="longdash" ,
               colour="#6a6a6a" ,
               linewidth=0.5,
               show.legend = FALSE)+
    labs(y = Ylabel ,
         x = Xlabel) +
    scale_fill_manual("",
                      labels = legendlabel,
                      values = color,
                      # guide = guide_legend()
    ) +
    scale_x_discrete (expand = c(0,0.7)
                      , labels = arrange(distinct(final,aedecod, order), order)$ aedecod) +
    scale_y_continuous(breaks = seq(0,100,by = 10), #Y轴长度
                       expand = c(0,0)
                       # expand = expansion(mult = c(0,Y_expand)) ,#2024-12更新：Y轴的扩展长度
                       # No space below the bars but Y_expand*100% above them
                       # limits = c(0,100)
                      ) +
    coord_cartesian(ylim = c(0, 100),clip = "off" )+
    theme_bw() +
    theme(
      text = element_text(face = "bold", color = "black", size = 8),
      axis.text = element_text(face = "bold", color = "black", size = 6),
      # strip.text = element_blank(),
      # panel.border = element_blank(),
      # panel.background = element_blank(),
      # panel.spacing.x =unit(0, "pt"),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      legend.position = legendpos , #图例位置
      # legend.title = element_blank(),
      # legend.text = element_text(family = graphfont ,
      #                            size = 12 ,
      #                            colour = "black" ,
      #                            face="bold"),
      # legend.background =element_rect(fill = "white"),
      legend.background = element_blank(),
      # legend.key.size =unit(12,"pt"),
      # plot.title = element_blank(),
      # plot.background = element_rect(fill = "white"),
      plot.margin = unit(marginunit,"cm"),
      # # plot.margin = unit(rep(4, 4), "cm")
      # strip.background = element_blank(),
      # strip.text.x = element_blank(),
      # axis.text.x = element_text(family = graphfont,
      #                            size = 11 ,
      #                            colour = "black" ),
      # axis.text.y = element_text(family = "arialbd" ,
      #                            size = 10 ,
      #                            colour = "black" ),
      # axis.line = element_line(linetype = "solid"),
      # axis.title = element_text(family = graphfont ,
      #                           size = 12,
      #                           colour = "black" ,
      #                           face="bold" ),
      # axis.ticks.x = element_blank() ,#横坐标的刻度点
      # axis.ticks.y = element_line() #横坐标的刻度点
    )



  if (max(nchar(final$aedecod))<=4 & max(final$order)<=11 ) {    #横轴文字旋转设置
    result = result
  }else if (max(nchar(final$aedecod))<=4 & max(final$order)>11 & max(final$order)<=17){
    result = result +
      theme(axis.text.x =element_text(angle = 45,hjust = 1, vjust = 0.5) )
  }else {
    result = result +
      theme(axis.text.x =element_text(angle = 90,hjust = 1, vjust = 0.5) )
  }

  return(result)
}
