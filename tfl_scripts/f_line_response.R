# tfl_scripts/f_line_response.R
generate_tfl <- function(data, popdata, params) {
  # 参数验证
  required_params <- c("analysis_pop", "x_varc", "x_varn","y_var", "treatment_group", "parameter")
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
  
  
  # point_jitter <- position_dodge(width = 0.4)
  
  filtered_data <- filtered_data %>% 
    mutate(trtc = filtered_data[[params$treatment_group]],
           xvarc = filtered_data[[params$x_varc]],
           xvarn = filtered_data[[params$x_varn]],
           yvar = filtered_data[[params$y_var]]) %>% 
    mutate(xvar = forcats::fct_reorder(xvarc, xvarn, min)) %>% 
    filter(PARAM == params$parameter)
  
  freq <- filtered_data %>% 
    group_by(PARAMCD, trtc, xvar) %>% 
    summarise(count = n()) %>% 
    ungroup()
  
  freqr <- filtered_data %>% 
    filter(AVAL == 1) %>% 
    group_by(PARAMCD, trtc, xvar) %>% 
    summarise(num = n()) %>% 
    ungroup()
  
  final <- left_join(freq, freqr, by = c("PARAMCD", "trtc", "xvar")) %>% 
    mutate(num = if_else(is.na(num), 0, num)) %>% 
    mutate(pct = 100*num/count) 
  
  
  result <- ggplot(final, aes(x = xvar, y = pct, group = trtc, color = trtc, shape = trtc, linetype = trtc)) +
    geom_line(linewidth = 1.2, na.rm = TRUE) +
    geom_point(size = 4, na.rm = TRUE) +
    theme_bw() +
    labs(y = "Response Rate (%)", x = NULL) +
    # scale_x_continuous(breaks = pull(arrange(distinct(final,AVISIT,AVISITN),AVISITN),AVISITN),
    #                    labels = pull(arrange(distinct(final,AVISIT,AVISITN),AVISITN),AVISIT),
    #                    limits = c(1, 32))+
    scale_y_continuous(breaks = seq(0,100, by = 10), limits = c(0, 100)) +
    # scale_color_manual(values = grp_color) +
    # scale_linetype_manual(values = c(1,1,1,3)) +
    # scale_shape_manual(values = c(16, 15, 17,1)) +
    theme(text = element_text(family = "sans", face = "bold", color = "black", size = 18),
          axis.text = element_text(family = "sans", face = "bold", color = "black", size = 12),
          # axis.text.x = element_text(family = "KT"),
          legend.position = "bottom", legend.title = element_blank(), legend.text = element_text(size = 12),
          legend.key.size = unit(1, "cm"),
          # plot.margin = unit(c(0,0,0,1), "cm"),
          legend.key.height = unit(0.5, "cm")) +
    coord_cartesian(clip = "off")
  
  # browser()
  return(result)
}

