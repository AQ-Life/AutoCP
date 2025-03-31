# tfl_scripts/f_kmplot.R
generate_tfl <- function(data, popdata, params) {
  # 参数验证
  required_params <- c("analysis_pop", "cnsr", "y_var", "ByTime","treatment_group", "treatment_groupN", "parameter")
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


library(sqldf)
library(survival)
library(cowplot)


ByTime <- eval(parse(text = params$ByTime))
Cuminc <- eval(parse(text = params$Cuminc))
if (Cuminc == FALSE){
  legendpos <- c(0.85, 0.9)
} else {
  legendpos <- c(0.85, 0.1)
}
ShowAreaYN <- eval(parse(text = params$ShowAreaYN))
LineMedianYN <- eval(parse(text = params$LineMedianYN))
PvalYN <- eval(parse(text = params$PvalYN))
RiskLabel <- "Number at risk"


  filtered_data <- filtered_data %>% 
    mutate(trtn = filtered_data[[params$treatment_groupN]],
           trtc = filtered_data[[params$treatment_group]],
           yvar = filtered_data[[params$y_var]],
           cnsr = filtered_data[[params$cnsr]]) %>% 
    filter(PARAM == params$parameter)

GrpLabelData <- filtered_data %>% 
  distinct(trtn, trtc) %>% 
  rename(StratumNum = trtn) %>% 
  mutate(Stratum = paste0(trtc,"[",StratumNum,"]"))


survdata <- data.frame()
risktable <- data.frame()
Xvline <- c()
MaxAval <- ceiling(max(filtered_data$yvar)/ByTime)*ByTime

for (i in 1:nrow(GrpLabelData)) {
  fitdata <- filtered_data %>%
    filter(trtn == i)
  
  fit <- survfit(Surv(yvar, cnsr==0) ~ trtc, data = fitdata)
  
  fit1 <- data.frame(time = fit$time,
                     risk = fit$n.risk,
                     event = fit$n.event,
                     censor = fit$n.censor,
                     surv = fit$surv,
                     upper = fit$upper,
                     lower = fit$lower,
                     StratumNum = i) %>%
    mutate(surv_cnsr = if_else(censor==1,surv,NA))
  
  risk1 <- summary(fit, times = seq(0,MaxAval,by = ByTime))
  risk2 <- data.frame(time = risk1$time,
                      risk = risk1$n.risk,
                      event = risk1$n.event,
                      censor = risk1$n.censor,
                      surv = risk1$surv,
                      upper = risk1$upper,
                      lower = risk1$lower,
                      StratumNum = i)
  
  Xvline[i] <- median(fit)
  
  survdata <- bind_rows(survdata, fit1)
  risktable <- bind_rows(risktable, risk2)
}


PvalueFormat <- function(x){
  if (x < 0.001){
    y <- "<0.001"
  } else if(x > 0.999){
    y <- ">0.999"
  } else {
    y <- round(x, 3)
  }
  return(y)
}

Pvalue1 <- survdiff(Surv(yvar, cnsr==0) ~ trtn, data = filtered_data)
Pvalue2 <- data.frame(chisq = Pvalue1$chisq, pvalue = Pvalue1$pvalue)
PvalueText <- PvalueFormat(Pvalue1$pvalue)
XendData <- as.data.frame(Xvline)

dummy <- data.frame()
for (i in seq(0, MaxAval, by = ByTime)) {
  for (j in c(1:nrow(GrpLabelData))) {
    dummy1 <- data.frame(time = i,
                         StratumNum = j)
    dummy <- bind_rows(dummy, dummy1)
  }
}

survdata <- left_join(survdata, GrpLabelData, by = c("StratumNum"))
survdata <- survdata %>% 
  mutate(Stratum = forcats::fct_reorder(Stratum, StratumNum, min))
if (Cuminc == TRUE){
  survdata <- survdata %>%
    mutate(surv = 1 - surv,
           upper = 1- upper,
           lower = 1- lower,
           surv_cnsr = 1- surv_cnsr)
}

risktable <- left_join(dummy, risktable, by = c("time", "StratumNum")) %>%
  mutate(risk = if_else(is.na(risk), 0, risk)) %>%
  left_join(GrpLabelData, by = c("StratumNum"))

# browser()

p1 <- ggplot(survdata) +
  geom_step(aes(x = time, y = surv*100, group = Stratum, color = factor(Stratum)), linewidth = 1.2) +
  geom_point(aes(x=time, y=surv_cnsr*100), shape = 3, na.rm = TRUE) +
  labs(x="Time", y="proportion (%)") +
  scale_x_continuous(breaks=seq(0,MaxAval,by=ByTime), limits = c(0,MaxAval)) +
  scale_y_continuous(breaks=seq(0,100,by=10), limits = c(0,100)) +
  theme_bw() +
  theme(legend.position = legendpos,
        legend.title = element_blank(),
        legend.key.width = grid::unit(1, "cm"),
        legend.key.height = unit(1, "cm"),
        legend.background = element_blank(),
        axis.title.y = element_text(margin = margin(r = 40)),
        text = element_text(face = "bold", color = "black", size = 18),
        axis.text = element_text( face = "bold", color = "black", size = 12)
        # axis.title = element_text(family = "sans",
        #                           face = "bold",
        #                           color = "black",
        #                           size = 12),
        # legend.text = element_text(face = "bold",
        #                            color = "black",
        #                            size = 12)
        )



if (ShowAreaYN == TRUE){
  p1 <- p1 + geom_ribbon(aes(x = time, ymin = lower*100, ymax = upper*100, group = Stratum, fill = factor(Stratum)), alpha = 0.2, show.legend = FALSE)
}

if (LineMedianYN == TRUE){
  p1 <- p1 +
    annotate("segment", x = -Inf, y = 50, xend = max(Xvline), yend = 50, linetype = 2, alpha = 0.5) +
    geom_segment(data=XendData, aes(x = Xvline, y=-Inf, xend = Xvline, yend = 50), linetype = 2, alpha = 0.5)
}

if (PvalYN == TRUE){
  p1 <- p1 + geom_label(x = 0, y = 0, label = paste0("P = ",PvalueText), size = 4, hjust = "inward")
}



p2 <- ggplot(risktable) +
  geom_text(aes(x = time, y = -StratumNum, label = risk, color = factor(Stratum)),
            size = 4, fontface = "bold")+
  theme_classic()+
  scale_x_continuous(breaks=seq(0,MaxAval,by=ByTime), limits = c(0,MaxAval)) +
  scale_y_continuous(breaks=-seq(1:nrow(GrpLabelData)),
                     expand = c(0.1, 0.1),
                     labels = paste0("[",GrpLabelData$StratumNum,"]")
                     ) +
  theme(legend.position = "none",
        plot.margin = unit(c(0,0,0,0.3), "cm"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), panel.border = element_blank(),
        axis.line = element_blank(),
        plot.title = element_text(family = "sans",
                                  face = "bold",
                                  color = "black",
                                  size = 12),
        text = element_text(face = "bold", color = "black", size = 18),
        axis.text.x = element_blank(), axis.ticks = element_blank(),
        axis.text.y = element_text(hjust = 1, color = "black", face = NULL))+
  labs(x = NULL, y = NULL, title = RiskLabel) +
  coord_cartesian( clip = "off")

result <- plot_grid(p1, p2, ncol = 1, align = "v", rel_heights = c(1, 0.1*nrow(GrpLabelData)))

return(result)
}


