# tfl_scripts/lineplot.R
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
 

  
  filtered_data <- filtered_data %>% 
    mutate(trtc = filtered_data[[params$treatment_group]],
           xvarc = filtered_data[[params$x_varc]],
           xvarn = filtered_data[[params$x_varn]],
           yvar = filtered_data[[params$y_var]]) %>% 
    mutate(xvar = forcats::fct_reorder(xvarc, xvarn, min)) %>% 
    filter(PARAM == params$parameter) %>% 
    group_by(trtc, PARAMCD, xvar) %>% 
    summarise(mean = mean(yvar)) %>% 
    ungroup() %>% 
    select(PARAMCD, trtc, xvar, mean)
  
  result <- ggplot(filtered_data, aes(x = xvar, y = mean, group = trtc, color = trtc, shape = trtc)) +
    # facet_grid(rows = vars(PARAMCD), 
    #            cols = vars(trtc),
    #            scales = "free_y") +
    geom_line(linewidth = 1.2) +
    geom_point(size = 4) +
    theme_bw() +
    labs(y = "Mean Change by Visit", x = "Weeks") +
    theme(text = element_text(family = "sans", face = "bold", color = "black", size = 18),
          axis.text = element_text(family = "sans", face = "bold", color = "black", size = 12),
          legend.key.width = grid::unit(1, "cm"),
          legend.position = "bottom", legend.title = element_blank(), legend.text = element_text(size = 12),
          )

  # browser()
  return(result)
}


