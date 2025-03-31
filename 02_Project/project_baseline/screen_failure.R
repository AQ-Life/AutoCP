
output$screen_failure <- renderUI({
  
  dataadsl <- datasets$data$adsl
  
  adsl_fail <- dataadsl %>% 
    filter(ITTFL != "是") %>% 
    mutate(trtc = "合计",
           SCFAILRE = factor(SCFAILRE))
  
  result <- basic_table() %>% 
    split_cols_by("trtc") %>% 
    add_colcounts() %>% 
    summarize_row_groups(label_fstr = "任一筛选失败") %>% 
    analyze_vars("SCFAILRE",
                 .stats = "count_fraction",
                 denom = "N_col") %>%
    build_table(adsl_fail)
  
  return(as_html(result))
})




  
