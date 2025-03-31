
output$analysis_pop_list <- renderDT({
  library(Hmisc)
  
  dataadsl <- datasets$data$adsl
  
  adsl <- dataadsl %>% 
    filter(ITTFL == "是") 
  
  fl_vars <- names(adsl)[grepl("FL$", names(adsl))]
  
  keep_vars <- c()
  
  for (var in fl_vars) {
    var_label <- label(adsl[[var]])
    if (!grepl("失败", var_label)) {
      keep_vars <- c(keep_vars, var)
    }
  }
  
  labels <- sapply(keep_vars, function(var) label(adsl[[var]]))
  
  result <- adsl %>% 
    select(USUBJID, TRT01P, TRT01PN, keep_vars)
  
  colnames(result) <- var_labels(result)[colnames(result)]
  
  return(result)
})

