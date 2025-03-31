
output$analysis_pop <- renderUI({
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
  
  convert2logical <- function(x){
    ifelse(tolower(as.character(x)) %in% c ("是", "y", "yes"), TRUE, FALSE)
  }
  
  adsl <- adsl %>% 
    mutate(TRT01P = forcats::fct_reorder(TRT01P, TRT01PN, min),
           across(all_of(fl_vars), convert2logical))
  
  
  result <- basic_table() %>% 
    split_cols_by("TRT01P") %>% 
    add_overall_col("合计") %>% 
    add_colcounts() %>% 
    count_patients_with_flags(var = "USUBJID",
                              flag_variables = keep_vars,
                              denom = "N_col",
                              .labels = labels) %>% 
    build_table(adsl)
  
  # browser()
  return(as_html(result))
})























