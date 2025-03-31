
# output$baseline_listing <- renderDT({
#   dataadsl <- datasets$data$adsl
#   summary_df <- as.data.frame(do.call(cbind, lapply(dataadsl, summary)))
#   datatable(summary_df, options = list(pageLength = 5))
# })

rm(list = ls())
library(haven)
library(tidyverse)
library(rtables)

adsl <- read_sas("data/adsl.sas7bdat")

adsl_fail <- adsl %>% 
  filter(ITTFL != "是") %>% 
  mutate(trtc = "Total",
         SCFAILRE = factor(SCFAILRE))

adsl_itt <- adsl %>% 
  filter(ITTFL == "是")

basic_table() %>% 
  split_cols_by("trtc") %>% 
  add_colcounts() %>% 
  summarize_row_groups(label_fstr = "任一筛选失败") %>% 
  analyze_vars("SCFAILRE",
               .stats = "count_fraction",
               denom = "N_col") %>%
  build_table(adsl_fail)
  

# basic_table() %>% 
#   split_cols_by("TRT01P") %>% 
#   add_overall_col("Total") %>% 
#   add_colcounts() %>% 
#   build_table(adsl_itt)
