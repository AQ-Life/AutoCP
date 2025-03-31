rm(list = ls())
library(haven)
library(tidyverse)
library(rtables)
library(tern)

adsl <- read_sas("data/adsl.sas7bdat") %>% 
  mutate(trtc = TRT01A,
         trtn = TRT01AN)

adae <- read_sas("data/adae.sas7bdat") %>% 
  filter(AECAT == "征集性不良事件") %>% 
  mutate(trtc = TRTA,
         trtn = TRTAN,
         TOXGR = factor(ATOXGRN, labels = c("1级", "2级", "3级及以上"),levels = c(1, 2, 3)))

lyt <- basic_table(title = "Adverse events",
                   subtitles = "Safety Set",
                   main_footer = "MedDRA version xx.x.",
                   show_colcounts = TRUE) %>% 
  split_cols_by("trtc") %>% 
  analyze_num_patients("USUBJID",
                       .stats = c("unique"),
                       .labels = c(unique = "Total number of pts with at least one AE.")) %>% 
  split_rows_by("AEBODSYS",
                split_label = "System Organ Class",
                label_pos = "topleft",
                child_labels = "visible") %>% 
  summarize_num_patients("USUBJID",
                         .stats = c("unique"),
                         .labels = c(unique = "any AEBODSYS")) %>% 
  count_occurrences("AEDECOD",
                    .indent_mods = -1L) %>%
  append_topleft("  Preferred Team") 

build_table(lyt, adae
            ,alt_counts_df = adsl
) %>% 
  prune_table(prune_func = keep_rows(
    has_fraction_in_any_col(
      atleast = 0, # specify threshold，根据需要指定发生率>=5%的AE 
      col_names = levels(adsl$trtc)
    )
  ))
# trim_rows(teae)

