output$sae_socpt_toxgr <- renderUI({
  
  dataadsl <- datasets$data$adsl
  dataadae <- datasets$data$adae

adsl <- dataadsl %>% 
  mutate(trtc = TRT01A,
         trtn = TRT01AN)

adae <- dataadae %>% 
  filter(AESER %in% c("是", "Y")) %>% 
  mutate(trtc = forcats::fct_reorder(TRTA, TRTAN, min),
         TOXGR = factor(ATOXGRN, labels = c("1级", "2级", "3级及以上"),levels = c(1, 2, 3)))

lyt <- basic_table(#title = "Adverse events",
                   subtitles = "Safety Set",
                   main_footer = "MedDRA version xx.x.",
                   show_colcounts = TRUE) %>% 
  split_cols_by("trtc") %>% 
  count_occurrences_by_grade("TOXGR",
                             grade_groups = list("any TEAE" = c("1级", "2级", "3级及以上"))) %>%
  split_rows_by("AEBODSYS",
                child_labels = "visible",
                label_pos = "topleft",
                split_label = "System Organ Class") %>%
  summarize_occurrences_by_grade("TOXGR",
                                 grade_groups = list("any AEBODSYS" = c("1级", "2级", "3级及以上"))) %>%
  split_rows_by("AEDECOD",
                child_labels = "visible",
                label_pos = "topleft",
                split_label =  "Preferred Team") %>%
  summarize_num_patients("USUBJID",
                         .stats = c("unique"),
                         .labels = c(unique = "any AEDECOD")) %>% 
  count_occurrences_by_grade("TOXGR",
                             .indent_mods = -1L)

if (nrow(adae) == 0) {
  result <- "无可用数据"
  return(result)
} else {
  result <- build_table(lyt, adae
                        ,alt_counts_df = adsl
  )
  return(as_html(result))
}
})

