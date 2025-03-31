output$sae_socpt <- renderUI({
  
  dataadsl <- datasets$data$adsl
  dataadae <- datasets$data$adae

adsl <- dataadsl %>% 
  mutate(trtc = TRT01A,
         trtn = TRT01AN)

adae <- dataadae %>% 
  filter(AESER %in% c("是", "Y")) %>% 
  mutate(trtc = forcats::fct_reorder(TRTA, TRTAN, min),
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


# browser()

if (nrow(adae) == 0) {
  result <- "无可用数据"
  return(result)
} else {
  result <- build_table(lyt, adae
                        ,alt_counts_df = adsl
  ) %>% 
    prune_table(prune_func = keep_rows(
      has_fraction_in_any_col(
        atleast = 0, # specify threshold，根据需要指定发生率>=5%的AE 
        col_names = levels(adsl$trtc)
      )
    ))
  return(as_html(result))
}

})

