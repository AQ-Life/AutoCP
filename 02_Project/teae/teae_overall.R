output$teae_overall <- renderUI({

  dataadsl <- datasets$data$adsl
  dataadae <- datasets$data$adae
  
adsl <- dataadsl %>% 
  filter(SAFFL == "是") %>% 
  mutate(TRTP = TRT01P, TRTPN = TRT01PN)

adae <- dataadae %>% 
  filter(SAFFL == "是") %>% 
  mutate(TRTP = forcats::fct_reorder(TRTA, TRTAN, min))

var_exist <- c("AECAT") %in% names(adae)
solicited_exist <- any(grepl("征集性", unique(adae$AECAT)))

if (var_exist){
  if (solicited_exist){
    adae <- adae %>% 
      mutate(flag1 = TRUE %>% with_label("接种后发生的不良事件         "),
             flag2 = (AEREL == "相关" | AEREL == "") %>% with_label("接种后发生的与研究疫苗相关的不良事件"),
             flag5 = (ATOXGRN >=3 | ATOXGRN == .) %>% with_label("严重程度>=3级的不良事件"),
             flag6 = ((ATOXGRN >=3 | ATOXGRN == .) & (AEREL == "相关" | AEREL == "")) %>% with_label("与研究疫苗相关的严重程度>=3级的不良事件"),
             flag11 = (AECAT == "征集性不良事件") %>% with_label("征集性不良事件"),
             flag12 = (AESCAT == "征集性局部不良事件") %>% with_label("征集性局部不良事件"),
             flag13 = (AESCAT == "征集性全身不良事件") %>% with_label("征集性全身不良事件"),
             flag14 = ((AEREL == "相关" | AEREL == "") & AECAT == "征集性不良事件") %>% with_label("相关的征集性不良事件"),
             flag15 = ((AEREL == "相关" | AEREL == "") & AESCAT == "征集性局部不良事件") %>% with_label("相关的征集性局部不良事件"),
             flag16 = ((AEREL == "相关" | AEREL == "") & AESCAT == "征集性全身不良事件") %>% with_label("相关的征集性全身不良事件"),
             flag21 = (AECAT == "非征集性不良事件") %>% with_label("非征集性不良事件"),
             flag22 = ((AEREL == "相关" | AEREL == "") & AECAT == "非征集性不良事件") %>% with_label("相关的非征集性不良事件"),
             flag31 = (AESER == "是") %>% with_label("SAE"),
             flag32 = ((AEREL == "相关" | AEREL == "") & AESER == "是") %>% with_label("相关的SAE"),
             flag41 = (AESI == "是") %>% with_label("AESI"),
             flag42 = ((AEREL == "相关" | AEREL == "") & AESI == "是") %>% with_label("相关的AESI"),
      )
  }
} else {
  adae <- adae %>% 
    mutate(flag1 = TRUE %>% with_label("Any AE         "),
           flag2 = (TRTEMFL == "是") %>% with_label("Any TEAE"),
           flag2 = (TRTEMFL == "是" & (AEREL == "相关" | AEREL == "")) %>% with_label("Any TRAE"),
           flag5 = (TRTEMFL == "是" & (ATOXGRN >=3 | ATOXGRN == .)) %>% with_label("严重程度>=3级的TEAE"),
           flag6 = (TRTEMFL == "是" & (ATOXGRN >=3 | ATOXGRN == .) & (AEREL == "相关" | AEREL == "")) %>% with_label("严重程度>=3级的TRAE"),
           flag31 = (TRTEMFL == "是" & AESER == "是") %>% with_label("SAE"),
           flag32 = (TRTEMFL == "是" & (AEREL == "相关" | AEREL == "") & AESER == "是") %>% with_label("相关的SAE"),
           flag41 = (TRTEMFL == "是" & AESI == "是") %>% with_label("AESI"),
           flag42 = (TRTEMFL == "是" & (AEREL == "相关" | AEREL == "") & AESI == "是") %>% with_label("相关的AESI"),
    )
}

result <- basic_table(show_colcounts = TRUE,
            inset = 2) %>% 
  split_cols_by("TRTP") %>% 
  add_overall_col("合计") %>%
  count_patients_with_flags(
    "USUBJID",
    flag_variables = grep("^flag", names(adae), value = TRUE),
    denom = "N_col"
  ) %>% 
  build_table(adae, alt_counts_df= adsl)


return(as_html(result))
})