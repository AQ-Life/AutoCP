# tfl_scripts/teae.R
generate_tfl <- function(data, popdata, params) {
  # 参数验证
  required_params <- c("analysis_pop", "treatment_group", "aebodsys", "aedecod", "ae_grade")
  missing_params <- setdiff(required_params, names(params))
  if (length(missing_params) > 0) {
    stop(paste("缺少必要参数:", paste(missing_params, collapse = ", ")))
  }
  
  # 数据筛选
  filtered_data <- data %>% 
    mutate(AEGrade = factor(get(params$ae_grade)))
  bigNdata <- popdata %>% 
    mutate(TRTA = TRT01A,
           TRTAN = TRT01AN,
           TRTP = TRT01P,
           TRTPN = TRT01PN
           )
  if (!is.null(params$filter_condition) && params$filter_condition != "") {
    filtered_data <- filtered_data %>% 
      filter(eval(parse(text = params$filter_condition)))
  }
  if (!is.null(params$analysis_pop) && params$analysis_pop != "") {
    filtered_data <- filtered_data %>% 
      filter(eval(parse(text = paste(params$analysis_pop,"%in% c('是','Y')") ))) %>% 
      mutate(TRTA = forcats::fct_reorder(TRTA, TRTAN, min))
    bigNdata <- bigNdata %>% 
      filter(eval(parse(text = paste(params$analysis_pop,"%in% c('是','Y')") )))
  }
  
  # 分组统计
  result <- basic_table(title = "不良事件按系统器官分类和首选术语汇总",
                        # subtitles = "Safety Set",
                        # main_footer = "MedDRA version xx.x.",
                        show_colcounts = TRUE) %>% 
    split_cols_by(params$treatment_group) %>% 
    count_occurrences_by_grade("AEGrade", grade_groups = list("any TEAE" = levels(filtered_data$AEGrade))) %>%
    # analyze_num_patients("USUBJID",
    #                      .stats = c("unique"),
    #                      .labels = c(unique = "Any TEAE")) %>%
    split_rows_by(params$aebodsys,
                  split_label = "System Organ Class",
                  label_pos = "topleft",
                  child_labels = "visible") %>% 
    summarize_occurrences_by_grade("AEGrade") %>% 
    split_rows_by(params$aedecod,
                  split_label = "Preferred Team",
                  label_pos = "topleft",
                  child_labels = "visible") %>% 
    summarize_num_patients("USUBJID",
                           .stats = c("unique"),
                           .labels = c(unique = "Any AEDECOD")) %>% 
    count_occurrences_by_grade("AEGrade", .indent_mods = -1L) %>% 
    build_table(filtered_data
                ,alt_counts_df = bigNdata
    )
  
  return(result)
}

