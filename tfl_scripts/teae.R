# tfl_scripts/teae.R
generate_tfl <- function(data, popdata, params) {
  # 参数验证
  required_params <- c("analysis_pop", "treatment_group", "aebodsys", "aedecod")
  missing_params <- setdiff(required_params, names(params))
  if (length(missing_params) > 0) {
    stop(paste("缺少必要参数:", paste(missing_params, collapse = ", ")))
  }
  # 数据筛选
  filtered_data <- data %>%
    mutate(across(all_of(c(params$aebodsys, params$aedecod)), ~if_else(is.na(.) | . == "", "未指定", .)))
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
    analyze_num_patients("USUBJID",
                         .stats = c("unique"),
                         .labels = c(unique = "至少发生一次TEAE")) %>% 
    split_rows_by(params$aebodsys,
                  split_label = "System Organ Class",
                  label_pos = "topleft",
                  child_labels = "visible") %>% 
    summarize_num_patients("USUBJID",
                           .stats = c("unique"),
                           .labels = c(unique = "any AEBODSYS")) %>% 
    count_occurrences(params$aedecod,
                      .indent_mods = -1L) %>%
    append_topleft("  Preferred Team") %>% 
    build_table(filtered_data
                ,alt_counts_df = bigNdata
    )
  
  return(result)
}