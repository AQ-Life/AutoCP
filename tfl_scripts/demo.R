# tfl_scripts/demo.R
generate_tfl <- function(data, popdata, params) {
  # 参数验证
  required_params <- c("treatment_group", "analysis_vars")
  missing_params <- setdiff(required_params, names(params))
  if (length(missing_params) > 0) {
    stop(paste("缺少必要参数:", paste(missing_params, collapse = ", ")))
  }

  # 数据筛选
  filtered_data <- data
  if (!is.null(params$filter_condition) && params$filter_condition != "") {
    filtered_data <- filtered_data %>% 
      filter(eval(parse(text = params$filter_condition)))
  }
  if (!is.null(params$analysis_pop) && params$analysis_pop != "") {
    filtered_data <- filtered_data %>% 
      filter(eval(parse(text = paste(params$analysis_pop,"%in% c('是','Y')") )))
  }
  
  
  DemoBaseline <- function(x, .N_col){
    if (is.character(x)) {
      x <- as.factor(x) # 将字符型转换为因子型
    }
    
    if (is.numeric(x)){
      in_rows(n = sum(!is.na(x)),
              "median (sd)" = c(mean(x), sd(x)),
              "median (min, max)" = c(median(x), min(x), max(x)),
              .formats = c(n = "xx",
                           "mean (sd)" = "xx.x (xx.x)",
                           "median (min, max)" = "xx.x (xx.x - xx.x)"))
    } else if (is.factor(x)){
      lapply(table(x), function(xi){
        rcell(xi * c(1, 1/.N_col), format = "xx (xx.x%)")
      })
    } else {
      stop("Not Applied.")
    }
  }
  
  result <- basic_table(title = "人口学资料",
                        # subtitles = "Safety Set",
                        # main_footer = "Footnote: xxx.",
                        # prov_footer = "le provenance.",
                        show_colcounts = TRUE, # 展示big N
                        inset = 10) %>% 
    split_cols_by(params$treatment_group) %>% #设置列的分组
    add_overall_col("合计") %>% 
    # summarize_row_groups() %>% #为上一行代码中的行的分组添加合计，或为行的总的合计
    # split_rows_by("MHYN", page_by = TRUE) %>%  #设置行的分组
    # split_rows_by("ARM", split_fun = drop_split_levels) %>%  #设置行的子分组
    # summarize_row_groups() %>%
    # analyze(vars(!!!rlang::syms(params$analysis_vars)), afun = DemoBaseline) %>%
    analyze( params$analysis_vars,
            afun = DemoBaseline) %>%
    append_topleft("基线特征") %>% 
    build_table(filtered_data)

  return(result)
}


