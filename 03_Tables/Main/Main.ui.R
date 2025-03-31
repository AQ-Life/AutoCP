# TABLE
layout_columns(
  accordion(
    id = "table_accordion",
    open = TRUE,
    accordion_panel(
      "表格设置",
      icon = icon("file-alt"),
      selectInput("table_selected_dataset", "选择数据集", choices = NULL),
      selectInput("table_type", "选择 TFL 类型", choices = NULL),
      uiOutput("table_parameters"),
      actionButton("run_table", "生成表格", class = "btn-primary")
    )
  ),
  card(
    card_header("表格结果"),
    htmlOutput("tfl_result_table")
  ),
  col_widths = c(3, 9)
)