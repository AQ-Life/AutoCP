# DATA VIEW
layout_columns(
  accordion(
    id = "data_accordion",
    open = TRUE,
    accordion_panel(
      "数据集设置",
      icon = icon("table"),
      fileInput("sas_files", "上传 SAS 数据集 (.sas7bdat)", multiple = TRUE),
      selectInput("selected_dataset", "选择数据集", choices = NULL),
      uiOutput("column_selector")
    )
  ),
  card(
    card_header("数据预览"),
    DTOutput("data_table")
  ),
  col_widths = c(3, 9)
)
