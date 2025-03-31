# FIGURE
layout_columns(
  accordion(
    id = "graph_accordion",
    open = TRUE,
    height_style = "content", 
    accordion_panel(
      "图形设置",
      icon = icon("chart-bar"),
      selectInput("graph_selected_dataset", "选择数据集", choices = NULL),
      selectInput("graph_type", "选择图形类型", choices = NULL),
      uiOutput("graph_parameters"),
      actionButton("run_graph", "生成图形", class = "btn-primary")
    )
  ),
  card(
    card_header("图形结果"),
    plotOutput("tfl_result_graph")
  ),
  col_widths = c(3, 9)
)