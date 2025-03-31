# DATA VIEW

# 动态生成列选择器
output$column_selector <- renderUI({
  req(input$selected_dataset)
  dataset <- datasets$data[[input$selected_dataset]]
  selected_var_label <- var_labels(dataset)[colnames(dataset)]
  colnamelabel <- paste(colnames(dataset), selected_var_label, sep = "  ")
  checkboxGroupInput("selected_columns", "选择需要展示的列",
                     selected = colnames(dataset),
                     choiceValues = colnames(dataset),
                     choiceNames = colnamelabel)
})


# 显示筛选后的数据
output$data_table <- renderDT({
  req(input$selected_dataset)
  dataset <- datasets$data[[input$selected_dataset]]
  
  # 确保至少有一列被选中
  selected_cols <- input$selected_columns
  if (is.null(selected_cols) || length(selected_cols) == 0) {
    selected_cols <- colnames(dataset)
  }
  
  # 应用筛选条件
  filtered_data <- dataset
  for (col in selected_cols) {
    if (exists(paste0("filter_", col))) {
      filter_values <- input[[paste0("filter_", col)]]
      if (is.character(filter_values) || is.factor(filter_values)) {
        filtered_data <- filtered_data[filtered_data[[col]] %in% filter_values, ]
      } else if (is.numeric(filter_values)) {
        filtered_data <- filtered_data[filtered_data[[col]] >= filter_values[1] & filtered_data[[col]] <= filter_values[2], ]
      }
    }
  }
  
  # 渲染表格
  DT::datatable(
    mutate_if(filtered_data[, selected_cols, drop = FALSE], is.character, as.factor),
    selection = "none",
    filter = list(position = "top"),
    escape = FALSE,
    extensions = "KeyTable",
    options = list(
      keys = TRUE,
      search = list(regex = TRUE),
      columnDefs = list(
        list(orderSequence = c("desc", "asc"), targets = "_all"),
        list(className = "dt-center", targets = "_all")
      ),
      autoWidth = TRUE,
      processing = FALSE,
      pageLength = 10
    )
  )
})
