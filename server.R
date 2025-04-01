source(
  file = "global.R",
  local = TRUE,
  encoding = "UTF-8"
)

source(
  file = "ui.R",
  local = TRUE,
  encoding = "UTF-8"
)

shinyServer <- shinyServer(
  function(input, output, session) {
    
    datasets <- reactiveValues()
    
    table_config <- reactive({
      read_excel("config/tfl_parameters.xlsx",  sheet = "Table") %>% 
        mutate(Parameter = make.names(Parameter)) %>%
        # 确保 Default 列存在
        mutate(Default = if("Default" %in% names(.)) Default else NA_character_)
    })
    
    graph_config <- reactive({
      read_excel("config/tfl_parameters.xlsx",  sheet = "Graph") %>% 
        mutate(Parameter = make.names(Parameter)) %>%
        # 确保 Default 列存在
        mutate(Default = if("Default" %in% names(.)) Default else NA_character_)
    })
    

    observeEvent(input$sas_files, {
      req(input$sas_files)
      datasets$data <- setNames(
        lapply(input$sas_files$datapath, read_sas),
        tools::file_path_sans_ext(input$sas_files$name)
      )
      
      source(file = paste0(DATAVIEW_PATH, "Main/Main.server.R"), local = T, encoding = "UTF-8")
      source(file = paste0(PROJECT_PATH, "Main/Main.server.R"), local = T, encoding = "UTF-8")
      
      updateSelectInput(session, "selected_dataset", choices = names(datasets$data))
      updateSelectInput(session, "table_selected_dataset", choices = names(datasets$data))
      updateSelectInput(session, "graph_selected_dataset", choices = names(datasets$data))
    })
    
    observe({
      updateSelectInput(session, "table_type", choices = unique(table_config()$table_type))
      updateSelectInput(session, "graph_type", choices = unique(graph_config()$graph_type))
    })
    
    output$table_parameters <- renderUI({
      req(input$table_type, input$table_selected_dataset)
      dataset <- datasets$data[[input$table_selected_dataset]]
      params <- table_config() %>% filter(table_type == input$table_type)
      
      lapply(1:nrow(params), function(i) {
        p <- params[i, ]
        inputId <- p$Parameter
        label <- p$Label
        default_value <- p$Default
        
        switch(as.character(p$Type),
               "column_select" = selectInput(
                 inputId, label, 
                 choices = c("", colnames(dataset)),
                 selected = if(!is.na(default_value) && default_value %in% colnames(dataset)) 
                   default_value else ""),
               
               "multi_column" = selectInput(
                 inputId, label,
                 choices = c("", colnames(dataset)),
                 multiple = TRUE,
                 selected = if(!is.na(default_value)) {
                 #   c("AGE", "SEX",    "ETHNIC")
                   default_values <- strsplit(default_value, ",\\s*")[[1]]
                   default_values[default_values %in% colnames(dataset)]
                   # browser()
                 } else NULL
                 ),

               "text_input" = textInput(
                 inputId, label,
                 value = if(!is.na(default_value)) default_value else ""),
               
               NULL
        )
      })
    })
    
    
    output$graph_parameters <- renderUI({
      req(input$graph_type, input$graph_selected_dataset)
      dataset <- datasets$data[[input$graph_selected_dataset]]
      params <- graph_config() %>% filter(graph_type == input$graph_type)
      
      # 每行显示的列数
      num_cols <- 1
      
      # 将参数列表按列分割
      param_groups <- split(1:nrow(params), ceiling(seq_len(nrow(params)) / num_cols))
      
      # 动态生成多列布局
      tagList(
        lapply(param_groups, function(group) {
          fluidRow(
            lapply(group, function(i) {
              p <- params[i, ]
              inputId <- p$Parameter
              label <- p$Label
              default_value <- p$Default
              
              # 创建控件
              control <- switch(as.character(p$Type),
                                "column_select" = selectInput(
                                  inputId, label, 
                                  choices = c("", colnames(dataset)),
                                  selected = if(!is.na(default_value) && default_value %in% colnames(dataset)) 
                                    default_value else ""),
                                
                                "parameter_select" = selectInput(
                                  inputId, label, 
                                  choices = c("", unique(dataset$PARAM)),
                                  selected = if(!is.na(default_value) && default_value %in% colnames(dataset)) 
                                    default_value else ""),
                                
                                "yn_select" = selectInput(
                                  inputId, label, 
                                  choices = c("TRUE", "FALSE"),
                                  selected = c("FALSE")),
                                
                                "num_select" = selectInput(
                                  inputId, label, 
                                  choices = seq(1,100),
                                  selected = c(5)),
                                
                                "multi_column" = pickerInput(
                                  inputId, label,
                                  choices = colnames(dataset),
                                  multiple = TRUE,
                                  # 处理多个默认值
                                  selected = if(!is.na(default_value)) {
                                    default_values <- strsplit(default_value, ",\\s*")[[1]]
                                    default_values[default_values %in% colnames(dataset)]
                                  } else NULL,
                                  options = list(actionsBox = TRUE)),
                                
                                "text_input" = textInput(
                                  inputId, label,
                                  value = if(!is.na(default_value)) default_value else ""),
                                
                                NULL
              )
              
              # 返回一个 column 包裹的控件
              column(width = 12 / num_cols, control)
            })
          )
        })
      )
    })
    
    
    # output$graph_parameters <- renderUI({
    #   req(input$graph_type, input$graph_selected_dataset)
    #   dataset <- datasets$data[[input$graph_selected_dataset]]
    #   params <- graph_config() %>% filter(graph_type == input$graph_type)
    #   
    #   lapply(1:nrow(params), function(i) {
    #     p <- params[i, ]
    #     inputId <- p$Parameter
    #     label <- p$Label
    #     default_value <- p$Default
    #     
    #     switch(as.character(p$Type),
    #            "column_select" = selectInput(
    #              inputId, label, 
    #              choices = c("", colnames(dataset)),
    #              selected = if(!is.na(default_value) && default_value %in% colnames(dataset)) 
    #                default_value else ""),
    #            
    #            "parameter_select" = selectInput(
    #              inputId, label, 
    #              choices = c("", unique(dataset$PARAM)),
    #              selected = if(!is.na(default_value) && default_value %in% colnames(dataset)) 
    #                default_value else ""),
    #            
    #            "yn_select" = selectInput(
    #              inputId, label, 
    #              choices = c("TRUE", "FALSE"),
    #              selected = c("FALSE")),
    #            
    #            "num_select" = selectInput(
    #              inputId, label, 
    #              choices = seq(1,100),
    #              selected = c(5)),
    #            
    #            "multi_column" = pickerInput(
    #              inputId, label,
    #              choices = colnames(dataset),
    #              multiple = TRUE,
    #              # 处理多个默认值
    #              selected = if(!is.na(default_value)) {
    #                default_values <- strsplit(default_value, ",\\s*")[[1]]
    #                default_values[default_values %in% colnames(dataset)]
    #              } else NULL,
    #              options = list(actionsBox = TRUE)),
    #            
    #            "text_input" = textInput(
    #              inputId, label,
    #              value = if(!is.na(default_value)) default_value else ""),
    #            
    #            NULL
    #     )
    #   })
    # })
    
    table_result <- eventReactive(input$run_table, {
      req(input$table_type, input$table_selected_dataset)
      dataset <- datasets$data[[input$table_selected_dataset]]
      adsldata <- datasets$data[["adsl"]]
      params <- table_config() %>% filter(table_type == input$table_type)
      
      param_values <- lapply(params$Parameter, function(p) input[[p]])
      names(param_values) <- params$Parameter
      
      # 获取对应的 TFL 程序文件名
      table_program <- table_config() %>%
        filter(table_type == input$table_type) %>%
        pull(table_program) %>% 
        unique()
      
      script_file <- file.path("tfl_scripts", paste0(table_program, ".R"))
      
      if (!file.exists(script_file)) {
        validate("未找到对应的 TFL 程序文件")
        return()
      }
      
      # 创建独立环境运行脚本
      env <- new.env()
      if (!is.environment(env)) {
        stop("无法创建有效的环境对象")
      }
      source(script_file, local = env)
      tryCatch({
        do.call(env$generate_tfl, list(data = dataset, popdata = adsldata, params = param_values))
      }, error = function(e) {
        validate(paste("执行错误:", e$message))
      })
    })
    
    graph_result <- eventReactive(input$run_graph, {
      req(input$graph_type, input$graph_selected_dataset)
      dataset <- datasets$data[[input$graph_selected_dataset]]
      adsldata <- datasets$data[["adsl"]]
      params <- graph_config() %>% filter(graph_type == input$graph_type)
      
      param_values <- lapply(params$Parameter, function(p) input[[p]])
      names(param_values) <- params$Parameter
      
      # 获取对应的 TFL 程序文件名
      graph_program <- graph_config() %>%
        filter(graph_type == input$graph_type) %>%
        pull(graph_program) %>% 
        unique()
      
      script_file <- file.path("tfl_scripts", paste0(graph_program, ".R"))
      
      if (!file.exists(script_file)) {
        validate("未找到对应的 TFL 程序文件")
        return()
      }
      
      # 创建独立环境运行脚本
      env <- new.env()
      if (!is.environment(env)) {
        stop("无法创建有效的环境对象")
      }
      source(script_file, local = env)
      tryCatch({
        do.call(env$generate_tfl, list(data = dataset, popdata = adsldata, params = param_values))
      }, error = function(e) {
        validate(paste("执行错误:", e$message))
      })
    })
    
    output$tfl_result_table <- renderUI({
      req(table_result())
      # browser()
      if (inherits(table_result(), "TableTree")) {
        tags$div(as_html(table_result()))
      } else if (is.data.frame(table_result()) || is.matrix(table_result())) {
        DTOutput(table_result())
      } else {
        validate("生成的表格格式不支持")
      }
    })
    
    output$tfl_result_graph <- renderPlot({
      req(graph_result())
      # browser()
      if (inherits(graph_result(), "gg") || inherits(graph_result(), "ggplot")) {
        # 如果是 ggplot 对象，使用 plotOutput 渲染
        graph_result()
      } else {
        validate("生成的表格格式不支持")
      }
    }, height = 600)
  }
)

shinyApp(shinyUI,shinyServer)
