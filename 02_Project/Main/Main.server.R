# Project
source(file = paste0(PROJECT_PATH, "project_baseline/screen_failure.R"), local = T, encoding = "UTF-8")
source(file = paste0(PROJECT_PATH, "project_baseline/analysis_pop.R"), local = T, encoding = "UTF-8")
source(file = paste0(PROJECT_PATH, "project_baseline/analysis_pop_list.R"), local = T, encoding = "UTF-8")

source(file = paste0(PROJECT_PATH, "teae/teae_overall.R"), local = T, encoding = "UTF-8")
source(file = paste0(PROJECT_PATH, "teae/sae_socpt.R"), local = T, encoding = "UTF-8")
source(file = paste0(PROJECT_PATH, "teae/sae_socpt_toxgr.R"), local = T, encoding = "UTF-8")
  
  # browser()

  # 监听导航栏变化
  observeEvent(input$projectNav, {
    output$projectDynamicContent <- renderUI({
      switch(
        input$projectNav,
        "disposition" = tagList(
          h4("筛选失败", class = "chinese-text"),
          uiOutput("screen_failure"),
          h4("分析人群", class = "chinese-text", style = "margin-top: 100px;"),
          uiOutput("analysis_pop"),
          DTOutput("analysis_pop_list")
        ),
        "teae" = tagList(
          h4("不良事件汇总", class = "chinese-text"),
          # plotOutput("sex_barchart"),
          uiOutput("teae_overall")
        ),
        "teae_socpt" = tagList(
          h4("严重不良事件按系统器官分类和首选术语汇总", class = "chinese-text"),
          uiOutput("sae_socpt"),
          h4("严重不良事件按系统器官分类，首选术语和严重等级汇总", class = "chinese-text", style = "margin-top: 100px;"),
          uiOutput("sae_socpt_toxgr"),
        ),
        NULL
      )
    })
  })














# output$right_project <- renderUI({
#   selected_panel <- req(input$project_accordion)
# 
#   source(file = paste0(PROJECT_PATH, "project_baseline/sex_barchart.R"), local = T, encoding = "UTF-8")
#   source(file = paste0(PROJECT_PATH, "project_baseline/baseline_listing.R"), local = T, encoding = "UTF-8")
#   
#   switch(selected_panel,
#          # 基线特征布局：2x2网格 [[7]]
#          "project_baseline" = {
#            layout_columns(
#              col_widths = c(6, 6),  # 12列网格系统中的6+6 [[2]]
#              gap = "15px",          # 列间距 [[7]]
#              card(card_header("年龄分布"),
#                   card_body(plotOutput("sex_barchart"))),
#              card(card_header("基线特征列表"),
#                   card_body(DTOutput("baseline_listing")))
#            )
         # },
         # 不良事件布局：图+表格 [[10]]
         # "project_ae" = {
         #   card(
         #     card_header("不良事件趋势"),
         #     card_body(plotOutput("ae_plot"))
         #   )
         #   card(
         #     card_header("事件统计表"),
         #     card_body(DTOutput("ae_table")),
         #     margin_top = "20px"  # 上下间距 [[10]]
         #   )
         # }
#   )
# })