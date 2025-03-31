
source(
  file = "global.R",
  local = T,
  encoding = "UTF-8"
)

link_github <- tags$a(shiny::icon("github"), "Shiny", href = "https://github.com/rstudio/shiny", target = "_blank")

shinyUI(page_navbar(
  title = "AutoCP: Clinical Programming Tool",
  navbar_options = navbar_options(),
  header = list(
    tags$style(HTML("
     .navbar-header .navbar-brand {
        color: #3c8dbc !important;  /* 蓝色字体，覆盖默认颜色 */
        font-weight: bold;          /* 可选：加粗显示 */
      }
      body {
        font-family: Arial, '楷体', sans-serif;
        font-weight: bold;  /* 全局加粗 */
      }
      /* 针对中文元素强制使用楷体加粗（防止字体回退） */
      .chinese-text {
        font-family: '楷体', serif !important;
        font-weight: bold !important;
      }
      /* 覆盖特定组件的默认非粗体样式（如按钮、标签） */
      .btn, .form-control, .nav-link {
        font-weight: bold !important;
      }
    "))
  ),
  
  # 数据视图标签页
  navset_pill(
    # Data View
    nav_panel("Data View",
              source(
                file = paste0(DATAVIEW_PATH, "Main/Main.ui.R"),
                local = T,
                encoding = "UTF-8"
              )$value
    ),
    
    # Project Overview
    nav_panel("Project Overview",
              source(
                file = paste0(PROJECT_PATH, "Main/Main.ui.R"),
                local = T,
                encoding = "UTF-8"
              )$value
    ),

  # Table
  nav_panel("Table Generator",
            source(
              file = paste0(TABLE_PATH, "Main/Main.ui.R"),
              local = T,
              encoding = "UTF-8"
            )$value
  ),

  # Graph
  nav_panel("Graph Generator",
            source(
              file = paste0(FIGURE_PATH, "Main/Main.ui.R"),
              local = T,
              encoding = "UTF-8"
            )$value
  ),
  
  nav_spacer(),
  nav_menu(title = "Links", align = "right", nav_item(link_github))
  ),
  
  
  
  theme = bs_theme(
    version = 5,
    bg = "white",
    fg = "black",
    primary = "#0062cc",
    "accordion-button-active-bg" = "#e7f1ff",
    "accordion-button-active-color" = "#004085"
  ),
  footer = div(
    class = "d-flex justify-content-between w-100 p-3",  # 使用 Bootstrap Flex 布局 [[2]]
    div("Developed By Qi Luo", class = "text-muted small"),  # 左下角文字
    div("Copyright © 2025-2026, All Rights Reserved.", class = "text-muted small")   # 右下角文字
  )
)
)


