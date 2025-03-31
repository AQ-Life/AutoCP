# Project

# 文件路径：Project/Main/Main.ui.R
fluidPage(
  sidebarLayout(
    # 左侧手风琴侧边栏
    sidebarPanel(
      width = 3,  # 侧边栏宽度
      # 使用 navlistPanel 实现手风琴结构
      navlistPanel(
        id = "projectNav",  # 导航栏ID，用于服务器端监听
        widths = c(12, 12),  # 控制导航项宽度
        "基线",  # 导航栏标题
        tabPanel("受试者分布", value = "disposition"),  # 导航项1
        "安全性分析",
        tabPanel("不良事件汇总", value = "teae"),
        tabPanel("严重不良事件（SAE）", value = "teae_socpt")
      )
    ),

    # 右侧动态内容面板
    mainPanel(
      width = 9,  # 主面板宽度
      # 根据左侧选择显示不同内容
      uiOutput("projectDynamicContent")
    )
  )
)
