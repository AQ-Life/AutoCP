# TEAL MODULE


fluidPage(
  title = "Your app with teal as a module",
  selectInput("teal_data_id", "Select datasets", choices = c("iris", "mtcars"), selected = "iris", multiple = TRUE),
  ui_teal("teal", mods),
  ui_session_info("session_info")
)
