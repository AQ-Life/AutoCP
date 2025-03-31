
output$sex_barchart <- renderPlot({
  dataadsl <- datasets$data$adsl
  ggplot(dataadsl) +
    geom_bar(aes(x = SEX))
})
