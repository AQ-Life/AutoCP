
output$baseline_listing <- renderDT({
  dataadsl <- datasets$data$adsl
  summary_df <- as.data.frame(do.call(cbind, lapply(dataadsl, summary)))
  datatable(summary_df, options = list(pageLength = 5))
})
