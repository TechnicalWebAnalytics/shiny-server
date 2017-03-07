dateRange <- reactive({
  req(auth())
  dateRangeInput('dateRange',
                 label = 'Date range input: yyyy-mm-dd',
                 start = Sys.Date() - 2, end = Sys.Date() + 2
  )
})