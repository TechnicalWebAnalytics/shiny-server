vals <- reactiveValues(account = NULL)
valsReact <- reactiveValues(id = NULL)

# capture profiles
prof <- reactive({
  req(auth())
  authorize(token=auth())
  vals$account <- list_accounts()
  selectInput("account", 
              "Choose Account:", 
              vals$account$name, selectize = TRUE)
})

render2 <- reactive({
  req(prof())
  # store selected account id
  # if vals account values name is equal to selected account, get selected account id
  valsReact$id <- vals$account[vals$account$name == input$account,]$id 
  # store selected property data
  vals$property <- list_webproperties(accountId = valsReact$id) 
  selectInput("property", 
              "Choose Property:", 
              paste0(vals$property$name,sep = " ", vals$property$id))
})

render3 <- reactive({
  req(render2())
  valsReact$uaid <- vals$property[paste0(vals$property$name,sep = " ", vals$property$id) == input$property,]$id # store selected property id
  vals$view <- list_profiles(accountId = valsReact$id, webPropertyId = valsReact$uaid) # store selected view data
  selectInput("view", 
              "Choose View:", 
              vals$view$name)
})

renderUI({
  req(auth())
  dateRange()
})

renderUI({
  req(auth())
  prof()
})

renderUI({
  req(prof())
  render2()
})

renderUI({
  req(render2())
  render3()
})