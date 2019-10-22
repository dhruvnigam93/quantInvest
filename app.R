library(shiny)
library(rvest)
library(PortfolioAnalytics)
source('funs.R')
source('portfolioAnalytics.R')
schemeCodes = getAllSchemeMFCodes()

# Use a fluid Bootstrap layout
ui <- fluidPage(    
  
  # Give the page a title
  titlePanel("Historic Portfolio Performance"),
  
  # Generate a row with a sidebar
  sidebarLayout(      
    
    # Define the sidebar with one input
    sidebarPanel(
      numericInput(inputId = "nFunds" , label = "Number of Funds:",value = 2),
      hr(),
      selectInput(inputId = "mf1", label = "Fund Name:", 
                  choices=getAllSchemeCodes()$`Scheme Name`),
      numericInput(inputId = "w1" , label = "Capital Invested:",value = 100),
      hr(),
      selectInput(inputId = "mf2", label = "Fund Name:", 
                  choices=getAllSchemeCodes()$`Scheme Name`),
      numericInput(inputId = "w2" , label = "Capital Invested:",value = 100),
      hr(),
      selectInput(inputId = "rebalancePeriod", label = "Rebalance Frequency:", 
                  choices=c("Yearly", "Monthly" , "Never")),
      selectInput(inputId = "investmentMode", label = "Mode of Investment:", 
                  choices=c("Monthly SIP" , "Lump sum")),
      actionButton("do", "update")
    ),
    
    # Create a spot for the plot
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Historical Performance", plotOutput("histPlot")),
                  tabPanel("Expected Future Performance", plotOutput("expectedPerf"))
                  )
    )
    
  )
)

#### Define server logic to plot historic performance for given portfolio #### 
server <- function(input, output) {
  observeEvent(input$do , {
    output$histPerfPlot <- renderPlot({
      mfNames = isolate(c(input$mf1 , input$mf2))
      capital = isolate(c(input$w1 , input$w2))
      modeInvest = isolate( ifelse(input$investmentMode=="Monthly SIP", 1,0) )
      rebalPeriod  = isolate(input$rebalancePeriod)
      
      if(any(duplicated(mfNames))) stop("Multiple entries for same fund") ## Throw error if multiple entries for same fund
      
      if(modeInvest == 1){
        perfData = getHistoricPerfSIP(mfNames, capital,schemeCodes , rebalPeriod)
      } else{
        perfData = getHistoricPerfLump(mfNames, capital,schemeCodes , rebalPeriod)
      }
      assign(x = "perfData", value = perfData, envir = .GlobalEnv)
      if(Sys.info()[1] == "Darwin") {writeLog()} ## write log only on local machine
      output$histPlot = perfData$plot_historic
      
      output$expectedPerf = getExpectedPerformance(capital = capital , 
                                                   mean_return = perfData$numericMetrics$values[1] ,
                                                   sd_return = perfData$numericMetrics$values[2]  , n = 3)
    })
  })
  
}

shinyApp(ui, server) # launch app
