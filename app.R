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
    
    # Create a spot for the barplot
    mainPanel(
      plotOutput(outputId = "histPerfPlot")  
    )
    
  )
)

#### Define server logic to plot various variables against mpg #### 
server <- function(input, output) {
  observeEvent(input$do , {
    output$histPerfPlot <- renderPlot({
      mfNames = isolate(c(input$mf1 , input$mf2))
      modeInvest = isolate( ifelse(input$investmentMode=="Monthly SIP", 1,0) )
      
      if(modeInvest == 1){
        perfData = getHistoricPerfSIP(mfNames, c(0.5,0.5),schemeCodes)
      } else{
        perfData = getHistoricPerfLump(mfNames, c(0.5,0.5),schemeCodes)
      }
      
      # writeLog()
      perfData$plot_historic
    })
  })
  
}

shinyApp(ui, server)
