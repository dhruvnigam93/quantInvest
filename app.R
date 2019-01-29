library(shiny)
library(rvest)
library(PortfolioAnalytics)
source('H:/R Workspace/project-x/funs.R')
source('H:/R Workspace/project-x/portfolioAnalytics.R')

# Use a fluid Bootstrap layout
ui <- fluidPage(    
  
  # Give the page a title
  titlePanel("Historic Portfolio Performance"),
  
  # Generate a row with a sidebar
  sidebarLayout(      
    
    # Define the sidebar with one input
    sidebarPanel(
      selectInput(inputId = "mf1", label = "Fund Name:", 
                  choices=getAllSchemeCodes()$`Scheme Name`),
      hr(),
      selectInput(inputId = "mf2", label = "Fund Name:", 
                  choices=getAllSchemeCodes()$`Scheme Name`),
      hr()
    ),
    
    # Create a spot for the barplot
    mainPanel(
      plotOutput(outputId = "phonePlot")  
    )
    
  )
)

# Define server logic to plot various variables against mpg ----
server <- function(input, output) {
  mfNames = c(input$mf1 , input$mf2)
  
  
  
}

shinyApp(ui, server)
