library(shiny)
library(DT)
library(ggplot2)
mashableData <- read.csv(file="OnlineNewsPopularity.csv", header=TRUE, sep=",")

# Initialize the UI
ui <- fluidPage(
  titlePanel("Online News Popularity"),
  
  DT::dataTableOutput("table")
)

# Initialize the server
server <- function(input, output) {

  output$table <- DT::renderDataTable(DT::datatable({mashableData}))
  
}

# Return a shiny app object
shinyApp(ui, server)