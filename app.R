library(shiny)
library(DT)
library(ggplot2)
library(tidyverse)
library(corrgram)

#options(scipen = 999)

# Add column values to better organize data
mashableData <- read.csv(file="OnlineNewsPopularity.csv", header=TRUE, sep=",")
mashableData <- mashableData %>%
  mutate(weekend = if_else(is_weekend == 0, "False", 
                           ifelse(is_weekend == 1, "True", NA)))

mashableData <- mashableData %>% 
  mutate(weekday = if_else(weekday_is_monday == 1, "Monday",
                           ifelse(weekday_is_tuesday == 1, "Tuesday", 
                                  ifelse(weekday_is_wednesday == 1, "Wednesday", 
                                         ifelse(weekday_is_thursday == 1, "Thursday", 
                                                ifelse(weekday_is_friday == 1, "Friday", 
                                                       ifelse(weekday_is_saturday == 1, "Saturday", 
                                                              ifelse(weekday_is_sunday == 1, "Sunday", NA))))))))

mashableData <- mashableData %>% 
  mutate(weekday_value = if_else(weekday_is_monday == 1, 1,
                                 ifelse(weekday_is_tuesday == 1, 2, 
                                        ifelse(weekday_is_wednesday == 1, 3, 
                                               ifelse(weekday_is_thursday == 1, 4, 
                                                      ifelse(weekday_is_friday == 1, 5, 
                                                             ifelse(weekday_is_saturday == 1, 6, 
                                                                    ifelse(weekday_is_sunday == 1, 7, NA))))))))

mashableData <- mashableData %>% 
  mutate(data_channel = ifelse(data_channel_is_lifestyle == 1, "Lifestyle",
                               ifelse(data_channel_is_entertainment == 1, "Entertainment", 
                                      ifelse(data_channel_is_socmed == 1, "Social Media", 
                                             ifelse(data_channel_is_bus == 1, "Business",
                                                    ifelse(data_channel_is_tech == 1, "Tech",
                                                           ifelse(data_channel_is_world == 1, "World", NA)))))))

mashableData <- mashableData %>% 
  mutate(data_channel_value = ifelse(data_channel_is_lifestyle == 1, 1,
                                     ifelse(data_channel_is_entertainment == 1, 2, 
                                            ifelse(data_channel_is_socmed == 1, 3, 
                                                   ifelse(data_channel_is_bus == 1, 4,
                                                          ifelse(data_channel_is_tech == 1, 5,
                                                                 ifelse(data_channel_is_world == 1, 6, NA)))))))

# For ordering days chronologically on plot axis
mashableData$weekday <- as.character(mashableData$weekday)
mashableData$weekday <- factor(mashableData$weekday, levels=unique(mashableData$weekday))

# Initialize the UI
ui <- fluidPage(
  titlePanel("Online News Popularity"),
  
  # Display filters in sidebar
  # Create checkboxes and sliders to filter data 
  sidebarLayout(position = "left",
    sidebarPanel(
      sliderInput("shares_filter", "Number of Shares",
                  min(mashableData$shares), 
                  max(mashableData$shares),
                  c(min(mashableData$shares), max(mashableData$shares))
      ),
      sliderInput("date_filter", "Timedelta",
                  min(mashableData$timedelta), 
                  max(mashableData$timedelta),
                  c(min(mashableData$timedelta), max(mashableData$timedelta))
      ),
      checkboxGroupInput("data_channel_filter", "Category",
                         choices = c("Lifestyle", "Entertainment", "Social Media", "Business", "Tech", "World"),
                         selected = c("Lifestyle", "Entertainment", "Social Media", "Business", "Tech", "World")
      ),
      checkboxGroupInput("weekday_filter", "Days",
                         choices = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"),
                         selected = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
      ) 
    ),
    # Display data table in main panel
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Table", DT::dataTableOutput("table")),
                  tabPanel("Plot", plotOutput("plot")),
                  #tabPanel("Word Cloud", tableOutput("word_cloud")),
                  tabPanel("About", tableOutput("about"))
      )
    )
  )
)

# Initialize the server
server <- function(input, output) {

  # Filter data from selected inputs and display data table
  output$table <- DT::renderDataTable(DT::datatable({
      filteredData <- mashableData[mashableData$data_channel %in% input$data_channel_filter,]
      filteredData <- filteredData[filteredData$weekday %in% input$weekday_filter,]
      filteredData
    })
  )
  
  # Filter data and create plots
  output$plot <- renderPlot({
    filteredData <- mashableData[mashableData$data_channel %in% input$data_channel_filter,]
    filteredData <- filteredData[filteredData$weekday %in% input$weekday_filter,]
    ggplot(filteredData) +
      geom_bar(aes(x = weekday, y = shares, fill = data_channel), stat = "identity") +
      labs(title = "Number of Total Article Shares by Weekday") +
      theme_bw()
  })
  
  output$summary <- renderTable({
    
  })
}

# Return a shiny app object
shinyApp(ui, server)