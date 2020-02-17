#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(tidyverse)
library(shiny)
library(DT)
library(plotly)


data <- read_csv("data//nesta_data_export.csv") %>% 
  select(-description, -resources)
# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("AI governance data"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         sliderInput("tests",
                     "Number of bins:",
                     min = 1,
                     max = 50,
                     value = 30)
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        plotlyOutput("time_series"),
        dataTableOutput("table")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  
  
  my_data <- data %>%
    filter()
  
  
   output$table <- renderDataTable({
     return(my_data)
   })
   output$time_series <- renderPlotly({
     
     year_data <- my_data %>%
       mutate(year = lubridate::year(lubridate::parse_date_time(date, orders = c("mdY")))) %>% 
       group_by(year) %>%
       summarise(count = n())
     
     
     plot <- ggplot(year_data, aes(x = year, y = count)) +
       geom_line()
     
     ggplotly(plot)
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

