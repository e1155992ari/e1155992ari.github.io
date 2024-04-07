#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(dplyr)
library(ggplot2)
dataset <- read.csv("Video Games Sales.csv")

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Global Sales/Count of Video Game Genres over Time"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput(inputId = "select",
                        label = "Select Y-Variable to be Examined:",
                        choices = c("Global Sales", "Global Count")),
            sliderInput(inputId = "range",
                        label = "Range of Years Examined:",
                        min = 1983,
                        max = 2012,
                        value = c(1983,2012)),
            checkboxGroupInput(inputId = "options",
                               label = "Select Genres to Examine from List:", 
                               choices = unique(dataset$Genre),
                               selected = unique(dataset$Genre))
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("line"),
           h5("This visualisation helps with examining, through a line graph, not only the trend of sales/number of copies sold for each video game genre, but also a comparison of sales/number of copies sold among genres against time.")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  output$line <- renderPlot({
    # Filter dataset based on selected range of years
    filtered_data <- subset(dataset, Year >= input$range[1] & Year <= input$range[2])
    # Filter dataset based on selected genres
    filtered_data <- filtered_data[filtered_data$Genre %in% input$options, ]
    
    if (input$select == "Global Sales") {
      # Create line plot for Global Sales
      ggplot(data = filtered_data,
             aes(x = Year, y = Global, colour = Genre)) +
        geom_line() +
        labs(title = "Global Sales of each Genre over Time",
             x = "Year", 
             y = "Global Sales",
             colour = "Genre") +
        theme_minimal()
    } else if (input$select == "Global Count") {
      # Create a new data frame with count of observations for each genre per year
      count_data <- filtered_data %>%
        group_by(Year, Genre) %>%
        summarise(Count = n())
      
      # Create line plot for Global Count
      ggplot(data = count_data,
             aes(x = Year, y = Count, colour = Genre)) +
        geom_line() +
        labs(title = "Global Count of Copies Sold for Each Video Game Genre over Time",
             x = "Year", 
             y = "Count",
             colour = "Genre") +
        theme_minimal()
    }
  })
}


# Run the application 
shinyApp(ui = ui, server = server)
