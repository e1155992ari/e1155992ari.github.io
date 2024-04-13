library(shiny)
library(dplyr)
library(ggplot2)
dataset <- read.csv("Video Games Sales.csv")

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Data Analyses to Answer the Question"),
    
    # Tabset Panel
    tabsetPanel(
    
    # 1st Page (Analysis 1)
    tabPanel("Analysis 1",
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
           h5("This visualisation helps with examining, through a line graph, not only the trend of sales/number of copies sold for each video game genre, but also a comparison of sales/number of copies sold among genres against time. This will help in uncovering how well each game genre has performed over time in terms of popularity among gamers against each other, so that developers know what kind of games appeal the most to the crowd, and study the changes in consumer preferences as well.")
        )
    )
),

  # 2nd Page (Analysis 2)
  tabPanel("Analysis 2",
           # Sidebar with a slider input for number of bins 
           sidebarLayout(
             sidebarPanel(
               sliderInput(inputId = "range2",
                           label = "Range of Years Examined:",
                           min = 1983,
                           max = 2012,
                           value = c(1983,2012))
             ),
             
             # Show a plot of the generated distribution
             mainPanel(
               plotOutput("stackedbar"),
               h5("This visualisation compare Year against Global, with comparisons among regions like North America, Europe, Japan and others as the focus this time. Stacked bar charts, where each region is expressed as a percentage of the total contribution will be helpful in visualising which regions contribute the most amount of sales for games in comparison to each other.")
             )
           )
  ),

# 3rd Page (Analysis 3)
tabPanel("Analysis 3",
         # Sidebar with a slider input for number of bins 
         sidebarLayout(
           sidebarPanel(
             sliderInput(inputId = "range3",
                         label = "Range of Years Examined:",
                         min = 1983,
                         max = 2012,
                         value = c(1983,2012))
           ),
           
           # Show a plot of the generated distribution
           mainPanel(
             plotOutput("scatterplot"),
             h5("This visualisation compares Year against Rank since both these variables are numerical variables, and using a scatterplot to visualise the trend can help with finding out if there exists an association between year and global sales generated. Investigating the association can then help with determining if time context indeed contributes to sales or not, and help developers be more cognizant of influences changing the times the game industry is in, so that they can take advantages of certain major developments to maximise their sales.")
           )
         )
)

))


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
  
  # Define server logic for Analysis #2
  output$stackedbar <- renderPlot({
    # Filter data based on selected range of years
    filtered_df <- dataset %>%
      filter(Year >= input$range2[1] & Year <= input$range2[2])
    
    # Group data by year and calculate the total sales for each region
    
    japan_df <- filtered_df %>%
      group_by(Year) %>%
      summarise(sum = sum(Japan)) %>%
      mutate(Region = "Japan")
    
    na_df <- filtered_df %>%
      group_by(Year) %>%
      summarise(sum = sum(North_America)) %>%
      mutate(Region = "North America")
    
    europe_df <- filtered_df %>%
      group_by(Year) %>%
      summarise(sum = sum(Europe)) %>%
      mutate(Region = "Europe")
    
    row_df <- filtered_df %>%
      group_by(Year) %>%
      summarise(sum = sum(Rest_of_World)) %>%
      mutate(Region = "Rest of World")
    
    # Combine dataframes into one
    bar_chart_df <- bind_rows(japan_df, na_df, europe_df, row_df)
    
    # Plot bar chart
    ggplot(bar_chart_df, aes(x = Year, y = sum, fill = Region)) +
      geom_bar(stat = "identity") +
      labs(
        title = "Regional Sales over Time",
        x = "Year",
        y = "Sales",
        fill = "Region"
      ) +
      theme_minimal()
  })
  
  # Define server logic for Analysis #3
  output$scatterplot <- renderPlot({
    # Filter data based on selected range of years
    filtered_df <- dataset %>%
      filter(Year >= input$range3[1] & Year <= input$range3[2])
    
    # Create scatter plot
    ggplot(filtered_df, aes(x = Year, y = Rank)) +
      geom_point() +
      geom_smooth(method = "lm", se = FALSE) +  # Add linear trend line
      labs(
        title = "Rank vs Year",
        x = "Year",
        y = "Rank"
      )
  })
  
}


# Run the application 
shinyApp(ui = ui, server = server)
