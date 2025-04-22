
library(shiny)
library(plotly)
library(dplyr)
library(rsconnect)

# Load dataset
#housing_data <- read.csv("/Users/drashti/Desktop/final_dashboard/current_topics.csv", stringsAsFactors = FALSE)
housing_data <- read.csv("current_topics.csv", stringsAsFactors = FALSE)


# UI
ui <- fluidPage(
  titlePanel("Crime Rate and Square Footage Analysis for Top 10 Crime-Heavy Zip Codes"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("price_range", "Select Price Range:",
                  min = min(housing_data$Price, na.rm = TRUE), 
                  max = max(housing_data$Price, na.rm = TRUE),
                  value = c(min(housing_data$Price, na.rm = TRUE), max(housing_data$Price, na.rm = TRUE)),
                  step = 50000, pre = "$")
    ),
    mainPanel(
      plotlyOutput("linePlot"),  # Interactive Line Graph for Top 10 Crime-Heavy Zip Codes
      p("Key Takeaways: This line graph shows the relationship between the average square footage and crime rate in the top 10 most crime-heavy zip codes. As square footage decreases, there is an observable trend of increasing crime rates across the zip codes."),
      plotlyOutput("linePlotLeast"),  # Interactive Line Graph for Top 10 Least Crime-Heavy Zip Codes
      p("Key Takeaways: This line graph illustrates the contrast between the average square footage and crime rates in the top 10 least crime-heavy zip codes. These zip codes show a tendency for higher average square footage, with lower crime rates, providing insights into more stable housing markets with less crime.")
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Calculate crime rate for each zip code and filter top 10 most crime-heavy zip codes
  top_crime_zips <- housing_data %>%
    group_by(ZipCode) %>%
    summarise(avg_crime_rate = mean(CrimeRate, na.rm = TRUE)) %>%
    arrange(desc(avg_crime_rate)) %>%
    head(10)  
  
  # Filter data for the top 10 zip codes
  filtered_data <- reactive({
    housing_data %>%
      filter(ZipCode %in% top_crime_zips$ZipCode) %>%
      filter(Price >= input$price_range[1] & Price <= input$price_range[2])
  })
  
  # Calculate average square footage and other stats per zip code
  avg_data <- reactive({
    filtered_data() %>%
      group_by(ZipCode) %>%
      summarise(
        avg_square_footage = mean(SquareFeet, na.rm = TRUE),
        avg_crime_rate = mean(CrimeRate, na.rm = TRUE),
        avg_bedrooms = mean(Bedrooms, na.rm = TRUE),
        avg_bathrooms = mean(Bathrooms, na.rm = TRUE)
      ) %>%
      arrange(ZipCode)  
  })
  
  # Line Plot: Zip Code vs. Average Square Footage for Crime-Heavy Zip Codes
  output$linePlot <- renderPlotly({
    req(nrow(avg_data()) > 0)  
    
    plot_ly(
      data = avg_data(),
      x = ~factor(ZipCode),  
      y = ~avg_square_footage,  
      type = "scatter",
      mode = "lines+markers",  
      text = ~paste(
        "Zip Code:", ZipCode,
        "<br>Average Square Footage:", round(avg_square_footage, 2),
        "<br>Crime Rate:", round(avg_crime_rate, 2),
        "<br>Bedrooms:", round(avg_bedrooms, 2),
        "<br>Bathrooms:", round(avg_bathrooms, 2)
      ),
      hoverinfo = "text"  
    ) %>% layout(
      title = "Average Square Footage vs. Zip Code (Top 10 Crime-Heavy Areas)",
      xaxis = list(title = "Zip Code"),
      yaxis = list(title = "Average Square Footage"),
      showlegend = FALSE
    )
  })
  
  # Calculate crime rate for each zip code and filter top 10 least crime-heavy zip codes
  least_crime_zips <- housing_data %>%
    group_by(ZipCode) %>%
    summarise(avg_crime_rate = mean(CrimeRate, na.rm = TRUE)) %>%
    arrange(avg_crime_rate) %>%  
    head(10)  
  
  # Filter data for the top 10 least crime-heavy zip codes
  filtered_data_least <- reactive({
    housing_data %>%
      filter(ZipCode %in% least_crime_zips$ZipCode) %>%
      filter(Price >= input$price_range[1] & Price <= input$price_range[2])
  })
  
  # Calculate average square footage and other stats per zip code for least crime-heavy zip codes
  avg_data_least <- reactive({
    filtered_data_least() %>%
      group_by(ZipCode) %>%
      summarise(
        avg_square_footage = mean(SquareFeet, na.rm = TRUE),
        avg_crime_rate = mean(CrimeRate, na.rm = TRUE),
        avg_bedrooms = mean(Bedrooms, na.rm = TRUE),
        avg_bathrooms = mean(Bathrooms, na.rm = TRUE)
      ) %>%
      arrange(ZipCode)  
  })
  
  # Line Plot: Zip Code vs. Average Square Footage for Least Crime-Heavy Zip Codes
  output$linePlotLeast <- renderPlotly({
    req(nrow(avg_data_least()) > 0)  
    
    plot_ly(
      data = avg_data_least(),
      x = ~factor(ZipCode),  
      y = ~avg_square_footage,  
      type = "scatter",
      mode = "lines+markers",  
      text = ~paste(
        "Zip Code:", ZipCode,
        "<br>Average Square Footage:", round(avg_square_footage, 2),
        "<br>Crime Rate:", round(avg_crime_rate, 2),
        "<br>Bedrooms:", round(avg_bedrooms, 2),
        "<br>Bathrooms:", round(avg_bathrooms, 2)
      ),
      hoverinfo = "text"  
    ) %>% layout(
      title = "Average Square Footage vs. Zip Code (Top 10 Least Crime-Heavy Areas)",
      xaxis = list(title = "Zip Code"),
      yaxis = list(title = "Average Square Footage"),
      showlegend = FALSE
    )
  })
}

# Run the Shiny app
shinyApp(ui, server)






