if (!require(shinythemes)) {
  install.packages("shinythemes")
}
if (!require(shinydashboard)) {
  install.packages("shinydashboard")
}
if (!require(leaflet)) {
  install.packages("leaflet")
}
if (!require(dplyr)) {
  install.packages("dplyr")
}

if (!require(dplyr)) {
  install.packages("crosstalk")
}

library(shiny)
library(leaflet)
library(ggplot2)
library(shinythemes)
library(shinydashboard)
library(dplyr)
library(crosstalk)
# data
data <- read.csv("Excel.csv")


ui <- dashboardPage(
  dashboardHeader(title = "Vélo'v"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Tableau de bord", tabName = "Dashboard", icon = icon("dashboard")),
      menuItem("Table", tabName = "Table", icon = icon("dashboard"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "Dashboard",
              fluidRow(
                box(title = "Carte", width = 7, solidHeader = TRUE, status = "primary", leafletOutput("map")),
                box(title = "Top 10 station", width = 5, plotOutput("graphique2"))
              ),
              box(title = "Contract Distribution", width = 6, plotOutput("contract_dist_plot")),
              box(title = "Bike Stands Histogram", width = 6, plotOutput("bike_stands_hist")),
              box(title = "Scatter Plot", width = 6, plotOutput("scatter_plot")),
              box(title = "Bike Stands and Available Bikes", width = 6, plotOutput("stacked_bar_chart")),
              box(title = "Filter by Postcode", width = 6,
                  selectInput("postcode_filter", "Select Postcode:",
                              choices = c("", unique(data$postcode)), # Use unique_postcodes
                              selected = "",
                              multiple = TRUE)
              )
      ),
      tabItem(tabName = "Table",
              tableOutput("table")
      )
    )
  )
)

server <- function(input, output, session) {
  
  d <- reactive({
    data
  })
  
  output$graphique1 <- renderPlot({
    data_filtered <- data %>%
      arrange(desc(available_bikes)) %>%
      head(10)
    
    data_filtered$modified_name <- substr(data_filtered$name, 8, nchar(data_filtered$name))
    
    ggplot(data_filtered, aes(x = modified_name, y = available_bikes)) +
      geom_bar(stat = "identity") +
      labs(x = "Name", y = "Available bike", title = "Nombre de vélo disponible par station")
  })
  
  output$graphique2 <- renderPlot({
    data_filtered <- data %>%
      arrange(desc(available_bikes)) %>%
      head(10)
    
    data_filtered$name <- substr(data_filtered$name, 8, nchar(data_filtered$name))
    
    ggplot(data_filtered, aes(x = reorder(name, -available_bikes), y = available_bikes)) +
      geom_bar(stat = "identity") +
      labs(x = "Name", y = "Available bikes", title = "Stations with the most available bikes") +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
  })
    
    output$contract_dist_plot <- renderPlot({
      ggplot(data, aes(x = contract_name)) +
        geom_bar(fill = "blue") +
        labs(title = "Bicycle Stations by Contract",
             x = "Contract Name",
             y = "Number of Stations")
    })
    
    output$bike_stands_hist <- renderPlot({
      ggplot(data, aes(x = bike_stands)) +
        geom_histogram(binwidth = 10, fill = "blue") +
        labs(title = "Distribution of Bike Stands",
             x = "Number of Bike Stands",
             y = "Count")
    })
    
    output$scatter_plot <- renderPlot({
      ggplot(data, aes(x = bike_stands, y = available_bike_stands)) +
        geom_point() +
        labs(title = "Bike Stands vs. Available Bike Stands",
             x = "Bike Stands",
             y = "Available Bike Stands")
    })
    
    df <- data
    df$unavailable_bike_stands <- df$bike_stands - df$available_bike_stands
    
    output$stacked_bar_chart <- renderPlot({
      ggplot(df, aes(x = contract_name)) +
        geom_bar(aes(y = bike_stands, fill = "Total Bike Stands"), stat = "identity") +
        geom_bar(aes(y = unavailable_bike_stands, fill = "Unavailable Bike Stands"), stat = "identity") +
        labs(title = "Bike Stands and Available Bikes by Contract",
             x = "Contract Name",
             y = "Count") +
        scale_fill_manual(values = c("Total Bike Stands" = "blue", "Unavailable Bike Stands" = "red")) +
        guides(fill = guide_legend(title = "Legend"))
    })
    
    output$map <- renderLeaflet({
      filtered_data <- data
      if (!is.null(input$postcode_filter) && !("" %in% input$postcode_filter)) {
        filtered_data <- filtered_data[filtered_data$postcode %in% input$postcode_filter, ]
      }
      
      leaflet() %>%
        addTiles() %>%
        addMarkers(
          data = filtered_data,
          lat = ~`position.lat`,
          lng = ~`position.lng`,
          popup = ~paste("Postcode: ", postcode, "<br>",
                         "Available Bikes: ", available_bikes, "<br>",
                         "Available Bike Stands: ", available_bike_stands)
        )
    })
    
    output$table <- renderTable({
      d()
    })
    
    output$summary <- renderPrint({
      summary(d())
    })
    
    output$plot <- renderPlot({
      data$available_bikes
      hist(data$available_bikes,
           main = "histo",
           col = "#75AADB", border = "white", breaks = input$n)
    })
}

shinyApp(ui, server)
