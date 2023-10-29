if (!require(shinythemes)) {
  install.packages("shinythemes")
}
if (!require(shinydashboard)) {
  install.packages("shinydashboard")
}
if (!require(leaflet)) {
  install.packages("leaflet")
}
if (!require(leaflet.extras)) {
  install.packages("leaflet.extras")
}
if (!require(dplyr)) {
  install.packages("dplyr")
}


library(shiny)
library(leaflet)
library(ggplot2)
library(shinythemes)
library(shinydashboard)
library(dplyr)
library(leaflet.extras)
# data
data <- read.csv("Excel.csv")
adresse_station = read.csv("Excel.csv")

ui <- dashboardPage(
  dashboardHeader(title = "Vélo'v"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Tableau de bord", tabName = "Dashboard", icon = icon("dashboard")),
      menuItem("Table", tabName = "Table", icon = icon("dashboard")),
      actionButton("reload_button", "Reload Data")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "Dashboard",
              fluidRow(
                # Add the valueBox to display the total number of bicycles
                valueBox(
                  value = sum(data$available_bikes),  # Calculate the total number of bikes
                  subtitle = "Nombre total de vélos disponibles",
                  icon = icon("bicycle"),
                  color = "blue"  # You can adjust the color as needed
                )),
              fluidRow(
                # Add the valueBox to display the total number of bicycles
                valueBox(
                  value = sum(data$available_bike_stands),  # Calculate the total number of bikes
                  subtitle = "Nombre total de vélos en utilisation",
                  icon = icon("bicycle"),
                  color = "red"  # You can adjust the color as needed
                )),
              fluidRow(
                box(title = "Carte", width = 7, solidHeader = TRUE, status = "primary", leafletOutput("map")),
                box(title = "Status de la station", width = 5, plotOutput("graphique2"))
              ),
              box(title = "Nombre de stations à bonus", width = 6, plotOutput("bonus_vs_no_bonus_plot")),
              box(title = "bike heatmap", width = 6, plotOutput("bike_heatmap")),
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
    ggplot(data, aes(x = status)) +
      geom_bar() +
      labs(x = "Station Status", y = "Count", title = "Distribution of Station Status")
  })
    
  output$bonus_vs_no_bonus_plot <- renderPlot({
    ggplot(data, aes(x = bonus, fill = bonus)) +
      geom_bar() +
      labs(title = "Bonus vs. No Bonus Stations", x = "Bonus Availability", y = "Count") +
      scale_fill_manual(values = c("yes" = "blue", "no" = "red")) +
      theme_minimal()
  })
  output$bike_heatmap <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%  # Add a base map
      addHeatmap(
        data = data,
        lat = ~position.lat,
        lng = ~position.lng,
        radius = 15,  # Adjust the radius as needed
        blur = 20  # Adjust the blur as needed
      )
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
    
    # Observer to handle data reload
    observeEvent(input$reload_button, {
      # Reload data from the API using the API URL
      raw_data <- GET(API_URL)
      updated_data <- fromJSON(rawToChar(raw_data$content), flatten = TRUE)
      
      # join the two dataframes
      merged_df <- bind_cols(updated_data, adresse_station %>%
                               select(-one_of(names(updated_data)))
      )
      # Update the 'data' variable with the newly loaded data
      data <<- updated_data
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
