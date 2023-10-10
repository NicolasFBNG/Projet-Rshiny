library(shiny)
library(leaflet)
library(ggplot2)

# Read data
data <- read.csv("C:/Users/msaoudi/Documents/GitHub/Projet-Rshiny/Excel.csv")

# Define UI for the combined app
ui <- fluidPage(
  
  # App title
  titlePanel("Tabsets"),
  
  # Sidebar layout with input and output definitions
  sidebarLayout(
    
    # Sidebar panel for inputs
    sidebarPanel(
      
      # Input: Slider for the number of observations to generate
      sliderInput("n",
                  "Number of classes:",
                  value = 10,
                  min = 1,
                  max = 20)
      
    ),
    
    # Main panel for displaying outputs
    mainPanel(
      
      # Output: Tabset w/ plot, summary, table, map, and graph
      tabsetPanel(type = "tabs",
                  tabPanel("Plot", plotOutput("plot")),
                  tabPanel("Summary", verbatimTextOutput("summary")),
                  tabPanel("Table", tableOutput("table")),
                  tabPanel("Map", leafletOutput("map")),
                  tabPanel("Graphique", plotOutput("graphique"))  # New tab for the graph
      )
      
    )
  )
)

# Define server logic for the combined app
server <- function(input, output, session) {
  
  # Reactive expression to generate the requested distribution
  d <- reactive({
    data
  })
  
  # Generate a plot of the data
  output$plot <- renderPlot({
    data$available_bikes
    hist(data$available_bikes,
         main = "histo",
         col = "#75AADB", border = "white", breaks = input$n)
  })
  
  # Generate a summary of the data
  output$summary <- renderPrint({
    summary(d())
  })
  
  # Generate an HTML table view of the data
  output$table <- renderTable({
    d()
  })
  
  # Generate and render the map
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      addMarkers(
        data = data,
        lat = ~`position.lat`,   # Use position.lat column
        lng = ~`position.lng`    # Use position.lng column
      )
  })
  
  # Generate and render the graph
  output$graphique <- renderPlot({
    ggplot(data, aes(x = name, y = available_bikes)) +
      geom_bar(stat = "identity") +
      labs(x = "Name", y = "Available bike", title = "Nombre de velo disponible par station")
  })
}

# Create and run the combined Shiny app
shinyApp(ui, server)
