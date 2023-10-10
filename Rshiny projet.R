if(!require(ggplot2))
{
  install.packages("ggplot2")
  library(ggplot2)
}

if(!require(shiny))
{
  install.packages("shiny")
  library(shiny)
}

if(!require(RMySQL))
{
  install.packages("RMySQL")
  library(RMySQL)
}

install.packages("httr")
install.packages("jsonlite")
install.packages("dplyr")
library(dplyr)
library(httr)
library(jsonlite)

base <- 'https://api.jcdecaux.com/vls/v1/stations?contract=Lyon&apiKey=6a406398a0b8768df2169c2858b7b6bcd368dde2'
res <- GET(base)

data <- fromJSON(rawToChar(res$content), flatten = TRUE)

# Création de table
#Host: sql11.freesqldatabase.com
#Database name: sql11646686
#Database user: sql11646686
#Database password: 3eB8YUZEsX
#Port number: 3306

con <- dbConnect(MySQL(),
                 user = 'sql11646686',
                 password = '3eB8YUZEsX',
                 host = 'sql11.freesqldatabase.com',
                 dbname = 'sql11646686')

# TEST
dbWriteTable(con,"test",iris)
dbGetQuery(con, "SELECT * FROM test LIMIT 2;")


write.csv(df, "C:/Users/Jbusson/Documents/Rshiny/Excel.csv")


# Charger les données depuis un fichier CSV
data <- read.csv("Excel.csv") 

# Création application Shiny
ui <- fluidPage(
  # En-tête de l'application
  headerPanel("Tableau de bord"),
  
  # Onglets
  tabsetPanel(
    tabPanel("Données",
             # Tableau des données
             dataTableOutput("tableau")
    ),
    tabPanel("Graphique",
             # Graphique
             plotOutput("graphique")
    )
  )
)


server <- function(input, output) {
  output$tableau <- renderDataTable({
    data
  })
  
  # Créer un graphique dans l'onglet "Graphique"
  output$graphique <- renderPlot({
    ggplot(data, aes(x = name, y = available_bike)) +
      geom_point() +
      labs(x = "Name", y = "Available bike", title = "Nombre de velo disponible par station")
  })
}


server <- function(input, output) {
  # Histogramme 1
  output$histogramme1 <- renderPlot({
    ggplot(data, aes(x = variable1)) +
      geom_histogram(binwidth = 1, fill = "blue", color = "black") +
      labs(x = "Variable 1", y = "Variable 2", title = "Graphique 1 1")
  })
  
  # Histogramme 2
  output$histogramme2 <- renderPlot({
    ggplot(data, aes(x = variable2)) +
      geom_histogram(binwidth = 1, fill = "green", color = "black") +
      labs(x = "Variable 2", y = "Variable 2", title = "Graphique 2")
  })
  
}

shinyApp(ui, server)