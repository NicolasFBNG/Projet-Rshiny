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

if(!require(dplyr))
{
  install.packages("dplyr")
  library(dplyr)
}

install.packages("httr")
install.packages("jsonlite")
library(httr)
library(jsonlite)

base <- 'https://api.jcdecaux.com/vls/v3/stations?contract=Lyon&apiKey=6a406398a0b8768df2169c2858b7b6bcd368dde2'
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

#GRAPHIQUE

server <- function(input, output) {
  output$tableau <- renderDataTable({
    data
  })
  
#en cours de modification pour mettre le nom en abscisse verticalement et les bars par ordre croissant
  # Créer un graphique dans l'onglet "Graphique"
  output$graphique <- renderPlot({
    # Filtrer les stations avec le moins de vélos disponibles
    data_filtered <- data %>%
      arrange(desc(available_bikes)) %>%
      head(10) # Vous pouvez modifier le nombre de stations à afficher ici
    
    ggplot(data_filtered, aes(x = reorder(name, -available_bikes), y = available_bikes)) +
      geom_bar(stat = "identity") +
      labs(x = "Name", y = "Available bikes", title = "Les stations avec le plus de vélos disponibles") +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  })
  
  # Créer un graphique dans l'onglet "Graphique"
  output$graphique <- renderPlot({
    # Filtre avec les stations avec le moins de vélos disponibles
    data_filtered <- data %>%
      arrange(available_bikes) %>%
      head(10)
    ggplot(data_filtered, aes(x = name, y = available_bikes)) +
      geom_bar(stat = "identity") +
      labs(x = "Name", y = "Available bike", title = "Les stations avec le moins de vélos disponibles")
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