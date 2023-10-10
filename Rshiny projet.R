base<-'https://api.jcdecaux.com/vls/v3'
contract<-'Lyon'
key<-'&apiKey=b59e18c899829b0903cc948041c9286aecc68768'

API_URL<-paste0(base, contract, key)

library(shiny)
library(shinythemes)
library(ggmap)

raw_data<-GET(API_URL)
station_list<-fromJSON(rawToChar(raw_data$content), flatten = TRUE)

#Création table

colonne<-c("number","bike_stands","available_bike_stands","available_bikes","status","last_update")
Etat<-station_list[,colonne]

Etat<-data.frame(station_list$number,
                 station_list$bike_stands,
                 station_list$available_bike_stands,
                 station_list$available_bikes,
                 station_list$status,
                 station_list$last_update)

adresse_station<-reverse_geocode(lat=station_list$position.latitude, long=station_list$position.longitude, method = 'osm',
                                 address = address_found, full_results = TRUE)






#Création d'une connexion avec la base

con<-dbConnect(MySQL(),
               user='sql11645719',
               password='RgUN3i8rfb',
               host='sql11.freesqldatabase.com',
               dbname='sql11645719')

# Envoyer data dans la base
dbWriteTable(con, name = 'Données_vélos', value = station_list, overwrite=TRUE)


UI: 
  
  if(!require(shinythemes)){install.packages("shinythemes")}


library(shiny)
library(shinythemes)

ui <- navbarPage(
  title = NULL,
  theme = shinythemes::shinytheme("united"), # Utilisation d'un thème prédéfini (facultatif)
  tags$div(
    style = "display: flex; align-items: center;",
    tags$img(src ="logo_fixed.png", width = "100px", height = "auto"),
    tags$h3("Visualisation", style = "margin-left: 10px;") # Add a title if needed
  ),
  tabPanel("Carte"),
  tabPanel("Tableau"),
)



server <- function(input, output) {}

shinyApp(ui, server)