
library(httr)
library(jsonlite)
library(RMySQL)
library(tidygeocoder)
library(tidyr)
library(dplyr)
#Obtention clé API

base<-'https://api.jcdecaux.com/vls/v1/stations?contract='
contract<-'Lyon'
key<-'&apiKey=b59e18c899829b0903cc948041c9286aecc68768'

API_URL<-paste0(base, contract, key)



raw_data<-GET(API_URL)
station_list<-fromJSON(rawToChar(raw_data$content), flatten = TRUE)

write.csv(station_list, "Excel.csv")
#Création table



adresse_station<-reverse_geocode(.tbl = station_list, lat = position.lat, long = position.lng, method = 'osm',
                                 address = address_found, full_results = TRUE)
adresse_station = read.csv("adresse.csv")
adresse_station$boundingbox = NULL
write.csv(adresse_station, "Adresse.csv")

merged_df <- bind_cols(station_list, adresse_station %>%
                         select(-one_of(names(station_list)))
)


#Création d'une connexion avec la base

con<-dbConnect(MySQL(), 
               user='sql11645719',
               password='RgUN3i8rfb',
               host='sql11.freesqldatabase.com',
               dbname='sql11645719')

# Envoyer data dans la base 
dbWriteTable(con, name = 'Données_vélos', value = station_list, overwrite=TRUE)



adresse_station = read.csv("Excel.csv")


