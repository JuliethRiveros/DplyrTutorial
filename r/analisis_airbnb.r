# Carga la base de datos de arriendos disponibles en Airbnb en Amsterdam en mayo 6 de 2019
# Fuente: http://insideairbnb.com/get-the-data.html
# Archivo: 	listings.csv.gz
# Descripci�n: Detailed Listings data for Amsterdam
arriendos <- read.csv("data/amsterdam_airbnb.csv")

# Cargar librer�a dplyr
library(dplyr)

# Preguntas:
# 1. �Cuales son los nombres de las columnas?
names(arriendos)
# Hay 106 columnas

# 2. �Cuales son los tipos de licencia de arriendamiento?
arriendos %>%
  distinct(license)

# �Cuales son las pol�ticas de cancelaci�n de arriendamiento?
arriendos %>%
  distinct(cancellation_policy)

# �En qu� ciudades/paises est�n los arrendatarios?
arriendos %>%
  distinct(host_location)

# �En qu� barrios est�n los arriendos?
arriendos %>%
  distinct(neighbourhood)

# 3. �Cuales son los 5 arrendatarios con m�s anuncios de arriendo?
arriendos %>%
  group_by(host_name) %>%
  summarise(cantidad = length(host_name)) %>%
  arrange(desc(cantidad)) %>%
  top_n(5)

# 4. �Cuales son los 5 arrendatarios que arrienda m�s noches a un huesped y cuantas noches?
arriendos %>%
  arrange(desc(maximum_nights)) %>%
  select(host_name, maximum_nights) %>%
  top_n(5)

# 5. �Cualdes son los nombres y urls de los 5 arriendos m�s costosos por noche?
arriendos %>%
  mutate( precio = as.numeric(substr(as.character(price), 2, 10))) %>% 
  arrange(desc(precio)) %>%
  select(precio, price, name, listing_url, host_name) %>%
  top_n(5, precio)

# 6. �Cuales son los 3 barrios con mayor cantidad de arriendos?
arriendos %>%
  group_by(neighbourhood) %>%
  summarise(barrio = neighbourhood[1], cantidad = length(host_id)) %>%
  arrange(desc(cantidad)) %>%
  top_n(3)
  

# 7. �Qu� arriendos tienen menor m�s ba�os?
arriendos %>%
  arrange(desc(bathrooms)) %>%
  select(listing_url, bathrooms) %>%
  top_n(5)

# �Qu� arriendos tienen mayor m�s camas?
arriendos %>%
  arrange(desc(beds)) %>%
  select(listing_url, beds) %>%
  top_n(5)

# 8. �Qu� arrendamientos tienen un �rea mayor a 100 metros cuadrados / 328 pies cuadrados?
arriendos %>%
  filter(square_feet>328) %>%
  arrange(desc(square_feet)) %>%
  select(square_feet, listing_url,price)

# 9. �Cuales son los arrendatarios con peor calificaci�n en cuanto a la comunicaci�n?
arriendos %>%
  filter(review_scores_communication < 4) %>%
  select(host_name, review_scores_communication) %>%
  arrange(desc(review_scores_communication)) %>%
  top_n(5)

# 10. �Cuantos arriendos no tienen TV?
arriendos %>%
  filter(!grepl("TV", amenities)) %>%
  tally()
