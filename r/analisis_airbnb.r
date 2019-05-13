# Carga la base de datos de arriendos disponibles en Airbnb en Amsterdam en mayo 6 de 2019
# Fuente: http://insideairbnb.com/get-the-data.html
# Archivo: 	listings.csv.gz
# Descripciín: Detailed Listings data for Amsterdam
arriendos <- read.csv("data/amsterdam_airbnb.csv")

# Cargar librería dplyr
library(dplyr)

# Preguntas:
# 1. ¿Cuales son los nombres de las columnas?
names(arriendos)
# Hay 106 columnas

# 2. ¿Cuales son los tipos de licencia de arriendamiento?
arriendos %>%
  distinct(license)

# ¿Cuales son las políticas de cancelación de arriendamiento?
arriendos %>%
  distinct(cancellation_policy)

# ¿En qué ciudades/paises están los arrendatarios?
arriendos %>%
  distinct(host_location)

# ¿En qué barrios están los arriendos?
arriendos %>%
  distinct(neighbourhood)

# 3. ¿Cuales son los 5 arrendatarios con más anuncios de arriendo?
arriendos %>%
  group_by(host_name) %>%
  summarise(cantidad = length(host_name)) %>%
  arrange(desc(cantidad)) %>%
  top_n(5)

# 4. ¿Cuales son los 5 arrendatarios que arrienda más noches a un huesped y cuantas noches?
arriendos %>%
  arrange(desc(maximum_nights)) %>%
  select(host_name, maximum_nights) %>%
  top_n(5)

# 5. ¿Cualdes son los nombres y urls de los 5 arriendos más costosos por noche?
arriendos %>%
  mutate( precio = as.numeric(substr(as.character(price), 2, 10))) %>% 
  arrange(desc(precio)) %>%
  select(precio, price, name, listing_url, host_name) %>%
  top_n(5, precio)

# 6. ¿Cuales son los 3 barrios con mayor cantidad de arriendos?
arriendos %>%
  group_by(neighbourhood) %>%
  summarise(barrio = neighbourhood[1], cantidad = length(host_id)) %>%
  arrange(desc(cantidad)) %>%
  top_n(3)
  

# 7. ¿Qué arriendos tienen menor más baños?
arriendos %>%
  arrange(desc(bathrooms)) %>%
  select(listing_url, bathrooms) %>%
  top_n(5)

# ¿Qué arriendos tienen mayor más camas?
arriendos %>%
  arrange(desc(beds)) %>%
  select(listing_url, beds) %>%
  top_n(5)

# 8. ¿Qué arrendamientos tienen un área mayor a 100 metros cuadrados / 328 pies cuadrados?
arriendos %>%
  filter(square_feet>328) %>%
  arrange(desc(square_feet)) %>%
  select(square_feet, listing_url,price)

# 9. ¿Cuales son los arrendatarios con peor calificación en cuanto a la comunicación?
arriendos %>%
  filter(review_scores_communication < 4) %>%
  select(host_name, review_scores_communication) %>%
  arrange(desc(review_scores_communication)) %>%
  top_n(5)

# 10. ¿Cuantos arriendos no tienen TV?
arriendos %>%
  filter(!grepl("TV", amenities)) %>%
  tally()
