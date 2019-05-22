  cities <- data.frame("id" = 1:5,
                       "name" = c("Paris","Tokyo", "Bogotá", "Barcelona", "New York"))

persons <- data.frame("id" = 1:4,
                      name = c("Cristian", "Jean", "Hikimura", "Steven"),
                      id_cities = c(3, 1, 2, 5))

# import dplyr
library(dplyr)

# inner_join  
inner_join(cities, persons, by=c("id" = "id_cities"))
left_join(cities, persons, by=c("id" = "id_cities"))
right_join(persons, cities, by=c("id_cities" = "id"))

full_join(persons, cities, by=c("id_cities" = "id"))
full_join(cities, persons, by=c("id" = "id_cities"))