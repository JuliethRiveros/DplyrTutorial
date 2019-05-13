# Carga la base de datos de migraciones de la OECD (Organización para la Cooperación y el Desarrollo Económicos)
# Fuente: https://stats.oecd.org/viewhtml.aspx?datasetcode=MIG&lang=en
# Descripción: http://www.oecd.org/els/mig/Annex-EN.pdf
# This series of tables relates to stocks and flows of immigrants for the period 2006-16 or 2007-17 
# Variable B11: Emigraciones  desde pais (COU) por personas de nacionalidad (CO2)
# Variable B12: Inmigraciones hacia pais (COU) por personas de nacionalidad (CO2)
# Variable B14: Cantidad de personas extranjeras (CO2) que nacieron en el país (COU)
# Variable B13: Personas de origen CO2 que pidieron asilo en país COU
# Variable B15: Cantidad de personas de nacionalidad CO2 en el país COU
# Variable B16: Adquisición de nacionalidad COU por parte de personas de nacionalidad CO2
# Variable B23: Cantidad de personas que nacieron en el extranjero CO2 y que hacen parte de fuerza laboral en país COU
# Variable B24: Cantidad de personas de país CO2 y que hacen parte de fuerza laboral en país COU
migraciones <- read.csv("data/migrations.csv")

# funciones a usar: 
# visualizacion: head + str + plot + 
# resumenes de datos: summary + 
# basicas de r: max + var + tapply + subset + by + which + which.min + which.max

# dplyr: solamente funciona con data.frames
#     summarise + mutate + select + filter + arrange + 
#     group_by + join + slice +
#     top_n + tbl_df + distinct + n_distinct
# cargando la librería dplyr
library(dplyr)

# Preguntas:

# 1. ¿Cuales son los diferentes valores de la columna variable?
migraciones %>% 
  distinct(VAR, Variable)

# 2. ¿Cuales son las diversas nacionalidades que se tienen en cuenta?
migraciones %>% 
  distinct(CO2, Country.of.birth.nationality)

# 3. ¿Cuales son los diversos países de los cuales se tienen datos de flujos de migración?
migraciones %>% 
  distinct(COU, Country)

# 4. ¿Cuales son los años de los cuales se tienen datos?
migraciones %>% 
  distinct(YEA, Year)

# 5. ¿Cuales fueron las estadísticas de inmigración para Colombia en la base de datos en 2016?
migraciones %>%
  filter(COU == "COL") %>%
  filter(Year == 2016) %>%
  filter(VAR == "B11") %>%
  select(Country.of.birth.nationality, Value, Country) %>%
  arrange(desc(Value))
# No se tiene esta estadística.
# ¿Cuales fueron las estadísticas de inmigración de Colombianos en 2016 en el mundo?
migraciones %>%
  filter(Country.of.birth.nationality == "Colombia") %>%
  filter(Year == 2016) %>%
  filter(VAR == "B11") %>%
  select(Country.of.birth.nationality, Value, Country) %>%
  arrange(desc(Value))

# 6. ¿Cuales fueron las estadísticas de emigración de Colombianos en 2017 en el mundo?
migraciones %>%
  filter(Country.of.birth.nationality == "Colombia") %>%
  filter(Year == 2016) %>%
  filter(VAR == "B12") %>%
  select(Country.of.birth.nationality, Value, Country) %>%
  arrange(desc(Value))

# 7. ¿Cuales son los 6 países con más emigraciones durante el 2008?
# Estas son dos formas de hacerlo que en teoría deberían dar los mismos resultados,
# sin embargo aunque son parecidos no son iguales.
migraciones %>% 
  filter(Country.of.birth.nationality != "Total") %>%    # quitar los totales
  filter(VAR == "B12") %>%    # filtrar emigraciones
  filter(Year == 2008) %>%    # filtrar por 2008
  group_by(COU) %>%           # argupo por pais
  summarise(Pais = Country[1], Emigraciones = sum(Value)) %>% # resume las tablas agrupadas por pais en una sola tabla
  arrange(desc(Emigraciones)) %>% # organiza la tabla de forma descendente por Emigraciones
  top_n(6, Emigraciones)      # obtiene los 6 más

# 8. ¿Cuales son los 6 países con menos emigraciones durante el 2010?
# Estas son dos formas de hacerlo que en teoría deberían dar los mismos resultados,
# sin embargo aunque son parecidos no son iguales.
migraciones %>% 
  filter(CO2 != "TOT") %>%   # quitar los totales
  filter(VAR == "B12") %>%   # filtrar emigraciones
  filter(Year == 2010) %>%   # filtrar por 2010
  group_by(COU) %>%          # argupo por pais
  summarise(Pais = Country[1], Emigraciones = sum(Value)) %>%
  arrange(Emigraciones) %>% 
  top_n(-6)

migraciones %>% 
  filter(CO2 == "TOT") %>%   # quitar los totales
  filter(VAR == "B12") %>%   # filtrar emigraciones
  filter(Year == 2010) %>%   # filtrar por 2010
  select(Country, Value) %>%
  arrange(Value) %>% 
  top_n(-6)

# 9. ¿Cuales son las 5 nacionalidades de las personas que más emigraron en el mundo en el año 2016?
migraciones %>% 
  filter(CO2 != "TOT") %>%
  filter(Year == 2016) %>%
  filter(VAR == "B11") %>%
  group_by(CO2) %>%
  summarise(Nacionalidad = Country.of.birth.nationality[1], Emigraciones = sum(Value)) %>%
  arrange(desc(Emigraciones)) %>%
  top_n(5)

# 10. ¿Cuales son las 5 nacionalidades de las personas que más inmigraron en el mundo en el año 2016?
migraciones %>% 
  filter(CO2 != "TOT") %>%
  filter(Year == 2016) %>%
  filter(VAR == "B12") %>%
  group_by(CO2) %>%
  summarise(Nacionalidad = Country.of.birth.nationality[1], Inmigraciones = sum(Value)) %>%
  arrange(desc(Inmigraciones)) %>%
  top_n(5)

# 11. ¿Cuantos Colombianos nacieron en USA en el 2017?
migraciones %>% 
  filter(VAR == "B14") %>% 
  filter(CO2 == "COL") %>% 
  filter(COU == "USA") %>%
  filter(Year == 2017) %>%
  select(Country.of.birth.nationality, Country, Year, Value)

# 12. ¿Cuales son las nacionalidades originales de los bebÃ©s extranjeros que nacen en Alemania?
