# Carga la base de datos de migraciones de la OECD (Organizaci�n para la Cooperaci�n y el Desarrollo Econ�micos)
# Fuente: https://stats.oecd.org/viewhtml.aspx?datasetcode=MIG&lang=en
# Descripci�n: http://www.oecd.org/els/mig/Annex-EN.pdf
# This series of tables relates to stocks and flows of immigrants for the period 2006-16 or 2007-17 
# Variable B11: Emigraciones  desde pais (COU) por personas de nacionalidad (CO2)
# Variable B12: Inmigraciones hacia pais (COU) por personas de nacionalidad (CO2)
# Variable B14: Cantidad de personas extranjeras (CO2) que nacieron en el pa�s (COU)
# Variable B13: Personas de origen CO2 que pidieron asilo en pa�s COU
# Variable B15: Cantidad de personas de nacionalidad CO2 en el pa�s COU
# Variable B16: Adquisici�n de nacionalidad COU por parte de personas de nacionalidad CO2
# Variable B23: Cantidad de personas que nacieron en el extranjero CO2 y que hacen parte de fuerza laboral en pa�s COU
# Variable B24: Cantidad de personas de pa�s CO2 y que hacen parte de fuerza laboral en pa�s COU
migraciones <- read.csv("data/migrations.csv")

# funciones a usar: 
# visualizacion: head + str + plot + 
# resumenes de datos: summary + 
# basicas de r: max + var + tapply + subset + by + which + which.min + which.max

# dplyr: solamente funciona con data.frames
#     summarise + mutate + select + filter + arrange + 
#     group_by + join + slice +
#     top_n + tbl_df + distinct + n_distinct
# cargando la librer�a dplyr
library(dplyr)

# Preguntas:

# 1. �Cuales son los diferentes valores de la columna variable?
migraciones %>% 
  distinct(VAR, Variable)

# 2. �Cuales son las diversas nacionalidades que se tienen en cuenta?
migraciones %>% 
  distinct(CO2, Country.of.birth.nationality)

# 3. �Cuales son los diversos pa�ses de los cuales se tienen datos de flujos de migraci�n?
migraciones %>% 
  distinct(COU, Country)

# 4. �Cuales son los a�os de los cuales se tienen datos?
migraciones %>% 
  distinct(YEA, Year)

# 5. �Cuales fueron las estad�sticas de inmigraci�n para Colombia en la base de datos en 2016?
migraciones %>%
  filter(COU == "COL") %>%
  filter(Year == 2016) %>%
  filter(VAR == "B11") %>%
  select(Country.of.birth.nationality, Value, Country) %>%
  arrange(desc(Value))
# No se tiene esta estad�stica.
# �Cuales fueron las estad�sticas de inmigraci�n de Colombianos en 2016 en el mundo?
migraciones %>%
  filter(Country.of.birth.nationality == "Colombia") %>%
  filter(Year == 2016) %>%
  filter(VAR == "B11") %>%
  select(Country.of.birth.nationality, Value, Country) %>%
  arrange(desc(Value))

# 6. �Cuales fueron las estad�sticas de emigraci�n de Colombianos en 2017 en el mundo?
migraciones %>%
  filter(Country.of.birth.nationality == "Colombia") %>%
  filter(Year == 2016) %>%
  filter(VAR == "B12") %>%
  select(Country.of.birth.nationality, Value, Country) %>%
  arrange(desc(Value))

# 7. �Cuales son los 6 pa�ses con m�s emigraciones durante el 2008?
migraciones %>% 
  filter(Country.of.birth.nationality != "Total") %>%    # quitar los totales
  filter(VAR == "B12") %>%    # filtrar emigraciones
  filter(Year == 2008) %>%    # filtrar por 2008
  group_by(COU) %>%           # argupo por pais
  summarise(Pais = Country[1], Emigraciones = sum(Value)) %>% # resume las tablas agrupadas por pais en una sola tabla
  arrange(desc(Emigraciones)) %>% # organiza la tabla de forma descendente por Emigraciones
  top_n(6, Emigraciones)      # obtiene los 6 m�s

# 8. �Cuales son los 6 pa�ses con menos emigraciones durante el 2010?
head(migraciones %>% 
  filter(COU != "TOT") %>%
  filter(VAR == "B12") %>%
  filter(Year == 2010) %>%
  group_by(COU) %>%
  summarise(Pais = Country[1], Emigraciones = sum(Value)) %>%
  arrange(Emigraciones), 6)

# 9. �Cuales son las 5 nacionalidades de las personas que m�s emigraron en el mundo en el a�o 2016?
migraciones %>% 
  filter(Country.of.birth.nationality != "Total") %>%
  filter(Year == 2016) %>%
  filter(VAR == "B12") %>%
  group_by(Country.of.birth.nationality) %>%
  summarise(Nacionalidad = Country.of.birth.nationality[1], Emigraciones = sum(Value)) %>%
  arrange(desc(Emigraciones)) %>%
  top_n(5)

# 10. �Cuales son las 5 nacionalidades de las personas que m�s inmigraron en el mundo en el a�o 2016?
migraciones %>% 
  filter(Country.of.birth.nationality != "Total") %>%
  filter(Year == 2016) %>%
  filter(VAR == "B11") %>%
  group_by(Country.of.birth.nationality) %>%
  summarise(Nacionalidad = Country.of.birth.nationality[1], Inmigraciones = sum(Value)) %>%
  arrange(desc(Inmigraciones)) %>%
  top_n(5)

# 11. �Cuantos Colombianos nacieron en USA en el 2017?
migraciones %>% 
  filter(VAR == "B14") %>% 
  filter(Country.of.birth.nationality == "Colombia") %>% 
  filter(COU == "USA") %>%
  filter(Year == 2017) %>%
  select(Country.of.birth.nationality, Country, Year, Value)

# 12. �Cuales son las nacionalidades originales de los beb�s extranjeros que nacen en Alemania en 2015?
migraciones %>%
  filter(VAR =="B14") %>%
  filter(Country.of.birth.nationality != "Total") %>%
  filter(Country == "Germany") %>%
  filter(Year == 2015) %>%
  select(Country.of.birth.nationality,Year,Value) %>%
  arrange(desc(Value))