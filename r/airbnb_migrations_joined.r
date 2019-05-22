# loading both tables
arriendos <- read.csv("data/amsterdam_airbnb.csv")
migraciones <- read.csv("data/migrations.csv")

# loading dplyr library
library(dplyr)

#################################
# First try joining tables
#################################

# We will join both tables using:
# in arriendos: the host_location column
# in migraciones: the country column

# First we have to transform the host_location
# to refer to the Country. The column host_location has
# the following format: city, state, country
# We only need the Country, so we need to do a mutation
# We will create a table Countries from arriendos
extractCountry <- function(l) {
  l <- sapply(l, as.character, USE.NAMES = FALSE)
  l <- sapply(l, strsplit, ",", USE.NAMES = FALSE)
  l <- sapply(l, tail, n = 1L, USE.NAMES = FALSE)
  l <- sapply(l, trimws, USE.NAMES = FALSE)
  return(l)
}

arriendos_filtrados1 <- arriendos %>%
  mutate(Country = extractCountry(host_location)) %>%
  select(host_name, Country)

inner_join_arriendos_migraciones <- arriendos_filtrados1 %>%
  inner_join(migraciones, by=c("Country"="Country.of.birth.nationality"))

migraciones_filtradas <- migraciones %>%
  mutate(CountryList = as.list(sapply(migraciones$Country.of.birth.nationality, as.character)))

inner_join_arriendos_migraciones_1 <- arriendos_filtrados1 %>%
  inner_join(migraciones_filtradas, by=c("Country"="CountryList"))

