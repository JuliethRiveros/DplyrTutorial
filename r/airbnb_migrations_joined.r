# loading both tables
arriendos <- read.csv("data/amsterdam_airbnb.csv")
migraciones <- read.csv("data/migrations.csv")

# loading dplyr library
library(dplyr)
library(digest)

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
countryToHash <- function(c) {
  c <- sapply(c, digest, algo="xxhash32")
  c <- sapply(c, strtoi, 16)
  return(c)
}
arriendos_filtrados <- arriendos %>%
  mutate(Country = extractCountry(host_location)) %>%
  mutate(CountryHash = countryToHash(Country)) %>%
  filter(!is.na(CountryHash)) %>%
  filter(!is.na(Country)) %>%
  select(host_name, Country, CountryHash)

# Now we have to transform the migrations database
# to transform the column Country to a Hash list
migraciones_filtradas <- migraciones %>%
  filter(Year == 2017) %>%
  mutate(CountryHash = countryToHash(Country)) %>%
  filter(!is.na(CountryHash)) %>%
  filter(!is.na(Country)) %>%
  select(Country, CountryHash)

# joining the databases
inner_join_arriendos_migraciones <- migraciones_filtradas %>% 
  inner_join(arriendos_filtrados, by=c("CountryHash" = "CountryHash"))