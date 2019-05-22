# loading both tables
arriendos <- read.csv("data/amsterdam_airbnb.csv")
migraciones <- read.csv("data/migrations.csv")

# loading dplyr library
library(dplyr)
library(digest)

#################################
# Second try joining tables
#################################

# We will join both tables using a hash of the countries,
# to work with numeric values:
# in arriendos: the host_location column
# in migraciones: the country column

extractCountry <- function(l) {
  l <- sapply(l, as.character, USE.NAMES = FALSE)
  l <- sapply(l, strsplit, ",", USE.NAMES = FALSE)
  l <- sapply(l, tail, n = 1L, USE.NAMES = FALSE)
  l <- sapply(l, trimws, USE.NAMES = FALSE)
  return(l)
}

countryToHash <- function(c) {
  c <- sapply(c, as.character)
  c <- sapply(c, digest, algo="xxhash32")
  c <- sapply(c, strtoi, 16)
  return(c)
}

arriendos_filtrados <- arriendos %>%
  mutate(Country = extractCountry(host_location)) %>%
  mutate(CountryHash = countryToHash(Country)) %>%
  filter(!is.na(CountryHash)) %>%
  filter(!is.na(Country))

# Now we have to transform the migrations database
# to transform the column Country to a Hash list
migraciones_filtradas <- migraciones %>%
  mutate(CountryHash = countryToHash(Country.of.birth.nationality)) %>%
  filter(!is.na(CountryHash))

# trying to join the databases
inner_join_arriendos_migraciones_2 <- migraciones_filtradas %>% 
  inner_join(arriendos_filtrados, by=c("CountryHash" = "CountryHash"))
