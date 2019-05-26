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

# Question 1
# How many listings are there from the countrys
# with more emigrations in 2016
migraciones_filtradas %>% 
  filter(Year == 2016) %>%
  filter(VAR == "B11") %>%
  group_by(Country) %>%
  summarise(n=n(), Pais=Country[1], Hash=CountryHash[1]) %>%
  arrange(desc(n)) %>%
  top_n(1, n) %>%
  inner_join(arriendos_filtrados, by=c("Hash"="CountryHash"))

# Question 2: How many countrys are in both tables?
inner_join_arriendos_migraciones_2 %>%
  distinct(CountryHash) %>%
  tally()

# Question 3: Whats the country that appears the most
# in both tables joined?
inner_join_arriendos_migraciones_2 %>%
  group_by(Country.x) %>%
  summarise(n=n()) %>%
  arrange(desc(n)) %>%
  top_n(1, n)

# Question 4: Whats the host_name that appears the most
# in both tables joined?
inner_join_arriendos_migraciones_2 %>%
  group_by(host_name) %>%
  summarise(n=n()) %>%
  arrange(desc(n)) %>%
  top_n(1, n)

# Question 5: Whats the neighbourhood that appears the most
# in both tables joined?
inner_join_arriendos_migraciones_2 %>%
  group_by(neighbourhood) %>%
  summarise(n=n()) %>%
  arrange(desc(n)) %>%
  top_n(1, n)

# Question 6: Whats the price that appears the most
# in both tables joined?
inner_join_arriendos_migraciones_2 %>%
  group_by(price) %>%
  summarise(n=n()) %>%
  arrange(desc(n)) %>%
  top_n(1, n)

# Question 7: How many emmigrations had the host_location
# with more beds in 2015?
arriendos_filtrados %>%
  arrange(desc(beds)) %>%
  top_n(1, beds) %>%
  select(CountryHash) %>%
  distinct() %>%
  inner_join(
    migraciones_filtradas %>% filter(Year == 2015) %>% filter(VAR == "B11"), by=c("CountryHash"="CountryHash")
  )
  
# Question 8: How many immigrations had the country with more
# listings in airbnb amsterdam in 2014?
arriendos_filtrados %>%
  group_by(CountryHash) %>%
  summarise(n=n()) %>%
  top_n(1, n) %>%
  inner_join(
    migraciones_filtradas %>%
      filter(VAR == "B12") %>%
      filter(Year == 2014),
      by=c("CountryHash"="CountryHash")
  )

# Question 9: Whats the Var with more ocurrences in the joined table?
inner_join_arriendos_migraciones_2 %>%
  group_by(VAR) %>%
  summarise(n=n()) %>%
  arrange(desc(n)) %>%
  top_n(1, n)

# Question 10: Whats the Country of birth with more ocurrences in the joined table?
inner_join_arriendos_migraciones_2 %>%
  group_by(Country.of.birth.nationality) %>%
  summarise(n=n()) %>%
  arrange(desc(n)) %>%
  top_n(1, n)

