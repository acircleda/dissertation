# get list of all PISA countries

library(tidyverse)

load(file="data/final data/pisa_results.Rdata")

pisa_countries <- rbind(
labelled::remove_labels(pisa_2006_countries) %>% select(cnt, country),
labelled::remove_labels(pisa_2009_countries) %>% select(cnt, country),
labelled::remove_labels(pisa_2012_countries) %>% select(cnt, country),
labelled::remove_labels(pisa_2015_countries) %>% select(cnt, country),
labelled::remove_labels(pisa_2018_countries) %>% select(cnt, country)) %>%
  distinct(cnt, .keep_all = T) %>%
  # fix names
  mutate(country =
           case_when(
             country == "Chinese Taipei" ~ "Taiwan",
             country == "Hong Kong-China" ~ "Hong Kong",
             country == "Russian Federation" ~ "Russia",
             country == "Slovak Republic" ~ "Slovakia",
             country == "Shanghai-China" ~ "China",
             str_detect(country, "Moldova") ~ "Moldova",
             country == "Viet Nam" ~ "Vietnam",
             country == "Macao-China" ~ "Macao",
             str_detect(country, "(China)") ~ "China",
             str_detect(country, "Argentina") ~ "Argentina",
             str_detect(country, "Baku") ~ "Azerbaijan",
             str_detect(country, "Korea") ~ "South Korea",
             TRUE ~ as.character(country)
           ))



participation <- pisa_countries %>%
  mutate(x2006 = ifelse(cnt %in% pisa_2006_countries$cnt, 1, 0),
         x2009 = ifelse(cnt %in% pisa_2009_countries$cnt, 1, 0),
         x2012 = ifelse(cnt %in% pisa_2012_countries$cnt, 1, 0),
         x2015 = ifelse(cnt %in% pisa_2015_countries$cnt, 1, 0),
         x2018 = ifelse(cnt %in% pisa_2018_countries$cnt, 1, 0)) %>%
  rowwise() %>%
  mutate(participation = sum(x2006+x2009+x2012+x2015+x2018))

# OECD status
pisa_oecd <- rbind(
  labelled::remove_labels(pisa_2006_countries) %>% select(cnt, oecd) %>%
    mutate(year = 2006, .after=1),
  labelled::remove_labels(pisa_2009_countries) %>% select(cnt, oecd) %>%
    mutate(year = 2009, .after=1),
  labelled::remove_labels(pisa_2012_countries) %>% select(cnt, oecd) %>%
    mutate(year = 2012, .after=1),
  labelled::remove_labels(pisa_2015_countries) %>% select(cnt, oecd) %>%
    mutate(year = 2015, .after=1),
  labelled::remove_labels(pisa_2018_countries) %>% select(cnt, oecd) %>%
    mutate(year = 2018, .after=1))

save(pisa_countries, pisa_oecd, file="data/final data/pisa_countries.Rdata")
