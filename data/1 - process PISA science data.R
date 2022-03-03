library(haven)
library(tidyverse)
library(janitor)
library(intsvy)

# 2006 ----

pisa_2006_data_student <- read_spss("data/PISA/2006/PISA 2006 student.sav")

pisa_2006_data <- pisa_2006_data_student %>%
  mutate(country = as_factor(CNT),
         gender = as_factor(ST04Q01),
         group = paste0(CNT, " ", gender))

remove(pisa_2006_data_student)
gc()

pisa_2006_total <- intsvy.mean.pv(pvnames = "SCIE",
                       by="CNT",
                       data=pisa_2006_data,
                       config=pisa_conf)

pisa_2006_gender <- intsvy.mean.pv(pvnames = "SCIE",
                                           by="group",
                                           data=pisa_2006_data,
                                           config=pisa_conf)


pisa_2006_gender <- pisa_2006_gender %>%
  separate(group, into=c("cnt", "gender")) %>%
  filter(gender != "NA")


pisa_2006_countries <- pisa_2006_data %>%
  select(CNT, country, OECD, CNTFAC) %>%
  distinct(CNT, country, OECD, CNTFAC) %>%
  clean_names() %>%
  mutate(oecd = as_factor(oecd)) %>%
  rename(country_weight = cntfac)

save(pisa_2006_total, pisa_2006_gender, pisa_2006_countries, file="data/final data/pisa_results.Rdata")

remove(pisa_2006_data, pisa_2006_data_student)

# 2009 ----

pisa_2009_data_student <- read_spss("data/PISA/2009/PISA 2009 student.sav")

pisa_2009_data <- pisa_2009_data_student %>%
  mutate(country = as_factor(CNT),
         gender = as_factor(ST04Q01),
         group = paste0(CNT, " ", gender))

remove(pisa_2009_data_student)
gc()

pisa_2009_total <- intsvy.mean.pv(pvnames = "SCIE",
                                  by="CNT",
                                  data=pisa_2009_data,
                                  config=pisa_conf)

pisa_2009_gender <- intsvy.mean.pv(pvnames = "SCIE",
                                   by="group",
                                   data=pisa_2009_data,
                                   config=pisa_conf)


pisa_2009_gender <- pisa_2009_gender %>%
  separate(group, into=c("cnt", "gender")) %>%
  filter(gender != "NA")


pisa_2009_countries <- pisa_2009_data %>%
  select(CNT, country, OECD, CNTFAC) %>%
  distinct(CNT, country, OECD, CNTFAC) %>%
  clean_names() %>%
  mutate(oecd = as_factor(oecd)) %>%
  rename(country_weight = cntfac)

save(pisa_2006_total, pisa_2006_gender, pisa_2006_countries,
     pisa_2009_total, pisa_2009_gender, pisa_2009_countries,
     file="data/final data/pisa_results.Rdata")

remove(pisa_2009_data)

# 2012

pisa_2012_data_student <- read_spss("data/PISA/2012/PISA 2012 student.sav")

pisa_2012_data <- pisa_2012_data_student %>%
  mutate(country = as_factor(CNT),
         gender = as_factor(ST04Q01),
         group = paste0(CNT, " ", gender))

remove(pisa_2012_data_student)
gc()

pisa_2012_total <- intsvy.mean.pv(pvnames = "SCIE",
                                  by="CNT",
                                  data=pisa_2012_data,
                                  config=pisa_conf)

pisa_2012_gender <- intsvy.mean.pv(pvnames = "SCIE",
                                   by="group",
                                   data=pisa_2012_data,
                                   config=pisa_conf)


pisa_2012_gender <- pisa_2012_gender %>%
  separate(group, into=c("cnt", "gender")) %>%
  filter(gender != "NA")


pisa_2012_countries <- pisa_2012_data %>%
  select(CNT, country, OECD, senwgt_STU) %>%
  distinct(CNT, country, OECD, senwgt_STU) %>%
  clean_names() %>%
  mutate(oecd = as_factor(oecd)) %>%
  rename(country_weight = senwgt_stu)

save(pisa_2006_total, pisa_2006_gender, pisa_2006_countries,
     pisa_2009_total, pisa_2009_gender, pisa_2009_countries,
     pisa_2012_total, pisa_2012_gender, pisa_2012_countries,
     file="data/final data/pisa_results.Rdata")

remove(pisa_2012_data)
gc()

#2015 ----

pisa_2015_data_student <- read_sav("data/PISA/2015/PISA 2015 student.sav")

pisa_2015_data <- pisa_2015_data_student %>%
  mutate(country = as_factor(CNT),
         gender = as_factor(ST004D01T),
         group = paste0(CNT, " ", gender))

remove(pisa_2015_data_student)
gc()


pisa_2015_total <- intsvy.mean.pv(pvnames = "SCIE",
                                  by="CNT",
                                  data=pisa_2015_data,
                                  config=pisa_conf)

pisa_2015_gender <- intsvy.mean.pv(pvnames = "SCIE",
                                   by="group",
                                   data=pisa_2015_data,
                                   config=pisa_conf)


pisa_2015_gender <- pisa_2015_gender %>%
  separate(group, into=c("cnt", "gender")) %>%
  filter(gender != "NA")


pisa_2015_countries <- pisa_2015_data %>%
  select(CNT, country, OECD, SENWT) %>%
  distinct(CNT, country, OECD, SENWT) %>%
  clean_names() %>%
  mutate(oecd = as_factor(oecd)) %>%
  rename(country_weight = senwt)

save(pisa_2006_total, pisa_2006_gender, pisa_2006_countries,
     pisa_2009_total, pisa_2009_gender, pisa_2009_countries,
     pisa_2012_total, pisa_2012_gender, pisa_2012_countries,
     pisa_2015_total, pisa_2015_gender, pisa_2015_countries,
     file="data/final data/pisa_results.Rdata")

remove(pisa_2015_data)
gc()

#2018 -----

pisa_2018_data_student <- read_sav("data/PISA/2018/PISA 2018 student.sav")

pisa_2018_data <- pisa_2018_data_student %>%
  mutate(country = as_factor(CNT),
         gender = as_factor(ST004D01T),
         group = paste0(CNT, " ", gender))

remove(pisa_2018_data_student)
gc()


pisa_2018_total <- intsvy.mean.pv(pvnames = "SCIE",
                                  by="CNT",
                                  data=pisa_2018_data,
                                  config=pisa_conf)

pisa_2018_gender <- intsvy.mean.pv(pvnames = "SCIE",
                                   by="group",
                                   data=pisa_2018_data,
                                   config=pisa_conf)


pisa_2018_gender <- pisa_2018_gender %>%
  separate(group, into=c("cnt", "gender")) %>%
  filter(gender != "NA")


pisa_2018_countries <- pisa_2018_data %>%
  select(CNT, country, OECD, SENWT) %>%
  distinct(CNT, country, OECD, SENWT) %>%
  clean_names() %>%
  mutate(oecd = as_factor(oecd)) %>%
  rename(country_weight = senwt)

save(pisa_2006_total, pisa_2006_gender, pisa_2006_countries,
     pisa_2009_total, pisa_2009_gender, pisa_2009_countries,
     pisa_2012_total, pisa_2012_gender, pisa_2012_countries,
     pisa_2015_total, pisa_2015_gender, pisa_2015_countries,
     pisa_2018_total, pisa_2018_gender, pisa_2018_countries,
     pisa_all,
     file="data/final data/pisa_results.Rdata")

remove(pisa_2018_data)
gc()

pisa_process_total <- function(x, year){
  x %>%
    select(1:3) %>%
    rename(n_total = Freq,
           mean_total = Mean) %>%
    mutate(year = year, .after=1)
}

pisa_total <- rbind(
pisa_process_total(pisa_2006_total, 2006),
pisa_process_total(pisa_2009_total, 2009),
pisa_process_total(pisa_2012_total, 2012),
pisa_process_total(pisa_2015_total, 2015),
pisa_process_total(pisa_2018_total, 2018)) %>%
  clean_names()


pisa_process_gender <- function(x, year){
x %>%
  select(1:4) %>%
  pivot_wider(names_from=gender, values_from = c(Freq, Mean)) %>%
  clean_names() %>%
    mutate(year = year, .after=1)
}

pisa_gender <- rbind(
  pisa_process_gender(pisa_2006_gender, 2006),
  pisa_process_gender(pisa_2009_gender, 2009),
  pisa_process_gender(pisa_2012_gender, 2012),
  pisa_process_gender(pisa_2015_gender, 2015),
  pisa_process_gender(pisa_2018_gender, 2018))

pisa_all <- pisa_total %>%
  left_join(pisa_gender, by=c("cnt", "year"))

save(pisa_2006_total, pisa_2006_gender, pisa_2006_countries,
     pisa_2009_total, pisa_2009_gender, pisa_2009_countries,
     pisa_2012_total, pisa_2012_gender, pisa_2012_countries,
     pisa_2015_total, pisa_2015_gender, pisa_2015_countries,
     pisa_2018_total, pisa_2018_gender, pisa_2018_countries,
     pisa_all,
     file="data/final data/pisa_results.Rdata")
