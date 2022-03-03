# prepare covariates

library(tidyverse)
library(janitor)
library(readxl)
load("data/final data/pisa_countries.Rdata")
load("data/final data/pisa_results.Rdata")

#time -----

all_years <- seq(2006,2020,1)
pisa_years <- seq(2006,2018,3)

# pisa countries



# co2 ----

co2 <- readxl::read_excel("data/co2/co2emissions.xlsx",
                          skip=1) %>%
  rename(year = 1) %>%
  pivot_longer(Afghanistan:Zimbabwe, names_to="country", values_to="co2") %>%
  filter(year >= 2006) %>%
  arrange(country, year) %>%
  mutate(co2 = ifelse(country == "Kosovo" & year == 2006, 3.9, co2),
         co2 = ifelse(country == "Kosovo" & year == 2007, 4.1, co2),
         country = case_when(
           str_detect(country, "Russia") ~ "Russia",
           str_detect(country, "United States") ~ "United States",
           TRUE~as.character(country)
         ))

# check missing data
co2 %>% filter(is.na(co2)) %>%
  filter(year %in% pisa_years) %>% distinct(country)

co2_countries <- co2 %>% distinct(country, .keep_all = T)

# check countries between two primary varibles
pisa_countries %>% left_join(co2_countries) %>% view()

# make final country list ----
all_countries <- pisa_countries %>% left_join(co2_countries) %>%
  drop_na(co2) %>%
  filter(country != "Liechtenstein") %>%
  select(cnt, country)


# world population ##########

pop_raw <- read_excel("data/Worldbank/world population.xls", skip=2) %>%
  janitor::clean_names() %>%
  select(country_name,x2006:x2019) %>%
  pivot_longer(x2006:x2019, names_to="year", values_to = "pop") %>%
  mutate(year = str_remove(year, "x"),
         year = parse_number(year),
         country = case_when(
           country_name == "Hong Kong SAR, China" ~ "Hong Kong",
           country_name == "Macao SAR, China" ~ "Macao",
           country_name == "Slovak Republic" ~ "Slovakia",
           country_name == "Korea, Rep." ~ "South Korea",
           country_name == "Kyrgyz Republic" ~ "Kyrgyzstan",
           country_name == "Russian Federation" ~ "Russia",
           TRUE ~ country_name
         ))

pop_raw %>%
  distinct(country, .keep_all = T) %>%
  right_join(all_countries, by=c("country" = "country")) %>% view()

pop_taiwan <- tribble(~country_name, ~year, ~pop,
                      #https://eng.stat.gov.tw/point.asp?index=9
                      "Taiwan", 2006, 22814616,
                      "Taiwan", 2007, 22901897,
                      "Taiwan", 2008, 22994262,
                      "Taiwan", 2009, 23069345,
                      "Taiwan", 2010, 23138381,
                      "Taiwan", 2011, 23180477,
                      "Taiwan", 2012, 23261747,
                      "Taiwan", 2013, 23344213,
                      "Taiwan", 2014, 23392036,
                      "Taiwan", 2015, 23461562,
                      "Taiwan", 2016, 23508362,
                      "Taiwan", 2017, 23552470,
                      "Taiwan", 2018, 23574274,
                      "Taiwan", 2019, 23591031) %>%
  mutate(country = country_name)

#check
rbind(pop_raw, pop_taiwan) %>%
  distinct(country_name, .keep_all = T) %>%
  right_join(all_countries, by=c("country" = "country")) %>% view()

pop <- rbind(pop_raw, pop_taiwan) %>%
  select(country, year, pop)


# GDP-PPP ##########

gdp_ppp_raw <- read_excel("data/Worldbank/gdp ppp per capita 2015 us.xls", skip=2) %>%
  janitor::clean_names() %>%
  select(country_name,x2006:x2019) %>%
  pivot_longer(x2006:x2019, names_to="year", values_to = "gdp") %>%
  mutate(year = str_remove(year, "x"),
         year = parse_number(year),
         country = case_when(
           country_name == "Hong Kong SAR, China" ~ "Hong Kong",
           country_name == "Macao SAR, China" ~ "Macao",
           country_name == "Slovak Republic" ~ "Slovakia",
           country_name == "Korea, Rep." ~ "South Korea",
           country_name == "Kyrgyz Republic" ~ "Kyrgyzstan",
           country_name == "Russian Federation" ~ "Russia",
           TRUE ~ country_name
         ))

all_countries %>% left_join(gdp_ppp_raw) %>%
  filter(is.na(gdp))

gdp_taiwan <- tribble(~country_name, ~year, ~gdp,
                      "Taiwan", 2006, 16934,
                      "Taiwan", 2007, 17757,
                      "Taiwan", 2008, 18081,
                      "Taiwan", 2009, 16931,
                      "Taiwan", 2010, 19197,
                      "Taiwan", 2011, 20866,
                      "Taiwan", 2012, 21295,
                      "Taiwan", 2013, 21973,
                      "Taiwan", 2014, 22874,
                      "Taiwan", 2015, 22780,
                      "Taiwan", 2016, 23091,
                      "Taiwan", 2017, 25080,
                      "Taiwan", 2018, 25838,
                      "Taiwan", 2019, 25908) %>%
  mutate(country = country_name)

#check
rbind(gdp_ppp_raw, gdp_taiwan) %>%
  distinct(country_name, .keep_all = T) %>%
  right_join(all_countries, by=c("country" = "country")) %>% view()

gdp_ppp <- rbind(gdp_ppp_raw, gdp_taiwan) %>%
  filter(country %in% all_countries$country) %>%
  drop_na(gdp) %>%
  select(country, year, gdp)

# GDP as a % of Exports ##########

gdp_exports_raw <- read_csv("data/Worldbank/gdp as pct of exports.csv",
                            skip=4) %>%
  janitor::clean_names() %>%
  select(country_name,x2006:x2019) %>%
  pivot_longer(x2006:x2019, names_to="year", values_to = "exports") %>%
  mutate(year = str_remove(year, "x"),
         year = parse_number(year),
         country = case_when(
           country_name == "Hong Kong SAR, China" ~ "Hong Kong",
           country_name == "Macao SAR, China" ~ "Macao",
           country_name == "Slovak Republic" ~ "Slovakia",
           country_name == "Korea, Rep." ~ "South Korea",
           country_name == "Kyrgyz Republic" ~ "Kyrgyzstan",
           country_name == "Russian Federation" ~ "Russia",
           TRUE ~ country_name
         ))

# name check

gdp_exports_raw %>%
  distinct(country, .keep_all = T) %>%
  right_join(all_countries, by=c("country" = "country")) %>% view()
# missing Taiwan

gdp_exports_raw %>%
  filter(country %in% all_countries$country) %>%
  filter(year %in% pisa_years) %>%
  filter(is.na(exports)) %>%
  distinct(country)

# missing Trinidad and Tobago
gdp_exports <- gdp_exports_raw %>%
  select(country, year, exports)

# Manufacturing as a % of Exports ##########

gdp_mfg_raw <- read_excel("data/Worldbank/mfg as pct of gdp.xls",
                              sheet="Data",
                            skip=3) %>%
  janitor::clean_names() %>%
  select(country_name,x2006:x2019) %>%
  pivot_longer(x2006:x2019, names_to="year", values_to = "mfg") %>%
  mutate(year = str_remove(year, "x"),
         year = parse_number(year),
         country = case_when(
           country_name == "Hong Kong SAR, China" ~ "Hong Kong",
           country_name == "Macao SAR, China" ~ "Macao",
           country_name == "Slovak Republic" ~ "Slovakia",
           country_name == "Korea, Rep." ~ "South Korea",
           country_name == "Kyrgyz Republic" ~ "Kyrgyzstan",
           country_name == "Russian Federation" ~ "Russia",
           TRUE ~ country_name
         ))

# name and missing check

gdp_mfg_raw %>%
  distinct(country, .keep_all = T) %>%
  right_join(all_countries, by=c("country" = "country")) %>% view()


gdp_mfg_raw %>%
  filter(country %in% all_countries$country) %>%
  filter(year %in% pisa_years) %>%
  filter(is.na(mfg)) %>%
  distinct(country)
# missing Bulgaria, T&T, Kosovo, Taiwan

gdp_mfg <- gdp_mfg_raw %>%
  select(country, year, mfg)

# GINI ##########

load("data/swiid9_2/swiid9_2.rda")

gini <- swiid_summary %>%
  mutate(  country = case_when(
    country == "Korea" ~ "South Korea",
    country == "Brunei" ~ "Brunei Darussalam",
    TRUE ~ country
  )) %>%
  # distinct(country, .keep_all = T) %>%
  # right_join(all_countries) %>% view()
  filter(country %in% all_countries$country) %>%
  filter(between(year, 2006, 2018)) %>%
  select(country, year, gini_disp) %>%
  # add in Macao and set to same as China
  add_row(
    swiid_summary %>%
      select(country, year, gini_disp) %>%
      filter(between(year, 2006, 2018)) %>%
      filter(country == "China") %>%
      mutate(country = "Macao")) %>%
  rename(gini = gini_disp)

# polity and corruption #########

vdem_raw <- read_rds("data/Polity/vdem/V-Dem-CY-Core-v11.1.rds")

vdem <- vdem_raw %>%
  select(country_name, year, v2x_polyarchy, v2x_corr) %>%
  filter(between(year, 2006, 2018)) %>%
  rename("country" = country_name) %>%
  mutate(country = case_when(
    str_detect(country, "United States") ~ "United States",
               TRUE ~ as.character(country))
  ) %>%
  rename(democracy = v2x_polyarchy,
         corruption = v2x_corr)

vdem %>% right_join(all_countries) %>%
  filter(is.na(democracy))

# Net Enrollment / Coverage ##########

# 2006 ----
## from https://www.oecd.org/education/school/programmeforinternationalstudentassessmentpisa/pisa2006results.htm
## https://www.oecd.org/pisa/pisaproducts/39705422.xls

ci3_2006 <- read_excel("data/PISA/Coverage Index 3/PISA Tables 2006.xls",
                       sheet="TA2.1", skip=5) %>%
  select(1, `Coverage Index 3: Coverage of 15-year-old population`) %>%
  rename("cnt" = 1,
         "coverage_index_3" = 2) %>%
  drop_na(coverage_index_3) %>%
  mutate(year = 2006, .before=1) %>%
  mutate(country = case_when(
    cnt == "Korea" ~ "South Korea",
    cnt == "Slovak Republic" ~ "Slovakia",
    cnt == "Chinese Taipei" ~ "Taiwan",
    cnt == "Hong Kong-China" ~ "Hong Kong",
    cnt == "Russian Federation" ~ "Russia",
    cnt == "Macao-China" ~ "Macao",
    TRUE~cnt
  )) %>%
  select(country, year, coverage_index_3)

ci3_2006 %>% anti_join(all_countries, by=c("country"="country"))


# 2009 ----
## from https://www.oecd-ilibrary.org/education/pisa-2009-results-what-students-know-and-can-do_9789264091450-en
## https://www.oecd-ilibrary.org/education/pisa-2009-results-what-students-know-and-can-do/pisa-target-populations-and-samples_9789264091450-table45-en

ci3_2009 <- read_excel("data/PISA/Coverage Index 3/PISA Tables 2009.xls",
                       sheet="TAB A2.1", skip=8) %>%
  select(1, `Coverage Index 3: Coverage of 15-year-old population`) %>%
  rename("cnt" = 1,
         "coverage_index_3" = 2) %>%
  drop_na(coverage_index_3) %>%
  slice(-1) %>%
  mutate(year = 2009, .before=1) %>%
  mutate(country = case_when(
    cnt == "Korea" ~ "South Korea",
    cnt == "Slovak Republic" ~ "Slovakia",
    cnt == "Russian Federation" ~ "Russia",
    cnt == "Chinese Taipei" ~ "Taiwan",
    cnt == "Hong Kong-China" ~ "Hong Kong",
    cnt == "Macao-China" ~ "Macao",
    cnt == "Dubai (UAE)" ~ "United Arab Emirates",
    cnt == "Shanghai-China" ~ "China",
    TRUE~cnt
  )) %>%
  select(country, year, coverage_index_3)

ci3_2009 %>% anti_join(all_countries, by=c("country"="country"))

# 2012 ----
## from https://www.oecd.org/pisa/keyfindings/pisa-2012-results-volume-i.htm - extracted from PDF

ci3_2012 <- read_excel("data/PISA/Coverage Index 3/PISA Tables 2012.xlsx", skip=3) %>%
  select(4,13) %>%
  rename("cnt" = 1,
         "coverage_index_3" = 2) %>%
  slice(-c(1:3)) %>%
  drop_na(cnt) %>%
  drop_na(coverage_index_3) %>%
  mutate(cnt = str_to_title(cnt)) %>%
  mutate(year = 2012, .before=1) %>%
  mutate(country = case_when(
    cnt == "Korea" ~ "South Korea",
    cnt == "Slovak Republic" ~ "Slovakia",
    cnt == "Hong Kong-China" ~ "Hong Kong",
    cnt == "Viet Nam" ~ "Vietnam",
    cnt == "Russian Federation" ~ "Russia",
    TRUE~cnt
  )) %>%
  select(country, year, coverage_index_3)

ci3_2012 %>% anti_join(all_countries, by=c("country"="country"))

# 2015
## extracted from PDF
ci3_2015 <- read_excel("data/PISA/Coverage Index 3/PISA Tables 2015.xlsx", skip=3) %>%
  select(4,23) %>%
  rename("cnt" = 1,
         "coverage_index_3" = 2) %>%
  drop_na(cnt) %>%
  drop_na(coverage_index_3) %>%
  mutate(year = 2015, .before=1) %>%
  mutate(country = case_when(
    cnt == "Korea" ~ "South Korea",
    cnt == "Slovak Republic" ~ "Slovakia",
    cnt == "Chinese Taipei" ~ "Taiwan",
    cnt == "Hong Kong (China)" ~ "Hong Kong",
    cnt == "Macao (China)" ~ "Macao",
    cnt == "Dubai (UAE)" ~ "United Arab Emirates",
    cnt == "Shanghai-China" ~ "China",
    cnt == "Russian Federation" ~ "Russia",
    cnt == "Viet Nam" ~ "Vietnam",
    cnt == "Cyprus*" ~ "Cyprus",
    TRUE~cnt
  ),
  country = ifelse(str_detect(country, "China"), "China", country)) %>%
  select(country, year, coverage_index_3)

ci3_2015 %>% anti_join(all_countries, by=c("country"="country"))

# 2018 ----
## extracted from PDF
ci3_2018 <- read_excel("data/PISA/Coverage Index 3/PISA Tables 2018.xlsx", skip=3) %>%
  select(2,3,11) %>%
  rename("cnt1" = 1,
         "cnt2" = 2,
         "coverage_index_3" = 3) %>%
  mutate(cnt = ifelse(is.na(cnt1), cnt2, cnt1), .before=1) %>%
  select(1,4) %>%
  drop_na(cnt) %>%
  drop_na(coverage_index_3) %>%
  mutate(year = 2018, .before=1)%>%
  mutate(country = case_when(
    cnt == "Korea" ~ "South Korea",
    cnt == "Slovak Republic" ~ "Slovakia",
    cnt == "Russian Federation" ~ "Russia",
    TRUE~cnt
  )) %>%
  select(country, year, coverage_index_3)

ci3_2018 %>% anti_join(all_countries, by=c("country"="country"))

# combine ----
coverage_index <- rbind(ci3_2006, ci3_2009, ci3_2012, ci3_2015, ci3_2018)

# coverage index alt - net enrollment ----
# https://data.worldbank.org/indicator/SE.SEC.NENR?view=chart

netedu_raw <- read_excel("data/Worldbank/net enrollment.xls",
                         skip=2) %>%
  janitor::clean_names() %>%
  select(country_name,x2006:x2019) %>%
  pivot_longer(x2006:x2019, names_to="year", values_to = "net_enrollment") %>%
  mutate(year = str_remove(year, "x"),
         year = parse_number(year),
         country = case_when(
           country_name == "Hong Kong SAR, China" ~ "Hong Kong",
           country_name == "Macao SAR, China" ~ "Macao",
           country_name == "Slovak Republic" ~ "Slovakia",
           country_name == "Korea, Rep." ~ "South Korea",
           country_name == "Kyrgyz Republic" ~ "Kyrgyzstan",
           country_name == "Russian Federation" ~ "Russia",
           TRUE ~ country_name
         ))

# name check

netedu_raw %>%
  distinct(country, .keep_all = T) %>%
  right_join(all_countries, by=c("country" = "country")) %>% view()

netedu <- netedu_raw %>%
  select(country, year, net_enrollment)

netedu_combined <- netedu %>% left_join(coverage_index,
                                        by = c("country", "year")) %>%
  filter(country %in% all_countries$country) %>%
  filter(year %in% pisa_years) %>%
  mutate(net_enrollment = net_enrollment/100,
         coverage_index_3 = parse_number(coverage_index_3))

# correlation between two
# rmcorr::rmcorr(participant = country, measure1 = coverage_index_3,
#                       measure2 = net_enrollment, dataset = netedu_combined,
#                nreps = 100)

netedu_combined %>%
  group_by(year) %>%
  nest() %>%
  mutate(cor = map(data, ~cor(.x$net_enrollment, .x$coverage_index_3,
                              use = "pairwise.complete.obs"))) %>%
  unnest(cols=c(data, cor)) %>%
  ungroup() %>%
  summarize(mean = mean(cor, na.rm=T))

finalfit::missing_glimpse(netedu_combined)

netedu_corrected <- netedu_combined %>%
  mutate(net_enrollment_corrected = ifelse(is.na(coverage_index_3),
                                           net_enrollment,
                                           coverage_index_3))

finalfit::missing_glimpse(netedu_corrected)


# oecd ##########

oecd <- all_countries %>%
  left_join(pisa_oecd) %>%
  mutate(oecd = case_when(
    oecd == "No" ~ "Non-OECD",
    oecd == "Yes" ~ "OECD",
    TRUE~ as.character(oecd)
  )) %>%
  distinct(cnt, year, .keep_all=T) %>%
  select(-cnt)

# left-right ##########
# see file 3.5 - left-right scale.R
load(file="data/final data/left_right.Rdata")

# individualism - collectivism ##########

culture_raw <- read_csv("data/Hofstede/BEUGELSDIJK.csv") %>%
  janitor::clean_names()

culture_raw %>%
  select(country, collectivism_individualism) %>%
  mutate(country = case_when(
    country == "Bosnia" ~ "Bosnia and Herzegovina",
    country == "Macedonia" ~ "North Macedonia",
    country == "Great Britain" ~ "United Kingdom",
    TRUE ~ country
  )) %>%
  add_row(
    tribble(~country, ~collectivism_individualism,
            "Costa Rica", 15, #hofstede
            "Israel", 54, #hofstede
            "Luxembourg", 60, #hofstede
            "Macao", 29.6, # based on China
            "Panama", 11) #hofstede
  ) -> culture

# regions ##########

regions_raw <- read_excel("data/2018-jme-regional-classifications.xlsx") %>%
  clean_names() %>%
  select(country, sdg_region, un_sub_region, iso_code) %>%
  mutate(country = case_when(
    country == "Czechia" ~ "Czech Republic",
    country == "Republic of Korea" ~ "South Korea",
    country == "Russian Federation" ~ "Russia",
    country == "Republic of Moldova" ~ "Moldova",
    country == "Viet Nam" ~ "Vietnam",
    TRUE ~ country
  ),
    sdg_region = case_when(
      country == "Anguilla" ~ "Latin America and the Caribbean",
      TRUE ~ sdg_region)) %>%
  add_row(
    tribble(~country, ~sdg_region, ~un_sub_region, ~iso_code,
            "Taiwan", "Eastern Asia and South-eastern Asia",
            "Eastern Asia", "1",
            "Hong Kong", "Eastern Asia and South-eastern Asia",
            "Eastern Asia","1",
            "Macao", "Eastern Asia and South-eastern Asia",
            "Eastern Asia","1",
            "Kosovo", "Northern America and Europe", "Southern Europe","1")
  )

regions_raw %>% right_join(all_countries) %>% view()

regions <- regions_raw %>% select(-iso_code)

# combine covariates ##########

pisa <- pisa_all %>%
  left_join(all_countries) %>%
  select(-cnt) %>%
  relocate(country, .before=1)

all_data <-
expand_grid(country=unique(all_countries$country),
            all_years) %>%
  rename(year = all_years) %>%
  left_join(co2) %>%
  left_join(pisa) %>%
  left_join(pop) %>%
  left_join(gdp_ppp) %>%
  left_join(gdp_exports) %>%
  left_join(gdp_mfg) %>%
  left_join(gini) %>%
  left_join(left_right) %>%
  left_join(vdem) %>%
  left_join(netedu_corrected) %>%
  left_join(oecd) %>%
  left_join(culture) %>%
  left_join(regions) %>%
  # transformations and scalings ----
    #co2 lag
  mutate(
    co2_lag = lead(co2, 1),
    co2_lag2 = lead(co2, 2),
    co2_lag3 = lead(co2, 3),
    .after=3
  ) %>%
    # time
  mutate(time = case_when(
    year == 2006 ~ 0,
    year == 2009 ~ 1,
    year == 2012 ~ 2,
    year == 2015 ~ 3,
    year == 2018 ~ 4
  ),
    #scalings
  pop = pop/10000000,
  gdp = gdp/10000,
  exports = exports/100,
  mfg = mfg/100,
  gini = gini/100,
  collectivism_individualism = collectivism_individualism/100) %>%
  # fix missing oecd data
  group_by(country) %>%
  fill(oecd, .direction = "downup") %>%
  ungroup() %>%
  filter(year %in% pisa_years) %>%
  # centering variables
  group_by(country) %>%
  #rowwise() %>%
    #between
  mutate(sl_lvl2 = mean(mean_total, na.rm=T)-500,
         sl_lvl2_female = mean(mean_female, na.rm=T)-500,
         sl_lvl2_male = mean(mean_male, na.rm=T)-500) %>%
    # within
  ungroup() %>%
  rowwise() %>%
  mutate(sl_lvl1 = mean_total-sl_lvl2,
         sl_lvl1_female = mean_female-sl_lvl2_female,
         sl_lvl1_male = mean_male-sl_lvl2_male) %>% # does this look right?
  #mean centering
  ungroup() %>%
  mutate_at(vars("pop", "gdp", "exports", "mfg", "gini", "left_right", "left_right_scaled", "left_right_fa", "left_right_fa_scaled",
                   "democracy", "corruption", "net_enrollment_corrected",
                   "collectivism_individualism"),
            .funs=list(c = ~.x-mean(.x, na.rm=T))) %>%
  rename(sl_total = mean_total,
         sl_female = mean_female,
         sl_male = mean_male)


cor(all_data$co2_lag, all_data$left_right_c,
    use="pairwise.complete.obs")
cor(all_data$co2_lag, all_data$left_right_scaled,
    use="pairwise.complete.obs")

all_data %>%
  ggplot(aes(x=left_right_fa_c, y=co2_lag))+
  geom_point(aes(color=oecd))

#check for missing data

all_data %>% filter(left_right_fa_c < -4)

missing_ins <- function(x){
  all_data %>%
    select(country, year, !! sym(x)) %>%
    filter(is.na(!! sym(x)))
}

missing_ins("co2")
missing_ins("sl_total")
missing_ins("mean_female")
missing_ins("mean_male")
missing_ins("pop")
missing_ins("gdp")
missing_ins("exports")
missing_ins("mfg")
missing_ins("gini")
missing_ins("left_right_fa") %>% view()
missing_ins("democracy")
missing_ins("corruption")
missing_ins("net_enrollment_corrected")
missing_ins("oecd")
missing_ins("collectivism_individualism")
missing_ins("sdg_region")

save(all_data, file="data/final data/final_data.Rdata")

