library(tidyverse)
library(janitor)
library(readxl)
library(psych)
library(flextable)



# load data =====
# data: https://www.worldvaluessurvey.org/WVSDocumentationWVL.jsp

# wvs data -----

load("data/WVS/WVS_TimeSeries_1981_2020_R_v2_0.rdata")

wvs <- WVS_TimeSeries_1981_2020_v2_0. %>%
  select(S001,  #study
         S002VS, #wave
         S020, #year
         S003, #country code
         S007, #respondent number
         E033, #left-right 1=left, 10=right, <0 = drop
         F118, #Justifiable: Homosexuality, 1=never, 10=always, needs reverse coding
         F120, #Justifiable: Abortion, same as above, needs reverse coding
         C001, #Jobs scarce: Men should have more right to a job than women,
              # 1 = agree, 2= disagree, 3= neither
         E143,# Immigrant policy, 1=let anyone, 4 = prohibit
         E224, #Democracy: Governments tax the rich and subsidize the poor.
              #0 against democracy, 10 essential - needs reverse coding
         E225, #Democracy: Religious authorities interpret the laws.
              #0 it is against democracy, 10 an essential characteristic
         E227, #Democracy: People receive state aid for unemployment. (same)
         E233, #Democracy: Women have the same rights as men. (same)
         E233A, #Democracy: The state makes people's incomes equal (same)
         E035, #Income equality, 1 made more equal, 10 need large differences
         E036, #Private vs state ownership of business, 1=private increased
              #10 = government ownership increased
         E037)  #Government responsibility, 1=people should take,
              # 10=government should)


wvs_codebook <- read_excel("data/WVS/F00003844-WVS_Time_Series_List_of_Variables_and_equivalences.xlsx", skip=1)

waves <- wvs_codebook %>%
  filter(Variable == "S002") %>%
  select(Categories) %>%
  mutate(Categories = str_split(Categories, "\r\r")) %>%
  unnest() %>%
  mutate(Categories = str_remove(Categories, "\n")) %>%
  separate(Categories, into = c("num", "wave"), sep="  ") %>%
  mutate(num = parse_number(num))

wvs_countries <- read_excel("data/WVS/F00003844-WVS_Time_Series_List_of_Variables_and_equivalences.xlsx", sheet="S003 ISO Codes", skip=2) %>%
  rename(S003 = Code,
         country = Label) %>%
  mutate(S003 = parse_number(S003),
         country = case_when(
           country == "Taiwan ROC" ~ "Taiwan",
           country == "Hong Kong SAR" ~ "Hong Kong",
           TRUE~country
         ))

wvs_lr <- wvs %>%
  mutate(study = case_when(
    S001 == 1 ~ "EVS",
    S001 == 2 ~ "WVS"
  )) %>%
  left_join(waves, by=c("S002VS"="num")) %>%
  left_join(wvs_countries) %>%
  select(-c(S001, S002VS, S003)) %>%
  rename(year = S020,
         id = S007) %>%
  select(country, study, wave, year, id, everything())

# evs data -----

evs <- haven::read_dta("data/WVS/evs/ZA7503_v2-0-0.dta")

evs_lr <- evs %>%
  select(S001, S002EVS, S020, COW_NUM,  S007_01, E033,
         F118, F120, C001, E143, E224, E225, E227, E233,
         E233A, E035, E036,E037) %>%
  mutate_at(vars("S001", "S002EVS", "COW_NUM"),
            ~as_factor(.x)) %>%
  rename(study = S001,
         wave = S002EVS,
         id = S007_01,
         year = S020,
         country = COW_NUM) %>%
  select(country, study, wave, year, id, everything()) %>%
  mutate(country = str_squish(country))

# check if duplicated countries in surveys

wvs_lr %>% distinct(country, wave) %>%
  inner_join(
    evs_lr %>% distinct(country, wave))

wvs_lr %>%
  filter(country == "Ukraine") %>%
  filter(wave == "2017-2020") %>%
  select(country, wave, id) %>%
  inner_join(

  evs_lr %>%
    filter(country == "Ukraine") %>%
    filter(wave == "2017-2020") %>%
    select(country, wave, id) %>%
    mutate(flag=1))


# seems like a few countries are duplicated but no duplicate ids

# combine ----

ivs <- rbind(wvs_lr, evs_lr)

save(wvs, evs, ivs, file="data/WVS/wvs.Rdata")

load("data/WVS/wvs.Rdata")


# function to standardize between 0-1 from https://osf.io/jcrk7/
range01 <- function(x){(x-min(x, na.rm = T))/
    (max(x, na.rm = T)-min(x, na.rm = T))}


# clean IVS data
ivs_data <- ivs %>%
  filter(year >= 2006) %>%
  mutate_all(~labelled::remove_labels(.x)) %>%
  # convert DK, missing, etc. to NA
  mutate_at(vars("E033",
                 "F118", "F120", "C001", "E143", "E224", "E225", "E227", "E233",
                 "E233A", "E035", "E036","E037"),
            ~ifelse(. < 0, NA, .x)) %>%
  #reverse code if needed so left is low and right is high
  mutate(F118_rev = 11 - F118,
         F120_rev = 11 - F120,
         C001_rev = case_when(C001 == 1 ~ 1,
                              C001 == 3 ~ 0.5,
                              C001 == 2 ~ 0,
                              TRUE ~ NA_real_),
         E224_rev = 11 - E224,
         E227_rev = 11 - E227,
         E233A_rev = 11 - E233A,
         E036_rev = 11 - E036,
         E037_rev = 11 - E037) %>%
  mutate_at(vars("E033",
                 "F118_rev", "F120_rev", "C001_rev",
                 "E143", "E224_rev", "E225", "E227", "E233",
                 "E233A_rev", "E035", "E036_rev","E037_rev"),
            .funs = list(scaled = ~range01(.x)))


# build hetcor correlation matrix

cor_data <- ivs_data %>% select("E033",
                    "F118_rev", "F120_rev", "C001_rev",
                    "E143", "E224_rev", "E227", "E233",
                    "E035", "E036_rev","E037_rev")

polycor_matrix <- polycor::hetcor(cor_data, use="pairwise.complete.obs")
cor_matrix <- cor(cor_data, use="pairwise.complete.obs")

cor_matrix - polycor_matrix[["correlations"]]

corrplot::corrplot(polycor_matrix$correlations)


fa_data <- polycor_matrix[["correlations"]]
corrplot::corrplot(fa_data)
# no major differences

fa_pa <- psych::fa.parallel(fa_data, n.obs = nrow(ivs_data), fm = "ml")
# maybe 4 factors?

fa_1 <- psych::fa(fa_data, n.obs = nrow(ivs_data), fm = "ml", nfactors = 11)

plot(fa_1$values, type = "b")
KMO(fa_data) #.62
cortest.bartlett(fa_data, n = nrow(ivs_data))
# suggests 1 factor based on eigenvalue

fa_table <- function(x, cut) {
  #get sorted loadings
  loadings <- fa.sort(x)$loadings %>% round(3)
  #cut loadings
  loadings[loadings < cut] <- ""
  #get additional info
  add_info <- cbind(x$communalities,
                    x$uniquenesses,
                    x$complexity) %>%
    as.data.frame() %>%
    rename("Communality" = V1,
           "Uniqueness" = V2,
           "Complexity" = V3) %>%
    rownames_to_column("item")
  #build table
  loadings %>%
    unclass() %>%
    as.data.frame() %>%
    rownames_to_column("item") %>%
    left_join(add_info) %>%
    mutate(across(where(is.numeric), round, 3))
}

# try 4 factors
fa_2 <- psych::fa(fa_data, n.obs = nrow(ivs_data), fm = "ml", nfactors = 4, rotate = "oblimin")


fa_table(fa_2, cut = .32) %>%
  flextable()
# high loadings, but only 2 items per factor

# try 1 factor

fa_3 <- psych::fa(fa_data, n.obs = nrow(ivs_data), fm = "ml", nfactors = 1, rotate = "oblimin", scores="tenBerge",
                  missing=T)


fa_table(fa_3, cut = .32) %>%
  flextable()

# 3 variables that load together: F188_rev, F120_rev, C001_rev


factor1 <- ivs_data %>%
  select(F118_rev, F120_rev, C001_rev)

alpha(factor1) #.61

#does it make sense?
check_factor <- factor1 %>%
  mutate(left_right_fa_orig = rowMeans(data.frame(F118_rev,
                       F120_rev, C001_rev),
                                       na.rm = TRUE),
         left_right_fa_scaled = rowMeans(data.frame(range01(F118_rev), range01(F120_rev), range01(C001_rev),
                                         na.rm = TRUE)))
left_right_all <- ivs_data %>%
  mutate(left_right = E033, #left_right question only
         left_right_fa_orig = rowMeans(data.frame(F118_rev,
                                             F120_rev, C001_rev),
                                  na.rm = TRUE),
         left_right_fa_scaled = rowMeans(data.frame(F118_rev_scaled,
                   F120_rev_scaled, C001_rev_scaled),
                   na.rm = TRUE)) %>%
  group_by(country, wave, year) %>%
  summarize(left_right = mean(E033, na.rm=T),
            left_right_scaled = mean(E033_scaled, na.rm=T),
            left_right_fa = mean(left_right_fa_orig, na.rm=T),
            left_right_fa_scaled = mean(left_right_fa_scaled,
                                        na.rm=T))



hist(left_right_all$left_right)
hist(left_right_all$left_right_scaled)
hist(left_right_all$left_right_fa)
hist(left_right_all$left_right_fa_scaled)

all_years <- seq(2006,2020,1)

left_right <- expand_grid(year = all_years,
                          country = unique(left_right_all$country)) %>%
  left_join(left_right_all, by=c("country", "year")) %>%
  arrange(country, year) %>%
  group_by(country) %>%
  fill(left_right, .direction = "downup") %>%
  fill(left_right_scaled, .direction = "downup") %>%
  fill(left_right_fa, .direction = "downup") %>%
  fill(left_right_fa_scaled, .direction = "downup") %>%
  mutate(country = case_when(
    country == "Great Britain" ~ "United Kingdom",
    TRUE ~ country
  ))

left_right %>% distinct(country, .keep_all = T) %>%
  right_join(all_countries) %>% view()

save(left_right, file="data/final data/left_right.Rdata")
