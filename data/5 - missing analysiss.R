library(finalfit)
missing_data <- model_data %>%
  select(co2_lag, sl_total, oecd, pop, gdp, exports, mfg, gini, left_right_fa_scaled, democracy, net_enrollment_corrected)

missing_glimpse(missing_data)
missing_pattern(missing_data)
missing_pairs(missing_data)


#gini
model_data %>%
  missing_compare("gini", c("pop", "democracy", "sl_total", "oecd", "exports",
                                 "net_enrollment_corrected", "co2_lag", "pop", "exports", "mfg", "left_right_fa_scaled"))

#democracy
model_data %>%
  missing_compare("democracy", c("gini", "pop", "oecd", "exports",
                            "net_enrollment_corrected", "co2_lag", "exports", "mfg"))

#l-r
model_data %>%
  missing_compare("left_right_fa_scaled", c("gini", "pop", "oecd", "exports",
                                 "net_enrollment_corrected", "co2_lag", "exports", "mfg"))



