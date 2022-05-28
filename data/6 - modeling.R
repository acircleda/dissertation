# modeling

library(tidyverse)
library(lme4)
library(lmerTest)
library(JointAI)
library(mlmhelpr)
library(sjPlot)
library(broom.mixed)
library(flextable)
library(ftExtra)
library(modelsummary)
library(mice)
library(mitml)
library(ggeffects)
library(performance)
library(clubSandwich)
load("data/final data/final_data.Rdata")
load("models/imp_data.Rdata")
#load("models/model_data.Rdata")


# prep model data ----

model_data <- all_data %>%
  drop_na(sl_total) %>%
  select(1:6, time, oecd, sdg_region, un_sub_region, pop, gdp, exports, mfg, gini, left_right, left_right_scaled, left_right_fa, left_right_fa_scaled, democracy, corruption, net_enrollment_corrected, collectivism_individualism, contains("sl"), contains("_c")) %>%
  # re-centering SL
  group_by(country) %>%
  #between - removing additional center around 500
  mutate(sl_lvl2 = mean(sl_total, na.rm=T),
         sl_lvl2_female = mean(sl_female, na.rm=T),
         sl_lvl2_male = mean(sl_male, na.rm=T)) %>%
  # within
  ungroup() %>%
  rowwise() %>%
  mutate(sl_lvl1 = sl_total-sl_lvl2,
         sl_lvl1_female = sl_female-sl_lvl2_female,
         sl_lvl1_male = sl_male-sl_lvl2_male,
         oecd = as.factor(oecd),
         sdg_region = as.factor(sdg_region),
         region = case_when(
           un_sub_region == "Australia/New Zealand" ~ "Western Nations",
           un_sub_region == "Western Europe" ~ "Western Nations",
           un_sub_region == "Northern Europe" ~ "Western Nations",
           un_sub_region == "Northern America" ~ "Western Nations",
           un_sub_region == "Central America" ~ "Latin America/Caribbean/Other",
           un_sub_region == "South America" ~ "Latin America/Caribbean/Other",
           un_sub_region == "Caribbean" ~ "Latin America/Caribbean/Other",
           un_sub_region == "Eastern Africa" ~ "Latin America/Caribbean/Other",
           un_sub_region == "Western Asia" ~ "MENA",
           un_sub_region == "Northern Africa" ~ "MENA",
           TRUE~un_sub_region
         ),
         region = as.factor(region),
         sdg_region = as.factor(sdg_region)) %>%
  ungroup() %>%
  mutate_at(vars("pop", "gdp", "exports", "mfg", "gini", "left_right", "left_right_scaled", "left_right_fa", "left_right_fa_scaled",
                 "democracy", "corruption", "net_enrollment_corrected",
                 "collectivism_individualism"),
            .funs=list(c = ~.x - mean(.x, na.rm=T))) %>%

  mutate(
    # fix Kosovo data
    co2_lag = ifelse(country == "Kosovo" & year == 2018, 4.8, co2_lag),
    # create log of dv
    co2_log = log(co2_lag),
    # create Qatar dummy
    qatar = ifelse(country == "Qatar", "Qatar", "Not-Qatar"))





# multicollinearity checks ----

  # all covars
vif_check_model <- lm(row ~ sl_lvl1 + sl_lvl2 + pop_c + gdp_c + exports_c + mfg_c + gini_c + left_right_fa_c + democracy_c + corruption_c + net_enrollment_corrected_c + collectivism_individualism_c,
                data = all_data %>%
                  rownames_to_column("row"))

# without corruption
vif_wo_corruption <- lm(row ~ sl_lvl1 + sl_lvl2 + pop_c + gdp_c + exports_c + mfg_c + gini_c + left_right_fa_c + democracy_c  +  net_enrollment_corrected_c + collectivism_individualism_c,
                                        data = all_data %>%
                                          rownames_to_column("row"))

# without collectivism
vif_wo_coll<- lm(row ~ sl_lvl1 + sl_lvl2 + pop_c + gdp_c + exports_c + mfg_c + gini_c + left_right_fa_c + democracy_c  +  net_enrollment_corrected_c + corruption_c,
                        data = all_data %>%
                          rownames_to_column("row"))

# without corruptioncollectivism
vif_wo<- lm(row ~ sl_lvl1 + sl_lvl2 + pop_c + gdp_c + exports_c + mfg_c + gini_c + left_right_fa_c + democracy_c  +  net_enrollment_corrected_c,
                 data = all_data %>%
                   rownames_to_column("row"))

# with region
vif_region <- lm(row ~ sl_lvl1 + sl_lvl2 + pop_c + gdp_c + exports_c + mfg_c + gini_c + left_right_fa_c + democracy_c  +  net_enrollment_corrected_c + region,
            data = model_data %>%
              rownames_to_column("row"))

olsrr::ols_coll_diag(vif_region)[["vif_t"]] %>%
  select(Variables, VIF) %>%
  mutate(VIF = round(VIF, 3)) %>%
  clipr::write_clip()

# run checks
vif_check <- olsrr::ols_coll_diag(vif_check_model)
vif_check_wo_corruption <- olsrr::ols_coll_diag(vif_wo_corruption)
vif_check_wo_coll <- olsrr::ols_coll_diag(vif_wo_coll)
vif_check_wo <- olsrr::ols_coll_diag(vif_wo)
vif_check_region <- olsrr::ols_coll_diag(vif_region)

# compares vifs
vif_table <- vif_check$vif_t %>% as.data.frame() %>%
  left_join(vif_check_wo_coll$vif_t, by="Variables") %>%
  select(1,3, 5) %>%
  rename("Model 1: All Continuous Variables" = VIF.x,
         "Model 2: w/o Collectivism" = VIF.y) %>%
  left_join(vif_check_wo_corruption$vif_t, by="Variables") %>%
  rename("Model 3: w/o Corruption" = VIF) %>%
  left_join(vif_check_wo$vif_t, by="Variables") %>%
  rename("Model 4: w/o Corruption and Collectivism" = VIF) %>%
  full_join(vif_check_region$vif_t, by="Variables") %>%
  rename("Model 5: w/Region" = VIF) %>%
  mutate(across(where(is.numeric), round, 2)) %>%
  select(-contains("Tolerance")) %>%
  mutate(
    Variables = ifelse(str_detect(Variables, "region"),
                       str_replace(Variables, "region", ""),
                       Variables),
    Variables = ifelse(str_detect(Variables, "/"),
                                  str_replace_all(Variables, "/", "/ "),
                                  Variables),
    Variables = case_when(
    Variables == "sl_lvl1" ~ "Scientific Literacy (within)",
    Variables == "sl_lvl2" ~ "Scientific Literacy (between)",
    Variables == "pop_c" ~ "Population",
    Variables == "gdp_c" ~ "GDP-PPP",
    Variables == "exports_c" ~ "Exports",
    Variables == "mfg_c" ~ "Manufacturing",
    Variables == "left_right_fa_c" ~ "Left-Right Orientation",
    Variables == "gini_c" ~ "Gini",
    Variables == "democracy_c" ~ "Democracy Index",
    Variables == "corruption_c" ~ "Corruption Index",
    Variables == "net_enrollment_corrected_c" ~ "Net Enrollment",
    Variables == "collectivism_individualism_c" ~ "Collectivism-Individualism",
    TRUE ~ Variables
  )) #%>%
  #arrange(Variables) %>%
  flextable(vif_table)

 # VIF All: left_right, corruption, collectivism are high
 # VIF w/o corruption: collectivism and left_right high
 # vif w/o collectivism: corruuption high
 # vif w/o corruption and collectivism: best VIF for sl_lvl1 and sl_lvl2 - left_right is lowest here

# condition indexes
vif_check$eig_cindex %>% as.data.frame() %>%
  mutate(across(where(is.numeric), round, 3)) %>% view("with")

vif_check_wo$eig_cindex %>% as.data.frame() %>%
  mutate(across(where(is.numeric), round, 3)) %>% view("without")

# seems like the models will be better without corruption AND collectivism_individualism, so I will remove them.

# models ----

# model 0 - null model - fe for time ----
model_0 <- lmer(co2_lag ~ time + (1 | country),
                data=model_data,
                REML = F)

performance::icc(model_0) #.965
mlmhelpr::icc(model_0) # .965
# most of the variation is between countries
# ICC: .965 = proportion of variance between countries after controlling for time (Hoffman, 162).

multilevelR2(model_0)
as.character(scales::percent(multilevelR2(model_0)[1],1))


mlmhelpr::design_effect(model_0) # de of 3.95 = good

sjPlot::tab_model(model_0)
  # CO2 decreasing over time (-.40)
  # intercept: average CO2 per capita in 2006

mlmhelpr::plausible_values(model_0)
# predictive interval includes negative values
# ?? is this cause for concern? This may be because a lot of countries have lower values? Also, a lot of variance between countries could be it

# co2 is bounded by 0, could transform dv (natural log)
# tobit regression - censored DV

min(predict(model_0))
max(predict(model_0))
# predicted values look ok, so don't worry too much about PVs


ggplot(model_data, aes(x=co2_lag))+
  geom_histogram(binwidth = .5, color="white")+
  scale_x_continuous(breaks=seq(0,56,2))

plot_model(model_0, "re")

# model 1 - null model with re for time ----
model_1 <- lmer(co2_lag ~ time + (time | country),
                   data=model_data,
                   REML = F,
                control = lmerControl(optimizer ="Nelder_Mead"))

mlmhelpr::r2(model_0)
pve(model_0, model_1)

plot_model(model_1, "re")

# includes optimizer b/c of convergence issue

min(predict(model_1))
max(predict(model_1))

# includes optimizer b/c of convergence issue

performance::icc(model_1)
  # 97.1 - increased, but hoffman says not to compute icc with random effects for time (Hoffman, 164)

mlmhelpr::icc(model_1)
  # results not interpretable - warning msg as expected

anova(model_0, model_1)
  # model_1 with re for time fits better
  # slope effect for time is different - change in CO2 emissions within each country differs

multilevelR2(model_1)

summary(model_1)
0.2737/(60.2565+0.2737+ 0.9989+2*-3.8766462)

#.5% of random effect variance is related to time
# Is this the correct interpretation: only a small proportion of variance in CO2 emissions is related to slope effects time, meaning rates are more or less stable on average
  # related to the small within-country variance (3.5%)

# Rocconi: # Yes on 3.5% of var in CO2 is even due within countries differences

pve(model_0, model_1)
 # ?? I get a negative value - I believe this is expected
  # more useful for fixed effects

sjPlot::tab_model(model_0, model_1)
modelsummary(models=list("Model 0: Unconditional Model, Random Intercepts" = model_0,
                         "Model 1: Unconditional Model: Random Intercepts, Random Slopes" = model_1))

# similar interpretation to model_0, though between country variance is much higher

# country.time is the variation in the slope of time

# t_00 is the variance in co2 between countries
# t_11: Rocconi:
# This is the covariance between the random slope and intercept (displayed as a correlation)
# Since it is negative, it indicates the higher the intercept, the smaller the slope.
# If the country-level CO2 is high, the effect of time in that country is smaller

# negative correlation between CO2 and time - for countries with higher CO2, the effect of time is weaker - CO2 is decling more slowly


plot_model(model_1, "re")

# model 2 - check for non-linear effects ----

model_2 <- lmer(co2_lag ~ time + I(time^2) + (time + I(time^2) | country),
                data=model_data,
                REML = F,
                control = lmerControl(optimizer ="Nelder_Mead"))

model_2_fit <- anova(model_1, model_2)

model_2_fit_table <- model_2_fit %>%
  as.data.frame() %>%
  rownames_to_column("Model") %>%
  mutate(Model = str_to_title(Model),
         Model = str_replace(Model, "_", " ")) %>%
  rename(Parameters = npar,
         `Log-Likelihood` = logLik,
          Deviance = deviance,
          df = Df,
          p = `Pr(>Chisq)`) %>%
  mutate(across(where(is.numeric), round, 3))



# model 3 - conditional growth model - fe and re for SL ----

model_3 <- lmer(co2_lag ~ time + sl_lvl1 + sl_lvl2 + (time + sl_lvl1 | country),
                data=model_data,
                REML = F,
                control = lmerControl(optimizer ="Nelder_Mead"))


# singular fit issue
# don't need a random effect for sl_lvl1
## This is simply saying that the impact of SL does not vary across countries. The main effect is sufficient

model_3_nosl <- lmer(co2_lag ~ time + sl_lvl1 + sl_lvl2 + (time | country),
                data=model_data,
                REML = F,
                control = lmerControl(optimizer ="Nelder_Mead"))

tab_model(model_3_nosl)
anova(model_1, model_3_nosl) # model 1 fits slightly better
anova(model_3, model_3_nosl) # model 3 fits slightly better

plot_model(model_3_nosl, "re")
plot_model(model_3_nosl, "pred")

# the slope effect for SL is the same across countries
# not enough evidence for SL_lvl1 as a random effect
# SL seems stable across countries

sjPlot::tab_model(model_1, model_3_nosl)
  # similar results. Small, non-sig fe for scientific literacy

anova(model_1, model_3_nosl)

# model 3 does not fit better than model 2
# ?? is this suggesting that separating out between/within effects are not a good idea because of low variance?


mitml::multilevelR2(model_1)
mitml::multilevelR2(model_3_nosl)

coef(model_3)$country %>%
  mutate(across(where(is.numeric), round, 3)) %>%
  summarize(min = min(sl_lvl1),
            mean = mean(sl_lvl1),
            max = max(sl_lvl1))

coef(model_3_nosl)$country %>%
  mutate(across(where(is.numeric), round, 3))


min(predict(model_3_nosl))
max(predict(model_3_nosl))

# choose no sl re

# model 3 diagnostics ----

# outliers
car::influenceIndexPlot(model_3_nosl)

model_3_influence <- HLMdiag::hlm_influence(model_3_nosl)
model_3_influence %>% select(country, cooksd) %>%
  psych::describe()

# residuals
HLMdiag::hlm_resid(model_3_nosl) %>% view()

# trying without outlier

model_3_noqatar <- lmer(co2_lag ~ time + sl_lvl1 + sl_lvl2 + (time | country),
                     data=model_data %>%
                       filter(country != "Qatar"),
                     REML = F,
                     control = lmerControl(optimizer ="Nelder_Mead"))

# trying with dummy for outlier

model_3_qatar <- lmer(co2_lag ~ time + sl_lvl1 + sl_lvl2 + qatar + (time | country),
                     data=model_data,
                     REML = F,
                     control = lmerControl(optimizer ="Nelder_Mead"))

# trying with log of dv due to non-normality/mis-spec

model_3_log <- lmer(co2_log ~ time + sl_lvl1 + sl_lvl2 + (time | country),
                      data=model_data,
                      REML = F,
                      control = lmerControl(optimizer ="Nelder_Mead"))

# trying log and control for outlier

model_3_log_qatar <- lmer(co2_log ~ time + sl_lvl1 + sl_lvl2 + qatar + (time | country),
                    data=model_data,
                    REML = F,
                    control = lmerControl(optimizer ="Nelder_Mead"))

# trying log and no outlier

model_3_log_noqatar <- lmer(co2_log ~ time + sl_lvl1 + sl_lvl2 + (time | country),
                        data=model_data %>%
                          filter(country != "Qatar"),
                        REML = F,
                        control = lmerControl(optimizer ="Nelder_Mead"))


# trying quad effect for time

model_3_quad <- lmer(co2_lag ~ time + I(time^2) + sl_lvl1 + sl_lvl2 + (time + I(time^2) | country),
                     data=model_data,
                     REML = F,
                     control = lmerControl(optimizer ="Nelder_Mead"))


# diagnostics

check_model(model_3_nosl)
check_model(model_3_noqatar)
check_model(model_3_qatar)
check_model(model_3_log)
check_model(model_3_log_noqatar)
check_model(model_3_log_qatar)
check_model(model_3_quad)
compare_performance(model_3_nosl, model_3_noqatar, model_3_qatar, model_3_log, model_3_log_noqatar, model_3_log_qatar, model_3_quad) %>%
  mutate(across(where(is.numeric), round, 3)) %>%
  flextable()

#robust model 3


model_3_robust <- coef_test(model_3_noqatar, vcov=vcovCR(model_3_noqatar, type="CR2"))

# model 3 log ----

anova(model_1, model_3_log)
# model 3 has better fit according to AIC/BIC

model_0_log <- lmer(co2_log ~ time + (1 | country),
                    data=model_data %>%
                      filter(country != "Qatar"),
                    REML = F,
                    control = lmerControl(optimizer ="Nelder_Mead"))

model_1_log <- lmer(co2_log ~ time + (time | country),
                    data=model_data %>%
                      filter(country != "Qatar"),
                                   REML = F,
                                   control = lmerControl(optimizer ="Nelder_Mead"))

# true lrt
anova(model_1_log, model_3_log_noqatar)
# model_3 does not fully improve model fit but is better specificed according to fit stats

mlmhelpr::r2(model_3_log_noqatar)
performance::r2(model_3_log_noqatar)
pve(model_1_log, model_3_log)
# data imputation ----

# this takes ~8 mins to run, load the result here
# imp_model <- lmer_imp(co2_lag ~ time + sl_lvl1 + sl_lvl2 +
#                          pop_c + gdp_c + mfg_c + exports_c + gini_c + left_right_fa_scaled_c + democracy_c + net_enrollment_corrected_c + sdg_region + oecd +
#                          (time | country),
#                        data=as.data.frame(model_data),
#                        seed=1983, n.iter = 10000, n.chains=10,
#                        timevar = "time",
#                        monitor_params = c(imps = TRUE)); beepr::beep(2)

# save(imp_model, file="models/imp_data.Rdata")


trace <- traceplot(imp_model, use_ggplot = T)+
  theme(stri)
ggsave("figures/trace.jpg", plot=trace, width=8, height=5)

gr_table <- GR_crit(imp_model) # these look good
plot(MC_error(imp_model)) # all below .05
density_plot_jai <- densplot(imp_model)

list_models(imp_model,
            priors = FALSE, regcoef = FALSE, otherpars = FALSE)

summary(imp_model)

# make imputed data sets ----

#get 20 imputed dfs from bayesian model
midat <- get_MIdat(imp_model, m = 20, seed = 1983) %>%
  rename(.imp = Imputation_) %>%
  as.mids()

# imp data without qatar, with log, rescales
midat_no_qatar <- get_MIdat(imp_model, m = 20, seed = 1983) %>%
  rename(.imp = Imputation_) %>%
  filter(country != "Qatar") %>%
  # rescaling
  mutate(
    mfg_c = mfg_c*100,
    exports_c = exports_c*100,
    gini_c = gini_c*100,
    left_right_fa_scaled_c = left_right_fa_scaled_c*100,
    democracy_c = democracy_c*100,
    corruption_c = corruption_c*100,
    net_enrollment_corrected_c = net_enrollment_corrected_c*100,
    collectivism_individualism_c = collectivism_individualism_c*100
    ) %>%
  as.mids()




# model 4 - conditional model with covariates ----

# all variables
model_4 <- with(midat, lme4::lmer(co2_lag ~ time + sl_lvl1 + sl_lvl2 + pop_c + gdp_c + mfg_c + gini_c + exports_c + left_right_fa_scaled_c + democracy_c + net_enrollment_corrected_c + oecd*sl_lvl1 + region +
                                (time | country),
                                             REML = F,
                                             control = lmerControl(optimizer ="Nelder_Mead")))
# singularity for all models

# removing region
model_4_noregion <- with(midat, lme4::lmer(co2_lag ~ time + sl_lvl1 + sl_lvl2 + pop_c + gdp_c + mfg_c + exports_c + gini_c + gdp_c + gini_c  + left_right_fa_scaled_c + democracy_c + net_enrollment_corrected_c + oecd*sl_lvl1 +
                                    (time | country),
                                  REML = F,
                                  control = lmerControl(optimizer ="Nelder_Mead")))

model_4_no_qatar <- with(midat_no_qatar, lme4::lmer(co2_lag ~ time + sl_lvl1 + sl_lvl2 + pop_c + gdp_c + mfg_c + exports_c + gini_c + gdp_c + gini_c  + left_right_fa_scaled_c + democracy_c + net_enrollment_corrected_c + oecd*sl_lvl1 +
                                             (time | country),
                                           REML = F,
                                           control = lmerControl(optimizer ="Nelder_Mead")))

model_4_no_qatar_quad <- with(midat_no_qatar, lme4::lmer(co2_lag ~ time + I(time^2) + sl_lvl1 + sl_lvl2 + pop_c + gdp_c + mfg_c + exports_c + gini_c + gdp_c + gini_c  + left_right_fa_scaled_c + democracy_c + net_enrollment_corrected_c + oecd*sl_lvl1 +
                                                      (time  + I(time^2)| country),
                                                    REML = F,
                                                    control = lmerControl(optimizer ="Nelder_Mead")))
# trying some alternative models

model_4_no_qatar_quad <- with(midat_no_qatar, lme4::lmer(co2_lag ~ time + I(time^2) + sl_lvl1 + sl_lvl2 + pop_c + gdp_c + mfg_c + exports_c + gini_c + gdp_c + gini_c  + left_right_fa_scaled_c + democracy_c + net_enrollment_corrected_c + oecd*sl_lvl1 +
                                                      (time | country),
                                                    REML = F,
                                                    control = lmerControl(optimizer ="Nelder_Mead")))

model_4_log <- with(midat, lme4::lmer(co2_log ~ time + sl_lvl1 + sl_lvl2 + pop_c + gdp_c + mfg_c + exports_c + gini_c + gdp_c + gini_c  + left_right_fa_scaled_c + democracy_c + net_enrollment_corrected_c + oecd*sl_lvl1 +
                                          (time || country),
                                        REML = F,
                                        control = lmerControl(optimizer ="Nelder_Mead")))

# final model ----
model_4_noqatar_log <- with(midat_no_qatar, lme4::lmer(co2_log ~ time + sl_lvl1 + sl_lvl2 + pop_c + gdp_c + mfg_c + exports_c + gini_c + gdp_c + gini_c  + left_right_fa_scaled_c + democracy_c + net_enrollment_corrected_c + oecd*sl_lvl1 +
                                        (time | country),
                                      REML = F,
                                      control = lmerControl(optimizer ="Nelder_Mead")))

# model 4 diagnostics ----

imp_1 <- model_4_noregion$analyses[[1]]
imp_1_q <- model_4_no_qatar$analyses[[1]]
imp_1_log <- model_4_log$analyses[[1]]
imp_1_qq <- model_4_noqatar_log$analyses[[1]]

check_model(imp_1_qq)
check_model(imp_1_qq, check=c("pp_check", "linearity", "homogeneity", "normality", "qq", "reqq")) # no qatar
ggsave(plot=last_plot(), file="figures/diagmod4.jpg", width=9, height=7)


check_model(imp_1_log)
check_model(imp_1_qq)


# generate robust SE
# remove Qatar from main data
# rm_qatar <- model_4_no_qatar$analyses[[1]]@frame[["country"]]
#
#
# model_4_robust <- with(midat_no_qatar, lme4::lmer(co2_lag ~ time + sl_lvl1 + sl_lvl2 + pop_c + gdp_c + mfg_c + exports_c + gini_c + gdp_c + gini_c  + left_right_fa_scaled_c + democracy_c + net_enrollment_corrected_c + oecd*sl_lvl1 +
#                                                       (time | country),
#                                                     REML = F,
#                                                     control = lmerControl(optimizer ="Nelder_Mead")) %>%
#                         coef_test(cluster=rm_qatar, vcov="CR2"))

# function to pool robust estimates https://www.jepusto.com/mi-with-clubsandwich/
# pool_robust <- function(x){
#
#   pooled <- x$analyses %>%
#
#   # add coefficient names as a column
#   lapply(function(x) {
#     x$coef <- row.names(x)
#     x
#   }) %>%
#   bind_rows() %>%
#   as.data.frame() %>%
#
#
# # summarize by coefficient
#   group_by(coef) %>%
#   summarise(
#     m = n(),
#     B = var(beta),
#     beta_bar = mean(beta, na.rm=T),
#     V_bar = mean(SE^2, na.rm=T),
#     eta_bar = mean(df_Satt, na.rm=T)
#   ) %>%
#
#   mutate(
#
#     # calculate intermediate quantities to get df
#     V_total = V_bar + B * (m + 1) / m,
#     gamma = ((m + 1) / m) * B / V_total,
#     df_m = (m - 1) / gamma^2,
#     df_obs = eta_bar * (eta_bar + 1) * (1 - gamma) / (eta_bar + 3),
#     df = 1 / (1 / df_m + 1 / df_obs),
#
#     # calculate summary quantities for output
#     se = sqrt(V_total),
#     t = beta_bar / se,
#     p_val = 2 * pt(abs(t), df = df, lower.tail = FALSE),
#     crit = qt(0.975, df = df),
#     lo95 = beta_bar - se * crit,
#     hi95 = beta_bar + se * crit
#   )
#
#   pooled %>%
#     select(coef, estimate = beta_bar, std.error = se,
#            t, df, p.value = p_val, lo95, hi95, gamma) %>%
#     mutate_at(vars(estimate:gamma), round, 3)
# }
#
# pool_robust(model_4_robust) %>%
#   flextable()

pool(model_4_noqatar_log$analyses)

testEstimates(model_4_noqatar_log$analyses)$estimates %>%
  as.data.frame() %>%
  mutate(exp = (exp(Estimate)-1)*100) %>%
  mutate(across(where(is.numeric), round, 3)) %>%
  view()


confint(testEstimates(model_4_noqatar_log$analyses))

testEstimates(model_4_noqatar_log$analyses, extra.pars = T)$extra.pars %>%
  sqrt() %>%
  as.data.frame() %>%
  mutate(across(where(is.numeric), round, 3))


# refitting model for LRT
model_3_nosl2 <- with(midat_no_qatar, lmer(co2_log ~ time + sl_lvl1 + sl_lvl2 + (time | country),
                     REML = F,
                     control = lmerControl(optimizer ="Nelder_Mead")))

model_3_log_noqatar <- lmer(co2_log ~ time + sl_lvl1 + sl_lvl2 + (time | country),
                            data=model_data %>%
                              filter(country != "Qatar"),
                            REML = F,
                            control = lmerControl(optimizer ="Nelder_Mead"))


testModels(model_4_noqatar_log$analyses, model_3_nosl2$analyses,
           method="D3")

mean(sapply(model_4_noqatar_log$analyses, AIC))
mean(sapply(model_4_noqatar_log$analyses, logLik))
mean(sapply(model_4_noqatar_log$analyses, deviance))
mean(sapply(model_4_noqatar_log$analyses, AIC))
mean(sapply(model_4_noqatar_log$analyses, logLik))
mean(sapply(model_4_noqatar_log$analyses, deviance))

mitml::multilevelR2(model_4_noqatar_log$analyses)


check_model(model_4_noqatar_log$analyses[[1]])

# get predictions
rbind(
  data.frame(pred=predict(model_4_noqatar_log$analyses[[1]])),
  data.frame(pred=predict(model_4_noqatar_log$analyses[[2]])),
  data.frame(pred=predict(model_4_noqatar_log$analyses[[3]])),
  data.frame(pred=predict(model_4_noqatar_log$analyses[[4]])),
  data.frame(pred=predict(model_4_noqatar_log$analyses[[5]])),
  data.frame(pred=predict(model_4_noqatar_log$analyses[[6]])),
  data.frame(pred=predict(model_4_noqatar_log$analyses[[7]])),
  data.frame(pred=predict(model_4_noqatar_log$analyses[[8]])),
  data.frame(pred=predict(model_4_noqatar_log$analyses[[9]])),
  data.frame(pred=predict(model_4_noqatar_log$analyses[[10]]))) %>%
  psych::describe()

# predictive range makes senses

# interactions ----

# probe interactions at +- 10 point intervals
int_values <- seq(-50,50,10)

# probe formaula
probe <- function(model, x){
  ggemmeans(model[["analyses"]][[x]], terms = c("sl_lvl1 [int_values]", "oecd"), type="random")
}

int_model_4 <- rbind(
  probe(model_4_noqatar_log, 1),
  probe(model_4_noqatar_log, 2),
  probe(model_4_noqatar_log, 3),
  probe(model_4_noqatar_log, 4),
  probe(model_4_noqatar_log, 5),
  probe(model_4_noqatar_log, 6),
  probe(model_4_noqatar_log, 7),
  probe(model_4_noqatar_log, 8),
  probe(model_4_noqatar_log, 9),
  probe(model_4_noqatar_log, 10),
  probe(model_4_noqatar_log, 11),
  probe(model_4_noqatar_log, 12),
  probe(model_4_noqatar_log, 13),
  probe(model_4_noqatar_log, 14),
  probe(model_4_noqatar_log, 15),
  probe(model_4_noqatar_log, 16),
  probe(model_4_noqatar_log, 17),
  probe(model_4_noqatar_log, 18),
  probe(model_4_noqatar_log, 19),
  probe(model_4_noqatar_log, 20)) %>%
  group_by(group, x) %>%
  summarize_all(.funs=mean)

int_model_4 %>%
  ggplot()+
  geom_ribbon(aes(x=x, ymin=predicted-std.error,
                  ymax=predicted+std.error), fill="grey90")+
  geom_line(aes(x=x, y=predicted, color=group))+
  facet_wrap(~group)+
  xlab("Points Above/Below SL (within) Scores")+
  ylab("Predicted\nValues\nof CO2")+
  scale_y_continuous(limits=c(0,4))+
  scale_color_manual(values=c("#EE3E80", "#2197A9"), name=NULL)+
  theme(panel.background = element_blank(),
        panel.grid.major.y = element_line(color="grey80"),
        axis.ticks = element_blank(),
        strip.text = element_text(face="bold"),
        strip.background = element_blank(),
        legend.position = "none",
        axis.title = element_text(face="italic"),
        axis.title.y = element_text(angle = 0, vjust=.5),
        panel.spacing = unit(2, "lines"))+
  ggh4x::force_panelsizes(cols = c(.9, 1))

ggsave(plot=last_plot(), file="figures/int.jpg", width=5, height=4)


# male and female models ----

model_female <- with(midat_no_qatar, lme4::lmer(co2_log ~ time + sl_lvl1_female + sl_lvl2_female + pop_c + gdp_c + mfg_c + exports_c + gini_c + left_right_fa_scaled_c + democracy_c + net_enrollment_corrected_c + oecd*sl_lvl1_female +
                                    (time | country),
                                  REML = F,
                                  control = lmerControl(optimizer ="Nelder_Mead")))




model_male <- with(midat_no_qatar, lme4::lmer(co2_log ~ time + sl_lvl1_male + sl_lvl2_male + pop_c + gdp_c + mfg_c + exports_c + gini_c + left_right_fa_scaled_c + democracy_c + net_enrollment_corrected_c + oecd*sl_lvl1_male +
                                         (time | country),
                                       REML = F,
                                       control = lmerControl(optimizer ="Nelder_Mead")))



female <- testEstimates(model_female$analyses)$estimate %>%
  as.data.frame()
male <- testEstimates(model_male$analyses)$estimate %>%
  as.data.frame()


z <- (female$Estimate-male$Estimate)/sqrt(
    (female$Std.Error^2)+(male$Std.Error^2)) %>%
    as.data.frame() %>%
  rename(z = 1)



# no sig differences

probe_fem <- function(model, x){
  ggemmeans(model[["analyses"]][[x]], terms = c("sl_lvl1_female [int_values]", "oecd"), type="random")
}
int_model_female <- rbind(
  probe_fem(model_female, 1),
  probe_fem(model_female, 2),
  probe_fem(model_female, 3),
  probe_fem(model_female, 4),
  probe_fem(model_female, 5),
  probe_fem(model_female, 6),
  probe_fem(model_female, 7),
  probe_fem(model_female, 8),
  probe_fem(model_female, 9),
  probe_fem(model_female, 10),
  probe_fem(model_female, 11),
  probe_fem(model_female, 12),
  probe_fem(model_female, 13),
  probe_fem(model_female, 14),
  probe_fem(model_female, 15),
  probe_fem(model_female, 16),
  probe_fem(model_female, 17),
  probe_fem(model_female, 18),
  probe_fem(model_female, 19),
  probe_fem(model_female, 20)) %>%
  group_by(group, x) %>%
  summarize_all(.funs=mean)

probe_male <- function(model, x){
  ggemmeans(model[["analyses"]][[x]], terms = c("sl_lvl1_male [int_values]", "oecd"), type="random")
}
int_model_male <- rbind(
  probe_male(model_male, 1),
  probe_male(model_male, 2),
  probe_male(model_male, 3),
  probe_male(model_male, 4),
  probe_male(model_male, 5),
  probe_male(model_male, 6),
  probe_male(model_male, 7),
  probe_male(model_male, 8),
  probe_male(model_male, 9),
  probe_male(model_male, 10),
  probe_male(model_male, 11),
  probe_male(model_male, 12),
  probe_male(model_male, 13),
  probe_male(model_male, 14),
  probe_male(model_male, 15),
  probe_male(model_male, 16),
  probe_male(model_male, 17),
  probe_male(model_male, 18),
  probe_male(model_male, 19),
  probe_male(model_male, 20)) %>%
  group_by(group, x) %>%
  summarize_all(.funs=mean)

rbind(int_model_male %>% mutate(type="male"),
      int_model_female %>% mutate(type="female")) %>%
  ggplot()+
  geom_ribbon(aes(x=x, ymin=predicted-std.error,
                  ymax=predicted+std.error), fill="grey90")+
  geom_line(aes(x=x, y=predicted), color="blue")+
  facet_wrap(~group+type)+
  xlab("Points Above/Below SL (within) Scores")+
  ylab("Predicted Values of log(CO2)")+
  scale_y_continuous(limits=c(1,3))+
  theme(panel.background = element_blank())

# try a three-level

model_3lvl <- with(midat_no_qatar, lme4::lmer(co2_lag ~ time + sl_lvl1 + sl_lvl2 +  (time | region/country),
                                       REML = F,
                                       control = lmerControl(optimizer ="Nelder_Mead")))

remove(imp_model)
save.image("models/model_data.Rdata")
save(model_data, midat, file="models/model_and_imp_data.Rdata")
save(gr_table, file="models/gr_table.Rdata")
# power-analysis


