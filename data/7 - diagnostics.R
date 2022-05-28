library(tidyverse)
library(lme4)
library(lmerTest)
library(mitml)
library(HLMdiag)
library(JointAI)
library(performance)
#save(model_data, imp_data, midat, file="diagnostic_data.Rdata")
load("diagnostic_data.Rdata")

# model_data = original data set
# imp_data = 20 imputations
# midat = imputed data ready for lmer

# final model

model_4 <- with(midat, lme4::lmer(co2_lag ~ time + sl_lvl1 + sl_lvl2 + pop_c + gdp_c + mfg_c + exports_c + gini_c + gdp_c + gini_c  + left_right_fa_scaled_c + democracy_c + net_enrollment_corrected_c + oecd*sl_lvl1 +
                                             (time | country),
                                           REML = F,
                                           control = lmerControl(optimizer ="Nelder_Mead")))

# individual models for diagnostic testing
imp_1 <- model_4$analyses[[1]]
imp_1_res <- HLMdiag::hlm_resid(imp_1, 1)
imp_1_res_2 <- HLMdiag::hlm_resid(imp_1, "country")
imp_8 <- model_4$analyses[[8]]
imp_8_res <- HLMdiag::hlm_resid(imp_8, 1)
imp_8_res_2 <- HLMdiag::hlm_resid(imp_8, "country")
imp_17 <- model_4$analyses[[17]]
imp_17_res <- HLMdiag::hlm_resid(imp_17, 1)
imp_17_res_2 <- HLMdiag::hlm_resid(imp_17, "country")

# diagnostics ----

# linearity
plot(model_data %>% select(co2_lag, sl_lvl1, sl_lvl2, pop_c, gdp_c,
                           mfg_c, exports_c, gini_c, gdp_c, gini_c,
                           left_right_fa_scaled_c, democracy_c,
                           net_enrollment_corrected_c))

plot(
  #stndardized
  scale(imp_1_res$.resid, center=F),
  #vs outcome
  predict(imp_1)) # issues

DHARMa::plotQQunif(imp_1)
DHARMa::plotQQunif(imp_8)
DHARMa::plotQQunif(imp_17)


#outliers
car::influencePlot(imp_1, id=T) # 201,205,310,320
car::influencePlot(imp_8, id=T) # same
car::influencePlot(imp_17, id=T) # same

model_data %>%
  slice(205, 201, 310, 320)


lattice::qqmath(imp_17, id=.05)
# qatar, UAE, kosovo, SK

check_model(imp_1) # high VIF from interaction
check_model(imp_8)
check_model(imp_17)


# model with log of co2 ----

model_4_log <- with(midat, lme4::lmer(co2_log ~ time + sl_lvl1 + sl_lvl2 + pop_c + gdp_c + mfg_c + exports_c + gini_c + gdp_c + gini_c  + left_right_fa_scaled_c + democracy_c + net_enrollment_corrected_c + oecd*sl_lvl1 +
                                        (time | country),
                                      REML = F,
                                      control = lmerControl(optimizer ="Nelder_Mead")))

testEstimates(model_4_log$analyses)$estimates


imp_log_1 <- model_4_log$analyses[[1]]
imp_log_1_res <- HLMdiag::hlm_resid(imp_log_1, 1)

plot(
  predict(imp_log_1), scale(imp_log_1_res$.resid, center=F),)

DHARMa::plotQQunif(imp_log_1)
lattice::qqmath(imp_log_1, id=.05)
car::influencePlot(imp_log_1, id=T) # 163,310,320,323,324
model_data %>%
  slice(163,310,320,323,324)

check_model(imp_log_1)

# model with outliers remove

`%notin%` <- Negate(`%in%`)

midat_out <- imp_data %>%
  filter(country %notin% c("Qatar", "United Arab Emirates", "Kazakhstan", "Kosovo", "Mauritius")) %>%
  as.mids()

# model with outliers removed
  model_4_out <- with(midat_out, lme4::lmer(co2_lag ~ time + sl_lvl1 + sl_lvl2 + pop_c + gdp_c + mfg_c + exports_c + gini_c + gdp_c + gini_c  + left_right_fa_scaled_c + democracy_c + net_enrollment_corrected_c + oecd*sl_lvl1 +
                                          (time | country),
                                        REML = F,
                                        control = lmerControl(optimizer ="Nelder_Mead")))

  check_model(model_4_out$analyses[[1]])

  # do estimates change?
  testEstimates(model_4_out$analyses)$estimates %>%
    as.data.frame() %>%
    rownames_to_column("var") %>%
    mutate(across(where(is.numeric), round, 3)) %>%
    flextable()
  # different coefficients, diff sign for sl_lvl1, same p values


  # model with outliers removed and log of c02
  model_4_out_log <- with(midat_out, lme4::lmer(co2_log ~ time + sl_lvl1 + sl_lvl2 + pop_c + gdp_c + mfg_c + exports_c + gini_c + gdp_c + gini_c  + left_right_fa_scaled_c + democracy_c + net_enrollment_corrected_c + oecd*sl_lvl1 +
                                              (time | country),
                                            REML = F,
                                            control = lmerControl(optimizer ="Nelder_Mead")))

check_model(model_4_out_log$analyses[[1]])

# robust SE ----

sjPlot::tab_model(imp_1)
mlmhelpr::robust_se(imp_1)

sjPlot::tab_model(imp_8)
mlmhelpr::robust_se(imp_8)

sjPlot::tab_model(imp_17)
mlmhelpr::robust_se(imp_17)
