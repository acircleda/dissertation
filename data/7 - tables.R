# tables and figures

library(tidyverse)
library(flextable)
library(ftExtra)
library(janitor)
library(broom.mixed)
library(mitml)
library(performance)
load("data/final data/final_data.Rdata")
load("models/model_data.Rdata")
# load("data/final data/table_figure_data.Rdata")

all_data <- all_data %>%
  mutate( region = case_when(
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
  ))


# PISA SL table ----

# unfill: https://github.com/tidyverse/tidyr/issues/250
unfill_vec = function(x, direction = c("down", "up")) {
  if (direction[1] == "down") {
    x[which(x[-1] == x[1:(length(x) - 1)]) + 1] = NA
  } else {
    x = rev(unfill(rev(x), direction = "down"))
  }
  x
}

#make country/oecd list)
sl_table_oecd <- all_data %>%
  distinct(country, oecd) %>%
  add_count(country) %>%
  mutate(oecd_status = ifelse(n == 1, oecd,
                              "Both")) %>%
  distinct(country, oecd_status) %>%
  mutate(oecd_status = ifelse(country == "Colombia",
                              "Non-OECD", oecd_status))

sl_table_prep <- all_data %>%
  left_join(sl_table_oecd) %>%
  select(country, oecd_status, region, year, n_total:sl_male) %>%
  rename(n_male = freq_male,
         n_female = freq_female) %>%
  pivot_longer(n_total:sl_male) %>%
  separate(name, into = c("stat", "group"), sep="_")

sl_table_n <- sl_table_prep %>%
  filter(stat == "n")

sl_table_data <- sl_table_prep %>%
  filter(stat == "sl") %>%
  left_join(sl_table_n, by=c("country", "oecd_status", "region", "year", "group")) %>%
  rename(mean = value.x, n = value.y) %>%
  select(-stat.y, -stat.x) %>%
  mutate(group = str_to_title(group),
         data = ifelse(is.na(mean), NA,
                       paste0(scales::comma(n,1), " / ", round(mean,0)))) %>%
  select(-mean, -n) %>%
  distinct(country, oecd_status, year, group, .keep_all=T) %>%
  pivot_wider(names_from=year, values_from=data) %>%
  arrange(country) %>%
  mutate(country = unfill_vec(country),
         region = unfill_vec(region),
         oecd_status = unfill_vec(oecd_status)) %>%
  rename(Country = country,
         Region = region,
         `OECD Status` = oecd_status,
         " " = group) %>%
  relocate(Region, .after=1)



sl_table_final <- sl_table_data %>%
  flextable() %>%
  autofit() %>%
  add_header_row(top=T,
                 values=c("", "", "", "", "Students / Scientific Literacy Score"),
                 colwidths = c(1,1, 1,1,5)) %>%
  align(i=1, part="header", align="center") %>%
  hline(i=seq(3, nrow(sl_table_data), 3)) %>%
  vline(j=4:9, part="body") %>%
  footnote(i=2, j=2,
           value=as_paragraph("Several countries changed from non-OECD to OECD during the 12-year study period: Chile (201), Estonia (2010), Israel (2010), Latvia (2016), Lithuania (2018), Slovenia (2010)"), part="header", ref_symbols="a") %>%
  footnote(i=which(sl_table_data == "China"), j=1,
           value=as_paragraph("China refers to Shanghai (2009, 2012); Beijing, Shanghai, Jiangsu, and Guangdong (2015); and Beijing, Shanghai, Jiangsu, and Zhejiang (2018)"), ref_symbols="b") %>%
  fontsize(size=8, part="footer")



# summary data table for continuous variables ----
var_summary_table <- model_data %>%
  select(co2_lag, sl_total, sl_female, sl_male, pop, gdp, exports, mfg,
         gini, left_right_fa, democracy, corruption, net_enrollment_corrected,
         collectivism_individualism) %>%
  mutate(pop = pop*10000000,
         gdp = gdp*10000) %>%
  ungroup() %>%
  rename("CO~2~ per capita (1 year lag)" = co2_lag,
         "Scientific Literacy (All)" = sl_total,
         "Scientific Literacy (Female)" = sl_female,
         "Scientific Literacy (Male)" = sl_male,
         "Population" = pop,
         "GDP-PPP (US$2015)" = gdp,
         "Exports as % of GDP" = exports,
         "Mfg. as % of GDP" = mfg,
         "Gini (income inequality)" = gini,
         "Left-Right Orientation" = left_right_fa,
         "Democracy Index" = democracy,
         "Corruption Index" = corruption,
         "Net Enrollment" = net_enrollment_corrected,
         "Collectivism-Individualism" = collectivism_individualism) %>%
skimr::skim() %>%
  filter(skim_type != "character") %>%
  select(skim_variable, complete_rate,
         numeric.mean, numeric.sd, numeric.p0,
         numeric.p50, numeric.p100, numeric.hist ) %>%
  mutate(across(where(is.numeric), round, 2)) %>%
  mutate(complete_rate = scales::percent(1-complete_rate,1)) %>%
  rename("Variable" = skim_variable,
         "Percent Missing" = complete_rate,
         "Mean" = numeric.mean,
         "SD" = numeric.sd,
         "Min" = numeric.p0,
         "Median" = numeric.p50,
         "Max"= numeric.p100,
         "Histogram" = numeric.hist) %>%
  mutate(across(where(is.numeric), round, 2))

model_data %>%
  distinct(sdg_region, country, oecd) %>%
  add_count(country) %>%
  mutate(oecd_status = ifelse(n == 1, oecd,
                              "Both")) %>%
  distinct(sdg_region, country, oecd_status) %>%
  mutate(oecd_status = ifelse(country == "Colombia",
                              "Non-OECD", oecd_status)) %>%
  tabyl(sdg_region, oecd_status) %>%
  relocate(`Both`, .after=4) %>%
  rename(Region = 1) %>%
  adorn_totals(c("row", "col")) %>%
  flextable() %>%
  bold(j=5) %>%
  bold(i=nrow(.[["body"]][["dataset"]])) %>%
  bold(j=5, part="header")



# line graphs -----

# co2 ----

co2_line_graph <- model_data %>% select(country, year, co2) %>%
  group_by(country) %>%
  mutate(mean = mean(co2, na.rm=T)) %>%
  ungroup() %>%
  mutate(rank = rank(-mean),
         top = ifelse(rank <= 20, country, NA),
         highlight = ifelse(is.na(top), NA, co2)) %>%
  filter(country != "Brunei Darussalam")

dscale <- viridis::magma(n = 5)
dscale[5] <- "#787c10"

ggplot()+
  geom_line(data=co2_line_graph,
            aes(x=year, y=co2, group=country), color="grey90")+
  geom_line(data=co2_line_graph,
            aes(x=year, y=highlight, group=country,
                color=top), size=1)+
  # ggrepel::geom_label_repel(data = filter(co2_line_graph, year==2018),
  #           aes(x=2020, y=highlight, group=country, label=country,
  #               color=top), hjust=0, size=3)+
  stat_summary(data=co2_line_graph,
               aes(x=year, y=co2, color="mean"),
               geom="line", fun="mean", color="black", size=1.2,
               linetype="11")+
  stat_summary(data=co2_line_graph %>% filter(year==2018),
               aes(x=2018, y=co2, color="mean"), label="Average CO2",
               geom="text", fun="mean", color="black", size=3,
               hjust=-.2)+
  scale_x_continuous(breaks=seq(2006,2018,2))+
  coord_cartesian(clip="off")+
  scale_color_manual(values=dscale, name="Top Emitting Countries",
                     na.translate = F)+
  theme(legend.position = "right",
        panel.background = element_blank())

line_top <- model_data %>%
  select(country, year,  co2, sl_total, sl_female, sl_male,  pop, gdp, ) %>%
  rename("CO2 per capita" = co2,
         "Scientific Literacy (All)" = sl_total,
         "Scientific Literacy (Female)" = sl_female,
         "Scientific Literacy (Male)" = sl_male,
         "Population" = pop,
         "GDP-PPP (US$2015)" = gdp) %>%
  pivot_longer(`CO2 per capita`:`GDP-PPP (US$2015)`) %>%
  mutate(name = fct_inorder(name)) %>%
  ggplot(aes(x=year, y=value))+
  facet_wrap(~name,
             scale="free_y",
             ncol=2,
             labeller = labeller(name = label_wrap_gen(25)))+
  geom_line(aes(group=country), color="grey80")+
  stat_summary(geom="line", fun="mean", na.rm=T,
               color="#FF8200", size=1.2)+
  scale_x_continuous(breaks=seq(2006,2018,3))+
  xlab(NULL)+
  ylab(NULL)+
  theme(panel.background = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(face="bold", vjust=1),
        axis.ticks=element_blank())

line_bottom <- model_data %>%
  select(country, year, exports, mfg,
         gini, right_left, democracy, corruption, net_enrollment_corrected,
         collectivism_individualism) %>%
  rename(
         "Exports as % of GDP" = exports,
         "Mfg. as % of GDP" = mfg,
         "Gini (income inequality" = gini,
         "Right-Left Orientation" = right_left,
         "Democracy Index" = democracy,
         "Corruption Index" = corruption,
         "Net Enrollment" = net_enrollment_corrected,
         "Collectivism-Individualism" = collectivism_individualism) %>%
  pivot_longer(`Exports as % of GDP`:`Collectivism-Individualism`) %>%
  mutate(name = fct_inorder(name)) %>%
  ggplot(aes(x=year, y=value))+
  facet_wrap(~name,
             ncol=2,
             labeller = labeller(name = label_wrap_gen(25)))+
  geom_line(aes(group=country), color="grey80")+
  stat_summary(geom="line", fun="mean", na.rm=T,
               color="#FF8200", size=1)+
  scale_y_continuous(limits=c(0,1), breaks=seq(0,1,.5),
                     labels=scales::percent_format(1))+
  scale_x_continuous(breaks=seq(2006,2018,3))+
  xlab(NULL)+
  ylab(NULL)+
  theme(panel.background = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(face="bold", vjust=1),
        axis.ticks=element_blank())

library(patchwork)
line_top / line_bottom


# correlations -----
library(corrr)
# library(Hmisc)
cor_all <- function(year){
 Hmisc::rcorr(as.matrix(model_data %>%
        filter(year == {{year}}) %>%
        select(-year, -country) %>%
        select(co2_lag, sl_total, pop, gdp, exports, mfg, gini, left_right_fa_scaled, democracy, corruption, net_enrollment_corrected, collectivism_individualism) %>%
        relocate(sl_total, .after=1) %>%
        rename(CO2 = co2_lag,
               `Scientific Literacy` = sl_total,
               `Pop.` = pop,
               GDP = gdp,
               `Exports` = exports,
               `Mfg.` = mfg,
               Gini = gini,
               `Left-Right` = left_right_fa_scaled,
               Democracy = democracy,
               Corruption = corruption,
               `Net Enrollment` = net_enrollment_corrected,
               `Coll.-Individ.` = collectivism_individualism)))
}

cor2006 <- cor_all(2006)
cor2009 <- cor_all(2009)
cor2012 <- cor_all(2012)
cor2015 <- cor_all(2015)
cor2018 <- cor_all(2018)

cor_data <- ((cor2006$r+cor2009$r+cor2012$r+cor2015$r+cor2018$r)/5)
cor_data_df <- ((cor2006$r+cor2009$r+cor2012$r+cor2015$r+cor2018$r)/5) %>%
  as.data.frame() %>%
  rownames_to_column("Variable") %>%
  mutate(across(where(is.numeric), round, 3))

cor_data_p_df <- ((cor2006$P+cor2009$P+cor2012$P+cor2015$P+cor2018$P)/5) %>%
  as.data.frame() %>%
  rownames_to_column("Variable") %>%
  mutate(across(where(is.numeric), round, 3))

#plot of correlations averaged over time
library(corrplot)
corrplot::corrplot(cor_data, diag=F, type="upper", tl.col = 'black',
                   col = COL2('PuOr', 10))


corrplot::corrplot.mixed(cor_data, upper = "circle",
                         tl.pos = "lt", lower.col = "black",
                         number.cex = .85, upper.col = COL2('PuOr', 10),
                         lower="number", tl.col = 'black')

corrplot::corrplot.mixed(cor_data,
                         tl.pos = "lt", lower.col = "black",
                         number.cex = .5, upper.col = COL2('PuOr', 10),
                         lower="number", tl.col = 'black')

# histogram of countries and responses ----

model_data %>%
  group_by(country) %>%
  count(name = "participation") %>%
  group_by(participation) %>%
  count() %>%
  ggplot(aes(y = n, x = participation))+
  geom_col(fill="#FF8200")+
  geom_text(aes(label = n), vjust=-1, size=3, fontface="bold")+
  scale_y_continuous(limits=c(0,60))+
  ylab("Number of\nCountries")+
  xlab("Number of Times Participated")+
  theme(panel.background = element_blank(),
        axis.title.y = element_text(angle=0, vjust=.5, face="italic",
                                    size=10),
        axis.title.x = element_text(face="italic", size=10),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_text(margin = margin(t=-5)))

ggsave(file="figures/participation.jpg", plot=last_plot(),
       height=2, width=5)


# countries included -----

# sl -----

model_data %>%
  ggplot(aes(y=reorder(country, sl_total),
             x=sl_total,
             color=as.factor(year)))+
  geom_point()+
  xlab("PISA Scientific Literacy Score")+
  ylab(NULL)+
  scale_x_continuous(breaks = seq(300, 600, 50))+
  theme(panel.background = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major.x = element_line(color="grey80"),
        panel.grid.major.y = element_line(color="grey90"),
        legend.position = "top",
        legend.background = element_blank(),
        legend.key = element_blank())+
  scale_color_manual(values=c(
    "#FED535", "#ABC178", "#8D2048", "#2197A9", "#FF8200"
  ), name="Year")

anova(model_1, model_2) -> model_2_fit

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

# world map

world <- map_data("world")

# check for missing countries

model_data %>% distinct(country) %>%
  rownames_to_column("id") %>%
  left_join(world %>% distinct(country, .keep_all=T) %>%
              select(country) %>%
              rownames_to_column("id2"),
            by = c("country" = "country")) %>% view()

# missing: Hong Kong, Macao, United Kingdom, United States, Trinidad and Tobago, Brunei Darussalam

world_updated <- world %>%
  mutate(country = case_when(
    region == "China" & subregion == "Hong Kong" ~ "Hong Kong",
    region == "China" & subregion == "Macao" ~ "Macao",
    region == "UK" ~ "United Kingdom",
    region == "USA" ~ "United States",
    region == "Trinidad" ~ "Trinidad and Tobago",
    region == "Tobago" ~ "Trinidad and Tobago",
    region == "Brunei" ~ "Brunei Darussalam",
    TRUE ~ region
  )) %>%
  left_join(model_data %>% distinct(country) %>%
              mutate(cat = "Included in Analysis"), by="country") %>%
  mutate(cat = ifelse(is.na(cat), "Not Included", cat))

ggplot(data=world_updated, aes(x=long, y=lat))+
  geom_polygon(aes(group=group))+
  geom_polygon(aes(group=group, fill=cat),
               color="white")+
  scale_fill_manual(values=c("#FF8200", "#A7A9AC"), name=NULL)+
  theme_void()+
  theme(legend.position = c(.15,.4),
        axis.text = element_blank(),
        axis.title = element_blank())


ggsave(file="figures/coverage map.jpg", plot=last_plot(),
       width=6, height=3)



# diagnostic plots ----

# models 1_2
model_1_res <- HLMdiag::hlm_resid(model_1) %>%
  select(.resid, .fitted) %>%
  mutate(model = "Model 1")
model_2_res <- HLMdiag::hlm_resid(model_2) %>%
  select(.resid, .fitted) %>%
  mutate(model = "Model 2 with Quadratic\nEffect for Time")

model_12_scatter <- rbind(model_1_res,
      model_2_res) %>%
  mutate(model = fct_inorder(model)) %>%
  ggplot(aes(y=.resid, x=.fitted))+
  facet_wrap(~model, nrow=1, scales="free")+
  geom_point()+
  geom_smooth() +
  ylab("Residuals") +
  xlab("Fitted Values")+
  theme(panel.background = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(face="bold"))

model_12_qq <- rbind(model_1_res,
      model_2_res) %>%
  mutate(model = fct_inorder(model)) %>%
  ggplot(aes(sample=.resid))+
  facet_wrap(~model, nrow=1, scales="free")+
  geom_qq()+
  geom_qq_line()+
  ylab("Sample Quantiles") +
  xlab("Theoretical Quantiles")+
  theme(panel.background = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(face="bold"))

model_12_scatter/model_12_qq

ggsave(plot=last_plot(), file="figures/model_12_diagnostics.jpg", width=6, height=3)



# residuals of some of these models
model_3_res <- HLMdiag::hlm_resid(model_3_nosl) %>%
  mutate(model = "Model 3")
model_3_noqatar_res <- HLMdiag::hlm_resid(model_3_noqatar) %>%
  mutate(model = "Mdel 3 - No Qatar")
model_3_log_res <- HLMdiag::hlm_resid(model_3_log) %>%
  mutate(model = "Model 3 - Logged DV") %>%
  rename(co2_lag = co2_log)


# linear/homo plot
rbind(model_3_res,
      model_3_noqatar_res,
      model_3_log_res) %>%
  mutate(model = fct_inorder(model)) %>%
  ggplot(aes(y=.resid, x=.fitted))+
  facet_wrap(~model, nrow=1, scales="free")+
  geom_point()+
  geom_smooth() +
  ylab("Residuals") +
  xlab("Fitted Values")+
  theme(panel.background = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(face="bold"))

ggsave(plot=last_plot(), file="figures/linearity_model_3.jpg", width=6, height=3)

# make qq plots ----

rbind(model_3_res,
      model_3_noqatar_res,
      model_3_log_res) %>%
  mutate(model = fct_inorder(model)) %>%
  ggplot(aes(sample=.resid))+
  facet_wrap(~model, nrow=1, scales="free")+
  geom_qq()+
  geom_qq_line()+
  ylab("Sample Quantiles") +
  xlab("Theoretical Quantiles")+
  theme(panel.background = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(face="bold"))

ggsave(plot=last_plot(), file="figures/qqmodel_3.jpg", width=6, height=3)

# table of models ----

# models 0, 1, 2 ----

model_table_prep <- function(x, number){

  name = paste0("estimate_", number)

  tab <- tidy(x) %>%
    select(3, 4, 5, 8) %>%
    rename_at(vars(2,3,4), paste0, "_", number)


  b1 <- multilevelR2(x, "MVP") %>%
    as.data.frame() %>%
    mutate(term = "marginal r2") %>%
    mutate(!!name := round(`.`, 3)) %>%
    select(term, !!name)

  b2 <- glance(x) %>%
    select(3:5) %>%
    pivot_longer(1:3, names_to="term", values_to=name)

  b3 <- performance::icc(x)[["ICC_adjusted"]] %>%
    as.data.frame() %>%
    mutate(term = "ICC",
           !!name := round(`.`, 3)) %>%
    select(term, !!name)

  nobs <- glance(x)[1] %>%
    mutate(term = "nobs", .before=1,
           !!name := nobs) %>%
    select(term, !!name)

  bind_rows(tab, b1, b3, b2) %>%
    mutate(across(where(is.numeric), round, 3)) %>%
    bind_rows(nobs)
}

model_0_tab <- model_table_prep(model_0, 0)
model_1_tab <- model_table_prep(model_1, 1)
model_2_tab <- model_table_prep(model_2, 2)
model_3_tab <- model_table_prep(model_3_nosl, 3)


c(model_2_tab$term[1:3],
  model_3_tab$term[3:4],
  model_2_tab$term[4:16]) %>%
  as.data.frame() %>%
  rename(term = 1) %>%
  left_join(model_0_tab) %>%
  left_join(model_1_tab) %>%
  left_join(model_2_tab) %>%
  left_join(model_3_tab) %>%
  cbind(Variable = c("(Intercept)",
                   "Time",
                   "$Time^2$",
                   "Scientific Literacy (within)",
                   "Scientific Literacy (between)",
                   "$\\tau_{00}$", #sd__(Intercept)
                   "$\\rho_{01}$", #cor__(Intercept).time
                   "$\\rho_{02}$", #cor__(Intercept).I(time^2)
                   "$\\tau_{11}$", #sd__time
                   "$\\rho_{12}$", #cor__time.I(time^2)
                   "$\\tau_{22}$",  #sd__I(time^2)
                   "$\\sigma$", #sd__Observation
                   "Marginal $R^2$",
                   "ICC",
                   "Log Likelihood",
                   "AIC",
                   "BIC",
                   "Observations"
)) %>%
  relocate(Variable, .before=1) %>%
  # fix p = 0
  mutate_at(vars(contains("p.value")), ~ifelse(.x == 0, "< 0.001", .x)) %>%
  #fix NAN
  mutate_at(vars(contains("estimate")), ~ifelse(is.nan(.x), "-1.00", .x)) %>%
  select(-term) %>%
  flextable() %>%
  theme_zebra(
    odd_header = "transparent",
    odd_body = "#F6F6F6",
    even_header = "transparent",
    even_body = "transparent"
  ) %>%
  hline(1, part="header") %>%
  hline(2, part="footer") %>%
  hline(5, border=officer::fp_border(width=1.5)) %>% #random effects
  hline(12, border=officer::fp_border(width=1.5)) %>% # model fit
  hline(17, border=officer::fp_border(width=1.5)) %>% # obs
  add_header_row(values = c("", # add model names
                            "Model 0: Unconditional Model",
                            "Model 1: Random Effect for Time",
                            "Model 2: Non-Linear Effect for Time",
                            "Model 3: Conditional Model"),
                 colwidths = c(1,3,3,3,3))  %>%
  colformat_double(i=18, digits = 0) %>%
  bold(~ parse_number(p.value_0) < .05,4) %>%
  bold(~ parse_number(p.value_1) < .05,7) %>%
  bold(~ parse_number(p.value_2) < .05,10) %>%
  bold(~ parse_number(p.value_3) < .05,13) %>%
  colformat_md(j=1) %>%
  set_header_labels( # rename columns
    values = list(estimate_0 = "Estimate",
                  std.error_0 = "SE",
                  p.value_0 = "p",
                  estimate_1 = "Estimate",
                  std.error_1 = "SE",
                  p.value_1 = "p",
                  estimate_2 = "Estimate",
                  std.error_2 = "SE",
                  p.value_2 = "p",
                  estimate_3 = "Estimate",
                  std.error_3 = "SE",
                  p.value_3 = "p")) %>%
  italic(i=2, j=2:ncol(.[["header"]][["dataset"]]), part="header") %>%
  align(align="center", part="header") %>%
  valign(valign="top", part="header")  -> model_table_1

save_as_docx(model_table_1, path = "figures/model_table_1.docx",
             pr_section = officer::prop_section(
               page_size = officer::page_size(orient = "landscape")))


# models 1 log, 3, 4 ----

model_log_table_prep <- function(x, number){

  name = paste0("estimate_", number)

  tab <- tidy(x) %>%
    mutate(exp = ifelse(effect == "fixed" & term != "(Intercept)",
             (exp(estimate)-1)*100, NA),
           .after="estimate") %>%
    select(3, 4, 5, 6, 9) %>%
    rename_at(vars(2,3,4, 5), paste0, "_", number)


  b1 <- multilevelR2(x, "MVP") %>%
    as.data.frame() %>%
    mutate(term = "marginal r2") %>%
    mutate(!!name := round(`.`, 3)) %>%
    select(term, !!name)

  b2 <- glance(x) %>%
    select(3:5) %>%
    pivot_longer(1:3, names_to="term", values_to=name)

  b3 <- performance::icc(x)[["ICC_adjusted"]] %>%
    as.data.frame() %>%
    mutate(term = "ICC",
           !!name := round(`.`, 3)) %>%
    select(term, !!name)

  nobs <- glance(x)[1] %>%
    mutate(term = "nobs", .before=1,
           !!name := nobs) %>%
    select(term, !!name)

  bind_rows(tab, b1, b3, b2) %>%
    mutate(across(where(is.numeric), round, 3)) %>%
    bind_rows(nobs)
}

model_1_log_tab <- model_log_table_prep(model_1_log, 1)
model_3_log_tab <- model_log_table_prep(model_3_log_noqatar, 2)



# imputed models (with logs)
imp_model_table_prep <- function(x, number){

  name = paste0("estimate_", number)

  tidy.mitml.fe <- function(x, ...) {
    out <- as.data.frame(testEstimates(x$analyses)$estimates[, c(1:2, 5)])
    colnames(out) <- c("estimate", "std.error", "p.value")
    out$term <- row.names(out)
    out <- out %>%
      mutate(exp = ifelse(term != "(Intercept)",
                          (exp(estimate)-1)*100,
                          NA), .after="estimate")
    return(out)
  }

  tidy.mitml.re <- function(x, ...) {
    estimates <- testEstimates(x$analyses, extra.pars = T)$extra.pars %>% sqrt() %>% as.data.frame() %>%
      filter(!is.nan(Estimate))
    term <- c("sd__(Intercept)",  "sd__time","sd__Observation", "ICC")
    data.frame(term, estimates) %>%
      rename(estimate = Estimate)
  }


  taus <- lapply(x$analyses, FUN = function(x)
    as.data.frame(VarCorr(x)) %>%
      select(sdcor) %>%
      slice(3) %>%
      pull())

  tau <- mean(taus %>% unlist) %>%
    as.data.frame() %>%
    mutate(estimate = round(`.`, 3),
           term = "cor__(Intercept).time") %>%
    select(term, estimate)


  b1 <- multilevelR2(x$analyses, "sb") %>%
    as.data.frame() %>%
    mutate(estimate = round(`.`, 3),
           term = "marginal r2") %>%
    select(term, estimate)

  b2 <- tribble(~term, ~estimate,
                "logLik", mean(sapply(x$analyses, logLik)),
                "AIC", mean(sapply(x$analyses, AIC)),
                "BIC", mean(sapply(x$analyses, BIC)))

  nobs <- median(sapply(x$analyses, nobs)) %>%
    as.data.frame() %>%
    mutate(term = "nobs", .before=1,
           estimate := `.`) %>%
    select(term, estimate)

  nimp <- length(x$analyses) %>%
    as.data.frame() %>%
    mutate(term = "nimp", .before=1,
           estimate := `.`) %>%
    select(term, estimate)

  bind_rows(tidy.mitml.fe(x), tidy.mitml.re(x) %>% slice(1:3),
            tau, tidy.mitml.re(x) %>% slice(4),
            b1, b2, nobs, nimp) %>%
    relocate(term, .before=1) %>%
    mutate(across(where(is.numeric), round, 3)) %>%
    rename_at(vars(2,3,4,5), paste0, "_", number)
}




model_4_tab <- imp_model_table_prep(model_4_noqatar_log, 4)


model_4_tab$term[1:25] %>%
  as.data.frame() %>%
  rename(term = 1) %>%
  left_join(model_1_log_tab) %>%
  left_join(model_3_log_tab) %>%
  left_join(model_4_tab) %>%
  cbind(Variable = c("(Intercept)",
                     "Time",
                     "Scientific Literacy (within)",
                     "Scientific Literacy (between)",
                     "Population",
                     "GDP",
                     "Manufacturing",
                     "Exports",
                     "Gini",
                     "Left-Right Orientation",
                     "Democracy Index",
                     "Net Enrollment",
                     "OECD",
                     "Scientific Literacy (within) x OECD",
                     "$\\tau_{00}$",
                     "$\\tau_{11}$",
                     "$\\sigma$",
                     "$\\rho_{01}$",
                     "ICC",
                     "$Marginal R^2$",
                     "Log Likelihood",
                     "AIC",
                     "BIC",
                     "Observations",
                     "Imputations"
  )) %>%
  relocate(Variable, .before=1) %>%
  # fix p = 0
  mutate_at(vars(contains("p.value")), ~ifelse(.x == 0, "< 0.001", .x)) %>%
  #fix NAN
  mutate_at(vars(contains("estimate")), ~ifelse(is.nan(.x), "-1.00", .x)) %>%
  select(-term) %>%
  flextable() %>%
  theme_zebra(
    odd_header = "transparent",
    odd_body = "#F6F6F6",
    even_header = "transparent",
    even_body = "transparent"
  ) %>%
  hline(1, part="header") %>%
  hline(2, part="footer") %>%
  hline(14, border=officer::fp_border(width=1.5)) %>% #random effects
  hline(23, border=officer::fp_border(width=1.5)) %>% # model fit
  add_header_row(values = c("", # add model names
                            "Model 1: Random Effect for Time\n(Log of CO~2~)",
                            "Model 3: Conditional Model\n(Log of CO~2~)",
                            "Model 4: Conditional Model with Covariates\n(Log of CO~2~)"),
                 colwidths = c(1,4,4,4)) %>%
  bold(~ parse_number(p.value_1) < .05,5) %>%
  bold(~ parse_number(p.value_2) < .05,9) %>%
  bold(~ parse_number(p.value_4) < .05,13) %>%
  colformat_md(part="all") %>%
  set_header_labels( # rename columns
    values = list(
                  estimate_1 = "Estimate",
                  exp_1 = "%Δ",
                  std.error_1 = "SE",
                  p.value_1 = "p",
                  estimate_2 = "Estimate",
                  exp_2 = "%Δ",
                  std.error_2 = "SE",
                  p.value_2 = "p",
                  estimate_4 = "Estimate",
                  exp_4 = "%Δ",
                  std.error_4 = "SE",
                  p.value_4 = "p")) %>%
  italic(i=2, j=2:ncol(.[["header"]][["dataset"]]), part="header") %>%
  align(align="center", part="header") %>%
  valign(valign="top", part="header") %>%
  footnote(i=18, j=10, ref_symbols = c("a"), part="body",
           value=as_paragraph(
             c("The correlation coefficient is based on mean across imputed models"))) %>%
  footnote(i=21:23, j=10, ref_symbols = c("b"),
           value=as_paragraph(
             c("Fit statistics based on mean across imputed models")
           )) %>%
  colformat_double(i=24:25, digits = 0) -> model_table_2

save_as_docx(model_table_2, path = "figures/model_table_2.docx",
             pr_section = officer::prop_section(
               page_size = officer::page_size(orient = "landscape")))

# male female models ----

model_female_tab <- imp_model_table_prep(model_female, "_f") %>%
  mutate(term = str_remove(term, "_female"))
model_male_tab <- imp_model_table_prep(model_male, "_m") %>%
  mutate(term = str_remove(term, "_male"))
z_tab <- cbind(term = model_4_tab$term[1:14], z) %>%
            mutate(p = pnorm(z)) %>%
  mutate(across(where(is.numeric), round, 3))


model_4_tab$term[1:25] %>%
  as.data.frame() %>%
  rename(term = 1) %>%
  left_join(model_female_tab) %>%
  left_join(model_male_tab) %>%
  left_join(z_tab) %>%
  cbind(Variable = c("(Intercept)",
                     "Time",
                     "Scientific Literacy (within)",
                     "Scientific Literacy (between)",
                     "Population",
                     "GDP",
                     "Manufacturing",
                     "Exports",
                     "Gini",
                     "Left-Right Orientation",
                     "Democracy Index",
                     "Net Enrollment",
                     "OECD",
                     "Scientific Literacy (within) x OECD",
                     "$\\tau_{00}$",
                     "$\\tau_{11}$",
                     "$\\sigma$",
                     "$\\rho_{01}$",
                     "ICC",
                     "$Marginal R^2$",
                     "Log Likelihood",
                     "AIC",
                     "BIC",
                     "Observations",
                     "Imputations"
  )) %>%
  relocate(Variable, .before=1) %>%
  # fix p = 0
  mutate_at(vars(contains("p.value")), ~ifelse(.x == 0, "< 0.001", .x)) %>%
  #fix NAN
  mutate_at(vars(contains("estimate")), ~ifelse(is.nan(.x), "-1.00", .x)) %>%
  select(-term) %>%
  flextable() %>%
  theme_zebra(
    odd_header = "transparent",
    odd_body = "#F6F6F6",
    even_header = "transparent",
    even_body = "transparent"
  ) %>%
  hline(1, part="header") %>%
  hline(2, part="footer") %>%
  hline(14, border=officer::fp_border(width=1.5)) %>% #random effects
  hline(23, border=officer::fp_border(width=1.5)) %>% # model fit
  add_header_row(values = c("", # add model names
                            "Model for Female Students",
                            "Model for Male Students",
                            "Coefficient Tests of Equality"),
                 colwidths = c(1,4,4,2)) %>%
  bold(~ parse_number(p.value__f) < .05,5) %>%
  bold(~ parse_number(p.value__m) < .05, 9) %>%
  bold(~ p < .05,11) %>%
  colformat_md(part="all")  %>%
  set_header_labels(  #rename columns
    values = list(estimate__f = "Estimate",
                  exp__f = "%Δ",
                  std.error__f = "SE",
                  p.value__f = "p",
                  estimate__m = "Estimate",
                  exp__m = "%Δ",
                  std.error__m = "SE",
                  p.value__m = "p")) %>%
  italic(i=2, j=2:ncol(.[["header"]][["dataset"]]), part="header") %>%
  align(align="center", part="header") %>%
  valign(valign="top", part="header") %>%
  footnote(i=18, j=1, ref_symbols = c("a"), part="body",
           value=as_paragraph(
             c("The correlation coefficient is based on mean across imputed models"))) %>%
  footnote(i=21:23, j=1, ref_symbols = c("b"),
           value=as_paragraph(
             c("Fit statistics based on mean across imputed models")
           )) %>%
  colformat_double(i=24:25, digits = 0) -> model_table_3

save_as_docx(model_table_3, path = "figures/model_table_3.docx",
             pr_section = officer::prop_section(
               page_size = officer::page_size(orient = "landscape")))


# discussion graphics ----

# plot of slwithin and bw
HLMdiag::hlm_resid(model_4_noqatar_log$analyses[[1]]) %>%
  #augment(model_3_log) %>%
  ggplot(aes(x = sl_lvl1+sl_lvl2, y = co2_log, color = country)) +
  # Add points
  # geom_point(size = 0.5, alpha = 0.2) +
  # Add within-cluster lines
  geom_smooth(aes(y = .fitted),
              method = "lm", se = FALSE, size = .5) +
  # Add group means
  # stat_summary(aes(x = sl_lvl2, y = .fitted,
  #                  fill = country),
  #              color = "red",  # add border
  #              fun = mean,
  #              geom = "point",
  #              shape = 24,
  #              # use triangles
  #              size = 2.5) +
  # Add between coefficient
  geom_smooth(aes(x = sl_lvl2, y = .fitted),
              method = "lm", se = FALSE,
              color = "black") +
  labs(y = "Log(CO2)",
       x = "Scientific Literacy")+
  annotate(geom="text", x=300, y=3, label=
             "Colored Lines: Within-country effects",
           hjust=0, size=3)+
  annotate(geom="text", x=300, y=2.8, label=
             "  (Each line represents one country)",
           hjust=0, size=2.3)+
  annotate(geom="text", x=300, y=2.6, label=
             "Black Line: Between-country effect",
           hjust=0, size=3)+
  theme_classic()+
  scale_color_manual(values=rep("#FF8200",72))+
  theme(legend.position = "none",
        axis.title.y = element_text(angle = 0, face="italic", vjust=.5))

ggsave(file="figures/betweenwithin.jpg", plot=last_plot(),
       height=3, width=6)

model_data %>%
  mutate(region = ifelse(region == "Latin America/Caribbean/Other",
                         "Latin America/Caribbean/\nOther", as.character(region))) %>%
  filter(region=="Eastern Asia") %>%
  ggplot(aes(y=sl_total, x=year, color=country))+
  geom_line()+
  facet_wrap(~region, ncol=4)
  scale_x_continuous(breaks=seq(2006,2018,3),
                     labels=c("'06","'09","'12","'15","'18"))+
  theme_classic()+
  theme(legend.position = "none")+
  scale_color_manual(values=rep("grey80", 82))+
  ylab("Scientific Literacy")+
  xlab("Year")+
  theme(strip.background = element_blank(),
        strip.text = element_text(face="bold"))

ggsave(file="figures/slchanges.jpg", plot=last_plot(),
       height=3, width=6)



model_data %>%
  filter(country != "Qatar") %>%
  mutate(gdp = gdp*10000) %>%
  ggplot(aes(y=co2, x=sl_total, size=gdp, color=region))+
  facet_wrap(~oecd)+
  geom_point(alpha=.5)+
  geom_hline(yintercept = mean(model_data$co2, na.rm=T)) +
  geom_vline(xintercept = mean(model_data$sl_total, na.rm=T))+
  geom_text(data=tribble(~y, ~x, ~oecd, ~gdp, ~label,
                         8.5, 300, "Non-OECD", 0, "Average CO2 Per Capita",
                         14, mean(model_data$sl_total)+1, "Non-OECD", 0,
                         "Average Scientific Literacy"),
            aes(x=x,y=y, label=label), size=3, hjust=0, color="black")+
  xlab("Scientific Literacy")+
  ylab("CO2 Per Capita")+
  #scale_size_continuous(name="GDP Per Capita")+
  #scale_color_discrete(name="Region")+
  guides(colour = guide_legend(override.aes = list(size=3)))+
  scale_size(breaks=c(1000,seq(10000,100000,25000), 100000),
             guide="legend",
             name="GDP Per Capita")+
  scale_color_manual(values=c(
    "#8D2048", #MENA
    "#FED535", #LA
    "#FF8200", #western
    "#00746F", #east europe
    "#EE3E80", #e asia
    "#ABC178", #s europe
    "#2197A9", # s e asia
    "#754A7E" # c asia
    ), name="Region")+
    theme_classic()+
    theme(legend.key = element_blank(),
          strip.background = element_blank(),
          strip.text = element_text(face="bold"))


ggsave(file="figures/co2slgdp.jpg", plot=last_plot(),
       height=6, width=8)

# save data ====
save(sl_table_final, var_summary_table, cor_data, model_2_fit_table, model_table_1, model_table_2, model_table_3, file="data/final data/table_figure_data.Rdata")



