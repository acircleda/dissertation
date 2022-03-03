# tables and figures

library(tidyverse)
library(flextable)
load("data/final data/final_data.Rdata")

model_data <- all_data %>%
  drop_na(sl_total) %>%
  select(1:6, time, oecd, sdg_region, pop, gdp, exports, mfg, gini, left_right, democracy, corruption, net_enrollment_corrected, collectivism_individualism, contains("sl"), contains("_c"))


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
  select(country, oecd_status, sdg_region, year, n_total:sl_male) %>%
  rename(n_male = freq_male,
         n_female = freq_female) %>%
  pivot_longer(n_total:sl_male) %>%
  separate(name, into = c("stat", "group"), sep="_")

sl_table_n <- sl_table_prep %>%
  filter(stat == "n")

sl_table_data <- sl_table_prep %>%
  filter(stat == "sl") %>%
  left_join(sl_table_n, by=c("country", "oecd_status", "sdg_region", "year", "group")) %>%
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
         sdg_region = unfill_vec(sdg_region),
         oecd_status = unfill_vec(oecd_status)) %>%
  rename(Country = country,
         Region = sdg_region,
         `OECD Status` = oecd_status,
         " " = group) %>%
  relocate(Region, .after=1)



sl_table_data %>%
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
         gini, left_right, democracy, corruption, net_enrollment_corrected,
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
         "Gini (income inequality" = gini,
         "Left-Right Orientation" = left_right,
         "Democracy Index" = democracy,
         "Corruption Index" = corruption,
         "Net Enrollment" = net_enrollment_corrected,
         "Collectivism-Individualism" = collectivism_individualism) %>%
skimr::skim() %>%
  filter(skim_type != "character") %>%
  select(skim_variable, complete_rate,
         numeric.mean, numeric.sd, numeric.p0,
         numeric.p50, numeric.p100, numeric.hist ) %>%
  mutate(across(where(is.numeric), round, 3)) %>%
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

var_summary_table %>% flextable() %>%
  ftExtra::colformat_md()

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

cor_all <- function(year){
  cor(model_data %>%
        filter(year == year) %>%
        select(-year, -country) %>%
        select(
               -c(contains("freq"), co2, co2_lag2, co2_lag3,
                  oecd, sdg_region, time, contains("lvl"),
                  contains("_c"), contains("male"), contains("female")
        )) %>%
        relocate(sl_total, .after=1),
      use="pairwise.complete.obs")
}

cor2006 <- cor_all(2006)
cor2009 <- cor_all(2009)
cor2012 <- cor_all(2012)
cor2015 <- cor_all(2015)
cor2018 <- cor_all(2018)

cor_data <- ((cor2006+cor2009+cor2012+cor2015+cor2018)/5)
cor_data_df <- ((cor2006+cor2009+cor2012+cor2015+cor2018)/5) %>%
  as.data.frame() %>%
  rownames_to_column("Variable") %>%
  mutate(across(where(is.numeric), round, 3))

#plot of correlations averaged over time
library(corrplot)
corrplot::corrplot(cor_data, diag=F, type="upper", tl.col = 'black',
                   col = COL2('PuOr', 10))


# histograms and md? ------

# geo line graph ------

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


# save data ====
save(sl_table_data, var_summary_table, cor_data, file="data/final data/table_figure_data.Rdata")



