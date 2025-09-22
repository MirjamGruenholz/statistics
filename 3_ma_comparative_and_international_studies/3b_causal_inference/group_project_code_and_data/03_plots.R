#plots

rm(list=ls())

library(ggswissmaps)
library(ggplot2)
library(dplyr)
library(ggforce)
library(ggrepel)
library(RColorBrewer)
install.packages("ggtext")

#preparing helper vectors with relevant parties, cantons and municipality codes
green_parties <- c("GPS", "GLP")
relevant_cantons <- c("be", "so", "ag")
akw_sites <- c(668, 2572, 4304, 4311)
km03_muehleberg <- c(670, 351, 663, 360, 309, 664, 671, 662, 667)
km03_goesgen <- c(2585, 2576, 2495, 2497, 2573)
km03_beznau <- c(4047, 4320, 4323, 4309, 4303)
km03_leibstadt <- c(4307, 4313, 4184, 4176)
km03_municipalities <- c(akw_sites, 
                         km03_muehleberg, km03_goesgen,
                         km03_beznau, km03_leibstadt)

# Catchment area Muehleberg -------------------------------------------------

gemeinden <- shp_df[[4]]

#Coordinates of catchment area

bezirke_muehleberg <-  c(243, 246, 1005, 1006)

km03_muehleberg <- c(668, 670, 351, 663, 360, 309, 664, 671, 662, 667)

km10_muehleberg <- c(0671, 665, 666, 668, 355, 307, 
                     311, 312, 304, 301, 302, 498, 499, 500, 493,
                     2258, 2265, 2275, 2276, 2259, 2260, 2274, 2260, 2270, 2278, 2277, 2264, 2262, 2266, 
                     2295, 2309, 2308, 2391)

muehleberg <- c(km03_muehleberg, km10_muehleberg)

mittelland <-  gemeinden %>%
  filter(BZNR %in% bezirke_muehleberg) %>% 
  mutate(treatment = as.factor(case_when(GMDNR %in% km03_muehleberg ~ 1,
                                         GMDNR %in% km10_muehleberg ~ 2,
                                         TRUE ~ 3)))

#coordiantes of Muehleberg

muehleberg_standort <- gemeinden %>%
  filter(GMDNR == 668) %>% 
  summarise(long_muehhleberg = mean(long),
            lat_muehleberg = mean(lat)) 

#names area Muehleberg

names_mittelland <- mittelland %>% 
  filter(GMDNR %in% km03_muehleberg) %>% 
  group_by(GMDNR, group, KTNR) %>% 
  summarise(long = mean(long), 
            lat = mean(lat)) %>% 
  mutate(muehleberg = ifelse(GMDNR == 668, 1,0),
         muehleberg_name = ifelse(GMDNR == 668, "Mühleberg",""))

#Plot

ggplot(mittelland, aes(x = long, y = lat, group = group)) +
  geom_path() +
  coord_equal() +
  geom_polygon(aes(x = long, y = lat, fill = as.factor(treatment), group = group), color = "white") +
  xlab("") +
  ylab("") +
  theme_minimal() +
  labs(title = "")+
  scale_fill_brewer(palette = "Blues", direction = -1, labels=c("Main units (3km)","Secondary units (10km)","Surrounding districts")) +
  geom_label_repel(data = names_mittelland, aes(label = muehleberg_name), size = ifelse(names_mittelland$GMDNR == 668, 4,2), label.padding = 0.5,
                   label.r = 0.5) +
  theme(legend.position = "bottom", 
        legend.title = element_blank(),
        legend.text = element_text(size = 14))


# Coefplot: Main (Hypotheses 1 & 2) ---------------------------------------------------------------------

library(lfe)
library(broom)

load("data_spillover.RData")

preparingplot <- function(felmobject, party, group){
  d <- tidy(felmobject) %>% 
    mutate(low = estimate - 2*std.error,
           high = estimate + 2*std.error,
           partei = party,
           group = group)
  return(d)
}

# Subset data for AG/SO Green & GLP
data_3km_glp_as <- filter(data_spillover, kanton == "ag" & partei_name == "GLP" | kanton == "so"& partei_name == "GLP" )
data_3km_gps_as <- filter(data_spillover, kanton == "ag" & partei_name == "GPS" | kanton == "so"& partei_name == "GPS" )

# OLS AG/SO with clustered SE: 
fe0_3km_glp_as <- felm(parteistaerke ~ time*treatment | 0 | 0 | code, data = data_3km_glp_as)
fe0_3km_gps_as <- felm(parteistaerke ~ time*treatment | 0 | 0 | code, data = data_3km_gps_as)

coef_main_glp_ols_as <- preparingplot(fe0_3km_glp_as, "GLP", group = "Aargau & Solothurn")
coef_main_gps_ols_as <- preparingplot(fe0_3km_gps_as, "GPS", group = "Aargau & Solothurn")

main_as <- bind_rows(coef_main_glp_ols_as,
                  coef_main_gps_ols_as)

main_as <- main_as %>% 
  mutate(Coef = case_when(
    term == "(Intercept)" ~ 1,
    term == "treatment" ~ 2,
    term == "time1" ~ 3, 
    term == "time1:treatment" ~ 4 ))


labels <- c("Intercept", "Treatment coef.", "Time coef.", "Diff-in-Diff coef.\n(ATET)")
main_as$partei <- factor(main_as$partei, levels = c("GPS", "GLP"))


main_as %>%
  ggplot(aes(x = estimate, y = Coef, color = partei, shape = partei)) +
  geom_point(position = position_dodge(width = 0.5)) +
  geom_label(aes(label = round(estimate, digits = 2)), position = position_dodge(width = 1), show.legend = F) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_errorbarh(aes(xmin = low, xmax = high, height = 0), position = position_dodge(width = 0.5)) +
  scale_y_continuous(breaks = seq(1,4,1), labels = labels) +
  scale_x_continuous(breaks = seq(-2,6,2), labels = c("-2%", "0", "2%", "4%", "6%")) +
  scale_colour_manual(values = c("#1269b0", "#ff842c")) +
  ylab("") +
  xlab("") +
  theme_minimal() +
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 12),
        legend.text = element_text(size = 12))


# Coefplot: Main (Hypothesis 3) ---------------------------------------------------------------------

# Subset data for BE Green & GLP
data_3km_glp_be <- filter(data_spillover, kanton == "be" & partei_name == "GLP")
data_3km_gps_be <- filter(data_spillover, kanton == "be" & partei_name == "GPS")

# OLS BE with clustered SE: 
fe0_3km_glp_be <- felm(parteistaerke ~ time*treatment | 0 | 0 | code, data = data_3km_glp_be)
fe0_3km_gps_be <- felm(parteistaerke ~ time*treatment | 0 | 0 | code, data = data_3km_gps_be)

coef_main_glp_ols_be <- preparingplot(fe0_3km_glp_be, "GLP", group = "Bern")
coef_main_gps_ols_be <- preparingplot(fe0_3km_gps_be, "GPS", group = "Bern")

main_be <- bind_rows(coef_main_glp_ols_be,
                     coef_main_gps_ols_be)

main_be <- main_be %>% 
  mutate(Coef = case_when(
    term == "(Intercept)" ~ 1,
    term == "treatment" ~ 2,
    term == "time1" ~ 3, 
    term == "time1:treatment" ~ 4 ))


labels <- c("Intercept", "Treatment coef.", "Time coef.", "Diff-in-Diff coef.\n(ATET)")
main_be$partei <- factor(main_as$partei, levels = c("GPS", "GLP"))


main_be %>%
  ggplot(aes(x = estimate, y = Coef, color = partei, shape = partei)) +
  geom_point(position = position_dodge(width = 0.5)) +
  geom_label(aes(label = round(estimate, digits = 2)), position = position_dodge(width = 1), show.legend = F) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_errorbarh(aes(xmin = low, xmax = high, height = 0), position = position_dodge(width = 0.5)) +
  scale_y_continuous(breaks = seq(1,4,1), labels = labels) +
  scale_x_continuous(breaks = seq(-2,6,2), labels = c("-2%", "0", "2%", "4%", "6%")) +
  scale_colour_manual(values = c("#1269b0", "#ff842c")) +
  ylab("") +
  xlab("") +
  theme_minimal() +
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 12),
        legend.text = element_text(size = 12))



# Parallel trends ---------------------------------------------------------------

load("all_gemeinden.RData")

partrend <- all_gemeinden %>% 
  mutate(treatment = as.factor(ifelse(code %in% km03_municipalities & partei_name %in% green_parties,1,0))) %>% 
  filter(kanton %in% relevant_cantons,
         partei_name %in% green_parties) %>% 
  mutate(partei_name = as.factor(partei_name)) %>% 
  group_by(kanton, treatment, partei_name, jahr) %>% 
  summarise(ci = list(mean_cl_normal(parteistaerke) %>%
                        rename(mean=y, Lower=ymin, Upper=ymax))) %>%
  unnest(cols = c(ci))

partrend$partei_name <- factor(partrend$partei_name, levels = c("GPS", "GLP"))

#Plot

# Facet label names for kanton variable
kanton.labs <- c("Aargau", "Bern", "Solothurn")
names(kanton.labs) <- c("ag", "be", "so")

ggplot(data = partrend, aes(x = jahr, y = mean, color = treatment, group = treatment)) +
  geom_point(position = position_dodge(width=0.2)) +
  geom_line() +
  geom_errorbar(aes(ymin=Lower, ymax=Upper), width=0.1, position = position_dodge(width=0.2)) +
  scale_y_continuous(breaks = c(0,5,10,15,20), labels = c("0%","5%","10%", "15%", "20%")) +
  scale_x_continuous(breaks = c(2019, 2015, 2011, 2007, 2003, 1999), 
                     labels = c("'19", "'15", "'11", "'07", "'03", "'99")) +
  xlab("") +
  ylab("") +
  geom_vline(data = partrend, mapping = aes(xintercept = 2017), alpha = 0.7, linetype = "dashed") +
  facet_grid(partei_name ~ kanton, 
             labeller = labeller(kanton = kanton.labs)) +
  
  scale_color_manual(values=c("#00539CFF", "#EEA47FFF"), 
                     name="",
                     breaks=c(0,1),
                     labels=c("Control", "Treatment")) +
  labs(title = "Visualizing pre-treatment trends",
       subtitle = "The dashed line represents the approval of the Federal Energy Act in 2017") +
  theme_bw()

# Autor plots -------------------------------------------------------------

# Note: if there are T pre-treatment periods, you can only estimate T-1 pre-treatment effects (weshalb weiss ich im Moment noch nicht)
# Our parallel trends plots for the Greens start in 1999 -> T-1 = 2003
# Our parallel trends plots for the GLP start in 2007 -> T-1 = 2011

# new subset: since 2003 (for Green) and since 2011 (for GLP)
years_since03 <- c(1999, 2003, 2007, 2011, 2015, 2019)
years_since11 <- c(2007, 2011, 2015, 2019)

# define new datasets
data_spillover_authorGPS <- all_gemeinden %>% 
  mutate(treatment = ifelse(code %in% km03_municipalities & partei_name %in% green_parties,1,0)) %>% 
  filter(jahr %in% years_since03,
         kanton %in% relevant_cantons)

data_spillover_authorGLP <- all_gemeinden %>% 
  mutate(treatment = ifelse(code %in% km03_municipalities & partei_name %in% green_parties,1,0)) %>% 
  filter(jahr %in% years_since11,
         kanton %in% relevant_cantons)

# define "treatments" for GPS Autor plots
data_spillover_authorGPS$tr19 <- ifelse(data_spillover_authorGPS$treatment == 1 & data_spillover_authorGPS$jahr == 2019, 1, 0)
data_spillover_authorGPS$tr15 <- ifelse(data_spillover_authorGPS$treatment == 1 & data_spillover_authorGPS$jahr == 2015, 1, 0)
data_spillover_authorGPS$tr11 <- ifelse(data_spillover_authorGPS$treatment == 1 & data_spillover_authorGPS$jahr == 2011, 1, 0)
data_spillover_authorGPS$tr07 <- ifelse(data_spillover_authorGPS$treatment == 1 & data_spillover_authorGPS$jahr == 2007, 1, 0)
data_spillover_authorGPS$tr03 <- ifelse(data_spillover_authorGPS$treatment == 1 & data_spillover_authorGPS$jahr == 2003, 1, 0)
data_spillover_authorGPS$tr99 <- ifelse(data_spillover_authorGPS$treatment == 1 & data_spillover_authorGPS$jahr == 1999, 1, 0)


# define "treatments" for GLP Autor plots
data_spillover_authorGLP$tr19 <- ifelse(data_spillover_authorGLP$treatment == 1 & data_spillover_authorGLP$jahr == 2019, 1, 0)
data_spillover_authorGLP$tr15 <- ifelse(data_spillover_authorGLP$treatment == 1 & data_spillover_authorGLP$jahr == 2015, 1, 0)
data_spillover_authorGLP$tr11 <- ifelse(data_spillover_authorGLP$treatment == 1 & data_spillover_authorGLP$jahr == 2011, 1, 0)
data_spillover_authorGLP$tr07 <- ifelse(data_spillover_authorGLP$treatment == 1 & data_spillover_authorGLP$jahr == 2007, 1, 0)


# Subset per unit for GPS
author_gps_as <- filter(data_spillover_authorGPS, kanton == "ag" & partei_name == "GPS" | kanton == "so"& partei_name == "GPS")
author_gps_be <- filter(data_spillover_authorGPS, kanton == "be" & partei_name == "GPS")

# Subset per unit for GLP
author_glp_as <- filter(data_spillover_authorGLP, kanton == "ag" & partei_name == "GPS" | kanton == "so"& partei_name == "GPS")
author_glp_be <- filter(data_spillover_authorGLP, kanton == "be" & partei_name == "GPS")


# FE-Regression per unit 
felm_author_GPS_as <- felm(parteistaerke ~ tr19 + tr15 + tr11 + tr07 + tr03 | code + jahr | 0 | code, author_gps_as) 
felm_author_GPS_be <- felm(parteistaerke ~ tr19 + tr15 + tr11 + tr07 + tr03 | code + jahr | 0 | code, author_gps_be)

felm_author_GLP_as <- felm(parteistaerke ~ tr19 + tr15 + tr11 | code + jahr | 0 | code, author_glp_as)
felm_author_GLP_be <- felm(parteistaerke ~ tr19 + tr15 + tr11 | code + jahr | 0 | code, author_glp_be)


########## Autor Plot GPS AG/SO ###############
m_GPS_AS <- matrix(NA, nrow = 5, ncol = 4)
rownames(m_GPS_AS) <- c("tr19", "tr15", "tr11", "tr07", "tr03")
colnames(m_GPS_AS) <- c("lowerCI", "mean", "upperCI", "years")
m_GPS_AS[,1] <- confint(felm_author_GPS_as)[,1]
m_GPS_AS[,2] <- felm_author_GPS_as$beta
m_GPS_AS[,3] <- confint(felm_author_GPS_as)[,2]
m_GPS_AS[,4] <- c(2003, 2007, 2011, 2015, 2019)

df_GPS_AS <- as.data.frame(m_GPS_AS)

autor_plot_GPS_agso <- ggplot(df_GPS_AS, aes(x = years, y = mean)) +
  geom_errorbar(aes(ymin = lowerCI, ymax = upperCI), width=.1) +
  geom_point() +
  geom_line() +
  theme_bw() + theme(panel.grid = element_blank()) +
  geom_hline(yintercept = 0, lty = "dashed") +
  ggtitle("Autor Plot for Pre- (2003, 2007, 2011 and 2015) and Post-treatment period (2019),\nwith 95% Confidence Interval, Cantons AG and SO") +
  ylab("Vote Share GPS [%]") +
  xlab("Years of Elections")


########## Autor Plot GPS BE ###############
m_GPS_BE <- matrix(NA, nrow = 5, ncol = 4)
rownames(m_GPS_BE) <- c("tr19", "tr15", "tr11", "tr07", "tr03")
colnames(m_GPS_BE) <- c("lowerCI", "mean", "upperCI", "years")
m_GPS_BE[,1] <- confint(felm_author_GPS_be)[,1]
m_GPS_BE[,2] <- felm_author_GPS_be$beta
m_GPS_BE[,3] <- confint(felm_author_GPS_be)[,2]
m_GPS_BE[,4] <- c(2003, 2007, 2011, 2015, 2019)

df_GPS_BE <- as.data.frame(m_GPS_BE)

autor_plot_GPS_be <- ggplot(df_GPS_BE, aes(x = years, y = mean)) +
  geom_errorbar(aes(ymin = lowerCI, ymax = upperCI), width=.1) +
  geom_point() +
  geom_line() +
  theme_bw() + theme(panel.grid = element_blank()) +
  geom_hline(yintercept = 0, lty = "dashed") +
  ggtitle("Autor Plot for Pre- (2003, 2007, 2011 and 2015) and Post-treatment Period (2019),\nwith 95% Confidence Intervals, Canton Bern") +
  ylab("Vote Share GPS [%]") +
  xlab("Years of Elections")


########## Autor Plot GLP AG/SO ###############
m_GLP_AS <- matrix(NA, nrow = 3, ncol = 4)
rownames(m_GLP_AS) <- c("tr11", "tr07", "tr03")
colnames(m_GLP_AS) <- c("lowerCI", "mean", "upperCI", "years")
m_GLP_AS[,1] <- confint(felm_author_GLP_as)[,1]
m_GLP_AS[,2] <- felm_author_GLP_as$beta
m_GLP_AS[,3] <- confint(felm_author_GLP_as)[,2]
m_GLP_AS[,4] <- c(2011, 2015, 2019)

df_GLP_AS <- as.data.frame(m_GLP_AS)

autor_plot_GLP_agso <- ggplot(df_GLP_AS, aes(x = years, y = mean)) +
  geom_errorbar(aes(ymin = lowerCI, ymax = upperCI), width=.1) +
  geom_point() +
  geom_line() +
  theme_bw() + theme(panel.grid = element_blank()) +
  geom_hline(yintercept = 0, lty = "dashed") +
  ggtitle("Autor Plot for Pre- (2011 and 2015) and Post-treatment Period (2019), \nwith 95% Confidence Intervals, Cantons AG and SO") +
  ylab("Vote Share GLP [%]") +
  xlab("Years of Elections")


########## Autor Plot GLP BE ###############
m_GLP_BE <- matrix(NA, nrow = 3, ncol = 4)
rownames(m_GLP_BE) <- c("tr11", "tr07", "tr03")
colnames(m_GLP_BE) <- c("lowerCI", "mean", "upperCI", "years")
m_GLP_BE[,1] <- confint(felm_author_GLP_be)[,1]
m_GLP_BE[,2] <- felm_author_GLP_be$beta
m_GLP_BE[,3] <- confint(felm_author_GLP_be)[,2]
m_GLP_BE[,4] <- c(2011, 2015, 2019)

df_GLP_BE <- as.data.frame(m_GLP_BE)

autor_plot_GLP_be <- ggplot(df_GLP_BE, aes(x = years, y = mean)) +
  geom_errorbar(aes(ymin = lowerCI, ymax = upperCI), width=.1) +
  geom_point() +
  geom_line() +
  xlab("Years of Elections") +
  theme_bw() + theme(panel.grid = element_blank()) +
  geom_hline(yintercept = 0, lty = "dashed") +
  ggtitle("Autor Plot for Pre- (2011 and 2015) and Post-treatment Period (2019), \nwith 95% Confidence Interval, Canton Bern") +
  ylab("Vote Share GLP [%]") 


gridExtra::grid.arrange(autor_plot_GPS_agso, autor_plot_GLP_agso, 
                        autor_plot_GPS_be, autor_plot_GLP_be, ncol = 2)

