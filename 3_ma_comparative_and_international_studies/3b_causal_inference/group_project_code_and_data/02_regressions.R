#regressions

rm(list = ls())

library(tidyverse)
library(lfe)
library(ggswissmaps)

load("all_gemeinden.RData")

# Main analysis: Municipalities with AKW site --------------------------------------------------

akw_sites <- c(668, 2572, 4304, 4311)
relevant_cantons <- c("be", "so", "ag")
relevant_years <- c(2015, 2019)
green_parties <- c("GPS", "GLP")

data_main <- all_gemeinden %>% 
  mutate(treatment = ifelse(code %in% akw_sites & partei_name %in% green_parties,1,0)) %>% 
  filter(jahr %in% relevant_years,
         kanton %in% relevant_cantons)

data_main %>% 
  filter(treatment == 1) %>% 
  distinct(code) %>% 
  tally()

## --> 4 municipalities treated in total

# Main analysis II: Test for spillovers (3km radius --> neighbouring municipalities) -----------------------------------
km03_muehleberg <- c(670, 351, 663, 360, 309, 664, 671, 662, 667)
km03_goesgen <- c(2585, 2576, 2495, 2497, 2573)
km03_beznau <- c(4047, 4320, 4323, 4309, 4303)
km03_leibstadt <- c(4307, 4313, 4184, 4176)

km03_municipalities <- c(akw_sites, 
                         km03_muehleberg, km03_goesgen,
                         km03_beznau, km03_leibstadt)


data_spillover <- all_gemeinden %>% 
  mutate(treatment = ifelse(code %in% km03_municipalities & partei_name %in% green_parties,1,0)) %>% 
  filter(jahr %in% relevant_years,
         kanton %in% relevant_cantons)

#counting number of treated units
data_spillover %>% 
  filter(treatment == 1) %>% 
  distinct(code) %>% 
  tally()

save(data_spillover, file = "data_spillover.RData")

## --> 26 municipalities treated in total

# Robustness check 2: Municipalities in 10km radius ------------------------------------------------------

#vectors Einzugsgebiete

km10_muehleberg <- c(664, 0671, 665, 662, 666, 667, 668, 670, 355, 351, 663, 360, 307, 
                     311, 312, 309, 304, 301, 302, 498, 499, 500, 493,
                     2258, 2265, 2275, 2276, 2259, 2260, 2274, 2260, 2270, 2278, 2277, 2264, 2262, 2266, 
                     2295, 2309, 2308, 2391)

km10_goesgen <- c(2428, 2425,
                  2401, 2406, 2404, 2402, 
                  2577, seq(2571, 2573, 1), seq(2575, 2586,1),
                  seq(2490,2505,1),
                  2841, 2852, 2850, 2843, 2856, 2855, 2868, 2865, 2862, 2851, 2860, 2846, 2859, 2847, 2867, 2845, 2864, 2869, 2863, 2861, 2842, 2858, 
                  seq(2884,2888,1), 2882, 2891, 2892, 2895, 
                  4271, 4282, 4280, 4279, 4287, 4285, 4283, 
                  4005)

km10_beznau <- c(seq(4300, 4305,1), 4307, seq(4309, 4323,1),
                 4176, 4184, 4164,
                 4105, 4121, 4110, 4112, 4106, 4124, 4111, 4095, 4123, 
                 4047, 4044, 4049, 4028, 4026, 4038, 4021, 4042, 4029)

km10_leibstadt <- c(seq(4300, 4305,1), 4315, 4320, 4309, 4310, 4307, 4311, 4313,
                    4176, 4184, 4164, 4170, 4169,
                    4105, 4121, 4110, 4112, 4106, 4124, 4111, 4097, 4096,
                    4047, 4044)

#Putting the vectors together
km10_municipalities <- c(akw_sites, 
                         km10_muehleberg, km10_goesgen,
                         km10_beznau, km10_leibstadt)

length(km10_municipalities) #194 municipalities (however, still some duplicates included)

#Removing duplicates from the vector
km10_municipalities_clean <- unique(km10_municipalities) 
length(km10_municipalities_clean) # after removing duplicates, 163 municipalities remain

km10_municipalities_clean <- sort(km10_municipalities_clean) #ordering the vector

#Comparing the vectors (now only 150 municipalities due to municipality merges, special territories, etc. (checked for manually))
km10_municipalities_vec <- all_gemeinden %>% 
  mutate(treatment = ifelse(code %in% km10_municipalities_clean,1,0)) %>% 
  filter(treatment == 1) %>% 
  distinct(code) %>% 
  pull()

length(km10_municipalities_vec) 
akw_gemeinden_vec <- sort(km10_municipalities_vec)

km10_municipalities_clean
km10_municipalities_vec

#Creating the data
data_robust2 <- all_gemeinden %>% 
  rename(GMDNR = code) %>% 
  mutate(treatment = ifelse(GMDNR %in% km10_municipalities_clean & partei_name %in% green_parties,1,0)) %>% 
  filter(jahr %in% relevant_years,
         kanton %in% relevant_cantons)

#counting number of treated units
data_robust2 %>% 
  filter(treatment == 1) %>% 
  distinct(GMDNR) %>% 
  tally()

## --> 106 municipalities treated in total

# Robustness check 3: All municipalities in district with AKW sites  -------------------------------------------------------------------

#retreiving municipality and district codes
gemeinden <- shp_df[[4]]

gemeinden <- gemeinden %>% 
  select(GMDNR, BZNR) %>% 
  distinct(GMDNR, .keep_all = TRUE)

#treatment districts

#GMDNR: 668 / BZNR: 246 (Muehleberg)
#GMDNR: 2491 / BZNR: 1105 (Goesgen)
#GMDNR: 4304 / BZNR: 1911 (Beznau)
#GMDNR: 4311 / BZNR: 1911 (Leibstadt)

districts <- c(246, 1105, 1911)


#data for gps
data_robust3 <- all_gemeinden %>% 
  rename(GMDNR = code) %>%
  left_join(gemeinden, by = "GMDNR") %>% 
  mutate(treatment = ifelse(BZNR %in% districts & partei_name %in% green_parties,1,0)) %>% 
  filter(jahr %in% relevant_years,
         kanton %in% relevant_cantons)

#counting number of treated units
data_robust3 %>% 
  filter(treatment == 1) %>% 
  distinct(GMDNR) %>% 
  tally()

## --> 112 municipalities treated in total

# Regression analyses -----------------------------------------------------

########################################### MAIN: 3 KM (Hypotheses 1 & 2) ##########################################################

# Subset data for AG/SO Green & GLP
data_3km_glp_as <- filter(data_spillover, kanton == "ag" & partei_name == "GLP" | kanton == "so"& partei_name == "GLP" )
data_3km_gps_as <- filter(data_spillover, kanton == "ag" & partei_name == "GPS" | kanton == "so"& partei_name == "GPS" )

# OLS AG/SO with clustered SE: 
fe0_3km_glp_as <- felm(parteistaerke ~ time*treatment | 0 | 0 | code, data = data_3km_glp_as)
fe0_3km_gps_as <- felm(parteistaerke ~ time*treatment | 0 | 0 | code, data = data_3km_gps_as)

summary(fe0_3km_glp_as)
summary(fe0_3km_gps_as)

########################################### MAIN: 3 KM (Hypothesis 3) ##########################################################

# Subset data for BE Green & GLP
data_3km_glp_be <- filter(data_spillover, kanton == "be" & partei_name == "GLP")
data_3km_gps_be <- filter(data_spillover, kanton == "be" & partei_name == "GPS")

# OLS BE with clustered SE: 
fe0_3km_glp_be <- felm(parteistaerke ~ time*treatment | 0 | 0 | code, data = data_3km_glp_be)
fe0_3km_gps_be <- felm(parteistaerke ~ time*treatment | 0 | 0 | code, data = data_3km_gps_be)

summary(fe0_3km_glp_be)
summary(fe0_3km_gps_be)

########################################### SPILLOVER: 10 KM ##################################################
#data_robust2
data_robust2$tr19 <- ifelse(data_robust2$treatment == 1 & data_robust2$jahr == 2019, 1, 0)

# Subset data for AG/SO Green & GLP
data_robust2_glp_as <- filter(data_robust2, kanton == "ag" & partei_name == "GLP" | kanton == "so"& partei_name == "GLP" )
data_robust2_gps_as <- filter(data_robust2, kanton == "ag" & partei_name == "GPS" | kanton == "so"& partei_name == "GPS" )

# OLS AG/SO with clustered SE: 
fe0_robust2_glp_as <- felm(parteistaerke ~ time*treatment | 0 | 0 | GMDNR, data = data_robust2_glp_as)
fe0_robust2_gps_as <- felm(parteistaerke ~ time*treatment | 0 | 0 | GMDNR, data = data_robust2_gps_as)

# Subset data for BE Green & GLP
data_robust2_glp_be <- filter(data_robust2, kanton == "be" & partei_name == "GLP")
data_robust2_gps_be <- filter(data_robust2, kanton == "be" & partei_name == "GPS")

# OLS BE with clustered SE: 
fe0_robust2_glp_be <- felm(parteistaerke ~ time*treatment | 0 | 0 | GMDNR, data = data_robust2_glp_be)
fe0_robust2_gps_be <- felm(parteistaerke ~ time*treatment | 0 | 0 | GMDNR, data = data_robust2_gps_be)


stargazer::stargazer(fe0_robust2_gps_as, fe0_robust2_glp_as, fe0_robust2_gps_be, fe0_robust2_glp_be, 
                     column.labels = c("AG/SO Green", "AG/SO GLP", "BE Green", "BE GLP"),
                     type = "text", title = "Robustness Check 1: 10km" )



########################################### SPILLOVER: BEZIRK #################################################
#data_robust3
data_robust3$tr19 <- ifelse(data_robust3$treatment == 1 & data_robust3$jahr == 2019, 1, 0)

# Subset data for AG/SO Green & GLP
data_robust3_glp_as <- filter(data_robust3, kanton == "ag" & partei_name == "GLP" | kanton == "so"& partei_name == "GLP" )
data_robust3_gps_as <- filter(data_robust3, kanton == "ag" & partei_name == "GPS" | kanton == "so"& partei_name == "GPS" )

# OLS AG/SO with clustered SE: 
fe0_robust3_glp_as <- felm(parteistaerke ~ time*treatment | 0 | 0 | GMDNR, data = data_robust3_glp_as)
fe0_robust3_gps_as <- felm(parteistaerke ~ time*treatment | 0 | 0 | GMDNR, data = data_robust3_gps_as)

# Subset data for BE Green & GLP
data_robust3_glp_be <- filter(data_robust3, kanton == "be" & partei_name == "GLP")
data_robust3_gps_be <- filter(data_robust3, kanton == "be" & partei_name == "GPS")

# OLS BE with clustered SE: 
fe0_robust3_glp_be <- felm(parteistaerke ~ time*treatment | 0 | 0 | GMDNR, data = data_robust3_glp_be)
fe0_robust3_gps_be <- felm(parteistaerke ~ time*treatment | 0 | 0 | GMDNR, data = data_robust3_gps_be)


stargazer::stargazer(fe0_robust3_gps_as, fe0_robust3_glp_as, fe0_robust3_gps_be, fe0_robust3_glp_be, 
                     column.labels = c("AG/SO Green", "AG/SO GLP", "BE Green", "BE GLP"),
                     type = "text", title = "Robustness Check 2: Bezirk" )

