#datawrangling

rm(list=ls())

#loading libraries
library(tidyverse)
library(readxl)

#reading in the raw data
rohdaten <- read_excel("rohdaten.xlsx") %>% 
  fill(name, jahr, code) %>% 
  mutate(jahr = as.numeric(jahr),
         partei_code = as.numeric(partei_code),
         code = as.numeric(code))

# Aargau ------------------------------------------------------------------

#row IDs Aargau: 56281-65604 (in "rohdaten")

aargau <- rohdaten %>% 
  slice(56281:65604) %>% 
  mutate(kanton = "ag")

bezirke_aargau <- c("Bezirk Aarau","Bezirk Baden","Bezirk Bremgarten","Bezirk Brugg",
                    "Bezirk Kulm","Bezirk Laufenburg","Bezirk Lenzburg","Bezirk Muri",
                    "Bezirk Rheinfelden","Bezirk Zofingen","Bezirk Zurzach")

aargau_bezirke <- aargau %>% 
  filter(name %in% bezirke_aargau)

#unique(aargau_bezirke$name) --> 11 districts, correct

aargau_gemeinden <- aargau %>% 
  filter(!name %in% bezirke_aargau)

#unique(aargau_gemeinden$name) --> 211 municipalities, but should be only 210 sein (fusion)

# Bern --------------------------------------------------------------------

#row IDs Bern: 7351-22302 (in "rohdaten")

bern <- rohdaten %>% 
  slice(7351:22302) %>% 
  mutate(kanton = "be")

bezirke_bern <- c("Arrondissement administratif Jura bernois","Verwaltungskreis Bern-Mittelland","Verwaltungskreis Biel/Bienne",
                  "Verwaltungskreis Emmental","Verwaltungskreis Frutigen-Niedersimmental","Verwaltungskreis Interlaken-Oberhasli",
                  "Verwaltungskreis Oberaargau","Verwaltungskreis Obersimmental-Saanen","Verwaltungskreis Seeland","Verwaltungskreis Thun")

bern_bezirke <- bern %>% 
  filter(name %in% bezirke_bern)

#unique(bern_bezirke$name) --> 10 Verwaltungskreise, stimmt.

bern_gemeinden <- bern %>% 
  filter(!name %in% bezirke_bern)

#unique(bern_gemeinden$name) --> 346 municipalities, currently only 342 (again due to fusion)

# Solothurn ---------------------------------------------------------------

#row IDs Solothurn: 36079-41076 (in "rohdaten")

solothurn <- rohdaten %>% 
  slice(36079:41076) %>% 
  mutate(kanton = "so")

bezirke_solothurn <- c("Bezirk Bucheggberg","Bezirk Dorneck","Bezirk Gaeu","Bezirk Goesgen",
                       "Bezirk Lebern","Bezirk Olten","Bezirk Solothurn","Bezirk Thal",
                       "Bezirk Thierstein","Bezirk Wasseramt")

solothurn_bezirke <- solothurn %>% 
  filter(name %in% bezirke_solothurn)

#unique(solothurn_bezirke$name) --> 10 districts, correct

solothurn_gemeinden <- solothurn %>% 
  filter(!name %in% bezirke_solothurn)

#unique(solothurn_gemeinden$name) --> 109 municipalities, correct

# Merging the datasets ----------------------------------------------------------

all_gemeinden <- full_join(aargau_gemeinden, bern_gemeinden) %>% 
  full_join(solothurn_gemeinden) %>% 
  mutate(time = factor(ifelse(jahr == 2019,1,0))) %>% 
  replace_na(list(parteistimmen = 0, parteistaerke = 0))

save(all_gemeinden, file = "all_gemeinden.RData")

