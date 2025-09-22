
rm(list = ls())

# Libraries
##############
library(readxl)
library(tidyverse)
library(ggplot2)
library(ggpubr)

# Data preparation
###################
M3_Arbeitsmarktdaten <- read_excel("Arbeitsmarktdaten.xlsx")
head(M3_Arbeitsmarktdaten)
amdata <- M3_Arbeitsmarktdaten

# Add percentage values to dataframe
amprozent <- as.data.frame(
  amdata %>% 
  mutate(G_Prozent = 100/TotalReal*Gemeinde) %>% 
  mutate(R_Prozent = 100/TotalReal*Region) %>% 
  mutate(K_Prozent = 100/TotalReal*Kanton) %>% 
  mutate(GG_Prozent = 100/TotalReal*Grenzgaenger) %>% 
  mutate(AK_Prozent = 100/TotalReal*Andere_KT) %>% 
  mutate(Rest= c(K_Prozent + GG_Prozent + AK_Prozent))
  )

amprozent1 <- amprozent[, c("Kraftwerke", "G_Prozent", "R_Prozent")]

am_data <- amprozent1 %>% group_by(Kraftwerke) %>% 
  gather(key = ebene, value = prozente, -Kraftwerke) %>% 
  mutate(ebene = ebene %>%  factor() %>% fct_relevel("R_Prozent", "G_Prozent")) 


# Einzelne Plots pro geografischer Ebene
##########################################
## colors: "#DEEBF7" "#9ECAE1" "#3182BD"

plot_arbeitsmarkt <- ggplot(am_data, aes(x = Kraftwerke, y = prozente*0.01, fill = ebene)) + 
  geom_bar(position="stack", stat="identity", width = 0.5) +
  xlab("") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  ylab("") +
  scale_fill_manual(values = c("#9ECAE1", "#3182BD"), 
                      name = "Levels", labels = c("Region", "Municipality")) +
  ggtitle("Share of employees living in municipalities \nwith and close to a nuclear plant") +
  theme_bw() +
  theme(axis.text.x=element_text(size=rel(1.5))) +
  theme(axis.title.x = element_text(size=rel(1.5))) +
  #theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(axis.text.y = element_text(size=rel(1.5))) +
  theme(axis.title.y = element_text(size=rel(1.5))) +
  theme(plot.title = element_text(size=rel(1.8))) +
  theme(legend.title = element_text(size=rel(1.5))) +
  theme(legend.text = element_text(size=rel(1.3)))

