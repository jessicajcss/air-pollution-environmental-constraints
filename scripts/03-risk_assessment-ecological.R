#############################################


# SCRIPT for Ecological Risk Assessment

# REFERENCE VALUES: 
# i) Igeo: Mason, B.(1966).
# ii) Toxic response factor: Douay et al. (2013), Egbueri (2020), Zhang et al. (2021), Zhi et al. (2021)

###################
# Last update: 2022-04-05
# Author: Santos-Silva, J. C. <jessica.jcss@gmail.com>

##############################################
############### LOAD PACKAGES ################

library(lubridate)  
library(tidyverse)
library(bazar)
library(openair)
library(magrittr)
library(reshape2)




##############################################
################### INPUT ####################
############### FROM SAMPLING  ###############

dataset <- read_csv(
  "./output/02-horizontal_flow_characterization/concentracao_meteo_windflow.csv")


## Working with mean values
concentration_melt <- dataset %>%
  select(site, Filter.ID, date,  rep, PM2.5, Al:Hg) %>%
  melt(id=c("site", "Filter.ID", "date", "rep"))

# Average
dataset <- dcast(concentration_melt, site + Filter.ID + date ~ variable, mean)


####  Geo-accumulation indexes ----


##### Reference concentration 

references <- data.frame(81300,	277200,	100,	55,	13,	50000,	25,	4400,	950,
                          260,	375,	2.50,	1050, 28300,	20900, 130, 
                          25900, 36300, 70, 75, 135)
colnames(references) <- c("Al", "Si", "Cr", "Cu", "Pb", "Fe", "Co", "Ti", "Mn", 
                           "S", "Sr", "Br", "P", "Na", "Mg", "Cl",
                           "K", "Ca", "Zn", "Ni", "V")


##### Igeo determination

geo.index <- data.frame(matrix(NA,
                               ncol = ncol(references),
                               nrow = nrow(dataset)))
colnames(geo.index) <- c("Al", "Si", "Cr", "Cu", "Pb", "Fe", "Co", "Ti", "Mn",
                         "S", "Sr", "Br", "P", "Na", "Mg", "Cl", 
                         "K", "Ca", "Zn", "Ni", "V")


for (i in 1:ncol(geo.index)) {
  pm <- dataset$PM2.5
  xexp <- (dataset[as.character(colnames(geo.index[i]))]*10^6)/pm
  xcr <- as.numeric(references[as.character(colnames(geo.index[i]))])
  geo.index[i] <- geo_index(xexp, xcr) 
  geo.index[geo.index == -Inf] <- NA
}

geo.index$date <- dataset$date
geo.index$site <- dataset$site
geo.index <- geo.index %>% relocate(date, site, .before = Al)






#### Ecological Risk Index ----

# Select only samples analysed by EDXRF

dataset_elements <- dataset %>% 
  arrange(date) %>%
  filter(!is.na(Al))

##### Reference: metal's toxic response factor

toxic.resp_factor <- data.frame(5, 5, 5, 5, 2, 2, 1, 1, 1)
colnames(toxic.resp_factor) <- c('Co', 'Pb', 'Cu', 'Ni', 'V', 'Cr', 'Ti', 'Mn', 'Zn')


# Potential ecological risk coefficient 
pot.ecorisk <- data.frame(matrix(NA, 
                                 ncol = ncol(toxic.resp_factor), 
                                 nrow = nrow(dataset_elements)))
colnames(pot.ecorisk) <- c('Co', 'Pb', 'Cu', 'Ni', 'V', 'Cr', 'Ti', 'Mn', 'Zn')



for (i in 1:ncol(pot.ecorisk)) {
  pm <- dataset_elements$PM2.5
  xexp <- dataset_elements[as.character(colnames(pot.ecorisk[i]))]*10^6/pm
  xcr <- as.numeric(references[as.character(colnames(pot.ecorisk[i]))])
  tr <- as.numeric(toxic.resp_factor[as.character(colnames(pot.ecorisk[i]))])
  pot.ecorisk[i] <- pot_ecorisk(tr, xexp, xcr)
}
pot.ecorisk$site <- dataset_elements$site
pot.ecorisk$date <- dataset_elements$date


pot.ecorisk <- cutData(pot.ecorisk, 
                       hemisphere = "southern",
                       type = "season")


#REORDER FACTOR SEASON
pot.ecorisk$season <- factor(pot.ecorisk$season, 
                             levels = c("summer (DJF)", 
                                        "autumn (MAM)", 
                                        "winter (JJA)", 
                                        "spring (SON)"))


# Ecological risk index (RI) ----


RI <- pot.ecorisk %>%
  group_by(site) %>%
  summarise_all(~if(is.numeric(.)) sum(., na.rm=T) else "Total")


# Ecological Risk Level
df <- pot.ecorisk %>%
  select(-date) %>%
  melt(id = c("site", "season")) %>%
  mutate(risk = case_when(value < 40 ~ "low risk", 
                          value >= 40 & value < 80 ~ "moderate risk",
                          value >= 80 & value < 160 ~ "considerable risk", 
                          value >= 160 & value < 320 ~ "high risk",
                          value >= 320 ~ "extremely high",
                          TRUE ~ "not available")) %>%
  arrange(variable)


df$risk <- factor(df$risk,
                  levels = c("low risk",
                             "moderate risk",
                             "considerable risk",
                             "high risk",
                             "extremely high",
                             "not available"))

