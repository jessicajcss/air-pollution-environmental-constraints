#############################################


# SCRIPT for Health Risk Assessment

# REFERENCE VALUES: The values of Tr, RfDo, RfCi, GIABS, SFo, and IUR for the anthropogenic metals, where T_r^i is the ith metal's toxic response factor, and RfDo, RfCi, GIABS, SFo, and IUR are the oral Reference Doses [mg·(kg·day)−1], Inhalation Reference Concentration (mg·m−3), Gastrointestinal Absorption Factor, oral Slope Factor [(mg·(kg·day)−1)−1], and Inhalation Unit Risk [(μg·m−3)−1], respectively.


# OBSERVATION: This script must be used in association with ProUCL (Version 5.1), a software package provided by the US EPA (2015b), to determine C95% of each element specific, for each season, and for each site:
# i) ProUCL-INPUT file - [line: 265 - 340]
# ii) INPUT ProUCL-OUTPUT file - [line: 357]

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



######## REFERENCE CONCENTRATIONS/DOSE #######

# RfC or REL
RfC <- data.frame(0.005, 0.0001, 0.002,
                  0.00015, 45.85, 0.000006,
                  0.00005, 0.0005, 0.0017,
                  0.0002, 0.1, 0.035, 
                  0.0001, 0.000014, 0.00015)
colnames(RfC) <- c('Al', 'Cr', 'Cu', 
                   'Pb', 'Fe', 'Co',
                   'Mn', 'Se', 'Br',
                   'P', 'Mg', 'Zn', 
                   'V', 'Ni', 'Cl')

# IUR
IUR <- data.frame(0.084, 0.000012, 0.009, 0.00024)
colnames(IUR) <- c('Cr', 'Pb', 'Co', 'Ni')

# RfD
RfD <- data.frame(0.005, 0.0035, 0.3, 0.04, 0.007, 0.003, 0.02)
colnames(RfD) <- c('Se', 'Pb', 'Zn', 'Cu', 'V', 'Cr', 'Ni')

# GIABS
GIABS <- data.frame(1, 1, 1 , 1, 0.026, 0.025, 0.04)
colnames(GIABS) <- c('Se', 'Pb', 'Zn', 'Cu', 'V', 'Cr', 'Ni')

# SFo
SFo <- data.frame(0.0085, 0.5)
colnames(SFo) <- c('Pb', 'Cr')

# ABS
ABS <- data.frame(0.01, 0.01, 0.01,
                  0.1, 0.01, 0.01,
                  0.01, 0.01, 0.01,
                  0.01, 0.01, 0.01, 
                  0.01, 0.01, 0.01)
colnames(ABS) <- c('Al', 'Cr', 'Cu', 
                   'Pb', 'Fe', 'Co',
                   'Mn', 'Se', 'Br',
                   'P', 'Mg', 'Zn', 
                   'V', 'Ni', 'Cl')

# Other exposure parameters
Notation <- c('IngR', 'EF', 'ED', 'ED40', 'CF', 'BW', 
              'ATnonc6', 'ATnonc25', 'ATnonc40', 'ATc', 
              'ET', 
              'ATnnonc6', 'ATnnonc25', 'ATnnonc40', 'ATnc',
              'SA', 'AF')
Children <- c(200, 350, 6, NA, 0.000001, 15,
              6*365, NA, NA, 70*365,
              24, 
              6*365*24, NA, NA, 70*365*24,
              2373, 0.2)
Adults <- c(100, 350, 25, 40, 0.000001, 80,
            NA, 25*365, 40*365, 70*365,
            24, 
            NA, 25*365*24, 40*365*24, 70*365*24,
            6032, 0.07)

parameters_exposure <- data.frame(Notation, Children, Adults)






###################################################
#### Exposure concentration through inhalation ####

# non carcinogenic
ET <- parameters_exposure$Children[Notation == "ET"]
EF <- parameters_exposure$Children[Notation == "EF"]
ED <- parameters_exposure$Children[Notation == "ED"]
AT <- parameters_exposure$Children[Notation == "ATnnonc6"]
factor_noncarc_6years <- (ET * EF * ED) / AT

ET <- parameters_exposure$Adults[Notation == "ET"]
EF <- parameters_exposure$Adults[Notation == "EF"]
ED <- parameters_exposure$Adults[Notation == "ED"]
AT <- parameters_exposure$Adults[Notation == "ATnnonc25"]
factor_noncarc_25years <- (ET * EF * ED) / AT  

ET <- parameters_exposure$Adults[Notation == "ET"]
EF <- parameters_exposure$Adults[Notation == "EF"]
ED <- parameters_exposure$Adults[Notation == "ED40"]
AT <- parameters_exposure$Adults[Notation == "ATnnonc40"]
factor_noncarc_40years <- (ET * EF * ED) / AT 


# carcinogenic

ET <- parameters_exposure$Children[Notation == "ET"]
EF <- parameters_exposure$Children[Notation == "EF"]
ED <- parameters_exposure$Children[Notation == "ED"]
AT <- parameters_exposure$Children[Notation == "ATnc"]
factor_carc_6years <- (ET * EF * ED) / AT

ET <- parameters_exposure$Adults[Notation == "ET"]
EF <- parameters_exposure$Adults[Notation == "EF"]
ED <- parameters_exposure$Adults[Notation == "ED"]
AT <- parameters_exposure$Adults[Notation == "ATnc"]
factor_carc_25years <- (ET * EF * ED) / AT  

ET <- parameters_exposure$Adults[Notation == "ET"]
EF <- parameters_exposure$Adults[Notation == "EF"]
ED <- parameters_exposure$Adults[Notation == "ED40"]
AT <- parameters_exposure$Adults[Notation == "ATnc"]
factor_carc_40years <- (ET * EF * ED) / AT

#### Chemical daily intake through oral ingestion  ####

IngR_children <- parameters_exposure$Children[Notation == "IngR"]
IngR_adults <- parameters_exposure$Adults[Notation == "IngR"]

EF_children <- parameters_exposure$Children[Notation == "EF"]
EF_adults <- parameters_exposure$Adults[Notation == "EF"]

ED6 <- parameters_exposure$Children[Notation == "ED"]
ED25 <- parameters_exposure$Adults[Notation == "ED"]
ED40 <- parameters_exposure$Adults[Notation == "ED40"]

CF_children <- parameters_exposure$Children[Notation == "CF"]
CF_adults <- parameters_exposure$Adults[Notation == "CF"]

BW_children <- parameters_exposure$Children[Notation == "BW"]
BW_adults <- parameters_exposure$Adults[Notation == "BW"]

AT_children6 <- parameters_exposure$Children[Notation == "ATnonc6"]
AT_adults25 <- parameters_exposure$Adults[Notation == "ATnonc25"]
AT_adults40 <- parameters_exposure$Adults[Notation == "ATnonc40"]
ATc <- parameters_exposure$Adults[Notation == "ATc"]

Ing.factor_ncarc_6years <- (
  IngR_children * EF_children * ED6 * CF_children) / (BW_children * AT_children6
  )
Ing.factor_ncarc_25years <- (
  IngR_adults * EF_adults * ED25 * CF_adults) / (BW_adults * AT_adults25
  )
Ing.factor_ncarc_40years <- (
  IngR_adults * EF_adults * ED40 * CF_adults) / (BW_adults * AT_adults40
  )

Ing.factor_carc_6years <- (
  IngR_children * EF_children * ED6 * CF_children) / (BW_children * ATc
  )
Ing.factor_carc_25years <- (
  IngR_adults * EF_adults * ED25 * CF_adults) / (BW_adults * ATc
  )
Ing.factor_carc_40years <- (
  IngR_adults * EF_adults * ED40 * CF_adults) / (BW_adults * ATc
  )



#### Dermal absorption dose through dermal contact  ####

SA_children <- parameters_exposure$Children[Notation == "SA"]
SA_adults <- parameters_exposure$Adults[Notation == "SA"]

AF_children <- parameters_exposure$Children[Notation == "AF"]
AF_adults <- parameters_exposure$Adults[Notation == "AF"]

EF_children <- parameters_exposure$Children[Notation == "EF"]
EF_adults <- parameters_exposure$Adults[Notation == "EF"]

ED6 <- parameters_exposure$Children[Notation == "ED"]
ED25 <- parameters_exposure$Adults[Notation == "ED"]
ED40 <- parameters_exposure$Adults[Notation == "ED40"]

CF_children <- parameters_exposure$Children[Notation == "CF"]
CF_adults <- parameters_exposure$Adults[Notation == "CF"]

BW_children <- parameters_exposure$Children[Notation == "BW"]
BW_adults <- parameters_exposure$Adults[Notation == "BW"]

AT_children6 <- parameters_exposure$Children[Notation == "ATnonc6"]
AT_adults25 <- parameters_exposure$Adults[Notation == "ATnonc25"]
AT_adults40 <- parameters_exposure$Adults[Notation == "ATnonc40"]
ATc <- parameters_exposure$Adults[Notation == "ATc"]




der.factor_ncarc_6years <- (
  SA_children * AF_children * EF_children * ED6 * CF_children) / (BW_children * AT_children6
  )
der.factor_ncarc_25years <- (
  SA_adults * AF_adults * EF_adults * ED25 * CF_adults) / (BW_adults * AT_adults25
  )
der.factor_ncarc_40years <- (
  SA_adults * AF_adults * EF_adults * ED40 * CF_adults) / (BW_adults * AT_adults40
  )

der.factor_carc_6years <- (
  SA_children * AF_children * EF_children * ED6 * CF_children) / (BW_children * ATc
  )
der.factor_carc_25years <- (
  SA_adults * AF_adults * EF_adults * ED25 * CF_adults) / (BW_adults * ATc
  )
der.factor_carc_40years <- (
  SA_adults * AF_adults * EF_adults * ED40 * CF_adults) / (BW_adults * ATc
  )














###########################################
########## Seasonal Analysis ##############

dataset <- dataset %>% 
  cutData(hemisphere = "southern", 
          type = "season")

#REORDER FACTOR SEASON
dataset$season <- factor(dataset$season, 
                         levels = c("summer (DJF)", 
                                    "autumn (MAM)", 
                                    "winter (JJA)", 
                                    "spring (SON)"))



##### To generate INPUT dataset for proUCL (to calculate UCL 95%) ----


df <- dataset %>%
  select(site, season, PM2.5, Al, Cr, #only variables of interest
         Cu, Pb, Fe, Co,
         Mn, Se, Br,
         P, Mg, Zn, 
         V, Ni, Cl) %>%
  filter(!is.na(Al)) %>%
  mutate(D_PM2.5 = if_else(PM2.5 == 0, "0", "1"),
         D_Al = if_else(Al == 0, "0", "1"),
         D_Cr = if_else(Cr == 0, "0", "1"),
         D_Cu = if_else(Cu == 0, "0", "1"),
         D_Pb = if_else(Pb == 0, "0", "1"),
         D_Fe = if_else(Fe == 0, "0", "1"),
         D_Co = if_else(Co == 0, "0", "1"),
         D_Mn = if_else(Mn == 0, "0", "1"),
         D_Se = if_else(Se == 0, "0", "1"),
         D_Br = if_else(Br == 0, "0", "1"),
         D_P = if_else(P == 0, "0", "1"),
         D_Mg = if_else(Mg == 0, "0", "1"),
         D_Zn = if_else(Zn == 0, "0", "1"),
         D_V = if_else(V == 0, "0", "1"),
         D_Ni = if_else(Ni == 0, "0", "1"),
         D_Cl = if_else(Cl == 0, "0", "1")) %>% 
  relocate(D_PM2.5, .after = PM2.5) %>% 
  relocate(D_Al, .after = Al) %>% 
  relocate(
    D_Cr, .after = Cr) %>% 
  relocate(
    D_Cu, .after = Cu) %>% 
  relocate(
    D_Pb, .after = Pb) %>% 
  relocate(
    D_Fe, .after = Fe) %>% 
  relocate(
    D_Co, .after = Co) %>% 
  relocate(
    D_Mn, .after = Mn) %>% 
  relocate(
    D_Se, .after = Se) %>% 
  relocate(
    D_Br, .after = Br) %>% 
  relocate(
    D_P, .after = P) %>% 
  relocate(
    D_Mg, .after = Mg) %>% 
  relocate(
    D_Zn, .after = Zn) %>% 
  relocate(
    D_V, .after = V) %>% 
  relocate(
    D_Ni, .after = Ni) %>% 
  relocate(
    D_Cl, .after = Cl)


# Dataset proUCL-INPUT
df_MIC <- df %>%
  filter(site == "MIC")

df_NID <- df %>%
  filter(site == "NID") 


proUCL_INPUT_MIC <- write_csv(
  df_MIC,
  "./output/03-risk_assessment-health/proUCL_INPUT_MIC.csv")

proUCL_INPUT_NID <- write_csv(
  df_NID,
  "./output/03-risk_assessment-health/proUCL_INPUT_NID.csv")










#### Using UCL95% seasonal proUCL-OUTPUT as INPUT to health risk assessment ----

########### INPUT ########### 
conf_UCL_ugm3_season <- read.csv(
  "./data/03-risk_assessment-health/UCL_ProUCL.csv", sep = ";")


# Dataset with concentration units required by the methodology
conf_UCL_mgkg_season <- conf_UCL_ugm3_season %>%
  mutate_at(vars(Al:Cl), ~replace(., !is.na(PM2.5),.*10^6/PM2.5))

conf_UCL_ugm3_season <- conf_UCL_ugm3_season %>% 
  melt() %>%
  set_colnames(c("site", "season", "variable", "C95ugm3"))

conf_UCL_mgkg_season <- conf_UCL_mgkg_season %>% 
  melt() %>%
  set_colnames(c("site", "season", "variable", "C95mgkg"))

conf_UCL_season <- merge(conf_UCL_ugm3_season, conf_UCL_mgkg_season, 
                         by = c("site", "season", "variable"))


### Calculations for health risk assessment - site: MIC ----

health_risks_MIC_season <- conf_UCL_season %>%
  filter(site == "MIC") %>%
  select(-site) %>%
  merge(., melt(RfC), all = T, by = "variable") %>%
  merge(., melt(IUR), all = T, by = "variable") %>%
  merge(., melt(RfD), all = T, by = "variable") %>%
  merge(., melt(GIABS), all = T, by = "variable") %>%
  merge(., melt(SFo), all = T, by = "variable") %>%
  merge(., melt(ABS), all = T, by = "variable") %>%
  set_colnames(c("metal", "season", "C95ugm3", "C95mgkg", "RfC", "IUR", "RfD", "GIABS", 'SFo', 'ABS')) %>%
  mutate(site = "MIC",
         EC_noncarc_6years = C95ugm3 * factor_noncarc_6years,
         EC_noncarc_25years = C95ugm3 * factor_noncarc_25years,
         EC_noncarc_40years = C95ugm3 * factor_noncarc_40years,
         EC_carc_6years = C95ugm3 * factor_carc_6years,
         EC_carc_25years = C95ugm3 * factor_carc_25years,
         EC_carc_40years = C95ugm3 * factor_carc_40years,
         CDIing_nonc_6years = C95mgkg * Ing.factor_ncarc_6years,
         CDIing_nonc_25years = C95mgkg * Ing.factor_ncarc_25years,
         CDIing_nonc_40years = C95mgkg * Ing.factor_ncarc_40years,
         CDIing_carc_6years = C95mgkg * Ing.factor_carc_6years,
         CDIing_carc_25years = C95mgkg * Ing.factor_carc_25years,
         CDIing_carc_40years = C95mgkg * Ing.factor_carc_40years,
         DADder_nonc_6years = C95mgkg * ABS * der.factor_ncarc_6years,
         DADder_nonc_25years = C95mgkg * ABS * der.factor_ncarc_25years,
         DADder_nonc_40years = C95mgkg * ABS * der.factor_ncarc_40years,
         DADder_carc_6years = C95mgkg * ABS * der.factor_carc_6years,
         DADder_carc_25years = C95mgkg * ABS * der.factor_carc_25years,
         DADder_carc_40years = C95mgkg * ABS * der.factor_carc_40years,
         HQing_noncarc_6years = CDIing_nonc_6years/RfD,
         HQing_noncarc_25years = CDIing_nonc_25years/RfD,
         HQing_noncarc_40years = CDIing_nonc_40years/RfD,         
         HQinh_noncarc_6years = EC_noncarc_6years/(RfC * 1000),
         HQinh_noncarc_25years = EC_noncarc_25years/(RfC * 1000),
         HQinh_noncarc_40years = EC_noncarc_40years/(RfC * 1000),
         HQder_noncarc_6years = DADder_nonc_6years/(RfD * GIABS),
         HQder_noncarc_25years = DADder_nonc_25years/(RfD * GIABS),
         HQder_noncarc_40years = DADder_nonc_40years/(RfD * GIABS),
         CRinh_6years = IUR * EC_carc_6years,
         CRinh_25years = IUR * EC_carc_25years,
         CRinh_40years = IUR * EC_carc_40years,
         CRing_6years = SFo * CDIing_carc_6years,
         CRing_25years = SFo * CDIing_carc_25years,
         CRing_40years = SFo * CDIing_carc_40years,
         CRder_6years = DADder_carc_6years * SFo / GIABS,
         CRder_25years = DADder_carc_25years * SFo / GIABS,
         CRder_40years = DADder_carc_40years * SFo / GIABS
  )



### Calculations for health risk assessment - site: NID ----
health_risks_NID_season <- conf_UCL_season %>%
  filter(site == "NID") %>%
  select(-site) %>%
  merge(., melt(RfC), all = T, by = "variable") %>%
  merge(., melt(IUR), all = T, by = "variable") %>%
  merge(., melt(RfD), all = T, by = "variable") %>%
  merge(., melt(GIABS), all = T, by = "variable") %>%
  merge(., melt(SFo), all = T, by = "variable") %>%
  merge(., melt(ABS), all = T, by = "variable") %>%
  set_colnames(c("metal", "season", "C95ugm3", "C95mgkg", "RfC", "IUR", "RfD", "GIABS", 'SFo', 'ABS')) %>%
  mutate(site = "NID",
         EC_noncarc_6years = C95ugm3 * factor_noncarc_6years,
         EC_noncarc_25years = C95ugm3 * factor_noncarc_25years,
         EC_noncarc_40years = C95ugm3 * factor_noncarc_40years,
         EC_carc_6years = C95ugm3 * factor_carc_6years,
         EC_carc_25years = C95ugm3 * factor_carc_25years,
         EC_carc_40years = C95ugm3 * factor_carc_40years,
         CDIing_nonc_6years = C95mgkg * Ing.factor_ncarc_6years,
         CDIing_nonc_25years = C95mgkg * Ing.factor_ncarc_25years,
         CDIing_nonc_40years = C95mgkg * Ing.factor_ncarc_40years,
         CDIing_carc_6years = C95mgkg * Ing.factor_carc_6years,
         CDIing_carc_25years = C95mgkg * Ing.factor_carc_25years,
         CDIing_carc_40years = C95mgkg * Ing.factor_carc_40years,
         DADder_nonc_6years = C95mgkg * ABS * der.factor_ncarc_6years,
         DADder_nonc_25years = C95mgkg * ABS * der.factor_ncarc_25years,
         DADder_nonc_40years = C95mgkg * ABS * der.factor_ncarc_40years,
         DADder_carc_6years = C95mgkg * ABS * der.factor_carc_6years,
         DADder_carc_25years = C95mgkg * ABS * der.factor_carc_25years,
         DADder_carc_40years = C95mgkg * ABS * der.factor_carc_40years,
         HQing_noncarc_6years = CDIing_nonc_6years/RfD,
         HQing_noncarc_25years = CDIing_nonc_25years/RfD,
         HQing_noncarc_40years = CDIing_nonc_40years/RfD,         
         HQinh_noncarc_6years = EC_noncarc_6years/(RfC * 1000),
         HQinh_noncarc_25years = EC_noncarc_25years/(RfC * 1000),
         HQinh_noncarc_40years = EC_noncarc_40years/(RfC * 1000),
         HQder_noncarc_6years = DADder_nonc_6years/(RfD * GIABS),
         HQder_noncarc_25years = DADder_nonc_25years/(RfD * GIABS),
         HQder_noncarc_40years = DADder_nonc_40years/(RfD * GIABS),
         CRinh_6years = IUR * EC_carc_6years,
         CRinh_25years = IUR * EC_carc_25years,
         CRinh_40years = IUR * EC_carc_40years,
         CRing_6years = SFo * CDIing_carc_6years,
         CRing_25years = SFo * CDIing_carc_25years,
         CRing_40years = SFo * CDIing_carc_40years,
         CRder_6years = DADder_carc_6years * SFo / GIABS,
         CRder_25years = DADder_carc_25years * SFo / GIABS,
         CRder_40years = DADder_carc_40years * SFo / GIABS
  )



##### Full dataset - results for both sites
health_risks_season <- rbind(health_risks_MIC_season, health_risks_NID_season)




########################################
################ OUTPUT ################

write_csv(health_risks_season,
          "./output/03-risk_assessment-health/health_risks_season.csv")
