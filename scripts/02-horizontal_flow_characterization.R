#############################################


# SCRIPT to classify the horizontal flow conditions at the measurement point by calculating and comparing discrete integral quantities such as the ‘wind run’ (S), which represents a measure of the total distance the parcel travelled in that time, and the ‘recirculation factor’ (R). 

# i) For historical data: data preparation [line: 100 - 270] 
# and classification [line: 290 - 565]
# ii) For the sampling period [line: 590 - 805]

###################
# Last update: 2022-07-01 corrected lines 600/601 e 681/682 [date.meteo]
# Author: Santos-Silva, J. C. <jessica.jcss@gmail.com>

##############################################
############### LOAD PACKAGES ################

library(lubridate)  
library(tidyverse)
library(bazar) # sumNA
library(matrixStats)
library(REdaS)






############## INPUT: RAW DATA ###############
############### FROM SAMPLING  ###############

# FULL DATASET: 5-min meteorological data, without any filling
data_sampling_meteo <- read_csv(
"./output/01-input_wind-circulation/data_sampling_meteo_only2stations.csv",
col_types = cols(
  bromide = col_double(),
  ammonium = col_double(),
  Hg = col_number()))

# FULL DATASET: sampling period-averaged meteorological data, with filling
meteo_sampling_climatol <- read_csv(
  "./output/01-input_wind-circulation/meteo_sampling_climatol.csv") 

############## INPUT: RAW DATA ###############
############### METEOROLOGICAL ###############


#### 1-HOUR INTERVAL
aeroporto <- read_csv("./data/01-rawdata_meteo/aeroporto_raw.csv") %>% 
  arrange(date)
summary(aeroporto)

#### 5-MIN INTERVAL

flotflux <- read_csv("./data/01-rawdata_meteo/flotflux_raw.csv") %>%
  arrange(date)
summary(flotflux)

iateclube <- read_csv("./data/01-rawdata_meteo/iateclube_raw.csv") %>%
  arrange(date)
summary(iateclube)

cubatao <- read_csv("./data/01-rawdata_meteo/cubatao_raw.csv") %>%
  arrange(date)
summary(cubatao)

aguasdejoi <- read_csv("./data/01-rawdata_meteo/aguasdejoi_raw.csv") %>%
  arrange(date)
summary(aguasdejoi)

rodovia <- read_csv("./data/01-rawdata_meteo/rodovia_raw.csv") %>% 
  arrange(date)
summary(rodovia)

itaum <- read_csv("./data/01-rawdata_meteo/itaum_raw.csv") %>% 
  arrange(date)
summary(itaum)

ceasa <- read_csv("./data/01-rawdata_meteo/ceasa_raw.csv") %>% 
  arrange(date)
summary(ceasa)


##############################################
##############################################
##############################################
########### WIND: DATA ANALYSIS ##############
##############################################
##############################################
##############################################



##############################################################
#### HORIZONTAL FLOW CHARACTERIZATION: HISTORICAL DATASET ####
##### Procedure: To prepare the dataset ----

#### 1-HOUR INTERVAL ----

dataset_meteo <- aeroporto

interval_hour <- as.data.frame(seq.POSIXt(
  as_datetime(head(dataset_meteo[["date"]], n = 1)),
  as_datetime(tail(dataset_meteo[["date"]], n = 1)),
  by = "hour"))
colnames(interval_hour) <- c("date")


#### full hourly dataset
dataset_meteo <- left_join(interval_hour, dataset_meteo, by = "date")

#### wind components
dataset_meteo$u.wind <- u.wind(dataset_meteo$ws, dataset_meteo$wd)
dataset_meteo$v.wind <- v.wind(dataset_meteo$ws, dataset_meteo$wd)

#### variable average
meteo_days <- aggregate(dataset_meteo[c(2:ncol(dataset_meteo))],
                        list(cut(dataset_meteo[["date"]], "1 day", 
                                 right = FALSE)), 
                        mean, na.rm = T)
names(meteo_days)[1] <- "date"
meteo_days$date <- as.Date(meteo_days$date)
meteo_days$wd <- wind_direction(meteo_days$u.wind, meteo_days$v.wind)


meteo_days <- meteo_days %>%
  mutate_at(vars(temp:v.wind), ~replace(., . == 'NaN', NA))






##### To calculate the discrete integral quantities R, S, L ---- 
# Only days with valid data >= 75% were considered

for (i in 1:nrow(meteo_days)) {
  start <- meteo_days$date[i]
  end <- meteo_days$date[i] + 1
  time_T <- 60*60 # interval in seconds
  subperiodo <- subset(dataset_meteo, as.Date(date) < end & 
                         as.Date(date) >= start)
  meteo_days$count_NA[i] <- sum(length(which(is.na(subperiodo$u.wind))))/length(subperiodo$u.wind)
  if (meteo_days$count_NA[i] <= 0.25) { 
  meteo_days$L[i] <- L_it(time_T, subperiodo$u.wind, subperiodo$v.wind)
  meteo_days$S[i] <- S_it(time_T, subperiodo$u.wind, subperiodo$v.wind)
  meteo_days$R[i] <- R_it(meteo_days$L[i], meteo_days$S[i])
  } else {
    meteo_days$L[i] <- NA
    meteo_days$S[i] <- NA
    meteo_days$R[i] <- NA
  }
}

# To convert from meters to kilometers
meteo_days$L <- meteo_days$L/1000
meteo_days$S <- meteo_days$S/1000



# Final dataset: SBJV airport station
dataset_aeroporto_recirculacao <- meteo_days



######################################
######################################


#### 5-MIN INTERVAL ----
##### Procedure: it should be run to each meteorological station dataset (manually)

dataset_meteo <- flotflux %>% select(-prec)
dataset_meteo <- iateclube %>% select(-prec)
dataset_meteo <- cubatao %>% select(-prec)
dataset_meteo <- aguasdejoi %>% select(-prec)
dataset_meteo <- rodovia %>% select(-prec)
dataset_meteo <- itaum %>% select(-prec)
dataset_meteo <- ceasa %>% select(-prec)


intervalo_5min <- as.data.frame(seq.POSIXt(
  as_datetime(head(dataset_meteo[["date"]], n = 1)),
  as_datetime(tail(dataset_meteo[["date"]], n = 1)),
  by = "5 min"))
colnames(intervalo_5min) <- c("date")



#### full hourly dataset
dataset_meteo <- left_join(intervalo_5min, dataset_meteo, by = "date")

#### wind components
dataset_meteo$u.wind <- u.wind(dataset_meteo$ws, dataset_meteo$wd)
dataset_meteo$v.wind <- v.wind(dataset_meteo$ws, dataset_meteo$wd)

#### variable average
meteo_days <- aggregate(dataset_meteo[c(2:ncol(dataset_meteo))],
                        list(cut(dataset_meteo[["date"]], "1 day", 
                                 right = FALSE)), 
                        mean, na.rm = T)
names(meteo_days)[1] <- "date"
meteo_days$date <- as.Date(meteo_days$date)
meteo_days$wd <- wind_direction(meteo_days$u.wind, meteo_days$v.wind)

meteo_days <- meteo_days %>%
  mutate_at(vars(temp:v.wind), ~replace(., . == 'NaN', NA))




##### To calculate the discrete integral quantities R, S, L ---- 
# Only days with valid data >= 75% were considered

for (i in 1:nrow(meteo_days)) {
  start <- meteo_days$date[i]
  end <- meteo_days$date[i] + 1
  time_T <- 5*60 # interval in seconds
  subperiodo <- subset(dataset_meteo, as.Date(date) < end & 
                         as.Date(date) >= start)
  meteo_days$count_NA[i] <- sum(length(which(is.na(subperiodo$u.wind))))/length(subperiodo$u.wind)
  if (meteo_days$count_NA[i] <= 0.25) { 
    meteo_days$L[i] <- L_it(time_T, subperiodo$u.wind, subperiodo$v.wind)
    meteo_days$S[i] <- S_it(time_T, subperiodo$u.wind, subperiodo$v.wind)
    meteo_days$R[i] <- R_it(meteo_days$L[i], meteo_days$S[i])
  } else {
    meteo_days$L[i] <- NA
    meteo_days$S[i] <- NA
    meteo_days$R[i] <- NA
  }
}



# To convert from meters to kilometers
meteo_days$L <- meteo_days$L/1000
meteo_days$S <- meteo_days$S/1000

meteo_days$R[meteo_days$R < 0] <- 0 #correction




# Final dataset: Flotflux station
dataset_flotflux_recirculacao <- meteo_days

# Final dataset: IateClube station
dataset_iateclube_recirculacao <- meteo_days

# Final dataset: Cubatao station
dataset_cubatao_recirculacao <- meteo_days

# Final dataset: Aguas de Joinville station
dataset_aguasdejoi_recirculacao <- meteo_days

# Final dataset: Rodovia do Arroz station
dataset_rodovia_recirculacao <- meteo_days

# Final dataset: Itaum station
dataset_itaum_recirculacao <- meteo_days

# Final dataset: Ceasa station
dataset_ceasa_recirculacao <- meteo_days


######################################
######################################















#################################################################
###### HORIZONTAL FLOW CHARACTERIZATION: HISTORICAL DATASET #####
##### Procedure: To classify horizontal wind flow conditions ----



###### Classification for individual stations, average daily x CTIs ----
##### CTIs according to Russo et al. 2018


# A unique dataset containing discrete integral quantities (S, R) across meteorological stations to determine critical values of dispersion from the historical dataset (CTIs) available for the period from 2012 to 2021 for the municipality region.

dataset <- unique(
  Reduce(function(x, y) merge(x, y, all=TRUE, by = "date"), 
         list(dataset_iateclube_recirculacao[c("date", "S", "R")],
              dataset_flotflux_recirculacao[c("date", "S", "R")], 
              dataset_cubatao_recirculacao[c("date", "S", "R")], 
              dataset_aguasdejoi_recirculacao[c("date", "S", "R")], 
              dataset_aeroporto_recirculacao[c("date", "S", "R")],
              dataset_rodovia_recirculacao[c("date", "S", "R")], 
              dataset_itaum_recirculacao[c("date", "S", "R")],
              dataset_ceasa_recirculacao[c("date", "S", "R")])))

colnames(dataset) <- c("date", "S_iate", "R_iate", 
                       "S_flot", "R_flot", 
                       "S_cub", "R_cub",
                       "S_aguas", "R_aguas",
                       "S_aero", "R_aero",
                       "S_rodo", "R_rodo",
                       "S_itaum", "R_itaum",
                       "S_ceasa", "R_ceasa")



# S (Wind run) average across meteorological stations
dataset$S_avg <- dataset %>% 
  select(starts_with("S")) %>% 
  rowMeans(na.rm=T)

# R (Recirculation) average across meteorological stations
dataset$R_avg <- dataset %>%
  select(starts_with("R")) %>% 
  rowMeans(na.rm=T)

# S (Wind run) std deviation across meteorological stations
dataset$S_sd <- dataset %>% 
  select(starts_with("S")) %>% 
  as.matrix() %>%
  rowSds(na.rm=T)

# R (Recirculation) std deviation across meteorological stations
dataset$R_sd <- dataset %>%
  select(starts_with("R")) %>% 
  as.matrix() %>%
  rowSds(na.rm=T)


dataset <- dataset %>%
  mutate_at(vars(S_iate:R_sd), ~replace(., . == 'NaN', NA))


# correct average and std deviation values for when there is data for only one meteorological station

for (i in 1:nrow(dataset)) {
  if (dataset$S_sd[i] == 0 | is.na(dataset$S_sd[i])) {
    dataset$S_avg[i] <- NA
    dataset$S_sd[i] <- NA
    dataset$R_avg[i] <- NA
    dataset$R_sd[i] <- NA
  }
}



###################################

##### Avg and SD values by meteorological station during the whole historical period and the resulting CTIs following Russo et al.(2018).

df <- data.frame(
  matrix(NA, 
         ncol = 4, # number of variables: station, S_avg, R_avg, Classification
         nrow = 9) # number of stations + average
)

colnames(df) <- c("Station", "S_avg", "R_avg", "Classification")

df$Station <- c("average",
                "iateclube", 
                "flotflux", 
                "cubatao", 
                "aguasdejoi", 
                "aeroporto",                
                "rodovia",
                "itaum",
                "ceasa")


# Average all stations, full period ----
df[df$Station == "average", "S_avg"] <- mean(dataset$S_avg, na.rm = T)
df[df$Station == "average", "R_avg"] <- mean(dataset$R_avg, na.rm = T)
df[df$Station == "iateclube", "S_avg"] <- mean(dataset$S_iate, na.rm = T)
df[df$Station == "iateclube", "R_avg"] <- mean(dataset$R_iate, na.rm = T)
df[df$Station == "flotflux", "S_avg"] <- mean(dataset$S_flot, na.rm = T)
df[df$Station == "flotflux", "R_avg"] <- mean(dataset$R_flot, na.rm = T)
df[df$Station == "cubatao", "S_avg"] <- mean(dataset$S_cub, na.rm = T)
df[df$Station == "cubatao", "R_avg"] <- mean(dataset$R_cub, na.rm = T)
df[df$Station == "aguasdejoi", "S_avg"] <- mean(dataset$S_aguas, na.rm = T)
df[df$Station == "aguasdejoi", "R_avg"] <- mean(dataset$R_aguas, na.rm = T)
df[df$Station == "aeroporto", "S_avg"] <- mean(dataset$S_aero, na.rm = T)
df[df$Station == "aeroporto", "R_avg"] <- mean(dataset$R_aero, na.rm = T)
df[df$Station == "rodovia", "S_avg"] <- mean(dataset$S_rodo, na.rm = T)
df[df$Station == "rodovia", "R_avg"] <- mean(dataset$R_rodo, na.rm = T)
df[df$Station == "itaum", "S_avg"] <- mean(dataset$S_itaum, na.rm = T)
df[df$Station == "itaum", "R_avg"] <- mean(dataset$R_itaum, na.rm = T)
df[df$Station == "ceasa", "S_avg"] <- mean(dataset$S_ceasa, na.rm = T)
df[df$Station == "ceasa", "R_avg"] <- mean(dataset$R_ceasa, na.rm = T)


df[df$Station == "average", "P25_R"] <- quantile(dataset$R_avg, 
                                                 probs = c(.25), 
                                                 na.rm = T)
df[df$Station == "average", "P75_S"] <- quantile(dataset$S_avg, 
                                                 probs = c(.75), 
                                                 na.rm = T)



##### Procedure: Classification of horizontal wind flow conditions in each meteorological stations and for the municipality region according to the average values. ----


# Average daily CTIs:
avg_S_avg <- round(df$S_avg[df$Station == "average"], 1)
avg_R_avg <- round(df$R_avg[df$Station == "average"], 1)
P75_S <- round(df$P75_S[df$Station == "average"], 1)
P25_R <- round(df$P25_R[df$Station == "average"], 1)



###### Classification
df <- df %>% 
  mutate(Classification = case_when(
    round(S_avg, 1) <= round(avg_S_avg, 1) & 
      round(R_avg, 1) >= round(avg_R_avg, 1) ~"Stagnation/Recirculation",
    round(S_avg, 1) <= round(avg_S_avg, 1) ~ "Stagnation", 
    round(R_avg, 1) >= round(avg_R_avg, 1) ~ "Recirculation",
    round(S_avg, 1) >= round(P75_S[df$Station == "average"], 1) & 
      round(R_avg, 1) <= round(P25_R[df$Station == "average"], 1) ~ "Ventilation",
    TRUE ~ as.character(Classification)))

historical_classification <- df








##### Procedure: Classification to each daily value, to calculate percent occurrence by meteorological station [CTIs according with Russo et al (2018)] ----

dataset <- dataset %>% mutate(Class_iate = NA,
                              Class_cub = NA,
                              Class_flot = NA,
                              Class_aguas = NA,
                              Class_aero = NA,
                              Class_rodo = NA,
                              Class_itaum = NA,
                              Class_ceasa = NA,
                              Classification = NA)

# Average daily CTIs:
avg_S_avg <- df$S_avg[df$Station == "average"]
avg_R_avg <- df$R_avg[df$Station == "average"]
P75_S <- df$P75_S[df$Station == "average"]
P25_R <- df$P25_R[df$Station == "average"]



# Classification: IateClube station
dataset <- dataset %>%
  mutate(Class_iate = case_when(
    S_iate <= avg_S_avg & R_iate >= avg_R_avg ~"Stagnation/Recirculation",
    S_iate <= avg_S_avg ~ "Stagnation", 
    R_iate >= avg_R_avg ~ "Recirculation",
    S_iate >= P75_S & R_iate <= P25_R ~"Ventilation",
    TRUE ~ as.character(NA)))

# Classification: IateClube station
dataset <- dataset %>%
  mutate(Class_flot = case_when(
    S_flot <= avg_S_avg & R_flot >= avg_R_avg ~"Stagnation/Recirculation",
    S_flot <= avg_S_avg ~ "Stagnation", 
    R_flot >= avg_R_avg ~ "Recirculation",
    S_flot >= P75_S & R_flot <= P25_R ~"Ventilation",
    TRUE ~ as.character(NA)))

# Classification: Cubatao station
dataset <- dataset %>%
  mutate(Class_cub = case_when(
    S_cub <= avg_S_avg & R_cub >= avg_R_avg ~"Stagnation/Recirculation", 
    S_cub <= avg_S_avg ~ "Stagnation",
    R_cub >= avg_R_avg ~ "Recirculation",
    S_cub >= P75_S & R_cub <= P25_R ~"Ventilation",
    TRUE ~ as.character(NA)))

# Classification: Aguas de Joinville station
dataset <- dataset %>%
  mutate(Class_aguas = case_when(
    S_aguas <= avg_S_avg & R_aguas >= avg_R_avg ~"Stagnation/Recirculation",
    S_aguas <= avg_S_avg ~ "Stagnation", 
    R_aguas >= avg_R_avg ~ "Recirculation",
    S_aguas >= P75_S & R_aguas <= P25_R ~"Ventilation",
    TRUE ~ as.character(NA)))

# Classification: SBJV airport station
dataset <- dataset %>%
  mutate(Class_aero = case_when(
    S_aero <= avg_S_avg & R_aero >= avg_R_avg ~"Stagnation/Recirculation",
    S_aero <= avg_S_avg ~ "Stagnation",
    R_aero >= avg_R_avg ~ "Recirculation",
    S_aero >= P75_S & R_aero <= P25_R ~"Ventilation",
    TRUE ~ as.character(NA)))

# Classification: Rodovia do Arroz station
dataset <- dataset %>%
  mutate(Class_rodo = case_when(
    S_rodo <= avg_S_avg & R_rodo >= avg_R_avg ~"Stagnation/Recirculation",
    S_rodo <= avg_S_avg ~ "Stagnation",
    R_rodo >= avg_R_avg ~ "Recirculation",
    S_rodo >= P75_S & R_rodo <= P25_R ~"Ventilation",
    TRUE ~ as.character(NA)))

# Classification: Itaum station
dataset <- dataset %>%
  mutate(Class_itaum = case_when(
    S_itaum <= avg_S_avg & R_itaum >= avg_R_avg ~"Stagnation/Recirculation",
    S_itaum <= avg_S_avg ~ "Stagnation",
    R_itaum >= avg_R_avg ~ "Recirculation",
    S_itaum >= P75_S & R_itaum <= P25_R ~"Ventilation",
    TRUE ~ as.character(NA)))

# Classification: Ceasa station
dataset <- dataset %>%
  mutate(Class_ceasa = case_when(
    S_ceasa <= avg_S_avg & R_ceasa >= avg_R_avg ~"Stagnation/Recirculation",
    S_ceasa <= avg_S_avg ~ "Stagnation", 
    R_ceasa >= avg_R_avg ~ "Recirculation",
    S_ceasa >= P75_S & R_ceasa <= P25_R ~"Ventilation", 
    TRUE ~ as.character(NA)))

# Classification to daily-average values among stations 
dataset <- dataset %>%
  mutate(Classification = case_when(
    S_avg <= avg_S_avg & R_avg >= avg_R_avg ~"Stagnation/Recirculation",
    S_avg <= avg_S_avg ~ "Stagnation", 
    R_avg >= avg_R_avg ~ "Recirculation",
    S_avg >= P75_S & R_avg <= P25_R ~"Ventilation",
    TRUE ~ as.character(NA)))


historical_classification_daily <- dataset





######################################
############### OUTPUT ###############

write_csv(historical_classification,
        "./output/02-horizontal_flow_characterization/historical_classification.csv"
        )

write_csv(historical_classification_daily,
          "./output/02-horizontal_flow_characterization/historical_classification_daily.csv"
          )























##############################################################
#### HORIZONTAL FLOW CHARACTERIZATION: SAMPLING PERIOD ####
##### Procedure: To prepare the dataset ----
# Criteria: only raw meteorological dataset (5-min interval) is used 
# (i.e., without any filling)



#### DATASET - site: MIC ----


dataset_MIC <- subset(data_sampling_meteo, site == "MIC")
dataset_MIC_meteo <- dataset_MIC %>% 
  select(date.meteo:prec) %>% 
  na.omit() %>% arrange(date.meteo) %>% distinct()


# full 5-min interval
interval <- as.data.frame(seq.POSIXt(
  as_datetime(head(na.omit(dataset_MIC[["date.meteo"]]), n = 1)),
  as_datetime(tail(na.omit(dataset_MIC[["date.meteo"]]), n = 1)),
  by = "5 min"))
colnames(interval) <- c("date.meteo")


dataset_MIC_meteo <- merge(interval, 
                           dataset_MIC_meteo, 
                           by = "date.meteo", all = T)

# wind components
dataset_MIC_meteo$u.wind <- u.wind(dataset_MIC_meteo$ws, 
                                   dataset_MIC_meteo$wd)
dataset_MIC_meteo$v.wind <- v.wind(dataset_MIC_meteo$ws, 
                                   dataset_MIC_meteo$wd)



##### To calculate the discrete integral quantities R, S, L ---- 
# Only days with valid data >= 75% were considered

periodo <- dataset_MIC %>% 
  select(date, date.end) %>% 
  distinct() %>% 
  na.omit %>%
  mutate(count_NA = NA,
         Lit = NA, Sit = NA, Rit = NA)


for (i in 1:nrow(periodo)) {
  start <- periodo$date[i]
  end <- periodo$date.end[i]
  time_T <- 5*60 # interval in seconds
  subperiodo <- subset(dataset_MIC_meteo, 
                       date.meteo <= ymd_hms(end) & 
                         date.meteo >= ymd_hms(start))
  periodo$count_NA[i] <- sum(length(which(is.na(subperiodo$u.wind))))/length(subperiodo$u.wind)
  if (periodo$count_NA[i] <= 0.25) {
    subperiodo <- subperiodo %>% fill(names(subperiodo))
    periodo$Lit[i] <- L_it(time_T, subperiodo$u.wind, subperiodo$v.wind)
    periodo$Sit[i] <- S_it(time_T, subperiodo$u.wind, subperiodo$v.wind)
    periodo$Rit[i] <- R_it(periodo$Lit[i], periodo$Sit[i])
  } else {
    periodo$Lit[i] <- NA
    periodo$Sit[i] <- NA
    periodo$Rit[i] <- NA
  }
}


dataset_MIC_concentration <- dataset_MIC %>% 
  select(-date.meteo, -temp, 
         -umid, -ws, 
         -wd, -prec) %>% 
  subset(!is.na("date.end")) %>% unique()


windflow_MIC <- distinct(Reduce(
  function(x, y) merge(x, y, all = TRUE,
                       by = c("date", "date.end")),
  list(dataset_MIC_concentration, periodo)))


# To convert from meters to kilometers
windflow_MIC$Lit <- windflow_MIC$Lit/1000
windflow_MIC$Sit <- windflow_MIC$Sit/1000



#######################


#### DATASET - site: NID ----


dataset_NID <- subset(data_sampling_meteo, site == "NID")
dataset_NID_meteo <- dataset_NID %>% 
  select(date.meteo:prec) %>% 
  na.omit() %>% arrange(date.meteo) %>% distinct()


# full 5-min interval
interval <- as.data.frame(seq.POSIXt(
  as_datetime(head(na.omit(dataset_NID[["date.meteo"]]), n = 1)),
  as_datetime(tail(na.omit(dataset_NID[["date.meteo"]]), n = 1)),
  by = "5 min"))
colnames(interval) <- c("date.meteo")


dataset_NID_meteo <- merge(interval, 
                           dataset_NID_meteo, 
                           by = "date.meteo", all = T)

# wind components
dataset_NID_meteo$u.wind <- u.wind(dataset_NID_meteo$ws, 
                                   dataset_NID_meteo$wd)
dataset_NID_meteo$v.wind <- v.wind(dataset_NID_meteo$ws, 
                                   dataset_NID_meteo$wd)



##### To calculate the discrete integral quantities R, S, L ---- 
# Only days with valid data >= 75% were considered

periodo <- dataset_NID %>% 
  select(date, date.end) %>% 
  distinct() %>% 
  na.omit %>%
  mutate(count_NA = NA,
         Lit = NA, Sit = NA, Rit = NA)


for (i in 1:nrow(periodo)) {
  start <- periodo$date[i]
  end <- periodo$date.end[i]
  time_T <- 5*60 # interval in seconds
  subperiodo <- subset(dataset_NID_meteo, 
                       date.meteo <= ymd_hms(end) & 
                         date.meteo >= ymd_hms(start))
  periodo$count_NA[i] <- sum(length(which(is.na(subperiodo$u.wind))))/length(subperiodo$u.wind)
  if (periodo$count_NA[i] <= 0.25) {
    subperiodo <- subperiodo %>% fill(names(subperiodo))
    periodo$Lit[i] <- L_it(time_T, subperiodo$u.wind, subperiodo$v.wind)
    periodo$Sit[i] <- S_it(time_T, subperiodo$u.wind, subperiodo$v.wind)
    periodo$Rit[i] <- R_it(periodo$Lit[i], periodo$Sit[i])
  } else {
    periodo$Lit[i] <- NA
    periodo$Sit[i] <- NA
    periodo$Rit[i] <- NA
  }
}


dataset_NID_concentration <- dataset_NID %>% 
  select(-date.meteo, -temp, 
         -umid, -ws, 
         -wd, -prec) %>% 
  subset(!is.na("date.end")) %>% unique()


windflow_NID <- distinct(Reduce(
  function(x, y) merge(x, y, all = TRUE,
                       by = c("date", "date.end")),
  list(dataset_NID_concentration, periodo)))


# To convert from meters to kilometers
windflow_NID$Lit <- windflow_NID$Lit/1000
windflow_NID$Sit <- windflow_NID$Sit/1000



# FINAL WIND FLOW DATASET
windflow_sites <- rbind(windflow_MIC, windflow_NID)


# ADDING averaged meteorological data
meteo_periodo <- meteo_sampling_climatol %>% select(rep, site,
                                                   date, date.end,
                                                   temp, umid,
                                                   ws, wd, prec)
concentracao_meteo_windflow <- distinct(
  Reduce(function(x, y) merge(x, y, all = TRUE, 
                              by = c("rep", "site", "date", "date.end")),
         list(windflow_sites, meteo_periodo)))

concentracao_meteo_windflow <- concentracao_meteo_windflow %>%
  relocate("temp", "umid", "ws", "wd", "prec", .after = calcium) %>%
  relocate("rep", .after = Filter.ID) %>% 
  arrange(date)


# Reference datasets
dataset <- concentracao_meteo_windflow
df <- historical_classification


# Average daily CTIs:
avg_S_avg <- round(df$S_avg[df$Station == "average"], 1)
avg_R_avg <- round(df$R_avg[df$Station == "average"], 1)
P75_S <- round(df$P75_S[df$Station == "average"], 1)
P25_R <- round(df$P25_R[df$Station == "average"], 1)



#### Classification to sampling-period-average values ----
dataset <- dataset %>%
  mutate(wind_classification = case_when(
    Sit <= avg_S_avg & Rit >= avg_R_avg ~"Stagnation/Recirculation",
    Sit <= avg_S_avg ~ "Stagnation", 
    Rit >= avg_R_avg ~ "Recirculation",
    Sit >= P75_S & Rit <= P25_R ~"Ventilation",
    TRUE ~ as.character(NA)))

summary(dataset)



dataset <- dataset %>% 
  mutate_at(vars(wind_classification), 
            ~replace(., is.na(.) & count_NA <= 0.25, "Unclassified"))

dataset$wind_classification <- factor(dataset$wind_classification, 
                                          levels = c("Recirculation", 
                                                     "Stagnation", 
                                                     "Stagnation/Recirculation", 
                                                     "Ventilation",
                                                     "Unclassified"))



######################################
############### OUTPUT ###############

write_csv(dataset, 
          "./output/02-horizontal_flow_characterization/concentracao_meteo_windflow.csv")

