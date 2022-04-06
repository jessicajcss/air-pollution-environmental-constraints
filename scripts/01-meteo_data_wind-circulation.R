#############################################


# SCRIPT for generating meteorological dataset to analyse horizontal wind circulation during the sampling period:
## i) full dataset (5-min/1-hour interval) without missing data filling [line: 45-90]
## ii) average/sum values, without missing data filling [line: 100 - 190]
## iii) average/sum values, with missing data filling [line: 200 - 240]


###################
# Last update: 2022-04-04
# Author: Santos-Silva, J. C. <jessica.jcss@gmail.com>

##############################################
############### LOAD PACKAGES ################

library(lubridate)  
library(tidyverse)
library(bazar)


##############################################
############## INPUT: RAW DATA ###############
############### FROM SAMPLING  ###############

concentration <- read_csv("./data/01-labdata_sampling/01-concentration_data.csv")


##############################################
############## INPUT: RAW DATA ###############
############### METEOROLOGICAL ###############

flotflux <- read_csv("./data/01-rawdata_meteo/flotflux_raw.csv")
summary(flotflux)

iateclube <- read_csv("./data/01-rawdata_meteo/iateclube_raw.csv")
summary(iateclube)

iateclube_homogen <- read_csv(
  "./output/01-climatol/dataset_output_climatol/iateclube_homogen.csv")
summary(iateclube_homogen)

flotflux_homogen <- read_csv(
  "./output/01-climatol/dataset_output_climatol/flotflux_homogen.csv")
summary(flotflux_homogen)






############################################################################
############## OUTPUT: METEOROLOGICAL DATA FOR EACH SAMPLE #################

## Description:  Full datasets are used (5-min interval) for the nearest meteorological stations to the sampling sites, raw data - without any filling.

### PROCEDURE ----
#### save the results in a list, and then combine into a single vector after the loop is done

out <- vector("list", length(flotflux))

for (i in 1:nrow(concentration)) {
  if ((concentration$site == "MIC")[i]) {
    start <- concentration$date[i]
    end <- concentration$date.end[i]
    subperiodo <- subset(iateclube, date <= ymd_hms(end) & date >= ymd_hms(start))
    out[[i]] <- subperiodo
    out[[i]]$site <- rep("MIC", nrow(out[[i]]))
    out[[i]]$Filter.ID <- rep(concentration$Filter.ID[i], nrow(out[[i]]))
  } else {
    start <- concentration$date[i]
    end <- concentration$date.end[i]
    subperiodo <- subset(flotflux, date <= ymd_hms(end) & date >= ymd_hms(start))
    out[[i]] <- subperiodo
    out[[i]]$site <- rep("NID", nrow(out[[i]]))
    out[[i]]$Filter.ID <- rep(concentration$Filter.ID[i], nrow(out[[i]]))
  }
}  

str(out)
output <- dplyr::bind_rows(out) %>% distinct() # combine the output into a single data frame



data_sampling_meteo <- distinct(Reduce(function(x, y) merge(x, y, all=TRUE,
                                                      by = c("site", "Filter.ID")),
                                 list(concentration, 
                                      output))) 
colnames(data_sampling_meteo)[
  which(names(data_sampling_meteo) == "date.x")] <- "date"
colnames(data_sampling_meteo)[
  which(names(data_sampling_meteo) == "date.y")] <- "date.meteo"

data_sampling_meteo <- data_sampling_meteo %>% arrange(date) 

######################################
############### OUTPUT ###############

write_csv(data_sampling_meteo, 
          "./output/01-input_wind-circulation/data_sampling_meteo_only2stations.csv")














############################################################################
############## OUTPUT: METEOROLOGICAL DATA FOR EACH SAMPLE #################

## Description:  Average (sum in the case of precipitation) values for the nearest meteorological stations to the sampling sites, raw data - with missing data filling.

### PROCEDURE ----

meteo_sampling_climatol <- concentration

meteo_sampling_climatol$temp <- NA
meteo_sampling_climatol$umid <- NA
meteo_sampling_climatol$prec <- NA
meteo_sampling_climatol$ws <- NA #scalar
meteo_sampling_climatol$wd <- NA
meteo_sampling_climatol$u <- NA
meteo_sampling_climatol$v <- NA



# WIND COMPONENTS: Create columns to and calculate the u and v wind components to all data

flotflux$u.wind <- u.wind(flotflux$ws, flotflux$wd)
flotflux$v.wind <- v.wind(flotflux$ws, flotflux$wd)

iateclube$u.wind <- u.wind(iateclube$ws, iateclube$wd)
iateclube$v.wind <- v.wind(iateclube$ws, iateclube$wd)

flotflux_homogen$u.wind <- u.wind(flotflux_homogen$ws, flotflux_homogen$wd)
flotflux_homogen$v.wind <- v.wind(flotflux_homogen$ws, flotflux_homogen$wd)

iateclube_homogen$u.wind <- u.wind(iateclube_homogen$ws, iateclube_homogen$wd)
iateclube_homogen$v.wind <- v.wind(iateclube_homogen$ws, iateclube_homogen$wd)




### First, considering only raw data from the nearest station

for (i in 1:nrow(meteo_sampling_climatol)) {
  if ((meteo_sampling_climatol$site == "MIC")[i]) {
    start <- meteo_sampling_climatol$date[i]
    end <- meteo_sampling_climatol$date.end[i]
    subperiodo <- subset(iateclube, date <= ymd_hms(end) & date >= ymd_hms(start))
    meteo_sampling_climatol$temp[i]<- mean(subperiodo$temp, na.rm=T)
    meteo_sampling_climatol$umid[i]<- mean(subperiodo$umid, na.rm=T)
    meteo_sampling_climatol$prec[i]<- sumNA(subperiodo$prec, na.rm=T)
    meteo_sampling_climatol$u[i]<- mean(subperiodo$u.wind, na.rm=T)
    meteo_sampling_climatol$v[i]<- mean(subperiodo$v.wind, na.rm=T)
    meteo_sampling_climatol$ws[i]<- mean(subperiodo$ws, na.rm=T)
    meteo_sampling_climatol$wd[i]<- wind_direction(meteo_sampling_climatol$u[i],
                                                   meteo_sampling_climatol$v[i])
  } else {
    start <- meteo_sampling_climatol$date[i]
    end <- meteo_sampling_climatol$date.end[i]
    subperiodo <- subset(flotflux, date <= ymd_hms(end) & date >= ymd_hms(start))
    meteo_sampling_climatol$temp[i]<- mean(subperiodo$temp, na.rm=T)
    meteo_sampling_climatol$umid[i]<- mean(subperiodo$umid, na.rm=T)
    meteo_sampling_climatol$prec[i]<- sumNA(subperiodo$prec, na.rm=T)
    meteo_sampling_climatol$u[i]<- mean(subperiodo$u.wind, na.rm=T)
    meteo_sampling_climatol$v[i]<- mean(subperiodo$v.wind, na.rm=T)
    meteo_sampling_climatol$ws[i]<- mean(subperiodo$ws, na.rm=T)
    meteo_sampling_climatol$wd[i]<- wind_direction(meteo_sampling_climatol$u[i],
                                                   meteo_sampling_climatol$v[i])
  }
}  

meteo_sampling_climatol <- meteo_sampling_climatol %>%
  mutate_at(vars(temp:v), ~replace(., . == 'NaN', NA))

######################################
############### OUTPUT ###############
### using only raw data

write_csv(meteo_sampling_climatol,
          "./output/01-input_wind-circulation/meteo_sampling_nofilling.csv")







### Then, filling missing data using climatol output for the nearest station


for (i in 1:nrow(meteo_sampling_climatol)) {
  if ((meteo_sampling_climatol$site == "MIC")[i] && 
      (is.na(meteo_sampling_climatol$temp))[i]) {
    start <- meteo_sampling_climatol$date[i]
    end <- meteo_sampling_climatol$date.end[i]
    subperiodo <- subset(iateclube_homogen, 
                         date <= as.Date(end) & date >= as.Date(start))
    meteo_sampling_climatol$temp[i]<- mean(subperiodo$temp, na.rm=T)
    meteo_sampling_climatol$umid[i]<- mean(subperiodo$umid, na.rm=T)
    meteo_sampling_climatol$prec[i]<- sumNA(subperiodo$prec, na.rm=T)
    meteo_sampling_climatol$u[i]<- mean(subperiodo$u.wind, na.rm=T)
    meteo_sampling_climatol$v[i]<- mean(subperiodo$v.wind, na.rm=T)
    meteo_sampling_climatol$ws[i]<- mean(subperiodo$ws, na.rm=T)
    meteo_sampling_climatol$wd[i]<- wind_direction(meteo_sampling_climatol$u[i],
                                                   meteo_sampling_climatol$v[i])
  } else if((meteo_sampling_climatol$site == "NID")[i] && 
            (is.na(meteo_sampling_climatol$temp))[i]) {
    start <- meteo_sampling_climatol$date[i]
    end <- meteo_sampling_climatol$date.end[i]
    subperiodo <- subset(flotflux_homogen, 
                         date <= as.Date(end) & date >= as.Date(start))
    meteo_sampling_climatol$temp[i]<- mean(subperiodo$temp, na.rm=T)
    meteo_sampling_climatol$umid[i]<- mean(subperiodo$umid, na.rm=T)
    meteo_sampling_climatol$prec[i]<- sumNA(subperiodo$prec, na.rm=T)
    meteo_sampling_climatol$u[i]<- mean(subperiodo$u.wind, na.rm=T)
    meteo_sampling_climatol$v[i]<- mean(subperiodo$v.wind, na.rm=T)
    meteo_sampling_climatol$ws[i]<- mean(subperiodo$ws, na.rm=T)
    meteo_sampling_climatol$wd[i]<- wind_direction(meteo_sampling_climatol$u[i],
                                                   meteo_sampling_climatol$v[i])
  }
}  

meteo_sampling_climatol <- meteo_sampling_climatol %>%
  mutate_at(vars(temp:v), ~replace(., . == 'NaN', NA))

######################################
############### OUTPUT ###############
### missing data filling with output from climatol
write_csv(meteo_sampling_climatol,
          "./output/01-input_wind-circulation/meteo_sampling_climatol.csv")


