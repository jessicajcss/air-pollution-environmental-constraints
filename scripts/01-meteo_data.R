#############################################


# SCRIPT for missing data, homogenization and interpolation were done using the **Climatol package in R**.

# Date: 2021-11-20
# Author: Santos-Silva, J. C. <jessica.jcss@gmail.com>

##############################################
############### LOAD PACKAGES ################


library(lubridate)
library(tidyverse)
library(readxl)
library(magrittr)
library(reshape2)
library(bazar) 
library(climatol)



##############################################
############## INPUT: RAW DATA ###############
################ 2012 - 2020 #################

flotflux <- read_csv("./data/01-rawdata_meteo/flotflux_raw.csv")
summary(flotflux)

iateclube <- read_csv("./data/01-rawdata_meteo/iateclube_raw.csv")
summary(iateclube)

cubatao <- read_csv("./data/01-rawdata_meteo/cubatao_raw.csv")
summary(cubatao)

aguasdejoi <- read_csv("./data/01-rawdata_meteo/aguasdejoi_raw.csv")
summary(aguasdejoi)

ceasa <- read_csv("./data/01-rawdata_meteo/ceasa_raw.csv")
summary(ceasa)

itaum <- read_csv("./data/01-rawdata_meteo/itaum_raw.csv")
summary(itaum)

rodovia <- read_csv("./data/01-rawdata_meteo/rodovia_raw.csv")
summary(rodovia)

aeroporto <- read_csv("./data/01-rawdata_meteo/aeroporto_raw.csv")
summary(aeroporto)

pluviometros <- read_csv("./data/01-rawdata_meteo/pluviometros_raw.csv")
summary(pluviometros)




#####################################################
############### PREPARAÇÃO DOS DADOS ################


### Unifying variables across datasets ----

#### variable: TEMPERATURE
temperature <- unique(Reduce(function(x,y) merge(x, y, all=TRUE, 
                                                 by = "date"), 
                             list(aeroporto[c("date", "temp")], 
                                  cubatao[c("date", "temp")], 
                                  aguasdejoi[c("date", "temp")], 
                                  iateclube[c("date", "temp")],
                                  flotflux[c("date", "temp")], 
                                  ceasa[c("date", "temp")], 
                                  itaum[c("date", "temp")],
                                  rodovia[c("date", "temp")])))
colnames(temperature) <- c("date", "aeroporto", "cubatao", "aguasdejoi",
                           "iateclube", "flotflux", "ceasa", "itaum", "rodovia")


#### variable: RELATIVE HUMIDITY
humidity <- unique(Reduce(function(x, y) merge(x, y, all=TRUE, 
                                              by = "date"), 
                         list(aeroporto[c("date", "umid")], 
                              cubatao[c("date", "umid")], 
                              aguasdejoi[c("date", "umid")], 
                              iateclube[c("date", "umid")],
                              flotflux[c("date", "umid")], 
                              ceasa[c("date", "umid")], 
                              itaum[c("date", "umid")],
                              rodovia[c("date", "umid")])))
colnames(humidity) <- c("date", "aeroporto", "cubatao", "aguasdejoi",
                       "iateclube", "flotflux", "ceasa", "itaum", "rodovia")


#### variable: WIND SPEED
windspeed <- unique(Reduce(function(x, y) merge(x, y, all=TRUE, 
                                                by = "date"), 
                           list(aeroporto[c("date", "ws")], 
                                cubatao[c("date", "ws")], 
                                aguasdejoi[c("date", "ws")], 
                                iateclube[c("date", "ws")], 
                                flotflux[c("date", "ws")], 
                                ceasa[c("date", "ws")], 
                                itaum[c("date", "ws")],
                                rodovia[c("date", "ws")])))
colnames(windspeed) <- c("date", "aeroporto", "cubatao", "aguasdejoi",
                         "iateclube", "flotflux", "ceasa", "itaum", "rodovia")


#### variable: WIND DIRECTION
winddirection <- unique(Reduce(function(x, y) merge(x, y, all=TRUE, 
                                                    by = "date"), 
                               list(aeroporto[c("date", "wd")], 
                                    cubatao[c("date", "wd")], 
                                    aguasdejoi[c("date", "wd")], 
                                    iateclube[c("date", "wd")], 
                                    flotflux[c("date", "wd")], 
                                    ceasa[c("date", "wd")], 
                                    itaum[c("date", "wd")],
                                    rodovia[c("date", "wd")])))
colnames(winddirection) <- c("date", "aeroporto", "cubatao", "aguasdejoi",
                             "iateclube", "flotflux", "ceasa", "itaum", "rodovia")


#### variable: PRECIPITATION
precipitation <- unique(Reduce(function(x, y) merge(x, y, all=TRUE, 
                                                   by = "date"), 
                              list(pluviometros,
                                   cubatao[c("date", "prec")], 
                                   aguasdejoi[c("date", "prec")], 
                                   iateclube[c("date", "prec")], 
                                   flotflux[c("date", "prec")],
                                   ceasa[c("date", "prec")], 
                                   itaum[c("date", "prec")],
                                   rodovia[c("date", "prec")])))
colnames(precipitation) <- c("date", "aventureiro", "centro", 
                            "costaesilva", "estacaocidadania", "estradageral",
                            "iririu", "itinga", "novabrasilia", "paranaguamirim",
                            "cubatao", "aguasdejoi", "iateclube", "flotflux", 
                            "ceasa", "itaum", "rodovia")






### Standardizing datasets: to 1-hour intervals. The following was applied to all meteorological stations with a 5-min interval collection and rain gauges dataset (10-min interval collection) ----




#### For precipitation data: sum --

precipitation_daily <- aggregate(precipitation[c(2:ncol(precipitation))], 
                                  list(cut(precipitation[["date"]], 
                                           "1 day", 
                                           right = T)), 
                                  sumNA, na.rm = T)
colnames(precipitation_daily)[1] <- "date"
precipitation_daily[["date"]] <- as.Date(precipitation_daily[["date"]])



#### For temperature, relative humidity and wind speed data: mean --


##### variable: TEMPERATURE
temperature_daily <- aggregate(temperature[c(2:ncol(temperature))], 
                                list(cut(temperature[["date"]], 
                                         "1 day", 
                                         right = F)), 
                                mean, na.rm = T)

colnames(temperature_daily)[1] <- "date"
temperature_daily[["date"]] <- as.Date(temperature_daily[["date"]])

temperature_daily[c(2:ncol(temperature_daily))] <- 
  replace(temperature_daily[c(2:ncol(temperature_daily))],
          temperature_daily[c(2:ncol(temperature_daily))] == 'NaN', NA)


##### variable: RELATIVE HUMIDITY
humidity_daily <- aggregate(humidity[c(2:ncol(humidity))], 
                                list(cut(humidity[["date"]], 
                                         "1 day", 
                                         right = F)), 
                                mean, na.rm = T)

colnames(humidity_daily)[1] <- "date"
humidity_daily[["date"]] <- as.Date(humidity_daily[["date"]])

humidity_daily[c(2:ncol(humidity_daily))] <- 
  replace(humidity_daily[c(2:ncol(humidity_daily))],
          humidity_daily[c(2:ncol(humidity_daily))] == 'NaN', NA)


##### variable: WIND SPEED
windspeed_daily <- aggregate(windspeed[c(2:ncol(windspeed))], 
                             list(cut(windspeed[["date"]], 
                                      "1 day", 
                                      right = F)), 
                             mean, na.rm = T)

colnames(windspeed_daily)[1] <- "date"
windspeed_daily[["date"]] <- as.Date(windspeed_daily[["date"]])

windspeed_daily[c(2:ncol(windspeed_daily))] <- 
  replace(windspeed_daily[c(2:ncol(windspeed_daily))],
          windspeed_daily[c(2:ncol(windspeed_daily))] == 'NaN', NA)


#### For wind direction: from mean of wind components --

# u.wind and v.wind to each measure
check <- windspeed$date == winddirection$date 

uwind <- u.wind(windspeed[,-1], winddirection[,-1])
uwind$date <- winddirection$date
vwind <- v.wind(windspeed[,-1], winddirection[,-1])
vwind$date <- winddirection$date

# u.wind components daily-average
uwind_daily <- aggregate(uwind[c(1:(ncol(uwind)-1))],
                          list(cut(uwind[["date"]],
                                   "1 day",
                                   right = F)),
                          mean, na.rm = T)
colnames(uwind_daily)[1] <- "date"
uwind_daily[["date"]] <- as.Date(uwind_daily[["date"]])

uwind_daily[c(2:ncol(uwind_daily))] <- 
  replace(uwind_daily[c(2:ncol(uwind_daily))],
          uwind_daily[c(2:ncol(uwind_daily))] == 'NaN', NA)


# v.wind components daily-average
vwind_daily <- aggregate(vwind[c(1:(ncol(vwind)-1))],
                          list(cut(vwind[["date"]],
                                   "1 day",
                                   right = F)),
                          mean, na.rm = T)
colnames(vwind_daily)[1] <- "date"
vwind_daily[["date"]] <- as.Date(vwind_daily[["date"]])

vwind_daily[c(2:ncol(vwind_daily))] <- 
  replace(vwind_daily[c(2:ncol(vwind_daily))],
          vwind_daily[c(2:ncol(vwind_daily))] == 'NaN', NA)


##### variable: WIND DIRECTION
winddirection_daily <- data.frame(matrix(NA, 
                                           ncol = ncol(uwind_daily), 
                                           nrow = nrow(uwind_daily)))
colnames(winddirection_daily) <- colnames(uwind_daily)
winddirection_daily$date <- uwind_daily$date

for (i in 2:ncol(winddirection_daily)) {
  winddirection_daily[ ,i] <- wind_direction(uwind_daily[ ,i], vwind_daily[ ,i])
}

winddirection_daily[c(2:ncol(winddirection_daily))] <- 
  replace(winddirection_daily[c(2:ncol(winddirection_daily))],
          winddirection_daily[c(2:ncol(winddirection_daily))] == 'NaN', NA)


#### FULL datetime to complete dataset used as INPUT for Climatol
day_interval <- as.data.frame(seq.POSIXt(
  as_datetime(head(temperature_daily[["date"]], n = 1)),
  as_datetime(tail(temperature_daily[["date"]], n = 1)),
  by = "day"))
colnames(day_interval) <- c("date")
day_interval$date <- as.Date(day_interval$date)


#### FULL datasets
precipitation <- merge(day_interval, precipitation_daily, by = "date", all = T)
temperature <- merge(day_interval, temperature_daily, by = "date", all = T)
humidity <- merge(day_interval, humidity_daily, by = "date", all = T)
windspeed <- merge(day_interval, windspeed_daily, by = "date", all = T)
winddirection <- merge(day_interval, winddirection_daily, by = "date", all = T)





##############################################
######## INPUT DATASETS FOR CLIMATOL #########
##############################################



# Precipitation --
# missing data filled with data from <https://ciram.epagri.sc.gov.br/agroconnect/>
precipitation$cubatao[precipitation$date =="2019-10-09"] <- 0
precipitation$estradageral[precipitation$date =="2019-10-09"] <- 0 
precipitation$cubatao[precipitation$date =="2020-12-31"] <- 8.6
precipitation$estradageral[precipitation$date =="2020-12-31"] <- 123

write_csv(precipitation, 
          "./output/01-climatol/precipitation_daily_raw2012_2020.csv")
precipitation2012_2020 <- precipitation[c(2:ncol(precipitation))] %>% 
  as.matrix()
write(precipitation2012_2020, 
      "./output/01-climatol/precipitation_2012-2020.dat")



# Temperature --
write_csv(temperature, 
          "./output/01-climatol/temperature_daily_raw2012_2020.csv")
temperature2012_2020 <- temperature[c(2:ncol(temperature))] %>% 
  as.matrix()
write(temperature2012_2020, 
      "./output/01-climatol/temperature_2012-2020.dat")


# Humidity --
write_csv(humidity, 
          "./output/01-climatol/humidity_daily_raw2012_2020.csv")
humidity2012_2020 <- humidity[c(2:ncol(humidity))] %>% 
  as.matrix()
write(humidity2012_2020, 
      "./output/01-climatol/humidity_2012-2020.dat")


# Wind Direction --
write_csv(winddirection, 
          "./output/01-climatol/winddirection_daily_raw2012_2020.csv")
wd2012_2020 <- winddirection[c(2:ncol(winddirection))] %>% 
  as.matrix()
write(wd2012_2020, 
      "./output/01-climatol/wd_2012-2020.dat")


# Wind Speed --
write_csv(windspeed, 
          "./output/01-climatol/windspeed_daily_raw2012_2020.csv")
ws2012_2020 <- windspeed[c(2:ncol(windspeed))] %>% 
  as.matrix()
write(ws2012_2020, 
      "./output/01-climatol/ws_2012-2020.dat")




###################################
##### Location dataset ----

est.c <- read_xlsx("./data/01-rawdata_meteo/stations.xlsx")
est.p <- read_xlsx("./data/01-rawdata_meteo/stations_pluv.xlsx")

write.table(est.c, 
            "./output/01-climatol/temperature_2012-2020.est", 
            row.names = FALSE, col.names = FALSE)

write.table(est.c, 
            "./output/01-climatol/humidity_2012-2020.est", 
            row.names = FALSE, col.names = FALSE)
write.table(est.c, 
            "./output/01-climatol/wd_2012-2020.est", 
            row.names = FALSE, col.names = FALSE)
write.table(est.c, 
            "./output/01-climatol/ws_2012-2020.est", 
            row.names = FALSE, col.names = FALSE)
write.table(est.p, 
            "./output/01-climatol/precipitation_2012-2020.est", 
            row.names = FALSE, col.names = FALSE)



########################################################
##################### CLIMATOL #########################
##################### RUNNING ##########################
########################################################

homogen("./output/01-climatol/precipitation", 
        2012, 2020, expl = TRUE, std = 2)
dahstat("./output/01-climatol/precipitation", 
        2012, 2020, stat = "series")

homogen("./output/01-climatol/temperature", 
        2012, 2020, expl = TRUE)
dahstat("./output/01-climatol/temperature", 
        2012, 2020, stat = "series")

homogen("./output/01-climatol/humidity", 
        2012, 2020, expl = TRUE)
dahstat("./output/01-climatol/humidity",
        2012, 2020, stat = "series")

homogen("./output/01-climatol/ws",
        2012, 2020, expl = TRUE, std = 2)
dahstat("./output/01-climatol/ws",
        2012, 2020, stat = "series")

homogen("./output/01-climatol/wd",
        2012, 2020, expl = TRUE)
dahstat("./output/01-climatol/wd",
        2012, 2020, stat = "series")


##############################################
############ OUTPUT FROM CLIMATOL ############
##############################################

## Precipitation ----

load("./output/01-climatol/precipitation_2012-2020.rda")
View(dat) # ORIGINAL DATASETS #
View(dah) # OUTPUT DATASETS #
View(x)


precipitation_homogen <- as.data.frame(dah)
precipitation_homogen$date <- x
precipitation_homogen <- precipitation_homogen %>% relocate(date,
                                                            .before = "V1")

colnames(precipitation_homogen) <- c("date", "aventureiro", "centro", 
                                    "costaesilva", "estacaocidadania",
                                    "estradageral", "iririu", "itinga",
                                    "novabrasilia", "paranaguamirim",
                                    "cubatao", "aguasdejoi", "iateclube",
                                    "flotflux", "ceasa", "itaum", "rodovia")


## Temperature  ----

load("./output/01-climatol/temperature_2012-2020.rda")

temperature_homogen <- as.data.frame(dah)
temperature_homogen$date <- x
temperature_homogen <- temperature_homogen %>% relocate(date, 
                                                        .before = "V1")

colnames(temperature_homogen) <-  c("date", "aeroporto", "cubatao", "aguasdejoi",
                                    "iateclube", "flotflux", "ceasa", "itaum",
                                    "rodovia")



## Humidity ----

load("./output/01-climatol/humidity_2012-2020.rda")

humidity_homogen <- as.data.frame(dah)
humidity_homogen$date <- x
humidity_homogen <- humidity_homogen %>% relocate(date, 
                                                .before = "V1")

colnames(humidity_homogen) <-  c("date", "aeroporto", "cubatao", "aguasdejoi",
                                    "iateclube", "flotflux", "ceasa", "itaum",
                                "rodovia")



## Wind Direction ----

load("./output/01-climatol/wd_2012-2020.rda")

wd_homogen <- as.data.frame(dah)
wd_homogen$date <- x
wd_homogen <- wd_homogen %>% relocate(date, 
                                      .before = "V1")

colnames(wd_homogen) <-  c("date", "aeroporto", "cubatao", "aguasdejoi",
                                "iateclube", "flotflux", "ceasa", "itaum",
                           "rodovia")


## Wind Speed ----

load("./output/01-climatol/ws_2012-2020.rda")

ws_homogen <- as.data.frame(dah)
ws_homogen$date <- x
ws_homogen <- ws_homogen %>% relocate(date, 
                                      .before = "V1")

colnames(ws_homogen) <-  c("date", "aeroporto", "cubatao", "aguasdejoi",
                           "iateclube", "flotflux", "ceasa", "itaum",
                           "rodovia")









##################################################
####### CLIMATOL OUTPUT: STATION DATASETS ########

# SBJV AIRPORT ---- 

aeroporto_homogen <- unique(Reduce(function(x, y) merge(x, y, all=TRUE, 
                                                       by = c("date")), 
                                  list(temperature_homogen[c("date", "aeroporto")], 
                                       humidity_homogen[c("date", "aeroporto")],
                                       wd_homogen[c("date", "aeroporto")],
                                       ws_homogen[c("date", "aeroporto")])))

colnames(aeroporto_homogen)<- c("date", "temp", "umid", "wd", "ws")


# Cubatao ---- 

cubatao_homogen <- unique(Reduce(function(x, y) merge(x, y, all=TRUE, 
                                                       by = c("date")), 
                                  list(temperature_homogen[c("date", "cubatao")], 
                                       precipitation_homogen[c("date", "cubatao")],
                                       humidity_homogen[c("date", "cubatao")],
                                       wd_homogen[c("date", "cubatao")],
                                       ws_homogen[c("date", "cubatao")])))

colnames(cubatao_homogen)<- c("date", "temp", "prec", "umid", "wd", "ws")



# Aguasdejoi ---- 

aguasdejoi_homogen <- unique(Reduce(function(x, y) merge(x, y, all=TRUE, 
                                                       by = c("date")), 
                                  list(temperature_homogen[c("date", "aguasdejoi")], 
                                       precipitation_homogen[c("date", "aguasdejoi")],
                                       humidity_homogen[c("date", "aguasdejoi")],
                                       wd_homogen[c("date", "aguasdejoi")],
                                       ws_homogen[c("date", "aguasdejoi")])))

colnames(aguasdejoi_homogen)<- c("date", "temp", "prec", "umid", "wd", "ws")



# IateClube ---- 


iateclube_homogen <- unique(Reduce(function(x, y) merge(x, y, all=TRUE, 
                                                       by = c("date")), 
                                  list(temperature_homogen[c("date", "iateclube")], 
                                       precipitation_homogen[c("date", "iateclube")],
                                       humidity_homogen[c("date", "iateclube")],
                                       wd_homogen[c("date", "iateclube")],
                                       ws_homogen[c("date", "iateclube")])))

colnames(iateclube_homogen)<- c("date", "temp", "prec", "umid", "wd", "ws")




# FlotFlux ---- 


flotflux_homogen <- unique(Reduce(function(x, y) merge(x, y, all=TRUE, 
                                                       by = c("date")), 
                                  list(temperature_homogen[c("date", "flotflux")], 
                                       precipitation_homogen[c("date", "flotflux")],
                                       humidity_homogen[c("date", "flotflux")],
                                       wd_homogen[c("date", "flotflux")],
                                       ws_homogen[c("date", "flotflux")])))

colnames(flotflux_homogen)<- c("date", "temp", "prec", "umid", "wd", "ws")







#######################################
############ FINAL OUTPUT #############
##### BY METEOROLOGICAL STATION #######

write_csv(aeroporto_homogen, 
          "./output/01-climatol/dataset_output_climatol/aeroporto_homogen.csv")

write_csv(cubatao_homogen, 
          "./output/01-climatol/dataset_output_climatol/cubatao_homogen.csv")

write_csv(aguasdejoi_homogen, 
          "./output/01-climatol/dataset_output_climatol/aguasdejoi_homogen.csv")

write_csv(iateclube_homogen, 
          "./output/01-climatol/dataset_output_climatol/iateclube_homogen.csv")

write_csv(flotflux_homogen, 
          "./output/01-climatol/dataset_output_climatol/flotflux_homogen.csv")

