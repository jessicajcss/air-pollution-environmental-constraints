#############################################


# SCRIPT to analyse backward trajectories - 96-h back trajectories for each sampling site were calculated using the HYSPLIT model integrated to **openair R package** to get an insight into the origin and transport pathway of the air masses arriving near receptor level (10 m asl) in these sites.

## References ---- 
#
# Production of HYSPLIT trajectory files:
#https://bookdown.org/david_carslaw/openair/sec-prod-hyspl-traj.html

# Trajectory analysis
#https://bookdown.org/david_carslaw/openair/sec-trajPlot.html



###################
# Last update: 2022-04-05
# Author: Santos-Silva, J. C. <jessica.jcss@gmail.com>

##############################################
############### LOAD PACKAGES ################

library(lubridate)  
library(tidyverse)
library(bazar)
library(openair)
library(lattice)
library(devtools)





############## INPUT: RAW DATA ###############

# FULL DATASET: 5-min meteorological data, without any filling
data_sampling_meteo <- read_csv(
"./output/01-input_wind-circulation/data_sampling_meteo_only2stations.csv",
col_types = cols(
  brometo = col_double(),
  amonio = col_double(),
  Hg = col_number()))
 
dataset <- read_csv( 
          "./output/02-horizontal_flow_characterization/concentracao_meteo_windflow.csv")


################# INPUT: RAW DATA ##################
############### HYSPLIT TRAJECTORIES ###############


# HYSPLIT trajectory files:
traj_BV <- readRDS("./data/02-trajectory-analysis/myTrajData_MIC_96h_10m_3h.rds") 
head(traj_BV)

traj_BR <- readRDS("./data/02-trajectory-analysis/myTrajData_NID_96h_10m_3h.rds")
head(traj_BR)

# unique dataset
traj <- rbind(traj_BV, traj_BR)

# renaming sampling sites
traj <- traj %>% 
  mutate(site = recode((site), 
                       'Boa Vista, Joinville' = 'MIC'),
         site = recode((site), 
                       'Bom Retiro, Joinville' = 'NID'))

# selecting only the same sampling period
traj <- traj %>%
  subset(date >= '2018-09-01 15:46:00' & date <= '2020-02-29 09:28:00')


# adding season column
traj <- traj %>% 
  cutData(hemisphere = "southern", 
          type = "season")

traj$season <- 
  factor(traj$season,
         levels = c("summer (DJF)", 
                    "autumn (MAM)", 
                    "winter (JJA)",
                    "spring (SON)"))



#################################################################
####################### CLUSTER ANALYSIS ########################

clust_MIC <- traj %>%
  filter(site == "MIC") %>%
  trajCluster(method = "Angle", 
              n.cluster = 6, 
              cols = "Dark2",
              map.cols = openColours("Paired", 10),
              #type = "season",
              fontsize = 14,
              #layout = c(4, 1)
              xlab = 'longitude',
              ylab = 'latitude')

clust_NID <- traj %>%
  filter(site == "NID") %>%
  trajCluster(method = "Angle", 
              n.cluster = 6, 
              cols = "Dark2",
              map.cols = openColours("Paired", 10),
              #type = "season",
              fontsize = 14,
              #layout = c(4, 1)
              xlab = 'longitude',
              ylab = 'latitude')

# Cluster: final dataset
clust_data <- rbind(clust_MIC$data, clust_NID$data)





#################################################################
#################### TRAJECTORY ANALYSIS ########################

### now merge with trajectory data by 'date' (neste caso, como já foi iterado antes por data, merge é por site e i)


dataset$i <- seq.int(nrow(dataset)) # to identify each trajectory by sample
names(dataset)[names(dataset) == 'date'] <- 'date.start'


datalist <- list()

for (i in 1:nrow(dataset)) {
  if ((dataset$site == "MIC")[i]) {
    start <- dataset$date.start[i]
    end <- dataset$date.end[i]
    dat <- subset(clust_data, date <= end & 
                    date >= start & site == 'MIC')
    dat$i <- i  # maybe you want to keep track of which iteration produced it?
    datalist[[i]] <- dat # add it to your list
  } else {
    start <- dataset$date.start[i]
    end <- dataset$date.end[i]
    dat <- subset(clust_data, date <= end & 
                    date >= start & site == 'NID')
    dat$i <- i  # maybe you want to keep track of which iteration produced it?
    datalist[[i]] <- dat # add it to your list  
  }
}


traj_samplingdates <- do.call(rbind, datalist)



# Final dataset: containing sampling, local meteorological data, trajectory and cluster analysis results
traj_pm <- left_join(dataset, traj_samplingdates, by = c("site", "i")) %>% unique()



######################################
############### OUTPUT ###############

write_csv(traj_pm, 
          "./output/02-trajectory-analysis/traj_sampling_meteo.csv")





