#### ..................................................................... ####
####          Load and prepare the data set on Weather                     ####
#### ..................................................................... ####
#### 1. Download the DWD weather data 
#### 2. Prepare weather data  
#### ..................................................................... ####

#### Clear the space ####
rm(list = ls()); gc()

#### Load necessary packages ####
library(rdwd)
library(tidyverse)
library(geosphere)
library(sf)

#### ..................................................................... ####
#### 1. Download the DWD weather data                                      ####
#### ..................................................................... ####

#### Notes ####
# rdwd is an R package to select, download and read climate data from the German Weather Service (Deutscher Wetterdienst, DWD).
# The DWD provides thousands of datasets with weather observations online at opendata.dwd.de.
# Since May 2019, rdwd also supports reading the Radolan (binary) raster data at grids_germany.
# https://bookdown.org/brry/rdwd/

#### Select the link ####
links = selectDWD(name = "",res = "daily",
                  var = "kl",per = "rh", current=TRUE)

#### Drop unnecessary years from historical files before downloading files ####
links_recent = links[grepl("akt.zip", links)]
links_hist = links[grepl("hist.zip", links)]
links_hist <- links_hist[as.numeric(str_sub(links_hist, -17, -14)) >= 2016]

links = c(links_recent, links_hist)

#### Download the weather data for Germany ####
file = dataDWD(links, read = FALSE, dir="01_data/weather_dwd/", force = NA, overwrite = TRUE)
clim = readDWD(file, varnames = T, fread = F)

#### Check the weather data ####
str(clim)

#### Save the weather data ####
saveRDS(clim, file = "03_gen_data/weather_list.rds")

#### Clear the space ####
rm(list = ls());gc()

#### ..................................................................... ####
#### 2. Prepare weather data                                                ####
#### ..................................................................... ####

#### Load list of weather data ####
weather = readRDS(file = "03_gen_data/weather_list.rds")

#### List to data frame  ####
weather = bind_rows(weather)

#### Explore the weather variables ####
str(weather)
sapply(weather, function(x) length(which(is.na(x))))

#### Rename variables  ####
weather = select(weather,
                 id = STATIONS_ID,
                 date = MESS_DATUM,
                 wind_speed_max = FX.Windspitze,
                 wind_speed = FM.Windgeschwindigkeit,
                 precipitation = RSK.Niederschlagshoehe,
                 sunshine = SDK.Sonnenscheindauer,
                 snow = SHK_TAG.Schneehoehe,
                 vapor_pressure = VPM.Dampfdruck,
                 atm_pressure = PM.Luftdruck,
                 temperature = TMK.Lufttemperatur,
                 humidity = UPM.Relative_Feuchte,
                 temperature_max = TXK.Lufttemperatur_Max, 
                 temperature_min = TNK.Lufttemperatur_Min)

#### Remove observations prior to 2016 ####
weather = filter(weather, date >= "2016-01-01", date < "2023-01-01")

#### POSIXct to date format  ####
weather = mutate(weather, date = as.Date(date))

#### Filter out rows with NAs for all weather vars  ####
weather = weather[rowSums(is.na(weather)) < 11, ]

#### Save data  ####
saveRDS(weather, file = "03_gen_data/weather.rds")

#### Save data on weather stations locations ####
data(geoIndex)
geoIndex = filter(geoIndex, id %in% unique(weather$id))

length(which(duplicated(geoIndex$id))) 
# 57 duplicates (8%) but diff. in coordinates are marginal ->
# select either one of them
geoIndex = geoIndex[!duplicated(geoIndex$id), ]

saveRDS(geoIndex, file = "03_gen_data/weather_stations.rds")

#### Clear the space ####
rm(list = ls()); gc()
