#### ..................................................................... ####
####          Merge weather data with traffic data                         ####
#### ..................................................................... ####
#### 1. Distance between weather monitors and traffic monitors 
#### 2. IDW between weather and traffic monitors
#### 3. Merge weighted averages of weather covariates with traffic data
#### ..................................................................... ####

#### Clear the space ####
rm(list = ls()); gc()

#### Load necessary packages ####
library(rdwd)
library(tidyverse)
library(geosphere)
library(sf)

#### _____________________________________________________________________ ####
#### 1. Calculate the distance between weather and traffic monitors        ####
#### _____________________________________________________________________ ####

#### DWD station data (name, lat, lon, elevation)  ####
wst = read_rds("03_gen_data/weather_stations.rds")
pst = readRDS("03_gen_data/traffic/be_mq.rds")

#### select only the relevant variables ####
pst = select(pst, id_name, lon, lat)

#### Compute the distance between all measuring stations ####
distance = lapply(split(pst, f = pst$id_name), function(x)
  x = data.frame(distance = distHaversine(select(x, lon, lat), 
                                          select(wst, lon, lat)),
                 pst = x$id_name, wst = wst$id))

#### Bind the distances among all stations together ####
distance = bind_rows(distance)

#### Restrict the distance to stations within 100 km from each other ####
distance = filter(distance, distance < 100000)

#### Glimpse the data ####
glimpse(distance)

#### save the distance to the stations ####
saveRDS(distance, file = "03_gen_data/traffic/distance_weather_traffic.rds")

#### Clear the space ####
rm(list = ls()); gc()

#### _____________________________________________________________________ ####
#### 2. IDW between weather monitors and traffic monitors                  ####
#### _____________________________________________________________________ ####

#### Clear the space ####
rm(list = ls()); gc()

#### Load the data ####
weather = read_rds("03_gen_data/weather.rds")
distance = read_rds("03_gen_data/traffic/distance_weather_traffic.rds")

#### Create a long format of the rain data ####
idw = gather(weather, var, value, -c(id, date)) %>%
  filter(is.na(value) == F); idw = split(idw, f = idw$var)

#### Calculate the IDW for each weather variable ####
idw = lapply(idw, function(x) left_join(x, distance, by = c("id" = "wst")) |> 
               filter(is.na(date) == F) %>% group_by(pst, date) %>% 
               summarise(value = sum((1/distance^2) * value)/sum(1/distance^2))) %>% 
  bind_rows(., .id = "var") %>% spread(., var, value)

####  save the IDW of each pollutant and weather variable ####
saveRDS(idw, file = "03_gen_data/traffic/weather_idw.rds")

#### Clear the space ####
rm(list = ls()); gc()

#### _____________________________________________________________________ ####
#### 3. Merge weighted averages of weather covariates with traffic data    ####
#### _____________________________________________________________________ ####

#### Clear the space ####
rm(list = ls()); gc()

#### Load the data ####
dat = readRDS("03_gen_data/traffic/be_mq_hourly_bans.rds")
idw = readRDS("03_gen_data/traffic/weather_idw.rds") %>%
  rename(id_name = pst)

####  Merge with pollution and weather data ####
dat = left_join(dat, idw)

####  Check for duplicated values ####
length(which(duplicated(dat[, c("id_name", "date", "hour")])))
sapply(dat, function(x) length(which(is.na(x))))

#### Glimpse the data ####
glimpse(dat)

####  save the data set ####
saveRDS(dat, file = "03_gen_data/traffic/be_mq_hourly_weather.rds")

#### Clear the space ####
rm(list = ls()); gc()
