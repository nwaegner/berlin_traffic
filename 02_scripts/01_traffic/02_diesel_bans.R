#########################################################################
####        Merge diesel bans with traffic data                      ####
#########################################################################

rm(list=ls()); gc()

library(tidyverse)
library(sf)
library(scales)

#### Load traffic monitor and diesel bans data ####
stations = readRDS("03_gen_data/traffic/be_mq.rds")
bans = readRDS("03_gen_data/diesel_bans.rds") 

#### Exclude UWZ in Stuttgart from ban data ####
lez = filter(bans, type == "Zonale Durchfahrtsbeschränkung")
bans = filter(bans, type != "Zonale Durchfahrtsbeschränkung")

#### Monitor locations as simple geometry feature ####
coords = st_as_sf(select(stations, id, lon, lat), coords = c("lon", "lat"), crs = 4326)
stations = left_join(coords, stations)

#### Calculate shortest distance between each station and diesel bans ####
near = st_nearest_feature(stations, bans)
dist = st_distance(stations, bans[near,], by_element=TRUE)
head(dist)

dist_df = data.frame(id = stations$id, 
                     ban_closest = bans$name[near],
                     ban_dist = as.vector(dist),
                     ban_startdate = bans$start_date[near],
                     ban_enddate = bans$end_date[near],
                     ban_stringency =  bans$stringency[near])

#### Combine diesel bans with hourly traffic data ####
dat = readRDS("03_gen_data/traffic/be_mq_hourly.rds")

nrow(dat)
dat = left_join(dat, dist_df)
nrow(dat)

#### Check data frame ####
length(which(duplicated(dat[, c("id", "date", "hour")])))
sapply(dat, function(x) length(which(is.na(x))))

#### Save data ####
saveRDS(dat, "03_gen_data/traffic/be_mq_hourly_bans.rds")
rm(list=ls()); gc()
