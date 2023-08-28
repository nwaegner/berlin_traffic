#########################################################################
####     Prepare data on diesel bans & OSM data                      ####
#########################################################################

rm(list=ls()); gc()

library(tidyverse)
library(sf)
library(xtable)
library(osmextract) 

#### .................................................................. ####
#### 1. Prepare data on diesel bans                                     ####
#### .................................................................. ####

#### Load data on diesel bans (Excel with implementation dates and shapefiles) ####
bans = readxl::read_xlsx("01_data/bans/diesel_bans.xlsx", skip = 3) 
head(bans)

berlin = st_read("01_data/bans/berlin.kml") 

stuttgart = st_read("01_data/bans/stuttgart.kml")
stuttgart2 = st_read("01_data/bans/stuttgart_UWZ.kml") %>% 
  mutate(Name = "Stuttgart (Gebiet der Umweltzone Stuttgart)")
stuttgart3 = st_read("01_data/bans/stuttgart_kleineUWZ.kml") %>% 
  mutate(Name = "Stuttgart (kleine UWZ)")

hamburg = st_read("01_data/bans/hamburg.kml")
darmstadt = st_read("01_data/bans/darmstadt.kml")

bans_shp = rbind(berlin, stuttgart, stuttgart2, stuttgart3, hamburg, darmstadt) %>% 
  select(-Description)

#### Combine shapefiles with implementation dates ###
bans = left_join(bans, bans_shp, by = c("Ort" = "Name")) %>% st_as_sf() %>%
  st_transform(., crs = 4326)
head(bans)
str(bans)

#### Rename variables ####
names(bans)
names(bans) = c("bl_name", "name", "type", "stringency", "start_date", "end_date", "geometry")

#### Bans to latex table ####
tab <- select(bans, -type) %>% 
  mutate(start_date = as.character(start_date),
         end_date = as.character(end_date),
         length = st_length(geometry)) %>%
  st_drop_geometry() %>%
  as.data.frame()
latex = xtable(tab, 
               caption = "List of diesel bans", label = NULL, 
               booktabs = TRUE, align = "p{.75in}lp{1.55in}p{1in}lll")
print(latex, 
      include.rownames = F, booktabs = TRUE,
      sanitize.text.function = function(x) sanitize(x, type = "latex"),
      size = "\\scriptsize",
      file = "05_tables/list_bans.tex")

#### Remove z dimension & use Date format ####
bans = st_zm(bans) %>%
  mutate(start_date = as.Date(start_date),
         end_date = as.Date(end_date))

#### Add federal state number (AGS) and name abbreviation ####
ags_bl = readxl::read_excel("01_data/shapefiles_de/ags_bl.xlsx")
bans = left_join(bans, ags_bl) %>% select(bl_nr, bl, bl_name, everything())

#### Save sf data frame ####
saveRDS(bans, "03_gen_data/diesel_bans.rds")
rm(list=ls()); gc()

#### .................................................................. ####
#### 2. Download and prepare OSM data on roads                          ####
#### .................................................................. ####

#### Loop over federal states (to ease computational burden) ####
ags_bl = readxl::read_excel("01_data/shapefiles_de/ags_bl.xlsx")

lapply(ags_bl$bl, function(b){
  
  ## Get road network for specific state: type of road, speed limit, nr. of lanes  ####
  # oe_get_keys(osm_lines_all, layer = "lines")
  osm_lines = oe_get(ags_bl$bl_name[ags_bl$bl == b], 
                     layer = "lines",
                     extra_tags = c("maxspeed", "lanes", "ref", "postal_code"),
                     download_directory = "01_data/osm/",
                     # boundary = st_as_sfc(st_bbox(df)), boundary_type = "spat",
                     stringsAsFactors = FALSE)
  # plot(osm_lines["highway"], key.pos = 1)
  
  ## Keep only roads for cars (discard paths, cycle lanes, etc.) & calculate length of road segment ####
  ht = c("motorway", "trunk", "primary", "secondary", "tertiary",
         "motorway_junction",
         "motorway_link", "trunk_link", "primary_link", "secondary_link", "tertiary_link",
         "residential", "living_street", "unclassified") # "road", "service"
  roads = osm_lines %>%
    filter(highway %in% ht) %>%
    mutate(road_length = as.numeric(st_length(geometry))) %>%
    select(road_id = osm_id, road_type = highway, road_name = name, 
           road_maxspeed = maxspeed, road_lanes = lanes, road_ref = ref, 
           road_plz = postal_code,
           road_length, geometry)
  
  ## Save data frame ####
  saveRDS(roads, paste0("03_gen_data/osm/roads_", b, ".rds"))
  rm("roads", "osm_lines"); gc()
})
rm(list=ls()); gc()

#### .................................................................. ####
#### 3. Identify road segments affected by diesel bans                  ####
#### .................................................................. ####

#### Load ban data ####
bans = readRDS("03_gen_data/diesel_bans.rds") %>% 
  filter(type != "Zonale Durchfahrtsbeschränkung")

#### Determine length of diesel ban ####
bans$ban_length <- as.numeric(st_length(bans))

#### Loop over federal states with diesel bans ####
list_roads <- lapply(unique(bans$bl), function(b){
  
  ## Filter bans within specific federal state ####
  df <- filter(bans, bl == b)
  df <- st_buffer(df, 25) # draw buffer around linestrings to cover the whole street
  
  ## Get highway data for area with diesel ban ####
  roads <- readRDS(paste0("03_gen_data/osm/roads_", b, ".rds")) 
  # plot(roads["road_type"], key.pos = 1)
  
  #### Determine intersecting roads ####
  roads_int <- roads[unlist(st_intersects(df, roads)), ]
  # ggplot() +
  #   geom_sf(data = df) +
  #   geom_sf(data = roads_int, color = "red")
  df_int <- st_intersection(df, roads_int)
  # plot(df_int["name"])
  
  return(df_int)
})

roads = bind_rows(list_roads)
head(roads)

#### Check plots ####
plot(bans[bans$bl_name == "Baden-Württemberg", "name"], key.pos = 1)
plot(roads[roads$bl_name == "Baden-Württemberg", "name"], key.pos = 1)

plot(bans[bans$bl_name == "Berlin", "name"], key.pos = 1)
plot(roads[roads$bl_name == "Berlin", "name"], key.pos = 1)

plot(bans[bans$bl_name == "Hamburg", "name"], key.pos = 1)
plot(roads[roads$bl_name == "Hamburg", "name"], key.pos = 1)

plot(bans[bans$bl_name == "Hessen", "name"], key.pos = 1)
plot(roads[roads$bl_name == "Hessen", "name"], key.pos = 1)

#### Save data ####
saveRDS(roads, "03_gen_data/roads_diesel_bans.rds")

# # Example: Stromstraße
# ggplot() +
#   geom_sf(data = st_crop(df, xmin = 13.336457, ymin = 52.525275, xmax = 13.347960, ymax = 52.532191), color = "red") +
#   geom_sf(data = filter(roads, road_name == "Stromstraße"), alpha = .25) +
#   # geom_sf(data = st_crop(roads, xmin = 13.336457, ymin = 52.525275, xmax = 13.347960, ymax = 52.532191), alpha = .25) +
#   geom_sf(data = st_crop(roads_int, xmin = 13.336457, ymin = 52.525275, xmax = 13.347960, ymax = 52.532191), color = "blue", alpha = .5)
# 
# 
# ggplot() +
#   geom_sf(data = st_crop(df, xmin = 13.336457, ymin = 52.525275, xmax = 13.347960, ymax = 52.532191), color = "red") +
#   geom_sf(data = filter(roads, road_name == "Stromstraße"), alpha = .25) +
#   geom_sf(data = st_crop(df_int, xmin = 13.336457, ymin = 52.525275, xmax = 13.347960, ymax = 52.532191), color = "blue", alpha = .5)
# 
# # Example: Leipziger Str.
# ggplot() +
#   geom_sf(data = st_crop(df, xmin = 13.379396, ymin = 52.508709, xmax = 13.389633, ymax = 52.511052 ), color = "red") +
#   geom_sf(data = filter(roads, road_name == "Leipziger Straße"), alpha = .25) +
#   geom_sf(data = st_crop(df_int, xmin = 13.379396, ymin = 52.508709, xmax = 13.389633, ymax = 52.511052 ), color = "blue", alpha = .5)

