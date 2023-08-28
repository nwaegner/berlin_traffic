#### ..................................................................... ####
####      Descriptives on traffic data, weather, and bans                  ####
#### ..................................................................... ####
#### 1. Maps of bans and traffic counters  

#### 2. Descriptives on traffic & weather measurements
####  1.1. Treatment group
####  1.2. Control group: Cities with diesel ban
####  1.3. Control group: Monitors with limit value exceedances
#### ..................................................................... ####

rm(list=ls()); gc()

library(tidyverse)
library(ggmap)
library(viridis)
library(xtable)
library(sf)


#### ...................................................................... ####
#### 1. Maps of bans and traffic counters                                   ####
#### ...................................................................... ####

#### Load daily data ####
daily <- readRDS("03_gen_data/traffic/be_mq_daily_treated.rds")

#### Load shapefiles of bans  ####
bans <- readRDS("03_gen_data/diesel_bans.rds") %>% st_zm()
bans$city <- gsub( " .*$", "", bans$name)


#### Loop over cities ####
lapply(c("Berlin"), function(c){
  
  monitor <- unique(daily[, c("id", "id_name", "lon", "lat", "ban_dist")])
  
  # Diesel bans within city of interest
  bans_city = bans[bans$city == c, ]
  
  # City map
  city = read_sf("01_data/shapefiles_de/VG250_GEM.shp") %>%
    st_transform(., crs = 4326) %>%
    filter(GF == 4, GEN == c)

  map = get_stamenmap(bbox=as.numeric(st_bbox(city)),
                      zoom = 13,
                      maptype='terrain-lines')
  
  ggmap(map) +
    geom_sf(data = bans_city, inherit.aes = F, color = "red", alpha = 0.5, linewidth = 2) +
    geom_point(data = monitor, inherit.aes = F, aes(x = lon, y = lat), alpha = 0.75, size = .5) +
    # scale_color_manual(values = c("background" = "green", "traffic" = "blue")) +
    labs(x = "", y = "") +
    theme(plot.background = element_rect(fill = "white", color = "white"),
          panel.background = element_rect(fill = "white"),
          legend.position = "bottom",
          legend.background = element_rect(fill = "white", color = "white"),
          legend.key = element_blank(),
          legend.text = element_text(size = 8),
          legend.box.spacing = unit(-5, "pt"),
          legend.margin = margin(0,0,0,0),
          # legend.spacing.x = unit(0.1, 'cm'),
          axis.line = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank())
  ggsave(paste0("04_figures/map_", c, ".png"), width = 6, height = 6)
  
}
)

#### ......................... ####
#### Exemplary map: Leipziger  ####
#### ......................... ####

#### Select exemplary bans in Berlin ####
leipzig <- c(xmin = 13.35, ymin = 52.495, xmax = 13.425, ymax = 52.53)
map = get_stamenmap(bbox=as.numeric(leipzig), 
                    zoom = 13,  
                    maptype='terrain-lines')

bans_leipzig = filter(bans, grepl("Leipziger Straße|Reinhardtstraße|Friedrichstraße|Brückenstr", name))
bans_leipzig_b = st_buffer(bans_leipzig, 1500)

stations <- readRDS(file = "03_gen_data/traffic/be_mq.rds")
stations_leipzig = filter(stations, 
                          lat > leipzig[["ymin"]] & lat < leipzig[["xmax"]] &
                            lon > leipzig[["xmin"]] & lon < leipzig[["xmax"]])

p1 <- ggmap(map) + 
  # geom_sf(data = bans_leipzig, inherit.aes = F, color = "red", alpha = 0.5, linewidth = 2) +
  geom_point(data = stations, inherit.aes = F, aes(x = lon, y = lat), alpha = 0.75, size = 2.5) +
  # geom_sf(data = bans_leipzig_b, inherit.aes = F, fill = "blue", alpha = 0.4) +
  labs(x = "", y = "") +
  theme(plot.background = element_rect(fill = "white", color = "white"),
        panel.background = element_rect(fill = "white"),
        legend.background = element_rect(fill = "white", color = "white"), 
        legend.key = element_blank(), 
        # legend.text = element_text(size = 8),
        legend.box.spacing = unit(-5, "pt"), 
        legend.margin = margin(0,0,0,0),
        axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())
p1

p2 <- p1 + geom_sf(data = bans_leipzig, inherit.aes = F, color = "red1", alpha = 0.5, linewidth = 2)
p2
ggsave("04_figures/map_leipziger_etc.png", width = 6, height = 5)

p2 + geom_sf(data = bans_leipzig_b, inherit.aes = F, fill = "blue", color = NA, alpha = 0.25) 


#### .................................................................. ####
#### 2. Descriptives on traffic & weather                               ####
#### .................................................................. ####

dat <- readRDS(file = "03_gen_data/traffic/be_mq_daily_treated.rds")
monthly <- readRDS(file = "03_gen_data/traffic/be_mq_monthly.rds")

#### ................................. ####
#### Table with descriptives per group ####
#### ................................. ####

## Create variables of interest ####
vars = c("q_kfz", "q_pkw", "q_lkw",
         "v_kfz", "v_pkw", "v_lkw",
         "ban_dist",
         "precipitation", "temperature", "sunshine", "wind_speed", "humidity") 
vars_names = c("Intensity KFZ [thsd. vehicles per day]", "Intensity PKW [thsd. vehicles per day]", "Intensity LKW  [thsd. vehicles per day]",
               "Speed KFZ [km/h]", "Speed PKW  [km/h]", "Speed LKW  [km/h]",
               "Dist. to ban [m]",
               "Rain [mm]",
               "Temperature [C]",
               "Sunshine [h]",
               "Wind speed [m/s]",
               "Humidity [%]")

## Samples of interest ####
list_groups = list(filter(monthly, treatgrp25 == 1),
                   filter(monthly, treatgrp25 == 1 & score < 0),
                   filter(monthly, treatgrp25 == 1 & score >= 0),
                   filter(monthly, treatgrp25 == 0 & city_with_ban == 1),
                   filter(monthly, treatgrp25 == 0 & city_with_ban == 1 & drop1500 == 0))
names(list_groups) = c("Total", "Pre", "Post", "Total1", "Total2")
## Number of locations ####
nindiv = lapply(list_groups, function(x) x = ungroup(x) %>% summarise(length(unique(id))))
nobs = lapply(list_groups, function(x) x = ungroup(x) %>% summarise(n()))

## Mean and SD ####
table = lapply(list_groups, function(x){
  
  avg = ungroup(x) %>%
    mutate_at(vars(starts_with("q_")), ~./10^3) %>%
    summarise_at(vars(all_of(vars)), list(~mean(., na.rm = T), ~sd(., na.rm = T))) %>%
    mutate_at(vars(ends_with("mean")), list(~sprintf("%.3f", .))) %>%
    mutate_at(vars(ends_with("sd")), list(~sprintf("(%.3f)", .))) %>%
    pivot_longer(cols = everything(),names_to = c("variable", "stat"), names_pattern = "(.*)_(mean|sd)") %>%
    pivot_wider(id_cols = "stat", names_from = "variable", values_from = "value")
  
  return(avg)
  
})
# Format results
table = data.frame(bind_rows(table, .id = "sample"))
table = table %>% gather(variable, value, -sample, -stat) %>% 
  mutate(sample = fct_relevel(factor(sample), "Total", "Pre", "Post", "Total1", "Total2")) %>%
  mutate(variable = fct_relevel(factor(variable), vars)) %>%
  spread(sample, value)
table = arrange(table, variable, stat, .by_group = TRUE)
# Rename rows in table
table$variable = rep(vars_names, each = 2)
table$variable[seq(2,length(table$variable),2)] = ""
table = select(table, -stat)
# Create tex tables
latex = xtable(table, label = NULL, booktabs = TRUE, digits = 1,
               caption = "Descriptives on traffic and weather for treated and control samples (monthly avg.)")
print(latex)
print(latex, 
      include.rownames = F, booktabs = TRUE,
      sanitize.text.function = function(x) sanitize(x, type = "latex"),
      table.placement = "!htb",
      size = "\\footnotesize",
      file = "05_tables/descriptives.tex")

# Edit tex tables 
s = readLines("05_tables/descriptives.tex")
s

mc = paste0("& \\multicolumn{3}{c}{Inside ban} & Outside ban & Outside $>$ 1500m \\\\ \\cmidrule(r){2-4} \\cmidrule(r){5-5} \\cmidrule(r){6-6}")

s = c(s[1:7], mc, s[8:length(s)])
s

nindiv2 = paste0(" N.Units & ", paste(unlist(nindiv), collapse = "&"), "\\\\")
s = c(s[1:(length(vars)*2+10)], nindiv2, s[(length(vars)*2+10+1):length(s)])

nobs2 = paste0("\\midrule N.Observations & ", paste(unlist(nobs), collapse = "&"), "\\\\")
s = c(s[1:(length(vars)*2+10)], nobs2, s[(length(vars)*2+10+1):length(s)])

s

write_lines(s, file = "05_tables/descriptives.tex")


#### Monthly pollution averages in t25 group and unrestricted control group (levels) ####
list("Inside ban" = filter(monthly, treatgrp25 == 1),
     "Outside ban" = filter(monthly, treatgrp75 != 1)) %>%
  lapply(., function(x)
    x %>% group_by(year_month) %>% 
      summarize_at(vars(q_kfz, q_pkw, q_lkw), mean, na.rm = T)
  ) %>%
  bind_rows(., .id = "sample") %>%
  pivot_longer(cols = starts_with("q_"), names_to = "pol", values_to = "val") %>%
  ungroup() %>%
  mutate(sample = fct_relevel(factor(sample), "Treated (25m)")) %>%
  ggplot() +
  geom_line(aes(x = year_month, y = val, color = sample)) +
  scale_color_viridis(discrete = T, direction = -1, end = .5) +
  geom_vline(xintercept = zoo::as.yearmon("2019-11"), linetype = "dashed", color = "grey70") +
  facet_wrap(. ~ pol, scales = "free_y") + labs(x = "", y = "") +  
  theme_bw() + theme(legend.position = "bottom")
ggsave(paste0("04_figures/monthly_unrestricted.png"), width = 10, height = 5)

#### .............................................. ####
####  1.1. Treatment group                          ####
#### .............................................. ####

#### Number of stations and daily observations by treatment group and pollutant ####
list_treat = list(t15 = filter(dat, treatgrp15 == 1),
                  t25 = filter(dat, treatgrp25 == 1),
                  t75 =  filter(dat, treatgrp75 == 1),
                  t150 = filter(dat, treatgrp150 == 1)) 
tab = lapply(list(q_kfz = "q_kfz", q_pkw = "q_pkw", q_lkw = "q_lkw"), function(p){
  lapply(list_treat, function(x){
    x %>% 
      filter(!is.na(!!sym(p))) %>%
      select(id, post, pol = !!sym(p)) %>%
      group_by(post) %>%
      summarize(N = length(unique(id)),
                nobs = n()) 
  }) %>%  
    bind_rows(., .id = "sample") 
}) %>% 
  bind_rows(., .id = "pol") %>%
  mutate(post = ifelse(post == 0, "pre", "post"),
         sample = paste0(sample, "_", post)) %>% select(-post) %>%
  pivot_longer(cols = c("N", "nobs"), names_to = "stat", values_to = "val") %>%
  mutate(pol = paste0(pol, "_", stat)) %>% select(-stat) %>%
  pivot_wider(id_cols = c("pol"), names_from = "sample", values_from = "val")
# pivot_wider(id_cols = c("sample"), names_from = "pol", values_from = "val")
tab

latex = xtable(tab, digits = 0, 
               caption = "Number of treated monitors and complete daily averages", label = NULL, 
               booktabs = TRUE)
print(latex, 
      include.rownames = F, booktabs = TRUE,
      sanitize.text.function = function(x) sanitize(x, type = "latex"),
      table.placement = "!htb",
      size = "\\footnotesize",
      file = "05_tables/treated_N_nobs_daily.tex")

#### Number of stations and monthly observations by treatment group and pollutant ####
list_treat = list(t15 = filter(monthly, treatgrp15 == 1),
                  t25 = filter(monthly, treatgrp25 == 1),
                  t75 =  filter(monthly, treatgrp75 == 1),
                  t150 = filter(monthly, treatgrp150 == 1)) 
tab = lapply(list(q_kfz = "q_kfz", q_pkw = "q_pkw", q_lkw = "q_lkw"), function(p){
  lapply(list_treat, function(x){
    x %>% 
      filter(!is.na(!!sym(p))) %>%
      select(id, post, pol = !!sym(p)) %>%
      group_by(post) %>%
      summarize(N = length(unique(id)),
                nobs = n()) 
  }) %>%  
    bind_rows(., .id = "sample") 
}) %>% 
  bind_rows(., .id = "pol") %>%
  mutate(post = ifelse(post == 0, "pre", "post"),
         sample = paste0(sample, "_", post)) %>% select(-post) %>%
  pivot_longer(cols = c("N", "nobs"), names_to = "stat", values_to = "val") %>%
  mutate(pol = paste0(pol, "_", stat)) %>% select(-stat) %>%
  pivot_wider(id_cols = c("pol"), names_from = "sample", values_from = "val")
# pivot_wider(id_cols = c("sample"), names_from = "pol", values_from = "val")
tab

latex = xtable(tab, digits = 0, 
               caption = "Number of treated monitors and monthly averages", label = NULL, 
               booktabs = TRUE)
print(latex, 
      include.rownames = F, booktabs = TRUE,
      sanitize.text.function = function(x) sanitize(x, type = "latex"),
      table.placement = "!htb",
      size = "\\footnotesize",
      file = "05_tables/treated_N_nobs_monthly.tex")

#### Centered monthly pollution averages across different treatment radii ####
sample <- filter(monthly, city_with_ban == 1)

list(t15 = filter(sample, treatgrp15 == 1),
     t25 = filter(sample, treatgrp25 == 1),
     t75 = filter(sample, treatgrp75 == 1),
     t150 = filter(sample, treatgrp150 == 1),
     controls1000 = filter(sample, drop1000 == 0)) %>%
  lapply(., function(x)
    x %>% group_by(score) %>% 
      summarize_at(vars(q_kfz, q_pkw, q_lkw), mean, na.rm = T)
  ) %>%
  bind_rows(., .id = "sample") %>%
  pivot_longer(cols = c(starts_with("q_")), names_to = "pol", values_to = "val") %>%
  ungroup() %>%
  mutate(sample = fct_relevel(factor(sample), "t15", "t25", "t75", "t150")) %>%
  ggplot() +
  geom_line(aes(x = score, y = val, color = sample), alpha = .8, position=position_jitter(w=0.05, h=0)) +
  scale_color_viridis(discrete = T, direction = -1) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  facet_wrap(pol ~ ., scales = "free_y", nrow = 1) + labs(x = "", y = "") +
  theme_bw() + theme(legend.position = "bottom")
ggsave(paste0("04_figures/monthly_cities_centered.png"), width = 10, height = 5)
