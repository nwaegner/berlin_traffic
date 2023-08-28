#########################################################################
#### Preparation of Berlin traffic data, 2015 - 2022                 ####
#########################################################################
#### Notes:
#### Traffic monitor locations (MQ - Messquerschnitt) with individual
#### detectors (DET - detektors) for each lane - > focus on monitor locations

#### Clear the space ####
rm(list = ls()); gc()
options(scipen = 999)

#### Load packages ####
library(readr)
library(tidyverse)
library(data.table)
library(sf)
library(ggmap)
theme_set(theme_bw())

#### ..................................................................... ####
#### 1. Information on traffic monitors/detectors                           ####
#### ..................................................................... ####

#### Load traffic monitor data ####
monitor <- readxl::read_excel("01_data/traffic/berlin/Stammdaten_Verkehrsdetektion_2022_07_20.xlsx")

#### Check data ####
str(monitor)
sapply(monitor, function(x) length(which(is.na(x))))

length(which(duplicated(monitor$DET_NAME_NEU)))
monitor$DET_NAME_NEU[which(duplicated(monitor$DET_NAME_NEU))]

#### Determine number of (active) detectors per MQ location ####
monitor <- monitor %>% 
  group_by(MQ_ID15) %>%
  mutate(det_total = length(unique(DET_ID15)),
         det_active = length(unique(DET_ID15[is.na(DEINSTALLIERT)]))) %>%
  ungroup()
table(monitor$det_total, monitor$det_active, useNA = "ifany")

#### Select unique MQ locations ####
mq = select(monitor, 
            id = MQ_ID15,
            id_name = MQ_KURZNAME,
            street = STRASSE,
            direction = RICHTUNG,
            det_total, det_active,
            commissioned = INBETRIEBNAHME,
            decommissioned = ABBAUDATUM,
            lon = "LÄNGE (WGS84)", 
            lat = "BREITE (WGS84)") %>%
  distinct()

#### Remove duplicates (single counters decommissioned or deinstalled) ####
mq$id[which(duplicated(mq$id))]

mq = mq %>% group_by(id) %>% 
  mutate(dupl = n())

dupl = filter(mq, dupl > 1) %>% 
  mutate(decommissioned = max(decommissioned, na.rm = T)) %>%
  distinct()

nrow(mq)
mq = rbind(filter(mq, dupl == 1),
           dupl) %>%
  select(-dupl)
nrow(mq)

#### Number of decommissioned monitors ####
table(mq$decommissioned)

#### Save monitor-level data ####
saveRDS(data.frame(mq), file = "03_gen_data/traffic/be_mq.rds")

#### Clear the space ####
rm(list = ls()); gc()

#### ..................................................................... ####
#### 2. Hourly traffic counts                                              ####
#### ..................................................................... ####

# tag - Datum
# stunde - Stunde des Tages für die die Messwerte ermittelt wurden (8 => 08:00 - 08:59).
# qualitaet - gibt den Anteil der für die Stunde vorliegenden einwandfreien Messintervalle wieder: 1.0 = 100%.
# q_kfz_det_hr - Anzahl aller Kraftfahrzeuge in der Stunde.
# v_kfz_det_hr - Mittlere Geschwindigkeit [km/h] über alle Kraftfahrzeuge in der Stunde.
# q_pkw_det_hr - Anzahl aller Pkw in der Stunde.
# v_pkw_det_hr - Mittlere Geschwindigkeit [km/h] über alle Pkw in der Stunde.
# q_lkw_det_hr - Anzahl aller Lkw in der Stunde.
# v_lkw_det_hr - Mittlere Geschwindigkeit [km/h] über alle Lkw in der Stunde.

#### Load hourly monitor-level counts ####
dir <- "01_data/traffic/berlin/messquerschnitt/"
files <- list.files(dir)

dat <- lapply(paste0(dir, files), fread) %>%
  rbindlist(.)

#### Check data ####
str(dat)
sapply(dat, function(x) length(which(is.na(x))))
sapply(dat, function(x) length(which(x==-1)))

prop.table(table(substr(dat$tag, 7, 10), dat$q_kfz_mq_hr==-1), 1)*100

cols = colnames(dat[, -seq(1:4)])
# NAs per year
lapply(cols, function(x) prop.table(table(substr(dat$tag, 7, 10), dat[[x]]==-1), 1)*100) %>%
  setNames(., cols)
# Zeros per year
lapply(cols, function(x) prop.table(table(substr(dat$tag, 7, 10), dat[[x]]==0), 1)*100) %>%
  setNames(., cols)

#### Recode -1 to NA ####
dat =  mutate_at(dat,
                 vars(q_kfz_mq_hr:v_lkw_mq_hr), ~ifelse(. == -1, NA, .))

#### All variables NA? ####
dat %>% select(starts_with("q_"), starts_with("v_")) %>% filter_all(any_vars(!is.na(.)))

#### Rename variables ####
dat <- dat %>%
  rename(id_name = mq_name,
         date = tag,
         hour = stunde) %>%
  rename_with(~str_remove(., '_mq_hr')) %>%
  select(-qualitaet)

#### Reformat date ####
dat <- mutate(dat, date = as.Date(date,"%d.%m.%Y"))

#### Create time-related variables ####
dat = mutate(dat,
             year = year(date),
             month = month(date),
             week = week(date),
             weekday = weekdays(date),
             doy = yday(date))
dat = mutate(dat, year_month = zoo::as.yearmon(paste0(year,"-",month)))

#### Restrict to 2016 to 2021 (in line with pollution analysis) ####
dat <- filter(dat, year >= 2016, year <= 2021)

#### Merge with monitor-level characteristics ####
mq <- readRDS(file = "03_gen_data/traffic/be_mq.rds")

nrow(dat)
dat_mq <- left_join(dat, mq)
nrow(dat_mq)

#### Reorder and save hourly data ####
dat_mq <- select(dat_mq, id, id_name, street:lat, date, hour, year, month, year_month, week, weekday, doy,
                 everything()) 
saveRDS(dat_mq, file = "03_gen_data/traffic/be_mq_hourly.rds")


#### ..................................................................... ####
#### 3. Simple descriptives                                                ####
#### ..................................................................... ####

#### Read pollution and ban data ####
dat <- readRDS("03_gen_data/traffic/be_mq_hourly.rds")

#### Number of monitors per year-month ####
ggplot(unique(dat[, c("id", "year_month", "det_total")])) +
  geom_bar(aes(x = year_month, group = as.factor(det_total), fill = as.factor(det_total)), stat = "count") +
  labs(x = "Month", y = "Nr. of monitors", fill = "Nr. of detectors") +
  scale_fill_grey() +
  theme_bw() +
  theme(legend.position = "bottom")
ggsave("04_figures/nrmonitors_per_yearmonth.png", width = 6, height = 4)

#### Annual sum of Q ####
dat %>% group_by(year_month) %>%
  summarize_at(vars(starts_with("q")), sum, na.rm = T) %>%
  pivot_longer(cols = starts_with("q"), names_to = "var", values_to = "val") %>%
  separate(var, c("var", "type")) %>%
  filter(type != "kfz") %>%
  mutate(val = val/10^6,
         type = fct_relevel(type, "pkw", "lkw")) %>%
  ggplot() +
  geom_bar(aes(x = year_month, y = val, fill = type), stat="identity") +
  labs(x = "Month", y = "Sum of vehicle counts [mio.]", fill = "Vehicle type") +
  scale_fill_grey() +
  theme_bw() +
  theme(legend.position = "bottom")
ggsave(paste0("04_figures/yearmonth_sum.png"), width = 6, height = 4)


#### Monthly average of Q and V ####
dat %>% group_by(year_month) %>%
  summarize_at(vars(q_kfz:v_lkw), mean, na.rm = T) %>%
  pivot_longer(cols = q_kfz:v_lkw, names_to = "var", values_to = "val") %>%
  separate(var, c("var", "type")) %>%
  ggplot() +
  geom_line(aes(x = year_month, y = val, color = type)) +
  facet_wrap(. ~ var, scales = "free_y") +
  labs(x = "Month", y = "Mean values", fill = "Vehicle type") +
  theme_bw() +
  theme(legend.position = "bottom")
ggsave(paste0("04_figures/yearmonth_avg.png"),  width = 8, height = 5)

#### Monthly NAs Q and V ####
dat %>% group_by(year_month) %>%
  summarize_at(vars(q_kfz:v_lkw), list(~ length(which(is.na(.))))) %>%
  pivot_longer(cols = q_kfz:v_lkw, names_to = "var", values_to = "nas") %>%
  separate(var, c("var", "type")) %>%
  ggplot() +
  geom_line(aes(x = year_month, y = nas, color = type)) +
  facet_wrap(. ~ var, scales = "free_y") +
  labs(x = "Month", y = "Sum of NA values", fill = "Vehicle type") +
  theme_bw() +
  theme(legend.position = "bottom")
ggsave(paste0("04_figures/yearmonth_nas.png"),  width = 8, height = 5)

#### Monthly zeros Q and V ####
dat %>% group_by(year_month) %>%
  summarize_at(vars(q_kfz:v_lkw), list(~ length(which(. == 0)))) %>%
  pivot_longer(cols = q_kfz:v_lkw, names_to = "var", values_to = "zeros") %>%
  separate(var, c("var", "type")) %>%
  ggplot() +
  geom_line(aes(x = year_month, y = zeros, color = type)) +
  facet_wrap(. ~ var, scales = "free_y") +
  labs(x = "Month", y = "Sum of zero values", fill = "Vehicle type") +
  theme_bw() +
  theme(legend.position = "bottom")
ggsave(paste0("04_figures/yearmonth_zeros.png"),  width = 8, height = 5)

#### Weekday averages ####
dat %>%
  mutate(weekday = lubridate::wday(date, label = T)) %>% 
  group_by(weekday) %>% 
  summarize_at(vars(q_kfz:v_lkw), mean, na.rm = T) %>%
  pivot_longer(cols = q_kfz:v_lkw, names_to = "var", values_to = "val") %>%
  separate(var, c("var", "type")) %>%
  ggplot() +
  geom_line(aes(x = weekday, y = val, group = type, color = type)) +
  facet_wrap(. ~ var, scales = "free_y") +
  labs(x = "Weekday", y = "Mean value", fill = "Vehicle type") +
  theme_bw() +
  theme(legend.position = "bottom")
ggsave("04_figures/weekday_avg.png", width = 6, height = 5)


#### Hourly averages ####
dat %>% group_by(hour) %>% 
  summarize_at(vars(q_kfz:v_lkw), mean, na.rm = T) %>%
  pivot_longer(cols = q_kfz:v_lkw, names_to = "var", values_to = "val") %>%
  separate(var, c("var", "type")) %>%
  ggplot() +
  geom_line(aes(x = hour, y = val, color = type)) +
  facet_wrap(. ~ var, scales = "free_y") +
  labs(x = "Hour of the day", y = "Mean value", fill = "Vehicle type") +
  theme_bw() +
  theme(legend.position = "bottom")
ggsave("04_figures/hourly_avg.png", width = 6, height = 5)

#### number of daily nobs ####
dat %>% group_by(year_month, id, date) %>%
  summarize(nobs = n()) %>%
  group_by(year_month) %>%
  summarize(nobs = mean(nobs)) %>%
  ggplot() + geom_line(aes(x = year_month, y = nobs))

dat %>% group_by(year_month) %>%
  summarize(N = length(unique(id))) %>%
  ggplot() + geom_line(aes(x = year_month, y = N))