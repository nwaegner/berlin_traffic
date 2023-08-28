#### ..................................................................... ####
####      Treatment definition and sample selection                         ####
#### ..................................................................... ####
#### 1. Aggregate hourly traffic to daily averages
#### 2. Treatment definition and sample selection
#### 3. Aggregate daily traffic averages to monthly averages
#### ..................................................................... ####

rm(list=ls()); gc()

library(tidyverse)
library(zoo)
library(ggmap)
library(viridis)
library(purrr)
theme_set(theme_bw())

#### .................................................................. ####
#### 1. Aggregate hourly traffic to daily averages                      ####
#### .................................................................. ####

#### Load pollution data ####
dat <- readRDS(file = "03_gen_data/traffic/be_mq_hourly_weather.rds") 

#### Count hours with available measurements ####
notnas <- dat %>% group_by(id, date) %>%
  summarize_at(vars(q_kfz:v_lkw), list("hours" =~ length(which(!is.na(.)))))

summary(select(notnas, ends_with("hours")))

#### Daily sum (24h) ####
total <- dat %>% group_by(id, date) %>%
  summarize_at(vars(starts_with("q_")), sum, na.rm = T)

#### Daily averages (24h) ####
total_mean <- dat %>% group_by(id, date) %>%
  summarize_at(vars(starts_with("v_")), mean, na.rm = T)

#### count hours with available measurements in rush hours ####
notnas_rush <- dat %>% group_by(id, date) %>%
  mutate(time = ifelse(hour >= 7 & hour <= 10, "morning",
                       ifelse(hour >= 16 & hour <= 19, "evening", 
                              "rest"))) %>%
  group_by(id, date, time) %>%
  summarize_at(vars(q_kfz:v_lkw), list("hours" =~ length(which(!is.na(.))))) %>%
  pivot_wider(id_cols = c(id, date), names_from = "time", 
              values_from = c(q_kfz_hours, q_pkw_hours, q_lkw_hours, v_kfz_hours, v_pkw_hours, v_lkw_hours))

#### Daily sum (rush hours) ####
rush <- dat %>% 
  mutate(time = ifelse(hour >= 7 & hour <= 10, "morning",
                       ifelse(hour >= 16 & hour <= 19, "evening", 
                              "rest"))) %>%
  group_by(id, date, time) %>%
  summarize_at(vars(starts_with("q_")), sum, na.rm = T) %>% 
  pivot_wider(id_cols = c(id, date), names_from = "time", 
              values_from = c(q_kfz, q_pkw, q_lkw))

#### Daily averages (rush hours) ####
rush_mean <- dat %>% 
  mutate(time = ifelse(hour >= 7 & hour <= 10, "morning",
                       ifelse(hour >= 16 & hour <= 19, "evening", 
                              "rest"))) %>%
  group_by(id, date, time) %>%
  summarize_at(vars(v_kfz:v_lkw), mean, na.rm = T) %>%
  pivot_wider(id_cols = c(id, date), names_from = "time", 
              values_from = c(v_kfz, v_pkw, v_lkw))

#### Combine daily data frames ####
daily = select(dat, id:doy, ban_closest:ban_stringency, -hour) %>% distinct() 
weather = select(dat, id, date, atm_pressure:humidity_2) %>% distinct() 

df = list(daily,
          total, total_mean, notnas,
          rush, rush_mean, notnas_rush,
          weather) %>%
  reduce(left_join, by = c("id", "date"))

#### Save daily df ####
saveRDS(df, "03_gen_data/traffic/be_mq_daily.rds")
rm(list=ls()); gc()

#### .................................................................. ####
#### 2. Treatment definition and sample selection                       ####
#### .................................................................. ####

#### Load daily data ####
dat = readRDS("03_gen_data/traffic/be_mq_daily.rds")

#### Check whether data is complete ####
sapply(dat, function(x) length(which(x == 0)))
sapply(dat, function(x) length(which(is.na(x))))
summary(select(dat, ends_with("hours")))

table(dat$q_kfz_hours, dat$year, useNA = "ifany")

### Share of daily counts with h<24 per month ####
dat %>% 
  group_by(year_month) %>%
  summarize(q_kfz_incomplete = length(which(q_kfz_hours < 24))/length(which(!is.na(q_kfz))),
            q_pkw_incomplete = length(which(q_pkw_hours < 24))/length(which(!is.na(q_pkw))),
            q_lkw_incomplete = length(which(q_lkw_hours < 24))/length(which(!is.na(q_lkw))),
            v_kfz_incomplete = length(which(v_kfz_hours < 24))/length(which(!is.na(v_kfz))),
            v_pkw_incomplete = length(which(v_pkw_hours < 24))/length(which(!is.na(v_pkw))),
            v_lkw_incomplete = length(which(v_lkw_hours < 24))/length(which(!is.na(v_lkw)))) %>%
  pivot_longer(cols = q_kfz_incomplete:v_lkw_incomplete, names_to = "var", values_to = "val") %>%
  separate(var, c("var", "type")) %>%
  ggplot() +
  geom_line(aes(x = year_month, y = val, color = type)) +
  facet_wrap(. ~ var, scales = "free_y") +
  labs(x = "Month", y = "Share of NA daily values", fill = "Vehicle type") +
  theme_bw() +
  theme(legend.position = "bottom")
ggsave(paste0("04_figures/daily_incomplete_share.png"), width = 8, height = 5)

#### Drop 2021 due to incomplete data (zeros eliminated?) ####
dat = filter(dat, year < 2021)

#### Set incomplete daily measurements to NA ####
dat[dat$q_kfz_hours < 24, c("q_kfz")] <- NA
dat[dat$q_pkw_hours < 24, c("q_pkw")] <- NA
dat[dat$q_lkw_hours < 24, c("q_lkw")] <- NA
dat[dat$v_kfz_hours < 24, c("v_kfz")] <- NA
dat[dat$v_pkw_hours < 24, c("v_pkw")] <- NA
dat[dat$v_lkw_hours < 24, c("v_lkw")] <- NA

summary(select(dat, ends_with("hours_evening"), ends_with("hours_morning"), ends_with("hours_rest")))
dat[dat$q_kfz_hours_evening < 4 | is.na(dat$q_kfz_hours_evening), c("q_kfz_evening")] <- NA
dat[dat$q_pkw_hours_evening < 4 | is.na(dat$q_pkw_hours_evening), c("q_pkw_evening")] <- NA
dat[dat$q_lkw_hours_evening < 4 | is.na(dat$q_lkw_hours_evening), c("q_lkw_evening")] <- NA

dat[dat$q_kfz_hours_morning < 4 | is.na(dat$q_kfz_hours_morning), c("q_kfz_morning")] <- NA
dat[dat$q_pkw_hours_morning < 4 | is.na(dat$q_pkw_hours_morning), c("q_pkw_morning")] <- NA
dat[dat$q_lkw_hours_morning < 4 | is.na(dat$q_lkw_hours_morning), c("q_lkw_morning")] <- NA

dat[dat$q_kfz_hours_rest < 16 | is.na(dat$q_kfz_hours_rest), c("q_kfz_rest")] <- NA
dat[dat$q_pkw_hours_rest < 16 | is.na(dat$q_pkw_hours_rest), c("q_pkw_rest")] <- NA
dat[dat$q_lkw_hours_rest < 16 | is.na(dat$q_lkw_hours_rest), c("q_lkw_rest")] <- NA


#### Define treatment pre-/post dummy for nearest DB ####
dat = mutate(dat, 
             score = date - ban_startdate,
             post = ifelse(score >= 0, 1, 0))

#### Define treatment group dummies based on 25m, 50m, 75m, and 100m distance to DB #### 
dat <- mutate(dat,
              treatgrp15 = ifelse(ban_dist <= 15, 1, 0),
              treatgrp25 = ifelse(ban_dist <= 25, 1, 0),
              treatgrp75 = ifelse(ban_dist <= 75, 1, 0),
              treatgrp150 = ifelse(ban_dist <= 150, 1, 0))

#### Define ban radii ####
summary(dat$ban_dist)
dat <- mutate(dat,
              drop500 = ifelse(ban_dist <= 500, 1, 0),
              drop1000 = ifelse(ban_dist <= 1000, 1, 0),
              drop1500 = ifelse(ban_dist <= 1500, 1, 0))

#### Drop stations that enter/exit the panel after/before (nearest) ban is enacted ####
nrow(dat); length(unique(dat$id))
dat <- dat %>% group_by(id) %>% mutate(min_score = min(score),
                                       max_score = max(score))
table(dat$max_score, useNA = "ifany")
table(dat$min_score, useNA = "ifany")

#### ......................... ####
#### Create shares of PKW/LKW  ####
#### ......................... ####

dat <- mutate(dat,
              share_pkw = q_pkw/(q_pkw + q_lkw)*100,
              share_lkw = q_lkw/(q_pkw + q_lkw)*100)
summary(dat[, c("share_pkw", "share_lkw")])

#### .................... ####
#### Create log outcomes  ####
#### .................... ####

dat <- mutate_at(dat,
                 vars(q_kfz, q_pkw, q_lkw),
                 list("log" =~ log(.)))

#### ............................................................ ####
#### Create variables for control group selection / robustness    ####
#### ............................................................ ####

#### Create dummy for municipalities with diesel ban ####
dat <- dat %>% 
  mutate(city_with_ban = 1)
#### Create dummy for Covid lockdowns ####
dat <- mutate(dat,
              lockdown = ifelse(date >= as.Date("2020-03-22"), 1, 0))

saveRDS(dat, file = "03_gen_data/traffic/be_mq_daily_treated.rds")
rm(list=ls()); gc()

#### .................................................................. ####
#### 2. Aggregate daily pollution measurements to monthly averages      ####
#### .................................................................. ####

#### Load daily data ####
dat <- readRDS(file = "03_gen_data/traffic/be_mq_daily_treated.rds")
table(dat$week, dat$year, useNA = "ifany")
table(dat$ban_startdate)

#### Aggregate to monthly averages ####
monthly = dat %>%
  group_by(id, id_name, street, direction, det_total, det_active,
           commissioned, decommissioned, lon, lat, 
           year, month, year_month,
           ban_closest,  ban_dist, ban_startdate, ban_enddate, ban_stringency, 
           treatgrp15, treatgrp25, treatgrp75, treatgrp150, 
           drop500, drop1000, drop1500, city_with_ban) %>%
  summarise_at(vars(starts_with("q_"), starts_with("v_"), share_pkw, share_lkw,
                    atm_pressure, humidity, precipitation, snow, sunshine, temperature,
                    temperature_max, temperature_min, vapor_pressure, wind_speed, wind_speed_max),
               mean, na.rm = T) %>% ungroup()

length(which(duplicated(monthly[, c("id", "year_month")])))

#### Define treatment pre-/post dummy for nearest DB ####
monthly = mutate(monthly, 
                 score = round((year_month - as.yearmon(ban_startdate))*12),
                 post = ifelse(score >= 0, 1, 0))

#### Create log outcomes ####
monthly <- mutate_at(monthly,
                     vars(q_kfz, q_pkw, q_lkw),
                     list("log" =~ log(.)))

#### Create dummy for Covid lockdowns ####
monthly <- mutate(monthly,
                  lockdown = ifelse(year_month >= as.yearmon("2020-03"), 1, 0))

#### Create squared weather variables ####
monthly = mutate_at(monthly, 
                    vars(temperature, precipitation, sunshine, wind_speed, humidity), 
                    list("2" = ~ .^2))

#### Create cubic weather variables ####
monthly = mutate_at(monthly, 
                    vars(temperature, precipitation, sunshine, wind_speed, humidity), 
                    list("3" = ~ .^3))

#### Create quintiles and deciles of weather variables ####
monthly = mutate_at(monthly, 
                    vars(temperature, precipitation, sunshine, wind_speed, humidity), 
                    list("quint" = ~
                           as.factor(as.numeric(cut(., 
                                                    breaks = unique(quantile(., probs = seq(0, 1, length = 6), na.rm = T)), 
                                                    na.rm = T, include.lowest=TRUE)))))

monthly = mutate_at(monthly, 
                    vars(temperature, precipitation, sunshine, wind_speed, humidity), 
                    list("dec" = ~ 
                           as.factor(as.numeric(cut(., 
                                                    breaks = unique(quantile(., probs = seq(0, 1, length = 11), na.rm = T)), 
                                                    na.rm = T, include.lowest=TRUE)))))

#### Create dummy for stations that are observed continuously from 2016 to 2021 ####
nobs <- monthly %>%
  group_by(id, treatgrp25, treatgrp150) %>%
  summarize(dur = n(),
            balanced = ifelse(dur == 58, 1, 0)) 
table(nobs$treatgrp25, nobs$dur)
table(nobs$treatgrp150, nobs$dur)

monthly = left_join(monthly, nobs)

#### Check for duplicates and missings ####
length(which(duplicated(monthly[, c("id", "year_month")])))
sapply(monthly, function(x) length(which(is.na(x))))

#### Save monthly data ####
saveRDS(monthly, file = "03_gen_data/traffic/be_mq_monthly.rds")
