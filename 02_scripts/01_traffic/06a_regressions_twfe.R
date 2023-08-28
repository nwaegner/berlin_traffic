#### ..................................................................... ####
####      TWFE regressions of impact on diesel bans on traffic             ####
#### ..................................................................... ####
#### 0. Function to estimate standard TWFE models
#### 1. TWFE regressions on monthly data
####    1.1. Control group: Cities with diesel ban
####      1.1.1. Weather & FE specifications
####      1.1.2. Treatment and buffer radii 
####      1.1.3. Pre-trends
####      1.1.4. Robustness

#### 2. TWFE regressions on daily data
####    2.1. Control group: Cities with diesel ban
####    2.2. Control group: NO2 exceeding stations
#### ..................................................................... ####

rm(list=ls()); gc()

library(tidyverse)
library(fixest)
library(viridis)
library(texreg)
# remotes::install_github("grantmcdermott/ggiplot")
library(ggiplot)

#### .................................................................. ####
#### 0. Function to estimate standard TWFE models 
#### .................................................................. ####

#### Function to run TWFE regressions #####
# rtreat - numeric value determining treatment radius around bans (default: 25m)
# rbuffer - numeric value determining buffer radius around bans (default: 0m)
# y - character value with dependent variable in data.frame "data" (default: NO2)
# covs - character vector with left-hans side variables (default: post-treatment dummy)
# fes - character vector with fixed effects (default: station and year FEs)
# clustervar - character vector indivating the level of clustering (default: station)

twfe <- function(rtreat = 25, rbuffer = 0, y = "q_kfz", covs = "post", fes = c("id", "year"), clustervar = "id"){
  
  # rtreat = 25; rbuffer = 0; y = "o3"; covs = "post"; fes = c("station", "year")
  
  print(paste0("Fct. call with treat radius: ", rtreat, ", buffer radius:", rbuffer,
               " dep. var.: ", y, ", indep. vars.: ", paste(covs, collapse = "+"), 
               " fes: ", paste(fes, collapse = "+")))
  
  # --- Define treatment dummy (pre-post ban)
  sample = mutate(data, treatgrp = ifelse(ban_dist <= rtreat, 1, 0))
  sample = mutate(sample, post = treatgrp*post)
  
  # --- Impose buffer radius
  sample = filter(sample, treatgrp == 1 | ban_dist > rbuffer)
  
  # --- Generate formula for regression
  formula = as.formula(paste0(y, 
                              " ~ ",
                              paste(covs, collapse = " + "), 
                              " | ",
                              paste(fes, collapse = " + ")))
  
  # --- Run linear regression
  reg = feols(fml = formula, data = sample, cluster = clustervar)
  regsum = summary(reg)
  
  # --- Save regression results
  coefs = data.frame("indep" = rownames(regsum$coeftable),
                     "est" = regsum$coeftable[, 1],
                     "se" = regsum$coeftable[, 2],
                     "tval" = regsum$coeftable[ ,3],
                     "pval" = regsum$coeftable[ ,4])
  mstat = data.frame("rtreat" = rep(rtreat, nrow(coefs)),
                     "rbuffer" = rep(rbuffer, nrow(coefs)),
                     "depvar" = rep(y, nrow(coefs)),
                     "covs" = rep(paste(covs, collapse = "+"), nrow(coefs)),
                     "fes" = rep(paste(fes, collapse = "+"), nrow(coefs)),
                     "clustervar" = rep(clustervar, nrow(coefs)),
                     "rsq" = rep(as.numeric(r2(reg, type = "ar2")), nrow(coefs)),
                     "nobs" = rep(regsum$nobs, nrow(coefs)),
                     "N" = rep(regsum$fixef_sizes[1], nrow(coefs)))
  
  res = cbind(coefs, mstat) 
  return(res)
}

#### .................................................................. ####
#### 1. TWFE regressions on monthly data                                ####
#### .................................................................. ####

#### ........................................................... ####
#### 1.1. Control group: Cities with diesel bans                 ####
#### ........................................................... ####

#### Select stations in cities with diesel bans as controls ####
pol <- readRDS(file = "03_gen_data/traffic/be_mq_monthly.rds")
data <- filter(pol, city_with_ban == 1)

# sample = mutate(data, post = treatgrp25*post) %>%
#   filter(lockdown == 0)
# reg = feols(fml = as.formula("q_kfz ~ post + precipitation + temperature + sunshine + wind_speed + humidity | id + year_month"),
#             data = sample, cluster = "id")
# summary(reg)

#### ........................................... ####
#### 1.1.1. Weather & FE specifications          ####
#### ........................................... ####

#### Weather specifications ###
w0 = c("post")
w1 = c("post", 
       "precipitation", "temperature", "sunshine", "wind_speed", "humidity")
w2 = c("post",
       "precipitation", "temperature", "sunshine", "wind_speed", "humidity",
       "precipitation_2", "temperature_2", "sunshine_2", "wind_speed_2", "humidity_2")
w3 = c("post", 
       "precipitation_quint", "temperature_quint", "sunshine_quint", "wind_speed_quint", "humidity_quint")

#### FE specifications ####
fes0 = c("id", "year")
fes1 = c("id", "year", "month")
fes2 = c("id", "year_month")
# fes3 = c("station", "year_month", "mun_name^year_month")
# fes4 = c("station^month", "year_month", "mun_name^year_month")

#### Fct. calls over different weather covariates ####
inp = expand.grid(25,
                  1000,
                  list("q_kfz", "q_pkw", "q_lkw", "q_kfz_log", "q_pkw_log", "q_lkw_log",
                       "q_kfz_morning", "q_pkw_morning", "q_lkw_morning",
                       "q_kfz_evening", "q_pkw_evening", "q_lkw_evening",
                       "q_kfz_rest", "q_pkw_rest", "q_lkw_rest",
                       "share_pkw", "share_lkw"), 
                  list(w0, w1, w2, w3),
                  list(fes0, fes1, fes2),
                  list("id"))

out = mapply(twfe,
             rtreat = inp[,1],
             rbuffer = inp[,2],
             y = inp[,3],
             covs = inp[,4],
             fes = inp[,5],
             clustervar = inp[,6],
             SIMPLIFY = F)

#### List of results to data frame ####
res = dplyr::bind_rows(out, .id = "fct_call")
head(res)

#### Plot results ####
# Baseline: ban dummy
res = mutate(res,
             depvar = fct_relevel(depvar, 
                                  "q_kfz", "q_pkw", "q_lkw", "q_kfz_log", "q_pkw_log", "q_lkw_log",
                                  "q_kfz_morning", "q_pkw_morning", "q_lkw_morning",
                                  "q_kfz_evening", "q_pkw_evening", "q_lkw_evening",
                                  "q_kfz_rest", "q_pkw_rest", "q_lkw_rest",
                                  "share_pkw", "share_lkw"),
             weather = ifelse(covs == paste(w0, collapse = "+"), "nocovs",
                              ifelse(covs == paste0(w1, collapse = "+"), "weather",
                                     ifelse(covs == paste0(w2, collapse = "+"), "weather2",
                                            ifelse(covs == paste0(w3, collapse = "+"), "weather5", covs)))),
             weather = fct_relevel(weather, "nocovs", "weather", "weather2", "weather5"),
             fes = fct_relevel(fes, c(paste(fes0, collapse = "+"), paste(fes1, collapse = "+"), paste(fes2, collapse = "+"))))

ggplot(filter(res, indep == "post")) + 
  geom_point(aes(x = weather, y = est, color = fes), 
             position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(ymax = est + se*1.96, ymin = est - se*1.96, x = weather, color = fes), width = .2, 
                position = position_dodge(width=0.5)) +
  facet_wrap(clustervar ~ depvar, scales = "free_y", ncol = 3) +
  scale_color_viridis(discrete = T) + 
  theme(panel.background = element_rect(fill = "white", color = "white"), 
        text = element_text(size = 10), 
        axis.line = element_line(), 
        strip.text = element_text(hjust = 0)) +
  ggpubr::grids() + 
  geom_hline(yintercept = 0, color = "black", linetype = "dashed") +
  theme(legend.position="bottom") +
  guides(color=guide_legend(nrow=1))
ggsave(paste0("04_figures/twfe_monthly_bancities_treat25.png"), width = 12, height = 8)

#### Tables ####
## Levels ####
d = res %>% 
  filter(depvar %in% c("q_kfz", "q_pkw", "q_lkw"), 
         indep == "post", clustervar == "id") %>% 
  arrange(depvar) %>% 
  filter((weather == "nocovs" & fes == paste(fes2, collapse = "+")) |
           (weather == "weather2" & fes == c(paste(fes2, collapse = "+"))) |
           (weather == "weather5" & fes == c(paste(fes2, collapse = "+")))) %>%
  group_split(depvar, weather, fes) 

latex = lapply(d, function(x) createTexreg("Post", x$est, x$se, x$pval, 
                                           gof.names = c(rep("Nobs", nrow(x)),
                                                         rep("N", nrow(x)),
                                                         rep("Adj.R2", nrow(x))),
                                           gof = c(x$nobs, 
                                                   x$N,
                                                   x$rsq),
                                           gof.decimal = c(rep(F, nrow(x)),
                                                           rep(F, nrow(x)),
                                                           rep(T, nrow(x)))))
latex
texreg(latex, 
       digits = 0, booktabs = TRUE, use.packages = F,
       custom.model.names = paste0("(", c(rep(1:3, times = 3)), ")"),
       caption = "Impact of diesel bans on traffic intensity",
       caption.above = TRUE, float.pos = "h", fontsize = "footnotesize",
       custom.note = "\\item Treated monitors within 25m distance to a diesel ban. 
       Control monitors in cities with diesel ban further away than 1000m from the ban. 
       Standard errors clustered at the monitor level.
       Significance levels %stars.",
       threeparttable = TRUE,
       file = paste0("05_tables/twfe_monthly_bancities_treat25_b1000.tex"))

## Add model details in separate rows
s = readLines("05_tables/twfe_monthly_bancities_treat25_b1000.tex")

s = c(s[1:8],
      "& \\multicolumn{3}{c}{KFZ} & \\multicolumn{3}{c}{PKW}  & \\multicolumn{3}{c}{LKW} \\\\ \\cmidrule(r){2-4} \\cmidrule(r){5-7} \\cmidrule(r){8-10}",
      s[9:length(s)])
s = c(s[1:14],
      "Monitor FEs   & Yes & Yes & Yes  & Yes & Yes & Yes & Yes & Yes & Yes    \\\\",
      "Year-month FEs     & Yes & Yes & Yes  & Yes & Yes & Yes & Yes & Yes & Yes  \\\\",
      "Weather     & No & Quadr. & Quint.  & No & Quadr. & Quint. & No & Quadr. & Quint.   \\\\ \\midrule",
      s[15:length(s)])
s
writeLines(s, "05_tables/twfe_monthly_bancities_treat25_b1000.tex")

## Logs ####
d = res %>% 
  filter(depvar %in% c("q_kfz_log", "q_pkw_log", "q_lkw_log"), 
         indep == "post", clustervar == "id") %>% 
  arrange(depvar) %>% 
  filter((weather == "nocovs" & fes == paste(fes2, collapse = "+")) |
           (weather == "weather2" & fes == c(paste(fes2, collapse = "+"))) |
           (weather == "weather5" & fes == c(paste(fes2, collapse = "+")))) %>%
  group_split(depvar, weather, fes) 

latex = lapply(d, function(x) createTexreg("Post", x$est, x$se, x$pval, 
                                           gof.names = c(rep("Nobs", nrow(x)),
                                                         rep("N", nrow(x)),
                                                         rep("Adj.R2", nrow(x))),
                                           gof = c(x$nobs, 
                                                   x$N,
                                                   x$rsq),
                                           gof.decimal = c(rep(F, nrow(x)),
                                                           rep(F, nrow(x)),
                                                           rep(T, nrow(x)))))
latex
texreg(latex, 
       digits = 3, booktabs = TRUE, use.packages = F,
       custom.model.names = paste0("(", c(rep(1:3, times = 3)), ")"),
       caption = "Impact of diesel bans on traffic intensity (log)",
       caption.above = TRUE, float.pos = "h", fontsize = "footnotesize",
       custom.note = "\\item Treated monitors within 25m distance to a diesel ban. 
       Control monitors in cities with diesel ban further away than 1000m from the ban. 
       Standard errors clustered at the monitor level.
       Significance levels %stars.",
       threeparttable = TRUE,
       file = paste0("05_tables/twfe_monthlylog_bancities_treat25_b1000.tex"))

## Add model details in separate rows
s = readLines("05_tables/twfe_monthlylog_bancities_treat25_b1000.tex")

s = c(s[1:8],
      "& \\multicolumn{3}{c}{Log KFZ } & \\multicolumn{3}{c}{Log PKW }  & \\multicolumn{3}{c}{Log LKW } \\\\ \\cmidrule(r){2-4} \\cmidrule(r){5-7} \\cmidrule(r){8-10}",
      s[9:length(s)])
s = c(s[1:14],
      "Monitor FEs   & Yes & Yes & Yes  & Yes & Yes & Yes & Yes & Yes & Yes    \\\\",
      "Year-month FEs     & Yes & Yes & Yes  & Yes & Yes & Yes & Yes & Yes & Yes  \\\\",
      "Weather     & No & Quadr. & Quint.  & No & Quadr. & Quint. & No & Quadr. & Quint.   \\\\ \\midrule",
      s[15:length(s)])
s
writeLines(s, "05_tables/twfe_monthlylog_bancities_treat25_b1000.tex")


#### ................................................ ####
#### 1.1.2. Treatment and buffer radii                ####
#### ................................................ ####

#### Fct. calls over different weather covariates ####
inp = expand.grid(list(15, 25, 75, 150),
                  list(0, 500, 1000, 1500),
                  list("q_kfz", "q_pkw", "q_lkw", "q_kfz_log", "q_pkw_log", "q_lkw_log"), 
                  list(w2),
                  list(fes2),
                  list("id"))

out = mapply(twfe,
             rtreat = inp[,1],
             rbuffer = inp[,2],
             y = inp[,3],
             covs = inp[,4],
             fes = inp[,5],
             clustervar = inp[,6],
             SIMPLIFY = F)

#### List of results to data frame ####
res = dplyr::bind_rows(out, .id = "fct_call")
head(res)

#### Plot results ####
res = mutate(res, 
             depvar = fct_relevel(depvar, "q_kfz", "q_pkw", "q_lkw", "q_kfz_log", "q_pkw_log", "q_lkw_log"),
             rtreat = fct_relevel(factor(rtreat), as.character(c(15, 25, 75, 150))),
             rbuffer = fct_relevel(factor(rbuffer), "0", "500", "1000", "1500"),
             clustervar = fct_recode(factor(clustervar), 
                                     "SEs clustered at monitor level" = "id"))
# "SEs clustered at municipality level" = "mun_name"))

ggplot(filter(res, indep == "post")) + 
  geom_point(aes(x = rtreat, y = est, color = rbuffer), 
             position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(ymax = est + se*1.96, ymin = est - se*1.96, 
                    x = rtreat, color = rbuffer), width = .2, 
                position = position_dodge(width=0.5)) +
  facet_wrap(fct_rev(clustervar) ~ depvar, scales = "free_y", ncol = 3) +
  scale_color_viridis(discrete = T) + 
  theme(panel.background = element_rect(fill = "white", color = "white"), 
        text = element_text(size = 10), 
        axis.line = element_line(), 
        strip.text = element_text(hjust = 0)) +
  ggpubr::grids() + geom_hline(yintercept = 0, color = "black", linetype = "dashed") +
  theme(legend.position="bottom")
ggsave(paste0("04_figures/twfe_monthly_bancities_treatrad_bufferrad.png"), width = 12, height = 8)

#### ........................................... ####
#### 1.1.3. Pre-trends                           ####
#### ........................................... ####

data_pt <- filter(data, treatgrp25 == 1 | ban_dist > 1000) 

#### KFZ ####
pt_kfz_1 <- feols(fml = as.formula("q_kfz ~ i(score, treatgrp25, ref = -1) + 
                              precipitation + temperature + sunshine + wind_speed + humidity +
                              precipitation_2 + temperature_2 + sunshine_2 + wind_speed_2 + humidity_2 | 
                              id + year_month"), 
                  data = data_pt, cluster = "id")
iplot(pt_kfz_1)

pt_kfzlog_1 <- feols(fml = as.formula("q_kfz_log ~ i(score, treatgrp25, ref = -1) + 
                              precipitation + temperature + sunshine + wind_speed + humidity +
                              precipitation_2 + temperature_2 + sunshine_2 + wind_speed_2 + humidity_2 | 
                              id + year_month"), 
                     data = data_pt, cluster = "id")
iplot(pt_kfzlog_1)


#### PKW ####
pt_pkw_1 <- feols(fml = as.formula("q_pkw ~ i(score, treatgrp25, ref = -1) + 
                              precipitation + temperature + sunshine + wind_speed + humidity +
                              precipitation_2 + temperature_2 + sunshine_2 + wind_speed_2 + humidity_2 | 
                              id + year_month"), 
                  data = data_pt, cluster = "id")
iplot(pt_pkw_1)

pt_pkwlog_1 <- feols(fml = as.formula("q_pkw_log ~ i(score, treatgrp25, ref = -1) + 
                              precipitation + temperature + sunshine + wind_speed + humidity +
                              precipitation_2 + temperature_2 + sunshine_2 + wind_speed_2 + humidity_2 | 
                              id + year_month"), 
                     data = data_pt, cluster = "id")
iplot(pt_pkwlog_1)

#### LKW ####
pt_lkw_1 <- feols(fml = as.formula("q_lkw ~ i(score, treatgrp25, ref = -1) + 
                              precipitation + temperature + sunshine + wind_speed + humidity +
                              precipitation_2 + temperature_2 + sunshine_2 + wind_speed_2 + humidity_2 | 
                              id + year_month"), 
                  data = data_pt, cluster = "id")
iplot(pt_lkw_1)

pt_lkwlog_1 <- feols(fml = as.formula("q_lkw_log ~ i(score, treatgrp25, ref = -1) + 
                              precipitation + temperature + sunshine + wind_speed + humidity +
                              precipitation_2 + temperature_2 + sunshine_2 + wind_speed_2 + humidity_2 | 
                              id + year_month"), 
                     data = data_pt, cluster = "id")
iplot(pt_lkwlog_1)


#### Plots ####
## Levels ####
ggiplot(list("KFZ" = pt_kfz_1, "PKW" = pt_pkw_1, "LKW" = pt_lkw_1),
        ref.line = -1,
        xlab = 'Time to treatment',
        multi_style = 'facet',
        geom_style = 'ribbon',
        facet_args = list(labeller = labeller(id = \(x) gsub(".*: ", "", x))),
        theme = theme_minimal() +
          theme(
            plot.title = element_text(hjust = 0.5),
            legend.position = 'none'
          )
)
ggsave("04_figures/twfe_monthly_pt.png", width = 10, height = 5)

## Logs ####
ggiplot(list("Log KFZ " = pt_kfzlog_1, "Log PKW " = pt_pkwlog_1, "Log LKW " = pt_lkwlog_1),
        ref.line = -1,
        xlab = 'Time to treatment',
        multi_style = 'facet',
        geom_style = 'ribbon',
        facet_args = list(labeller = labeller(id = \(x) gsub(".*: ", "", x))),
        theme = theme_minimal() +
          theme(
            plot.title = element_text(hjust = 0.5),
            legend.position = 'none'
          )
)
ggsave("04_figures/twfe_monthly_pt_log.png", width = 10, height = 5)

#### ........................................... ####
#### 1.1.4. Robustness                           ####
#### ........................................... ####

#### Keep data frame with all controls ####
data_sample <- data

#### Regressions with more flexible weather controls ####
#### ............................................... ####

wflex1 <- c("post",
            "precipitation", "temperature", "sunshine", "wind_speed", "humidity",
            "precipitation_2", "temperature_2", "sunshine_2", "wind_speed_2", "humidity_2",
            "precipitation_3", "temperature_3", "sunshine_3", "wind_speed_3", "humidity_3")
wflex2 <- c("post",
            "precipitation", "temperature", "sunshine", "wind_speed", "humidity",
            "precipitation_quint", "temperature_quint", "sunshine_quint", "wind_speed_quint", "humidity_quint")
wflex3 <- c("post",
            "precipitation", "temperature", "sunshine", "wind_speed", "humidity",
            "precipitation_dec", "temperature_dec", "sunshine_dec", "wind_speed_dec", "humidity_dec")
wflex4 <- c("post",
            "precipitation", "temperature", "sunshine", "wind_speed", "humidity", 
            "temperature_min", "temperature_max", "wind_speed_max", "atm_pressure", "vapor_pressure")

inp = expand.grid(list(25),
                  list(1000),
                  list("q_kfz", "q_pkw", "q_lkw", "q_kfz_log", "q_pkw_log", "q_lkw_log"), 
                  list(wflex1, wflex2, wflex3, wflex4),
                  list(fes2),
                  list("id"))

weather = mapply(twfe,
                 rtreat = inp[,1],
                 rbuffer = inp[,2],
                 y = inp[,3],
                 covs = inp[,4],
                 fes = inp[,5],
                 clustervar = inp[,6],
                 SIMPLIFY = F) %>%
  bind_rows(., .id = "fct_call") %>%
  mutate(depvar = fct_relevel(depvar, "q_kfz", "q_pkw", "q_lkw", "q_kfz_log", "q_pkw_log", "q_lkw_log"),
         weather = ifelse(covs == paste(wflex1, collapse = "+"), "cubic",
                          ifelse(covs == paste0(wflex2, collapse = "+"), "quint",
                                 ifelse(covs == paste0(wflex3, collapse = "+"), "decile",
                                        ifelse(covs == paste0(wflex4, collapse = "+"), "allvars", covs)))),
         weather = fct_relevel(weather, "cubic", "quint", "decile", "allvars"))

#### Tables ####
## KFZ ####
d = weather %>%
  filter(depvar %in% c("q_kfz", "q_kfz_log"), indep == "post", clustervar == "id") %>%
  group_split(depvar, weather)

latex = lapply(d, function(x) createTexreg("Post", x$est, x$se, x$pval,
                                           gof.names = c(rep("Nobs", nrow(x)),
                                                         rep("N", nrow(x)),
                                                         rep("Adj.R2", nrow(x))),
                                           gof = c(x$nobs,
                                                   x$N,
                                                   x$rsq),
                                           gof.decimal = c(rep(F, nrow(x)),
                                                           rep(F, nrow(x)),
                                                           rep(T, nrow(x)))))
latex
texreg(latex,
       digits = 3, booktabs = TRUE, use.packages = F,
       custom.model.names = paste0("(", c(rep(1:4, times = 2)), ")"),
       caption = "Weather robustness: Impact of diesel bans on KFZ intensity",
       caption.above = TRUE, float.pos = "h", fontsize = "footnotesize",
       custom.note = "\\item Treated monitors within 25m distance to a diesel ban.
       Control monitors in cities with diesel ban further away than 1000m from the ban.
       Standard errors clustered at the monitor level.
       Significance levels %stars.",
       threeparttable = TRUE,
       file = paste0("05_tables/twfe_monthly_kfz_bancities_robust_weather.tex"))

## Add model details in separate rows
s = readLines("05_tables/twfe_monthly_kfz_bancities_robust_weather.tex")

s = c(s[1:8],
      "& \\multicolumn{4}{c}{KFZ} & \\multicolumn{4}{c}{Log KFZ}  \\\\ \\cmidrule(r){2-5} \\cmidrule(r){6-9}",
      s[9:length(s)])
s = c(s[1:14],
      "Monitor FEs   & Yes & Yes & Yes  & Yes & Yes & Yes & Yes & Yes     \\\\",
      "Year-month FEs     & Yes & Yes & Yes  & Yes & Yes & Yes & Yes & Yes   \\\\",
      "Mun-year-month FEs     & Yes & Yes & Yes  & Yes & Yes & Yes & Yes & Yes   \\\\",
      "Weather     & Cubic & Quint. & Dec.  & All & Cubic & Quint. & Dec.  & All    \\\\ \\midrule",
      s[15:length(s)])
s
writeLines(s, "05_tables/twfe_monthly_kfz_bancities_robust_weather.tex")


## PKW ####
d = weather %>%
  filter(depvar %in% c("q_pkw", "q_pkw_log"), indep == "post", clustervar == "id") %>%
  group_split(depvar, weather)

latex = lapply(d, function(x) createTexreg("Post", x$est, x$se, x$pval,
                                           gof.names = c(rep("Nobs", nrow(x)),
                                                         rep("N", nrow(x)),
                                                         rep("Adj.R2", nrow(x))),
                                           gof = c(x$nobs,
                                                   x$N,
                                                   x$rsq),
                                           gof.decimal = c(rep(F, nrow(x)),
                                                           rep(F, nrow(x)),
                                                           rep(T, nrow(x)))))
latex
texreg(latex,
       digits = 3, booktabs = TRUE, use.packages = F,
       custom.model.names = paste0("(", c(rep(1:4, times = 2)), ")"),
       caption = "Weather robustness: Impact of diesel bans on PKW intensity",
       caption.above = TRUE, float.pos = "h", fontsize = "footnotesize",
       custom.note = "\\item Treated monitors within 25m distance to a diesel ban.
       Control monitors in cities with diesel ban further away than 1000m from the ban.
       Standard errors clustered at the monitor level.
       Significance levels %stars.",
       threeparttable = TRUE,
       file = paste0("05_tables/twfe_monthly_pkw_bancities_robust_weather.tex"))

## Add model details in separate rows
s = readLines("05_tables/twfe_monthly_pkw_bancities_robust_weather.tex")

s = c(s[1:8],
      "& \\multicolumn{4}{c}{PKW} & \\multicolumn{4}{c}{Log PKW}  \\\\ \\cmidrule(r){2-5} \\cmidrule(r){6-9}",
      s[9:length(s)])
s = c(s[1:14],
      "Monitor FEs   & Yes & Yes & Yes  & Yes & Yes & Yes & Yes & Yes     \\\\",
      "Year-month FEs     & Yes & Yes & Yes  & Yes & Yes & Yes & Yes & Yes   \\\\",
      "Mun-year-month FEs     & Yes & Yes & Yes  & Yes & Yes & Yes & Yes & Yes   \\\\",
      "Weather     & Cubic & Quint. & Dec.  & All & Cubic & Quint. & Dec.  & All    \\\\ \\midrule",
      s[15:length(s)])
s
writeLines(s, "05_tables/twfe_monthly_pkw_bancities_robust_weather.tex")


## LKW ####
d = weather %>%
  filter(depvar %in% c("q_lkw", "q_lkw_log"), indep == "post", clustervar == "id") %>%
  group_split(depvar, weather)

latex = lapply(d, function(x) createTexreg("Post", x$est, x$se, x$pval,
                                           gof.names = c(rep("Nobs", nrow(x)),
                                                         rep("N", nrow(x)),
                                                         rep("Adj.R2", nrow(x))),
                                           gof = c(x$nobs,
                                                   x$N,
                                                   x$rsq),
                                           gof.decimal = c(rep(F, nrow(x)),
                                                           rep(F, nrow(x)),
                                                           rep(T, nrow(x)))))
latex
texreg(latex,
       digits = 3, booktabs = TRUE, use.packages = F,
       custom.model.names = paste0("(", c(rep(1:4, times = 2)), ")"),
       caption = "Weather robustness: Impact of diesel bans on LKW intensity",
       caption.above = TRUE, float.pos = "h", fontsize = "footnotesize",
       custom.note = "\\item Treated monitors within 25m distance to a diesel ban.
       Control monitors in cities with diesel ban further away than 1000m from the ban.
       Standard errors clustered at the monitor level.
       Significance levels %stars.",
       threeparttable = TRUE,
       file = paste0("05_tables/twfe_monthly_lkw_bancities_robust_weather.tex"))

## Add model details in separate rows
s = readLines("05_tables/twfe_monthly_lkw_bancities_robust_weather.tex")

s = c(s[1:8],
      "& \\multicolumn{4}{c}{LKW} & \\multicolumn{4}{c}{Log LKW}  \\\\ \\cmidrule(r){2-5} \\cmidrule(r){6-9}",
      s[9:length(s)])
s = c(s[1:14],
      "Monitor FEs   & Yes & Yes & Yes  & Yes & Yes & Yes & Yes & Yes     \\\\",
      "Year-month FEs     & Yes & Yes & Yes  & Yes & Yes & Yes & Yes & Yes   \\\\",
      "Mun-year-month FEs     & Yes & Yes & Yes  & Yes & Yes & Yes & Yes & Yes   \\\\",
      "Weather     & Cubic & Quint. & Dec.  & All & Cubic & Quint. & Dec.  & All    \\\\ \\midrule",
      s[15:length(s)])
s
writeLines(s, "05_tables/twfe_monthly_lkw_bancities_robust_weather.tex")


# #### Interaction with LEZs ####
# #### ..................... ####
# 
# lezs = mapply(twfe,
#               rtreat = 25,
#               rbuffer = 1000,
#               y = list("no2", "pm10", "no2_log", "pm10_log", "no2_above40", "pm10_above40"),
#               covs = list(c("post*lez_active",
#                             "precipitation", "temperature", "sunshine", "wind_speed", "humidity",
#                             "precipitation_2", "temperature_2", "sunshine_2", "wind_speed_2", "humidity_2")),
#               fes = list(fes3),
#               clustervar = "station",
#               SIMPLIFY = F) %>%
#   bind_rows(., .id = "fct_call") %>%
#   mutate(robustness = "lez")

#### Drop Covid lockdown periods and run TWFE regs ####
#### ............................................. ####

data <- filter(data_sample, lockdown == 0) 

#### Fct. calls over different weather covariates ####
inp = expand.grid(25,
                  1000,
                  list("q_kfz", "q_pkw", "q_lkw", "q_kfz_log", "q_pkw_log", "q_lkw_log"), 
                  list(w0, w1, w2, w3),
                  list(fes0, fes1, fes2),
                  list("id"))

lockdown = mapply(twfe,
                  rtreat = inp[,1],
                  rbuffer = inp[,2],
                  y = inp[,3],
                  covs = inp[,4],
                  fes = inp[,5],
                  clustervar = inp[,6],
                  SIMPLIFY = F) %>%
  bind_rows(., .id = "fct_call") %>%
  mutate(robustness = "lockdown")

#### Plot results ####
# Baseline: ban dummy
lockdown = mutate(lockdown,
                  depvar = fct_relevel(depvar, "q_kfz", "q_pkw", "q_lkw", "q_kfz_log", "q_pkw_log", "q_lkw_log"),
                  weather = ifelse(covs == paste(w0, collapse = "+"), "nocovs",
                                   ifelse(covs == paste0(w1, collapse = "+"), "weather",
                                          ifelse(covs == paste0(w2, collapse = "+"), "weather2",
                                                 ifelse(covs == paste0(w3, collapse = "+"), "weather5", covs)))),
                  weather = fct_relevel(weather, "nocovs", "weather", "weather2", "weather5"),
                  fes = fct_relevel(fes, c(paste(fes0, collapse = "+"), paste(fes1, collapse = "+"), paste(fes2, collapse = "+"))))

ggplot(filter(lockdown, indep == "post")) + 
  geom_point(aes(x = weather, y = est, color = fes), 
             position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(ymax = est + se*1.96, ymin = est - se*1.96, x = weather, color = fes), width = .2, 
                position = position_dodge(width=0.5)) +
  facet_wrap(clustervar ~ depvar, scales = "free_y", ncol = length(unique(res$depvar))) +
  scale_color_viridis(discrete = T) + 
  theme(panel.background = element_rect(fill = "white", color = "white"), 
        text = element_text(size = 10), 
        axis.line = element_line(), 
        strip.text = element_text(hjust = 0)) +
  ggpubr::grids() + 
  geom_hline(yintercept = 0, color = "black", linetype = "dashed") +
  theme(legend.position="bottom") +
  guides(color=guide_legend(nrow=1))
ggsave(paste0("04_figures/twfe_monthly_bancities_treat25_lockdown.png"), width = 12, height = 6)

#### Tables ####
## Levels ####
d = lockdown %>% 
  filter(depvar %in% c("q_kfz", "q_pkw", "q_lkw"), 
         indep == "post", clustervar == "id") %>% 
  arrange(depvar) %>% 
  filter((weather == "nocovs" & fes == paste(fes2, collapse = "+")) |
           (weather == "weather2" & fes == c(paste(fes2, collapse = "+"))) |
           (weather == "weather5" & fes == c(paste(fes2, collapse = "+")))) %>%
  group_split(depvar, weather, fes) 

latex = lapply(d, function(x) createTexreg("Post", x$est, x$se, x$pval, 
                                           gof.names = c(rep("Nobs", nrow(x)),
                                                         rep("N", nrow(x)),
                                                         rep("Adj.R2", nrow(x))),
                                           gof = c(x$nobs, 
                                                   x$N,
                                                   x$rsq),
                                           gof.decimal = c(rep(F, nrow(x)),
                                                           rep(F, nrow(x)),
                                                           rep(T, nrow(x)))))
latex
texreg(latex, 
       digits = 1, booktabs = TRUE, use.packages = F,
       custom.model.names = paste0("(", c(rep(1:3, times = 3)), ")"),
       caption = "Impact of diesel bans on traffic intensity (w.o. Covid)",
       caption.above = TRUE, float.pos = "h", fontsize = "footnotesize",
       custom.note = "\\item Treated monitors within 25m distance to a diesel ban. 
       Control monitors in cities with diesel ban further away than 1000m from the ban. 
       Standard errors clustered at the monitor level.
       Significance levels %stars.",
       threeparttable = TRUE,
       file = paste0("05_tables/twfe_monthly_bancities_treat25_b1000_lockdown.tex"))

## Add model details in separate rows
s = readLines("05_tables/twfe_monthly_bancities_treat25_b1000_lockdown.tex")

s = c(s[1:8],
      "& \\multicolumn{3}{c}{KFZ} & \\multicolumn{3}{c}{PKW}  & \\multicolumn{3}{c}{LKW} \\\\ \\cmidrule(r){2-4} \\cmidrule(r){5-7} \\cmidrule(r){8-10}",
      s[9:length(s)])
s = c(s[1:14],
      "Monitor FEs   & Yes & Yes & Yes  & Yes & Yes & Yes & Yes & Yes & Yes    \\\\",
      "Year-month FEs     & Yes & Yes & Yes  & Yes & Yes & Yes & Yes & Yes & Yes  \\\\",
      "Weather     & No & Quadr. & Quint.  & No & Quadr. & Quint. & No & Quadr. & Quint.   \\\\ \\midrule",
      s[15:length(s)])
s
writeLines(s, "05_tables/twfe_monthly_bancities_treat25_b1000_lockdown.tex")

## Logs ####
d = lockdown %>% 
  filter(depvar %in% c("q_kfz_log", "q_pkw_log", "q_lkw_log"), 
    indep == "post", clustervar == "id") %>% 
  arrange(depvar) %>% 
  filter((weather == "nocovs" & fes == paste(fes2, collapse = "+")) |
           (weather == "weather2" & fes == c(paste(fes2, collapse = "+"))) |
           (weather == "weather5" & fes == c(paste(fes2, collapse = "+")))) %>%
  group_split(depvar, weather, fes) 

latex = lapply(d, function(x) createTexreg("Post", x$est, x$se, x$pval, 
                                           gof.names = c(rep("Nobs", nrow(x)),
                                                         rep("N", nrow(x)),
                                                         rep("Adj.R2", nrow(x))),
                                           gof = c(x$nobs, 
                                                   x$N,
                                                   x$rsq),
                                           gof.decimal = c(rep(F, nrow(x)),
                                                           rep(F, nrow(x)),
                                                           rep(T, nrow(x)))))
latex
texreg(latex, 
       digits = 3, booktabs = TRUE, use.packages = F,
       custom.model.names = paste0("(", c(rep(1:3, times = 3)), ")"),
       caption = "Impact of diesel bans on traffic intensity (w.o. Covid, log)",
       caption.above = TRUE, float.pos = "h", fontsize = "footnotesize",
       custom.note = "\\item Treated monitors within 25m distance to a diesel ban. 
       Control monitors in cities with diesel ban further away than 1000m from the ban. 
       Standard errors clustered at the monitor level.
       Significance levels %stars.",
       threeparttable = TRUE,
       file = paste0("05_tables/twfe_monthlylog_bancities_treat25_b1000_lockdown.tex"))

## Add model details in separate rows
s = readLines("05_tables/twfe_monthlylog_bancities_treat25_b1000_lockdown.tex")

s = c(s[1:8],
      "& \\multicolumn{3}{c}{Log KFZ } & \\multicolumn{3}{c}{Log PKW }  & \\multicolumn{3}{c}{Log LKW } \\\\ \\cmidrule(r){2-4} \\cmidrule(r){5-7} \\cmidrule(r){8-10}",
      s[9:length(s)])
s = c(s[1:14],
      "Monitor FEs   & Yes & Yes & Yes  & Yes & Yes & Yes & Yes & Yes & Yes    \\\\",
      "Year-month FEs     & Yes & Yes & Yes  & Yes & Yes & Yes & Yes & Yes & Yes  \\\\",
      "Weather     & No & Quadr. & Quint.  & No & Quadr. & Quint. & No & Quadr. & Quint.   \\\\ \\midrule",
      s[15:length(s)])
s
writeLines(s, "05_tables/twfe_monthlylog_bancities_treat25_b1000_lockdown.tex")



#### Pre-trends for sample without lockdown ####
#### ...................................... ####

data_pt <- filter(data, treatgrp25 == 1 | ban_dist > 1000) 

#### KFZ ####
pt_kfz_1 <- feols(fml = as.formula("q_kfz ~ i(score, treatgrp25, ref = -1) + 
                              precipitation + temperature + sunshine + wind_speed + humidity +
                              precipitation_2 + temperature_2 + sunshine_2 + wind_speed_2 + humidity_2 | 
                              id + year_month"), 
                  data = data_pt, cluster = "id")
iplot(pt_kfz_1)

pt_kfzlog_1 <- feols(fml = as.formula("q_kfz_log ~ i(score, treatgrp25, ref = -1) + 
                              precipitation + temperature + sunshine + wind_speed + humidity +
                              precipitation_2 + temperature_2 + sunshine_2 + wind_speed_2 + humidity_2 | 
                              id + year_month"), 
                     data = data_pt, cluster = "id")
iplot(pt_kfzlog_1)


#### PKW ####
pt_pkw_1 <- feols(fml = as.formula("q_pkw ~ i(score, treatgrp25, ref = -1) + 
                              precipitation + temperature + sunshine + wind_speed + humidity +
                              precipitation_2 + temperature_2 + sunshine_2 + wind_speed_2 + humidity_2 | 
                              id + year_month"), 
                  data = data_pt, cluster = "id")
iplot(pt_pkw_1)

pt_pkwlog_1 <- feols(fml = as.formula("q_pkw_log ~ i(score, treatgrp25, ref = -1) + 
                              precipitation + temperature + sunshine + wind_speed + humidity +
                              precipitation_2 + temperature_2 + sunshine_2 + wind_speed_2 + humidity_2 | 
                              id + year_month"), 
                     data = data_pt, cluster = "id")
iplot(pt_pkwlog_1)

#### LKW ####
pt_lkw_1 <- feols(fml = as.formula("q_lkw ~ i(score, treatgrp25, ref = -1) + 
                              precipitation + temperature + sunshine + wind_speed + humidity +
                              precipitation_2 + temperature_2 + sunshine_2 + wind_speed_2 + humidity_2 | 
                              id + year_month"), 
                  data = data_pt, cluster = "id")
iplot(pt_lkw_1)

pt_lkwlog_1 <- feols(fml = as.formula("q_lkw_log ~ i(score, treatgrp25, ref = -1) + 
                              precipitation + temperature + sunshine + wind_speed + humidity +
                              precipitation_2 + temperature_2 + sunshine_2 + wind_speed_2 + humidity_2 | 
                              id + year_month"), 
                     data = data_pt, cluster = "id")
iplot(pt_lkwlog_1)


#### Plots ####
## Levels ####
ggiplot(list("KFZ" = pt_kfz_1, "PKW" = pt_pkw_1, "LKW" = pt_lkw_1),
        ref.line = -1,
        xlab = 'Time to treatment',
        multi_style = 'facet',
        geom_style = 'ribbon',
        facet_args = list(labeller = labeller(id = \(x) gsub(".*: ", "", x))),
        theme = theme_minimal() +
          theme(
            plot.title = element_text(hjust = 0.5),
            legend.position = 'none'
          )
)
ggsave("04_figures/twfe_monthly_pt_lockdown.png", width = 10, height = 5)

## Logs ####
ggiplot(list("Log KFZ " = pt_kfzlog_1, "Log PKW " = pt_pkwlog_1, "Log LKW " = pt_lkwlog_1),
        ref.line = -1,
        xlab = 'Time to treatment',
        multi_style = 'facet',
        geom_style = 'ribbon',
        facet_args = list(labeller = labeller(id = \(x) gsub(".*: ", "", x))),
        theme = theme_minimal() +
          theme(
            plot.title = element_text(hjust = 0.5),
            legend.position = 'none'
          )
)
ggsave("04_figures/twfe_monthly_pt_log_lockdown.png", width = 10, height = 5)
