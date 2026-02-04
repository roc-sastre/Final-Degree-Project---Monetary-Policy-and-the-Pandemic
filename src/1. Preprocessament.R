# Paquets ----------------------------------------------------------------------

install.packages(c("fredr", "eurostat", "ecb", "dplyr", "tidyr", "lubridate"))
library(fredr)
library(eurostat)
library(ecb)
library(dplyr)
library(tidyr)
library(lubridate)
library(zoo)
library(readxl)
library(tempdisagg)

# ------------------------------------------------------------------------------
# Setup ------------------------------------------------------------------------

fredr_set_key("58e782b2afa6ed9b66ec749d3e91558f")
start_date <- as.Date("2014-01-01")
path <- "C:/Users/rocsa/Desktop/Universitat/TFG/Data"
setwd(path)

# ------------------------------------------------------------------------------
# Obtenció de les dades --------------------------------------------------------

# ---------------------
# 🇺🇸 US DATA
# ---------------------

# 1. Index of industrial production (mothly, index 2017 = 100)
us_output <- fredr(series_id = "INDPRO", observation_start = start_date) %>%
  select(date, value) %>%
  rename(us_output = value) %>%
  mutate(month = as.yearmon(date)) %>%
  select(month, us_output)

# 2. CPI (monthly, index 1982-1984 = 100)
us_prices <- fredr(series_id = "CPIAUCSL", observation_start = start_date) %>%
  select(date, value) %>%
  rename(us_prices = value) %>%
  mutate(month = as.yearmon(date)) %>%
  select(month, us_prices)

# 3. Fed Funds Rate
us_rate <- fredr(series_id = "FEDFUNDS", observation_start = start_date) %>%
  select(date, value) %>%
  rename(fed_funds = value) %>%
  mutate(month = as.yearmon(date)) %>%
  select(month, fed_funds)

# ---------------------
# 🇪🇺 EU DATA
# ---------------------

# 1. Index of industrial production (monthly, index 2021 = 100)
eu_output <- get_eurostat("sts_inpr_m", time_format = "date") %>%
  filter(
    geo == "EU27_2020",
    nace_r2 == "B-D",
    unit == "I21",
    s_adj == "SCA",
    TIME_PERIOD >= start_date
  ) %>%
  select(TIME_PERIOD, values) %>%
  rename(date = TIME_PERIOD, eu_output = values) %>%
  mutate(month = as.yearmon(date)) %>%
  select(month, eu_output)

# 2. HICP (monthly, index 2015 = 100)
eu_prices <- get_eurostat("prc_hicp_midx", time_format = "date") %>%
  filter(
    geo == "EU27_2020",
    coicop == "CP00",
    unit == "I15",
    TIME_PERIOD >= start_date) %>%
  select(TIME_PERIOD, values) %>%
  rename(eu_prices = values, date = TIME_PERIOD) %>%
  mutate(month = as.yearmon(date)) %>%
  select(month, eu_prices)

# 3. EONIA and €STR
# EONIA: ECB ID = "EON.D.EONIA_TO.RATE"
eonia <- get_data("EON.D.EONIA_TO.RATE") %>%
  select(obstime, obsvalue) %>%
  rename(date = obstime, eonia = obsvalue) %>%
  filter(date >= start_date)

eonia$date <- as.Date(eonia$date)

eonia <- eonia %>%
  mutate(month = as.yearmon(date)) %>%
  group_by(month) %>%
  summarise(eonia = mean(eonia, na.rm = TRUE)) %>%
  ungroup()

# €STR: ECB ID = "EST.B.EU000A2X2A25.WT"
estr <- get_data("EST.B.EU000A2X2A25.WT") %>%
  select(obstime, obsvalue) %>%
  rename(date = obstime, estr = obsvalue) %>%
  filter(date >= start_date)

estr$date <- as.Date(estr$date)

estr <- estr %>%
  mutate(month = as.yearmon(date)) %>%
  group_by(month) %>%
  summarise(estr = mean(estr, na.rm = TRUE)) %>%
  ungroup()

# ------------------------------------------------------------------------------
# Annex: Aproximació al nivell potencial d'output ------------------------------

# Definim la data d'inici per la tendència lineal
trend_start_date <- as.Date("1999-01-01")

# EUA: calcula la tendència lineal i desa com a us_p_output
us_trend_data <- us_output %>%
  filter(month >= as.yearmon(trend_start_date)) %>%
  mutate(time_index = as.numeric(month - min(month)))

us_trend_model <- lm(us_output ~ time_index, data = us_trend_data)

us_output <- us_output %>%
  filter(month >= as.yearmon(start_date)) %>%
  mutate(time_index = as.numeric(month - min(us_trend_data$month))) %>%
  mutate(us_p_output = predict(us_trend_model, newdata = .)) %>%
  select(month, us_output, us_p_output)

# UE: calcula la tendència lineal i desa com a eu_p_output
eu_trend_data <- eu_output %>%
  filter(month >= as.yearmon(trend_start_date)) %>%
  mutate(time_index = as.numeric(month - min(month)))

eu_trend_model <- lm(eu_output ~ time_index, data = eu_trend_data)

eu_output <- eu_output %>%
  filter(month >= as.yearmon(start_date)) %>%
  mutate(time_index = as.numeric(month - min(eu_trend_data$month))) %>%
  mutate(eu_p_output = predict(eu_trend_model, newdata = .)) %>%
  select(month, eu_output, eu_p_output)

rm(eu_trend_data, eu_trend_model, us_trend_data, us_trend_model)

save(eonia, estr, eu_prices, eu_output, us_prices, us_output,
     us_rate, path, file = "1.RData")

# ------------------------------------------------------------------------------