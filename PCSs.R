# ============================================================
# title: PCSs definition and result analysis ####
# 
# This script:
#   - Reads WPP life tables (male & female)
#   - Interpolates mx (mortality rates) over years and countries
#   - Links mortality with WCDE educational attainment & UNESCO data
#   - Defines edu-specific PCSs via SVD
# ============================================================


## libraries ----
library(readxl)     # read Excel files
library(openxlsx)   # write Excel files
library(zoo)        # interpolation and LOCF
library(tidyverse)  # tidy data wrangling
library(wcde)       # World Countries Database on Education

# there forsees the download of life tables from the WPP.


# --- Load WPP female and male abridged life tables ----
WPP.data.F <- read_excel(
  './data/WPP2019/WPP2019_MORT_F17_3_ABRIDGED_LIFE_TABLE_FEMALE.xlsx', 
  skip = 16
) %>% 
  mutate(sex = 'F')

WPP.data.M <- read_excel(
  './data/WPP2019/WPP2019_MORT_F17_2_ABRIDGED_LIFE_TABLE_MALE.xlsx', 
  skip = 16
) %>% 
  mutate(sex = 'M')

# Combine
WPP.data <- rbind(WPP.data.F, WPP.data.M)


# --- Age recoding (collapse 85+) ----
age_rec <- tibble(
  age = seq(15, 100, by = 5), 
  x   = c(seq(15, 80, by = 5), rep(85, 4))
)


# --- Clean and reshape WPP data ----
df.to.interpol <- WPP.data %>% 
  tibble() %>% 
  filter(!"Region, subregion, country or area *" %in% c(
    'UN development groups',
    'World Bank income groups',
    'Geographic regions',
    'Sustainable Development Goal (SDG) regions'
  )) %>% 
  select(
    'Age (x)',
    "Region, subregion, country or area *",
    "Country code",
    'Number of deaths d(x,n)',
    "Central death rate m(x,n)",
    'Number of survivors l(x)',
    'Person-years lived T(x)',
    'Period', sex
  ) %>% 
  rename(
    country     = `Region, subregion, country or area *`,
    country.code = `Country code`,
    age         = `Age (x)`
  ) %>% 
  filter(age < 90) %>% 
  mutate(
    LX = as.numeric(`Number of survivors l(x)`),
    TX = as.numeric(`Person-years lived T(x)`),
    m.x = ifelse(age == 85, LX / TX, as.numeric(`Central death rate m(x,n)`)), 
    period = str_sub(Period, start = 1 , end = 4)
  ) %>% 
  filter(!is.na(m.x)) %>% 
  select(period, country, country.code, m.x, age, sex)

# Quick checks
df.to.interpol %>% distinct(age) %>% pull()
df.to.interpol %>% distinct(period)

# Add missing years (1950–2015)
df.to.interpol.new <- merge(
  data.frame(period = seq(1950,2015)),
  df.to.interpol, all = TRUE
) %>% tibble()

df.to.interpol.new %>% View()


# --- Interpolation over country-age-sex groups ----
splitted <- split(df.to.interpol, list(
  df.to.interpol$country.code,
  df.to.interpol$age,
  df.to.interpol$sex
))

df.WPP.interpolated <- do.call("rbind", lapply(splitted, function(df_subset) {
  df_subset %>% 
    merge(data.frame(period=seq(1950,2015)), all=TRUE) %>% 
    tibble() %>% 
    mutate(
      mx_interpol = na.approx(m.x),     # interpolate mx
      country     = na.locf(country),   # fill forward categorical values
      age         = na.locf(age),
      sex         = na.locf(sex),
      country.code = na.locf(country.code)
    ) %>% 
    select(-c(m.x))
}))


# until here we deined the data which we needed in terms of life tables, 
# now we work on the edu specific PCSs 
# here we need info in terms of geographical data of the countries and of the regions  


# --- Geographic reference data (ISO country codes) ----
urlfile <- "https://raw.githubusercontent.com/lukes/ISO-3166-Countries-with-Regional-Codes/master/all/all.csv"

geog_data <- read_csv(url(urlfile)) %>% 
  rename(
    sub_region      = `sub-region`,
    region_code     = 'region-code',
    sub_region_code = `sub-region-code`
  )

# Cluster of interest
regions_cluster1 <- c("Southern Europe", "Northern Africa", "Western Asia", "Eastern Europe")

countries_regions <- geog_data %>% 
  filter(sub_region %in% c(regions_cluster1)) %>% 
  distinct(name) %>% 
  pull()


# --- WCDE data (bmys = mean years of schooling) ----
bmys <- wcde::get_wcde(
  indicator = "bmys",
  scenario = 2,
  include_scenario_names = FALSE,
  version =  "wcde-v2"
)

bmys_15plus <- bmys %>% 
  mutate(
    name = recode(
      name, 
      "The former Yugoslav Republic of Macedonia" = "North Macedonia",
      "Occupied Palestinian Territory" = "Palestine, State of"
    )
  ) %>% 
  filter(age == "15+") %>% 
  filter(name %in% countries_regions) %>% 
  select(-c(scenario, country_code, age))


# these are data coming from the UNESCO which is giving us the info 
# for the duration of the primary education in terms of years. 
# The dataset is contained in the folder "data".  

duration_primary <- read.csv('./data/durantion_primary.csv') %>% 
  tibble() %>% 
  mutate(Country = recode(Country, "Palestine" = "Palestine, State of")) %>% 
  filter(Country %in% countries_regions)

colnames(duration_primary) <- c(
  'country',
  str_sub(colnames(duration_primary)[2:length(colnames(duration_primary))], start = 2)
)

duration_primary.gathered <- duration_primary %>% 
  filter(!country %in% c('Western Sahara', 'Sudan')) %>% 
  gather(year, primary.duration, -country)


# --- Interpolate UNESCO duration data ----
split.duration.primary <- split(duration_primary.gathered, list(duration_primary.gathered$country))

duration.primary.interpolated <- do.call("rbind", lapply(split.duration.primary, function(df_subset) {
  df_subset %>% 
    merge(data.frame(year=seq(1970,2020)), all=TRUE) %>% 
    tibble() %>% 
    mutate(
      country         = na.locf(country), 
      primary.duration = na.locf(primary.duration)
    )
}))


# --- Merge WCDE (bmys) with UNESCO (duration primary) ----
bmys_edu.groups <- bmys_15plus %>% 
  mutate(year = as.character(year)) %>% 
  inner_join(duration.primary.interpolated, by = c('name' = 'country', 'year')) %>% 
  filter(year > 1979, sex == 'Female') %>% 
  # in.which.group splits countries depending on whether mean years > primary duration
  mutate(in.which.group = ifelse(bmys > primary.duration, "midHigh", 'lower')) %>% 
  ungroup() %>% 
  select(name, year, in.which.group)


# --- Merge mortality with education group info ----
data.by.edu.att <- df.WPP.interpolated %>% 
  mutate(country = recode(country, "State of Palestine" = "Palestine, State of")) %>% 
  filter(
    country %in% countries_regions,
    period %in% seq(1980,2020),
    sex == 'F',
    age > 10
  ) %>%
  inner_join(
    bmys_edu.groups %>% mutate(year = as.double(year)), 
    by = c('country'='name', 'period' = 'year')
  ) %>% 
  select(-c(country.code, sex)) %>% 
  spread(age, mx_interpol) %>% 
  select(-c(period, country))


# --- Construct mortality matrices by education group ----
m.ga.lower <- data.by.edu.att %>% 
  filter(in.which.group == 'lower') %>% 
  select(-c(in.which.group)) %>% 
  as.matrix()

m.ga.midHigh <- data.by.edu.att %>% 
  filter(in.which.group == 'midHigh') %>% 
  select(-c(in.which.group)) %>% 
  as.matrix()


# --- Apply log transform and compute PCSs ----
log_m.ga.lower   <- log(m.ga.lower)
log_m.ga.midHigh <- log(m.ga.midHigh)

pcs.lower   <- svd(log_m.ga.lower)$v[, 1:3]
pcs.midHigh <- svd(log_m.ga.midHigh)$v[, 1:3]

# if we want to save them   
# write.csv(pcs.lower,   file = "./data/pcs_lower_cluster1_Female.csv")
# write.csv(pcs.midHigh, file = "./data/pcs.midHigh_cluster1_Female.csv")
