# ============================================================
# Title: INPUT file for model definition
#
# Purpose:
#   In this file we report the steps to define the new input file
#   using the new approach for total mortality splitting.
#
# Workflow:
#   1. Libraries
#   2. Starting point: log-linear vs DHS data
#   3. Reconstruction lines
#   4. Period table
#   5. Mortality rates by education (age 15–19)
#   6. Starting value correction (with WPP interpolated data)
#   7. Reconstruct country/period-specific mortality lines
#   8. Build base dataset structure
#   9. Construct full dataset d
# ============================================================


# ------------------------------------------------------------
# 1. Libraries ####
# ------------------------------------------------------------
library(tidyverse)
library(latex2exp)


# ------------------------------------------------------------
# 2. Starting point ####
# ------------------------------------------------------------
# Analyse values coming from log-linear modelling and DHS.
# We need to decide between them.

log_lin_results <- read.csv('./data/log_lin_results.csv') %>% tibble()
log_lin_results %>% head()

# Education-differentiated mortality (age 15–19) from DHS (mother’s education split)
mx.15.DHS.split.M <- read.csv('./data/mx15DHSsplit_RECODED_Male.csv') %>%
  tibble() %>%
  mutate(log.mx.15 = log(mx.15), Sex = 'M')

mx.15.DHS.split.F <- read.csv('./data/mx15DHSsplit_RECODED_Female.csv') %>%
  tibble() %>%
  mutate(log.mx.15 = log(mx.15), Sex = 'F')

mort_rates_byEdu_age1519_splitted <- rbind(mx.15.DHS.split.M, mx.15.DHS.split.F)

# Merge DHS split with log-linear results
mort_rates_byEdu_age1519_splitted_log.lin <-
  mort_rates_byEdu_age1519_splitted %>%
  right_join(
    log_lin_results %>% mutate(log.mx.15.log.lin = mu),
    by = c('country' = 'geo', 'Sex' = 'sex', 'edu.att', 'year')
  ) %>%
  mutate(mx.15 = ifelse(is.na(mx.15), exp(mu), mx.15))


# ------------------------------------------------------------
# 3. Reconstruction lines ####
# ------------------------------------------------------------
# Use results of splitting from UN + WiC data.
# Here we have population by edu.att and total log mortality.

load('./data/tot_LogMx_splitting.RData')

mort_rates_byEdu_age1519_splitted_log.lin %>% colnames()
# "year", "country", "edu.att", "Sex", "age.group",
# "pop.size", "region", "log.mx.15.log.lin"

mort_rates_byEdu_age1519_splitted_log.lin %>% distinct(year) %>% pull()


# ------------------------------------------------------------
# 4. Period table ####
# ------------------------------------------------------------
period.tbbl <- tibble(
  year   = seq(1975, 2015),
  period = c(rep(seq(1975, 2010, by = 5), each = 5), 2015)
)


# ------------------------------------------------------------
# 5. Mortality rates by education, age 15–19 ####
# ------------------------------------------------------------
mort_rates_byEdu_age1519 <-
  mort_rates_byEdu_age1519_splitted_log.lin %>%
  left_join(period.tbbl, by = 'year') %>%
  rename(sex = Sex) %>%
  select(year, country, edu.att, sex, age.group,
         pop.size, region, log.mx.15.log.lin) %>%
  mutate(
    edu.att   = as.factor(edu.att),
    age.group = as.factor(age.group)
  ) %>%
  mutate(
    edu.att = recode(edu.att,
                     'noEdu.primary'    = 'no.edu.primary',
                     'secondary.higher' = 'more.than.primary'),
    mx.15.log.lin = exp(log.mx.15.log.lin),
    country       = recode(country,
                           'Palestine, State of' = 'State of Palestine')
  )


# ------------------------------------------------------------
# 6. Starting value correction (with WPP interpolated) ####
# ------------------------------------------------------------
myspread <- function(df, key, value) {
  # Spread helper
  keyq   <- rlang::enquo(key)
  valueq <- rlang::enquo(value)
  s      <- rlang::quos(!!valueq)
  df %>% gather(variable, value, !!!s) %>%
    unite(temp, !!keyq, variable) %>%
    spread(temp, value)
}

df.WPP.interpolated <- read.csv('./data/df_WPP_interpolated.csv') %>%
  tibble() %>%
  mutate(log_mx_interpol = log(mx_interpol))

# Check proportions and logMx against WPP interpolated
mort_rates_byEdu_age1519 %>%
  select(-c(region, mx.15.log.lin)) %>%
  rename(logMx15LL = log.mx.15.log.lin) %>%
  myspread(edu.att, c(pop.size, logMx15LL)) %>%
  left_join(
    df.WPP.interpolated %>%
      filter(age == 15) %>%
      select(-c(country.code, mx_interpol, age)),
    by = c(year = 'period', 'country', 'sex')
  ) %>%
  mutate(
    pop.prop.low     = no.edu.primary_pop.size /
      (no.edu.primary_pop.size + more.than.primary_pop.size),
    pop.prop.midhigh = more.than.primary_pop.size /
      (no.edu.primary_pop.size + more.than.primary_pop.size),
    L_H              = no.edu.primary_logMx15LL / more.than.primary_logMx15LL,
    more.than.primary = log_mx_interpol / (pop.prop.low * L_H + pop.prop.midhigh),
    no.edu.primary    = L_H * more.than.primary
  ) %>%
  select(year, country, sex, age.group,
         log_mx_interpol, more.than.primary, no.edu.primary) %>%
  rename(tot.UN = log_mx_interpol) %>%
  gather(key = edu.att, value = log.mx, -c(year, country, sex, age.group)) %>%
  ggplot(aes(x = year, y = log.mx, color = edu.att, linetype = sex)) +
  geom_line(aes(group = interaction(country, sex, edu.att))) +
  facet_grid(sex ~ country)

# Country/year coverage check
co <- mort_rates_byEdu_age1519 %>% distinct(country) %>% pull()
ye <- mort_rates_byEdu_age1519 %>% distinct(year) %>% pull()

df.WPP.interpolated %>%
  filter(country %in% co, period %in% ye, age == 15) %>%
  ggplot(aes(x = period, y = log_mx_interpol, color = sex)) +
  geom_line(aes(group = interaction(country, sex))) +
  facet_grid(sex ~ country)


# --- Corrected version ----
mort_rates_byEdu_age1519.corrected <-
  mort_rates_byEdu_age1519 %>%
  select(-c(region, mx.15.log.lin)) %>%
  rename(logMx15LL = log.mx.15.log.lin) %>%
  myspread(edu.att, c(pop.size, logMx15LL)) %>%
  left_join(
    df.WPP.interpolated %>%
      filter(age == 15) %>%
      mutate(log_mx_interpol = log(mx_interpol)) %>%
      select(-c(country.code, mx_interpol, age)),
    by = c(year = 'period', 'country', 'sex')
  ) %>%
  mutate(
    pop.prop.low     = no.edu.primary_pop.size /
      (no.edu.primary_pop.size + more.than.primary_pop.size),
    pop.prop.midhigh = more.than.primary_pop.size /
      (no.edu.primary_pop.size + more.than.primary_pop.size),
    L_H              = no.edu.primary_logMx15LL / more.than.primary_logMx15LL,
    more.than.primary = log_mx_interpol / (pop.prop.low * L_H + pop.prop.midhigh),
    no.edu.primary    = L_H * more.than.primary
  ) %>%
  select(year, country, sex, age.group,
         log_mx_interpol, more.than.primary, no.edu.primary) %>%
  rename(tot.UN = log_mx_interpol) %>%
  gather(key = edu.att, value = log.mx, -c(year, country, sex, age.group)) %>%
  mutate(mx.15 = exp(log.mx))


# ------------------------------------------------------------
# 7. Reconstruct country/period-specific mortality lines ####
# ------------------------------------------------------------
tot.LogMx.splitting %>% distinct(edu.att) %>% pull()

MX.reconLogReturns.SPLIT <- c()

for (y in unique(mort_rates_byEdu_age1519.corrected$year)) {
  for (cou in unique(mort_rates_byEdu_age1519.corrected %>%
                     filter(year == y) %>% pull(country))) {
    for (Edu.att in c("no.edu.primary", "more.than.primary")) {
      
      per <- period.tbbl[period.tbbl$year == y,]$period
      
      # --- Female ---
      df_F <- data.frame(
        log = tot.LogMx.splitting %>%
          filter(country == cou, period == per,
                 edu.att == Edu.att, sex == 'F') %>%
          pull()
      )
      df_F$logr      <- c(NA, diff(df_F$log))
      df_F$logr_na0  <- ifelse(is.na(df_F$logr), 0, df_F$logr)
      df_F$cuml_log  <- cumsum(df_F$logr_na0)
      df_F$reconstructed_price_norm <- exp(df_F$cuml_log)
      initial_price  <- mort_rates_byEdu_age1519.corrected %>%
        filter(edu.att == Edu.att, year == y, sex == 'F', country == cou) %>%
        pull(mx.15)
      df_F$Log_recon.mx <- log(initial_price * df_F$reconstructed_price_norm)
      df_F$edu.att <- Edu.att
      df_F$time    <- y
      df_F$sex     <- "F"
      df_F$geo     <- cou
      df_F$age     <- seq(15, 85, by = 5)
      df_F$age_groups <- factor(c(paste(seq(15, 80, by=5),
                                        seq(19, 84, by=5), sep = "-"), '85+'),
                                levels = c(paste(seq(15, 80, by=5),
                                                 seq(19, 84, by=5), sep = "-"),
                                           '85+'))
      
      # --- Male ---
      df_M <- data.frame(
        log = tot.LogMx.splitting %>%
          filter(country == cou, period == per,
                 edu.att == Edu.att, sex == 'M') %>%
          pull()
      )
      df_M$logr      <- c(NA, diff(df_M$log))
      df_M$logr_na0  <- ifelse(is.na(df_M$logr), 0, df_M$logr)
      df_M$cuml_log  <- cumsum(df_M$logr_na0)
      df_M$reconstructed_price_norm <- exp(df_M$cuml_log)
      initial_price  <- mort_rates_byEdu_age1519.corrected %>%
        filter(edu.att == Edu.att, year == y, sex == 'M', country == cou) %>%
        pull(mx.15)
      df_M$Log_recon.mx <- log(initial_price * df_M$reconstructed_price_norm)
      df_M$edu.att <- Edu.att
      df_M$time    <- y
      df_M$sex     <- "M"
      df_M$geo     <- cou
      df_M$age     <- seq(15, 85, by = 5)
      df_M$age_groups <- factor(c(paste(seq(15, 80, by=5),
                                        seq(19, 84, by=5), sep = "-"), '85+'),
                                levels = c(paste(seq(15, 80, by=5),
                                                 seq(19, 84, by=5), sep = "-"),
                                           '85+'))
      
      # Append
      MX.reconLogReturns.SPLIT <- rbind(MX.reconLogReturns.SPLIT, df_F, df_M)
    }
  }
}

reconstruction.results.DHS.cou.and.period.spec.recon.lines.clust1 <-
  MX.reconLogReturns.SPLIT %>%
  tibble() %>%
  select(time, sex, geo, edu.att, age_groups, Log_recon.mx) %>%
  mutate(
    log.mx = Log_recon.mx,
    mx     = exp(Log_recon.mx),
    x      = recode(age_groups,
                    "15-19" = 15, "20-24" = 20, "25-29" = 25, "30-34" = 30,
                    "35-39" = 35, "40-44" = 40, "45-49" = 45, "50-54" = 50,
                    "55-59" = 55, "60-64" = 60, "65-69" = 65, "70-74" = 70,
                    "75-79" = 75, "80-84" = 80, "85+" = 85)
  ) %>%
  select(-Log_recon.mx)

reconstruction.results.DHS.cou.and.period.spec.recon.lines.clust1


# ------------------------------------------------------------
# 8. Build base dataset structure ####
# ------------------------------------------------------------
urlfile <- "https://raw.githubusercontent.com/lukes/ISO-3166-Countries-with-Regional-Codes/master/all/all.csv"
geog_data <- read_csv(url(urlfile))

res.first.clustering <- read.csv('./data/res_first_clustering.csv')

years <- reconstruction.results.DHS.cou.and.period.spec.recon.lines.clust1 %>%
  distinct(time) %>% pull()

countries <- res.first.clustering %>%
  tibble() %>%
  left_join(geog_data %>% select(name, `alpha-3`), by = c('name'='alpha-3')) %>%
  filter(value == 1) %>%
  select(name.y) %>%
  mutate(name.y = recode(name.y,
                         'Palestine, State of' = 'State of Palestine')) %>%
  pull()

education <- c("no.edu.primary","more.than.primary")

age_g <- reconstruction.results.DHS.cou.and.period.spec.recon.lines.clust1 %>%
  distinct(age_groups)

x <- seq(15, 85, by = 5)

# base tibble
a <- tibble(
  age    = rep(pull(age_g), 4),
  edu.att = rep(rep(education, length(pull(age_g)), each = 2)),
  sex     = rep(rep(c('F','M'), each = length(pull(age_g))), 2)
)

b <- do.call("rbind", replicate(length(years), a, simplify = FALSE)) %>%
  cbind(year = rep(years, each = 60)) %>% tibble()

d_basis <- do.call("rbind", replicate(length(countries), b, simplify = FALSE)) %>%
  cbind(country = rep(countries, each = 2280)) %>% tibble() %>%
  mutate(m.x = NA)


# ------------------------------------------------------------
# 9. Construct full dataset d ####
# ------------------------------------------------------------
WiC.data.interpolated <- read.csv('./data/WiC.data.interpolated.csv') %>%
  tibble() %>%
  mutate(edu.att = recode(edu.att,
                          'noEdu.primary'    = 'no.edu.primary',
                          'secondary.higher' = 'more.than.primary')) %>%
  mutate(name = recode(name,
                       'Palestine, State of' = 'State of Palestine'))

d <-
  d_basis %>%
  left_join(
    reconstruction.results.DHS.cou.and.period.spec.recon.lines.clust1 %>%
      select(geo, sex, edu.att, age_groups, time, mx),
    by = c('age'='age_groups','edu.att','sex','year'='time','country'='geo')
  ) %>%
  mutate(mx = ifelse(mx > 0.99999, NA, mx)) %>%
  select(-m.x) %>%
  mutate(x = as.double(str_sub(age, start = 1, end = 2))) %>%
  left_join(WiC.data.interpolated,
            by = c('country'='name','year','x','edu.att','sex'='Sex')) %>%
  left_join(
    df.WPP.interpolated %>%
      mutate(year = as.double(period)) %>%
      select(-c(country.code, period)),
    by = c('year','country','x'='age','sex')
  ) %>%
  group_by(country, sex, age, year, x) %>%
  mutate(
    pop.tot   = sum(pop.size),
    deaths.tot = ceiling(mx_interpol * pop.tot)
  ) %>%
  ungroup() %>%
  mutate(deaths.new = ceiling(mx * pop.size)) %>%
  select(country, sex, edu.att, age, year,
         deaths.new, pop.size, x, mx, mx_interpol, pop.tot, deaths.tot)

colnames(d) <- c('geo','sex','edu.att','age.group','year','deaths.new',
                 'pop.size','age','mx','mx.tot','pop.tot','deaths.tot')

d <- d %>%
  group_by(age.group, year, sex, geo) %>%
  mutate(deaths_edu_prop = deaths.new/sum(deaths.new)) %>%
  mutate(
    deaths.new  = ceiling(deaths.tot * deaths_edu_prop),
    log.mx      = log(mx),
    log.mx.new  = log(deaths.new / pop.size)
  )
