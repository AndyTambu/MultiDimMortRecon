library('rstanarm')
library(tidyverse)

raw_1519_Recoded <- read.csv('./data/raw_1519_Recoded.csv') %>% tibble()


log.lin.data <- 
  raw_1519_Recoded %>% 
  filter(!is.na(deaths.new)) %>% 
  # 
  # filter(!is.na(deaths.new), !geo == 'Jordan') %>% 
  # 
  filter(age.group == "15-19") %>% 
  mutate(log.mx.new = log(deaths.new/pop.size), 
         log.pop = log(pop.size)) %>% 
  select(geo, sex, edu.att, age.group, year,log.mx.new, deaths.new, pop.size, log.pop) 


log.lin.data.region <- 
  
  log.lin.data %>% 
  mutate(region = recode(geo, 
                         "Albania"   = 'region1',
                         "Armenia" = 'region2',   "Azerbaijan"= 'region2', "Turkey"= 'region2',   
                         "Egypt" = 'region3',      "Jordan"  = 'region3',   "Tunisia" = 'region3'  ) )  %>% 
  dplyr::select('edu.att','region','year','sex','deaths.new', 'log.pop')



############# Systematic model selection (non Bayesian) ##########

# with the REGION ####

vars <- c('edu.att','region','year','sex')
v <- c('edu.att','region','year','sex')


cb0 <- v

cb1.1 <- combn(v,1, function(x) paste(x, collapse=" + "))

cb1.2 <- combn(v,2,function(x) paste(x, collapse=" + "))

cb1.3 <- combn(v,3, function(x) paste(x, collapse=" + "))

cb1.4 <- combn(v,4, function(x) paste(x, collapse=" + "))


# una interazione
cb2.0 <- combn(v,2,function(x) paste(x, collapse=" * "))

cb2.1 <- apply(crossing(cb1.1, cb2.0),1, paste,collapse = " + ")

cb2.2  <- apply(crossing(cb1.2, cb2.0),1, paste,collapse = " + ")

cb2.3  <- apply(crossing(cb1.3, cb2.0),1, paste,collapse = " + ")

cb2.4  <- apply(crossing(cb1.4, cb2.0),1, paste,collapse = " + ")



# due interazioni
cb3.0 <- combn(cb2.0,2,function(x) paste(x, collapse=" + "))

cb2.1 <- apply(crossing(cb1.1, cb3.0),1, paste,collapse = " + ")

cb2.2  <- apply(crossing(cb1.2, cb3.0),1, paste,collapse = " + ")

cb2.3  <- apply(crossing(cb1.3, cb3.0),1, paste,collapse = " + ")

cb2.4  <- apply(crossing(cb1.4, cb3.0),1, paste,collapse = " + ")


# tre interazioni

cb4.0 <- combn(cb2.0,3,function(x) paste(x, collapse=" + "))

cb3.1 <- apply(crossing(cb1.1, cb4.0),1, paste,collapse = " + ")

cb3.2  <- apply(crossing(cb1.2, cb4.0),1, paste,collapse = " + ")

cb3.3  <- apply(crossing(cb1.3, cb4.0),1, paste,collapse = " + ")

cb3.4  <- apply(crossing(cb1.4, cb4.0),1, paste,collapse = " + ")



# quattro interazioni


cb5.0 <- combn(cb2.0,4,function(x) paste(x, collapse=" + "))

cb4.1 <- apply(crossing(cb1.1, cb5.0),1, paste,collapse = " + ")

cb4.2  <- apply(crossing(cb1.2, cb5.0),1, paste,collapse = " + ")

cb4.3  <- apply(crossing(cb1.3, cb5.0),1, paste,collapse = " + ")

cb4.4  <- apply(crossing(cb1.4, cb5.0),1, paste,collapse = " + ")



# cinque interazioni


cb6.0 <- combn(cb2.0,5,function(x) paste(x, collapse=" + "))

cb5.1 <- apply(crossing(cb1.1, cb6.0),1, paste,collapse = " + ")

cb5.2  <- apply(crossing(cb1.2, cb6.0),1, paste,collapse = " + ")

cb5.3  <- apply(crossing(cb1.3, cb6.0),1, paste,collapse = " + ")

cb5.4  <- apply(crossing(cb1.4, cb6.0),1, paste,collapse = " + ")



# sei interazioni


cb7.0 <- combn(cb2.0,6,function(x) paste(x, collapse=" + "))

cb6.1 <- apply(crossing(cb1.1, cb7.0),1, paste,collapse = " + ")

cb6.2  <- apply(crossing(cb1.2, cb7.0),1, paste,collapse = " + ")

cb6.3  <- apply(crossing(cb1.3, cb7.0),1, paste,collapse = " + ")

cb6.4  <- apply(crossing(cb1.4, cb7.0),1, paste,collapse = " + ")





# Exclude interactions with 'year' to avoid dimensionality explosion and overfitting. 
# Temporal effects are modeled additively instead, since group-specific year interactions 
# would create high-dimensional, weakly identified posteriors and hamper convergence 
# without adding clear interpretive value.



vars_comb <- 
  c(
    cb1.1,cb1.2,cb1.3,cb1.4,
    cb2.1,cb2.2,cb2.3,cb2.4,
    cb3.1,cb3.2,cb3.3,cb3.4,
    cb4.1,cb4.2,cb4.3,cb4.4,
    cb5.1,cb5.2,cb5.3,cb5.4,
    cb6.1,cb6.2,cb6.3,cb6.4
  ) 
vars_comb_filter <- vars_comb[!str_detect(vars_comb, "\\* year")&!str_detect(vars_comb, "year \\*")]

formula_vec <- paste0('deaths.new', " ~ ", vars_comb_filter)

# with the full data set ####

# create models: with the full data set 

log.lin.data.region <- 
  log.lin.data.region %>% 
  mutate(edu.att = as.factor(edu.att),
         region = as.factor(region),
         year = as.factor(year),
         sex = as.factor(sex))



log.lin.data.region_test_t <- log.lin.data.region %>% 
  dplyr::mutate(year = as.numeric(year))




# review---BIC ----

glm_res <- lapply( formula_vec, function(f)   {
  fit1 <- glm( f, data = log.lin.data.region_test_t, family = 'poisson', offset = log.pop)
  
  BIC <- BIC(fit1)
  
  fit1$coefficients <- coef( summary(fit1))
  return(BIC)
})
names(glm_res) <- formula_vec

unlist(glm_res)


# Unlist the list into a data frame
df <- stack(glm_res)

# Rename columns for clarity
colnames(df) <- c("Value", "Name")

# Display the result

df %>% 
  tibble() %>% 
  filter(Value == min(Value)) %>% 
  pull(Name)
# 
# deaths.new ~ edu.att + region + year + edu.att * region + edu.att * sex + region * sex 

# running the model in a Bayesian fashion ----

library('rstanarm')

func_selected_model <-  deaths.new ~ edu.att + region + year + edu.att * region + edu.att * sex + region * sex
model_Bayesian_test_func_to_test <- stan_glm( func_selected_model,
                                              data = log.lin.data.region,
                                              family = 'poisson',
                                              offset = log.pop,
                                              chains = 3,
                                              iter = 5000)

# some basic convergence checks 
max(model_Bayesian_test_func_to_test$stan_summary[,'Rhat'])
min(model_Bayesian_test_func_to_test$stan_summary[,'n_eff'])

















