# <<Paper's model running>>   ----

# libs and functions ####
library(tidyverse)
library(rjags)
library(R2jags)
library(latex2exp)
library(reshape2)



## the input file "d" ####


d <- read.csv('d_DHS_1519_Recoded_loglin_country_time_specific.csv') %>% 
  mutate(log.mx = log(mx)) 

d <- d %>% filter(sex == 'F') %>% select(-sex)

d <- d %>% filter(year %in% c(1978, 1979,seq(1980,2020, by = 5)))

d %>% distinct(year)


# some renaming for the sake of clarity

colnames(d)[1] <- 'country'
colnames(d)[2] <- 'edu.att'

colnames(d)[3] <- 'age_group'

colnames(d)[6] <- 'pop'
colnames(d)[7] <- 'age'

# get the meta data about unique age groups, years and counties. 

age_groups <- unique(d$age_group)
ages <- c(seq(15, 85, by = 5))
years <- unique(d$year)
countries <- unique(d$country)
educations <- unique(d$edu.att)

# data preparation in JAGS format ####


## Get the data into JAGS format, i.e. arrays of dimension age x time x nation x education


logMx.atc <- array(NA, c(length(age_groups), length(years), length(countries)))


for(j in 1:length(years)){j
  this_value <- as.matrix(d %>% 
                            filter(year == years[j]) %>% 
                            mutate(log.mx.tot = log(mx.tot)) %>% 
                            
                            distinct(country, age, log.mx.tot) %>% 
                            spread(country, log.mx.tot) %>% 
                            select(-age))
  these_countries <- colnames(this_value)
  for(k in 1:length(these_countries)){
    logMx.atc[,j,which(countries==these_countries[k])] <- this_value[,k]
  }
}


y.atce <- array(NA, c(length(age_groups), length(years), length(countries), length(educations)))
pop.atce <- array(NA, c(length(age_groups), length(years), length(countries), length(educations)))


for (z in 1:length(educations))
{
  for(j in 1:length(years)){
    this_value <- as.matrix(d %>% filter(year == years[j],edu.att ==educations[z] ) %>% select(country, age, deaths.new) %>% spread(country, deaths.new) %>% select(-age))
    this_pop <- as.matrix(d %>% filter(year == years[j],edu.att ==educations[z] ) %>% select(country,age, pop) %>% spread(country, pop) %>% select(-age))
    these_countries <- colnames(this_value)
    these_countries_pop <- colnames(this_pop)
    for(k in 1:length(these_countries)){
      y.atce[,j,which(countries==these_countries[k]),which(educations==educations[z])] <- this_value[,k]
    }
    for(l in 1:length(these_countries_pop)){
      pop.atce[,j,which(countries==these_countries_pop[l]),which(educations==educations[z])] <- this_pop[,l]
    }
  }
}    






## the variances


log_lin_results <-
  read.csv(file = './data/log_lin_results.csv') %>%
  mutate(edu.att = factor(recode(edu.att,'secondary.higher' = 'more.than.primary',
                                 'noEdu.primary' = "no.edu.primary"),
                          levels = c('no.edu.primary','more.than.primary')))

# visualise
# 
# log_lin_results  %>%
#   filter(year %in% seq(1980, 2015, by = 5), sex == 'F') %>%
#   ggplot(aes(x = year, y = mu, color = edu.att))+
#   geom_point(aes(group = interaction(geo, edu.att, year)))+
#   facet_wrap(~geo)






DHS_sigma_values_estimates_withNAs <- read.csv(file = './data/DHS_sigma_values_estimates_withNAs.csv')


sigma_values_estimates <- read.csv(file = './data/DHS_sigma_values_estimates_withNAs.csv') %>% 
  tibble() %>% 
  mutate(edu.att = recode(edu.att, no.edu.primary = "noEdu.primary")) 


sigma.DHS.tce.INPUT <- array(NA, c(length(years),length(countries),length(educations)))


for(z in 1:length(educations)){ 
  for(j in 1:length(years)){ 
    this_values <- pull(
      
      
      d %>% 
        left_join(sigma_values_estimates %>% 
                    filter(edu.att != 'tot') %>%
                    mutate(edu.att = factor(recode(edu.att,'secondary.higher' = 'more.than.primary', 
                                                   'noEdu.primary' = "no.edu.primary"), 
                                            levels = c('no.edu.primary','more.than.primary')))
                  , by = c('country', 'edu.att', 'year')) %>% 
        filter(year == years[j], edu.att == educations[z], age_group == "15-19") %>% 
        # group_by(state, year) %>% 
        # mutate(log.mx = ifelse(!is.na(log.mx), log.mx, mean(log.mx, na.rm = T))) %>% 
        select(sigma))
    
    sigma.DHS.tce.INPUT[j,,z] <- this_values
    
    
  }
}



sigma.log.lin <- 
  log_lin_results %>% 
  rename(sigma_LogLin = sigma) %>% 
  filter(sex == 'F') %>% 
  left_join(
    DHS_sigma_values_estimates_withNAs %>% 
      tibble() %>% 
      filter(edu.att != 'tot')  
    , by = c('geo' = 'country', 'edu.att', 'year'))







sigma.logLin.tce.INPUT <- array(NA, c(length(years),length(countries),length(educations)))


for(z in 1:length(educations)){ 
  for(j in 1:length(years)){    
    this_values <- pull(
      
      sigma.log.lin %>% 
        # filter(geo  !=  "Palestine, State of"  ) %>% 
        filter(year == years[j], edu.att == educations[z], age.group == "15-19") %>% 
        # group_by(state, year) %>% 
        # mutate(log.mx = ifelse(!is.na(log.mx), log.mx, mean(log.mx, na.rm = T))) %>% 
        select(sigma_LogLin))
    
    sigma.logLin.tce.INPUT[j,,z] <- this_values
    
    
  }
}




# the log mortalities inputs ####


log.INPUT.atce <- array(NA, c(length(age_groups), length(years), length(countries), length(educations)))


for (z in 1:length(educations))
{
  for(j in 1:length(years)){
    this_value <- as.matrix(d %>% filter(year == years[j],edu.att ==educations[z] ) %>% select(country, age, log.mx) %>% spread(country, log.mx) %>% select(-age))
    these_countries <- colnames(this_value)
    for(k in 1:length(these_countries)){
      log.INPUT.atce[,j,which(countries==these_countries[k]),which(educations==educations[z])] <- this_value[,k]
    }
  }
}    




# principal components ----


pcs <- array(NA, c(length(age_groups), length(educations), 3))

pcs[,1,] <- as.matrix(read.csv("./data/pcs_lower_cluster1_Female.csv")[1:15,1:3])
pcs[,2,] <- as.matrix(read.csv("./data/pcs_midHigh_cluster1_Female.csv")[1:15,1:3])


# renaming for the model 
Yx = pcs





# the distributions from which we took the pcs
estimated_normals_2edu_groups <- read.csv("./data/estimated_normals_2edu_groups.csv")


sdUN.e.INPUT <- array(NA, c(length(age_groups), length(educations)))

sdUN.e.INPUT[,1] <- estimated_normals_2edu_groups %>% filter(in.which.group == 'lower') %>%  pull(mod_est_sd)
sdUN.e.INPUT[,2] <- estimated_normals_2edu_groups %>% filter(in.which.group == 'midHigh') %>%  pull(mod_est_sd)

sdUN.e.INPUT %>% dim()



log.sigma.DHS.tce.INPUT <- log(sigma.DHS.tce.INPUT)
log.sigma.logLin.tce.INPUT <- log(sigma.logLin.tce.INPUT)


# here we collect all the jags data 

jags.data <- list(
  
  
  log.sigma.DHS.tce.INPUT = log.sigma.DHS.tce.INPUT,
  sigma.DHS.tce.INPUT = sigma.DHS.tce.INPUT,
  
  
  
  log.sigma.logLin.tce.INPUT = log.sigma.logLin.tce.INPUT,
  sigma.logLin.tce.INPUT = sigma.logLin.tce.INPUT,
  
  
  
  
  pop.atce = pop.atce,
  logMx.atc = logMx.atc,
  log.INPUT.atce = log.INPUT.atce,
  sdUN.e.INPUT = sdUN.e.INPUT,
  S = length(educations), 
  X = length(age_groups), 
  'T' = length(years), 
  n.a=length(countries),
  P=3,
  Yx = Yx
)





# countriy                             region 
# 1  "Albania"                          1        
# 2   "Armenia"                          2
# 3  "Azerbaijan"                       2  
# 4   "Bosnia and Herzegovina"           1
# 5   "Egypt"                            3       
# 6   "Georgia"                          2
# 7   "Jordan"                           3     
# 8 "Lebanon"                          3  
# 9  "North Macedonia"                   1 
# 10  "Montenegro"                        1
# 11  "Palestine, State of"               3
# 12  "Tunisia"                           3
# 13  "Turkey"                            2
# 
# 
# 
# geo        region 
# <chr>      <fct>  
# 1 Albania    region1
# 2 Armenia    region2
# 3 Azerbaijan region2
# 4 Egypt      region3
# 5 Jordan     region3
# 6 Tunisia    region3
# 7 Turkey     region2


# parameters to monitor ####
parnames <- c(
              'beta.ate',
              'u.ace',
              'logmx.atce',
              'sigma.beta',
              'mu.beta',
              'sigma.u',
              'sigma.mu',
              'sigma.DHS.te',
              'sig.u.DHSte',
              'sdUN.e',
              'logmx.PCS.atce',
              'sigma.DHS.te',
              'sigma.u.DHS.te',
              'nu.te',
              'u.ace',
              'sigma.LogLin.tce',
              
              'logmx.atce_COPY',
              'mu.DHS.te',
              
              
              'sigma.LogLin.te',
              'mu.log.sigma.LogLin.te'
              # ,
              # 'r.unif',
              # 'lambda.unif'
              )



# run model ####



# list.files('./Review_1/models')





time_taken <- system.time({
  
  mod_quick_test <- R2jags::jags(
    n.chains = 3,
    data = jags.data,
    parameters.to.save=parnames, 
    n.burnin = 35000,
    n.iter = 125000,
    model.file = "./model/model_review2_allU_0_40_sigmaUN_gamma0101.txt"
    #model.file = "./Review_1/models/model_3_estimateSIGMAloglin&SIGMAun.txt")
    #model.file = "./Review_1/models/model_3_tests_REVIEW2.txt"
    )

})


time_taken



# check for the convergence. 

max(mod_quick_test$BUGSoutput$summary[,"Rhat"])
mod_quick_test$BUGSoutput$summary[,"Rhat"] %>%
  as.data.frame() %>%
  rownames_to_column() %>%
  tibble() %>% View()

mod <- mod_quick_test

mod$model
mod$BUGSoutput$summary[,"Rhat"] %>% density() %>%  plot()

# pull out the posterior samples
mcmc.array <- mod$BUGSoutput$sims.array
mcmc.array %>% dim()


# res ----

res <- tibble(year = rep(unique(d$year), each = length(countries)*length(ages)*length(educations)),
              age = rep(unique(d$age), length(countries)*length(educations)*length(years)), 
              country = rep(rep(unique(d$country), each = length(educations)*length(age_groups)), length(years)),
              edu = rep(rep(unique(d$edu.att), each = length(ages)), length(years)*length(countries)),
              median = rep(NA, length(age_groups)*length(countries)*length(educations)*length(years)), 
              upper = rep(NA, length(age_groups)*length(countries)*length(educations)*length(years)), 
              lower = rep(NA, length(age_groups)*length(countries)*length(educations)*length(years)))





for(k in 1:length(years)){ 
  for(z in 1:length(educations)){ 
    for(i in 1:length(age_groups)){ 
      for(j in 1:length(countries)){  
        sms <-  c(mcmc.array[,,paste0("logmx.atce[",i,",",k,"," ,j ,",", z,"]")]  ) # 2013 == years[1] just one year available !!
        res$median[res$age==unique(d$age)[i]&res$country==countries[j]&res$edu==educations[z]&res$year==years[k]] <- median((sms))
        res$upper[res$age==unique(d$age)[i]&res$country==countries[j]&res$edu==educations[z]&res$year==years[k]] <- quantile((sms), 0.975)
        res$lower[res$age==unique(d$age)[i]&res$country==countries[j]&res$edu==educations[z]&res$year==years[k]] <- quantile((sms), 0.025)
      }
    }
  }
}




# u.ace ----

res.u.ace <- tibble(age = rep(unique(d$age), length(countries)*length(educations)),
                country = rep(unique(d$country), each =  length(educations)*length(ages)),
                edu = rep(rep(unique(d$edu.att),each = length(ages)),    length(countries)),
                median = rep(NA, length(countries)*length(educations)*length(ages)),
                upper = rep(NA, length(countries)*length(educations)*length(ages)),
                lower = rep(NA, length(countries)*length(educations)*length(ages)))
# 
for(k in 1:length(ages)){
  for(z in 1:length(educations)){
      for(j in 1:length(countries)){
       sms <-  c(mcmc.array[,,paste0("u.ace[",k,"," ,j ,",", z,"]")]  ) # 2013 == years[1] just one year available !!
       res.u.ace$median[res.u.ace$country==countries[j]&res.u.ace$edu==educations[z]&res.u.ace$age==ages[k]] <- median((sms))
       res.u.ace$upper[res.u.ace$country==countries[j]&res.u.ace$edu==educations[z]&res.u.ace$age==ages[k]] <- quantile((sms), 0.9)
       res.u.ace$lower[res.u.ace$country==countries[j]&res.u.ace$edu==educations[z]&res.u.ace$age==ages[k]] <- quantile((sms), 0.1)
      }
    }
  }
# 

# 


res.u.ace %>%
  ggplot(aes(x = age, median)) +
  geom_line(aes(color = edu, group = interaction(edu,country))) + 
  geom_ribbon(aes(x = age,ymin = lower, ymax = upper, fill = edu, group = interaction(edu, country)), alpha = 0.3)+
  scale_fill_manual(values = c("#749AAA","#D30D45"),name = "Edu. Att.",labels = c("More than primary","No education or primary")) +
  scale_color_manual(values = c("#749AAA","#D30D45"),name = "Edu. Att.",labels = c("More than primary","No education or primary"))+
  facet_wrap(~country)+
 # ggtitle("u.ace (80% C.I.)",
  #        subtitle = "")+
  # ylab(TeX("$u_{a,c,e}$"))+
  # xlab("age group")+
  ylab(TeX(""))+
  xlab("")+
  theme_bw()+
  theme(legend.position = "bottom",
        axis.text=element_text(size=14),
        axis.text.x=element_text(angle=90),
        axis.title=element_text(size=12),
        strip.text.x = element_text(size = 14),
        strip.text.y = element_text(size = 18),
        legend.key.size = unit(1, 'cm'), #change legend key size
        legend.key.height = unit(1, 'cm'), #change legend key height
        legend.key.width = unit(1, 'cm'), #change legend key width
        legend.title = element_text(size=14), #change legend title font size
        legend.text = element_text(margin = margin(t = 10, b = 10),size=14),
        legend.spacing.x = unit(1.0, 'cm'),
        plot.title = element_text(size = 18),
        plot.subtitle = element_text(size = 15),
        strip.background = element_rect(fill = "white", color = "black")
  )


# sigma.u----

sigma.u.ae  <- tibble(age = rep(unique(ages),length(educations)),
                         edu = rep(unique(educations),each = length(ages)),
                         median = rep(NA,length(educations)*length(ages)),
                         upper = rep(NA,length(educations)*length(ages)),
                         lower = rep(NA,length(educations)*length(ages)))

for(k in 1:length(educations)){
  for(z in 1:length(ages)){
    sms <-  c(mcmc.array[,,paste0("sigma.u[",z,",", k,"]")]  ) # 2013 == years[1] just one year available !!
    sigma.u.ae$median[sigma.u.ae$age==ages[z]&sigma.u.ae$edu==educations[k]] <- median((sms))
    sigma.u.ae$upper[sigma.u.ae$age==ages[z]&sigma.u.ae$edu==educations[k]] <- quantile((sms), 0.95)
    sigma.u.ae$lower[sigma.u.ae$age==ages[z]&sigma.u.ae$edu==educations[k]] <- quantile((sms), 0.05)
  }
}



# 
sigma.u.ae %>%
  ggplot() +
  geom_pointrange(aes(x = age, y = median, ymin = lower, ymax = upper, color = edu),
                  position = position_dodge(width = 0.9))+
  scale_color_manual(values = c("#749AAA","#D30D45"),name = "Edu. Att.",labels = c("More than primary","No education or primary"))+
  ggnewscale::new_scale_color() +
  coord_flip()+
  
  # ggtitle("log-mortality rates (80% C.I.)",
  #         subtitle = "Albania, female population")+
  ylab("")+
  xlab("")+
  theme_bw()+
  theme(axis.text=element_text(size=18),
        axis.text.x=element_text(angle=90),
        axis.title=element_text(size=12),
        strip.text.x = element_text(size = 30),
        strip.text.y = element_text(size = 18),
        strip.background =element_rect(fill="white"),
        legend.key.size = unit(2, 'cm'), #change legend key size
        legend.key.height = unit(2, 'cm'), #change legend key height
        legend.key.width = unit(2, 'cm'), #change legend key width
        legend.title = element_text(size=18), #change legend title font size
        legend.text = element_text(margin = margin(t = 10, b = 10),size=18),
        legend.spacing.x = unit(1.0, 'cm'),
        plot.title = element_text(size = 18),
        plot.subtitle = element_text(size = 15))



# nu.te ----



res.nu.te <- tibble(year = rep(unique(years),length(educations)),
                           edu = rep(unique(d$edu.att),each = length(years)),
                           median = rep(NA,length(educations)*length(years)),
                           upper = rep(NA,length(educations)*length(years)),
                           lower = rep(NA,length(educations)*length(years)))

for(k in 1:length(years)){
  for(z in 1:length(educations)){
    sms <-  c(mcmc.array[,,paste0("nu.te[",k,",", z,"]")]  ) # 2013 == years[1] just one year available !!
    res.nu.te$median[res.nu.te$edu==educations[z]&res.nu.te$year==years[k]] <- median((sms))
    res.nu.te$upper[res.nu.te$edu==educations[z]&res.nu.te$year==years[k]] <- quantile((sms), 0.9)
    res.nu.te$lower[res.nu.te$edu==educations[z]&res.nu.te$year==years[k]] <- quantile((sms), 0.1)
  }
}



res.nu.te %>%
  dplyr::filter(year > 1979) %>% 
  ggplot(aes(x = year, median)) +
  geom_line(aes(color = edu, group = interaction(edu)), size = 1.5, alpha = 1) + 
  geom_ribbon(aes(x = year,ymin = lower, ymax = upper, fill = edu, group = interaction(edu)), alpha = 0.3)+
  scale_fill_manual(values = c("#749AAA","#D30D45"),name = "Edu. Att.",labels = c("More than primary","No education or primary")) +
  scale_color_manual(values = c("#749AAA","#D30D45"),name = "Edu. Att.",labels = c("More than primary","No education or primary"))+
  # ggtitle(TeX('$nu_{t,e}$ (80% C.I.)')
  ggtitle(TeX(''),
          subtitle = "")+
  ylab(TeX(""))+
  xlab("")+
  theme_bw()+
  theme(   legend.position = "bottom",          # Moves the legend under the plot
           legend.direction = "horizontal",
           
           axis.text=element_text(size=14),
        axis.text.x=element_text(angle=90),
        axis.title=element_text(size=12),
        strip.text.x = element_text(size = 14),
        strip.text.y = element_text(size = 18),
        legend.key.size = unit(1, 'cm'), #change legend key size
        legend.key.height = unit(1, 'cm'), #change legend key height
        legend.key.width = unit(1, 'cm'), #change legend key width
        legend.title = element_text(size=14), #change legend title font size
        legend.text = element_text(margin = margin(t = 10, b = 10),size=14),
        legend.spacing.x = unit(1.0, 'cm'),
        plot.title = element_text(size = 18),
        plot.subtitle = element_text(size = 15),
        strip.background = element_rect(fill = "white", color = "black")
  )

# res.sig.u.DHS.te ----
res.sig.u.DHS.te <- tibble(year = rep(unique(years),length(educations)),
                           edu = rep(unique(d$edu.att),each = length(years)),
                           median = rep(NA,length(educations)*length(years)),
                           upper = rep(NA,length(educations)*length(years)),
                           lower = rep(NA,length(educations)*length(years)))

for(k in 1:length(years)){
  for(z in 1:length(educations)){
    sms <-  c(mcmc.array[,,paste0("sigma.DHS.te[",k,",", z,"]")]  ) # 2013 == years[1] just one year available !!
    res.sig.u.DHS.te$median[res.sig.u.DHS.te$edu==educations[z]&res.sig.u.DHS.te$year==years[k]] <- median((sms))
    res.sig.u.DHS.te$upper[res.sigma.DHS.te$edu==educations[z]&res.sig.u.DHS.te$year==years[k]] <- quantile((sms), 0.975)
    res.sig.u.DHS.te$lower[res.sig.u.DHS.te$edu==educations[z]&res.sig.u.DHS.te$year==years[k]] <- quantile((sms), 0.025)
  }
}







res.sig.u.DHS.te %>%
  filter(year %in%  seq(1980, 2015, by = 5)) %>%
  ggplot() +
  geom_pointrange(aes(x = year, y = median, ymin = lower, ymax = upper, color = edu),
                  position = position_dodge(width = 0.9), size = 1, linewidth = 1)+
  scale_color_manual(values = c("#749AAA","#D30D45"),name = "Edu. Att.",labels = c("More than primary","No education or primary"))+
  ggnewscale::new_scale_color()+

  
  coord_flip()+
  
  # ggtitle("log-mortality rates (80% C.I.)",
  #         subtitle = "Albania, female population")+
  ylab("")+
  xlab("")+
  theme_bw()+
  theme(axis.text=element_text(size=18),
        axis.text.x=element_text(angle=90),
        axis.title=element_text(size=12),
        strip.text.x = element_text(size = 30),
        strip.text.y = element_text(size = 18),
        strip.background =element_rect(fill="white"),
        legend.key.size = unit(2, 'cm'), #change legend key size
        legend.key.height = unit(2, 'cm'), #change legend key height
        legend.key.width = unit(2, 'cm'), #change legend key width
        legend.title = element_text(size=18), #change legend title font size
        legend.text = element_text(margin = margin(t = 10, b = 10),size=18),
        legend.spacing.x = unit(1.0, 'cm'),
        plot.title = element_text(size = 18),
        plot.subtitle = element_text(size = 15))

caterpillar_res.sigma.DHS.te


# res.sigma.DHS.te ####

res.sigma.DHS.te <- tibble(year = rep(unique(years),length(educations)),
                            edu = rep(unique(d$edu.att),each = length(years)),
              median = rep(NA,length(educations)*length(years)),
              upper = rep(NA,length(educations)*length(years)),
              lower = rep(NA,length(educations)*length(years)))

for(k in 1:length(years)){
  for(z in 1:length(educations)){
        sms <-  c(mcmc.array[,,paste0("sigma.DHS.te[",k,",", z,"]")]  ) # 2013 == years[1] just one year available !!
        res.sigma.DHS.te$median[res.sigma.DHS.te$edu==educations[z]&res.sigma.DHS.te$year==years[k]] <- median((sms))
        res.sigma.DHS.te$upper[res.sigma.DHS.te$edu==educations[z]&res.sigma.DHS.te$year==years[k]] <- quantile((sms), 0.975)
        res.sigma.DHS.te$lower[res.sigma.DHS.te$edu==educations[z]&res.sigma.DHS.te$year==years[k]] <- quantile((sms), 0.025)
      }
    }
  





sigma.DHS.te_df =  melt( sigma.DHS.tce.INPUT,
                         value.name="sigma.DHS.tce.INPUT", varnames=c('year','country','edu')) %>% 
  tibble() %>% 
  dplyr::mutate(edu = as.factor(edu)) %>% 
   dplyr::filter(year > 2) %>% 
  dplyr::mutate(year = years[year])
  

caterpillar_res.sigma.DHS.te <- 
res.sigma.DHS.te %>%
  filter(year %in%  seq(1980, 2015, by = 5)) %>%
  ggplot() +
  geom_pointrange(aes(x = year, y = median, ymin = lower, ymax = upper, color = edu),
                  position = position_dodge(width = 0.9), size = 1, linewidth = 1)+
  scale_color_manual(values = c("#749AAA","#D30D45"),name = "Edu. Att.",labels = c("More than primary","No education or primary"))+
  ggnewscale::new_scale_color() +
  geom_point(data = sigma.DHS.te_df %>% 
               mutate(edu = as.character(recode(edu, `1`="no.edu.primary",`2`="more.than.primary"))) ,
                  aes(x = year, y = sigma.DHS.tce.INPUT, color = edu), shape = 17, size = 3,
             position = position_dodge(width = 0.9))+
  scale_color_manual(values = c("#749AAA","#D30D45"),name = "Edu. Att.",labels = c("More than primary","No education or primary"))+
  



  scale_x_continuous(breaks = (res.sigma.DHS.te %>%
                       filter(year %in%  seq(1980, 2015, by = 5)) )$year)+  # This ensures all x values are shown

  #scale_color_manual(values = c("#749AAA","#D30D45"),name = "Edu. Att.",labels = c("More than primary","No education or primary"))+
  
  
  coord_flip()+
  
  # ggtitle("log-mortality rates (80% C.I.)",
  #         subtitle = "Albania, female population")+
  ylab("")+
  xlab("")+
  theme_bw()+
  theme(axis.text=element_text(size=18),
        axis.text.x=element_text(angle=90),
        axis.title=element_text(size=12),
        strip.text.x = element_text(size = 30),
        strip.text.y = element_text(size = 18),
        strip.background =element_rect(fill="white"),
        legend.key.size = unit(2, 'cm'), #change legend key size
        legend.key.height = unit(2, 'cm'), #change legend key height
        legend.key.width = unit(2, 'cm'), #change legend key width
        legend.title = element_text(size=18), #change legend title font size
        legend.text = element_text(margin = margin(t = 10, b = 10),size=18),
        legend.spacing.x = unit(1.0, 'cm'),
        plot.title = element_text(size = 18),
        plot.subtitle = element_text(size = 15))

caterpillar_res.sigma.DHS.te





log.sigma.DHS.te_df =  melt( log.sigma.DHS.tce.INPUT,
                         value.name="sigma.DHS.tce.INPUT", varnames=c('year','country','edu')) %>% 
  tibble() %>% 
  dplyr::mutate(edu = as.factor(edu)) %>% 
  dplyr::filter(year > 2) %>% 
  dplyr::mutate(year = years[year])


res.sigma.DHS.te %>%
  filter(year %in%  seq(1980, 2015, by = 5)) %>%
  ggplot() +
  geom_pointrange(aes(x = year, y = log(median), ymin = log(lower), ymax = log(upper), color = edu),
                  position = position_dodge(width = 0.9))+
  scale_color_manual(values = c("#749AAA","#D30D45"),name = "Edu. Att.",labels = c("More than primary","No education or primary"))+
  ggnewscale::new_scale_color() +
  geom_point(data = log.sigma.DHS.te_df,
             aes(x = year, y = sigma.DHS.tce.INPUT, color = edu), shape = 17,
             position = position_dodge(width = 0.9))+
  scale_color_manual(values = c("#749AAA","#D30D45"),name = "Edu. Att.",labels = c("More than primary","No education or primary"))+
  
  coord_flip()+
  
  # ggtitle("log-mortality rates (80% C.I.)",
  #         subtitle = "Albania, female population")+
  ylab("")+
  xlab("")+
  theme_bw()+
  theme(axis.text=element_text(size=18),
        axis.text.x=element_text(angle=90),
        axis.title=element_text(size=12),
        strip.text.x = element_text(size = 30),
        strip.text.y = element_text(size = 18),
        strip.background =element_rect(fill="white"),
        legend.key.size = unit(2, 'cm'), #change legend key size
        legend.key.height = unit(2, 'cm'), #change legend key height
        legend.key.width = unit(2, 'cm'), #change legend key width
        legend.title = element_text(size=18), #change legend title font size
        legend.text = element_text(margin = margin(t = 10, b = 10),size=18),
        legend.spacing.x = unit(1.0, 'cm'),
        plot.title = element_text(size = 18),
        plot.subtitle = element_text(size = 15))




  melt( sigma.logLin.tce.INPUT,
         value.name="sigma.logLin.tce.INPUT", varnames=c('year','country','edu')) %>% 
  tibble() %>% 
  dplyr::mutate(edu = as.factor(edu)) %>% 
  dplyr::mutate(year = years[year]) %>% 
    ggplot(aes(y = year, x = sigma.logLin.tce.INPUT, shape = edu ))+
    geom_point()+
    facet_wrap(~country)

  

# res sdUN.e.ae  ----


  res.sdUN.e.ae  <- tibble(age = rep(unique(ages),length(educations)),
                             edu = rep(unique(educations),each = length(ages)),
                             median = rep(NA,length(educations)*length(ages)),
                             upper = rep(NA,length(educations)*length(ages)),
                             lower = rep(NA,length(educations)*length(ages)))

  for(k in 1:length(educations)){
    for(z in 1:length(ages)){
      sms <-  c(mcmc.array[,,paste0("sdUN.e[",z,",", k,"]")]  ) # 2013 == years[1] just one year available !!
      res.sdUN.e.ae$median[res.sdUN.e.ae$age==ages[z]&res.sdUN.e.ae$edu==educations[k]] <- median((sms))
      res.sdUN.e.ae$upper[res.sdUN.e.ae$age==ages[z]&res.sdUN.e.ae$edu==educations[k]] <- quantile((sms), 0.95)
      res.sdUN.e.ae$lower[res.sdUN.e.ae$age==ages[z]&res.sdUN.e.ae$edu==educations[k]] <- quantile((sms), 0.05)
    }
  }


  
  # 
  res.sdUN.e.ae %>%
    ggplot() +
    geom_pointrange(aes(x = age, y = median, ymin = lower, ymax = upper, color = edu),
                    position = position_dodge(width = 0.9))+
    scale_color_manual(values = c("#749AAA","#D30D45"),name = "Edu. Att.",labels = c("More than primary","No education or primary"))+
    ggnewscale::new_scale_color() +
    coord_flip()+

    # ggtitle("log-mortality rates (80% C.I.)",
    #         subtitle = "Albania, female population")+
    ylab("")+
    xlab("")+
    theme_bw()+
    theme(axis.text=element_text(size=18),
          axis.text.x=element_text(angle=90),
          axis.title=element_text(size=12),
          strip.text.x = element_text(size = 30),
          strip.text.y = element_text(size = 18),
          strip.background =element_rect(fill="white"),
          legend.key.size = unit(2, 'cm'), #change legend key size
          legend.key.height = unit(2, 'cm'), #change legend key height
          legend.key.width = unit(2, 'cm'), #change legend key width
          legend.title = element_text(size=18), #change legend title font size
          legend.text = element_text(margin = margin(t = 10, b = 10),size=18),
          legend.spacing.x = unit(1.0, 'cm'),
          plot.title = element_text(size = 18),
          plot.subtitle = element_text(size = 15))
  
  # res sdUN.e.ae  ----
  
  
  res.tauUN.e.ae  <- tibble(age = rep(unique(ages),length(educations)),
                           edu = rep(unique(educations),each = length(ages)),
                           median = rep(NA,length(educations)*length(ages)),
                           upper = rep(NA,length(educations)*length(ages)),
                           lower = rep(NA,length(educations)*length(ages)))
  
  for(k in 1:length(educations)){
    for(z in 1:length(ages)){
      sms <-  1/(c(mcmc.array[,,paste0("sdUN.e[",z,",", k,"]")]  )^2) #     2013 == years[1] just one year available !!
      res.tauUN.e.ae$median[res.tauUN.e.ae$age==ages[z]&res.tauUN.e.ae$edu==educations[k]] <- median((sms))
      res.tauUN.e.ae$upper[res.tauUN.e.ae$age==ages[z]&res.tauUN.e.ae$edu==educations[k]] <- quantile((sms), 0.95)
      res.tauUN.e.ae$lower[res.tauUN.e.ae$age==ages[z]&res.tauUN.e.ae$edu==educations[k]] <- quantile((sms), 0.05)
    }
  }
  
  
  
  # 
  res.tauUN.e.ae %>%
    ggplot() +
    geom_pointrange(aes(x = age, y = median, ymin = lower, ymax = upper, color = edu),
                    position = position_dodge(width = 0.9))+
    scale_color_manual(values = c("#749AAA","#D30D45"),name = "Edu. Att.",labels = c("More than primary","No education or primary"))+
    ggnewscale::new_scale_color() +
    coord_flip()+
    
    # ggtitle("log-mortality rates (80% C.I.)",
    #         subtitle = "Albania, female population")+
    ylab("")+
    xlab("")+
    theme_bw()+
    theme(axis.text=element_text(size=18),
          axis.text.x=element_text(angle=90),
          axis.title=element_text(size=12),
          strip.text.x = element_text(size = 30),
          strip.text.y = element_text(size = 18),
          strip.background =element_rect(fill="white"),
          legend.key.size = unit(2, 'cm'), #change legend key size
          legend.key.height = unit(2, 'cm'), #change legend key height
          legend.key.width = unit(2, 'cm'), #change legend key width
          legend.title = element_text(size=18), #change legend title font size
          legend.text = element_text(margin = margin(t = 10, b = 10),size=18),
          legend.spacing.x = unit(1.0, 'cm'),
          plot.title = element_text(size = 18),
          plot.subtitle = element_text(size = 15))
  
  
  


  # res.beta.ate ----
  
  res.beta.ate1 <- tibble(     
    age = rep(unique(d$age), length(educations)*length(years)), 
    year = rep(unique(d$year), each = length(ages)*length(educations)),
    edu = rep(rep(unique(d$edu.att), each = length(ages)), length(years)),
    median = rep(NA, length(age_groups)*length(educations)*length(years)), 
    upper = rep(NA, length(age_groups)*length(educations)*length(years)), 
    lower = rep(NA, length(age_groups)*length(educations)*length(years)))
  
  
  for(k in 1:length(years)){ 
    for(z in 1:length(educations)){ 
      for(i in 1:length(age_groups)){ 
          sms <-  c(mcmc.array[,,paste0("beta.ate[",i,",",k,",", z,",1]")]  ) # 2013 == years[1] just one year available !!
          res.beta.ate1$median[res.beta.ate1$age==unique(d$age)[i]&res.beta.ate1$edu==educations[z]&res.beta.ate1$year==years[k]] <- median((sms))
          res.beta.ate1$upper[res.beta.ate1$age==unique(d$age)[i]&res.beta.ate1$edu==educations[z]&res.beta.ate1$year==years[k]] <- quantile((sms), 0.975)
          res.beta.ate1$lower[res.beta.ate1$age==unique(d$age)[i]&res.beta.ate1$edu==educations[z]&res.beta.ate1$year==years[k]] <- quantile((sms), 0.025)
        }
      }
    }
  
  
  

  # res.sigma.LogLin.tce ----
  
  res.sigma.LogLin.tce <- tibble(year = rep(unique(d$year),   length(countries)*length(educations)),
                country = rep(unique(d$country), each =  length(educations)*length(years)),
                edu = rep(rep(unique(d$edu.att),each = length(years)),    length(countries)),
                median = rep(NA, length(countries)*length(educations)*length(years)),
                upper = rep(NA, length(countries)*length(educations)*length(years)),
                lower = rep(NA, length(countries)*length(educations)*length(years)))

  for(k in 1:length(years)){
    for(z in 1:length(educations)){
        for(j in 1:length(countries)){
          sms <-  c(mcmc.array[,,paste0("sigma.LogLin.tce[",k,"," ,j ,",", z,"]")]  ) # 2013 == years[1] just one year available !!
          res.sigma.LogLin.tce $median[res.sigma.LogLin.tce $country==countries[j]&res.sigma.LogLin.tce $edu==educations[z]&res.sigma.LogLin.tce $year==years[k]] <- median((sms))
          res.sigma.LogLin.tce $upper[res.sigma.LogLin.tce $country==countries[j]&res.sigma.LogLin.tce $edu==educations[z]&res.sigma.LogLin.tce $year==years[k]] <- quantile((sms), 0.9)
          res.sigma.LogLin.tce $lower[res.sigma.LogLin.tce $country==countries[j]&res.sigma.LogLin.tce $edu==educations[z]&res.sigma.LogLin.tce $year==years[k]] <- quantile((sms), 0.1)
        }
      }
    }


  res.sigma.LogLin.tce %>%
    filter(year %in%  seq(1980, 2015, by = 5)) %>%
    # mutate(median = exp(median), upper = exp(upper), lower = exp(lower)) %>%
    ggplot(aes(x = year)) +
    geom_line(aes(y = median, color = edu, group = interaction(edu)), size = 1) +
    geom_ribbon(aes(x = year,ymin = lower, ymax = upper, fill = edu, group = interaction(country, edu)), alpha = 0.3)+
    
    scale_color_manual(values = c("#749AAA","#D30D45"),name = "Edu. Att.",labels = c("More than primary","No education or primary"))+
    scale_fill_manual(values = c("#749AAA","#D30D45"),name = "Edu. Att.",labels = c("More than primary","No education or primary"))+
    facet_wrap(~country)+
    # ggtitle(TeX('$sigma^{info}_{t,c,e}$, 80% C.I. female population'),
    #         subtitle = "")+
    ylab(TeX(""))+
    xlab("")+
    theme_bw()+
    theme(legend.position = "bottom",
      axis.text=element_text(size=14),
          axis.text.x=element_text(angle=90),
          axis.title=element_text(size=12),
          strip.text.x = element_text(size = 14),
          strip.text.y = element_text(size = 18),
          legend.key.size = unit(1, 'cm'), #change legend key size
          legend.key.height = unit(1, 'cm'), #change legend key height
          legend.key.width = unit(1, 'cm'), #change legend key width
          legend.title = element_text(size=14), #change legend title font size
          legend.text = element_text(margin = margin(t = 10, b = 10),size=14),
          legend.spacing.x = unit(1.0, 'cm'),
          plot.title = element_text(size = 18),
          plot.subtitle = element_text(size = 15),
      strip.background = element_rect(fill = "white", color = "black"))






# logmx.acte results ####
# library(latex2exp)

age_names <- tibble(x = c(0,1,seq(5, 85, by = 5)),
                    age.group = fct_inorder(c("<0","1-4", paste(seq(5,80,by = 5),seq(9,84,by = 5), sep = '-'), "85+")))

res %>%
  filter(year %in%  seq(1980, 2015, by = 5)) %>%
  # mutate(median = exp(median), upper = exp(upper), lower = exp(lower)) %>% 
  left_join(age_names, by = c('age' = 'x')) %>% 
  group_by(year, edu, age.group) %>% 
summarise(median = mean(median), upper = mean(upper), lower = mean(lower)) %>% 
  ggplot(aes(x = age.group, median)) +
  geom_line(aes(color = edu, group = interaction(edu,year))) + 
  geom_ribbon(aes(x = age.group,ymin = lower, ymax = upper, fill = edu, group = interaction(edu, year)), alpha = 0.5)+
  scale_fill_manual(values = c("#749AAA","#D30D45"),name = "Edu. Att.",labels = c("More than primary","No education or primary")) +
  scale_color_manual(values = c("#749AAA","#D30D45"),name = "Edu. Att.",labels = c("More than primary","No education or primary"))+
  facet_wrap(~year)+
  ggtitle("Cluster 1, female population: log mortality rates (80% C.I.)",
          subtitle = "")+
  # ylab(TeX("$log(m_x)$"))+
  xlab("age group")+
  theme_bw()+
  theme(axis.text=element_text(size=14),
        axis.text.x=element_text(angle=90),
        axis.title=element_text(size=12),
        strip.text.x = element_text(size = 14),
        strip.text.y = element_text(size = 18),
        legend.key.size = unit(1, 'cm'), #change legend key size
        legend.key.height = unit(1, 'cm'), #change legend key height
        legend.key.width = unit(1, 'cm'), #change legend key width
        legend.title = element_text(size=14), #change legend title font size
        legend.text = element_text(margin = margin(t = 10, b = 10),size=14),
        legend.spacing.x = unit(1.0, 'cm'),
        plot.title = element_text(size = 18),
        plot.subtitle = element_text(size = 15))


## specific year ----


results_1990_CI90 <- 
res %>%
  filter(year ==  1990) %>%
  

  # mutate(median = exp(median), upper = exp(upper), lower = exp(lower)) %>% 
  left_join(age_names, by = c('age' = 'x')) %>% 
  group_by(country, edu, age.group) %>% 
  summarise(median = mean(median), upper = mean(upper), lower = mean(lower)) %>% 
  
  inner_join(d %>% 
                filter(year == 1980) %>% 
               select('year', 'age_group','country' , 'edu.att', 'log.mx'),by = c(  'age.group' = 'age_group','country', 'edu' = 'edu.att')) %>% 
  ggplot(aes(x = age.group, median)) +
  geom_line(aes(color = edu, group = interaction(edu,country))) + 
  
  geom_point(aes(x = age.group,y = log.mx, fill = edu, color = edu, group = interaction(edu, country)), size = 3, alpha = 1)+
  
  geom_ribbon(aes(x = age.group,ymin = lower, ymax = upper, fill = edu, group = interaction(edu, country)), alpha = 0.5)+
  scale_fill_manual(values = c("#749AAA","#D30D45"),name = "Edu. Att.",labels = c("More than primary","No education or primary")) +
  scale_color_manual(values = c("#749AAA","#D30D45"),name = "Edu. Att.",labels = c("More than primary","No education or primary"))+
  facet_wrap(~country)+
  ggtitle("Year 1990, Cluster 1, female population: log mortality rates (90% C.I.)",
          subtitle = "")+
  ylab(TeX("$log(m_x)$"))+
  xlab("age group")+
  theme_bw()+
  theme(axis.text=element_text(size=14),
        axis.text.x=element_text(angle=90),
        axis.title=element_text(size=12),
        strip.text.x = element_text(size = 14),
        strip.text.y = element_text(size = 18),
        legend.key.size = unit(1, 'cm'), #change legend key size
        legend.key.height = unit(1, 'cm'), #change legend key height
        legend.key.width = unit(1, 'cm'), #change legend key width
        legend.title = element_text(size=14), #change legend title font size
        legend.text = element_text(margin = margin(t = 10, b = 10),size=14),
        legend.spacing.x = unit(1.0, 'cm'),
        plot.title = element_text(size = 18),
        plot.subtitle = element_text(size = 15),
        strip.background = element_rect(fill = "white", color = "black")
  )



results_2000_CI95 <- 
  res %>%
  filter(year ==  2000) %>%
  
  
  # mutate(median = exp(median), upper = exp(upper), lower = exp(lower)) %>% 
  left_join(age_names, by = c('age' = 'x')) %>% 
  group_by(country, edu, age.group) %>% 
  summarise(median = mean(median), upper = mean(upper), lower = mean(lower)) %>% 
  
  inner_join(d %>% 
               filter(year == 2000) %>% 
               select('year', 'age_group','country' , 'edu.att', 'log.mx'),by = c(  'age.group' = 'age_group','country', 'edu' = 'edu.att')) %>% 
  ggplot(aes(x = age.group, median)) +
  geom_line(aes(color = edu, group = interaction(edu,country))) + 
  
  geom_point(aes(x = age.group,y = log.mx, fill = edu, color = edu, group = interaction(edu, country)), size = 2, alpha = 1)+
  
  geom_ribbon(aes(x = age.group,ymin = lower, ymax = upper, fill = edu, group = interaction(edu, country)), alpha = 0.5)+
  scale_fill_manual(values = c("#749AAA","#D30D45"),name = "Edu. Att.",labels = c("More than primary","No education or primary")) +
  scale_color_manual(values = c("#749AAA","#D30D45"),name = "Edu. Att.",labels = c("More than primary","No education or primary"))+
  facet_wrap(~country)+
  ggtitle("Year 1990, Cluster 1, female population: log mortality rates (95% C.I.)",
          subtitle = "")+
  ylab(TeX("$log(m_x)$"))+
  xlab("age group")+
  theme_bw()+
  theme(axis.text=element_text(size=14),
        axis.text.x=element_text(angle=90),
        axis.title=element_text(size=12),
        strip.text.x = element_text(size = 14),
        strip.text.y = element_text(size = 18),
        legend.key.size = unit(1, 'cm'), #change legend key size
        legend.key.height = unit(1, 'cm'), #change legend key height
        legend.key.width = unit(1, 'cm'), #change legend key width
        legend.title = element_text(size=14), #change legend title font size
        legend.text = element_text(margin = margin(t = 10, b = 10),size=14),
        legend.spacing.x = unit(1.0, 'cm'),
        plot.title = element_text(size = 18),
        plot.subtitle = element_text(size = 15),
        strip.background = element_rect(fill = "white", color = "black")
  )




## all-results ----
all_results <- 
res %>%
  filter(year %in%  seq(1980, 2015, by = 5)) %>%
  
  # mutate(median = exp(median), upper = exp(upper), lower = exp(lower)) %>% 
  left_join(age_names, by = c('age' = 'x')) %>% 
  group_by(country, edu, age.group, year) %>% 
  summarise(median = mean(median), upper = mean(upper), lower = mean(lower)) %>% 
  
  inner_join(d %>% 
               select('year', 'age_group','country' , 'edu.att', 'log.mx', 'year'),
             by = c(  'age.group' = 'age_group','country', 'edu' = 'edu.att', "year")) %>% 
  
  dplyr::mutate(country = recode(country, "Bosnia and Herzegovina" = "Bosnia Herz.",
                                           "State of Palestine" = "Palestine", 
                                           "North Macedonia" = "N. Macedonia")) %>% 
  
  ggplot(aes(x = age.group, median)) +
  geom_line(aes(color = edu, group = interaction(edu,country))) + 
  
  geom_point(aes(x = age.group,y = log.mx, fill = edu, color = edu, group = interaction(edu, country)), size = 1)+
  
  geom_ribbon(aes(x = age.group,ymin = lower, ymax = upper, fill = edu, group = interaction(edu, country)), alpha = 0.5)+
  scale_fill_manual(values = c("#749AAA","#D30D45"),name = "Edu. Att.",labels = c("More than primary","No education or primary")) +
  scale_color_manual(values = c("#749AAA","#D30D45"),name = "Edu. Att.",labels = c("More than primary","No education or primary"))+
  facet_grid(year~country)+
  ggtitle("Cluster 1, female population: log mortality rates (95% C.I.)",
          subtitle = "")+
  ylab(TeX(""))+
  xlab("")+
  theme_bw()+
  theme(axis.text.x=element_text(angle = 90,size=5),
        axis.text.y=element_text(size=10),
        axis.title=element_text(size=6),
        strip.text.x = element_text(size = 10),
        strip.text.y = element_text(size = 18),
        legend.key.size = unit(4, 'cm'), #change legend key size
        legend.key.height = unit(1, 'cm'), #change legend key height
        legend.key.width = unit(1, 'cm'), #change legend key width
        legend.title = element_text(size=14), #change legend title font size
        legend.text = element_text(margin = margin(t = 10, b = 10),size=14),
        legend.spacing.x = unit(1.0, 'cm'),
        plot.title = element_text(size = 18),
        plot.subtitle = element_text(size = 15),
          strip.background = element_rect(fill = "white", color = "black")
        )




## the countries ----

plots_countries_res <- lapply(countries,
function(x){
  res %>% 
  filter(country == x) %>% 
  filter(year %in%  seq(1980, 2015, by = 5)) %>%
  left_join(age_names, by = c('age' = 'x')) %>% 
  group_by(year, edu, age.group) %>% 
  
  # inner_join(mort_rates_FI_stat_office %>% filter(sex == 'F'), by = c('year' = 'time', 'edu' = 'edu.att','age' )) %>%
  # filter(log.mx != -Inf) %>%
  select(year, age.group, edu, median, upper, lower,country) %>% 
    
    inner_join(d %>% 
                 select('year', 'age_group','country' , 'edu.att', 'log.mx', 'year'),
               by = c(  'age.group' = 'age_group','country', 'edu' = 'edu.att', "year")) %>% 
  arrange(edu) %>% 
  ggplot(aes(x = age.group, median)) +
  geom_line(aes(color = edu, group = interaction(edu,year))) + 
  geom_ribbon(aes(x = age.group,ymin = lower, ymax = upper, fill = edu, group = interaction(edu, year)), alpha = 0.5) + 
  geom_point(aes(x = age.group,y = log.mx, colour = edu,  group = interaction(edu, country)), size = 2, alpha = 1)+
  scale_fill_manual(values = c("#749AAA","#D30D45"),name = "Edu. Att.",labels = c("More than primary","No education or primary")) +
  scale_color_manual(values = c("#749AAA","#D30D45"),name = "Edu. Att.",labels = c("More than primary","No education or primary"))+
  facet_wrap(~year)+
  ggtitle(paste0(x,", female population: log mortality rates (95% C.I.)"),
          subtitle = "")+
  # ylab(TeX("$log(m_x)$"))+
  xlab("age group")+
  theme_bw()+
  theme(axis.text=element_text(size=14),
        axis.text.x=element_text(angle=90),
        axis.title=element_text(size=12),
        strip.text.x = element_text(size = 14),
        strip.text.y = element_text(size = 18),
        legend.key.size = unit(1, 'cm'), #change legend key size
        legend.key.height = unit(1, 'cm'), #change legend key height
        legend.key.width = unit(1, 'cm'), #change legend key width
        legend.title = element_text(size=14), #change legend title font size
        legend.text = element_text(margin = margin(t = 10, b = 10),size=14),
        legend.spacing.x = unit(1.0, 'cm'),
        plot.title = element_text(size = 18),
        plot.subtitle = element_text(size = 15),
        strip.background = element_rect(fill = "white", color = "black")
  )}

)















