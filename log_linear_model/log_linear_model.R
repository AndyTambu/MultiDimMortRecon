library('rstanarm')
library(tidyverse)

###################################
##################################



# the Rstan version of one example ####

bayesplot::color_scheme_set("viridis")


load('log_lin_data_region.RData')





log.lin.data.region
keys <- seq(1978,2015) # the abbreviations you want to replace
vals <- c(rep(1975,2),
          rep(seq(1980,2010, by = 5), each = 5),
          2015)
keysvals <- setNames(vals, keys) # create named vector

log.lin.data.region$'period' <- recode(log.lin.data.region$year,!!!keysvals)





model_Bayesian_test_Review1 <- stan_glm(deaths.new ~ year+
                                                      sex+
                                           edu.att*region+
                                             edu.att*year+
                                              region*year+
                                               region*sex+
                                                 year*sex,
                                            data = log.lin.data.region, family = 'poisson', 
                                            offset = log.pop,
                                            chains = 4,
                                            iter = 10000)






model_Bayesian_test_Review1_noTimeInter <- stan_glm(deaths.new ~ year+
                                          sex+
                                          edu.att*region+
                                          region*sex,
                                        data = log.lin.data.region, family = 'poisson', 
                                        offset = log.pop,
                                        chains = 4,
                                        iter = 1000)


model_Bayesian_test_Review1_noTimeInter
library('rstanarm')
library(tidyverse)

###################################
##################################



# the Rstan version of one example ####

bayesplot::color_scheme_set("viridis")


load('log_lin_data_region.RData')





log.lin.data.region
keys <- seq(1978,2015) # the abbreviations you want to replace
vals <- c(rep(1975,2),
          rep(seq(1980,2010, by = 5), each = 5),
          2015)
keysvals <- setNames(vals, keys) # create named vector

log.lin.data.region$'period' <- recode(log.lin.data.region$year,!!!keysvals)





model_Bayesian_test_Review1 <- stan_glm(deaths.new ~ year+
                                          sex+
                                          edu.att*region+
                                          edu.att*year+
                                          region*year+
                                          region*sex+
                                          year*sex,
                                        data = log.lin.data.region, family = 'poisson', 
                                        offset = log.pop,
                                        chains = 4,
                                        iter = 10000)





time_taken <- system.time({
  
model_Bayesian_test_Review1_noTimeInter <- stan_glm(deaths.new ~ year+
                                                      sex+
                                                      edu.att*region+
                                                      region*sex,
                                                    data = log.lin.data.region,
                                                    family = 'poisson', 
                                                    offset = log.pop,
                                                    chains = 4,
                                                 iter = 5000)})

time_taken

max(model_Bayesian_test_Review1_noTimeInter$stan_summary[,'Rhat'])

max(model_Bayesian_test_Review1$stan_summary[,'Rhat'])
min(model_Bayesian_test_Review1$stan_summary[,'n_eff'])
model_Bayesian_test_Review1$stan_summary





summary(model_Bayesian_test_Review1)
save(model_Bayesian_test_Review1_noTimeInter, file = 'model_Bayesian_test_Review1_noTimeInter.RData')



# here is what we can have a look at from the PPC perspective, this is the model with the longer n.iter (10000) and with 4 chains


model_Bayesian_test_notimeInter

yrep <- posterior_predict(model_Bayesian_test_notimeInter, draws = 5000)
ppc_dens_overlay(log.lin.data.region$deaths.new, yrep[1:50,])

ppc_hist(log.lin.data.region$deaths.new, yrep[1:5,])


ppc_scatter(log.lin.data.region$deaths.new, matrix(colMeans(yrep),nrow=1))

ppc_stat(log.lin.data.region$deaths.new, yrep, stat = 'mean')
ppc_stat(log.lin.data.region$deaths.new, yrep, stat = 'sd')

ppc_stat(log.lin.data.region$deaths.new, yrep, stat = 'max')


pp_check(model_Bayesian_test_notimeInter, plotfun = "stat_2d", stat = c('mean', 'sd'), offset = model_Bayesian_test_notimeInter$offset )

pp_check(model_Bayesian_test_notimeInter, plotfun = "intervals")
sd(log.lin.data.region$deaths.new)







max(model_Bayesian_test_Review1$stan_summary[,'Rhat'])
min(model_Bayesian_test_Review1$stan_summary[,'n_eff'])
model_Bayesian_test_Review1$stan_summary





summary(model_Bayesian_test_Review1)
save(model_Bayesian_test_Review1, file = 'model_Bayesian_test_Review1.RData')



# here is what we can have a look at from the PPC perspective, this is the model with the longer n.iter (10000) and with 4 chains


model_Bayesian_test_notimeInter

yrep <- posterior_predict(model_Bayesian_test_notimeInter, draws = 5000)
ppc_dens_overlay(log.lin.data.region$deaths.new, yrep[1:50,])

ppc_hist(log.lin.data.region$deaths.new, yrep[1:5,])


ppc_scatter(log.lin.data.region$deaths.new, matrix(colMeans(yrep),nrow=1))

ppc_stat(log.lin.data.region$deaths.new, yrep, stat = 'mean')
ppc_stat(log.lin.data.region$deaths.new, yrep, stat = 'sd')

ppc_stat(log.lin.data.region$deaths.new, yrep, stat = 'max')


pp_check(model_Bayesian_test_notimeInter, plotfun = "stat_2d", stat = c('mean', 'sd'), offset = model_Bayesian_test_notimeInter$offset )

pp_check(model_Bayesian_test_notimeInter, plotfun = "intervals")
sd(log.lin.data.region$deaths.new)





