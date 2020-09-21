#load in simulation function
source("./pandemic_sim.R")

#load in packages
library(tidyverse)

#Set up random effects

seasonal_eff <- tibble::tibble(name=c("high","low"),shape1=c(1,1),shape2=c(3,15.67))
city_eff <- tibble::tibble(name = c("city_a", "city_b"), shape1=c(1,1),shape2=c(8,20))
community_eff <- tibble::tibble(name = c("com_a", "com_b"), shape1=c(1,1),shape2=c(3,50))
#intervention_eff <- tibble::tibble(name = c("yes","no"), shape1=c(0.25,0.2),shape2=c(0,0.2))

#set the number of datasets to generate and fit models upon
repetitions = 10
#number of individuals to simulate for each set of params
num_ppl = 200
#number of runs of random effects to sample. Each run is a single high/low season pair.
runs = 6
#significance level of the test
alpha = 0.05
#preallocate vector to store results of each model
sig = rep(NA,repetitions)

start_T = proc.time()[3]
for(i in 1:repetitions){
  #generate data
  sims = purrr::rerun(runs, pandemic_sim(seasonal_eff = seasonal_eff,
                                      city_eff = city_eff,
                                      community_eff = community_eff,
                                      intervention_eff = intervention_eff,
                                      num_ppl = num_ppl))
  #bind data into single dataframe
  df = do.call(rbind,temp)
  #fit logistic model
  model = glm(disease ~., data = df, family = "binomial")
  sig[i] = summary(model)$coefficients[5,4] < alpha
}

power = sum(sig)/repetitions
#produce power function for different strengths of intervention effect


# Notes ----
#ignore below
temp = purrr::rerun(6, pandemic_sim(seasonal_eff = seasonal_eff,
                                     city_eff = city_eff,
                                     community_eff = community_eff,
                                     intervention_eff = intervention_eff,
                                     num_ppl = 200))

pandemic_data = do.call(rbind,temp) %>% as.data.frame()

#correcting variable types
pandemic_data[colnames(pandemic_data)] <- lapply(pandemic_data[colnames(pandemic_data)], factor)

#fit the LR model
pandemic.lr <- glm(disease~., data = pandemic_data, family = "binomial")

#obtain coefficient for intervention effect
summary(pandemic.lr)$coefficients[5,4] 
