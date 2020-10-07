#load in simulation function
source("./pandemic_pwr_calc.R")

pacman::p_load(lme4,lmerTest)

N = c(50,200)
hs=0.025#/10  #high season in population
ls=0.006#6/10  #low season in population
cv.t=0.75 #coefficient of variation for seasons (UK surveillance data)

pwr = lapply(N, pandemic_pwr_calc, nruns = 100,eff.size = 0.25,num_set = 3,
       num_com = 5,ntp = 6,sig.alpha = 0.05, hs = hs, ls = ls ,cv.t = cv.t )

#ignore below ----

#load in packages
library(tidyverse)

#Set up random effects

seasonal_eff <- tibble::tibble(name=c("high","low"),shape1=c(1,0.08),shape2=c(7/3,11.5))
city_eff <- tibble::tibble(name = c("city_a", "city_b"), shape1=c(1,1),shape2=c(8,20))
community_eff <- tibble::tibble(name = c("com_a", "com_b"), shape1=c(1,1),shape2=c(3,50))


#set the number of datasets to generate and fit models upon
num_sims = 50
#number of individuals to simulate for each set of params
num_ppl = 100
#number of runs of random effects to sample. Each run is a single high/low season pair.
runs = 6
#significance level of the test
alpha = 0.05
#vector of intervention effect sizes to try
effs = seq(0,0.9,by=0.1)

startT=Sys.time()
results = lapply(effs, function(eff){power_calc(seasonal_eff = seasonal_eff,city_eff = city_eff,community_eff = community_eff,intervention_eff = eff,num_ppl = num_ppl,runs = runs,sig = sig,num_sim = num_sims)} )
endT=Sys.time()
power_results = data.table::rbindlist(results)


# Notes ----
#ignore below

#preallocate vector to store results of each model
sig = rep(NA,repetitions*length(effs))
eff_size = rep(NaN, repetitions*length(effs))
results = tibble::tibble(sig,eff_size)

start_T = proc.time()[3]
#I will turn use lapply for this later
k = 1
for(eff in effs){
  for(i in 1:repetitions){
    #generate data
    sims = purrr::rerun(runs, pandemic_sim(seasonal_eff = seasonal_eff,
                                           city_eff = city_eff,
                                           community_eff = community_eff,
                                           intervention_eff = tibble::tibble(name = c("yes","no"), value = c(eff,1)),
                                           num_ppl = num_ppl))
    #bind data into single dataframe
    df = do.call(rbind,sims)
    #fit logistic model
    model = glm(disease ~., data = df, family = "binomial")
    results[k,]$sig = summary(model)$coefficients[5,4] < alpha
    results[k,]$eff_size = eff
    k = k + 1
  }
}
endT= proc.time()[3]
endT-start_T

power = sum(sig)/repetitions
#produce power function for different strengths of intervention effect

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
