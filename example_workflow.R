#load in simulation function
source("./pandemic_pwr_calc.R")

pacman::p_load(lme4,lmerTest)

N = seq(50,250,by=50)
#N = c(5,10)
hs = c(0.021, 0.21)
ls = c(0.006, 0.06)
cv.t = c(0.75,1,1.25) #coefficient of variation for seasons (UK surveillance data)
ntp = c(6,11,16)

pwr = lapply(N, pandemic_pwr_calc, nruns = 100,eff.size = 0.25,num_set = 3,
       num_com = 5,ntp = 6,sig.alpha = 0.05, hs = hs, ls = ls ,cv.t = cv.t )

library(doParallel)
cl <- makeCluster(4)
registerDoParallel(cl)
foreach(i=1:3) %dopar% sqrt(i)

start = Sys.time()
pwr_df = foreach(n = 1:length(N)) %:%
  foreach(tps =1:length(ntp)) %:%
  foreach(svar = 1:length(cv.t)) %:%
  foreach(h = 1:length(hs)) %:%
  foreach(l = 1:length(ls)) %dopar% {
    #N[n]+hs[h]+ls[l]
    library(lme4)
    pandemic_pwr_calc(nsam = N[n], nruns = 10,eff.size = 0.25,num_set = 3,
                      num_com = 5,ntp = ntp[tps],sig.alpha = 0.05,hs = hs[h],
                      ls = ls[l],cv.t = cv.t[svar])
  }
end = Sys.time()

temp = pwr_df

for(i in 1:4){
  temp = unlist(temp,recursive = F)
}

df=data.table::rbindlist(temp)

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
