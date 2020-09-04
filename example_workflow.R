#Set up random effects

seasonal_eff <- tibble::tibble(name=c("high","low"),shape1=c(0.3,0.2),shape2=c(0.08,0.4))
city_eff <- tibble::tibble(name = c("city_a", "city_b"), shape1=c(0.2,0.2),shape2=c(0.3,0.4))
community_eff <- tibble::tibble(name = c("com_a", "com_b"), shape1=c(0.1,0.2),shape2=c(0.3,0.4))
intervention_eff <- tibble::tibble(name = c("yes","no"), shape1=c(0.1,0.2),shape2=c(0.3,0.4))

#load in simulation function
source("./pandemic_sim.R")

temp = purrr::rerun(10, pandemic_sim(seasonal_eff = seasonal_eff,
                                     city_eff = city_eff,
                                     community_eff = community_eff,
                                     intervention_eff = intervention_eff,
                                     num_ppl = 100))
