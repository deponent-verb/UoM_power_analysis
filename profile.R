#profiling script to access computational time

source("./pandemic_pwr_calc.R")
pacman::p_load(lme4,lmerTest,profvis)

profvis({
  pandemic_pwr_calc(nruns = 10,nsam = 30,eff.size = 0.25,num_set = 3,
                    num_com = 10,ntp = 11,sig.alpha = 0.05)
})
