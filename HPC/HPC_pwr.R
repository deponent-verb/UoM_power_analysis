.libPaths(c("/fast/users/a1708050/local/RLibs",.libPaths()))
libs = .libPaths(c("/fast/users/a1708050/local/RLibs",.libPaths()))

library(doParallel)

slurm_ntasks <- as.numeric(Sys.getenv("SLURM_NTASKS")) # Obtain environment variable SLURM_NTASKS
if (is.numeric(slurm_ntasks)) {
  cores = slurm_ntasks # if slurm_ntasks is numerical, then assign it to cores
} else {
  cores = detectCores() # Figure out how many cores there are
}
cl<-makeCluster(cores)

setwd("/fast/users/a1708050/mphil/UoM/UoM_power_analysis/")
source("./pandemic_pwr_calc.R")

#set params
N = seq(50,250,by=50)
#N = c(5,10)
hs = c(0.021, 0.21)
ls = c(0.006, 0.06)
cv.t = c(0.75,1,1.25) #coefficient of variation for seasons (UK surveillance data)
ntp = c(6,11,16)

doParallel::registerDoParallel(cl,cores = cores)

pwr_df = foreach(n = 1:length(N)) %:%
  foreach(tps =1:length(ntp)) %:%
  foreach(svar = 1:length(cv.t)) %:%
  foreach(h = 1:length(hs)) %:%
  foreach(l = 1:length(ls)) %dopar% {

    .libPaths(libs)
    library(lme4)
    pandemic_pwr_calc(nsam = N[n], nruns = 2,eff.size = 0.25,num_set = 3,
                      num_com = 5,ntp = ntp[tps],sig.alpha = 0.05,hs = hs[h],
                      ls = ls[l],cv.t = cv.t[svar])
  }

temp = pwr_df

for(i in 1:4){
  temp = unlist(temp,recursive = F)
}

df=data.table::rbindlist(temp)
readr::write_csv(df,path="/fast/users/a1708050/mphil/UoM/UoM_power_analysis/power_df.csv")

