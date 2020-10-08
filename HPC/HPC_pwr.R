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

