#' Pandemic_pwr_calc function
#' 
#' Computes the power of detecting an intervention using a random mixed effects model. The
#' number of communities are constrained for practical reasons. Treating" a community takes 2 months, and the project is 30 months. 
#  So, we can sample at time 0, and then every 6 months (6 total samples), 
#  every 3 months (11 total samples), or every 2 months (16 total samples).
#' 
#' @param nruns : Number of runs to do for estimating power
#' @param nsam : Number of individuals to sample for each 
#' @param eff.size : A float indicating the reduction of the infection rate due to the intervention.
#' @param num_set : Number of settlements, nested within each community. (3 or 5)
#' @param num_com : Number of communities. (5 or 10 if s=3; 3 or 6 if s=5)
#' @param ntp : Number of timepoints to take samples. (6, 11, 16)
#' @param sig.alpha : Significance level of the test
#' @param hs: coefficient for high season effect
#' @param ls: coefficient for low season effect
#' @param cv.t: coefficient of variation for seasons 
#'
#' @return The power of a random mixed effects model for detecting the intervention.
#' @export
#'
#' @examples pandemic_pwr_calc(nruns = 10,nsam = 10,eff.size = 0.25,num_set = 3,num_com = 10,ntp = 11,sig.alpha = 0.05)
pandemic_pwr_calc <- function (nruns, nsam, eff.size, num_set, 
                               num_com, ntp, sig.alpha,
                               hs, ls, cv.t){

  s=num_set  #number of settlements (3 or 5)
  c=num_com  #number of communities (5 or 10 if s=3; 3 or 6 if s=5)
  
  #initialise sampling timepoints (in months)
  sp=(0:(ntp-1))*(30/(ntp-1)) #sampling time points
  
  #Because we can treat a commnity every two months and the study will take 30 months, 
  #the number of communities treated will be a multiple of 15.
  
  #create data frame with settlement, community, and treatment completion date for that community
  sched = data.frame(settlement = rep(1:s,c), community = rep(1:c,each=s),
                     treatment_comp_date = rep(2*(1:15),each=s*c/15))
  sched$id = paste(sched[,1],sched[,2],sep=".")
  
  #create data frame with predictors at community level
  pred=data.frame(time.point = rep(sp,each=s*c),settlement = rep(rep(1:s,each=c),ntp),community = rep(1:c,ntp*s))
  pred$id=paste(pred[,2],pred[,3],sep=".")
  pred$treated=1*(sched[match(pred$id,sched$id),3]<=pred[,1])  #indicates whether the community has been treated at a given time point
  pred$month=pred$time.point%%12
  pred$hs=1*(pred$month<4)  #indicates whether we are in the high flu season
  pred[colnames(pred)[1:4]] <- lapply(pred[colnames(pred)[1:4]],factor)
  
  #Assign expectations. This is the key to our analysis, since our assumptions about 
  # variability determine our predicted power. First, I think the infection rates that we 
  # have are for clinic visitors, not for the population as a whole. So, I have 
  # (arbitrarily) divided by 10. I got the variation among years from the last 10 years 
  # of UK flu data at the national level. Variation among settlements matches variation 
  # among the 6 largest UK cities in COVID death rates at spring peak. Variation among 
  # communities matches variation among boroughs within Manchester and within London in 
  # COVID death rates at spring peaks. These may overestimate, because some of this variance
  # will be due to variation between the true and expected death rates due to small sample
  # sizes. This is fine, because overestiamting variability will make our analysis conservative.
  
  cv.s=0.25 #coefficient of variation for settlements (UK cities)
  cv.c=0.35 #coefficient of variation for communities  (Manchester and London weeks 14-20)
  
  #store p-values
  ps=rep(NA,nruns)
  
  for(i in 1:nruns){
    
    print(i) #to track progress
    
    #baseline expectation for infection rates in each time step, first 4 months is hs
    expect.t=(hs-ls)*((sp%%12)<4)+ls
    #expectation with randomness for seasons
    expect.t.r=rbeta(length(expect.t),shape1=(1-(1+cv.t^2)*expect.t)/(cv.t^2),shape2=(expect.t-1)*(expect.t-1+expect.t*cv.t^2)/(expect.t*cv.t^2))
    expect.t.r[which(expect.t.r>.5)]=.5 #sloppy approach to avoiding NAs for high infection rates (Sylvia's)
    #expectation with randomness for settlements
    expect.s=rep(expect.t.r,each=s)
    expect.s.r=rbeta(length(expect.s),shape1=(1-(1+cv.s^2)*expect.s)/(cv.s^2),shape2=(expect.s-1)*(expect.s-1+expect.s*cv.s^2)/(expect.s*cv.s^2))
    expect.s.r[which(expect.s.r>.5)]=.5 #sloppy approach to avoiding NAs for high infection rates (Sylvia's)
    #expectation with randomness for communities
    expect.c=rep(expect.s.r,each=c)
    expect.c.r=rbeta(length(expect.c),shape1=(1-(1+cv.c^2)*expect.c)/(cv.c^2),shape2=(expect.c-1)*(expect.c-1+expect.c*cv.c^2)/(expect.c*cv.c^2))
    
    #attach these to the predictor matrix, impose treatment, and expand to sample 
    #(modify infection rate by intervention effect)
    pred$expect=expect.c.r*(1-pred$treated*eff.size)
    
    #sample n individuals from each community
    pred.long=pred[rep(1:nrow(pred),each=nsam),]
    pred.long$infect=1*(runif(nrow(pred.long))<pred.long$expect)
    
    #try to fit the model to the data
    r.glmer=lme4::glmer(infect~treated+hs+(1|time.point)+(1|time.point:settlement)+(1|time.point:settlement:community),data=pred.long,family="binomial",control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)))
    
    #store the p-value for the fitted model
    p_val = summary(r.glmer)[10][[1]][2,4]
    if(summary(r.glmer)[10][[1]][2,1]>0){
      #ignore models that show a significant positive effect of treatment, 
      #because these are false positives (as we have set the effect of treatment to be negative)
      ps[i] = 1
    } else {
      ps[i] = p_val
    }
  }
  power = sum(ps<sig.alpha)/length(ps)
  df = tibble::tibble(hs = hs, ls = ls, season_var = cv.t, 
                      sig = sig.alpha, sample_freq = ntp , 
                      nsam=nsam,power = power)
  return(df)
}