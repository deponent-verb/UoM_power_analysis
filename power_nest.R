
#Import packages for fitting logistic regressions with random effects
library(lme4)
library(lmerTest)

#Set study size. I have constrained these to certain values because of practicalisties of the study. "Treating" a community takes 2 months, and the project is 30 months. So, we can sample at time 0, and then every 6 months (6 total samples), every 3 months (11 total samples), or every 2 months (16 total samples). Because we can treat a commnity every two months and the study will take 30 months, the number of communities treates will be a multiple of 15.
eff.size=0.25 #anticipated effect size
s=3  #number of settlements (3 or 5)
c=10  #number of communities (5 or 10 if s=3; 3 or 6 if s=5)
n=100 #number of individuals per sample
ntp=11 #number of time points for samples (6, 11, or 16)
sp=(0:(ntp-1))*(30/(ntp-1)) #sampling time points

#create data frame with settlement, community, and treatment completion date for that community
sched=data.frame(rep(1:s,c),rep(1:c,each=s),rep(2*(1:15),each=s*c/15))
sched$id=paste(sched[,1],sched[,2],sep=".") #add column for patch ID

#create data frame with predictors at community level
pred=data.frame(rep(sp,each=s*c),rep(rep(1:s,each=c),ntp),rep(1:c,ntp*s))
colnames(pred)=c("time.point","settlement","community")
pred$id=paste(pred[,2],pred[,3],sep=".")
pred$treated=1*(sched[match(pred$id,sched$id),3]<=pred[,1])  #indicates whether the community has been treated at a given time point
pred$month=pred$time.point%%12
pred$hs=1*(pred$month<4)  #indicates whether we are in the high flu season
for (i in 1:4){
  pred[,i]=as.factor(pred[,i])  #converts predictors to factors (so R treats community ID as class rather than a number)
}

#Assign expectations. This is the key to our analysis, since our assumptions about variability determine our predicted power. First, I think the infection rates that we have are for clinic visitors, not for the population as a whole. So, I have (arbitrarily) divided by 10. I got the variation among years from the last 10 years of UK flu data at the national level. Variation among settlements matches variation among the 6 largest UK cities in COVID death rates at spring peak. Variation among communities matches variation among boroughs within Manchester and within London in COVID death rates at spring peaks. These may overestimate, because some of this variance will be due to variation between the true and expected death rates due to small sample sizes. This is fine, because overestiamting variability will make our analysis conservative.

hs=0.025#/10  #high season in population
ls=0.006#6/10  #low season in population
cv.t=0.75 #coefficient of variation for seasons (UK surveillance data)
cv.s=0.25 #coefficient of variation for settlements (UK cities)
cv.c=0.35 #coefficient of variation for communities  (Manchester and London weeks 14-20)

#illustrate beta distributions (because it would be silly to set variance so high that they become U-shaped)
a.hs=(1-(1+cv.t^2)*hs)/(cv.t^2)
b.hs=(hs-1)*(hs-1+hs*cv.t^2)/(hs*cv.t^2)
plot(dbeta((0:100)/100,a.hs,b.hs))

#create a loop to simulate data and fit models


samples=100
ps=NULL
for (i in 1:samples){

  print(i) #to track progress
  
#baseline expectation for infection rates in each time step
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
pred$expect=expect.c.r*(1-pred$treated*eff.size)

#sample n individuals from each community
pred.long=pred[rep(1:nrow(pred),each=n),]
pred.long$infect=1*(runif(nrow(pred.long))<pred.long$expect)

#try to fit the model to the data
r.glmer=glmer(infect~treated+hs+(1|time.point)+(1|time.point:settlement)+(1|time.point:settlement:community),data=pred.long,family="binomial",control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)))

#store the p-value for the fitted model
ps=c(ps,summary(r.glmer)[10][[1]][2,4])
if (summary(r.glmer)[10][[1]][2,1]>0){
  ps[length(ps)]=1  #ignore models that show a significant positive effect of treatment, because these are false positives (as we have set the effect of treatment to be negative)
}

}

#count and report the power
sum(ps<0.05)/length(ps)

