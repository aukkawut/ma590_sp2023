library(rdd)

## these are for plotting the RDD
library(dplyr)
library(ggplot2)


print(load("lsoSynth.rda"))

str(lsoSynth)

lsoSynth$Z=ifelse(lsoSynth$R>0,1,0) ## Z is "not AP"


#### plot the RDD
lsoSynth%>%
mutate(Z=as.factor(Z))%>%
group_by(R,Z)%>%
summarize(AvgNextGPA=mean(nextGPA,na.rm=TRUE),n=n())%>%
ggplot(aes(R,AvgNextGPA,color=Z))+geom_point(aes(size=n),alpha=0.25)+geom_smooth(linewidth=2)+
geom_vline(xintercept=0)+xlab("R (centered)")

### Thistlewhite & Campbell's ANCOVA model
mod1=lm(nextGPA~R+Z,data=lsoSynth)

### Let the slopes change
mod2=lm(nextGPA~R+Z+R:Z,data=lsoSynth)

### McCrary test (are people manipulating their GPAs to avoid academic probation?)
### set bin=0.01 because GPAs are rounded to 2 digits, and there are a lot of ties
### if R were really continuous (no/few ties) leave it blank
### Cutpoint is 0 because R was centered at c
DCdensity(lsoSynth$R,cutpoint=0, bin=0.01)

### Local linear regression (with IK bandwidth)
localLin=RDestimate(nextGPA~R, cutpoint = 0  ,data=lsoSynth)
plot(localLin)
summary(localLin)

#### IV version (fuzzy regression discontinuity)

### effect of Z on W (probation_year1)
lsoSynth%>%
mutate(Z=as.factor(Z))%>%
group_by(R,Z)%>%
summarize(probation=mean(probation_year1,na.rm=TRUE),n=n())%>%
ggplot(aes(R,probation,color=Z))+geom_point(aes(size=n),alpha=0.25)+geom_smooth(linewidth=2)+
geom_vline(xintercept=0)+xlab("R (centered)")

fuzzy=RDestimate(nextGPA~R+probation_year1,cutpoint = 0,data=lsoSynth)

plot(fuzzy)
summary(fuzzy)