library(estimatr)


dat=foreign::read.dta('nsw_dw.dta')


#### regression without covariates

## usual version:
sls1=lm(re78~treat,data=dat)
summary(sls1)

## better standard error:
sls2=lm_robust(re78~treat,data=dat)
sls2

cbind(coef(sls1),coef(sls2))

### difference in means
with(dat,mean(re78[treat==1])-mean(re78[treat==0]))
t.test(re78~treat,data=dat)

###### what about with one covariate?
with(dat, plot(re75,re78))
abline(lm(re78~re75,data=dat))

### log transformation?
### what about the 0s?
### for now,
dat$learn78=log(dat$re78+1)
dat$learn75=log(dat$re75+1)
dat$learn74=log(dat$re74+1)
with(dat, plot(learn75,learn78))
abline(lm(learn78~learn75,data=dat))

### differnece in means for log earnings
lm_robust(learn78~treat,data=dat)

#### now regression
lm_robust(learn78~learn75+treat,data=dat)

### what about with all the covariates?
ols=lm_robust(learn78~treat+learn74+learn75+age+education+black+hispanic+married+nodegree,data=dat)

plot(fitted(ols),resid(ols))

### break it down (Frisch-Waugh-Lovell)
regY=lm(learn78~learn74+learn75+age+education+black+hispanic+married+nodegree,data=dat)
regZ=lm(treat~learn74+learn75+age+education+black+hispanic+married+nodegree,data=dat)
lm(resid(regY)~resid(regZ))

plot(resid(regZ),resid(regY))
abline(lm(resid(regY)~resid(regZ)))

#############################################################################
### Lin's method

datLin=dat
datLin<-within(datLin,{
  learn74<-scale(learn74)
  learn75<-scale(learn75)
  age<-scale(age)
  education<-scale(education)
  black<-scale(black)
  hispanic<-scale(hispanic)
  married<-scale(married)
  nodegree<-scale(nodegree)
})

summary(olsLin<-lm_robust(learn78~treat*(learn74+learn75+age+education+black+hispanic+married+nodegree),data=dat))

#### OOPS!
summary(olsLin<-lm_robust(learn78~treat*(learn74+learn75+age+education+black+hispanic+married+nodegree),data=datLin))

### shortcut 1
reg1=lm(learn78~learn74+learn75+age+education+black+hispanic+married+nodegree,data=dat,subset=treat==1)
reg0=lm(learn78~learn74+learn75+age+education+black+hispanic+married+nodegree,data=dat,subset=treat==0)
y1hat=predict(reg1,newdata=dat)
y0hat=predict(reg0,newdata=dat)
mean(y1hat-y0hat)
## CI:
mean(y1hat-y0hat)+t.test(residuals(reg1),residuals(reg0))$conf.int


### shortcut 2
lm_lin(learn78~treat,covariates=~learn74+learn75+age+education+black+hispanic+married+nodegree,data=dat)



#############################################################################
### Lin's method, generalized
## hacky "zero-inflated" model
## predict if learn78=0, then predict earnings for people with earnings>0
dat$learn78zero=dat$re78==0
dat$learn75zero=dat$re75==0
dat$learn74zero=dat$re74==0

reg1zero=glm(learn78zero~learn74+learn75+learn75zero+learn74zero+age+education+black+hispanic+married+nodegree,data=dat,subset=treat==1,family=binomial)
reg1=lm(learn78~learn74+learn75+age+education+black+hispanic+married+nodegree,data=dat,subset=treat==1&learn78>0)
y1hat=predict(reg1zero,newdata=dat,type='response')*predict(reg1,newdata=dat)
### make it "prediction unbiased"
y1hat=y1hat-mean(y1hat[dat$treat==1])+mean(dat$learn78[dat$treat==1])


reg0zero=glm(learn78zero~learn74+learn75+learn75zero+learn74zero+age+education+black+hispanic+married+nodegree,data=dat,subset=treat==0,family=binomial)
reg0=lm(learn78~learn74+learn75+age+education+black+hispanic+married+nodegree,data=dat,subset=treat==0&learn78>0)
y0hat=predict(reg0zero,newdata=dat,type='response')*predict(reg0,newdata=dat)
y0hat=y0hat-mean(y0hat[dat$treat==0])+mean(dat$learn78[dat$treat==0])

resid1=dat$learn78[dat$treat==1]-y1hat[dat$treat==1]
resid0=dat$learn78[dat$treat==0]-y0hat[dat$treat==0]

(tau.hat=mean(y1hat-y0hat))
(ci=tau.hat+t.test(resid1,resid0)$conf.int)


#### what about not logged?
reg1=lm(learn78~learn74+learn75+age+education+black+hispanic+married+nodegree,data=dat,subset=treat==1)
reg0=lm(learn78~learn74+learn75+age+education+black+hispanic+married+nodegree,data=dat,subset=treat==0)
y1hat=exp(predict(reg1,newdata=dat))
y0hat=exp(predict(reg0,newdata=dat))
## debias
y1hat=y1hat-mean(y1hat[dat$treat==1]-dat$re78[dat$treat==1])
y0hat=y0hat-mean(y0hat[dat$treat==0]-dat$re78[dat$treat==0])

resid1=y1hat[dat$treat==1]-dat$re78[dat$treat==1]
resid0=y0hat[dat$treat==0]-dat$re78[dat$treat==0]

(tau.hat=mean(y1hat-y0hat))
(ci=tau.hat+t.test(resid1,resid0)$conf.int)
t.test(re78~treat,data=dat)



#install.packages('devtools')
#devtools::install_github("adamSales/rebarLoop/sim code/loop.estimator")

## or download loop.estimator_1.0.0.tar.gz from Examples folder and install that way

library(loop.estimator)
X=model.matrix(~learn74+learn75+age+education+black+hispanic+married+nodegree,data=dat)
X=X[,-1]
tau.loop=loop(dat$re78,dat$treat,X,p=.41)



##############################################################
### Hansen & Bowers method for binary outcome

### preliminary, to make the method work: manufacture binary outcome---do they have non-zero earnings in 1978
dat$nonZero=dat$re78>0

### step 1: use logistic regression to model Y as a function of covariates in control group
mod=glm(nonZero~age+education+black+hispanic+married+nodegree+I(re74==0)+I(re75==0)+learn74+learn75,data=dat,subset=treat==0,family=binomial)

### step 2: get predicted outcomes for the whole sample (trt & ctl)
ycPred=predict(mod,dat,type="response")  ## type="response" gives predictions as probabilities, not log-odds

### step 3: estimate "attributable effect": total change in Y due to treatment
(ae=sum(dat$nonZero-ycPred))

## (this is the same as just looking at the treatment group...why?)
(ae1=sum(dat$nonZero[dat$treat==1]-ycPred[dat$treat==1]))

### confidence interval:
### step 4: get residuals in ctl group
resids=ycPred[dat$treat==0]-dat$nonZero[dat$treat==0]

## check that they sum to zero
sum(resids)

### step 5: get confidence interval for attributable effect
ae+nrow(dat)*t.test(resids)$conf.int

#[1] -5.221168 45.547534
## interpreation: treatment could have caused up to 5 people to have zero in come or caused up to 45 people to have non-zero income, or anything in-between



