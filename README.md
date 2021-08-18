# STA314-Kaggle-Competition
library(plotmo) # nicer plotting of glmnet output

library(glmnet) # this contains procedures for fitting ridge and lasso

library(ISLR)  

library(coefplot)

library(pls) 

library(leaps) # this contains the function regsubsets

library(MASS)


library(gam)

library(caret)


dat1<-read.csv("SampleSubmission.csv")

dat2<-read.csv("test_predictors.csv")

dat3<-read.csv("trainingdata.csv")

# ----------------ridge, LASSO ------------   #


dat4<-dat3[,-1]

dat4<-dat4[,-53] ## just to remove ones to compare

dat4<-dat4[,-62]  ## just to remove ones to compare

dat4<-dat4[,-101]  ## just to remove ones to compare, but we should add these

pr<-as.data.frame(dat3[,-1])

x = model.matrix(y ~ . ,pr)[,-1]  

y = dat3$y

grid = 10^seq(10,-2,length = 100) 

set.seed(1)

train = sample(1:nrow(x),nrow(x)/2) 

test = (-train)

ytest = y[test]


# fit ridge and lasso models

fit.ri = glmnet(x[train,],y[train],alpha =0, lambda = grid)

fit.la = glmnet(x[train,],y[train],alpha =1, lambda = grid)



# do cross-validation

# this is implemented for ridge and lasso in glmnet

set.seed(1)

cv.la = cv.glmnet(x[train,],y[train],alpha =1, lambda = grid) # cross validation for ridge

plot(cv.la)



cv.la$lambda.min

# lambda.min is by cross-validation

cv.la$lambda.1se

# lambda.1se is obtained from one standard error rule
grid = 10^seq(10,-2,length = 100) 


lasso.mod = glmnet(x,y,alpha = 1, lambda = grid)# nlambda=100

plot(lasso.mod, label = TRUE,xvar = 'lambda', cex=0.5)

abline(v=cv.la$lambda.min, add=T)

abline(v=cv.la$lambda.1se, add=T)


plot_glmnet(lasso.mod)

#####################################

## must look to observe this plot ##

#####################################

coefpath(lasso.mod)

which(coef(lasso.mod)[,85]>0)

# negative side also matters for lambda


## Ridge 

set.seed(1)

cv.ri = cv.glmnet(x[train,],y[train],alpha =0, lambda = grid) # cross validation for ridge

plot(cv.ri)

cv.ri$lambda.min

cv.ri$lambda.1se

# Ridge

grid = 10^seq(10,-2,length = 100) 

ridge.mod = glmnet(x,y,alpha = 0, lambda = grid)# nlambda=100

plot(ridge.mod, label = TRUE,xvar = 'lambda')

abline(v=cv.ri$lambda.min, add=T)

abline(v=cv.ri$lambda.1se, add=T)

plot_glmnet(ridge.mod)

coefpath(ridge.mod)

which(coef(ridge.mod)[,2]>0)


# ------------------- Spline to select df ------------------ #

## smoothing spline for df

# first variable

fit1 = smooth.spline(dat3$X23,dat3$y,cv=TRUE)

fit1

plot(dat3$X23, dat3$y, cex = .5, col= 'darkgrey')

lines(fit1 ,col ="red ",lwd =2)

fit11 = smooth.spline(dat3$X23,dat3$y,df = 200) # I find this adequate

lines(fit11 ,col =" blue",lwd =2)

# second variable

fit2 = smooth.spline(scale(dat3$X44),dat3$y,cv=TRUE)

fit2

plot(scale(dat3$X44), dat3$y, cex = .5, col= 'darkgrey')

lines(fit2 ,col ="red ",lwd =2)

fit22 = smooth.spline(dat3$X44,dat3$y,df = 3)

lines(fit22 ,col =" blue",lwd =2) ## -> 3

# third variable

fit3 = smooth.spline(scale(dat3$X47),dat3$y,cv=TRUE)

fit3

plot(scale(dat3$X47), dat3$y, cex = .5, col= 'darkgrey')

lines(fit3 ,col ="red ",lwd =2)


fit33 = smooth.spline(scale(dat3$X47),dat3$y,df = 3)

lines(fit33 ,col =" blue",lwd =2) ## -> 3

# 4th variable
fit4 = smooth.spline(scale(dat3$X50),dat3$y,cv=TRUE)

fit4

plot(scale(dat3$X50), dat3$y, cex = .5, col= 'darkgrey')

lines(fit4 ,col ="red ",lwd =2)

fit44 = smooth.spline(scale(dat3$X50),dat3$y,df = 12)

lines(fit44 ,col =" blue",lwd =2) ## -> 12

# 5th variable

fit5 = smooth.spline(dat3$X74,dat3$y,cv=TRUE)

fit5

plot(dat3$X60, dat3$y, cex = .5, col= 'darkgrey')

lines(fit5 ,col ="red ",lwd =2)

fit55 = smooth.spline(dat3$X60,dat3$y,df = 3)

lines(fit55 ,col =" blue",lwd =2) ## -> 12


# 6th variable

fit6 = smooth.spline(scale(dat3$X90),dat3$y,cv=TRUE)

fit6

plot(scale(dat3$X90), dat3$y, cex = .5, col= 'darkgrey')

lines(fit6 ,col ="red ",lwd =2)

fit66 = smooth.spline(scale(dat3$X90),dat3$y,df = 5)

lines(fit66 ,col =" blue",lwd =2) ## -> 5

# 7th variable

fit7 = smooth.spline(dat3$X101,dat3$y,cv=TRUE)

fit7

plot(dat3$X101, dat3$y, cex = .5, col= 'darkgrey')

lines(fit7 ,col ="red ",lwd =2)

fit77 = smooth.spline(dat3$X101,dat3$y,df = 2) # just 0,1

lines(fit77 ,col =" blue",lwd =2) 

# 8th variable

fit8 = smooth.spline(dat3$X5, dat3$y, cv=TRUE)

fit8

plot(dat3$X5, dat3$y, cex = .5, col= 'darkgrey')

lines(fit8 ,col ="red ",lwd =2)

fit88 = smooth.spline(dat3$X53,dat3$y,df = 3) # just 0,1

lines(fit88 ,col =" blue",lwd =2) 

# 9th variable

fit9 = smooth.spline(dat3$X87, dat3$y, cv=TRUE)

fit9

plot(dat3$X87, dat3$y, cex = .5, col= 'darkgrey')

lines(fit9 ,col ="red ",lwd =2)

fit99 = smooth.spline(dat3$X87,dat3$y,df = 23) # just 0,1

lines(fit99 ,col =" blue",lwd =2) 

# 10th variable

fit10 = smooth.spline(dat3$X54, dat3$y, cv=TRUE)

fit10

plot(dat3$X54, dat3$y, cex = .5, col= 'darkgrey')

lines(fit10 ,col ="red ",lwd =2)

fit101 = smooth.spline(dat3$X40,dat3$y,df = 3) # just 0,1

lines(fit101 ,col =" blue",lwd =2) 

# 11th variable

fit11 = loessMod10 <- loess(dat3$y ~ dat3$X74, data=dat3, span=0.1) 

fit11

plot(dat3$X74, dat3$y, cex = .5, col= 'darkgrey')

lines(fit11 ,col ="red ",lwd =2)

fit111 = smooth.spline(dat3$X74,dat3$y,df = 21) # just 0,1

lines(fit101 ,col =" blue",lwd =2) 



# ------------------ GAM model ------------------------- #

gam04 = gam(y~s(X47,3)+s(X50,12)+s(X95,3)+s(X90,5)+s(X44,3)
+s(X23,20)+s(X74,3)+X101+X53,data=dat3)

# data prepare

fin2<-as.data.frame(cbind(dat3[1800:2000,]$X47, 

dat3[1800:2000,]$X50, dat3[1800:2000,]$X95, 

dat3[1800:2000,]$X90, dat3[1800:2000,]$X44,

dat3[1800:2000,]$X23, dat3[1800:2000,]$X74, 

dat3[1800:2000,]$X101, dat3[1800:2000,]$X53))

colnames(fin2)<-c('X47','X50',"X95", 'X90', 'X44', 'X23', 'X74',
'X101', 'X53')

yl4<- predict(gam04, fin2,type="response") 


# visual comparison

par(mfrow=c(1,3))
hist(dat3[1800:2000,]$y, breaks=10); hist(yl4)

# MSE
mean((dat3[1800:2000,]$y-yl4)^2)


######################## more ############################

# second trial
gam04 = gam(y~s(X47,3)+s(X50,12)+s(X95,3)+s(X90,5)+s(X44,3)
+s(X23,20)+s(X74,3)+X101,data=dat3[200:2000,])


gam04 = gam(y~s(X47,3)+s(X50,12)+s(X95,3)+s(X90,5)+s(X44,3)
+s(X23,20)+s(X74,3)+X101+X53,data=dat3[1:1800,])

gam04 = gam(y~s(X47,3)+ns(X50,6)+s(X95,3)+s(X90,5)+s(X44,3)
+ns(X23,15)+s(X74,3)+s(X30,3)+s(X60,3)+X53+X101,
data=dat3[200:2000,])



tr<-1:1800

t<-1800:2000

t0<-1:200

##########################################################

gam04 = gam(y~s(X47,2)+s(X50,2)+s(X95,2)+s(X90,2)+s(X44,2)+s(X69,2)
+ns(X23,18)+s(X74,2)+s(X30,2)+s(X60,2)+X101+X32+ns(X54,4),
data=dat3[tr,])

# best

ols_step_both_aic(gam04, details = TRUE)


gam04 = gam(y~s(X47,2)+s(X50,2)+s(X95,2)+s(X90,2)+s(X44,2)+s(X69,2)
+ns(X23,18)+s(X74,2)+X101+ns(X32,4)+ns(X54,4)+X56,data=dat3[tr,])

# best

##########################################################

gam04 = gam(y~X53:s(X47,2)+s(X50,2)+s(X95,2)+s(X90,2)+s(X44,2)+s(X69,2)
+ns(X23,18)+scale(X74)+s(X30,2)+s(X60,2)+s(X107,2)+
X101+s(X92,2)+scale(X112),

data=dat3[tr,])

##########################################################
gam04 = gam(y~X53:s(X47,2)+s(X50,2)+s(X95,2)+s(X90,2)+s(X44,2)+s(X69,2)
+ns(X23,18)+s(X30,2)+s(X60,2)+s(X107,2)+X101+s(X92,2)+X112:X74,

data=dat3[tr,])


gam04 = gam(y~X53:X47+X50+X95+X90+X44+X69+ns(X23,18)+
X30+X60+X107+X101+X92+X112:X74,

data=dat3[tr,])


# data prepare
fin2<-as.data.frame(cbind(dat3[t,]$X47, dat3[t,]$X50, dat3[t,]$X95, 
dat3[t,]$X90, dat3[t,]$X44, dat3[t,]$X23, dat3[t,]$X74, 
dat3[t,]$X101, dat3[t,]$X53, dat3[t,]$X30, dat3[t,]$X60, 
dat3[t,]$X74, dat3[t,]$X69, dat3[t,]$X107, dat3[t,]$X112,
dat3[t,]$X92, dat3[t,]$X32, dat3[t,]$X42, dat3[t,]$X54, 
dat3[t,]$X56, dat3[t,]$X5))

colnames(fin2)<-c('X47','X50',"X95", 'X90', 'X44', 'X23', 'X74',
'X101', 'X53', 'X30', 'X60', 'X74', 'X69', 'X107', 'X112', 'X92', 'X32', 
'X42', 'X54', 'X56', 'X5')

yl4<- predict(gam04, fin2,type="response") 

# visual comparison

par(mfrow=c(1,3))

hist(dat3[t,]$y, breaks=10); hist(yl4)

# MSE

mean((dat3[t,]$y-yl4)^2)

plot(yl4,dat3[t,]$y)

# linear model

#yl4<-(dat3[t,]$y-yl4)

da<-as.data.frame(cbind(dat3$y[t], yl4))

lin = plsr(V1~yl4, data=da ,scale =TRUE ,validation ="CV")

# to select number of components via CV, use following function

validationplot(pls.fit, val.type = "MSEP")

# linear

lin = lm(V1~yl4, da)

# gam
da<-as.data.frame(cbind(dat3$y[t], yl4))

lin = gam(V1~s(yl4,4), data=da)

# gam model
f = smooth.spline(da$V1, da$yl4, cv=TRUE)

f

plot(da$yl4, da$V1, cex = .5, col= 'darkgrey')

lines(f ,col ="red ",lwd =2)

f2 = smooth.spline(da$V1, da$yl4,df = 6) # just 0,1

lines(f2 ,col =" blue",lwd =2) 



new<-as.data.frame(yl4)

colnames(new)<-c('yl4')

yr<-predict(lin, newdata=new)



mean((dat3[t,]$y-yr)^2)

# ------------ 2nd trial submission data ------------ #
# data formation 

dd<-as.data.frame(cbind(dat2$X47,dat2$X50,dat2$X95,
dat2$X53,dat2$X69,dat2$X107, dat2$X60, dat2$X44, dat2$X23, 
dat2$X74, dat2$X101, dat2$X90, dat2$X30, dat2$X92, dat2$X101, 
dat2$X32, dat2$X54, dat2$X56))

colnames(dd)<-c('X47','X50','X95',
'X53', 'X69', 'X107', 'X60', 'X44', 'X23',
 'X74', 'X101','X90', 'X30', 'X92', 'X101', 'X32','X54', 'X56')

# spline

gam01=gam(y~s(X47,3)+s(X50,12)+s(X95,3)+s(X90,5)+s(X44,3),
data=dat3)

# second trial
gam04 = gam(y~s(X47,3)+s(X50,12)+s(X95,3)+s(X90,5)+s(X44,3)
+s(X23,20)+s(X74,3)+X101,data=dat3)

# third trial
gam04 = gam(y~X53:s(X47,2)+s(X50,2)+s(X95,2)+s(X90,2)+s(X44,2)+s(X69,2)
+ns(X23,18)+X74+s(X30,2)+s(X60,2)+s(X107,2)+X101,
data=dat3)


# 4th trial
gam04 = gam(y~X53:s(X47,2)+s(X50,2)+s(X95,2)+s(X90,2)+s(X44,2)+s(X69,2)
+ns(X23,18)+X74+s(X30,2)+s(X60,2)+s(X107,2)+X101+s(X92,2),
data=dat3)

# 5th trial
gam04 = gam(y~s(X47,2)+s(X50,2)+s(X95,2)+s(X90,2)+s(X44,2)+s(X69,2)
+ns(X23,18)+s(X74,2)+s(X30,2)+s(X60,2)+X101+ns(X32,4)+ns(X54,4),
data=dat3)

# 6th trial
gam04 = gam(y~s(X47,2)+s(X50,2)+s(X95,2)+s(X90,2)+s(X44,2)+s(X69,2)
+ns(X23,18)+s(X74,2)+X101+ns(X32,4)+ns(X54,4)+
X56,data=dat3)



# data prepare

yl4<- predict(gam04, dat3,type="response")


# linear model

da<-as.data.frame(cbind(dat3$y, yl4))

#lin = lm(V1~yl4, da)

lin = gam(V1~s(yl4,20), data=da)

# prediciton value

yy<- predict(gam04,dd,type="response") 

new<-as.data.frame(yy)

colnames(new)<-c('yl4')

yr<-predict(lin, newdata=new)



# data

dat<-as.data.frame(yr)

colnames(dat)<-c('y')

head(dat)

# must add 'id' in excel

write.csv(dat,'submission5.csv')


