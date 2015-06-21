# Illustrate via simulation and associated explanatory text the properties of the distribution of the mean of 40 exponentials.  You should
# 1. Show the sample mean and compare it to the theoretical mean of the distribution.
# 2. Show how variable the sample is (via variance) and compare it to the theoretical variance of the distribution.
# 3. Show that the distribution is approximately normal.

# In point 3, focus on the difference between the distribution of a large collection of random exponentials and the distribution of a large collection of averages of 40 exponentials. 

#Statistical Inference Project 
hist(rexp(40, 0.2), main="Exponential")


layout(matrix(c(1,1,2,2), 2, 2, byrow = TRUE))
mne = NULL
for (i in 1 : 1000) mne = c(mne, mean(rexp(40, 0.2)))
h<-hist(mne, main ="Mean 40 obs")
h
abline(v=5,lwd=3)

mne = NULL
for (i in 1 : 1000) mne = c(mne, mean(rexp(400, 0.2)))
hist(mne, main ="Mean 400 obs")
abline(v=25,lwd=3)

layout(matrix(c(1,1,2,2), 2, 2, byrow = TRUE))
vre = NULL
for (i in 1 : 1000) vre = c(vre, var(rexp(40, 0.2)))
hist(vre)

vre = NULL
for (i in 1 : 1000) vre = c(vre, var(rexp(400, 0.2)))
hist(vre)

abline(v=25,lwd=3)

x<-mne
xfit<-seq(min(x),max(x),length=40) 
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x)) 
yfit <- yfit*diff(h$mids[1:2])*length(x) 
lines(xfit, yfit, col="blue", lwd=2)
#########################################################################
#########################################################################

##################
#ToothGrowth Data#
##################

data(ToothGrowth)
str(ToothGrowth)
summary(ToothGrowth)

tapply(ToothGrowth$len,ToothGrowth$sup,mean)

boxplot(len~supp,d=ToothGrowth)
plot(ToothGrowth$dose,ToothGrowth$len) 
plot(ToothGrowth$sup,ToothGrowth$len)
 
#T test for the difference in mean in pig with VC and OJ. 
VC<-subset(ToothGrowth,supp=='VC')
OJ<-subset(ToothGrowth,supp=='OJ')

Sx<-var(VC$len)
Sy<-var(OJ$len)

sp<-sqrt((29*Sx^2 + 29*Sy^2)/(30+30-2))
20.66333-16.96333+c(-1,1)*qt(.975,58)*sp*(1/30 + 1/30)^0.5


#O is in The confidence interval is in so with a certainty of 95% there is no 
#Statistical difference between the pigs with OR and the pigs with VC.

#Assumptions are : 
#Data from pics is iid normal distribuited. 
#Pigs with OJ and pigs with VC are from independent groups.


#Using R 
t.test(Sxlen,Sy$dose)
