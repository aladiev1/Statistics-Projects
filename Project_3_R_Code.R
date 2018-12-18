# Anna Aladiev
# HK55332
# STAT 355-04, Fall 2018
# Discussion 06 (Thursday)
# Project 3

library(pwr)        # power analysis functions
library(ggplot2)    # plot creation functions

### NUMBER 1 ##############################################################################
# two-tailed z-test
z = (73.2-72.4)/(2.1/sqrt(35))   
pvalue = 2*pnorm(-abs(z))         
print(pvalue)
# reject null hypothesis because p-value < .05
###########################################################################################


### NUMBER 2 ##############################################################################
# left-tailed t-test
t = (73.2-75)/(7.9/sqrt(12))   
pvalue = pt(t,df=11)            
print(pvalue)
# fail to reject null hypothesis because p-value > .05
###########################################################################################


### NUMBER 3 ##############################################################################
# two-tailed t-test
weights<-c(66, 63, 64, 62, 65) 

t = (mean(weights)-60)/(sd(weights)/sqrt(length(weights)))
pvalue = 2*pt(-abs(t),df=length(weights)-1)
print(pvalue)
# reject null hypothesis because p-value < .05

# alternative calculation method
# t.test(weights,mu=60,alternative='two.sided')

# Boxplot
boxplot(weights,main = '#3: Boxplot')

# Normal Probability Plot
qqnorm(weights,datax=F,main = '#3: Normal Probability Plot')
###########################################################################################


### NUMBER 4 ##############################################################################
# two-tailed z-test
z = (5.4-5.2)/(sqrt(0.8)/sqrt(15))    
pvalue = 2*pnorm(-abs(z))             
print(pvalue)
# fail to reject null hypothesis because p-value > .05
###########################################################################################


### NUMBER 5 ##############################################################################
ptab<-cbind(NULL,NULL,NULL,NULL)
for (i in seq(0,0.2,0.01)){     # do not change for project
  pwrt1<-pwr.t.test(d=i/0.1,n=5,sig.level=0.05,type='one.sample',alternative='greater')
  pwrt2<-pwr.t.test(d=i/0.1,n=10,sig.level=0.05,type='one.sample',alternative='greater')  
  pwrt3<-pwr.t.test(d=i/0.1,n=15,sig.level=0.05,type='one.sample',alternative='greater')  
  ptab<-rbind(ptab,cbind(pwrt1$d,pwrt1$power,pwrt2$power,pwrt3$power))
}

require(ggplot2)
ptab<-as.data.frame(ptab)
g<-ggplot(ptab,aes(ptab[,1]))
g<-g+geom_line(aes(y=ptab[,2]), color="red")
g<-g+geom_line(aes(y=ptab[,3]), color="green")
g<-g+geom_line(aes(y=ptab[,4]), color="blue")
g<-g+xlab('Standardized Difference')+ylab('Power')
g<-g+ggtitle('#5: Power Curve
[red: n=5]
[green: n=10]
[blue: n=15]') + theme(plot.title = element_text(hjust = 0.5))
print(g)
###########################################################################################


### NUMBER 6 ##############################################################################
pvalue<-numeric()

# mu = 20
for (i in 1:10000){
  sample<-rnorm(n=4,mean=20,sd=2)     
  t<-(mean(sample)-20)/(sd(sample)/sqrt(4))
  pvalue[i]<-2*(1-pt(abs(t),df=3))
}
hist(pvalue,main='P-Values Under Null (mu = 20)')

# mu = 21
for (i in 1:10000){
  sample<-rnorm(n=4,mean=21,sd=2)     
  t<-(mean(sample)-20)/(sd(sample)/sqrt(4))
  pvalue[i]<-2*(1-pt(abs(t),df=3))
}
hist(pvalue,main='P-Values Under Alternative (mu = 21)')

# mu = 22
for (i in 1:10000){
  sample<-rnorm(n=4,mean=22,sd=2)     
  t<-(mean(sample)-20)/(sd(sample)/sqrt(4))
  pvalue[i]<-2*(1-pt(abs(t),df=3))
}
hist(pvalue,main='P-Values Under Alternative (mu = 22)')
###########################################################################################