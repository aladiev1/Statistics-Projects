# Anna Aladiev
# HK55332
# STAT 355-04, Fall 2018
# Discussion 06 (Thursday)
# Project 2

### NORMAL PROBABILITY PLOT FUNCTIONS ################################################################

# Percentiles of a Standard Normal Distribution
standard_normal_percentiles <- function(n) {
  normal_percentiles<-numeric()
  for (i in 1:n){         
    normal_percentiles[i]<-qnorm(p=(i-.5)/n,mean=0,sd=1)
  }
  return(normal_percentiles)
}

# Standard Normal Probability Plot
standard_normal_probability_plot <- function(normal_percentiles, sample) {
  plot(normal_percentiles, sort(sample), 
       xlab = 'Z Percentile',
       ylab = 'Observed Values',
       main = 'Normal Probability Plot') 
}
######################################################################################################


### NUMBER 1 #########################################################################################
size<-1000          # number of random samples
n<-40               # sample size 
mean<-3             # mean (mu)
sd<-2               # standard deviation (sigma)
z<-1.96             # phi(1.96) = .9750

indicator<-numeric()

for (i in 1:size){
  sample_1<-rnorm(n,mean,sd)                             # normal distribution
  lower_1<-mean(sample_1)-z*sd/sqrt(n)                    # lower interval bound
  upper_1<-mean(sample_1)+z*sd/sqrt(n)                    # upper interval bound
  indicator[i]<-ifelse(lower_1<mean & mean<upper_1,1,0)   # check for interval containing true mean
}

mean(indicator)             # rate of confidence interval containing true mean
######################################################################################################


### NUMBER 2 #########################################################################################
n<-100    # sample size

#2a
sample_2a<-rt(n, 1)               # t-distribution (df=1)
normal_percentiles_2a<-standard_normal_percentiles(n)
standard_normal_probability_plot(normal_percentiles_2a, sample_2a)

#2b
sample_2b<-rt(n, 10)              # t-distribution (df=10)
normal_percentiles_2b<-standard_normal_percentiles(n)
standard_normal_probability_plot(normal_percentiles_2b, sample_2b)

#2c
sample_2c<-rt(n, 100)             # t-distribution (df=100)
normal_percentiles_2c<-standard_normal_percentiles(n)
standard_normal_probability_plot(normal_percentiles_2c, sample_2c)

#2d
sample_2d<-rnorm(n,mean=0,sd=1)   # standard normal distribution
normal_percentiles_2d<-standard_normal_percentiles(n)
standard_normal_probability_plot(normal_percentiles_2d, sample_2d)
######################################################################################################


### NUMBER 3 #########################################################################################
sample_3 <- c(418,421,421,422,425,427,431,434,437,439,446,447,448,453,454,463,465)
n<-17     # data size
p<-0.975  # two-sided 95% confidence interval
t<-2.12   # alpha = .025 ; nu = 16

# Boxplot
boxplot(sample_3, main = 'Boxplot')

# Normal Probability Plot
normal_percentiles_3<-standard_normal_percentiles(n)
standard_normal_probability_plot(normal_percentiles_3, sample_3)

# Two-Sided 95% Confidence Interval
qt(df=n-1,p)
mean(sample_3)-t*sd(sample_3)/sqrt(n)    # lower interval bound
mean(sample_3)+t*sd(sample_3)/sqrt(n)    # upper interval bound
######################################################################################################

