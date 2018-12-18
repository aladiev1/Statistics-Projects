#NUMBER 1
sample_means_normal<-numeric()
sample_sds_normal<-numeric()
for(i in 1:1000){                                   #generate 1000 random samples from normal
  sample_normal<-rnorm(n=40,mean=3,sd=2)            #each sample is of size 40, mean 3, S.D. 2
  sample_means_normal[i]<-mean(sample_normal)       #store each mean in a vector
  sample_sds_normal[i]<-sd(sample_normal)           #store each standard deviation in a vector
}

sample_means_normal[1]                              #mean of first sample 
sample_sds_normal[1]                                #S.D. of first sample 

mean(sample_means_normal)                           #mean of 1000 sample means
sd(sample_means_normal)                             #S.D. of 1000 sample means
hist(sample_means_normal)                           #histogram of 1000 sample means
 

#NUMBER 2
sample_means_bin<-numeric()
sample_sds_bin<-numeric()
for (i in 1:1000){                                  #generate 1000 random samples from Bin(10,0.15) dist.
  sample_bin<-numeric()    
  for (j in 1:15){                                  #generate each binomial sample as a sum of Bernoulli random variables  
    ber_sample<-rbinom(n=10,size=1,prob=0.15)       #each sample is of size 10, probability 0.15
    sample_bin[j]<-sum(ber_sample)                  #sum of Bernoulli r.v.s is Binomial
  }
  sample_means_bin[i]<-mean(sample_bin)             #store each mean in a vector
  sample_sds_bin[i]<-sd(sample_bin)                 #store each S.D. in a vector
}

sample_means_bin[1]                                 #mean of first sample
sample_sds_bin[1]                                   #S.D. of first sample

mean(sample_means_bin)                              #mean of 1000 sample means
sd(sample_sds_bin)                                  #S.D. of 1000 sample means
hist(sample_means_bin)                              #histogram of 1000 sample means


#NUMBER 3
sample_means_bin2<-numeric()
sample_sds_bin2<-numeric()
for (i in 1:1000){                                   #generate 1000 random samples from Bin(10,0.15) dist.
  sample_bin2<-numeric()    
  for (j in 1:120){                                  #generate each binomial sample as a sum of Bernoulli random variables  
    ber_sample2<-rbinom(n=10,size=1,prob=0.15)       #each sample is of size 10, probability 0.15
    sample_bin2[j]<-sum(ber_sample2)                 #sum of Bernoulli r.v.s is Binomial
  }
  sample_means_bin2[i]<-mean(sample_bin2)            #store each mean in a vector
  sample_sds_bin2[i]<-sd(sample_bin2)                #store each S.D. in a vector
}

sample_means_bin2[1]                                 #mean of first sample
sample_sds_bin2[1]                                   #S.D. of first sample

mean(sample_means_bin2)                              #mean of 1000 sample means
sd(sample_sds_bin2)                                  #S.D. of 1000 sample means
hist(sample_means_bin2)                              #histogram of 1000 sample means

