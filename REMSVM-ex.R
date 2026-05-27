setwd("D:/yufeng/elvis/program/Ming")

source('REMSVM.R')

#generate a three-class linear example

###################A toy example generation

three.data <- function(n.data, sigma)
{
    
    # generate x and y
    rx<-rnorm(n.data*2, 0, sigma)
    x <- matrix(rx, n.data, 2)
    y <- rep(0, n.data)
    u <- runif(n.data)
    
    for(i in 1:n.data)
    {if(u[i]<1/3) {x[i,]<-x[i,]+c(0,2)
                   y[i]<-1}
     if(u[i]>=1/3 & u[i]<2/3) {x[i,]<-x[i,]+c(-1.732, -1)
                               y[i]<-2}
     if(u[i]>=2/3){x[i,]<-x[i,]+c(1.732, -1)
                   y[i]<-3}
     }
     return(list(x=x,y=y))
        
}



set.seed(123)
n.test<-10^4
sigma<-1.5
data.test<-three.data(n.test, sigma)

#calculate the Bayes rule
#############################

ypred.test<-rep(0, n.test)
for(i in 1:n.test)
{if(data.test$x[i,2]>(data.test$x[i,1]/1.732) & data.test$x[i,2]>(-data.test$x[i,1]/1.732)) ypred.test[i]<-1
 if(data.test$x[i,2]<=(data.test$x[i,1]/1.732) & data.test$x[i,1]>0) ypred.test[i]<-3
 if(data.test$x[i,1]<0 & data.test$x[i,2]<(-data.test$x[i,1]/1.732)) ypred.test[i]<-2
}
bayes.error<-sum(ypred.test != data.test$y)/n.test

set.seed(190029)

ntrain<-50
lam.vec<- seq(-16,15,2)


data1<-three.data(ntrain,sigma)



#tune
data2<-three.data(ntrain,sigma)

       
          
remsvm.outn5 <- remsvm(as.matrix(data1$x), as.vector(data1$y), a=2, gamma=0.5, lambda =lam.vec, epsilon.H = 1e-3, epsilon.C=1e-3,kernel.type = 'linear',
                x.test=as.matrix(data2$x), y.test=as.vector(data2$y))
                

                                
# prediction
kernel.lin <- list(type='linear')

fit.test5 <- predict.remsvm(as.matrix(data1$x), as.matrix(data.test$x), kernel.lin, remsvm.outn5$model)


# test error rate:

error5<-error.rate(data.test$y, fit.test5)

error5
bayes.error
