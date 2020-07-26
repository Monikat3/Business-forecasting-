#Q.1

plot(jitter(Usage)~jitter(Temperature,xlab="Temperature",ylab="Usage",data=bf)
      bf<-lm(Usage~Temperature, data=bf )
      abline(bf)
      summary(bf)

     
     
     
#Q.2
      

     
     
     
     
     
     
     Q.3) 

plot(jitter(age)~jitter(fev),xlab="age",ylab="Forced Expiratory Volume", data=fevdata)
     tb<-lm(age~fev, data = fevdata )
     abline(tb)
     summary(tb)




     
# 4)
 
 y<-lm(fev~age + ht, data = fevdata)
  plot(y)
  summary(y)
  
  
  #Q.5)
  library(fpp)
library(fpp2)
  d1<-ts(c(123,130,125,138), start=c(2000,1),frequency=4)
  d2<-ts(c(123,130,125,138,123,130,125,138),frequency=4)
  d1fit1<-meanf(d1, h=8)
  d1fit1
  d1fit2<-rwf(d1, h=8)
  d1fit2
  d2fit3<-snaive(d2, h=8)
  d2fit3
  
  
 
  
  
  

  autoplot(bicoal)+
    autolayer(meanf(bicoal, h=10),
              series="Mean", PI=FALSE) +
    autolayer(naive(bicoal, h=10),
              series="Naive", PI=FALSE) +
    autolayer(snaive(bicoal, h=10),
              series="Seasonal Naive", PI=FALSE) +
    autolayer(rwf(bicoal, h=10, drift=TRUE),
              series="Drift", PI=FALSE) +
    ggtitle("Annual bitumious coal production in the USA") +
    xlab("Year") + ylab("Coal Production") +
    guides(colour=guide_legend(title="Forecast"))
  #autocorelation:
  ggAcf(bicoal)
  
  
  #Question 7B
  autoplot(chicken)+
    autolayer(meanf(chicken, h=10),
              series="Mean", PI=FALSE) +
    autolayer(naive(chicken, h=10),
              series="Naive", PI=FALSE) +
    autolayer(snaive(chicken, h=10),
              series="Seasonal Naive", PI=FALSE) +
    autolayer(rwf(chicken, h=10, drift=TRUE),
              series="Drift", PI=FALSE) +
    ggtitle("Price of Chicken") +
    xlab("Year") + ylab("Price") +
    guides(colour=guide_legend(title="Forecast"))
  #autocorelation:
  ggAcf(chicken)
  
  