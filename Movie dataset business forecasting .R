library(fpp)
library(fpp2)
attach(Moviedataset)
options(scipen=5)

#1A
plot(`7-day Gross`,`Total US Gross`,data=Moviedataset)


Moviedataset<-na.omit(Moviedataset)
cor(`7-day Gross`, `Total US Gross`)


plot(`14-day Gross`,`Total US Gross`,data=Moviedataset)
Moviedataset<-na.omit(Moviedataset)
cor(`14-day Gross`, `Total US Gross`)

#1B
oneweek<-lm(`Total US Gross`~`7-day Gross`,data=Moviedataset)
summary(oneweek)
attributes(oneweek)
oneweek$coefficients
abline(oneweek)


twoweek<-lm(`Total US Gross`~`14-day Gross`,data=Moviedataset)
summary(twoweek)
attributes(twoweek)
twoweek$coefficients
abline(twoweek)

#1C 
plot(x=Moviedataset$`7-day Gross`,y=Moviedataset$`Total US Gross`,title= residuals)
oneweek<-lm(`Total US Gross`~`7-day Gross`,data=Moviedataset)
abline(oneweek)


plot(x=Moviedataset$`14-day Gross`,y=Moviedataset$`Total US Gross`,title= residuals)
twoweek<-lm(`Total US Gross`~`14-day Gross`,data=Moviedataset)
abline(twoweek)


## cleaning the columns
Moviedataset$`7-day Gross`= as.numeric(as.character(gsub("[\\$,',']", "", Moviedataset$`7-day Gross`)))
Moviedataset$`14-day Gross`= as.numeric(as.character(gsub("[\\$,',']", "", Moviedataset$`14-day Gross`)))

## plotting with outliers
plot(x=Moviedataset$`7-day Gross`,y=Moviedataset$`14-day Gross`)

## Finding Outliers in both columns
Outliers_7day_gross <- boxplot(Moviedataset$`7-day Gross`,plot=FALSE)$out
View(Outliers_7day_gross)
Outliers_14day_gross <-  boxplot(Moviedataset$`14-day Gross`, plot=FALSE)$out
View(Outliers_14day_gross)

## Removing Outliers
Moviedataset[which(Moviedataset$`7-day Gross` %in% Outliers_7day_gross),] <- NA
Moviedataset[which(Moviedataset$`14-day Gross` %in% Outliers_14day_gross),] <- NA
Moviedataset <- na.omit(Moviedataset)

## plotting graph without outlier
withoutOutlier <- plot(x=Moviedataset$`7-day Gross`,y=Moviedataset$`14-day Gross`)

oneweek<-lm(`Total US Gross`~`7-day Gross`,data=Moviedataset)
summary(oneweek)


twoweek<-lm(`Total US Gross`~`14-day Gross`,data=Moviedataset)
summary(twoweek)


#1-d)

Moviedataset<-na.omit(Moviedataset)
Moviedataset$`Total US Gross`= as.numeric(as.character(gsub("[\\S,',']","",Moviedataset$`Total US Gross`)))
Moviedataset$`International Gross`= as.numeric(as.character(gsub("[\\S,',']","",Moviedataset$`International Gross`)))
Moviedataset$`US DVD Sales`= as.numeric(as.character(gsub("[\\S,',']","",Moviedataset$`US DVD Sales`)))

Moviedataset$totalrevenue<-Moviedataset$`Total US Gross`+ Moviedataset$`US DVD Sales` + Moviedataset$`International Gross`
totalrevenue
tr<-lm(totalrevenue~Distributor+ Genre+ `7-day Gross`+ `14-day Gross`, data= Moviedataset)
summary(tr)



View(Moviedataset)

Q1_data_2_

library(dplyr)
## Finding count of distributors to relabel them
distributorCount<-Q1_data_2_%>%group_by(Q1_data_2_$Distributor)%>%summarise(Count=n())%>%arrange(-Count)
View(distributorCount)

## Splitting distributors into Large distributors and small distributors
largeDistributors <- distributorCount%>%filter(distributorCount$Count>=12)
smallDistributors <- distributorCount%>%filter(distributorCount$Count<12)

## Relabelling the distributor column
Q1_data_2_[which(Q1_data_2_$Distributor %in% largeDistributors$`Q1_data_2_$Distributor`),3] <- "Large Distributor"
Q1_data_2_[which(Q1_data_2_$Distributor %in% smallDistributors$`Q1_data_2_$Distributor`),3] <- "Small Distributor"

## Relabelling the genre column
OtherList <- c("Black Comedy", "Documentary", "Horror", "Musical", "Romantic Comedy")
Q1_data_2_[which(Q1_data_2_$Genre %in% OtherList),4] <- "Other"





#Q-2
library(fpp)
library(fpp2)
library(ggplot2)

Storedata
View(Storedata)

#a) Scatter plot for sales vs price



#Simple Scatterplot

plot(x =Storedata$Price,y=Storedata$Sales,xlab = "Price",ylab = "Sales",main = "Price v/s Sales")

xlim = c(0.4,1.0)
ylim = c(900,1000)		 


#Q2-b)  Fit a quadratic regression model and state the quadratic regression equation

plot(Storedata$Price,Storedata$Sales)
Storedata$Price_sq=Storedata$Price^2
Storedata

output<-lm(Storedata$Sales~Storedata$Price+Storedata$Price_sq)
output
summary(output)

#qra=875+363*Price-354*Price_sq

#Q2-c Predict the mean weekly sales for a small coffee priced at 79 cents.



m= Storedata[Storedata$Price==0.79, "Sales"]
m
mean(942,937,945, 948,945,941)

#Q-d) Perform a residual analysis on the results and determine whether the regression model is valid.

library(ggplot2)
fit<-lm(Storedata$Sales~Storedata$Price+Storedata$Price_sq, data=Storedata)
summary(fit)
Storedata$Predicted<- predict(fit)
Storedata$Residuals<-residuals(fit)

plot(Storedata$Predicted, Storedata$Residuals, pch=21,bg="red",col="red")
abline(0,0)

#2-e

cor(Storedata$Price,Storedata$Sales, method="pearson")
confint(output,conf.level=0.95)

#2-F)

output2<-lm(Storedata$Sales~Storedata$Price)
output2

summary(output2)





