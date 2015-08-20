ds.search<-read.csv("______")
all.search<-read.csv("______")

ds.search$Date <- as.Date(substr(ds.search[, 1], 1, 10)) #creating a date field
all.search$Date <- as.Date(substr(all.search[, 1], 1, 10))

ds.search_day<-ds.search[which(ds.search$Date=="2015-07-29"),] #774
all.search_day<-all.search[which(all.search$Date=="2015-07-29"),] #16664

##Analysis of one week##
hist(all.search$DUR)
plot(all.search$DUR)

hist(all.search[which(all.search$DUR<5000),]$DUR)
hist(ds.search[which(ds.search$DUR<5000),]$DUR)
subset.all=all.search[which(all.search$DUR<5000),]
subset.ds=ds.search[which(ds.search$DUR<5000),]

x.all<-seq(min(subset.all$DUR), max(subset.all$DUR), length=5000)
x.ds<-seq(min(subset.ds$DUR), max(subset.ds$DUR), length=5000)
all.fit<-dnorm(x.all, mean=mean(subset.all$DUR),sd=sd(subset.all$DUR))
ds.fit<-dnorm(x.ds, mean=mean(subset.ds$DUR),sd=sd(subset.ds$DUR))
hist(all.search[which(all.search$DUR<5000),]$DUR)

##Analysis of one day##
ds.search_day<-ds.search[which(ds.search$Date=="2015-07-26"),] 
all.search_day<-all.search[which(all.search$Date=="2015-07-26"),]

plot(density(ds.search_day$DUR), main="Density Function of All vs. DS (7/26)", col="red")
lines(density(all.search_day$DUR), col="black")
legend(58000, 0.0013, c("All", "DS"), lty=c(1,1), lwd=c(2.5,2.5), col=c("black", "red"))

ds.search_day_fast=ds.search_day[which(ds.search_day$DUR<5000),]
all.search_day_fast=all.search_day[which(all.search_day$DUR<5000),]
dim(ds.search_day_fast)[1]/dim(ds.search_day)[1]
dim(all.search_day_fast)[1]/dim(all.search_day)[1]

plot(density(all.search_day_fast$DUR), main="Density Function of All vs. DS (7/26)")
lines(density(ds.search_day_fast$DUR), col="red")

#Normalize and test
plot(density(sqrt(ds.search_day_fast$DUR)), main="Normalized Density Function of All vs. DS (7/26)", col="red")
lines(density(sqrt(all.search_day_fast$DUR)), col="black") #not advisable, but will proceed anyway
tds.search_day_fast=sqrt(ds.search_day_fast$DUR)
tall.search_day_fast=sqrt(all.search_day_fast$DUR)

t.test(tds.search_day_fast, tall.search_day_fast)

#Time plot
all.search_day_fast$DateTime <- strptime(substr(all.search_day_fast[, 1], 1, 19), format = "%Y-%m-%dT%H:%M:%S", tz = "UTC") #creating a datetime field
ds.search_day_fast$DateTime <- strptime(substr(ds.search_day_fast[,1],1,19), format ="%Y-%m-%dT%H:%M:%S", tz = "UTC")

library(ggplot2)
ggplot(data=all.search_day_fast, aes(DateTime, DUR))+geom_line()+geom_step(data=ds.search_day_fast, col="red") + ggtitle("Time Series Chart")

#Count searches per hour
ds.search_day<-ds.search[which(ds.search$Date=="2015-08-05"),] 
all.search_day<-all.search[which(all.search$Date=="2015-08-05"),]
ds.search_day_fast=ds.search_day[which(ds.search_day$DUR<5000),]
all.search_day_fast=all.search_day[which(all.search_day$DUR<5000),]
dim(ds.search_day_fast)[1]/dim(ds.search_day)[1]
dim(all.search_day_fast)[1]/dim(all.search_day)[1]

all.search_day_fast$Hour<-substr(all.search_day_fast[,1],12,13)
ds.search_day_fast$Hour<-substr(ds.search_day_fast[,1],12,13)
ds.count=aggregate(ds.search_day_fast$DUR, by=list(ds.search_day_fast$Hour), FUN=length)
all.count=aggregate(all.search_day_fast$DUR, by=list(all.search_day_fast$Hour), FUN=length)
colnames(ds.count)=c("Hour", "Count")
colnames(all.count)=c("Hour", "Count")
ds.count=as.data.frame(ds.count)
all.count=as.data.frame(all.count)
ds.count$Hour=as.numeric(ds.count$Hour)
all.count$Hour=as.numeric(all.count$Hour)

ggplot(data=all.count, aes(Hour, Count))+geom_line()+geom_step(data=ds.count, col="red") + ggtitle("Count per Hour")
ggplot(data=ds.count, aes(Hour, Count))+geom_line()
ggplot(data=all.count, aes(Hour, Count))+geom_line()

###Distribution of times over 5000
ds.search_day<-ds.search[which(ds.search$Date=="2015-07-30"),] 
all.search_day<-all.search[which(all.search$Date=="2015-07-30"),]
ds.search_day_fast=ds.search_day[which(ds.search_day$DUR>5000),]
all.search_day_fast=all.search_day[which(all.search_day$DUR>5000),]
dim(ds.search_day)[1]
dim(all.search_day)[1]
dim(ds.search_day_fast)[1]/dim(ds.search_day)[1]
dim(all.search_day_fast)[1]/dim(all.search_day)[1]
