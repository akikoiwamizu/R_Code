#######################################
#Engineering Group Comparison#
#Author: Akiko Iwamizu, Data Scientist#
#######################################
#######STB#########
alviso_stb=read.csv("_______", sep="\t", header=TRUE, stringsAsFactors=FALSE)
iasa_stb=read.csv("_______", sep="\t", header=TRUE, stringsAsFactors=FALSE)

#######STB 10k#########
#alviso_10k=read.csv("_____", sep="\t", header=TRUE, stringsAsFactors=FALSE)
#iasa_10k=read.csv("_____", sep="\t", header=TRUE, stringsAsFactors=FALSE)

#######MOBILE#########

#######PORTAL#########

#######SERVICE#########

##########################
######RESOURCES & URLS##########
##########################
#http://yatani.jp/teaching/doku.php?id=hcistats:datatransformation

##########################
######PLOT DATA##########
##########################
plot(alviso_stb$Employee, alviso_stb$LOC)
plot(iasa_stb$Employee, iasa_stb$LOC)

##########################
######DETERMINE DISTRIBUTION##########
##########################
library(fitdistrplus)
library(logspline)
descdist(alviso_stb$LOC, discrete = FALSE)
descdist(iasa_stb$LOC, discrete = FALSE)

####################################################
#EXPONENTIAL?? Bartlett's goodness-of-fit test for exponential distribution
require(vcd)
require(MASS)
#fit1 <- fitdistr(alviso_stb$LOC, "exponential")
#fit2 <- fitdistr(iasa_stb$LOC, "exponential")

#goodness of fit test
# if p-value > 0.05, then distribution is NOT rejected
# is p-value <= 0.05, then distribution is rejected
#ks.test(alviso_stb$LOC, "pexp", fit1$estimate) #p-value < 2.2e-16
#ks.test(iasa_stb$LOC, "pexp", fit2$estimate) #p-value = 0.02676
#EXPONENTIAL DISTRIBUTION POSSIBLY REJECTED?

#LINK: http://artax.karlin.mff.cuni.cz/r-help/library/Renext/html/gofExp.test.html
library(Renext)
exp <- gofExp.test(alviso_stb$LOC)
print(exp)

#plot
hist(alviso_stb$LOC, freq = FALSE, breaks = 30, xlim = c(0, quantile(alviso_stb$LOC, 0.99)))
curve(dexp(x, rate = fit1$estimate), col = "red", add = TRUE)

####################################################
####################################################
#WEIBULL?
fitdistr(alviso_stb$LOC, "weibull")
fitdistr(iasa_stb$LOC, "weibull")

#ks-test for weibull distribution
ks.test(alviso_stb$LOC, "pweibull", scale = 3.399280e+03, shape = 4.159260e-01) #p-value = 0.2423
ks.test(iasa_stb$LOC, "pweibull", scale = 55.9453803, shape = 0.4947085) #p-value = 0.9569
#WEIBULL DISTRIBUTION NOT REJECTED

fit.weibull <- fitdist(alviso_stb$LOC, "weibull")
plot(fit.weibull)

####################################################
####################################################
#ks-test for normal distribution
ks.test(alviso_stb$LOC, "pnorm", mean = mean(alviso_stb$LOC), sd = sd(alviso_stb$LOC)) #p-value < 2.2e-16
#NORMAL DISTRIBUTION REJECTED#
####################################################



##########################
######CLEAN RAW DATA##########
##########################

#TRANSFORM DATA TO NORMAL DISTRIBUTION#
exponential.model <- lm(log(alviso_stb$Employee)~log(alviso_stb$LOC))
summary(exponential.model)

transform_log <- log(alviso_stb$LOC)
results_log <- t.test(transform_log)
print(results_log)
plot(alviso_stb$Employee, transform_log)

transform_sqrt <- sqrt(alviso_stb$LOC)
results_sqrt <- t.test(transform_sqrt)
print(results_sqrt)
plot(alviso_stb$Employee, transform_sqrt)

transform_log2 <- log(iasa_stb$LOC)
results_log2 <- t.test(transform_log2)
print(results_log2)
plot(iasa_stb$Employee, transform_log2)

transform_sqrt2 <- sqrt(iasa_stb$LOC)
results_sqrt2 <- t.test(transform_sqrt2)
print(results_sqrt2)
plot(iasa_stb$Employee, transform_sqrt2)

##########################
######TOST for TESTING EQUIVALENCE##########
##########################
library(equivalence)
tost(alviso_stb$LOC, iasa_stb$LOC, alpha = 0.05, epsilon = 1)
#tost(alviso_mobile$LOC, iasa_mobile$LOC, alpha = 0.05, epsilon = 1)


##########################
######GRAPHING RAW DATA POINTS#######
##########################
plot(alviso_stb$Employee, alviso_stb$LOC, main="Alviso STB", type = "l",xlab = "Employee (s)", 
     ylab = "Lines of Code", pch=16)



##########################
######TESTING FOR NORMALITY USING SHAPIRO-WILK TEST######
##########################
#Null Hypothesis: That the LOC follow NORMALITY. p-value < 0.05, so we REJECT the Null Hyp ==> Data is NOT normal
shapiro.test(transform_log) #p-value = 0.004
shapiro.test(transform_log2) #p-value = 0.7886
shapiro.test(transform_sqrt) #p-value < 2.2e-16
shapiro.test(transform_sqrt2) #p-value = 0.001

##########################
######TOST for TESTING EQUIVALENCE##########
##########################
library(equivalence)
tost(transform_sqrt, transform_sqrt2, alpha = 0.05, epsilon = 1)

##########################
######TESTING FOR EQUALITY USING MANN-WHITNEY TEST######
##########################
library(stats)
# independent 2-group Mann-Whitney U Test
wilcox.test(alviso_stb$LOC, iasa_stb$LOC) 

##########################
######TRANSFORM EXPONENTIAL TO NORMAL DISTRIBUTION######
##########################
#LOG TRANSFORMATION
# log_iasa <- log(iasa_stb)
# log_alviso <- log(alviso_10k)
# 
# #SQUARE ROOT TRANSFORMATION
# sqrt_iasa <- sqrt(iasa_stb)
# sqrt_alviso <- sqrt(alviso_10k)

##########################
######GRAPHING NEW DATA POINTS#######
##########################
# plot(log_alviso$Employee, log_alviso$LOC, main="Alviso STB", type = "l",xlab = "Employee (s)", 
#      ylab = "Lines of Code", pch=16)
# shapiro.test(log_alviso$LOC)
# 
# plot(sqrt_alviso$Employee, sqrt_alviso$LOC, main="Alviso STB", type = "l",xlab = "Employee (s)", 
#      ylab = "Lines of Code", pch=16)
# shapiro.test(sqrt_alviso$LOC)
