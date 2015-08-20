adskip=read.csv("______", header=TRUE)

##########################
######CLEAN DATA##########
##########################

##Filter for relevant viewership (viewership within 30 days)
##Calculate total viewership per program
library(sqldf)
adskip$viewtime=strptime(adskip$VIEWTIMELOCAL, "%m/%d/%Y %I:%M:%S %p")
adskip$viewdate=as.Date(adskip$VIEWTIMELOCAL, "%m/%d/%Y %H:%M:%S")
adskip$progdate=as.Date(adskip$PROGRAMSTARTLOCAL, "%m/%d/%Y %H:%M:%S")
adskip$daydiff=as.numeric(adskip$viewdate-adskip$progdate)
adskip$rel_indicate=ifelse(adskip$daydiff<31,1,0)
adskip_relevant=adskip[which(adskip$rel_indicate==1),]

adskip_relevant=adskip_relevant[,1:18] #remove columns with date conversions since SQLDF doesn't work with POSIXit
adskip_relevant=as.data.frame(adskip_relevant)

###Sum up total viewership for each tsn & program

adskip_tot=sqldf("select TSN, ZIP, DATE_TRICKS, NETWORK_KEY, SHORTPROGRAMTITLE, PROGRAMSTARTLOCAL, SHOWDURATION, GENRE1, SUM(DURATION) TOT_DUR 
                 from adskip_relevant
                 group by TSN, ZIP, DATE_TRICKS, NETWORK_KEY, SHORTPROGRAMTITLE, PROGRAMSTARTLOCAL, SHOWDURATION, GENRE1")


############################
###CALCULATE ADSKIP RATIO###
############################

##Adskip ratio per TSN = # shows watched with adskip / # eligible & relevant shows
##First filter for eligible shows - shows where at least 10 minutes are watched
adskip_tot_eligible=sqldf("select * 
                          from adskip_tot 
                          where TOT_DUR>=600")

adskip_final=adskip_tot_eligible
##Calculate adskip_ratio denominator for each tsn based on relevant eligible ad skipping shows
adskip_denominator=sqldf("select TSN, count(SHORTPROGRAMTITLE) den
                         from adskip_final
                         group by TSN")

##Calculate numerator for Ad Skip Ratio 1: Top 20 Networks
##Top 20 networks (regardless of daypart) - just do a count distinct TSN by network on the ad.viewership table and see which networks have the most TSNs watching them. Call those the top 20.

top_nets=sqldf("select NETWORK_KEY, COUNT(DISTINCT TSN) tsns
               from adskip_final
               group by NETWORK_KEY
               order by tsns desc")
top_nets20=top_nets$NETWORK_KEY[1:20]

adskip_num1=sqldf("select TSN, count(SHORTPROGRAMTITLE) num1
                    from adskip_final
                    where NETWORK_KEY IN (40007, 40001, 40022, 40013, 11867, 11164, 10021, 14321, 11207, 10149, 14902, 12574, 10179, 10989, 10093, 10240, 14771, 10035, 11158, 40034)
                    group by TSN")
##Join with denominator
adskip_ratio1=sqldf("select adskip_denominator.TSN, den, num1
                    from adskip_denominator
                    left outer join adskip_num1
                    on adskip_denominator.TSN=adskip_num1.TSN")

adskip_ratio1$ratio1=adskip_ratio1$num1/adskip_ratio1$den

##Calculate numerator for Ad Skip Ratio 3: Top 20 Networks during primetime
##Define dayparts

adskip_final$progtime=strptime(adskip_final$PROGRAMSTARTLOCAL, format="%m/%d/%Y %I:%M:%S %p")
adskip_final$progtime=strftime(adskip_final$progtime, format="%H:%M:%S")
adskip_final$dayofweek=weekdays(as.Date(adskip_final$PROGRAMSTARTLOCAL, "%m/%d/%Y"))

library(chron)
tms=c("19:00:00", "21:00:00", "10:00:00", "6:00:00")
t=chron(time=tms)
adskip_final_primeonly=NULL

adskip_final$primetime_ind=ifelse((adskip_final$progtime<=chron(time="23:00:00") &
            adskip_final$progtime>=chron(time="20:00:00") &
            adskip_final$dayofweek!="Sunday") | 
           (adskip_final$progtime<=chron(time="23:00:00") & 
               adskip_final$progtime>=chron(time="19:00:00") &
               adskip_final$dayofweek=="Sunday"),1,0) #Prime is monday-saturday 8pm to 11pm and sunday 7pm to 11pm

adskip_final_primeonly=adskip_final[which(adskip_final$primetime==1),]

#####################
####Bottom 20########
#####################
###Ratio 1 - tsns without an adskip ratio greater than 80%

poor_experience=adskip_ratio1$TSN[which(adskip_ratio1$ratio1<0.8)]
poor_experience=as.data.frame(poor_experience)
colnames(poor_experience)=c("TSN")
poor_experience1=sqldf("select poor_experience.TSN, adskip_final.*
                        from poor_experience
                        left outer join adskip_final
                        on poor_experience.TSN=adskip_final.TSN")
write.csv(poor_experience1, "________")

###########################
####Plot distributions#####
###########################
library(ggplot2)
ggplot(adskip_ratio1, aes(x=ratio1))+geom_histogram(binwidth=.1, color="black", fill="white")




