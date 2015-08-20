##Colbert Report + Late Show  - Data Cleaning
colbert_raw=read.csv("_______", header=TRUE, stringsAsFactors=FALSE)

##########################
######CLEAN RAW DATA##########
##########################

#Remove columns to include ONLY the ones that are necessary
keeps <- c("TSN", "NETWORK_NAME", "PROGRAMSTARTLOCAL", "VIEWTIMELOCAL", "BUCKET", "DURATION", "ZIP", "SHOWDURATION", "SHORTPROGRAMTITLE", "EPISODETITLE")
colbert <- colbert_raw[ , keeps]

#Clean up Zipcode column
library(zipcode)
colbert$ZIP <- clean.zipcodes(colbert$ZIP)

#Filter for relevant viewership
#Calculate total viewership per program
library(sqldf)
colbert$viewtime=strptime(colbert$VIEWTIMELOCAL, "%m/%d/%Y %I:%M:%S %p")
colbert$viewdate=as.Date(colbert$VIEWTIMELOCAL, "%m/%d/%Y %H:%M:%S")
colbert$progdate=as.Date(colbert$PROGRAMSTARTLOCAL, "%m/%d/%Y %H:%M:%S")
colbert$daydiff=as.numeric(colbert$viewdate-colbert$progdate)
colbert$rel_indicate=ifelse(colbert$daydiff<30,1,0)
colbert_relevant=colbert[which(colbert$rel_indicate==1),]

colbert_relevant=colbert_relevant[,1:10] #remove columns with date conversions since SQLDF doesn't work with POSIXit
colbert_relevant=as.data.frame(colbert_relevant)

#Sum up total viewership for each tsn & program
colbert_tot=sqldf("select distinct TSN, ZIP, SHORTPROGRAMTITLE, EPISODETITLE, NETWORK_NAME, PROGRAMSTARTLOCAL, VIEWTIMELOCAL, SHOWDURATION, BUCKET, SUM(DURATION) TOT_DUR 
                 from colbert_relevant
                 group by TSN, ZIP, SHORTPROGRAMTITLE, EPISODETITLE, NETWORK_NAME, PROGRAMSTARTLOCAL, VIEWTIMELOCAL, SHOWDURATION")

#Combine duplicates
library(plyr)
colbert_tot <- ddply(colbert_tot, .(TSN, ZIP, EPISODETITLE, SHORTPROGRAMTITLE), numcolwise(mean))

#Make %-Watched column
colbert_tot$Percent.Watched <- (colbert_tot$TOT_DUR / colbert_tot$SHOWDURATION) * 100


##############################
#LOYALTY DEFINITIONS
##############################
#Subset data 
loyal_fans=sqldf("select distinct TSN, ZIP, SHORTPROGRAMTITLE, COUNT(DISTINCT(EPISODETITLE)) NUM_EPS, SUM(TOT_DUR) TOTAL_DUR 
                 from colbert_tot
                 group by TSN, ZIP, SHORTPROGRAMTITLE")

#Determine the % of the season that a HH watched
ep_min <- 22
num_eps <- 40
tot_min_in_season <- (ep_min*num_eps)
tot_secs_in_season <- (tot_min_in_season*60)
loyal_fans$Percent_Season_Watched_Secs <- (loyal_fans$TOTAL_DUR/tot_secs_in_season)*100
loyal_fans$Percent_Season_Watched_Eps <- (loyal_fans$NUM_EPS/40)*100

#DEF1: Watched at least 25% of total seconds in last season
loyal_fans_def1 <- subset(loyal_fans, Percent_Season_Watched_Secs >= 25)

#DEF2: Watched at least 75% of total number of episodes in last season
loyal_fans_def2 <- subset(loyal_fans, Percent_Season_Watched_Eps >= 75)

#Remove row.names column in dataset
row.names(loyal_fans_def1) <- NULL
row.names(loyal_fans_def2) <- NULL



##############################
#MERGE DATA SETS & SORT BY TSN
##############################
#Read in "Late Show with Letterman" data 
letterman=read.csv("/Users/aiwamizu/Desktop/Work/Projects/Colbert Report/lateshow_clean.csv", header=TRUE, stringsAsFactors=FALSE)
library(sqldf)
letterman=sqldf("select distinct TSN, ZIP, SHORTPROGRAMTITLE, COUNT(DISTINCT(EPISODETITLE)) NUM_EPS, SUM(TOT_DUR) TOTAL_DUR 
                 from letterman
                 group by TSN, ZIP, SHORTPROGRAMTITLE")

#Remove row.names column in combined_data
row.names(letterman) <- NULL

#Determine the % of the season that a HH watched
letterman_ep_min <- 62
letterman_num_eps <- 149
letterman_tot_min_in_season <- (letterman_ep_min*letterman_num_eps)
letterman_tot_secs_in_season <- (letterman_tot_min_in_season*60)
letterman$Percent_Season_Watched_Secs <- (letterman$TOTAL_DUR/letterman_tot_secs_in_season)*100
letterman$Percent_Season_Watched_Eps <- (letterman$NUM_EPS/letterman_num_eps)*100

#Merge datasets
combined_def1 <- merge(loyal_fans_def1, letterman, by = "TSN")
combined_def2 <- merge(loyal_fans_def2, letterman, by = "TSN")

#Order by TSN
combined_def1 <- combined_def1[order(combined_def1$TSN),] 
combined_def2 <- combined_def2[order(combined_def2$TSN),] 

#How do loyal viewers of Colbert watch the Letterman show?
loyal_fans_def1$Letterman_Percent_Watched_Secs <- ifelse((loyal_fans_def1$TSN %in% letterman$TSN), as.numeric(letterman$Percent_Season_Watched_Secs), 0)
loyal_fans_def1$Letterman_Percent_Watched_Eps <- ifelse((loyal_fans_def1$TSN %in% letterman$TSN), as.numeric(letterman$Percent_Season_Watched_Eps), 0)
loyal_fans_def2$Letterman_Percent_Watched_Secs <- ifelse((loyal_fans_def2$TSN %in% letterman$TSN), as.numeric(letterman$Percent_Season_Watched_Secs), 0)
loyal_fans_def2$Letterman_Percent_Watched_Eps <- ifelse((loyal_fans_def2$TSN %in% letterman$TSN), as.numeric(letterman$Percent_Season_Watched_Eps), 0)


##############################
#EXPORT DATA#
##############################
#finaldata_def1 = write.csv(loyal_fans_def1, "_______")
#finaldata_def2 = write.csv(loyal_fans_def2, "________")


