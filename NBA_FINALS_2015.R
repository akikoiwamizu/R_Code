##NBA Finals Reporting - Infographic - Data
rawdata=read.csv("_______", sep = "\t", header=FALSE, stringsAsFactors=FALSE)
nba_schedule=read.csv("_______", sep = "\t", header=TRUE, stringsAsFactors=FALSE)

##########################
######CLEAN RAW DATA##########
##########################

##Remove columns to include ONLY the ones that are necessary
keeps <- c("V3", "V5", "V6", "V7", "V8", "V9", "V11", "V13", "V14", "V15", "V16", "V22")
nba <- rawdata[ , keeps]

##Make the first row the header for columns
names(nba) = as.character(unlist(nba[1, ]))
nba = nba[-1, ]

##Filter for relevant viewership
##Calculate total viewership per program
library(sqldf)
nba$viewtime=strptime(nba$VIEWTIMELOCAL, "%m/%d/%Y %I:%M:%S %p")
nba$viewdate=as.Date(nba$VIEWTIMELOCAL, "%m/%d/%Y %H:%M:%S")
nba$progdate=as.Date(nba$PROGRAMSTARTLOCAL, "%m/%d/%Y %H:%M:%S")
nba$daydiff=as.numeric(nba$viewdate-nba$progdate)
nba$rel_indicate=ifelse(nba$daydiff<30,1,0)
nba_relevant=nba[which(nba$rel_indicate==1),]

nba_relevant=nba_relevant[,1:12] #remove columns with date conversions since SQLDF doesn't work with POSIXit
nba_relevant=as.data.frame(nba_relevant)

###Sum up total viewership for each tsn & program
nba_tot=sqldf("select TSN, ZIP, SHORTPROGRAMTITLE, EPISODETITLE, PROGRAMSTARTLOCAL, SHOWDURATION, NETWORK_NAME, BUCKET, SUM(DURATION) TOT_DUR 
                 from nba_relevant
                 group by TSN, ZIP, SHORTPROGRAMTITLE, EPISODETITLE, PROGRAMSTARTLOCAL, SHOWDURATION, NETWORK_NAME, BUCKET")

nba_tot$GAMEDATE=as.Date(nba_tot$PROGRAMSTARTLOCAL, "%m/%d/%Y %H:%M:%S")

########################################################
##Joining Tables//Organizing Data
########################################################
nba_tot$Series <- NA
nba_tot$Visitors <- NA
nba_tot$Home <- NA
nba_tot$Visitors.Score <- NA
nba_tot$Home.Score <- NA
nba_tot$Point.Diff <- NA
nba_tot$Percent.Diff <- NA
nba_tot$Region <- NA
nba_tot$Round <- NA

#Fill out Series
nba_tot$Series[is.na(nba_tot$Series)] <- nba_schedule$Series[match(nba_tot$EPISODETITLE[is.na(nba_tot$Series)],nba_schedule$Game.Name)]
is.na(nba_tot$Series) <- NA

#Fill out Team Names
nba_tot$Visitors[is.na(nba_tot$Visitors)] <- nba_schedule$Visitors[match(nba_tot$EPISODETITLE[is.na(nba_tot$Visitors)],nba_schedule$Game.Name)]
nba_tot$Home[is.na(nba_tot$Home)] <- nba_schedule$Home[match(nba_tot$EPISODETITLE[is.na(nba_tot$Home)],nba_schedule$Game.Name)]


#Fill out Team Scores
########################
#nba_tot$Visitors.Score[is.na(nba_tot$Visitors.Score)] <- nba_schedule$Visitors.Score[(match(nba_tot$PROGRAM_DATE_KEY_PORTAL, nba_schedule$Game.Date) & (match(nba_tot$EPISODETITLE, nba_schedule$Game.Name)))]
#nba_tot$Home.Score[is.na(nba_tot$Home.Score)] <- nba_schedule$Home.Score[(match(nba_tot$Game.Date, nba_schedule$Game.Date) & (match(nba_tot$EPISODETITLE, nba_schedule$Game.Name)))]
########################


#Fill out Region
nba_tot$Region[is.na(nba_tot$Region)] <- nba_schedule$Region[match(nba_tot$EPISODETITLE[is.na(nba_tot$Region)],nba_schedule$Game.Name)]

#Fill out Round
nba_tot$Round[is.na(nba_tot$Round)] <- nba_schedule$Round[match(nba_tot$EPISODETITLE[is.na(nba_tot$Round)],nba_schedule$Game.Name)]

#Remove irrelevant rows
nba_tot <- nba_tot[!(nba_tot$SHORTPROGRAMTITLE!="NBA BASKETBALL"),]
nba_tot <- nba_tot[!is.na(nba_tot$Series),]


##Export data
cleandata=write.csv(nba_tot, "________")


