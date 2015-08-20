##DigitalSmiths Search Integration
##Proportion of searches: Text vs. Contextual
##Author: Akiko Iwamizu, Data Scientist at TiVo

raw_data=read.csv("_______", header=TRUE, stringsAsFactors=FALSE)

##########################
######CLEAN RAW DATA##########
##########################

#Remove columns to include ONLY the ones that are necessary
keeps <- c("EVENT_DATE_KEY", "TSN", "SEARCH_STARTING_TIMESTAMP", "SEARCH_TEXT", "UI_VALUE_DESC", "SEARCH_CURRENT_TIMESTAMP", "SEARCH_EVENT_TYPE", "DISPLAY_NAME", "SEARCH_RESULT_CATEGORY")
clean_data <- raw_data[ , keeps]

#Search Title or Contextual?
clean_data$Search_Title <- ifelse(clean_data$SEARCH_TEXT %in% clean_data, TRUE, FALSE)


