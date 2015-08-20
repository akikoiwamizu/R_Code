##DigitalSmiths Search Integration
##Proportion of searches: Text vs. Contextual
##Author: Akiko Iwamizu, Data Scientist at TiVo

raw_data=read.csv("/Users/aiwamizu/Desktop/Work/Projects/Search Integration/SEARCH_INTEGRATION2.txt", header=TRUE, stringsAsFactors=FALSE)

##########################
######CLEAN RAW DATA##########
##########################

#Remove columns to include ONLY the ones that are necessary
keeps <- c("EVENT_DATE_KEY", "TSN", "SEARCH_STARTING_TIMESTAMP", "SEARCH_TEXT", "UI_VALUE_DESC", "SEARCH_CURRENT_TIMESTAMP", "SEARCH_EVENT_TYPE", "DISPLAY_NAME", "SEARCH_RESULT_CATEGORY")
clean_data <- raw_data[ , keeps]

#Subset only successful searches
new_data <- clean_data[which(clean_data$SEARCH_RESULT_CATEGORY == 'SUCCESS'), ]

#Search Title or Contextual?
new_data$SEARCH_TEXT <- tolower(new_data$SEARCH_TEXT)
new_data$UI_VALUE_DESC <- tolower(new_data$UI_VALUE_DESC)
new_data$SEARCH_CONTEXT <- sapply(new_data$UI_VALUE_DESC, grepl, new_data$SEARCH_TEXT)
  #ifelse(new_data$SEARCH_TEXT %in% new_data$UI_VALUE_DESC, TRUE, FALSE)
  
  #grepl(new_data$SEARCH_TEXT, new_data$UI_VALUE_DESC, fixed = TRUE)

#Proportion count
table(new_data$SEARCH_CONTEXT)

