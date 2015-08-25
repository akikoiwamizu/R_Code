##DigitalSmiths Search Integration
##Proportion of searches: Text vs. Contextual
##Author: Akiko Iwamizu, Data Scientist at TiVo

raw_data=read.csv("/Users/aiwamizu/Desktop/Work/Projects/Search Integration/SEARCH_INTEGRATION2.txt", header=TRUE, stringsAsFactors=FALSE)

##########################
######CLEAN RAW DATA##########
##########################

#Remove columns to include ONLY the ones that are necessary
keeps <- c("EVENT_DATE_KEY", "TSN", "SEARCH_TEXT", "UI_VALUE_DESC", "SEARCH_RESULT_CATEGORY")
clean_data <- raw_data[ , keeps]

#Subset only successful searches
successful_searches <- clean_data[which(clean_data$SEARCH_RESULT_CATEGORY == 'SUCCESS'), ]
successful_searches$SEARCH_RESULT_CATEGORY <- NULL
successful_searches$SEARCH_TEXT <- tolower(successful_searches$SEARCH_TEXT)
successful_searches$UI_VALUE_DESC <- tolower(successful_searches$UI_VALUE_DESC)
successful_searches$SEARCH_TEXT <- strsplit(as.character(successful_searches$SEARCH_TEXT),' ', fixed=TRUE)
#successful_searches$UI_VALUE_DESC <- strsplit(as.character(successful_searches$UI_VALUE_DESC),' ', fixed=TRUE)

#Search Title or Contextual?

#Splitting search result into separate columns in order to compare the search text to each word in the result
library(splitstackshape)
expanded_search_list <- cSplit(successful_searches, "UI_VALUE_DESC", sep = " ", "wide")

#Attempting a match up between search text and search result
expanded_search_list$SEARCH_CONTEXT <- (grepl(expanded_search_list$SEARCH_TEXT, expanded_search_list$UI_VALUE_DESC_01, ignore.case = TRUE, fixed = FALSE)
                                               | grepl(expanded_search_list$SEARCH_TEXT, expanded_search_list$UI_VALUE_DESC_02, ignore.case = TRUE, fixed = FALSE)
                                               | grepl(expanded_search_list$SEARCH_TEXT, expanded_search_list$UI_VALUE_DESC_03, ignore.case = TRUE, fixed = FALSE)
                                               | grepl(expanded_search_list$SEARCH_TEXT, expanded_search_list$UI_VALUE_DESC_04, ignore.case = TRUE, fixed = FALSE))
                                              
                                              
#Manually fix the negative false results
neg_false <- subset(expanded_search_list, expanded_search_list$SEARCH_CONTEXT == FALSE)
neg_false$SEARCH_CONTEXT <- (pmatch(neg_false$SEARCH_TEXT, neg_false$UI_VALUE_DESC_01, nomatch = FALSE, duplicates.ok = TRUE)
                             | pmatch(neg_false$SEARCH_TEXT, neg_false$UI_VALUE_DESC_02, nomatch = FALSE, duplicates.ok = TRUE)
                             | pmatch(neg_false$SEARCH_TEXT, neg_false$UI_VALUE_DESC_03, nomatch = FALSE, duplicates.ok = TRUE)
                             | pmatch(neg_false$SEARCH_TEXT, neg_false$UI_VALUE_DESC_04, nomatch = FALSE, duplicates.ok = TRUE)
                             | pmatch(neg_false$SEARCH_TEXT, neg_false$UI_VALUE_DESC_05, nomatch = FALSE, duplicates.ok = TRUE)
                             | grepl(neg_false$SEARCH_TEXT, neg_false$UI_VALUE_DESC_01, ignore.case = TRUE, fixed = FALSE)
                             | grepl(neg_false$SEARCH_TEXT, neg_false$UI_VALUE_DESC_02, ignore.case = TRUE, fixed = FALSE)
                             | grepl(neg_false$SEARCH_TEXT, neg_false$UI_VALUE_DESC_03, ignore.case = TRUE, fixed = FALSE)
                             | grepl(neg_false$SEARCH_TEXT, neg_false$UI_VALUE_DESC_04, ignore.case = TRUE, fixed = FALSE)
                             | grepl(neg_false$SEARCH_TEXT, neg_false$UI_VALUE_DESC_05, ignore.case = TRUE, fixed = FALSE)
                             | (neg_false$SEARCH_TEXT %in% neg_false$UI_VALUE_DESC_01)
                             | (neg_false$SEARCH_TEXT %in% neg_false$UI_VALUE_DESC_02)
                             | (neg_false$SEARCH_TEXT %in% neg_false$UI_VALUE_DESC_03)
                             | (neg_false$SEARCH_TEXT %in% neg_false$UI_VALUE_DESC_04)
                             | (neg_false$SEARCH_TEXT %in% neg_false$UI_VALUE_DESC_05)
                             )


#Proportion count
table(expanded_search_list$SEARCH_CONTEXT)
table(neg_false$SEARCH_CONTEXT)

#Sum the number of title and contextual searches & find the percentages of total successful searches
title_searches <- sum(length(which(expanded_search_list$SEARCH_CONTEXT == TRUE)), length(which(neg_false$SEARCH_CONTEXT == TRUE)), na.rm = TRUE)
(title_searches/106721) * 100
((106721 - title_searches)/106721) * 100
