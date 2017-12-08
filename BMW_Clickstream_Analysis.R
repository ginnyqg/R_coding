library(data.table)

Oct_2_2017 <- fread('/Users/qinqingao/Desktop/Columbia/Contest/Data/Oct2 sample.csv')
#Read 21258 rows and 503 (of 503) columns from 0.117 GB file in 00:00:03

View(Oct_2_2017)

dim(Oct_2_2017)
#[1] 21258   503


#Data selection
df <- data.frame(Oct_2_2017)
library(dplyr)


#####################
#  post_visid_type  #
#####################

#Take a look what's in there before cleaning
unique(df$post_visid_type)

#Select post_visid_type value of 0,1,2,3,4,5 per Reference 1
selected_data <- filter(df, df$post_visid_type %in% c(0,1,2,3,4,5))

#Take a look what's in there after cleaning
unique(selected_data$post_visid_type)

dim(selected_data)
#[1] 11823   503

df <- selected_data
dim(df)
#[1] 11823   503


#####################
#    exclude_hit    #
#####################

#Take a look what's in there before cleaning
unique(df$exclude_hit)

#Select exclude_hit value of 0 per Reference 2
selected_data <- filter(df, df$exclude_hit == 0)

#Take a look what's in there after cleaning
unique(selected_data$exclude_hit)

dim(selected_data)
#[1] 11761   503

df <- selected_data
dim(df)
#[1] 11761   503


#####################
#     hit_source    #
#####################

#Take a look what's in there before cleaning
unique(df$hit_source)
#[1] "1"

#Select hit_source value NOT of 5,7,8,9 per Reference 2
selected_data <- filter(df, !(df$hit_source %in% c(5,7,8,9)))

#Take a look what's in there after cleaning
unique(selected_data$hit_source)
#[1] "1"

dim(selected_data)
#[1] 11761   503

df <- selected_data
dim(df)
#[1] 11761   503


#####################
#   daily_visitor   #
#####################

#Take a look what's in there before cleaning
unique(df$daily_visitor)
#[1] "0" "1"

#Select daily_visitor value of 0,1 per Reference 1
selected_data <- filter(df, df$daily_visitor %in% c(0,1))

#Take a look what's in there after cleaning
unique(selected_data$daily_visitor)
#[1] "0" "1"

dim(selected_data)
#[1] 11761   503

df <- selected_data
dim(df)
#[1] 11761   503


#####################
#   truncated_hit   #
#####################

#Take a look what's in there before cleaning
unique(df$truncated_hit)
#[1] "N"

#Select truncated_hit value of Y,N per Reference 1
selected_data <- filter(df, df$truncated_hit %in% c('Y','N'))

#Take a look what's in there after cleaning
unique(selected_data$truncated_hit)
#[1] "N"

dim(selected_data)
#[1] 11761   503

df <- selected_data
dim(df)
#[1] 11761   503


#####################
# va_instance_event #
#####################

#Take a look what's in there before cleaning
unique(df$va_instance_event)
#[1] "0" "1" "" 

#Select va_instance_event value of 0,1 per Reference 1
selected_data <- filter(df, df$va_instance_event %in% c(0,1))

#Take a look what's in there after cleaning
unique(selected_data$va_instance_event)
#[1] "0" "1"

dim(selected_data)
#[1] 11760   503

df <- selected_data
dim(df)
#[1] 11760   503


##############################################################################################################

#import latest version
library(data.table)
Oct_2_2017 <- fread('/Users/qinqingao/Desktop/Columbia/Contest/Data/selected_data_Oct_2_2017_v4.csv')

#install.packages('bit64')

View(Oct_2_2017)

dim(Oct_2_2017)
#[1] 11880   100

df <- data.frame(Oct_2_2017)

#concatenate post_visid_high and post_visid_low to unique_visitor_id
df$unique_visitor_id <- paste(df$post_visid_high, "_", df$post_visid_low)

dim(df)
#[1] 11880   101


###########################
#   Get rid of NA in V1   #
###########################

#check number of missing value in V1
sum(is.na(df$V1))
#[1] 120

#get rid of blank entries in V1
df <- df[!(is.na(df$V1) | df$V1 == ""), ]

dim(df)
#[1] 11760   101

###########################################
#   Get rid of NA in click_context_type   #
###########################################

#check number of missing value in click_context_type
sum(is.na(df$click_context_type))
#[1] 38

#get rid of NAs in click_context_type
df <- df[!is.na(df$click_context_type), ]

dim(df)
#[1] 11722   100



write.csv(df, file = "/Users/qinqingao/Desktop/Columbia/Contest/Data/selected_data_Oct_2_2017_v5.csv")


##############################################################################################################

#make indicators

######################################
#   create post_campaign_indicator   #
######################################

df$post_campaign_indicator <- ifelse (is.na(df$post_campaign) | df$post_campaign == '', 0, 1)


#count value in post_campaign_indicator
length(df$post_campaign_indicator)
#[1] 11722

#count distinct value in post_campaign_indicator
#method 1
table(df$post_campaign_indicator)

#   0    1 
#8961 2761 

#or

#method 2
install.packages('plyr')
library('plyr')

count(df$post_campaign_indicator)
#  x freq
#1 0 8961
#2 1 2761


###################################################
#   create post_pagename_quote_dealer_indicator   #
###################################################

df$post_pagename_quote_dealer_indicator <- ifelse (grepl("quote", df$post_pagename, ignore.case = TRUE) | grepl("dealer", df$post_pagename, ignore.case = TRUE), 1, 0)

#df$post_pagename_quote_dealer_indicator <- ifelse (df$post_pagename %like% "Quote" | grepl("dealer", df$post_pagename, ignore.case = TRUE), 1, 0)

length(df$post_pagename_quote_dealer_indicator)
#[1] 11722

dim(df)
#[1] 11722   101



####################################################
#   create post_pagename_lease_finance_indicator   #
####################################################

df$post_pagename_lease_finance_indicator <- ifelse (grepl("leas", df$post_pagename, ignore.case = TRUE) | grepl("financ", df$post_pagename, ignore.case = TRUE), 1, 0)

length(df$post_pagename_lease_finance_indicator)
#[1] 11722

dim(df)
#[1] 11722   102



###############################################
#   create post_pagename_OrderNow_indicator   #
###############################################

df$post_pagename_OrderNow_indicator <- ifelse (grepl("order", df$post_pagename, ignore.case = TRUE), 1, 0)

length(df$post_pagename_OrderNow_indicator)
#[1] 11722

dim(df)
#[1] 11722   103



#############################################
#   create post_page_event_var2_indicator   #
#############################################

df$post_page_event_var2_LeadInitiated_indicator <- ifelse (grepl("lead-initiated", df$post_page_event_var2, ignore.case = TRUE), 1, 0)

length(df$post_page_event_var2_LeadInitiated_indicator)
#[1] 11722

dim(df)
#[1] 11722   104



##############################################################################################################


Oct_2_2017_selected <- fread('/Users/qinqingao/Desktop/Columbia/Contest/Data/selected_data_Oct_2_2017.csv')

names(Oct_2_2017_selected)[2] <- "V2"

#select numbers in column 1, disregard problem values
selected_data <- df[grep("[[:digit:]]", df[1]), ]




#####################
#     Reference     #
#####################

#Clickstream data column reference (Reference 1):
https://marketing.adobe.com/resources/help/en_US/sc/clickstream/datafeeds_reference.html


#Identifying Visitors (Reference 2) :
https://marketing.adobe.com/resources/help/en_US/sc/clickstream/datafeeds_visid.html

