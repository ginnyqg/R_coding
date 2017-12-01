library(data.table)

Oct_2_2017 <- fread('Oct2 sample.csv')
#Read 21258 rows and 503 (of 503) columns from 0.117 GB file in 00:00:03

View(Oct_2_2017)

dim(Oct_2_2017)
#[1] 21258   503

#Data selection
df <- data.frame(Oct_2_2017)
library(dplyr)

#Select post_visid_type value of 0,1,2,3,4,5
selected_data <- filter(df, df$post_visid_type %in% c(0,1,2,3,4,5))

dim(selected_data)
#[1] 11823   503

df <- selected_data

dim(df)
#[1] 11823   503

selected_data <- filter(df, df$exclude_hit == 0)

dim(selected_data)
#[1] 11761   503

df <- selected_data

dim(df)
#[1] 11761   503

selected_data <- filter(df, !(df$hit_source %in% c(5,7,8,9)))

dim(selected_data)
#[1] 11761   503



Oct_2_2017_selected <- fread('/Users/qinqingao/Desktop/Columbia/Contest/Data/selected_data_Oct_2_2017.csv')

names(Oct_2_2017_selected)[2] <- "V2"

#select numbers in column 1, disregard problem values
selected_data <- df[grep("[[:digit:]]", df[1]), ]


