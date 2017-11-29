library(data.table)

Oct_2_2017 <- fread('Oct2 sample.csv')
#Read 21258 rows and 503 (of 503) columns from 0.117 GB file in 00:00:03

View(Oct_2_2017)

dim(Oct_2_2017)
#[1] 21258   503

#Exclude all rows where exclude_hit > 0
nrow(Oct_2_2017[Oct_2_2017$exclude_hit > 0, ])
#[1] 117

#Exclude all rows where hit_source = 5,7,8,9
nrow(Oct_2_2017[Oct_2_2017$hit_source %in% c(5,7,8,9), ])
#[1] 1








