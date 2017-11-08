#Import, EDA, train

library(data.table)

#import train data
fire_train <- fread('Fire Peril Loss Cost/train.csv')

#dimension: number of rows and columns
dim(fire_train)

#peek at first few rows
head(fire_train)

#overview of variables
summary(fire_train)


#EDA target, a continuous variable, exclude target is 0
hist(fire_train$target[! fire_train$target == 0], labels = TRUE)

#EDA categorical varialbe
barplot(table(fire_train $var1))


#count number of NAs in continuous column
sum(is.na(fire_train$var12))

#impute NA in var12, 14, 15, 16 to 0
fire_train$var12[is.na(fire_train$var12)] = 0


#transform datset to data frame
df_raw <- data.frame(fire_train)

#extract useful columns
df_train <- df_raw[, c(2, 12:19)]


