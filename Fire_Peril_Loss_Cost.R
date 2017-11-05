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


#EDA target, exclude target is 0
hist(fire_train$target[! fire_train$target == 0], labels = TRUE)

#EDA cat varialbe
barplot(table(fire_train $var1))





