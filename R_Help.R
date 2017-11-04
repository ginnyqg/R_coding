#import csv Data
dataset_you_name_it = read.csv(file = “csv file path”)

#faster read of large file
library(data.table)
dataset_you_name_it <- fread('csv file path')

#additional features of fread
https://github.com/Rdatatable/data.table/wiki/Convenience-features-of-fread

#import csv file, first row is header
#read.table() returns data.frame object
dataset_you_name_it = read.table('file name.format', sep = ',')

#import csv file, first row is not header
dataset_you_name_it = read.table('file name.format', header = F, sep = ',')

#examine first few rows
head(dataset_you_name_it)

#get dimension (row, column) of dataset
dim(dataset_you_name_it)

#get column names of dataset
names(dataset_you_name_it)

#get class/types of variables
#apply a function to each element of a list in turn, but you want a vector back
sapply(dataset_you_name_it, class)

#apply a function to each element of a list in turn and get a list back
lapply(dataset_you_name_it, class)

#descriptive summary of dataset: min, median, mean, max, 1st Qu, 3rd Qu…
summary(dataset_you_name_it)

#pairwise correlation visualization
pairs(dataset_you_name_it)

#boxplot
boxplot(“y variable”~”x variable”, data = dataset_you_name_it)

#histogram for categorical variables
barplot(table(dataset_you_name_it$categorical_column))

#install package
install.packages(“package_name”)

#check R version
R.Version()

#make individual variables available as vectors
attach(dataset_you_name_it)

#change directory tip
Put \ before the space in file path

#run R file at terminal
r -f your_r_file.r

#calculate group (x) means (of y)
aggregate(y~x, dataset_you_name_it, mean)

#drop unused levels
new_field <- droplevels(dataset_you_name_it$old_field)

#switch between character and numeric
as.character()
as.numeric()


#if else
if () {
  
} else {
  
}


#for
for () {
  
}


#while
while () {
  
}


#help, example
help()
example()


#access nth row of data
#access first n rows of data
#access 1st, 3rd, 5th row of data
dataset_you_name_it[n, ]
dataset_you_name_it[1:n, ]
dataset_you_name_it[c(1, 3, 5), ]


#access specific column of data
dataset_you_name_it$specific_column_name
dataset_you_name_it[['specific_column_name']]
dataset_you_name_it[ ,'specific_column_name']


#attach column names to the data frame, then no need to refer to dataset name when calling a column
attach(dataset_you_name_it)


#switch between matrix and dataframe
as.matrix()
as.data.frame()


#write data frames to files
write.csv(dataset_you_name_it, 'csv_output.csv', row.names = F)
write.table()





#access data from db
library(RPostgreSQL)

#create an RPostgreSQL instance
drv = dbDriver('PostgreSQL')

#open the connection
conn = dbConnect(drv, user = 'username', password = 'password', dbname = 'testdb', host = 'localhost')

#run an SQL statement by creating first a result set
object rs <- dbSendQuery(conn, statement = 'select * from dataset_you_name_it where 'clause')

#fetch all rows from result set into
data frame db.data <- fetch(rs, n = -1)

#disconnect db
dbDisconnect(conn)

print(db.data)


#sqldf can query text files without loading into dadta frames first
library(sqldf)

sqldf('select column1, avg(numeric_column) from dataset_you_name_it group by column1')



#convert to factor
as.factor()

#init_num_column has value 0, 1
dataset_you_name_it$init_num_column = as.factor(dataset_you_name_it$init_num_column)


library(BCA)

dataset_you_name_it$init_num_column <- relabel.factor(dataset_you_name_it$init_num_column, 
                                new.labels=c('No','Yes'))


#factors: categorical variables, nominal varibles


#convert a factor to an ordinal variable
ordered()

dataset_you_name_it$column1 = ordered(dataset_you_name_it$column1, levels = c('lvl1', 'lvl2', 'lvl3', 'lvl4'))

#check levels of ordinal variables
levels(dataset_you_name_it$column1)

#counts of levels in a categorical variable
table(dataset_you_name_it$column1)


#get unique value
unique(dataset_you_name_it$column_x)


#find which rows have value1
which(dataset_you_name_it#column_x == 'value1')


#replace rows with value of value1 with value2
dataset_you_name_it$column_x[which(dataset_you_name_it$column_x == 'value1')] = 'value2'

dataset_you_name_it$column_x[dataset_you_name_it$column_x == 'value1'] = 'value2'


#missing value, value not exists: NA
#not an integer, but value exists: NaN


#check missing value
is.na()

#check which rows have missing value for column1
which(is.na(dataset_you_name_it$column1))


#if want to compute mean of column1 that has NA
mean(dataset_you_name_it$column1, na.rm = T)


#check if a data frame contains missing value
na.fail()

#if returns an error, has missing value
#if outputs the same as input, do not have missing value


#replace missing value in column1 with constant_c
dataset_you_name_it$column1[is.na(dataset_you_name_it$column1)] = constant_c


#replace missing value with mean
female.height.no.missing.value = mean(data$height[which(data$set == 'Female')], na.rm = T)

data$height[which(data$set == 'Female' & is.na(data$height))] = female.height.no.missing.value

#count of missing value in a column
sum(is.na(df$col))


           



