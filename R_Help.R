#import csv Data
dataset_you_name_it <- read.csv('csv file path')

#FAST read of large file
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

#check datatype of a column
typeof(dataset_you_name_it$colname)

#get first n column names of dataset
dataset_you_name_it %>% colnames() %>% head(n)

#get first m entries of columnX of dataset
dataset_you_name_it %>% select(X) %>% head(m)

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

#histrogram for numerical variables
hist(dataset_you_name_it$numerical_column, labels = TRUE)

#install package
install.packages(“package_name”)

#check R version
R.Version()

#clear console
Ctrl+L

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

#select dataset where a column meet the condition of starting with specific string
dataset_you_name_it[startsWith(dataset_you_name_it$specific_column_name, 'abc') == TRUE,]

#attach column names to the data frame, then no need to refer to dataset name when calling a column
attach(dataset_you_name_it)


#switch between matrix and dataframe
as.matrix()
as.data.frame()


#write data frames to files, row.names = F means to exclude first index column)
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
which(dataset_you_name_it$column_x == 'value1')


#replace rows with value of value1 with value2
dataset_you_name_it$column_x[which(dataset_you_name_it$column_x == 'value1')] = 'value2'

dataset_you_name_it$column_x[dataset_you_name_it$column_x == 'value1'] = 'value2'


#missing value, value not exists: NA
#not an integer, but value exists: NaN


#check missing value
is.na()

#count missing value in a column
sum(is.na(df$col))

#count missing value in a dataset
sum(is.na(dataset_you_name_it))

#calculate percentage of missing value in a dataset
sum(is.na(dataset_you_name_it))/(nrow(dataset_you_name_it) * ncol(dataset_you_name_it))

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


#drop columnA, notice no quotation mark
df <- subset(df, select = -c(columnA))


#ggplot by category
ggplot(df_5YEAR, aes(x = timeframe_quit)) + geom_bar(stat="count", fill = '#56B4E9', color = '#56B4E9', alpha = 0.6) + facet_grid(year~.) + geom_text(stat = 'count', aes(label=..count..), vjust = 0.2) + labs(x = 'Timeframe quit', y = 'Count') + ggtitle('Count of timeframes thinking of quitting smoking by year') + theme(plot.title = element_text(hjust = 0.5), panel.background = element_rect(fill = NA, color = "black"))

                         
#confusion matrix between a txt file and Rdata
l <- read.table('test_labels.txt')

                         
#open and read first few lines of RData file
head(get(load('file path')))                         
                         
output <- get(load('pred_test.RData'))
o <- as.matrix(output)[1:100,] #to compare with first 100 rows of o, for example

confusionMatrix(o, l[1:100,])
                         
#select rows where ColB (from ColA, ColB, ColC) has value of 1 (satify a condition)
matrix[matirx[, 'ColB'] == 1, ]                        
matrix[matirx[, 2] == 1, ]

#or

subset(matrix, matrix[, 'ColB'] == 1)                      
subset(matrix, matrix[, 2] == 1)

                         
#select rows based on substring match                         
abc <- esp_wide[well %like% "Amer"]
                         
                         
#find system time to run something
system.time()                       
                         
#find memory used to run something
object.size()

        
#change RData file to data frame                         
df <- as.data.frame(get(load('path.RData')))                         

                         
#set operation on x, y
union(x, y) #in either
intersect(x, y) #common in both
setdiff(x, y) #in x, not in y
setdiff(y, x) #in y, not in x
setequal(x, y) #boolean, true or false
                         
                         
#check if all elements in d1 is equal to all elements in d2                         
all(d1[ , ] == d2[ , ])      #na.rm = TRUE                   
                         
#extract date from date time time format                         
#date time combined: 2013-08-01 00:00:00                         
new_date <- gsub( " .*$", "", esp_wide$tstamp)
#"2013-08-01"
new_time <- gsub( ".* ", "", esp_wide$tstamp)
#"00:00:00"
                         
# A space (), then any character (.) any number of times (*) until the end of the string ($). 
                         
# find column names in df where they are all NA
colnames(df)[apply(df, 2, function(x) all(is.na(x)))]                         

                 
#remove columns where colnames contain certain string
df_new <- df[, -which(grepl("abc", colnames(df)))]
                   

                   
                   
                   
                   
