#import csv Data
dataset_you_name_it <- read.csv('csv file path')
dataset_you_name_it <- read.csv('url/file.csv', header = T, na.string = ',')

#read data from website
dat <- read.table("http://...txt")

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

                         
#select rows from dataset where a column meet a condition                         
dat[which(dat$colA > 0), ]


#change value from one to another in a column                         
dat$colA <- gsub("Abc", 1, dat$colA)                         
                         
                         
#select rows based on substring match                         
abc <- esp_wide[well %like% "Amer"]

                         
#extract specific columns by column name                        
dat[, c('colA', 'colB')]                         
                         
                                                 
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
# 2 represents margin = 2, over columns
# 1 means over rows
                 
names(dat)[sapply(dat, function(x) all(is.na(x)))]                   
                   
          
                   
#remove columns where colnames contain certain string
df_new <- df[, -which(grepl("abc", colnames(df)))]
                   
#find length of a vector
length(vec)
                   
#save top 20 variable importance plot in pdf, png, etc.                   
pdf('filepath.pdf')
plot(gg_vimp(file, nvar = 20))
dev.off()


#time operation
begin <- Sys.time()
model <- rfsrc(target_variable ~ ., data = train.dat,
       tree.err = T, ntree = #, nodesize = #, do.trace = #)
end <- Sys.time()
end - begin                   
                   
       
#predict
predict(model, dataframe)
               
               
# conf OutOfMemoryError (Java): GC overhead limit
options(java.parameters = "- Xmx1024m")               
               
                               
#histogram, set num of bins
hist(vector, breaks = num)
              
       
#for loop, python dict equivalent
file_list <- c(test_fpath1, test_fpath2)

actual <- 2
pred <- 0
thres <- 10

#ds is test dataset for each file
ds <- c()
test.dat <- c()
rf.pred <- c()
err <- c()
pred_fail <- c()

for (i in seq(1, 2)) {
ds[[i]] <- fread(file_list[i])
ds[[i]] <- data.frame(ds[[i]])
print(unique(ds[[i]]$well))
test.dat[[i]] <- ds[[i]][, -which(names(ds[[i]]) %in% c('colA', 'colB', 'colC'))]
print(dim(test.dat[[i]]))
mod.pred[[i]] <- predict(modelM, test.dat[[i]]) 
print(mod.pred[[i]])
err[[i]] <- abs(mod.pred[[i]]$predicted - mod.pred[[i]]$yvar)
pred_fail[[i]] <- if (sum(sum(err[[i]] > thres)) > 1) {1} else {0}
print(pred_fail[[i]])
tot <- tot + pred_fail[[i]]
}

print(tot)

                             
#find values based on column values in a  dataframe              
df$col_ValueIn[df$ColA == 1 & df$ColB == 'Abc']               
       
#plot with vertical lines
#change datatype from character to timestamp
timestamp <- as.POSIXct(df$ts)
      
#plot residuals, center title, with styled vertical, horizontal lines, add legend
#save fig in the path as pdf, specify fig size
pdf('path/fig_123.pdf', width = 10, height = 6)
               
qplot(as.Date(timestamp), res, xlab = "X", ylab = "Y", main = paste('Abc', 'def')) + 
theme(plot.title = element_text(hjust = 0.5, face = 'bold', size = 16)) +
geom_vline(aes(xintercept = c(as.Date(as.POSIXct(start_ts)), as.Date(as.POSIXct(end_ts))), color = c('start', 'end')), linetype = c('solid', 'dashed'), lwd = 0.5, show.legend = T) + 
scale_color_manual("Start, end", values = c("start" = "red", "end" = "green")) +
geom_hline(yintercept = 5, color = 'coral', linetype = 'dashed', lwd = 0.8) +
scale_x_date(labels = date_format("%Y-%b-%d"))
               
dev.off()

               

#plot 4 in 1, share x-axis               
p1 <- qplot(as.Date(timestamp), res1, xlab = 'X', ylab = 'Y1', main = 'Abc') + 
theme(plot.title = element_text(hjust = 0.5, face = 'bold', size = 14)) +
geom_vline(aes(xintercept = c(as.Date(as.POSIXct(start_ts)), as.Date(as.POSIXct(end_ts))), color = c('start', 'end')), linetype = c('solid', 'dashed'), lwd = 0.5, show.legend = T) + 
scale_color_manual('Start, end', values = c('start' = 'red', 'end' = 'green')) + 
geom_hline(yintercept = 10, color = 'coral', linetype = 'dashed', lwd = 0.8) +
scale_x_date(labels = date_format("%Y-%b-%d")) +
theme(axis.title.x = element_blank(), axis.text.x = element_blank(), plot.margin = unit(c(1, 1, -0.02, 1), "cm"))

p2 <- qplot(as.Date(timestamp), res2, xlab = 'X', ylab = 'Y2') + 
theme(plot.title = element_text(hjust = 0.5, face = 'bold', size = 14)) +
geom_vline(aes(xintercept = c(as.Date(as.POSIXct(start_ts)), as.Date(as.POSIXct(end_ts))), color = c('start', 'end')), linetype = c('solid', 'dashed'), lwd = 0.5, show.legend = T) + 
scale_color_manual('Start, end', values = c('start' = 'red', 'end' = 'green')) + 
geom_hline(yintercept = 200, color = 'coral', linetype = 'dashed', lwd = 0.8) +
scale_x_date(labels = date_format("%Y-%b-%d")) +
theme(axis.title.x = element_blank(), axis.text.x = element_blank(), plot.margin = unit(c(-0.02, 1, -0.02, 1), "cm"))

p3 <- qplot(as.Date(timestamp), res3, xlab = 'X', ylab = 'Y3') + 
theme(plot.title = element_text(hjust = 0.5, face = 'bold', size = 14)) +
geom_vline(aes(xintercept = c(as.Date(as.POSIXct(start_ts)), as.Date(as.POSIXct(end_ts))), color = c('start', 'end')), linetype = c('solid', 'dashed'), lwd = 0.5, show.legend = T) + 
scale_color_manual('Start, end', values = c('start' = 'red', 'end' = 'green')) + 
geom_hline(yintercept = 2, color = 'coral', linetype = 'dashed', lwd = 0.8) +
scale_x_date(labels = date_format("%Y-%b-%d")) +
theme(axis.title.x = element_blank(), axis.text.x = element_blank(), plot.margin = unit(c(-0.02, 1, -0.02, 1), "cm"))

p4 <- qplot(as.Date(timestamp), res4, xlab = 'X', ylab = 'Y4') + 
theme(plot.title = element_text(hjust = 0.5, face = 'bold', size = 14)) +
geom_vline(aes(xintercept = c(as.Date(as.POSIXct(start_ts)), as.Date(as.POSIXct(end_ts))), color = c('start', 'end')), linetype = c('solid', 'dashed'), lwd = 0.5, show.legend = T) + 
scale_color_manual('Start, end', values = c('start' = 'red', 'end' = 'green')) + 
geom_hline(yintercept = 0.05, color = 'coral', linetype = 'dashed', lwd = 0.8) +
scale_x_date(labels = date_format("%Y-%b-%d")) +
theme(axis.title.x = element_blank(), plot.margin = unit(c(-0.02, 1, 1, 1), "cm"))


grid.newpage()
grid.draw(rbind(ggplotGrob(p1), ggplotGrob(p2), ggplotGrob(p3), ggplotGrob(p4), size = "last"))


               
#plot histogram-like if data very skewed               
plot(cut(data, breaks = 4))
               
#remove last item of a vector x
x[1 : (length(x) - 1)]
               
               
#create survival object
dat <- fread('file')
head(dat$time_to_fail)
dat <- data.frame(dat)
dim(dat)
# ttf <- as.numeric(as.POSIXct('1999-01-10 09:00:00') - as.POSIXct(dat$tstamp))
# km <- with(dat, Surv(ttf, failure_zone))
km <- with(dat, Surv(time_to_fail, failure_zone))               
dim(km)              
km_fit <- survfit(Surv(time_to_fail, failure_zone) ~ 1, data = dat)
summary(km_fit, times = c(1, 30, 60, 90 * (1 : 10)))
autoplot(km_fit)               
               
               
#  https://rstudio-pubs-static.s3.amazonaws.com/5588_72eb65bfbe0a4cb7b655d2eee0751584.html                          
#http://rstudio-pubs-static.s3.amazonaws.com/16003_8d6f2069af094998bfbb3d3a7dfb1c17.html
               
#concatenate 2 dataframes vertically
dat <- rbind(a, b)               
      
               
               
#check if packages needed already installed, if not installing, loading required packages  
               
pkg <- c("tidyverse", "survival", "ggfortify", "survminer", "plotly", "gridExtra", 
         "Epi", "KMsurv", "gnm", "cmprsk", "mstate", "flexsurv", "splines",
         "epitools", "eha", "shiny", "ctqr", "scales")
               
new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
               
if (length(new.pkg)) install.packages(new.pkg, dependencies = TRUE)
               
sapply(pkg, require, character.only = TRUE)               
               

               
#draw diagrams in R
tm <- matrix(c(NA, NA, 1, NA), ncol = 2)
rownames(tm) <- colnames(tm) <- c("Result 1", "Result 2")
tm2 <- matrix(c(NA, NA, NA, 1, NA, NA, 2, NA, NA), ncol = 3)
rownames(tm2) <- colnames(tm2) <- levels(dat$event)
par(mfrow = c(1, 2))
layout(rbind(c(1, 2, 2)))
boxes.matrix(tm, boxpos = TRUE)
title("A)")
boxes.matrix(tm2, boxpos = TRUE)
title("B)")
               
#https://rpubs.com/alecri/258589
            
#print class for each column in the dataset
sapply(dat, class)
               
               
               
#Survival analysis

library(data.table)
library(randomForestSRC)
library(ggRandomForests)
library(dplyr)
library(survival)
library(pec)
library(ggfortify)
library(flexsurv)
library(survminer)
library(randomForestSRC)
library(survcomp)


dat <- fread('path to train file')
dat <- data.frame(dat)

#remove useless column
dat <- dat[, -which(names(dat) %in% c('useless_col'))]
dim(dat)

#check any column all NA and remove them               
colnames(dat)[apply(dat, 2, function(x) all(is.na(x)))]
length(colnames(dat)[apply(dat, 2, function(x) all(is.na(x)))])
dat <- dat[, -which(grepl("Abc", colnames(dat)))]
dim(dat)

#retain complete cases only                           
dat_cc <- dat[complete.cases(dat), ]
dim(dat_cc)


## Create survival object
dat_cc$SurvObj <- with(dat_cc, Surv(time_to_fail, status))
head(dat_cc)
                           

## Fit Cox Proportional Hazards Model
res.cox <- coxph(formula = SurvObj ~ Col1 + Col2, data = dat_cc)
summary(res.cox)               
                            
               
## Plot survival curve
res.cox_fit <- survfit(res.cox)
summary(res.cox_fit)
plot(res.cox_fit, main = 'Cox Proportional Hazards Model', xlab = 'Hours', ylab = 'Surv Prob')
ggsurvplot(fit = res.cox_fit, data = dat_cc, risk.table = TRUE, xlab = 'Time (hours)', censor = T, title = 'Cox Proportional Hazards Model')
               
               
## Prep for test data (if separate from train)
dat_test <- fread('path to file name')
dat_test <- data.frame(dat_test)

#remove useless column
dat_test <- dat_test[, -which(names(dat_test) %in% c('useless_col'))]
dim(dat_test)

#check any column all NA and remove them
colnames(dat_test)[apply(dat_test, 2, function(x) all(is.na(x)))]
length(colnames(dat_test)[apply(dat_test, 2, function(x) all(is.na(x)))])
dat_test <- dat_test[, -which(grepl("Abc", colnames(dat_test)))]
dim(dat_test)

#retain complete cases only
dat_test_cc <- dat_test[complete.cases(dat_test), ]
dim(dat_test_cc)

# Create survival estimates on validation data
pred_validation = predict(res.cox, newdata = dat_test_cc)

# Determine concordance
cindex_validation = concordance.index(pred_validation, surv.time = dat_test_cc$time_to_fail,
                                       surv.event = dat_test_cc$status, method = "noether")
cindex_validation$c.index
               
               
#print current working directory
getwd()
                                
                                
# table function, don't show counts == 0
table(droplevels(dataset$col))

                                
# count non-zero counts
sum(table(droplevels(dataset$col))                                )
                                
                                
# get number from table function
as.vector(table(dataset))
                                
                                
# get variable names from table function
names(table(dataset))                              
  
                                
# sort value descendingly in table
sort(table(dataset), decreasing = TRUE)                                
                                

# plot barplot for top 20 items for condition
par(mar = c(10, 4, 2, 2) + 2)
ylim <- c(0, 1.1 * max(as.vector(table(dataset)))
# plot barplot
bplot <- barplot(height = table(dataset), ylim = ylim, names.arg = names(table(dataset)), horiz = F, las = 2, col = rainbow(21), main = "Top 20 items for condition", cex.names = 0.8)
# add label for bars
text(x = bplot, y = as.vector(table(dataset)), label = as.vector(table(dataset)), pos = 3, cex = 1.1)

                                
# create pie chart
lbls <- paste(names(table(dataset)), "\n", table(dataset), sep = "")
# pie(table(dataset), labels = lbls, main = "XYZ")
library(plotrix)
pie3D(as.vector(table(dataset)), labels = lbls, main = NA, explode = 0.1, radius = .9, labelcex = 1.2,  start = 0.01)
title("XYZ", line = -5)

          
# histogram          
hist(as.numeric(data0[data0$colA == 'Abc' ,]$colB), main = 'XYZ', xlab = 'time', breaks = 15, xlim = c(0, 4000), col = 'darksalmon', labels = TRUE)
          
          
# Add Straight Lines to a Plot
abline(ls2, col = 'red')
legend('bottomright', c('regression line'), col = 'red', lwd = 1, bty = 'n')
          
          
# fit model with every variable (except for target) other than 1 independent variable
model <- modeltype(DV ~ . -excluded_IV, data = abc)

          
          
