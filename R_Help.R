#Import csv Data
dataset_you_name_it = read.csv(file = “csv file path”)

#Examine first few rows
head(dataset_you_name_it)

#Get dimension (row, column) of dataset
dim(dataset_you_name_it)

#Get column names of dataset
names(dataset_you_name_it)

#Get class/types of variables

#Apply a function to each element of a list in turn, but you want a vector back
sapply(dataset_you_name_it, class)

#Apply a function to each element of a list in turn and get a list back
lapply(dataset_you_name_it, class)

#Descriptive summary of dataset: min, median, mean, max, 1st Qu, 3rd Qu…
summary(dataset_you_name_it)

#Some visualization
pairs(dataset_you_name_it)

#boxplot
boxplot(“y variable”~”x variable”, data = dataset_you_name_it)

#install package
install.packages(“package_name”)

#Check R version
R.Version()

#make individual variables available as vectors
attach(dataset_you_name_it)


#Change directory tip
Put \ before the space in file path

#Run R file at terminal
r -f your_r_file.r

#Calculate group (x) means (of y)
aggregate(y~x, dataset_you_name_it, mean)


#Drop unused levels
new_field <- droplevels(dataset_you_name_it$old_field)

