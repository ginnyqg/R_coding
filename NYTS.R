
## **Definitions**

#Non-Trier - Someone who reports not having tried even a single puff of a cigarette in their life.
#Non-Trier (Not at Risk) - Someone who reports not having tried even a single puff of a cigarette in their life, and also reports absolute resolution to abstain from cigarette initiation, even if offered a cigarette by a best friend.
#Non-Trier (At Risk) - Someone who reports not having tried even a single puff of a cigarette in their life, but reports some degree of probability for trying one soon or if offered by a best friend.
#Experimenter - Someone who reports having tried even a single puff of a cigarette in their life, but has not smoked more than 100 cigarettes in their lifetime yet.
#Current Smoker - Someone who reports having smoked at least 100 cigarettes in their lifetime, and has had at least one cigarette in the past 30 days.
#Former Smoker - Someone who reports having smoked at least 100 cigarettes in their lifetime, but has not had a cigarette in the past 30 days.



#Loads the required libraries
library(readr)
library(dplyr)
library(ggplot2)

#Imports datasets, ASSUMING DATA SETS ARE IN THE WORKING DIRECTORY
NYTS_2011 <- read_csv("nyts2011_dataset.csv")
NYTS_2012 <- read_csv("nyts2012_dataset.csv")
NYTS_2013 <- read_csv("nyts2013_dataset.csv")
NYTS_2014 <- read_csv("nyts2014_dataset.csv")
NYTS_2015 <- read_csv("nyts2015_dataset.csv")


#CREATE FUNCTIONS TO BE USED FOR SEGMENTING AND CLASSIFYING EACH YEAR'S DATASET

#This function classifies the respondent's primary segment, based on their responses to three questions.
#These questions are the worded the same across all surveys, but their number in the survey might differ, so the function allows me to change the inputs accordingly.
#Any respondents that lack a value for classification should be left as a null value.

cig_segment_function <- function(tried, lifetime, thirtydays) {
  ifelse(tried==2, 'Non-Trier',
         (ifelse(tried==1 & lifetime > 1 & lifetime < 8, 'Experimenter',
                 (ifelse(tried==1 & lifetime ==8 & thirtydays == 1, 'Former Smoker',
                         (ifelse(tried==1 & lifetime==8 & thirtydays > 1, 'Current Smoker',
                                 NA)))))))}


#This function translates the respondent's grade level from CDC's coding.
grade_function <- function(grade) {
  ifelse(grade >= 1 & grade <= 7, paste("Grade",grade + 5,sep=" "), NA)}

#This function classifies the respondent's e-cigarette segment.
ecig_segment_function <- function(ecig) {
  ifelse(ecig == 1, 'E-Cig Trier',
         (ifelse(ecig ==2, 'E-Cig Non-Trier', NA)))}


#This function resolves the challenge where the 2011 and 2012 surveys capture Hispanic ethnicity across multiple values in Question 4
hisp_2011_agg <- function(qn4) {
  ifelse(qn4 > 1, 1, NA)
}

#This function resolves the challenge where the 2013, 2014, and 2015 surveys capture Hispanic ethnicity across multiple questions.
hisp_2013_agg <- function(qn4b, qn4c, qn4d, qn4e) {
  ifelse(!is.na(qn4b) | !is.na(qn4c) | !is.na(qn4d) | !is.na(qn4e), 1, NA)
}

#This function determines whether the respondent is multi-ethnic if they've selected multiple etnicities.
#When applied, this function must run after the previous function related to aggregating Hispanic ethnicity.
multi_ethnic_tag <- function(namerican, asian, black, hawaiian, white, hispanic) {
  as.integer(!is.na(namerican)) + as.integer(!is.na(asian)) + as.integer(!is.na(black)) + as.integer(!is.na(hawaiian)) + as.integer(!is.na(white)) + as.integer(!is.na(hispanic))
}

#This function recodes the respondent's ethnicity.
#When applied, this function cannot run before the Hispanic and Multi-Ethnic functions.
ethnicity_function <- function(namerican, asian, black, hawaiian, white, hispanic, multiethnic) {
  ifelse(multiethnic > 1, 'Multi-ethnic',
         (ifelse(!is.na(namerican), 'Native American',
                 (ifelse(!is.na(asian), 'Asian',
                         (ifelse(!is.na(black), 'Black',
                                 (ifelse(!is.na(hawaiian), 'Hawaiian or Other PI',
                                         (ifelse(!is.na(white), 'White',
                                                 (ifelse(!is.na(hispanic), 'Hispanic or Latino', NA)
                                                 ))))))))))))}



#This function determines age of youth first started using cigar products
age_group_cigar_function <- function(age_group_cigar) {
 ifelse(age_group_cigar == 1, 'Never',
 (ifelse(age_group_cigar == 2, '8 or younger',
 (ifelse(age_group_cigar == 3, '9',
 (ifelse(age_group_cigar == 4, '10',
 (ifelse(age_group_cigar == 5, '11',
 (ifelse(age_group_cigar == 6, '12',
 (ifelse(age_group_cigar == 7, '13',
 (ifelse(age_group_cigar == 8, '14',
 (ifelse(age_group_cigar == 9, '15',
 (ifelse(age_group_cigar == 10, '16',
 (ifelse(age_group_cigar == 11, '17',
 (ifelse(age_group_cigar == 12, '18',
 (ifelse(age_group_cigar == 13, '19 or older', NA)))))))))))))))))))))))))}


#This function determines age of youth first started using tobacco products
age_group_tobacco_function <- function(age_group_tobacco) {
 ifelse(age_group_tobacco == 1, 'Never',
 (ifelse(age_group_tobacco == 2, '8 or younger',
 (ifelse(age_group_tobacco == 3, '9',
 (ifelse(age_group_tobacco == 4, '10',
 (ifelse(age_group_tobacco == 5, '11',
 (ifelse(age_group_tobacco == 6, '12',
 (ifelse(age_group_tobacco == 7, '13',
 (ifelse(age_group_tobacco == 8, '14',
 (ifelse(age_group_tobacco == 9, '15',
 (ifelse(age_group_tobacco == 10, '16',
 (ifelse(age_group_tobacco == 11, '17',
 (ifelse(age_group_tobacco == 12, '18',
 (ifelse(age_group_tobacco == 13, '19 or older', NA)))))))))))))))))))))))))}


#This function categorizes how often youth sees ads/promotion on Internet
ads_Internet_function <- function(ads_Internet) {
 ifelse(ads_Internet == 1, "Don't use Internet",
 (ifelse(ads_Internet == 2, 'Never',
 (ifelse(ads_Internet == 3, 'Rarely',
 (ifelse(ads_Internet == 4, 'Sometimes',
 (ifelse(ads_Internet == 5, 'Most of the time',
 (ifelse(ads_Internet == 6, 'Always', NA)))))))))))}


#This function categorizes how often youth sees ads/promotion on newspaper/magazine
ads_news_mag_function <- function(ads_news_mag) {
 ifelse(ads_news_mag == 1, "Don't read News/Mag",
 (ifelse(ads_news_mag == 2, 'Never',
 (ifelse(ads_news_mag == 3, 'Rarely',
 (ifelse(ads_news_mag == 4, 'Sometimes',
 (ifelse(ads_news_mag == 5, 'Most of the time',
 (ifelse(ads_news_mag == 6, 'Always', NA)))))))))))}


#This function categorizes how often youth sees actors using cig/tobacco on TV/movie
actors_tobacco_function <- function(actors_tobacco) {
 ifelse(actors_tobacco == 1, "Don't watch TV/movie",
 (ifelse(actors_tobacco == 2, 'Never',
 (ifelse(actors_tobacco == 3, 'Rarely',
 (ifelse(actors_tobacco == 4, 'Sometimes',
 (ifelse(actors_tobacco == 5, 'Most of the time',
 (ifelse(actors_tobacco == 6, 'Always', NA)))))))))))}


#This function checks if anyone lives with the youth now used any form of tobacco
noone_live_together_used_tobacco_function <- function(noone_live_together_used_tobacco) {
 ifelse(noone_live_together_used_tobacco == 1, TRUE, NA)}


#This function categorizes timeframe youth seriously think about quitting all tobacco in 2011
timeframe_quit_2011_function <- function(timeframe_quit_2011) {
 ifelse(timeframe_quit_2011 == 1, 'Never used tobacco',
 (ifelse(timeframe_quit_2011 == 2, 'Within 30 days',
 (ifelse(timeframe_quit_2011 == 3, 'Within 6 months',
 (ifelse(timeframe_quit_2011 == 4, 'Within > 6 months',
 (ifelse(timeframe_quit_2011 == 5, 'Not think about quitting', NA)))))))))}
 
 
#This function categorizes timeframe youth seriously think about quitting all tobacco 2012 - 2015
timeframe_quit_2012_2015_function <- function(timeframe_quit_2012_2015) {
 ifelse(timeframe_quit_2012_2015 == 1, 'Never used tobacco',
 (ifelse(timeframe_quit_2012_2015 == 2, 'Within 30 days',
 (ifelse(timeframe_quit_2012_2015 == 3, 'Within 6 months',
 (ifelse(timeframe_quit_2012_2015 == 4 | timeframe_quit_2012_2015 == 5, 'Within > 6 months',
 (ifelse(timeframe_quit_2012_2015 == 6, 'Not think about quitting', NA)))))))))}



#CLEAN AND CREATE SUBSETS OF EACH YEAR'S DATASET

#Creates dataset for 2011
#Additional variables are purposefully created in different mutate statements due to reliance on a prior new variable.
#The previously defined functions are applied here, with the appropriate question numbers as inputs.
NYTS_2011_subset <- NYTS_2011 %>%
  mutate(cigarette_segment = cig_segment_function(qn7, qn12, qn13),
         grade_level = grade_function(qn3),
         ecigarette_segment = ecig_segment_function(eelcigt_r),
         weighted_pop = wt,
         age_cigar_first = age_group_cigar_function(qn22),
         age_tobacco_first = age_group_tobacco_function(qn27),
         ads_Internet = ads_Internet_function(qn40),
         ads_news_mag = ads_news_mag_function(qn41),
         actors_tobacco = actors_tobacco_function(qn46),
         noone_live_together_used_tobacco = noone_live_together_used_tobacco_function(qn67j),
         timeframe_quit = timeframe_quit_2011_function(qn58),
         qn4_agg = hisp_2011_agg(qn4)) %>%
  mutate(multiethnic = multi_ethnic_tag(qn5a, qn5b, qn5c, qn5d, qn5e, qn4_agg)) %>%
  mutate(ethnicity = ethnicity_function(qn5a, qn5b, qn5c, qn5d, qn5e, qn4_agg, multiethnic)) %>%
  mutate(cigarette_subsegment = ifelse(qn7==2 & qn8==4 & qn9 == 3 & qn10 == 4, 'Non-Trier: Not at Risk',
                                       (ifelse(qn7==2 & (qn8 != 4 | qn9 != 3 |qn10 != 4), 'Non-Trier: At Risk', cigarette_segment)
                                       ))) %>%
  select(year, weighted_pop, ethnicity, grade_level, cigarette_segment, cigarette_subsegment, ecigarette_segment, age_cigar_first, age_tobacco_first, ads_Internet, ads_news_mag, actors_tobacco, noone_live_together_used_tobacco, timeframe_quit)


#Creates dataset for 2012
NYTS_2012_subset <- NYTS_2012 %>%
  mutate(cigarette_segment = cig_segment_function(qn7, qn12, qn13),
         grade_level = grade_function(qn3),
         ecigarette_segment = ecig_segment_function(eelcigt_r),
         weighted_pop = wt,
         age_cigar_first = age_group_cigar_function(qn24),
         age_tobacco_first = age_group_tobacco_function(qn30),
         ads_Internet = ads_Internet_function(qn45),
         ads_news_mag = ads_news_mag_function(qn46),
         actors_tobacco = actors_tobacco_function(qn51),
         noone_live_together_used_tobacco = noone_live_together_used_tobacco_function(qn80e),
         timeframe_quit = timeframe_quit_2012_2015_function(qn57),
         qn4_agg = hisp_2011_agg(qn4)) %>%
  mutate(multiethnic = multi_ethnic_tag(qn5a, qn5b, qn5c, qn5d, qn5e, qn4_agg)) %>%
  mutate(ethnicity = ethnicity_function(qn5a, qn5b, qn5c, qn5d, qn5e, qn4_agg, multiethnic)) %>%
  mutate(cigarette_subsegment = ifelse(qn7==2 & qn8==4 & qn9 == 4 & qn10 == 4, 'Non-Trier: Not at Risk',
                                       (ifelse(qn7==2 & (qn8 != 4 | qn9 != 4 |qn10 != 4), 'Non-Trier: At Risk', cigarette_segment)
                                       ))) %>%
  select(year, weighted_pop, ethnicity, grade_level, cigarette_segment, cigarette_subsegment, ecigarette_segment, age_cigar_first, age_tobacco_first, ads_Internet, ads_news_mag, actors_tobacco, noone_live_together_used_tobacco, timeframe_quit)


#Creates dataset for 2013
NYTS_2013_subset <- NYTS_2013 %>%
  mutate(cigarette_segment = cig_segment_function(qn9, qn14, qn15),
         grade_level = grade_function(qn3),
         ecigarette_segment = ecig_segment_function(eelcigt_r),
         weighted_pop = wt,
         age_cigar_first = age_group_cigar_function(qn24),
         age_tobacco_first = age_group_tobacco_function(qn29),
         ads_Internet = ads_Internet_function(qn40),
         ads_news_mag = ads_news_mag_function(qn41),
         actors_tobacco = actors_tobacco_function(qn45),
         noone_live_together_used_tobacco = noone_live_together_used_tobacco_function(qn66j),
         timeframe_quit = timeframe_quit_2012_2015_function(qn56),
         qn4_agg = hisp_2013_agg(qn4b, qn4c, qn4d, qn4e)) %>%
  mutate(multiethnic = multi_ethnic_tag(qn5a, qn5b, qn5c, qn5d, qn5e, qn4_agg)) %>%
  mutate(ethnicity = ethnicity_function(qn5a, qn5b, qn5c, qn5d, qn5e, qn4_agg, multiethnic)) %>%
  mutate(cigarette_subsegment = ifelse(qn9==2 & qn10==4 & qn11 == 4 & qn12 == 4, 'Non-Trier: Not at Risk',
                                       (ifelse(qn9==2 & (qn10 != 4 | qn11 != 4 |qn12 != 4), 'Non-Trier: At Risk', cigarette_segment)
                                       ))) %>%
  select(year, weighted_pop, ethnicity, grade_level, cigarette_segment, cigarette_subsegment, ecigarette_segment, age_cigar_first, age_tobacco_first, ads_Internet, ads_news_mag, actors_tobacco, noone_live_together_used_tobacco, timeframe_quit)


#Creates dataset for 2014
NYTS_2014_subset <- NYTS_2014 %>%
  mutate(cigarette_segment = cig_segment_function(qn7, qn12, qn13),
         grade_level = grade_function(qn3),
         ecigarette_segment = ecig_segment_function(eelcigt_r),
         weighted_pop = wt,
         age_cigar_first = age_group_cigar_function(qn23),
         age_tobacco_first = age_group_tobacco_function(qn28),
         ads_Internet = ads_Internet_function(qn64),
         ads_news_mag = ads_news_mag_function(qn65),
         actors_tobacco = actors_tobacco_function(qn67),
         noone_live_together_used_tobacco = noone_live_together_used_tobacco_function(qn80j),
         timeframe_quit = timeframe_quit_2012_2015_function(qn50),
         qn4_agg = hisp_2013_agg(qn4b, qn4c, qn4d, qn4e)) %>%
  mutate(multiethnic = multi_ethnic_tag(qn5a, qn5b, qn5c, qn5d, qn5e, qn4_agg)) %>%
  mutate(ethnicity = ethnicity_function(qn5a, qn5b, qn5c, qn5d, qn5e, qn4_agg, multiethnic)) %>%        
  mutate(cigarette_subsegment = ifelse(qn7==2 & qn8==4 & qn9 == 4 & qn10 == 4, 'Non-Trier: Not at Risk',
                                       (ifelse(qn7==2 & (qn8 != 4 | qn9 != 4 |qn10 != 4), 'Non-Trier: At Risk', cigarette_segment)
                                       ))) %>%
  select(year, weighted_pop, ethnicity, grade_level, cigarette_segment, cigarette_subsegment, ecigarette_segment, age_cigar_first, age_tobacco_first, ads_Internet, ads_news_mag, actors_tobacco, noone_live_together_used_tobacco, timeframe_quit)


#Creates dataset for 2015
NYTS_2015_subset <- NYTS_2015 %>%
  mutate(cigarette_segment = cig_segment_function(qn6, qn11, qn12),
         grade_level = grade_function(qn3),
         ecigarette_segment = ecig_segment_function(eelcigt_r),
         weighted_pop = wt,
         age_cigar_first = age_group_cigar_function(qn21),
         age_tobacco_first = age_group_tobacco_function(qn25),
         ads_Internet = ads_Internet_function(qn65),
         ads_news_mag = ads_news_mag_function(qn66),
         actors_tobacco = actors_tobacco_function(qn68),
         noone_live_together_used_tobacco = noone_live_together_used_tobacco_function(qn77j),
         timeframe_quit = timeframe_quit_2012_2015_function(qn53),
         qn4_agg = hisp_2013_agg(qn4b, qn4c, qn4d, qn4e)) %>%
  mutate(multiethnic = multi_ethnic_tag(qn5a, qn5b, qn5c, qn5d, qn5e, qn4_agg)) %>%
  mutate(ethnicity = ethnicity_function(qn5a, qn5b, qn5c, qn5d, qn5e, qn4_agg, multiethnic)) %>%    
  mutate(cigarette_subsegment = ifelse(qn6==2 & qn7==4 & qn8 == 4 & qn9 == 4, 'Non-Trier: Not at Risk',
                                       (ifelse(qn6==2 & (qn7 != 4 | qn8 != 4 |qn9 != 4), 'Non-Trier: At Risk', cigarette_segment)
                                       ))) %>%
  select(year, weighted_pop, ethnicity, grade_level, cigarette_segment, cigarette_subsegment, ecigarette_segment, age_cigar_first, age_tobacco_first, ads_Internet, ads_news_mag, actors_tobacco, noone_live_together_used_tobacco, timeframe_quit)


#COMBINES DATASETS ACROSS THE FIVE YEARS
NYTS_5YEAR_subset <- rbind(NYTS_2011_subset, NYTS_2012_subset, NYTS_2013_subset, NYTS_2014_subset, NYTS_2015_subset)

#Set up dataframe for combined 5 year dataset
df_5YEAR <- data.frame(NYTS_5YEAR_subset)

#Take a look of first few rows in the dataframe
head(df_5YEAR)

#Check dimension of data frame
dim(df_5YEAR)

#Check unique values of newly added fields
unique(df_5YEAR[,'age_cigar_first'])
unique(df_5YEAR[,'age_tobacco_first'])
unique(df_5YEAR[,'ads_Internet'])
unique(df_5YEAR[,'ads_news_mag'])
unique(df_5YEAR[,'actors_tobacco'])
unique(df_5YEAR[,'noone_live_together_used_tobacco'])
unique(df_5YEAR[,'timeframe_quit'])



#ASSIGNS FACTOR LEVELS TO SOME VARIABLES
NYTS_5YEAR_subset$cigarette_segment <- factor(NYTS_5YEAR_subset$cigarette_segment, levels = c("Non-Trier", "Experimenter", "Current Smoker", "Former Smoker"))
NYTS_5YEAR_subset$grade_level <- factor(NYTS_5YEAR_subset$grade_level, levels = c("Grade 6", "Grade 7", "Grade 8", "Grade 9", "Grade 10", "Grade 11", "Grade 12"))

#Create data for snapshot of Cigarette Segment by Year, with percentages
NYTS_by_year_cigsegment <-
  NYTS_5YEAR_subset %>%
  group_by(year, cigarette_segment) %>%
  filter(!is.na(cigarette_segment)) %>%
  summarize(weighted_pop = sum(weighted_pop)) %>%
  mutate(percent = weighted_pop/sum(weighted_pop)*100)

#Create data for snapshot of Non-Trier Subsegment by Year
NYTS_by_year_cigsubsegment <-
  NYTS_5YEAR_subset %>%
  group_by(year, cigarette_subsegment) %>%
  filter(!is.na(cigarette_subsegment)) %>%
  summarize(weighted_pop = sum(weighted_pop)) %>%
  mutate(percent = weighted_pop/sum(weighted_pop) * 100) %>%
  filter(cigarette_subsegment == 'Non-Trier: At Risk' | cigarette_subsegment == 'Non-Trier: Not at Risk')

#Create data for snapshot of Segment by Year and Grade Level
NYTS_by_year_cigsegment_grade <-
  NYTS_5YEAR_subset %>%
  group_by(grade_level, year, cigarette_segment) %>%
  filter(!is.na(cigarette_segment) & !is.na(grade_level)) %>%
  summarize(weighted_pop = sum(weighted_pop)) %>%
  mutate(percent = weighted_pop/sum(weighted_pop)*100) 

#Create data for snapshot of Segment by Year and Ethnicity
NYTS_by_year_cigsegment_ethnicity <-
  NYTS_5YEAR_subset %>%
  group_by(ethnicity, year, cigarette_segment) %>%
  filter(!is.na(cigarette_segment) & !is.na(ethnicity)) %>%
  summarize(weighted_pop = sum(weighted_pop)) %>%
  mutate(percent = weighted_pop/sum(weighted_pop)*100) 

#Create data for snapshot of E-Cigarette Segment by Year
NYTS_by_year_ecigsegment <-
  NYTS_5YEAR_subset %>%
  group_by(year, ecigarette_segment) %>%
  filter(!is.na(ecigarette_segment)) %>%
  summarize(weighted_pop = sum(weighted_pop)) %>%
  mutate(percent = weighted_pop/sum(weighted_pop)*100) 

#Create data for snapshot of E-Cigarette Segment by Year and Grade
NYTS_by_year_ecigsegment_grade <-
  NYTS_5YEAR_subset %>%
  group_by(grade_level, year, ecigarette_segment) %>%
  filter(!is.na(ecigarette_segment) & !is.na(grade_level)) %>%
  summarize(weighted_pop = sum(weighted_pop)) %>%
  mutate(percent = weighted_pop/sum(weighted_pop)*100) 

#Create data for E-Cigarette Segment for Non-Triers
NYTS_by_year_ecigsegment_nontriers <-
  NYTS_5YEAR_subset %>%
  filter(cigarette_segment=='Non-Trier') %>%
  group_by(year, ecigarette_segment) %>%
  filter(!is.na(ecigarette_segment)) %>%
  summarize(weighted_pop = sum(weighted_pop)) %>%
  mutate(percent = weighted_pop/sum(weighted_pop)*100) 



#VISUALIZATIONS
#VISUALIZATIONS
#Create line chart mapping segment percentages, with custom color choices
ggplot(NYTS_by_year_cigsegment, aes(year, percent, color=cigarette_segment)) +
  geom_line(size=1) +
  geom_point(size=3) +
  scale_color_manual(values=c("#003366", "#CC0066", "#CC0000", "#CCCCCC"))+
  labs(color="Segment", y="Percent of Teens (%)", x="Year") +
  theme(axis.text.x = element_text(size=8), axis.title.x = element_text(size=9), axis.text.y = element_text(size=8), axis.title.y = element_text(size=9), legend.text = element_text(size=8), legend.title = element_text(size=8))

#Create line chart mapping Non-Trier segment percentages, with custom color choices
ggplot(NYTS_by_year_cigsubsegment, aes(year, weighted_pop, fill=cigarette_subsegment)) +
  geom_area(position="fill") +
  scale_color_manual(values=c("#FF9999", "#339966")) +
  labs(fill="Subsegment", y="Percent of Non-Triers", x="Year") +
  theme(axis.text.x = element_text(size=8), 
        axis.title.x = element_text(size=9), 
        axis.text.y = element_text(size=8), 
        axis.title.y = element_text(size=9), 
        legend.text = element_text(size=8), 
        legend.title = element_text(size=9))


#Creates stacked area chart mapping segment percentages, with custom color choices
ggplot(NYTS_by_year_cigsegment_grade, aes(year, percent, color=cigarette_segment)) +
  geom_line(size=1) +
  geom_point(size=3) +
  scale_color_manual(values=c("#003366", "#CC0066", "#CC0000", "#CCCCCC"))+
  labs(color="Segment", y="Percent of Teens (%)", x="Year")+
  theme(axis.text.x = element_text(size=8), 
        axis.title.x = element_text(size=9), 
        axis.text.y = element_text(size=8), 
        axis.title.y = element_text(size=9), 
        legend.text = element_text(size=8), 
        legend.title = element_text(size=9)) +
  facet_wrap(~grade_level)


#Creates a line chart mapping E-cigarette segment percentages, with custom color choices
ggplot(NYTS_by_year_ecigsegment, aes(year, percent, color=ecigarette_segment)) +
  geom_line(size=1) +
  geom_point(size=3) +
  labs(color="Segment", y="Percent of Teens (%)", x="Year")+
  theme(axis.text.x = element_text(size=8), 
        axis.title.x = element_text(size=9), 
        axis.text.y = element_text(size=8), 
        axis.title.y = element_text(size=9), 
        legend.text = element_text(size=8), 
        legend.title = element_text(size=9)) +  
  scale_color_manual(values=c("#CCCCCC", "#CC0000"))


#ads_Internet cleansing

#Convert ads_Internet categories to ordinal variable
df_5YEAR$ads_Internet = ordered(df_5YEAR$ads_Internet, levels = c('Never', 'Rarely', 'Sometimes', 'Most of the time', 'Always'))

#Examine levels for the ordered variable ads_Internet
levels(df_5YEAR$ads_Internet)

#Find counts for ads_Internet
table(df_5YEAR$ads_Internet)

#Find count for NA in ads_Internet
sum(is.na(df_5YEAR$ads_Internet))

#Plot bar chart to show distribution of ordinal variable and missing value for ads_Internet
ggplot(df_5YEAR) + geom_bar(aes(x = ads_Internet, fill = 'red')) + labs(x = 'Frequency') + labs(y = 'Count') + labs(title = 'How often does youth see tobacco ads on Internet?', subtitle = 'After')



#Plot “Before” bar chart to show distribution of ads_Internet without data cleaning and processing  for comparison
#Revert ads_Internet back to no function applied, just input (question number) from different years.

NYTS_2011_subset <- NYTS_2011 %>%
   mutate(cigarette_segment = cig_segment_function(qn7, qn12, qn13),
          grade_level = grade_function(qn3),
          ecigarette_segment = ecig_segment_function(eelcigt_r),
          weighted_pop = wt,
          age_cigar_first = age_group_cigar_function(qn22),
          age_tobacco_first = age_group_tobacco_function(qn27),
          ads_Internet = qn40,
          ads_news_mag = ads_news_mag_function(qn41),
          actors_tobacco = actors_tobacco_function(qn46),
          noone_live_together_used_tobacco = noone_live_together_used_tobacco_function(qn67j),
          timeframe_quit = timeframe_quit_2011_function(qn58),
          qn4_agg = hisp_2011_agg(qn4)) %>%
   mutate(multiethnic = multi_ethnic_tag(qn5a, qn5b, qn5c, qn5d, qn5e, qn4_agg)) %>%
   mutate(ethnicity = ethnicity_function(qn5a, qn5b, qn5c, qn5d, qn5e, qn4_agg, multiethnic)) %>%
   mutate(cigarette_subsegment = ifelse(qn7==2 & qn8==4 & qn9 == 3 & qn10 == 4, 'Non-Trier: Not at Risk',
                                        (ifelse(qn7==2 & (qn8 != 4 | qn9 != 3 |qn10 != 4), 'Non-Trier: At Risk', cigarette_segment)
                                        ))) %>%
   select(year, weighted_pop, ethnicity, grade_level, cigarette_segment, cigarette_subsegment, ecigarette_segment, age_cigar_first, age_tobacco_first, ads_Internet, ads_news_mag, actors_tobacco, noone_live_together_used_tobacco, timeframe_quit)

   
NYTS_2012_subset <- NYTS_2012 %>%
   mutate(cigarette_segment = cig_segment_function(qn7, qn12, qn13),
          grade_level = grade_function(qn3),
          ecigarette_segment = ecig_segment_function(eelcigt_r),
          weighted_pop = wt,
          age_cigar_first = age_group_cigar_function(qn24),
          age_tobacco_first = age_group_tobacco_function(qn30),
          ads_Internet = qn45,
          ads_news_mag = ads_news_mag_function(qn46),
          actors_tobacco = actors_tobacco_function(qn51),
          noone_live_together_used_tobacco = noone_live_together_used_tobacco_function(qn80e),
          timeframe_quit = timeframe_quit_2012_2015_function(qn57),
          qn4_agg = hisp_2011_agg(qn4)) %>%
   mutate(multiethnic = multi_ethnic_tag(qn5a, qn5b, qn5c, qn5d, qn5e, qn4_agg)) %>%
   mutate(ethnicity = ethnicity_function(qn5a, qn5b, qn5c, qn5d, qn5e, qn4_agg, multiethnic)) %>%
   mutate(cigarette_subsegment = ifelse(qn7==2 & qn8==4 & qn9 == 4 & qn10 == 4, 'Non-Trier: Not at Risk',
                                        (ifelse(qn7==2 & (qn8 != 4 | qn9 != 4 |qn10 != 4), 'Non-Trier: At Risk', cigarette_segment)
                                        ))) %>%
   select(year, weighted_pop, ethnicity, grade_level, cigarette_segment, cigarette_subsegment, ecigarette_segment, age_cigar_first, age_tobacco_first, ads_Internet, ads_news_mag, actors_tobacco, noone_live_together_used_tobacco, timeframe_quit)
 
   
NYTS_2013_subset <- NYTS_2013 %>%
   mutate(cigarette_segment = cig_segment_function(qn9, qn14, qn15),
          grade_level = grade_function(qn3),
          ecigarette_segment = ecig_segment_function(eelcigt_r),
          weighted_pop = wt,
          age_cigar_first = age_group_cigar_function(qn24),
          age_tobacco_first = age_group_tobacco_function(qn29),
          ads_Internet = qn40,
          ads_news_mag = ads_news_mag_function(qn41),
          actors_tobacco = actors_tobacco_function(qn45),
          noone_live_together_used_tobacco = noone_live_together_used_tobacco_function(qn66j),
          timeframe_quit = timeframe_quit_2012_2015_function(qn56),
          qn4_agg = hisp_2013_agg(qn4b, qn4c, qn4d, qn4e)) %>%
   mutate(multiethnic = multi_ethnic_tag(qn5a, qn5b, qn5c, qn5d, qn5e, qn4_agg)) %>%
   mutate(ethnicity = ethnicity_function(qn5a, qn5b, qn5c, qn5d, qn5e, qn4_agg, multiethnic)) %>%
   mutate(cigarette_subsegment = ifelse(qn9==2 & qn10==4 & qn11 == 4 & qn12 == 4, 'Non-Trier: Not at Risk',
                                        (ifelse(qn9==2 & (qn10 != 4 | qn11 != 4 |qn12 != 4), 'Non-Trier: At Risk', cigarette_segment)
                                        ))) %>%
   select(year, weighted_pop, ethnicity, grade_level, cigarette_segment, cigarette_subsegment, ecigarette_segment, age_cigar_first, age_tobacco_first, ads_Internet, ads_news_mag, actors_tobacco, noone_live_together_used_tobacco, timeframe_quit)
   

NYTS_2014_subset <- NYTS_2014 %>%
   mutate(cigarette_segment = cig_segment_function(qn7, qn12, qn13),
          grade_level = grade_function(qn3),
          ecigarette_segment = ecig_segment_function(eelcigt_r),
          weighted_pop = wt,
          age_cigar_first = age_group_cigar_function(qn23),
          age_tobacco_first = age_group_tobacco_function(qn28),
          ads_Internet = qn64,
          ads_news_mag = ads_news_mag_function(qn65),
          actors_tobacco = actors_tobacco_function(qn67),
          noone_live_together_used_tobacco = noone_live_together_used_tobacco_function(qn80j),
          timeframe_quit = timeframe_quit_2012_2015_function(qn50),
          qn4_agg = hisp_2013_agg(qn4b, qn4c, qn4d, qn4e)) %>%
   mutate(multiethnic = multi_ethnic_tag(qn5a, qn5b, qn5c, qn5d, qn5e, qn4_agg)) %>%
   mutate(ethnicity = ethnicity_function(qn5a, qn5b, qn5c, qn5d, qn5e, qn4_agg, multiethnic)) %>%        
   mutate(cigarette_subsegment = ifelse(qn7==2 & qn8==4 & qn9 == 4 & qn10 == 4, 'Non-Trier: Not at Risk',
                                        (ifelse(qn7==2 & (qn8 != 4 | qn9 != 4 |qn10 != 4), 'Non-Trier: At Risk', cigarette_segment)
                                        ))) %>%
   select(year, weighted_pop, ethnicity, grade_level, cigarette_segment, cigarette_subsegment, ecigarette_segment, age_cigar_first, age_tobacco_first, ads_Internet, ads_news_mag, actors_tobacco, noone_live_together_used_tobacco, timeframe_quit)


NYTS_2015_subset <- NYTS_2015 %>%
   mutate(cigarette_segment = cig_segment_function(qn6, qn11, qn12),
          grade_level = grade_function(qn3),
          ecigarette_segment = ecig_segment_function(eelcigt_r),
          weighted_pop = wt,
          age_cigar_first = age_group_cigar_function(qn21),
          age_tobacco_first = age_group_tobacco_function(qn25),
          ads_Internet = qn65,
          ads_news_mag = ads_news_mag_function(qn66),
          actors_tobacco = actors_tobacco_function(qn68),
          noone_live_together_used_tobacco = noone_live_together_used_tobacco_function(qn77j),
          timeframe_quit = timeframe_quit_2012_2015_function(qn53),
          qn4_agg = hisp_2013_agg(qn4b, qn4c, qn4d, qn4e)) %>%
   mutate(multiethnic = multi_ethnic_tag(qn5a, qn5b, qn5c, qn5d, qn5e, qn4_agg)) %>%
   mutate(ethnicity = ethnicity_function(qn5a, qn5b, qn5c, qn5d, qn5e, qn4_agg, multiethnic)) %>%    
   mutate(cigarette_subsegment = ifelse(qn6==2 & qn7==4 & qn8 == 4 & qn9 == 4, 'Non-Trier: Not at Risk',
                                        (ifelse(qn6==2 & (qn7 != 4 | qn8 != 4 |qn9 != 4), 'Non-Trier: At Risk', cigarette_segment)
                                        ))) %>%
   select(year, weighted_pop, ethnicity, grade_level, cigarette_segment, cigarette_subsegment, ecigarette_segment, age_cigar_first, age_tobacco_first, ads_Internet, ads_news_mag, actors_tobacco, noone_live_together_used_tobacco, timeframe_quit)


NYTS_5YEAR_subset <- rbind(NYTS_2011_subset, NYTS_2012_subset, NYTS_2013_subset, NYTS_2014_subset, NYTS_2015_subset)


df_5YEAR <- data.frame(NYTS_5YEAR_subset)


#Now plot "Before" data cleansing picture for ads_Internet  for comparison
ggplot(df_5YEAR) + geom_bar(aes(x = ads_Internet, fill = 'red')) + labs(x = 'Frequency') + labs(y = 'Count') + labs(title = 'How often does youth see tobacco ads on Internet?', subtitle = 'Before')





#ads_news_mag cleansing

#Convert ads_news_mag categories to ordinal variable
df_5YEAR$ads_news_mag = ordered(df_5YEAR$ads_news_mag, levels = c('Never', 'Rarely', 'Sometimes', 'Most of the time', 'Always'))

#Examine levels for the ordered variable ads_news_mag
levels(df_5YEAR$ads_news_mag)

#Find counts for ads_news_mag
table(df_5YEAR$ads_news_mag)

#Find count for NA in ads_news_mag
sum(is.na(df_5YEAR$ads_news_mag))

#Plot bar chart to show distribution of ordinal variable and missing value for ads_news_mag
ggplot(df_5YEAR) + geom_bar(aes(x = ads_news_mag, fill = 'red')) + labs(x = 'Frequency') + labs(y = 'Count') + labs(title = 'How often does youth see tobacco ads on News/Magazine?', subtitle = 'After')

#Plot “Before” bar chart to show distribution of ads_news_mag without data cleaning and processing for comparison
#Revert ads_news_mag back to no function applied before executing the ggplot code below, refer to how this step is done for ads_Internet previously.

ggplot(df_5YEAR) + geom_bar(aes(x = ads_news_mag, fill = 'red')) + labs(x = 'Frequency') + labs(y = 'Count') + labs(title = 'How often does youth see tobacco ads on News/Magazine?', subtitle = 'Before')


#actors_tobacco cleansing

#Convert actors_tobacco categories to ordinal variable
df_5YEAR$actors_tobacco = ordered(df_5YEAR$actors_tobacco, levels = c('Never', 'Rarely', 'Sometimes', 'Most of the time', 'Always'))

#Examine levels for the ordered variable actors_tobacco
levels(df_5YEAR$actors_tobacco)

#Find counts for actors_tobacco
table(df_5YEAR$actors_tobacco)

#Find count for NA in actors_tobacco
sum(is.na(df_5YEAR$actors_tobacco))

#Plot bar chart to show distribution of ordinal variable and missing value for actors_tobacco
ggplot(df_5YEAR) + geom_bar(aes(x = actors_tobacco, fill = 'red')) + labs(x = 'Frequency') + labs(y = 'Count') + labs(title = 'How often does youth see actors use tobacco products on TV/movies?', subtitle = 'After')

#Plot “Before” bar chart to show distribution of actors_tobacco without data cleaning and processing for comparison
#Revert actors_tobacco back to no function applied before executing the ggplot code below, refer to how this step is done for ads_Internet previously.

ggplot(df_5YEAR) + geom_bar(aes(x = actors_tobacco, fill = 'red')) + labs(x = 'Frequency') + labs(y = 'Count') + labs(title = 'How often does youth see actors use tobacco products on TV/movies?', subtitle = 'Before')



#noone_live_together_used_tobacco cleansing


#Find counts for noone_live_together_used_tobacco
table(df_5YEAR$noone_live_together_used_tobacco)

#Find count for NA in noone_live_together_used_tobacco
sum(is.na(df_5YEAR$noone_live_together_used_tobacco))

#Plot bar chart to show distribution of boolean value TRUE and missing value for noone_live_together_used_tobacco
ggplot(df_5YEAR) + geom_bar(aes(x = noone_live_together_used_tobacco, fill = 'red')) + labs(x = 'Frequency') + labs(y = 'Count') + labs(title = 'Does no one live with you use tobacco products?', subtitle = 'After')

#Plot “Before” bar chart to show distribution of noone_live_together_used_tobacco without data cleaning and processing for comparison
#Revert noone_live_together_used_tobacco back to no function applied before executing the ggplot code below, refer to how this step is done for ads_Internet previously.

ggplot(df_5YEAR) + geom_bar(aes(x = noone_live_together_used_tobacco, fill = 'red')) + labs(x = 'Frequency') + labs(y = 'Count') + labs(title = 'Does no one live with you use tobacco products?', subtitle = 'Before')




#timeframe_quit cleansing


#Convert timeframe_quit categories to ordinal variable
df_5YEAR$timeframe_quit = ordered(df_5YEAR$timeframe_quit, levels = c('Never used tobacco', 'Within 30 days', 'Within 6 months', 'Within > 6 months', 'Not thinking about quitting'))

#Examine levels for the ordered variable timeframe_quit
levels(df_5YEAR$timeframe_quit)

#Find counts for timeframe_quit
table(df_5YEAR$timeframe_quit)

#Find count for NA in timeframe_quit
sum(is.na(df_5YEAR$timeframe_quit))

#Plot bar chart to show distribution of ordinal variable and missing value for timeframe_quit
ggplot(df_5YEAR) + geom_bar(aes(x = timeframe_quit, fill = 'red')) + labs(x = 'Frequency') + labs(y = 'Count') + labs(title = 'How soon is youth seriously thinking about quitting all tobacco products?', subtitle = 'After')

#Plot “Before” bar chart to show distribution of timeframe_quit without data cleaning and processing for comparison
#Revert timeframe_quit back to no function applied before executing the ggplot code below, refer to how this step is done for ads_Internet previously.

ggplot(df_5YEAR) + geom_bar(aes(x = timeframe_quit, fill = 'red')) + labs(x = 'Frequency') + labs(y = 'Count') + labs(title = 'How soon is youth seriously thinking about quitting all tobacco products?', subtitle = 'Before')




*******

#Deliverable 2

df_5YEAR <- read.csv("df_5YEAR.csv")

unique(df_5YEAR$timeframe_quit)
#[1] "Not think about quitting" "Never used tobacco"      
#[3] "Within 30 days"           NA                        
#[5] "Within > 6 months"        "Within 6 months"    

#create a quit_flag to show if a teen plans to quit smoking at all
df_5YEAR$quit_flag <- ifelse(grepl("Within", df_5YEAR$timeframe_quit, ignore.case = TRUE), 1, ifelse(grepl("Not", df_5YEAR$timeframe_quit, ignore.case = TRUE), 0, 2))

unique(df_5YEAR$quit_flag)
#[1] 0 2 1

#before converting to ordinal variable
levels(df_5YEAR$timeframe_quit)
#[1] "Never used tobacco"       "Not think about quitting"
#[3] "Within > 6 months"        "Within 30 days"          
#[5] "Within 6 months" 

#Convert timeframe_quit categories to ordinal variable
df_5YEAR$timeframe_quit = ordered(df_5YEAR$timeframe_quit, levels = c('Never used tobacco', 'Within 30 days', 'Within 6 months', 'Within > 6 months', 'Not thinking about quitting'))

#after converting to ordinal variable
levels(df_5YEAR$timeframe_quit)
#[1] "Never used tobacco"          "Within 30 days"             
#[3] "Within 6 months"             "Within > 6 months"          
#[5] "Not thinking about quitting"

unique(df_5YEAR$timeframe_quit)
#[1] Not think about quitting Never used tobacco      
#[3] Within 30 days           <NA>                    
#[5] Within > 6 months        Within 6 months         
#5 Levels: Never used tobacco ... Within 6 months


#change timeframe_quit to factor
#timeframe_quit_f <- factor(df_5YEAR$timeframe_quit)

library(ggplot2)


#count of timeframe_quit by year
ggplot(df_5YEAR, aes(x = timeframe_quit)) + geom_bar(stat="count", fill = '#56B4E9', color = '#56B4E9', alpha = 0.6) + facet_grid(year~.) + geom_text(stat = 'count', aes(label=..count..), vjust = 0.2) + labs(x = 'Timeframe quit', y = 'Count') + ggtitle('Count of timeframes thinking of quitting smoking by year') + theme(plot.title = element_text(hjust = 0.5), panel.background = element_rect(fill = NA, color = "black"))


#plot timeframe_quit_f to show counts
#qplot(timeframe_quit_f, xlab = 'Timeframe of quitting', ylab = 'Count') + geom_text(stat = 'count', aes(label=..count..), vjust = -1)


#create timeframe_quit_chart
timeframe_quit_chart <- ggplot() + geom_bar(aes(y = timeframe_quit, x = year, fill = factor(timeframe_quit)), data = df_5YEAR, stat="identity") + labs(x = 'Year', y = 'Count') + ggtitle('Composition of timeframes teens think of quitting smoking')

#plot chart
timeframe_quit_chart


# select data with value 0 or 1 in quit_flag for predictive modeling purpose
# predict whether a teen will think about quitting and if so, in what timeframe
# quit_flag == 0 means 'Not think about quitting'
# quit_flag == 1 means thinkin about quitting, and within a certain period
# quit_flag == 2 means 'Never used tobacco' or NA

library(dplyr)
selected_data <- filter(df, df$quit_flag %in% c(0,1))

#remove first column: auto-generated counts without column name, since new extract will have that again
selected_data <- selected_data[, -1]

#save as a separate file named 'df_5YEAR_selected.csv'
write.csv(selected_data, file = '/Users/qinqingao/Desktop/Columbia/Courses/Fall 2017/APAN 5200_Frameworks and Methods/Project/Deliverable 2/df_5YEAR_selected.csv')


library(BCA)

#custermize rpart control
library(rpart)
rpartContr = rpart.control(minsplit = 200, cp = 1e-04, minbucket = 100)

#fit data to decision tree model
tree1 = rpart(quit_flag~., control = rpartContr, data = df_5YEAR)

#plot tree
library(partykit)
plot(as.party(tree1))


#prune the tree
library(caret)

set.seed(1234)

Prunedtree1 = rpart(quit_flag ~ ., data = df_5YEAR,cp = 0.06015038)

plot(as.party(Prunedtree1))



set.seed(2166)

#define training set
trainingRows <- createDataPartition(df_5YEAR$quit_flag, p = .60, list= FALSE)

#create training set
trainData <- df_5YEAR[trainingRows, ]

#create test set
testData <- df_5YEAR[-trainingRows, ]



