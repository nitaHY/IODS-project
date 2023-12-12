##Nita Mettinen, 10.12.2023
#IODS-course week 6, Data Wrangling: Preparation of BPRS and RATS data sets to analyses. 
#Data retrieved 10.12.2023 from: https://raw.githubusercontent.com/KimmoVehkalahti/MABS/master/Examples/data/BPRS.txt
#and from: https://raw.githubusercontent.com/KimmoVehkalahti/MABS/master/Examples/data/rats.txt

#Install/activate the needed libraries
library(dplyr)
library(readr)
library(tidyverse)
#set the working directory to data folder:
#(I'm in the IODS-project folder at this point)
setwd("data")

#1. read and explore data
BPRS = read.table("https://raw.githubusercontent.com/KimmoVehkalahti/MABS/master/Examples/data/BPRS.txt", header = T)
RATS = read.table("https://raw.githubusercontent.com/KimmoVehkalahti/MABS/master/Examples/data/rats.txt", header = T)
#more information about the data sets are here based on the course material:
#https://moodle.helsinki.fi/pluginfile.php/5358048/course/section/735857/MABS4IODS-Part6.pdf, 
#wich is not the original source of the data information but works well
#enough here when I summarize the contents of data sets. 

#structur and dimensions
dim(BPRS)
#40 rows and 11 columns
colnames(BPRS)
#[1] "treatment" "subject"   "week0"     "week1"     "week2"     "week3"     "week4"    
#[8] "week5"     "week6"     "week7"     "week8" 

str(BPRS)
#all the columns in the data include numerical data in integer form

#maybe better to watch the actual dataframe here to see what the "wide form" means
head(BPRS)
#This form describes the wide-format better in my opinion.
#Data is arranged so that each male subject (subject-column) has one row of 
#information and weekly repeated BPRS, brief psychiatric rating scale, 
#measures are shown in 
#different columns of the data (from week0 to week8). There is also 
#treatment-column where the treatment group of a subject can be seen. 

#same to RATS
dim(RATS)
#16 rows and 13 columns 
colnames(RATS)
#[1] "ID"    "Group" "WD1"   "WD8"   "WD15"  "WD22"  "WD29"  "WD36"  "WD43"  "WD44" 
#[11] "WD50"  "WD57"  "WD64" 


str(RATS)
#RATS data also includes numerical data in integer format. 

head(RATS)
#In the RATS data, each row includes data of one rat, identified by the number
#found in ID-column.For each rat there are 11 weight (grams) measurements in 
#the columns starting with "WD". The number after WD tells the day count 
#from the beginning of the experiment. 
#There is also "Group" column in which the diet group of a rat is specified. 

#2. Factors to factors
#in BPRS, there are two factor variables: treatment and subject
BPRS$treatment = as.factor(BPRS$treatment)
BPRS$subject = as.factor(BPRS$subject)

# in RATS data, ID and Group are factor variables
RATS$ID = as.factor(RATS$ID)
RATS$Group = as.factor(RATS$Group)

#3. data from wide to long format and addition of week and Time variables
#BPRS
BPRSL <-  pivot_longer(BPRS, cols = -c(treatment, subject),
                       names_to = "weeks", values_to = "bprs") %>%
  arrange(weeks) #order by weeks variable

# week number variable
BPRSL <-  BPRSL %>% 
  mutate(week = as.integer(substr(weeks,start = 5, stop = 5)))

#RATS
RATSL <- pivot_longer(RATS, cols = -c(ID, Group), 
                      names_to = "WD",
                      values_to = "Weight") %>% 
  mutate(Time = as.integer(substr(WD,start = 3, stop = 5))) %>%
  arrange(Time)

#4.Serious look to data, variable names and summaries and 
#comparison of wide and long formats

#BPRSL
dim(BPRS) #just for comparison
#40 rows and 11 columns
str(BPRSL)
# tibble [360 × 5] (S3: tbl_df/tbl/data.frame)
# $ treatment: Factor w/ 2 levels "1","2": 1 1 1 1 1 1 1 1 1 1 ...
# $ subject  : Factor w/ 20 levels "1","2","3","4",..: 1 2 3 4 5 6 7 8 9 10 ...
# $ weeks    : chr [1:360] "week0" "week0" "week0" "week0" ...
# $ bprs     : int [1:360] 42 58 54 55 72 48 71 30 41 57 ...
# $ week     : int [1:360] 0 0 0 0 0 0 0 0 0 0 ...
head(BPRSL)
table(BPRSL$subject, BPRSL$week)
# 0 1 2 3 4 5 6 7 8
# 1  2 2 2 2 2 2 2 2 2
# 2  2 2 2 2 2 2 2 2 2
# 3  2 2 2 2 2 2 2 2 2
# 4  2 2 2 2 2 2 2 2 2
# 5  2 2 2 2 2 2 2 2 2 
# ... and so on, 20 rows in total
table(BPRSL$subject, BPRSL$weeks)
# week0 week1 week2 week3 week4 week5 week6 week7 week8
# 1      2     2     2     2     2     2     2     2     2
# 2      2     2     2     2     2     2     2     2     2
# 3      2     2     2     2     2     2     2     2     2
# 4      2     2     2     2     2     2     2     2     2
# 5      2     2     2     2     2     2     2     2     2
# ... 20 rows in total

table(BPRSL$subject, BPRSL$treatment )
#    1 2
# 1  9 9
# 2  9 9
# 3  9 9
# 4  9 9
# 5  9 9
#and so on, 20 rows here also in total

#This data is actually very confusing: the pdf-material says there are 40 
#different males in the data but for some reason, somebody has given 
#exactly the same subject IDs (1 to 20) for the two treatment groups. 
#If I look at the subject IDs only, it seems that each subject appears twice in the data,
# and if I look the measurement counts (weekly), it seems that one individual
# has two measurements for each week. This is not the truth, however, and I should remember 
# to include group information with subject ID if want to be sure that I'm processing
# information of a certain individual. 
# Otherwise, the data looks what I expected: there are now one row per one week
# including one weekly measurement for each subject. So, each subject has now 9 
# rows in the longer format data while there were only one row for each subject
# in the wide format, containing all the 9 measurements of the individual in 
#the same row. 
colnames(BPRSL)
#"treatment" "subject"   "weeks"     "bprs"      "week" 
head(BPRSL,30)
tail(BPRSL)
# From the column names, views of the data (head and tail) and table of
# week-variable we can see that weeks and week variables are found 40 times
# each from the data, meaning that for every subject there is one row for each week. 
# Column bprs shows the bprs = brief psychiatric rating scale, 
# which is a score consisting of sum of evaluated numerical values for certain 
#symptoms, for each week. The treatment columns shows 
# the tretment group of the subject, including one of the two possible values
# (1 or 2) and is essential information to differentiate between the 40 subjects. 

#summary of continuous bprs-variable, for all the measurements from different weeks
# and all patients in different treatment groups and :
summary(BPRSL$bprs)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#18.00   27.00   35.00   37.66   43.00   95.00
#values range from 18 to 95, having mean value 37.66. Median is 37.66.
#summaries by treatment group:
summary(BPRSL[which(BPRSL$treatment == 1),"bprs"])
# bprs      
# Min.   :19.00  
# 1st Qu.:28.00  
# Median :36.00  
# Mean   :37.37  
# 3rd Qu.:43.00  
# Max.   :77.00  
summary(BPRSL[which(BPRSL$treatment == 2),"bprs"])
#When we compare groupwise summaries, we can observe, that they are actually
#quite similar in range of values and mean and median values. This may or may not tell 
# anything useful about the effects of the treatments, as individual changes are
# more important.



#RATSL
dim(RATSL)
#176 rows and 5 columns
str(RATSL)
# tibble [176 × 5] (S3: tbl_df/tbl/data.frame)
# $ ID    : Factor w/ 16 levels "1","2","3","4",..: 1 2 3 4 5 6 7 8 9 10 ...
# $ Group : Factor w/ 3 levels "1","2","3": 1 1 1 1 1 1 1 1 2 2 ...
# $ WD    : chr [1:176] "WD1" "WD1" "WD1" "WD1" ...
# $ Weight: int [1:176] 240 225 245 260 255 260 275 245 410 405 ...
# $ Time  : int [1:176] 1 1 1 1 1 1 1 1 1 1 ...

colnames(RATSL)
#"ID"     "Group"  "WD"     "Weight" "Time"
length(unique(RATSL$ID))
# in the RATSL data, all the rats have unique ID in the ID column. 
head(RATSL)
tail(RATSL)
table(RATS$Group)
#originally there were 8 rats in group 1 and 4 rats in 2 as well as in group 3. 
table(RATSL$Group)
# now we can observe that there are 88 rows for group 1 rats and 44 
# rows for groups 2 and three, meaning that 10 weight measurements of rats are 
# now seen one in each row instead of 10 columns and one row for each rat. 
#About the other columns of RATSL: Time includes the number of day (from the beginning)
# when the weight seen in the Weight column was measured. WD contains old 
# columns names for the measurement days. ID 

#Tables and explanations summarise contents of the ID, Group and Week. 
#Summary for Weight, a continous variable in the RATSL, looks like this:
summary(RATSL$Weight, exclude=NULL)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#225.0   267.0   344.5   384.5   511.2   628.0
# no unavailable values, the total Weight range of all measurements from 
#different weeks and including different and same rats varies 
#from 225 grams to 628 grams and average weight 
# is 384.5 grams. 

# saving the long format datasets:
#(I'm already in the data folder)
write_csv(BPRSL, "BPRSL.csv")
write_csv(RATSL, "RATSL.csv")
