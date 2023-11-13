#Nita Mettinen, 11.11.2023
#IODS-course week 2, Data Wrangling: Preparation of data to further analyses. 
#Data retrieved 11.11.2023 from: http://www.helsinki.fi/~kvehkala/JYTmooc/JYTOPKYS3-data.txt
#Metadata retrieved 11.11.2023 from: http://www.helsinki.fi/~kvehkala/JYTmooc/JYTOPKYS2-meta.txt

#Install/activate the needed libraries
library(dplyr)



#2. 
#Read the data:
lrn14 = read.table("http://www.helsinki.fi/~kvehkala/JYTmooc/JYTOPKYS3-data.txt", sep = "\t", na.strings = "NA", header = T)

# See the structure of the new dataset
str(lrn14)
#most of the variables (questions, shown here in the str output as rows but are
#actually columns in the dataframe) seem to numeric data except the gender variable
# which looks more like factor with levels "F" and "M" but is actually here in 
#character form (not in the factor form)
dim(lrn14)
#result: 183  60 meaning that there are 183 rows and 60 columns

#3.
#Creation of the combination variables: gender, age, attitude, deep, stra, surf and points
#as done in the Exercise2 (this the same code copied from the exercise but I have added own comments)

#choose the relevant column names to the creation 
#of combination variables deep, stra and surf 
deep_questions <- c("D03", "D11", "D19", "D27", "D07", "D14", "D22", "D30","D06",  "D15", "D23", "D31")
surface_questions <- c("SU02","SU10","SU18","SU26", "SU05","SU13","SU21","SU29","SU08","SU16","SU24","SU32")
strategic_questions <- c("ST01","ST09","ST17","ST25","ST04","ST12","ST20","ST28")

# actually select those columns needed to create "deep" combination variable 
deep_columns <- select(lrn14, one_of(deep_questions))
#Create the "deep" named combination variable to the data and include only the
#scaled value to the combination variable. The scaled value in this case is
#the mean per row for the selected set of variables
lrn14$deep <- rowMeans(deep_columns)
#repeat the last two steps to create the other two combination variables 
surface_columns <- select(lrn14, one_of(surface_questions))
lrn14$surf <- rowMeans(surface_columns)
strategic_columns <- select(lrn14, one_of(strategic_questions))
lrn14$stra <- rowMeans(strategic_columns)

#Here I choose only the necessary columns to our dataset:
keep_columns <- c("gender","Age","Attitude", "deep", "stra", "surf", "Points")
learning2014 <- select(lrn14, one_of(keep_columns))

#Next I tidy the data a little by removing those "Points" that are 0
learning2014 <- filter(learning2014, Points > 0)

#4.
#Saving the data as csv-format to the data folder 
#and testing to read it from there 

#first I set the working directory location here, 
#just pasted the filepath inside the setwd()-function using
#""-marks and switching this: \ to this: / 
#setwd(".../IODS-project")#I have commented this out as this is not exactly the information used to run this command

#writing the data as csv-file to the data-folder in the IODS-folder
write_csv(learning2014, "data/learning2014.csv", na = "NA", quote = "none")

#trying to read the data from the data-folder:
learning2014_test = read_csv("data/learning2014.csv", col_names = TRUE)

