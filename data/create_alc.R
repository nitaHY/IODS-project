#Nita Mettinen, 19.11.2023
#IODS-course week 3, Data Wrangling: Preparation of alc-data to logistic regression analyses. 
#Data retrieved 19.11.2023 from: http://www.archive.ics.uci.edu/dataset/320/student+performance

#Install/activate the needed libraries
library(dplyr)
#set the working directory to data folder:
#(I'm in the IODS-project folder at this point)
setwd("data")


#3. 
#Read data:
math = read.csv("student-mat.csv", sep = ";")
por = read.csv("student-por.csv", sep = ";")

# Explore the structure of the new dataset
str(math)
str(por)
#contents of the dataframes looks quite similar; 
#there are numeric as well as character class variables, and some of them seem categorical. 

setdiff(colnames(math), colnames(por))
#it seems that the colnames are identical in both dataframes

dim(math)
#395 rows and  33 columns
dim(por)
#649 rows  33 columns

#4.Combine datasets: keep only students present in both sets and join by columns 
#other than "failures", "paid", "absences", "G1", "G2", "G3" 
#as this seems to be similar to the exercise3, I will use the code from there but little bit modified
variable_colums = c("failures", "paid", "absences", "G1", "G2", "G3")

# as I checked earlier, there are same columns in both datasets
# I will thus choose por-data to identify the "student identification" columns
join_cols <- setdiff(colnames(por), variable_colums)

# join the two data by the identifier columns that should be similar in 
#same students in both data sets
math_por <- inner_join(math, por, by = join_cols)
str(math_por)
#as expected, there are two versions (.x and .y) of the "variable_columns" in the math_por data
dim(math_por)
#370 rows and 39 columns: there are most of the people that were present in the 
#math data and 370 of those who were found from por data (same person in math and por)

#here I will add the data specific identifier to the duplicate columns:
math_por = inner_join(math, por, by = join_cols, suffix = c(".math", ".por"))

#5. Getting rid off the duplicate records by copying solution from the Exercise3:
# First I create a new dataframe called alc, which includes the identifier
# columns that were used to combine the same students to the math_por data.
# Then I will add the variable columns in the for loop using the same logic as 
# in the Exercise3: if the two duplicate columns contain numeric data, I will take
# mean of them to the alc-data, but if the data is not numeric, I will choose 
# the answer from the first columns, which comes from mat in this case. 

alc <- select(math_por, all_of(join_cols))

for(col_name in variable_colums) {
  # select two columns from 'math_por' with the same original name
  two_cols <- select(math_por, starts_with(col_name))
  # select the first column vector of those two columns
  first_col <- select(two_cols, 1)[[1]]
  
  # then, enter the if-else structure!
  # if that first column vector is numeric...
  if(is.numeric(first_col)) {
    # take a rounded average of each row of the two columns and
    # add the resulting vector to the alc data frame
    alc[col_name] <- round(rowMeans(two_cols))
  } else { # else (if the first column vector was not numeric)...
    # add the first column vector to the alc data frame
    alc[col_name] <- first_col
  }
}
head(alc)
#looks good

#6. new columns: alc_use, which is the average of weekday and weekend use of alcohol, and 
# indicator column of high_use, which is TRUE, if the mean use is more than 2
alc <- mutate(alc, alc_use = (Dalc + Walc) / 2)
alc <- mutate(alc, high_use = alc_use > 2)

#7. glimpse of the data and saving it to data folder if everything is allright
glimpse(alc)
#all the columns filled with information, no duplicated columns, 
#number of rows (=observations) is 370 as expected
#saving the data:
getwd()
#I'm still in the data folder
write.table(alc, "alc.csv", sep = ";")
#DONE