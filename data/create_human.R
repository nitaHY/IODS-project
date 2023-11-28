#Nita Mettinen, 28.11.2023
#IODS-course week 4, Data Wrangling: Preparation of data to next week. 

#Install/activate the needed libraries
library(dplyr)
library(readr)


#2. Read in the “Human development” and “Gender inequality” data sets

hd <- read_csv("https://raw.githubusercontent.com/KimmoVehkalahti/Helsinki-Open-Data-Science/master/datasets/human_development.csv")
gii <- read_csv("https://raw.githubusercontent.com/KimmoVehkalahti/Helsinki-Open-Data-Science/master/datasets/gender_inequality.csv", na = "..")
# more information about the data and variables can be found here:
# https://hdr.undp.org/data-center/human-development-index#/indicies/HDI
# and technical documentation from here:
# https://hdr.undp.org/system/files/documents/technical-notes-calculating-human-development-indices.pdf


#3. structure, dimensions and summaries of the datasets
#hd
str(hd)
dim(hd)
#195 rows and 8 columns
summary(hd)

#gii
str(gii)
dim(gii)
#195 rows and 10 columns
summary(gii)
#both data sets include mostly numerical data, except Country,
#which seems to be character varaible

#4. renaming variables based on meta-file found here:
#https://github.com/KimmoVehkalahti/Helsinki-Open-Data-Science/blob/master/datasets/human_meta.txt
#"GNI" = Gross National Income per capita
hd <- hd %>% rename_at("Gross National Income (GNI) per Capita", ~'GNI')
#"Life.Exp" = Life expectancy at birth
hd <- hd %>% rename_at("Life Expectancy at Birth", ~"Life.Exp")
#"Edu.Exp" = Expected years of schooling 
hd <- hd %>% rename_at("Expected Years of Education", ~"Edu.Exp")

#I give shorter and more machine usable names to other variables 
#too in the hd data
hd <- hd %>% rename_at("HDI Rank", ~"HDI.rank")
hd <- hd %>% rename_at("Human Development Index (HDI)", ~"HDI")
hd <- hd %>% rename_at("Mean Years of Education", ~"Edu.Mean")
hd <- hd %>% rename_at("GNI per Capita Rank Minus HDI Rank", ~"GNI.rank_minus_HDI.rank")
colnames(hd)
#[1] "HDI.rank"                "Country"                 "HDI"                    
#[4] "Life.Exp"                "Edu.Exp"                 "Edu.Mean"               
#[7] "GNI"                     "GNI.rank_minus_HDI.rank"
#shorter names changed successfully

#gii data and new column names based on meta file:
#"Mat.Mor" = Maternal mortality ratio
gii <- gii %>% rename_at("Maternal Mortality Ratio", ~"Mat.Mor")
#"Ado.Birth" = Adolescent birth rate
gii <- gii %>% rename_at("Adolescent Birth Rate", ~"Ado.Birth")
# Empowerment
#"Parli.F" = Percetange of female representatives in parliament
gii <- gii %>% rename_at("Percent Representation in Parliament", ~"Parli.F")

#"Edu2.F" = Proportion of females with at least secondary education
gii <- gii %>% rename_at("Population with Secondary Education (Female)", ~"Edu2.F")

#"Edu2.M" = Proportion of males with at least secondary education
gii <- gii %>% rename_at("Population with Secondary Education (Male)", ~"Edu2.M")

#"Labo.F" = Proportion of females in the labour force
gii <- gii %>% rename_at("Labour Force Participation Rate (Female)", ~"Labo.F")

#"Labo.M" " Proportion of males in the labour force
gii <- gii %>% rename_at("Labour Force Participation Rate (Male)", ~"Labo.M")


#other columns named also with shorter names:
gii <- gii %>% rename_at("GII Rank", ~"GII.rank")
gii <- gii %>% rename_at("Gender Inequality Index (GII)", ~"GII")
colnames(gii)
#[1] "GII.rank"  "Country"   "GII"       "Mat.Mor"   "Ado.Birth" "Parli.F"   "Edu2.F"   
#[8] "Edu2.M"    "Labo.F"    "Labo.M"  

#5. mutate gii with two new variables
#ratio of female and male populations with secondary education
gii <- mutate(gii, Edu2.FM = Edu2.F / Edu2.M)
#ratio of labor force participation of females and males
gii <- mutate(gii, Labo.FM = Labo.F / Labo.M)

#6.join datasets using "Country" as identifier, keep only those found 
#in both datasets, and write csv

human = inner_join(hd, gii, by = "Country")
dim(human)
#195  19 which was expected

#write csv:
write_csv(human, "data/human.csv")


