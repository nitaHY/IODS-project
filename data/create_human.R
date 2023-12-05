#Nita Mettinen, 28.11.2023, continued 1.12.2023
#IODS-course week 4, Data Wrangling: Preparation of data to next week. 
#IODS-course week 5, Data Wrangling: continue working with the human-data
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

#Week 5 Data wrangling starts here#

# 1. Describe the human-data and explore structure and dimensions
human = read_csv("data/human.csv")
dim(human)
# Data contains 195 rows. Each row represents information 
# related to one country seen in the "Country" column. Represented information 
# is divided into 19 columns.
str(human)
# The 19 columns include numerical information about the countries. 
# HDI means Human development index, which consists of variables from 
# three dimensions: long and healthy life, knowledge and a decent standard or 
# living. Healthy life dimension is defined by life expectancy at birth, seen in 
# variable Life.Exp. Expected years of schooling, variable Edu.Exp, and mean 
# years of schooling, variable Edu.Mean, form the index of knowledge 
# dimension in HDI. The third dimension, a decent standard of living, is 
# defined by variable GNI, which refers to GNI per capita. 
# There are also other variables in the data. HDI.rank is order of countries
# according to HDI. Another rank found is GNI.rank_minus_HDI.rank, defining 
# another ordering method to countries. There is also GII.rank, which ranks 
# countries according to variable GII, Gender Inequality Index. GII is also 
# three-dimensional index describing gender-based inequality or loss in human 
# development due to gender inequalities. The index is defined by variables 
# related to reproductive health, empowerment and the labour market and the 
# value 0 of GII refers to genders having quite equal conditions and 1 refers to
# unequal conditions for one of the genders. GII reproductive health-dimension
# related variables include: 
# Mat.Mor = maternal mortality ratio = deaths per 100,000 live births and
# Ado.Birth = Adolescent birth rate = births per 1,000 women ages 15–19. 
# GII empowerment-dimension is defined by variables: Parli.F = Share of seats in 
# parliament (% held by women) and Edu2.F (female) and Edu2.M (male) = 
# Population with at least some secondary education (%). Labour market -dimension
# is described with gender specific variables Labo.F and Labo.M = Labour 
# force participation rate (%). 
# In addition, there are variables Edu2.FM and Labo.FM, which are the ratios of 
# women and men for the similarly named original variables. 
# GII information was found here: https://hdr.undp.org/system/files/documents/technical-notes-calculating-human-development-indices.pdf, 3.12.2023
# and in the original data set (for this course): https://raw.githubusercontent.com/KimmoVehkalahti/Helsinki-Open-Data-Science/master/datasets/gender_inequality.csv
# HDI information was also found in the original data set (for this course): https://raw.githubusercontent.com/KimmoVehkalahti/Helsinki-Open-Data-Science/master/datasets/human_development.csv
# and from: https://hdr.undp.org/data-center/human-development-index#/indicies/HDI, 3.12.2023

# 2. Keep only variables: "Country", "Edu2.FM", "Labo.FM", "Edu.Exp", "Life.Exp", "GNI", "Mat.Mor", "Ado.Birth", "Parli.F"
human = select(human, all_of(c("Country", "Edu2.FM", "Labo.FM", "Edu.Exp", "Life.Exp", "GNI", "Mat.Mor", "Ado.Birth", "Parli.F")))

# 3. Remove rows with missing values
human <- filter(human, complete.cases(human))

# 4. Remove regions, keep countries
human[c(156:162),"Country"]
#1 Arab States                    
#2 East Asia and the Pacific      
#3 Europe and Central Asia        
#4 Latin America and the Caribbean
#5 South Asia                     
#6 Sub-Saharan Africa             
#7 World  
# it seems that indices 156-162 in the table (last seven rows) include 
# regions rather than countries, so lets remove them
last <- nrow(human) - 7
human <- human[1:last, ]#ok!
#

# 5. checking the dimension and overwriting the
# old version of human data in the data folder
dim(human)
#155   9 <- ok
write_csv(human, "data/human.csv")




