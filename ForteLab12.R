
library(RCurl)
library(dplyr)
library(RJSONIO)
library(sqldf)
library(tidyverse)
library(lubridate)

fileURL <- "https://opendata.maryland.gov/resource/rqid-652u.json"

#fileURL <- "http://opendata.maryland.gov/resource/rqid-652u.json"
#just in case the link above does not work, use the json file provided instead. 

######### 1. investigate the url above.
######### 2. covert the JSON format dataset into R objects

mydata <- fromJSON(fileURL)

# look into the data summary

#summary(mydata)

######### 3. what is the data type of "mydata" after reading the URL using the appropriate function.  

# str(mydata)   # Investigating the structure of mydata reveals that it is a nested list.

######## 4. Print the number of rows below.

numRows <- sapply(mydata, nrow)

# nameList1 <-names(mydata[[1]])

# Step 2: Clean the data

#If you investigate mydata, you will find the length of each element is different. We must make it consistent.
#try map_df function to flatten mydata to a dataframe

mydata2 <- map(mydata, as.list)
mydata2 <- map_df(mydata2, flatten)

#then create a dataframe from it.

df1 <- data.frame(mydata2, stringsAsFactors = FALSE)

#change the name of each variables with meaningful names (column names of the original data)
#extract meaningful names from the original data. You need to create two separate columns to include both longitude and latitude
#The total number of columns would be seven columns
#create day of week variable
#convert characters to the proper data formats (numeric or date format)

df1$date <- as.Date(df1$date)
df1$day_of_week <- wday(df1$date, label = TRUE, abbr = FALSE)
df1$day_of_week <- as.character(df1$day_of_week)
df1$X..computed_region_r4de_cuuv <- as.numeric(df1$X..computed_region_r4de_cuuv)
df1$latitude <- as.numeric(df1$latitude)
df1$longitude <- as.numeric(df1$longitude)
names(df1)[names(df1) == "X..computed_region_r4de_cuuv"] <- "computed_region_r4de_cuuv"
df1 <- df1[!is.na(df1$cc_number), ]

#convert/standardize all the accident_type values from acronym to meaningful terms.
#The Maryland Open Data Portal documentation is not correct. 
#So, you have to print out all the unique values included in the accident_type column and their frequency.    
############## Clean the data 
#4.1 Print out all the values and their frequencies here..

sqldf("SELECT accident_type, COUNT(accident_type) FROM df1 GROUP BY accident_type")

#4.2 Merge the values so that the final values include only three categories: 
#######Property Damage (pd, PD, Property Damage Crash), Personal Injury (PI and Injury Crash), and Fatal Crash (F)

df1$accident_type <- gsub("pd", "Property Damage", df1$accident_type)
df1$accident_type <- gsub("PD", "Property Damage", df1$accident_type)
df1$accident_type <- gsub("Property Damage Crash", "Property Damage", df1$accident_type)

df1$accident_type <- gsub("PI", "Personal Injury", df1$accident_type)
df1$accident_type <- gsub("Injury Crash", "Personal Injury", df1$accident_type)
df1$accident_type <- gsub("IS", "Personal Injury", df1$accident_type)

df1$accident_type <- gsub("F", "Fatal Crash", df1$accident_type)

# Step 3: Understand the data using SQL (via SQLDF)
############## 5. how many accidents happen on Sunday?
# Use sql to count how many accidents on "Sunday"

sun_acc <- sqldf("SELECT COUNT(accident_type) FROM df1 WHERE day_of_week = 'Sunday'")

# Print the result

print(sun_acc)


############## 6. how many accidents had injuries? Read the documentation from the Maryland Open Data portal.
#Use sql to count how many observations meet the criterion that accident type is Injury Crash
inj_acc <- sqldf("SELECT COUNT(accident_type) FROM df1 WHERE accident_type == 'Personal Injury'")
# Print the result
print(inj_acc)

# list the injuries by day
# count the number of injuries for each day of the week

list_inj <- sqldf("SELECT day_of_week, COUNT(accident_type) FROM df1 WHERE accident_type == 'Personal Injury' GROUP BY day_of_week")

# Print the result
print(list_inj)

# Step 4: Understand the data using tapply
###########7.how many accidents happen on SUNDAY?
# tapply(Summary Variable, Group Variable, Function):
# apply the length function on the "Sunday" subset of the column day_of_week

tapply(df1$day_of_week, as.vector(df1$day_of_week == "Sunday"), length)

# how many accidents had injuries
# apply the length function

tapply(df1$accident_type, as.vector(df1$accident_type == "Personal Injury"), length)

# list the injuries by day
# apply the length function on subset of the column accident_type broken down by the value in Wday_of_week 
# and accident_type == "Injury Crash"

tapply(df1$accident_type == "Personal Injury", df1$day_of_week, length)
tapply(df1$accident_type, df1$day_of_week, length)

###########8: What is the percentage of injury for all accidents?

percentInjury <- inj_acc / length(df1$accident_type) * 100
percentInjury

########## 9. Which day of a week do you observe the most injury?

list_inj[which.max(list_inj$`COUNT(accident_type)`), ]