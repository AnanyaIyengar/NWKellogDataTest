################################
###TASK 2: MEDICARE ADVANTAGE###
################################

#Setting Working Directory

setwd("C:/Users/anniy/OneDrive/Desktop/NWKellog Assignment/NWKelloDataTest")

########################################################

#Installing Required Packages

library(readr) #to import .csv file
library(dplyr) #for data frame manipulation
library(rmarkdown) #to compile code document

#########################################################

#Importing Data

x <- paste("https://raw.github.com/AnanyaIyengar/NWKellogDataTest/main/scp-1205.csv")
raw_data <- read_csv(x, col_names = FALSE)
raw_data <- as.data.frame(raw_data)

##########################################################

#Renaming and Describing Variables in the Data Set

colnames(raw_data) <- c("countyname", "state", "contract", "healthplanname", "typeofplan", "countyssa", "eligibles", "enrollees", "penetration", "ABrate")

#Variable Descriptors

# - countyname: name of the county
# - state: postal code of the state in which the county lies
# - contract: contract code unique to a health plan
# - healthplannname: name of the health plan
# - typeofplan: type of the health plan
# - countyssa: social security administration code of the county
# - eligibles number of individuals in the county eligible for medicare
# - enrollees: number of individuals in the county who enrolled for medicare
# - penetration: (enrollees/eligibles)*100
# - ABrate: medicare's monthly payments to the health plan 

###########################################################

#Finding and Replacing NA Observations in the Data for the eligibles, enrollees and penetration Variables

#Finding the Number of NA observations in these variables 

print(sum(is.na(raw_data$eligibles))) #There are 81 NA observations for the eligibles variable
print(sum(is.na(raw_data$enrollees))) #There are 30908 NA observations for the enrollees variable
print(sum(is.na(raw_data$penetration))) #There are 30974 NA observations for the penetration variable

#Replacing the NA observations by 0

raw_data$eligibles[is.na(raw_data$eligibles)] <- 0
raw_data$enrollees[is.na(raw_data$enrollees)] <- 0
raw_data$penetration[is.na(raw_data$penetration)] <- 0

#NA observations in the state variable

print(sum(is.na(raw_data$state))) #There are 56 NA observations in the state variable

raw_data[which(is.na(raw_data$state)), ] #Extracts all NA observations in the state variable 

#All these variables have characteristic "Rollup by STATE of Under-11". Due to lack of context about the medicare scheme, I decide to delete these variables from the data frame

raw_data <- raw_data[complete.cases(raw_data), ]

#The data set now has 37957 complete cases

#Finding total unique counties in the data

length(unique(raw_data$countyssa)) #There are 3120 unique county names 

###########################################################

#Variables Required in the New Data Set

# - countyname: name of the county
# - state : state postal code
# - numberofplans1 : number of plans in the county with more than 10 enrollees
# - numberofplans2 : number of plans in the county with penetration greater than 0.5
# - countyssa : social security administration countu code
# - eligibles : eligible individuals in the county
# - totalenrollees : number of individuals in the county with a medicare plan
# - totalpenetration: (totalenrollees/eligibles)*100

############################################################

#Creating new variables as described above

#Grouping data as per countyssa and creating numberofplans1, numberofplans2 and totalenrollees variables

raw_data <- raw_data%>%group_by(countyssa)%>%mutate(numberofplans1 = sum(enrollees > 10), numberofplans2 = sum(penetration > 0.5), totalenrollees = sum(enrollees))

#Computing the totalpenetration variable

raw_data <- raw_data%>%group_by(countyssa)%>%mutate(totalpenetration = (totalenrollees/eligibles)*100)

#Considering unique cases and selecting required variables in the new data frame 

new_data <- raw_data%>%dplyr::select(countyname, state, numberofplans1, numberofplans2, countyssa, eligibles, totalenrollees, totalpenetration)%>%unique()

#Converting NaN observations in the totalpenetration variable to 0 in the new data set

print(sum(is.nan(new_data$totalpenetration))) #There are 4 NaN observations in the totalpenetration variable
new_data$totalpenetration[is.nan(new_data$totalpenetration)] <- 0

###############################################################

#Sorting the created data frame first by state and then by county in alphabetical order

new_data <- new_data%>%dplyr::arrange(state, countyssa)

###############################################################

#Downloading the newly created data frame as a .csv file 

write.csv(new_data, "C:\\Users\\anniy\\OneDrive\\Desktop\\NWKellog Assignment\\NWKelloDataTest\\newdata.csv", row.names = FALSE)






