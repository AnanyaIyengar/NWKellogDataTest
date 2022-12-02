###################################
###TASK 1: RED SOX TICKET ORDERS###
###################################

#Setting Working Directory 

setwd("C:/Users/anniy/OneDrive/Desktop/NWKellog Assignment/NWKelloDataTest")

##############################################################

#Loading Required Packages

library(readr) #to import .csv files
library(dplyr) #for data manipulation
library(ggplot2) #for data visualisation
library(fastDummies) #to create dummy variables
library(lmtest) #for coeftest()
library(AER) #for durbinWatsonTest
library(stargazer) #for tables
library(psych) #for summary statistics
library(sandwich) #for robust standard errors
library(rmarkdown) #to compile code and results

###############################################################

#Importing Data from GitHub

x1 <- paste("https://raw.github.com/AnanyaIyengar/NWKellogDataTest/main/red_sox_2009.csv")
x2 <- paste("https://raw.github.com/AnanyaIyengar/NWKellogDataTest/main/red_sox_2010.csv")
x3 <- paste("https://raw.github.com/AnanyaIyengar/NWKellogDataTest/main/red_sox_2011.csv")
x4 <- paste("https://raw.github.com/AnanyaIyengar/NWKellogDataTest/main/red_sox_2012.csv")

redsox9 <- read_csv(x1)
redsox10 <- read_csv(x2)
redsox11 <- read_csv(x3)
redsox12 <- read_csv(x4)

################################################################

#Variable Descriptions 

# - transaction_date: date of transaction of booking tickets
# - sectiontype: section of stadium chosen, prices will vary according to section
# - price_per_ticket: per unit price of ticket, varies as per game characteristics, seat characteristics and date of transaction
# - number_of_tickets: number of tickets purchased in a given transaction
# - gamemonth: month in which the game is to be played
# - team: opponent team playing
# - day_game: dummy variable for whether or not the game is a day game 
# - weekend_game: dummy variable for whether or not the game is a weekend game 
# - gamedate: date of the game
# - logprice: natural log of the price_per_ticket variable
# - days_from_transaction_until_game: number of days between transaction_date and gamedate variables

#################################################################

#Creating a combined data set for all years

#Creating a year variable for each data set

redsox9$year <- rep(2009, 105673)
redsox10$year <- rep(2010, 118895)
redsox11$year <- rep(2011, 152525)
redsox12$year <- rep(2012, 76078)

#Combining Data Sets

redsoxcombined <- dplyr::bind_rows(redsox9, redsox10, redsox11, redsox12)

##################################################################

#Exploratory Data Analysis 

#Preliminary summary of data by year

describeBy(redsoxcombined, group = "year")


#Looking for NA observations

print(sum(complete.cases(redsoxcombined))) #There are 452936 complete cases i.e. 453171-452936 = 235 NA observations. 

print(sum(is.na(redsoxcombined$day_game))) #We find that all the NA observations are in the day_game and weekend_game dummy variables

na_data <- redsoxcombined[which(is.na(redsoxcombined$day_game)), ] #Extracting NA observations 

#day_game and weekend_game are important predictors of price. people are freer to watch games over a weekend and might be willing to pay higher!
#We can, using a calendar, figure out whether the games in the na_data table were weekend games or not, but we cannot find out if the games played were day games or not.
#Therefore, we remove these 235 observations from the data set. We have enough remaining observations s.t. this trimming does not affect power

redsoxcombined <- redsoxcombined[complete.cases(redsoxcombined), ]

###################################################################

#Visualising Data 

#Plotting logprice and price_per_ticket against days_from_transaction_until_game using different functional forms

#Linear
plot1 <- ggplot(data = redsoxcombined, aes(x = days_from_transaction_until_game, y = logprice)) + geom_smooth(formula = y ~ x) + facet_wrap(vars(year)) + xlab("Days between Transaction Date and Game Date") + ylab("Log of Ticket Price")
print(plot1)

plot1a <- ggplot(data = redsoxcombined, aes(x = days_from_transaction_until_game, y = price_per_ticket)) + geom_smooth(formula = y ~ x) + facet_wrap(vars(year)) + xlab("Days between Transaction Date and Game Date") + ylab("Ticket Price")
print(plot1a)

#Quadratic
plot2 <- ggplot(data = redsoxcombined, aes(x = days_from_transaction_until_game, y = logprice)) + geom_smooth(formula = y ~ x + I(x^2))+ facet_wrap(vars(year)) + xlab("Days between Transaction Date and Game Date") + ylab("Log of Ticket Price")
print(plot2) 

plot2a <- ggplot(data = redsoxcombined, aes(x = days_from_transaction_until_game, y = price_per_ticket)) + geom_smooth(formula = y ~ x + I(x^2))+ facet_wrap(vars(year)) + xlab("Days between Transaction Date and Game Date") + ylab("Ticket Price")
print(plot2a)

#Cubic
plot3 <- ggplot(data = redsoxcombined, aes(x = days_from_transaction_until_game, y = logprice)) + geom_smooth(formula = y ~ x + I(x^2) + I(x^3))+ facet_wrap(vars(year)) + xlab("Days between Transaction Date and Game Date") + ylab("Log of Ticket Price")
print(plot3)

plot3a <- ggplot(data = redsoxcombined, aes(x = days_from_transaction_until_game, y = price_per_ticket)) + geom_smooth(formula = y ~ x + I(x^2) + I(x^3))+ facet_wrap(vars(year)) + xlab("Days between Transaction Date and Game Date") + ylab("Ticket Price")
print(plot3a)

#Plotting to see how ticket price varies with game characteristics: Whether the game is a day game

plot4 <- ggplot(data = redsoxcombined, aes(x = as.factor(day_game), y = logprice)) + geom_boxplot() + facet_wrap(vars(year)) + xlab("Whether Day Game or Not") + ylab("Log of Ticket Price")
print(plot4)

#Plotting to see how ticket price varies with game characteristics: Whether the game was played on a weekend

plot5 <- ggplot(data = redsoxcombined, aes(x = as.factor(weekend_game), y = logprice)) + geom_boxplot() + facet_wrap(vars(year)) + xlab("Whether Weekend Game or Not") + ylab("Log of Ticket Price")
print(plot5)

#Plotting to see how ticket price varies with game characteristics: Opposition Team

plot6a <- ggplot(data = redsox9, aes(x = as.factor(team), y = logprice)) + geom_boxplot() + xlab("Opposition Team") + ylab("Log of Ticket Price 2009")
print(plot6a)

plot6b <- ggplot(data = redsox10, aes(x = as.factor(team), y = logprice)) + geom_boxplot() + xlab("Opposition Team") + ylab("Log of Ticket Price 2010")
print(plot6b)

plot6c <- ggplot(data = redsox11, aes(x = as.factor(team), y = logprice)) + geom_boxplot() + xlab("Opposition Team") + ylab("Log of Ticket Price 2011")
print(plot6c)

plot6d <- ggplot(data = redsox12, aes(x = as.factor(team), y = logprice)) + geom_boxplot() + xlab("Opposition Team") + ylab("Log of Ticket Price 2012")
print(plot6d)

#Plotting to see how ticket price varies with game characteristics: Game Month 

plot7 <- ggplot(data = redsoxcombined, aes(x = as.factor(gamemonth), y = logprice)) + geom_boxplot() + facet_wrap(vars(year)) + xlab("Game Month") + ylab("Log of Ticket Price")
print(plot7)

#Plotting to see how ticket price varies with purchase characteristics: Stadium Section Chosen

plot8a <- ggplot(data = redsox9, aes(x = as.factor(sectiontype), y = logprice)) + geom_boxplot()  + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
print(plot8a)

plot8b <- ggplot(data = redsox10, aes(x = as.factor(sectiontype), y = logprice)) + geom_boxplot() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
print(plot8b)

plot8c <- ggplot(data = redsox11, aes(x = as.factor(sectiontype), y = logprice)) + geom_boxplot() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
print(plot8c)

plot8d <- ggplot(data = redsox12, aes(x = as.factor(sectiontype), y = logprice)) + geom_boxplot() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
print(plot8d)

#Are there price benefits by buying more tickets? Plotting to see how ticket price varies with the number of tickets bought

plot9 <- ggplot(data = redsoxcombined, aes(x = number_of_tickets, y = logprice)) + geom_smooth(formula = y ~ x + I(x^2) + I(x^3), method = "lm") + facet_wrap(vars(year))
print(plot9)

###################################################################

#Estimation

#We have a repeated cross section data frame with different number of observations for each year.

#We first create time dummies for each of the four years. 

redsoxcombined <- dummy_cols(redsoxcombined, select_columns = "year")

#First, consider a pooled regression with time dummies with intercept. To avoid a dummy variable trap, let 2009 be the reference time period.

#Recreating the days_from_transaction_until_game_variable for ease of typing

redsoxcombined$days <- redsoxcombined$days_from_transaction_until_game

#Creating the squared days variable

redsoxcombined$sqdays <- (redsoxcombined$days)^2

#Pooled Baseline Regression with no covariates with logprice as the dependent variable
pooled_baseline <- lm(data = redsoxcombined, logprice ~ days + sqdays + year_2010 + year_2011 + year_2012 + I(days*year_2010) + I(days*year_2011) + I(days*year_2012))
summary(pooled_baseline)

#We know that prices vary with other game and transaction characteristics as well, so the estimates above may be afflicted by omitted variable bias!

#Pooled alternate specification with covariates, logprice is the dependent variable
pooled2 <- lm(data = redsoxcombined, logprice ~ days + sqdays + year_2010 + year_2011 + year_2012 + I(days*year_2010) + I(days*year_2011) + I(days*year_2012) + I(sqdays*year_2010) + I(sqdays*year_2011) + I(sqdays*year_2012) + day_game + weekend_game + factor(sectiontype) + factor(gamemonth) + factor(team))
summary(pooled2)

#Now, we saw from our exploratory data analysis that the number of tickets also is correlated with unit price, however controlling for it would be equivalent to controlling for a collider.
#Therefore, we introduce a new explanatory variable i.e. log(total expenditure on tickets), where total expenditure is the product of number of tickets and per unit price

redsoxcombined <- redsoxcombined%>%dplyr::mutate(totexp = price_per_ticket*number_of_tickets)

#We run the same specifications now on our new outcome variable

pooled3 <- lm(data = redsoxcombined, log(totexp) ~ days + sqdays + year_2010 + year_2011 + year_2012 + I(days*year_2010) + I(days*year_2011) + I(days*year_2012))
summary(pooled3)

pooled4 <- lm(data = redsoxcombined, log(totexp) ~ days + sqdays + year_2010 + year_2011 + year_2012 + I(days*year_2010) + I(days*year_2011) + I(days*year_2012) + I(sqdays*year_2010) + I(sqdays*year_2011) + I(sqdays*year_2012) + day_game + weekend_game + factor(sectiontype) + factor(gamemonth) + factor(team))
summary(pooled4)

######################################################################

#Regression Diagnostics

#Heteroskedasticity Check

bptest(pooled_baseline) #decision: reject H0
bptest(pooled2) #decision: reject H0
bptest(pooled3) #decision: reject H0
bptest(pooled4) #decision: reject H0

#Autocorrelation Check

dwtest(pooled_baseline) #decision: reject H0
dwtest(pooled2) #decision: reject H0
dwtest(pooled3) #decision: reject H0
dwtest(pooled4) #decision: reject H0

#Newey-West HAC Standard Errors used to improve efficiency 

robust_baseline <- coeftest(pooled_baseline, vcov = NeweyWest(pooled_baseline))
robust_baseline

robust_pooled2 <- coeftest(pooled2, vcov = NeweyWest(pooled2))
robust_pooled2

robust_pooled3 <- coeftest(pooled3, vcov = NeweyWest(pooled3))
robust_pooled3

robust_pooled4 <- coeftest(pooled4, vcov = NeweyWest(pooled4))
robust_pooled4

######################################################################

#Creating LATEX Tables for Regression Output

#For logprice as the dependent variable

stargazer(pooled_baseline, pooled2, se = list(robust_baseline[, "Std. Error"], robust_pooled2[, "Std. Error"]))

#For log(total expenditure) as the dependent variable

stargazer(pooled3, pooled4, se = list(robust_pooled3[, "Std. Error"], robust_pooled4[, "Std. Error"]))
