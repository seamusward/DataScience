# CA 2:  Hybrid Vechicle preferences 
# Dataset ref: http://users.cecs.anu.edu.au/~u4940058/CarPreferences.html
# Author: Seamus Ward
# Purpose: Hypotheses Testing 


# Import and Amalgamate data
user_data <- read.csv(header = TRUE, file = "Data/users1.csv", stringsAsFactors = TRUE)
item_data <- read.csv(header = TRUE, file = "Data/items1.csv", stringsAsFactors = TRUE)
pref_data <- read.csv(header = TRUE, file = "Data/prefs1.csv", stringsAsFactors = TRUE)

# Rename columns to help with merging data
colnames(user_data) <- c('UserID', 'Education', 'Age', 'Gender','Region','Controls')
colnames(item_data) <- c('ItemID', 'BodyType', 'Transmission', 'EngineCap', 'FuelConsumed')
colnames(pref_data) <- c('UserID', 'ItemID', 'ItemID2', 'Control')

# Merge preferrence data with item data 
merge_pref <- merge(pref_data, item_data, by = 'ItemID')

head(pref_data)
head(item_data)
head(merge_pref)
head(user_data)

# Merge User data with preference and item data
my_data <- merge(merge_pref, user_data, by = 'UserID')
head(my_data, 10)

# Determine users preference and gender
# Method 1 use aggreagate and mean values < 1.5 = Hybrid,   values > 1.5 = NonHybrid 
attach(my_data)
aggdata <- aggregate(my_data$FuelConsumed, by = list(UserID), FUN = mean)
head(aggdata, 10)
aggdata
str(aggdata)

# Method 2 
library(plyr)
sumdata <- count(my_data, c('UserID' ,'FuelConsumed'))
head(sumdata, 10)

##############################################################
#   Hybrid or Non Hybrid                                    ##
##############################################################
car_type <- sumdata[order(sumdata$UserID, sumdata$freq),]
Fuel_pref <- car_type[!duplicated(car_type$UserID, fromLast = TRUE),]
head(car_type)
head(Fuel_pref)
nrow(Fuel_pref)
total_hybrid <- sum(Fuel_pref$FuelConsumed == 1)
total_hybrid
total_Non_hybrid <- sum(Fuel_pref$FuelConsumed == 2)
total_Non_hybrid

######################################################################################
## One sample proportion test                                                        #
## Null Hypothesis: There is no preference between hybrid or non-hybrid vehicles     #
## Hpothesis:  There is a preference for either hytbrid or non hybrid vechicles      #
######################################################################################

install.packages("pwr")
library(pwr)
power_changes <- pwr.p.test(h = NULL, n = 60, sig.level = 0.05, p = 0.8)
plot(power_changes)

power_changes2 <- pwr.p.test(h = ES.h(p1 = 0.7, p2 = 0.5), n =60, sig.level = 0.05, alternative = "two.sided")
plot(power_changes2)

effectsize = c(0.2, 0.5, 0.8)
pwr.p.test(h = effectsize, n = 60, sig.level = 0.05, alternative = "two.sided")

#Apply 1 sample proportion test
result.prop <- prop.test(table(Fuel_pref$FuelConsumed), correct = FALSE)
result.prop

######################################################################################
## Two sample proportion test                                                        #
## Null Hypothesis: There is no gender preference towards hybrid vehicles            #
## Hpothesis:  Women prefer hybrids vechicles more than men                          #
######################################################################################

gender_data <- user_data[, c("UserID", "Gender")]
head(gender_data, 10)
sumdata_gender <- merge(sumdata, gender_data, by = 'UserID')
head(sumdata_gender, 10)
# Get Fuel preference by ordering data by freq and selecting fuel of highest frequency
sumdata_gender <- sumdata_gender[order(sumdata_gender$UserID, sumdata_gender$freq),]
Fuel_pref_gender <- sumdata_gender[!duplicated(sumdata_gender$UserID, fromLast = TRUE),]
head(Fuel_pref_gender)
Fuel_pref_gender
women_hybrid <- sum((Fuel_pref_gender$Gender == 2) & (Fuel_pref_gender$FuelConsumed == 1))
women_hybrid
men_hybrid <- sum((Fuel_pref_gender$Gender == 1) & (Fuel_pref_gender$FuelConsumed == 1))
men_hybrid
women_Nonhybrid <- sum((Fuel_pref_gender$Gender == 2) & (Fuel_pref_gender$FuelConsumed == 2))
women_Nonhybrid
men_Nonhybrid <- sum((Fuel_pref_gender$Gender == 1) & (Fuel_pref_gender$FuelConsumed == 2))
men_Nonhybrid
sum((Fuel_pref_gender$Gender == 1))
# Apply power test porportion - 2 sample proportion test
install.packages("pwr")
library(pwr)
power_changes <- pwr.2p2n.test(h = NULL, n1 = 30, n2=29, sig.level = 0.05, p = 0.8, alternative = "greater")
### Copy plots using metafile
plot(power_changes)

power_changes2 <- pwr.2p2n.test(h = ES.h(p1 = 0.8, p2 = 0.5), n1 = 30, n2=NULL, sig.level = 0.05, power = 0.8, alternative = "greater")
plot(power_changes2)

effectsize = c(0.2, 0.5, 0.8)
pwr.2p.test(h = effectsize, n = 30, sig.level = 0.05, alternative = "greater")

#Apply 2 sample proportion test
result.prop2 <- prop.test(c(women_hybrid, men_hybrid), c(30,29), correct = FALSE)
result.prop2
