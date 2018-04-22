# Lab 4: Electric Vechicle Web Scraping
# Author: Seamus Ward
# Purpose: To obtain EV model information and peoples view on EVs through web scraping 

install.packages("rvest")
library(rvest)


url <- 'https://ev-database.uk/'

web_page <- read_html(url)
head(web_page)

# Electric Vehicle Models
ev_models_html <- html_nodes(web_page, '#evdb > main > div.content.jplist > div.list > div:nth-child(n) > div > div.title-wrap > h2 > a > span.model')
head(ev_models_html, 10)

#convert html data to text
ev_models <- html_text(ev_models_html)
head(ev_models)
length(ev_models)
ev_models
# Check array type
typeof(ev_models)
is.atomic(ev_models)
str(ev_models)

#EV Type
ev_type_html <- html_nodes(web_page, '#evdb > main > div.content.jplist > div.list > div:nth-child(n) > div > div.title-wrap > div')
ev_type <- html_text(ev_type_html)
head(ev_type)
ev_type <- gsub(" -\n.*", "", ev_type)
str(ev_type)
length(ev_type)
ev_type

# EV 0-62 Acceleration
ev_acc_0to62_html <- html_nodes(web_page, '#evdb > main > div.content.jplist > div.list > div:nth-child(n) > div > div.specs > p:nth-child(1) > span.acceleration')
ev_acc_0to62 <- html_text(ev_acc_0to62_html)
head(ev_acc_0to62)
ev_acc_0to62 <- gsub(" sec", "", ev_acc_0to62)
head(ev_acc_0to62)
ev_acc_0to62_num <- as.numeric(ev_acc_0to62)
length(ev_acc_0to62_num)
head(ev_acc_0to62_num)
str(ev_acc_0to62_num)


# EV Top Speed
ev_topspeed_html <- html_nodes(web_page, '#evdb > main > div.content.jplist > div.list > div:nth-child(n) > div > div.specs > p:nth-child(2) > span.topspeed')
ev_topspeed <- html_text(ev_topspeed_html)
head(ev_topspeed)
ev_topspeed <- gsub(" mph", "", ev_topspeed)
head(ev_topspeed)
ev_topspeed_num <- as.numeric(ev_topspeed)
head(ev_topspeed_num)
length(ev_topspeed_num)
ev_topspeed_num
str(ev_topspeed_num)

# EV Range
ev_range_html <- html_nodes(web_page, '#evdb > main > div.content.jplist > div.list > div:nth-child(n) > div > div.specs > p:nth-child(3) > span.erange_real')
ev_range <- html_text(ev_range_html)
head(ev_range)
ev_range <- gsub(" mi", "", ev_range)
head(ev_range)
ev_range_num <- as.numeric(ev_range)
length(ev_range_num)
head(ev_range_num)
str(ev_range_num)

# EV Fuel Range (F)
ev_rangeF_html <- html_nodes(web_page, '#evdb > main > div.content.jplist > div.list > div:nth-child(n) > div > div.specs > p:nth-child(4) > span.icerange_real')
ev_rangeF <- html_text(ev_rangeF_html)
head(ev_rangeF)
ev_rangeF <- gsub(" mi", "", ev_rangeF)
head(ev_rangeF)
ev_range_numF <- as.numeric(ev_rangeF)
length(ev_range_numF)
head(ev_range_numF)
str(ev_range_numF)
ev_range_numF

# EV Cost
ev_cost_html <- html_nodes(web_page, '#evdb > main > div.content.jplist > div.list > div:nth-child(n) > div > div.pricing.align-center > span > span.price')
ev_cost <- html_text(ev_cost_html)
head(ev_cost)
ev_cost <- gsub(" .*", "", ev_cost)
head(ev_cost)
ev_cost <- gsub("£", "", ev_cost)
head(ev_cost)
ev_cost <- gsub(",", "", ev_cost)
head(ev_cost)
ev_cost_num <- as.numeric(ev_cost)
length(ev_cost_num)
head(ev_cost_num)
str(ev_cost_num)
# Convert price to Euro  
ev_cost_num <- ev_cost_num*1.15


##### Merge data into dataframe 
ev_dataframe <- data.frame(ev_models, ev_type, ev_acc_0to62_num, ev_topspeed_num, ev_range_num, ev_cost_num)
head(ev_dataframe)
colnames(ev_dataframe) <- c('EV_Models', 'EV_Type', 'Acc0to62', 'Top_Speed_mph', 'Range_miles', 'Cost_Euros')
ev_dataframe$'Fuel_Range' <- NA

#Get location of hybrid EVs that also use fuel 
Idx <- ev_dataframe$'EV_Type' == 'Plug-in Hybrid' | ev_dataframe$'EV_Type' == 'EV with Extender'
#Assign the fuel ranges to the appropriate EVs
ev_dataframe$'Fuel_Range'[Idx == TRUE] <- ev_range_numF
ev_dataframe
#Swap the last 2 columns around placing cost in the last column
ev_dataframe <- ev_dataframe[, c(1:5, 7, 6)]
write.csv(ev_dataframe, file = "Data/EVModelData.csv", row.names = FALSE)
str(ev_dataframe)


# Attempt to use API to get sentiment analysis on EVs from twitter
# Ref https://www.r-bloggers.com/how-to-use-r-to-scrape-tweets-super-tuesday-2016/


install.packages("twitteR")
install.packages("ROAuth")
install.packages("httr")
install.packages("plyr")

library(twitteR)
library(ROAuth)
library(httr)

# Set API Keys
api_key <- "nmxqHfLygHA6I8VNbqX6qubFV"
api_secret <- "NR6ANzEvgm9rzD63GaOR3etJNZTYC6qrFsHLXrTUODAbWznNa5"
access_token <- "855350187597983745-6cBgEKnr90Ys023Uq6qW1AWeqf0eNoc"
access_token_secret <- "TE4yrDMWxuFFhDWT2zPYTXeLIZcixFpExnxdj1OEslOeh"
setup_twitter_oauth(api_key, api_secret, access_token, access_token_secret)


tweets_ev <- searchTwitter('#electricvehicles', n = 1500)
library(plyr)
feed_ev = laply(tweets_ev, function(t) t$getText())
feed_ev
