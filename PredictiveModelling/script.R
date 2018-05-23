###########################################################################
# CA 3 - Predictive Modelling 
# Auhor: Seamus Ward
# Purpose : To create models to analyse the effect of Electric Vehichles
#           on the energy grid
###########################################################################

#### [a] Greenhose gas emissions
greenhouse_emissions <- read.csv(header = TRUE, "Data/GHGSector.csv", stringsAsFactors = FALSE)
head(greenhouse_emissions, 4)
greenhouse_emissions[, 1]
indx <- which(greenhouse_emissions == "Road transportation", arr.ind = TRUE);
rowidx = indx[ , 1]
rowidx
roadtransGHG <- greenhouse_emissions[rowidx, 2:ncol(greenhouse_emissions)]
roadtransGHG
years <- 1990:2016
colnames(roadtransGHG) <- years
roadtransGHG_ts <- ts(roadtransGHG)
#Transpose matrix 
roadtransGHG_ts <- t(roadtransGHG_ts)
plot(roadtransGHG_ts, xaxt = "n", xlab = "Years", ylab ="kilotonnes Co2" )
axis(1, at = 1:nrow(roadtransGHG_ts), labels = rownames(roadtransGHG_ts))
title(main = "Road Transport Greenhouse Gas Emissions", col.main = "blue", font.main = 4)

## Install e1071: contains misc. statistical and probability functions
install.packages("e1071")
library(e1071)
## Install Datacombine : tools for combining and cleaning data sets
install.packages("DataCombine")
library(DataCombine)

# Install Forecast library
install.packages("forecast")
library(forecast)

library(tseries)

### Time Series has no seasonal periods due to few samples and generally increasing
### trend therefore time series won't decompose 
decomposed_result <- decompose(roadtransGHG_ts, type = "additive")
decomposed_result <- decompose(roadtransGHG_ts, type = "mult")

# p-value < 0.05 indicates the ts is stationary
adf.test(roadtransGHG_ts)
#compute KPSS test  
kpss.test(roadtransGHG_ts)
plot(roadtransGHG_ts)

# Non seasonal data
nsdiffs(roadtransGHG_ts)

#Get Autocorrelations
acf_res <- acf(roadtransGHG_ts)
#partial autocorrelation
pacf_res <- pacf(roadtransGHG_ts)

# Get first difference
ghg_diff <- diff(roadtransGHG_ts, lag = frequency(roadtransGHG_ts), differences = 1)
plot(ghg_diff, type = "o", col = "blue")
#Get Autocorrelations
acf_res <- acf(ghg_diff)
#partial autocorrelation
pacf_res <- pacf(ghg_diff)

# p-value < 0.05 indicates the ts is stationary
adf.test(ghg_diff)
#compute KPSS test  
kpss.test(ghg_diff)


ghg_diff2 <- diff(ghg_diff, lag = frequency(ghg_diff), differences = 1)
plot(ghg_diff2, type = "o", col = "blue")
acf_res <- acf(ghg_diff2)

# p-value < 0.05 indicates the ts is stationary
adf.test(ghg_diff2)
#compute KPSS test  
kpss.test(ghg_diff2)

fit <- auto.arima(roadtransGHG_ts)
fit
#Accuracy measure
accuracy(fit)

qqnorm(fit$residuals)
qqline(fit$residuals)
Box.test(fit$residuals, type = "Ljung-Box")
# Forecast the result
forecast(fit, 5)
plot(forecast(fit, 5), main = "",  type = "o", xlab = "Years from 1990", ylab = "kilotonnes Co2")
title(main = "Road Transport GHG emissions Arima(1,1,0) 5 year Forecast", col.main = "blue", font.main = 4)


#### [b] Transport Energy: Petroleum Consumption data  ######


fuel_consumption <- read.csv(header = TRUE, "Data/SEI06.csv", stringsAsFactors = FALSE)
head(fuel_consumption,2)

years <- fuel_consumption[2,]
head(years)
years[ncol(years)]
years <- subset(years, select = X.1:X.27)
transport_petroleum <- fuel_consumption[679,]
transport_petroleum <- subset(transport_petroleum, select = X.1:X.27)
colnames(transport_petroleum) <- years
transport_petroleum
transport_petroleum_ts <- ts(transport_petroleum)
#Transpose matrix 
transport_petroleum_ts <- t(transport_petroleum_ts)
plot(transport_petroleum_ts, xaxt = "n", xlab = "Years", ylab = "Petroleum Consumption (ktoe)")
axis(1, at= 1:27, labels = rownames(transport_petroleum_ts))
title(main = "Private Car Petroleum Consumption", col.main = "blue", font.main = 4)

### trend therefore time series won't decompose 
decomposed_result <- decompose(transport_petroleum_ts, type = "additive")

# p-value < 0.05 indicates the ts is stationary
adf.test(transport_petroleum_ts)
#compute KPSS test  
kpss.test(transport_petroleum_ts)
plot(transport_petroleum_ts)

# Non seasonal data
nsdiffs(transport_petroleum_ts)

#Get Autocorrelations
acf_res <- acf(transport_petroleum_ts)
#partial autocorrelation
pacf_res <- pacf(transport_petroleum_ts)

# Get first difference
petrol_diff <- diff(transport_petroleum_ts, lag = frequency(transport_petroleum_ts), differences = 1)
plot(petrol_diff, type = "o", col = "blue")
#Get Autocorrelations
acf_res <- acf(petrol_diff)
#partial autocorrelation
pacf_res <- pacf(petrol_diff)

# p-value < 0.05 indicates the ts is stationary
adf.test(petrol_diff)
#compute KPSS test  
kpss.test(petrol_diff)


petrol_diff2 <- diff(petrol_diff, lag = frequency(petrol_diff), differences = 1)
plot(petrol_diff2, type = "o", col = "blue")
acf_res <- acf(petrol_diff2)

# p-value < 0.05 indicates the ts is stationary
adf.test(petrol_diff2)
#compute KPSS test  
kpss.test(petrol_diff2)

fit <- auto.arima(transport_petroleum_ts)
fit
#Accuracy measure
accuracy(fit)

qqnorm(fit$residuals)
qqline(fit$residuals)
Box.test(fit$residuals, type = "Ljung-Box")
acf(fit$residuals)
# Forecast the result
forecast(fit, 5)
plot(forecast(fit, 5),  main = "", type = "o", xlab = "Years from 1990", ylab = "Petroleum consumption ktoe")
title(main = "Transport energy consumption petroleum Arima(0,2,0) 5 year Forecast", col.main = "blue", font.main = 4)


#### [c] Transport Energy: Renewable Energy Consumption data  ######
transport_renewable <- fuel_consumption[694,]
transport_renewable <- subset(transport_renewable, select = X.1:X.27)
colnames(transport_renewable) <- years
transport_renewable
transport_renewable_ts <- ts(transport_renewable[17:27])
#Transpose matrix 
transport_renewable_ts <- t(transport_renewable_ts)
head(transport_renewable_ts)
plot(transport_renewable_ts, xaxt = "n", xlab = "Years", ylab = "Renewable Energy Consumption (ktoe)")
axis(1, at = 1:11, labels = rownames(transport_renewable_ts))
title(main = "Private Car Renewable Energy Consumption", col.main = "blue", font.main = 4)
transport_renewable_ts


#plot(transport_renewable_ts, type = "b", xaxt = "n", xlab = "Years", ylab = "Renewable Energy Consumption (ktoe)")
#axis(1, at = 17:27, labels = rownames(transport_renewable_ts[17:27]))
#str(transport_renewable_ts)
#transport_renewable_ts <- ts(transport_renewable_ts)
#rownames(transport_renewable_ts)
time <- 1:11
str(time)
str(transport_renewable_ts)
fitreg <- lm(transport_renewable_ts ~ time)
transport_renewable_ts
#f <- forecast(fitreg, h = 5, level = c(80, 95))
plot((transport_renewable_ts), xlab = "Years", ylab = "Renewable Energy Consumption (ktoe)")
plot(fitreg)

summary(fitreg)
confint(fitreg)
acf(resid(fitreg))
pacf(resid(fitreg))
plot(forecast(fitreg, 12:20), xlab = "Years from 2006", ylab = "Renewable Energy Consumption (ktoe)")



 
## [d] Number of private electric cars on the road
private_cars <- read.csv(header = TRUE, "Data/TEA27.csv", stringsAsFactors = FALSE)
private_cars
years <- 2010:2017
years
str(private_cars)
indx <- which(private_cars == "Electric", arr.ind = TRUE);
cols <- indx[,2]
cols
elec_private_cars <- private_cars[4, cols]
colnames(elec_private_cars) <- years
years <- fuel_consumption[2,]
elec_private_cars_ts <- ts(elec_private_cars)
#Transpose matrix 
elec_private_cars_ts <- t(elec_private_cars_ts)
plot(elec_private_cars_ts, xaxt = "n", type = "b", xlab = "Years", ylab = "Number of Electric Vehicles")
axis(1, at = 1:8, labels = rownames(elec_private_cars_ts))
title(main = "Number of Private Electric Cars",  col.main = "blue", font.main = 4)

plot(elec_private_cars_ts)
time <- 1:8
str(elec_private_cars_ts)
str(time)
fitreg <- lm(elec_private_cars_ts ~ time)
str(elec_private_cars_ts)

summary(fitreg)
confint(fitreg)
acf(resid(fitreg))
pacf(resid(fitreg))
plot(forecast(fitreg, 10:20))

## Exponential growth model for electric cars
expmodel <- lm(log(elec_private_cars_ts) ~ time)
summary(expmodel)
timevalues <- seq(0, 30, 1)
expmodel2 <- exp(predict(expmodel, list(time=timevalues)))
plot(time, elec_private_cars_ts, pch = 16, xlim = range(0:30), ylim = range(0:40000), xlab = "Years from 2008", ylab = "No. of EVs")
title(main = "Number of Private Electric Cars", col.main = "blue", font.main = 4)
lines(timevalues, expmodel2, lwd = 2, col = "red")
points(timevalues, expmodel2, pch = 16, lwd = 2, col = "blue")
timevalues
points(10, 20000, pch = 4, lwd = 4, col = "blue")


install.packages("gtools")
library(gtools)
install.packages("corrplot")
library(corrplot)

#### Examine relationship between number of Electric private cars and the renewable 
#### energy consumed by private car transport


transport_renewable_ts[5:11]
transport_renewable_ts
elec_private_cars_ts[1:7]
par(mfrow = c(2, 1))
plot(2010:2016, transport_renewable_ts[5:11], type = "b")
plot(2010:2016, elec_private_cars_ts[1:7], type = "b", col="blue")
# Running correlation over 3 samples
running(transport_renewable_ts[5:11], elec_private_cars_ts[1:7], fun = cor, width = 3, by = 1, allow.fewer = TRUE, align = c("right"), simplify = TRUE)
# Cross correltation res <- cor(data_sel)
cor(transport_renewable_ts[5:11], elec_private_cars_ts[1:7])

### Linear regression between number of cars and renewable energy consumption
reg1 <- lm(transport_renewable_ts[5:11] ~ elec_private_cars_ts[1:7])
summary(reg1)
confint(reg1)
acf(resid(reg1))
pacf(resid(reg1))
plot(elec_private_cars_ts[1:7], transport_renewable_ts[5:11])
abline(reg1)
AIC(reg1)
BIC(reg1)


# Forecast future energy consumption 
par(mfrow = c(1, 1))
time <- 1:11
expmodelRenew <- lm(log(transport_renewable_ts) ~ time)
summary(expmodelRenew)
timevalues <- seq(0, 30, 1)
expmodelRenew2 <- exp(predict(expmodelRenew, list(time = timevalues)))
plot(time, transport_renewable_ts, pch = 16, xlim = range(0:30), ylim = range(0:10000), main = "", type = "o", xlab = "Years from 2006", ylab = "Transport Renewable energy consumption (ktoe)")
lines(timevalues, expmodelRenew2, lwd = 2, col = "red")
points(timevalues, expmodelRenew2, pch = 16, lwd = 2, col = "blue")

# Renewable Eneergy consumption (ktoe) by 2020 
expmodelRenew2[14]
# No. of Electric vehicles by 2020 
expmodel2[12]

plot(time, elec_private_cars_ts, pch = 16, xlim = range(0:30), ylim = range(0:40000))
lines(timevalues, expmodel2, lwd = 2, col = "red", xlab = "Years from 2008", ylab = "No. of EVs")
points(timevalues, expmodel2, pch = 16, lwd = 2, col = "blue")
timevalues
points(10, 20000, pch = 4, lwd = 4, col = "blue")

