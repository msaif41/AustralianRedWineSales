# install.packages('forecast')
# install.packages("zoo")

# Read data
data <- read.csv("Downloads/AustralianWines.csv")
data <- data[-c(181:188),] #delete the 8 rows containing NA
str(data)
head(data)

# Convert data into time series object in R
library(forecast)

# start: the time of the first observation
# frequency: number of times per year
x <- ts(dataSM$NIKE, start=c(2000,Q1),frequency = 12)
x
plot(x)

# Model 1: Linear Trend Model
RedSM.lm <- tslm(x~trend)
summary(RedSM.lm) # Yt = 833.0178 + 8.8034 * t

# Data partition for time series data
# Use the last 24 months data as the validation dataset
nValid <- 24 
nTrain <- length(x)-nValid

train.ts <- window(x,start=c(1980,1),end=c(1980,nTrain))
valid.ts <- window(x,start=c(1980,nTrain+1),end=c(1980,nTrain+nValid))

trainSM.lm <- tslm(train.ts~trend)
summary(trainSM.lm) # trend coefficient is significant
trainSM.lm.pred <- forecast(trainSM.lm,h=nValid,level=0) # level = 0 means no confidence interval

# Evaluate model performance
accuracy(trainSM.lm.pred,valid.ts) # Test set MAPE = 25.26361

# Model 2: Seasonality model
# In R, function tslm() uses ts() which automatically creates the categorical Season column (called season) and converts it into dummy variables.
trainSM.lm.season <- tslm(train.ts ~ season)
summary(trainSM.lm.season) # Yt = 811.3+271.4*F+460.2*M+563.1*A...(baseline is January, which is season1); July has highest red wine sales
trainSM.lm.season.pred <- forecast(trainSM.lm.season, h = nValid, level = 0)
accuracy(trainSM.lm.season.pred,valid.ts) # Test set MAPE = 31.47595

# Model 3: Linear Trend and Seasonality
trainSM.lm.trend.season <- tslm(train.ts ~ trend + season)
summary(trainSM.lm.trend.season) # All months are significant
trainSM.lm.trend.season.pred <- forecast(trainSM.lm.trend.season, h = nValid, level = 0)
accuracy(trainSM.lm.trend.season.pred,valid.ts) # Test set MAPE = 14.25010

# Model 4: Simple exponential smoothing

# alpha = 0.2 to fit simple exponential smoothing.
sesSM <- ses(train.ts, alpha = 0.2, h=24)
autoplot(sesSM)
summary(sesSM)
accuracy(sesSM,valid.ts) # Test set MAPE = 26.20372

# Use ses function to estimate alpha
ses1SM <- ses(train.ts, alpha = NULL, h=24)
autoplot(ses1SM)
summary(ses1SM) # optimal alpha  based on training data = 0.6492
accuracy(ses1SM,valid.ts) # Test set MAPE = 31.78312
