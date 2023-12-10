
################################################################################
################################################################################
#####                                                                      #####
#####                    EMPLOYER PROJECT: THOUGHTWORKS                    #####
#####          CYCLING IN MAJOR CITIES: GROWING CYCLING IN LONDON          #####
#####                    FORECAST STUDY FOR MAIN GOAL 1                    #####
#####                                                                      #####
################################################################################
################################################################################

## BACKGROUND AND CONTEXT 

## Transport for London (TfL) is a local government body responsible for most of 
## the transport network in London.

## TfL was created in 2000 as part of the Greater London Authority (GLA) and is 
## managed by a board chaired by the Mayor of London.

## They are also guided by the Mayor’s Transport Strategy, which aims to 
## transform the city’s streets, improve public transport and create 
## opportunities for new homes and jobs.

## The central aim of the strategy is for 80% of all trips in London to be made 
## on foot, by cycle or using public transport by 2041. 

## DataSurfers has been commissioned to investigate three main goals:
## - Main goal 1: How much progress has been made towards achieving our goal of 
##   80% of all trips in London being made on foot, by cycle or using public 
##   transport by 2041 - and what actions should be taken to increase cycle 
##   transit in particular?
## - Main goal 2: How might we also use data to support our efforts towards 
##   reaching this goal?
## - Main goal 3: TfL has a hypothesis that there is a correlation between the 
##   availability of cycling infrastructure and the prevalence of cycling.

## This R script is the collection of the codes applied to uncover trends and 
## patterns on the data sets provided by the client (along with others that 
## DataSurfers have researched further), in relation to main goal 1. Main goals
## 2 and 3 will be investigated in Python.

################################################################################

# 1. Prepare your workstation for forecast with full years.

# Install necessary packages.
# Install Tidyverse.
install.packages('tidyverse')
# Generic function for forecasting.
install.packages('forecast')
# Time series analysis.
install.packages('tseries')


# Import the necessary libraries.
library(tidyverse)
library(forecast)
library(tseries)


# Set the working directory and import the data set.
# Entire London.
# With COVID years.
london_full <- read.csv('trips_month2.csv', header = T)
# Otherwise we can use:
london_full <- read.csv(file.choose(), header = T)

# Sense-check the new data set.
# The top six rows of the data frame.
head(london_full)

summary(london_full)

# The top 15 rows of the data frame.
head(london_full, 15)

# The bottom six rows of the data frame.
tail(london_full)

################################################################################

# 2. Prepare the data.

# Specify the column, change the variables, and format the date.
# View the options available:
?strptime


# Based on the data, we will use the d/m/Y format.
london_full$Survey.Month <- as.Date(london_full$Survey.Month, format = '%d/%m/%Y')


# View the new data format.
head(london_full$Survey.Month)


# Convert the data into a time series.
# Create a new data frame and assign time series value,
# and specify the 'ts' function.
london_full_ts <- ts(london_full$Total.cycles,
              start = c(2014, 1),
              # Monthly frequency without missing values in data.
              frequency = 12)


# Sense-check the new object.
head(london_full_ts)  

################################################################################

# 3. Visualise the data.

# Plot the time series.
plot(london_full_ts)

# View the data by creating a smaller sample of the visualisation.
plot(window(london_full_ts, 2014, 2015))

################################################################################

# 4. Group the data by month.

# Specify the boxplot function and specific operands. 
boxplot(london_full_ts~cycle(london_full_ts))

# Add labels and titles.
boxplot(london_full_ts~cycle(london_full_ts),
        ylab = "Cycle trips", 
        xlab = "Month",
        main = "Total number of cycle trips by month")

################################################################################

# 5. Decomposing the data.

# Extract and plot the main components to decompose the time series.
london_full_components <- decompose(london_full_ts)

# What is the object?
class(london_full_components)

# Determine the structure.
str(london_full_components)

# Visualise the decomposed time series.
plot(london_full_components) 

# Look at a single year (seasonal pattern) of the data.
plot(window(london_full_components$seasonal,
            c(2019, 1), c(2020, 12)))

# Plot the trend component.
plot(window(london_full_components$trend,
            c(2019, 1), c(2021, 12)))

################################################################################

# 6. Testing stationarity

# Test stationarity with augmented ADF test.
adf.test(london_full_ts)
# As the p-value is smaller than 0.05 we reject H0 and it can be said that the 
# time series is stationary.

################################################################################

# 7. Testing autocorrelation 

# Review random time series variables.
london_full_components$random

# Plot values removing NA values while doing so.
autoplot(acf(na.remove(london_full_components$random), plot=FALSE)) + 
  # Add a title.
  labs(title="Randomness value") + 
  # Set the theme.
  theme_classic() 

# Plot random variables to check the distribution.
hist(london_full_components$random)

################################################################################

# 8. Fit an ARIMA model

# Fit the model to our time series. 
arima_london_full_ts <- auto.arima(london_full_ts)

# Make a forecast for the next three months.
forecast3_london_full_ts <- forecast(arima_london_full_ts, 3)

# Plot the forecast on a graph.
autoplot(forecast3_london_full_ts) + theme_classic()

# Print the values in the data frame.
forecast3_london_full_ts

################################################################################

# 9. Extend the prediction to 2024

# Extend the prediction, set data source, time span, and assign a new object.
forecast2_london_full_ts <- forecast(arima_london_full_ts, 60)

# Plot the output and set the theme. 
autoplot(forecast2_london_full_ts) + theme_classic()

# Print the values in the data frame.
forecast2_london_full_ts

################################################################################

# 10. Extend the prediction to 2041

# Extend the prediction, set data source, time span, and assign a new object.
forecast_london_full_ts <- forecast(arima_london_full_ts, 240)

# Plot the output and set the theme. 
autoplot(forecast_london_full_ts) + theme_classic()

# Print the values in the data frame.
forecast_london_full_ts  

tail(forecast_london_full_ts)

write.csv(forecast_london_full_ts, 'forecast_full.csv')


################################################################################
################################################################################

# 1. Prepare your workstation for forecast without COVID years.

# Import dataset without COVID years.
london_no_covid <- read.csv('trips_month.csv', header = T)
# Otherwise we can use:
london_no_covid <- read.csv(file.choose(), header = T)

# Sense-check the new data set.
# The top six rows of the data frame.
head(london_no_covid)
summary(london_no_covid)

# The top 15 rows of the data frame.
head(london_no_covid, 15)

# The bottom six rows of the data frame.
tail(london_no_covid)

################################################################################

# 2. Prepare the data.

# Based on the data, we will use the d/m/Y format.
london_no_covid$Survey.Month <- as.Date(london_no_covid$Survey.Month, format = '%d/%m/%Y')


# View the new data format.
head(london_no_covid$Survey.Month)


# Convert the data into a time series.
# Create a new data frame and assign time series value,
# and specify the 'ts' function.
london_no_covid_ts <- ts(london_no_covid$Total.cycles,
                     start = c(2014, 1),
                     # Monthly frequency without missing values in data.
                     frequency = 12)


# Sense-check the new object.
head(london_no_covid_ts)  

################################################################################

# 3. Visualise the data.

# Plot the time series.
plot(london_no_covid_ts)

# View the data by creating a smaller sample of the visualisation.
plot(window(london_no_covid_ts, 2014, 2015))

################################################################################

# 4. Group the data by month.

# Specify the boxplot function and specific operands. 
boxplot(london_no_covid_ts~cycle(london_no_covid_ts))

# Add labels and titles.
boxplot(london_no_covid_ts~cycle(london_no_covid_ts),
        ylab = "Cycle trips", 
        xlab = "Month",
        main = "Total number of cycle trips by month")

################################################################################

# 5. Decomposing the data.

# Extract and plot the main components to decompose the time series.
london_no_covid_components <- decompose(london_no_covid_ts)

# What is the object?
class(london_no_covid_components)

# Determine the structure.
str(london_no_covid_components)

# Visualise the decomposed time series.
plot(london_no_covid_components) 

# Look at a single year (seasonal pattern) of the data.
plot(window(london_no_covid_components$seasonal,
            c(2014, 1), c(2015, 12)))

# Plot the trend component.
plot(window(london_no_covid_components$trend,
            c(2018, 1), c(2019, 12)))

################################################################################

# 6. Testing stationarity

# Test stationarity with augmented ADF test.
adf.test(london_no_covid_ts)
# As the p-value is smaller than 0.05 we reject H0 and it can be said that the 
# time series is stationary.

################################################################################

# 7. Testing autocorrelation 

# Review random time series variables.
london_no_covid_components$random

# Plot values removing NA values while doing so.
autoplot(acf(na.remove(london_no_covid_components$random), plot=FALSE)) + 
  # Add a title.
  labs(title="Randomness value") + 
  # Set the theme.
  theme_classic() 

# Plot random variables to check the distribution.
hist(london_no_covid_components$random)

################################################################################

# 8. Fit an ARIMA model

# Fit the model to our time series. 
arima_london_no_covid_ts <- auto.arima(london_no_covid_ts)

# Make a forecast for the next three months.
forecast3_london_no_covid_ts <- forecast(arima_london_no_covid_ts, 3)

# Plot the forecast on a graph.
autoplot(forecast3_london_no_covid_ts) + theme_classic()

# Print the values in the data frame.
forecast3_london_no_covid_ts

################################################################################

# 9. Extend the prediction to 2024

# Extend the prediction, set data source, time span, and assign a new object.
forecast2_london_no_covid_ts <- forecast(arima_london_no_covid_ts, 60)

# Plot the output and set the theme. 
autoplot(forecast2_london_no_covid_ts) + theme_classic()

# Print the values in the data frame.
forecast2_london_no_covid_ts

################################################################################

# 10. Extend the prediction to 2041

# Extend the prediction, set data source, time span, and assign a new object.
forecast_london_no_covid_ts <- forecast(arima_london_no_covid_ts, 240)

# Plot the output and set the theme. 
autoplot(forecast_london_no_covid_ts) + theme_classic()

# Print the values in the data frame.
forecast_london_no_covid_ts  

tail(forecast_london_no_covid_ts)

write.csv(forecast_london_no_covid_ts, 'forecast_no_covid.csv')


################################################################################

# The following section provides a forecast per area in London (inner, central 
# and outer).
# Note that the following sections have not been used in the report, however it 
# is worth mentioning our output.

################################################################################

# 1. Prepare your workstation for forecast with full years.

# Set your working directory.
london_area_full <- read.csv('trips_month_area.csv', header = T)
# Otherwise we can use:
london_area_full <- read.csv(file.choose(), header = T)

# Sense-check the new data set.
summary(london_area_full)

# The top 15 rows of the data frame.
head(london_area_full, 15)

# The bottom six rows of the data frame.
tail(london_area_full)

################################################################################

# 2. Prepare the data.

# Based on the data, we will use the d/m/Y format.
london_area_full$Survey.Month <- as.Date(london_area_full$Survey.Month, format = '%d/%m/%Y')


# View the new data format.
head(london_area_full$Survey.Month)

#### 

# SPLIT THE DATA IN INNER OUTER AND CENTRAL

# CENTRAL LONDON
central_london <- subset(london_area_full, Area == "Central")

# INNER LONDON
inner_london <- subset(london_area_full, Area == "Inner")

# OUTER LONODN
outer_london <- subset(london_area_full, Area == "Outer")

######

# Convert the data into a time series.
# Create a new data frame and assign time series value,
# and specify the 'ts' function.

# CENTRAL LONDON TS
central_london_ts <- ts(central_london$Total.cycles,
                        start = c(2014, 1),
                        # Monthly frequency without missing values in data.
                        frequency = 12)

# Sense-check the new object.
head(central_london_ts) 

# INNER LONDON TS
inner_london_ts <- ts(inner_london$Total.cycles,
                      start = c(2014, 1),
                      # Monthly frequency without missing values in data.
                      frequency = 12)

# Sense-check the new object.
head(inner_london_ts) 

# OUTER LONDON TS
outer_london_ts <- ts(outer_london$Total.cycles,
                      start = c(2014, 1),
                      # Monthly frequency without missing values in data.
                      frequency = 12)

# Sense-check the new object.
head(outer_london_ts)  


################################################################################

# 4. Group the data by month.

# CENTRAL LONDON
# Specify the boxplot function and specific operands. 
boxplot(central_london_ts~cycle(central_london_ts))


# Add labels and titles.
boxplot(central_london_ts~cycle(central_london_ts),
        ylab = "Cycle trips", 
        xlab = "Month",
        main = "Total number of cycle trips by month")

# INNER LONDON
# Specify the boxplot function and specific operands. 
boxplot(inner_london_ts~cycle(inner_london_ts))


# Add labels and titles.
boxplot(inner_london_ts~cycle(inner_london_ts),
        ylab = "Cycle trips", 
        xlab = "Month",
        main = "Total number of cycle trips by month")

# OUTER LONDON
# Specify the boxplot function and specific operands. 
boxplot(outer_london_ts~cycle(outer_london_ts))


# Add labels and titles.
boxplot(outer_london_ts~cycle(outer_london_ts),
        ylab = "Cycle trips", 
        xlab = "Month",
        main = "Total number of cycle trips by month")

################################################################################

# 5. Decomposing the data.

# CENTRAL LONDON 
# Extract and plot the main components to decompose the time series.
central_london_components <- decompose(central_london_ts)

# What is the object?
class(central_london_components)


# Determine the structure.
str(central_london_components)


# Visualise the decomposed time series.
plot(central_london_components) 


# Look at a single year (seasonal pattern) of the data.
plot(window(central_london_components$seasonal,
            c(2016, 1), c(2016, 12)))


# Plot the trend component.
plot(window(central_london_components$trend,
            c(2019, 1), c(2021, 12)))

# INNER LONDON
# Extract and plot the main components to decompose the time series.
inner_london_components <- decompose(inner_london_ts)

# What is the object?
class(inner_london_components)


# Determine the structure.
str(inner_london_components)


# Visualise the decomposed time series.
plot(inner_london_components) 


# Look at a single year (seasonal pattern) of the data.
plot(window(inner_london_components$seasonal,
            c(2016, 1), c(2016, 12)))


# Plot the trend component.
plot(window(inner_london_components$trend,
            c(2019, 1), c(2021, 12)))

# OUTER LONDON
# Extract and plot the main components to decompose the time series.
outer_london_components <- decompose(outer_london_ts)

# What is the object?
class(outer_london_components)


# Determine the structure.
str(outer_london_components)


# Visualise the decomposed time series.
plot(outer_london_components) 


# Look at a single year (seasonal pattern) of the data.
plot(window(outer_london_components$seasonal,
            c(2016, 1), c(2016, 12)))


# Plot the trend component.
plot(window(outer_london_components$trend,
            c(2019, 1), c(2021, 12)))

################################################################################

# 6. Testing stationarity

# CENTRAL LONDON
# Test stationarity with augmented ADF test.
adf.test(central_london_ts)
# As the p-value is greater than 0.05 we cannot reject H0 and it can be said that 
# the time series is non-stationary.

# INNER LONDON
# Test stationarity with augmented ADF test.
adf.test(inner_london_ts)
# As the p-value is smaller than 0.05 we reject H0 and it can be said that the 
# time series is stationary.

# OUTER LONDON
# Test stationarity with augmented ADF test.
adf.test(outer_london_ts)
# As the p-value is smaller than 0.05 we reject H0 and it can be said that the 
# time series is stationary.

################################################################################

# 7. Testing autocorrelation 

# CENTRAL LONDON
# Review random time series variables.
central_london_components$random

# Plot values removing NA values while doing so.
autoplot(acf(na.remove(central_london_components$random), plot=FALSE)) + 
  # Add a title.
  labs(title="Randomness value") + 
  # Set the theme.
  theme_classic() 

# Plot random variables to check the distribution.
hist(central_london_components$random)

# INNER LONDON
# Review random time series variables.
inner_london_components$random

# Plot values removing NA values while doing so.
autoplot(acf(na.remove(inner_london_components$random), plot=FALSE)) + 
  # Add a title.
  labs(title="Randomness value") + 
  # Set the theme.
  theme_classic() 

# Plot random variables to check the distribution.
hist(inner_london_components$random)

# OUTER LONDON
# Review random time series variables.
outer_london_components$random

# Plot values removing NA values while doing so.
autoplot(acf(na.remove(outer_london_components$random), plot=FALSE)) + 
  # Add a title.
  labs(title="Randomness value") + 
  # Set the theme.
  theme_classic() 

# Plot random variables to check the distribution.
hist(outer_london_components$random)

################################################################################

# 8. Fit an ARIMA model

# CENTRAL LONDON
# Fit the model to our time series. 
arima_central_london_ts <- auto.arima(central_london_ts)

# Make a forecast for the next three months.
forecast3_central_london_ts <- forecast(arima_central_london_ts, 3)

# Plot the forecast on a graph.
autoplot(forecast3_central_london_ts) + theme_classic()

# Print the values in the data frame.
forecast3_central_london_ts

# INNER LONDON
# Fit the model to our time series. 
arima_inner_london_ts <- auto.arima(inner_london_ts)

# Make a forecast for the next three months.
forecast3_inner_london_ts <- forecast(arima_inner_london_ts, 3)

# Plot the forecast on a graph.
autoplot(forecast3_inner_london_ts) + theme_classic()

# Print the values in the data frame.
forecast3_inner_london_ts

# OUTER LONDON
# Fit the model to our time series. 
arima_outer_london_ts <- auto.arima(outer_london_ts)

# Make a forecast for the next three months.
forecast3_outer_london_ts <- forecast(arima_outer_london_ts, 3)

# Plot the forecast on a graph.
autoplot(forecast3_outer_london_ts) + theme_classic()

# Print the values in the data frame.
forecast3_outer_london_ts

################################################################################

# 9. Extend the prediction to 2024

# CENTRAL LONDON
# Extend the prediction, set data source, time span, and assign a new object.
forecast2_central_london_ts <- forecast(arima_central_london_ts, 60)

# Plot the output and set the theme. 
autoplot(forecast2_central_london_ts) + theme_classic()

# Print the values in the data frame.
forecast2_central_london_ts  

# INNER LONDON
# Extend the prediction, set data source, time span, and assign a new object.
forecast2_inner_london_ts <- forecast(arima_inner_london_ts, 60)

# Plot the output and set the theme. 
autoplot(forecast2_inner_london_ts) + theme_classic()

# Print the values in the data frame.
forecast2_inner_london_ts  

# OUTER LONDON
# Extend the prediction, set data source, time span, and assign a new object.
forecast2_outer_london_ts <- forecast(arima_outer_london_ts, 60)

# Plot the output and set the theme. 
autoplot(forecast2_outer_london_ts) + theme_classic()

# Print the values in the data frame.
forecast2_outer_london_ts  

################################################################################

# 10. Extend the prediction to 2041

# CENTRAL LONDON
forecast_central_london_ts <- forecast(arima_central_london_ts, 264)

# Plot the output and set the theme. 
autoplot(forecast_central_london_ts) + theme_classic()

# Print the values in the data frame.
forecast_central_london_ts  

tail(forecast_central_london_ts)

# INNER LONDON
forecast_inner_london_ts <- forecast(arima_inner_london_ts, 264)

# Plot the output and set the theme. 
autoplot(forecast_inner_london_ts) + theme_classic()

# Print the values in the data frame.
forecast_inner_london_ts  

tail(forecast_inner_london_ts)

# OUTER LONDON
forecast_outer_london_ts <- forecast(arima_outer_london_ts, 264)

# Plot the output and set the theme. 
autoplot(forecast_outer_london_ts) + theme_classic()

# Print the values in the data frame.
forecast_outer_london_ts  

tail(forecast_outer_london_ts)

# EXPORT THE DATA
write.csv(forecast_central_london_ts, 'forecast_central_full_2041.csv')
write.csv(forecast_inner_london_ts, 'forecast_inner_full_2041.csv')
write.csv(forecast_outer_london_ts, 'forecast_outer_full_2041.csv')


################################################################################

################################################################################

# 1. Prepare your workstation for forecast without COIVD years.

# Set your working directory.
# Import data set
london_area_no_covid <- read.csv('trips_month_area_2019.csv', header = T)
# Otherwise we can use:
london_area_no_covid <- read.csv(file.choose(), header = T)

# Sense-check the new data set.
summary(london_area_no_covid)
# The top 15 rows of the data frame.
head(london_area_no_covid, 15)

# The bottom six rows of the data frame.
tail(london_area_no_covid)

###############################################################################

# 2. Prepare the data.

# Based on the data, we will use the d/m/Y format.
london_area_no_covid$Survey.Month <- as.Date(london_area_no_covid$Survey.Month, format = '%d/%m/%Y')

# View the new data format.
head(london_area_no_covid$Survey.Month)

#### 

# SPLIT THE DATA IN INNER OUTER AND CENTRAL

# CENTRAL LONDON
central_london <- subset(london_area_no_covid, Area == "Central")

# INNER LONDON
inner_london <- subset(london_area_no_covid, Area == "Inner")

# OUTER LONODN
outer_london <- subset(london_area_no_covid, Area == "Outer")

######

# Convert the data into a time series.
# Create a new data frame and assign time series value,
# and specify the 'ts' function.

# CENTRAL LONDON TS
central_london_ts <- ts(central_london$Total.cycles,
                        start = c(2014, 1),
                        # Monthly frequency without missing values in data.
                        frequency = 12)

# Sense-check the new object.
head(central_london_ts) 

# INNER LONDON TS
inner_london_ts <- ts(inner_london$Total.cycles,
                      start = c(2014, 1),
                      # Monthly frequency without missing values in data.
                      frequency = 12)

# Sense-check the new object.
head(inner_london_ts) 

# OUTER LONDON TS
outer_london_ts <- ts(outer_london$Total.cycles,
                      start = c(2014, 1),
                      # Monthly frequency without missing values in data.
                      frequency = 12)

# Sense-check the new object.
head(outer_london_ts)  

################################################################################

# 4. Group the data by month.

# CENTRAL LONDON
# Specify the boxplot function and specific operands. 
boxplot(central_london_ts~cycle(central_london_ts))


# Add labels and titles.
boxplot(central_london_ts~cycle(central_london_ts),
        ylab = "Cycle trips", 
        xlab = "Month",
        main = "Total number of cycle trips by month")

# INNER LONDON
# Specify the boxplot function and specific operands. 
boxplot(inner_london_ts~cycle(inner_london_ts))


# Add labels and titles.
boxplot(inner_london_ts~cycle(inner_london_ts),
        ylab = "Cycle trips", 
        xlab = "Month",
        main = "Total number of cycle trips by month")

# OUTER LONDON
# Specify the boxplot function and specific operands. 
boxplot(outer_london_ts~cycle(outer_london_ts))


# Add labels and titles.
boxplot(outer_london_ts~cycle(outer_london_ts),
        ylab = "Cycle trips", 
        xlab = "Month",
        main = "Total number of cycle trips by month")

################################################################################

# 5. Decomposing the data.

# CENTRAL LONDON 
# Extract and plot the main components to decompose the time series.
central_london_components <- decompose(central_london_ts)

# What is the object?
class(central_london_components)


# Determine the structure.
str(central_london_components)


# Visualise the decomposed time series.
plot(central_london_components) 


# Look at a single year (seasonal pattern) of the data.
plot(window(central_london_components$seasonal,
            c(2016, 1), c(2016, 12)))


# Plot the trend component.
plot(window(central_london_components$trend,
            c(2019, 1), c(2021, 12)))

# INNER LONDON
# Extract and plot the main components to decompose the time series.
inner_london_components <- decompose(inner_london_ts)

# What is the object?
class(inner_london_components)


# Determine the structure.
str(inner_london_components)


# Visualise the decomposed time series.
plot(inner_london_components) 


# Look at a single year (seasonal pattern) of the data.
plot(window(inner_london_components$seasonal,
            c(2016, 1), c(2016, 12)))


# Plot the trend component.
plot(window(inner_london_components$trend,
            c(2019, 1), c(2021, 12)))

# OUTER LONDON
# Extract and plot the main components to decompose the time series.
outer_london_components <- decompose(outer_london_ts)

# What is the object?
class(outer_london_components)


# Determine the structure.
str(outer_london_components)


# Visualise the decomposed time series.
plot(outer_london_components) 


# Look at a single year (seasonal pattern) of the data.
plot(window(outer_london_components$seasonal,
            c(2016, 1), c(2016, 12)))


# Plot the trend component.
plot(window(outer_london_components$trend,
            c(2019, 1), c(2021, 12)))


################################################################################

# 6. Testing stationarity

# CENTRAL LONDON
# Test stationarity with augmented ADF test.
adf.test(central_london_ts)
# As the p-value is smaller than 0.05 we reject H0 and it can be said that the time series is stationary.

# INNER LONDON
# Test stationarity with augmented ADF test.
adf.test(inner_london_ts)
# As the p-value is smaller than 0.05 we reject H0 and it can be said that the time series is stationary.

# OUTER LONDON
# Test stationarity with augmented ADF test.
adf.test(outer_london_ts)
# As the p-value is smaller than 0.05 we reject H0 and it can be said that the time series is stationary.

################################################################################

# 7. Testing autocorrelation 

# CENTRAL LONDON
# Review random time series variables.
central_london_components$random

# Plot values removing NA values while doing so.
autoplot(acf(na.remove(central_london_components$random), plot=FALSE)) + 
  # Add a title.
  labs(title="Randomness value") + 
  # Set the theme.
  theme_classic() 

# Plot random variables to check the distribution.
hist(central_london_components$random)

# INNER LONDON
# Review random time series variables.
inner_london_components$random

# Plot values removing NA values while doing so.
autoplot(acf(na.remove(inner_london_components$random), plot=FALSE)) + 
  # Add a title.
  labs(title="Randomness value") + 
  # Set the theme.
  theme_classic() 

# Plot random variables to check the distribution.
hist(inner_london_components$random)

# OUTER LONDON
# Review random time series variables.
outer_london_components$random

# Plot values removing NA values while doing so.
autoplot(acf(na.remove(outer_london_components$random), plot=FALSE)) + 
  # Add a title.
  labs(title="Randomness value") + 
  # Set the theme.
  theme_classic() 

# Plot random variables to check the distribution.
hist(outer_london_components$random)

################################################################################

# 8. Fit an ARIMA model

# CENTRAL LONDON
# Fit the model to our time series. 
arima_central_london_ts <- auto.arima(central_london_ts)

# Make a forecast for the next three months.
forecast3_central_london_ts <- forecast(arima_central_london_ts, 3)

# Plot the forecast on a graph.
autoplot(forecast3_central_london_ts) + theme_classic()

# Print the values in the data frame.
forecast3_central_london_ts

# INNER LONDON
# Fit the model to our time series. 
arima_inner_london_ts <- auto.arima(inner_london_ts)

# Make a forecast for the next three months.
forecast3_inner_london_ts <- forecast(arima_inner_london_ts, 3)

# Plot the forecast on a graph.
autoplot(forecast3_inner_london_ts) + theme_classic()

# Print the values in the data frame.
forecast3_inner_london_ts

# OUTER LONDON
# Fit the model to our time series. 
arima_outer_london_ts <- auto.arima(outer_london_ts)

# Make a forecast for the next three months.
forecast3_outer_london_ts <- forecast(arima_outer_london_ts, 3)

# Plot the forecast on a graph.
autoplot(forecast3_outer_london_ts) + theme_classic()

# Print the values in the data frame.
forecast3_outer_london_ts

################################################################################

# 9. Extend the prediction to 2024

# CENTRAL LONDON
# Extend the prediction, set data source, time span, and assign a new object.
forecast2_central_london_ts <- forecast(arima_central_london_ts, 60)

# Plot the output and set the theme. 
autoplot(forecast2_central_london_ts) + theme_classic()

# Print the values in the data frame.
forecast2_central_london_ts  

# INNER LONDON
# Extend the prediction, set data source, time span, and assign a new object.
forecast2_inner_london_ts <- forecast(arima_inner_london_ts, 60)

# Plot the output and set the theme. 
autoplot(forecast2_inner_london_ts) + theme_classic()

# Print the values in the data frame.
forecast2_inner_london_ts  

# OUTER LONDON
# Extend the prediction, set data source, time span, and assign a new object.
forecast2_outer_london_ts <- forecast(arima_outer_london_ts, 60)

# Plot the output and set the theme. 
autoplot(forecast2_outer_london_ts) + theme_classic()

# Print the values in the data frame.
forecast2_outer_london_ts  

################################################################################

# 10. Extend the prediction to 2041

# CENTRAL LONDON
forecast_central_london_ts <- forecast(arima_central_london_ts, 264)

# Plot the output and set the theme. 
autoplot(forecast_central_london_ts) + theme_classic()

# Print the values in the data frame.
forecast_central_london_ts  

tail(forecast_central_london_ts)

# INNER LONDON
forecast_inner_london_ts <- forecast(arima_inner_london_ts, 264)

# Plot the output and set the theme. 
autoplot(forecast_inner_london_ts) + theme_classic()

# Print the values in the data frame.
forecast_inner_london_ts  

tail(forecast_inner_london_ts)

# OUTER LONDON
forecast_outer_london_ts <- forecast(arima_outer_london_ts, 264)

# Plot the output and set the theme. 
autoplot(forecast_outer_london_ts) + theme_classic()

# Print the values in the data frame.
forecast_outer_london_ts  

tail(forecast_outer_london_ts)

# EXPORT THE DATA
write.csv(forecast_central_london_ts, 'forecast_central_no_covid.csv')
write.csv(forecast_inner_london_ts, 'forecast_inner_no_covid.csv')
write.csv(forecast_outer_london_ts, 'forecast_outer_no_covid.csv')

################################################################################

# FINAL REMARKS
# More data is required to get a reliable forecast per Area in London. 
# Further research could be done to get more insights on the future of cycling 
# in each London Area.

################################################################################
