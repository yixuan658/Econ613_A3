getwd()
setwd("/Users/86185/Documents")
rm(list = ls())
library (dplyr)

## Exercise 1
population <- read.csv ("population.csv",TRUE,',')
crime_long <- read.csv ("crime_long.csv",TRUE,',')
officers <- read.csv ("officers.csv",TRUE,',')

## Exercise 2
# Exercise 2.1
# Calculate total crime per month and plot the time series of crime
crime_group <- group_by (crime_long, crime_month)
sum_crime_by_month <- summarise (crime_group, crimes = sum (crimes))
mean (sum_crime_by_month$crimes)
plot (x = as.Date (sum_crime_by_month$crime_month),
     y = sum_crime_by_month$crimes,
     main = "Time Series of Total Crime Per Month",
     xlab = "Time",
     ylab = "Number of Crimes")

# Exercise 2.2
# Merge the two datasets by districts-units and period
data_merged <- merge (x=crime_long, y=population, by.x = c ("crime_month","district"), 
               by.y = c ("month","district"), all.y =TRUE)

# Exercise 2.3
# Construct a panel data of unit over time with the following variables
crime_grouped <- group_by (crime_long, crime_month, district)
crime_by_month_district <- summarise (crime_grouped, districtcrimes = sum(crimes))
data1 <- merge (x = data_merged, y = crime_by_month_district, by =c("crime_month","district"), all.x = TRUE)

data_violent <- filter (data1,crime_type=="violent")
violent_grouped <- group_by (data_violent, crime_month, district)
data_violent <- summarise (violent_grouped, violentcrimes = sum(crimes))
data_property <- filter (data1,crime_type=="property")
property_grouped <- group_by (data_property, crime_month, district)
data_property <- summarise (property_grouped, propertycrimes = sum(crimes))
data2 <- merge(x = data1, y = data_violent, by = c("crime_month","district"), all.x = TRUE)
data2 <- merge(x = data2, y = data_property, by = c("crime_month","district"), all.x = TRUE)
# Total crimes per resident
data2$cpr <- data2$districtcrimes / data2$tot_pop
# Total Violent crimes per resident
data2$vpr <- data2$violentcrimes / data2$tot_pop
# Property crimes per resident
data2$pcr <- data2$propertycrimes / data2$tot_pop
# Share of black, hispanic, and white residents
data2$b <- data2$tot_black / data2$tot_pop
data2$h <- data2$tot_hisp / data2$tot_pop
data2$w <- data2$tot_white / data2$tot_pop
data2$cpr
data2$vpr
data2$pcr
data2$b 
data2$h
data2$w

## Exercise 3
install.packages("AER")
library(AER)
data3 <- data2
data4 <- select (data3, -c(3:9))
data4 <- unique (data4)
data5 <- merge (x = officers, y = data4, by.x = c("month","unit"), by.y = c("crime_month","district"), all.x = TRUE)
model1 <- lm (arrest~tenure+districtcrimes+p50_inc+b+h+w-1, data = data5)
summary (model1)

## Exercise 4
model2 <- lm (arrest~tenure+districtcrimes+p50_inc+b+h+w+factor(unit)+factor(month)-1, data = data5)
summary(model2)

## Exercise 5
# 5.1 Implement a within, between, and first difference estimator
# Within estimator
install.packages("plm")
library(plm)
WI <- plm (arrest~tenure+districtcrimes+p50_inc+b+h+w,
                   data = data5,
                   index = c("NUID","month","unit"),
                   model = "Within",
                   effect = "nested")
summary(WI)
coeftest (WI, vcov. = vcovHC, type = "HC1")

# Between estimator
BTW <- plm (arrest~tenure+districtcrimes+p50_inc+b+h+w,
                    data = data5,
                    index = c("NUID","month","unit"),
                    model = "Between",
                    effect = "nested")
summary(BTW)
coeftest (BTW, vcov. = vcovHC, type = "HC1")

# First difference estimator
FD <- plm (arrest~tenure+districtcrimes+p50_inc+b+h+w,
               data = data5,
               index = c("NUID","month","unit"),
               model = "First Difference",
               effect = "nested")
summary(FD)
coeftest (FD, vcov. = vcovHC, type = "HC1")

# 5.2
install.packages("sandwich")
library(gmm)

