# Applied Macroeconomics
# Problem Set 5
# Ali Suliman - 01651668

# installing all the necessary packages

# install.packages("wbstats")
# install.packages("ggplot2")
# install.packages("dplyr)

# importing all the necessary packages
library(wbstats)
library(dplyr)
library(ggplot2)

# getting the data using the World Bank API from wbstats
df <- wb_data(c("NY.GDP.MKTP.CD","NY.GDP.MKTP.KD", "FP.CPI.TOTL.ZG"), start_date = 1960, end_date = 2018, freq="Y")

# keeping only the columns we need for now
df <- df[c('country','date','FP.CPI.TOTL.ZG','NY.GDP.MKTP.CD','NY.GDP.MKTP.KD')]

# renaming the data frame columns
names(df) <- c('country','year','inflation','nominalGDP','realGDP')

# removing all the missing values (NAs)
df <- na.omit(df)

# after removing the missing values, we want to leave the countries that have the most number of occurences in the data set
# in our case that is 59, because we have 59 years in our time period

# getting the maximum number of any country ocurring in the data frame (that is 59)
max_ocurrence = max(sort(table(df$country), decreasing = TRUE))

# getting the names of the countries that are ocurring = max_occurence times
countries = names(which(table(df$country) == max_ocurrence))

# taking a subset of the dataframe with just those countries
df = subset(df, country %in% countries)

# creating new columns where we have the GDP values in log format
df$lognominalGDP <- log(df$nominalGDP)
df$logrealGDP <- log(df$realGDP)

# calculate the mean and std for all countries in the period 1960-2019
df_average <- df %>% group_by(country) %>% summarise("inflation-mean" = mean(inflation), 
                                       "inflation-std" = sd(inflation),
                                       "nominal-GDP-mean" = mean(nominalGDP), 
                                       "nominal-GDP-std" = sd(nominalGDP),
                                       "real-GDP-mean" = mean(realGDP),
                                       "real-GDP-std" = sd(realGDP),
                                       )

# do the regression for each country separately and get the tau value
tau_values = c()

# going through every country we have in the balanced sample
for(one_country in countries){
  # accessing the data by comparing the country name
  country_data <- filter(df, country == one_country)
  country_data <- country_data %>% arrange(desc(country_data$year))
  
  # calculating the change in log nominal GFP for every country separately
  change_log_nominalGDP <- diff(country_data$lognominalGDP)
  # adding the 0 number at the end of the list because we can not have the change in log nominal GDP for the year 1960
  # as we don't have the data for the previosu year, therefore we append 0
  change_log_nominalGDP = append(change_log_nominalGDP, 0)
  
  country_data$change_log_nominalGDP = change_log_nominalGDP
  
  # fitting the regression model
  model <- lm(logrealGDP ~ change_log_nominalGDP +  lag(logrealGDP, 1), data = country_data)
  
  # storing the tau values in our list
  tau_values<- c(tau_values , coefficients(model)[2][["change_log_nominalGDP"]])
  }

# combining the tau values along with the country they belong to in a dataframe
df_tau = data.frame(unlist(countries),unlist(tau_values))
names(df_tau) = c("country","tau")

## Problem 1.

# a) Give the economic interpretation of τ . What does it mean if τ is low or high?
# answer in the pdf


# b) Choose a subset of three countries and compare your results to those of Ball et
# al. (1988). Did the parameter τ change under your period of study compared
# to that by Ball et al. (1988)?

# taking the subset of the data for the 3 countries I chose
df_subset = subset(df_tau, country %in% c("Austria", "United Kingdom", "United States"))

# more about the comparison in the pdf

# c) Equivalent to Ball et al. (1988) produce a scatterplot where τ is on the y-axis
# and mean inflation is on the x-axis. How does your graph compare to the one
# by Ball et al. (1988)?

# quickly creating a new dataframe that will contain the data we need to make the scatter plot
new_df = data.frame(unlist(df_average$`inflation-mean`),unlist(df_tau$tau))
names(new_df) = c("inflation", "tau")

# using the ggplot library to make the scatter plot
ggplot(new_df, aes(x = inflation , y = tau)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  labs(x = "Mean Inflation", y = "Trade-off parameter")

# more in the pdf about how they compare

# d) What are potential explanations for any cross-country differences in τ?
# answer in the pdf

# Problem 2. We want you to work on the Phillips curve by describing its theoretical
# foundation and checking whether it has been stable in selected economies.

# a) Describe the Phillips curve and the relationship between its variables.

# b) Plot the Phillips curves for Austria, France, the United Kingdom and the
# United States from 1990 until today. Plot the inflation rate on the y-axis and
# the unemployment rate on the x-axis.

# getting the data using the World Bank API from wbstats
df <- wb_data(c("NY.GDP.MKTP.CD","NY.GDP.MKTP.KD", "FP.CPI.TOTL.ZG", "SL.UEM.TOTL.NE.ZS"), start_date = 1990, end_date=2023, freq="Y")

# keeping only the columns we need for now
df <- df[c('country','date','FP.CPI.TOTL.ZG','NY.GDP.MKTP.CD','NY.GDP.MKTP.KD', 'SL.UEM.TOTL.NE.ZS')]

# renaming the data frame columns
names(df) <- c('country','year','inflation','nominalGDP','realGDP', 'unemployment')

# removing all the missing values (NAs)
df <- na.omit(df)

# getting the data separately for every country and storing them in separate data frames
df_austria <- df[df$country == "Austria", ]
df_france <- df[df$country == "France", ]
df_uk <- df[df$country == "United Kingdom", ]

# plotting the phillips curve for Austria
ggplot(df_austria, aes(x = unemployment, y = inflation)) +
  geom_point() +
  geom_smooth(method = "lm") +
  xlab("Unemployment Rate (%)") +
  ylab("Inflation Rate (%)")

# plotting the phillips curve for France
ggplot(df_france, aes(x = unemployment, y = inflation)) +
  geom_point() +
  geom_smooth(method = "lm") +
  xlab("Unemployment Rate (%)") +
  ylab("Inflation Rate (%)")

# plotting the phillips curve for UK
ggplot(df_uk, aes(x = unemployment, y = inflation)) +
  geom_point() +
  geom_smooth(method = "lm") +
  xlab("Unemployment Rate (%)") +
  ylab("Inflation Rate (%)")

# c) Check the stability of the Phillips curve through time. Plot the Phillips curve
# for Austria and the United Kingdom from 1971 to 1986, from 1987 to 2002 and
# from 2003 until today separately for each country in one graph.
# Does the Phillips curve change or remain constant? If it changes, explain how

# again same process, getting the data, leaving the columns we need, renaming them, removing the missing values
df <- wb_data(c("NY.GDP.MKTP.CD","NY.GDP.MKTP.KD", "FP.CPI.TOTL.ZG", "SL.UEM.TOTL.NE.ZS"), start_date = 1971, end_date=1986, freq="Y")
df <- df[c('country','date','FP.CPI.TOTL.ZG','NY.GDP.MKTP.CD','NY.GDP.MKTP.KD', 'SL.UEM.TOTL.NE.ZS')]
names(df) <- c('country','year','inflation','nominalGDP','realGDP', 'unemployment')
df <- na.omit(df)

# creating the separate dataframe for the 1st period we need
df_1st <- df[df$country %in% c("Austria", "United Kingdom"), ]

ggplot(df_1st, aes(x = unemployment, y = inflation, color = country)) +
  geom_point() +
  geom_smooth(method = "lm") +
  xlab("Unemployment Rate (%)") +
  ylab("Inflation Rate (%)") +
  theme(legend.position="top")

# again same process as for the 1st period
df <- wb_data(c("NY.GDP.MKTP.CD","NY.GDP.MKTP.KD", "FP.CPI.TOTL.ZG", "SL.UEM.TOTL.NE.ZS"), start_date = 1987, end_date=2002, freq="Y")
df <- df[c('country','date','FP.CPI.TOTL.ZG','NY.GDP.MKTP.CD','NY.GDP.MKTP.KD', 'SL.UEM.TOTL.NE.ZS')]
names(df) <- c('country','year','inflation','nominalGDP','realGDP', 'unemployment')
df <- na.omit(df)
df_2nd <- df[df$country %in% c("Austria", "United Kingdom"), ]

ggplot(df_2nd, aes(x = unemployment, y = inflation, color = country)) +
  geom_point() +
  geom_smooth(method = "lm") +
  xlab("Unemployment Rate (%)") +
  ylab("Inflation Rate (%)")+ 
  theme(legend.position="top")

# again same process as for the 1st and 2nd period
df <- wb_data(c("NY.GDP.MKTP.CD","NY.GDP.MKTP.KD", "FP.CPI.TOTL.ZG", "SL.UEM.TOTL.NE.ZS"), start_date = 2003, end_date=2023, freq="Y")
df <- df[c('country','date','FP.CPI.TOTL.ZG','NY.GDP.MKTP.CD','NY.GDP.MKTP.KD', 'SL.UEM.TOTL.NE.ZS')]
names(df) <- c('country','year','inflation','nominalGDP','realGDP', 'unemployment')
df <- na.omit(df)
df_3rd <- df[df$country %in% c("Austria", "United Kingdom"), ]


ggplot(df_3rd, aes(x = unemployment, y = inflation, color = country)) +
  geom_point() +
  geom_smooth(method = "lm") +
  xlab("Unemployment Rate (%)") +
  ylab("Inflation Rate (%)") +
  theme(legend.position="top")

# d) What are possible reasons for a change in the Phillips curve over time?
# answer in the pdf