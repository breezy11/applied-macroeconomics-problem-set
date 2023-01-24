# Zadaća dnevnica

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

# after removing the missing values, we want to leave the countries that are repeating 59 times,
# which means they have data for every year for the time frame 1960-2019, which is what we need in our case 
sort(table(df$country), decreasing = TRUE)

# getting the first 41 countries from our list, because we saw that 41 countries have all the values
countries = names(head(sort(table(df$country), decreasing = TRUE), 41))

# taking a subset of the dataframe with just those countries
df = subset(df, country %in% countries)

# creating new columns where we have the GDP values in log format
df$lognominalGDP <- log(df$nominalGDP)
df$logrealGDP <- log(df$realGDP)

# calculate the mean and std for all countries in the period 1960-2019
df_average <- df_average %>% group_by(country) %>% summarise("inflation-mean" = mean(inflation), 
                                       "inflation-std" = sd(inflation),
                                       "nominal-GDP-mean" = mean(nominalGDP), 
                                       "nominal-GDP-std" = sd(nominalGDP),
                                       "real-GDP-mean" = mean(realGDP),
                                       "real-GDP-std" = sd(realGDP),
                                       )

# do the regression for each country separately and get the tau value
tau_values = c()

for(one_country in countries){
  country_data <- filter(df, country == one_country)
  country_data <- country_data %>% arrange(desc(country_data$year))
  
  change_log_nominalGDP <- diff(country_data$lognominalGDP)
  change_log_nominalGDP = append(change_log_nominalGDP, 0)
  
  country_data$change_log_nominalGDP = change_log_nominalGDP
  
  model <- lm(logrealGDP ~ change_log_nominalGDP +  lag(logrealGDP, 1), data = country_data)
  
  tau_values<- c(tau_values , coefficients(model)[2][["change_log_nominalGDP"]])
  }

df_tau = data.frame(unlist(countries),unlist(tau_values))
names(df_tau) = c("country","tau")

## Problem 1.

# a) Give the economic interpretation of τ . What does it mean if τ is low or high?
# 
# T is the output-inflation trade-off which refers to the relationship between the level
# of economic output (usually measured in GDP), and the rate of inflation. The trade-off
# is important regarded as a metric that is looked when considering macroeconomi policies,
# business cycles, long term growth, etc.
# When the output-inflation tradeoff is high, it means that a country is able to
# achieve higher levels of output without experiencing significant increases in 
# inflation. When the output-inflation tradeoff is low, it means that a country 
# experiences significant increases in inflation as it tries to achieve higher 
# levels of output.

# b) Choose a subset of three countries and compare your results to those of Ball et
# al. (1988). Did the parameter τ change under your period of study compared
# to that by Ball et al. (1988)?

df_subset = subset(df_tau, country %in% c("Austria", "United Kingdom", "United States"))

# The comparison of tau values in the paper and ours.
# country             tau_paper     tau_now
# 2         Austria   - 0.0196      -0.01316750
# 39 United Kingdom   -0.0199       -0.01204883
# 40  United States   0.6714        -0.06360382

# The comparison of the two countries looks normal, the tau values have not changed much which is 
# expected, but for the United States, we have a big shift in our period towards theirs, so Im not sure
# really whats up with that.

# c) Equivalent to Ball et al. (1988) produce a scatterplot where τ is on the y-axis
# and mean inflation is on the x-axis. How does your graph compare to the one
# by Ball et al. (1988)?

new_df = data.frame(unlist(df_average$`inflation-mean`),unlist(df_tau$tau))
names(new_df) = c("inflation", "tau")

ggplot(new_df, aes(x = inflation , y = tau)) +
  geom_point() +
  ggtitle("Mean inflation v Trade-off parameter scatter plot")+
  geom_hline(yintercept = 0) +
  labs(x = "Mean Inflation", y = "Trade-off parameter")

df_average[c("country", "inflation-mean")]

# They are very similar in nature, but I need to rebrand the values that I use,
# that is the problem with the numbers I get for sure.

# d) What are potential explanations for any cross-counry differences in τ?

# Answer the economics question.

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

df_austria <- df[df$country == "Austria", ]
df_france <- df[df$country == "France", ]
df_uk <- df[df$country == "United Kingdom", ]

ggplot(df_austria, aes(x = unemployment, y = inflation)) +
  geom_point() +
  geom_smooth(method = "lm") +
  ggtitle("Austria Phillips Curve") +
  xlab("Unemployment Rate (%)") +
  ylab("Inflation Rate (%)")


ggplot(df_france, aes(x = unemployment, y = inflation)) +
  geom_point() +
  geom_smooth(method = "lm") +
  ggtitle("France Phillips Curve") +
  xlab("Unemployment Rate (%)") +
  ylab("Inflation Rate (%)")


ggplot(df_uk, aes(x = unemployment, y = inflation)) +
  geom_point() +
  geom_smooth(method = "lm") +
  ggtitle("United Kingdom Phillips Curve") +
  xlab("Unemployment Rate (%)") +
  ylab("Inflation Rate (%)")

# c) Check the stability of the Phillips curve through time. Plot the Phillips curve
# for Austria and the United Kingdom from 1971 to 1986, from 1987 to 2002 and
# from 2003 until today separately for each country in one graph.
# Does the Phillips curve change or remain constant? If it changes, explain how

df <- wb_data(c("NY.GDP.MKTP.CD","NY.GDP.MKTP.KD", "FP.CPI.TOTL.ZG", "SL.UEM.TOTL.NE.ZS"), start_date = 1971, end_date=1986, freq="Y")
df <- df[c('country','date','FP.CPI.TOTL.ZG','NY.GDP.MKTP.CD','NY.GDP.MKTP.KD', 'SL.UEM.TOTL.NE.ZS')]
names(df) <- c('country','year','inflation','nominalGDP','realGDP', 'unemployment')
df <- na.omit(df)

df_1st <- df[df$country %in% c("Austria", "United Kingdom"), ]


ggplot(df_1st, aes(x = unemployment, y = inflation, color = country)) +
  geom_point() +
  ggtitle("Phillips Curve 1971-1986") +
  xlab("Unemployment Rate (%)") +
  ylab("Inflation Rate (%)")

df <- wb_data(c("NY.GDP.MKTP.CD","NY.GDP.MKTP.KD", "FP.CPI.TOTL.ZG", "SL.UEM.TOTL.NE.ZS"), start_date = 1987, end_date=2002, freq="Y")
df <- df[c('country','date','FP.CPI.TOTL.ZG','NY.GDP.MKTP.CD','NY.GDP.MKTP.KD', 'SL.UEM.TOTL.NE.ZS')]
names(df) <- c('country','year','inflation','nominalGDP','realGDP', 'unemployment')
df <- na.omit(df)

df_2nd <- df[df$country %in% c("Austria", "United Kingdom"), ]


ggplot(df_2nd, aes(x = unemployment, y = inflation, color = country)) +
  geom_point() +
  ggtitle("Phillips Curve 1987-2002") +
  xlab("Unemployment Rate (%)") +
  ylab("Inflation Rate (%)")

df <- wb_data(c("NY.GDP.MKTP.CD","NY.GDP.MKTP.KD", "FP.CPI.TOTL.ZG", "SL.UEM.TOTL.NE.ZS"), start_date = 2003, end_date=2023, freq="Y")
df <- df[c('country','date','FP.CPI.TOTL.ZG','NY.GDP.MKTP.CD','NY.GDP.MKTP.KD', 'SL.UEM.TOTL.NE.ZS')]
names(df) <- c('country','year','inflation','nominalGDP','realGDP', 'unemployment')
df <- na.omit(df)

df_3rd <- df[df$country %in% c("Austria", "United Kingdom"), ]


ggplot(df_3rd, aes(x = unemployment, y = inflation, color = country)) +
  geom_point() +
  geom_smooth(method = "lm") +
  ggtitle("Phillips Curve 2003-2023") +
  xlab("Unemployment Rate (%)") +
  ylab("Inflation Rate (%)")

# d) What are possible reasons for a change in the Phillips curve over time?

