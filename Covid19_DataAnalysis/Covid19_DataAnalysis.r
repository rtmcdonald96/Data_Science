#Project 1 Data Science
#Ryan McDonald
#CPSC 375 - Dr. Panangadan
#Spring 2021
#To execute project simply open file in rstudio and select and run all lines.
#CSV files need to be in the same folder as this .r file.
#To execute correctly in RStudio: Session -> Set Working Directory -> To Source File Location

#Library Loading
library(tidyverse)

#Data Set Loading
covid_confirmed <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")
covid_deaths <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv")
hospital_data <- read_csv('data.csv')
demographics <- read_csv('demographics.csv')

#=======================================
#Data Preparation/wrangling
#=======================================
#==============================
#Tidying Up covid_deaths
#==============================
covid_deaths[covid_deaths$'Country/Region' == "Congo (Brazzaville)", 'Country/Region'] <- "Congo"
covid_deaths[covid_deaths$'Country/Region' == "Congo (Kinshasa)", 'Country/Region'] <- "Congo"
covid_deaths[covid_deaths$'Country/Region' == "Taiwan*", 'Country/Region'] <- "Taiwan"
covid_deaths[covid_deaths$'Country/Region' == "Korea, South", 'Country/Region'] <- "South Korea"

#Pivot into tidy format
covid_deaths <- covid_deaths %>% 
  pivot_longer(-(1:4),names_to= "Date", values_to ="Deaths")

#Convert to factors for grouping
covid_deaths$`Date` <- as_factor(covid_deaths$`Date`)
covid_deaths$`Country/Region` <- as_factor(covid_deaths$`Country/Region`)

#Sum deaths up for each country
covid_deaths <- aggregate(covid_deaths$'Deaths', by=list(Group=covid_deaths$'Country/Region', covid_deaths$'Date'), FUN=sum)

#Rename Columns for description
names(covid_deaths)[1] <- 'Country'
names(covid_deaths)[2] <- 'Date'
names(covid_deaths)[3] <- 'Deaths'

#==============================
#Tidying Up covid_confirmed
#==============================
covid_confirmed[covid_confirmed$'Country/Region' == "Congo (Brazzaville)", 'Country/Region'] <- "Congo"
covid_confirmed[covid_confirmed$'Country/Region' == "Congo (Kinshasa)", 'Country/Region'] <- "Congo"
covid_confirmed[covid_confirmed$'Country/Region' == "Taiwan*", 'Country/Region'] <- "Taiwan"
covid_confirmed[covid_confirmed$'Country/Region' == "Korea, South", 'Country/Region'] <- "South Korea"

#Pivot into tidy format
covid_confirmed <- covid_confirmed %>% 
  pivot_longer(-(1:4),names_to= "Date", values_to ="confirmed")

#Convert to factors for grouping
covid_confirmed$`Date` <- as_factor(covid_confirmed$`Date`)
covid_confirmed$`Country/Region` <- as_factor(covid_confirmed$`Country/Region`)

#Sum deaths up for each country
covid_confirmed <- aggregate(covid_confirmed$'confirmed', by=list(Group=covid_confirmed$'Country/Region', covid_confirmed$'Date'), FUN=sum)

#Rename Columns for description
names(covid_confirmed)[1] <- 'Country'
names(covid_confirmed)[2] <- 'Date'
names(covid_confirmed)[3] <- 'confirmed'

#==============================
#Tidying up hospital_data
#==============================
#Pull only hospital bed number from most recent year
hospital_data <- hospital_data %>% group_by(Country) %>% top_n(1, Year) %>% select(-Year)

#==============================
#Tidy up demographics
#==============================
#Pivot into tidy Format
demographics <- demographics %>% 
  select(`Country Name`,`Series Name`, YR2015) %>% 
  pivot_wider(names_from = `Series Name`, values_from = 'YR2015')

#Combine Male and Female Data
demographics <- demographics %>% mutate(demographics[5]+demographics[6]) %>% select(-6)
names(demographics)[5]<- 'Population ages 80 and above'

demographics <- demographics %>% mutate(demographics[6]+demographics[7]) %>% select(-7)
names(demographics)[6]<- 'Population ages 15-64'

demographics <- demographics %>% mutate(demographics[7]+demographics[8]) %>% select(-8)
names(demographics)[7]<- 'Population ages 0-14'

demographics <- demographics %>% mutate((demographics[8]+demographics[9])/2) %>% select(-9)
names(demographics)[8]<- 'Mortality rate, adult (per 1000 adults)'

demographics <- demographics %>% select(-(9:10))

demographics <- demographics %>% mutate((demographics[9]+demographics[10])) %>% select(-10)
names(demographics)[9]<- 'Population ages 65 and above'
names(demographics)[1]<- 'Country'

#==============================
#Standardize Names
#==============================
#hospital_data
hospital_data[hospital_data$Country == "Iran (Islamic Republic of)", "Country"] <- "Iran"
hospital_data[hospital_data$Country == "Republic of Korea", "Country"] <- "South Korea"
hospital_data[hospital_data$Country == "United Kingdom of Great Britain and Northern Ireland", "Country"] <- "United Kingdom"

#demographics
demographics[demographics$Country == "Bahamas, The", "Country"] <- "Bahamas"
demographics[demographics$Country == "Iran, Islamic Rep.", "Country"] <- "Iran"
demographics[demographics$Country == "Korea, Rep.", "Country"] <- "South Korea"
demographics[demographics$Country == "Korea, Dem. People's Rep.", "Country"] <- "North Korea"

#==============================
#Join Into One Table
#==============================
covid_data <- merge(covid_deaths,covid_confirmed, by=c('Country','Date'))
covid_data <- covid_data %>% left_join(hospital_data, by='Country')
covid_data <- covid_data %>% left_join(demographics, by='Country')



#=======================================
#Linear Modeling
#======================================
#Covid Data columns 4-9 are potential Predictor variables
#Covid Data column 3(Deaths) is the independent variable

model_1<- lm(Deaths~confirmed, data=covid_data)
summary(model_1)

model_2<- lm(Deaths~covid_data[,4]+covid_data[,5]+covid_data[,6]+covid_data[,7]+covid_data[,8]+covid_data[,9], data=covid_data)
summary(model_2)

#Omit NA to further analyze data
covid_data <- na.omit(covid_data)

model_3<- lm(Deaths~confirmed, data=covid_data)
summary(model_3)

model_4<- lm(Deaths~covid_data[,4]+covid_data[,5]+covid_data[,6]+covid_data[,7]+covid_data[,8]+covid_data[,9], data=covid_data)
summary(model_4)

model_5<- lm(Deaths~confirmed+covid_data[,9], data=covid_data)
summary(model_5)

model_6<- lm(Deaths~confirmed+covid_data[,8]+covid_data[,9], data=covid_data)
summary(model_6)

model_7<- lm(Deaths~covid_data[,8], data=covid_data)
summary(model_7)

model_8<- lm(Deaths~confirmed+covid_data[,8]+covid_data[,13]+covid_data[,12], data=covid_data)
summary(model_8)

model_9<- lm(Deaths~confirmed+covid_data[,5]+covid_data[,8]+covid_data[,9], data=covid_data)
summary(model_9)

model_10<- lm(Deaths~confirmed+covid_data[,5]+covid_data[,8]+covid_data[,9]+covid_data[,12], data=covid_data)
summary(model_10)

model_11<- lm(Deaths~confirmed+covid_data[,8], data=covid_data)
summary(model_11)


#=======================================
#Variable Importance using caret Library
#======================================
library(caret)

#Model 4 is the model using all features
varImp(model_4)

model_12<- lm(Deaths~covid_data[,4]+covid_data[,7]+covid_data[,8], data=covid_data)
summary(model_12)