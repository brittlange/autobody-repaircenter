#load libraries
library(dplyr) # Used for data manipulation
library(knitr) # Used for R-Markdown knitting
library(kableExtra) # Used for Kable Tables
library(plotly) # Used for plotting
library(lubridate) #Used for Date manipulation
library(randomForest) # Used for building Random Forest models
library(rpart) # Used for building CART models
library(rpart.plot) # Used for plotting tree

#### Load Crash Data
crashes_df <- read.csv('../../..Auto Repair Shop Planning/Data/Crashes.csv', stringsAsFactors = FALSE) %>% 
  mutate(CRASH.DATE = as.Date(CRASH.DATE, "%m/%d/%Y")) #1,896,229 x 29

#### Load Person Data
person_df <- read.csv('../../..Auto Repair Shop Planning/Data/Person.csv', stringsAsFactors = FALSE) %>%
  mutate(CRASH_DATE = as.Date(CRASH_DATE, "%m/%d/%Y")) #4,692,054 x 21

#### Load Vehicles Data
vehicles_df <- read.csv('../../..Auto Repair Shop Planning/Data/Vehicles.csv', stringsAsFactors = FALSE) %>%
  mutate(CRASH_DATE = as.Date(CRASH_DATE, "%m/%d/%Y")) #3,704,406 x 25

#### Trim Down Data
monthly_agg1 <- (person_df %>% 
                   mutate(yr_mo = paste0(substr(CRASH_DATE,1,4),'-',substr(CRASH_DATE,6,7))) %>% 
                   group_by(yr_mo) %>% 
                   summarise(n = n()) %>% 
                   arrange(yr_mo)
)
#filter data, create combined_df2
combined_df2 <- (crashes_df %>% 
                   select(-c(CRASH.DATE, CRASH.TIME, CONTRIBUTING.FACTOR.VEHICLE.1, CONTRIBUTING.FACTOR.VEHICLE.2,
                             CONTRIBUTING.FACTOR.VEHICLE.3, CONTRIBUTING.FACTOR.VEHICLE.4, CONTRIBUTING.FACTOR.VEHICLE.5,
                             VEHICLE.TYPE.CODE.4, VEHICLE.TYPE.CODE.5,
                             NUMBER.OF.PEDESTRIANS.INJURED, NUMBER.OF.PEDESTRIANS.KILLED,
                             NUMBER.OF.CYCLIST.INJURED, NUMBER.OF.CYCLIST.KILLED,
                             ON.STREET.NAME, CROSS.STREET.NAME, OFF.STREET.NAME)) %>% 
                   inner_join(vehicles_df %>% 
                                filter(!is.na(VEHICLE_ID)) %>% 
                                select(-c(CRASH_DATE, CRASH_TIME, VEHICLE_ID))
                              , by = 'COLLISION_ID') %>% 
                   inner_join(person_df %>% 
                                select(-c(UNIQUE_ID, CONTRIBUTING_FACTOR_1, CONTRIBUTING_FACTOR_2))
                              , by= c('COLLISION_ID'='COLLISION_ID', 'UNIQUE_ID'='VEHICLE_ID')) %>% 
                   filter((PED_ROLE %in% c('Driver'))) %>%  #drivers only
                   filter((CRASH_DATE >= '2019-01-01') & (CRASH_DATE < '2020-12-31')) %>% 
                   #only from 2017-01-01 to 2021-11-30
                   filter(PERSON_AGE > 14 & PERSON_AGE < 101) %>%
                   filter(PERSON_SEX == 'F' | PERSON_SEX == 'M') %>%
                   mutate(DAY = substr(CRASH_DATE,9,10)) %>%
                   mutate(TIMESTEP2 = as.period(interval(as.Date('2019-01-01'), CRASH_DATE)) %/% days(1)) %>% 
                   ####add TIMESTEP2
                   mutate(yr_mo_day = paste0(substr(CRASH_DATE,1,4),'-',substr(CRASH_DATE,6,7),'-',substr(CRASH_DATE,9,10))) %>% 
                   ######add yr_mo_day
                   select(COLLISION_ID, CRASH_DATE, DAY, TIMESTEP2, yr_mo_day, NUMBER.OF.PERSONS.KILLED, 
                          PERSON_AGE, PERSON_SEX) %>% 
                   ####ADD KILLED, DAY, TIMESTEP2, yr_mo_day
                   arrange(CRASH_DATE)
)
kable(t(summary(combined_df2))) %>% kable_classic(full_width = TRUE, html_font = "Cambria", font_size = 14)
#create daily aggregate and view crashes by day
daily_agg1 <- (combined_df2 %>% 
                 group_by(TIMESTEP2, yr_mo_day, DAY) %>% 
                 summarise(n = n(), .groups = 'drop') %>% 
                 arrange(TIMESTEP2, yr_mo_day, DAY)
)
plot_ly(x=daily_agg1$yr_mo_day, y=daily_agg1$n, type='bar') %>%
  layout(title = "Crash Data by Day from 2019-2020", xaxis = list(title = 'Day'), yaxis = list(title = 'Number of Crashes'))
#create covid_df for covid analysis
#create covid_df for COVID analysis
covid_df <- (combined_df2 %>% 
               filter((CRASH_DATE >= '2019-01-01') & (CRASH_DATE < '2020-12-31')) %>% #only from 2019-01-01 to 2020-12-31))
               mutate(covid = ifelse(CRASH_DATE >= '2020-03-16', 1, 0)) %>%
               rename(fatalities = NUMBER.OF.PERSONS.KILLED) %>%
               group_by(TIMESTEP2, yr_mo_day, DAY, covid, fatalities) %>% 
               summarise(n = n(), .groups = 'drop') %>% 
               arrange(TIMESTEP2, yr_mo_day, DAY, covid, fatalities)
)

#### Model to Analyze Covid Effect
covid_model1 <- lm(n ~ covid, data = covid_df)
summary(covid_model1)
