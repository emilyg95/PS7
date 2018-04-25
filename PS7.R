library(readr)
March2018 <- read_csv("~/Documents/School/Second Sem/R/Problem Sets/PS7/March2018.CSV")
View(March2018)

install.packages("tidyr")
library(tidyr)

install.packages("dplyr")
library(dplyr)

# separates DateOccur into 2 separate variables for date and time
March2018 <- March2018 %>% 
  separate(DateOccur, into = c("Date", "Time"), sep = " ")

# splits description into one variable to describe more general categories and another variable with details
March2018 <- March2018 %>% 
  separate(Description, into = c("Main", "Secondary"), sep = "-")

# splits again to cover missed ground
March2018 <- March2018 %>% 
  separate('Main', into = c("Main", "Secondary2"), sep = "/")

# removes secondary descriptions
March2018$Secondary <- NULL
March2018$Secondary2 <- NULL

#sorts crime by type and date
crime_date <- March2018 %>% 
  group_by(Date, Main) %>% 
  summarise(count=n())
crime_date

# you can see there are some dates outside of March listed but they were coded March by the police presumably for a reason so I'm leaving them

# lists crime count by description
crime_type <- March2018 %>% 
  group_by(Main) %>% 
  summarise(count=n())
crime_type

# most frequently occuring crime in March is larceny
top_n(crime_type, 1)

# lists crime count by neighborhood and date
neighborhood_date <- March2018 %>% 
  group_by(Neighborhood, Date) %>% 
  summarise(count=n())
neighborhood_date

# lists crime count by neighborhood
neighborhood_crime <- March2018 %>% 
  group_by(Neighborhood) %>% 
  summarise(count=n())
neighborhood_crime

# neighborhood 35 has the most crime in March with 305 incidents
top_n(neighborhood_crime, 1)

# factors main description to identify crimes related to robbery
March2018$Main <- as.factor(March2018$Main)
levels(March2018$Main)

# filters dataset by robbery related crimes
just_robbery <- filter(March2018, Main == 'ROBBERY' | Main == 'BURGLARY' | Main == 'EMBEZZLEMENT'
                       | Main == 'LARCENY' | Main == 'STLG BY DECEIT' | Main == 'AUTO THEFT'
                       | Main == 'FAILURE TO RETURN BORROWED' | Main == 'ROBBERY CARJACKING' | Main == 'STOLEN PROPERTY')

# lists robbery count by neighborhood
neighborhood_robbery <- just_robbery %>% 
  group_by(Neighborhood) %>% 
  summarise(count=n())
neighborhood_robbery

# renaming count columns for merging
colnames(neighborhood_robbery) <- c("Neighborhood", "Robbery_Count")
colnames(neighborhood_crime) <- c("Neighborhood", "Crime_Count")

# combining crime datasets
neighborhood_crime <- left_join(neighborhood_robbery, neighborhood_crime)

# creating new variable with robbery as a proportion of crime by neighborhood
neighborhood_crime <- mutate(neighborhood_crime, Robbery_Prop = Robbery_Count/Crime_Count)

# neighborhoods 45 and 85 have the highest proportions of robbery related crime -- 100%
top_n(neighborhood_crime, 1)


ggplot(data=diamonds)





