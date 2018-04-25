library(readr)
March2018 <- read_csv("~/Documents/School/Second Sem/R/Problem Sets/PS7/March2018.CSV")
View(March2018)

install.packages("tidyr")
library(tidyr)

# separates DateOccur into 2 separate variables for date and time
March2018 <- March2018 %>% 
  separate(DateOccur, into = c("Date", "Time"), sep = " ")

# splits description into one variable to describe more general categories and another variable with details
March2018 <- March2018 %>% 
  separate(Description, into = c("Main Description", "Secondary Description"), sep = "-")

# splits again to cover missed ground
March2018 <- March2018 %>% 
  separate('Main Description', into = c("Main Description", "Secondary Description2"), sep = "/")

# combines the two secondary description variables
March2018 <-
  unite(March2018, col = "Secondary Description", c("Secondary Description", "Secondary Description2"))

# factors main description to look at all possible values
March2018$'Main Description' <- as.factor(March2018$`Main Description`)
levels(March2018$`Main Description`)

# orders dataset by day
by_day <- group_by(March2018, Date)

# lists crime count by description
crime_type <- March2018 %>% 
  group_by(`Main Description`) %>% 
  summarise(count=n())
crime_type

# most frequently occuring crime in March is larceny
top_n(crime_type, 1)

# lists crime count by neighborhood
neighborhood_crime <- March2018 %>% 
  group_by(Neighborhood) %>% 
  summarise(count=n())
neighborhood_crime

# neighborhood 35 has the most crime
top_n(neighborhood_crime, 1)

