install.packages('tidyverse','lubridate')
install.packages('lubridate')
library('tidyverse')
library('lubridate')

# headers for csv files
headersID = c("id",'first','last','hand','DOB','country')
headersRankings = c("date",'rank','player_id','points')

# import data files with headers passed
players <- read.csv(url("https://raw.githubusercontent.com/maxfritz/tennis_atp/master/atp_players.csv"),header=FALSE, col.names=headersID)
rankings00s <- read.csv(url("https://raw.githubusercontent.com/maxfritz/tennis_atp/master/atp_rankings_00s.csv"),header=FALSE, col.names=headersRankings)

# transform the DOB into date; pull out year
players <- transform(players, DOB = as.Date(as.character(DOB), "%Y%m%d"))
players$year<-year(players$DOB)
players$month<-month(players$DOB)
players$day<-day(players$DOB)

# some exploratory analysis on playing hand
levels(players$hand)
table(players$hand)

# add a new level for empty "E"
levels(players$hand) <- c("E","A","L","R","U")
table(players$hand)

ggplot(players)+
  geom_bar(map = aes(players$hand))

#subset and chart on L and R handed players
players %>% 
  subset(players$hand == "L" | players$hand == "R") %>%
  ggplot()+
  geom_bar(map = aes(hand))

# exploratory analysis on year
sum(players$year)
table(players$year)

ggplot(players)+
  geom_bar(mapping = aes(players$year))

# analysis on country
summary(players$country)

ggplot(players)+
  geom_bar(mapping = aes(players$country))

# top 10 countries by count
topCountries = c('USA','ESP','AUS','GER','GBR','ITA','FRA','BRA','ARG','MEX')

# subset and graph by collection
players %>%
  subset(country %in% topCountries) %>%
  ggplot() +
  geom_bar(map = aes(country))

pftC = filter(players, country %in% topCountries)
ggplot(pftC)+
  geom_bar(map = aes(pftC$country))

    