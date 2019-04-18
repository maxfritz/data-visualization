install.packages('tidyverse','lubridate')
install.packages('lubridate')
install.packages("ggthemes")
library('ggthemes')
library('tidyverse')
library('lubridate')

source("queries.R")

# headers for csv files
headersID = c("id",'first','last','hand','DOB','country')
headersRankings = c("date",'rank','player_id','points')

# import data files with headers passed
atp.players <- read.csv(url("https://raw.githubusercontent.com/maxfritz/tennis_atp/master/atp_players.csv"),header=FALSE, col.names=headersID)
wta.players <- read.csv(url("https://raw.githubusercontent.com/maxfritz/tennis_wta/master/wta_players.csv"),header=FALSE, col.names=headersID)
atp.rankings00s <- read.csv(url("https://raw.githubusercontent.com/maxfritz/tennis_atp/master/atp_rankings_00s.csv"),header=FALSE, col.names=headersRankings)
atp.rankings10s <- read.csv(url("https://raw.githubusercontent.com/maxfritz/tennis_atp/master/atp_rankings_10s.csv"),header=FALSE, col.names=headersRankings)
atp.rankings90s <- read.csv(url("https://raw.githubusercontent.com/maxfritz/tennis_atp/master/atp_rankings_90s.csv"),header=FALSE, col.names=headersRankings)
atp.rankings80s <- read.csv(url("https://raw.githubusercontent.com/maxfritz/tennis_atp/master/atp_rankings_80s.csv"),header=FALSE, col.names=headersRankings)
wta.rankings00s <- read.csv(url("https://raw.githubusercontent.com/maxfritz/tennis_wta/master/wta_rankings_00s.csv"),header=FALSE, col.names=headersRankings)
wta.rankings10s <- read.csv(url("https://raw.githubusercontent.com/maxfritz/tennis_wta/master/wta_rankings_10s.csv"),header=FALSE, col.names=headersRankings)
wta.rankings90s <- read.csv(url("https://raw.githubusercontent.com/maxfritz/tennis_wta/master/wta_rankings_90s.csv"),header=FALSE, col.names=headersRankings)
wta.rankings80s <- read.csv(url("https://raw.githubusercontent.com/maxfritz/tennis_wta/master/wta_rankings_80s.csv"),header=FALSE, col.names=headersRankings)

# take a peek
atp.players %>% glimpse()
atp.rankings00s %>% glimpse()

# transform the DOB into date; pull out year
atp.players <- transform(atp.players, DOB = as.Date(as.character(DOB), "%Y%m%d"))
wta.players <- transform(wta.players, DOB = as.Date(as.character(DOB), "%Y%m%d"))
atp.players$year<-year(atp.players$DOB)
wta.players$year<-year(wta.players$DOB)
atp.players$month<-month(atp.players$DOB)
wta.players$month<-month(wta.players$DOB)
atp.players$day<-day(atp.players$DOB)
wta.players$day<-day(wta.players$DOB)

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
  geom_bar(map = aes(hand)) #probably junk, look at later

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

# subset and graph by collection
players %>%
  subset(country %in% topCountries)
  
#------------------------------------------

#       PLAYERS BY COUNTRY

#------------------------------------------


country_no_sel = 5;

wta_by_country <- wta.players %>% 
  group_by(country) %>% 
  summarise(n=n()) %>% 
  arrange(-n) %>%
  slice(c(1:country_no_sel)) %>%
  arrange(n)%>%
  mutate(country = factor(country, levels = country))%>%
  ggplot() +
  geom_bar(stat="identity",mapping = aes(x=country,y=n))+
  #theme_tufte()
  theme_economist() + 
  scale_color_economist()+
  ggtitle("Countries by ranked players (WTA all time)")

atp_by_country <- atp.players %>% 
  group_by(country) %>% 
  summarise(n=n()) %>% 
  arrange(-n) %>%
  slice(c(1:country_no_sel)) %>%
  arrange(n)%>%
  mutate(country = factor(country, levels = country))%>%
  ggplot() +
  geom_bar(stat="identity",mapping = aes(x=country,y=n))+
  #theme_tufte()
  theme_economist() + 
  scale_color_economist()+
  ggtitle("Countries by ranked players (ATP all time)")

#------------------------------------------

#       RANKINGS 

#------------------------------------------

dates00 <- rankings00s %>% 
  select(date) %>%
  unique()

date_selector = 1
date_sel = dates00[date_selector,]

rankings_full_info(date_sel) # returns top 1000 (default) from selected date
rankings_full_info(date_sel,1,10) # returns top 10 from selected date
my_examp <- rankings_full_info(date_sel,10,1) # also works fine

my_examp %>%
ggplot()+
  geom_col(mapping = aes(x=rank, y=points))
           

rankings_full_info(date_sel,10,1) # also works fine

