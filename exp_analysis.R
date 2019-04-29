install.packages(c('tidyverse','lubridate','ggthemes','plotly'))
install.packages('gganimate')
install.packages('gifski')
install.packages('png')
install.packages('gghighlight')
install.packages('gapminder')
library(png)
library(gapminder)
library(gifski)
library(gganimate)
library(gghighlight)
library('plotly')
library('ggthemes')
library('tidyverse')
library('lubridate')
source("queries.R")

#----------------------------------------------------------------------------
#----------------------------------------------------------------------------
# data import
#----------------------------------------------------------------------------
#----------------------------------------------------------------------------

headersID = c("id",'first','last','hand','DOB','country')
headersRankingsATP = c("date",'rank','player_id','points')
headersRankingsWTA = c("date",'rank','player_id','points',"drop")

# import data files with headers passed
atp.players <- read.csv(url("https://raw.githubusercontent.com/maxfritz/tennis_atp/master/atp_players.csv"),header=FALSE, col.names=headersID)
wta.players <- read.csv(url("https://raw.githubusercontent.com/maxfritz/tennis_wta/master/wta_players.csv"),header=FALSE, col.names=headersID)
atp.rankings00s <- read.csv(url("https://raw.githubusercontent.com/maxfritz/tennis_atp/master/atp_rankings_00s.csv"),header=FALSE, col.names=headersRankingsATP)
atp.rankings10s <- read.csv(url("https://raw.githubusercontent.com/maxfritz/tennis_atp/master/atp_rankings_10s.csv"),header=FALSE, col.names=headersRankingsATP)
atp.rankings90s <- read.csv(url("https://raw.githubusercontent.com/maxfritz/tennis_atp/master/atp_rankings_90s.csv"),header=FALSE, col.names=headersRankingsATP)
atp.rankings80s <- read.csv(url("https://raw.githubusercontent.com/maxfritz/tennis_atp/master/atp_rankings_80s.csv"),header=FALSE, col.names=headersRankingsATP)
atp.rankingsCurr <- read.csv(url("https://raw.githubusercontent.com/maxfritz/tennis_atp/master/atp_rankings_current.csv"),header=FALSE, col.names=headersRankingsATP)
wta.rankings00s <- read.csv(url("https://raw.githubusercontent.com/maxfritz/tennis_wta/master/wta_rankings_00s.csv"),header=FALSE, col.names=headersRankingsWTA)
wta.rankings10s <- read.csv(url("https://raw.githubusercontent.com/maxfritz/tennis_wta/master/wta_rankings_10s.csv"),header=FALSE, col.names=headersRankingsWTA)
wta.rankings90s <- read.csv(url("https://raw.githubusercontent.com/maxfritz/tennis_wta/master/wta_rankings_90s.csv"),header=FALSE, col.names=headersRankingsWTA)
wta.rankings80s <- read.csv(url("https://raw.githubusercontent.com/maxfritz/tennis_wta/master/wta_rankings_80s.csv"),header=FALSE, col.names=headersRankingsWTA)
wta.rankingsCurr <- read.csv(url("https://raw.githubusercontent.com/maxfritz/tennis_wta/master/wta_rankings_current.csv"),header=FALSE, col.names=headersRankingsWTA)

#----------------------------------------------------------------------------
#----------------------------------------------------------------------------
# data cleaning
#----------------------------------------------------------------------------
#----------------------------------------------------------------------------

wta.rankings00s<-wta.rankings00s %>% select(-drop)
wta.rankings80s<-wta.rankings80s %>% select(-drop)
wta.rankings90s<-wta.rankings90s %>% select(-drop)
wta.rankings10s<-wta.rankings10s %>% select(-drop)
wta.rankingsCurr <- wta.rankingsCurr %>% select(-drop)

wta.rankings00s <- transform(wta.rankings00s, date = as.Date(as.character(date), "%Y%m%d"))
wta.rankings80s <- transform(wta.rankings80s, date = as.Date(as.character(date), "%Y%m%d"))
wta.rankings90s <- transform(wta.rankings90s, date = as.Date(as.character(date), "%Y%m%d"))
wta.rankings10s <- transform(wta.rankings10s, date = as.Date(as.character(date), "%Y%m%d"))
wta.rankingsCurr <- transform(wta.rankingsCurr, date = as.Date(as.character(date), "%Y%m%d"))

atp.rankings00s <- transform(atp.rankings00s, date = as.Date(as.character(date), "%Y%m%d"))
atp.rankings80s <- transform(atp.rankings80s, date = as.Date(as.character(date), "%Y%m%d"))
atp.rankings90s <- transform(atp.rankings90s, date = as.Date(as.character(date), "%Y%m%d"))
atp.rankings10s <- transform(atp.rankings10s, date = as.Date(as.character(date), "%Y%m%d"))
atp.rankingsCurr <- transform(atp.rankingsCurr, date = as.Date(as.character(date), "%Y%m%d"))

# transform the DOB into date; pull out year
atp.players <- transform(atp.players, DOB = as.Date(as.character(DOB), "%Y%m%d"))
wta.players <- transform(wta.players, DOB = as.Date(as.character(DOB), "%Y%m%d"))
atp.players$year<-year(atp.players$DOB)
wta.players$year<-year(wta.players$DOB)
atp.players$month<-month(atp.players$DOB)
wta.players$month<-month(wta.players$DOB)
atp.players$day<-day(atp.players$DOB)
wta.players$day<-day(wta.players$DOB)

atp.rankings00s$points <- as.integer(atp.rankings00s$points)
atp.rankings90s$points <- as.integer(atp.rankings90s$points)
atp.rankings80s$points <- as.integer(atp.rankings80s$points)
atp.rankings10s$points <- as.integer(atp.rankings10s$points)
atp.rankingsCurr$points <- as.integer(atp.rankingsCurr$points)

wta.rankings00s$points <- as.integer(wta.rankings00s$points)
wta.rankings90s$points <- as.integer(wta.rankings90s$points)
wta.rankings80s$points <- as.integer(wta.rankings80s$points)
wta.rankings10s$points <- as.integer(wta.rankings10s$points)
wta.rankingsCurr$points <- as.integer(wta.rankingsCurr$points)

#----------------------------------------------------------------------------
#----------------------------------------------------------------------------
# exp data analysis
#----------------------------------------------------------------------------
#----------------------------------------------------------------------------

# take a peek
atp.players %>% glimpse()
atp.rankings00s %>% glimpse()

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
sum(atp.players$year)
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

rankings_full_info()

#----------------------------------------------------------------------------
#----------------------------------------------------------------------------
# data munging
#----------------------------------------------------------------------------
#----------------------------------------------------------------------------

#full joined data
atpfull <- bind_rows(atp.rankings80s,atp.rankings90s,atp.rankings00s,atp.rankings10s,atp.rankingsCurr)
wtafull <- bind_rows(wta.rankings80s,wta.rankings90s,wta.rankings00s,wta.rankings10s,wta.rankingsCurr)

#top 10 data
atptop10 <- bind_rows(atp.rankings80s,atp.rankings90s,atp.rankings00s,atp.rankings10s,atp.rankingsCurr) %>%
  filter(rank<=10)

wtatop10 <- bind_rows(wta.rankings80s,wta.rankings90s,wta.rankings00s,wta.rankings10s,wta.rankingsCurr) %>%
  filter(rank<=10)

#number one data
ATPno1s <- atptop10 %>%
  filter(rank==1)%>%
  distinct(player_id)%>%
  merge(atp.players,by.x = "player_id", by.y = "id")

WTAno1s <- wtatop10 %>%
  filter(rank==1)%>%
  distinct(player_id)%>%
  merge(wta.players,by.x = "player_id", by.y = "id")

#step graph data - grab all ranking entries for any player ever ranked no.1
stepATP <- atpfull %>%
  filter(player_id %in% ATPno1s$player_id)%>%
  merge(atp.players,by.x = "player_id", by.y = "id")

stepWTA <- wtafull %>%
  filter(player_id %in% WTAno1s$player_id)%>%
  merge(wta.players,by.x = "player_id", by.y = "id")

stepATP$player_id <- factor(stepATP$player_id)
stepATP$first <- factor(stepATP$first)
stepATP$last <- factor(stepATP$last)

stepWTA$player_id <- factor(stepWTA$player_id)
stepWTA$first <- factor(stepWTA$first)
stepWTA$last <- factor(stepWTA$last)

#--------------------------------------------
## number one progress-rankings chart
#--------------------------------------------

stepATP$last = as.character(stepATP$last)
stepATP$first = as.character(stepATP$first)
stepATP$name <- paste(stepATP$first,stepATP$last,sep=" ")
stepWTA$last = as.character(stepWTA$last)
stepWTA$first = as.character(stepWTA$first)
stepWTA$name <- paste(stepWTA$first,stepWTA$last,sep=" ")

stepATP %>% 
  group_by(name)%>%
  plot_ly(x=~date)%>%
  add_lines(y=~rank,line=list(shape="hv",
                              width=3),
            hoverinfo='text',
            color=~factor(name),
            text=~paste(name,
                        '<br>Rank:',
                        rank,
                        '<br>Date:',date))%>%
  layout(yaxis=list(range=c(10,1),showgrid=TRUE),showlegend=TRUE)%>%
  layout(
    title = "Ranking History of World No.1s - ATP",
    xaxis = list(
      showgrid=FALSE,
      rangeselector = list(
        buttons = list(
          list(
            count = 10,
            label = "10 years",
            step = "year",
            stepmode = "backward"),
          list(
            count = 5,
            label = "5 years",
            step = "year",
            stepmode = "backward"),
          list(step = "all"))),
      rangeslider = list(type = "date")),
    yaxis = list(title="Rank"))

stepWTA %>% 
  group_by(name)%>%
  plot_ly(x=~date)%>%
  add_lines(y=~rank,line=list(shape="hv",
                              width=3),
            hoverinfo='text',
            color=~factor(name),
            text=~paste(name,
                        '<br>Rank:',
                        rank,
                        '<br>Date:',date))%>%
  layout(yaxis=list(range=c(10,1),showgrid=TRUE),showlegend=TRUE)%>%
  layout(
    title = "Ranking History of World No.1s - WTA",
    xaxis = list(
      showgrid=FALSE,
      rangeselector = list(
        buttons = list(
          list(
            count = 10,
            label = "10 years",
            step = "year",
            stepmode = "backward"),
          list(
            count = 5,
            label = "5 years",
            step = "year",
            stepmode = "backward"),
          list(step = "all"))),
      rangeslider = list(type = "date")),
    yaxis = list(title="Rank"))


q <- ggplot(data=stepATP,mapping=aes(x=stepATP$date,y=stepATP$rank,color=stepATP$player_id,group=stepATP$player_id))+
  ylim(100,0)+
  #geom_point()+
  geom_line()
ggplotly(q,tooltip = c("x", "y", "colour"))%>%
  highlight("plotly_hover")

g <- ggplot(mapping=aes(color=player_id))+
  xlab("Off-axis distance (mm)") +
  ylab("Rank") +
  ylim(50,0)+
  geom_step(data=(stepATP %>% filter(player_id==100656)),mapping=aes(x=date, y=rank))+
  geom_step(data=(stepATP %>% filter(player_id==101222)),mapping=aes(x=date, y=rank))+
  geom_step(data=(stepATP %>% filter(player_id==101404)),mapping=aes(x=date, y=rank))+
  geom_step(data=(stepATP %>% filter(player_id==101414)),mapping=aes(x=date, y=rank))+
  geom_step(data=(stepATP %>% filter(player_id==101736)),mapping=aes(x=date, y=rank))+
  geom_step(data=(stepATP %>% filter(player_id==101948)),mapping=aes(x=date, y=rank))+
  geom_step(data=(stepATP %>% filter(player_id==102158)),mapping=aes(x=date, y=rank))+
  geom_step(data=(stepATP %>% filter(player_id==102338)),mapping=aes(x=date, y=rank))+
  geom_step(data=(stepATP %>% filter(player_id==102701)),mapping=aes(x=date, y=rank))+
  geom_step(data=(stepATP %>% filter(player_id==102845)),mapping=aes(x=date, y=rank))+
  geom_step(data=(stepATP %>% filter(player_id==102856)),mapping=aes(x=date, y=rank))+
  geom_step(data=(stepATP %>% filter(player_id==103498)),mapping=aes(x=date, y=rank))+
  geom_step(data=(stepATP %>% filter(player_id==103507)),mapping=aes(x=date, y=rank))+
  geom_step(data=(stepATP %>% filter(player_id==103720)),mapping=aes(x=date, y=rank))+
  geom_step(data=(stepATP %>% filter(player_id==103819)),mapping=aes(x=date, y=rank))+
  geom_step(data=(stepATP %>% filter(player_id==104053)),mapping=aes(x=date, y=rank))+
  geom_step(data=(stepATP %>% filter(player_id==104745)),mapping=aes(x=date, y=rank))+
  geom_step(data=(stepATP %>% filter(player_id==104918)),mapping=aes(x=date, y=rank))+
  geom_step(data=(stepATP %>% filter(player_id==104925)),mapping=aes(x=date, y=rank))

g
ggplotly(g,group=player_id)
atptop10$player_id<-factor(atptop10$player_id)

#--------------------------------------------
## working
#--------------------------------------------

rankings_full_info(rank_high=10,rank_low=1)%>%
  ggplot()+
  geom_col(mapping=aes(x=reorder(rank,-rank), y=points,fill=country,group=player_id))+
  guides(fill=FALSE)+
  geom_text(mapping=aes(x=reorder(rank,-rank), y=points, label=points,hjust=-0.3))+
  coord_flip()+
  theme_classic()+
  transition_states(date, transition_length = 50, state_length = 80)+
  ease_aes('linear')+
  labs(title='{closest_state}', x = "", y = "Points") +
  theme(plot.title = element_text(hjust = 0, size = 22),
        axis.ticks.y = element_blank(),  # These relate to the axes post-flip
        axis.text.y  = element_blank(),  # These relate to the axes post-flip
        plot.margin = margin(1,1,1,4, "cm"))

rankings_fmt <- rankings_full_info(rank_high=10,rank_low=1) %>%
  group_by(date) %>%
  mutate(rank = rank(-points),
         Value_rel = points/points[rank==1],
         Value_lbl = paste0(" ",round(points/1e9))) %>%
  group_by(player_id) %>% 
  filter(rank <=10) %>%
  ungroup()


#--------------------------------------------
# TOP TEN RANKINGS GRAPHIC
#--------------------------------------------

rankings_test <-rankings_fmt[1:130,]

staticplot = ggplot(rankings_test, aes(rank, group = player_id, 
                                       fill = as.factor(player_id), color = as.factor(player_id))) +
  geom_tile(aes(y = points/2,
                height = points,
                width = 0.8), alpha = 0.8, color = NA) +
  geom_text(aes(y = 0, label = paste(last, " ")), vjust = 0.2, hjust = 1) +
  geom_text(aes(y=points,label = round(points,1), hjust=0)) +
  coord_flip(clip = "off", expand = FALSE) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_reverse() +
  guides(color = FALSE, fill = FALSE) +
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position="none",
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.grid.major.x = element_line( size=.1, color="grey" ),
        panel.grid.minor.x = element_line( size=.1, color="grey" ),
        plot.title=element_text(size=25, hjust=0.5, face="bold", colour="grey", vjust=-1),
        plot.subtitle=element_text(size=18, hjust=0.5, face="italic", color="grey"),
        plot.caption =element_text(size=8, hjust=0.5, face="italic", color="grey"),
        plot.background=element_blank(),
        plot.margin = margin(2,2, 2, 4, "cm"))

anim = staticplot + transition_states(date, transition_length = 4, state_length = 8) +
  view_follow(fixed_x = TRUE)  +
  labs(title = 'Date : {closest_state}',  
       subtitle  =  "Top 10 Players",
       caption  = "Points | Data Source: ATP World Tour")

animate(anim, 20, fps = 24,  width = 600, height = 500, duration=12, 
        renderer = gifski_renderer("gganim_test.gif"))
