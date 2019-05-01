library(lubridate)
library(tidyverse)
library(plyr)
library(svglite)
library(countrycode)
library('viridis')
#----------------------------------------------------------------------------
#----------------------------------------------------------------------------
# data import
#----------------------------------------------------------------------------
#----------------------------------------------------------------------------

headersID = c("id",'first','last','hand','DOB','country')
headersRankingsATP = c("date",'rank','player_id','points')
headersRankingsWTA = c("date",'rank','player_id','points',"drop")

# import data 
atp.players <- read.csv(url("https://raw.githubusercontent.com/maxfritz/tennis_atp/master/atp_players.csv"),header=FALSE, col.names=headersID)
wta.players <- read.csv(url("https://raw.githubusercontent.com/maxfritz/tennis_wta/master/wta_players.csv"),header=FALSE, col.names=headersID)

atp.rankings00s <- read.csv(url("https://raw.githubusercontent.com/maxfritz/tennis_atp/master/atp_rankings_00s.csv"),header=FALSE, col.names=headersRankingsATP)
atp.rankings10s <- read.csv(url("https://raw.githubusercontent.com/maxfritz/tennis_atp/master/atp_rankings_10s.csv"),header=FALSE, col.names=headersRankingsATP)
atp.rankings90s <- read.csv(url("https://raw.githubusercontent.com/maxfritz/tennis_atp/master/atp_rankings_90s.csv"),header=FALSE, col.names=headersRankingsATP)
atp.rankings80s <- read.csv(url("https://raw.githubusercontent.com/maxfritz/tennis_atp/master/atp_rankings_80s.csv"),header=FALSE, col.names=headersRankingsATP)
atp.rankings70s <- read.csv(url("https://raw.githubusercontent.com/maxfritz/tennis_atp/master/atp_rankings_70s.csv"),header=FALSE, col.names=headersRankingsATP)
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

atp.rankings10s <- atp.rankings10s %>%
  filter(points!=">")
atp.rankings10s$points <- as.integer(atp.rankings10s$points)
atp.rankings00s$points <- as.integer(atp.rankings00s$points)

#full joined data
atpfull <- bind_rows(atp.rankings70s,atp.rankings80s,atp.rankings90s,atp.rankings00s,atp.rankings10s,atp.rankingsCurr)
wtafull <- bind_rows(wta.rankings80s,wta.rankings90s,wta.rankings00s,wta.rankings10s,wta.rankingsCurr)

atpfull$points <- as.integer(atpfull$points)
wtafull$points <- as.integer(wtafull$points)
atpfull$player_id <- as.factor(atpfull$player_id)
wtafull$player_id <- as.factor(wtafull$player_id)
wtafull <- wtafull %>% select(-drop)

wtafull$date <- ymd(wtafull$date)
atpfull$date <- ymd(atpfull$date)

atp.players <- transform(atp.players, DOB = as.Date(as.character(DOB), "%Y%m%d"))
wta.players <- transform(wta.players, DOB = as.Date(as.character(DOB), "%Y%m%d"))
atp.players$year<-year(atp.players$DOB)
wta.players$year<-year(wta.players$DOB)
atp.players$month<-month(atp.players$DOB)
wta.players$month<-month(wta.players$DOB)
atp.players$day<-day(atp.players$DOB)
wta.players$day<-day(wta.players$DOB)

atpfull %>%
  filter(year(date)>=1983)%>%
  filter(rank<=1) %>%
  group_by(year(date))%>%
  distinct(player_id)%>%
  tally()%>%
  ggplot()+
  geom_col(mapping=aes(x=`year(date)`,y=n))

atp.players$YDAY <- yday(atp.players$DOB)
atp.players$WDAY <- wday(atp.players$DOB)
atp.players$WDAY <- as.factor(atp.players$WDAY)
atp.players$WDAY <- revalue(atp.players$WDAY, c(
  '1'='Sunday',
  '2'='Monday',
  '3'='Tuesday',
  '4'='Wednesday',
  '5'='Thursday',
  '6'='Friday',
  '7'='Saturday'
))

atp.players$year <- as.factor(atp.players$year)
atp.players$month <- as.factor(atp.players$month)
atp.players$day <- as.factor(atp.players$day)
atp.players$month <- revalue(atp.players$month,c(
             "1"="January",
             '2'="February",
             '3'="March",
             '4'="April",
             '5'="May",
             '6'="June",
             '7'="July",
             '8'="August",
             '9'="September",
             '10'="October",
             '11'="November",
             '12'="December")
)

####################################################################################
# MONTH
####################################################################################

atpfull %>%
  #filter(year(date)>=1983)%>%
  filter(rank<=1) %>%
  merge(atp.players,by.x = "player_id", by.y = "id")%>%
  filter(!is.na(month))%>%
  group_by(month)%>%
  distinct(player_id)%>%
  tally()%>%
  arrange(-n)%>%
  ggplot()+
  geom_col(mapping=aes(x=month,y=n),width=0.5,fill='cadetblue3')+
  # theme_bw()+
  theme(panel.background = element_rect(fill = 'white'))+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_text(margin=margin(-25,0,10,0),family="Palatino",color='white',face='bold',angle=90,size=3,hjust=0,vjust=0.5),
        axis.ticks = element_blank(),
  )
#scale_x_discrete(labels = c("January","February","March","April","May","June","July","August","September","October","November","December"))
ggsave("MONTH_1.png", device="png",width=2, height=2)

atpfull %>%
  #filter(year(date)>=1983)%>%
  filter(rank<=10) %>%
  merge(atp.players,by.x = "player_id", by.y = "id")%>%
  filter(!is.na(month))%>%
  group_by(month)%>%
  distinct(player_id)%>%
  tally()%>%
  arrange(-n)%>%
  ggplot()+
  geom_col(mapping=aes(x=month,y=n),width=0.5,fill='cadetblue3')+
  # theme_bw()+
  theme(panel.background = element_rect(fill = 'white'))+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_text(margin=margin(-25,0,10,0),family="Palatino",color='white',face='bold',angle=90,size=3,hjust=0,vjust=0.5),
        axis.ticks = element_blank(),
  )
#scale_x_discrete(labels = c("January","February","March","April","May","June","July","August","September","October","November","December"))
ggsave("MONTH_10.png", device="png",width=2, height=2)

####################################################################################
# DAY
####################################################################################

atpfull %>%
  #filter(year(date)>=1983)%>%
  filter(rank<=1) %>%
  merge(atp.players,by.x = "player_id", by.y = "id")%>%
  filter(!is.na(day))%>%
  group_by(day)%>%
  distinct(player_id)%>%
  tally()%>%
  arrange(-n)%>%
  ggplot()+
  geom_col(mapping=aes(x=as.integer(day),y=n),width=0.5,fill='cadetblue3')+
  # theme_bw()+
  theme(panel.background = element_rect(fill = 'white'))+
  xlim(0,31)+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        #axis.text.x = element_text(margin=margin(-25,0,10,0),family="Palatino",color='white',face='bold',angle=90,size=2,hjust=0,vjust=0.5),
        axis.ticks = element_blank(),
        )
  #scale_x_discrete(labels = c("January","February","March","April","May","June","July","August","September","October","November","December"))
ggsave("DAY_1.png", device="png",width=2, height=2)

atpfull %>%
  #filter(year(date)>=1983)%>%
  filter(rank<=10) %>%
  merge(atp.players,by.x = "player_id", by.y = "id")%>%
  filter(!is.na(day))%>%
  group_by(day)%>%
  distinct(player_id)%>%
  tally()%>%
  arrange(-n)%>%
  ggplot()+
  geom_col(mapping=aes(x=day,y=n),width=0.55,fill='cadetblue3')+
  # theme_bw()+
  theme(panel.background = element_rect(fill = 'white'))+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_text(margin=margin(-15,0,10,0),family="Palatino",color='white',face='bold',angle=90,size=2,hjust=0,vjust=0.5),
        axis.ticks = element_blank(),
  )
#scale_x_discrete(labels = c("January","February","March","April","May","June","July","August","September","October","November","December"))
ggsave("DAY_10.png", device="png",width=2, height=2)


####################################################################################
# WEEKDAY
####################################################################################

atpfull %>%
  #filter(year(date)>=1983)%>%
  filter(rank<=1) %>%
  merge(atp.players,by.x = "player_id", by.y = "id")%>%
  filter(!is.na(WDAY))%>%
  group_by(WDAY)%>%
  distinct(player_id)%>%
  tally()%>%
  arrange(-n)%>%
  ggplot()+
  geom_col(mapping=aes(x=WDAY,y=n),width=0.5,fill='cadetblue3')+
  # theme_bw()+
  theme(panel.background = element_rect(fill = 'white'))+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_text(margin=margin(-28,0,10,0),family="Palatino",color='white',face='bold',angle=90,size=3,hjust=0,vjust=0.5),
        axis.ticks = element_blank(),
  )
#scale_x_discrete(labels = c("January","February","March","April","May","June","July","August","September","October","November","December"))
ggsave("WDAY_1.png", device="png",width=2, height=2)

atpfull %>%
  #filter(year(date)>=1983)%>%
  filter(rank<=10) %>%
  merge(atp.players,by.x = "player_id", by.y = "id")%>%
  filter(!is.na(WDAY))%>%
  group_by(WDAY)%>%
  distinct(player_id)%>%
  tally()%>%
  arrange(-n)%>%
  ggplot()+
  geom_col(mapping=aes(x=WDAY,y=n),width=0.5,fill='cadetblue3')+
  # theme_bw()+
  theme(panel.background = element_rect(fill = 'white'))+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_text(margin=margin(-28,0,10,0),family="Palatino",color='white',face='bold',angle=90,size=3,hjust=0,vjust=0.5),
        axis.ticks = element_blank(),
  )
#scale_x_discrete(labels = c("January","February","March","April","May","June","July","August","September","October","November","December"))
ggsave("WDAY10_1.png", device="png",width=2, height=2)

####################################################################################
#OTHER STUFF
####################################################################################

atpfull %>%
  #filter(year(date)>=1983)%>%
  filter(rank<=10) %>%
  merge(atp.players,by.x = "player_id", by.y = "id")%>%
  filter(!is.na(year))%>%
  group_by(year)%>%
  distinct(player_id)%>%
  tally()%>%
  arrange(-n)%>%
  ggplot()+
  geom_col(mapping=aes(x=year,y=n),width=0.5,fill='cadetblue3')+
  # theme_bw()+
  theme(panel.background = element_rect(fill = 'white'))+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_text(margin=margin(-25,0,10,0),family="Palatino",color='white',face='bold',angle=90,size=3,hjust=0,vjust=0.5),
        axis.ticks = element_blank(),
  )
#scale_x_discrete(labels = c("January","February","March","April","May","June","July","August","September","October","November","December"))
ggsave("YEAR_10.png", device="png",width=2, height=2)


atpfull %>%
  #filter(year(date)>=1983)%>%
  filter(rank<=50) %>%
  merge(atp.players,by.x = "player_id", by.y = "id")%>%
  filter(!is.na(day))%>%
  group_by(day)%>%
  distinct(player_id)%>%
  tally()%>%
  arrange(-n)%>%
  ggplot()+
  geom_col(mapping=aes(x=day,y=n),width=0.5)+
  theme_economist_white()
ggsave(file="test.svg", plot=image, width=1, height=1)

atpfull %>%
  #filter(year(date)>=1983)%>%
  filter(rank<=50) %>%
  merge(atp.players,by.x = "player_id", by.y = "id")%>%
  filter(!is.na(day))%>%
  group_by(day)%>%
  distinct(player_id)%>%
  tally()%>%
  arrange(-n)%>%
  ggplot()+
  geom_col(mapping=aes(x=day,y=n),width=0.05)+
  geom_point(mapping=aes(x=day, y=n))%>%
ggsave(file="test.svg", plot=image, width=10, height=8)

##########################################################################################
# Chloropleth
##########################################################################################

dfCODES <- data.frame(codelist$iso3c)%>%
  filter(!is.na(codelist$iso3c))
dfCODES$codelist.iso3c <- as.factor(dfCODES$codelist.iso3c)


atp1983 <- atpfull %>%
  filter(year(date)==1983)%>%
  filter(rank<=1000) %>%
  merge(atp.players,by.x = "player_id", by.y = "id")%>%
  filter(!is.na(country))%>%
  group_by(country)%>%
  distinct(player_id)%>%
  tally()%>%
  arrange(-n)%>%
  merge(dfCODES,by.x="country",by.y="codelist.iso3c",all.y = TRUE)%>%
  filter(!is.na(country))

atp1988 <- atpfull %>%
  filter(year(date)==1988)%>%
  filter(rank<=1000) %>%
  merge(atp.players,by.x = "player_id", by.y = "id")%>%
  filter(!is.na(country))%>%
  group_by(country)%>%
  distinct(player_id)%>%
  tally()%>%
  arrange(-n)%>%
  merge(dfCODES,by.x="country",by.y="codelist.iso3c",all.y = TRUE)%>%
  filter(!is.na(country))

atp1993 <- atpfull %>%
  filter(year(date)==1993)%>%
  filter(rank<=1000) %>%
  merge(atp.players,by.x = "player_id", by.y = "id")%>%
  filter(!is.na(country))%>%
  group_by(country)%>%
  distinct(player_id)%>%
  tally()%>%
  arrange(-n)%>%
  merge(dfCODES,by.x="country",by.y="codelist.iso3c",all.y = TRUE)%>%
  filter(!is.na(country))

atp1998 <- atpfull %>%
  filter(year(date)==1998)%>%
  filter(rank<=1000) %>%
  merge(atp.players,by.x = "player_id", by.y = "id")%>%
  filter(!is.na(country))%>%
  group_by(country)%>%
  distinct(player_id)%>%
  tally()%>%
  arrange(-n)%>%
  merge(dfCODES,by.x="country",by.y="codelist.iso3c",all.y = TRUE)%>%
  filter(!is.na(country))

atp2003 <- atpfull %>%
  filter(year(date)==2003)%>%
  filter(rank<=1000) %>%
  merge(atp.players,by.x = "player_id", by.y = "id")%>%
  filter(!is.na(country))%>%
  group_by(country)%>%
  distinct(player_id)%>%
  tally()%>%
  arrange(-n)%>%
  merge(dfCODES,by.x="country",by.y="codelist.iso3c",all.y = TRUE)%>%
  filter(!is.na(country))

atp2008 <- atpfull %>%
  filter(year(date)==2008)%>%
  filter(rank<=1000) %>%
  merge(atp.players,by.x = "player_id", by.y = "id")%>%
  filter(!is.na(country))%>%
  group_by(country)%>%
  distinct(player_id)%>%
  tally()%>%
  arrange(-n)%>%
  merge(dfCODES,by.x="country",by.y="codelist.iso3c",all.y = TRUE)%>%
  filter(!is.na(country))

atp2013 <- atpfull %>%
  filter(year(date)==2013)%>%
  filter(rank<=1000) %>%
  merge(atp.players,by.x = "player_id", by.y = "id")%>%
  filter(!is.na(country))%>%
  group_by(country)%>%
  distinct(player_id)%>%
  tally()%>%
  arrange(-n)%>%
  merge(dfCODES,by.x="country",by.y="codelist.iso3c",all.y = TRUE)%>%
  filter(!is.na(country))

atp2018 <- atpfull %>%
  filter(year(date)==2018)%>%
  filter(rank<=1000) %>%
  merge(atp.players,by.x = "player_id", by.y = "id")%>%
  filter(!is.na(country))%>%
  group_by(country)%>%
  distinct(player_id)%>%
  tally()%>%
  arrange(-n)%>%
  merge(dfCODES,by.x="country",by.y="codelist.iso3c",all.y = TRUE)%>%
  filter(!is.na(country))

# COLOR SCALING AND NA=0
atp2018[is.na(atp2018)] <- 0
atp2013[is.na(atp2013)] <- 0
atp2008[is.na(atp2008)] <- 0
atp2003[is.na(atp2003)] <- 0
atp1998[is.na(atp1998)] <- 0
atp1993[is.na(atp1993)] <- 0
atp1988[is.na(atp1988)] <- 0
atp1983[is.na(atp1983)] <- 0
atp2018$colorscale <- log10(atp2018$n+2)
atp2013$colorscale <- log10(atp2013$n+2)
atp2008$colorscale <- log10(atp2008$n+2)
atp2003$colorscale <- log10(atp2003$n+2)
atp1998$colorscale <- log10(atp1998$n+2)
atp1993$colorscale <- log10(atp1993$n+2)
atp1988$colorscale <- log10(atp1988$n+2)
atp1983$colorscale <- log10(atp1983$n+2)

# light grey boundaries
l <- list(color = toRGB("grey"), width = 0.5)

# specify map projection/options
g <- list(
  showframe = FALSE,
  showcoastlines = FALSE,
  projection = list(type = 'Mercator')
)

plot_geo(atp1988) %>%
  add_trace(
    z = ~(log10(n+1)), color = ~colorscale, colors='Blues',
    text=~paste(country, '<br>Count:',n), 
    locations = ~country, marker = list(line = l), hoverinfo = "text", showscale=FALSE
  ) %>%
  colorbar(title = 'Player Count', tickprefix = ' ') %>%
  layout(
    showlegend = FALSE,
    geo = g
  )%>%
api_create(filename="ATPchloro1988")




