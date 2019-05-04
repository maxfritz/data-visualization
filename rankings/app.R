#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shiny)
library(tidyverse)
library('lubridate')

headersRankingsATP = c("date",'rank','player_id','points')
headersRankingsWTA = c("date",'rank','player_id','points',"drop")
headersID = c("id",'first','last','hand','DOB','country')

APPatp.rankings00s <- read.csv(url("https://raw.githubusercontent.com/maxfritz/tennis_atp/master/atp_rankings_00s.csv"),header=FALSE, col.names=headersRankingsATP)
APPatp.rankings10s <- read.csv(url("https://raw.githubusercontent.com/maxfritz/tennis_atp/master/atp_rankings_10s.csv"),header=FALSE, col.names=headersRankingsATP)
APPatp.rankings90s <- read.csv(url("https://raw.githubusercontent.com/maxfritz/tennis_atp/master/atp_rankings_90s.csv"),header=FALSE, col.names=headersRankingsATP)
APPatp.rankings80s <- read.csv(url("https://raw.githubusercontent.com/maxfritz/tennis_atp/master/atp_rankings_80s.csv"),header=FALSE, col.names=headersRankingsATP)
APPatp.rankings70s <- read.csv(url("https://raw.githubusercontent.com/maxfritz/tennis_atp/master/atp_rankings_70s.csv"),header=FALSE, col.names=headersRankingsATP)
APPatp.rankingsCurr <- read.csv(url("https://raw.githubusercontent.com/maxfritz/tennis_atp/master/atp_rankings_current.csv"),header=FALSE, col.names=headersRankingsATP)

APPwta.rankings00s <- read.csv(url("https://raw.githubusercontent.com/maxfritz/tennis_wta/master/wta_rankings_00s.csv"),header=FALSE, col.names=headersRankingsWTA)
APPwta.rankings10s <- read.csv(url("https://raw.githubusercontent.com/maxfritz/tennis_wta/master/wta_rankings_10s.csv"),header=FALSE, col.names=headersRankingsWTA)
APPwta.rankings90s <- read.csv(url("https://raw.githubusercontent.com/maxfritz/tennis_wta/master/wta_rankings_90s.csv"),header=FALSE, col.names=headersRankingsWTA)
APPwta.rankings80s <- read.csv(url("https://raw.githubusercontent.com/maxfritz/tennis_wta/master/wta_rankings_80s.csv"),header=FALSE, col.names=headersRankingsWTA)
APPwta.rankingsCurr <- read.csv(url("https://raw.githubusercontent.com/maxfritz/tennis_wta/master/wta_rankings_current.csv"),header=FALSE, col.names=headersRankingsWTA)

APPatp.players <- read.csv(url("https://raw.githubusercontent.com/maxfritz/tennis_atp/master/atp_players.csv"),header=FALSE, col.names=headersID)
APPwta.players <- read.csv(url("https://raw.githubusercontent.com/maxfritz/tennis_wta/master/wta_players.csv"),header=FALSE, col.names=headersID)

APPatp.players <- transform(APPatp.players, DOB = as.Date(as.character(DOB), "%Y%m%d"))
APPwta.players <- transform(APPwta.players, DOB = as.Date(as.character(DOB), "%Y%m%d"))

APPatp.rankings10s <- APPatp.rankings10s %>%
    filter(points!=">")
APPatp.rankings10s$points <- as.integer(APPatp.rankings10s$points)
APPatp.rankings00s$points <- as.integer(APPatp.rankings00s$points)

APPatpfull <- rbind(APPatp.rankings70s,APPatp.rankings80s,APPatp.rankings90s,APPatp.rankings00s,APPatp.rankings10s,APPatp.rankingsCurr)
APPwtafull <- rbind(APPwta.rankings80s,APPwta.rankings90s,APPwta.rankings00s,APPwta.rankings10s,APPwta.rankingsCurr)

APPatpfull$points <- as.integer(APPatpfull$points)
APPwtafull$points <- as.integer(APPwtafull$points)
APPatpfull$player_id <- as.factor(APPatpfull$player_id)
APPwtafull$player_id <- as.factor(APPwtafull$player_id)

APPwtafull <- APPwtafull %>% select(-drop)

APPwtafull$date <- ymd(APPwtafull$date)
APPatpfull$date <- ymd(APPatpfull$date)

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel("Rankings"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            # Input: Select the random distribution type ----
            radioButtons("source", "Data Source",
                         c("ATP" = "ATP",
                           "WTA" = "WTA",
                           "ITF" = "ITF")),
            
            # br() element to introduce extra vertical spacing ----
            
            br(),
            numericInput("rank_sel",
                        "Filter <= Rank",
                        min = 0,
                        max = 1000,
                        value = 10)
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            tabsetPanel(type = "tabs",
                        tabPanel("Data", dataTableOutput("rank_table")),
                        tabPanel("Summary", textOutput("summary"))
                        
            )
        )
    ))

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$rank_table <- renderDataTable({
        if(input$source == "ATP"){
            data <- APPatpfull %>%
                filter(rank<=input$rank_sel) %>%
                merge(APPatp.players,by.x = "player_id", by.y = "id")%>%
                select(1,2,5,6,3,4)
        } else if (input$source =="WTA"){
            data <- APPwtafull %>%
                filter(rank<=input$rank_sel) %>%
                merge(APPwta.players,by.x = "player_id", by.y = "id")%>%
                select(1,2,5,6,3,4)
        }
    })
    
    
    output$summary <- renderText({
        paste("From data source:", input$source)
        
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
