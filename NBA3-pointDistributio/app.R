####################################################

library(shiny)
library(shinydashboard)
library(readxl)
library(nlme)
library(tidyverse)
library(lubridate)
library(grid)
library(gridExtra)
library(plotly)
library(devtools)
library(stringr)
library(shinyWidgets)
library(dplyr)
library(readr)
library(readxl)
library(data.table)
library(tidyverse)
library(arm)
library(lme4)
library(corrplot)
library(RColorBrewer)
library(caTools)
library(Metrics)
library(caret)
library(car)
library(betareg)
library(emmeans)
library(benford.analysis)
library(ggpubr)


####################################################
# Read the data
player_data1<- read.csv("player_data.csv",sep=",") # data from kaggle, scraped from Basketball Reference
colnames(player_data1)[1] <- "Player"
player_data2<- read.csv("Players.csv",sep=",")  # data from kaggle,scraped from Basketball Reference
player_data<-inner_join(player_data2,player_data1,by="Player")%>%dplyr:: select(Player,born,college,birth_city,birth_state,year_start,year_end,position,height.y,weight.y,birth_date)
colnames(player_data)[9]<-"height"
colnames(player_data)[10]<-"weight"
Seasons_Stats<-read.csv("Seasons_Stats.csv",sep=",") # data from kaggle,scraped from Basketball Reference

EDA_data<-inner_join(player_data,Seasons_Stats,by="Player")

# Notation for some basketball terms: TM=Team, MP=Minutes Played, PER=Player Efficiency Rating, TS.=True Shooting %, OWS=Offensive Win Shares, DWS=Defensive Win Shares, WS=Win Share, FG=Field Goals, FGA=Field Goal Attempts, FG.=Field Goal Percentage, X3P=3-Point Field Goals, X3PA=3-Point Field Goal Attempts, X3P.=3-Point Field Goal Percentage, X2P=2-Point Field Goals, X2PA=2-Point Field Goal Attempts, X2P.=2-Point Field Goal Percentage, eFG.=Effective Field Goal Percentage
EDA_data_Player_2014_2018<-EDA_data%>%dplyr::select(Player,college,birth_date,Year,position,height,weight,Age,Tm,MP,PER,TS.,OWS,DWS,WS,FG,FGA,FG.,X3P,X3PA,X3P.,X2P,X2PA,X2P.,eFG.,TRB)%>%dplyr::filter(Year<2019 & Year>2013)

EDA_data_Team_2014_2018<-read.csv("NBATeamStats.csv",sep=",")%>% mutate(WinRate=WIN.*100)%>%mutate(X2PA=FGA-X3PA) # data from NBA official website
colnames(EDA_data_Team_2014_2018)[11] <- "X3PM"
colnames(EDA_data_Team_2014_2018)[27] <- "RPM"
################################################
  
 sidebar= dashboardSidebar(
    #Create the sidebar menu tabs
    sidebarMenu(
      menuItem("Abstract and Background", tabName = "about", icon = icon("info")),
      menuItem("Data Exploration", tabName = "data", icon = icon("database")),
      menuItem("3-point Evolution by Team", tabName = "Team", icon = icon("area-chart")),
      menuItem("3-point Evolution by Player", tabName = "Player", icon = icon("area-chart")),
      menuItem("Distribution Analysis", tabName = "Distribution")
    )
  )
  
  #Create the body
  body=dashboardBody(
   # background-image: url("header.png");
      tabItems(
      # About and Methodology Tab
      tabItem(tabName = "about",
              fluidRow(
                box(
                  title = "Abstract and Background", 
                  width = 12, 
                  solidHeader = TRUE, 
                  status = "primary",
                  collapsible  = TRUE,
                  h3("Abstract"),
                  print("In this project, I will analyze how 3 Point Field Goals Attempted and 3 Point Field Goals
                        Percentage will improve Win Rate for each NBA Team, and whether more 3 Point Field Goals 
                        Attempted and higher 3 points percentage will increase/decrease the win share for each NBA 
                        player in different positions. Also, I will test both Team and Player data whether they 
                        follow Benford's rule. If not, I will do the normality test.")
              )),
              
              fluidRow(
                box(
                  title = "Background", 
                  width = 12, 
                  solidHeader = TRUE, 
                  status = "primary",
                  collapsible = TRUE,
                  h3("Background"),
                  print("In 2014–15, Curry won the NBA Most Valuable Player Award and led the Warriors to their
                        first championship since 1975. The following season, he became the first player in NBA 
                        history to be elected MVP by a unanimous vote and to lead the league in scoring while shooting 
                        above 50–40–90. That same year, the Warriors broke the record for the most wins in an NBA 
                        season. We could not imagine how crazy Golden State Warriors and Curry's three-pointer were.
                        Curry nailed more 3s than everyone on the Bucks combined in 2016. Since the traditional basketball
                        philosophy is that the closer you get to the basket, the easier it is to score, Curry and his 
                        Golden State Warriors definitely created a new era that NBA teams began to shot more and more 
                        3-points instead of midrange Medium and long range 2-points jump shot." )
              )
                )),
      
      # Data Exploration Tab
      tabItem(tabName = "data",
              fluidRow(
                box(
                  title = "NBA Team/Player 3-points distribution", 
                  width = 12, 
                  solidHeader = TRUE, 
                  status = "primary",
                  collapsible  = F,
                  selectizeInput("data1",
                                 label = "NBA Data",
                                 choices = c("Team","Player"),
                                 multiple = F,
                                 selected = "Team"
                                 
                  ),
                  plotlyOutput("distribution")
                )),
                
    
              
              fluidRow(
                box(
                  title = "Data Summary", 
                  width = 12, 
                  solidHeader = TRUE, 
                  status = "primary",
                  collapsible  = TRUE,
                  numericInput("obs", "Number of observations to view:", 10),
                  tableOutput("view")
                ),
                box(
                  title = "Correlation Check", 
                  width = 12, 
                  solidHeader = TRUE, 
                  status = "primary",
                  collapsible = TRUE,
                  plotOutput("correlation")
                )
              )
      ),
      
      tabItem(tabName = "Team",
              fluidRow(
                box(
                  title = "NBA Team 3-points Revolution", 
                  width = 12, 
                  solidHeader = TRUE, 
                  status = "primary",
                  collapsible  = F,
                  selectizeInput("teamData",
                                 label = "NBA Team Data",
                                 choices = unique(EDA_data_Team_2014_2018$TEAM),
                                 multiple = F,
                                 # options = list(maxItems = 10, placeholder = "NBA Data"),
                                 selected = "Los Angeles Lakers"
                             
                  ),
                  plotOutput("teamTrend"),
                  plotOutput("teamTrend2")
                )
      )),
      
      tabItem(tabName = "Player",
              fluidRow(
                box(
                  title = "NBA Player 3-points Revolution By Position", 
                  width = 12, 
                  solidHeader = TRUE, 
                  status = "primary",
                  collapsible  = F,
                  selectizeInput("playerData",
                                 label = "NBA Player Position",
                                 choices = unique(EDA_data_Player_2014_2018$position),
                                 multiple = F,
                                 # options = list(maxItems = 10, placeholder = "NBA Data"),
                                 selected = "G"
                         
                  ),
                  plotOutput("playerTrend"),
                  plotOutput("playerTrend2")
                )
              )
      ),
      tabItem(tabName = "Distribution",
              fluidRow(
                box(
                  title = "Check Benford Or Other Distribution", 
                  width = 12, 
                  solidHeader = TRUE, 
                  status = "primary",
                  collapsible  = F,
                  selectizeInput("data44",
                                 label = "NBA Team Data",
                                 choices = c("Normal Distribution for Team","Benford Analysis for Player"),
                                 multiple = F,
                                 selected = "Benford Analysis for Player"
                                
                  ),
                  plotOutput("dist"),
                  tableOutput("view2"),
                  plotOutput("dist2"),
                  plotOutput("dist3")
                )
              )
      )
      )
      )
              
      
    
  
  shinyApp(
    ui = dashboardPage(
      dashboardHeader(title = "NBA 3-Points Evolution"),
      sidebar,
      body),
    server = shinyServer(function(input, output) { 
      output$distribution<- renderPlotly({
        if(input$data1=="Team"){
          ggplot(EDA_data_Team_2014_2018, aes(X3PA, fill = TEAM)) + 
            geom_histogram(binwidth = 5) + theme_light() +   
            xlab("number of 3 points attemped per game") + 
            ylab("Frequency of NBA Teams in 5 seasons ")+ 
            ggtitle("3 points attemped Distribution by Team in last 5 seasons") + 
            theme(axis.title.x = element_text(face="bold",  size=10), 
                  axis.title.y = element_text(face="bold",  size=12),
                  plot.title = element_text(size=14, face="bold"),  
                  axis.text.x  = element_text(vjust=0.5, size=10)) +
            theme(plot.title = element_text(hjust = 0.5))
        }else if(input$data1=="Player"){
          ggplot(EDA_data_Player_2014_2018, aes(X3P, fill = position)) + 
            geom_histogram(binwidth = 5) + theme_light() +   
            xlab("number of 3 points attemped per Season") + 
            ylab("Frequency of NBA Players by position in 5 seasons ")+ 
            ggtitle("3 points attemped Distribution by Players in last 5 seasons") + 
            theme(axis.title.x = element_text(face="bold",  size=10), 
                  axis.title.y = element_text(face="bold",  size=12),
                  plot.title = element_text(size=14, face="bold"),  
                  axis.text.x  = element_text(vjust=0.5, size=10)) +
            theme(plot.title = element_text(hjust = 0.5))
        }
      })
      output$view <- renderTable({
        if(input$data1=="Team"){head(EDA_data_Team_2014_2018, n = input$obs)}
        else if(input$data1=="Player"){head(EDA_data_Player_2014_2018, n = input$obs)}
      })
      
      output$correlation <- renderPlot({
        if(input$data1=="Team"){
        M <- cor(EDA_data_Team_2014_2018 %>% dplyr::select(X3PM,X3P.,RPM,Year,REB,X3PA))
                                       
                                       corrplot(M, type = "upper", order = "hclust",
                                                col = brewer.pal(n = 8, name = "RdBu"))}
        else if(input$data1=="Player"){
          M <- (cor(na.omit(EDA_data_Player_2014_2018 %>% dplyr::select(X3P,X3P.,WS,Year,TRB,X3PA))))
        
        corrplot(M, type = "upper", order = "hclust",
                 col = brewer.pal(n = 8, name = "RdBu"))}
                                       
                                       })
    
    
    output$teamTrend<-renderPlot({
      data22<-EDA_data_Team_2014_2018%>%filter(TEAM==input$teamData)
      ggplot() + geom_smooth(data = data22, aes(x = Year, y = X3PA, color = "3 Point Field Goals Attemped"), size=1.5,se=FALSE) + geom_smooth(data = data22,aes(x = Year, y = X2PA, color = "2 Point Field Goals Attemped",se=FALSE),size=1.5)+theme_classic()+xlab("Year")+ylab("Field Goals Attemped")+ 
        ggtitle("Trend of 3-point and 2-point shot") + 
        theme(axis.title.x = element_text(face="bold",  size=14), 
              axis.title.y = element_text(face="bold",  size=14),
              plot.title = element_text(size=14, face="bold"),  
              axis.text.x  = element_text(vjust=0.5, size=14,face="bold"),
              axis.text.y  = element_text(vjust=0.5, size=14,face="bold"),
              legend.text=element_text(face="bold",size=14)) +
        theme(plot.title = element_text(hjust = 0.5))
      
    })
    
    output$teamTrend2<-renderPlot({
      data22<-EDA_data_Team_2014_2018%>%filter(TEAM==input$teamData)
      ggplot() + geom_line(data = data22, aes(x = Year, y = X3PA/0.6, color = "3 Point Field Goals Attempted"), size=1.5) + geom_line(data = data22, aes(x = Year, y = WinRate, color = "Win Percentage"),size=1.5)+xlab("Year")+ylab("Percentage")+scale_y_continuous(name="Win Percentage")+ggtitle("3-Point Field Goals Attemped vs Win Percentage")+ theme(axis.title.x = element_text(face="bold",  size=14), 
              axis.title.y = element_text(face="bold",  size=14),
              plot.title = element_text(size=14, face="bold"),  
              axis.text.x  = element_text(vjust=0.5, size=14,face="bold"),
              axis.text.y  = element_text(vjust=0.5, size=14,face="bold"),
              legend.text=element_text(face="bold",size=14)) +
        theme(plot.title = element_text(hjust = 0.5))
      
    })
    
    output$playerTrend2<-renderPlot({
      data33<-EDA_data_Player_2014_2018%>%filter(position==input$playerData)
      ggplot()+geom_smooth(data = data33, aes(x = X3P,y=WS),size=1.5,se=FALSE)+geom_point(data = data33, mapping= aes(x =X3P, y = WS,color=height))+xlab("3-points Field Goals Made")+scale_y_continuous(name="Win Share")+ggtitle("3-points Field Goals Made vs Win Share")+theme(axis.title.x = element_text(face="bold",  size=14), 
               axis.title.y = element_text(face="bold",  size=14),
               plot.title = element_text(size=14, face="bold"),  
               axis.text.x  = element_text(vjust=0.5, size=14,face="bold"),
               axis.text.y  = element_text(vjust=0.5, size=14,face="bold"),
               legend.text=element_text(face="bold",size=14)) +theme(plot.title = element_text(hjust = 0.5))
      
    })
    
    output$playerTrend<-renderPlot({
      data33<-EDA_data_Player_2014_2018%>%filter(position==input$playerData)
      ggplot(data33, aes(y=X3P, x=Year,fill=height)) +geom_bar(stat = "identity", position = "dodge")+ggtitle("3-points Field Goals Made By Height In Different Seasons")+theme(axis.title.x = element_text(face="bold",  size=14), 
                                                                                                 axis.title.y = element_text(face="bold",  size=14),
                                                                                                 plot.title = element_text(size=14, face="bold"),  
                                                                                                 axis.text.x  = element_text(vjust=0.5, size=14,face="bold"),
                                                                                                 axis.text.y  = element_text(vjust=0.5, size=14,face="bold"),
                                                                                                 legend.text=element_text(face="bold",size=14)) +theme(plot.title = element_text(hjust = 0.5))
      
    })
    
    output$dist <- renderPlot({
      if(input$data44=="Normal Distribution for Team"){
        benford1 <- benford(EDA_data_Team_2014_2018$X3PA,number.of.digits = 2)
      plot(benford1)
      }
      else if(input$data44=="Benford Analysis for Player"){
        benford2 <- benford(EDA_data_Player_2014_2018$X3PA,number.of.digits = 2)
        plot(benford2)
        }
    })
   
     benford2 <- benford(EDA_data_Player_2014_2018$X3PA,number.of.digits = 2)
    output$view2 <- renderTable({
      if(input$data44=="Benford Analysis for Player"){
      head(suspectsTable(benford2),20) #prints the digits by decreasing order of discrepancies
      #  duplicatesTable(benford2)#prints the duplicates by decreasing order
      #  chisq(benford2)#gets the Chi-squared test
        }
     
    })
    
    output$dist2 <- renderPlot({
      if(input$data44=="Normal Distribution for Team"){
        #Density plot to test normality
        ggdensity(EDA_data_Team_2014_2018$X3PA, 
                  main = "Density plot of 3-points Field Goals",
                  xlab = "3-points Field Goals")
      }
    })
    
    output$dist3 <- renderPlot({
      if(input$data44=="Normal Distribution for Team"){
        #qqplot to test normality
        ggqqplot(EDA_data_Team_2014_2018$X3PA)
      }
    })
    
    }
    )
  )


