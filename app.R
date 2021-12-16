library(readr)
library(tidyverse)
library(ggplot2)
library(gganimate)
library(babynames)
library(hrbrthemes)
library(viridis)
library(gifski)
library(shiny)
library(shinydashboard)
library(DT)
library(shinyWidgets)

Deliveries <- read_csv("dataset/Deliveries.csv")

Matches_Played <- Deliveries %>%
  group_by(batsman) %>% 
  distinct(match_no,batsman) %>% 
  summarise(Matches_Played = n())

Runs <- Deliveries %>% 
  group_by(batsman) %>% 
  summarise(Runs = sum(batsman_run))

Out_matches <- Deliveries %>% 
  filter(Deliveries$is_wicket == 1) %>% 
  group_by(batsman) %>% 
  distinct(match_no,batsman) %>% 
  summarise(Out = n())

Ball_Faced <- Deliveries %>% 
  filter(extra_run == 0 | extra_type == "lb") %>% 
  group_by(batsman) %>% 
  summarise(Ball_Faced = n())

Score <- Deliveries %>% 
  group_by(batsman,match_no) %>% 
  summarise(S = sum(batsman_run))

HS <- Score %>% 
  group_by(batsman) %>% 
  summarise(HS = max(S))


Four <- Deliveries %>% 
  group_by(batsman) %>% 
  filter(batsman_run == 4) %>% 
  summarise(Four = n())

Six <- Deliveries %>% 
  group_by(batsman) %>% 
  filter(batsman_run == 6) %>% 
  summarise(Six = n())


Batting_Stat <- merge(Matches_Played,Runs, all = TRUE)

Batting_Stat <- merge(Batting_Stat, Out_matches, all = TRUE)

Batting_Stat$NO <- Batting_Stat$Matches_Played - Batting_Stat$Out

Batting_Stat <- merge(Batting_Stat, Ball_Faced, all = TRUE)

Batting_Stat <- merge(Batting_Stat, HS, all = TRUE)

Batting_Stat$Average <- round((Batting_Stat$Runs / Batting_Stat$Out), digits = 2)

Batting_Stat$SR <- round(((Batting_Stat$Runs / Batting_Stat$Ball_Faced) * 100), digits = 2)

Batting_Stat <- merge(Batting_Stat, Four, all = TRUE)

Batting_Stat <- merge(Batting_Stat, Six, all = TRUE)

Batting_Stat[is.na(Batting_Stat)] <- 0

Matches_Played_bowler <- Deliveries %>%
  group_by(bowler) %>% 
  distinct(match_no,bowler) %>% 
  summarise(Match = n())

Runs_bowler <- Deliveries %>% 
  group_by(bowler) %>% 
  filter(extra_type != "lb" | extra_run == 0) %>% 
  summarise(Runs = sum(batsman_run,extra_run))

Wickets <- Deliveries %>% 
  filter(Deliveries$is_wicket == 1 & dismissal_type != "runout") %>% 
  group_by(bowler)%>%
  summarise(Wickets = n())

Ball_bowled <- Deliveries %>% 
  filter(extra_run == 0 | extra_type != "lb" | extra_run == 1) %>% 
  group_by(bowler) %>% 
  summarise(Balls = n())

Overs_Bowled <- Deliveries %>% 
  filter(extra_run == 0) %>% 
  group_by(bowler) %>% 
  summarise(Overs = n() %/% 6, Rem = n() %% 6) %>% 
  mutate(Over = as.numeric(str_c(Overs,".",Rem))) %>% 
  select(-c(Overs, Rem))

Most_runs_per_over <- Deliveries %>% 
  filter(extra_type != "lb" | extra_run == 0 | extra_run == 1) %>% 
  group_by(bowler,match_no,over) %>% 
  summarise(D = sum(batsman_run, extra_run)) %>% 
  summarise(fi = max(D)) %>% 
  summarise(Over_Runs = max(fi))

Best <- Deliveries %>% 
  group_by(bowler,match_no) %>% 
  filter(is_wicket == 1 & dismissal_type != "runout") %>% 
  summarise(Wickets = sum(is_wicket)) 


Most_Wickets_per_match <- Best %>% 
  group_by(bowler) %>% 
  summarise(Best = max(Wickets))



Bowling_Stat <- merge(Matches_Played_bowler,Runs_bowler, all = TRUE)

Bowling_Stat <- merge(Bowling_Stat, Wickets, all = TRUE)

Bowling_Stat <- merge(Bowling_Stat, Ball_bowled, all = TRUE)

Bowling_Stat <- merge(Bowling_Stat, Overs_Bowled, all = TRUE)

Bowling_Stat <- merge(Bowling_Stat, Most_runs_per_over, all = TRUE)

Bowling_Stat <- merge(Bowling_Stat, Most_Wickets_per_match, all = TRUE)

Bowling_Stat$Average <- round((Bowling_Stat$Runs / Bowling_Stat$Wickets), digits = 2)

Bowling_Stat$SR <- round((Bowling_Stat$Balls / Bowling_Stat$Wickets), digits = 2)

Bowling_Stat$Economy <- round((Bowling_Stat$Runs / (Bowling_Stat$Over)), digits = 2)

Bowling_Stat[is.na(Bowling_Stat)] <- 0

don <-  Deliveries %>% 
  group_by(batsman,match_no) %>% 
  summarise(Runs = sum(batsman_run))

Bat_chart_Compare <- function(batsman1, batsman2, batsman3, batsman4){
  filter(don, batsman == batsman1 | batsman == batsman2 | batsman == batsman3 | batsman == batsman4)
}

Bat_Stat_Compare <- function(batsman1, batsman2, batsman3, batsman4){
  a <- filter(don, batsman == batsman1)
  a$match_id <- seq.int(nrow(a))
  b <- filter(don, batsman == batsman2)
  b$match_id <- seq.int(nrow(b))
  c <- filter(don, batsman == batsman3)
  c$match_id <- seq.int(nrow(c))
  d <- filter(don, batsman == batsman4)
  d$match_id <- seq.int(nrow(d))
  a <- rbind(a, b)
  a <- rbind(a, c)
  a <- rbind(a, d)
}

Batting_standalone <- function(batsman1){
  a <- filter(don, batsman == batsman1)
  a$match_id <- seq.int(nrow(a))
  a
}

unique_values <- sort(unique(Batting_Stat$batsman))

a <- Batting_Stat[sample(nrow(Batting_Stat), size=4), ]
count <- 0
for (i in a$batsman){
  if (count == 0){
    first <- i
    count <- count + 1
  }
  else if (count == 1){
    second <- i
    count <- count + 1
  }
  else if (count == 2){
    third <- i 
    count <- count + 1
  }
  else if (count == 3){
    fourth <- i 
    count <- count + 1
  }
}

ui <- dashboardPage(
  skin = "red",
  dashboardHeader(
    title = "IPL Stats"
  ),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Batting", tabName = "bat", icon = icon("table")),
      menuItem("Bowling", tabName = "ball", icon = icon("table")),
      menuItem("Compare", tabName = "comp", icon = icon("equals"))
    )
  ),
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "bat",
              fluidPage(
                h1(strong("Batting Stats"),
                   align = "center"),
                dataTableOutput("batting")
              )
      ),
      
      # Second tab content
      tabItem(tabName = "ball",
              fluidPage(
                h1(strong("Bowling Stats"),
                   align = "center"),
                fluidRow(
                dataTableOutput("bowling")
                )
              )
      ),
      tabItem(tabName = "comp",
              fluidPage(
                h1(strong("Batting Comparisons"),
                   align = "center")
              ),
              fluidRow(
                box(title = "Multi Line",
                    status = "warning",
                    solidHeader = TRUE,
                    collapsible = TRUE,
                  plotOutput("animate",height = 250)
                ),
                box(
                  title = "Circular Bar",
                  status = "warning",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  plotOutput("anim", height = 250)
                )
              ),
              fluidRow(
                box(
                  selectInput("bat1", "Batsman 1:", choices = unique_values,
                              selected = "AB de Villiers" 
                  )
                ),
                box(
                  selectInput("bat2", "Batsman 2:", choices = unique_values,
                              selected = "Virat Kohli"
                  )
                )
              ),
              fluidRow(
                box(
                  selectInput("bat3", "Batsman 3:", choices = unique_values,
                              selected = "SV Samson"
                  )
                ),
                box(
                  selectInput("bat4", "Batsman 4:", choices = unique_values,
                              selected = "KL Rahul"
                  )
                )
              ),
              materialSwitch(inputId = "id", label = "Random?", status = "success")
      )
    )
  )
)

server <- function(input, output) { 
  output$batting <- renderDataTable(Batting_Stat)
  output$bowling <- renderDataTable(Bowling_Stat)
  output$animate <- renderPlot({
  if (input$id == TRUE){

    b <- Bat_Stat_Compare(first, second, third, fourth)
      
  }
  else{
    b <- Bat_Stat_Compare(input$bat1, input$bat2, input$bat3, input$bat4)
  }
      ggplot(b, aes(x=match_id, y=Runs, group=batsman, color=batsman)) +
      geom_line() +
      geom_point() +
      ggtitle("Run Comparison") +
        theme(plot.title = element_text(hjust = 0.5)) +
      ylab("Runs Scored") +
      xlab("Matches Played")
  }
  )
  output$anim <- renderPlot({
    if (input$id == TRUE){
      
      Ccompare <- Bat_chart_Compare(first, second, third, fourth)
    }
    else{
      Ccompare <- Bat_chart_Compare(input$bat1, input$bat2, input$bat3, input$bat4)
    }
      ggplot(Ccompare, aes(x = as.factor(batsman), y = Runs)) +
        geom_bar(stat = "identity", fill = alpha("red",0.4)) +
        ylim(-1,635) +
        theme_minimal() +
        theme(
          axis.text = element_text(size = 10),
          axis.title = element_blank(),
          panel.grid = element_blank(),
          plot.margin = unit(rep(-2,4), "cm")     # This remove unnecessary margin around plot
        ) +
        
        # This makes the coordinate polar instead of cartesian.
        coord_polar(start = 0)
      
  }
  )
}

shinyApp(ui, server)