library(shiny)
library(shinydashboard)
library(DT)
library(shinyWidgets)

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