library(shiny)
library(ggplot2)
library(dbplyr)
library(dplyr)
library(ggplot2)
library(scales)
library(MASS) 
library(reshape2) 
library(reshape) 
library(ggrepel)

df <- read.csv("ANZ_Premiership_2017_2022.csv")
df1<- unique(df$Team)
team<-aggregate(df[,c('W','L')],by=list(Team=df$Team),sum)
df2<-melt(team[, c("Team", "W", "L")])
df$Wins<-paste('Wins:', df$W)
df$Wins


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("The visualizations of ANZ Premiership Netball data"),
    sidebarLayout(
      sidebarPanel(
        checkboxGroupInput("Teams",
                           "Choose Teams:",
                           choices = df1,
                           selected = df1),
        sliderInput("years", "Choose years", sep="",
                    min=2017, max=2022, value=c(2017,2022),
                    animate=animationOptions(interval = 1000)),
        
      ),
      mainPanel(
        h2("Chart Tabs"),
        tabsetPanel(
          tabPanel("Wins & Losses By Teams", plotOutput("numbers")),
          tabPanel("Team Ladder Position",plotOutput("ladderp")),
          tabPanel("Goals for vs Goals against", plotOutput("goals"))  
        )
      )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  filteryearteam <- reactive({
    
    df.selection <- filter(df, Team %in% input$Teams, Year %in% (input$years[1]:input$years[2])) 
  
    group_by(df.selection, Team) %>% mutate(Team = factor(Team, levels=c(df1))) 
  })
  
  filterteam <- reactive({
    df.selection <- filter(df2, Team %in% input$Teams) 
    group_by(df.selection, Team) %>% mutate(Team = factor(Team, levels=c(df1))) 
  })

    output$numbers <- renderPlot({
        # generate bins based on input$bins from ui.R

      filterteam ()%>%ggplot(df2, mapping =aes(value, y = Team, fill = variable)) +
          geom_bar(stat = "identity")

    })
    
    output$ladderp <- renderPlot({
      # generate bins based on input$bins from ui.R

      filteryearteam() %>% ggplot(df, mapping =aes(Year, y = Pts, colour = Team)) +
        geom_line()+ geom_point()
    })
    
    output$goals <- renderPlot({
      # generate bins based on input$bins from ui.R
      
      filteryearteam() %>% ggplot(df, mapping =aes(x=GA, y=GF,colour=Team)) +
        geom_point(alpha=0.5,size=3)+geom_text_repel(mapping =aes(label=Wins))
    })
}


# Run the application 
shinyApp(ui = ui, server = server)
