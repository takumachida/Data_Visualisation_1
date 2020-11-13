library(shiny)
library(shinydashboard)
library(tidyverse)
library(ggplot2)


ui <- dashboardPage(
    dashboardHeader(),
    dashboardSidebar(
      selectInput(inputId = "gender_input",label = "Select Gender",choices = c("male","female"))
      #label-> is what the user is going to see n the dashboard side bar
      #choices -> these are the different options the user can select
      #inputId = "gender_input"-> is going to be used in the server (check in server how its used)
    ),
    dashboardBody(
      plotOutput("plot1_survived")
    )
)
server <- function(input,output){
  df<-tibble(read.csv("train.csv"))
  data_gender<- reactive({
    df_1 <- df %>% filter(Sex==input$gender_input)
    df_2 <- na.omit(df_1)
    return(df_2)
  }) 
  output$plot1_survived <- renderPlot({
    data_gender() %>% group_by(Pclass,Survived) %>% count() %>% 
      ggplot(aes(Pclass,y=n,fill=Survived)))+
      geom_col()
  })
}
shinyApp(ui=ui,server=server)

