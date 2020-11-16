library(shiny)
library(shinydashboard)
library(tidyverse)
library(ggplot2)


ui <- dashboardPage(
    dashboardHeader(
      title=" Titanic Data Analysis "
    ),
    dashboardSidebar(
      selectInput(inputId = "gender_input",label = "Select Gender for 1st graph ",choices = c("Male"="male","Female"="female","Both"="n")),
      #label-> is what the user is going to see n the dashboard side bar
      #choices -> these are the different options the user can select
      #input_Id = "gender_input"-> is going to be used in the server (check in server how its used)
      radioButtons("gender_button","Select Gender for 2nd graph",choiceNames=c("Male","Female"),choiceValues = c("male","female"))
      
    ),
    dashboardBody(
     column(width = 6, plotOutput("plot1_survived",height=200)),
     column(width = 6, plotOutput("plot2_survived",height=200)),
     column(width = 6, tableOutput("table1"))
    )
)

server <- function(input,output){
  df<-tibble(read.csv("train.csv"))
  data_gender<- reactive({
   if(input$gender_input!="n"){
     df_1 <- df %>% filter(Sex==input$gender_input) %>% 
       mutate(Survived=as.factor(Survived),Survived=as.factor(Survived))
     df_2 <- na.omit(df_1)
   }else{
     df_1 <- df %>% mutate(Survived=as.factor(Survived),Survived=as.factor(Survived))
     df_2 <- na.omit(df_1)
   }
    return(df_2)
  }) 
  data_gender_2 <- reactive({
    df_1 <- df %>% filter(Sex==input$gender_button) %>% 
      mutate(Survived=as.factor(Survived),Survived=as.factor(Survived))
    df_2 <- na.omit(df_1)
    return(df_2)
  }) 
  output$plot1_survived <- renderPlot({
    data_gender() %>% group_by(Pclass,Survived) %>% count() %>% 
      ggplot(aes(Pclass,y=n,fill=Survived))+
      geom_col()+
      labs(x="Classes",y="Number of Survived/Not Survived",title="Classes Against Survived/Not Survived")+
      theme_classic()+
      theme_linedraw()+
      theme(plot.title = element_text(hjust=0.5))
      
  })
  output$table1 <- renderTable({
    data_gender() 
  })
  output$plot2_survived <- renderPlot({
    data_gender_2() %>% group_by(Pclass,Survived) %>% count() %>% 
      ggplot(aes(Pclass,y=n,fill=Survived))+
      geom_col()+
      labs(x="Classes",y="Number of Survived/Not Survived",title="Titanic Data")+
      theme_linedraw()+
      theme_bw()+
      theme(plot.title = element_text(hjust=0.5))
  })
}
shinyApp(ui=ui,server=server)

