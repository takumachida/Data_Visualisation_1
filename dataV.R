library(tidyverse)
library(ggplot2)
library(shiny)
library(shinydashboard)


print ("hello world")
print (df)
df_new <- df %>% select(Pclass,Survived,Sex) 

print(df_new)
