#
#    http://shiny.rstudio.com/
#

library(jsonlite)
library(stringr)
library(dplyr)
j = readLines("~/HELL/Crocodeal/beers.jsonlines") %>% 
  str_c(collapse = ",") %>%  
  (function(str) str_c("[", str, "]")) %>% 
  fromJSON(simplifyDataFrame = T)
j<-na.omit(j)


library(shiny)

ui <- fluidPage(
  sliderInput(inputId = "num",
              label = "Выберите крепость пива (в градусах)",
              value = 6, min = 0, max = 15),
  sliderInput(inputId = "num1",
              label = "Выберите густоту пива",
              value = 12, min = 0.5, max = 27),
  checkboxGroupInput(inputId = "num2",
                     label = "Выберите вкус пива",
                     choices = c("Вишневое", "Ореховое", "Со вкусом дуба"))
)

server <- function(input, output) {}


shinyApp(ui = ui, server = server)
