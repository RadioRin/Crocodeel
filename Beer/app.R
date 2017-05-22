
#    http://shiny.rstudio.com/
#

library(jsonlite)
library(stringr)
library(dplyr)
j = readLines("~/Projects/Crocodeal/Crocodeal/beers.jsonlines") %>% 
  str_c(collapse = ",") %>%  
  (function(str) str_c("[", str, "]")) %>% 
  fromJSON(simplifyDataFrame = T)
j<-na.omit(j)

## app.R ##
library(shinydashboard)
library(shiny)

## Сайт, где мы брали информацию и туториалы
# https://rstudio.github.io/shinydashboard/structure.html
# http://deanattali.com/blog/building-shiny-apps-tutorial/


ui <- dashboardPage(
  dashboardHeader(title = "Бухнем?"),
  ## Sidebar content
  dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      menuItem("Парамтры напитка", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Оценка различных видов пива", tabName = "widgets", icon = icon("th")),
      menuItem("Три предложения", tabName = "beer", icon = icon("dashboard"))
    )
  ),
  ## Body content
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "dashboard",
              fluidRow(
                column(width = 7,
                       box(
                         title = "Определите крепость желаемого напитка",
                         width = 0.25,
                         sliderInput("slider", "градусы:", value = c(0.25, 14.75), min = 0, max = 14.75),
                         "Как известно, все алкогольные напитки различаются по градусам. Если для вас важна крепость пива, вы можете выбрать её двигая слайдер, если же этот параметр для вас не важен - укажите значение равное 1"
                       ),
                       box(
                         title = "Определите оттенок желаемого напитка",
                         width = NULL,
                         checkboxGroupInput("color", NULL,
                                            c("Светлое", "Тёмное", "Нефильтрованное")),
                         "Если вы желаете, чтобы вам предложили конкретное из трех видов пива, выберите вас интересующее; если для вас цвет не важет - не выбирайте ничего"
                       ),
                         actionButton("button", "Завершить")
                )
              )
      ),
      
      
      # Second tab content
      tabItem(tabName = "widgets",
              fluidRow(
                column(width = 7,
                       box(
                         title = "Оцените предложенное пиво",
                         width = 0.9,
                         sliderInput("slider2", "НАЗВАНИЕ ПИВА", 1, 5, 3),
                         "ОПИСАНИЕ ПИВА"
                       ),
                       box(
                         title = "Оцените предложенное пиво",
                         width = 0.9,
                         sliderInput("slider3", "НАЗВАНИЕ ПИВА", 1, 5, 3),
                         "ОПИСАНИЕ ПИВА"
                       ),
                       box(
                         title = "Оцените предложенное пиво",
                         width = 0.9,
                         sliderInput("slider4", "НАЗВАНИЕ ПИВА", 1, 5, 3),
                         "ОПИСАНИЕ ПИВА"
                       ),
                       box(
                         title = "Оцените предложенное пиво",
                         width = 0.9,
                         sliderInput("slider5", "НАЗВАНИЕ ПИВА", 1, 5, 3),
                         "ОПИСАНИЕ ПИВА"
                       ),
                       box(
                         title = "Оцените предложенное пиво",
                         width = 0.9,
                         sliderInput("slider6", "НАЗВАНИЕ ПИВА", 1, 5, 3),
                         "ОПИСАНИЕ ПИВА"
                       ),
                       actionButton("button2", "Завершить")
                )
              )
      )
    )
  )
)

server <- function(input, output, session) {
  observeEvent(input$button, {
    newtab <- switch(input$tabs,
                     "dashboard" = "widgets",
                     "widgets" = "dashboard"
    )
    updateTabItems(session, "tabs", newtab)
  })
}

shinyApp(ui, server)
