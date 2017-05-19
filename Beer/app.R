
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
      menuItem("Парамтры напитка", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Оценка различных видов пива", tabName = "widgets", icon = icon("th"))
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
                         sliderInput("slider", "градусы:", 0.25, 14.75, 6),
                         "Как известно, все алкогольные напитки различаются по градусам. Если для вас важна крепость пива, вы можете выбрать её двигая слайдер, если же этот параметр для вас не важен - укажите значение равное 1"
                       ),
                       box(
                         title = "Определите оттенок желаемого напитка",
                         width = NULL,
                         checkboxGroupInput("color", NULL,
                                            c("Светлое", "Тёмное", "Нефильтрованное")),
                         "Если вы желаете, чтобы вам предложили конкретное из трех видов пива, выберите вас интересующее; если для вас цвет не важет - не выбирайте ничего"
                       )
                )
              )
      ),
      
      
      # Second tab content
      tabItem(tabName = "widgets",
              h2("Widgets tab content")
      )
    )
  )
)

server <- function(input, output) {
  set.seed(122)
  histdata <- rnorm(500)
  
  output$plot1 <- renderPlot({
    data <- histdata[seq_len(input$slider)]
    hist(data)
  })
}

shinyApp(ui, server)
