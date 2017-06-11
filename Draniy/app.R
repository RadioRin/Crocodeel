#    http://shiny.rstudio.com/

library(dplyr)
library(recommenderlab)
library(reshape2)
library(ggplot2)

recommend <- function(scores) {
  
  # отбросили пропуски
  scores = na.omit(scores)
  
  # конвертируем колонки -- приводим к нужному типу
  scores$review_overall = as.numeric(scores$review_overall)
  scores$review_profilename = as.factor(scores$review_profilename)
  scores$beer_style = as.factor(scores$beer_style)
  
  lasUserId <- length(unique(scores$review_profilename)) + 1
  
  message("lasrUserId", lasUserId)
  
  # строим разреженную матрицу на основе зачитанных оценок
  scores2 = sparseMatrix(as.integer(scores$review_profilename), as.integer(scores$beer_style), x = scores$review_overall)
  
  rev <- as(scores2, "realRatingMatrix")
  
  # ищем ближайших пользователей
  similarity_users50 <- similarity(rev[1:50, ], method = "cosine", which = "users")
  ratings_beers <- rev[rowCounts(rev) > 1,  colCounts(rev) > 2] 
  # ratings_beers
  
  average_ratings_per_user <- rowMeans(ratings_beers)
  set.seed(100)
  
  test_ind <- sample(1:nrow(ratings_beers), size = nrow(ratings_beers)*0.2)
  recc_data_train <- ratings_beers[-test_ind, ]
  recc_data_test <- ratings_beers[test_ind, ]
  
  recc_model <- Recommender(data = recc_data_train, method = "IBCF",  parameter = list(k = 30))
  # recc_model
  
  recc_predicted <- predict(object = recc_model, newdata = recc_data_test, n = 3)
  # recc_predicted
  
  recc_user_1 <- recc_predicted@items[[2]]
  # recc_user_1
  
  message(recc_user_1)
  
  recc_user_1
}

# залили датасет
scores = readRDS("~/HELL/Crocodeal/Databeer.rda")

# задали дефолтного пользователя
userId = "YOU"

# отбросили пропуски
scores = na.omit(scores)

# конвертируем колонки -- приводим к нужному типу
scores$review_overall = as.numeric(scores$review_overall)
scores$review_profilename = as.factor(scores$review_profilename)
scores$beer_style = as.factor(scores$beer_style)

updatedScores <- scores

print(scores[1,])

# chouse6fromN<-function(N=100, BeerTable = scores){
#   
#   reslt<-unique(as.character(BeerTable$beer_style[c(sample(1:(N/3), 2), sample(round(N/3+1):(2*N/3), 2),sample(round(2*N/3):N, 2))]))
# }

uniqBeer <- as.data.frame(unique(scores$beer_style))
uniqDesc <- as.data.frame(unique(scores$description))
uniq <- cbind(uniqBeer, uniqDesc)
names(uniq) = c("beer_style", "description")

chouse6fromN <<- function(N = 92, BeerTable = scores){
  
  reslt <- as.character(BeerTable$beer_style[c(sample(1:unique(N), 6))])
}

chosenBeersList <<- chouse6fromN(length(scores$beer_style), scores)

## app.R ##
library(shinydashboard)
library(shiny)

## Сайт, где мы брали информацию и туториалы
# https://rstudio.github.io/shinydashboard/structure.html
# http://deanattali.com/blog/building-shiny-apps-tutorial/

ui <- dashboardPage(
  
  dashboardHeader(title = "Думаешь, ты пробовал все? Тогда тебе к нам..."),
  
  ## Sidebar content
  dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      menuItem("Параметры напитка", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Оценка различных видов пива", tabName = "widgets", icon = icon("th")),
      menuItem("Три предложения", tabName = "beer", icon = icon("th"))
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
                         sliderInput("slider", "градусы:", value = c(0.25, 14.75), min = 0.25, max = 14.75),
                         "Как известно, все алкогольные напитки различаются по градусам. Если для вас важна крепость пива, 
                         вы можете выбрать её двигая слайдеры, если же этот параметр для вас не важен - переместите слайдеры на минимальное и максимальное значение"
                       ),
                       box(
                         title = "Определите оттенок желаемого напитка",
                         width = NULL,
                         checkboxGroupInput("color", NULL,
                                            c("Pale Straw", "Straw", "Pale Gold", "Deep Gold", "Pale Amber", "Medium Amber", "Deep Amber", "Amber-Brown", "Brown", "Ruby Brown", "Deep Brown", "Black")),
                         "Если вы желаете попробовать пиво конкретного цвета, можете отметить цвет галочкой. Если вам все равно - выберите все значения"
                       ),
                       actionButton("button", "Далее")
                       )
      )
      ),
      
      
      # Second tab content
      tabItem(tabName = "widgets",
              fluidRow(
                column(width = 7,
                       box(
                         title = "Оцените предложенный стиль пива",
                         width = 0.9,
                         sliderInput("slider2", chosenBeersList[1], 1, 5, 3),
                         filter(uniq, beer_style == chosenBeersList[1])[2]
                       ),
                       box(
                         title = "Оцените предложенный стиль пива",
                         width = 0.9,
                         sliderInput("slider3", chosenBeersList[2], 1, 5, 3),
                         filter(uniq, beer_style == chosenBeersList[2])[2]
                       ),
                       box(
                         title = "Оцените предложенный стиль пива",
                         width = 0.9,
                         sliderInput("slider4", chosenBeersList[3], 1, 5, 3),
                         filter(uniq, beer_style == chosenBeersList[3])[2]
                       ),
                       box(
                         title = "Оцените предложенный стиль пива",
                         width = 0.9,
                         sliderInput("slider5", chosenBeersList[4], 1, 5, 3),
                         filter(uniq, beer_style == chosenBeersList[4])[2]
                       ),
                       box(
                         title = "Оцените предложенный стиль пива",
                         width = 0.9,
                         sliderInput("slider6", chosenBeersList[5], 1, 5, 3),
                         filter(uniq, beer_style == chosenBeersList[5])[2]
                       ),
                       box(
                         title = "Оцените предложенный стиль пива",
                         width = 0.9,
                         sliderInput("slider7", chosenBeersList[6], 1, 5, 3),
                         filter(uniq, beer_style == chosenBeersList[6])[2]
                       ),
                       actionButton("button2", "Далее")
                )
              )
      ),
      #Third tab
      tabItem(tabName = "beer",
              fluidRow(
                box(
                  title = "Все еще уверен, что мы не сможем тебя удивить? Тогда вот идеальное предложение для тебя...",
                  tableOutput("recommendedBeer")
                )
              )
      )
    )
)
  )




server <- function(input, output, session) {
  
  
  # Reactive expression to compose a data frame containing filtered values
  scoresValues <- reactive({
    newscores <- scores[scores$avg2>input$slider[1]&scores$avg2<input$slider[2],]
    newscores
  })
  
  # Show the values using an HTML table
  output$scores <- renderTable({
    scoresValues()
  })
  
  bestRecommendations <- reactive({
    
    print("Running beer recommendations...")
    
    oldScores <- scoresValues()
    
    message("Len of old ", nrow(oldScores))
    message("Head of old ", oldScores[1,])
    
    # формируем датафрейм из пользовательских оценок разному пиву со страницы
    beersRatings <- 
      data.frame(
        c(userId, userId, userId, userId, userId, userId),
        reslt,
        c(input$slider2[1], input$slider3[1], input$slider4[1], 
          input$slider5[1], input$slider6[1], input$slider7[1]),
        c(0.0, 0.0, 0.0, 0.0, 0.0, 0.0),
        fix.empty.names = FALSE
      )
    
    colnames(beersRatings) <- colnames(oldScores)
    updatedScores <- rbind(oldScores, beersRatings)
    # n <- nrow(updatedScores)
    recommended <- recommend(updatedScores)
    print(recommended)
    recommended
  })
  
  output$recommendedBeer <-  renderTable({
    bestRecommendations()
  })
  
  
  observeEvent(
    input$button2, {
      
      print(bestRecommendations())      
      message(bestRecommendations())
      
      newtab <- switch(input$tabs, "beer" = "widgets", "widgets" = "beer")
      updateTabItems(session, "tabs", newtab)
    }
  )
  observeEvent(
    input$button, {
      newtab <- switch(input$tabs, "dashboard" = "widgets", "widgets" = "dashboard")
      updateTabItems(session, "tabs", newtab)
    }
  )
}


shinyApp(ui, server)

