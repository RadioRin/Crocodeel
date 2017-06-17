#    http://shiny.rstudio.com/

library(dplyr)
library(recommenderlab)
library(reshape2)
library(ggplot2)
library(RColorBrewer)
library(leaflet)
library(readr)

recommend <- function(scoresInput, complements) {
  
  message("compl " , complements)
  
  scoresArg <- cbind(scoresInput)
  
  # отбросили пропуски
  scoresArg = na.omit(scoresArg)
  
  # конвертируем колонки -- приводим к нужному типу
  message("beeerstylenames " , unique(scoresArg$beer_style))
  beerStyleNames <- as.character(scoresArg$beer_style)
  message("beeerstylenames2 " ,  unique(beerStyleNames))
  
  scoresArg$review_overall = as.numeric(scoresArg$review_overall)
  scoresArg$review_profilename = as.factor(scoresArg$review_profilename)
  beerStyleInteger <- match(scoresArg$beer_style, unique(scoresArg$beer_style)) 
  
  lasUserId <- length(unique(scoresArg$review_profilename)) + 1
  
  styles_nums <- data.frame(
    beerStyleNames,
    beerStyleInteger,
    fix.empty.names = FALSE,
    stringsAsFactors=FALSE
  )
  
  # message("\n\nstyles_nums " , styles_nums)
  
  colnames(styles_nums) <- c("beer_style", "beer_style_int")
  styles_nums <- unique(styles_nums[c("beer_style", "beer_style_int")])
  
  # message("styles_nums_unique " , styles_nums)
  
  # строим разреженную матрицу на основе зачитанных оценок
  scores2 = sparseMatrix(as.integer(scoresArg$review_profilename), 
                         beerStyleInteger, 
                         x = scoresArg$review_overall)
  
  # message("scores matrix ", scores2)
  
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
  
  recc_predicted <- predict(object = recc_model, newdata = recc_data_test, n = 3)
  
  message("Recommended items ", recc_predicted@items)
  
  recc_user_1 <- recc_predicted@items[[2]]
  
  message("Styles recommended to user: ", recc_user_1)
  
  # todo: add complements
  # df <- scoresArg[match(recc_user_1, as.integer(scoresArg$beer_style))]
  # message("df ", df)
  
  recc_df = data.frame(
    as.integer(recc_user_1),
    fix.empty.names = FALSE
  )
  
  colnames(recc_df) <- c("recommended_style")
  
  # merging with beer style names
  recc <- merge(recc_df, styles_nums, by.x="recommended_style", by.y="beer_style_int", all.x=TRUE)
  
  # merging with zacousquea
  recc2 <- merge(recc, complements, by.x="beer_style", by.y="beer_style", all.x=TRUE)
  
  recc2  %>% select(beer_style, zacus)
}

# залили датасет
scores = readRDS("Databeer.rda")
zacuson = read.csv("zacuski.csv", header = TRUE)
# rownames(zacuson) <- zacuson$X1
# zacuson <- tidyr::gather(zacuson, "beer_style", "score", 2:90)
# names(zacuson) = c("zacus", "beer_style", "score")

#MAPS
rosals = read.csv("rosals.csv")

#MAPS

# задали дефолтного пользователя
userId = "YOU"

# отбросили пропуски
scores = na.omit(scores)

# конвертируем колонки -- приводим к нужному типу
scores$review_overall = as.numeric(scores$review_overall)
scores$review_profilename = as.factor(scores$review_profilename)
scores$beer_style = as.factor(scores$beer_style)
# pal = brewer.pal(9,"BuPu")

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

chouse6fromN <<- function(N = 89, BeerTable = scores){
  #message("SETTING RESLT")
  reslt <- as.character(BeerTable$beer_style[c(sample(1:unique(N), 6))])
  #message("RESLT ", reslt)
}

#message("RESLT OUTER ", reslt)

chosenBeersList <<- chouse6fromN(length(scores$beer_style), scores)

## app.R ##
library(shinydashboard)
library(shiny)

ui <- dashboardPage(
  
  dashboardHeader(title = "Думаешь, ты пробовал все? Тогда тебе к нам...", titleWidth = 1580),
  
  ## Sidebar content
  dashboardSidebar(width = 280,
                   sidebarMenu(
                     id = "tabs",
                     menuItem("Параметры напитка", tabName = "dashboard", icon = icon("list-ol")),
                     menuItem("Оценка различных видов пива", tabName = "widgets", icon = icon("check-square-o")),
                     menuItem("Три предложения", tabName = "beer", icon = icon("beer")),
                     menuItem("В добрый путь!", tabName = "nameM", icon = icon("thumbs-up"))
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
                         sliderInput("slider", "градусы:", value = c(0.25, 57.70), min = 0.25, max = 57.70),
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
                         width = 0.5,
                         sliderInput("slider2", chosenBeersList[1], 1, 5, 3),
                         filter(uniq, beer_style == chosenBeersList[1])[2]
                       ),
                       box(
                         title = "Оцените предложенный стиль пива",
                         width = 0.5,
                         sliderInput("slider3", chosenBeersList[2], 1, 5, 3),
                         filter(uniq, beer_style == chosenBeersList[2])[2]
                       ),
                       box(
                         title = "Оцените предложенный стиль пива",
                         width = 0.5,
                         sliderInput("slider4", chosenBeersList[3], 1, 5, 3),
                         filter(uniq, beer_style == chosenBeersList[3])[2]
                       ),
                       box(
                         title = "Оцените предложенный стиль пива",
                         width = 0.5,
                         sliderInput("slider5", chosenBeersList[4], 1, 5, 3),
                         filter(uniq, beer_style == chosenBeersList[4])[2]
                       ),
                       box(
                         title = "Оцените предложенный стиль пива",
                         width = 0.5,
                         sliderInput("slider6", chosenBeersList[5], 1, 5, 3),
                         filter(uniq, beer_style == chosenBeersList[5])[2]
                       ),
                       box(
                         title = "Оцените предложенный стиль пива",
                         width = 0.5,
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
      ),
      #Forth tab
      tabItem(tabName = "nameM", 
              dashboardSidebar(
                dashboardBody(
                  fluidPage(
                    fluidRow(
                      leafletOutput("map", width = 1000)
                    ), 
                    fluidRow(
                      actionButton("button3", "Начать заново")
                  )
              )
                )
              )
      )
    )
  )
)




server <- function(input, output, session) {
  
  
  # Reactive expression to compose a data frame containing filtered values
  scoresValues <- reactive({
    newscores <- scores[scores$beer_abv>input$slider[1]&scores$beer_abv<input$slider[2],]
    newscores
  })
  # Show the values using an HTML table
  output$scores <- renderTable({
    scoresValues()
  })
  
  bestRecommendations <- reactive({
    
    print("Running beer recommendations...")
    
    # review_overall review_profilename beer_style beer_name beer_abv beer_beerid beer_color description
    oldScores <- scoresValues()
    
    # message("Len of old ", nrow(oldScores))
    # message("Head of old ", oldScores[1,])
    
    # формируем датафрейм из пользовательских оценок разному пиву со страницы
    beersRatings <- 
      data.frame(
        # review_profilename
        c(userId, userId, userId, userId, userId, userId),
        # beer_style
        chosenBeersList,
        # review_overall
        c(input$slider2[1], input$slider3[1], input$slider4[1], 
          input$slider5[1], input$slider6[1], input$slider7[1]),
        # beer_abv
        c(0.0, 0.0, 0.0, 0.0, 0.0, 0.0),
        # beer_color
        c(input$color[1],input$color[1],input$color[1],
          input$color[1],input$color[1],input$color[1]),
        fix.empty.names = FALSE
      )
    
    
    # message("inputcolor ", input$color[1])
    #  message("beersRatings ", beersRatings)
    #  message("oldScores ", colnames(oldScores))
    #  message("beersRatings ", colnames(beersRatings))
    
    oldScoresProjection <- oldScores %>% select(review_profilename, beer_style, review_overall, beer_abv, beer_color)
    
    # https://stackoverflow.com/questions/13043928/selecting-rows-where-a-column-has-a-string-like-hsa-partial-string-match
    oldScoresProjection <- oldScoresProjection[grep(input$color[1], oldScoresProjection$beer_color), ]
    
    colnames(beersRatings) <- colnames(oldScoresProjection)
    
    updatedScores <- rbind(oldScoresProjection, beersRatings)
    # n <- nrow(updatedScores)
    recommended <- recommend(updatedScores, zacuson)
    print(recommended)
    recommended
  })
  
  output$recommendedBeer <-  renderTable({
    bestRecommendations()
  })
  
  
  observeEvent(
    input$button2, {
      
      # print(bestRecommendations())      
      # message(bestRecommendations())
      
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
  observeEvent(
    input$button3, {
      newtab <- switch(input$tabs, "dashboard" = "nameM", "nameM" = "dashboard")
      updateTabItems(session, "tabs", newtab)
    }
  )
  output$map <- renderLeaflet({ 
    #todo: rosals
    leaflet(rosals, width = ) %>% 
      addTiles() %>%
      fitBounds(~min(lon), ~min(lat), ~max(lon), ~max(lat))  %>%
      addCircles(lng = ~lon, lat = ~lat, radius = 50, weight = 15,
                 fillColor = 1, fillOpacity = 0.7, popup = ~paste(Pivov)) 
  })
}


shinyApp(ui, server)



