library(dplyr)
library(recommenderlab)
library(reshape2)
library(ggplot2)
scores = bc %>% select(member_url,url,overall)
scores$overall=as.numeric(scores$overall)
scores$member_url=as.factor(scores$member_url)
scores$url=as.factor(scores$url)
scores2  = sparseMatrix(as.integer(scores$member_url), as.integer(scores$url), x = scores$overall)

rev <- as(scores2, "realRatingMatrix")

similarity_users50 <- similarity(rev[1:50, ], method = "cosine", which = "users")
as.matrix(similarity_users50)
image(as.matrix(similarity_users50), main = "User similarity")
ratings_beers <- rev[rowCounts(rev) > 5,
                             colCounts(rev) > 10] 
ratings_beers

average_ratings_per_user <- rowMeans(ratings_beers)
ggplot()+geom_histogram(aes(x=average_ratings_per_user)) +
  ggtitle("Распределение средних оценок пользователей")


set.seed(100)
test_ind <- sample(1:nrow(ratings_beers), size = nrow(ratings_beers)*0.2)
recc_data_train <- ratings_beers[-test_ind, ]
recc_data_test <- ratings_beers[test_ind, ]

recc_model <- Recommender(data = recc_data_train, method = "IBCF",
                          parameter = list(k = 30))
recc_model
