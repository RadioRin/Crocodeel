library(dplyr)
library(recommenderlab)
library(reshape2)
library(ggplot2)
#bc$url=str_replace(bc$url,"\\?","")
#j=na.omit(j)
#j1 = j %>% select(style,profile_url,avg) %>%group_by(profile_url) %>% mutate(avg2 = mean(avg))
#j1=unique(j1)
#j1$avg=as.numeric(j1$avg)
#j1 = j1 %>% mutate(avg2 = mean(avg))
#j1$avg=NULL
#colnames(j1)[2] <- "url"
#bc= left_join(bc,j1)
#write.csv(bc,"rev_st.csv")
#write.csv(scores,"scores.csv")
#scores = bc %>% select(member_url,style,overall,avg2)
scores=read.csv("scores.csv", header=TRUE)
scores=na.omit(scores)
scores$overall=as.numeric(scores$overall)
scores$member_url=as.factor(scores$member_url)
scores$style=as.factor(scores$style)
scores2  = sparseMatrix(as.integer(scores$member_url), as.integer(scores$style), x = scores$overall)

rev <- as(scores2, "realRatingMatrix")

similarity_users50 <- similarity(rev[1:50, ], method = "cosine", which = "users")
as.matrix(similarity_users50)
image(as.matrix(similarity_users50), main = "User similarity")
ratings_beers <- rev[rowCounts(rev) > 1,
                             colCounts(rev) > 2] 
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

recc_predicted <- predict(object = recc_model, newdata = recc_data_test, n = 6)
recc_predicted

recc_user_1 <- recc_predicted@items[[1]]
recc_user_1
beers_user_1 <- recc_predicted@itemLabels[recc_user_1]
beers_user_1
