library(dplyr)
library(recommenderlab)
library(reshape2)
scores = bc %>% select(member_url,url,overall)
scores$overall=as.numeric(scores$overall)
scores$member_url=as.factor(scores$member_url)
scores$url=as.factor(scores$url)
scores2  = sparseMatrix(as.integer(scores$member_url), as.integer(scores$url), x = scores$overall)

rev <- as(scores2, "realRatingMatrix")

