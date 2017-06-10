library(readr)
beer_desc_ful <- read_delim("~/HELL/Crocodeal/beer_desc_ful.csv", 
                            ";", escape_double = FALSE, col_names = FALSE, 
                            trim_ws = TRUE)

descbeer<-beer_desc_ful[,1:2]

beer_reviews <- read_csv("~/HELL/Crocodeal/beer_reviews.csv")
berrev<-beer_reviews[,c(4,7,8,11,12,13)]
berrev<-na.omit(berrev)
colnames(descbeer)<- c("beer_style", "desc")
brv2<-inner_join(berrev, descbeer, by = "beer_style")
desc<-as.data.frame(unique(berrev$beer_style))

colnames(desc) <- "beer_style"
desc<-as.data.frame(as.character(desc$beer_style))
colnames(descbeer) <- c("beer_style", "description")
descfin<-left_join(desc,descbeer, by = "beer_style")
big<-as.data.frame(unique(berrev$beer_style))
colnames(big) <- "beer_style"
small<-as.data.frame(unique(descbeer$beer_style))
colnames(small) <- "beer_style"
bs<-left_join(big,small, by = "beer_style")
write.csv(descfin, file = "description.csv")
