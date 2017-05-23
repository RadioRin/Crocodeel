library(jsonlite)
library(stringr)
library(dplyr)
j = readLines("~/Projects/Crocodeal/Crocodeal/beers.jsonlines") %>% 
  str_c(collapse = ",") %>%  
  (function(str) str_c("[", str, "]")) %>% 
  fromJSON(simplifyDataFrame = T)

<<<<<<< HEAD
bc = readLines("~/Crocodeal/beer_comments.jsonlines") %>% 
  str_c(collapse = ",") %>%  
  (function(str) str_c("[", str, "]")) %>% 
  fromJSON(simplifyDataFrame = T)

memb = readLines("~/Crocodeal/members.jsonlines") %>% 
  str_c(collapse = ",") %>%  
  (function(str) str_c("[", str, "]")) %>% 
  fromJSON(simplifyDataFrame = T)


=======




j<-na.omit(j)
>>>>>>> befad5a97bb52c1cdf8121b256e8f0f5a8376a83
