library(jsonlite)
library(stringr)
library(dplyr)
j = readLines("~/Crocodeal/beers.jsonlines") %>% 
  str_c(collapse = ",") %>%  
  (function(str) str_c("[", str, "]")) %>% 
  fromJSON(simplifyDataFrame = T)

bc = readLines("~/Crocodeal/beer_comments.jsonlines") %>% 
  str_c(collapse = ",") %>%  
  (function(str) str_c("[", str, "]")) %>% 
  fromJSON(simplifyDataFrame = T)

memb = readLines("~/Crocodeal/members.jsonlines") %>% 
  str_c(collapse = ",") %>%  
  (function(str) str_c("[", str, "]")) %>% 
  fromJSON(simplifyDataFrame = T)


