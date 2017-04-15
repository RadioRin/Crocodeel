library(jsonlite)
library(stringr)
library(dplyr)
j = readLines("~/Projects/Crocodeal/Crocodeal/beers.jsonlines") %>% 
  str_c(collapse = ",") %>%  
  (function(str) str_c("[", str, "]")) %>% 
  fromJSON(simplifyDataFrame = T)
j<-na.omit(j)
