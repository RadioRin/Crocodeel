library(jsonlite)
library(stringr)
library(dplyr)
j = readLines("~/Crocodeal/beers.jsonlines") %>% 
  str_c(collapse = ",") %>%  
  (function(str) str_c("[", str, "]")) %>% 
  fromJSON(simplifyDataFrame = T)
p=unique(j$style)