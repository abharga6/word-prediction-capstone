suppressPackageStartupMessages(c(
  library(shinythemes),
  library(shiny),
  library(tm),
  library(stringr),
  library(markdown),
  library(stylo),library(stringi),
library(ggplot2),
library(magrittr),
library(markdown),
library(RWeka),
library(openNLP),
library(wordcloud),
library(tm),
library(NLP),
library(qdap),
library(RColorBrewer),
library(dplyr)))

y <- readRDS(file="./fourgramTable.RData")
z <- readRDS(file="./threegramTable.RData")
k <- readRDS(file="./twogramTable.RData")

prediction_model <- function(x,y,z,k){
  t<- tolower(x)
  t <- removePunctuation(t)
  t <- removeNumbers(t)
  t <- str_replace_all(t, "[^[:alnum:]]", " ")
  m<- paste(tail(unlist(strsplit(t,' ')),3), collapse=" ")
  u<- paste(tail(unlist(strsplit(t,' ')),2), collapse=" ")
  v<- paste(tail(unlist(strsplit(t,' ')),1), collapse=" ")
  if (stri_count_words(x)>2){
    if (m %in% y$nminusgram){
      i <- y %>% filter(nminusgram==m) %>% .$lastword
      print(i[1])
    } else if (u %in% z$nminusgram){
      i <- z %>% filter(nminusgram==u) %>% .$lastword
      print(i[1])
    } else if (v %in% k$nminusgram){
      i <- k %>% filter(nminusgram==u) %>% .$lastword
      print(i[1])
    } else {print('the')}
  } else if(stri_count_words(x)==2){
    if (u %in% z$nminusgram){
      i <- z %>% filter(nminusgram==u) %>% .$lastword
      print(i[1])
    } else if (v %in% k$nminusgram){
      i <- k %>% filter(nminusgram==u) %>% .$lastword
      print(i[1])
    } else {print('the')}
  } else if(stri_count_words(x)==1){
    if (v %in% k$nminusgram){
      i <- k %>% filter(nminusgram==u) %>% .$lastword
      print(i[1])
    }else {print('the')}
  } else {print('wrong input')}
}
