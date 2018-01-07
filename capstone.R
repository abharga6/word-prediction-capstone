library(stringi)
library(ggplot2)
library(magrittr)
library(markdown)
library(RWeka)
library(openNLP)
library(wordcloud)
library(tm)
library(NLP)
library(qdap)
library(RColorBrewer)
library(dplyr)
setwd("C:/Users/ichbi/Desktop/Data science specialization/Capstone project/Dataset/final/en_US")
twitter <- readLines("en_US.twitter.txt", encoding="UTF-8")
news <- readLines("en_US.news.txt", encoding="UTF-8")
blogs <- readLines("en_US.blogs.txt", encoding="UTF-8")
size <- function(x){file.info(x)$size/1024/1024}
lines <- function(x){length(x)}
char <- function(x){sum(stri_length(x)-stri_count_fixed(x," "))}
words<- function(x){sum(stri_count_words(x))}
filesummary<- data.frame(source=c("twitter","blogs","news"),
		  file_size=c(size("en_US.twitter.txt"),size("en_US.blogs.txt"),size("en_US.news.txt")),
		  no_of_lines=c(lines(twitter),lines(blogs),lines(news)),
		  no_of_characters=c(char(twitter),char(blogs),char(news)),
		  no_of_words=c(words(twitter),words(blogs),words(news)))
con1 <- file("en_US.twitter.txt", "r")
con2<- file("en_US.blogs.txt", "r")
con3<-file("en_US.news.txt", "r")
twitter <- readLines(con1,5000)
blogs<- readLines(con2,5000)
news<- readLines(con3,5000)
badwords<- readLines("en_US.profane.txt")
data <- paste(twitter,blogs,news)
data <- sent_detect(data,language="en",model=NULL)
data <- gsub("[^a-zA-Z ]","",data)
corpus<- VCorpus(VectorSource(data))
badwords <- VectorSource(badwords)
corpus <- tm_map(corpus,removeNumbers)
corpus<- tm_map(corpus,removePunctuation)
corpus<- tm_map(corpus,stripWhitespace)
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removeWords, badwords)
unitoken<- function(x){NGramTokenizer(x, Weka_control(min = 1, max = 1))}
tdm_for_onegram <- TermDocumentMatrix(corpus,control = list(tokenize = unitoken))
fm <- rowSums(as.matrix(tdm_for_onegram))
onegram <- data.frame(words=names(fm),freq=fm)
onegram <- onegram[order(onegram$freq, decreasing=TRUE),]
bitoken <- function(x){NGramTokenizer(x, Weka_control(min=2, max=2))}
tdm_for_twogram <- TermDocumentMatrix(corpus,control=list(tokenizer = bitoken))
fm <- rowSums(as.matrix(tdm_for_twogram))
twogram <- data.frame(words=names(fm),freq=fm)
twogram <- twogram[order(twogram$freq, decreasing=TRUE),]
tritoken <- function(x){NGramTokenizer(x, Weka_control(min=3, max=3))}
tdm_for_threegram <- TermDocumentMatrix(corpus,control=list(tokenizer = tritoken))
fm <- rowSums(as.matrix(tdm_for_threegram))
threegram <- data.frame(words=names(fm),freq=fm)
threegram <- threegram[order(threegram$freq, decreasing=TRUE),]
qudratoken <- function(x){NGramTokenizer(x, Weka_control(min=4, max=4))}
tdm_for_fourgram <- TermDocumentMatrix(corpus, control=list(tokenizer = qudratoken))
fm <- rowSums(as.matrix(tdm_for_fourgram))
fourgram <- data.frame(words=names(fm), freq=fm)
fourgram <- fourgram[order(fourgram$freq, decreasing=TRUE),]
top_onegram <- onegram[1:15,]
top_twogram <- twogram[1:15,]
top_threegram <- threegram[1:15,]
top_fourgram <- fourgram[1:15,]

ggplot(top_onegram, aes(x=reorder(words,freq),y=freq))+geom_bar(stat="identity", fill="red")+ geom_text(aes(label=freq),vjust=1)+coord_flip()
ggplot(top_twogram, aes(x=reorder(words,freq),y=freq))+geom_bar(stat="identity", fill="blue")+ geom_text(aes(label=freq),vjust=0.1)+coord_flip()
ggplot(top_threegram, aes(x=reorder(words,freq),y=freq))+geom_bar(stat="identity", fill="green")+ geom_text(aes(label=freq),vjust=0.1)+coord_flip()
ggplot(top_fourgram, aes(x=reorder(words,freq),y=freq))+geom_bar(stat="identity", fill="purple")+ geom_text(aes(label=freq),vjust=0.1)+coord_flip()

wordcloud(onegram$words,onegram$freq, max.words=50,random.order=FALSE,rot.per=0.35,colors=brewer.pal(8, "Dark2"))
wordcloud(twogram$words,twogram$freq, max.words=50,random.order=FALSE,rot.per=0.35,colors=brewer.pal(8, "Dark2"))
wordcloud(threegram$words,threegram$freq, max.words=50,random.order=FALSE,rot.per=0.35,colors=brewer.pal(8, "Dark2"))
wordcloud(fourgram$words,fourgram$freq, max.words=50,random.order=FALSE,rot.per=0.35,colors=brewer.pal(8, "Dark2"))


woperc <- function(percentage) {
totalwords <- sum(onegram$freq)
percent = 0
cumsum = 0
i = 1
while (percent < percentage)
{
cumsum = cumsum + onegram$freq[i]
percent = cumsum/totalwords
i = i + 1
}
return(i)
}

#creating table for prediction model

createNgramTable <- function(x){
z <- strsplit(as.character(x$words)," ")
x$nminusgram <- NA
x$lastword <- NA
for (i in 1:nrow(x)){
wo <- vector()
for (j in 1:(length(z[[i]])-1)){
wo <- c(wo,z[[i]][j])
}
x$nminusgram[i] <- paste(wo, collapse=" ")
x$lastword[i] <- tail(z[[i]],1)
}
return(as.data.frame(x, row.names =NULL, stringsAsFactors=FALSE))
}

twogramTable <- createNgramTable(twogram)
threegramTable <- createNgramTable(threegram)
fourgramTable <- createNgramTable(fourgram)


prediction_model <- function(x,y,z,k){
  t<- tolower(x)
  m<- paste(tail(unlist(strsplit(t,' ')),3), collapse=" ")
  u<- paste(tail(unlist(strsplit(t,' ')),2), collapse=" ")
  v<- paste(tail(unlist(strsplit(t,' ')),1), collapse=" ")
  if (stri_count_words(x)>2){
    if (m %in% y$nminusgram){
	i <- y %>% filter(nminusgram==m) %>% .$lastword
      return(i[1])
    } else if (u %in% z$nminusgram){
	i <- z %>% filter(nminusgram==u) %>% .$lastword
        return(i[1])
    } else if (v %in% k$nminusgram){
	i <- k %>% filter(nminusgram==u) %>% .$lastword
        return(i[1])
      } else {return('the')}
  } else if(stri_count_words(x)==2){
    if (u %in% z$nminusgram){
	i <- z %>% filter(nminusgram==u) %>% .$lastword
        return(i[1])
    } else if (v %in% k$nminusgram){
      i <- k %>% filter(nminusgram==u) %>% .$lastword
        return(i[1])
      } else {return('the')}
  } else if(stri_count_words(x)==1){
    if (v %in% k$nminusgram){
      i <- k %>% filter(nminusgram==u) %>% .$lastword
        return(i[1])
      }else {return('the')}
  } else {print('wrong input')}
}





