if (!require('tm')) {install.packages('tm')
library(tm)}
if (!require('stringi')) {install.packages('stringi')
library(stringi)}
if (!require('stringr')) {install.packages('stringr')
library(stringr)}
if (!require('RWeka')) {install.packages('RWeka')
library(RWeka)}
if (!require('SnowballC')) {install.packages('SnowballC')
library(SnowballC)}
if (!require('ggplot2')) {install.packages('ggplot2')
library(ggplot2)}
if (!require('data.table')) {install.packages('data.table')
library(data.table)}

url <- "https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip"
if (!file.exists("Dataset/Coursera-SwiftKey.zip")){
  download.file(my_url, "Dataset/Coursera-SwiftKey.zip")
  unzip("Coursera-SwiftKey.zip")
}


# Loading datasets
blog <- readLines("../Dataset/en_US.blogs.txt", encoding="UTF-8", skipNul=TRUE)
twitter <- readLines("../Dataset/en_US.twitter.txt", encoding="UTF-8",skipNul=TRUE)
news <- readLines(file("../Dataset/en_US.news.txt","rb"), encoding="UTF-8",skipNul=TRUE)

##Exploring the Files

#Memory in every file and number of lines
 data.frame(Data=c("blog","twitter","news"),
           Memory_MB=c(object.size(blog)/(1024*1024),object.size(twitter)/(1024*1024),object.size(news)/(1024*1024)),          Lines=c(length(blog),length(twitter),length(news)))


# words per line
bwords <- stri_count_words(blog)
nwords <- stri_count_words(news)
twords <- stri_count_words(twitter)

#Some lines are empty or with one word
# data.frame(Data=c("blog","news","twitter"),EmptyLines=c(length(blog[bwords == 0 | bwords == 1]),length(news[nwords == 0 | nwords == 1]),length(twitter[twords == 0 | twords == 1])))
#Removing those lines
blog <- blog[!(bwords == 0 | bwords == 1)]
news <- news[!(nwords == 0 | nwords == 1)]
twitter <- twitter[!(twords == 0 | twords == 1)]

#Sampling
set.seed(333)
blogTraining <- sample(blog, length(blog)*0.04)
newsTraining <- sample(news, length(news)*0.04)
twitterTraining <- sample(twitter, length(twitter)*0.2)
rm(blog)
rm(news)
rm(twitter)
#Merge the data sets
#corpus <- c(blogTraining, newsTraining, twitterTraining)
corpus <- c(blogTraining, newsTraining, twitterTraining)
rm(blogTraining)
rm(newsTraining)
rm(twitterTraining)

# corp <- sapply(corpus,function(row) iconv(row, "latin1", "ASCII", sub=""))
# corp <- sapply(corp,function(row) iconv(row,"UTF-8", sub=""))
##Cleaning the Data
# corpus <- data.frame(corpus, stringsAsFactors = FALSE)
# corpus <- DataframeSource(corpus)
writeLines(corpus, "../Dataset/sample/sample.txt")
corpus <- Corpus(DirSource("../Dataset/sample"), readerControl = list(language = "en_US", encoding = "UTF-8"))


corpus<- VectorSource(corpus)
corpus <- Corpus(corpus)
removeCharacter <- content_transformer(function(x, pattern) gsub(pattern, " ", x))
corpus <- tm_map(corpus, removeCharacter, "/|@|\\|")


#corpus<-stri_trans_general(corpus, "latin-ascii")

corpus <- tm_map(corpus, removeNumbers) # removing numbers
corpus <- tm_map(corpus, stripWhitespace) # removing whitespaces
corpus <- tm_map(corpus, content_transformer(tolower)) #lowercasing all contents
corpus <- tm_map(corpus, removePunctuation) # removing special characters

##Word Analysis

#Now we can look to the main statistic of all data sets in the terms of word count.

#remove bad words
profanity <- readLines("../Dataset/profanity.txt")
corpus <- tm_map(corpus, removeWords, profanity)


# tdm<- TermDocumentMatrix(corpus)
# freq <- rowSums(as.matrix(tdm))
# sortfreq <-freq[order(-freq)]

unigt <- function(x) NGramTokenizer(x, Weka_control(min = 1, max = 1))
bigt <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
trigt <- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3))
quadgt <- function(x) NGramTokenizer(x, Weka_control(min = 4, max = 4))

unigram <- DocumentTermMatrix(corpus, control = list(tokenize = unigt))
saveRDS(unigram,"unigram.rds")
bigram <- DocumentTermMatrix(corpus, control = list(tokenize = bigt))
saveRDS(bigram,"bigram.rds")
trigram <- DocumentTermMatrix(corpus, control = list(tokenize = trigt))
saveRDS(trigram,"trigram.rds")
quadgram <- DocumentTermMatrix(corpus, control = list(tokenize = quadgt))
saveRDS(quadgram,"quadgram.rds")

sortunigram <-sort(colSums(as.matrix(unigram)), decreasing = TRUE)
unigramDf <- data.frame(words = names(sortunigram), frequency = sortunigram)
unigramDf<-unigramDf[unigramDf$frequency>1,]

sortbigram <-sort(colSums(as.matrix(bigram)), decreasing = TRUE)
bigramDf <- data.frame(words = names(sortbigram), frequency = sortbigram)
bigramDf<-bigramDf[bigramDf$frequency>1,]

sorttrigram <-sort(colSums(as.matrix(trigram)), decreasing = TRUE)
trigramDf <- data.frame(words = names(sorttrigram), frequency = sorttrigram)
trigramDf<-trigramDf[trigramDf$frequency>1,]

sortquadgram <-sort(colSums(as.matrix(quadgram)), decreasing = TRUE)
quadigramDf <- data.frame(words = names(sortquadgram), frequency = sortquadgram)
quadigramDf<-quadigramDf[quadigramDf$frequency>1,]
quadigramDf<-quadigramDf[1:600000,]

saveRDS(unigramDf,"unigramDf.rds")
saveRDS(bigramDf,"bigramDf.rds")
saveRDS(trigramDf,"trigramDf.rds")
saveRDS(quadigramDf,"quadigramDf.rds")

unigramDf<-readRDS("unigramDf.rds")
bigramDf<-readRDS("bigramDf.rds")
trigramDf<-readRDS("trigramDf.rds")
quadigramDf<-readRDS("quadigramDf.rds")


#Probability tables
uniProb<-data.frame(words=unigramDf$words, probability=unigramDf$frequency/sum(unigramDf$frequency))
biProb<-data.frame(words=bigramDf$words, probability=bigramDf$frequency/sum(bigramDf$frequency))
triProb<-data.frame(words=trigramDf$words, probability=trigramDf$frequency/sum(trigramDf$frequency))
quadiProb<-data.frame(words=quadigramDf$words, probability=quadigramDf$frequency/sum(quadigramDf$frequency))

biProb$words<-strsplit(as.character(biProb$words)," ")
mat<-matrix(unlist(biProb$words),ncol=2,byrow=TRUE)
biProb$w1<-mat[,1];biProb$w2<-mat[,2];biProb$prob<-biProb$probability;biProb$words<-NULL;biProb$probability<-NULL

triProb$words<-strsplit(as.character(triProb$words)," ")
mat<-matrix(unlist(triProb$words),ncol=3,byrow=TRUE)
triProb$w1<-mat[,1];triProb$w2<-mat[,2];triProb$w3<-mat[,3];triProb$prob<-triProb$probability;triProb$words<-NULL;triProb$probability<-NULL

quadiProb$words<-strsplit(as.character(quadiProb$words)," ")
mat<-matrix(unlist(quadiProb$words),ncol=4,byrow=TRUE)
quadiProb$w1<-mat[,1];quadiProb$w2<-mat[,2];quadiProb$w3<-mat[,3];quadiProb$w4<-mat[,4]
quadiProb$prob<-quadiProb$probability;quadiProb$words<-NULL;quadiProb$probability<-NULL


#1 split text; calculate input text length
#2 If length is less than 3, choose choose the right Ngram probability table, else, choose the last 3 and 
#the quadigram probability table
#3 compare word 1 with W1, word 2 with W2, etc. get the top 4 matches
#
#4 less Ngram trial
#5 Backoff and smoothing
#
uniProb2<-uniProb[order(-uniProb$prob),]
uniProb2 <- data.table(uniProb2)
biProb2<-biProb[order(-biProb$prob),]
biProb2 <- data.table(biProb2)
triProb2<-triProb[order(-triProb$prob),]
triProb2 <- data.table(triProb2)
quadiProb2<-quadiProb[order(-quadiProb$prob),]
quadiProb2 <- data.table(quadiProb2)
saveRDS(uniProb2,"uniProb2.rds")
saveRDS(biProb2,"biProb2.rds")
saveRDS(triProb2,"triProb2.rds")
saveRDS(quadiProb2,"quadiProb2.rds")
saveRDS(uniProb,"uniProb.rds")
Wpredict<-function(a){
  a<-tolower(a)
  n<-sapply(gregexpr("[[:alpha:]]+", a), function(x) sum(x > 0))

  temp<-data.frame()
if (n==1) {
  e<- word(a,-1)
  #########data.table
  system.time(temp <- biProb2["w1" == e, "w2"])
  ##########dplyr
  system.time(temp <- biProb %>% 
    filter(w1 == e) %>%
    arrange(desc(prob)) %>%
    select(w2))
  ############
}
if (n==2) {
  e<- word(a,c(-2,-1))
  #########data.table
  system.time(temp <- triProb2[w1 == e[1]&w2==e[2],w3])
  ##########dplyr
  system.time(temp<-triProb %>%
    filter(w1==e[1],w2==e[2])%>%
    arrange(desc(prob)) %>%
    select(w3))
  ############
}
if (n>2) {
 e<- word(a,c(-3,-2,-1))
 #########data.table
 system.time(temp <- biProb2["w1" == e[1]&"w2"==e[2]&"w3"==e[3], "w4"])
 ##########dplyr
system.time(temp <- quadiProb %>%
  filter(w1 == e[1], w2 == e[2], w3 == e[3]) %>%
  arrange(desc(prob)) %>%
  select(w4))
############
}
}
