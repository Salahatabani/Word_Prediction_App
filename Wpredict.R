library(stringr)
library(data.table)

# read the probability tables generated by the code in Data_processing.R
uniProb2<-readRDS("../Dataset/Prob_tables/uniProb2.rds")
biProb2<-readRDS("../Dataset/Prob_tables/biProb2.rds")
triProb2<-readRDS("../Dataset/Prob_tables/triProb2.rds")
quadiProb2<-readRDS("../Dataset/Prob_tables/quadiProb2.rds")


Wpredict<-function(a){
  # Wpredict function will be used to predict the next word in the shiny app
  # input: a word or a sentence
  # output: most likely next word and three other likely next words. 
  
  trim <- function (x) gsub("^\\s+|\\s+$", "", x)
  a<-trim(a)
  a <- str_replace_all(a,"[[:punct:]]","") #remove symbols
  a<-tolower(a)
  n<-sapply(gregexpr("[[:alpha:]]+", a), function(x) sum(x > 0)) #count the number of words in the sentence. 
  temp<-data.table()
  
  if (n==1) {
    e<- word(a,-1)
    temp <- biProb2[w1 == e, w2]
  }
  if (n==2) {
    e<- word(a,c(-2,-1))
  
    temp <- triProb2[w1 == e[1]&w2==e[2],w3]

  }
  if (n>2) {
    e<- word(a,c(-3,-2,-1))
    
    temp <- quadiProb2[w1 == e[1]&w2==e[2]&w3==e[3], w4]
 
  }
  
  if (length(temp)>=4) {return(temp[1:4])}
  if (length(temp)<4) {if (n==1) return (c(temp,word(uniProb2[1:4,words])))[1:4]
    if (n==2)  {return (unique(c(temp,Wpredict(word(a,-1))))[1:4])}
    if (n>2)  {return (unique(c(temp,Wpredict(paste(word(a,-2),word(a,-1)))))[1:4])}
  }
  
}