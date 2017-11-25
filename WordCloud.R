## Assignment 1

#Step : 1
##--------------------------------------------------------##
## Product Name : Amazon Echo Dot
## Product URL : https://www.amazon.com/All-New-Amazon-Echo-Dot-Add-Alexa-To-Any-Room/product-reviews/B01DFKC2SO/ref=cm_cr_dp_d_show_all_top?ie=UTF8&reviewerType=all_reviews
setwd("D:/Omis 670")
getwd()


#Step : 2
##-----------------------------------------------------------------------------------##
#installing the package required for this assignment

install.packages("rvest")
install.packages("SnowballC") 
install.packages("wordcloud")
install.packages("RColorBrewer")
install.packages("tm")

#loading the library

library(rvest)
library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)




#Scraping information from Amazon for 1000 reviews
url<-"https://www.amazon.com/All-New-Amazon-Echo-Dot-Add-Alexa-To-Any-Room/product-reviews/B01DFKC2SO/ref=cm_cr_dp_d_show_all_top?ie=UTF8&reviewerType=avp_only_reviews&pageNumber="
N_pages <- 100
A <- NULL
for (j in 1: N_pages){
  pant <- read_html(paste0(url, j)) 
  B <- cbind(pant %>% html_nodes(".review-text") %>%     html_text()     )
  A <- rbind(A,B)
}
View(A)


##converting it as data frame and saving the file in directory

D<-as.data.frame(A) 
write.csv(file="review.csv", x=D)




#Step : 3
##-------------------------------------------------------------------------------------------------------------##
## Text Cleaning and Running Analysis
##

reviews<-read.csv("review.csv")    #reading the file
corpus<-Corpus(VectorSource(reviews$V1))   #converting the file into VectorSource before creating a corpus
inspect(corpus)
corpus<-tm_map(corpus, function(x) iconv(x, to='UTF-8', sub='byte'))  
corpus<- tm_map(corpus, content_transformer(tolower))  #converting the text to lower

corpus <- tm_map(corpus, removePunctuation)  #removing punctuations
corpus <- tm_map(corpus, removeWords, stopwords("english")) #removing stopwords
corpus <- tm_map(corpus, stripWhitespace) #removing whitespaces
corpus<- tm_map(corpus, removeNumbers) #removing numbers
corpus<- tm_map(corpus, stemDocument) #text stemming to extract the root words


##creating a term document matrix

dtm<-TermDocumentMatrix(corpus, control = list(weighting=weightTf)) 
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
v
df <- data.frame(word = names(v),freq=v)
head(df, 10) 




##Step : 4
##---------------------------------------------------------------------------------------------------------------##
#creating a wordcloud

wordcloud(words = df$word, freq = df$freq, min.freq = 20, max.words=1000, scale=c(4,0.1), random.order=FALSE, rot.per=0.35, colors=brewer.pal(8, "Dark2"))


