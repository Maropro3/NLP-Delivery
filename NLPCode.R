library(tm)
library(ggplot2)
library(wordcloud)
library(reshape2)
library(SentimentAnalysis)
library(dplyr)
library(tidyr)
library(quanteda)
library(stringr) 
library(tidytext)

#Set Working directory
setwd("C:\\Users\\pablo\\Desktop\\NLP")
#Load CSV
x <- read.csv("PerSent.csv", header = TRUE)
#Clean the dataset from non-useful characters
cleaned_x <- x %>%
  filter(str_detect(DOCUMENT, "^[^>]+[A-Za-z\\d]") | DOCUMENT !="") 
cleaned_x$DOCUMENT <- gsub("[^\u0001-\u007F]+", "", cleaned_x$DOCUMENT)

#Drop columns and generate corpus
y = subset(cleaned_x, select = -c(TITLE,TARGET_ENTITY,MASKED_DOCUMENT,TRUE_SENTIMENT,Paragraph0,Paragraph1,Paragraph2,Paragraph3,Paragraph4,Paragraph5,Paragraph6,Paragraph7,Paragraph8,Paragraph9,Paragraph10,Paragraph11,Paragraph12,Paragraph13,Paragraph14,Paragraph15))
colnames(y) <- c("doc_id", "text") 
require(tm)
corpus <- Corpus(DataframeSource(y))
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
corpus <- tm_map(corpus, toSpace, "\n")

#Plot the 10 most frequent entities
cleaned_x_plot <- cleaned_x %>%
  group_by(TARGET_ENTITY) %>%
  summarise(count = n()) %>%
  top_n(n = 10, wt = count)
ggplot(cleaned_x_plot , aes(reorder(TARGET_ENTITY,count), count))+
  geom_bar(stat="identity")+
  coord_flip() 

#Generate the tdm from corpus
myStopwords = c(stopwords(), "said", "told", "say")

tdm = TermDocumentMatrix(corpus,
                         control=list(stopwords = myStopwords,
                                      removePunctuation = T, 
                                      removeNumbers = T,
                                      stemming = T))


#Plot most frequent words
freq=rowSums(as.matrix(tdm))
high.freq=tail(sort(freq),n=10)
hfp.df=as.data.frame(sort(high.freq))
hfp.df$names <- rownames(hfp.df) 
ggplot(hfp.df, aes(reorder(names,high.freq), high.freq)) +
  geom_bar(stat="identity") + coord_flip() + 
  xlab("Terms") + ylab("Frequency") +
  ggtitle("Term frequencies")


#Generate wordcloud
set.seed(1234)
m <- as.matrix(tdm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.2, 
          colors=brewer.pal(8, "Set2"),scale=c(3.5,0.005))

#TF-IDF Graph and Values
tdm.tfidf = TermDocumentMatrix(corpus,
                         control=list(stopwords = myStopwords,
                                      removePunctuation = T, 
                                      removeNumbers = T,
                                      stemming = T))

freq=rowSums(as.matrix(tdm.tfidf))

plot(sort(freq, decreasing = T),col="blue",main="Word TF-IDF frequencies", xlab="TF-IDF rank", ylab = "TF-IDF")
tail(sort(freq),n=10)



##Sentiment Analysis
sentiment <- analyzeSentiment(tdm, 
                              rules=list(
                                "SentimentGI"=list(
                                  ruleSentiment, loadDictionaryGI()
                                ),
                                "SentimentLM"=list(
                                  ruleSentiment, loadDictionaryLM()
                                )
                              ))

#Obtain the average between the two methods and each column 
sentiment_nom <- transform(sentiment, Average = rowMeans(sentiment, na.rm = TRUE))
meanGI <- mean(sentiment_nom$SentimentGI, na.rm = TRUE)
meanLM <- mean(sentiment_nom$SentimentLM, na.rm = TRUE)
meanAvg <- mean(sentiment_nom$Average, na.rm = TRUE)

#Categorize the results as positive, negative or neutral
sentiment_nom$SentimentGI <- ifelse(sentiment_nom$SentimentGI>0.025, "Positive",ifelse(sentiment_nom$Average < -0.025, "Negative", "Neutral"))
sentiment_nom$SentimentLM <- ifelse(sentiment_nom$SentimentLM>0.025, "Positive",ifelse(sentiment_nom$Average < -0.025, "Negative", "Neutral"))
sentiment_nom$Average <- ifelse(sentiment_nom$Average>0.025, "Positive",ifelse(sentiment_nom$Average < -0.025, "Negative", "Neutral"))
sentiment_nom$Result <- apply(sentiment_nom,1,function(x) names(which.max(table(x))))
sentiment_nom$Target = cleaned_x$TARGET_ENTITY

#Obtain the percentage of each sentiment 
countP <- sum( sentiment_nom$Result == "Positive")
countN <- sum( sentiment_nom$Result == "Negative")
countNN <- sum( sentiment_nom$Result == "Neutral")
countP/3354
countN/3354
countNN/3354
countP <- sum(cleaned_x$TRUE_SENTIMENT == "Positive")
countN <- sum(cleaned_x$TRUE_SENTIMENT == "Negative")
countNN <- sum(cleaned_x$TRUE_SENTIMENT == "Neutral")
countP/3354
countN/3354
countNN/3354

#Plot the sentiments for the top 10 most common targets
topTargets <- dplyr::pull(cleaned_x_plot, TARGET_ENTITY)
topTargets_df <- dplyr::filter(sentiment_nom, Target %in% topTargets)
ggplot(topTargets_df, aes(factor(Target), fill=Result))+
  geom_bar(stat="count", position = "dodge")+
  theme(text = element_text(size=20))




