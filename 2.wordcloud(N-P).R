
#####-------------------------
#      Part2 wordcloud(n-p) 
#####-------------------------
setwd("D:/R/R-profile/text-mining/social network analysis")
#use the polarity function in the package qdap
#please install and load the package

install.packages("qdap")
install.packages("ggplot2")
install.packages("ggthemes")
install.packages("wordcloud")
install.packages("sentimentr")
install.packages("syuzhet")
install.packages("lubridate")
install.packages("reshape2")
install.packages("dplyr")
install.packages("zoo")

library(sentimentr)
library(syuzhet)
library(lubridate)
library(reshape2)
library(dplyr)
library(zoo)
library(qdap)
library(ggplot2)
library(ggthemes)
library(wordcloud)

#---wordcloud(n-p)---#

#the key.pol is a dataset in qdap keeping some polarity words
#here we are going to extract this list but also add some new words
pos.new<-scan("p-emojis.txt",what = "character",comment.char = "")
pos.old<-subset(as.data.frame(key.pol),key.pol$y==1)
pos.words<-c(pos.new,pos.old[,1])
neg.new<-scan("n-emojis.txt",what = "character",comment.char = "")
neg.old<-subset(as.data.frame(key.pol),key.pol$y==-1)
neg.words<-c(neg.new,neg.old[,1])
all.polarity<-sentiment_frame(pos.words,neg.words,1,-1)

#check the polarity using a sentence
#the function calculate using the following function
#sum(positive+negative+amplifier)/sqrt(number of words)
polarity('it is good',polarity.frame=all.polarity)
polarity('It is very good',polarity.frame=all.polarity)
polarity('it is bad',polarity.frame=all.polarity)
polarity('It is very bad',polarity.frame=all.polarity)

#read a file containing 1,000 airbnb reviews about stays in Boston
options(stringsAsFactors=F)

bos.pol<-polarity(ba.data12.o$text)

#plot the histogram of the polarity
ggplot(bos.pol$all,aes(x=polarity,y=..density..))+theme_gdocs()+
  geom_histogram(binwidth = .25,
                 fill="darkred", colour="grey60", size=.2) + 
  geom_density(size=.75)

#save the polarity back to the orginial dataset
ba.data12.o$screen_name<-scale(bos.pol$all$polarity)
#plotting wordclouds (one for positive and one for negative)

pos.comments<-subset(ba.data12.o$text,ba.data12.o$screen_name>0)
neg.comments<-subset(ba.data12.o$text,ba.data12.o$screen_name<0)

#compress it into a document with only two components
#(one for positive and one for negative)
pos.terms<-paste(pos.comments, collapse=" ")
neg.terms<-paste(neg.comments, collapse=" ")
all.terms<-c(pos.terms,neg.terms)
all.corpus<-VCorpus(VectorSource(all.terms))

#create the term by document matrix using tfidf
all.tdm<-TermDocumentMatrix(all.corpus,
                            control=list(weighting=weightTf,
                                         removePunctuation=TRUE, 
                                         stopwords=stopwords(kind='en')))

#switch to matrix and add column names
all.tdm.m<-as.matrix(all.tdm)
colnames(all.tdm.m)<-c('positive','negative')

#build wordclouds
comparison.cloud(all.tdm.m,max.words=100)
colors=c('darkgreen','darkpurple')

#---Emotional Valence---#

#read files named tweet.ba.12 with rename tweet.b.12
ba.text <-c(ba.data12.o$text)

#obtain sentiment scores
ba<-get_nrc_sentiment(ba.text)
head(ba) 

#Classify according to sentences
sentiment(ba.text) 

#Seperate them
sentiment_by(ba.text) 

#Find N/P words in every sentence
extract_sentiment_terms(ba.text) #找出每段话中的好词和坏词

#Plot the figure
plot(sentiment(ba.text))


