
#####------------------------
#        Part1 Pre-process
#####------------------------

setwd("D:/R/R-profile/Text-mining Ass")
#load the library

library(wordcloud)
library(mgsub)
library(stopwords)
library(ggplot2)
library(tm)
library(stringr)
library(qdap)
library(lubridate)
library(caret)

###Read dataset and combination
setwd("D:/R/R-profile/Text-mining Ass")
#list.files命令将input文件夹下所有文件名输入a
ba = list.files("tweets.ba")  
#用paste命令构建路径变量dir
dir = paste("./tweets.ba/",ba,sep="")
#读取dir长度，也就是文件夹下的文件个数
n = length(dir)
#读入第一个文件内容（可以不用先读一个，但是为了简单，省去定义data.frame的时间，我选择先读入一个文件。
ba.data = readRDS(file = dir[1])
for (i in 2:n){
  new.data = readRDS(file = dir[i])
  ba.data = rbind(ba.data,new.data)
}

library(data.table)
ba.data <- data.table(ba.data)

#delete repeat tweets
index <- duplicated(ba.data$status_id)
ba.data <- ba.data[!index,]

# subset data in Dec
ba.data$created_at<-as.POSIXct(ba.data$created_at,format="%Y%m%d % H:%M")
ba.data12<-subset(ba.data,created_at>=as.POSIXct('2019-12-01 00:00:00')
                  &created_at<=as.POSIXct('2019-12-31 23:59:59'))

#Order according to time
ba.data12.o <- ba.data12[order(ba.data12$created_at),]

#check data to see if there are missing values
length(which(!complete.cases(ba.data12.o$text)))

#have a look of top 10
head(ba.data12.o,n=10)
#tailor-made a few things

#A function changes all to lower case (and return NA stead of error if it is a special character)
#Return NA instead of tolower error
tryTolower <-function(x){
  #return NA when there is an error
  y=NA
  #tryCatch error
  try_error=tryCatch(tolower(x),error=function(e) e)
  #if not an error 
  if (!inherits(try_error, 'error'))
    y=tolower(x)
  return(y)
}

# create a pre-processing function using gsub 
custom.gusb <- c('flight','can','get','fli','just',
                 'one','now','pleas','amp','take',
                 'plea','thank','hi','dont','tri',
                 'use','due','ba')
clean.gsub<-function(corpus){
  corpus$text <-gsub("@\\S*", "", corpus$text) 
  corpus$text <-gsub("https\\S*", "", corpus$text)
  corpus$text <- gsub("[’]" , "",corpus$text)
  corpus$text <- gsub("[,]" , "",corpus$text)
  corpus$text <- gsub("[[:punct:]]" , "",corpus$text)
  return(corpus)
}
ba.data12.o <- clean.gsub(ba.data12.o)

#create my stop words list
custom.stopwords<-c(stopwords('english'),'flight','can','get','fli','just',
                                         'one','now','pleas','amp','take',
                                         'plea','thank','hi','dont','tri',
                                         'use','due','ba','paper攼<u+3e64>愼<u+3e30>戼<u+3e64>dnt')

#create a pre-processing function using tm functions and the above two
clean.corpus<-function(corpus){
  corpus<-tm_map(corpus,content_transformer(tryTolower))
  corpus<-tm_map(corpus,removeWords,custom.stopwords)
  corpus<-tm_map(corpus,removePunctuation)
  corpus<-tm_map(corpus,stripWhitespace)
  corpus<-tm_map(corpus,removeNumbers)
  corpus<-tm_map(corpus,stemDocument, language = "english")
  return(corpus)
}


#define the tweets object
#when measuring va using the following codes
# the.corpus <- VCorpus(VectorSource(va.data12.o$text))
the.corpus <- Corpus(VectorSource(ba.data12.o$text))

#clean the tweets with the function created earlier
the.corpus<-clean.gsub(the.corpus)
the.corpus<-clean.corpus(the.corpus)

the.corpus<-tm_map(the.corpus,removePunctuation)
the.corpus<-tm_map(the.corpus,removeWords,c('flight','can','get','fli','just',
                                            'one','now','pleas','amp','take',
                                            'plea','thank','hi','dont','tri',
                                            'use','due','ba','paper攼<u+3e64>愼<u+3e30>戼<u+3e64>dnt'))

the.corpus <- Corpus(VectorSource(the.corpus))
head(the.corpus)
#Create the term document matrix
tdm <- DocumentTermMatrix(the.corpus,control=list(weighting=weightTf))

#remove sparse terms from a doucment if the sparsity is more than 99%
tdm.n<-removeSparseTerms(tdm, 0.99)

#redefine it as matrix for easy to computation
tdm.tweets<-as.matrix(tdm.n)

#save the pre-processed document term matrix
saveRDS(tdm.tweets, file="matrix.tweets")

#check dimension of the tweets
dim(tdm.tweets)

#check term frequency
term.freq<-colSums(tdm.tweets)

#create a dataframe with the term and then the frequency as the second column
freq.df<-data.frame(word=names(term.freq),frequency=term.freq)
freq.df<-freq.df[order(freq.df[,2],decreasing=T),]
freq.df[1:20,]


#Plot word frequencies when frequency is higher than 500
hp <- ggplot(subset(freq.df, term.freq>500), aes(word, frequency))    
hp <- hp + geom_bar(stat="identity")   
hp <- hp + theme(axis.text.x=element_text(angle=45, hjust=1))   
hp   


