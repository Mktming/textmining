
#####-----------------------------------
#             Part 3 SVM
#####-----------------------------------
setwd("D:/R/R-profile/Text-mining Ass")
pdata <- read.csv("la.csv", header = T)

install.packages('e1071')
install.packages('SparseM')
install.packages('caret')

library(caret)
library(e1071)
library(Matrix)
library(SparseM)
library(tm)
library(SnowballC)
library(naivebayes)
###---Seperate&Train
#split into testing and training
set.seed(123)
split<-createDataPartition(pdata$V2, p=0.7, list=FALSE)
traindata<-pdata[split,]
testdata<-pdata[-split,]

#function to deal with unmatched terms
match.matrix<-function(text.col,original.matrix=NULL,weighting=weightTf)
{
  control<-list(weighting=weighting)
  training.col<-
    sapply(as.vector(text.col,mode="character"),iconv,
           to="UTF8",sub="byte")
  corpus<-VCorpus(VectorSource(training.col))
  matrix<-DocumentTermMatrix(corpus,control=control);
  if( !is.null(original.matrix)){
    terms<-
      colnames(original.matrix[,
                               which(!colnames(original.matrix) %in% colnames(matrix))])
    weight<-0
    if(attr(original.matrix,"weighting")[2]=="tfidf")
      weight <-0.000000001
    amat<-matrix(weight,nrow=nrow(matrix),
                 ncol=length(terms))
    colnames(amat)<-terms
    rownames(amat)<-rownames(matrix)
    fixed<-as.DocumentTermMatrix(
      cbind(matrix[,which(colnames(matrix) %in%
                            colnames(original.matrix))],amat),
      weighting=weighting)
    matrix<-fixed
  }
  matrix<-matrix[,sort(colnames(matrix))]
  gc()
  return(matrix)
}

#function to clean the data
#create a pre-processing function using tm functions and the above two
clean.d<-function(x){
  x<-tolower(x)
  x<-removeWords(x,stopwords('en'))
  x<-removePunctuation(x)
  x<-stripWhitespace(x)
  return(x)
}

#clean the training data and build a term by document matrix
traindata$V1<-clean.d(traindata$V1)
testdata$V1<-clean.d(testdata$V1)

###SVM training & predicting
#clean the training data and build a term by document matrix
clean.train<-clean.d(traindata$V1)
train.dtm<-match.matrix(clean.train,weighting=tm::weightTfIdf)

train.matrix<-as.matrix(train.dtm)
train.matrix<-Matrix(train.matrix,sparse=T)

SVM.model <- svm(x=train.matrix, y=as.factor(traindata$V2),
                 kernel="linear")

#clean the testing data and build a term by document matrix
clean.test<-clean.d(testdata$V1)
test.dtm<-match.matrix(clean.test,weighting=tm::weightTfIdf,
                       original.matrix=train.dtm )

test.matrix<-as.matrix(test.dtm)
test.matrix<-Matrix(test.matrix,sparse=T)

#apply the model into the testing set and see the results
preds<-predict(SVM.model,as.matrix(test.matrix))
table("SVM-Prediction" = preds, "Actual" = testdata$V2)

### ROC CURVE for SVM
install.packages("pROC")
library(pROC)
modelroc <- roc(as.numeric(testdata$V2),as.numeric(preds))
plot(modelroc, print.auc=TRUE, auc.polygon=TRUE, grid=c(0.1, 0.2),
     grid.col=c("green", "red"), max.auc.polygon=TRUE,
     auc.polygon.col="skyblue", print.thres=TRUE)

### SVM-predicting ba
set.seed(123)
clean.ba <- clean.d(ba.data12.o$text)
ba.dtm<-match.matrix(clean.ba,weighting=tm::weightTfIdf,
                       original.matrix=train.dtm )

ba.matrix<-as.matrix(ba.dtm)
ba.matrix<-Matrix(ba.matrix,sparse=T)

preds.ba<-predict(SVM.model,as.matrix(ba.matrix))
table(preds.ba)



