
#####------------------------
#    Part4 Improved Bayes
#####------------------------


install.packages("caTools")
install.packages("MASS")


library(RTextTools)
library(e1071)
library(dplyr)
library(tm)
library(naivebayes)
library(caret)
library(MASS)
library(caTools)

#create the vector format
trainvector <- as.vector(traindata$V1)
testvector <- as.vector(testdata$V1)

#create corpus for data
traincorpus <- VCorpus(VectorSource(trainvector))
testcorpus <- VCorpus(VectorSource(testvector))

#create the term-by-document matrix
trainmatrix <- DocumentTermMatrix(traincorpus)
testmatrix <- DocumentTermMatrix(testcorpus)



# feature selection
dim(trainmatrix)

# restrict the DTM to use only the frequent words using the ¡®dictionary¡¯ option
fivefreq <- findFreqTerms(trainmatrix, 5)
length((fivefreq))

# Use only 5 most frequent words (fivefreq) to build the DTM
trainmatrix.nb <- DocumentTermMatrix(traincorpus, control=list(dictionary = fivefreq))
dim(trainmatrix.nb)

testmatrix.nb <- DocumentTermMatrix(testcorpus, control=list(dictionary = fivefreq))
dim(testmatrix.nb)


# Function to convert the word frequencies to yes (presence) and no (absence) labels
convert_count <- function(x) {
  y <- ifelse(x > 0, 1,0)
  y <- factor(y, levels=c(0,1), labels=c("No", "Yes"))
  y
}

# Apply the convert_count function to get final training and testing DTMs
trainNB <- apply(trainmatrix.nb, 2, convert_count)
testNB <- apply(testmatrix.nb, 2, convert_count)

# Train the classifier
system.time( classifier <- naiveBayes(as.matrix(trainNB), as.factor(traindata$V2)) )

# Use the NB classifier we built to make predictions on the test set.
system.time( pred <- predict(classifier, testNB) )

# Create a truth table by tabulating the predicted class labels with the actual class labels 
table("Naive Bayes-Predictions"= pred,  "Actual" = testdata$V2 )

### ROC CURVE for Bayes

install.packages("pROC")
library(pROC)
modelroc <- roc(as.numeric(testdata$V2),as.numeric(pred))
plot(modelroc, print.auc=TRUE, auc.polygon=TRUE, grid=c(0.1, 0.2),
     grid.col=c("green", "red"), max.auc.polygon=TRUE,
     auc.polygon.col="skyblue", print.thres=TRUE)

### test in ba

dtm.ba.nb <- DocumentTermMatrix(ba.bayes.corpus, control=list(dictionary = fivefreq))
ba.NB <- apply(dtm.ba.nb, 2, convert_count)
system.time( ba.pred <- predict(classifier, ba.NB) )
table("Predictions"= ba.pred)

