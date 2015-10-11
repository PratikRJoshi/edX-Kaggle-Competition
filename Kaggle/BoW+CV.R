setwd("~/Documents/edX/Analytics Edge/Kaggle")
ebayTest = read.csv("ebayiPadTest.csv", stringsAsFactors = FALSE)
ebayTrain = read.csv("ebayiPadTrain.csv", stringsAsFactors = FALSE)
library(tm)
CorpusDescription = Corpus(VectorSource(c(ebayTrain$description, ebayTest$description)))
CorpusDescription = tm_map(CorpusDescription, content_transformer(tolower), lazy = TRUE)
CorpusDescription = tm_map(CorpusDescription, PlainTextDocument, lazy = TRUE)
CorpusDescription = tm_map(CorpusDescription, removePunctuation, lazy = TRUE)
CorpusDescription = tm_map(CorpusDescription, removeWords, stopwords("english"), lazy = TRUE)
CorpusDescription = tm_map(CorpusDescription, stemDocument, lazy = TRUE)
dtm = DocumentTermMatrix(CorpusDescription)
sparse = removeSparseTerms(dtm, 0.99)
DescriptionWords = as.data.frame(as.matrix(sparse))
colnames(DescriptionWords) = make.names(colnames(DescriptionWords))
DescriptionWordsTrain = head(DescriptionWords, nrow(ebayTrain))
DescriptionWordsTest = tail(DescriptionWords, nrow(ebayTest))
DescriptionWordsTrain$sold = ebayTrain$sold
#str(DescriptionWordsTrain)
DescriptionWordsTrain$biddable = ebayTrain$biddable
DescriptionWordsTrain$startprice = ebayTrain$startprice
DescriptionWordsTrain$productline = ebayTrain$productline
DescriptionWordsTest$biddable = ebayTest$biddable
DescriptionWordsTest$startprice = ebayTest$startprice
DescriptionWordsTest$productline = ebayTest$productline
library(rpart)
library(rpart.plot)
library(caTools)
library(ROCR)
str(DescriptionWordsTrain)
DescriptionWordsTrain$sold <- as.factor(DescriptionWordsTrain$sold)
DescriptionWordsTrain$productline <- as.factor(DescriptionWordsTrain$productline)
DescriptionWordsTest$productline <- as.factor(DescriptionWordsTest$productline)
str(DescriptionWordsTrain)
str(DescriptionWordsTest)
spl = sample.split(DescriptionWordsTrain$sold, SplitRatio = 0.85)
DescriptionWordsTrain_Train = subset(DescriptionWordsTrain, spl == TRUE)
DescriptionWordsTrain_Test = subset(DescriptionWordsTrain, spl == FALSE)
library(caret)
numFolds = trainControl(method = "cv", number = 10)
cpGrid = expand.grid(.cp = seq(0.01, 0.5, 0.01))
library(e1071)
DescriptionWordsTrain$sold <- as.factor(DescriptionWordsTrain$sold)
DescriptionWordsTrain_Train$sold <- as.factor(DescriptionWordsTrain_Train$sold)
DescriptionWordsTrain_Test$productline <- as.factor(DescriptionWordsTrain_Test$productline)
DescriptionWordsTrain_Train$productline <- as.factor(DescriptionWordsTrain_Train$productline)
CVTrain = train(sold ~ ., data = DescriptionWordsTrain_Train, method = "rpart", trControl = numFolds, tuneGrid = cpGrid)
CVTrain
bestCVTree = CVTrain$finalModel
prp(bestCVTree)
bestCVTreePred = predict(bestCVTree, newdata = DescriptionWordsTrain_Test)
str(DescriptionWordsTrain_Train)
str(DescriptionWordsTrain_Test)
#################################################### Tried CV but prediction failing ####################################################