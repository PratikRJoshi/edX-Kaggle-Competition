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
DescriptionWordsTrain$productline <- as.factor(DescriptionWordsTrain$productline)
DescriptionWordsTrain$sold <- as.factor(DescriptionWordsTrain$sold)
DescriptionWordsTest$productline <- as.factor(DescriptionWordsTest$productline)
library(rpart)
library(rpart.plot)
library(caTools)
library(ROCR)
spl = sample.split(DescriptionWordsTrain$sold, SplitRatio = 0.85)
DescriptionWordsTrain_Train = subset(DescriptionWordsTrain, spl == TRUE)
DescriptionWordsTrain_Test = subset(DescriptionWordsTrain, spl == FALSE)
#DescriptionWordsRFTrain_Train = DescriptionWordsTrain_Train
#DescriptionWordsRFTrain_Test = DescriptionWordsTrain_Test
#DescriptionWordsRFTrain_Test$productline = as.factor(DescriptionWordsRFTrain_Test$productline)
#DescriptionWordsRFTrain_Train$productline = as.factor(DescriptionWordsRFTrain_Train$productline)
library(randomForest)
DescriptionForest = randomForest(sold ~ ., data = DescriptionWordsTrain_Train)
#DescriptionForestPred = predict(DescriptionForest, newdata = DescriptionWordsTrain_Test, type = "response")
DescriptionForestPred = predict(DescriptionForest, newdata = DescriptionWordsTrain_Test, type = "prob")[,2]
ROC.Pred = prediction(DescriptionForestPred, DescriptionWordsTrain_Test$sold)
as.numeric(performance(ROC.Pred, "auc")@y.values)

############################ Need to calculate predictions on full test set from here ###############################
