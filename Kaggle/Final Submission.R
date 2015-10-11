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
library(rpart)
library(rpart.plot)
#DescriptionWordsTrainCART = rpart(sold ~ ., data = DescriptionWordsTrain, method = "class")
#prp(DescriptionWordsTrainCART)
#str(DescriptionWordsTrain)
library(caTools)
library(ROCR)
spl = sample.split(DescriptionWordsTrain$sold, SplitRatio = 0.85)
DescriptionWordsTrain_Train = subset(DescriptionWordsTrain, spl == TRUE)
DescriptionWordsTrain_Test = subset(DescriptionWordsTrain, spl == FALSE)
DescriptionWordsTrainCART = rpart(sold ~ ., data = DescriptionWordsTrain_Train, method = "class")
#prp(DescriptionWordsTrainCART)
DescriptionWordsTrainPred = predict(DescriptionWordsTrainCART, newdata = DescriptionWordsTrain_Test)
DescriptionWordsTrainPred[1:10,]
DescriptionWordsTrainPred.Prob = DescriptionWordsTrainPred[,2]
DescriptionWordsTrainPredAUC = prediction(DescriptionWordsTrainPred.Prob, DescriptionWordsTrain_Test$sold)
as.numeric(performance(DescriptionWordsTrainPredAUC, "auc")@y.values)
DescriptionWordsTest$biddable = ebayTest$biddable
DescriptionWordsTest$startprice = ebayTest$startprice
DescriptionWordsTest$productline = ebayTest$productline
DescriptionWordsFullTrainCART = rpart(sold ~ ., data = DescriptionWordsTrain, method = "class")
DescriptionWordsFullTestPred = predict(DescriptionWordsFullTrainCART, newdata = DescriptionWordsTest, method = "class")
MyBoWSubmission = data.frame(UniqueID = ebayTest$UniqueID, Probability1 = DescriptionWordsFullTestPred[,2])
write.csv(MyBoWSubmission, "MyBoWSubmission.csv", row.names = FALSE)

