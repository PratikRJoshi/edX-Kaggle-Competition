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
MyBoWSubmission = data.frame(UniqueID = ebayTest$UniqueID, Probability1 = DescriptionWordsFullTestPred)
write.csv(MyBoWSubmission, "MyBoWSubmission.csv", row.names = FALSE)
##########################  Works fine till here except for the error in submission #################################

#################################################### Tried RF but prediction failing ####################################################
DescriptionWordsRFTrain_Train = DescriptionWordsTrain_Train
DescriptionWordsRFTrain_Test = DescriptionWordsTrain_Test
DescriptionWordsRFTrain_Test$productline = as.factor(DescriptionWordsRFTrain_Test$productline)
DescriptionWordsRFTrain_Train$productline = as.factor(DescriptionWordsRFTrain_Train$productline)
library(randomForest)
DescriptionForest = randomForest(sold ~ ., data = DescriptionWordsRFTrain_Train)
DescriptionForestPred = predict(DescriptionForest, newdata = DescriptionWordsRFTrain_Test)
str(DescriptionWordsRFTrain_Test)
levels(DescriptionWordsRFTrain_Test$productline) <- levels(DescriptionWordsRFTrain_Train)
levels(DescriptionWordsRFTrain_Test)[is.na(levels(DescriptionWordsRFTrain_Test))] <- "NA"
DescriptionForestPred = predict(DescriptionForest, newdata = DescriptionWordsRFTrain_Test)


#########################################################################################################################################


#################################################### Tried CV but prediction failing ####################################################
library(caret)
numFolds = trainControl(method = "cv", number = 10)
cpGrid = expand.grid(.cp = seq(0.01, 0.5, 0.01))
library(e1071)
CVTrain = train(sold ~ ., data = DescriptionWordsTrain_Train, method = "rpart", trControl = numFolds, tuneGrid = cpGrid)
CVTrain
bestCVTree = CVTrain$finalModel
prp(bestCVTree)
bestCVTreePred = predict(bestCVTree, newdata = DescriptionWordsTrain_Test)
str(DescriptionWordsTrain_Train)
str(DescriptionWordsTrain_Test)

#########################################################################################################################################

DescriptionWordsCART = rpart(sold ~ ., data = DescriptionWordsTrain, method = "class")
DescriptionCARTPredTest = predict(DescriptionWordsCART, newdata = DescriptionWordsTest)
DescriptionWordsCART = rpart(sold ~ ., data = ebayTrain, method = "class")
DescriptionCARTPredTest = predict(DescriptionWordsCART, newdata = ebayTest)
DescriptionWordsTest$biddable = ebayTest$biddable
DescriptionWordsTest$startprice = ebayTest$startprice
DescriptionWordsTest$productline = ebayTest$productline
DescriptionCARTPredTest = predict(DescriptionWordsCART, newdata = ebayTest)
str(DescriptionWordsTrain)
str(DescriptionWordsTest)
DescriptionCARTPredTest = predict(DescriptionWordsCART, newdata = DescriptionWordsTest)
DescriptionWordsTest$description = ebayTest$description
DescriptionCARTPredTest = predict(DescriptionWordsCART, newdata = DescriptionWordsTest)
