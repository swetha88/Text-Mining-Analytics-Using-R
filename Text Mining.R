library(tm) # Library for text mining Framework
library(dplyr) 
library(ggplot2)
library(scales)
set.seed(1)

Doc1.TrainPath = system.file("texts",package="tm")

Temp1 <- DirSource(Doc1.TrainPath)
 
Doc1.TestPath = system.file("texts",package="tm")

Temp2 <- DirSource(Doc1.TestPath)

Doc2.TrainPath = system.file("texts",package="tm")

Temp3 <- DirSource(Doc2.TrainPath)

Doc2.TestPath = system.file("texts",package="tm")

Temp4 <- DirSource(Doc2.TestPath)

Doc1.Train <- Corpus(URISource(Temp1$filelist[1:100]),readerControl=list(reader=readPlain))

Doc1.Test <- Corpus(URISource(Temp2$filelist[1:100]),readerControl=list(reader=readPlain))

Doc2.Train <- Corpus(URISource(Temp3$filelist[1:100]),readerControl=list(reader=readPlain))

Doc2.Test <- Corpus(URISource(Temp4$filelist[1:100]),readerControl=list(reader=readPlain))

Doc<- c(Doc1.Train,Doc1.Test,Doc2.Train,Doc2.Test)

#getTransformations() 

#preprocessing
corpus.temp <- tm_map(Doc, removePunctuation) # to Remove Punctuations
corpus.temp <- tm_map(corpus.temp, stemDocument, language = "english")# To Perform Stemming
corpus.temp <- tm_map(corpus.temp,removeNumbers) # To Remove numbers


docTermMatrix = DocumentTermMatrix(corpus.temp, 
                                   control = list(minWordLength = 3,
                                                  minDocFreq = 5)) 
dim(docTermMatrix)
#dtm <- as.matrix(DocumentTermMatrix(corpus.temp)) # Document term matrix

# Text Classification
library(class) # Using kNN 
train.doc <- docTermMatrix[c(1:100,201:300),]
test.doc <- docTermMatrix[c(101:200,301:400),]

Tags <- factor(c(rep("RplaceTags",100), rep("RplaceTags",100))) # Tags 
prob.test<- knn(train.doc, test.doc, Tags, k = 2, prob=TRUE) # k-number of neighbors considered

# Display Classification Results
a <- 1:length(prob.test)
b <- levels(prob.test)[prob.test]
c <- attributes(prob.test)$prob
result <- data.frame(Doc=a, Predict=b,Prob=c)
result
sum(c)/length(Tags) # Overall probability

sum(prob.test==Tags)/length(Tags) # % Correct Classification
