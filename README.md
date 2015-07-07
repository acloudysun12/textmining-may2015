# textmining-may2015
Adam Sun's text mining overview from the Charlotte R Users Group, May 18, 2015

library(tm)
library(SnowballC)
library(wordcloud)
library(topicmodels)
library(ggplot2)
library(twitteR)
 
#The dataset I will use for the public code is a group of twitter feeds collected around R and data mining.
#Download "rdm.Tweets-201306.RData" and "rdm.Tweets.RData" at the following website: http://www.rdatamining.com/data
#Move the file to the directory you're working in
 
#Set the working directory to what yours is
setwd("C:/Users/ZK7QUTX/Documents")
 
#Read the files into R
 
#These are tweets pulled on June 2013
load("R Meetups Presentation/rdmTweets-201306.RData")
 
#These are tweets pulled on April 2012
load("R Meetups Presentation/rdmTweets.RData")
 
#Inspect the objects to see what these tweets look like.
rdmTweets[[1]]
tweets[[1]]
class(tweets[[1]])
 
#We only want the "text" object from the list of tweets, so we paste these tweets together into a large data frame to analyze.
TweetsTable<-NULL
i<-1
for (i in 1:length(tweets)){
  TweetRow<-as.character(tweets[[i]]$text) 
  TweetsTable<-rbind(TweetsTable, TweetRow)
}
 
#Do it with rdmTweets as well, but pasting on the same TweetsTable
i<-1
for (i in 1:length(rdmTweets)){
  TweetRow<-as.character(rdmTweets[[i]]$text) 
  TweetsTable<-rbind(TweetsTable, TweetRow)
}
 
#Ensure that the TweetsTable looks appropriate
View(TweetsTable)
colnames(TweetsTable)<-"Body"
 
#NOTE: we now have new data to use for our text mining analysis instead of the BofA news feed.
#In the code below, please change the nomenclature to match that of the tweets data
#e.g. Change IndustryArticlesInDir$Body to TweetsTable$Body.
 
#Use the 'DataframeSource" method of reading something as a text corpus to analyze
IndustryNews<-as.data.frame(IndustryArticlesInDir$Body)
IndustryNewsCorpus<-Corpus(DataframeSource(IndustryNews))
rm(IndustryNews)
 
#Use some pre-processing functions to clean up our text.
toSpaceFxn <- content_transformer(function(x, pattern) gsub(pattern, " ", x))
IndustryNewsCorpus <- tm_map(IndustryNewsCorpus, toSpaceFxn, "/|@|\\|")
IndustryNewsCorpus <- tm_map(IndustryNewsCorpus, toSpaceFxn, "'")
IndustryNewsCorpus <- tm_map(IndustryNewsCorpus, content_transformer(tolower))
IndustryNewsCorpus <- tm_map(IndustryNewsCorpus, removeNumbers)
IndustryNewsCorpus <- tm_map(IndustryNewsCorpus, removePunctuation)
IndustryNewsCorpus <- tm_map(IndustryNewsCorpus, removeWords, stopwords("english"))
IndustryNewsCorpus <- tm_map(IndustryNewsCorpus, stripWhitespace)
 
#Examine one of the articles post cleanup
IndustryNewsCorpus[[2]]
 
#Express these news articles as a matrix of terms
IndustryNewsTDM<-TermDocumentMatrix(IndustryNewsCorpus)
 
#Convert the DTM object to a matrix object
IndustryNewsTDMMatrix<-as.matrix(IndustryNewsTDM)
 
#The matrix has some weird characters that can't be cleaned.
View(IndustryNewsTDMMatrix[1:100,1])
 
#Use the 'function' below to do the final cleanup
FirstLetterTDM<-as.matrix(rownames(IndustryNewsTDMMatrix))
FirstLetterTDM<-as.matrix(substring(FirstLetterTDM[1:nrow(FirstLetterTDM),], 1,1))
 
#Find row in TermDocMatrx where words start with lette "a" instead of symbols
i<-1
while (FirstLetterTDM[i,]!="a"){
  i <- i+1
}
 
#New version of IndustryNewsTDMMatrix skips over the other words
IndustryNewsTDMMatrix<-IndustryNewsTDMMatrix[i:nrow(IndustryNewsTDMMatrix),]
View(IndustryNewsTDMMatrix)
 
#Write this new version of the TDMMatrix as Excel file
#### write.csv(IndustryNewsTDMMatrix, "R Meetups Presentation/IndustryNewsTDMMatrix.csv")
rm(FirstLetterTDM)
 
#Want to read about an article?
#We can express the specific article above as a matrix of terms:
ArticleOfInterest<-cbind(rownames(IndustryNewsTDMMatrix), IndustryNewsTDMMatrix[,2])
ArticleOfInterest<-ArticleOfInterest[order(ArticleOfInterest[,2], decreasing = TRUE),]
ArticleOfInterest<-ArticleOfInterest[1:20,]
View(ArticleOfInterest)
 
#Compare it to the document where this came from
IndustryNewsCorpus[[2]]
 
rm(ArticleOfInterest)
 
######################### Start KW Counts/Metrics ########################################
 
 
#We can look at just total count of keyword
TotalCountsTDM<-as.matrix(rowSums(IndustryNewsTDMMatrix))
colnames(TotalCountsTDM)<-"Total Counts"
 
#We can also look at total # of occurrences (article count) of the keyword
IndustryNewsTDMMatrix10<-IndustryNewsTDMMatrix
IndustryNewsTDMMatrix10[IndustryNewsTDMMatrix10>0]<-1
TotalCountOccurrencesTDM<-as.matrix(rowSums(IndustryNewsTDMMatrix10))
colnames(TotalCountOccurrencesTDM)<-"Total Count Occurrences"
 
 
#We can look at count/occurrence
IndustryNewsKWsCountsTbl<-cbind(TotalCountsTDM,
                             TotalCountOccurrencesTDM,
                             round(as.matrix(TotalCountOccurrencesTDM/ncol(IndustryNewsTDMMatrix)),3),
                             round(as.matrix(TotalCountsTDM/TotalCountOccurrencesTDM),3))
colnames(IndustryNewsKWsCountsTbl)<-c("Total Counts", "Total Count Occurrences", "Porp of Occurrences in Articles", "Count/Occurrence")
 
#Save the table we created
#### write.csv(IndustryNewsKWsCountsTbl, "R Meetups Presentation/IndustryNewsKWsCountsTable.csv")
 
#View the table we created
View(IndustryNewsKWsCountsTbl)
 
#Delete unnecessary data
rm(IndustryNewsTDMMatrix10)
rm(TotalCountsTDM)
rm(TotalCountOccurrencesTDM)
 
#View the table ordered by total count
IndustryNewsKWsCountsTbl<-IndustryNewsKWsCountsTbl[order(IndustryNewsKWsCountsTbl[,1], decreasing = TRUE),]
View(IndustryNewsKWsCountsTbl)
 
#View the table ordered by total count
IndustryNewsKWsCountsTbl<-IndustryNewsKWsCountsTbl[order(IndustryNewsKWsCountsTbl[,4], decreasing = TRUE),]
View(IndustryNewsKWsCountsTbl)
 
#Save Top 100 Words by Count separately
IndustryNewsKWsCountsTbl<-IndustryNewsKWsCountsTbl[order(IndustryNewsKWsCountsTbl[,1], decreasing = TRUE),]
IndustryNewsKWsTop100byCount<-IndustryNewsKWsCountsTbl[1:100,]
IndustryNewsKWsTop100byCount<-IndustryNewsKWsTop100byCount[order(IndustryNewsKWsTop100byCount[,4], decreasing = TRUE),]
View(IndustryNewsKWsTop100byCount)
 
#Save Top 100 Words by Count/Occurrence separately
IndustryNewsKWsCountsTbl<-IndustryNewsKWsCountsTbl[order(IndustryNewsKWsCountsTbl[,4], decreasing = TRUE),]
IndustryNewsKWsTop100byCountPerInstance<-IndustryNewsKWsCountsTbl[1:100,]
View(IndustryNewsKWsTop100byCountPerInstance)
 
#Save Top 100 Words by Count/Occurrence (not including 1 article occurrence) separately
IndustryNewsKWsCountsTbl<-IndustryNewsKWsCountsTbl[order(IndustryNewsKWsCountsTbl[,4], decreasing = TRUE),]
IndustryNewsKWsTop100byCountPerInstanceNoSparse<-IndustryNewsKWsCountsTbl[IndustryNewsKWsCountsTbl[,2]>1,]
IndustryNewsKWsTop100byCountPerInstanceNoSparse<-IndustryNewsKWsTop100byCountPerInstanceNoSparse[1:100,]
View(IndustryNewsKWsTop100byCountPerInstanceNoSparse)
 
#View wordclouds of the four tables. 3 from above, and the fourth as completely random
#par(mfrow=c(2,2))
 
wordcloud(row.names(IndustryNewsKWsTop100byCount),
          as.numeric(IndustryNewsKWsTop100byCount[,1]),
          random.order = FALSE, max.words = 100, colors=brewer.pal(6, "Dark2"))
 
wordcloud(row.names(IndustryNewsKWsTop100byCountPerInstance),
          as.numeric(IndustryNewsKWsTop100byCountPerInstance[,1]),
          random.order = FALSE, max.words = 100, colors=brewer.pal(6, "Dark2"))
 
wordcloud(row.names(IndustryNewsKWsTop100byCountPerInstanceNoSparse),
          as.numeric(IndustryNewsKWsTop100byCountPerInstanceNoSparse[,1]),
          random.order = FALSE, max.words = 100, colors=brewer.pal(6, "Dark2"))
 
wordcloud(row.names(IndustryNewsKWsCountsTbl),
          as.numeric(IndustryNewsKWsCountsTbl[,1]),
          random.order = TRUE, max.words = 100, colors=brewer.pal(6, "Dark2"))
 
#Remove unnecessary data
rm(IndustryNewsKWsTop100byCountPerInstance)
 
 
######################### End KW Counts/Metrics ########################################
 
 
 
 
 
########################## Start Topics Mining with topicmodels/clustering #################################
 
#Remove words we do not want from corpus. Done manually
IndustryNewsCorpus <- tm_map(IndustryNewsCorpus, removeWords,
                             c("said", "bank", "banks", "will", "new", "year", "can", "also",
                               "may", "make", "like", "now", "since", "even", "still", "just",
                               "get", "use", "says", "say"))
IndustryNewsCorpusStemmed <- tm_map(IndustryNewsCorpus, stemDocument, language = "english")
 
#Stem the document
IndustryNewsDTMStemmed<-DocumentTermMatrix(IndustryNewsCorpusStemmed)
 
#Run the TopicModels results and view topics
#### TopicModelsResults<-LDA(IndustryNewsDTMStemmed[1:800,], control = list(alpha = 0.1), k = 10)
##### FreqTopics<-topics(TopicModelsResults)
##### View(FreqTopics)
##### FreqTerms <- terms(TopicModelsResults, 20)
 
#Read the results into a csv file for future use
##### write.csv(FreqTerms, "R Meetups Presentation/TopicsModelsTopics.csv")
 
FreqTermsTopics<-read.csv("R Meetups Presentation/TopicsModelsTopics.csv", header = TRUE)
FreqTermsTopics<-FreqTermsTopics[,2:ncol(FreqTermsTopics)]
View(FreqTermsTopics)
 
#Use 'posterior' function to predict topic models
##### postpredictions<-posterior(TopicModelsResults, newdata = IndustryNewsDTMStemmed[-c(1:800),])
##### round(postpredictions$topics[1:10,], digits = 3)
##### write.csv(round(postpredictions$topics[1:10,], digits = 3), "R Meetups Presentation/TopicModelsPredictions.csv")
 
#Read back the predictiosn and view them.
TopicsPredictions<-read.csv("R Meetups Presentation/TopicModelsPredictions.csv", header = TRUE)
View(TopicsPredictions)
 
#Now, we try clustering by top 50 most 'meaningful' words in each article
#We want TFIDF table. We use raw frequency for tf (or we can use other ways to express frequency, like binary)
##### IndustryNewsDTMMatrixStemmed<-as.matrix(IndustryNewsDTMStemmed)
##### TFTable<-IndustryNewsDTMMatrixStemmed
 
#For idf table, we want 1: total num of documents and 2: how many documents each word appears in
##### TotalNumDocs<-nrow(TFTable)
##### TFTable10Ct<-TFTable
##### TFTable10Ct[TFTable10Ct>0] <- 1
##### DocCtOfTermsTable<-as.matrix(colSums(TFTable10Ct))
 
#Before we do anymore, let's eliminate words that appear on 1 document ONLY, as they are probably outliers for text.
##### DocCtOfTermsTable[DocCtOfTermsTable<=1]<-0
##### DocCtOfTermsTable[DocCtOfTermsTable>0] <- 1
 
#Eliminate all the keywords that only appear in 1 article only.
##### TFTable<-TFTable[,as.logical(DocCtOfTermsTable)]
 
#Now, start previous exercise again. For idf table, we want 1: total num of documents and 2: how many documents each word appears in
##### TotalNumDocs<-nrow(TFTable)
##### TFTable10Ct<-TFTable
##### TFTable10Ct[TFTable10Ct>0] <- 1
##### DocCtOfTermsTable<-as.matrix(colSums(TFTable10Ct))
##### IDFTable<-TotalNumDocs/DocCtOfTermsTable
##### IDFTable<-t(log(IDFTable))
 
##### rm(TFTable10Ct)
##### rm(DocCtOfTermsTable)
 
#Now, we algorithmically create our tf-idf table:
##### TFIDFTable<-NULL
##### i<-0
##### for (i in 1:nrow(TFTable)){
#####   TFIDFColumn<-as.matrix(TFTable[i,]*IDFTable)
#####   TFIDFTable<-rbind(TFIDFTable, TFIDFColumn)
##### }
 
##### TFIDFTable<-as.matrix(TFIDFTable)
##### DTMTFIDFTable<-TFIDFTable
##### rownames(DTMTFIDFTable)<-IndustryArticlesTitles
 
#Write this to csv file so we can easily import it later on.
##### write.csv(DTMTFIDFTable, "R Meetups Presentation/DTMTFIDFTableForTopicsClustering.csv")
 
##### rm(TFIDFTable)
 
#Read csv file back in and continue analysis
##### DTMTFIDFTable<-read.csv("R Meetups Presentation/DTMTFIDFTableForTopicsClustering.csv", header = TRUE)
##### rownames(DTMTFIDFTable)<-DTMTFIDFTable[,1]
##### DTMTFIDFTable<-DTMTFIDFTable[,2:ncol(DTMTFIDFTable)]
 
#Create a top-20 most prevalent KW in each document, and 'delete' the other columns (i.e. set value to 0)
##### Top20KWsDTMTFIDFTable<-matrix(0, nrow = nrow(DTMTFIDFTable),ncol = ncol(DTMTFIDFTable))
##### colnames(Top20KWsDTMTFIDFTable)<-colnames(DTMTFIDFTable)
##### rownames(Top20KWsDTMTFIDFTable)<-rownames(DTMTFIDFTable)
##### i<-1
##### for (i in 1:nrow(DTMTFIDFTable)){
##### DocOfInterest<-as.matrix(DTMTFIDFTable[i,order(DTMTFIDFTable[1,], decreasing = TRUE)])
##### TopKWDocOfInterest<-rownames(as.matrix(DocOfInterest[,1:20]))
##### Top20KWsDTMTFIDFTable[i,as.matrix(TopKWDocOfInterest)]<-DocOfInterest[,1:20]
##### }
 
#After creating this table above, we write it as csv and read from it later on.
##### write.csv(Top20KWsDTMTFIDFTable, "R Meetups Presentation/Top20KWsDTMTFIDFTable.csv")
 
#Read the table from the csv file
Top20KWsDTMTFIDFTable<-read.csv("R Meetups Presentation/Top20KWsDTMTFIDFTable.csv", header = TRUE)
rownames(Top20KWsDTMTFIDFTable)<-Top20KWsDTMTFIDFTable[,1]
Top20KWsDTMTFIDFTable<-Top20KWsDTMTFIDFTable[,2:ncol(Top20KWsDTMTFIDFTable)]
 
#View what the csv file that 'create' algorithmically looks like
View(Top20KWsDTMTFIDFTable)
 
#Now, run kmeans clustering algo
TopicsTFIDFKMeans<-kmeans(Top20KWsDTMTFIDFTable, centers = 100)
 
#Analyze/store results
KMeansRawResults<-as.matrix(TopicsTFIDFKMeans$cluster)
 
#Create table of cluster centers/properties
KMeansCentersAnalysis<-cbind(matrix(seq(1,100, by = 1)),
                             TopicsTFIDFKMeans$size,
                             TopicsTFIDFKMeans$withinss,
                             TopicsTFIDFKMeans$withinss/TopicsTFIDFKMeans$size)
colnames(KMeansCentersAnalysis)<-c("Cluster", "Size", "WithinSS", "WithinSS/Entry")
KMeansCentersAnalysis<-KMeansCentersAnalysis[order(as.numeric(KMeansCentersAnalysis[,4]), decreasing = TRUE),]
#Eliminate the clusters that only have 1 element in them (i.e. WithinSS/Entry = 0)
KMeansCentersAnalysis<-KMeansCentersAnalysis[KMeansCentersAnalysis[,4]>0,]
View(KMeansCentersAnalysis)
 
#Algorithm below essentially finds, for each cluster selected (as from above)...
#... the words that fall in the cluster (based on the TFIDF table) and use this as a proxy for 'theme'
KMeansCentersTable<-as.matrix(TopicsTFIDFKMeans$centers)
i<-1
TopicsTable<-NULL
for (i in 1:10){
  ClusterCenter<-KMeansCentersAnalysis[i,1]
  KMeansSpecificCenter<-as.matrix(KMeansCentersTable[ClusterCenter,])
  KMeansSpecificCenter<-as.matrix(sort(KMeansSpecificCenter[,], decreasing = TRUE))
  KMeansSpecificCenter<-cbind(rownames(KMeansSpecificCenter),KMeansSpecificCenter)
  TopicsTableColumn<-cbind(ClusterCenter,as.matrix(paste(KMeansSpecificCenter[1,1],
                                                     KMeansSpecificCenter[2,1],
                                                     KMeansSpecificCenter[3,1],
                                                     KMeansSpecificCenter[4,1],
                                                     KMeansSpecificCenter[5,1],sep="; ")))
  TopicsTable<-rbind(TopicsTable,TopicsTableColumn)
}
 
View(TopicsTable)
 
#Write our results into csv for future use/comparison.
write.csv(TopicsTable, "R Meetups Presentation/TopicsTableByClustering.csv")
 
#Remove unnecessary files from environment
rm(TopicsTable)
rm(KMeansSpecificCenter)
rm(KMeansCentersTable)
rm(TopicsTableColumn)
rm(Top20KWsDTMTFIDFTable)
rm(TopicsTFIDFKMeans)
rm(KMeansRawResults)
rm(KMeansCentersAnalysis)
rm(FreqTermsTopics)
rm(TopicsPredictions)
 
 
######################### End Topics Mining ########################################
 
 
 
 
######################### Start Sentiment Analysis ########################################
 
IndustryNewsCorpus <- tm_map(IndustryNewsCorpus, removeWords,
                             c("said", "bank", "banks", "will", "new", "year", "can", "also",
                               "may", "make", "like", "now", "since", "even", "still", "just",
                               "get", "use", "says", "say"))
 
#Re-create the IndustryNewsDTM that we had before. (This time, no stemming because sentiment)
IndustryNewsDTM<-DocumentTermMatrix(IndustryNewsCorpus)
IndustryNewsDTMMatrix<-as.matrix(IndustryNewsDTM)
 
#Read the AFFINDict into R
AFFINDict<-read.csv("Dictionaries for Text Mining/AFINN-111.csv", header = TRUE)
rownames(AFFINDict)<-AFFINDict[,1]
 
#Not all words are in the AFFINDict. We fill in the values of the words that are in the AFFINDict.
AllTermsIndustryNews<-as.matrix(colnames(IndustryNewsDTM))
AllTermsIndustryNews<-cbind(AllTermsIndustryNews,matrix(0,nrow=nrow(AllTermsIndustryNews),ncol=1))
rownames(AllTermsIndustryNews)<-AllTermsIndustryNews[,1]
i<-0
for (i in 1:nrow(AllTermsIndustryNews)){
  Word<-AllTermsIndustryNews[i,1]
  AllTermsIndustryNews[i,2]<-AFFINDict[Word,2]
}
 
#We look at how many of the keywords in our TDM are actually filled in by AFFINDict.
AllTermsIndustryNews[is.na(AllTermsIndustryNews[,2])]<-0
length(AllTermsIndustryNews[,2])
length(AllTermsIndustryNews[AllTermsIndustryNews[,2]!=0,2])
length(AllTermsIndustryNews[AllTermsIndustryNews[,2]!=0,2])/length(AllTermsIndustryNews[,2])
 
#Now, we use what we created the last step and an algorithm to solve for the sentiment/other metrics of each record.
AllTermsIndustryNews<-as.matrix(AllTermsIndustryNews[,2])
i<-1
DocumentSentimentMatrix<-NULL
for (i in 1:nrow(IndustryNewsDTMMatrix)){
  DocOfInterest<-as.matrix(as.numeric(IndustryNewsDTMMatrix[i,]))
  DocSentiment<-as.matrix(as.numeric(AllTermsIndustryNews))*DocOfInterest
  DocNumOfWords<-nrow(as.matrix(DocOfInterest[DocOfInterest!=0,]))
  DocSentimentSum<-as.numeric(colSums(DocSentiment))
  DocSentimentPositive<-as.numeric(colSums(as.matrix(DocSentiment[DocSentiment>0,])))
  DocSentimentNegative<-DocSentimentSum-DocSentimentPositive
  AverageDocSentiment<-DocSentimentSum/DocNumOfWords
  DocSentimentRow<-cbind(DocSentimentSum,DocNumOfWords,AverageDocSentiment,DocSentimentPositive,DocSentimentNegative)
  DocumentSentimentMatrix<-rbind(DocumentSentimentMatrix,DocSentimentRow)
}
DocumentSentimentMatrix<-cbind(IndustryArticlesDates,DocumentSentimentMatrix)
rownames(DocumentSentimentMatrix)<-IndustryArticlesTitles
 
#Save the matrix
#### write.csv(DocumentSentimentMatrix, "R Meetups Presentation/DocumentSentimentMatrix.csv")
 
#Write the matrix back in
DocumentSentimentMatrix<-read.csv("R Meetups Presentation/DocumentSentimentMatrix.csv", header = T)
rownames(DocumentSentimentMatrix)<-DocumentSentimentMatrix[,1]
DocumentSentimentMatrix<-DocumentSentimentMatrix[,2:ncol(DocumentSentimentMatrix)]
 
#View the matrix we have created that looks at each document and properties about its sentiment
View(DocumentSentimentMatrix)
 
#We use this document sentiment matrix to tell us a little bit
#Classify the articles by their months
i<-1
MonthColumn<-NULL
for (i in 1:nrow(DocumentSentimentMatrix)){
  DateOfInterest<-DocumentSentimentMatrix[i,1]
  if (grepl("2015-03", DateOfInterest) == TRUE){
    MonthValue<-"Mar"
  } else if (grepl("2015-04", DateOfInterest) == TRUE){
    MonthValue<-"Apr"
  } else{
    MonthValue<-"May"
  }
  MonthColumn<-rbind(MonthColumn,MonthValue)
}
 
#Bind this classification to the original Document Sentiment Matrix.
DocumentSentimentMatrix<-cbind(MonthColumn,DocumentSentimentMatrix)
 
#Make use of density plots to show us the overall distribution of sentiment values.
DocumentSentimentMatrix$MonthColumn <- factor(DocumentSentimentMatrix$MonthColumn,levels=c("Mar", "Apr", "May"),
                      labels=c("March", "April", "May"))
 
qplot(DocSentimentSum, data=DocumentSentimentMatrix, geom="density", fill=MonthColumn,
      alpha=I(.5),
      main="Sentiment Densities by Month",
      xlab="Sentiment Values",
      ylab="Density")
 
qplot(AverageDocSentiment, data=DocumentSentimentMatrix, geom="density", fill=MonthColumn,
      alpha=I(.5),
      main="Sentiment Densities by Month",
      xlab="Sentiment Values",
      ylab="Density")
 
 
#Another way to approach polarity: using the built-in QDAP package
#### library(qdap)
#### i<-1
#### PolarityTableQDAP<-NULL
#### for (i in 1:length(IndustryNewsCorpus)){
####   PolarityOutput<-polarity(IndustryNewsCorpus[[i]],
####                          negators = qdapDictionaries::negation.words,
####                          amplifiers = qdapDictionaries::amplification.words,
####                      deamplifiers = qdapDictionaries::deamplification.words,
####                     question.weight = 0, amplifier.weight = 0.8, n.before = 4,
####                     n.after = 2, rm.incomplete = FALSE, digits = 3)
####   PolarityOutput<-as.matrix(as.data.frame(PolarityOutput))
#### PolarityValue<-round(as.numeric(PolarityOutput[1,"all.polarity"]),3)
#### PolarityTableQDAP<-rbind(PolarityTableQDAP,PolarityValue)
#### }
#### rownames(PolarityTableQDAP)<-IndustryArticlesTitles
 
#Read this polarity table into Excel
#### write.csv(PolarityTableQDAP, "R Meetups Presentation/PolarityTableQDAP.csv")
 
#Write the polarity table back into R
PolarityTableQDAP<-read.csv("R Meetups Presentation/PolarityTableQDAP.csv", header = TRUE)
View(PolarityTableQDAP)
PolarityTableQDAP<-cbind(MonthColumn,PolarityTableQDAP)
View(PolarityTableQDAP)
 
#Make use of density plots here too to see how much they differ from the AFFIN dict.
PolarityTableQDAP$MonthColumn <- factor(PolarityTableQDAP$MonthColumn,levels=c("Mar", "Apr", "May"),
                                              labels=c("March", "April", "May"))
 
qplot(V1, data=PolarityTableQDAP, geom="density", fill=MonthColumn,
      alpha=I(.5),
      main="Sentiment Densities by Month",
      xlab="Sentiment Values",
      ylab="Density")
 
 
#Rmove unnecessary plots again
rm(PolarityTableQDAP)
rm(DocumentSentimentMatrix)
rm(DateOfInterest)
rm(MonthColumn)
rm(MonthValue)
rm(Word)
rm(DocSentimentRow)
rm(DocSentiment)
rm(DocOfInterest)
rm(DocNumOfWords)
rm(DocSentimentNegative)
rm(DocSentimentPositive)
rm(DocSentimentSum)
rm(AFFINDict)
 
 
######################### End Sentiment Analysis ########################################
 
 
 
 
 
 
######################### Start Document Relationship Building #######################################
 
#Re-read the stemmed DTM
IndustryNewsDTMStemmed<-DocumentTermMatrix(IndustryNewsCorpusStemmed)
IndustryNewsDTMMatrixStemmed<-as.matrix(IndustryNewsDTMStemmed)
rownames(IndustryNewsDTMMatrixStemmed)<-IndustryArticlesTitles
 
#Cluster on the stemmed DTM
SimpleClusteringKMeans<-kmeans(IndustryNewsDTMMatrixStemmed, centers = 100)
 
#NOTE: how do we choose ideal number of centers?
#Rule of thumb:
#### NumOfCenters = (nrow(IndustryNewsDTMMatrixStemmed)/2)^0.5
 
#Elbow method:
#### WSSDataTable<-NULL
#### for (i in 1:9){
#### NumOfClusters<-20*i
#### WSSData<-sum(kmeans(IndustryNewsDTMMatrixStemmed, centers=NumOfClusters)$withinss)
#### WSSDataTable<-rbind(WSSDataTable,cbind(NumOfClusters,WSSData))
#### }
 
# Reference: http://en.wikipedia.org/wiki/Determining_the_number_of_clusters_in_a_data_set
 
#### write.csv(WSSDataTable, "R Meetups Presentation/ClusteringWSSDataTable.csv")
 
#We read in the WSS for different number of clusters, and see how many clusters is most fitting.
WSSDataTable<-read.csv("R Meetups Presentation/ClusteringWSSDataTable.csv", header = TRUE)
View(WSSDataTable)
 
plot(WSSDataTable[,2], WSSDataTable[,3], type = "b", main = "WithinSS Not Explained vs. Cluster Size", xlab="Number of Clusters", ylab="WithinSS of Groups")
 
 
#We observe the results of our clustering
KMeansRawResults<-as.matrix(SimpleClusteringKMeans$cluster)
KMeansCentersAnalysis<-cbind(matrix(seq(1,100, by = 1)),
                             SimpleClusteringKMeans$size,
                             SimpleClusteringKMeans$withinss,
                             SimpleClusteringKMeans$withinss/SimpleClusteringKMeans$size)
colnames(KMeansCentersAnalysis)<-c("Cluster", "Size", "WithinSS", "WithinSS/Entry")
KMeansCentersAnalysis<-KMeansCentersAnalysis[order(as.numeric(KMeansCentersAnalysis[,4]), decreasing = FALSE),]
KMeansCentersAnalysis<-KMeansCentersAnalysis[KMeansCentersAnalysis[,4]>0,]
 
#View our results of clustering
View(KMeansCentersAnalysis)
 
#We take the top-most cluster and see which documents are related,...
#... and what words are most prevalent for these documents (i.e. sort of like a theme for this cluster).
KMeansCentersTable<-as.matrix(SimpleClusteringKMeans$centers)
KMeansSpecificCenter<-as.matrix(KMeansCentersTable[KMeansCentersAnalysis[1,1],])
KMeansSpecificCenter<-as.matrix(sort(KMeansSpecificCenter[,1], decreasing = TRUE))
View(KMeansSpecificCenter[1:20,])
 
#Delve deeper into specific clusters and their respective clustering with hierarchical clustering of the cluster.
KMeansRawResults<-cbind(matrix(seq(from = 1, to = nrow(KMeansRawResults), by = 1)), KMeansRawResults)
View(KMeansRawResults)
 
#WARNING: We manually pick a cluster to look at in this step.
#We use our KMeansCentersAnalysis table. choose cluster with more documents
KMeansOneCluster<-as.matrix(KMeansRawResults[KMeansRawResults[,2]==15,])
ElementsInCluster<-as.matrix(KMeansOneCluster[,1:2])
View(ElementsInCluster)
ElementsInCluster<-rownames(ElementsInCluster)
 
#We have the specific cluster to hierchically cluster, let's cluster it now.
IndustryNewsDTMMatrixPart<-IndustryNewsDTMMatrixStemmed[ElementsInCluster,]
View(IndustryNewsDTMMatrixPart)
 
#We create a "dist" object for hierarchical clustering and run the algorithm. Plot results
DistCorrel<-dist(IndustryNewsDTMMatrixPart)
HierClust<-hclust(DistCorrel)
plot(HierClust)
 
#In case our results don't look promising, I'll show an example that showcases how effectively we clustered.
#Now, remove any unnecessary elements
rm(DistCorrel, HierClust, IndustryNewsDTMMatrixPart, ElementsInCluster, KMeansOneCluster,
   KMeansRawResults, KMeansSpecificCenter, KMeansCentersTable, KMeansCentersAnalysis, WSSDataTable)
 
######################### End Document Relationship Building #######################################
 
 
 
######################### Start Keyword Relationship Building #######################################
 
IndustryNewsDTMStemmed<-DocumentTermMatrix(IndustryNewsCorpusStemmed)
 
#Find top associated keywords for the 100 top words by count/instance (that don't just occur in 1 article)
WordsForAssoc<-as.matrix(rownames(IndustryNewsKWsTop100byCountPerInstanceNoSparse))
WordsForAssoc<-as.matrix(WordsForAssoc[1:20,])
View(WordsForAssoc)
 
#Create a KWRelationshipTable to show these words and the words most associated with them
KWRelationshipTable<-NULL
for (i in 1:nrow(WordsForAssoc)){
  AssocsForKW<-as.matrix(findAssocs(IndustryNewsDTM,WordsForAssoc[i,1],corlimit=0.3))
  if (nrow(AssocsForKW)<20){
    AssocsForKW<-cbind(rownames(AssocsForKW),AssocsForKW)
    AssocsForKW<-t(AssocsForKW)
    AssocsForKW<-as.matrix(cbind(AssocsForKW[1,],matrix(NA,nrow=1,ncol=20-ncol(AssocsForKW))))
  } else {
    AssocsForKW<-cbind(rownames(AssocsForKW),AssocsForKW)
    AssocsForKW<-t(AssocsForKW[1:20,1])
  }
  KWRelationshipTable<-rbind(KWRelationshipTable, AssocsForKW)
  KWRelationshipTable<-as.matrix(KWRelationshipTable)
}
colnames(KWRelationshipTable)<-WordsForAssoc
 
#Write this document as a csv
write.csv(KWRelationshipTable, "R Meetups Presentation/KWRelationshipTable.csv")
 
#Read this document back in as a csv
KWRelationshipTable<-read.csv("R Meetups Presentation/KWRelationshipTable.csv", header = TRUE)
rownames(KWRelationshipTable)<-KWRelationshipTable[,1]
KWRelationshipTable<-KWRelationshipTable[,2:ncol(KWRelationshipTable)]
 
#View this result
View(KWRelationshipTable)
 
 
#Now, we use the "probability of match" idea to find correlation plots
#### IndustryNewsTDMStemmed<-TermDocumentMatrix(IndustryNewsCorpusStemmed)
#### IndustryNewsTDMStemmedMatrix<-as.matrix(IndustryNewsTDMStemmed)
#### IndustryNewsTDMStemmedMatrix10<-IndustryNewsTDMStemmedMatrix
#### IndustryNewsTDMStemmedMatrix10[IndustryNewsTDMStemmedMatrix10>0]<-1
 
#We use an algorithm/function to create this "probability of match" table.
#### TotalNumKWs<-nrow(IndustryNewsTDMStemmedMatrix10)
#### KWProbMatrix <-matrix(nrow=TotalNumKWs, ncol=TotalNumKWs)
#### rownames(KWProbMatrix)<-rownames(IndustryNewsTDMStemmedMatrix10)
#### colnames(KWProbMatrix)<-rownames(IndustryNewsTDMStemmedMatrix10)
 
#### FindJaccardIndex<-function(IndexCol, IndexRow){
####   CombinedVectorForSum <- as.matrix(IndustryNewsTDMStemmedMatrix10[IndexRow,] + IndustryNewsTDMStemmedMatrix10[IndexCol,])
####   NumSimilarity <- length(CombinedVectorForSum[CombinedVectorForSum[,1] > 1, 1])
####   NumDissimilarity <- length(CombinedVectorForSum[CombinedVectorForSum[,1] == 1, 1])
####   return(NumSimilarity/(NumSimilarity + NumDissimilarity))
#### }
 
#### i<-1
#### j<-1
#### for (i in 1:TotalNumKWs){
####   for (j in 1:TotalNumKWs){
####     KWProbMatrix[i,j]<-FindJaccardIndex(i,j)
####   }
#### }
 
#Write our results into a csv so we don't have to go through the same process next time.
#### write.csv(KWProbMatrix, "c:/Users/ZK7QUTX/My Documents/R Meetups Presentation/KWProbMatrix.csv")
 
#We do clustering on this KWProbMatrix to determine which words are similar
#### ClusteringKWProbMatrix<-kmeans(KWProbMatrix, centers = 500)
 
 
#Now, we use a stemmed DTM matrix to create our correlation plots.
#However, so many dimensions, so reduce dimensions first by...
#... removing words that only appear in one article
IndustryNewsDTMStemmedMatrix<-as.matrix(IndustryNewsDTMStemmed)
IndustryNewsDTMStemmedMatrix10<-IndustryNewsDTMStemmedMatrix
IndustryNewsDTMStemmedMatrix10[IndustryNewsDTMStemmedMatrix10>0]<-1
IndustryNewsDTMStemmedMatrix10ColSum<-colSums(IndustryNewsDTMStemmedMatrix10)
IndustryNewsDTMStemmedMatrix10ColSum<-as.logical(IndustryNewsDTMStemmedMatrix10ColSum)
#Now, the DTMStemmedMatrix is cleaned up
IndustryNewsDTMStemmedMatrix<-IndustryNewsDTMStemmedMatrix[,IndustryNewsDTMStemmedMatrix10ColSum]
rm(IndustryNewsDTMStemmedMatrix10)
rm(IndustryNewsDTMStemmedMatrix10ColSum)
 
#Heatmap and Correlation plot of high correlation words to A WORD.
#In the example below, we use "fed".
WordCorrelTable<-as.matrix(cor(IndustryNewsDTMStemmedMatrix[,"fed"],IndustryNewsDTMStemmedMatrix))
WordCorrelTable<-as.matrix(WordCorrelTable[,order(WordCorrelTable[1,], decreasing = TRUE)])
WordCorrelTable<-as.matrix(WordCorrelTable[WordCorrelTable[,1] < 0.90,])
WordCorrelTable<-as.matrix(WordCorrelTable[1:15,])
KWsForCorrelMatrix<-rbind("fed",as.matrix(row.names(WordCorrelTable)))
WordCorrelMatrix<-cor(IndustryNewsDTMStemmedMatrix[,KWsForCorrelMatrix])
WordCorrelMatrix<-round(WordCorrelMatrix, 3)
 
#View our "correlation table", something that
View(WordCorrelMatrix)
 
#Write our correlation matrix into R for further use.
write.csv(WordCorrelMatrix, "c:/Users/ZK7QUTX/My Documents/R Meetups Presentation/WordCorrelMatrix.csv")
 
#Correlation matrix to show  the heatmap
library(corrplot)
 
WordCorrelMatrix<-read.csv("c:/Users/ZK7QUTX/My Documents/R Meetups Presentation/WordCorrelMatrix.csv", header = TRUE)
rownames(WordCorrelMatrix)<-WordCorrelMatrix[,1]
WordCorrelMatrix<-WordCorrelMatrix[,2:ncol(WordCorrelMatrix)]
WordCorrelMatrix<-as.matrix(WordCorrelMatrix)
 
Col1<-colorRampPalette(c("yellow", "white", "gold", "blue"))
corrplot(WordCorrelMatrix, method = "number", col = Col1(50), cl.lim=c(-.5,1), diag = FALSE, order = "original", type = "upper")
 
 
 
######################### End Keyword Relationship Building #######################################
