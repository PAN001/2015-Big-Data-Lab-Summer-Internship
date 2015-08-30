#load library
library("class")
library("dplyr")
# install.packages('e1071', dependencies = TRUE)
library('e1071')
# install.packages("RWeka")
library(RWeka)
library(nnet)
# install.packages("se")
# install.packages("tm")
# install.packages("~/Desktop/R code/packages/Rstem_0.4-1.tar.gz", repos = NULL, type = "source")
# install.packages("~/Desktop/R code/packages/sentiment_0.1.tar.gz", repos = NULL, type = "source")
library(tm)
library(NLP)
library(Rstem)
library(sentiment)

# load data
gmOriginal <- read.csv("gm.csv",stringsAsFactors = FALSE) # original twitter whole file
gmOriginalTweet <- as.data.frame(gmOriginal$tweet)  # original tweet text
gmAll <- read.csv("gm_cleaned_stemmed.csv",stringsAsFactors = FALSE); # tweet text after cleaning

# generate dataest
gmAll2 <- as.data.frame(gmAll[0:1000,]) # 1000 of tweets as whole dataset(beacuse of the limitation of the capacity of the computer, the size of dataset is extremely constrained)
gmAll2Emo <- sentiment::classify_polarity(gmAll2) # using sentiment package to evaluate the emotions of each tweet
gmAll2Whole <- data.frame(emotion = gmAll2Emo[,4], tweet = gmAll2)  # combined as a whole dataset
allWords <- uniqueWord(gmAll2Whole)
gmAll2Data <- createDateset(allWords, gmAll2Whole)  # a completed dataset of bag-of-features format
summary(gmAll2Data)

# write dataset into file
## write.csv(gmAll2Data, file = "gmAll2Data.csv")
## write.csv(gmAll2Whole, file = "gmAll2Whole.csv")

# select one-third of dataset as training set
gmTrainData <- gmAll2Data[0:300,]

# select one-third of dataset as testing set
gmTestData <- gmAll2Data[301:1000,]

# sentiment analysis using different supervised methods
senAnalysis <- function() {  
  # lexion = read.table("lexion.txt", sep="\t", col.names = c("text", "emotion"), fill = FALSE, stringsAsFactors = FALSE, strip.white = TRUE)
  # lexion2 <- data.frame(emotion = lexion[,2], tweet = lexion[,1])
  # lexion2[,2] = as.character(lexion2[,2])
  
  #NB: trained by tweets
  NB <- make_Weka_classifier("weka/classifiers/bayes/NaiveBayes")
  NB_classifier <- NB(emotion~., data = gmAll2Data)
  NB_result <- evaluate_Weka_classifier(NB_classifier, numFolds = 10)
  
  # knn
  Knn_classifier <- IBk(emotion~., data = gmAll2Data)
  Knn_result <- evaluate_Weka_classifier(Knn_classifier, numFolds = 10)
  
  # decision tree
  J48_classifier <- J48(emotion~., data = gmAll2Data)
  J48_result <- evaluate_Weka_classifier(J48_classifier, numFolds = 10)
}

# find all unique words
uniqueWord <- function(s) {
  words = NULL;
  for(word in s[,2]) {
    words = c(words, unlist(strsplit(word, " ")));
  }
  unique(words);
}

# create a bag-form dataset following standard bag-of-features framework
createDateset <- function(allWords, data) {
  result = NULL;
  for(s in data[,2]) {
    freq <- vector("numeric", length = length(allWords));
    words = unlist(strsplit(s, " "));
    for(word in words) {
      index = which(allWords == word);
      if(length(index) > 0) {
        freq[index] = 1;
      }
    }
    result = rbind(result, freq);
  }
  result <- as.data.frame(result);
  names(result) <- allWords;
  finalRes <- data.frame(data, result);
}

