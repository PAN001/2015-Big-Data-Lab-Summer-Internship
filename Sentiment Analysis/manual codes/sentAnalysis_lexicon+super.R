# install and load library
## install.packages("~/Desktop/R code/packages/RWeka_0.4-24.tgz", repos = NULL, type = "source")
## install.packages("party")
library(RWeka)
library(party)

# creating lexcions
SM_positive <- c("good")
SM_negative <- c("bad")

SL_positive <- c("love", "lovewins", "gradedding", "mondaymotivation", "twibbon", "suppolovewinsdd", "loveislove", "rainbow", "beautiful", "loves")
SL_negative <- c("bad", "terrible", "worst", "sucks", "awful", "dumb")

PF_positive <- c("will", "has", "must", "is")
PF_negative <- c("was", "would", "had", "were")

PF_SL_positive <- c("will", "has", "must", "is","love", "lovewins", "gradedding", "mondaymotivation", "twibbon", "suppolovewinsdd", "loveislove", "rainbow", "beautiful", "loves")
PF_SL_negative <- c("was", "would", "had", "were", "bad", "terrible", "worst", "sucks", "awful", "dumb")

BL_positive <- read.table("Bing liu positive-words.txt", stringsAsFactors = FALSE) 
BL_positive <- as.vector(BL_positive[,1])
BL_negative <- read.table("Bing liu negative-words.txt", stringsAsFactors = FALSE) 
BL_negative <- as.vector(BL_negative[,1])

HLTEMNLP05_lexicon <- HLTEMNLP05_preprocess()

# sentiment analysis using lexicon
sentAnalysis_each_lexicon <- function(words, lexicon_positive, lexicon_negative) {
  score <- 0
  text_polarity <- ""
  for(word in words) {
    index <- which(lexicon_positive == word)
    if(length(index) > 0) {
      score <- score + 1
    }
    index_negative <- which(lexicon_negative == word)
    if(length(index_negative) > 0) {
      score <- score - 1
    }
  }
  if(score < 0) {
    text_ploarity <- "negative"
  }
  else if(score == 0) {
    text_ploarity <- "neutral"
  }
  else {
    text_ploarity <- "positive"
  }
  text_ploarity
}

blindNegation <- c("need", "should", "shall", "ought")
negation <- c("not", "don\'t", "doesn\'t", "didn\'t", "isn\'t", "aren\'t", "won\'t", "can\'t", "haven\'t", "hasn\'t", "hadn\'t", "hardly", "merely")

# sentiment analysis using HLTEMNLP05 lexicon
sentAnalysis_HLTEMNLP05_lexicon <- function(words, lexicon, blindNegation, negation) {
  score <- 0
  negation_count <- 0
  text_polarity <- ""
  for(word in words) {
    index <- which(lexicon$word == word)
    if(length(index) > 1) {
      index <- index[1]      
    }
    if(length(which(blindNegation == word)) > 0) {
      score <- -1
      break
    }
    else if(length(which(negation == word)) > 0) {
      negation_count = negation_count + 1
    }
    else if(length(index) > 0) {
      type <- lexicon[index, "type"]
      polarity <- lexicon[index, "polarity"]
      if(type == "weaksubj") {
        if(polarity == "negative") {
          score = score - 0.5;
        }
        else if(polarity == "positive") {
          score = score + 0.5;
        }
      }
      else {
        if(polarity == "negative") {
          score = score - 1;
        }
        else if(polarity == "positive") {
          score = score + 1;
        }
      }
    } 
  }
  if(negation_count > 0) {
    if((negation_count %% 2) > 0) {
      score = score * -1;
    }
  }
  if(score < 0) {
    text_ploarity <- "negative"
  }
  else if(score == 0) {
    text_ploarity <- "neutral"
  }
  else {
    text_ploarity <- "positive"
  }
  text_ploarity
}

# applying sentiment analysis using each different lexicon
sentAnalysis_combined_lexicon <- function(data) {
  final_result = NULL
  for(text in data[,2]) {
    words <- unlist(strsplit(text, " "))
    SM_polarity <- sentAnalysis_each_lexicon(words, SM_positive, SM_negative)
    SL_polarity <- sentAnalysis_each_lexicon(words, SL_positive, SL_negative)
    PF_polarity <- sentAnalysis_each_lexicon(words, PF_positive, PF_negative)
    PF_SL_polarity <- sentAnalysis_each_lexicon(words, PF_SL_positive, PF_SL_negative)
    BL_polarity <- sentAnalysis_each_lexicon(words, BL_positive, BL_negative)
    HLTEMNLP05_polarity <- sentAnalysis_HLTEMNLP05(words, HLTEMNLP05_lexicon, blindNegation, negation)
    result <- data.frame(text, SM_polarity, SL_polarity, PF_polarity, PF_SL_polarity, BL_polarity, HLTEMNLP05_polarity)
    final_result = rbind(final_result, result)
  }
  final_result <- cbind(emotion = data[,1], final_result)
  final_result  
}

# ensambling learning: stacking 
gmAll2Combined <- sentAnalysis_combined_lexicon(gmAll2Whole)
J48_classifier <- J48(emotion~., data = gmAll2Combined )
summary(J48_classifier)
if(require("party", quietly = TRUE)) plot(J48_classifier)

