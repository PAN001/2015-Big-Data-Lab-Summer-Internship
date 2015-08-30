# blind negation lexcion
blindNegation <- c("need", "should", "shall", "ought")

# negation lexcion
negation <- c("not", "don\'t", "doesn\'t", "didn\'t", "isn\'t", "aren\'t", "won\'t", "can\'t", "haven\'t", "hasn\'t", "hadn\'t", "hardly", "merely")

# preprocess the HLTEMNLP05 lexicon
HLTEMNLP05_preprocess <- function(file_name) {
  # file_name <- subjclueslen1-HLTEMNLP05.txt
  lexicon_stemmed <- read.table(file_name, stringsAsFactors =FALSE)
  names(lexicon_stemmed) <- c("type", "length", "word", "pos", "stemmed", "polarity")
  # lexicon_stemmed <- lexicon[which(lexicon$stemmed == "stemmed1=y"),]
  for(i in 1:nrow(lexicon_stemmed)) {
    lexicon_stemmed[i, "type"] <- gsub("type=","",lexicon_stemmed[i, "type"])
    lexicon_stemmed[i, "length"] <- gsub("len=","",lexicon_stemmed[i, "length"])
    lexicon_stemmed[i, "word"] <- gsub("word1=","",lexicon_stemmed[i, "word"])
    lexicon_stemmed[i, "pos"] <- gsub("pos1=","",lexicon_stemmed[i, "pos"])
    lexicon_stemmed[i, "stemmed"] <- gsub("stemmed1=","",lexicon_stemmed[i, "stemmed"])
    lexicon_stemmed[i, "polarity"] <- gsub("priorpolarity=","",lexicon_stemmed[i, "polarity"])
  }
  lexicon_stemmed
}

# sentiment analysis using HLTEMNLP05 lexicon
sentAnalysis_HLTEMNLP05 <- function(text, lexicon, blindNegation, negation) {
  words <- unlist(strsplit(text, " "))
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