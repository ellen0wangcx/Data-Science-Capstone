library(tm)

#Load data(profanities, freqtables)
load("./data/profanities.RData")
load("./data/tab2gram.RData")
load("./data/tab3gram.RData")
load("./data/tab4gram.RData")

#Main
predictThis <- function(x){
  pred <- predict_ngrams(process_input(x), tab4gram, tab3gram, tab2gram)
  return(pred)
}

#Helper functions
preprocessCorpus2 <- function(corpus){
  corpus <- tm_map(corpus, tolower)
  corpus <- tm_map(corpus, PlainTextDocument)
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, removeWords, stopwords("english"))
  corpus <- tm_map(corpus, removeWords, profanities)
  corpus <- tm_map(corpus, stripWhitespace)
  return(corpus)
}

process_input <- function(input_text){
  input_corpus <- VCorpus(VectorSource(input_text))
  input_corpus <- preprocessCorpus2(input_corpus)
  input <- as.character(unlist(input_corpus)[1])
  output <- unlist(strsplit(input, " ", fixed = TRUE))
  return(as.character(output))
}

predict_ngrams <- function(input, tabEnd, tab3gram, tab2gram){
  n <- length(input)
  if(n==0){
    prediction <- NULL
  }else{
    pred1 <- NULL
    pred2 <- NULL
    pred3 <- NULL
    if(n>=3){
      input1 <- paste(input[(n-2):n], collapse = " ")
      match <- tabEnd[tabEnd$firstword==input1,]
      pred3 <- match[order(match$freq, decreasing = TRUE),]$lastword
    }
    if(n>=2){
      input1 <- paste(input[(n-1):n], collapse = " ")
      match <- tab3gram[tab3gram$firstword==input1,]
      pred2 <- match[order(match$freq, decreasing = TRUE),]$lastword
    }
    if(n>=1){
      input1 <- input[length(input)]
      match <- tab2gram[tab2gram$firstword==input1,]
      pred1 <- match[order(match$freq, decreasing = TRUE),]$lastword
    }
    prediction <- c(as.character(pred3), as.character(pred2), as.character(pred1))
    if(length(prediction)==0){
      predction <- c("the", "be", "to", "of", "and", "a")
    }else if(length(prediction)>6){
      prediction <- prediction[1:6]
    }
  }
  return(prediction)
}
