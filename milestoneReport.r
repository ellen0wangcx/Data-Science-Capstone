
library(tm)
library(RWeka)
library(stringi)
library(ggplot2)
library(dplyr)

file1 <- file("./final/en_US/en_US.blogs.txt", "rb")
blogs <- readLines(file1, encoding = "UTF-8")
close(file1)

file2 <- file("./final/en_US/en_US.news.txt", "rb")
news <- readLines(file2, encoding = "UTF-8")
close(file2)

file3 <- file("./final/en_US/en_US.twitter.txt", "rb")
twitter <- readLines(file3, encoding = "UTF-8")
close(file3)

words_blogs <- stri_count_words(blogs)
words_news <- stri_count_words(news)
words_twitter <- stri_count_words(twitter)
size_blogs <- file.info("final/en_US/en_US.blogs.txt")$size/1024^2
size_news <- file.info("final/en_US/en_US.news.txt")$size/1024^2
size_twitter <- file.info("fianl/en_US/en_US.twitter.txt")$size/1024^2
summary_table <- data.frame(filename = c("blogs","news","twitter"),
                            file_size_MB = c(size_blogs, size_news, size_twitter),
                            num_lines = c(length(blogs), length(news), length(twitter)),
                            num_words = c(sum(words_blogs), sum(words_news), sum(words_twitter)),
                            mean_num_words = c(mean(words_blogs),mean(words_news),mean(words_twitter)))

summary_table

set.seed(1)
blogsSample <- sample(blogs, length(blogs)*0.01)
newsSample <- sample(news, length(news)*0.01)
twitterSample <- sample(twitter, length(twitter)*0.01)
twitterSample <- sapply(twitterSample, function(row) iconv(row, "latin1", "ASCII", sub = ""))

text_sample <- c(blogsSample, newsSample, twitterSample)
length(text_sample)
sum(stri_count_words(text_sample))
text_sample <- enc2utf8(text_sample)

file0 <- file("profanities.RData", "rb")
profanities <- readLines(file0, encoding="UTF-8")
profanities <- enc2utf8(profanities)
close(file0)

toSpace <- content_transformer(function(x, pattern) gsub(pattern, " ", x))
preprocessCorpus <- function(corpus){
  # Helper function to preprocess corpus
  corpus <- tm_map(corpus, toSpace, "/|@|\\|")
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, removeWords, stopwords("english"))
  corpus <- tm_map(corpus, removeWords, profanities)
  corpus <- tm_map(corpus, stripWhitespace)
  return(corpus)
}

freq_frame <- function(tdm){
  # Helper function to tabulate frequency
  freq <- sort(rowSums(as.matrix(tdm)), decreasing = TRUE)
  freq_frame <- data.frame(word=names(freq), freq=freq)
  return(freq_frame)
}

BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min=2, max=2))
trigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min=3, max=3))
QuadgramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min=4, max=4))

text_sample <- VCorpus(VectorSource(text_sample))

#text_sample <- preprocessCorpus(text_sample)

tdm1a <- TermDocumentMatrix(text_sample)
tdm1 <- removeSparseTerms(tdm1a, 0.99)
freq1_frame <- freq_frame(tdm1)

tdm2a <- TermDocumentMatrix(text_sample, control = list(tokenize=BigramTokenizer))
tdm2 <- removeSparseTerms(tdm2a, 0.999)
freq2_frame <- freq_frame(tdm2)

tdm3a <- TermDocumentMatrix(text_sample, control = list(tokenize=trigramTokenizer))
tdm3 <- removeSparseTerms(tdm3a, 0.9999)
freq3_frame <- freq_frame(tdm3)

tdm4a <- TermDocumentMatrix(text_sample, control = list(tokenize=QuadgramTokenizer))
tdm4 <- removeSparseTerms(tdm4a, 0.9999)
freq4_frame <- freq_frame(tdm4)

n <- 15
freq1_top15 <- freq1_frame[1:n,]
freq2_top15 <- freq2_frame[1:n,]
freq3_top15 <- freq3_frame[1:n,]
freq4_top15 <- freq4_frame

#wordcloud(freq1_frame$word, freq1_frame$freq, min.freq=200)

ggplot(freq1_top15, aes(x=reorder(word, freq), y=freq, fill=freq)) + 
  geom_bar(stat="identity") + 
  theme_bw() + 
  coord_flip() + 
  theme(axis.title.y = element_blank()) + 
  labs(y="Frequency", title = "Most common unigrams in text sample")

ggplot(freq2_top15, aes(x=reorder(word, freq), y=freq, fill=freq)) + 
  geom_bar(stat = "identity") + 
  theme_bw() + 
  coord_flip() + 
  theme(axis.title.y = element_blank()) + 
  labs(y="Frequency", title = "Most common bigrams in text sample")

ggplot(freq3_top15, aes(x=reorder(word, freq), y=freq, fill=freq)) + 
  geom_bar(stat = "identity") + 
  theme_bw() + 
  coord_flip() + 
  theme(axis.title.y = element_blank()) + 
  labs(y="Frequency", title = "Most common trigrams in text sample")

ggplot(freq4_top15, aes(x=reorder(word, freq), y=freq, fill=freq)) + 
  geom_bar(stat = "identity") + 
  theme_bw() + 
  coord_flip() + 
  theme(axis.title.y = element_blank()) + 
  labs(y="Frequency", title = "Most common quadgrams in text sample")
