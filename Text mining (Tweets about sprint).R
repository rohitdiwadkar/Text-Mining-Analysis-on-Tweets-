install.packages("data.table")

library(data.table)

data = fread("J:/tweets_about_sprint.csv", 
             strip.white=T, sep=",", header=T, na.strings=c(""," ", "NA","nan", "NaN", "nannan"))

data

data$tweet_id <- seq.int(nrow(data))

head(data, n=5)

update.packages ()
install.packages("Rcpp")
install.packages("tidytext")
install.packages("dplyr")

library(dplyr)

library(tidytext)


tidy_text <- data %>%
  unnest_tokens(word, tweet)

data(stop_words)


tidy_text <- tidy_text %>%
  anti_join(stop_words)


tidy_text %>%
  count(word, sort = TRUE)

library(ggplot2)


tidy_text %>%
  count(word, sort = TRUE) %>%
  filter(n > 1000) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_bar(stat = "identity") +
  xlab(NULL) +
  coord_flip()



tweet_words <- data %>%
  unnest_tokens(word, tweet) %>%
  count(user, word, sort = TRUE) %>% 
  ungroup()


total_words <- tweet_words %>% 
  group_by(user) %>% 
  summarize(total = sum(n)) 


head(total_words)


tweet_words <- left_join(tweet_words, total_words) 


head(tweet_words)


tweet_words <- tweet_words %>%
  bind_tf_idf(word, user, n)


head(tweet_words)

nrcjoy <- get_sentiments("nrc") %>% 
  filter(sentiment == "joy")


tidy_text %>%
  inner_join(nrcjoy) %>%
  count(word, sort = TRUE)


sentiment <- tidy_text %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE)


head(sentiment)

sentiment <- tidy_text %>%
  filter(user == "KeepMyCoat") %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) 

library(wordcloud)


tidy_text %>%
  anti_join(stop_words) %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 100))


library(reshape2)


tidy_text %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("#F8766D", "#00BFC4"),
                   max.words = 100)


library(RTextTools)


library(tm)
library(wordcloud)
library(topicmodels)
library(slam)


data <- data[1:1000,]


corpus <- Corpus(VectorSource(data$tweet), readerControl=list(language="en"))


tweet_dtm <- DocumentTermMatrix(corpus, control = list(stopwords = TRUE, minWordLength = 3, removeNumbers = TRUE, removePunctuation = TRUE))


tweet_dtm


lda <- LDA(tweet_dtm, k = 2, control = list(seed = 1234))

lda


library(tidytext)

lda_td <- tidy(lda)


lda_td


top_terms <- lda_td %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)


top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()
