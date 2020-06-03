# Install packages
#install.packages("twitteR")
#install.packages("RCurl")
#install.packages("httr")
#install.packages("syuzhet")
#install.packages("tidyverse")
#install.packages("tibble")
# Load the required Packages
library(tidyverse)
library(tibble)
library(twitteR)
library(RCurl)
library(httr)
library(tm)
library(wordcloud)
library(syuzhet)
library(ggplot2)
library(tm)
library(NLP)
library(openNLP)
library(RColorBrewer)

consumer_key <- "..." #Consumer key from twitter app
consumer_secret <- "..." #Consumer secret from twitter app
access_token <- "..." #access token from twitter app
access_secret <- "..." #access secret from twitter app
setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

tweets <- (searchTwitter("#tagtosearch", n = 100, lang = "en"))
tweets.df <- tibble((twListToDF(tweets)))

#Clean-up
tweets.df$text = gsub("&amp", "", tweets.df$text)
tweets.df$text = gsub("&amp", "", tweets.df$text)
tweets.df$text = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", tweets.df$text)
tweets.df$text = gsub("@\\w+", "", tweets.df$text)
tweets.df$text = gsub("[[:punct:]]", "", tweets.df$text)
tweets.df$text = gsub("[[:digit:]]", "", tweets.df$text)
tweets.df$text = gsub("http\\w+", "", tweets.df$text)
tweets.df$text = gsub("[ \t]{2,}", "", tweets.df$text)
tweets.df$text = gsub("^\\s+|\\s+$", "", tweets.df$text)
tweets.df$text <- iconv(tweets.df$text, "UTF-8", "ASCII", sub = "")

# Emotions for each tweet using NRC dictionary
emotions <- get_nrc_sentiment(as.character(tweets.df$text))
emo_bar <- colSums(emotions)
emo_sum <- data.frame(count = emo_bar, emotion = names(emo_bar))
emo_sum$emotion = factor(emo_sum$emotion, levels = emo_sum$emotion[order(emo_sum$count, decreasing = TRUE)])

library(plotly)
p <- plot_ly(emo_sum, x = ~emotion, y = ~count, type = "bar", color = ~emotion) %>%
  layout(xaxis = list(title = ""), showlegend = FALSE,
         title <- "Emotion Type for hashtag: #tag ")
#api_create(p, filename = "Sentimentanalysis")
# Create comparison word cloud data
wordcloud_tweet <- c(paste(tweets.df$text[emotions$anger > 0], collapse = " "),
                     paste(tweets.df$text[emotions$anticipation > 0], collapse = " "), paste(tweets.df$text[emotions$disgust > 0], collapse = " "),
                     paste(tweets.df$text[emotions$fear > 0], collapse = " "), paste(tweets.df$text[emotions$joy > 0], collapse = " "), paste(tweets.df$text[emotions$sadness > 0], collapse = " "), paste(tweets.df$text[emotions$surprise > 0], collapse = " "), paste(tweets.df$text[emotions$trust > 0], collapse = " ")
)


# create corpus
corpus <- Corpus(VectorSource(wordcloud_tweet))

# Clean-up
corpus <- tm_map(corpus, tolower)
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeWords, stopwords("english"))
corpus <- tm_map(corpus, stemDocument)

# create document term matrix
tdm <- TermDocumentMatrix(corpus)

# convert as matrix
tdm <- as.matrix(tdm)
tdmnew <- tdm[nchar(rownames(tdm)) < 11,]

# column name binding
colnames(tdm) <- c('anger', 'anticipation', 'disgust', 'fear', 'joy', 'sadness', 'surprise', 'trust')
colnames(tdmnew) <- colnames(tdm)
comparison.cloud(tdmnew, random.order = FALSE,
                 colors = c("#00B2FF", "red", "#FF0099", "#6600CC", "green", "orange", "blue", "brown"),
                 title.size = 1, max.words = 250, scale = c(2.5, 0.4), rot.per = 0.4)



