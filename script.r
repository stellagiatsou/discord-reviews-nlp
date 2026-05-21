# Install required packages
install.packages(c(
  "topicmodels",
  "topicdoc",
  "tidytext",
  "dplyr",
  "tm",
  "wordcloud",
  "syuzhet",
  "RColorBrewer",
  "readxl",
  "textclean",
  "textstem",
  "Rmisc"
))

# Libraries
library(topicmodels)
library(topicdoc)
library(tidytext)
library(dplyr)
library(tm)
library(wordcloud)
library(syuzhet) #for sentiment analysis
library(RColorBrewer)
library(readxl)
library(textclean)
library(textstem)
library(ggplot2)
library(Rmisc) # for multiplot

# Load dataset
data <- read.csv("path for Discord.csv", header = TRUE, stringsAsFactors = FALSE)

# Keep only text column
text <- data$content

# PREPROCESSING 
# -------------

# Remove NA values
text <- na.omit(text)

# Create corpus
corpus <- VCorpus(VectorSource(text))

# Lowercase
corpus <- tm_map(corpus, content_transformer(tolower))

# Replace contractions
corpus <- tm_map(corpus, content_transformer(replace_contraction))

# Custom slang corrections
replace_custom <- content_transformer(function(x) {

  x <- gsub("\\bdnt\\b|\\bdont\\b", "do not", x, ignore.case = TRUE)
  x <- gsub("\\bcant\\b", "cannot", x, ignore.case = TRUE)
  x <- gsub("\\bgodd\\b|\\bgud\\b", "good", x, ignore.case = TRUE)
  x <- gsub("\\bwont\\b", "will not", x, ignore.case = TRUE)
  x <- gsub("\\bshnt\\b", "shall not", x, ignore.case = TRUE)
  x <- gsub("\\bpls\\b", "please", x, ignore.case = TRUE)
  x <- gsub("\\bnsfw\\b", "not safe for work", x, ignore.case = TRUE)
  x <- gsub("\\bamaz\\b", "amazing", x, ignore.case = TRUE)
  x <- gsub("\\bxd\\b", "laugh", x, ignore.case = TRUE)
  x <- gsub("\\blol\\b", "laugh out loud", x, ignore.case = TRUE)
  x <- gsub("\\bstupi\\b", "stupid", x, ignore.case = TRUE)
  x <- gsub("\\bpeak shii\\b", "the best", x, ignore.case = TRUE)
  x <- gsub("\\bfrs\\b", "for real still", x, ignore.case = TRUE)

  return(x)
})

corpus <- tm_map(corpus, replace_custom)

# Replace emojis with words
corpus <- tm_map(corpus, content_transformer(replace_emoji))

# Remove HTML tags from replacing the emojis with words
corpus <- tm_map(corpus,
                 content_transformer(function(x)
                   gsub("<[^>]+>", " ", x)))

# Remove punctuation
corpus <- tm_map(corpus, removePunctuation)

# Remove numbers
corpus <- tm_map(corpus, removeNumbers)

# Remove stopwords
corpus <- tm_map(corpus, removeWords, stopwords("english"))

# Custom remove words
remove_words <- c(
    "chat", "message", "send", "server",
    "make", "try", "keep", "even", "now", "time",
    "phone", "mobile", "account", "new",
    "app", "discord", "one", "get", "just",
    "will", "thing", "use", "also", "can",
    "ive", "see", "still", "well", "like",
    "good", "great", "first", "product",
    "price", "year", "want", "start",
    "dont", "best", "without"
)

corpus <- tm_map(corpus, removeWords, remove_words)

# Lemmatization
corpus <- tm_map(corpus,
                 content_transformer(lemmatize_strings))

# Remove extra whitespace
corpus <- tm_map(corpus, stripWhitespace)

# Create Document-Term Matrix
dtm <- DocumentTermMatrix(corpus)

# Remove empty documents
raw_count <- rowSums(as.matrix(dtm))
dtm <- dtm[raw_count > 0, ]

# Inspect matrix
inspect(dtm)

# TOPIC MODELING 
# -------------

# LDA - Gibbs sampling
# Linear Discriminant Analysis (LDA) is a well-established machine learning technique and classification method for predicting categories
set.seed(42)

k <- 6 # according to coherance score

lda_model <- LDA(
  dtm,
  k = k,
  method = "Gibbs",
  control = list(
    iter = 1000,
    seed = 42,
    alpha = 0.1,
    delta = 0.1
  )
)

# Top terms per topic
terms(lda_model, 10)

# Wordcloud per topic
topic_terms <- posterior(lda_model)$terms

par(mfrow = c(2, 3))  # 6 topics layout

for (i in 1:k) {

  topic_probs <- topic_terms[i, ]

  top_words <- sort(topic_probs, decreasing = TRUE)[1:50]

  wordcloud(
    words = names(top_words),
    freq = top_words,
    scale = c(3, 0.5),
    random.order = FALSE,
    colors = brewer.pal(8, "Dark2"),
    rot.per = 0.3
  )

  title(paste("Topic", i))
}

# Document-topic distribution

doc_topics <- posterior(lda_model)$topics
doc_topics <- as.data.frame(doc_topics)

colnames(doc_topics) <- paste0("Topic_", 1:k)

doc_topics <- round(doc_topics, 3)

head(doc_topics)

# Dominant topic

doc_topics$dominant_topic <- apply(doc_topics, 1, which.max)

table(doc_topics$dominant_topic)


# SENTIMENT ANALYSIS 
# ------------------

# Convert corpus -> character vector
cleaned_text <- sapply(corpus, as.character)

sent_scores <- get_sentiment(cleaned_text, method = "syuzhet")

sent_scores <- sent_scores[1:nrow(doc_topics)]
doc_topics$sent_score <- sent_scores

doc_topics$polarity <- ifelse(
  doc_topics$sent_score > 0, "Positive",
  ifelse(doc_topics$sent_score < 0, "Negative", "Neutral")
)

topic_names <- c(
  "Communication/support issues",
  "Features/Nitro ecosystem",
  "Sentiment reactions/emojis",
  "Bugs/updates/system issues",
  "Social/gaming interaction",
  "Login/security"
)

# Histogram per topic
par(mfrow = c(2, 3))

for (i in 1:k) {
 
  topic_df <- doc_topics[doc_topics$dominant_topic == i, ]

  if (nrow(topic_df) == 0) next

  hist(
    topic_df$sent_score,
    breaks = 30,
    main = topic_names[i],
    xlab = "Sentiment Score",
    col = "skyblue"
    )
}

# Polarity per topic
plot_lst <- vector("list", length = k) # an empty list
for ( i in 1:k){
  plot_lst[[i]] <- local({
    i <- i 
    topic_df <- doc_topics[doc_topics$dominant_topic == i, ]

    if (nrow(topic_df) == 0) next

    df_plot <- as.data.frame(table(topic_df$polarity))
    colnames(df_plot) <- c("polarity", "count")
      plot_lst[[i]] <- ggplot(df_plot, aes(x = polarity, y = count, fill = polarity)) +
      geom_bar(stat = "identity") +
      theme_minimal() +
      labs(title = topic_names[i]) + ggtitle(topic_names[i])
  })
}
multiplot(plotlist = plot_lst, cols = 2)

# NRC emotion distribution analysis per topic
# NRC Emotion Lexicon, Syuzhet package which contains the built-in get_nrc_sentiment function
nrc_sentiment <- get_nrc_sentiment(cleaned_text)

nrc_sentiment <- nrc_sentiment[1:nrow(doc_topics), ]

doc_topics <- cbind(doc_topics, nrc_sentiment)

emotion_cols <- c(
  "anger","anticipation","disgust","fear",
  "joy","sadness","surprise","trust"
)
plot_lst <- vector("list", length = k) # an empty list
for (i in 1:i) {
  plot_lst[[i]] <- local({
    i <- i 
    topic_df <- doc_topics[doc_topics$dominant_topic == i, ]

    if (nrow(topic_df) == 0) next

    emotion_totals <- colMeans(topic_df[, emotion_cols])

    df_plot <- data.frame(
      emotion = names(emotion_totals),
      score = as.numeric(emotion_totals)
    )


    ggplot(df_plot, aes(x = emotion, y = score, fill = emotion)) +
      geom_bar(stat = "identity") +
      theme_minimal() +
      labs(title = topic_names[i])
  })
}
multiplot(plotlist = plot_lst, cols = 2)

# Polarity alaysis
sent_labels <- ifelse(sent_scores > 0, "Positive",
                      ifelse(sent_scores < 0, "Negative", "Neutral"))

#barplot(table(sent_labels),
#        col = c("red","gray","green"),
#        main = "Sentiment Distribution")
df_plot <- as.data.frame(table(sent_labels))
colnames(df_plot) <- c("polarity", "count")

print(
  ggplot(df_plot, aes(x = polarity, y = count, fill = polarity)) +
    geom_bar(stat = "identity") +
    theme_minimal() +
    labs(title = "Sentiment Distribution")
)

# NRC emotion distribution analysis
nrc <- get_nrc_sentiment(cleaned_text)

emotion_totals <- colSums(nrc)

#barplot(
#  emotion_totals,
#  las = 2,
#  col = "tomato",
#  main = "NRC Emotion Distribution",
#  ylab = "Count"
#)
df_plot <- data.frame(
  emotion = names(emotion_totals),
  score = as.numeric(emotion_totals)
)
print(
  ggplot(df_plot, aes(x = emotion, y = score, fill = emotion)) +
    geom_bar(stat = "identity") +
    theme_minimal() +
    labs(title = paste("NRC Emotion Distribution")) + coord_flip()
)