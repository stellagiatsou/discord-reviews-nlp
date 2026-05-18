install.packages(c("readxl", "tm", "SnowballC", "stopwords", "textclean", "humspell", "textstem"))
install.packages(c("quanteda", "stm", "tidyverse"))
install.packages(c("syuzhet", "wordcloud", "RColorBrewer"))
library(readxl)
library(tm)
library(SnowballC)
library(textclean) # για την συνάρτηση replace_contraction
library(textstem) # για lemmatization

library(quanteda)
library(stm)
library(tidyverse)
library(topicmodels)
library(syuzhet)

##### ----- Data collection ----- ####
#data <- read_excel("C:/Users/stella/OneDrive/Έγγραφα/MSc/DA/Εργασία/discord_v3.csv") 

data <- read_excel("C:/Users/stella/OneDrive/Έγγραφα/MSc/DA/Εργασία/Discord.xlsx") 
names(data) #ονόματα στηλών αρχείου excel

# Επιλογή της στήλης content από το excel
text <- data$content

##### ----- Cleaning and preprocessing ----- ####

# Δημιουργία cleaned_text στήλης
cleaned_text <- text

# Γραμμές που δεν είναι στα αγγλικά
rows_to_remove <- c(23, 153, 238, 260, 359, 445, 508, 521, 546) 
# Αφαίρεση των γραμμών
cleaned_text <- cleaned_text[-rows_to_remove]

# to lowercase
cleaned_text <- tolower(cleaned_text)

# Χειροκίνητη διόρθωση των πιο συχνών λαθών που επηρεάζουν το συναίσθημα
# Σημείωση: Το \\b εξασφαλίζει ότι θα αλλάξει η λέξη "dnt" και όχι αν η ακολουθία "dnt" αν βρίσκεται μέσα σε άλλη λέξη
cleaned_text <- gsub("\\bdnt\\b|\\bdont\\b", "do not", cleaned_text, ignore.case = TRUE)
cleaned_text <- gsub("\\bcant\\b", "cannot", cleaned_text, ignore.case = TRUE)
cleaned_text <- gsub("\\bgodd\\b|\\bgud\\b", "good", cleaned_text, ignore.case = TRUE)
cleaned_text <- gsub("\\bwont\\b", "will not", cleaned_text, ignore.case = TRUE)
cleaned_text <- gsub("\\bshnt\\b", "shall not", cleaned_text, ignore.case = TRUE)
cleaned_text <- gsub("\\bpls\\b", "please", cleaned_text, ignore.case = TRUE)
cleaned_text <- gsub("\\bNSFW\\b", "not safe for work", cleaned_text, ignore.case = TRUE)
cleaned_text <- gsub("\\bamaz\\b", "amazing", cleaned_text, ignore.case = TRUE)
cleaned_text <- gsub("\\bXD\\b", "laugh", cleaned_text, ignore.case = TRUE)
cleaned_text <- gsub("\\blol\\b", "laugh out loud", cleaned_text, ignore.case = TRUE)
cleaned_text <- gsub("\\bstupi\\b", "stupid", cleaned_text, ignore.case = TRUE)
cleaned_text <- gsub("\\bpeak shii\\b", "the best", cleaned_text, ignore.case = TRUE)
cleaned_text <- gsub("\\bfrs\\b", "for real, still", cleaned_text, ignore.case = TRUE)
cleaned_text <- gsub("\\bhaven\\'t\\b", "have not", cleaned_text, ignore.case = TRUE)

# Tα emoji τα κρατάω για το sentiment analysis και τα μετατρέπω σε λέξεις
cleaned_text <- replace_emoji(cleaned_text)
# Επειδή η μετατροπή του emoji σε text αφήνει hex κώδικα  <ef><b8><8f><e2><80><8d> 
# Αφαίρεση οτιδήποτε μέσα σε < > 
cleaned_text <- gsub("<[^>]+>", " ", cleaned_text)

# Μετατροπή του "don't" σε "do not" και τοu "it's" σε "it is"
cleaned_text <- replace_contraction(cleaned_text)

# Αφαίρεση URLs
cleaned_text <- gsub("http\\S+|www\\S+", "", cleaned_text)

# Αφαίρεση mentions
cleaned_text <- gsub("<@!?[0-9]+>", " ", cleaned_text)   # <@12345> or <@!12345>
cleaned_text <- gsub("@[A-Za-z0-9_]+", " ", cleaned_text)

# Αφαίρεση σημείων στίξης
cleaned_text <- gsub("[[:punct:]]", " ", cleaned_text)

# Αφαίρεση αριθμών
cleaned_text <- gsub("[0-9]", " ", cleaned_text)

# Αφαίρεση εξτρά spaces
cleaned_text <- stripWhitespace(cleaned_text) # προκύπτουν από γραμμές 42, 45, 51, 54 

my_stopwords <- stopwords("english")
keep_words <- c("not", "too", "very", "no", "more", "most", "but", "against", "however", "only", "up", "down", "off", "so",  "do", "would", "should", "could", "ought") # Θα γίνει ανάλυση με μέθοδο Bigrams δηλαδή ανά ζευγάρια λέξεων οπότε it's okay να μην αφαιρέσω το do 
custom_stopwords <- setdiff(my_stopwords, keep_words)
# Αφαίρεση stopwords
cleaned_text <- removeWords(cleaned_text, custom_stopwords)

cleaned_text <- lemmatize_strings(cleaned_text)

# Empty docs
cleaned_text <- cleaned_text[nchar(cleaned_text) > 0]

# Aποτελέσματα
head(cleaned_text)

##### ----- For Exploratory Text Analysis ----- ####

# Δημιουργία corpus
corpus <- VCorpus(VectorSource(cleaned_text))

# Δημιουργία term matrix
# dtm <- DocumentTermMatrix(corpus)

# Δημιουργία TF-IDF MATRIX
dtm <- DocumentTermMatrix(corpus, control = list(weighting = function(x) weightTfIdf(x, normalize = TRUE)))

# Κρατάω λέξεις που εμφανίζονται τουλάχιστον σε 3-4 κριτικές
dtm_clean <- removeSparseTerms(dtm, 0.993) 

# View matrix
inspect(dtm_clean)

# Οι πιο σημαντικοί όροι
m <- as.matrix(dtm_clean)
term_scores <- colSums(m)
sort(term_scores, decreasing = TRUE)[1:20]

# Συχνότητα λέξεων
word_freq <- colSums(as.matrix(dtm_clean))

# Sort descending
word_freq <- sort(word_freq, decreasing = TRUE)

# Οι Top 20 λέξεις
head(word_freq, 20)

# bar plot 
word_freq_df <- data.frame(
  word = names(word_freq),
  freq = as.numeric(word_freq)
)
top10_words <- head(word_freq, 10)

# horizontal barplot
barplot(
  top10_words,
  las = 2,
  col = "steelblue",
  main = "Top 10 Most Frequent Words",
  ylab = "Frequency",
  names.arg = names(top10_words)
)

# Topic modeling
qcorp <- corpus(cleaned_text)

tokens_bigram <- tokens(qcorp,
                        what = "word",
                        remove_punct = TRUE,
                        remove_numbers = TRUE)

tokens_bigram <- tokens_ngrams(tokens_bigram, n = 2)

dfm_bigram <- dfm(tokens_bigram)

# keep bigrams appearing at least 6 times
dfm_bigram <- dfm_trim(dfm_bigram, min_termfreq = 10)

stm_input <- convert(dfm_bigram, to = "stm")
stm_model <- stm(
  documents = stm_input$documents,
  vocab = stm_input$vocab,
  K = 6, #από 4 σε 6
  max.em.its = 75,
  init.type = "Spectral",
  seed = 1234
)

labelTopics(stm_model, n = 10)
plot(stm_model, type = "summary")


mod_out <- topicCorr(stm_model)
plot(mod_out)



# Sentiment Analysis
sent_scores <- get_sentiment(cleaned_text, method = "syuzhet")

# summary
summary(sent_scores)

hist(
  sent_scores,
  breaks = 30,
  main = "Sentiment Distribution",
  xlab = "Sentiment Score",
  col = "skyblue"
)

nrc <- get_nrc_sentiment(cleaned_text)

emotion_totals <- colSums(nrc)

barplot(
  emotion_totals,
  las = 2,
  col = "tomato",
  main = "NRC Emotion Distribution",
  ylab = "Count"
)




















# Εxport corpus
corpus_text <- sapply(corpus, as.character)
write.csv(corpus_text, "corpus_export3.csv", row.names = FALSE)

# Εxport word frequency
word_freq <- sort(colSums(as.matrix(dtm_clean)), decreasing = TRUE)
word_freq_df <- data.frame(
  word = names(word_freq),
  freq = as.numeric(word_freq)
)
write.csv(word_freq_df, "word_frequency3.csv", row.names = FALSE)

