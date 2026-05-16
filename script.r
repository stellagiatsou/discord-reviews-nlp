install.packages(c("readxl", "tm", "SnowballC", "stopwords", "textclean", "humspell"))

library(readxl)
library(tm)
library(SnowballC)
library(textclean) # για την συνάρτηση replace_contraction

data <- read_excel("C:/Users/user/Discord.xlsx") 
names(data) #ονόματα στηλών αρχείου excel

# Επιλογή της στήλης content από το excel
text <- data$content

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

# Tα emoji τα κρατάω για το sentiment analysis και τα μετατρέπω σε λέξεις
cleaned_text <- replace_emoji(cleaned_text)
# Επειδή η μετατροπή του emoji σε text αφήνει hex κώδικα  <ef><b8><8f><e2><80><8d> 
# Αφαίρεση οτιδήποτε μέσα σε < > 
cleaned_text <- gsub("<[a-z0-9]+>", " ", cleaned_text)

# Μετατροπή του "don't" σε "do not" και τοu "it's" σε "it is"
cleaned_text <- replace_contraction(cleaned_text)

# Αφαίρεση σημείων στίξης
cleaned_text <- gsub("[[:punct:]]", " ", cleaned_text)

# Αφαίρεση αριθμών
cleaned_text <- gsub("[0-9]", " ", cleaned_text)

# Αφαίρεση εξτρά spaces
cleaned_text <- stripWhitespace(cleaned_text) # προκύπτουν από γραμμές 42, 45, 51, 54 

# Αφαίρεση URLs
gsub("http\\S+|www\\S+", "", cleaned_text)


my_stopwords <- stopwords("english")
keep_words <- c("not", "too", "very", "no", "more", "most", "but", "against", "however", "only", "up", "down", "off", "so",  "do", "would", "should", "could", "ought") # Θα γίνει ανάλυση με μέθοδο Bigrams δηλαδή ανά ζευγάρια λέξεων οπότε it's okay να μην αφαιρέσω το do 
custom_stopwords <- setdiff(my_stopwords, keep_words)
# Αφαίρεση stopwords
cleaned_text <- removeWords(cleaned_text, custom_stopwords)

# Stemming
cleaned_text <- sapply(cleaned_text, function(x) {
  
  # split sentence into words
  words <- unlist(strsplit(x, " ")) #tokenization
  
  # stemming
  words <- wordStem(words, language = "english")
  
  # join words again
  paste(words, collapse = " ")
})


# Aποτελέσματα
head(cleaned_text)

# Δημιουργία corpus
corpus <- Corpus(VectorSource(cleaned_text))

# Δημιουργία term matrix
dtm <- DocumentTermMatrix(corpus)
# Κρατάω λέξεις που εμφανίζονται τουλάχιστον σε 3-4 κριτικές
dtm_clean <- removeSparseTerms(dtm, 0.993) 

# View matrix
inspect(dtm_clean)

# Συχνότητα λέξεων
word_freq <- colSums(as.matrix(dtm_clean))

# Sort descending
word_freq <- sort(word_freq, decreasing = TRUE)

# Οι Top 20 λέξεις
head(word_freq, 20)

# Εxport corpus
corpus_text <- sapply(corpus, as.character)
write.csv(corpus_text, "corpus_export2.csv", row.names = FALSE)

# Εxport word frequency
word_freq <- sort(colSums(as.matrix(dtm_clean)), decreasing = TRUE)
word_freq_df <- data.frame(
  word = names(word_freq),
  freq = as.numeric(word_freq)
)
write.csv(word_freq_df, "word_frequency2.csv", row.names = FALSE)
