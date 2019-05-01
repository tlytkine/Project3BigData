# Load required libraries 
library(devtools)
library(textreuse)
library(SnowballC)
library(wordcloud)
library(NLP)
library(rJava)
library(wordnet)
library(tm)
library(zipfR)
library(quanteda)
library(stringi)
library(syuzhet)
library(corpus)
library(openNLP)
library(readr)
library(stringr)


# Load text file (entire book)
lines <- read_lines("DrJekyllAndMrHyde.txt", skip = 0, n_max = -1L)
lines
# Check numrows
nrow(lines)

# Check head
head(lines)

# Create VCorpus of book with chapters separated out 
bookCorpus <- VCorpus(DirSource("text/",ignore.case = TRUE,mode="text"))
bookCorpus

# Inspect Chapter corpus 
inspect(bookCorpus)

# Check structure of chapter corpus
str(bookCorpus)

# Function to convert text to sentences 
convert_text_to_sentences <- function(text, lang = "en") {
  # Function to compute sentence annotations using the Apache OpenNLP Maxent sentence detector employing the default model for language 'en'. 
  sentence_token_annotator <- Maxent_Sent_Token_Annotator(language = lang)
  
  # Convert text to class String from package NLP
  text <- as.String(text)
  
  # Sentence boundaries in text
  sentence.boundaries <- annotate(text, sentence_token_annotator)
  
  # Extract sentences
  sentences <- text[sentence.boundaries]
  
  # return sentences
  return(sentences)
}
sentences <- convert_text_to_sentences(lines)
str(sentences)
sentences[1]
summary(sentences)
str_length(sentences[2])

# Sort array by number of characters 
sentences$numChar <- nchar(sentences)
sentences$numChar
sortedSentences <- sentences[order(-sentences$numChar)]

# Find 10 longest sentences
tenLongest <- sortedSentences[1:10]
tenLongest

tenLongest <- tenLongest[order(nwords(tenLongest))]
tenLongest



# Function to count number of words 
nwords <- function(string, pseudo=F){
  ifelse( pseudo, 
          pattern <- "\\S+", 
          pattern <- "[[:alpha:]]+" 
  )
  str_count(string, pattern)
}

# Find number of words in each sentence (add to report)
for(i in 1:10){ 
  print(nwords(tenLongest[i]))
  sprintf("\n")
}


# Extract book from corpus 
intro <- bookCorpus[1]
chapter1 <- bookCorpus[2]
chapter2 <- bookCorpus[3]
chapter3 <- bookCorpus[4]
chapter4 <- bookCorpus[5]
chapter5 <- bookCorpus[6]
chapter6 <- bookCorpus[7]
chapter7 <- bookCorpus[8]
chapter8 <- bookCorpus[9]
chapter9 <- bookCorpus[10]
chapter10 <- bookCorpus[11]
end <- bookCorpus[12]







# Create document term matrix for bookCorpus (rows are documents, terms are words)
bookDTM <- DocumentTermMatrix(bookCorpus)

# Shows non sparse entries, sparsity, maximum term length etc.
# 12 documents, 6385 terms, Non-/sparse entries: 10488/66132
# Sparsity: 86 % 
# Maximal Term Length: 30
# Weighting: term frequency(tf)
bookDTM
bookDTM$dimnames
bookDTM$dimnames$Terms
bookDTM$dimnames$Docs

# Create term document matrix for bookCorpus (rows are words, columns are documents)
bookTDM <- TermDocumentMatrix(bookCorpus)

# Shows same details as bookDTM 
bookTDM
bookTDM$dimnames 
bookTDM$dimnames$Terms[1:1000]
bookTDM$dimnames$Docs[1:12]


# Shows common words from each file in book corpus term document matrix, words are mostly fillers tho
inspect(bookTDM)


# Deciding between TDM or DTM means processing by words or documents ^^
# Document Term Frequency

# Term frequency for everything
bookTF <- termFreq(bookTDM$dimnames$Terms)
bookTF

# Convert to data frame 
bookDF <- as.data.frame(bookTF)
bookDF

# Start clean up of corpus
# Convert corpus to lower case 
cleanCorpus <- tm_map(bookCorpus,content_transformer(tolower))
# Function to remove punctuation and numbers 
removeNumPunct <- function(x) gsub("[^[:alpha:][:space:]]*","",x)
# Remove numbers and punctuation
cleanCorpus <- tm_map(cleanCorpus,content_transformer(removeNumPunct))
# toSpace function
toSpace <- content_transformer(function (x,pattern) gsub(pattern,"",x))
# Remove "/" symbols
cleanCorpus <- tm_map(cleanCorpus, toSpace, "/")
# Remove "@" symbols
cleanCorpus <- tm_map(cleanCorpus, toSpace, "@")
# Remove "|" symbols
cleanCorpus <- tm_map(cleanCorpus, toSpace, "\\|")
# Define stopwords (can bind additional stopwords to this)
myStopWords <- c(stopwords('english'))
# Remove stopwords 
cleanCorpus <- tm_map(cleanCorpus,removeWords,c(stopwords('english')))
# (Removing stopwords & punctuation yields a set of documents with content bearing words)
# Compute the TDM without the stopwords 
cleanTDM <- TermDocumentMatrix(cleanCorpus)
# Observe the # of entries, the sparsity and maximal term length 
cleanTDM
# Effect of cleaning VCorpus thus far 
# Before: 6385 terms, After: 4280 terms 
# Before: Sparsity 86%, After: Sparsity 85% 
# Before: Maximal Term Length 30, After: Maximal Term Length 23 

cleanTDM2 <- TermDocumentMatrix(cleanCorpus, control = list(wordlengths=c(1,Inf)))
cleanTDM2
# Finding Frequent Words 
# Find terms with a frequency of 4 or more 
freq4Plus <- findFreqTerms(cleanTDM2,lowfreq=4)
freq4Plus

# Find associations 
# Test 
statesAssoc <- findAssocs(cleanTDM2,"states",0.5)
statesAssoc
# Find terms with frequency of 80 or more 
freq80Plus <- findFreqTerms(cleanTDM2,lowfreq=80)
freq80Plus
# 5 most common terms: "hyde","jekyll","project","said","utterson"
# Associations with 5 most common terms 
findAssocs(cleanTDM2,"hyde",0.5)
findAssocs(cleanTDM2,"jekyll",0.5)
findAssocs(cleanTDM2,"project",0.5)
findAssocs(cleanTDM2,"said",0.5)
findAssocs(cleanTDM2,"utterson",0.5)

# Term Frequency
termFreq <- rowSums(as.matrix(cleanTDM2))
termFreqSub <- subset(termFreq,termFreq >=6)
termFreqdf <- as.data.frame(names(termFreq),freq=termFreq)
termFreq
# Check out tf-idf (term frequency-inverse document frequency): numerical
# statistic that is intended to reflect how important a word is to a document in a 
# collection or corpus 

# Binary/Boolean Term Frequency check
termFreq("word",cleanTDM2)
# There is also term count 

# Remove Sparse Terms from cleanTDM2
sparseTDM <- removeSparseTerms(cleanTDM2,sparse=0.95)
inspect(sparseTDM)
sparseTDM

# Finding Informative Words 
inspect(chapter1)

# Exercise for students: given the list of interesting words in an article, determine how to 
# find the position of each word in the original article,
# Then, determine how to compute the distance between each pair of words. (distMatrix)

# FIGURE OUT ABOVE 
# Clustering Terms via Dendrogram 
# fit <- hclust(distMatrix, method = "ward.D2")
# plot(fit)

# Word Cloud 
m1 <- as.matrix(cleanTDM2)
word.freq <- sort(rowSums(m1),decreasing = T)
word.freq
pal <- brewer.pal(9,"BuGn")
pal <- pal[-(1:4)]
wordcloud(words = names(word.freq),freq=word.freq,min.freq=3,random.order=F,colors=pal)

# Frequency Analysis - I 
cleanDTM <- DocumentTermMatrix(cleanCorpus)
freq <- colSums(as.matrix(cleanDTM))
cleanDTM
length(freq) # total number of terms 

# Sort freq in descending order 
ord <- order(freq,decreasing=TRUE)
# Most frequently occuring terms 
freq[head(ord)]
# Least frequently occuring terms
freq[tail(ord)]

# Reducing data: word lengths between 4 and 20 
cleanDTMR <- DocumentTermMatrix(cleanCorpus,control=list(wordLengths=c(4,20)))
cleanDTMR
freqr <- colSums(as.matrix(cleanDTMR))
ordr <- order(freqr, decreasing = TRUE)
freqr[head(ordr)]
freqr[tail(ordr)]

# Using TD-IDF

# Stopwords
# -terms that have td-idf value 0 or close to 0 
# -words that appear in all of the documents so idf term is zero

# Important Words
# -Sort td-idf values in decreasing order
# -Terms at top after sorting are most important 

# Text Clustering
# -Calculate tf-idf score for collection of documents 
# -Calculate pairwise distance matrix using cosine distance algorithm
# -Perform hierarchical clustering and visualize the results with a dendrogram

# N-gram: finds n words in a sequence in a text (cooccuring words in given window)
# unigram, n gram size 1 
# bigram,  n gram size 2 
# trigram, n gram size 3 
# n > = 2 find phrases that have greater meaning than just individual words 
# NgramsK = X - (N - 1)
# X is num words in given sentence K 

# Above is very useful (bigram, unigram representation etc.)
# TreeBnk representation pretty cool visualization

# Text Analysis: Tokenization

# Qquanted token functions useful
# stringi package useful
# Same format as below 
# bookCorpus[2][[1]][["content"]]

clBegin <- bookCorpus$content[1][[1]][["content"]]
clChapter1 <- bookCorpus$content[2][[1]][["content"]]
clChapter2 <- bookCorpus$content[3][[1]][["content"]]
clChapter3 <- bookCorpus$content[4][[1]][["content"]]
clChapter4 <- bookCorpus$content[5][[1]][["content"]]
clChapter5 <- bookCorpus$content[6][[1]][["content"]]
clChapter6 <- bookCorpus$content[7][[1]][["content"]]
clChapter7 <- bookCorpus$content[8][[1]][["content"]]
clChapter8 <- bookCorpus$content[9][[1]][["content"]]
clChapter9 <- bookCorpus$content[10][[1]][["content"]]
clChapter10 <- bookCorpus$content[11][[1]][["content"]]
clEnd <- bookCorpus$content[12][[1]][["content"]]

# get distances
# Chapter 1 
x1 <- VCorpus(DirSource("text/chapter1/",ignore.case = TRUE,mode="text"))
x1TDM <- TermDocumentMatrix(x1)
x1TDM
ch1Dist <- dist(x1TDM)
# Chapter 2 
x2 <- VCorpus(DirSource("text/chapter2/",ignore.case = TRUE,mode="text"))
x2TDM <- TermDocumentMatrix(x2)
x2TDM
ch2Dist <- dist(x2TDM)
# Chapter 3 
x3 <- VCorpus(DirSource("text/chapter3/",ignore.case = TRUE,mode="text"))
x3TDM <- TermDocumentMatrix(x3)
x3TDM
ch3Dist <- dist(x3TDM)
# Chapter 4 
x4 <- VCorpus(DirSource("text/chapter4/",ignore.case = TRUE,mode="text"))
x4TDM <- TermDocumentMatrix(x4)
x4TDM
ch4Dist <- dist(x4TDM)
# Chapter 5 
x5 <- VCorpus(DirSource("text/chapter5/",ignore.case = TRUE,mode="text"))
x5TDM <- TermDocumentMatrix(x5)
x5TDM
ch5Dist <- dist(x5TDM)
# Chapter 6 
x6 <- VCorpus(DirSource("text/chapter6/",ignore.case = TRUE,mode="text"))
x6TDM <- TermDocumentMatrix(x6)
x6TDM
ch6Dist <- dist(x6TDM)
# Chapter 7
x7 <- VCorpus(DirSource("text/chapter7/",ignore.case = TRUE,mode="text"))
x7TDM <- TermDocumentMatrix(x7)
x7TDM
ch7Dist <- dist(x7TDM)
# Chapter 8
x8 <- VCorpus(DirSource("text/chapter8/",ignore.case = TRUE,mode="text"))
x8TDM <- TermDocumentMatrix(x8)
x8TDM
ch8Dist <- dist(x8TDM)
# Chapter 9
x9 <- VCorpus(DirSource("text/chapter9/",ignore.case = TRUE,mode="text"))
x9TDM <- TermDocumentMatrix(x9)
x9TDM
ch9Dist <- dist(x9TDM)
# Chapter 10
x10 <- VCorpus(DirSource("text/chapter10/",ignore.case = TRUE,mode="text"))
x10TDM <- TermDocumentMatrix(x10)
x10TDM
ch10Dist <- dist(x10TDM)

# Dendrograms 
# Chapter 1 
fitCH1 <- hclust(ch1Dist,method="ward.D2")
fitCH1
plot(fitCH1)
# Chapter 2
fitCH2 <- hclust(ch2Dist,method="ward.D2")
fitCH2
plot(fitCH2)
# Chapter 3 
fitCH3 <- hclust(ch3Dist,method="ward.D2")
fitCH3
plot(fitCH3)
# Chapter 4 
fitCH4 <- hclust(ch4Dist,method="ward.D2")
fitCH4
plot(fitCH4)
# Chapter 5 
fitCH5 <- hclust(ch5Dist,method="ward.D2")
fitCH5
plot(fitCH5)
# Chapter 6 
fitCH6 <- hclust(ch6Dist,method="ward.D2")
fitCH6
plot(fitCH6)
# Chapter 7 
fitCH7 <- hclust(ch7Dist,method="ward.D2")
fitCH7
plot(fitCH7)
# Chapter 8 
fitCH8 <- hclust(ch8Dist,method="ward.D2")
fitCH8
plot(fitCH8)
# Chapter 9 
fitCH9 <- hclust(ch9Dist,method="ward.D2")
fitCH9
plot(fitCH9)
# Chapter 10 
fitCH10 <- hclust(ch10Dist,method="ward.D2")
fitCH10
plot(fitCH10)

# Ex: Chapter1 extracted 
clChapter1
# Extract tokens from Chapter 1 
# quanteda::tokens vs textreuse::tokens etc. (overlaps so specify package name)
clChapter1Tokens <- quanteda::tokens(clChapter1)
clChapter1Tokens
head(clChapter1Tokens)
# Read description of dfm in Quanteda 
# Apply dfm to clChapter1, (is there difference between applying tokens() & dfm())
dfm(clChapter1)
# Yes: Document-feature matrix of: 229 documents, 821 features (98.6% sparse).

# From syuzhet package 
# Returns data frame in which each row represents sentence in og file 
# 1 for each emotoin type as well as positive or negative sentiment valence 
# Allows us to take body of text and return which emotions it represents &
# whether emotion is positive or negative 

# Assessing chapters for sentiments 

# Beginning / Intro before ch1 
clIntroSent <- syuzhet::get_nrc_sentiment(clBegin)
clIntroSent 
head(clIntroSent)
# Row sum of sentiments 
introRowSum <- rowSums(clIntroSent)
introRowSum
# Column sum of sentiments 
introColSum <- colSums(clIntroSent)
introColSum

# Chapter 1 
clChapter1Sent <- syuzhet::get_nrc_sentiment(clChapter1)
clChapter1Sent
head(clChapter1Sent)
# Row sum of sentiments 
chapter1RowSum <- rowSums(clChapter1Sent)
chapter1RowSum
# Column sum of sentiments 
chapter1ColSum <- colSums(clChapter1Sent)
chapter1ColSum

# Chapter 2
clChapter2Sent <- syuzhet::get_nrc_sentiment(clChapter2)
clChapter2Sent
head(clChapter2Sent)
# Row sum of sentiments 
chapter2RowSum <- rowSums(clChapter2Sent)
chapter2RowSum
# Column sum of sentiments 
chapter2ColSum <- colSums(clChapter2Sent)
chapter2ColSum

# Chapter 3 
clChapter3Sent <- syuzhet::get_nrc_sentiment(clChapter3)
clChapter3Sent
head(clChapter3Sent)
# Row sum of sentiments 
chapter3RowSum <- rowSums(clChapter3Sent)
chapter3RowSum
# Column sum of sentiments 
chapter3ColSum <- colSums(clChapter3Sent)
chapter3ColSum

# Chapter 4
clChapter4Sent <- syuzhet::get_nrc_sentiment(clChapter4)
clChapter4Sent
head(clChapter4Sent)
# Row sum of sentiments 
chapter4RowSum <- rowSums(clChapter4Sent)
chapter4RowSum
# Column sum of sentiments 
chapter4ColSum <- colSums(clChapter4Sent)
chapter4ColSum

# Chapter 5 
clChapter5Sent <- syuzhet::get_nrc_sentiment(clChapter5)
clChapter5Sent
head(clChapter5Sent)
# Row sum of sentiments 
chapter5RowSum <- rowSums(clChapter5Sent)
chapter5RowSum
# Column sum of sentiments 
chapter5ColSum <- colSums(clChapter5Sent)
chapter5ColSum

# Chapter 6
clChapter6Sent <- syuzhet::get_nrc_sentiment(clChapter6)
clChapter6Sent
head(clChapter6Sent)
# Row sum of sentiments 
chapter6RowSum <- rowSums(clChapter6Sent)
chapter6RowSum
# Column sum of sentiments 
chapter6ColSum <- colSums(clChapter6Sent)
chapter6ColSum

# Chapter 7
clChapter7Sent <- syuzhet::get_nrc_sentiment(clChapter7)
clChapter7Sent
head(clChapter7Sent)
# Row sum of sentiments 
chapter7RowSum <- rowSums(clChapter7Sent)
chapter7RowSum
# Column sum of sentiments 
chapter7ColSum <- colSums(clChapter7Sent)
chapter7ColSum

# Chapter 8
clChapter8Sent <- syuzhet::get_nrc_sentiment(clChapter8)
clChapter8Sent
head(clChapter8Sent)
# Row sum of sentiments 
chapter8RowSum <- rowSums(clChapter8Sent)
chapter8RowSum
# Column sum of sentiments 
chapter8ColSum <- colSums(clChapter8Sent)
chapter8ColSum

# Chapter 9 
clChapter9Sent <- syuzhet::get_nrc_sentiment(clChapter9)
clChapter9Sent
head(clChapter9Sent)
# Row sum of sentiments 
chapter9RowSum <- rowSums(clChapter9Sent)
chapter9RowSum
# Column sum of sentiments 
chapter9ColSum <- colSums(clChapter9Sent)
chapter9ColSum

# Chapter 10
clChapter10Sent <- syuzhet::get_nrc_sentiment(clChapter10)
clChapter10Sent
head(clChapter10Sent)
# Row sum of sentiments 
chapter10RowSum <- rowSums(clChapter10Sent)
chapter10RowSum
# Column sum of sentiments 
chapter10ColSum <- colSums(clChapter10Sent)
chapter10ColSum

# End / after chapter 10
clEndSent <- syuzhet::get_nrc_sentiment(clEnd)
clEndSent 
head(clEndSent)
# Row sum of sentiments 
EndRowSum <- rowSums(clEndSent)
EndRowSum
# Column sum of sentiments 
EndColSum <- colSums(clEndSent)
EndColSum



# Text Weighting (Document Frequency of Words)
ch1DFM <- dfm(clChapter1Tokens)
ch1Freq <- docfreq(ch1DFM)
ch1Freq

ch1weights <- dfm_weight(ch1DFM,scheme="prop")
str(ch1weights)


ch1TFIDF <- dfm_tfidf(ch1DFM,scheme_tf="count",scheme_df="inverse")
str(ch1TFIDF)
ch1TFIDF@i

# Word Cloud for Chapter 1
ch1 <- as.matrix(x1TDM)
word1.freq <- sort(rowSums(ch1),decreasing = T)
word1.freq
pal1 <- brewer.pal(9,"BuGn")
pal1 <- pal1[-(1:4)]
wordcloud(words = names(word1.freq),freq=word1.freq,min.freq=3,random.order=F,colors=pal1)

# Word Cloud for Chapter 2
ch2 <- as.matrix(x2TDM)
word2.freq <- sort(rowSums(ch2),decreasing = T)
word2.freq
pal2 <- brewer.pal(9,"BuGn")
pal2 <- pal[-(1:4)]
wordcloud(words = names(word2.freq),freq=word2.freq,min.freq=3,random.order=F,colors=pal2)

# Word Cloud for Chapter 3
ch3 <- as.matrix(x3TDM)
word3.freq <- sort(rowSums(ch3),decreasing = T)
word3.freq
pal3 <- brewer.pal(9,"BuGn")
pal3 <- pal3[-(1:4)]
wordcloud(words = names(word3.freq),freq=word3.freq,min.freq=3,random.order=F,colors=pal3)


# Word Cloud for Chapter 4
ch4 <- as.matrix(x4TDM)
word4.freq <- sort(rowSums(ch4),decreasing = T)
word4.freq
pal4 <- brewer.pal(9,"BuGn")
pal4 <- pal4[-(1:4)]
wordcloud(words = names(word4.freq),freq=word4.freq,min.freq=3,random.order=F,colors=pal4)


# Word Cloud for Chapter 5
ch5 <- as.matrix(x5TDM)
word5.freq <- sort(rowSums(ch5),decreasing = T)
word5.freq
pal5 <- brewer.pal(9,"BuGn")
pal5 <- pal5[-(1:4)]
wordcloud(words = names(word5.freq),freq=word5.freq,min.freq=3,random.order=F,colors=pal5)


# Word Cloud for Chapter 6
ch6 <- as.matrix(x6TDM)
word6.freq <- sort(rowSums(ch6),decreasing = T)
word6.freq
pal6 <- brewer.pal(9,"BuGn")
pal6 <- pal6[-(1:4)]
wordcloud(words = names(word6.freq),freq=word6.freq,min.freq=3,random.order=F,colors=pal6)

# Word Cloud for Chapter 7
ch7 <- as.matrix(x7TDM)
word7.freq <- sort(rowSums(ch7),decreasing = T)
word7.freq
pal7 <- brewer.pal(9,"BuGn")
pal7 <- pal7[-(1:4)]
wordcloud(words = names(word7.freq),freq=word7.freq,min.freq=3,random.order=F,colors=pal7)

# Word Cloud for Chapter 8
ch8 <- as.matrix(x8TDM)
word8.freq <- sort(rowSums(ch8),decreasing = T)
word8.freq
pal8 <- brewer.pal(9,"BuGn")
pal8 <- pal8[-(1:4)]
wordcloud(words = names(word8.freq),freq=word8.freq,min.freq=3,random.order=F,colors=pal8)

# Word Cloud for Chapter 9
ch9 <- as.matrix(x9TDM)
word9.freq <- sort(rowSums(ch9),decreasing = T)
word9.freq
pal9 <- brewer.pal(9,"BuGn")
pal9 <- pal9[-(1:4)]
wordcloud(words = names(word9.freq),freq=word9.freq,min.freq=3,random.order=F,colors=pal9)

# Word Cloud for Chapter 10
ch10 <- as.matrix(x10TDM)
word10.freq <- sort(rowSums(ch10),decreasing = T)
word10.freq
pal10 <- brewer.pal(9,"BuGn")
pal10 <- pal10[-(1:4)]
wordcloud(words = names(word10.freq),freq=word10.freq,min.freq=3,random.order=F,colors=pal10)



# 1d. Prior to removing the punctuation, find the longest word and longest sentences 
# in each chapter. Print a table of the length of the shortest and longest sentences
# in each chapter 
# Chapter 1 
# Get lines from Chapter 1 
ch1Lines <- read_lines("text/Chapter01.txt", skip = 0, n_max = -1L)
# Convert text to sentences 
ch1Sentences <- convert_text_to_sentences(ch1Lines)
# Sort array by number of characters 
ch1Sentences$numChar <- nchar(ch1Sentences)
ch1SortedSentences <- sentences[order(ch1Sentences$numChar)]
ch1SortedSentences
# Find longest sentence 
ch1Longest <-ch1SortedSentences[1]
ch1Longest
# Length of longest sentence
str_length(ch1Longest)
# Number of words in longest sentence 
nwords(ch1Longest)
# Find shortest Sentence 
ch1Shortest <- ch1SortedSentences[108]
ch1Shortest
# Length of shortest sentence 
str_length(ch1Shortest)
# Number of words in shortest sentence 
nwords(ch1Shortest)
# Longest word
# Extract words 
x1DTM <- DocumentTermMatrix(x1)
ch1Words <- x1DTM$dimnames$Terms
ch1NumWords <- length(x1DTM$dimnames$Terms)
maxWordLength <- 0
maxWordCh1 <- "max"


for(i in 1:ch1NumWords){
  print(i)
  sprintf("\n")
  print("Length:")
  print(nchar(ch1Words[i]))
  if((nchar(ch1Words[i]) > maxWordLength)){
    maxWordLength <- nchar(ch1Words[i])
    maxWordCh1 <- ch1Words[i]
  }
}
print(maxWordLength)
print(maxWordCh1)

# Chapter 2
# Longest Word 
# Longest Sentence 
# Get lines from Chapter 2 
ch2Lines <- read_lines("text/Chapter02.txt", skip = 0, n_max = -1L)
# Convert text to sentences 
ch2Sentences <- convert_text_to_sentences(ch2Lines)
str(ch2Sentences)
# Sort array by number of characters 
ch2Sentences$numChar <- nchar(ch2Sentences)
ch2Sentences$numChar
ch2SortedSentences <- sentences[order(-ch2Sentences$numChar)]
ch2SortedSentences
# Find longest sentence 
ch2Longest <-ch2SortedSentences[1]
ch2Longest

# Length of longest sentence
str_length(ch2Longest)

# Number of words in longest sentence 
nwords(ch2Longest)

# Find shortest Sentence 
ch2Shortest <- ch2SortedSentences[136]
ch2Shortest

# Length of shortest sentence 
str_length(ch2Shortest)

# Number of words in shortest sentence 
nwords(ch2Shortest)


# Longest word
# Extract words 
x2DTM <- DocumentTermMatrix(x2)
ch2Words <- x2DTM$dimnames$Terms
ch2Words

ch2NumWords <- length(x2DTM$dimnames$Terms)
ch2NumWords 

maxWordLength2 <- 0
maxWordCh2 <- "max"


for(i in 1:ch2NumWords){
  print(i)
  sprintf("\n")
  print("Length:")
  print(nchar(ch2Words[i]))
  if((nchar(ch2Words[i]) > maxWordLength2)){
    maxWordLength2 <- nchar(ch2Words[i])
    maxWordCh2 <- ch2Words[i]
  }
}
print(maxWordLength2)
print(maxWordCh2)

# Chapter 3
# Longest Word 
# Longest Sentence 
# Get lines from Chapter 3
ch3Lines <- read_lines("text/Chapter03.txt", skip = 0, n_max = -1L)
ch3Lines
# Convert text to sentences 
ch3Sentences <- convert_text_to_sentences(ch3Lines)
str(ch3Sentences)
# Sort array by number of characters 
ch3Sentences$numChar <- nchar(ch3Sentences)
ch3Sentences$numChar
ch3SortedSentences <- sentences[order(ch3Sentences$numChar)]
# Find longest sentence 
ch3Longest <-ch1SortedSentences[1]
ch3Longest

# Length of longest sentence
str_length(ch3Longest)

# Number of words in longest sentence 
nwords(ch3Longest)

# Find shortest Sentence 
ch3Shortest <- ch3SortedSentences[108]
ch3Shortest

# Length of shortest sentence 
str_length(ch3Shortest)

# Number of words in shortest sentence 
nwords(ch3Shortest)


# Longest word
# Extract words 
x3DTM <- DocumentTermMatrix(x3)
ch3Words <- x3DTM$dimnames$Terms
ch3Words

ch3NumWords <- length(x3DTM$dimnames$Terms)
ch3NumWords 

maxWordLength3 <- 0
maxWordCh3 <- "max"


for(i in 1:ch3NumWords){
  print(i)
  sprintf("\n")
  print("Length:")
  print(nchar(ch3Words[i]))
  if((nchar(ch3Words[i]) > maxWordLength)){
    maxWordLength <- nchar(ch3Words[i])
    maxWordCh3 <- ch3Words[i]
  }
}
print(maxWordLength)
print(maxWordCh3)

# Chapter 4
# Longest Word 
# Longest Sentence 
# Get lines from Chapter 4 
ch4Lines <- read_lines("text/Chapter04.txt", skip = 0, n_max = -1L)
ch4Lines
# Convert text to sentences 
ch4Sentences <- convert_text_to_sentences(ch4Lines)
str(ch4Sentences)
# Sort array by number of characters 
ch4Sentences$numChar <- nchar(ch4Sentences)
ch4Sentences$numChar
ch4SortedSentences <- sentences[order(ch4Sentences$numChar)]
# Find longest sentence 
ch4Longest <-ch1SortedSentences[4]
ch4Longest

# Length of longest sentence
str_length(ch4Longest)

# Number of words in longest sentence 
nwords(ch4Longest)

# Find shortest Sentence 
ch4Shortest <- ch4SortedSentences[108]
ch4Shortest

# Length of shortest sentence 
str_length(ch4Shortest)

# Number of words in shortest sentence 
nwords(ch4Shortest)


# Longest word
# Extract words 
x4DTM <- DocumentTermMatrix(x4)
ch4Words <- x4DTM$dimnames$Terms
ch4Words

ch4NumWords <- length(x4DTM$dimnames$Terms)
ch4NumWords 

maxWordLength4 <- 0
maxWordCh4 <- "max"


for(i in 1:ch4NumWords){
  print(i)
  sprintf("\n")
  print("Length:")
  print(nchar(ch4Words[i]))
  if((nchar(ch4Words[i]) > maxWordLength)){
    maxWordLength <- nchar(ch4Words[i])
    maxWordCh4 <- ch4Words[i]
  }
}
print(maxWordLength)
print(maxWordCh4)

# Chapter 5
# Longest Word 
# Longest Sentence 
# Get lines from Chapter 5 
ch5Lines <- read_lines("text/Chapter05.txt", skip = 0, n_max = -1L)
ch5Lines
# Convert text to sentences 
ch5Sentences <- convert_text_to_sentences(ch5Lines)
str(ch5Sentences)
# Sort array by number of characters 
ch5Sentences$numChar <- nchar(ch5Sentences)
ch5Sentences$numChar
ch5SortedSentences <- sentences[order(ch5Sentences$numChar)]
# Find longest sentence 
ch5Longest <-ch1SortedSentences[5]
ch5Longest

# Length of longest sentence
str_length(ch5Longest)

# Number of words in longest sentence 
nwords(ch5Longest)

# Find shortest Sentence 
ch5Shortest <- ch5SortedSentences[108]
ch5Shortest

# Length of shortest sentence 
str_length(ch5Shortest)

# Number of words in shortest sentence 
nwords(ch5Shortest)


# Longest word
# Extract words 
x5DTM <- DocumentTermMatrix(x5)
ch5Words <- x5DTM$dimnames$Terms
ch5Words

ch5NumWords <- length(x5DTM$dimnames$Terms)
ch5NumWords 

maxWordLength5 <- 0
maxWordCh5 <- "max"


for(i in 1:ch5NumWords){
  print(i)
  sprintf("\n")
  print("Length:")
  print(nchar(ch5Words[i]))
  if((nchar(ch5Words[i]) > maxWordLength)){
    maxWordLength <- nchar(ch5Words[i])
    maxWordCh5 <- ch5Words[i]
  }
}
print(maxWordLength)
print(maxWordCh5)

# Chapter 6
# Longest Word 
# Longest Sentence 
# Get lines from Chapter 6 
ch6Lines <- read_lines("text/Chapter06.txt", skip = 0, n_max = -1L)
ch6Lines
# Convert text to sentences 
ch6Sentences <- convert_text_to_sentences(ch6Lines)
str(ch6Sentences)
# Sort array by number of characters 
ch6Sentences$numChar <- nchar(ch6Sentences)
ch6Sentences$numChar
ch6SortedSentences <- sentences[order(ch6Sentences$numChar)]
# Find longest sentence 
ch6Longest <-ch1SortedSentences[6]
ch6Longest

# Length of longest sentence
str_length(ch6Longest)

# Number of words in longest sentence 
nwords(ch6Longest)

# Find shortest Sentence 
ch6Shortest <- ch6SortedSentences[108]
ch6Shortest

# Length of shortest sentence 
str_length(ch6Shortest)

# Number of words in shortest sentence 
nwords(ch6Shortest)


# Longest word
# Extract words 
x6DTM <- DocumentTermMatrix(x6)
ch6Words <- x6DTM$dimnames$Terms
ch6Words

ch6NumWords <- length(x6DTM$dimnames$Terms)
ch6NumWords 

maxWordLength6 <- 0
maxWordCh6 <- "max"


for(i in 1:ch6NumWords){
  print(i)
  sprintf("\n")
  print("Length:")
  print(nchar(ch6Words[i]))
  if((nchar(ch6Words[i]) > maxWordLength)){
    maxWordLength <- nchar(ch6Words[i])
    maxWordCh6 <- ch6Words[i]
  }
}
print(maxWordLength)
print(maxWordCh6)

# Chapter 7
# Longest Word 
# Longest Sentence 
# Get lines from Chapter 7 
ch7Lines <- read_lines("text/Chapter07.txt", skip = 0, n_max = -1L)
ch7Lines
# Convert text to sentences 
ch7Sentences <- convert_text_to_sentences(ch7Lines)
str(ch7Sentences)
# Sort array by number of characters 
ch7Sentences$numChar <- nchar(ch7Sentences)
ch7Sentences$numChar
ch7SortedSentences <- sentences[order(ch7Sentences$numChar)]
# Find longest sentence 
ch7Longest <-ch1SortedSentences[7]
ch7Longest

# Length of longest sentence
str_length(ch7Longest)

# Number of words in longest sentence 
nwords(ch7Longest)

# Find shortest Sentence 
ch7Shortest <- ch7SortedSentences[108]
ch7Shortest

# Length of shortest sentence 
str_length(ch7Shortest)

# Number of words in shortest sentence 
nwords(ch7Shortest)


# Longest word
# Extract words 
x7DTM <- DocumentTermMatrix(x7)
ch7Words <- x7DTM$dimnames$Terms
ch7Words

ch7NumWords <- length(x7DTM$dimnames$Terms)
ch7NumWords 

maxWordLength7 <- 0
maxWordCh7 <- "max"


for(i in 1:ch7NumWords){
  print(i)
  sprintf("\n")
  print("Length:")
  print(nchar(ch7Words[i]))
  if((nchar(ch7Words[i]) > maxWordLength)){
    maxWordLength <- nchar(ch7Words[i])
    maxWordCh7 <- ch7Words[i]
  }
}
print(maxWordLength)
print(maxWordCh7)

# Chapter 8
# Longest Word 
# Longest Sentence 
# Get lines from Chapter 8 
ch8Lines <- read_lines("text/Chapter08.txt", skip = 0, n_max = -1L)
ch8Lines
# Convert text to sentences 
ch8Sentences <- convert_text_to_sentences(ch8Lines)
str(ch8Sentences)
# Sort array by number of characters 
ch8Sentences$numChar <- nchar(ch8Sentences)
ch8Sentences$numChar
ch8SortedSentences <- sentences[order(ch8Sentences$numChar)]
# Find longest sentence 
ch8Longest <-ch1SortedSentences[8]
ch8Longest

# Length of longest sentence
str_length(ch8Longest)

# Number of words in longest sentence 
nwords(ch8Longest)

# Find shortest Sentence 
ch8Shortest <- ch8SortedSentences[108]
ch8Shortest

# Length of shortest sentence 
str_length(ch8Shortest)

# Number of words in shortest sentence 
nwords(ch8Shortest)


# Longest word
# Extract words 
x8DTM <- DocumentTermMatrix(x8)
ch8Words <- x8DTM$dimnames$Terms
ch8Words

ch8NumWords <- length(x8DTM$dimnames$Terms)
ch8NumWords 

maxWordLength8 <- 0
maxWordCh8 <- "max"


for(i in 1:ch8NumWords){
  print(i)
  sprintf("\n")
  print("Length:")
  print(nchar(ch8Words[i]))
  if((nchar(ch8Words[i]) > maxWordLength)){
    maxWordLength <- nchar(ch8Words[i])
    maxWordCh8 <- ch8Words[i]
  }
}
print(maxWordLength)
print(maxWordCh8)

# Chapter 9
# Longest Word 
# Longest Sentence 
# Get lines from Chapter 9 
ch9Lines <- read_lines("text/Chapter09.txt", skip = 0, n_max = -1L)
ch9Lines
# Convert text to sentences 
ch9Sentences <- convert_text_to_sentences(ch9Lines)
str(ch9Sentences)
# Sort array by number of characters 
ch9Sentences$numChar <- nchar(ch9Sentences)
ch9Sentences$numChar
ch9SortedSentences <- sentences[order(ch9Sentences$numChar)]
# Find longest sentence 
ch9Longest <-ch1SortedSentences[9]
ch9Longest

# Length of longest sentence
str_length(ch9Longest)

# Number of words in longest sentence 
nwords(ch9Longest)

# Find shortest Sentence 
ch9Shortest <- ch9SortedSentences[108]
ch9Shortest

# Length of shortest sentence 
str_length(ch9Shortest)

# Number of words in shortest sentence 
nwords(ch9Shortest)


# Longest word
# Extract words 
x9DTM <- DocumentTermMatrix(x9)
ch9Words <- x9DTM$dimnames$Terms
ch9Words

ch9NumWords <- length(x9DTM$dimnames$Terms)
ch9NumWords 

maxWordLength9 <- 0
maxWordCh9 <- "max"


for(i in 1:ch9NumWords){
  print(i)
  sprintf("\n")
  print("Length:")
  print(nchar(ch9Words[i]))
  if((nchar(ch9Words[i]) > maxWordLength)){
    maxWordLength <- nchar(ch9Words[i])
    maxWordCh9 <- ch9Words[i]
  }
}
print(maxWordLength)
print(maxWordCh9)

# Chapter 10
# Longest Word 
# Longest Sentence 
# Get lines from Chapter 10 
ch10Lines <- read_lines("text/Chapter10.txt", skip = 0, n_max = -1L)
ch10Lines
# Convert text to sentences 
ch10Sentences <- convert_text_to_sentences(ch10Lines)
str(ch10Sentences)
# Sort array by number of characters 
ch10Sentences$numChar <- nchar(ch10Sentences)
ch10Sentences$numChar
ch10SortedSentences <- sentences[order(ch10Sentences$numChar)]
# Find longest sentence 
ch10Longest <-ch1SortedSentences[1]
ch10Longest

# Length of longest sentence
str_length(ch10Longest)

# Number of words in longest sentence 
nwords(ch10Longest)

# Find shortest Sentence 
ch10Shortest <- ch10SortedSentences[108]
ch10Shortest

# Length of shortest sentence 
str_length(ch10Shortest)

# Number of words in shortest sentence 
nwords(ch10Shortest)


# Longest word
# Extract words 
x10DTM <- DocumentTermMatrix(x10)
ch10Words <- x10DTM$dimnames$Terms
ch10Words

ch10NumWords <- length(x10DTM$dimnames$Terms)
ch10NumWords 

maxWordLength10 <- 0
maxWordCh10 <- "max"


for(i in 1:ch10NumWords){
  print(i)
  sprintf("\n")
  print("Length:")
  print(nchar(ch10Words[i]))
  if((nchar(ch10Words[i]) > maxWordLength)){
    maxWordLength <- nchar(ch10Words[i])
    maxWordCh10 <- ch10Words[i]
  }
}
print(maxWordLength)
print(maxWordCh10)

# 1e. Use WordNet to mark the parts of speech for the first chapter for nouns and verbs having a length
# of 5 or greater 

# 1f. Analyze word frequency using functions from package zipfR 

# 1g. Generate bigrams and trigrams for all words whose length is greater than 6 characters in Chapter 1.

# 1h. Process the text from the data document using corpusTools, stringi, corpustools, quanted, 
# and tidytext. Describe the methods you use, the results you get, and what you understand 
# about the theme of the book.

# 1i. By now, you should see that Data Science is an empirical science.
# So, these packages provide tools that can give greater insight into the text. 
# At a minimum, choose three (3) functions from each package and apply them to Chapter 1.






