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
library(textreuse)
library(quanteda)
library(stringi)
library(syuzhet)

# Load text file (entire book)
bookText <- read.delim("DrJekyllAndMrHyde.txt")
bookText

# Check numrows
nrow(bookText)

# Check head
head(bookText)

# Create VCorpus of book with chapters separated out 
bookCorpus <- VCorpus(DirSource("text/",ignore.case = TRUE,mode="text"))
bookCorpus

# Inspect Chapter corpus 
inspect(bookCorpus)

# Check structure of chapter corpus
str(bookCorpus)

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






