# install.packages("tm")
# install.packages("textreuse")
# install.packages("wordnet")
# install.packages("zipfR")
# install.packages("rjJava")
# install.packages("wordcloud")
library(devtools)
library(textreuse)
library(wordcloud)
library(wordnet)
library(tm)
library(zipfR)
# Project 3 Text Analytics 

# Load text file 
project_data <- read.delim("text/DrJekyllAndMrHyde.txt")
project_data

project_data

# Check numrows
nrow(project_data)

# Check head
head(project_data)


# Create a VCorpus - I (Voltatile corpus aka collection of documents containing natural language text 
# using the infrastructure of the package tm)
# Contains all files in working directory 
dataCorpus <- VCorpus(DirSource("text/",ignore.case = TRUE,mode="text"))
dataCorpus
inspect(dataCorpus)
str(dataCorpus)
dataCorpus[1]

data1 <- dataCorpus[[1]]
data1
dataDTM <- DocumentTermMatrix(dataCorpus)
dataDTM
inspect(dataDTM[1,100])

dataTDM <- TermDocumentMatrix(dataCorpus)
dataTDM
inspect(dataTDM[10,1])

data1TF <- termFreq(data1)
data1TF
data1TF[1:100]

data1DF <- as.data.frame(data1TF)
data1DF
data1DF[1]

# Data corpus lower case 
dataLow <- tm_map(dataCorpus,content_transformer(tolower))
dataLow


removeNumPunct <- function(x) gsub("[^[:alpha:][:space:]]*","",x)

dataCl <- tm_map(dataLow,content_transformer(removeNumPunct))
dataCl

toSpace <- content_transformer(function (x,pattern) gsub(pattern,"",x))

dataCl <- tm_map(dataCl,toSpace,"/")
dataCl <- tm_map(dataCl,toSpace,"@")
dataCl <- tm_map(dataCl,toSpace,"\\|")

myStopWords <- c(stopwords('english'))
myStopWords

dataCl <- tm_map(dataCl,removeWords,myStopWords)
inspect(dataCl[1:3])
str(dataCl)
dataCl
dataClDTM <- DocumentTermMatrix(dataCl)
dataClDTM
inspect(dataClDTM)
dataClTDM <- TermDocumentMatrix(dataCl)
dataClTDM 
inspect(dataClTDM)

freqTerms <- findFreqTerms(dataClDTM,lowfreq=5)
freqTerms

# statesAssoc <- findAssocs(dataClDTM,"dimnames[2]$Terms",0.1)
# statesAssoc

# findAssocs(dataClDTM,"dimnames",0.1)

for(var in 1:4280){
  print(dataClDTM$dimnames[2]$Terms[var])
}

termFreq1 <- rowSums(as.matrix(dataClDTM))
termFreq1

termFreq1Sub <- subset(termFreq1, termFreq1 >= 1)
termFreq1Sub

termFreq1DF <- as.data.frame(names(termFreq1),freq=termFreq1)
termFreq1DF

# computing the TDM without the stopwords

DATAtdm2 <- TermDocumentMatrix(DATAstop, control = list(wordlengths = c(1,Inf)))
DATAtdm2

# finding terms with a frequency of greater than 4

freqTerms <- findFreqTerms(DATAtdm2, lowfreq = 4)
freqTerms

# strength of correlation between search and result terms

statesAssoc <- findAssocs(DATAtdm2, "states", 0.5)
statesAssoc

# finding the frequency of terms

termFreq <- rowSums(as.matrix(DATAtdm2))
termFreqSub <- subset(termFreq, termFreq >= 6)
termFreqdf <- as.data.frame(names(termFreq), freq = termFreq)
termFreq

# removing sparse terms

DATAtdm2
sparsetdm2 <- removeSparseTerms(DATAtdm2, sparse = 0.75)
inspect(sparsetdm2)
sparsetdm2

# finding informative words maybe

test1 <- DATAstop[[1]]
inspect(test1)

# dendrogram

fit <- hclust(dataDTM, method="ward.D2")
plot(fit)

# starting a word cloud

m1 <- as.matrix(DATAtdm2)
word.freq <- sort(rowSums(m1), decreasing = T)
word.freq

pal <- brewer.pal(9, "BuGn")
pal <- pal[-(1:4)]
wordcloud(words = names(word.freq), freq = word.freq, min.freq = 10, random.order = F, colors = pal)

# frequency analysis starting

dataDTM<-DocumentTermMatrix(DATAstop)
freq<-colSums(as.matrix(dataDTM))
dataDTM

length(freq)

ord <- order(freq, decreasing = TRUE)
freq[head(ord)]

freq[tail(ord)]

DATAdtmr <- DocumentTermMatrix(DATAstop, control=list(wordLengths=c(4, 20)))
DATAdtmr

freqr <- colSums(as.matrix(dtmr))
ordr <- order(freqr, decreasing = TRUE)
freqr[head(ordr)]

freqr[tail(ordr)]



# Notes: Lecture 8
# Text analytics = analysis of structured and unstructured data to obtain and organize information
# Extraction of information from structured and unstructured text 
# Resolution of entities
# Co-reference analysis of pronouns and nouns 
# Mapping of information to ontological structures
# Synthesize knowledge in a knowledge structure 
# Understand content to generate actionable intelligence 
# Uncover patterns and themes in the text 
# Quantitative methods can be applied to extract info from text (turning text into numbers)
# 80% of info in organization represented as unstructured text 

# Terminology 
# Document is a sentence or composed of sentences 
# Tokens represent words. EX: "men", "liberty" ...
# Terms represent single words or multiple words as units/phrases ex: "civil war"
# Corpus is a collection of documents 
# Stopwards: set of commonly used words which you want to exclude while analyzing text 
#   ex: 'a','an','the','to','of','ABC company',etc...
# Document term matrix: matrix consisting of documents in a row and terms in a column 
# Sparse term: Terms occuring only in a very few documents (sentences)
# Tokenization: process of splitting unstructured data into token such as a words, phrases,
# sentences, but mostly words. 
# Stemming: Extracting core word from its variants such as "interesting", "interest' and "interested"
# are all stemmed to "interest"
# Polarity: Whether a document or sentence is positive, negative or neutral, commonly used in
# sentiment analysis
# Bag-of-words: each sentence (or document) is a bag of words ignoring grammar and even word order 
# Part of speech tagging: involves tagging/assigning every word in the document a part of speech
# noun, verb, adjective, pronoun, single noun, plur noun, etc. (Can be difficult bc context, ex: "like")
# Term Frequency - Inverse Document Frequency (td-idf): measures how important a word is based on its frequency
# of occurence, TF(t) = (Number of times a term t appears)/(Total number of terms)
# Inverse Document Frequency: measures how important a word is 
# IDF(t) = log to base e(Total number of documents/ Number of documents containing term t)
# Term Frequency Inverse Document Frequency
# tfidf = tf*idf

# Computational Linguistics 
# morphology = study of structure & form of words 
# syntax = study of how words and phrases form sentences 
# semantics = study of meaning of words & statements 
# phonology: study of sounds in languages (think homonyms)
# pragamatics: study of idiomatic phrases that cannot be analyzed with strict sentiment analysis









