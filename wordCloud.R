# Filename: wordCloud.R
# Purpose: Set up text data for creating wordclouds
# Date: 11/08/2014
# Last edited: 11/08/2014
# By: Nicole Zeng

# packages:
library(wordcloud)
library(tm)
# library(stringr)
library(RColorBrewer)

# data
load("wordcount.df.RData")
load("sparse.matrix.essay0.sample.RData")
load("longform.userkey.RData")

# clean up corpus
corp <- wordcount.df
corp <- tm_map(corp, removePunctuation)
corp <- tm_map(corp, removePunctuation)
corp <- tm_map(corp, tolower)
corp <- tm_map(corp, removeNumbers)
# corp <- tm_map(corp, function(x)removeWords(x,stopwords()))

# create term matrix
term.matrix <- TermDocumentMatrix(corp)
term.matrix <- as.matrix(term.matrix)
colnames(term.matrix) <- c("group 1","group 2")

# word clouds
comparison.cloud(term.matrix,max.words=300,random.order=FALSE)
commonality.cloud(term.matrix,random.order=FALSE)


