# getEssays.R
# Purpose: Get dictionaries and sparse matrices for a certain Essay for our cohort sample
# Nicole Zeng
# Latest review: 11/13/2014
# Written : 11/13/2014

# This time, we are getting essay 4. The steps for getting dictionaries and sparse matrices are:
# 1. Identify userids
# 2. Subset profile.text file with #1
# 2.5 import dictionary with key.tsv - same code as translate.R
# 2.5.1 Clean up dictionary (remove whitespace, remove duplicates)
# 2.5.2 merge dictionary with profile.text
# 3. Count #2, sort by multinomial count
# 4. Feature Selection #3 - same code as feature.selection.R
# 5. Create Sparse Matrix - same code as sparseCreation.R

# packages
require(dplyr)
require(tm)
require(tau)
require(multicore)
require(SnowballC)
require(Matrix)
require(stringr)

# datasets
load("~/hbsdata/sandbox/all.users/cohort.profile.df.RData")
load("~/hbsdata/sandbox/meta.data/cohortids.all.RData")

# Subset on cohortids, essay 4
cohort.profile.text.4.df <- subset(cohort.profile.df, essay_number==4)

# Multinomial Word Counts for each essay:
cohort.wordcount.essay4.df <- aggregate(cohort.profile.text.4.df$count, list(cohort.profile.text.4.df$word_id), sum)
colnames(cohort.wordcount.essay4.df) <- c("word_id", "multi.count")

# Import dictionary
dictionary <- read.table("~/hbsdata/sandbox/key.tsv", sep = "\t", stringsAsFactors = F, header = T, quote = "", colClasses="character")
dictionary$word <- str_trim(dictionary$word, side="both")
dictionary$word <- gsub(" ","", dictionary$word, fixed=TRUE)
x <- which(duplicated(dictionary$word))
dictionary.nodup <- dictionary[-x,]
save(dictionary, file="~/hbsdata/sandbox/essay.4/dictionary.original.RData")
remove(dictionary)
dictionary <- dictionary.nodup

# order wordcounts by count
cohort.wordcount.essay4.df <- cohort.wordcount.essay4.df[order(cohort.wordcount.essay4.df$multi.count, decreasing = T),]
head(cohort.wordcount.essay4.df)
# reset row index
rownames(cohort.wordcount.essay4.df) <- c()

# merge wordcounts with dictionary
cohort.wordcount.dictionary.essay4.df <- merge(cohort.wordcount.essay4.df, dictionary, by="word_id", all=FALSE)
cohort.wordcount.dictionary.essay4.df <- cohort.wordcount.dictionary.essay4.df[order(cohort.wordcount.dictionary.essay4.df$multi.count, decreasing = T),]
rownames(cohort.wordcount.dictionary.essay4.df) <- c()
# remove first column from wordcount.df
cohort.wordcount.dictionary.essay4.df <- cohort.wordcount.dictionary.essay4.df[,-1]

# Create column for the numerator of the corpus's word cumulative distr. function (CDF).
cohort.wordcount.dictionary.essay4.df$multi.cumsum <- cumsum(cohort.wordcount.dictionary.essay4.df$multi.count)
head(cohort.wordcount.dictionary.essay4.df)

# Create CDF for the essay 4 dictionary words
cohort.wordcount.dictionary.essay4.df$percent <- cohort.wordcount.dictionary.essay4.df$multi.cumsum/sum(cohort.wordcount.dictionary.essay4.df$multi.count)
head(cohort.wordcount.dictionary.essay4.df)

# Get the last row ID for the words that account for 95% of the variance
max(which(cohort.wordcount.dictionary.essay4.df$percent < 0.95))
# should be index= 7198. check:
cohort.wordcount.dictionary.essay4.df[7198,]

# Get the last row ID for the words that account for 99% of the variance
max(which(cohort.wordcount.dictionary.essay4.df$percent < 0.99))
# should be 17104. check:
cohort.wordcount.dictionary.essay4.df[17104,]
# word count is 5, so we assign the last row index for ~99.+% of the variance by the last word id where word count == 5.
feature.stop.index <- max(which(cohort.wordcount.dictionary.essay4.df$multi.count == 5))
feature.stop.index # should be N=17233.

# create a df of words that explain 99% of the variance
feature.4.df <- cohort.wordcount.dictionary.essay4.df[c(1:feature.stop.index),]
# create a subset df of the dictionary for the selected features, use merge to keep the wordcounts, too
feature.dictionary.4 <- merge(feature.4.df, dictionary, by="word", all.x=TRUE, all.y=FALSE)
feature.dictionary.4 <- feature.dictionary.4[order(feature.dictionary.4$multi.count, decreasing=TRUE),]
# reset row index
rownames(feature.dictionary.4) <- c()
# create new index for words from selected features
feature.dictionary.4$word_id <- NULL
feature.dictionary.4$word_id <- rownames(feature.dictionary.4)

# create new df of cohort.profile.text.4.df with featured words
cohort.profile.features.4.df <- subset(cohort.profile.text.4.df, word %in% feature.dictionary.4$word)
cohort.profile.features.4.df$word_id <- NULL
cohort.profile.features.4.df <- merge(cohort.profile.features.4.df, feature.dictionary.4, by="word", all.x=FALSE, all.y=TRUE)
head(cohort.profile.features.4.df)
x <- which(is.na(cohort.profile.features.4.df$obfuserid))
length(x)

# Get Row IDS
cohort.profile.features.4.df$rows <- as.numeric(as.factor(cohort.profile.features.4.df$obfuserid) )
longform.rows <- cohort.profile.features.4.df$rows
max(longform.rows) == n_distinct(cohort.profile.features.4.df$obfuserid)
longform.obfids <- as.character(as.factor(cohort.profile.features.4.df$obfuserid) )
longform.userkey <- data.frame(longform.obfids, longform.rows)
longform.userkey <- longform.userkey[!duplicated(longform.userkey$longform.rows),]
head(longform.userkey)
dim(longform.userkey)[1] == n_distinct(cohort.profile.features.4.df$obfuserid)
head(longform.userkey)
rownames(longform.userkey) <- c()
head(longform.userkey)

# get Col IDs
longform.col <- as.numeric(cohort.profile.features.4.df$word_id)
head(longform.col)
# get Matrix values
longform.val <- as.numeric(cohort.profile.features.4.df$count)
head(longform.val)
sparse.matrix.4 <- sparseMatrix(i = longform.rows, j = longform.col, x = longform.val)

#Create Sparse Matrix
sparse_matrix <- sparseMatrix(i = sparse.row.4, j = sparse.col.4, x = longform.val)
colnames(sparse_matrix) <- longform.wordkey$longform.colnames
full_matrix <- as.matrix(sparse_matrix)
head(sparse_matrix)
head(full_matrix)
head(new.index)


# Save files
save(cohort.profile.features.4.df, file="~/hbsdata/sandbox/essay.4/cohort.profile.features.4.df.RData")
save(cohort.profile.text.4.df, file="~/hbsdata/sandbox/essay.4/cohort.profile.text.4.df.RData")
save(cohort.wordcount.essay4.df, file="~/hbsdata/sandbox/essay.4/cohort.wordcount.essay4.df.RData")
save(cohort.wordcount.dictionary.essay4.df, file="~/hbsdata/sandbox/essay.4/cohort.wordcount.dictionary.essay4.df.RData")
save(cohort.wordcount.dictionary.essay4.noblanks.df, file="~/hbsdata/sandbox/essay.4/cohort.wordcount.dictionary.essay4.noblanks.df.RData")
save(feature.dictionary.4, file = "~/hbsdata/sandbox/essay.4/feature.dictionary.4.RData")
save(longform.userkey, file = "~/hbsdata/sandbox/essay.4/longform.userkey.RData")
save(sparse_matrix, file = "~/hbsdata/sandbox/essay.4/sparse.matrix.RData")
save(cohort.profile.features.4.df, file = "~/hbsdata/sandbox/essay.4/cohort.profile.features.4.df.RData")
save(dictionary.nodup, file="~/hbsdata/sandbox/essay.4/dictionary.nodup.RData")
save(full_matrix, file="~/hbsdata/sandbox/essay.4/full.matrix.RData")
save(feature.4.df, file="~/hbsdata/sandbox/essay.4/features.4.df.RData")
