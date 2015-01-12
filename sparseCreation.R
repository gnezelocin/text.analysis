# sparseCreation.R
# Purpose: Create Sparse Matrix
# Wei Ng & Nicole Zeng
# Written : 11/06/2014
# Latest review: 2014-11-13 (nicole)

# packages
require(dplyr)
require(tm)
require(tau)
require(multicore)
require(SnowballC)
require(Matrix)


# datasets
load("~/hbsdata/sandbox/my.sample.profile.df.RData")
load("~/hbsdata/sandbox/feature.dictionary.RData")
load("~/hbsdata/sandbox/wordcount.df.RData")

# Stem the words
stems <- sapply(my.sample.profile.df$word, stemDocument)
stem.output <- vector("character", dim(my.sample.profile.df)[1])
for ( i in 1:length(stem.output) ) {
stem.output[i] <- stemDocument(my.sample.profile.df$word[i])
cat("Processing:",i,"of",length(stem.output),"\n")
}

# put stems in working df
my.sample.profile.df$stem.output <- stem.output

#
table(my.sample.profile.df$essay_number)
my.sample.essays <- my.sample.profile.df[!duplicated(paste(my.sample.profile.df$obfuserid, my.sample.profile.df$essay_number)),]
table(my.sample.essays$essay_number)
user.no.essays <- table(my.sample.essays$obfuserid)

all.ten  <- user.no.essays[user.no.essays == 10]
my.sample.all.ten.df <- subset(my.sample.profile.df, obfuserid %in% names(all.ten) )
my.sample.summary.df <- subset(my.sample.all.ten.df, essay_number == 0 )

# Sort wordcount.df
wordcount.df <- wordcount.df[order(wordcount.df$multinomial, decreasing = T),]
rownames(wordcount.df) <- c()
wordcount.df$multi.id <- as.numeric(rownames(wordcount.df))
rownames(my.sample.summary.df) <- c()

# REAL Merge
my.sample.summary.df <- left_join(my.sample.summary.df, wordcount.df, by = "word")
my.sample.summary.df <- subset(my.sample.all.ten.df, essay_number == 0 )
wordcount.df <- wordcount.df[1:dim(feature.dictionary)[1],]


# At-the-time Bandaid
which(table(wordcount.df$word) > 1)
doublecount <- which(table(wordcount.df$word) > 1)
subset(wordcount.df, word %in% names(doublecount) )
problematics <- as.numeric(rownames(subset(wordcount.df, word %in% names(doublecount) )))
problematics <- problematics[-c(1,2)]
wordcount.df <- wordcount.df[-problematics,]
my.sample.summary.df <- left_join(my.sample.summary.df, wordcount.df, by = "word")

# Get RowIDs
longform.rows <- as.numeric(as.factor(my.sample.summary.df$obfuserid) )
unique(longform.rows)[order(unique(longform.rows))] ### Is this necessary?
longform.obfids <- as.character(as.factor(my.sample.summary.df$obfuserid) )
longform.userkey <- data.frame(longform.obfids, longform.rows)
longform.userkey <- longform.userkey[!duplicated(longform.userkey$longform.rows),]
rownames(longform.userkey) <- c()
save(longform.userkey, file = "longform.userkey.RData")

# Get ColIDs
longform.col <- as.numeric(my.sample.summary.df$multi.id)
longform.val <- as.numeric(my.sample.summary.df$count)

# Create Sparse Matrix
sparse_matrix <- sparseMatrix(i = longform.rows, j = longform.col, x = longform.val)

# Create Full Matrix
full.matrix <- as.matrix(sparse_matrix)

# Get ColIDs
sparse.colnames <- subset(wordcount.df, multi.id %in% longform.col)$word
word.intersect <- subset(wordcount.df, multi.id %in% longform.col)
max(word.intersect$multi.id)

table(my.sample.summary.df$word)[order(table(my.sample.summary.df$word),decreasing = T)]
new.index <- table(my.sample.summary.df$word)[order(table(my.sample.summary.df$word),decreasing = T)]
new.index <- aggregate(my.sample.summary.df$count, list(my.sample.summary.df$word), sum)
new.index <- new.index[order(new.index$x, decreasing = T),]
rownames(new.index) <- c()
new.index$multi.index <- as.numeric(rownames(new.index) )
colnames(new.index)[1:2] <- c("word","total.count")

my.sample.summary.df <- left_join(my.sample.summary.df, new.index, by = "word")
longform.col <- as.numeric(my.sample.summary.df$multi.index)

# Create Sparse Matrix
sparse_matrix <- sparseMatrix(i = longform.rows, j = longform.col, x = longform.val)
colnames(sparse_matrix) <- new.index$word
full_matrix <- as.matrix(sparse_matrix)


# Save files
save(longform.userkey, file = "~/hbsdata/sandbox/matrix.output/longform.userkey.RData")
save(sparse_matrix, file = "~/hbsdata/sandbox/matrix.output/sparse.matrix.RData")
save(sparse_matrix, file = "~/hbsdata/sandbox/matrix.output/sparse.matrix.essay0.sample.RData")
save(new.index, file = "~/hbsdata/sandbox/matrix.output/new.index.RData")
save(wordcount.df, file = "~/hbsdata/sandbox/wordcount.df.RData.v2")
