# File: firstpassText.R
# Date: 11/06/2014
# Last Edited: 11/08/2014
# By: Nicole Zeng

# packages
require(data.table)
require(dplyr)
require(stringr)
require(lubridate)

# data
load("demog.sample.essay0.df.RData")
load("longform.userkey.RData")
load("sparse.matrix.essay0.sample.RData")

# subset ethnicities - base
demog.sample.essay0.df$white <- demog.sample.essay0.df$ethn=="white"
demog.sample.essay0.df$asian <- demog.sample.essay0.df$ethn=="asian"
demog.sample.essay0.df$black <- demog.sample.essay0.df$ethn=="black"
demog.sample.essay0.df$latin <- demog.sample.essay0.df$ethn=="latin"
demog.sample.essay0.df$indian <- demog.sample.essay0.df$ethn=="indian"
demog.sample.essay0.df$pacisl <- demog.sample.essay0.df$ethn=="pacisl"
demog.sample.essay0.df$mideast <- demog.sample.essay0.df$ethn=="mideast"
demog.sample.essay0.df$natam <- demog.sample.essay0.df$ethn=="natam"
demog.sample.essay0.df$other <- demog.sample.essay0.df$ethn=="other"

# subset ethnicities - data.table
demog.sample.essay0.df[, white:=ethn=="white",]
demog.sample.essay0.df[, asian:=ethn=="asian",]
demog.sample.essay0.df[, black:=ethn=="black",]
demog.sample.essay0.df[, latin:=ethn=="latin",]
demog.sample.essay0.df[, indian:=ethn=="indian",]
demog.sample.essay0.df[, pacisl:=ethn=="pacisl",]
demog.sample.essay0.df[, mideast:=ethn=="mideast",]
demog.sample.essay0.df[, natam:=ethn=="natam",]
demog.sample.essay0.df[, ethn.other:=ethn=="other",]

# subset gender - data.table
demog.sample.essay0.df[, female:=gender=="female"]
demog.sample.essay0.df[, male:=gender=="male"]

# Run getAge.R

# subset age - data.table
demog.sample.essay0.df[, young:=age>=18L,]
demog.sample.essay0.df[, mid.age:=age>=24.5,]
demog.sample.essay0.df[, mid.age2:=age>=35.5,]
demog.sample.essay0.df[, old:=age>=45.5,]
demog.sample.essay0.df[, g.age:=young+mid.age+mid.age2+old,]
table(demog.sample.essay0.df$age, as.logical(demog.sample.essay0.df$g.age))
demog.sample.essay0.df[young==TRUE, age.bin:="18-25"]
demog.sample.essay0.df[mid.age==TRUE, age.bin:="25-35"]
demog.sample.essay0.df[mid.age2==TRUE, age.bin:="35-45"]
demog.sample.essay0.df[old==TRUE, age.bin:="45+"]
table(demog.sample.essay0.df$age.bin)

#### Summary Stats
demog.sample.essay0.df[, list(mmean(user.wc), CI(user.wc), (.N)), by=c("gender")]
demog.sample.essay0.df[, list(mmean(user.wc), CI(user.wc), (.N)), by=c("age.bin")]
demog.sample.essay0.df[, list(mmean(user.wc), CI(user.wc), (.N)), by=c("gender", "age.bin")]
demog.sample.essay0.df[, list(mmean(user.wc), CI(user.wc), (.N)), by=c("white", "gender")]
demog.sample.essay0.df[, list(mmean(user.wc), CI(user.wc), (.N)), by=c("asian", "gender")]
demog.sample.essay0.df[, list(mmean(user.wc), CI(user.wc), (.N)), by=c("black", "gender")]

#### Word Count Densities
graph <- ggplot(demog.sample.essay0.df, aes(user.wc, fill = age.bin)) + geom_histogram(alpha = 0.5, aes(y = ..density..), position = 'identity')
graph + ggtitle("Distribution of Word Counts by Age Bracket")
graph + xlab("Word Count") + ylab("Density")
graph + scale_fill_discrete(name="Age Brackets")
