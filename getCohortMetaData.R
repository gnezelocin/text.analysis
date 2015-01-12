# getCohortMetaData.R
# Purpose: Gather meta data for our cohort
# Nicole Zeng
# Latest review: 11/11/14
# Written : 11/11/14

# packages
require(plyr)

# datasets
load("~/hbsdata/master/demog2.RData")
load("~/hbsdata/master/demog.prof.RData")
load("~/hbsdata/master/demog.axes.RData")
load("~/hbsdata/sandbox/all.users/my.df.RData")
load("~/hbsdata/master/messages.RData")
load("~/hbsdata/master/views.RData")
load("~/hbsdata/master/votes.RData")

# Get cohort ids
cohortids <- unique(my.df$obfuserid)

# Create cohort subsets of meta data
# Demographic Profile Data
cohort.demog <- subset(demog2, userid %in% cohortids)
cohort.demog.categ <- subset(demog.prof, userid %in% cohortids)
cohort.demog.axes <- subset(demog.axes, userid %in% cohortids)
remove(cohort.demog, cohort.demog.categ, cohort.demog.axes)

# Messages
cohort.messages.sent <- subset(messages, senderid %in% cohortids)
cohort.messages.sent <- subset(cohort.messages.sent, timestamp < "2013-11-01")
cohort.messages.sent.count <- count(cohort.messages.sent, vars=c("senderid"))
colnames(cohort.messages.sent.count) <- c("userid", "n.msg.sent")
cohort.messages.received <- subset(messages, receiverid %in% cohortids)
cohort.messages.received <- subset(cohort.messages.received, timestamp < "2013-11-01")
remove(messages)
# Count # of messages received
cohort.messages.received.count.all <- count(cohort.messages.received, vars=c("receiverid", "qm"))
cohort.messages.received.count <- subset(cohort.messages.received.count.all, qm!="mutual_match" & qm!="staff_greeting")
cohort.messages.received.count$qm <- NULL
colnames(cohort.messages.received.count) <- c("userid", "n.msg.received")
# Merge messaging activity
cohort.messages.received.sent.count <- merge(cohort.messages.received.count, cohort.messages.sent.count, by.x="userid", by.y="userid", all=TRUE)
remove(cohort.messages.sent, cohort.messages.sent.count, cohort.messages.received, cohort.messages.received.count.all, cohort.messages.received.sent.count)

# Votes
cohort.votes.made <- subset(votes, voterid %in% cohortids)
cohort.votes.made <- subset(cohort.votes.made, timestamp < "2013-11-01")
cohort.votes.received <- subset(votes, voteeid %in% cohortids)
cohort.votes.received <- subset(cohort.votes.received, timestamp < "2013-11-01")
cohort.votes.received$vote <- as.numeric(cohort.votes.received$vote)
remove(votes)
# Collapse average vote score
cohort.votes.rec.nonzero <- subset(cohort.votes.received, vote > 0)
cohort.votes.scores.nonzero <- aggregate(cohort.votes.rec.nonzero$vote, list(cohort.votes.rec.nonzero$voteeid), mean)
colnames(cohort.votes.scores.nonzero) <- c("userid", "avg.vote.nozeros")
cohort.votes.scores <- aggregate(cohort.votes.received$vote, list(cohort.votes.received$voteeid), mean)
colnames(cohort.votes.scores) <- c("userid", "avg.vote.withzeros")
# Merge cohort scores
cohort.votes.received.scores <- merge(cohort.votes.scores, cohort.votes.scores.nonzero, by.x="userid", by.y="userid", all=TRUE)
# Count # of votes per user in cohort
cohort.votes.received.count <- count(cohort.votes.received, vars=c("voteeid"))
colnames(cohort.votes.received.count) <- c("userid", "n.votes.received")
# Count # of each vote for users
cohort.votes.received.count.perscore <- count(cohort.votes.received, vars=c("voteeid", "vote"))
colnames(cohort.votes.received.count.perscore) <- c("userid", "vote.score", "freq.score")
cohort.votes.rec.zeros <- subset(cohort.votes.received.count.perscore, vote.score==0)
cohort.votes.rec.zeros$vote.score <- NULL
colnames(cohort.votes.rec.zeros) <- c("userid", "n.zeros")
cohort.votes.rec.ones <- subset(cohort.votes.received.count.perscore, vote.score==1)
cohort.votes.rec.ones$vote.score <- NULL
colnames(cohort.votes.rec.ones) <- c("userid", "n.ones")
cohort.votes.rec.twos <- subset(cohort.votes.received.count.perscore, vote.score==2)
cohort.votes.rec.twos$vote.score <- NULL
colnames(cohort.votes.rec.twos) <- c("userid", "n.twos")
cohort.votes.rec.threes <- subset(cohort.votes.received.count.perscore, vote.score==3)
cohort.votes.rec.threes$vote.score <- NULL
colnames(cohort.votes.rec.threes) <- c("userid", "n.threes")
cohort.votes.rec.fours <- subset(cohort.votes.received.count.perscore, vote.score==4)
cohort.votes.rec.fours$vote.score <- NULL
colnames(cohort.votes.rec.fours) <- c("userid", "n.fours")
cohort.votes.rec.fives <- subset(cohort.votes.received.count.perscore, vote.score==5)
cohort.votes.rec.fives$vote.score <- NULL
colnames(cohort.votes.rec.fives) <- c("userid", "n.fives")
# Merge all cohort vote scores
cohort.vote.rec.perscore.merged.df <- merge(cohort.votes.rec.zeros, cohort.votes.rec.twos, by="userid", all=TRUE)
cohort.vote.rec.perscore.merged.df <- merge(cohort.vote.rec.perscore.merged.df, cohort.votes.rec.threes, by="userid", all=TRUE)
cohort.vote.rec.perscore.merged.df <- merge(cohort.vote.rec.perscore.merged.df, cohort.votes.rec.fours, by="userid", all=TRUE)
cohort.vote.rec.perscore.merged.df <- merge(cohort.vote.rec.perscore.merged.df, cohort.votes.rec.fives, by="userid", all=TRUE)
remove(cohort.votes.made, cohort.votes.received, cohort.votes.scores.nonzero, cohort.votes.rec.nonzero, cohort.votes.scores)

# Views
cohort.views.made <- subset(views, viewerid %in% cohortids)
cohort.views.made <- subset(cohort.views.made, timestamp_converted < "2013-11-01")
cohort.views.received <- subset(views, vieweeid %in% cohortids)
cohort.views.received <- subset(cohort.views.received, timestamp_converted < "2013-11-01")
cohort.views.rec.count <- count(cohort.views.received, vars=c("vieweeid"))
colnames(cohort.views.rec.count) <- c("userid", "n.views.received")
remove(views)
remove(cohort.views.made, cohort.views.received)

# Merge all cohort activity
cohort.outcomes <- merge(cohort.votes.received.scores, cohort.vote.rec.perscore.merged.df, by.x="userid", by.y="userid", all.x=TRUE, all.y=TRUE)
cohort.outcomes <- merge(cohort.outcomes, cohort.messages.received.sent.count, by.x="userid", by.y="userid", all.x=TRUE, all.y=TRUE)
cohort.outcomes <- merge(cohort.outcomes, cohort.views.rec.count, by.x="userid", by.y="userid", all=TRUE)

# Merge activity with demog
load("~/hbsdata/sandbox/meta.data/cohort.demog.RData")
load("~/hbsdata/sandbox/meta.data/cohort.demog.categ.RData")
load("~/hbsdata/sandbox/meta.data/cohort.demog.axes.RData")
cohort.demog.activity <- merge(cohort.demog, cohort.outcomes, by.x="userid", by.y="userid", all=TRUE)
cohort.demog.categ.activity <- merge(cohort.demog.categ, cohort.outcomes, by.x="userid", by.y="userid", all=TRUE)
cohort.demog.axes.activity <- merge(cohort.demog.axes, cohort.outcomes, by.x="userid", by.y="userid", all=TRUE)


# save files
print("Done with script!")

save(cohortids, file="~/hbsdata/sandbox/meta.data/cohortids.all.RData")
save(cohort.outcomes, file="~/hbsdata/sandbox/meta.data/cohort.outcomes.RData")
save(cohort.demog.activity, file="~/hbsdata/sandbox/meta.data/cohort.demog.activity.RData")
save(cohort.demog.categ.activity, file="~/hbsdata/sandbox/meta.data/cohort.demog.categ.activity.RData")
save(cohort.demog.axes.activity, file="~/hbsdata/sandbox/meta.data/cohort.demog.axes.activity.RData")
save(cohort.demog, file="~/hbsdata/sandbox/meta.data/cohort.demog.RData")
save(cohort.demog.categ, file="~/hbsdata/sandbox/meta.data/cohort.demog.categ.RData")
save(cohort.demog.axes, file="~/hbsdata/sandbox/meta.data/cohort.demog.axes.RData")
save(cohort.messages.sent, file="~/hbsdata/sandbox/meta.data/cohort.messages.sent.RData")
save(cohort.messages.sent.count, file="~/hbsdata/sandbox/meta.data/cohort.messages.sent.count.RData")
save(cohort.messages.received, file="~/hbsdata/sandbox/meta.data/cohort.messages.received.RData")
save(cohort.messages.received.count, file="~/hbsdata/sandbox/meta.data/cohort.messages.received.count.noqm.RData")
save(cohort.messages.received.count.all, file="~/hbsdata/sandbox/meta.data/cohort.messages.received.count.RData")
save(cohort.messages.received.sent.count, file="~/hbsdata/sandbox/meta.data/cohort.messages.sent.received.count.noqm.RData")
save(cohort.votes.made, file="~/hbsdata/sandbox/meta.data/cohort.votes.made.RData")
save(cohort.votes.received, file="~/hbsdata/sandbox/meta.data/cohort.votes.received.RData")
save(cohort.votes.rec.nonzero, file="~/hbsdata/sandbox/meta.data/cohort.votes.rec.nonzero.RData")
save(cohort.votes.scores.nonzero, file="~/hbsdata/sandbox/meta.data/cohort.votes.scores.nonzero.RData")
save(cohort.votes.scores, file="~/hbsdata/sandbox/meta.data/cohort.votes.scores.RData")
save(cohort.votes.received.scores, file="~/hbsdata/sandbox/meta.data/cohort.votes.received.scores.RData")
save(cohort.vote.rec.perscore.merged.df, file="~/hbsdata/sandbox/meta.data/cohort.vote.rec.perscore.merged.df.RData")
save(cohort.views.made, file="~/hbsdata/sandbox/meta.data/cohort.views.made.RData")
save(cohort.views.received, file="~/hbsdata/sandbox/meta.data/cohort.views.received.RData")
save(cohort.views.rec.count, file="~/hbsdata/sandbox/meta.data/cohort.views.rec.count.RData")
