# Models - sample essay 0 

demog.df.AF <- subset(demog.votes.df, gender=="female" & asian==TRUE)
demog.df.WF <- subset(demog.votes.df, gender=="female" & white==TRUE)
demog.df.BF <- subset(demog.votes.df, gender=="female" & black==TRUE)
demog.df.IF <- subset(demog.votes.df, gender=="female" & indian==TRUE)
demog.df.NF <- subset(demog.votes.df, gender=="female" & natam==TRUE)
demog.df.LF <- subset(demog.votes.df, gender=="female" & latin==TRUE)

demog.df.AM <- subset(demog.votes.df, gender=="male" & asian==TRUE)
demog.df.WM <- subset(demog.votes.df, gender=="male" & white==TRUE)
demog.df.BM <- subset(demog.votes.df, gender=="male" & black==TRUE)
demog.df.IM <- subset(demog.votes.df, gender=="male" & indian==TRUE)
demog.df.NM <- subset(demog.votes.df, gender=="male" & natam==TRUE)
demog.df.LM <- subset(demog.votes.df, gender=="male" & latin==TRUE)


demog.votes.df[asian ==TRUE, ethn1:="asian"]
demog.votes.df[white ==TRUE, ethn1:="white"]
demog.votes.df[black ==TRUE, ethn1:="black"]
demog.votes.df[latin ==TRUE, ethn1:="latin"]
demog.votes.df[indian ==TRUE, ethn1:="indian"]
demog.votes.df[, ethn1:=as.factor(ethn1)]
demog.votes.df[, gender1:=as.factor(gender)]
demog.votes.df[, t.test(user.wc ~ gender1), by=ethn1]
demog.votes.df[, t.test(avg.vote.nozero ~ gender1), by=ethn1]
demog.votes.df[gender=="female", t.test(avg.vote.nozero ~ ethn1)]

model1 <- avg.vote.nozero ~ user.wc
model2 <- avg.vote.nozero ~ user.wc + factor(age.bin)
model3 <- avg.vote.nozero ~ user.wc*factor(age.bin)

m1.AF<- lm(model1, demog.df.AF)
m1.WF<- lm(model1, demog.df.WF)
m1.BF<- lm(model1, demog.df.BF)
m1.IF<- lm(model1, demog.df.IF)
m1.NF<- lm(model1, demog.df.NF)
m1.LF<- lm(model1, demog.df.LF)
m1.AM<- lm(model1, demog.df.AM)
m1.WM<- lm(model1, demog.df.WM)
m1.BM<- lm(model1, demog.df.BM)
m1.IM<- lm(model1, demog.df.IM)
m1.NM<- lm(model1, demog.df.NM)
m1.LM<- lm(model1, demog.df.LM)

m2.AF<- lm(model2, demog.df.AF)
m2.WF<- lm(model2, demog.df.WF)
m2.BF<- lm(model2, demog.df.BF)
m2.IF<- lm(model2, demog.df.IF)
m2.NF<- lm(model2, demog.df.NF)
m2.LF<- lm(model2, demog.df.LF)
m2.AM<- lm(model2, demog.df.AM)
m2.WM<- lm(model2, demog.df.WM)
m2.BM<- lm(model2, demog.df.BM)
m2.IM<- lm(model2, demog.df.IM)
m2.NM<- lm(model2, demog.df.NM)
m2.LM<- lm(model2, demog.df.LM)

m3.AF<- lm(model3, demog.df.AF)
m3.WF<- lm(model3, demog.df.WF)
m3.BF<- lm(model3, demog.df.BF)
m3.IF<- lm(model3, demog.df.IF)
m3.NF<- lm(model3, demog.df.NF)
m3.LF<- lm(model3, demog.df.LF)
m3.AM<- lm(model3, demog.df.AM)
m3.WM<- lm(model3, demog.df.WM)
m3.BM<- lm(model3, demog.df.BM)
m3.IM<- lm(model3, demog.df.IM)
m3.NM<- lm(model3, demog.df.NM)
m3.LM<- lm(model3, demog.df.LM)

summary(m1.AF)
summary(m1.WF)
summary(m1.BF)
summary(m1.IF)
summary(m1.NF)
summary(m1.LF)
summary(m1.AM)
summary(m1.WM)
summary(m1.BM)
summary(m1.IM)
summary(m1.NM)
summary(m1.LM)

summary(m2.AF)
summary(m2.WF)
summary(m2.BF)
summary(m2.IF)
summary(m2.NF)
summary(m2.LF)
summary(m2.AM)
summary(m2.WM)
summary(m2.BM)
summary(m2.IM)
summary(m2.NM)
summary(m2.LM)

summary(m3.AF)
summary(m3.WF)
summary(m3.BF)
summary(m3.IF)
summary(m3.NF)
summary(m3.LF)
summary(m3.AM)
summary(m3.WM)
summary(m3.BM)
summary(m3.IM)
summary(m3.NM)
summary(m3.LM)
