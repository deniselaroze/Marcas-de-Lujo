
library(plyr)
library(dplyr)
library(stargazer)
library(nnet)
library(ggplot2)
#library(stringi)
theme_set(theme_bw())
#library(clusterSEs)
library(xtable)
library(psych)

library(rms)
library(FindIt)
library(data.table)


#setwd("C:/Users/Profesor/Dropbox/CESS-Santiago/archive/E Marinao/Data analysis")

rm(list = ls())


Fig.path<-"Figures"

#source("conjoint_recode.R")
load("Data/df_marcas_lujo_anonymous.Rdata")
df<-df[!is.na(df$Qtrustpre_1),]


##################################
### Recode
##################################

df$lottery_select<-df$Q187

df$treat<-ifelse(!is.na(df$QTnegcomp1), "Negative", ifelse(!is.na(df$QTbalcomp1), "Balanced", "Control"))
df$treat<- factor(df$treat,ordered=FALSE,levels=c("Control", "Negative", "Balanced"))



df$QTnegcomp1_correct<-ifelse(df$QTnegcomp1=="Ralph Lauren", 1, 0)
df$QTnegcomp2_correct<-ifelse(df$QTnegcomp2=="Argentina", 1, 0)
df$QTnegcomp3_correct<-ifelse(df$QTnegcomp3=="2010", 1, 0)

df$QTbalcomp1_correct<-ifelse(df$QTbalcomp1=="Ralph Lauren", 1, 0)
df$QTbalcomp2_correct<-ifelse(df$QTbalcomp2=="Argentina", 1, 0)
df$QTbalcomp3_correct<-ifelse(df$QTbalcomp3=="2010", 1, 0)

df$QTcontrolcomp1_correct<-ifelse(df$QTcontrolcomp1=="Comisión Europea", 1, 0)
df$QTcontrolcomp2_correct<-ifelse(df$QTcontrolcomp2=="Panamá", 1, 0)
df$QTcontrolcomp3_correct<-ifelse(df$QTcontrolcomp3=="Febrero", 1, 0)


vars<-c("QTnegcomp1_correct", "QTnegcomp2_correct", "QTnegcomp3_correct",
        "QTbalcomp1_correct", "QTbalcomp2_correct", "QTbalcomp3_correct",  
        "QTcontrolcomp1_correct", "QTcontrolcomp2_correct","QTcontrolcomp3_correct"
        )
reading<-df[,vars]

reading$sum<-rowSums(reading, na.rm=T)

df$reading_correct<-reading$sum
rm(reading)


educ<-df$Qeduc
levels<-c("Básica completa", "Educación media completa (rindiendo PSU o PAA)", "Media completa", "Media incompleta" )

df$educ_sup<-ifelse(educ %in% levels, 0, 1) 
df$educ_sup[df$Qeduc==""]<-NA


table(df$Qeduc, df$educ_sup)

df$age<-2019-df$Qbirthyear

### Recode gender, eliminate other category, as it only has 1 obs
table(df$Qgender)
df$gender<-df$Qgender
df$gender[df$gender==""]<-NA
df$gender[df$gender=="Otro"]<-NA


### Recode mis named variable

rep.vars<-paste0("Qreppost_", 1:11)
rep.vars.new<-paste0("Qreppre_", 1:11)
rep.vars.error<-paste0("Qreppost_", 1:11,".1")

setnames(df, old = rep.vars, new = rep.vars.new)
setnames(df, old = rep.vars.error, new = rep.vars)
names(df)


trust.pre<-paste0("Qtrustpre_", 1:11)
fam.pre<-paste0("Qfampre_", 1:11)
honest.pre<-paste0("Qhonestpre_", 1:11)
rep.pre<-  paste0("Qreppre_", 1:11)

trust.post<-paste0("Qtrustpost_", 1:11)
fam.post<-paste0("Qfampost_", 1:11)
honest.post<-paste0("Qhonestpost_", 1:11)
rep.post<-  paste0("Qreppost_", 1:11)



names.likert<-c(trust.pre, trust.post, fam.pre, fam.post, honest.pre, honest.post)
names.rep<-c(rep.pre, rep.post)

# Changing from text to number. Recoded values: 1 good, 2 OK, 3, bad
df.likert<-df[, names.likert]
df.likert.n<-df.likert
df.likert.n[] <- lapply(df.likert, as.numeric)
df.likert.n[df.likert.n==3]<-1
df.likert.n[df.likert.n==4]<-3

# Changing from text to number. Recoded values: 1 good, 2 OK, 3, bad
df.rep<-df[, names.rep]
df.rep.n<-df.rep
df.rep.n[] <- lapply(df.rep, as.numeric)
df.rep.n[df.rep.n==2]<-1
df.rep.n[df.rep.n==4]<-2

#merging into one df and cleaning

df.likert<-cbind(df.likert.n, df.rep.n)
rm(df.likert.n, df.rep, df.rep.n)

###differentces for Ralph Lauren
df.likert$LR.trust<-df.likert$Qtrustpre_1-df.likert$Qtrustpost_1
df.likert$LR.fam<-df.likert$Qfampre_1-df.likert$Qfampost_1
df.likert$LR.honest<-df.likert$Qhonestpre_1-df.likert$Qhonestpost_1
df.likert$LR.rep<-df.likert$Qreppre_1-df.likert$Qreppost_1


df<-cbind(df, df.likert)


##################
###Descriptives
##################

describe(df$lottery_select)

table(df.likert$LR.trust)
table(df.likert$LR.fam)
table(df.likert$LR.honest)
table(df.likert$LR.rep)


###########################
### Balace tests Vignette
###########################

bal_vignette <- multinom(treat ~ gender + age + Qwork + Qrelationshipstatus, data = df)
summary(bal_vignette)

stargazer(bal_vignette, type="html", out="Tables/balance_vignette.html")




############################################
### Data analysis on likert vars
############################################




model1<-lm(LR.trust ~ treat, df)
model2<-lm(LR.fam ~ treat, df)
model3<-lm(LR.honest ~ treat, df)
model4<-lm(LR.rep ~ treat, df)

stargazer(model1, model2, model3, model4, type="html", out="Tables/lm_likert.html")



vars<-c("T: Negative", "T. Balanced", "Constant")



stargazer(model1, model2, model3, model4, out="Tables/lm_likert.tex",
          keep.stat="n",  covariate.labels = vars, dep.var.labels.include = T, dep.var.labels = c("Trust", "Familiarity", "Honesty", "Reputation" ),
          dep.var.caption = "", star.cutoffs = c(0.1, 0.05, 0.01, 0.001), star.char = c("\\dagger", "*", "**", "***"),
          notes        = c("$\\dagger<0.1$; *$p<0.05$; **$p<0.01$; ***$p<0.001$", "More negative values imply lower DV evaluations."), notes.append = FALSE, notes.align = "l",
          title = "LM estimations on differences in likert valuations", no.space=TRUE, table.placement = "H")



############################################
### Data analysis on Lottery - Null results
############################################

model1<-glm(lottery_select ~ treat + reading_correct + Qrisk + age + Qwork + Qrelationshipstatus, df, family = binomial("logit"))
summary(model1)


