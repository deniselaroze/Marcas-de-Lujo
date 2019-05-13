
library(plyr)
library(dplyr)




setwd("C:/Users/Denise Laroze/Dropbox/CESS-Santiago/archive/E Marinao/Data analysis")






#################################################################################
### Clean data frame // non-anonyous data / included only for transparency / 
### Data not provided in replication material due to compliance with data
### protection legislation
##################################################################################
# df<-read.csv("Marcas_de_lujo_data.csv")
# 
# df[df==""]  <- NA 
# 
# df<-df[!is.na(df$Q74),]
# #Clean rut to only numeric digits
# df$rut<-gsub("[^0-9]", "", df$Q104) ## alternatively a good option is x<-gsub('-|\\.| ','',df$Q104)
# 
# #Eliminate responses from duplicated ruts (national id numbers), keep only first response
# df$rut_dupl<-duplicated(df$rut)
# df<-df[df$rut_dupl=="FALSE", ]
# 
# ### Check if there are any other duplications
# df$RecipientEmail_dupl<-duplicated(df$RecipientEmail)
# table(df$RecipientEmail_dupl)
# #View(subset(df, RecipientEmail_dupl==T ))
# #View(subset(df, RecipientEmail=="carlos.acosta@vtr.net"))  ### Looks as if it is husband and wife, not the same individual, so not eliminated
# 
# ### Anonymize data frame
# 
# df<-select(df,-c("IPAddress", "RecipientLastName", "RecipientFirstName", "RecipientEmail", "rut", "rut_dupl", "Q104", "RecipientEmail_dupl"))
# 
# save(df, file = "df_marcas_lujo_anonymous.RData")
# 


##########################################
### Working with anonymous data
##########################################
rm(list = ls())

load("Data/df_marcas_lujo_anonymous.RData")


colnames(df)[colnames(df) == 'X1_Qcsatisfy'] <- 'X1_Qc3satisfy'
colnames(df)[colnames(df) == 'X2_Qcsatisfy'] <- 'X2_Qc3satisfy'
colnames(df)[colnames(df) == 'X3_Qcsatisfy'] <- 'X3_Qc3satisfy'


master<-df

master<- master[!is.na(master$X1_Qc1trust),]

################################################
#### Save sample data - early replic repository
################################################
set.seed(3658)

df.sample<-sample_n(df, size=50, replace = FALSE)

save(df.sample, file= "Data/df_marcas_lujo_anonymous_sample.Rdata")


###########################################
### reshape data
###########################################

vars<-c("X1_Qc1select",	"X1_Qc1trust",	"X1_Qc1satisfy", "X2_Qc1select",	"X2_Qc1trust",	"X2_Qc1satisfy", "X3_Qc1select",	"X3_Qc1trust",	"X3_Qc1satisfy",
        "X1_Qc2select",	"X1_Qc2trust",	"X1_Qc2satisfy", "X2_Qc2select",	"X2_Qc2trust",	"X2_Qc2satisfy", "X3_Qc2select",	"X3_Qc2trust",	"X3_Qc2satisfy",
        "X1_Qc3select",	"X1_Qc3trust",	"X1_Qc3satisfy", "X2_Qc3select",	"X2_Qc3trust",	"X2_Qc3satisfy", "X3_Qc3select",	"X3_Qc3trust",	"X3_Qc3satisfy"
)


dfl<-reshape(df, direction='long', 
             varying=vars, 
             timevar='variant',
             times=c('X1', 'X2', 'X3'),
             v.names=c('Qc1select', 'Qc1trust',  'Qc1satisfy','Qc2select', 'Qc2trust',  'Qc2satisfy','Qc3select', 'Qc3trust',  'Qc3satisfy'),
             idvar='ResponseId')

dfl <- dfl[order(dfl$ResponseId, dfl$variant), ]

save(dfl, file= "Data/dfl_marcas_lujo_anonymous.Rdata")

#################################
#### Conjoint 1 data structuring
#################################

##CJ1a
df <- data.frame(Attribute=as.character(),Level=as.character(),Candidate=as.character())

for (attribute in c("Conciencia Social"   , "Confiabilidad"   ,"Familiaridad" , "Fuerza de Ventas" )) {
  level <- factor(ifelse(master$F.1.1 == attribute,as.character(master$F.1.1.1),
                         ifelse(master$F.1.2 == attribute,as.character(master$F.1.1.2),
                                ifelse(master$F.1.3 == attribute,as.character(master$F.1.1.3), as.character(master$F.1.1.4)))))
  df <- rbind(df,data.frame(Attribute = attribute, Level = level, Candidate = 1))
}

for (attribute in c("Conciencia Social"   , "Confiabilidad"   ,"Familiaridad" , "Fuerza de Ventas")) {
  level <- factor(ifelse(master$F.1.1 == attribute,as.character(master$F.1.2.1),
                         ifelse(master$F.1.2 == attribute,as.character(master$F.1.2.2),
                                ifelse(master$F.1.3 == attribute,as.character(master$F.1.2.3),as.character(master$F.1.2.4)))))
  df <- rbind(df,data.frame(Attribute = attribute, Level = level, Candidate = 2))
}

conciencia <- df$Level[df$Attribute == "Conciencia Social"]
confia <- df$Level[df$Attribute == "Confiabilidad"]
familiar <- df$Level[df$Attribute == "Familiaridad"]
ventas <- df$Level[df$Attribute == "Fuerza de Ventas"]
marca <- df$Candidate[df$Attribute == "Fuerza de Ventas"]
marca_select <- ifelse((master$X1_Qc1select == "Marca de lujo 1" & marca == 1) | (master$X1_Qc1select == "Marca de lujo 2" & marca == 2),1,0)
marca_satisfy <- ifelse((master$X1_Qc1satisfy == "Marca de lujo 1" & marca == 1) | (master$X1_Qc1satisfy == "Marca de lujo 2" & marca == 2),1,0)
marca_trust <- ifelse((master$X1_Qc1trust == "Marca de lujo 1" & marca == 1) | (master$X1_Qc1trust == "Marca de lujo 2" & marca == 2),1,0)





gender<-master$Qgender
age<-2019-master$Qbirthyear
educ<-master$Qeduc
work<-master$Qwork
relation<-master$Qrelationshipstatus
id<-master$ResponseId

conjoint1a <- data.frame(id, conciencia, confia, familiar, ventas, marca, marca_select, marca_satisfy, marca_trust, gender, age, educ, work, relation)



##CJ1b
df <- data.frame(Attribute=as.character(),Level=as.character(),Candidate=as.character())

for (attribute in c("Conciencia Social"   , "Confiabilidad"   ,"Familiaridad" , "Fuerza de Ventas" )) {
  level <- factor(ifelse(master$F.2.1 == attribute,as.character(master$F.2.1.1),
                         ifelse(master$F.2.2 == attribute,as.character(master$F.2.1.2),
                                ifelse(master$F.2.3 == attribute,as.character(master$F.2.1.3), as.character(master$F.2.1.4)))))
  df <- rbind(df,data.frame(Attribute = attribute, Level = level, Candidate = 1))
}

for (attribute in c("Conciencia Social"   , "Confiabilidad"   ,"Familiaridad" , "Fuerza de Ventas")) {
  level <- factor(ifelse(master$F.2.1 == attribute,as.character(master$F.2.2.1),
                         ifelse(master$F.2.2 == attribute,as.character(master$F.2.2.2),
                                ifelse(master$F.2.3 == attribute,as.character(master$F.2.2.3),as.character(master$F.2.2.4)))))
  df <- rbind(df,data.frame(Attribute = attribute, Level = level, Candidate = 2))
}

conciencia <- df$Level[df$Attribute == "Conciencia Social"]
confia <- df$Level[df$Attribute == "Confiabilidad"]
familiar <- df$Level[df$Attribute == "Familiaridad"]
ventas <- df$Level[df$Attribute == "Fuerza de Ventas"]
marca <- df$Candidate[df$Attribute == "Fuerza de Ventas"]
marca_select <- ifelse((master$X1_Qc1select == "Marca de lujo 1" & marca == 1) | (master$X1_Qc1select == "Marca de lujo 2" & marca == 2),1,0)
marca_satisfy <- ifelse((master$X1_Qc1satisfy == "Marca de lujo 1" & marca == 1) | (master$X1_Qc1satisfy == "Marca de lujo 2" & marca == 2),1,0)
marca_trust <- ifelse((master$X1_Qc1trust == "Marca de lujo 1" & marca == 1) | (master$X1_Qc1trust == "Marca de lujo 2" & marca == 2),1,0)



gender<-master$Qgender
age<-2019-master$Qbirthyear
educ<-master$Qeduc
work<-master$Qwork
relation<-master$Qrelationshipstatus
id<-master$ResponseId

conjoint1b <- data.frame(id, conciencia, confia, familiar, ventas, marca, marca_select, marca_satisfy, marca_trust, gender, age, educ, work, relation)


##CJ1c

df <- data.frame(Attribute=as.character(),Level=as.character(),Candidate=as.character())

for (attribute in c("Conciencia Social"   , "Confiabilidad"   ,"Familiaridad" , "Fuerza de Ventas" )) {
  level <- factor(ifelse(master$F.3.1 == attribute,as.character(master$F.3.1.1),
                         ifelse(master$F.3.2 == attribute,as.character(master$F.3.1.2),
                                ifelse(master$F.3.3 == attribute,as.character(master$F.3.1.3), as.character(master$F.3.1.4)))))
  df <- rbind(df,data.frame(Attribute = attribute, Level = level, Candidate = 1))
}

for (attribute in c("Conciencia Social"   , "Confiabilidad"   ,"Familiaridad" , "Fuerza de Ventas")) {
  level <- factor(ifelse(master$F.3.1 == attribute,as.character(master$F.3.2.1),
                         ifelse(master$F.3.2 == attribute,as.character(master$F.3.2.2),
                                ifelse(master$F.3.3 == attribute,as.character(master$F.3.2.3),as.character(master$F.3.2.4)))))
  df <- rbind(df,data.frame(Attribute = attribute, Level = level, Candidate = 2))
}

conciencia <- df$Level[df$Attribute == "Conciencia Social"]
confia <- df$Level[df$Attribute == "Confiabilidad"]
familiar <- df$Level[df$Attribute == "Familiaridad"]
ventas <- df$Level[df$Attribute == "Fuerza de Ventas"]
marca <- df$Candidate[df$Attribute == "Fuerza de Ventas"]
marca_select <- ifelse((master$X1_Qc1select == "Marca de lujo 1" & marca == 1) | (master$X1_Qc1select == "Marca de lujo 2" & marca == 2),1,0)
marca_satisfy <- ifelse((master$X1_Qc1satisfy == "Marca de lujo 1" & marca == 1) | (master$X1_Qc1satisfy == "Marca de lujo 2" & marca == 2),1,0)
marca_trust <- ifelse((master$X1_Qc1trust == "Marca de lujo 1" & marca == 1) | (master$X1_Qc1trust == "Marca de lujo 2" & marca == 2),1,0)


gender<-master$Qgender
age<-2019-master$Qbirthyear
educ<-master$Qeduc
work<-master$Qwork
relation<-master$Qrelationshipstatus
id<-master$ResponseId

conjoint1c <- data.frame(id, conciencia, confia, familiar, ventas, marca, marca_select, marca_satisfy, marca_trust, gender, age, educ, work, relation)


#### Gen one DF
conjoint1 <- rbind(conjoint1a,conjoint1b,conjoint1c)


rm(conjoint1a, conjoint1b, conjoint1c)

save(conjoint1, file ="Data/conjoint1.Rdata")


################################
#### Conjoint 2 data structuring
################################

##CJ2a
df <- data.frame(Attribute=as.character(),Level=as.character(),Candidate=as.character())

for (attribute in c("Conciencia Social"   , "Confiabilidad"   ,"Familiaridad" , "Fuerza de Ventas" )) {
  level <- factor(ifelse(master$G.1.1 == attribute,as.character(master$G.1.1.1),
                         ifelse(master$G.1.2 == attribute,as.character(master$G.1.1.2),
                                ifelse(master$G.1.3 == attribute,as.character(master$G.1.1.3), as.character(master$G.1.1.4)))))
  df <- rbind(df,data.frame(Attribute = attribute, Level = level, Candidate = 1))
}

for (attribute in c("Conciencia Social"   , "Confiabilidad"   ,"Familiaridad" , "Fuerza de Ventas")) {
  level <- factor(ifelse(master$G.1.1 == attribute,as.character(master$G.1.2.1),
                         ifelse(master$G.1.2 == attribute,as.character(master$G.1.2.2),
                                ifelse(master$G.1.3 == attribute,as.character(master$G.1.2.3),as.character(master$G.1.2.4)))))
  df <- rbind(df,data.frame(Attribute = attribute, Level = level, Candidate = 2))
}

conciencia <- df$Level[df$Attribute == "Conciencia Social"]
confia <- df$Level[df$Attribute == "Confiabilidad"]
familiar <- df$Level[df$Attribute == "Familiaridad"]
ventas <- df$Level[df$Attribute == "Fuerza de Ventas"]
marca <- df$Candidate[df$Attribute == "Fuerza de Ventas"]
marca_select <- ifelse((master$X1_Qc2select == "Marca de lujo 1" & marca == 1) | (master$X1_Qc2select == "Marca de lujo 2" & marca == 2),1,0)
marca_satisfy <- ifelse((master$X1_Qc2satisfy == "Marca de lujo 1" & marca == 1) | (master$X1_Qc2satisfy == "Marca de lujo 2" & marca == 2),1,0)
marca_trust <- ifelse((master$X1_Qc2trust == "Marca de lujo 1" & marca == 1) | (master$X1_Qc2trust == "Marca de lujo 2" & marca == 2),1,0)

gender<-master$Qgender
age<-2019-master$Qbirthyear
educ<-master$Qeduc
work<-master$Qwork
relation<-master$Qrelationshipstatus
id<-master$ResponseId

conjoint2a <- data.frame(id, conciencia, confia, familiar, ventas, marca, marca_select, marca_satisfy, marca_trust, gender, age, educ, work, relation)


##CJ2b
df <- data.frame(Attribute=as.character(),Level=as.character(),Candidate=as.character())

for (attribute in c("Conciencia Social"   , "Confiabilidad"   ,"Familiaridad" , "Fuerza de Ventas" )) {
  level <- factor(ifelse(master$G.2.1 == attribute,as.character(master$G.2.1.1),
                         ifelse(master$G.2.2 == attribute,as.character(master$G.2.1.2),
                                ifelse(master$G.2.3 == attribute,as.character(master$G.2.1.3), as.character(master$G.2.1.4)))))
  df <- rbind(df,data.frame(Attribute = attribute, Level = level, Candidate = 1))
}

for (attribute in c("Conciencia Social"   , "Confiabilidad"   ,"Familiaridad" , "Fuerza de Ventas")) {
  level <- factor(ifelse(master$G.2.1 == attribute,as.character(master$G.2.2.1),
                         ifelse(master$G.2.2 == attribute,as.character(master$G.2.2.2),
                                ifelse(master$G.2.3 == attribute,as.character(master$G.2.2.3),as.character(master$G.2.2.4)))))
  df <- rbind(df,data.frame(Attribute = attribute, Level = level, Candidate = 2))
}

conciencia <- df$Level[df$Attribute == "Conciencia Social"]
confia <- df$Level[df$Attribute == "Confiabilidad"]
familiar <- df$Level[df$Attribute == "Familiaridad"]
ventas <- df$Level[df$Attribute == "Fuerza de Ventas"]
marca <- df$Candidate[df$Attribute == "Fuerza de Ventas"]
marca_select <- ifelse((master$X1_Qc2select == "Marca de lujo 1" & marca == 1) | (master$X1_Qc2select == "Marca de lujo 2" & marca == 2),1,0)
marca_satisfy <- ifelse((master$X1_Qc2satisfy == "Marca de lujo 1" & marca == 1) | (master$X1_Qc2satisfy == "Marca de lujo 2" & marca == 2),1,0)
marca_trust <- ifelse((master$X1_Qc2trust == "Marca de lujo 1" & marca == 1) | (master$X1_Qc2trust == "Marca de lujo 2" & marca == 2),1,0)


gender<-master$Qgender
age<-2019-master$Qbirthyear
educ<-master$Qeduc
work<-master$Qwork
relation<-master$Qrelationshipstatus
id<-master$ResponseId

conjoint2b <- data.frame(id, conciencia, confia, familiar, ventas, marca, marca_select, marca_satisfy, marca_trust, gender, age, educ, work, relation)


##CJ2c

df <- data.frame(Attribute=as.character(),Level=as.character(),Candidate=as.character())

for (attribute in c("Conciencia Social"   , "Confiabilidad"   ,"Familiaridad" , "Fuerza de Ventas" )) {
  level <- factor(ifelse(master$G.3.1 == attribute,as.character(master$G.3.1.1),
                         ifelse(master$G.3.2 == attribute,as.character(master$G.3.1.2),
                                ifelse(master$G.3.3 == attribute,as.character(master$G.3.1.3), as.character(master$G.3.1.4)))))
  df <- rbind(df,data.frame(Attribute = attribute, Level = level, Candidate = 1))
}

for (attribute in c("Conciencia Social"   , "Confiabilidad"   ,"Familiaridad" , "Fuerza de Ventas")) {
  level <- factor(ifelse(master$G.3.1 == attribute,as.character(master$G.3.2.1),
                         ifelse(master$G.3.2 == attribute,as.character(master$G.3.2.2),
                                ifelse(master$G.3.3 == attribute,as.character(master$G.3.2.3),as.character(master$G.3.2.4)))))
  df <- rbind(df,data.frame(Attribute = attribute, Level = level, Candidate = 2))
}

conciencia <- df$Level[df$Attribute == "Conciencia Social"]
confia <- df$Level[df$Attribute == "Confiabilidad"]
familiar <- df$Level[df$Attribute == "Familiaridad"]
ventas <- df$Level[df$Attribute == "Fuerza de Ventas"]
marca <- df$Candidate[df$Attribute == "Fuerza de Ventas"]
marca_select <- ifelse((master$X1_Qc2select == "Marca de lujo 1" & marca == 1) | (master$X1_Qc2select == "Marca de lujo 2" & marca == 2),1,0)
marca_satisfy <- ifelse((master$X1_Qc2satisfy == "Marca de lujo 1" & marca == 1) | (master$X1_Qc2satisfy == "Marca de lujo 2" & marca == 2),1,0)
marca_trust <- ifelse((master$X1_Qc2trust == "Marca de lujo 1" & marca == 1) | (master$X1_Qc2trust == "Marca de lujo 2" & marca == 2),1,0)


gender<-master$Qgender
age<-2019-master$Qbirthyear
educ<-master$Qeduc
work<-master$Qwork
relation<-master$Qrelationshipstatus
id<-master$ResponseId

conjoint2c <- data.frame(id, conciencia, confia, familiar, ventas, marca, marca_select, marca_satisfy, marca_trust, gender, age, educ, work, relation)
#### Gen one DF
conjoint2 <- rbind(conjoint2a,conjoint2b,conjoint2c)


rm(conjoint2a, conjoint2b, conjoint2c)

save(conjoint2, file ="Data/conjoint2.Rdata")


################################
#### Conjoint 3 data structuring
################################

##CJ3a
df <- data.frame(Attribute=as.character(),Level=as.character(),Candidate=as.character())

for (attribute in c("Conciencia Social"   , "Confiabilidad"   ,"Familiaridad" , "Fuerza de Ventas" )) {
  level <- factor(ifelse(master$H.1.1 == attribute,as.character(master$H.1.1.1),
                         ifelse(master$H.1.2 == attribute,as.character(master$H.1.1.2),
                                ifelse(master$H.1.3 == attribute,as.character(master$H.1.1.3), as.character(master$H.1.1.4)))))
  df <- rbind(df,data.frame(Attribute = attribute, Level = level, Candidate = 1))
}

for (attribute in c("Conciencia Social"   , "Confiabilidad"   ,"Familiaridad" , "Fuerza de Ventas")) {
  level <- factor(ifelse(master$H.1.1 == attribute,as.character(master$H.1.2.1),
                         ifelse(master$H.1.2 == attribute,as.character(master$H.1.2.2),
                                ifelse(master$H.1.3 == attribute,as.character(master$H.1.2.3),as.character(master$H.1.2.4)))))
  df <- rbind(df,data.frame(Attribute = attribute, Level = level, Candidate = 2))
}

conciencia <- df$Level[df$Attribute == "Conciencia Social"]
confia <- df$Level[df$Attribute == "Confiabilidad"]
familiar <- df$Level[df$Attribute == "Familiaridad"]
ventas <- df$Level[df$Attribute == "Fuerza de Ventas"]
marca <- df$Candidate[df$Attribute == "Fuerza de Ventas"]
marca_select <- ifelse((master$X1_Qc3select == "Marca de lujo 1" & marca == 1) | (master$X1_Qc3select == "Marca de lujo 2" & marca == 2),1,0)
marca_satisfy <- ifelse((master$X1_Qc3satisfy == "Marca de lujo 1" & marca == 1) | (master$X1_Qc3satisfy == "Marca de lujo 2" & marca == 2),1,0)
marca_trust <- ifelse((master$X1_Qc3trust == "Marca de lujo 1" & marca == 1) | (master$X1_Qc3trust == "Marca de lujo 2" & marca == 2),1,0)

gender<-master$Qgender
age<-2019-master$Qbirthyear
educ<-master$Qeduc
work<-master$Qwork
relation<-master$Qrelationshipstatus
id<-master$ResponseId

conjoint3a <- data.frame(id, conciencia, confia, familiar, ventas, marca, marca_select, marca_satisfy, marca_trust, gender, age, educ, work, relation)


##CJ3b
df <- data.frame(Attribute=as.character(),Level=as.character(),Candidate=as.character())

for (attribute in c("Conciencia Social"   , "Confiabilidad"   ,"Familiaridad" , "Fuerza de Ventas" )) {
  level <- factor(ifelse(master$H.2.1 == attribute,as.character(master$H.2.1.1),
                         ifelse(master$H.2.2 == attribute,as.character(master$H.2.1.2),
                                ifelse(master$H.2.3 == attribute,as.character(master$H.2.1.3), as.character(master$H.2.1.4)))))
  df <- rbind(df,data.frame(Attribute = attribute, Level = level, Candidate = 1))
}

for (attribute in c("Conciencia Social"   , "Confiabilidad"   ,"Familiaridad" , "Fuerza de Ventas")) {
  level <- factor(ifelse(master$H.2.1 == attribute,as.character(master$H.2.2.1),
                         ifelse(master$H.2.2 == attribute,as.character(master$H.2.2.2),
                                ifelse(master$H.2.3 == attribute,as.character(master$H.2.2.3),as.character(master$H.2.2.4)))))
  df <- rbind(df,data.frame(Attribute = attribute, Level = level, Candidate = 2))
}

conciencia <- df$Level[df$Attribute == "Conciencia Social"]
confia <- df$Level[df$Attribute == "Confiabilidad"]
familiar <- df$Level[df$Attribute == "Familiaridad"]
ventas <- df$Level[df$Attribute == "Fuerza de Ventas"]
marca <- df$Candidate[df$Attribute == "Fuerza de Ventas"]
marca_select <- ifelse((master$X1_Qc3select == "Marca de lujo 1" & marca == 1) | (master$X1_Qc3select == "Marca de lujo 2" & marca == 2),1,0)
marca_satisfy <- ifelse((master$X1_Qc3satisfy == "Marca de lujo 1" & marca == 1) | (master$X1_Qc3satisfy == "Marca de lujo 2" & marca == 2),1,0)
marca_trust <- ifelse((master$X1_Qc3trust == "Marca de lujo 1" & marca == 1) | (master$X1_Qc3trust == "Marca de lujo 2" & marca == 2),1,0)


gender<-master$Qgender
age<-2019-master$Qbirthyear
educ<-master$Qeduc
work<-master$Qwork
relation<-master$Qrelationshipstatus
id<-master$ResponseId

conjoint3b <- data.frame(id, conciencia, confia, familiar, ventas, marca, marca_select, marca_satisfy, marca_trust, gender, age, educ, work, relation)

##CJ3c

df <- data.frame(Attribute=as.character(),Level=as.character(),Candidate=as.character())

for (attribute in c("Conciencia Social"   , "Confiabilidad"   ,"Familiaridad" , "Fuerza de Ventas" )) {
  level <- factor(ifelse(master$H.3.1 == attribute,as.character(master$H.3.1.1),
                         ifelse(master$H.3.2 == attribute,as.character(master$H.3.1.2),
                                ifelse(master$H.3.3 == attribute,as.character(master$H.3.1.3), as.character(master$H.3.1.4)))))
  df <- rbind(df,data.frame(Attribute = attribute, Level = level, Candidate = 1))
}

for (attribute in c("Conciencia Social"   , "Confiabilidad"   ,"Familiaridad" , "Fuerza de Ventas")) {
  level <- factor(ifelse(master$H.3.1 == attribute,as.character(master$H.3.2.1),
                         ifelse(master$H.3.2 == attribute,as.character(master$H.3.2.2),
                                ifelse(master$H.3.3 == attribute,as.character(master$H.3.2.3),as.character(master$H.3.2.4)))))
  df <- rbind(df,data.frame(Attribute = attribute, Level = level, Candidate = 2))
}

conciencia <- df$Level[df$Attribute == "Conciencia Social"]
confia <- df$Level[df$Attribute == "Confiabilidad"]
familiar <- df$Level[df$Attribute == "Familiaridad"]
ventas <- df$Level[df$Attribute == "Fuerza de Ventas"]
marca <- df$Candidate[df$Attribute == "Fuerza de Ventas"]
marca_select <- ifelse((master$X1_Qc3select == "Marca de lujo 1" & marca == 1) | (master$X1_Qc3select == "Marca de lujo 2" & marca == 2),1,0)
marca_satisfy <- ifelse((master$X1_Qc3satisfy == "Marca de lujo 1" & marca == 1) | (master$X1_Qc3satisfy == "Marca de lujo 2" & marca == 2),1,0)
marca_trust <- ifelse((master$X1_Qc3trust == "Marca de lujo 1" & marca == 1) | (master$X1_Qc3trust == "Marca de lujo 2" & marca == 2),1,0)



gender<-master$Qgender
age<-2019-master$Qbirthyear
educ<-master$Qeduc
work<-master$Qwork
relation<-master$Qrelationshipstatus
id<-master$ResponseId

conjoint3c <- data.frame(id, conciencia, confia, familiar, ventas, marca, marca_select, marca_satisfy, marca_trust, gender, age, educ, work, relation)

#### Gen one DF
conjoint3 <- rbind(conjoint3a,conjoint3b,conjoint3c)


rm(conjoint3a, conjoint3b, conjoint3c)

save(conjoint3, file ="Data/conjoint3.Rdata")

