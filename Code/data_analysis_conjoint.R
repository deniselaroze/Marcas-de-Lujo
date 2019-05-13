
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
#library(data.table)


#setwd("C:/Users/Profesor/Dropbox/CESS-Santiago/archive/E Marinao/Data analysis")

rm(list = ls())


Fig.path<-"Figures"

#source("conjoint_recode.R")

load("Data/conjoint1.Rdata")
conjoint1 <- droplevels(conjoint1)  
load("Data/conjoint2.Rdata")
conjoint2 <- droplevels(conjoint2)  
load("Data/conjoint3.Rdata")
conjoint3<- droplevels(conjoint3)  
load("Data/dfl_marcas_lujo_anonymous.Rdata")
dfl<-dfl[!is.na(dfl$Qtrustpre_1),]



###################################
### Data prep 
###################################
# Recoding base category

#unique(conjoint1$conciencia)
conjoint1 <- within(conjoint1, conciencia<- relevel(conciencia, ref = "Entrega bolsas de plástico"))

#unique(conjoint1$confia)
conjoint1 <- within(conjoint1, confia <- relevel(confia, ref = "Tiene letra chica"))

#unique(conjoint1$familiar)
conjoint1 <- within(conjoint1, familiar <- relevel(familiar, ref = "No se le conoce publicidad"))

#unique(conjoint1$ventas)
conjoint1 <- within(conjoint1, ventas <- relevel(ventas, ref = "No responde dudas"))


#unique(conjoint2$conciencia)
conjoint2 <- within(conjoint2, conciencia<- relevel(conciencia, ref = "Política de Responsabilidad Social Empresarial desconocida"))

conjoint2 <- within(conjoint2, confia <- relevel(confia, ref = "Tiene letra chica"))
conjoint2 <- within(conjoint2, familiar <- relevel(familiar, ref = "No se le conoce publicidad"))
conjoint2 <- within(conjoint2, ventas <- relevel(ventas, ref = "No responde dudas"))



conjoint3 <- within(conjoint3, conciencia<- relevel(conciencia, ref = "Entrega bolsas de plástico"))
conjoint3 <- within(conjoint3, confia <- relevel(confia, ref = "Tiene letra chica"))
conjoint3 <- within(conjoint3, familiar <- relevel(familiar, ref = "No se le conoce Publicidad"))

#unique(conjoint3$ventas)
conjoint3 <- within(conjoint3, ventas <- relevel(ventas, ref = "No disponible"))


####################################
#### Recode for Figures
####################################

conjoint <- rbind(conjoint1,conjoint2,conjoint3)
#stargazer(conjoint)

conjoint[conjoint==""]  <- NA 


educ<-conjoint$educ
levels<-c("Básica completa", "Educación media completa (rindiendo PSU o PAA)", "Media completa", "Media incompleta" )

conjoint$educ_sup<-ifelse(educ %in% levels, 0, 1) 
conjoint$educ_sup[conjoint$educ==""]<-NA


table(conjoint$educ, conjoint$educ_sup)

df <- data.frame(age = conjoint$age[1:300],gender = as.factor(as.character(conjoint$gender[1:300])), relation = conjoint$relation[1:300], 
                 educ_sup=as.factor(as.character(conjoint$educ_sup[1:300])), work= conjoint$work[1:300])

df$educ_sup_c<-ifelse(df$educ_sup==1, "Con Educ. Superior", "Sin Educ. Superior")




####################
### Descriptives
####################
dfl$Qc1select_n<-as.numeric(dfl$Qc1select)
dfl$Qc1trust_n<-as.numeric(dfl$Qc1trust)
dfl$Qc1satisfy_n<-as.numeric(dfl$Qc1satisfy)

vars.corr<-c("Qc1select_n"  ,  "Qc1trust_n",  "Qc1satisfy_n" )
corr.test(dfl[vars.corr])  ### These values are extreamly correlated, it is probably indifferent which measure is used as a dependent variable


tbl<-dfl[, c(vars.corr)]


mcor<-round(cor(tbl, use = "complete.obs"),2)
mcor

xtable(mcor)




#############################################
### Main model estimations
##############################################
########### Model estimations brand selection
model1 <- lrm(marca_select ~ conciencia+confia+ familiar + ventas,data=conjoint1,  x=T, y=T)
model2 <- lrm(marca_select ~ conciencia+confia+ familiar + ventas,data=conjoint2,  x=T, y=T)
model3 <- lrm(marca_select ~ conciencia+confia+ familiar + ventas,data=conjoint3,  x=T, y=T)

model1.cl<-bootcov(model1,cluster=conjoint1$id)
model2.cl<-bootcov(model2,cluster=conjoint2$id)
model3.cl<-bootcov(model3,cluster=conjoint3$id)



vars.order<-c(1, 2, 4, 3, 5, 6, 7, 8, 10, 9, 11, 12, 13)
  
vars<-c("Conciencia Social: Entrega bolsas de papel", "No entrega bolsas", 
        "Política de RSE publicitada", "No tiene Política de RSE",
        "Confiabilidad: Es lo que dice ser", "No se describe su composición",
        "Familiaridad: Publicidad Reconocible", "Publicidad vagamente reconocible", 
        "Fuerza de Ventas: Responde dudas adecuadamente","Responde algunas dudas", 
        "Asesora con cordialidad y amablemente", "No asesora. Invade para vender", "Constant")
    
  

stargazer(model1.cl, model2.cl, model3.cl, type="html", out="Tables/brand_select_models_cl.html",
          keep.stat="n",  order = vars.order,covariate.labels = vars, dep.var.labels.include = F,
          dep.var.caption = "", star.cutoffs = c(0.1, 0.05, 0.01, 0.001), star.char = c("&dagger;", "*", "**", "***"),
          notes        = "â p<0.1; *p<0.05; **p<0.01; ***p<0.001", notes.append = FALSE, notes.align = "l",
          title = "GLM estimation on selections preferences")

stargazer(model1.cl, model2.cl, model3.cl, out="Tables/brand_select_models_cl.tex",
          keep.stat="n",  order = vars.order,covariate.labels = vars, dep.var.labels.include = F,
          dep.var.caption = "", star.cutoffs = c(0.1, 0.05, 0.01, 0.001), star.char = c("\\dagger", "*", "**", "***"),
          notes        = "$\\dagger$<0.1; *$p<0.05$; **$p<0.01$; ***$p<0.001$", notes.append = FALSE, notes.align = "l",
          title = "GLM estimation on selections preferences", no.space=TRUE)

########### Model estimations brand trust
model1 <- lrm(marca_trust ~ conciencia+confia+ familiar + ventas,data=conjoint1,  x=T, y=T)
model2 <- lrm(marca_trust ~ conciencia+confia+ familiar + ventas,data=conjoint2,  x=T, y=T)
model3 <- lrm(marca_trust ~ conciencia+confia+ familiar + ventas,data=conjoint3,  x=T, y=T)

model1.cl<-bootcov(model1,cluster=conjoint1$id)
model2.cl<-bootcov(model2,cluster=conjoint2$id)
model3.cl<-bootcov(model3,cluster=conjoint3$id)



stargazer(model1.cl, model2.cl, model3.cl, type="html", out="Tables/brand_trust_models_cl.html",
          keep.stat="n",  order = vars.order,covariate.labels = vars, dep.var.labels.include = F,
          dep.var.caption = "", star.cutoffs = c(0.1, 0.05, 0.01, 0.001), star.char = c("&dagger;", "*", "**", "***"),
          notes        = "â p<0.1; *p<0.05; **p<0.01; ***p<0.001", notes.append = FALSE, notes.align = "l",
          title = "GLM estimation on most trusted brand")


stargazer(model1.cl, model2.cl, model3.cl, out="Tables/brand_trust_models_cl.tex",
          keep.stat="n",  order = vars.order,covariate.labels = vars, dep.var.labels.include = F,
          dep.var.caption = "", star.cutoffs = c(0.1, 0.05, 0.01, 0.001), star.char = c("\\dagger", "*", "**", "***"),
          notes        = "$\\dagger<0.1$; *$p<0.05$; **$p<0.01$; ***$p<0.001$", notes.append = FALSE, notes.align = "l",
          title = "GLM estimation on most trusted brand", no.space=TRUE)




########### Model estimations brand satisfaction
model1 <- lrm(marca_satisfy ~ conciencia+confia+ familiar + ventas,data=conjoint1,  x=T, y=T)
model2 <- lrm(marca_satisfy ~ conciencia+confia+ familiar + ventas,data=conjoint2,  x=T, y=T)
model3 <- lrm(marca_satisfy ~ conciencia+confia+ familiar + ventas,data=conjoint3,  x=T, y=T)

model1.cl<-bootcov(model1,cluster=conjoint1$id)
model2.cl<-bootcov(model2,cluster=conjoint2$id)
model3.cl<-bootcov(model3,cluster=conjoint3$id)


stargazer(model1.cl, model2.cl, model3.cl, type="html", out="Tables/brand_satisfy_models_cl.html",
          keep.stat="n",  order = vars.order,covariate.labels = vars, dep.var.labels.include = F,
          dep.var.caption = "", star.cutoffs = c(0.1, 0.05, 0.01, 0.001), star.char = c("&dagger;", "*", "**", "***"),
          notes        = "â p<0.1; *p<0.05; **p<0.01; ***p<0.001", notes.append = FALSE, notes.align = "l",
          title = "GLM estimation on brand satisfaction")

stargazer(model1.cl, model2.cl, model3.cl, out="Tables/brand_satisfy_models_cl.tex",
          keep.stat="n",  order = vars.order,covariate.labels = vars, dep.var.labels.include = F,
          dep.var.caption = "", star.cutoffs = c(0.1, 0.05, 0.01, 0.001), star.char = c("\\dagger", "*", "**", "***"),
          notes        = "$\\dagger<0.1$; *$p<0.05$; **$p<0.01$; ***$p<0.001$", notes.append = FALSE, notes.align = "l",
          title = "GLM estimation on brand satisfaction", no.space=TRUE)


###########################
#### Robustness tests
###########################

########## Model estimations brand selection + controls
model1 <- glm(marca_select ~ conciencia+confia+ familiar + ventas + work + relation,family=binomial(link='logit'),data=conjoint1)
summary(model1)

model2 <- glm(marca_select ~ conciencia+confia+ familiar + ventas + work + relation,family=binomial(link='logit'),data=conjoint2)
summary(model2)

model3 <- glm(marca_select ~ conciencia+confia+ familiar + ventas + work + relation,family=binomial(link='logit'),data=conjoint3)
summary(model3)


stargazer(model1, model2, model3, type="html", out="brand_select_models_controls.html")

########## Model estimations brand selection age differences

model1y <- lrm(marca_select ~ conciencia+confia+ familiar + ventas,data=conjoint1[conjoint1$age<30,],  x=T, y=T)
model2y <- lrm(marca_select ~ conciencia+confia+ familiar + ventas,data=conjoint2[conjoint2$age<30,],  x=T, y=T)
model3y <- lrm(marca_select ~ conciencia+confia+ familiar + ventas,data=conjoint3[conjoint3$age<30,],  x=T, y=T)

model1o <- lrm(marca_select ~ conciencia+confia+ familiar + ventas,data=conjoint1[conjoint1$age>=30,],  x=T, y=T)
model2o <- lrm(marca_select ~ conciencia+confia+ familiar + ventas,data=conjoint2[conjoint2$age>=30,],  x=T, y=T)
model3o <- lrm(marca_select ~ conciencia+confia+ familiar + ventas,data=conjoint3[conjoint3$age>=30,],  x=T, y=T)


model1y.cl<-bootcov(model1y,cluster=conjoint1$id[conjoint1$age<30])
model2y.cl<-bootcov(model2y,cluster=conjoint2$id[conjoint2$age<30])
model3y.cl<-bootcov(model3y,cluster=conjoint3$id[conjoint3$age<30])

model1o.cl<-bootcov(model1o,cluster=conjoint1$id[conjoint1$age>=30])
model2o.cl<-bootcov(model2o,cluster=conjoint2$id[conjoint2$age>=30])
model3o.cl<-bootcov(model3o,cluster=conjoint3$id[conjoint3$age>=30])

stargazer(model1y.cl, model1o.cl, model2y.cl, model2o.cl, model3y.cl, model3o.cl, type="html", out="Tables/brand_trust_models_cl_age.html",
          star.cutoffs = c(0.1, 0.05, 0.01, 0.001))


########## Model estimations brand trust + controls
model1 <- glm(marca_trust ~ conciencia+confia+ familiar + ventas + work + relation,family=binomial(link='logit'),data=conjoint1)
model2 <- glm(marca_trust ~ conciencia+confia+ familiar + ventas + work + relation,family=binomial(link='logit'),data=conjoint2)
model3 <- glm(marca_trust ~ conciencia+confia+ familiar + ventas + work + relation,family=binomial(link='logit'),data=conjoint3)

stargazer(model1, model2, model3, type="html", out="Tables/brand_trust_models_controls.html")

########## Model estimations brand satisfaction + controls
model1 <- glm(marca_satisfy ~ conciencia+confia+ familiar + ventas + work + relation,family=binomial(link='logit'),data=conjoint1)
model2 <- glm(marca_satisfy~ conciencia+confia+ familiar + ventas + work + relation,family=binomial(link='logit'),data=conjoint2)
model3 <- glm(marca_satisfy ~ conciencia+confia+ familiar + ventas + work + relation,family=binomial(link='logit'),data=conjoint3)

stargazer(model1, model2, model3, type="html", out="Tables/brand_satisfy_models_controls.html")





#######################
### FindIt estimations
#######################

conjoint1$conciencia<- factor(conjoint1$conciencia,ordered=FALSE,levels=c("Entrega bolsas de papel", "No entrega bolsas", "Entrega bolsas de plástico"))
conjoint1$confia <-factor(conjoint1$confia, ordered = F, levels = c("Es lo que dice ser", "No se describe su composición" , "Tiene letra chica" ))
conjoint1$familiar <-factor(conjoint1$familiar, ordered = F, levels = c( "Publicidad reconocible", "Publicidad vagamente reconocible", "No se le conoce publicidad"))
conjoint1$ventas<-factor(conjoint1$ventas, ordered = F, levels = c("Responde dudas adecuadamente", "Responde algunas dudas",  "No responde dudas" ))



conjoint2$conciencia<- factor(conjoint2$conciencia,ordered=FALSE,levels=c("Política de Responsabilidad Social Empresarial publicitada", 
                                                                          "Política de Responsabilidad Social Empresarial desconocida",
                                                                          "No tiene Política de Responsabilidad Social Empresarial"))
conjoint2$confia <-factor(conjoint2$confia, ordered = F, levels = c("Es lo que dice ser", "No se describe su composición" , "Tiene letra chica" ))
conjoint2$familiar <-factor(conjoint2$familiar, ordered = F, levels = c( "Publicidad reconocible", "Publicidad vagamente reconocible", "No se le conoce publicidad"))
conjoint2$ventas<-factor(conjoint2$ventas, ordered = F, levels = c("Responde dudas adecuadamente", "Responde algunas dudas",  "No responde dudas" ))



conjoint3$conciencia<- factor(conjoint3$conciencia,ordered=FALSE,levels=c("Entrega bolsas de papel", "No entrega bolsas", "Entrega bolsas de plástico"))
conjoint3$confia <-factor(conjoint3$confia, ordered = F, levels = c("Es lo que dice ser", "No se describe su composición" , "Tiene letra chica" ))
conjoint3$familiar <-factor(conjoint3$familiar, ordered = F, levels = c( "Publicidad reconocible", "Publicidad vagamente reconocible", "No se le conoce Publicidad"))
conjoint3$ventas<-factor(conjoint3$ventas, ordered = F, levels = c("Asesora con cordialidad y amablemente", 
                                                                   "No asesora. Invade para vender",  "No disponible" ))



fit1 <- CausalANOVA(formula=marca_select ~ conciencia+confia+ familiar + ventas,
                    data=conjoint1, cluster=conjoint1$id, nway=1)
#summary(fit1)
plot(fit1)


fit2 <- CausalANOVA(formula=marca_select ~ conciencia+confia+ familiar + ventas,
                    data=conjoint2, cluster=conjoint3$id, nway=1)
#summary(fit2)
plot(fit2)


fit3 <- CausalANOVA(formula=marca_select ~ conciencia+confia+ familiar + ventas,
                    data=conjoint3, cluster=conjoint3$id, nway=1)
#summary(fit3)
plot(fit3)

############
### Figures
############


#Age
ggplot(df, aes(x=age)) +
  geom_density(fill="red") +
  labs(y="Density", x = "Age")

ggsave(paste0("age.png" ,""), width = 15, height = 10, units = c("cm"), dpi = 300, path=Fig.path)

#Gender
ggplot(df, aes(x=gender,fill=gender)) +
  geom_bar(aes(y=(..count..)*100/sum(..count..))) +
  ylim(0,100) +
  labs(y="Percent", x="") + theme(legend.position = "none") 

ggsave(paste0("gender.png" ,""), width = 15, height = 10, units = c("cm"), dpi = 300, path=Fig.path)


#Education
ggplot(df, aes(x=educ_sup_c, fill=educ_sup_c)) +
  geom_bar(aes(y=(..count..)*100/sum(..count..))) +
  ylim(0,100) +
  labs(y="Percent", x="") + theme(legend.position = "none") 

ggsave(paste0("educ_sup.png" ,""), width = 15, height = 10, units = c("cm"), dpi = 300, path=Fig.path)


#Work
ggplot(df, aes(x=work , fill=work)) +
  geom_bar(aes(y=(..count..)*100/sum(..count..))) +
  ylim(0,100) +
  labs(y="Percent", x="") + theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust = 1)) 

ggsave(paste0("work.png" ,""), width = 15, height = 10, units = c("cm"), dpi = 300, path=Fig.path)

#relationship status
ggplot(df, aes(x=relation , fill=relation)) +
  geom_bar(aes(y=(..count..)*100/sum(..count..))) +
  ylim(0,100) +
  labs(y="Percent", x="") + theme(legend.position = "none") 

ggsave(paste0("relationship.png" ,""), width = 15, height = 10, units = c("cm"), dpi = 300, path=Fig.path)





######################################
#### Balance tests 
######################################


#### Balance tests #####
bal_conciencia <- multinom(conciencia ~ gender + age + educ_sup + work + relation, data = conjoint)
bal_confia <- multinom(confia ~ gender + age + educ_sup + work + relation, data = conjoint)
bal_familiar <- multinom(familiar ~ gender + age + educ_sup + work + relation, data = conjoint)
bal_ventas <- multinom(ventas ~ gender + age + educ_sup + work + relation, data = conjoint)

stargazer(bal_conciencia, type="html", out="Tables/balance_conciencia.html")
stargazer(bal_confia, type="html", out="Tables/balance_confia.html")
stargazer(bal_familiar, type="html", out="Tables/balance_familiar.html")
stargazer(bal_ventas, type="html", out="Tables/balance_ventas.html")
