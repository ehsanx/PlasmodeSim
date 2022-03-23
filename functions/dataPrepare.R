rm(list=ls(all=TRUE))
library(plyr) 
library(Publish) 
library(stringr)
library(mgcv)
setwd("~/GitHub/PlasmodeSimBinary/Binary outcome")

# Data DOwnload
# rhc <- read.csv("https://biostat.app.vumc.org/wiki/pub/Main/DataSets/rhc.csv")
rhc <- read.csv("rhc.csv")

# Data prepare
id = "id" 
exposure <- "swang1"
treatLabel <- "" # RHC
outcome <- "death"
vars = c("age", "sex", "race", "edu", "income", "ninsclas", "cat1", "das2d3pc", "dnr1", 
         "ca", "surv2md1", "aps1", "scoma1", "wtkilo1", "temp1", "meanbp1", "resp1", "hrt1", "pafi1", 
         "paco21", "ph1", "wblc1", "hema1", "sod1", "pot1", "crea1", "bili1", "alb1", "resp", "card", 
         "neuro", "gastr", "renal", "meta", "hema", "seps", "trauma", "ortho", "cardiohx", "chfhx", 
         "dementhx", "psychhx", "chrpulhx", "renalhx", "liverhx", "gibledhx", "malighx", "immunhx", 
         "transhx", "amihx")
rhc2 <- rhc[,c(outcome,exposure,vars)]
rhc2$swang1 <- ifelse(rhc2$swang1 == "RHC", 1, 0)
rhc2$death <- ifelse(rhc2$death == "Yes", 1, 0)
names(rhc2) <- c(outcome,exposure,vars)
rhc2$id <- 1:nrow(rhc2)
rhcX <- as.data.frame(na.omit(rhc2))
#dim(rhc2)[1]-dim(rhcX)[1]

# Initial estimate
RHScov = paste0(vars, collapse = "+")
Oformula = as.formula(paste0(outcome, "~", RHScov))
Oformula.adj = as.formula(paste0(outcome, "~", exposure, "+", RHScov))
glm.adj0<- glm(Oformula.adj,family=binomial(link="logit"),data= rhcX) 
estOR <- exp(summary(glm.adj0)$coef["swang1","Estimate"])
Yr <- round(table(rhcX$death)[2]/length(rhcX$death),2)
Er <- round(table(rhcX$swang1)[2]/length(rhcX$swang1),2)
cat("Original data", "has event rate =", Yr,
    "exposure prevalence =", Er,
    "OR =", estOR,"\n")
rhc <- rhcX

#save(rhc, exposure, outcome, vars, RHScov, Oformula, Yr, Er, file = "rhcX.RData")

effect.mod <- "sex"
vars <- vars[!(vars %in% effect.mod)]
RHScov = paste0(vars, collapse = "+")
Oformula.int = as.formula(paste0(outcome, "~", exposure, "+", RHScov, "+", effect.mod, "*", exposure))
glm.int<- glm(Oformula.int,family=binomial(link="logit"),data= rhc) 
summary(glm.int)
publish(glm.int)

