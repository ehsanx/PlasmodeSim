rm(list=ls(all=TRUE))
library(plyr) 
library(stringr)
library(mgcv)
require(Plasmode)
setwd("~/GitHub/PlasmodeSimBinary/Binary outcome")
load("rhcX.RData")
head(rhc)
ls()

# Plasmode simulation
source("PlasmodeSim.R")
nSim <- 20
simdata <- PlasmodeBinSim(x = rhc, idVar = "id", outcomeVar = "death", treatVar = "swang1", treatLabel = "",
                          form = paste0(vars, collapse = "+"), 
                          effectOR = 3, MM = 1, nsim = nSim, size = 3000, 
                          eventRate = 0.65, exposedPrev = 0.38)

saveOR <- NULL
for (i in 1:nSim){
  plasmodeData.i <- join(data.frame(id=simdata[,i],EVENT=simdata[,i+nSim]), rhc, by="id", type="left")
  estORx <- exp(summary(glm(as.formula(paste0("EVENT", "~", exposure, "+", paste(vars, collapse = "+"))),
                            family=binomial(link="logit"),data=plasmodeData.i))$coef[exposure,"Estimate"])
  saveOR <- c(estORx,saveOR)
  cat("iteration", i, "has event rate =", round(table(plasmodeData.i$EVENT)[2]/length(plasmodeData.i$EVENT),2),
      "exposure prevalence =", round(table(plasmodeData.i$swang1)[2]/length(plasmodeData.i$swang1),2),
      "OR =", round(estORx,2),
      "cumulative OR=", round(mean(saveOR),2),"\n")
}
rm(simdata)

nSim <- 20
formulaOut <- as.formula(paste0("death ~ swang1 +", paste(vars, collapse = "+")))
#fity <- glm(paste0("death ~ swang1 +", paste(vars, collapse = "+")), family = binomial, data = rhc)
simdata.obj <- PlasmodeBin(formulaOut,
                       #objectOut=fity, 
                       data=rhc,
                       idVar="id",
                       effectOR =3,
                       nsim=nSim, 
                       size=3000, 
                       eventRate=0.65, 
                       exposedPrev=0.38)
simdata <- simdata.obj$Sim_Data
saveOR <- NULL
for (i in 1:nSim){
  plasmodeData.i <- join(data.frame(id=simdata[,i],EVENT=simdata[,i+nSim]), rhc, by="id", type="left")
  estORx <- exp(summary(glm(as.formula(paste0("EVENT", "~", exposure, "+", paste(vars, collapse = "+"))),
                            family=binomial(link="logit"),data=plasmodeData.i))$coef[exposure,"Estimate"])
  saveOR <- c(estORx,saveOR)
  cat("iteration", i, "has event rate =", round(table(plasmodeData.i$EVENT)[2]/length(plasmodeData.i$EVENT),2),
      "exposure prevalence =", round(table(plasmodeData.i$swang1)[2]/length(plasmodeData.i$swang1),2),
      "OR =", round(estORx,2),
      "cumulative OR=", round(mean(saveOR),2),"\n")
}

