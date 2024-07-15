# setwd("E:/GitHub/PlasmodeSim/")
setwd("C:/Users/Ehsan/Documents/GitHub/PlasmodeSim")

require(Plasmode)
library(factoextra)
require(dplyr)
require(MatchIt)
require(jtools)

rhc_prep3 <- readRDS(file = "data/rhc_prep3.rds")
theta <- 1
nSim <- 1000
sel.binary.col <- readRDS("data/sel_binary_col.rds")
formulaOut <- as.formula(paste0("death ~ tx +", paste(sel.binary.col[-c(1:2)], collapse = "+"),
                                "+ age + age^2 + age^3 +  edu + exp(wtkilo1) + cos(aps1) +
                                surv2md1 +das2d3pc +temp1+
                                hrt1*meanbp1 + resp1 + wblc1 + pafi1 + pafi1^2 + paco21 + ph1+
                                crea1 +  alb1 + scoma1:hema1:sod1 + pot1 + bili1"))
formula_vars <- all.vars(formulaOut)
# Check if all variables in the formula exist in the data
missing_vars <- formula_vars[!formula_vars %in% names(rhc_prep3)]
if(length(missing_vars) == 0) {
  print("All variables in the formula exist in the data.")
} else {
  print("The following variables are missing in the data:")
  print(missing_vars)
}


simdata.obj <- PlasmodeBin(formulaOut,
                           #objectOut=fity,
                           data=rhc_prep3,
                           idVar="id",
                           effectOR =theta,
                           nsim=nSim,
                           size=3500,
                           eventRate=0.3,
                           exposedPrev=0.05)
simdata <- simdata.obj$Sim_Data
saveRDS(simdata, "data/simdata-OR1ER.rds")
