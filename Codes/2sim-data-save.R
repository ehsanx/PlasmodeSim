setwd("E:/GitHub/PlasmodeSim/")
require(Plasmode)
require(dplyr)
library(parallel)
library(pbapply)
library(progress)

make_ids_unique <- function(ids) {
  dup_ids <- ids[duplicated(ids) | duplicated(ids, fromLast = TRUE)]
  unique_dup_ids <- unique(dup_ids)
  for (id in unique_dup_ids) {
    suffix <- 1
    indices <- which(ids == id)
    for (index in indices) {
      ids[index] <- paste0(ids[index], ".", suffix)
      suffix <- suffix + 1
    }
  }
  return(ids)
}

subscript <- ""
base_dir <- "data"
intermediate_dir <- paste0(base_dir, "/scenario", subscript)
dir.create(base_dir, showWarnings = FALSE)
dir.create(intermediate_dir, showWarnings = FALSE)

rhc_prep3 <- readRDS("data/rhc_prep3.rds")
simdata <- readRDS(paste0("data/simdata", subscript, ".rds"))
nSim <- 1000
x.var <- c("age", "sex.Female", "race.black", "race.other", "edu", "income.25..50k",
           "income...50k", "incomeUnder..11k", "ninsclasMedicare", "ninsclasMedicare...Medicaid",
           "ninsclasNo.insurance", "ninsclasPrivate", "ninsclasPrivate...Medicare",
           "cat1.CHF", "cat1.Other", "cat1.MOSF", "respYes", "cardYes",
           "neuroYes", "gastrYes", "renalYes", "metaYes", "hemaYes", "sepsYes",
           "traumaYes", "orthoYes", "das2d3pc", "dnr1Yes", "ca.Localized..Yes.",
           "ca.Metastatic", "surv2md1", "aps1", "scoma1", "wtkilo1", "temp1",
           "meanbp1", "resp1", "hrt1", "pafi1", "paco21", "ph1", "wblc1",
           "hema1", "sod1", "pot1", "crea1", "bili1", "alb1", "cardiohx",
           "chfhx", "dementhx", "psychhx", "chrpulhx", "renalhx", "liverhx",
           "gibledhx", "malighx", "immunhx", "transhx", "amihx")

simdata$X <- NULL
exposure <- "tx"

simulate <- function(i, simdata, rhc_prep3, x.var, exposure, nSim, intermediate_dir) {
  plasmodeData.i <- left_join(x = data.frame(id = simdata[,i], EVENT = simdata[,i+nSim]), y = rhc_prep3, by = "id")
  plasmodeData.i$death <- plasmodeData.i$EVENT
  plasmodeData.i$EVENT <- NULL
  plasmodeData.i$X.1 <- NULL
  plasmodeData.i$X <- NULL
  plasmodeData.i$id <- make_ids_unique(plasmodeData.i$id)
  saveRDS(plasmodeData.i, file = paste0(intermediate_dir, "/data_", i, ".rds"))
  return(plasmodeData.i)
}

# Initialize the cluster
no_cores <- detectCores() - 2
cl <- makeCluster(no_cores)

# Export necessary variables and functions to the cluster
clusterExport(cl, varlist = c("simulate", "simdata", "rhc_prep3", "x.var", "exposure", "nSim", "make_ids_unique", "intermediate_dir"))

# Load necessary libraries on each worker node
clusterEvalQ(cl, {
  library(dplyr)
  library(Plasmode)
})

# Initialize the progress bar correctly
progress_bar <- progress::progress_bar$new(total = nSim)

# Wrapper to update the progress bar in the main thread
simulate_with_progress <- function(i) {
  res <- clusterCall(cl, simulate, i, simdata, rhc_prep3, x.var, exposure, nSim, intermediate_dir)
  progress_bar$tick()
  return(res[[1]])
}

# Use a loop to run the simulations and update the progress bar in the main thread
results <- tryCatch({
  lapply(1:nSim, simulate_with_progress)
}, error = function(e) {
  print(paste("Error in lapply:", e))
  stopCluster(cl)  # Ensure the cluster is stopped in case of error
  NULL
})

# Stop the cluster
stopCluster(cl)
