setwd("E:/GitHub/PlasmodeSim/data/hdData/")
require(Plasmode)
require(dplyr)
library(parallel)
library(pbapply)
library(progress)
library(plyr)

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

load(file = "data/plasmode.RData")

subscript <- ""
base_dir <- "data"
intermediate_dir <- paste0(base_dir, "/scenario", subscript)
dir.create(base_dir, showWarnings = FALSE)
dir.create(intermediate_dir, showWarnings = FALSE)

c <- 1000

simulate <- function(i, plasmode.data, analytic.data, demovars, lab.list, proxy.list, nSim, intermediate_dir) {
  plasmodeData.i <- plyr::join(x = data.frame(idx = plasmode.data[,i],
                                              EVENT = plasmode.data[,i + nSim]),
                               y = analytic.data, by = "idx", type = "left")
  plasmodeData.i$outcome <- plasmodeData.i$EVENT
  plasmodeData.i$EVENT <- NULL
  plasmodeData.i$id <- make_ids_unique(plasmodeData.i$id)
  saveRDS(plasmodeData.i, file = paste0(intermediate_dir, "/data_", i, ".rds"))
  return(plasmodeData.i)
}

# Initialize the cluster
no_cores <- detectCores() - 2
cl <- makeCluster(no_cores)
print(paste("Number of cores:", no_cores))

# Export necessary variables and functions to the cluster
clusterExport(cl, varlist = c("simulate", "simdata", "full.data", "demovars",
                              "labvars.original", "emp.cov.names", "nSim",
                              "make_ids_unique", "intermediate_dir"))

# Load necessary libraries on each worker node
clusterEvalQ(cl, {
  library(dplyr)
  library(Plasmode)
  library(plyr)
})

# Use pbapply to run the simulations in parallel with a progress bar
results <- tryCatch({
  pblapply(1:nSim, function(i) {
    simulate(i, simdata, full.data, demovars, labvars.original, emp.cov.names, nSim, intermediate_dir)
  }, cl = cl)
}, error = function(e) {
  print(paste("Error in parLapply:", e))
  stopCluster(cl)  # Ensure the cluster is stopped in case of error
  NULL
})

# Stop the cluster
stopCluster(cl)

print("Simulation completed.")
