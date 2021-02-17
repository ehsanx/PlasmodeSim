# simdata <- NULL
# x = rhcX
# idVar = id
# outcomeVar = outcome
# treatVar = exposure
# treatLabel = treatLabel
# form = RHScov
# effectOR = 3
# MM = 1
# size = 3000
# eventRate = .5
# exposedPrev = NULL

PlasmodeBinSim <- function(x, idVar, outcomeVar, treatVar, treatLabel = NULL, form, effectOR = 1.5, MM = 1, 
                       nsim = 500, size = nrow(x), eventRate = NULL, exposedPrev = NULL)
{
  # x = datset on which sims are based
  # idVar = name of id variable
  # outcomeVar = name of outcome variable
  # treatVar = name of treatment variable
  # treatLabel = label of the active treatment (if treatment coded as factor)
  # form = RHS of formula used for outcome simulation (except for treatment variable)
  # effectOR = the desired treatment effect odds ratio
  # MM = multiplier of confounder effects on outcome on the log-scale
  # nsim = number of desired datasets
  # size = desired size of simulated datasets (i.e., # of individuals)
  # eventRate = desired average event rate -- default is the event
  # rate observed in the base dataset
  
  test <- FALSE # test <- TRUE
  if (test == TRUE){
    x = masterfile
    idVar = id
    outcomeVar = outcome
    treatVar = exposure
    treatLabel = "RHC"
    form = RHScov 
    effectOR = 1
    MM = 1
    nsim = nSim
    size = 5000
    eventRate = .5
    exposedPrev = NULL
  }
  
  #library(mgcv)
  exposure.binary <- as.numeric(x[,treatVar])# - 1
  if (length(rle(exposure.binary)$lengths) > 2) x <- x[order(exposure.binary),] 
  #head(x)
  #tail(x)
  # order according to exposure status, unexposed first
  n <- nrow(x)
  n1 <- sum(exposure.binary)     # number of exposed in real data
  n0 <- n - n1
  size1 <- round(ifelse(is.null(exposedPrev), n1, size*exposedPrev))  
  # desired number of exposed
  size0 <- size - size1
  if(size1 > n1 | size0 > n0) {
    stop("Number of requested exposed or unexposed exceeds observed number -- reduce size")
  }
  form1 <- as.formula(paste(outcomeVar, " ~ ", treatVar, " + ", form, sep = ""))
  
  # estimate logit model for probability of outcome
  pmod <- bam(form1, data = x, family = "binomial", 
              control=gam.control(trace=TRUE))
  #save(pmod, file="trueSIMmodel.RData")
  X <- gam(form1, data = x, family = "binomial", fit = FALSE)$X
  #save(X, file="trueSIMmodelXmatrix.RData")
  pred <- fitted(pmod) # linear predictor
  p <- length(coef(pmod))
  
  # find event rate in base cohort
  if(is.null(eventRate)) eventRate <- mean(pred)
  
  # find intercept value needed to get approximate desired event rate under new parameters
  
  treatVar <- ifelse(is.null(treatLabel), treatVar, paste0(treatVar,treatLabel))
  bnew <- c(coef(pmod)[1], MM*coef(pmod)[-1])
  bnew <- replace(bnew, names(coef(pmod)) == treatVar, log(effectOR))
  Xbnew <- as.vector(X %*% bnew)
  fn <- function(d) mean(1 - 1/(1 + exp(d+Xbnew))) - eventRate
  delta <- uniroot(fn, lower = -20, upper = 20)$root
  pnew <- 1 - 1/(1 + exp(delta+Xbnew))
  
  # If there is any missing data
  # nX<-as.numeric(table(X[,"exposure"]))
  # n0 <- nX[1]
  # n1 <- nX[2]
  
  #### sample and simulate
  ids <- ynew <- data.frame(matrix(nrow = size, ncol = nsim))
  for(sim in 1:nsim) { # sim = 1
    idxs0 <- sample(1:n0, size0, replace = TRUE) 
    # sample unexposed (located in rows 1:n0 of x)
    ids[1:size0,sim] <- x[idxs0, idVar]
    idxs1 <- sample(n0+1:n1, size1, replace = TRUE) 
    # sample exposed (located in rows n0 + 1:n1 of x)
    ids[size0+1:size1,sim] <- x[idxs1, idVar]
    ynew[,sim] <- rbinom(size, 1, pnew[c(idxs0,idxs1)])
    cat("simulation ID created for iteration", sim, "\n")
  }
  names(ids) <- paste("ID", 1:nsim, sep = "")
  names(ynew) <- paste("EVENT", 1:nsim, sep = "")
  #save(ids,ynew,file="simdataX.RData")
  data.frame(ids, ynew)
}



