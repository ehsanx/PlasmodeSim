setwd("E:/GitHub/PlasmodeSim/")

require(caret)
require(Hmisc)
rhc <- read.csv("https://hbiostat.org/data/repo/rhc.csv")
rhc$tx  <-  ifelse(rhc$swang1=="RHC",1,0)  #  Make  treatment  variable  binary
rhc$death  <-  ifelse(rhc$dth30=="Yes",1,0)  #  Make  outcome  variable  binary
rhc$race <- factor(rhc$race, levels=c("white","black","other"))
rhc$sex <- as.factor(rhc$sex)
rhc$sex <- relevel(rhc$sex, ref = "Male")
rhc$cat1 <- as.factor(rhc$cat1)
levels(rhc$cat1) <- c("ARF","CHF","Other","Other","Other","Other","Other","MOSF","MOSF")
rhc$ca <- as.factor(rhc$ca)
levels(rhc$ca) <- c("Metastatic","None","Localized (Yes)")
rhc$ca <- factor(rhc$ca, levels=c("None","Localized (Yes)","Metastatic"))
var.names <- Cs(tx,death,
                age,sex, race,edu, income,
                ninsclas, cat1,
                resp, card,neuro, gastr,
                renal,meta, hema, seps,
                trauma, ortho, das2d3pc,
                dnr1, ca, surv2md1, aps1,
                scoma1, wtkilo1, temp1,
                meanbp1,resp1,hrt1,pafi1,
                paco21, ph1, wblc1,hema1,sod1,
                pot1, crea1,bili1,alb1,cardiohx,
                chfhx, dementhx, psychhx, chrpulhx,
                renalhx, liverhx, gibledhx,
                malighx, immunhx, transhx, amihx)
length(var.names)
rhc2 <- rhc[var.names]
dmy <- dummyVars(" ~ .", data = rhc2, fullRank = TRUE)
rhc_prep <- data.frame(predict(dmy, newdata = rhc2))
y	<- "death"	#	Outcome Variable: 30-day mortality
d	<- "tx"	#	Treatment Indicator
x.var <- names(rhc_prep)[!names(rhc_prep) %in% c(y,d)]
sel.col <- apply(rhc_prep,2,function(x) { all(x %in% 0:1) })
sel.continuous.col <- names(sel.col[sel.col==FALSE])
sel.binary.col <- names(sel.col[sel.col==TRUE])
rhc_prep1 <- rhc_prep[sel.binary.col]
rhc_prep2fit <- preProcess(rhc_prep[sel.continuous.col], method=c("center", "scale"))
rhc_prep2 <- predict(rhc_prep2fit, rhc_prep[sel.continuous.col])
rhc_prep3 <- cbind(rhc_prep1,rhc_prep2)
rhc_prep3$id <- 1:nrow(rhc_prep3)
names(rhc_prep3)
str(rhc_prep3)
#write.csv(rhc_prep3, "rhc3.csv")
saveRDS(sel.binary.col, "data/sel_binary_col.rds")
saveRDS(rhc_prep3, "data/rhc_prep3.rds")
