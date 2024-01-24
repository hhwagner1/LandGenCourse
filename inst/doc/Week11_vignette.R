## ----install global_options, include=TRUE, results="hide", message=FALSE, warning=FALSE--------------
if(!requireNamespace("qvalue", quietly = TRUE)) {  
  if (!requireNamespace("BiocManager", quietly = TRUE))
    install.packages("BiocManager")
  BiocManager::install(version = "3.14")
  BiocManager::install("qvalue")
}
if(!requireNamespace("lfmm", quietly = TRUE)) {  
  remotes::install_github("bcm-uga/lfmm")
}


## ----packages global_options, include=TRUE, results="hide", message=FALSE, warning=FALSE-------------
library(LandGenCourse)
library(vegan)    # Used to run PCA & RDA
library(lfmm)     # Used to run LFMM
library(qvalue)   # Used to post-process LFMM output


## ----load--------------------------------------------------------------------------------------------
gen <- read.csv(system.file("extdata", "wolf_geno_samp_10000.csv", package = "LandGenCourse"), row.names=1)
dim(gen)


## ----NA----------------------------------------------------------------------------------------------
sum(is.na(gen)) # 27,987 NAs in the matrix (~3% missing data)
gen.imp <- apply(gen, 2, function(x) replace(x, is.na(x), as.numeric(names(which.max(table(x))))))
sum(is.na(gen.imp)) # No NAs


## ----load.env----------------------------------------------------------------------------------------
env <- read.csv(system.file("extdata", "EnvironmentalData_8pred.csv", package = "LandGenCourse"))
str(env) # Look at the structure of the data frame
env$individual <- as.character(env$individual) # Make individual names characters (not factors)

# Confirm that genotypes and environmental data are in the same order
identical(rownames(gen.imp), env[,1]) 


## ----env.prep----------------------------------------------------------------------------------------
pred <- env[,5:12]
colnames(pred) <- c("AMT","MDR","sdT","AP","cvP","NDVI","Elev","Tree")


## ----fig.height=4, fig.width=6-----------------------------------------------------------------------
pred.pca <- rda(pred, scale=T)
summary(pred.pca)$cont
screeplot(pred.pca, main = "Screeplot: Eigenvalues of Wolf Predictor Variables")

# correlations between the PC axis and predictors:
round(scores(pred.pca, choices=1:8, display="species", scaling=0), digits=3)


## ----PC1---------------------------------------------------------------------------------------------
pred.PC1 <- scores(pred.pca, choices=1, display="sites", scaling=0)


## ----fig.height=4, fig.width=6-----------------------------------------------------------------------
screeplot(pred.pca, main = "Screeplot of Wolf Predictor Variables with Broken Stick", bstick=TRUE, type="barplot")


## ----fig.height=4, fig.width=6-----------------------------------------------------------------------
gen.pca <- rda(gen.imp, scale=T)
screeplot(gen.pca, main = "Screeplot of Genetic Data with Broken Stick", bstick=TRUE, type="barplot")


## ----setK--------------------------------------------------------------------------------------------
K <- 3


## ----------------------------------------------------------------------------------------------------
wolf.lfmm <- lfmm_ridge(Y=gen.imp, X=pred.PC1, K=K) # change K as you see fit


## ----------------------------------------------------------------------------------------------------
wolf.pv <- lfmm_test(Y=gen.imp, X=pred.PC1, lfmm=wolf.lfmm, calibrate="gif")

names(wolf.pv) # this object includes raw z-scores and p-values, as well as GIF-calibrated scores and p-values


## ----------------------------------------------------------------------------------------------------
wolf.pv$gif


## ----fig.width=5, fig.height=3, fig.show='hold'------------------------------------------------------
hist(wolf.pv$pvalue[,1], main="Unadjusted p-values")        
hist(wolf.pv$calibrated.pvalue[,1], main="GIF-adjusted p-values")


## ----------------------------------------------------------------------------------------------------
# Let's change the GIF and readjust the p-values:
zscore <- wolf.pv$score[,1]   # zscores for first predictor, we only have one in our case...
(gif <- wolf.pv$gif[1])       # default GIF for this predictor
new.gif1 <- 2.0               # choose your new GIF

# Manual adjustment of the p-values:
adj.pv1 <- pchisq(zscore^2/new.gif1, df=1, lower = FALSE)


## ----fig.width=5, fig.height=3, fig.show='hold'------------------------------------------------------
hist(wolf.pv$pvalue[,1], main="Unadjusted p-values")        
hist(wolf.pv$calibrated.pvalue[,1], main="GIF-adjusted p-values (GIF=2.8)")
hist(adj.pv1, main="REadjusted p-values (GIF=2.0)")


## ----------------------------------------------------------------------------------------------------
wolf.qv <- qvalue(wolf.pv$calibrated.pvalue)$qvalues

length(which(wolf.qv < 0.1)) # how many SNPs have an FDR < 10%?

(wolf.FDR.1 <- colnames(gen.imp)[which(wolf.qv < 0.1)]) # identify which SNPs these are


## ----rda---------------------------------------------------------------------------------------------
wolf.rda <- rda(gen.imp ~ ., data=pred, scale=T)
wolf.rda


## ----R2----------------------------------------------------------------------------------------------
RsquareAdj(wolf.rda)


## ----------------------------------------------------------------------------------------------------
summary(wolf.rda)$concont


## ----screeplot, , fig.width=6.2, fig.height=4--------------------------------------------------------
screeplot(wolf.rda)


## ----VIF---------------------------------------------------------------------------------------------
vif.cca(wolf.rda)


## ----simple_plot, fig.width=5, fig.height=5----------------------------------------------------------
plot(wolf.rda, scaling=3)  # default is axes 1 and 2


## ----loadings----------------------------------------------------------------------------------------
load.rda <- summary(wolf.rda)$species[,1:3]


## ----loadings_plot, fig.width=2.5, fig.height=3, fig.show='hold'-------------------------------------
hist(load.rda[,1], main="Loadings on RDA1")
hist(load.rda[,2], main="Loadings on RDA2")
hist(load.rda[,3], main="Loadings on RDA3") 


## ----outliers----------------------------------------------------------------------------------------
outliers <- function(x,z){
  lims <- mean(x) + c(-1, 1) * z * sd(x) # find loadings +/- z SD from mean loading     
  x[x < lims[1] | x > lims[2]]           # locus names in these tails
}


## ----candidates--------------------------------------------------------------------------------------
cand1 <- outliers(load.rda[,1], 3) # 38
cand2 <- outliers(load.rda[,2], 3) # 69
cand3 <- outliers(load.rda[,3], 3) # 34

wolf.rda.cand <- c(names(cand1), names(cand2), names(cand3)) # just the names of the candidates

length(wolf.rda.cand[duplicated(wolf.rda.cand)]) # 7 duplicate detections (detected on multiple RDA axes)
wolf.rda.cand <- wolf.rda.cand[!duplicated(wolf.rda.cand)] # 134 unique candidates 


## ----fig.width=5, fig.height=5, fig.show='hold'------------------------------------------------------
# Set up the color scheme for plotting:
bgcol  <- ifelse(colnames(gen.imp) %in% wolf.rda.cand, 'gray32', '#00000000')
snpcol <- ifelse(colnames(gen.imp) %in% wolf.rda.cand, 'red', '#00000000')

# axes 1 & 2 - zooming in to just the SNPs here...
plot(wolf.rda, type="n", scaling=3, xlim=c(-1,1), ylim=c(-1,1), main="Wolf RDA, axes 1 and 2")
points(wolf.rda, display="species", pch=21, cex=1, col="gray32", bg='#f1eef6', scaling=3)
points(wolf.rda, display="species", pch=21, cex=1, col=bgcol, bg=snpcol, scaling=3)
text(wolf.rda, scaling=3, display="bp", col="#0868ac", cex=1)

# axes 2 & 3
plot(wolf.rda, type="n", scaling=3, xlim=c(-1,1), ylim=c(-1,1), choices=c(2,3), main="Wolf RDA, axes 2 and 3")
points(wolf.rda, display="species", pch=21, cex=1, col="gray32", bg='#f1eef6', scaling=3, choices=c(2,3))
points(wolf.rda, display="species", pch=21, cex=1, col=bgcol, bg=snpcol, scaling=3, choices=c(2,3))
text(wolf.rda, scaling=3, display="bp", col="#0868ac", cex=1, choices=c(2,3))


## ----env.axes----------------------------------------------------------------------------------------
intersetcor(wolf.rda)[,1:3]


## ----overlap-----------------------------------------------------------------------------------------
intersect(wolf.FDR.1, wolf.rda.cand) # found by both LFMM and RDA
setdiff(wolf.FDR.1, wolf.rda.cand)   # unique to LFMM


## ----eval=F------------------------------------------------------------------------------------------
## ?rda  # help for running RDA
## 
## # pseudo code for simple RDA:
## foo <- rda(genomic.data ~ predictor1 + predictor2, data=dat
## 
## # pseudo code for partial RDA (correcting for population structure with PCs):
## bar <- rda(genomic.data ~ predictor1 + predictor2 + Condition(PC1), data=dat, scale=T)


## ----message=FALSE, warning=TRUE, include=FALSE------------------------------------------------------
#LandGenCourse::detachAllPackages()

