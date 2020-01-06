## ----packages global_options, include=TRUE, results="hide", message=FALSE, warning=FALSE----
library(LandGenCourse)
#library(vegan)    # Used to run PCA & RDA

## ----message=FALSE, warning=TRUE----------------------------------------------
if(!requireNamespace("qvalue", quietly = TRUE)) {  
  if (!requireNamespace("BiocManager", quietly = TRUE))
    install.packages("BiocManager")
  BiocManager::install("qvalue")
}

if(!requireNamespace("LEA", quietly = TRUE)) {  
  if (!requireNamespace("BiocManager", quietly = TRUE))
    install.packages("BiocManager")
  BiocManager::install("LEA")
}
#require(LEA)
#require(qvalue)

## ----setup--------------------------------------------------------------------
knitr::opts_knit$set(root.dir = normalizePath(paste0(here::here(),"/output"))) 

## ----load---------------------------------------------------------------------
gen <- read.csv(system.file("extdata", "wolf_geno_samp_10000.csv", 
                            package = "LandGenCourse"), row.names=1)
dim(gen)

## ----NA-----------------------------------------------------------------------
sum(is.na(gen)) # 27,987 NAs in the matrix (~3% missing data)
gen.imp <- apply(gen, 2, function(x) replace(x, is.na(x), 
                 as.numeric(names(which.max(table(x))))))
sum(is.na(gen.imp)) # No NAs

## ---- load.env----------------------------------------------------------------
env <- read.csv(system.file("extdata", "EnvironmentalData_8pred.csv", 
                            package = "LandGenCourse"))

# Look at the structure of the data frame:
str(env) 
# Make individual names characters (not factors):
env$individual <- as.character(env$individual) 

# Confirm that genotypes and environmental data are in the same order:
identical(rownames(gen.imp), env[,1]) 

## ---- env.prep----------------------------------------------------------------
pred <- env[,5:12]
colnames(pred) <- c("AMT","MDR","sdT","AP","cvP","NDVI","Elev","Tree")

## ---- PCA---------------------------------------------------------------------
pred.pca <- vegan::rda(pred, scale=T)
summary(pred.pca)$cont
screeplot(pred.pca, main = "Screeplot - Eigenvalues")

# correlations between the PC axis and predictors:
round(vegan::scores(pred.pca, choices=1:8, display="species", scaling=0), digits=3)

## ---- PC1---------------------------------------------------------------------
pred.PC1 <- vegan::scores(pred.pca, choices=1, 
                          display="sites", scaling=0)

## ---- snmf, results="hide"----------------------------------------------------
outfile <- file.path(here::here(), "output", "genotypes.geno")
LEA::write.geno(gen.imp, outfile)
findK <- NULL
findK <- LEA::snmf(outfile, K=1:6, ploidy=2, entropy=T, rep=1, project = "new")
#plot(findK)

#LEA::write.geno(gen.imp, "genotypes.geno")
#findK <- NULL
#findK <- LEA::snmf("genotypes.geno", 
#                   K=1:6, ploidy=2, entropy=T, rep=1, project = "new")
#plot(findK)

## ---- setK--------------------------------------------------------------------
K <- 3

## ---- LFMM, results="hide"----------------------------------------------------
write.table(gen.imp, file=file.path(here::here(), "output", "gen.lfmm"), 
            row.names=F, col.names=F)
write.table(pred.PC1, file=file.path(here::here(), "output","PC1.env"), 
            row.names=F, col.names=F)


## ---- LFMM1, results="hide"---------------------------------------------------
wolf.lfmm <- NULL

wolf.lfmm <- LEA::lfmm(
  input.file=file.path(here::here(), "output", "gen.lfmm"),
  environment.file=file.path(here::here(), "output", "PC1.env"), 
  K=K, project="new", repetitions=3, iterations=4000, burnin=2000)

#wolf.lfmm <- LEA::lfmm(
#  input.file=file.path(here::here(), "output", "gen.lfmm"), 
#  environment.file=file.path(here::here(), "output", "PC1.env"), 
#  K=K, project="new", repetitions=10, iterations=5000, burnin=2500)

## -----------------------------------------------------------------------------
zs <- LEA::z.scores(wolf.lfmm, K=K, d=1)
zs.med <- apply(zs, MARGIN=1, median)

## -----------------------------------------------------------------------------
GIF <- median(zs.med^2)/qchisq(0.5, df = 1)
GIF

## -----------------------------------------------------------------------------
adj.pv <- pchisq(zs.med^2/GIF, df = 1, lower = FALSE)
hist(adj.pv, main="Histogram of adjusted p-values")

## -----------------------------------------------------------------------------
qv <- which(qvalue::qvalue(adj.pv, fdr=0.05)$signif)
# the names of the loci detected as candidates by lfmm:
cand.lfmm <- colnames(gen.imp)[qv]   
length(cand.lfmm)

## ---- rda---------------------------------------------------------------------
wolf.rda <- vegan::rda(gen.imp ~ ., data=pred, scale=T)
wolf.rda

## ---- R2----------------------------------------------------------------------
vegan::RsquareAdj(wolf.rda)

## -----------------------------------------------------------------------------
summary(wolf.rda)$concont

## ---- screeplot, , fig.width=6.2, fig.height=4--------------------------------
screeplot(wolf.rda)

## ---- VIF---------------------------------------------------------------------
vegan::vif.cca(wolf.rda)

## ---- simple_plot, fig.width=4, fig.height=4, fig.show='hold'-----------------
plot(wolf.rda, scaling=3)          # default is axes 1 and 2
plot(wolf.rda, choices = c(1,3), scaling=3)  # axes 1 and 3

## ---- loadings----------------------------------------------------------------
load.rda <- summary(wolf.rda)$species[,1:3]

## ---- loadings_plot, fig.width=2.5, fig.height=2.5, fig.show='hold'-----------
hist(load.rda[,1], main="Loadings on RDA1")
hist(load.rda[,2], main="Loadings on RDA2")
hist(load.rda[,3], main="Loadings on RDA3") 

## ---- outliers----------------------------------------------------------------
outliers <- function(x,z){
  # find loadings +/- z SD from mean loading:     
  lims <- mean(x) + c(-1, 1) * z * sd(x) 
  # locus names in these tails:
  x[x < lims[1] | x > lims[2]]           
}

## ---- candidates--------------------------------------------------------------
cand1 <- outliers(load.rda[,1],3) # 38
cand2 <- outliers(load.rda[,2],3) # 69
cand3 <- outliers(load.rda[,3],3) # 34

length(cand1)+length(cand2)+length(cand3)

## ---- outliers_df1------------------------------------------------------------
cand1 <- cbind.data.frame(rep(1,times=length(cand1)), names(cand1), unname(cand1))
cand2 <- cbind.data.frame(rep(2,times=length(cand2)), names(cand2), unname(cand2))
cand3 <- cbind.data.frame(rep(3,times=length(cand3)), names(cand3), unname(cand3))

colnames(cand1) <- colnames(cand2)<- colnames(cand3) <- c("axis","snp","loading")

cand.rda <- rbind(cand1, cand2, cand3)
cand.rda$snp <- as.character(cand.rda$snp)

## ---- detections--------------------------------------------------------------
# 7 duplicate detections:
length(cand.rda$snp[duplicated(cand.rda$snp)])
# 134 unique candidate SNPs:
cand.rda <- cand.rda[!duplicated(cand.rda$snp),] 
# duplicates were on axis 2:
table(cand.rda$axis)                             

## ---- env.axes----------------------------------------------------------------
vegan::intersetcor(wolf.rda)[,1:3]

## ---- overlap-----------------------------------------------------------------
overlap <- cand.rda$snp[cand.rda$snp %in% cand.lfmm]
length(overlap)

## ----message=FALSE, warning=TRUE, include=FALSE-------------------------------
#LandGenCourse::detachAllPackages()

