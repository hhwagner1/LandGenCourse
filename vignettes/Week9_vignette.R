## ----message=FALSE, warning=TRUE----------------------------------------------
require(LandGenCourse)
#require(LandGenCourseData)
#require(fields)  
#require(RColorBrewer)
#require(maps)
#require(mapplots)
#require(here)

## ----message=FALSE, warning=TRUE----------------------------------------------
if(!requireNamespace("fields", quietly = TRUE)) install.packages("fields", repos='http://cran.us.r-project.org')
  
if(!requireNamespace("LEA", quietly = TRUE)) {  
  if (!requireNamespace("BiocManager", quietly = TRUE))
    install.packages("BiocManager")
  BiocManager::install("LEA")
}

## -----------------------------------------------------------------------------
if(!requireNamespace("LandGenCourseData", quietly = TRUE))
            devtools::install_github("hhwagner1/LandGenCourseData")

## -----------------------------------------------------------------------------
file <- scan(file = system.file("extdata", "dataNm2.txt", package = "LandGenCourse"),
             what ="character", sep="\n", skip = 2)
genotype <- NULL
for(locus in 1:100){
	res.locus <- file[4:203]
	file <- file[-(1:203)]
	genotype <- cbind(genotype, as.numeric(as.factor(res.locus)))}
dim(genotype)

## -----------------------------------------------------------------------------
pc = prcomp(genotype, scale =T)   

## -----------------------------------------------------------------------------
par(mar=c(4,4,0.5,0.5))
plot(pc$x, pch = 19, cex = 2, col = rep(c("blue2", "orange"), each = 100))

## -----------------------------------------------------------------------------
summary(pc)

## ----fig.height=3.5, fig.width=7----------------------------------------------
par(mar=c(4,4,0.5,0.5))
coord <- cbind(sort(c(rnorm(100, -2, 1), rnorm(100, 2, 1))), runif(200))
fit  <- fields::Krig(coord, pc$x[,1], m=1)
fields::surface(fit)
points(coord, pch=19) 

## -----------------------------------------------------------------------------
table((pc$x[,1] > 0) == (1:200 <= 100))

## -----------------------------------------------------------------------------
file <- scan(file = system.file("extdata", "dataNm10.txt", package = "LandGenCourse"),
             what ="character", sep="\n", skip = 2)

## -----------------------------------------------------------------------------
# A function for the shape of a cline
sigmoid <- function(x){ 1/(1 + exp(-x))}
p1 <- sigmoid( 0.5 * coord[,1])

## -----------------------------------------------------------------------------
genotype = read.table(file = system.file("extdata", "dataNm1.str", package = "LandGenCourse"))[,-(1:2)]
admixed.genotype <- matrix(NA, ncol = 100, nrow = 200)
for (i in 1:100){ for (j in 1:100)
admixed.genotype[i,j] = sample( c(genotype[i, j],genotype[i+100, j]), 1, prob = c(p1[i], 1 - p1[i]) )}

for (i in 101:200){ for (j in 1:100) 
admixed.genotype[i,j] = sample( c(genotype[i - 100, j],genotype[i, j]), 1, prob = c(p1[i], 1 - p1[i]) )}
res <- data.frame(coord, admixed.genotype)

## -----------------------------------------------------------------------------
par(mar=c(4,4,0.5,0.5))
pcA = prcomp(admixed.genotype, scale =T)   
plot(pcA$x, pch = 19, cex = 2, col = rep(c("blue2","orange"), each = 100))

## ----fig.height=3.5, fig.width=7----------------------------------------------
par(mar=c(4,4,0.5,0.5))
fit  <- fields::Krig(res, pcA$x[,1], m=1)
fields::surface(fit)
points(res) 

## -----------------------------------------------------------------------------
summary(lm(pcA$x[,1]~ p1))

## -----------------------------------------------------------------------------
data <- read.table(system.file("extdata", "stickleback_data.txt", package = "LandGenCourseData"), 
                   sep="\t", as.is=T, check.names=F)

## -----------------------------------------------------------------------------
pops <- unique(unlist(lapply(rownames(data),
        function(x){y<-c();y<-c(y,unlist(strsplit(x,"_")[[1]][1]))})))         

## -----------------------------------------------------------------------------
sample_sites <- rep(NA,nrow(data))
for (i in 1:nrow(data)){
  sample_sites[i] <- strsplit(rownames(data),"_")[[i]][1]}
N <- unlist(lapply(pops,function(x){length(which(sample_sites==x))}))
names(N) <- pops
N

## -----------------------------------------------------------------------------
par(mar=c(4,4,2,0.5))
pcaS <- prcomp(data,center=T)
plot(pcaS$sdev^2 / sum(pcaS$sdev^2), xlab="PC",
     ylab="Fraction Variation Explained", main="Scree plot")

## -----------------------------------------------------------------------------
perc <- round(100*(pcaS$sdev^2 / sum(pcaS$sdev^2))[1:10],2)
names(perc) <- apply(array(seq(1,10,1)), 1, function(x){paste0("PC", x)})
perc 

## -----------------------------------------------------------------------------
colors <- RColorBrewer::brewer.pal(9, "Paired") 

## ----fig.height=5, fig.width=7------------------------------------------------
par(mfrow=c(2,2), mar=c(4,4,0.5,0.5))
plot(pcaS$x[,1:2], col=colors[factor(sample_sites)], pch=16,cex=1.2)
legend("bottomleft", legend=levels(factor(sample_sites)), 
       col=colors, pch=16, ncol=3, cex=0.8)
plot(pcaS$x[,2:3], col=colors[factor(sample_sites)], pch=16, cex=1.2)
plot(pcaS$x[,3:4], col=colors[factor(sample_sites)], pch=16, cex=1.2)

## -----------------------------------------------------------------------------
#snmf2 <- LEA::snmf(paste0(here::here(), "/data/stickleback.geno"), 
#                   K=1:8, ploidy=2, entropy=T, alpha=100, project="new")
#snmf2 <- LEA::snmf("stickleback.geno", K=1:8, ploidy=2, entropy=T, 
#                   alpha=100, project="new")
#par(mfrow=c(1,1))
#plot(snmf2, col="blue4", cex=1.4, pch=19)

## -----------------------------------------------------------------------------
K=4
snmf = LEA::snmf(system.file("extdata", "stickleback.geno", package = "LandGenCourseData"), 
                 K = K, alpha = 100, project = "new")

## -----------------------------------------------------------------------------
qmatrix = LEA::Q(snmf, K = K)

## ----fig.height=3, fig.width=7------------------------------------------------
par(mar=c(4,4,0.5,0.5))
barplot(t(qmatrix), col=RColorBrewer::brewer.pal(9,"Paired"), 
        border=NA, space=0, xlab="Individuals", 
        ylab="Admixture coefficients")
#Add population labels to the axis:
for (i in 1:length(pops)){
  axis(1, at=median(which(sample_sites==pops[i])), labels=pops[i])}

## -----------------------------------------------------------------------------
sites <- read.csv(system.file("extdata", "stickleback_coordinates.csv", 
                              package = "LandGenCourseData"), as.is=T, check.names=F, h=T)

## -----------------------------------------------------------------------------
#initialize array for ancestry proportions:
qpop <- matrix(NA,nrow=length(pops),ncol=K) 

#intialize array for coordinates:
coord.pop <- matrix(NA,nrow=length(pops),ncol=2) 
index=0

for (i in 1:length(pops)){
  if (i==1){
    # input pop ancestry proportions for each K cluster:
    qpop[i,] <- apply(qmatrix[1:N[i],], 2, mean)
    #input pop coordinates:
    coord.pop[i,1] <- sites[which(sites[,1]==names(N[i])),6]
    #input pop coordinates:
    coord.pop[i,2] <- sites[which(sites[,1]==names(N[i])),5] 
    index = index + N[i]
  } else {
    qpop[i,] <- apply(qmatrix[(index+1):(index+N[i]),], 2, mean)
    coord.pop[i,1] <- sites[which(sites[,1]==names(N[i])),6]
    coord.pop[i,2] <- sites[which(sites[,1]==names(N[i])),5]
    index = index + N[i]
  }
}

