## ----------------------------------------------------------------------------------------------------
if(!requireNamespace("EcoGenetics", quietly = TRUE)) 
  remotes::install_github("leandroroser/EcoGenetics-devel")
if(!requireNamespace("GeNetIt", quietly = TRUE)) remotes::install_github("jeffreyevans/GeNetIt")
if(!requireNamespace("popgraph", quietly = TRUE))
{
  install.packages(c("RgoogleMaps", "geosphere", "proto", "sampling", 
                      "seqinr", "spacetime", "spdep"), dependencies=TRUE)
  remotes::install_github("dyerlab/popgraph")
}
if(!requireNamespace("gstudio", quietly = TRUE)) remotes::install_github("dyerlab/gstudio")


## ----message=FALSE, warning=TRUE---------------------------------------------------------------------

library(LandGenCourse)
library(GeNetIt)
library(dplyr)
library(EcoGenetics)
#require(adegenet)
#require(tibble)
#require(gstudio)
#require(hierfstat)
#require(PopGenReport)
#require(mmod)
#require(spdep)


## ----------------------------------------------------------------------------------------------------
Frogs.coords <- read.csv(system.file("extdata", "ralu_coords_allpops.csv", 
                                     package = "LandGenCourse"))


## ----------------------------------------------------------------------------------------------------
Frogs.diversity <- read.csv(system.file("extdata", "Frogs_diversity_allpops.csv", 
                                     package = "LandGenCourse"))


## ----------------------------------------------------------------------------------------------------
Frogs.loci <- read.csv(system.file("extdata", "ralu_loci_allpops.csv", 
                                     package = "LandGenCourse"))


## ----------------------------------------------------------------------------------------------------
Frogs.ecogen <- ecogen(G = Frogs.loci[,-c(1:2)], ploidy = 2, 
                       type = "codominant", sep = ":",
                       S = data.frame(Frogs.loci[,1:2]))
Frogs.ecogen


## ----------------------------------------------------------------------------------------------------
Frogs.ecopop <- ecogen2ecopop(Frogs.ecogen, hier = "SiteName")
Frogs.ecopop


## ----------------------------------------------------------------------------------------------------
Subset <- ecoslot.S(Frogs.ecogen) %>% group_by(SiteName, Pop) %>% summarize()
Subset <- left_join(Subset, Frogs.diversity)


## ----------------------------------------------------------------------------------------------------
Subset <- left_join(Subset, as.data.frame(Frogs.coords))


## ----------------------------------------------------------------------------------------------------
names(Subset)
Frogs.ecopop <- EcoGenetics::eco.fill_ecogen_with_df(Frogs.ecopop,
             pop ="pop", pop_levels = Subset$SiteName, 
             C = Subset[,3:6],
             XY = Subset[,7:10])
Frogs.ecopop


## ----------------------------------------------------------------------------------------------------
Frogs.genind <- EcoGenetics::ecogen2genind(Frogs.ecogen)
Frogs.genind@pop <- ecoslot.S(Frogs.ecogen)$SiteName


## ----------------------------------------------------------------------------------------------------
Frogs.genpop <- adegenet::genind2genpop(Frogs.genind)
Frogs.genpop


## ----------------------------------------------------------------------------------------------------
#Frogs.gstudio <- EcoGenetics::ecogen2gstudio(Frogs.ecogen, type="codominant")

  dat <- eco.convert(Frogs.ecogen@G, "matrix", sep.in = ":", sep.out = ":")
  dat <- as.data.frame(dat, stringsAsFactors = FALSE)
  for (i in 1:ncol(dat)) {
     class(dat[, i]) <- "locus"
  }
  dat[is.na(dat)] <- gstudio::locus(NA)
  colnames(dat) <- colnames(Frogs.ecogen@G)

Frogs.gstudio <- data.frame(ecoslot.S(Frogs.ecogen), dat)
tibble::as_tibble(Frogs.gstudio)        


## ----------------------------------------------------------------------------------------------------
GD.pop.PairwiseFst.hierfstat <- as.dist(hierfstat::pairwise.neifst(hierfstat::genind2hierfstat(Frogs.genind)))


## ----------------------------------------------------------------------------------------------------
GD.pop.propShared <- PopGenReport::pairwise.propShared(Frogs.genind)


## ----------------------------------------------------------------------------------------------------
GD.pop.Nei <- adegenet::dist.genpop(Frogs.genpop, method=1)   
GD.pop.Edwards <- adegenet::dist.genpop(Frogs.genpop, method=2)
GD.pop.Reynolds <- adegenet::dist.genpop(Frogs.genpop, method=3)  
GD.pop.Rogers <- adegenet::dist.genpop(Frogs.genpop, method=4)  
GD.pop.Provesti <- adegenet::dist.genpop(Frogs.genpop, method=5)


## ----------------------------------------------------------------------------------------------------
GD.pop.Joost <- mmod::pairwise_D(Frogs.genind, linearized = FALSE)
GD.pop.Hedrick <- mmod::pairwise_Gst_Hedrick(Frogs.genind, linearized = FALSE)
GD.pop.NeiGst <- mmod::pairwise_Gst_Nei(Frogs.genind, linearized = FALSE)


## ----------------------------------------------------------------------------------------------------
GD.pop.Euclidean.gstudio <-gstudio::genetic_distance(Frogs.gstudio, mode = "Euclidean", stratum="SiteName")
GD.pop.cGD.gstudio <-gstudio::genetic_distance(Frogs.gstudio, mode = "cGD", stratum="SiteName")
GD.pop.Nei.gstudio <-gstudio::genetic_distance(Frogs.gstudio, mode = "Nei", stratum="SiteName")
GD.pop.Dps.gstudio <-gstudio::genetic_distance(Frogs.gstudio, mode = "Dps", stratum="SiteName")
GD.pop.Jaccard.gstudio <-gstudio::genetic_distance(Frogs.gstudio, mode = "Jaccard", stratum="SiteName")


## ----------------------------------------------------------------------------------------------------
GD.pop <- list(pairwiseFst.hierfstat = GD.pop.PairwiseFst.hierfstat,
               propShared.PopGenReport = 1 - GD.pop.propShared,
               Nei.adegenet = GD.pop.Nei,
               Edwards.adegenet = GD.pop.Edwards,
               Reynolds.adegenet = GD.pop.Reynolds,
               Rogers.adegenet = GD.pop.Rogers,
               Provesti.adegenet = GD.pop.Provesti,
               Joost.mmod = GD.pop.Joost,
               Hedrick.mmod = GD.pop.Hedrick,
               Nei.mmod = GD.pop.NeiGst,
               Euclidean.gstudio = as.dist(GD.pop.Euclidean.gstudio),
               cGD.gstudio = as.dist(GD.pop.cGD.gstudio),
               Nei.gstudio = as.dist(GD.pop.Nei.gstudio),
               Dps.gstudio = as.dist(1 - GD.pop.Dps.gstudio),
               Jaccard.gstudio = as.dist(1 - GD.pop.Jaccard.gstudio))
round(cor(sapply(GD.pop, function(ls) as.vector(ls))),2)[,1:2]


## ----------------------------------------------------------------------------------------------------
#require(here)
#if(!dir.exists(paste0(here::here(),"/output"))) dir.create(paste0(here::here(),"/output"))

#save(GD.pop, file = paste0(here::here(),"/output/GD.pop.RData"))
#load(paste0(here::here(),"/output/GD.pop.RData"))


## ----------------------------------------------------------------------------------------------------
Dgeo <- as.vector(dist(ecoslot.XY(Frogs.ecopop)[,1:2]))


## ----------------------------------------------------------------------------------------------------
par(mar=c(4,4,0,0))

Dgen <- as.vector(GD.pop$propShared.PopGenReport)
dens <- MASS::kde2d(Dgeo, Dgen, n=300)
myPal <- colorRampPalette(c("white","blue","gold","orange","red"))
plot(Dgeo, Dgen, pch=20, cex=0.5,  
    xlab="Geographic Distance", ylab="Genetic Distance")
image(dens, col=transp(myPal(300), 0.7), add=TRUE)
abline(lm(Dgen ~ Dgeo))
lines(loess.smooth(Dgeo, Dgen), col="red")


## ----------------------------------------------------------------------------------------------------
par(mar=c(4,4,0,0))
dens <- MASS::kde2d(log(Dgeo), Dgen, n=300)
plot(log(Dgeo), Dgen, pch=20, cex=0.5,  
    xlab="Geographic Distance", ylab="Genetic Distance")
image(dens, col=transp(myPal(300), 0.7), add=TRUE)
abline(lm(Dgen ~ log(Dgeo)))
lines(loess.smooth(log(Dgeo), Dgen), col="red")


## ----------------------------------------------------------------------------------------------------
Dgen <- GD.pop$propShared.PopGenReport
Dgeo <- dist(ecoslot.XY(Frogs.ecopop)[,1:2])

IBD <- vegan::mantel(Dgen,Dgeo, method="pearson")
IBD


## ----------------------------------------------------------------------------------------------------
IBD <- vegan::mantel(Dgen,log(Dgeo), method="pearson")
IBD


## ----------------------------------------------------------------------------------------------------
IBD <- vegan::mantel(Dgen,Dgeo, method="spearman")
IBD


## ----------------------------------------------------------------------------------------------------
attributes(IBD)


## ----------------------------------------------------------------------------------------------------
Mantel.test <- lapply(GD.pop, function(x) vegan::mantel(x,Dgeo, method="pearson"))
data.frame(Mantel.r = sapply(Mantel.test, function(x) x$statistic),
           p.value = sapply(Mantel.test, function(x) x$signif))


## ----------------------------------------------------------------------------------------------------
Mantel.test <- lapply(GD.pop, function(x) ade4::mantel.randtest(x,log(Dgeo)))
data.frame(Mantel.r = sapply(Mantel.test, function(x) x$obs),
           p.value = sapply(Mantel.test, function(x) x$pvalue))


## ----------------------------------------------------------------------------------------------------
corm <- EcoGenetics::eco.cormantel(M = GD.pop$propShared.PopGenReport, 
        XY = ecoslot.XY(Frogs.ecopop)[,1:2],  nsim = 199, latlon=FALSE, 
        alternative="less", method = "pearson")
corm


## ----------------------------------------------------------------------------------------------------
EcoGenetics::eco.plotCorrelog(corm)


## ----fig.show='hold'---------------------------------------------------------------------------------
corm.50 <- EcoGenetics::eco.cormantel(M = GD.pop$propShared.PopGenReport, 
           XY = ecoslot.XY(Frogs.ecopop)[,1:2],  nsim = 199, latlon=FALSE,
           alternative="less", size=50)
EcoGenetics::ecoslot.OUT(corm.50)

corm.100 <- EcoGenetics::eco.cormantel(M = GD.pop$propShared.PopGenReport, 
           XY = ecoslot.XY(Frogs.ecopop)[,1:2],  nsim = 199, latlon=FALSE,
           alternative="less", size=100)
EcoGenetics::ecoslot.OUT(corm.100)

EcoGenetics::eco.plotCorrelog(corm.50)
EcoGenetics::eco.plotCorrelog(corm.100)


## ----------------------------------------------------------------------------------------------------
Lag1.def <- data.frame(rbind(Sturge = EcoGenetics::ecoslot.OUT(corm)[[1]][1,],
      size.50 = EcoGenetics::ecoslot.OUT(corm.50)[[1]][1,],
      size.100 = EcoGenetics::ecoslot.OUT(corm.100)[[1]][1,]))
Lag1.def$bin <- c(row.names(EcoGenetics::ecoslot.OUT(corm)[[1]])[1],
              row.names(EcoGenetics::ecoslot.OUT(corm.50)[[1]])[1],
              row.names(EcoGenetics::ecoslot.OUT(corm.100)[[1]])[1])
Lag1.def


## ----fig.width=8, fig.height=6, fig.show='hold'------------------------------------------------------
par(mfrow=c(3,1))

hist(Dgeo, nclass=50, main="Sturge's rule", axes=F, xlab="", ylab="")
for(i in 1:length(EcoGenetics::ecoslot.BREAKS(corm))){
  lines(rep(EcoGenetics::ecoslot.BREAKS(corm)[i], 2), c(0,50), col="blue")}

hist(Dgeo, nclass=50, main = "50 pairs per lag", axes=F)
for(i in 1:length(EcoGenetics::ecoslot.BREAKS(corm.50))){
  lines(rep(EcoGenetics::ecoslot.BREAKS(corm.50)[i], 2), c(0,50), col="blue")}

hist(Dgeo, nclass=50, main = "100 pairs per lag", axes=F)
for(i in 1:length(EcoGenetics::ecoslot.BREAKS(corm.100))){
  lines(rep(EcoGenetics::ecoslot.BREAKS(corm.100)[i], 2), c(0,50), col="blue")}


## ----------------------------------------------------------------------------------------------------
#corm.GD.pop <- lapply(GD.pop, function(x) EcoGenetics::eco.cormantel(M = x, 
#                      XY = ecoslot.XY(Frogs.ecopop)[,1:2],  nsim = 199, latlon=FALSE,
#                      alternative="less"))


## ----include=FALSE-----------------------------------------------------------------------------------
corm.GD.pop <- lapply(GD.pop, function(x) EcoGenetics::eco.cormantel(M = x, 
                      XY = ecoslot.XY(Frogs.ecopop)[,1:2],  nsim = 199, latlon=FALSE,
                      alternative="less"))


## ----------------------------------------------------------------------------------------------------
t(sapply(corm.GD.pop, function(x) EcoGenetics::ecoslot.OUT(x)[[1]][1,c(2,4)]))


## ----------------------------------------------------------------------------------------------------
EcoGenetics::eco.plotCorrelog(corm.GD.pop$Nei.adegenet)


## ----fig.width=8, fig.height=6, fig.show='hold'------------------------------------------------------
nb.del <- adegenet::chooseCN(xy = ecoslot.XY(Frogs.ecopop)[,1:2], 
          result.type = "nb", plot.nb = FALSE, type = 1)
nb.gab <- adegenet::chooseCN(xy = ecoslot.XY(Frogs.ecopop)[,1:2], 
          result.type = "nb", plot.nb = FALSE, type = 2)
nb.rel <- adegenet::chooseCN(xy = ecoslot.XY(Frogs.ecopop)[,1:2], 
          result.type = "nb", plot.nb = FALSE, type = 3)
nb.mst <- adegenet::chooseCN(xy = ecoslot.XY(Frogs.ecopop)[,1:2], 
          result.type = "nb", plot.nb = FALSE, type = 4)
nb.nbd <- adegenet::chooseCN(xy = ecoslot.XY(Frogs.ecopop)[,1:2], 
          result.type = "nb", plot.nb = FALSE, type = 5, d1=100, d2=15000)
nb.4nn <- adegenet::chooseCN(xy = ecoslot.XY(Frogs.ecopop)[,1:2], 
          result.type = "nb", plot.nb = FALSE, type = 6, k = 4)

par(mfrow=c(2,3), mai=c(0.1,0.1,0.1, 0.1))
plot(nb.del, coords=ecoslot.XY(Frogs.ecopop)); title(main="Delaunay")
plot(nb.gab, coords=ecoslot.XY(Frogs.ecopop)); title(main="Gabriel")
plot(nb.rel, coords=ecoslot.XY(Frogs.ecopop)); title(main= "Rel. neighbors")
plot(nb.mst, coords=ecoslot.XY(Frogs.ecopop)); title(main= "Min spanning tree")
plot(nb.nbd, coords=ecoslot.XY(Frogs.ecopop)); title(main = "Neighbor distance")
plot(nb.4nn, coords=ecoslot.XY(Frogs.ecopop)); title(main = "4 nearest neighbors")
par(mfrow=c(1,1))


## ----------------------------------------------------------------------------------------------------
spdep::nb2mat(nb.gab)[1:5,1:5]


## ----------------------------------------------------------------------------------------------------
spdep::moran.test(ecoslot.C(Frogs.ecopop)$Ar, spdep::nb2listw(nb.gab),
                  alternative="greater")


## ----------------------------------------------------------------------------------------------------
Frogs.moran <- lapply(ecoslot.C(Frogs.ecopop), 
                       function(x) spdep::moran.test(x,
                       spdep::nb2listw(nb.gab), alternative="two.sided"))
round(data.frame(obs = sapply(Frogs.moran, function(x) as.vector(x$estimate[1])),  
p.value = sapply(Frogs.moran, function(x) x$p.value)),3)


## ----------------------------------------------------------------------------------------------------
Pulsatilla.gstudio <- gstudio::read_population(path=system.file("extdata",
                            "pulsatilla_genotypes.csv", 
                            package = "LandGenCourse"), 
                    type="column", locus.columns=c(6:19), 
                    phased=FALSE, sep=",", header=TRUE)


## ----message=FALSE, warning=TRUE, include=FALSE------------------------------------------------------
LandGenCourse::detachAllPackages()

