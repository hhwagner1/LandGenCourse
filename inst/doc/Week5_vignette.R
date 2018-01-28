## ----message=FALSE, warning=TRUE-----------------------------------------
#require(adegenet)
#require(ade4)
require(LandGenCourse)
#require(tibble)
require(EcoGenetics)
#require(gstudio)
#require(hierfstat)
#require(PopGenReport)
#require(mmod)
#require(spdep)
#require(SoDA)

## ------------------------------------------------------------------------
data(dd.site)
dd.spatial <- dd.site[dd.site$Spatial==TRUE,]
#Snails.site.df <- as.data.frame(dd.spatial)

## ------------------------------------------------------------------------
xy <- SoDA::geoXY(dd.spatial@coords[,'Latitude'], 
                  dd.spatial@coords[,'Longitude'])
dd.spatial@data <- cbind(xy, dd.spatial@data)
tibble::as.tibble(dd.spatial@data)

## ------------------------------------------------------------------------
data(dd.ecogen, package="LandGenCourse")
dd.ecogen

## ------------------------------------------------------------------------
dd.ecogen <- EcoGenetics::eco.fill_ecogen_with_df(dd.ecogen,
             pop="SiteID", pop_levels=dd.site@data$SiteID, 
             XY = dd.site@coords, E = dd.site@data)

## ------------------------------------------------------------------------
Snails.ecogen <- dd.ecogen[dd.ecogen[["E"]]$Spatial == TRUE,]
Snails.ecogen

## ------------------------------------------------------------------------
#Snails.ecopop <- EcoGenetics::ecogen2ecopop(Snails.ecogen, hier="SITE")

Snails.genind <- EcoGenetics::ecogen2genind(Snails.ecogen)
Snails.genind@pop <- Snails.ecogen@S$SITE

## ------------------------------------------------------------------------
Snails.genpop <- adegenet::genind2genpop(Snails.genind)
Snails.genpop

## ------------------------------------------------------------------------
Snails.gstudio <- EcoGenetics::ecogen2gstudio(Snails.ecogen)
tibble::as.tibble(Snails.gstudio)

## ------------------------------------------------------------------------
GD.pop.PairwiseFst.hierfstat <- hierfstat::pairwise.fst(Snails.genind, 
                                pop = NULL, res.type = c("dist"))

## ------------------------------------------------------------------------
GD.pop.propShared <- PopGenReport::pairwise.propShared(Snails.genind)

## ------------------------------------------------------------------------
GD.pop.Nei <- adegenet::dist.genpop(Snails.genpop, method=1)   
GD.pop.Edwards <- adegenet::dist.genpop(Snails.genpop, method=2)
GD.pop.Reynolds <- adegenet::dist.genpop(Snails.genpop, method=3)  
GD.pop.Rogers <- adegenet::dist.genpop(Snails.genpop, method=4)  
GD.pop.Provesti <- adegenet::dist.genpop(Snails.genpop, method=5)

## ------------------------------------------------------------------------
GD.pop.Joost <- mmod::pairwise_D(Snails.genind, linearized = FALSE)
GD.pop.Hedrick <- mmod::pairwise_Gst_Hedrick(Snails.genind, linearized = FALSE)
GD.pop.NeiGst <- mmod::pairwise_Gst_Nei(Snails.genind, linearized = FALSE)

## ------------------------------------------------------------------------
GD.pop.Euclidean.gstudio <-gstudio::genetic_distance(Snails.gstudio, mode = "Euclidean", stratum="SITE")
GD.pop.cGD.gstudio <-gstudio::genetic_distance(Snails.gstudio, mode = "cGD", stratum="SITE")
GD.pop.Nei.gstudio <-gstudio::genetic_distance(Snails.gstudio, mode = "Nei", stratum="SITE")
GD.pop.Dps.gstudio <-gstudio::genetic_distance(Snails.gstudio, mode = "Dps", stratum="SITE")
GD.pop.Jaccard.gstudio <-gstudio::genetic_distance(Snails.gstudio, mode = "Jaccard", stratum="SITE")

## ------------------------------------------------------------------------
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

## ------------------------------------------------------------------------
#require(here)
#if(!dir.exists(paste0(here(),"/output"))) dir.create(paste0(here(),"/output"))

#save(GD.pop, file = paste0(here(),"/output/GD.pop.RData"))
#load(paste0(here(),"/output/GD.pop.RData"))

## ------------------------------------------------------------------------
Dgeo <- dist(dd.spatial@data[,c("X", "Y")])

## ------------------------------------------------------------------------
par(mar=c(4,4,0,0))
Dgen <- GD.pop$propShared.PopGenReport
dens <- MASS::kde2d(Dgeo, Dgen, n=300)
myPal <- colorRampPalette(c("white","blue","gold","orange","red"))
plot(Dgeo, Dgen, pch=20, cex=0.5,  
    xlab="Geographic Distance", ylab="Genetic Distance")
image(dens, col=transp(myPal(300), 0.7), add=TRUE)
abline(lm(Dgen ~ Dgeo))
lines(loess.smooth(Dgeo, Dgen), col="red")

## ------------------------------------------------------------------------
IBD <- ade4::mantel.randtest(Dgen,Dgeo)
IBD
attributes(IBD)

## ------------------------------------------------------------------------
Mantel.test <- lapply(GD.pop, function(x) ade4::mantel.randtest(x,Dgeo))
data.frame(Mantel.r = sapply(Mantel.test, function(x) x$obs),
           p.value = sapply(Mantel.test, function(x) x$pvalue))

## ------------------------------------------------------------------------
corm <- EcoGenetics::eco.cormantel(M = GD.pop$propShared.PopGenReport, 
        XY = dd.spatial@coords,  nsim = 199, latlon=TRUE, alternative="less")
corm

## ------------------------------------------------------------------------
EcoGenetics::eco.plotCorrelog(corm)

## ----fig.show='hold'-----------------------------------------------------
corm.20 <- EcoGenetics::eco.cormantel(M = GD.pop$propShared.PopGenReport, 
           XY = dd.spatial@coords,  nsim = 199, latlon=TRUE,
           alternative="less", size=20)
EcoGenetics::ecoslot.OUT(corm.20)

corm.50 <- EcoGenetics::eco.cormantel(M = GD.pop$propShared.PopGenReport, 
           XY = dd.spatial@coords,  nsim = 199, latlon=TRUE,
           alternative="less", size=50)
EcoGenetics::ecoslot.OUT(corm.50)

EcoGenetics::eco.plotCorrelog(corm.20)
EcoGenetics::eco.plotCorrelog(corm.50)

## ------------------------------------------------------------------------
Lag1.def <- data.frame(rbind(Sturge = EcoGenetics::ecoslot.OUT(corm)[[1]][1,],
      size.20 = EcoGenetics::ecoslot.OUT(corm.20)[[1]][1,],
      size.50 = EcoGenetics::ecoslot.OUT(corm.50)[[1]][1,]))
Lag1.def$bin <- c(row.names(EcoGenetics::ecoslot.OUT(corm)[[1]])[1],
              row.names(EcoGenetics::ecoslot.OUT(corm.20)[[1]])[1],
              row.names(EcoGenetics::ecoslot.OUT(corm.50)[[1]])[1])
Lag1.def

## ----fig.width=8, fig.height=6, fig.show='hold'--------------------------
par(mfrow=c(3,1))

hist(Dgeo, nclass=30, main="Sturge's rule", axes=F, xlab="", ylab="")
for(i in 1:length(EcoGenetics::ecoslot.BREAKS(corm))){
  lines(rep(EcoGenetics::ecoslot.BREAKS(corm)[i], 2), c(0,50), col="blue")}

hist(Dgeo, nclass=30, main = "20 pairs per lag", axes=F)
for(i in 1:length(EcoGenetics::ecoslot.BREAKS(corm.20))){
  lines(rep(EcoGenetics::ecoslot.BREAKS(corm.20)[i], 2), c(0,50), col="blue")}

hist(Dgeo, nclass=30, main = "50 pairs per lag", axes=F)
for(i in 1:length(EcoGenetics::ecoslot.BREAKS(corm.50))){
  lines(rep(EcoGenetics::ecoslot.BREAKS(corm.50)[i], 2), c(0,50), col="blue")}

## ------------------------------------------------------------------------
#corm.GD.pop <- lapply(GD.pop, function(x) EcoGenetics::eco.cormantel(M = x, 
#                      XY = dd.spatial@coords,  nsim = 199, latlon=TRUE,
#                      alternative="less"))

## ----include=FALSE-------------------------------------------------------
corm.GD.pop <- lapply(GD.pop, function(x) EcoGenetics::eco.cormantel(M = x, 
                      XY = dd.spatial@coords,  nsim = 199, latlon=TRUE,
                      alternative="less"))

## ------------------------------------------------------------------------
t(sapply(corm.GD.pop, function(x) EcoGenetics::ecoslot.OUT(x)[[1]][1,c(2,4)]))

## ------------------------------------------------------------------------
EcoGenetics::eco.plotCorrelog(corm.GD.pop$Nei.adegenet)

## ----fig.width=8, fig.height=6, fig.show='hold'--------------------------
nb.del <- adegenet::chooseCN(xy = dd.spatial@data[,1:2], 
          result.type = "nb", plot.nb = FALSE, type = 1)
nb.gab <- adegenet::chooseCN(xy = dd.spatial@data[,1:2], 
          result.type = "nb", plot.nb = FALSE, type = 2)
nb.rel <- adegenet::chooseCN(xy = dd.spatial@data[,1:2], 
          result.type = "nb", plot.nb = FALSE, type = 3)
nb.mst <- adegenet::chooseCN(xy = dd.spatial@data[,1:2], 
          result.type = "nb", plot.nb = FALSE, type = 4)
nb.nbd <- adegenet::chooseCN(xy = dd.spatial@data[,1:2], 
          result.type = "nb", plot.nb = FALSE, type = 5, d1=100, d2=15000)
nb.4nn <- adegenet::chooseCN(xy = dd.spatial@data[,1:2], 
          result.type = "nb", plot.nb = FALSE, type = 6, k = 4)

par(mfrow=c(2,3), mai=c(0.1,0.1,0.1, 0.1))
plot(nb.del, coords=dd.spatial@data[,1:2]); title(main="Delaunay")
plot(nb.gab, coords=dd.spatial@data[,1:2]); title(main="Gabriel")
plot(nb.rel, coords=dd.spatial@data[,1:2]); title(main= "Rel. neighbors")
plot(nb.mst, coords=dd.spatial@data[,1:2]); title(main= "Min spanning tree")
plot(nb.nbd, coords=dd.spatial@data[,1:2]); title(main = "Neighbor distance")
plot(nb.4nn, coords=dd.spatial@data[,1:2]); title(main = "4 nearest neighbors")
par(mfrow=c(1,1))

## ------------------------------------------------------------------------
spdep::nb2mat(nb.gab)[1:5,1:5]

## ------------------------------------------------------------------------
spdep::moran.test(dd.spatial$RA, spdep::nb2listw(nb.gab),
                  alternative="greater")

## ------------------------------------------------------------------------
Snails.moran <- lapply(dd.spatial@data[,c(11:14, 16:21, 23)], 
                       function(x) spdep::moran.test(x,
                       spdep::nb2listw(nb.gab), alternative="two.sided"))
round(data.frame(obs = sapply(Snails.moran, function(x) as.vector(x$estimate[1])),  
p.value = sapply(Snails.moran, function(x) x$p.value)),3)

## ----message=FALSE, warning=TRUE, include=FALSE--------------------------
LandGenCourse::detachAllPackages()

