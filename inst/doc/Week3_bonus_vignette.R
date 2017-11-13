## ----message=FALSE, warning=TRUE-----------------------------------------
#require(adegenet)
require(ade4)
require(LandGenCourse)
#require(pegas)       
#require(sp)
#require(hierfstat)  
#require(PopGenReport)
#require(GeNetIt)
require(dplyr)
#require(poppr)       
#require(mmod)
require(tibble)

## ------------------------------------------------------------------------
data(ralu.loci, package="LandGenCourse")
Frogs <- data.frame(FrogID = paste(substr(ralu.loci$Pop, 1, 3), 
                                   row.names(ralu.loci), sep="."), ralu.loci)
Frogs.genind <- adegenet::df2genind(X=Frogs[,c(4:11)], sep=":", ncode=NULL, 
                          ind.names= Frogs$FrogID, loc.names=NULL, 
                          pop=Frogs$Pop, NA.char="NA", ploidy=2, 
                          type="codom", strata=NULL, hierarchy=NULL)
Frogs.genpop <- adegenet::genind2genpop(Frogs.genind)

## ------------------------------------------------------------------------
data(ralu.site, package="GeNetIt")
coords.longlat <- sp::spTransform(ralu.site, tmaptools::get_proj4("longlat"))@coords
dimnames(coords.longlat)[[2]] <- c("Longitude", "Latitude")

## ------------------------------------------------------------------------
Frogs.sites <- data.frame(Pop=unique(ralu.loci$Pop),
                                       SiteName=unique(ralu.loci$SiteName))
Frogs.sites$n <- as.vector(table(ralu.loci$Pop))
Frogs.sites

## ------------------------------------------------------------------------
Frogs.sites <- dplyr::left_join(x=Frogs.sites,
                               y=data.frame(coords.longlat, ralu.site@coords,
                                       ralu.site@data), 
                               by = c("SiteName" = "SiteName"))
as.tibble(Frogs.sites)

## ------------------------------------------------------------------------
tmp <- dplyr::left_join(x=data.frame(Pop=Frogs.genind@pop), 
                 y=Frogs.sites, 
                 by = c("Pop" = "Pop"))
as.tibble(tmp)

## ------------------------------------------------------------------------
Frogs.genind@other$latlong <- tmp[,4:5]
Frogs.genind@other$xy <- tmp[,6:7]
Frogs.genind@other$site <- tmp[,-c(3:7)]

## ------------------------------------------------------------------------
#require(here)
#if(!dir.exists(paste0(here(),"/output"))) dir.create(paste0(here(),"/output"))
#save(Frogs.genind, file = paste0(here(),"/output/Frogs.genind.RData"))
#load(paste0(here(),"/output/Frogs.genind.RData"))

## ------------------------------------------------------------------------
# Define strata
adegenet::strata(Frogs.genind) <- with(Frogs.genind@other$site, data.frame(Drainage, Basin, SiteName, Pop))
# Define hierarchy
adegenet::hier(Frogs.genind) <- ~ Drainage/Basin/Pop

## ------------------------------------------------------------------------
Frogs.genind

## ------------------------------------------------------------------------
Fest <- lapply(adegenet::seppop(Frogs.genind), 
               function(ls) adegenet::inbreeding(ls, N=200, res.type="estimate"))

## ------------------------------------------------------------------------
temp <- lapply(adegenet::seppop(Frogs.genind), 
               function(ls) adegenet::inbreeding(ls, N=200, res.type="sample"))
Fbar <- lapply(temp, function (ls) sapply(ls, mean))

## ---- fig.show='hold'----------------------------------------------------
par(mar=c(5.5, 4.5, 1, 1))
boxplot(Fest, las=3, ylim=c(0,1), xlab="", ylab="Inbreeding coefficient (Fest)")
boxplot(Fbar, las=3, ylim=c(0,1), xlab="", ylab="Inbreeding coefficient (Fbar)")
Mean.inbreeding.per.pop <- sapply(Fbar, mean)

## ------------------------------------------------------------------------
# Individual-level genetic distances
GD.ind.smouse <- PopGenReport::gd.smouse(Frogs.genind, verbose=FALSE)  # GenAlEx
GD.ind.propShared <- adegenet::propShared(Frogs.genind)     

## ------------------------------------------------------------------------
# Population-level genetic distances
GD.pop.propShared <- PopGenReport::pairwise.propShared(Frogs.genind)
GD.pop.Nei <- adegenet::dist.genpop(Frogs.genpop, method=1)   
GD.pop.Edwards <- adegenet::dist.genpop(Frogs.genpop, method=2)
GD.pop.Reynolds <- adegenet::dist.genpop(Frogs.genpop, method=3)  # Co-ancestry coef
GD.pop.Rogers <- adegenet::dist.genpop(Frogs.genpop, method=4)  
GD.pop.Provesti <- adegenet::dist.genpop(Frogs.genpop, method=5)

GD.pop.Joost <- mmod::pairwise_D(Frogs.genind, linearized = FALSE)
GD.pop.Hedrick <- mmod::pairwise_Gst_Hedrick(Frogs.genind, linearized = FALSE)
GD.pop.NeiGst <- mmod::pairwise_Gst_Nei(Frogs.genind, linearized = FALSE)

## ------------------------------------------------------------------------
Pairwise.fst <- hierfstat::pairwise.fst(Frogs.genind, pop = NULL, 
                                        res.type = c("dist"))

## ------------------------------------------------------------------------
GD.ind <- list(smouse.PopGenReport = GD.ind.smouse,
               propShared.adegent = 1 - GD.ind.propShared)

GD.pop <- list(pairwiseFst.hierfstat = Pairwise.fst,
               propShared.PopGenReport = 1 - GD.pop.propShared,
               Nei.adegenet = GD.pop.Nei,
               Edwards.adegenet = GD.pop.Edwards,
               Reynolds.adegent = GD.pop.Reynolds,
               Rogers.adegenet = GD.pop.Rogers,
               Provesti.adegent = GD.pop.Provesti,
               Joost.mmod = GD.pop.Joost,
               Hedrick.mmod = GD.pop.Hedrick,
               Nei.mmod = GD.pop.NeiGst)

## ------------------------------------------------------------------------
#require(here)
#if(!dir.exists(paste0(here(),"/output"))) dir.create(paste0(here(),"/output"))

#save(GD.ind, file = paste0(here(),"/output/GD.ind.RData"))
#load(paste0(here(),"/output/GD.ind.RData"))

#save(GD.pop, file = paste0(here(),"/output/GD.pop.RData"))
#load(paste0(here(),"/output/GD.pop.RData"))

## ------------------------------------------------------------------------
coords.pop <- Frogs.genind@other$xy %>%
  group_by(Frogs.genind@other$site$Pop) %>%
  summarize(x = mean(coords.x1, na.rm = TRUE),
            y = mean(coords.x2, na.rm = TRUE))

## ------------------------------------------------------------------------
Dgeo <- dist(coords.pop[,-1])


## ------------------------------------------------------------------------
Result <- matrix(NA, length(GD.pop), 2, 
                 dimnames=list(names(GD.pop), c("Mantel.r", "p-value")))
for(i in 1:length(GD.pop))
{
  IBD <- ade4::mantel.randtest(as.dist(GD.pop[[i]]),Dgeo)
  Result[i,] <- c(IBD$obs, IBD$pvalue)
}
Result

## ------------------------------------------------------------------------

# Overall F statistics (hierfstat)
hierfstat::fstat(Frogs.genind, pop = NULL, fstonly = FALSE)

## ------------------------------------------------------------------------
# F statistics by locus (pegas)
Frogs.pegas <- pegas::genind2loci(Frogs.genind)
pegas::Fst(Frogs.pegas)

# NOTE: VERY DIFFERENT RESULTS WHEN USING MEAN OF PEGAS, SHOULD BE THE SAME)
# NEED TO CHECK ORDER?
apply(pegas::Fst(Frogs.pegas), 2, mean)

## ------------------------------------------------------------------------
# Fst analogues (mmod)
mmod::diff_stats(Frogs.genind)

## ------------------------------------------------------------------------
#amova.result.pegas <- poppr::poppr.amova(Frogs.genind, hier = ~ Drainage/Basin/Pop, 
#            clonecorrect = FALSE, within = FALSE,
#  dist = NULL, squared = FALSE, correction = "quasieuclid", sep = "_",
#  filter = FALSE, threshold = 0, algorithm = "farthest_neighbor",
#  missing = "loci", cutoff = 0.5, quiet = FALSE, method = c(
#  "pegas"), nperm = 200)

amova.result.ade4 <- poppr::poppr.amova(Frogs.genind, hier = Frogs.genind@hierarchy, 
            clonecorrect = FALSE, within = TRUE,
  dist = NULL, squared = FALSE, correction = "quasieuclid", sep = "_",
  filter = FALSE, threshold = 0, algorithm = "farthest_neighbor",
  missing = "loci", cutoff = 0.5, quiet = FALSE, method = c(
  "ade4"), nperm = 0)

amova.result.ade4

## ------------------------------------------------------------------------
amova.test <- ade4::randtest(amova.result.ade4, nrepet=199) 
amova.test

## ----message=FALSE, warning=TRUE, include=FALSE--------------------------
detach("package:ade4", unload=TRUE)
detach("package:dplyr", unload=TRUE)

