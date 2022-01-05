## ----message=FALSE, warning=TRUE----------------------------------------------
#require(adegenet)
require(LandGenCourse)
#require(pegas)       
#require(sp)
#require(PopGenReport)
require(dplyr)
require(poppr) 

## -----------------------------------------------------------------------------
data(ralu.loci, package="LandGenCourse")
Frogs <- data.frame(FrogID = paste(substr(ralu.loci$Pop, 1, 3), 
                                   row.names(ralu.loci), sep="."), ralu.loci)
Frogs.genind <- adegenet::df2genind(X=Frogs[,c(4:11)], sep=":", ncode=NULL, 
                          ind.names= Frogs$FrogID, loc.names=NULL, 
                          pop=Frogs$Pop, NA.char="NA", ploidy=2, 
                          type="codom", strata=NULL, hierarchy=NULL)
Frogs.genind

## -----------------------------------------------------------------------------
summary(Frogs.genind)

