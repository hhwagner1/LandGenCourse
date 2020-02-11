## ----message=FALSE, warning=TRUE----------------------------------------------
library(adegenet)
library(gstudio)
library(LandGenCourse)
library(tibble)
library(here)

## -----------------------------------------------------------------------------
data(ralu.loci)

## -----------------------------------------------------------------------------
if(!dir.exists(paste0(here(),"/downloads"))) dir.create(paste0(here(),"/downloads"))
file.copy(system.file("extdata", "ralu.loci.csv", package = "LandGenCourse"),
          paste0(here(), "/downloads/ralu.loci.csv"), overwrite=FALSE)

## ----out.width = "50%"--------------------------------------------------------
knitr::include_graphics(system.file("extdata", "ExcelTable.png", 
                            package = "LandGenCourse"))

## -----------------------------------------------------------------------------
Frogs <- read.csv(paste0(here(), "/downloads/ralu.loci.csv"), header=TRUE)
as_tibble(Frogs)

## -----------------------------------------------------------------------------
Frogs <- data.frame(FrogID = paste(substr(Frogs$Pop, 1, 3), row.names(Frogs), sep="."), Frogs)
as_tibble(Frogs)

## -----------------------------------------------------------------------------
#here() 
#paste0(here(),"/output")

## -----------------------------------------------------------------------------
if(!dir.exists(paste0(here(),"/output"))) dir.create(paste0(here(),"/output"))

## -----------------------------------------------------------------------------
write.csv(ralu.loci, paste0(here(),"/output/ralu.loci.csv"), 
          quote=FALSE, row.names=FALSE)

## -----------------------------------------------------------------------------
ralu.loci.2 <- read.csv(paste0(here(),"/output/ralu.loci.csv"), header=TRUE)

## -----------------------------------------------------------------------------
?df2genind

## -----------------------------------------------------------------------------
Frogs.genind <- df2genind(X=Frogs[,c(4:11)], sep=":", ncode=NULL, ind.names= Frogs$FrogID, loc.names=NULL, pop=Frogs$Pop, NA.char="NA", ploidy=2, type="codom", strata=NULL, hierarchy=NULL)

## -----------------------------------------------------------------------------
Frogs.genind

## -----------------------------------------------------------------------------
summary(Frogs.genind)

## -----------------------------------------------------------------------------
as.tibble(Frogs.genind@tab)

## -----------------------------------------------------------------------------
Frogs.genind@loc.n.all

## -----------------------------------------------------------------------------
Frogs.genind@loc.fac

## -----------------------------------------------------------------------------
Frogs.genind@all.names

## -----------------------------------------------------------------------------
?read_population

## -----------------------------------------------------------------------------
Frogs.gstudio <- read_population(path=system.file("extdata", "ralu.loci.csv", 
                                                  package = "LandGenCourse"), 
                   type="separated", locus.columns=c(3:10), 
                   phased=FALSE, sep=",", header=TRUE)

## -----------------------------------------------------------------------------
str(Frogs.gstudio)

## -----------------------------------------------------------------------------
Frogs.gstudio <- data.frame(FrogID=Frogs$FrogID, Frogs.gstudio) 
head(Frogs.gstudio)

## -----------------------------------------------------------------------------
Frogs.genind2 <- adegenet::df2genind(X=Frogs.gstudio[,c(4:11)], sep=":", ncode=NULL,   
                          ind.names=Frogs.gstudio$FrogID, loc.names=NULL, 
                          pop=Frogs.gstudio$Pop, NA.char="", ploidy=2, 
                          type="codom", strata=NULL, hierarchy=NULL)
Frogs.genind2

## -----------------------------------------------------------------------------
file.copy(system.file("extdata", "pulsatilla_genotypes.csv", package = "LandGenCourse"),
          paste0(here(), "/downloads/pulsatilla_genotypes.csv"), overwrite=FALSE)

## ----message=FALSE, warning=TRUE, include=FALSE-------------------------------
# Note: this line of code detaches all packages that were loaded. This is necessary only for technical reasons of building the R package.
LandGenCourse::detachAllPackages()

