## ----message=FALSE, warning=TRUE-----------------------------------------
require(adegenet)
require(gstudio)
require(LandGenCourse)
require(tibble)
#require(here)

## ------------------------------------------------------------------------
data(ralu.loci)

## ----out.width = "50%"---------------------------------------------------
knitr::include_graphics(system.file("extdata", "ExcelTable.png", 
                            package = "LandGenCourse"))

## ------------------------------------------------------------------------
Frogs <- read.csv(system.file("extdata", "ralu.loci.csv", 
                            package = "LandGenCourse"), header=TRUE)
as.tibble(Frogs)

## ------------------------------------------------------------------------
Frogs <- data.frame(FrogID = paste(substr(Frogs$Pop, 1, 3), row.names(Frogs), sep="."), Frogs)
as.tibble(Frogs)

## ------------------------------------------------------------------------
#require(here)
#if(!dir.exists(paste0(here(),"/output"))) dir.create(paste0(here(),"/output"))
#write.csv(ralu.loci, paste0(here(),"/output/ralu.loci.csv"), 
#          quote=FALSE, row.names=FALSE)

## ------------------------------------------------------------------------
?df2genind

## ------------------------------------------------------------------------
Frogs.genind <- df2genind(X=Frogs[,c(4:11)], sep=":", ncode=NULL, ind.names= Frogs$FrogID, loc.names=NULL, pop=Frogs$Pop, NA.char="NA", ploidy=2, type="codom", strata=NULL, hierarchy=NULL)

## ------------------------------------------------------------------------
Frogs.genind

## ------------------------------------------------------------------------
summary(Frogs.genind)

## ------------------------------------------------------------------------
as.tibble(Frogs.genind@tab)

## ------------------------------------------------------------------------
Frogs.genind@loc.n.all

## ------------------------------------------------------------------------
Frogs.genind@loc.fac

## ------------------------------------------------------------------------
Frogs.genind@all.names

## ------------------------------------------------------------------------
?read_population

## ------------------------------------------------------------------------
Frogs.gstudio <- read_population(path=system.file("extdata", "ralu.loci.csv", 
                                                  package = "LandGenCourse"), 
                   type="separated", locus.columns=c(3:10), 
                   phased=NULL, sep=",", header=TRUE)

## ------------------------------------------------------------------------
str(Frogs.gstudio)

## ------------------------------------------------------------------------
Frogs.gstudio <- data.frame(FrogID=Frogs$FrogID, Frogs.gstudio) 
str(Frogs.gstudio)

## ----message=FALSE, warning=TRUE, include=FALSE--------------------------
detach("package:adegenet", unload=TRUE)
detach("package:ade4", unload=TRUE)
detach("package:gstudio", unload=TRUE)

