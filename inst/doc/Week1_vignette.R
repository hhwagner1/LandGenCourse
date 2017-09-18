## ----message=FALSE, warning=TRUE-----------------------------------------
require(adegenet)
require(gstudio)
require(LandGenCourse)
require(tibble)

## ------------------------------------------------------------------------
if(!dir.exists("./data")) dir.create("./data")

## ------------------------------------------------------------------------
data(ralu.loci)
write.csv(ralu.loci, "./data/ralu.loci.csv", quote=FALSE, row.names=FALSE)

## ----out.width = "50%"---------------------------------------------------
knitr::include_graphics(system.file("extdata", "ExcelTable.png", 
                            package = "LandGenCourse"))

## ------------------------------------------------------------------------
Frogs <- read.csv("./data/ralu.loci.csv", header=TRUE)
as.tibble(Frogs)

## ------------------------------------------------------------------------
Frogs <- data.frame(FrogID = paste(substr(Frogs$Pop, 1, 3), row.names(Frogs), sep="."), Frogs)
as.tibble(Frogs)

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
Frogs.gstudio <- read_population("./data/ralu.loci.csv", 
                   type="separated", locus.columns=c(3:10), 
                   phased=NULL, sep=",", header=TRUE)

## ------------------------------------------------------------------------
as.tibble(Frogs.gstudio)

## ------------------------------------------------------------------------
Frogs.gstudio <- data.frame(FrogID=Frogs$FrogID, Frogs.gstudio) 
as.tibble(Frogs.gstudio)

