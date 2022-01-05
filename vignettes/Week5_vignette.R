## -----------------------------------------------------------------------------
if(!requireNamespace("EcoGenetics", quietly = TRUE)) 
  remotes::install_github("leandroroser/EcoGenetics-devel")
if(!requireNamespace("GeNetIt", quietly = TRUE)) remotes::install_github("jeffreyevans/GeNetIt")
if(!requireNamespace("popgraph", quietly = TRUE)) remotes::install_github("dyerlab/popgraph")
if(!requireNamespace("gstudio", quietly = TRUE)) remotes::install_github("dyerlab/gstudio")

## ----message=FALSE, warning=TRUE----------------------------------------------

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

## -----------------------------------------------------------------------------
Frogs.coords <- read.csv(system.file("extdata", "ralu_coords_allpops.csv", 
                                     package = "LandGenCourse"))

## -----------------------------------------------------------------------------
Frogs.diversity <- read.csv(system.file("extdata", "Frogs_diversity_allpops.csv", 
                                     package = "LandGenCourse"))

## -----------------------------------------------------------------------------
Frogs.loci <- read.csv(system.file("extdata", "ralu_loci_allpops.csv", 
                                     package = "LandGenCourse"))

## -----------------------------------------------------------------------------
Frogs.ecogen <- ecogen(G = Frogs.loci[,-c(1:2)], ploidy = 2, 
                       type = "codominant", sep = ":",
                       S = data.frame(Frogs.loci[,1:2]))
Frogs.ecogen

## -----------------------------------------------------------------------------
Frogs.ecopop <- ecogen2ecopop(Frogs.ecogen, hier = "SiteName")
Frogs.ecopop

## -----------------------------------------------------------------------------
Subset <- ecoslot.S(Frogs.ecogen) %>% group_by(SiteName, Pop) %>% summarize()
Subset <- left_join(Subset, Frogs.diversity)

## -----------------------------------------------------------------------------
Subset <- left_join(Subset, as.data.frame(Frogs.coords))

## -----------------------------------------------------------------------------
names(Subset)
Frogs.ecopop <- EcoGenetics::eco.fill_ecogen_with_df(Frogs.ecopop,
             pop ="pop", pop_levels = Subset$SiteName, 
             C = Subset[,3:6],
             XY = Subset[,7:10])
Frogs.ecopop

## -----------------------------------------------------------------------------
Frogs.genind <- EcoGenetics::ecogen2genind(Frogs.ecogen)
Frogs.genind@pop <- ecoslot.S(Frogs.ecogen)$SiteName

## -----------------------------------------------------------------------------
Frogs.genpop <- adegenet::genind2genpop(Frogs.genind)
Frogs.genpop

## -----------------------------------------------------------------------------
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

