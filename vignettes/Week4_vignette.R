## -----------------------------------------------------------------------------
if(!requireNamespace("EcoGenetics", quietly = TRUE)) 
  remotes::install_github("leandroroser/EcoGenetics-devel")

## ----message=FALSE, warning=TRUE----------------------------------------------
library(LandGenCourse)
library(EcoGenetics)
library(methods)
library(ggplot2)
#require(tibble)
#require(poppr)
#require(ade4)
#require(pwr)
#require(effsize)
#require(sp)
#require(ggmap)
#require(car)  

## -----------------------------------------------------------------------------
data(dd.ecogen, package = "LandGenCourse")
dd.ecogen
?dd.ecogen   

## -----------------------------------------------------------------------------
data(dd.site, package = "LandGenCourse")
tibble::as_tibble(dd.site)
?dd.site

## -----------------------------------------------------------------------------
dd.ecogen.Cluster <- dd.ecogen[!is.na(dd.ecogen[["S"]]$Cluster),]
dd.genind.Cluster <- EcoGenetics::ecogen2genind(dd.ecogen.Cluster)

