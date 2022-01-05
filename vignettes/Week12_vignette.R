## ----packages global_options, include=TRUE, results="hide", message=FALSE, warning=FALSE----
library(LandGenCourse)
library(ggplot2)
#require(lme4)
#require(usdm)

## -----------------------------------------------------------------------------
CSFdata <- read.csv(system.file("extdata", "CSF_network.csv", 
                                        package = "LandGenCourse"))
head(CSFdata) 

## -----------------------------------------------------------------------------
str(CSFdata)

## -----------------------------------------------------------------------------
Zl <- lapply(c("pop1","pop2"), function(nm) 
  Matrix:::fac2sparse(CSFdata[[nm]], "d", drop=FALSE))
ZZ <- Reduce("+", Zl[-1], Zl[[1]])

