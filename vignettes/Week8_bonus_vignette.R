## ----library, message=FALSE, warning=TRUE-------------------------------------
library(LandGenCourse)
#library(microbenchmark)
#library(profvis)
#library(here)
#library(readr)
#library(data.table)
#library(feather)
#library(rio)
#library(devtools)
#library(parallel)
#library(doParallel)
#library(knitr)
#library(compiler)

## ----home---------------------------------------------------------------------
getwd()
here::here()
Sys.getenv("HOME")
R.home()

## ----gen----------------------------------------------------------------------
gen <- read.csv(system.file("extdata", "wolf_geno_samp_10000.csv", 
                            package = "LandGenCourse"), row.names=1)
dim(gen)

## ----R.home-------------------------------------------------------------------
R.home()
system.file()

## ----system.file--------------------------------------------------------------
system.file(package = "LandGenCourse")
system.file("extdata", "wolf_geno_samp_10000.csv", 
                            package = "LandGenCourse")

## ----file.size----------------------------------------------------------------
myFile <- system.file("extdata", "wolf_geno_samp_10000.csv", 
                            package = "LandGenCourse")
file.size(myFile)
cat("File size: ", file.size(myFile) / 10^6, " MB")

## ----dir----------------------------------------------------------------------
dir()
dir(here::here())
dir(system.file(package = "LandGenCourse"))
dir(system.file("extdata", package = "LandGenCourse"))
dir(myFile)

