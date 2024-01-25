## ----library, message=FALSE, warning=TRUE------------------------------------------------------------
library(LandGenCourse)
#library(microbenchmark)
#library(profvis)
#library(here)
#library(readr)
#library(data.table)
library(feather)
#library(rio)
#library(devtools)
#library(parallel)
#library(doParallel)
#library(knitr)
#library(compiler)


## ----home--------------------------------------------------------------------------------------------
getwd()
here::here()
Sys.getenv("HOME")
R.home()


## ----gen---------------------------------------------------------------------------------------------
gen <- read.csv(system.file("extdata", "wolf_geno_samp_10000.csv", 
                            package = "LandGenCourse"), row.names=1)
dim(gen)


## ----R.home------------------------------------------------------------------------------------------
R.home()
system.file()


## ----system.file-------------------------------------------------------------------------------------
system.file(package = "LandGenCourse")
system.file("extdata", "wolf_geno_samp_10000.csv", 
                            package = "LandGenCourse")


## ----file.size---------------------------------------------------------------------------------------
myFile <- system.file("extdata", "wolf_geno_samp_10000.csv", 
                            package = "LandGenCourse")
file.size(myFile)
cat("File size: ", file.size(myFile) / 10^6, " MB")


## ----dir---------------------------------------------------------------------------------------------
dir()
dir(here::here())
dir(system.file(package = "LandGenCourse"))
dir(system.file("extdata", package = "LandGenCourse"))
dir(myFile)


## ----microbenchmark1, message=FALSE------------------------------------------------------------------
x = myFile
microbenchmark::microbenchmark(times = 1, unit = "ms", 
          read.csv(x), readr::read_csv(x, show_col_types = FALSE), data.table::fread(x),
          rio::import(x))


## ----microbenchmark2, message=FALSE------------------------------------------------------------------
library(readr); library(data.table); library(rio); library(microbenchmark)

microbenchmark(times = 1, unit = "ms", 
          read.csv(x), read_csv(x, show_col_types = FALSE), fread(x), import(x))


## ----class, message=FALSE----------------------------------------------------------------------------
gen <- read.csv(x); c(class(gen[[1]]), class(gen))
gen <- read_csv(x, show_col_types = FALSE); c(class(gen[[1]]), class(gen))
gen <- fread(x); c(class(gen[[1]]), class(gen))
gen <- import(x); c(class(gen[[1]]), class(gen))


## ----------------------------------------------------------------------------------------------------
if(!dir.exists(paste0(here::here(),"/output"))) dir.create(paste0(here::here(),"/output"))


## ----binary------------------------------------------------------------------------------------------
gen <- import(myFile)

export(gen, file.path(here::here(), "output", "gen.csv"))
save(gen, file=file.path(here::here(), "output", "gen.RData"))
saveRDS(gen, file=file.path(here::here(), "output", "gen.rds"))
export(gen, file=file.path(here::here(), "output", "gen.feather"))

c(csv=file.size(file.path(here::here(), "output", "gen.csv")),
  RData=file.size(file.path(here::here(), "output", "gen.RData")),
  rds=file.size(file.path(here::here(), "output", "gen.rds")),
  feather=file.size(file.path(here::here(), "output", "gen.feather")))/10^6
  


## ----microbenchmark3---------------------------------------------------------------------------------
microbenchmark(times = 10, unit = "ms", 
          csv= import(file.path(here::here(), "output", "gen.csv")),
          RData=import(file.path(here::here(), "output", "gen.RData")),
          rds=import(file.path(here::here(), "output", "gen.rds")),
          feather=import(file.path(here::here(), "output", "gen.feather")))


## ----load1-------------------------------------------------------------------------------------------
# Let's delete any copy of 'gen' from the workspace:
rm(gen)

# Create object 'myData' in a single step from 'rds' file:
myData <- readRDS(file.path(here::here(), "output", "gen.rds"))

# Two steps when importing 'RData': first, load the stored object:
load(file.path(here::here(), "output", "gen.RData")) 
# then assign to the new object 'myData':
myData <- gen


## ----load2-------------------------------------------------------------------------------------------
# Export 'gen' to a file with a different name 'gen2.RData':
save(gen, file=file.path(here::here(), "output", "gen2.RData"))
rm(gen)

# Load:
load(file.path(here::here(), "output", "gen2.RData")) 

# What is the name of the loaded object?
exists("gen")
exists("gen2")


## ----cmpfun------------------------------------------------------------------------------------------
myFunction <- function() {
    sum(rnorm(1000))/1000
}
myFunction.cmp <- compiler::cmpfun(myFunction)

microbenchmark::microbenchmark(myFunction(), myFunction.cmp())


## ----myChunkName-------------------------------------------------------------------------------------



## ----samplecode, include=FALSE-----------------------------------------------------------------------
dat <- data.frame(
     x = rnorm(5e5), 
     y = rnorm(5e5))   
mean(dat$x)
with(dat, cor(x, y))
lm(y ~ x, data=dat)


## ----profvis-----------------------------------------------------------------------------------------
profvis::profvis({
  dat <- data.frame(
       x = rnorm(5e5), 
       y = rnorm(5e5))   
  mean(dat$x)
  with(dat, cor(x, y))
  lm(y ~ x, data=dat)
})


## ----notebook----------------------------------------------------------------------------------------
file.copy(from=system.file("doc", "Week3_vignette.Rmd", package = "LandGenCourse"),
                     to=file.path(here::here(), "downloads", "Week3_vignette.Rmd"))


## ----purl--------------------------------------------------------------------------------------------
infile = here::here("downloads/Week3_vignette.Rmd")
tmpfile = here::here("downloads/tmp.Rmd")
outfile = here::here("downloads/Week3_vignette.R")

readr::write_lines(readr::read_lines(infile, n_max=252), tmpfile)
knitr::purl(tmpfile, outfile)


## ----file.show---------------------------------------------------------------------------------------
file.show(infile)
file.show(outfile)

#file.edit(infile)
#file.edit(outfile)


## ----Rprof, include=FALSE----------------------------------------------------------------------------
Rprof(filename=here::here("downloads/Rprof.out"))
source(outfile)
Rprof(NULL)


## ----sampling.interval-------------------------------------------------------------------------------
summaryRprof(here::here("downloads/Rprof.out"))[c("sampling.time", "sample.interval")]	


## ----summaryRprof------------------------------------------------------------------------------------
summaryRprof(here::here("downloads/Rprof.out"))$by.total


## ----BashScript--------------------------------------------------------------------------------------
myPath <- file.path(here::here(), "output/myBashScript.sh")
fileConn <- file(myPath)
writeLines(c("#!/bin/bash",
             "R --slave << EOF",
             "x <- rnorm(100)",
             "mean(x)",
             "png('my_plot.png', height = 400, width = 800)",
             "par(mfrow=c(1,2))", 
             "hist(x)", 
             "qqnorm(x)",
             "dev.off()",
             "EOF"), fileConn)
close(fileConn)
file.show(myPath)


## ----my_plot.png-------------------------------------------------------------------------------------
myPNG <- file.path(here::here(), "output/my_plot.png")
if(file.exists(myPNG))
{
  file.show(myPNG)
}


## ----BashExample-------------------------------------------------------------------------------------
writeLines(readLines(system.file("extdata", "BashExample.sh", package = "LandGenCourse")))


## ----platform----------------------------------------------------------------------------------------
Session <- devtools::session_info()
Session$platform


## ----packages----------------------------------------------------------------------------------------
head(Session$packages)


## ----session_info, include=FALSE---------------------------------------------------------------------
today <-format(Sys.Date(), "%Y-%m-%d")
outFile <- paste0("SessionInfo", "_", today, ".txt")
outPath <- file.path(here::here(), "output", outFile)
writeLines(capture.output(devtools::session_info()), outPath)
file.show(outPath)


## ----parallel----------------------------------------------------------------------------------------
library(parallel)
detectCores()


## ----nCores------------------------------------------------------------------------------------------
nCores <- detectCores()
if(Sys.info()[['sysname']]=="Windows") nCores = 1
nCores


## ----mclapply----------------------------------------------------------------------------------------
x <- gen[,-1]
m1 <- lapply(x, mean, na.rm=TRUE)
#m2 <- mclapply(x, mean, na.rm=TRUE, mc.cores=nCores)  # Use this line when running the code yourself
m2 <- mclapply(x, mean, na.rm=TRUE, mc.cores=1)        # Replace this line with the previous line


## ----microbenchmark4---------------------------------------------------------------------------------
method1 <- function(x) {colMeans(x, na.rm=TRUE)}
method2 <- function(x) {for(i in 1:ncol(x)) mean(x[,i], na.rm=TRUE)}
method3 <- function(x) {lapply(x, mean, na.rm=TRUE)}
#method4 <- function(x) {mclapply(x, mean, na.rm=TRUE, mc.cores=nCores)} # Use this line when running the code yourself
method4 <- function(x) {mclapply(x, mean, na.rm=TRUE, mc.cores=1)}  # Replace this line with the previous line

microbenchmark::microbenchmark(times = 10, unit = "ms",
                               method1(x), method2(x), method3(x), method4(x))



## ----doParallel--------------------------------------------------------------------------------------
library(doParallel)
#cl <- makeCluster(2)
#cl
#registerDoParallel(cl)


## ----foreach-----------------------------------------------------------------------------------------
m1 <- for(i in 1:ncol(x)) mean(x[,i], na.rm=TRUE)
m2 <- foreach(i = 1:ncol(x)) %do% (mean(x[,i], na.rm=TRUE))
#m3 <- foreach(i = 1:ncol(x)) %dopar% (mean(x[,i], na.rm=TRUE))


## ----microbenchmark5---------------------------------------------------------------------------------
#method1 <- function(x) {colMeans(x, na.rm=TRUE)}
#method2 <- function(x) {for(i in 1:ncol(x)) mean(x[,i], na.rm=TRUE)}
#method3 <- function(x) {lapply(x, mean, na.rm=TRUE)}
#method4 <- function(x) {mclapply(x, mean, na.rm=TRUE, mc.cores=nCores)}
#method5 <- function(x) {foreach(i = 1:ncol(x)) %do% (mean(x[,i], na.rm=TRUE))}
#method6 <- function(x) {foreach(i = 1:ncol(x)) %dopar% (mean(x[,i], na.rm=TRUE))}

#microbenchmark::microbenchmark(times = 3, unit = "ms",method1(x), method2(x), method3(x), method4(x), method5(x), method6(x))


## ----detach, message=FALSE, warning=TRUE, include=FALSE----------------------------------------------
LandGenCourse::detachAllPackages()

