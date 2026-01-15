installDGS <- function()
{


#####################################
# Check R version: FALSE = up to date, TRUE = dated
#####################################

tmp <- installr::check.for.updates.R(GUI=FALSE)

if(tmp == TRUE)
{
  installr::updateR(fast=TRUE)
}
cat("\n")




# Notes;
# 1) "QstFstComp" is no longer on CRAN and must be a GitHub install
# 2) The "poppr" package has a compile error and will not install from binary or source
#      this dependency is causing LandGenCourse to fail install. I am however able to
#      install windows binary from the archive

# set a CRAN mirror
local({r <- getOption("repos")
       r["CRAN"] <- "https://cloud.r-project.org"
       options(repos=r)})

# Set local library path to R install / library
if( Sys.info()['sysname'] != "Windows") {
  lib <- file.path(chartr("\\", "/", R.home()), "library")
} else {
  lib <- file.path(R.home(), "library")
}

pkg <- as.data.frame(utils::installed.packages())$Package
  if(!"devtools" %in% pkg) utils::install.packages("devtools")
    if(!"remotes" %in% pkg) utils::install.packages("remotes")

if( Sys.info()['sysname'] == "macOS") {
  if(!"rgeos" %in% pkg) {
    utils::install.packages("rgeos", repos="http://R-Forge.R-project.org", type="source")}
  if(!"rgdal" %in% pkg) {
    utils::install.packages("rgdal", repos="http://R-Forge.R-project.org", type="source")}
}

#####################################
# Install CRAN packages from source
#####################################
#if(!"base64enc" %in% pkg) {
#  utils::install.packages("http://cran.r-project.org/src/contrib/base64enc_0.1-3.tar.gz",
#                   repos=NULL, type="source")}



#####################################
# Install CRAN packages from binaries
#####################################
# Add require packages function
libs <- function(x, add = FALSE, install = TRUE) {
  pkg <- as.data.frame(utils::installed.packages())
  for(i in 1:length(x)) {
    if(!x[i] %in% pkg$Package)
	{
      cat(x[i], " is not installed","\n\n")
      if(install) {
        cat("    Installing", x[i], "to", file.path(R.home(), "library"), "\n\n")
	      try( utils::install.packages(x[i], repos = "http://cran.us.r-project.org") )
		if(add) require(x[i], quietly = TRUE, character.only = TRUE)
      }
	} else {
	  cat(x[i], pkg[which(pkg$Package %in% x[i]),]$Version, "is installed", "\n\n")
	    if(add) require(x[i], quietly = TRUE, character.only = TRUE)
    }
  }
}
cran.pkg <- c("ade4", "adegenet", "base64enc", "BiocManager", "car", "compiler", "cowplot", "data.table", "deldir",
              "doParallel", "dplyr", "effsize", "feather", "fields", "formatR", "gdistance",
              "GeNetIt", "geosphere", "ggeffects", "ggmap", "ggplot2", "gridExtra", "here",
              "httpuv", "hierfstat", "igraph", "knitr", "landscapemetrics", "lattice", "lfmm", "lme4", "maps",
              "mapplots", "Matrix", "microbenchmark", "mmod", "MuMIn", "mvtnorm", "nlme", "parallel",
              "pegas", "pinfsc50", "PopGenReport", "poppr", "predictmeans", "profvis", "proto", "purrr", "pwr",
              "RANN", "raster", "rasterVis", "readr", "RColorBrewer",
              "RgoogleMaps", "rio", "rlang", "rmarkdown", "sampling", "seqinr",
              "sf", "spacetime", "sp", "spatialEco", "spatialreg", "spdep", "spmoran", "stringi",
              "testthat", "terra", "tibble", "tmap", "usdm", "vegan", "vcfR", "vctrs")
libs(cran.pkg)

#### Install specific CRAN versions
r = "http://cran.r-project.org"
#if(!"ade4" %in% pkg) {
#  remotes::install_version("ade4", version = "1.7-8", repos = r, upgrade = "never")}
#if(!"adegenet" %in% pkg) {
#  remotes::install_version("adegenet", version = "2.1.0", repos = r, upgrade = "never")}
#if(!"rstudioapi" %in% pkg) {
#  remotes::install_version("rstudioapi", version = "0.5", repos = r, upgrade = "never")}
#if(!"spmoran" %in% pkg) {
#  remotes::install_version("spmoran", version = "0.1.7.2", repos = r, upgrade = "never")}
#if(!"shiny" %in% pkg) {
#  remotes::install_version("shiny", version = "0.13", repos = r, upgrade = "never")}
#if(!"swirl" %in% pkg) {
#  remotes::install_version("swirl", version = "2.4.3", repos = r, upgrade = "never")}
#if(!"swirlify" %in% pkg) {
#  remotes::install_version("swirlify", version = "0.5.1", repos = r, upgrade = "never")}
#if(!"formatR" %in% pkg) {
#  remotes::install_version("formatR", version = "1.5", repos = r, upgrade = "never")}
#if(!"miniUI" %in% pkg) {
#  remotes::install_version("miniUI", version = "0.1.1", repos = r, upgrade = "never")}
if(!"secr" %in% pkg) { utils::install.packages("secr", dependencies="Imports", type="binary")}

#### Install GitHub packages
if(!"EcoGenetics" %in% pkg) {
  remotes::install_github("leandroroser/EcoGenetics-devel", force=FALSE, upgrade = "never")}
#if(!"here" %in% pkg) {
#  remotes::install_github("krlmlr/here", force=FALSE, upgrade = "never")}
if(!"QstFstComp" %in% pkg) {
  remotes::install_github("kjgilbert/QstFstComp", force=FALSE, upgrade = "never")}
if(!"popgraph" %in% pkg) {
  utils::install.packages( c("RgoogleMaps", "geosphere", "proto", "sampling", "seqinr", "spacetime","spdep"), dependencies=TRUE )
  remotes::install_github("dyerlab/popgraph", force=FALSE, upgrade = "never")}
if(!"gstudio" %in% pkg) {
  remotes::install_github("dyerlab/gstudio", force=FALSE, upgrade = "never")}

if(!"LandGenCourseData" %in% pkg) {
  remotes::install_github("hhwagner1/LandGenCourseData", force=FALSE, upgrade = "never")}
if(!"LandGenCourseData" %in% pkg) {
  remotes::install_github("hhwagner1/LandGenCourseData", force=FALSE, upgrade = "never")}




#### Install Bioclim packages

if(!"qvalue" %in% pkg) {
  #BiocManager::install(version = "3.14", ask=FALSE, type="binary")
  BiocManager::install("qvalue", ask=FALSE, type="binary")
}
if(!"LEA" %in% pkg) {
  BiocManager::install("LEA", ask=FALSE, type="binary")
}

# update package list to include GitHub, alternate version(s) and source installs
cran.pkg <- unique(c(cran.pkg, "EcoGenetics", "QstFstComp", "popgraph", "gstudio",
                     "LandGenCourseData", "qvalue", "LEA",
			  "base64enc", "cowplot", "effsize", "hierfstat", "mapplots", "poppr",
			  "microbenchmark", "predictmeans", "profvis", "pwr", "Sunder", "usdm"))

# Check installs and then install LandGenCourse
pkg <- as.character(as.data.frame(utils::installed.packages())$Package)
  pkg.idx <- pkg[which(pkg %in% cran.pkg)]
  if(length(pkg.idx) != length(cran.pkg)) {
    cat("\n\n", "The following packages are missing: ", cran.pkg[which(!cran.pkg %in% pkg.idx)], "\n\n")
  } else {
    #remotes::install_github("hhwagner1/LandGenCourse", force=TRUE, upgrade = "never")
    cat("\n\n", "All suggested packages installed", "\n\n")
  }

  #####################################
  # Check RStudio version
  #####################################

  info <- rstudioapi::versionInfo()

  # check what version of RStudio is in use
  if (info$version < "1.4") {

    cat("Please update RStudio.")
  }
  cat("Your RStudio version is: ", as.character(info$version), "\n\n")


  #####################################
  # Print session information
  #####################################

  utils::sessionInfo()
}


