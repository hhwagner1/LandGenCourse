## ----message=FALSE, warning=TRUE---------------------------------------------------------------------
library(LandGenCourse)
library(PopGenReport )   #load the package
library(secr)            #to create a random habitat
#library(gdistance)
#library(mmod)
library(raster)
#library(tibble)
#library(here)
#library(ggplot2)
#library(MLPE)


## ----message=FALSE, warning=TRUE---------------------------------------------------------------------
if(!require(secr)) install.packages("secr", repos='http://cran.us.r-project.org')
#library(secr)


## ----------------------------------------------------------------------------------------------------
if(!requireNamespace("popgraph", quietly = TRUE))
{
  install.packages(c("RgoogleMaps", "geosphere", "proto", "sampling", 
                      "seqinr", "spacetime", "spdep"), dependencies=TRUE)
  remotes::install_github("dyerlab/popgraph")
}
if(!requireNamespace("gstudio", quietly = TRUE)) remotes::install_github("dyerlab/gstudio")


## ----setup-------------------------------------------------------------------------------------------
knitr::opts_knit$set(root.dir = normalizePath("..")) 


## ----------------------------------------------------------------------------------------------------
nx=50
ny=50
set.seed(555) #(to make sure we have the same example running)
#tempmask<-secr::make.mask(nx=nx,ny=ny,spacing=1)
tempgrid<-secr::make.grid(nx=nx,ny=ny,spacing=1)


## ----------------------------------------------------------------------------------------------------
tmp <- secr::randomHabitat(secr::as.mask(tempgrid), p = 0.5, A = 0.5)
r <- as.data.frame(tempgrid) 
r$resistance <- 10
r$resistance[as.numeric(row.names(tmp))] <- 1
r <- raster::rasterFromXYZ(r)


## ----------------------------------------------------------------------------------------------------
table(values(r), exclude="")


## ----------------------------------------------------------------------------------------------------
plot(r)


## ----echo=TRUE---------------------------------------------------------------------------------------
createpops <- function(n=10, minDist=5, landscape=r, habitat=1, plot=TRUE)
{ 
  HabitatCells <- c(1:length(values(landscape)))[values(landscape)==habitat]
  Selected <- sample(HabitatCells, 1)
  Remaining <- HabitatCells[!is.element(HabitatCells, Selected)]
  while (length(Selected) < n & length(Remaining) > 0)
  {
    Candidate <- sample(Remaining, 1)
    Remaining <- Remaining[!is.element(Remaining, Candidate)]
    Distances <- raster::pointDistance(raster::xyFromCell(landscape, Candidate), 
                               raster::xyFromCell(landscape, Selected), 
                               lonlat=FALSE)
    if(min(Distances) > minDist)
    {
      Selected <- append(Selected, Candidate)
    }
  }
  if(plot==TRUE) 
  {
    plot(landscape)  
    points(xyFromCell(landscape, Selected), pch=16)
  }
  return(Selected)
}


## ----echo=TRUE---------------------------------------------------------------------------------------
createpops(n=8, minDist = 3, landscape = r, plot = TRUE)


## ----------------------------------------------------------------------------------------------------
para<- list()
#Define populations (dynamics)
para$n.pops=8
para$n.ind=100

para$sex.ratio <- 0.5
#age distribution....

para$n.cov <- 3 
#number of covariates (before the loci in the data.frame, do not change this!!)


## ----------------------------------------------------------------------------------------------------
#reproduction
para$n.offspring = 2

#migration
para$mig.rate <- 0.1 

#dispersal: exponential dispersal with maximal distance in map units
para$disp.max=50   #average  dispersal of an individual in meters
para$disp.rate = 0.05 #proportion of dispersing individuals

#Define genetics
para$n.allels <- 10
para$n.loci <- 20
para$mut.rate <- 0.001


## ----------------------------------------------------------------------------------------------------
par(mar=c(1,1,1,1))
para$method <- "leastcost" #rSPDdistance, commute
para$NN <- 8  #number of neighbours for the cost distance method

# Initialize simulation of populations from scratch

 landscape<- r  #<-raster(system.file("external/rlogo.grd", package="raster"))

# Define x and y locations
 
 para$cells <- createpops(n=para$n.pops, minDist = 3, 
                         landscape = landscape, plot = FALSE)
 para$locs <- raster::xyFromCell(landscape, para$cells)
 
 #give the population some names 
 rownames(para$locs) <- LETTERS[1:para$n.pops]
  
  
# Create a matrix of pairwise cost distances...  
 
  cost.mat <- PopGenReport::costdistances(landscape, para$locs, 
                                          para$method, para$NN)
  
# ... and a matrix of pairwise Euclidean distances
  
  eucl.mat <- as.matrix(dist(para$locs))  #needed for the analysis later

# Plot your landscape with the populations....
  plot(landscape)
  points(para$locs[,1], para$locs[,2], pch=16, cex=2, col="orange")
  text(para$locs[,1],para$locs[,2], row.names(para$locs), cex=1.5)
  
# Check the parameter list
  
  para



## ----------------------------------------------------------------------------------------------------
simpops.0 <- PopGenReport::init.popgensim(para$n.pops, para$n.ind, 
                           para$sex.ratio, para$n.loci, 
                           para$n.allels, para$locs, para$n.cov )  


## ----------------------------------------------------------------------------------------------------
names(simpops.0)  #the names of the subpopulations
head(simpops.0$A[,1:6]) # a list of the first 6 individuals and columns of population A


## ----------------------------------------------------------------------------------------------------
gsp <- PopGenReport::pops2genind(simpops.0, locs =para$locs)
gsp #check the genind object
summary(gsp)  #some summary statistics
round(mmod::pairwise_Gst_Nei(gsp),5)


## ----------------------------------------------------------------------------------------------------
gen.mat <- PopGenReport::pairwise.fstb(gsp)  
round(gen.mat ,5)


## ----------------------------------------------------------------------------------------------------
PopGenReport::wassermann(eucl.mat = eucl.mat, cost.mats = list(cost=cost.mat), 
                           gen.mat = gen.mat, plot=F)$mantel.tab


## ----------------------------------------------------------------------------------------------------
simpops <- PopGenReport::run.popgensim(simpops.0, steps=3, cost.mat, 
                         n.offspring=para$n.offspring, n.ind=para$n.ind,
                         para$mig.rate, para$disp.max, para$disp.rate, 
                         para$n.allels, para$mut.rate,
                         n.cov=para$n.cov, rec="none")


## ----------------------------------------------------------------------------------------------------
gsp <- PopGenReport::pops2genind(simpops, para$locs, para$n.cov)


## ----------------------------------------------------------------------------------------------------
gen.mat <- PopGenReport::pairwise.fstb(gsp)   
round(gen.mat ,3)


## ----------------------------------------------------------------------------------------------------
PopGenReport::wassermann(eucl.mat = eucl.mat, cost.mats = list(cost=cost.mat), 
             gen.mat = gen.mat, plot=F)$mantel.tab


## ----------------------------------------------------------------------------------------------------
res <- PopGenReport::wassermann(eucl.mat = eucl.mat, 
                                cost.mats = list(cost=cost.mat), 
                                gen.mat = gen.mat, plot=F)$mantel.tab
res[res$model == "Gen ~cost | Euclidean", "p"]


## ----------------------------------------------------------------------------------------------------
if(!requireNamespace("corMLPE", quietly = TRUE)) 
  remotes::install_github("nspope/corMLPE")


## ----------------------------------------------------------------------------------------------------
eucl.vect <- as.vector(as.dist(eucl.mat))
cost.vect <- as.vector(as.dist(cost.mat))
gen.vect <- as.vector(as.dist(gen.mat))


## ----------------------------------------------------------------------------------------------------
Pop <- matrix(names(as.dist(eucl.mat)), nrow(eucl.mat), ncol(eucl.mat), byrow=F)
pop1 <-Pop[lower.tri(Pop)]
pop2 <-t(Pop)[lower.tri(Pop)]


## ----------------------------------------------------------------------------------------------------
Link.data <- data.frame(gen.vect, eucl.vect, cost.vect, pop1, pop2)


## ----------------------------------------------------------------------------------------------------
GE <- nlme::gls(gen.vect ~ eucl.vect + cost.vect, 
                  correlation=corMLPE::corMLPE(form=~pop1+pop2), 
                  data=Link.data, method="ML")
G <- update(GE,  ~ eucl.vect)
E <- update(GE,  ~ cost.vect)
MuMIn::AICc(GE, G, E)


## ----------------------------------------------------------------------------------------------------
tmp <- MuMIn::AICc(GE, G, E)
row.names(tmp)[tmp$AICc == min(tmp$AICc)]


## ----------------------------------------------------------------------------------------------------
getMLPE <- function(gen=gen.mat, eucl=eucl.mat, cost=cost.mat)
{
  Pop <- matrix(names(as.dist(eucl)), nrow(eucl), ncol(eucl), byrow=F)
  Link.data <- data.frame(
    eucl.vect = as.vector(as.dist(eucl)),
    cost.vect = as.vector(as.dist(cost)),
    gen.vect = as.vector(as.dist(gen)),
    pop1 = Pop[lower.tri(Pop)],
    pop2 = t(Pop)[lower.tri(Pop)]
  )
  GE <- nlme::gls(gen.vect ~ eucl.vect + cost.vect, 
                  correlation=corMLPE::corMLPE(form=~pop1+pop2), 
                  data=Link.data, method="ML")
  G <- update(GE,  ~ eucl.vect)
  E <- update(GE,  ~ cost.vect)
  tmp <- MuMIn::AICc(GE, G, E)
  return(row.names(tmp)[tmp$AICc == min(tmp$AICc)])
}


## ----------------------------------------------------------------------------------------------------
getMLPE()


## ----------------------------------------------------------------------------------------------------
timesteps <- seq(from=5 , to=45, by=20)


## ----------------------------------------------------------------------------------------------------
repeats <- factor(1:5)


## ----------------------------------------------------------------------------------------------------
para.space <- expand.grid(rep=repeats, time=timesteps)
tibble::as_tibble(para.space)


## ----eval =F-----------------------------------------------------------------------------------------
## #initialize
## simpops.0 <- PopGenReport::init.popgensim(para$n.pops, para$n.ind,
##                            para$sex.ratio, para$n.loci,
##                            para$n.allels, para$locs, para$n.cov )
## #run for 20 generations
## simpops <- PopGenReport::run.popgensim(simpops.0, steps=20, cost.mat,
##                          n.offspring=para$n.offspring, n.ind=para$n.ind,
##                          para$mig.rate, para$disp.max, para$disp.rate,
##                          para$n.allels, para$mut.rate,
##                          n.cov=para$n.cov, rec="none")


## ----eval=F------------------------------------------------------------------------------------------
## #for (i in 1:nrow(para.space))
## #{
## #  #initialize
## #  simpops.0 <- PopGenReport::init.popgensim(para$n.pops, para$n.ind,
## #                           para$sex.ratio, para$n.loci, para$n.allels,
## #                           para$locs, para$n.cov )
## #
## #  #run for para.space$time[i] generations
## #  simpops <- PopGenReport::run.popgensim(simpops.0,
## #                           steps=para.space$time[i], cost.mat,
## #                           n.offspring=para$n.offspring, n.ind=para$n.ind,
## #                           para$mig.rate, para$disp.max, para$disp.rate,
## #                           para$n.allels, para$mut.rate,
## #                           n.cov=para$n.cov, rec="none")
## #}


## ----------------------------------------------------------------------------------------------------
if(!dir.exists(paste0(here::here(),"/output"))) 
  dir.create(paste0(here::here(),"/output"))
if(!dir.exists(paste0(here::here(),"/output/simout")))
  dir.create(paste0(here::here(),"/output/simout"))


## ----------------------------------------------------------------------------------------------------
# create a timer (just to know how long it will take roughly)
timer0 <- round(proc.time()[3],2)

for (i in 1:nrow(para.space))
{
  # initialize
  simpops.0 <- PopGenReport::init.popgensim(para$n.pops, para$n.ind, 
                           para$sex.ratio, para$n.loci, para$n.allels, 
                           para$locs, para$n.cov )  
  
  # run for para.space$time[i] generations
  simpops <- PopGenReport::run.popgensim(simpops.0, 
                           steps=para.space$time[i], cost.mat, 
                           n.offspring=para$n.offspring, n.ind=para$n.ind,
                           para$mig.rate, para$disp.max, para$disp.rate, 
                           para$n.allels, para$mut.rate,
                           n.cov=para$n.cov, rec="none")
  
  # convert to genind object (smaller)
  gi <- PopGenReport::pops2genind(simpops)
  
  # create a list of all I want to collect
  sim <- list(para.space=para.space[i,], para=para, 
              landscape=landscape, cost.mat=cost.mat, gi=gi)
  
  # save everything in an output folder (with a consecutive number, with three leading zeros, so the file sorting is nicer)

  save(sim, file = paste0(here::here(),"/output/simout/sim_time5-45_rep5_",
                          sprintf("%03i",i) ,".RData"))
  
  cat(paste0("Finished run: ", i," out of ",nrow(para.space),
            ". So far, it took: ", round(proc.time()[3]-timer0,2)," sec.\n"))
  flush.console()
}



## ----------------------------------------------------------------------------------------------------
head(dir(paste0(here::here(), "/output/simout")))


## ----------------------------------------------------------------------------------------------------
res <- data.frame(rep=NA, time=NA, fst=NA, r.Eucl=NA, p.Eucl=NA, 
                  r.cost=NA, p.cost=NA, MLPE=NA)

#load all files in the folder
filenames <- list.files(path= paste0(here::here(), "/output/simout"), pattern="sim")

for (i in 1:length(filenames))
{
  #creates a sim object
  load(paste0(here::here(), "/output/simout/",filenames[i]))

  #now let us take what we need from the simulation
  res[i,1:2] <- sim$para.space
  
  #calculate a summary statistic: mean of pairwise fst values
  # here we only take the lower triangle of the matrix to avoid the diagonal values,
  # which are zero by definition (comparing each population to itself)
  gen.mat <- PopGenReport::pairwise.fstb(sim$gi)  
  res [i,3] <- mean(gen.mat[lower.tri(gen.mat)])
  
  #Distance matrices
  eucl.mat <- dist(sim$para$locs)
  cost.mats = list(cost=sim$cost.mat)
  
  #partial Mantel tests
  wass <- PopGenReport::wassermann(eucl.mat, 
                                cost.mats = list(cost=sim$cost.mat), 
                                gen.mat = gen.mat, plot=F)$mantel.tab
  res[i,4:5] <- wass[wass$model == "Gen ~Euclidean | cost", 2:3]
  res[i,6:7] <- wass[wass$model == "Gen ~cost | Euclidean", 2:3]
  
  #Sunder
  res[i,8] <- getMLPE(gen=gen.mat, eucl=eucl.mat, cost=sim$cost.mat)
}


## ----------------------------------------------------------------------------------------------------
head(res)


## ----------------------------------------------------------------------------------------------------
ggplot2::ggplot(res, ggplot2::aes(x=time, y=fst)) + 
         ggplot2::geom_point(position = ggplot2::position_jitter(w = 0.5))


## ----message=FALSE-----------------------------------------------------------------------------------
library(dplyr)
Pulsatilla.gstudio <- gstudio::read_population(path=system.file("extdata",
                            "pulsatilla_genotypes.csv", 
                            package = "LandGenCourse"), 
                    type="column", locus.columns=c(6:19), 
                    phased=FALSE, sep=",", header=TRUE)
Adults.gstudio <- Pulsatilla.gstudio %>% filter(OffID == 0)


## ----------------------------------------------------------------------------------------------------
#SamePatch <- outer(Adults.gstudio$Population, Adults.gstudio$Population, FUN = "==")
#SamePatch[SamePatch == "FALSE"] <- NA
#Dgeo.within <- SamePatch * Dgeo


## ----------------------------------------------------------------------------------------------------
#R = 499
#Cor.perm.unrestricted <- rep(NA, R)
#for(r in 1:R)
#{
#  a <- sample(Order)
#  Cor.perm.unrestricted[r] <- cor(as.dist(Dgen[a,a]),as.dist(Dgeo.within), method="spearman", use="complete.obs")
#}
#approx.p.unrestricted <- mean(c(Cor.obs, Cor.perm.unrestricted) >= Cor.obs)
#approx.p.unrestricted


## ----message=FALSE, warning=TRUE, include=FALSE------------------------------------------------------
LandGenCourse::detachAllPackages()

