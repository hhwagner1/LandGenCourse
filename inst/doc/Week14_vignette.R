## ---- packages-----------------------------------------------------------
library(LandGenCourse)
library(dplyr)
library(ggplot2)
#library(gstudio)
#library(pegas)
#library(vegan)
#library(purrr)
#library(MuMIn)
#library(lme4)
#library(cowplot)

## ---- import-------------------------------------------------------------
dat <- gstudio::read_population(system.file("extdata",              
        "pulsatilla_genotypes.csv", package="LandGenCourse"), 
        type = "column", locus.columns = 6:19)
dat$ID <- as.factor(dat$ID)

## ------------------------------------------------------------------------
head(dat)

## ----fig.height=5, fig.width=7-------------------------------------------
Coords <- dat %>% group_by(Population) %>% 
          summarize(X = mean(X), Y = mean(Y))
Coords <- data.frame(Coords, sf::sf_project(from = "+init=epsg:31468", 
          to = "+init=epsg:4326", pts = Coords[c("X", "Y")]))
names(Coords)[4:5] <- c("Longitude", "Latitude")

MyMap <- ggmap::qmplot(Longitude, Latitude, data=Coords, 
                       source = "google", maptype="terrain", zoom=12) 
MyMap + geom_text(data = Coords, mapping = aes(Longitude, Latitude, 
              label = Population), size = 4, col = "black", hjust = 0, 
              nudge_x = 0.005, nudge_y = c(0,0,0.002,-0.001,0,0,0)) 

## ---- dat----------------------------------------------------------------
dat[dat$ID == "3083",]

## ----warning=FALSE, minus_mom--------------------------------------------
pollen <- gstudio::minus_mom(dat, MomCol = "ID", OffCol = "OffID")
pollen[pollen$ID == "3083",]

## ---- pegas--------------------------------------------------------------
D <- gstudio::genetic_distance(pollen,mode="amova")
D <- as.dist(D)
Moms <- pollen$ID
Populations <- as.factor(pollen$Population) 
amova.result <- pegas::amova(D ~ Populations/Moms, nperm=500)
amova.result

## ------------------------------------------------------------------------
phi <- amova.result$varcomp[1]/sum(amova.result$varcomp[1])
names(phi) <- "phi"
phi

## ---- dps----------------------------------------------------------------
dps <- 1 - gstudio::genetic_distance(pollen, stratum = "ID", mode = "Dps")

## ---- dist---------------------------------------------------------------
xy <- unique(data.frame(pollen$X, pollen$Y))
xy.dist <- as.matrix(dist(xy))

## ---- plot---------------------------------------------------------------
par(mar=c(4,4,0,0))
plot(xy.dist[lower.tri(xy.dist)], dps[lower.tri(dps)], 
    xlab = "Geographic distance (m)", 
     ylab = "1 - Proportion of shared alleles (dps)")
abline(lm(dps[lower.tri(dps)]~xy.dist[lower.tri(xy.dist)]))

## ---- mantel-------------------------------------------------------------
vegan::mantel(xy.dist, dps)

## ---- exclusion_probability----------------------------------------------
# exclusion probabilities
pollen.freqs <- gstudio::frequencies(dat)
p.excl <- gstudio::exclusion_probability( pollen.freqs )
p.excl

## ---- prod---------------------------------------------------------------
1- prod((1-unlist(p.excl$Pexcl)))

## ---- paternity----------------------------------------------------------
# Select all offspring of mom 3083:
offspring.3083 <- subset(dat, OffID!=0 & ID == "3083")
# Select mom 3083:
mother.3083 <- subset(dat, ID == "3083" & dat$OffID == 0 )
# Select all potential fathers in the same population as mom 3083:
fathers.3083 <- subset(dat, OffID == 0 & Population %in% offspring.3083$Population)
# Paternity analysis: 
pat.3083 <- gstudio::paternity(offspring.3083, mother.3083, fathers.3083 )
pat.3083

## ---- full_paternity-----------------------------------------------------
# make a dataframe just for the offspring:
offspring <- subset(dat, OffID!=0)

# here is the function that we will apply to all mothers:
get.parentage <- function(x){
  tmp.offspring <- subset(offspring, ID == x)
  tmp.mother <- subset(dat, ID == x & OffID == 0)
  tmp.fathers <- subset(dat, OffID == 0 & Population %in% tmp.offspring$Population)
  return(gstudio::paternity(tmp.offspring, tmp.mother, tmp.fathers ))
}

## ---- purrrify-----------------------------------------------------------
# purrr-ify the function so that NA is returned when an error pops up:
possible_pat <- purrr::possibly(get.parentage, otherwise = NA_real_)

# run the function and store the output:
# list of results for each mother:
pat.all <- purrr::map(unique(offspring$ID), possible_pat)
# convert the list to a dataframe:
pat.all <- do.call(rbind, pat.all[!is.na(pat.all)]) 

## ---- threshold----------------------------------------------------------
# create a temporary ID that combines the MomID and the OffID
pat.all$tmpID <- paste(pat.all$MomID, pat.all$OffID, sep="_")
# get rid of all rows with duplicated tmpIDs, leaving just the first entry for each
pat.sub <- pat.all[!duplicated(pat.all$tmpID),]
# get rid of the tmpID column
pat.sub <- pat.sub[,1:4] # get rid of the tmp_ID column


## ---- spiderplot_data----------------------------------------------------
pat.sub <- gstudio::spiderplot_data(pat.sub, dat, longitude = "X", latitude = "Y")
# Join data to add population IDs
pat.sub <- merge(pat.sub, dat[, c("Population", "ID" ,"OffID")],
                 by.x=c("MomID", "OffID"), by.y=c("ID", "OffID"), all.x=T)

head(pat.sub)

## ---- plot_pollenFlow, fig.height=6, fig.width=7-------------------------
pop <- "A25"
ggplot() +
  geom_point(data=dat[dat$Population==pop,], 
             aes(X,Y),size=3, color="red") +
  geom_segment(data=pat.sub[pat.sub$Population==pop,], 
       aes(x=X, y=Y, xend=Xend, yend=Yend), size=0.5, alpha=0.2, 
       arrow = arrow(ends = "first", length = unit(0.3, "cm"))) +
  theme(legend.position = "none")

## ---- dispersal_distance-------------------------------------------------
pat.sub$pollen.dist <- unlist(lapply(1:nrow(pat.sub),
    function(x) dist(rbind(c(pat.sub$X[x], pat.sub$Y[x]), 
                c(pat.sub$Xend[x], pat.sub$Yend[x]))) ))

## ---- dispersal_kernel---------------------------------------------------
ggplot(pat.sub[pat.sub$pollen.dist >0,]) +
  geom_histogram( aes(x=pollen.dist),  bins=20) +
  xlab("Distance from pollen source (m)")  +
  ylab("Number of pollen flow events")

## ---- import_momVariables------------------------------------------------
# read in the data
mom.vars <- read.csv(system.file("extdata",                 
 "pulsatilla_momVariables.csv", package="LandGenCourse"))

# exclude selfing
pat.outcrossed <- subset(pat.sub, MomID != DadID)
# add mom variables to pat.outcrossed
pat.outcrossed <- merge(pat.outcrossed, mom.vars, by.x = "MomID", 
                        by.y = "ID", all.x=T)
# look at the data
head(pat.outcrossed)

## ---- lme----------------------------------------------------------------
# specify the model
mod <- lme4::lmer(log(pollen.dist) ~ scale(log(flower.density)) + 
            scale(log(mom.isolation)) + (1|Population/MomID),
            data=pat.outcrossed, na.action = "na.fail", REML=F)

## ---- dredge-------------------------------------------------------------
MuMIn::dredge(mod)

## ----fig.height=3, fig.width=7-------------------------------------------
Mom.isolation.plot <- ggplot(pat.outcrossed, 
       aes(x=log(mom.isolation), y=log(pollen.dist))) + 
       geom_point() + stat_smooth(method="lm", se=F) + 
       xlab("Log(mom isolation)") +
       ylab("Log(pollen flow distance)")

Flower.density.plot <- ggplot(pat.outcrossed, 
       aes(x=log(flower.density), 
       y=log(pollen.dist))) + geom_point() +
       stat_smooth(method="lm", se=F) + 
       xlab("Log(flower density)") +
       ylab("Log(pollen flow distance)")

cowplot::plot_grid(Mom.isolation.plot, Flower.density.plot, 
                   labels = c("A", "B"))

## ---- merge_pat.sub------------------------------------------------------
offspring <- merge(offspring, pat.sub[,1:4], by.x=c("ID", "OffID"), 
                   by.y=c("MomID", "OffID"), all.x=T)
head(offspring)

## ---- outside------------------------------------------------------------
num.out <- sapply(split(offspring$Fij, offspring$Population),
                  function(x) sum(is.na(x)))

## ---- total_pollination--------------------------------------------------
num.tot <- table(offspring$Population)

## ---- data.frame---------------------------------------------------------
# turn it into a dataframe:
pop.df <- data.frame(Population=names(num.out), 
    num.out=as.vector(num.out), num.tot=as.vector(num.tot))

# read in the population variable data:
pop.vars <- read.csv(system.file("extdata", 
  "pulsatilla_population.csv", package="LandGenCourse"))

# add the population variables to our outside pollination data:
pop.df <- merge(pop.df, pop.vars, by="Population")
pop.df

## ---- glm----------------------------------------------------------------
# specify the model
mod2 <- glm(cbind(num.out, num.tot-num.out) ~ forest.50 + forest.100 +
              forest.250 + forest.500 + forest.1000 + population.size,
              family = binomial(link = "logit"), data = pop.df, 
              na.action = "na.fail")

## ---- scale.x------------------------------------------------------------
MuMIn::dredge(mod2,m.lim=c(0,1))

## ---- plot_forest.250----------------------------------------------------
forest.250.mod <- glm(cbind(num.out, num.tot-num.out) ~ forest.250,
                family=binomial(link="logit"), data=pop.df)

ggplot(pop.df, aes(x=forest.250, y=num.out/num.tot)) + geom_point() +
  geom_line(aes(x=forest.250, y=predict(forest.250.mod, type="response"))) +
  xlab("Proportion of forest at 250 m") +
  ylab("Proportion of immigrant pollen")

## ----message=FALSE, warning=TRUE, include=FALSE--------------------------
LandGenCourse::detachAllPackages()

