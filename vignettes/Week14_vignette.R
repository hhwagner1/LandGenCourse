## -----------------------------------------------------------------------------
if(!requireNamespace("popgraph", quietly = TRUE)) remotes::install_github("dyerlab/popgraph")
if(!requireNamespace("gstudio", quietly = TRUE)) remotes::install_github("dyerlab/gstudio")

## ---- packages----------------------------------------------------------------
library(LandGenCourse)
library(dplyr)
library(ggplot2)
library(ggmap)
#library(gstudio)
#library(pegas)
#library(vegan)
#library(purrr)
#library(MuMIn)
#library(lme4)
#library(cowplot)

## ---- import------------------------------------------------------------------
dat <- gstudio::read_population(system.file("extdata",              
        "pulsatilla_genotypes.csv", package="LandGenCourse"), 
        type = "column", locus.columns = 6:19)
dat$ID <- as.factor(dat$ID)

## -----------------------------------------------------------------------------
head(dat)

## ----fig.height=5, fig.width=7------------------------------------------------
Coords <- dat %>% group_by(Population) %>% 
          summarize(X = mean(X), Y = mean(Y))
Coords <- data.frame(Coords, sf::sf_project(from = "+init=epsg:31468", 
          to = "+init=epsg:4326", pts = Coords[c("X", "Y")]))
names(Coords)[4:5] <- c("Longitude", "Latitude")

bbox <- make_bbox(lon = Longitude, lat = Latitude, data = Coords, f=0.2)

MyMap <- ggmap(get_stamenmap(bbox, maptype="terrain", zoom=12, force=TRUE)) 
MyMap + geom_text(data = Coords, mapping = aes(Longitude, Latitude, 
              label = Population), size = 4, col = "black", hjust = 0, 
              nudge_x = 0.005, nudge_y = c(0,0,0.002,-0.001,0,0,0)) 

## ---- dat---------------------------------------------------------------------
dat[dat$ID == "3083",]

## ----warning=FALSE, minus_mom-------------------------------------------------
pollen <- gstudio::minus_mom(dat, MomCol = "ID", OffCol = "OffID")
pollen[pollen$ID == "3083",]

