## ----message=FALSE, warning=TRUE----------------------------------------------
require(LandGenCourse)
require(sp)
require(sf)
require(raster)
require(GeNetIt)
require(terra)
library(tmap)
library(dplyr)

## -----------------------------------------------------------------------------
data(ralu.site)
Sites.sf <- st_as_sf(ralu.site)
class(Sites.sf)

## ---- fig.width=7, fig.height=6-----------------------------------------------
par(mar=c(2,2,2,2))
plot(Sites.sf, pch=16)

## ----fig.show='hold'----------------------------------------------------------
plot(Sites.sf[,c("Drainage", "Basin")], pch=16)
plot(Sites.sf[,c("AREA_m2", "Depth_m")], pch=16)

## -----------------------------------------------------------------------------
#require(here)
#if(!dir.exists(paste0(here(),"/output"))) dir.create(paste0(here(),"/output"))
#st_write(Sites.sf, paste0(here(),"/output/Sites.shp"), delete_dsn = TRUE)

## -----------------------------------------------------------------------------
data(rasters)
NLCD <- as.factor(raster(rasters[6]))
NLCD.terra <- rast(NLCD)
NLCD.terra

## -----------------------------------------------------------------------------
cats(NLCD.terra)

## -----------------------------------------------------------------------------
ColTab <- read.csv(system.file("extdata", "Colortable_LULC.csv", 
                            package = "LandGenCourse"), header=TRUE)
ColTab$color <- as.character(ColTab$color)
ColTab$attribute <- as.character(ColTab$attribute)
ColTab

## -----------------------------------------------------------------------------
RAT <- merge(terra::cats(NLCD.terra, layer=1), ColTab, 
             by.x="ID.1", by.y="value", all.x=TRUE, sort=TRUE)
RAT <- RAT[order(RAT$ID),]
RAT

## -----------------------------------------------------------------------------
terra::setCats(NLCD.terra, layer=1, value=RAT[,c(2,1,3,4)], index=2) 
terra::cats(NLCD.terra, layer=1)

## -----------------------------------------------------------------------------
terra::coltab(NLCD.terra, layer=1) <- RAT$color

## -----------------------------------------------------------------------------
NLCD.terra <- terra::as.factor(terra::rast(raster(rasters[6])))
RAT <- left_join(terra::cats(NLCD.terra)[[1]], ColTab, 
                 by=c("ID"="value"))
levels(NLCD.terra) <- RAT
terra::coltab(NLCD.terra, layer=1)<-RAT$color

## ----fig.width=7--------------------------------------------------------------
Map <- tm_shape(NLCD.terra) + 
  tm_raster(labels=cats(NLCD.terra)[[1]]$attribute,
            title="Land cover") +
  tm_layout(legend.outside=TRUE, legend.outside.position="right") +
  tm_grid(lines=FALSE)
Map

## ----fig.width=7, warning=FALSE-----------------------------------------------
Map + tm_shape(ralu.site) +
  tm_symbols(size=0.4, col="yellow", border.col="red")


## ----message=FALSE, warning=TRUE, include=FALSE-------------------------------
# Detach all packages except for some basic ones:
LandGenCourse::detachAllPackages()

