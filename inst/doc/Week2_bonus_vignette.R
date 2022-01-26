## ----message=FALSE, warning=TRUE----------------------------------------------
library(LandGenCourse)
library(sp)
library(sf)
library(raster)
library(GeNetIt)
library(terra)
library(tmap)
library(dplyr)
library(tibble)

## -----------------------------------------------------------------------------
data(ralu.site)
Sites.sf <- st_as_sf(ralu.site)
class(Sites.sf)

## -----------------------------------------------------------------------------
sf::st_crs(Sites.sf)$input

## -----------------------------------------------------------------------------
plot(st_geometry(Sites.sf))

## ---- fig.width=7, fig.height=6-----------------------------------------------
par(mar=c(2,2,2,2))
plot(Sites.sf, pch=16, cex=2)

## ----fig.show='hold'----------------------------------------------------------
plot(Sites.sf[,c("Basin", "Depth_m")], pch=16)

## ----message=FALSE------------------------------------------------------------
tmap_mode("plot")
tm_shape(Sites.sf) + tm_sf()

## ----message=FALSE------------------------------------------------------------
tmap_mode("plot")
tm_shape(Sites.sf) + tm_sf("Depth_m") 

## -----------------------------------------------------------------------------
Bbox = st_bbox(Sites.sf)
Bbox

## -----------------------------------------------------------------------------
delta.x <- Bbox[3] - Bbox[1]
delta.y <- Bbox[4] - Bbox[2]
Zoom <- 0.1
Bbox2 <- Bbox + c(-delta.x, -delta.y, delta.x, delta.y) * Zoom
Bbox2

## ----message=FALSE------------------------------------------------------------
tmap_mode("plot")
Map1 <- tm_shape(Sites.sf, bbox=Bbox2) + tm_sf(size = "Depth_m") 
Map1

## ----message=FALSE------------------------------------------------------------
#tmap_mode("view")
#tm_shape(Sites.sf) + tm_sf(size="Depth_m", col="Depth_m", palette = "Blues") 

## ----message=FALSE------------------------------------------------------------
#tmap_mode("view")

#Map2 <- tm_shape(Sites.sf, bbox=Bbox2) +  tm_sf("Basin", size=2, border.col="black") +
#  tm_shape(Sites.sf) + tm_sf(size=0.8, col="Depth_m", 
#                             palette = "Blues", border.col="black") +
#  tm_basemap(server = c("Esri.WorldTopoMap", "Esri.WorldGrayCanvas", 
#                        "OpenStreetMap", "OpenTopoMap", 
#                        leaflet::providers$Stamen.Terrain, 
#                        leaflet::providers$Stamen.Watercolor,
#                        leaflet::providers$Stamen.TonerLite))
#Map2

## -----------------------------------------------------------------------------
tmap_save(Map1, paste0(here::here(), "/output/StaticMap.png"), height=7)
#tmap_save(Map2, paste0(here::here(), "/output/InteractiveMap.html"))

## -----------------------------------------------------------------------------
#require(here)
#if(!dir.exists(paste0(here(),"/output"))) dir.create(paste0(here(),"/output"))
#st_write(Sites.sf, paste0(here(),"/output/Sites.shp"), delete_dsn = TRUE)

## -----------------------------------------------------------------------------
NLCD <- raster(rasters[6])
NLCD.terra <- terra::as.factor(terra::rast(NLCD))

## -----------------------------------------------------------------------------
tibble::as_tibble(terra::cats(NLCD.terra, layer=1)[[1]])

## -----------------------------------------------------------------------------
ColTab <- read.csv(system.file("extdata", "Colortable_LULC.csv", 
                            package = "LandGenCourse"), header=TRUE)
ColTab$color <- as.character(ColTab$color)
ColTab$attribute <- as.character(ColTab$attribute)
ColTab

## -----------------------------------------------------------------------------
RAT <- merge(terra::cats(NLCD.terra, layer=1), ColTab, 
             by.x="ID", by.y="value", all.x=TRUE, sort=TRUE)
RAT %>% filter(!is.na(nlcd))

## -----------------------------------------------------------------------------
terra::setCats(NLCD.terra, layer=1, value=RAT) 

## -----------------------------------------------------------------------------
terra::coltab(NLCD.terra, layer=1) <- RAT$color

## ----fig.width=7--------------------------------------------------------------
Map3 <- tm_shape(NLCD.terra) + 
  tm_raster(labels=cats(NLCD.terra, layer=1)$attribute,
            title="Land cover") +
  tm_layout(legend.outside=TRUE, legend.outside.position="right") +
  tm_grid(lines=FALSE)
Map3

## ----fig.width=7, warning=FALSE-----------------------------------------------
Map4 <- Map3 + tm_shape(Sites.sf) +
  tm_symbols(size=0.4, col="yellow", border.col="red") +
  tm_compass() + tm_scale_bar(bg.color="lightgray", bg.alpha=0.5)
Map4

## -----------------------------------------------------------------------------
tmap_save(Map4, paste0(here::here(), "/output/RasterMap.pdf"), height=6, width=8)

## ----message=FALSE, warning=TRUE, include=FALSE-------------------------------
# Detach all packages except for some basic ones:
LandGenCourse::detachAllPackages()

