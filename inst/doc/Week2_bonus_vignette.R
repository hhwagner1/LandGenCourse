## ----message=FALSE, warning=TRUE---------------------------------------------------------------------
library(LandGenCourse)
library(sf)
library(GeNetIt)
library(terra)
library(tmap)
library(dplyr)
library(tibble)
library(here)


## ----------------------------------------------------------------------------------------------------
data(ralu.site)
#if(!dir.exists(here("output"))) dir.create(here("output"))
#dir.create(here("output/Sites"))
#st_write(ralu.site, here("output/Sites/Sites.shp"), delete_dsn = TRUE)


## ----------------------------------------------------------------------------------------------------
#Sites.sf_a <- st_read(here("output/Sites/Sites.shp"))
#Sites.sf_a 


## ----------------------------------------------------------------------------------------------------
data(ralu.site)
Sites.sp <- sf::as_Spatial(ralu.site)
Sites.sp


## ----------------------------------------------------------------------------------------------------
Sites.sf_b <- sf::st_as_sf(Sites.sp)
Sites.sf_b


## ----------------------------------------------------------------------------------------------------
RasterMaps <- rast(system.file("extdata/covariates.tif", package="GeNetIt"))

RasterMaps.r <- raster::stack(RasterMaps)
RasterMaps.r 


## ----------------------------------------------------------------------------------------------------
nlcd.r <- raster::raster(RasterMaps$nlcd)
nlcd.r 


## ----------------------------------------------------------------------------------------------------
nlcd <- terra::rast(nlcd.r)
nlcd


## ----------------------------------------------------------------------------------------------------
RasterMaps_b <- terra::rast(RasterMaps.r)
RasterMaps_b


## ----------------------------------------------------------------------------------------------------
data(ralu.site)
Sites.sf_c <- ralu.site
plot(st_geometry(Sites.sf_c))


## ----fig.width=7, fig.height=6-----------------------------------------------------------------------
par(mar=c(2,2,2,2))
plot(Sites.sf_c, pch=16, cex=2)


## ----fig.show='hold'---------------------------------------------------------------------------------
plot(Sites.sf_c[,c("Basin", "Depth_m")], pch=16)


## ----message=FALSE-----------------------------------------------------------------------------------
tmap_mode("plot")
tm_shape(Sites.sf_c) + tm_sf()


## ----message=FALSE-----------------------------------------------------------------------------------
tmap_mode("plot")
tm_shape(Sites.sf_c) + tm_bubbles(size="Depth_m", col="Basin") +
  tm_layout(legend.outside=TRUE, legend.outside.position="right") 


## ----------------------------------------------------------------------------------------------------
Bbox = st_bbox(Sites.sf_c)
Bbox


## ----------------------------------------------------------------------------------------------------
delta.x <- Bbox[3] - Bbox[1]
delta.y <- Bbox[4] - Bbox[2]
Zoom <- 0.1
Bbox2 <- Bbox + c(-delta.x, -delta.y, delta.x, delta.y) * Zoom
Bbox2


## ----message=FALSE-----------------------------------------------------------------------------------
tmap_mode("plot")
Map1 <- tm_shape(Sites.sf_c, bbox=Bbox2) + 
  tm_bubbles(size="Depth_m", col="Basin") +
  tm_layout(legend.outside=TRUE, legend.outside.position="right") 
Map1


## ----message=FALSE-----------------------------------------------------------------------------------
#tmap_mode("view")
#tm_shape(Sites.sf_c) + tm_bubbles(size="Depth_m", col="Basin") 


## ----message=FALSE-----------------------------------------------------------------------------------
#tm_shape(Sites.sf_c) + tm_bubbles(size="Depth_m", col="Basin") +
#tm_basemap(server = c("Esri.WorldTopoMap", "Esri.WorldGrayCanvas", 
#                        "OpenStreetMap", "OpenTopoMap", "Esri.WorldImagery"))


## ----message=FALSE-----------------------------------------------------------------------------------
#tmap_mode("view")

#Map2 <- tm_shape(Sites.sf_c, bbox=Bbox2) +  tm_sf("Basin", size=2, border.col="black") +
#  tm_shape(Sites.sf_c) + tm_sf(size=0.8, col="Depth_m", 
#                             palette = "Blues", border.col="black") +
#  tm_basemap(server = c("Esri.WorldTopoMap", "Esri.WorldGrayCanvas", 
#                        "OpenStreetMap", "OpenTopoMap", "Esri.WorldImagery"))
#Map2


## ----------------------------------------------------------------------------------------------------
#if(!dir.exists(here("output"))) dir.create(here("output"))
#tmap_save(Map1, here::here("output/StaticMap.png"), height=7)
#tmap_save(Map2, here::here("output/InteractiveMap.html"))


## ----------------------------------------------------------------------------------------------------
RasterMaps <- rast(system.file("extdata/covariates.tif", package="GeNetIt"))
NLCD <- terra::as.factor(RasterMaps$nlcd)


## ----------------------------------------------------------------------------------------------------
levels(NLCD)[[1]]


## ----------------------------------------------------------------------------------------------------
ColTab <- read.csv(system.file("extdata", "Colortable_LULC.csv", 
                            package = "LandGenCourse"), header=TRUE)
ColTab


## ----------------------------------------------------------------------------------------------------
RAT <- dplyr::left_join(levels(NLCD)[[1]], ColTab, by=c("ID"="value")) %>%
  mutate(nlcd=attribute) %>% 
  dplyr::select(ID, nlcd, color)


## ----------------------------------------------------------------------------------------------------
levels(NLCD) <- RAT
NLCD


## ----------------------------------------------------------------------------------------------------
plot(NLCD, col=RAT$color)
points(Sites.sf_c, pch=21, col="black", bg="white", cex=1)


## ----fig.width=7-------------------------------------------------------------------------------------
Map3 <- tm_shape(NLCD) + 
  tm_raster(style="cat", palette=RAT$color, labels=RAT$nlcd,
            title="Land cover") +
  tm_layout(legend.outside=TRUE, legend.outside.position="right") 
Map3


## ----fig.width=7, warning=FALSE----------------------------------------------------------------------
Map4 <- Map3 + tm_shape(Sites.sf_c) +
  tm_symbols(size=0.4, col="yellow", border.col="red") +
  tm_compass() + tm_scale_bar(bg.color="lightgray", bg.alpha=0.5)
Map4


## ----------------------------------------------------------------------------------------------------
#if(!dir.exists(here("output"))) dir.create(here("output"))
#tmap_save(Map4, here::here("output/RasterMap.pdf"), height=6, width=8)


## ----message=FALSE, warning=TRUE, include=FALSE------------------------------------------------------
# Detach all packages except for some basic ones:
LandGenCourse::detachAllPackages()

