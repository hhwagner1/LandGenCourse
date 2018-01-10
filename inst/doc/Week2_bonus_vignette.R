## ----message=FALSE, warning=TRUE-----------------------------------------
require(LandGenCourse)
require(sp)
require(sf)
require(raster)
require(GeNetIt)
require(rasterVis)
require(tmaptools) 

## ------------------------------------------------------------------------
data(ralu.site)
Sites.sf <- st_as_sf(ralu.site)
class(Sites.sf)

## ---- fig.width=7, fig.height=6------------------------------------------
par(mar=c(2,2,2,2))
plot(Sites.sf, pch=16)

## ----fig.show='hold'-----------------------------------------------------
plot(Sites.sf[,c("Drainage", "Basin")], pch=16)
plot(Sites.sf[,c("AREA_m2", "Depth_m")], pch=16)

## ------------------------------------------------------------------------
#require(here)
#if(!dir.exists(paste0(here(),"/output"))) dir.create(paste0(here(),"/output"))
#st_write(Sites.sf, paste0(here(),"/output/Sites.shp"), delete_dsn = TRUE)

## ------------------------------------------------------------------------
data(rasters)
NLCD <- raster(rasters[6])
NLCD

## ------------------------------------------------------------------------
NLCD <- ratify(NLCD)
rat <- levels(NLCD)[[1]]
rat

## ------------------------------------------------------------------------
ColTab <- read.csv(system.file("extdata", "Colortable_LULC.csv", 
                            package = "LandGenCourse"), header=TRUE)
ColTab$color <- as.character(ColTab$color)
ColTab$attribute <- as.character(ColTab$attribute)
ColTab

## ------------------------------------------------------------------------
rat <- merge(rat, ColTab, by.x="ID", by.y="value", all=FALSE, sort=TRUE)
levels(NLCD) <- rat
NLCD@data@attributes

## ----fig.width=7---------------------------------------------------------
myOrder <- order(NLCD@data@attributes[[1]]$attribute)
Map <- levelplot(NLCD, att='attribute', colorkey=list(height=0.5), 
          col.regions=NLCD@data@attributes[[1]]$color[myOrder]) 
Map

## ----fig.width=7---------------------------------------------------------

Sites.sp <- ralu.site

Map + layer(sp.points(Sites.sp, pch=16, col="yellow", cex=1.1)) +
  layer(sp.points(Sites.sp, pch=1, col="black", cex=1.1))

## ----message=FALSE, warning=TRUE, include=FALSE--------------------------
LandGenCourse::detachAllPackages()

