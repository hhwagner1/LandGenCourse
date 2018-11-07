## ----packages global_options, include=TRUE, results="hide", message=FALSE, warning=FALSE----
library(LandGenCourse)
library(sp)
#require(raster)
#require(rgdal)
#require(raster)
#require(GeNetIt)
#require(spdep)
#require(maptools)
#require(RANN)
#require(spatialEco)
#require(GeNetIt)

## ------------------------------------------------------------------------
data(dps, package="GeNetIt")
data(ralu.site, package="GeNetIt")
data(rasters, package="GeNetIt")

## ------------------------------------------------------------------------
xvars <- raster::stack(rasters[-6]) 

## ------------------------------------------------------------------------
land.cov <- raster::stack(rasters[6]) 

## ------------------------------------------------------------------------
par(mar = c(0,0,0,0))
sp::plot(ralu.site)

## ----fig.height=4.7, fig.width=7-----------------------------------------
raster::plot(xvars)

## ------------------------------------------------------------------------
ralu.site@data <- data.frame(ralu.site@data, 
                  raster::extract(xvars[[c(1,2)]], ralu.site))

## ------------------------------------------------------------------------
head(ralu.site@data)  

## ------------------------------------------------------------------------
dist.graph <- GeNetIt::knn.graph(ralu.site, 
                row.names = ralu.site@data[,"SiteName"]) 
#dist.graph <- knn.graph(ralu.site, row.names = ralu.site@data[,"SiteName"], max.dist = 5000)

## ------------------------------------------------------------------------
dist.graph@data$from.to <- paste(dist.graph$i, 
                                 dist.graph$j, sep=".")
dps$from.to <- paste(dps$FROM_SITE, dps$TO_SITE, sep=".") 
dist.graph <- merge(dist.graph, dps, by = "from.to") 

## ------------------------------------------------------------------------
dist.graph@data <- dist.graph@data[,-c(7,8)]

## ------------------------------------------------------------------------
na.index <- unique(as.data.frame(which(is.na(dist.graph@data), 
                                       arr.ind = TRUE))[,1])
dist.graph <- dist.graph[-na.index, ]

## ----fig.width=4.5-------------------------------------------------------
str(dist.graph@data) 
par(mar=c(2.5,2,0.5,4))
plot(xvars[[2]])
plot(dist.graph, add=T)
points(ralu.site, pch=20, col="red")

## ------------------------------------------------------------------------
stats <- GeNetIt::graph.statistics(dist.graph, r = xvars, d=30, 
            stats = c("min", "mean", "max", "var"), sp = FALSE) 
dist.graph@data <- data.frame(dist.graph@data, stats)

## ------------------------------------------------------------------------
wet.pct <- function(x) { 
  x <- ifelse( x == 11 |  x == 12 | x == 90 | x == 95, 1, 0)
  prop.table(table(x))[2] 
}

## ------------------------------------------------------------------------
lc.stats <- GeNetIt::graph.statistics(dist.graph, r = land.cov, 
                                      d=30, stats = "wet.pct")

## ------------------------------------------------------------------------
lc.stats[is.na(lc.stats)] <- 0

## ------------------------------------------------------------------------
dist.graph@data <- data.frame(dist.graph@data, lc.stats)
str(dist.graph@data)

## ------------------------------------------------------------------------
from <- ralu.site@data[,c(1,6,8,18,19)]
names(from)[2:ncol(from)] <- 
  paste("f",names(from)[2:ncol(from)], sep=".") 
to <- ralu.site@data[,c(1,6,8,18,19)]
names(to)[2:ncol(to)] <- 
  paste("t", names(to)[2:ncol(to)], sep=".") 
site <- data.frame(from,to)  
site <- site[,-(dim(to)[2]+1)]

## ------------------------------------------------------------------------
cdata <- merge(dist.graph, site, by.x="from_ID", by.y="SiteName") 
cdata$Dps <- 1 - cdata$Dps
cdata <- cdata@data

## ------------------------------------------------------------------------
x = c("length", "mean.cti", "mean.err27", "mean.ffp", "mean.gsp",
      "f.AREA_m2", "f.Depth_m", "f.err27")

## ------------------------------------------------------------------------
gm <- GeNetIt::gravity(y = "Dps", x = x, d = "length", 
           group = "from_ID", method="ML",data = cdata) 

## ----fig.height=5.5, fig.width=7-----------------------------------------
par(mfrow=c(2,3))
for (i in 1:6) { plot(gm, type=i) } 

## ------------------------------------------------------------------------
x = c("length", "mean.cti", "mean.err27", "mean.ffp", "wet.pct.nlcd",   
      "f.Depth_m", "f.cti")

( gm.2 <- GeNetIt::gravity(y = "Dps", x = x, d = "length", 
            group = "from_ID", method= "ML", data = cdata) )

## ----fig.height=5.5, fig.width=7-----------------------------------------
par(mfrow=c(2,3))
for (i in 1:6) { plot(gm.2, type=i) } 

## ------------------------------------------------------------------------
#x = c("length", #insert names of your variables)

## ------------------------------------------------------------------------
( gm.X <- GeNetIt::gravity(y = "Dps", x = x, d = "length", 
            group = "from_ID", data = cdata, method="ML") )

## ----fig.height=5.5, fig.width=7-----------------------------------------
par(mfrow=c(2,3))
for (i in 1:6) { plot(gm, type=i) } 

## ------------------------------------------------------------------------
( gm.X <- GeNetIt::gravity(y = "Dps", x = x, d = "length", 
            group = "from_ID", data = cdata, method="REML") )

## ----message=FALSE, warning=TRUE, include=FALSE--------------------------
LandGenCourse::detachAllPackages()

