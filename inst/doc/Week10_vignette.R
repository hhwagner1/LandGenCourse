## -----------------------------------------------------------------------------
if(!requireNamespace("GeNetIt", quietly = TRUE)) remotes::install_github("jeffreyevans/GeNetIt")

## ----message=FALSE, warning=TRUE----------------------------------------------
require(LandGenCourse)
#require(GeNetIt)
#require(raster)
#require(gdistance)

## -----------------------------------------------------------------------------
data(rasters, package="GeNetIt")
RasterMaps <- raster::stack(rasters)

## -----------------------------------------------------------------------------
data(ralu.site, package="GeNetIt")
sites <- ralu.site

## ----fig.width=8, fig.height=5.5----------------------------------------------
raster::plot(RasterMaps)

## -----------------------------------------------------------------------------
par(mar=c(2,2,1,1))
raster::plot(RasterMaps$ffp)
points(sites, pch=3)

## -----------------------------------------------------------------------------
#cti <- raster::resample(cti, gsp, method= "bilinear")

## -----------------------------------------------------------------------------
RasterMaps$err27

## -----------------------------------------------------------------------------
err.cost <- (1/RasterMaps$err27)
err.cost

## -----------------------------------------------------------------------------
RasterMaps$ffp

## -----------------------------------------------------------------------------
ffp.cost <- (RasterMaps$ffp/5)
ffp.cost

## -----------------------------------------------------------------------------
RasterMaps$gsp

## -----------------------------------------------------------------------------
gsp.cost <- (RasterMaps$gsp-196)/15
gsp.cost

## -----------------------------------------------------------------------------
RasterMaps$cti

## -----------------------------------------------------------------------------
cti.cost <- RasterMaps$cti/5
cti.cost

## -----------------------------------------------------------------------------
cost1 <- (gsp.cost + cti.cost + err.cost + ffp.cost)
cost1

## -----------------------------------------------------------------------------
tr.cost1 <- gdistance::transition(cost1, transitionFunction=mean, directions=8) 
tr.cost1

## -----------------------------------------------------------------------------
par(mar=c(2,2,1,1))
raster::plot(raster::raster(tr.cost1))

## -----------------------------------------------------------------------------
tr.cost1 <- gdistance::geoCorrection(tr.cost1,type = "c",multpl=FALSE)

## -----------------------------------------------------------------------------
par(mar=c(2,2,1,2))
AtoB <- gdistance::shortestPath(tr.cost1, origin=sites[1,], 
                                goal=sites[2,], output="SpatialLines")
raster::plot(raster::raster(tr.cost1), xlab="x coordinate (m)", 
             ylab="y coordinate (m)",legend.lab="Conductance")
lines(AtoB, col="red", lwd=2)
points(sites[1:2,])

## ----message=FALSE------------------------------------------------------------
par(mar=c(2,2,1,2))
raster::plot(raster::raster(tr.cost1), xlab="x coordinate (m)", 
             ylab="y coordinate (m)", legend.lab="Conductance")
points(sites)

Neighbours <- spdep::tri2nb(sites@coords, row.names = sites$SiteName)

plot(Neighbours, sites@coords, col="darkgrey", add=TRUE)
for(i in 1:length(Neighbours))
{
  for(j in Neighbours[[i]][Neighbours[[i]] > i])
  {
    AtoB <- gdistance::shortestPath(tr.cost1, origin=sites[i,], 
                                goal=sites[j,], output="SpatialLines")
    lines(AtoB, col="red", lwd=1.5)
  }
}

## -----------------------------------------------------------------------------
cost1.dist <- gdistance::costDistance(tr.cost1,sites)

## -----------------------------------------------------------------------------
comm1.dist <- gdistance::commuteDistance(x = tr.cost1, coords = sites)

## -----------------------------------------------------------------------------
dist_df <- data.frame("cost1.dist"=as.numeric(cost1.dist),
                      "comm1.dist"=as.numeric(comm1.dist))

## -----------------------------------------------------------------------------
corr.LCD.comm <- cor(dist_df$cost1.dist, dist_df$comm1.dist, method = "spearman")
corr.LCD.comm
plot(cost1.dist~comm1.dist)

## -----------------------------------------------------------------------------
cor_cost <- c()
cor_comm <- c()
res_fact <- seq(2,20,2)
for(fac in res_fact){
  cost1_agg <- raster::aggregate(cost1, fact = fac)
  tr.cost_agg <- gdistance::transition(cost1_agg, 
                 transitionFunction=mean, directions=8)
  tr.cost_agg <- gdistance::geoCorrection(tr.cost_agg,type = "c",multpl=FALSE)
  cost.dist_agg <- gdistance::costDistance(tr.cost_agg,sites)
  comm.dist_agg <- gdistance::commuteDistance(x = tr.cost_agg, coords = sites)
  cost.dist_agg <- as.numeric(cost.dist_agg)
  comm.dist_agg <- as.numeric(comm.dist_agg)
  cor_cost <- c(cor_cost,cor(dist_df$cost1.dist, cost.dist_agg, 
                             method = "spearman"))
  cor_comm <- c(cor_comm,cor(dist_df$comm1.dist, comm.dist_agg, 
                             method = "spearman"))
}


## -----------------------------------------------------------------------------
par(mar=c(4,4,1,1))
plot(y = cor_cost, x = res_fact, col = "red", pch = 19, 
     ylim = c(0.9,1), xlab = "Aggregation factor", ylab = "Spearman correlation")
points(y = cor_comm, x = res_fact, col = "blue", pch = 19)
legend("bottomleft", legend = c("Costdist","Commdist"), 
       pch = 19, col = c("red", "blue"))


## ----message=FALSE, warning=TRUE, include=FALSE-------------------------------
LandGenCourse::detachAllPackages()

