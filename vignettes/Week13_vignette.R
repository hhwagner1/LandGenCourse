## -----------------------------------------------------------------------------
if(!requireNamespace("GeNetIt", quietly = TRUE)) remotes::install_github("jeffreyevans/GeNetIt")
if(!requireNamespace("spatialEco", quietly = TRUE)) remotes::install_github("jeffreyevans/spatialEco")

## ----packages global_options, include=TRUE, results="hide", message=FALSE, warning=FALSE----
library(LandGenCourse)
library(sp)
#library(landscapemetrics)
#library(raster)
#library(rgdal)
#library(GeNetIt)
#library(spatialEco)
#library(GeNetIt)
#library(igraph)
#library(deldir)

## -----------------------------------------------------------------------------
wetlands <- read.csv(system.file("extdata", "Wetlands.csv", 
                            package = "LandGenCourse"), header = TRUE) 
str(wetlands)

## -----------------------------------------------------------------------------
sp::coordinates(wetlands) <- ~X+Y
class(wetlands)
str(wetlands)

## -----------------------------------------------------------------------------
plot(wetlands, asp=1, bty="n", xlab="", ylab="", main = "All Wetlands")
  points(wetlands, pch=19, cex=0.75, col="blue")

## -----------------------------------------------------------------------------
options(warn=-1)
wetlandgraph <- deldir::deldir(coordinates(wetlands)[,1], 
                       coordinates(wetlands)[,2], 
                       z = wetlands$SiteName) 
options(warn=0)

## -----------------------------------------------------------------------------
plot(wetlands, asp=1, bty="n", xlab="", ylab="", main = "All Wetlands")
  points(wetlands, pch=19, cex=0.75, col="blue")
plot(wetlandgraph, wlines = "triang", wpoints="none",
     number=FALSE, add=TRUE, lty=1) 

## -----------------------------------------------------------------------------
ind <- wetlandgraph$delsgs[,5:6] #pull out individual nodes
adj <- matrix(0, length(wetlands$X), length(wetlands$Y)) 
  for (i in 1:nrow(ind)){ 
    adj[ind[i,1], ind[i,2]] <- 1 
    adj[ind[i,2], ind[i,1]] <- 1 
  } 

## -----------------------------------------------------------------------------
wetnet <- igraph::graph_from_adjacency_matrix(adj, weighted = NULL, mode="undirected") 
plot(wetnet)

## -----------------------------------------------------------------------------
wetlands@data$degree <- igraph::degree(wetnet)
head(wetlands@data)

## -----------------------------------------------------------------------------
wetlands@data$betweenness <- igraph::betweenness(wetnet) 
head(wetlands@data)

## -----------------------------------------------------------------------------
sites <- read.csv(system.file("extdata", "RALU_Site.csv", 
                            package = "LandGenCourse"), header = TRUE) 
head(sites)

## -----------------------------------------------------------------------------
nodestats <- as.data.frame(wetlands@data[,3:5])
degree.betweenness <- nodestats[which(nodestats$SiteName %in% sites$SiteName),]
head(degree.betweenness)

## -----------------------------------------------------------------------------
sites <- merge(degree.betweenness, sites, by= "SiteName" )
head(sites)

## -----------------------------------------------------------------------------
coordinates(sites) <- ~X+Y
str(sites)

## -----------------------------------------------------------------------------
summary(sites@data)
sites@data$SiteName <- as.character(sites@data$SiteName)
class(sites@data$SiteName)

## -----------------------------------------------------------------------------
sites@data$SiteID <- as.factor(sites@data$SiteID)
class(sites@data$SiteID)

## -----------------------------------------------------------------------------
dist.graph <- GeNetIt::knn.graph(sites, row.names = sites@data[,"SiteID"])
#dist.graph@proj4string@projargs <- "+proj=utm +zone=11 +ellps=GRS80 +towgs84=0,0,0,-0,-0,-0,0 +units=m +no_defs "
#dist.graph <- GeNetIt::knn.graph(sites, row.names = sites@data[,"SiteName"], max.dist=5000)

## -----------------------------------------------------------------------------
gdist <- read.csv(system.file("extdata", "RALU_Dps.csv", 
                            package = "LandGenCourse"), header=TRUE)
rownames(gdist) <- t(names(gdist))
gdist <- as.matrix (gdist)
head(gdist)

## -----------------------------------------------------------------------------
gdist <- GeNetIt::flow(gdist)
head(gdist)

## -----------------------------------------------------------------------------
gdist <- GeNetIt::dmatrix.df(gdist)
head(gdist)

## -----------------------------------------------------------------------------
names(gdist)[3] <- "GDIST"
names(gdist)

## -----------------------------------------------------------------------------
names(gdist)[1] <- "FROM"
names(gdist)[2] <- "TO"
gdist[,1] <-sub("X", "", gdist[,1])
gdist[,2] <-sub("X", "", gdist[,2])
names(gdist)

## -----------------------------------------------------------------------------
gdist <- cbind(from.to=paste(gdist[,1], gdist[,2], sep="."), gdist)
dist.graph@data$from.to <- paste(dist.graph$from_ID, dist.graph$to_ID, sep=".")
dist.graph <- merge(dist.graph, gdist, by = "from.to") 
head(dist.graph@data)

## -----------------------------------------------------------------------------
if(!dir.exists(paste0(here::here(),"/output"))) 
  dir.create(paste0(here::here(),"/output"))
write.csv(gdist, file= paste0(here::here(),"/output/gdist.csv"))

## -----------------------------------------------------------------------------
#rgdal::writeOGR(dist.graph, paste0(here::here(),"/output"), "DistGraph", 
#                driver="ESRI Shapefile", check_exists=TRUE, overwrite_layer=TRUE)

