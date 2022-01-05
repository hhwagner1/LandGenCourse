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

## -----------------------------------------------------------------------------
xvars <- rio::import("https://www.dropbox.com/s/xjl9zpgqplwg1us/ralu.rasters.rds?dl=1")
xvars
names(xvars)

## -----------------------------------------------------------------------------
m <- c(0,10.8, 0,10.9,12.1,1,12.9,89.1,0, 89.5,95.1,1)
  reclass <- matrix(m, ncol=3, byrow=TRUE)

## ----warning=FALSE------------------------------------------------------------
wetlnd <- raster::reclassify(xvars$nlcd, reclass) 

## ----warning=FALSE------------------------------------------------------------
  wetlnd@data@names <- "wetlnd"

## -----------------------------------------------------------------------------
plot(wetlnd)

## -----------------------------------------------------------------------------
xvars <- raster::stack(xvars, wetlnd)
names(xvars)

## -----------------------------------------------------------------------------
nlcd_sampled <- landscapemetrics::sample_lsm(landscape = xvars[["wetlnd"]], 
                                                   what = "lsm_c_pland",
                                                   shape = "circle",
                                                   y = sites, 
                                                   size = 300, 
                                                   return_raster = FALSE,
                                                   plot_id=sites@data$SiteID)
pwetland <- dplyr::select(dplyr::filter(nlcd_sampled, class == 1, 
                                        metric == "pland"), plot_id, value)  
names(pwetland) <- c("SiteID", "pwetland")
pwetland$pwetland <- pwetland$pwetland/100
head(pwetland)

## -----------------------------------------------------------------------------
sites@data <- dplyr::left_join(sites@data, pwetland)
sites@data$pwetland[is.na(sites@data$pwetland)] <- 0
head(sites@data)

## -----------------------------------------------------------------------------
sites@data <- data.frame(sites@data, raster::extract(xvars, sites))

## -----------------------------------------------------------------------------
names(sites@data)

## -----------------------------------------------------------------------------
idx <- which(names(xvars) %in% c("nlcd","wetlnd"))

## -----------------------------------------------------------------------------
dist.graph@proj4string@projargs <- "+proj=utm +zone=11 +ellps=GRS80 +towgs84=0,0,0,-0,-0,-0,0 +units=m +no_defs "

stats <- GeNetIt::graph.statistics(dist.graph, r = xvars[[-idx]], buffer= NULL, 
						  stats = c("min", "mean", "max", "var", "median"))
dist.graph@data <- data.frame(dist.graph@data, stats)
names(dist.graph@data)

## -----------------------------------------------------------------------------
wet.pct <- function(x) { 
  x <- ifelse( x == 11 | x == 90 | x == 95, 1, 0)
    prop.table(table(x))[2] 
}

## -----------------------------------------------------------------------------
wetstats <- GeNetIt::graph.statistics(dist.graph, r=xvars$nlcd, buffer= NULL, 
						stats = c("wet.pct"))
  wetstats[is.na(wetstats)] <- 0
  dist.graph@data <- data.frame(dist.graph@data, wetstats)
  names(dist.graph@data)

## -----------------------------------------------------------------------------
node.var <- c("degree", "betweenness", "Elev", "Length", "Area", "Perim", 
              "Depth", "pH","Dforest","Drock", "Dshrub", "pwetland", "cti",
			  "dd5", "ffp","gsp","pratio","hli","rough27","srr")

## -----------------------------------------------------------------------------
node <- GeNetIt::build.node.data(sites@data, group.ids = "SiteID", from.parms = node.var)
head(node)

## -----------------------------------------------------------------------------
gdata <- merge(dist.graph, node, by.x="from_ID", by.y="SiteID") 
  gdata <- gdata@data
  names(gdata)

## -----------------------------------------------------------------------------
nodeln <- node[,c(2:21)]
  for(i in 1:ncol(nodeln)) {
    nodeln[,i] <- log(nodeln[,i] - (min(nodeln[,i]) - 1))
  }
nodecor.ln <- cor(nodeln, y = NULL, 
                  use = "complete.obs", 
                  method = "pearson")
round(nodecor.ln, 3) 
#pairs(nodecor.ln, pch=19, cex=0.50) 

## ----fig.height=5, fig.width=8------------------------------------------------
edge.ln <- dist.graph@data[,10:length(dist.graph@data)]
  for(i in 1:ncol(edge.ln)) {
    edge.ln[,i] <- log(edge.ln[,i] - (min(edge.ln[,i]) - 1))
  }
edgecor.ln <- cor(edge.ln, y = NULL, 
                  use = "complete.obs", 
                  method = "pearson")
round(edgecor.ln, 3) 

## -----------------------------------------------------------------------------
pdf(file=paste0(here::here(),"/output/node.cor.pdf"), width=20, height=20)
   pairs(nodecor.ln, pch=19, cex=0.50)
dev.off()

## -----------------------------------------------------------------------------
write.csv(round(edgecor.ln, 4), 
          file = paste0(here::here(),"/output/EdgeCorrelationsLn.csv"))
write.csv(round(nodecor.ln, 4), 
          file = paste0(here::here(),"/output/NodeCorrelationsLn.csv"))

## -----------------------------------------------------------------------------
( null <- GeNetIt::gravity(y = "GDIST", x = c("length"), d = "length", group = "from_ID", 
                  data = gdata, method = "ML") )

## -----------------------------------------------------------------------------
  ( global <- GeNetIt::gravity(y = "GDIST", x = c("length", "wet.pct.nlcd", 
                                         "median.gsp", "from.Depth", 
                                         "from.ffp", "from.hli", "from.pratio", 
                                         "from.degree", "from.betweenness", 
                                         "from.pwetland", "median.srr", 
                                         "median.rough27"), d = "length", 
                      group = "from_ID", data = gdata, method = "ML") )

## -----------------------------------------------------------------------------
( published <- GeNetIt::gravity(y = "GDIST", x = c("length", "median.gsp", "from.Depth", 
                    "from.hli", "median.cti", "median.srr"), d = "length", 
				     group = "from_ID", data = gdata, method = "ML"))

## -----------------------------------------------------------------------------
( habitat <- GeNetIt::gravity(y = "GDIST", x = c("length", "wet.pct.nlcd", "median.gsp"), d = "length", 
                     group = "from_ID", data = gdata, method = "ML") )


## -----------------------------------------------------------------------------
#compare.models(null, depth, product, climate, wetlands, topo, habitat, global)
#compare.models(depth, product, climate, wetlands, topo, habitat, published, global, null) 
GeNetIt::compare.models(null, habitat, global, published) #NOTE - global will need to be edited to match your paramters

## -----------------------------------------------------------------------------
par(mfrow=c(2,3))
   for (i in 1:6) { plot(global, type=i) } 

## -----------------------------------------------------------------------------
habitat_fit <- GeNetIt::gravity(y = "GDIST", x = c("length", "wet.pct.nlcd", "median.gsp"), 
                       d = "length", group = "from_ID", data = gdata, method = "REML")

## -----------------------------------------------------------------------------
global_fit <- GeNetIt::gravity(y = "GDIST", x = c("length", "wet.pct.nlcd", "median.gsp", 
                                         "from.Depth", "from.ffp", "from.hli", 
                                         "from.pratio", "from.degree",  
                                         "from.betweenness", "from.pwetland", "median.srr", 
                                         "median.rough27"), 
                      d = "length", group = "from_ID", data = gdata, method = "REML")

## -----------------------------------------------------------------------------
published_fit <- GeNetIt::gravity(y = "GDIST", x = c("length", "median.gsp", "from.Depth", 
                    "from.hli", "median.cti", "median.srr"), d = "length", 
				     group = "from_ID", data = gdata, method = "REML") 

## -----------------------------------------------------------------------------
GeNetIt::compare.models(global_fit, habitat_fit, published_fit)

## -----------------------------------------------------------------------------
GeNetIt::gravity.es(habitat_fit)
GeNetIt::gravity.es(global_fit)
GeNetIt::gravity.es(published_fit)

