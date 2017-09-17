## ----message=FALSE, warning=TRUE-----------------------------------------
require(sp)
require(raster)
require(GeNetIt)
require(tmaptools) 
require(SDMTools) # for landscape metrics
require(tibble)
getwd()           # check your working directory

## ------------------------------------------------------------------------
data(ralu.site)
write.csv(data.frame(ralu.site@coords, ralu.site@data), file="ralu.site.csv", quote=FALSE, row.names=FALSE)
Sites <- read.csv("ralu.site.csv", header=TRUE)
as.tibble(Sites)

## ------------------------------------------------------------------------
Sites.sp <- Sites
coordinates(Sites.sp) <- ~coords.x1+coords.x2
as.tibble(Sites.sp)

## ------------------------------------------------------------------------
proj4string(Sites.sp) <- tmaptools::get_proj4("utm11")

## ------------------------------------------------------------------------
slotNames(Sites.sp)

## ------------------------------------------------------------------------
as.tibble(Sites.sp@coords)

## ------------------------------------------------------------------------
Sites.sp@proj4string

## ------------------------------------------------------------------------
ralu.site@proj4string

## ------------------------------------------------------------------------
Sites.sp@proj4string <- ralu.site@proj4string

## ------------------------------------------------------------------------
data(rasters)
class(rasters)

## ------------------------------------------------------------------------
RasterMaps <- stack(rasters)
class(RasterMaps)

## ------------------------------------------------------------------------
RasterMaps

## ----fig.width=7, fig.height=5-------------------------------------------
plot(RasterMaps)

## ------------------------------------------------------------------------
layerStats(RasterMaps, 'pearson', na.rm=T)

## ----fig.width=4, fig.height=4-------------------------------------------
plot(raster(RasterMaps, layer="ffp"), col=rev(rainbow(9)))
points(Sites.sp, pch=21, col="black", bg="white")

## ------------------------------------------------------------------------
Sites.sp@data <- data.frame(Sites.sp@data, extract(RasterMaps, Sites.sp))

## ------------------------------------------------------------------------
table(Sites.sp@data$nlcd)

## ------------------------------------------------------------------------
NLCD <- raster(RasterMaps, layer="nlcd")
NLCD.class <- SDMTools::ClassStat(NLCD,cellsize=30)

## ------------------------------------------------------------------------
?ClassStat

## ----fig.width=4, fig.height=4-------------------------------------------
Forest <- (NLCD==42)
plot(Forest)
points(Sites.sp, pch=21, bg="yellow", col="black")

## ------------------------------------------------------------------------
Patches <- SDMTools::ConnCompLabel(Forest)
NLCD.patch <- SDMTools::PatchStat(Patches,cellsize=30)
dim(NLCD.patch)

## ------------------------------------------------------------------------
as.tibble(NLCD.patch)

## ------------------------------------------------------------------------
?PatchStat

## ------------------------------------------------------------------------
a <- extract.data(Sites.sp@coords, Patches)   # get patch IDs
a[a==0] <- NA                                 # these are the non-forested areas
Sites.sp@data$ForestPatchSize <- NLCD.patch[a,"area"]
Sites.sp@data$ForestPatchSize[is.na(a)] <- 0  # set patch size to zero for nonforested
Sites.sp@data$ForestPatchSize

## ------------------------------------------------------------------------
bubble(Sites.sp, "ForestPatchSize", fill=FALSE, key.entries=as.numeric(names(table(Sites.sp@data$ForestPatchSize))))

## ------------------------------------------------------------------------
Radius <- 500    # Define buffer radius
Cellsize <- 30   # Indicate cell size in meters

## ------------------------------------------------------------------------
Sites.class <- list()
class.ID <- levels(ratify(NLCD))[[1]]

for(i in 1:nrow(Sites.sp@data))
{
  # Identify all cells that lie within buffer around site i:
  Buffer.cells <- extract(NLCD, Sites.sp[i,], cellnumbers=TRUE, 
                          buffer=Radius)[[1]][,1]
  
  # Copy land cover map and delete all values outside of buffer:
  Buffer.nlcd <- NLCD
  values(Buffer.nlcd)[-Buffer.cells] <- NA
  
  # Calculate class-level metrics for cells within buffer:
  Result <- ClassStat(Buffer.nlcd,cellsize=Cellsize)
  
  # Merge Results table with 'class.ID' to ensure that all cover types
  # are listed for all sites, even if they are not present in buffer,
  # write results into ith element of list 'Sites.class':
  Sites.class[[i]] <- merge(class.ID, Result, all=TRUE, by.x="ID", by.y="class")
}
# Add labels for list elements
names(Sites.class) <- Sites.sp@data$SiteName

## ------------------------------------------------------------------------
as.tibble(Sites.class[[2]])

## ------------------------------------------------------------------------
# Extract one variable, 'prop.landscape', for one cover type 42 (Evergreen Forest)
# (this returns a vector with a single value for each site)

PercentForest500 <- rep(NA, length(Sites.class))  # Create empty results vector
for(i in 1:length(Sites.class))
{
  # For site i, select row with cover type '42' and column 'prop.landscape':
  PercentForest500[i] <- Sites.class[[i]][class.ID$ID==42, "prop.landscape"]
}

# If there are any sites with no forest in buffer, set value to 0:
PercentForest500[is.na(PercentForest500)] <- 0

# Print results:
PercentForest500

## ------------------------------------------------------------------------
# Create empty matrix for storing results:
Prop.landscape <- matrix(data=NA, nrow=length(Sites.class), ncol=length(class.ID$ID))

# Create row and column names:
dimnames(Prop.landscape) <- list(names(Sites.class),
                                 paste("Prop.500", class.ID$ID, sep="."))

# For each site i, extract "prop.landscape" for all cover types
# and write results into row i of Prop.landscape:
for(i in 1:length(Sites.class))
{
  Prop.landscape[i,] <- Sites.class[[i]][,"prop.landscape"]
}

# Set any missing values to 0:
Prop.landscape[is.na(Prop.landscape)] <- 0

# Convert matrix to data frame:
Prop.landscape <- as.data.frame(Prop.landscape)
as.tibble(Prop.landscape)

## ------------------------------------------------------------------------
apply(X=Prop.landscape, MARGIN=1, FUN=sum)

## ------------------------------------------------------------------------
# Create empty matrix for storing results:
Forest.class <- matrix(data=NA, nrow=length(Sites.class), 
                       ncol=ncol(Sites.class[[1]]))

# Create row and column names:
dimnames(Forest.class) <- list(names(Sites.class),
                                 paste("42",names(Sites.class[[1]]), sep="."))

# For each site i, extract all landscape metrics for cover type 42
# and write results into row i of Forest.class:
for(i in 1:length(Sites.class))
{
  Forest.class[i,] <- unlist(Sites.class[[i]][class.ID$ID==42,])
}

# Convert matrix to data frame:
Forest.class <- as.data.frame(Forest.class)
as.tibble(Forest.class)

## ------------------------------------------------------------------------
Sites.sp@data <- data.frame(Sites.sp@data, Prop.landscape,
                                Forest.class) 

