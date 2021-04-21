---
title: 'Week 2: Bonus Material'
author: Helene Wagner
date: '`r Sys.Date()`'
show_toc: true
output:
  'knitr:::html_vignette':
    toc: 'yes'
    fig_width: 4
    fig_height: 3.5
vignette: >
  %\VignetteIndexEntry{Week 2: Bonus Material}
  %\VignetteEngine{knitr::rmarkdown} %\VignetteEncoding{UTF-8}
---

# Week2\_bonus\_vignette

### 1. Overview

#### a\) Goals

This bonus material expands the Worked Exakmple to show:

* How to use a new standard for spatial data \('sf' package\).
* How to plot site attributes in space \('sf' package\)
* How to export the site data to a shapefile for import into a GIS \('sf' package\).
* How to plot a categorical raster map with a predefined color scheme.

Try modifying the code to import your own data!

#### b\) Required R libraries

\`\`\`{r message=FALSE, warning=TRUE} require\(LandGenCourse\) require\(sp\) require\(sf\) require\(raster\) require\(GeNetIt\) require\(rasterVis\)

```text
## 2. Convert 'SpatialPointsDataFrame' to 'sf' object 

While 'sp' was a major achievement, it is being replaced now by 'sf', which is short for "simple feature". The conversion is easy. The resulting S3 object is a data frame and also an 'sf' object.

```{r}
data(ralu.site)
Sites.sf <- st_as_sf(ralu.site)
class(Sites.sf)
```

### 3. Plot variables \(first 10 in data set\)

If we use 'plot' on this 'sf' object, it will plot site attribute data in space! Here we set the point character 'pch' to a filled circle, which is symbol \#16.

For an overview of 'pch' symbol numbers, and colors, check: [http://vis.supstat.com/2013/04/plotting-symbols-and-color-palettes/](http://vis.supstat.com/2013/04/plotting-symbols-and-color-palettes/)

\`\`\`{r, fig.width=7, fig.height=6} par\(mar=c\(2,2,2,2\)\) plot\(Sites.sf, pch=16\)

```text
This is pretty cool! Let's have a closer look at some of the variables. 

- Variabes 'Drainage' and 'Basin' (left): these are factors, and each factor level is assigned a different color.
- Variables 'AREA_m2' and 'Depth_m' (right): these are quantitative variables, and R automatically uses a color ramp (from blue to pink to orange) to indicate variation in the values.

Note: To learn about options for the 'plot' function for 'sf' objects, access the help file by typing '?plot' and select 'Plot sf object'.

```{r fig.show='hold'}
plot(Sites.sf[,c("Drainage", "Basin")], pch=16)
plot(Sites.sf[,c("AREA_m2", "Depth_m")], pch=16)
```

### 4. Export site data as ESRI shapefile

Just in case you want to know how you could get your results into a GIS. This is really easy with the 'sf' package. The following code may produce a warning that column names were abbreviated, and writes the component files for the ESRI shapefile into the pre-existing folder 'output' \(the first line will create it if does not exist yet\). Remember to remove the hashtags '\#' to uncomment the code before running it.

The argument 'delete\_dsn' specifies whether any existing file with the same name should be deleted first \(i.e., overwritten\).

```text
#require(here)
#if(!dir.exists(paste0(here(),"/output"))) dir.create(paste0(here(),"/output"))
#st_write(Sites.sf, paste0(here(),"/output/Sites.shp"), delete_dsn = TRUE)
```

### 5. Display categorical map with a predefined color table

Now to a more tricky topica. Recall that the last raster layer in the Worked Example, 'nlcd', contains categorial land cover data that are coded numerically. The 'raster' package actually misinterpreted them as numeric data.

Let's again extract the categorical raster layer into a new object 'NLCD'.

```text
data(rasters)
NLCD <- raster(rasters[6])
NLCD
```

Then we use function 'ratify' to tell R that this is a categorical map \(factor\) that needs a 'raster attribute table' \(rat\). Nothing to do with rats...

Then create the raster attribute table from the factor levels. This is simply a vector with all factor levels present in 'NLCD'. The levels are codes as numbers between 11 and 95. Here's the legend information: [https://www.mrlc.gov/nlcd06\_leg.php](https://www.mrlc.gov/nlcd06_leg.php)

```text
NLCD <- ratify(NLCD)
rat <- levels(NLCD)[[1]]
rat
```

Let's add some columns with labels and predefined colors \(using hex color code\). A color table \("Colortable\_LULC.csv"\) is already in the data folder.

It has more rows than we need, because not all US land cover classes occur in the study area. We need to make sure the colors and cover types are stored as 'character', because the missing factor levels would later create problems.

```text
ColTab <- read.csv(system.file("extdata", "Colortable_LULC.csv", 
                            package = "LandGenCourse"), header=TRUE)
ColTab$color <- as.character(ColTab$color)
ColTab$attribute <- as.character(ColTab$attribute)
ColTab
```

Now we need to match the codes in 'NLCD.rat' and 'ColTab' and extract the additional variables for the land use categories that occur in NLCD. We can use the function 'merge' to do this.

Finally we tell R that the factor levels of NLCD are defined in 'rat', and R stores the information in the slot 'NLCD@data@attributes'.

```text
rat <- merge(rat, ColTab, by.x="ID", by.y="value", all=FALSE, sort=TRUE)
levels(NLCD) <- rat
NLCD@data@attributes
```

Now we can use the function 'levelplot' \(package 'rasterVis'\) to plot the land cover map with the correct color scheme and legend. We will overlay the sampling locations, using yellow circles with black outlines.

This code uses advanced plotting 'language', where we first define 'Map' as the levelplot of the categorical map. The argument 'att' specifies which column in the 'rat' should be used for the labels, 'colorkey' scales the size of the legend, and 'col.regions' defines the color scheme.

Then, we can plot 'Map' simply by writing its name.

\`\`\`{r fig.width=7} Map &lt;- levelplot\(NLCD, att='attribute', colorkey=list\(height=0.5\), col.regions=NLCD@data@attributes\[\[1\]\]$color\) Map

```text
Additional layers are added to the map with '+ layer()': first we add a layer with the yellow filled circles at the sampling locations, then another layer with the black symbol outlines.

```{r fig.width=7}
Sites.sp <- ralu.site

Map + layer(sp.points(Sites.sp, pch=16, col="yellow", cex=1.1)) +
  layer(sp.points(Sites.sp, pch=1, col="black", cex=1.1))
```

\`\`\`{r message=FALSE, warning=TRUE, include=FALSE}

## Detach all packages except for some basic ones:

LandGenCourse::detachAllPackages\(\)

\`\`\`

