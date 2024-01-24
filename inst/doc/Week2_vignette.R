## ----------------------------------------------------------------------------------------------------
if(!requireNamespace("GeNetIt", quietly = TRUE)) remotes::install_github("jeffreyevans/GeNetIt")


## ----message=FALSE, warning=TRUE---------------------------------------------------------------------
library(LandGenCourse)
library(here)
library(landscapemetrics)
library(dplyr)
library(sf)
library(terra)
library(GeNetIt)
library(tibble)
library(tmap)
library(RColorBrewer)


## ----------------------------------------------------------------------------------------------------
data(ralu.site)
class(ralu.site)


## ----------------------------------------------------------------------------------------------------
Coordinates <- st_coordinates(ralu.site)
head(Coordinates)


## ----------------------------------------------------------------------------------------------------
Data <- st_drop_geometry(ralu.site)
class(Data)


## ----------------------------------------------------------------------------------------------------
Sites <- data.frame(Data, Coordinates)
Sites.sf <- st_as_sf(Sites, coords=c("X", "Y"))
head(Sites.sf)


## ----------------------------------------------------------------------------------------------------
#require(here)
#if(!dir.exists(here("output"))) dir.create(here("output"))
#write.csv(data.frame(Data, Coordinates), file=here("output/ralu.site.csv"), quote=FALSE, row.names=FALSE)
#Sites <- read.csv(here("output/ralu.site.csv"), header=TRUE)
#Sites.sf <- st_as_sf(df, coords=c("X", "Y"))


## ----------------------------------------------------------------------------------------------------
st_crs(Sites.sf) <- 32611


## ----------------------------------------------------------------------------------------------------
st_crs(Sites.sf) <- st_crs(ralu.site)


## ----------------------------------------------------------------------------------------------------
Sites.sf.longlat <- st_transform(Sites.sf, crs = 4326)
head(Sites.sf.longlat)


## ----------------------------------------------------------------------------------------------------
require(tmap)

tmap_mode("view")
tm_shape(Sites.sf) + tm_sf(col="red", size=1)


## ----------------------------------------------------------------------------------------------------
RasterMaps <- rast(system.file("extdata/covariates.tif", package="GeNetIt"))
class(RasterMaps)


## ----------------------------------------------------------------------------------------------------
RasterMaps


## ----fig.width=8, fig.height=5.5---------------------------------------------------------------------
plot(RasterMaps)


## ----------------------------------------------------------------------------------------------------
layerCor(RasterMaps, "pearson", na.rm=TRUE)$pearson


## ----fig.width=4.45, fig.height=4--------------------------------------------------------------------
par(mar=c(3,3,1,2))
plot(RasterMaps, "ffp", col=rev(rainbow(9)))
points(Sites.sf, pch=21, col="black", bg="white")


## ----------------------------------------------------------------------------------------------------
Sites.terra <- terra::extract(RasterMaps, Sites.sf, bind=TRUE)


## ----------------------------------------------------------------------------------------------------
Sites.sf <- sf::st_as_sf(Sites.terra)
Sites.sf


## ----------------------------------------------------------------------------------------------------
mean(Sites.sf$ffp)


## ----------------------------------------------------------------------------------------------------
table(Sites.sf$nlcd)


## ----------------------------------------------------------------------------------------------------
landscapemetrics::check_landscape(RasterMaps)


## ----------------------------------------------------------------------------------------------------
landscapemetrics::list_lsm(level = "landscape", type = "diversity metric")


## ----------------------------------------------------------------------------------------------------
landscapemetrics::list_lsm(metric = "area")


## ----------------------------------------------------------------------------------------------------
landscapemetrics::list_lsm(level = c("class", "landscape"), type = "aggregation metric", 
                           simplify = TRUE)


## ----------------------------------------------------------------------------------------------------
# calculate percentage of landscape of class
percentage_class <- lsm_c_pland(landscape = RasterMaps$nlcd)

percentage_class


## ----------------------------------------------------------------------------------------------------
metrics <- rbind(
  landscapemetrics::lsm_c_pladj(RasterMaps$nlcd), 
  landscapemetrics::lsm_l_pr(RasterMaps$nlcd),
  landscapemetrics::lsm_l_shdi(RasterMaps$nlcd)
  )

metrics


## ----------------------------------------------------------------------------------------------------
nlcd_patch <- landscapemetrics::calculate_lsm(landscape = RasterMaps$nlcd,
                                              level = "patch")
nlcd_patch


## ----------------------------------------------------------------------------------------------------
unique(nlcd_patch$metric)


## ----------------------------------------------------------------------------------------------------
nlcd_landscape_aggr <- landscapemetrics::calculate_lsm(landscape = RasterMaps$nlcd, 
                                                       level = "landscape", 
                                                       type = "aggregation metric")
nlcd_landscape_aggr


## ----------------------------------------------------------------------------------------------------
nlcd_subset <- landscapemetrics::calculate_lsm(landscape = RasterMaps$nlcd, 
                                               what = c("lsm_c_pladj", 
                                                        "lsm_l_pr", 
                                                        "lsm_l_shdi"))
nlcd_subset


## ----------------------------------------------------------------------------------------------------
id_largest <- nlcd_patch %>% # previously calculated patch metrics
  dplyr::filter(metric == "area") %>% # only patch area
  dplyr::arrange(-value) %>% # order by decreasing size
  dplyr::filter(value > quantile(value, probs = 0.95)) # get only patches larger than 95% quantile

id_largest <- id_largest$id # get only patch id
id_largest


## ----------------------------------------------------------------------------------------------------
nlcd_subset_full_a <- landscapemetrics::calculate_lsm(RasterMaps$nlcd, 
                                                      what = c("lsm_c_pladj", 
                                                               "lsm_l_pr", 
                                                               "lsm_l_shdi"), 
                                                      full_name = TRUE)
nlcd_subset_full_a


## ----------------------------------------------------------------------------------------------------
nlcd_subset_full_b <- dplyr::left_join(x = nlcd_subset,
                                       y = lsm_abbreviations_names,
                                       by = c("metric", "level"))

nlcd_subset_full_b


## ----------------------------------------------------------------------------------------------------
forest_patch_metrics <- dplyr::filter(nlcd_patch, class == 42)
forest_patch_metrics


## ----------------------------------------------------------------------------------------------------
# connected components labeling of landscape
cc_nlcd <- landscapemetrics::get_patches(RasterMaps$nlcd, directions = 8)

# summarize the SpatRaster for class 42: 
cc_nlcd$layer_1$class_42


## ----------------------------------------------------------------------------------------------------
cc_forest <- landscapemetrics::get_patches(RasterMaps$nlcd, class = 42)

cc_forest$layer_1$class_42


## ----------------------------------------------------------------------------------------------------
show_patches(landscape = RasterMaps$nlcd, class = c(42, 52), labels = FALSE)


## ----warning=FALSE-----------------------------------------------------------------------------------
show_cores(landscape = RasterMaps$nlcd, class = c(42), edge_depth = 5, labels = FALSE)


## ----------------------------------------------------------------------------------------------------
show_lsm(landscape = RasterMaps$nlcd, class = c(42, 52), what = "lsm_p_area", labels = FALSE)


## ----------------------------------------------------------------------------------------------------
# extract patch area of all classes:
patch_size_sf <- extract_lsm(landscape = RasterMaps$nlcd, y = Sites.sf, what = "lsm_p_area")

# because we are only interested in the forest patch size, we set all area of class != 42 to 0:
patch_size_sf_forest <- dplyr::mutate(patch_size_sf, 
                                      value = dplyr::case_when(class == 42 ~ value, 
                                                               class != 42 ~ 0))
# add data to sf object:
Sites.sf$ForestPatchSize <- patch_size_sf_forest$value
Sites.sf$ForestPatchSize


## ----fig.width=4.45, fig.height=4--------------------------------------------------------------------
tmap_mode("view")
tm_shape(Sites.sf) + tm_bubbles(col="ForestPatchSize") 


## ----------------------------------------------------------------------------------------------------
nlcd_sampled <- landscapemetrics::sample_lsm(landscape = RasterMaps$nlcd, 
                                             what = c("lsm_l_ta", 
                                                      "lsm_c_np",
                                                      "lsm_c_pland", 
                                                      "lsm_c_ai"),
                                             shape = "square",
                                             y = Sites.sf, 
                                             size = 500)
nlcd_sampled


## ----------------------------------------------------------------------------------------------------
# sample some metrics within buffer around sample location and returning sample
# plots as raster

nlcd_sampled_plots <- landscapemetrics::sample_lsm(landscape = RasterMaps$nlcd, 
                                                   what = c("lsm_l_ta",
                                                            "lsm_c_np",
                                                            "lsm_c_pland",
                                                            "lsm_c_ai"),
                                                   shape = "square",
                                                   y = Sites.sf, 
                                                   size = 500, 
                                                   return_raster = TRUE)

nlcd_sampled_plots


## ----fig.width=8, fig.height=5.5---------------------------------------------------------------------
unique_plots <- unique(nlcd_sampled_plots$raster_sample_plots)[1:4]

par(mfrow = c(2,2))
for(i in 1:4)
{
  plot(unique_plots[[i]], type="classes",
     main = paste(Sites.sf$SiteName[i]))
}
par(mfrow = c(1,1))


## ----------------------------------------------------------------------------------------------------
nColors <- nrow(unique(RasterMaps$nlcd))
Colors <- RColorBrewer::brewer.pal(n = nColors, name = "Dark2") 

par(mfrow = c(2,2))
for(i in 1:4)
{
  Present <- is.element(unique(RasterMaps$nlcd)$nlcd, unique(unique_plots[[i]])$nlcd)
  plot(unique_plots[[i]], type="classes",
       col=Colors[Present==TRUE],
     main = paste(Sites.sf$SiteName[i]))
}
par(mfrow = c(1,1))


## ----------------------------------------------------------------------------------------------------
percentage_forest_500_a <- dplyr::pull(dplyr::filter(nlcd_sampled, 
                                                     class == 42, 
                                                     metric == "pland"), value)
percentage_forest_500_a


## ----------------------------------------------------------------------------------------------------
percentage_forest_500_b <- nlcd_sampled %>% 
  dplyr::filter(class == 42, 
                metric == "pland") %>% 
  dplyr::pull(value)
percentage_forest_500_b


## ----------------------------------------------------------------------------------------------------
# filter for percentage of landscape
percentage_forest_500_df <- dplyr::filter(nlcd_sampled,
                                          metric == "pland")

percentage_forest_500_df


## ----------------------------------------------------------------------------------------------------
# group by plot_id and sum all percentages
pland_sum_a <- dplyr::summarize(dplyr::group_by(percentage_forest_500_df, 
                                                by = plot_id), 
                                sum_pland = sum(value))
pland_sum_a


## ----------------------------------------------------------------------------------------------------
pland_sum_b <- percentage_forest_500_df %>% 
  dplyr::group_by(plot_id) %>% 
  dplyr::summarize(sum_pland = sum(value))
pland_sum_b


## ----------------------------------------------------------------------------------------------------
# filter for class == 42 (forest)
forest_500_df <- dplyr::filter(nlcd_sampled,
                               class == 42)

# data.frame with id and name of site
SiteName_df <- data.frame(id = 1:length(Sites.sf$SiteName), site_name = Sites.sf$SiteName)

# add site_name to metrics using plot_id and id of sampling sites
forest_500_df <- dplyr::left_join(forest_500_df, SiteName_df, by = c("plot_id" = "id"))

forest_500_df


## ----message=FALSE, warning=TRUE, include=FALSE------------------------------------------------------
# The following code detaches all packages except for some basic ones:
LandGenCourse::detachAllPackages()

