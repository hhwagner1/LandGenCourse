## -----------------------------------------------------------------------------
if(!requireNamespace("GeNetIt", quietly = TRUE)) remotes::install_github("jeffreyevans/GeNetIt")

## ----message=FALSE, warning=TRUE----------------------------------------------
library(LandGenCourse)
library(landscapemetrics)
library(dplyr)
library(sp)
library(raster)
library(GeNetIt)
library(tibble)

## -----------------------------------------------------------------------------
data(ralu.site)
class(ralu.site)
Sites <- data.frame(ralu.site@coords, ralu.site@data)
class(Sites)
head(Sites)

## -----------------------------------------------------------------------------
#require(here)
#if(!dir.exists(paste0(here(),"/output"))) dir.create(paste0(here(),"/output"))
#write.csv(Sites, file=paste0(here(),"/output/ralu.site.csv"), 
#          quote=FALSE, row.names=FALSE)
#Sites <- read.csv(paste0(here(),"/output/ralu.site.csv"), header=TRUE)

## -----------------------------------------------------------------------------
Sites.sp <- Sites
coordinates(Sites.sp) <- ~coords.x1+coords.x2

## -----------------------------------------------------------------------------
sp::proj4string(Sites.sp) <- sp::CRS(SRS_string = "EPSG:32611")

## -----------------------------------------------------------------------------
Sites.sp.longlat <- sp::spTransform(Sites.sp, 
                      CRSobj = sp::CRS(SRS_string = "EPSG:4326"))
head(Sites.sp.longlat@coords)

## -----------------------------------------------------------------------------
slotNames(Sites.sp)

## -----------------------------------------------------------------------------
head(Sites.sp@coords)

## -----------------------------------------------------------------------------
Sites.sp@proj4string

## -----------------------------------------------------------------------------
ralu.site@proj4string

## -----------------------------------------------------------------------------
Sites.sp@proj4string <- ralu.site@proj4string

## -----------------------------------------------------------------------------
data(rasters)
class(rasters)

## -----------------------------------------------------------------------------
RasterMaps <- stack(rasters)
class(RasterMaps)

## -----------------------------------------------------------------------------
RasterMaps

## ----fig.width=8, fig.height=5.5----------------------------------------------
plot(RasterMaps)

## -----------------------------------------------------------------------------
layerStats(RasterMaps, 'pearson', na.rm=T)

## ----fig.width=4.45, fig.height=4---------------------------------------------
par(mar=c(3,3,1,2))
plot(raster(RasterMaps, layer="ffp"), col=rev(rainbow(9)))
points(Sites.sp, pch=21, col="black", bg="white")

## -----------------------------------------------------------------------------
Sites.sp@data <- data.frame(Sites.sp@data, extract(RasterMaps, Sites.sp))

## -----------------------------------------------------------------------------
mean(Sites.sp@data$ffp)

## -----------------------------------------------------------------------------
table(Sites.sp@data$nlcd)

## -----------------------------------------------------------------------------
nlcd <- raster(RasterMaps, layer = "nlcd")

landscapemetrics::check_landscape(nlcd)

## -----------------------------------------------------------------------------
landscapemetrics::list_lsm(level = "landscape", type = "diversity metric")

landscapemetrics::list_lsm(metric = "area")

landscapemetrics::list_lsm(level = c("class", "landscape"), type = "aggregation metric", 
                           simplify = TRUE)

## -----------------------------------------------------------------------------
# calculate percentage of landscape of class
percentage_class <- lsm_c_pland(landscape = nlcd)

percentage_class

## -----------------------------------------------------------------------------
metrics <- rbind(
  landscapemetrics::lsm_c_pladj(nlcd), 
  landscapemetrics::lsm_l_pr(nlcd),
  landscapemetrics::lsm_l_shdi(nlcd)
  )

metrics

## -----------------------------------------------------------------------------
nlcd_patch <- landscapemetrics::calculate_lsm(landscape = nlcd,
                                              level = "patch")
nlcd_patch

## -----------------------------------------------------------------------------
unique(nlcd_patch$metric)

## -----------------------------------------------------------------------------
nlcd_landscape_aggr <- landscapemetrics::calculate_lsm(landscape = nlcd, 
                                                       level = "landscape", 
                                                       type = "aggregation metric")
nlcd_landscape_aggr

## -----------------------------------------------------------------------------
nlcd_subset <- landscapemetrics::calculate_lsm(landscape = nlcd, 
                                               what = c("lsm_c_pladj", 
                                                        "lsm_l_pr", 
                                                        "lsm_l_shdi"))
nlcd_subset

## -----------------------------------------------------------------------------
id_largest <- nlcd_patch %>% # previously calculated patch metrics
  dplyr::filter(metric == "area") %>% # only patch area
  dplyr::arrange(-value) %>% # order by decreasing size
  dplyr::filter(value > quantile(value, probs = 0.95)) # get only patches larger than 95% quantile

id_largest <- id_largest$id # get only patch id
id_largest

## -----------------------------------------------------------------------------
nlcd_subset_full_a <- landscapemetrics::calculate_lsm(nlcd, 
                                                      what = c("lsm_c_pladj", 
                                                               "lsm_l_pr", 
                                                               "lsm_l_shdi"), 
                                                      full_name = TRUE)
nlcd_subset_full_a

## -----------------------------------------------------------------------------
nlcd_subset_full_b <- dplyr::left_join(x = nlcd_subset,
                                       y = lsm_abbreviations_names,
                                       by = c("metric", "level"))

nlcd_subset_full_b

## -----------------------------------------------------------------------------
forest_patch_metrics <- dplyr::filter(nlcd_patch, class == 42)

## -----------------------------------------------------------------------------
# connected components labeling of landscape
cc_nlcd <- landscapemetrics::get_patches(nlcd, directions = 8)

# show name of each class
sapply(cc_nlcd, function(x) names(x)) 

# the fourth list entry is class forest
cc_forest_a <- cc_nlcd[4]
cc_forest_b <- landscapemetrics::get_patches(nlcd, class = 42) # watch out: result is list with one entry

cc_forest_a
cc_forest_b

## -----------------------------------------------------------------------------
show_patches(landscape = nlcd, class = c(42, 52), labels = FALSE)

## ----warning=FALSE------------------------------------------------------------
show_cores(landscape = nlcd, class = c(42), edge_depth = 5, labels = FALSE)

## -----------------------------------------------------------------------------
show_lsm(landscape = nlcd, class = c(42, 52), what = "lsm_p_area", labels = FALSE)

## -----------------------------------------------------------------------------
# extract patch area of all classes:
patch_size_sp <- extract_lsm(landscape = nlcd, y = Sites.sp, what = "lsm_p_area")

# because we are only interested in the forest patch size, we set all area of class != 42 to 0:
patch_size_sp_forest <- dplyr::mutate(patch_size_sp, 
                                      value = dplyr::case_when(class == 42 ~ value, 
                                                               class != 42 ~ 0))
# add data to sp object:
Sites.sp@data$ForestPatchSize <- patch_size_sp_forest$value
Sites.sp@data$ForestPatchSize

## ----fig.width=4.45, fig.height=4---------------------------------------------
par(mar = c(3,3,1,2))
bubble(Sites.sp, "ForestPatchSize", fill = FALSE, key.entries = as.numeric(names(table(Sites.sp@data$ForestPatchSize))))

## -----------------------------------------------------------------------------
nlcd_sampled <- landscapemetrics::sample_lsm(landscape = nlcd, 
                                             what = c("lsm_l_ta", 
                                                      "lsm_c_np",
                                                      "lsm_c_pland", 
                                                      "lsm_c_ai"),
                                             shape = "square",
                                             y = Sites.sp, 
                                             size = 500)
nlcd_sampled

## -----------------------------------------------------------------------------
# sample some metrics within buffer around sample location and returning sample
# plots as raster
nlcd_sampled_plots <- landscapemetrics::sample_lsm(landscape = nlcd, 
                                                   what = c("lsm_l_ta",
                                                            "lsm_c_np",
                                                            "lsm_c_pland",
                                                            "lsm_c_ai"),
                                                   shape = "square",
                                                   y = Sites.sp, 
                                                   size = 500, 
                                                   return_raster = TRUE)

nlcd_sampled_plots

## ---- fig.width=8, fig.height=5.5---------------------------------------------
unique_plots <- unique(nlcd_sampled_plots$raster_sample_plots)[1:4]

par(mfrow = c(2,2))
plot(unique_plots[[1]], 
     main = paste(Sites.sp$SiteName[1]), 
     col = rev(rainbow(9)))
plot(unique_plots[[2]],
     main = paste(Sites.sp$SiteName[2]),
     col = rev(rainbow(9)))
plot(unique_plots[[3]],
     main = paste(Sites.sp$SiteName[3]), 
     col = rev(rainbow(9)))
plot(unique_plots[[4]],
     main = paste(Sites.sp$SiteName[4]), 
     col = rev(rainbow(9)))
par(mfrow = c(1,1))

## -----------------------------------------------------------------------------
percentage_forest_500_a <- dplyr::pull(dplyr::filter(nlcd_sampled, 
                                                     class == 42, 
                                                     metric == "pland"), value)
percentage_forest_500_a

## -----------------------------------------------------------------------------
percentage_forest_500_b <- nlcd_sampled %>% 
  dplyr::filter(class == 42, 
                metric == "pland") %>% 
  dplyr::pull(value)
percentage_forest_500_b

## -----------------------------------------------------------------------------
# filter for percentage of landscape
percentage_forest_500_df <- dplyr::filter(nlcd_sampled,
                                          metric == "pland")

percentage_forest_500_df

## -----------------------------------------------------------------------------
# group by plot_id and sum all percentages
pland_sum_a <- dplyr::summarize(dplyr::group_by(percentage_forest_500_df, 
                                                by = plot_id), 
                                sum_pland = sum(value))
pland_sum_a

## -----------------------------------------------------------------------------
pland_sum_b <- percentage_forest_500_df %>% 
  dplyr::group_by(plot_id) %>% 
  dplyr::summarize(sum_pland = sum(value))
pland_sum_b

## -----------------------------------------------------------------------------
# filter for class == 42 (forest)
forest_500_df <- dplyr::filter(nlcd_sampled,
                               class == 42)

# data.frame with id and name of site
SiteName_df <- data.frame(id = 1:length(Sites.sp$SiteName), site_name = Sites.sp$SiteName)

# add site_name to metrics using plot_id and id of sampling sites
forest_500_df <- dplyr::left_join(forest_500_df, SiteName_df, by = c("plot_id" = "id"))

forest_500_df

## ----message=FALSE, warning=TRUE, include=FALSE-------------------------------
# The following code detaches all packages except for some basic ones:
LandGenCourse::detachAllPackages()

