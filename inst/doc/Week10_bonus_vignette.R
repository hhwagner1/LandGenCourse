## ----figure1, echo=FALSE, fig.cap="*Points in parameter space for a loglinear conductance model with two spatial covariates. At each point, resistance distance can be calculated and regressed with genetic distance/divergence.*"----
knitr::include_graphics("radish_tutorial_fig1A.png")


## ----figure2, echo=FALSE, fig.cap="*If the log-likelihood (goodness-of-fit) of the regression is measured across parameter space, this creates a likelihood surface. The parameter values at the maximum of the surface are the maximum likelihood estimates. Reduced models follow from restricting one or both parameters to 0.*"----
knitr::include_graphics("radish_tutorial_fig1B.png")


## ----packages----------------------------------------------------------------------------------------
if(!requireNamespace("GeNetIt", quietly = TRUE)) remotes::install_github("jeffreyevans/GeNetIt")

if(!requireNamespace("corMLPE", quietly = TRUE)) remotes::install_github("nspope/corMLPE")

if(!requireNamespace("radish", quietly = TRUE)) remotes::install_github("nspope/radish")

library(radish)
library(raster)
library(terra)
library(sf)
library(sp)


## ----rasters-----------------------------------------------------------------------------------------
covariates <- terra::rast(system.file("extdata/covariates.tif", package="GeNetIt"))


## ----location----------------------------------------------------------------------------------------
data(ralu.site, package="GeNetIt")
sites <- ralu.site


## ----genetic-----------------------------------------------------------------------------------------
ralu.dc <- as.matrix(read.csv(system.file("extdata", "ralu_dc.csv", 
                            package = "LandGenCourse")))


## ----select------------------------------------------------------------------------------------------
sites_select <- sites[c(1:3,5:6,8,11:12,21,23,25:26),]

## Must be a SpatialPoints object
sites_select <- sp::SpatialPoints(sf::as_Spatial(sites_select))

terra::plot(covariates[["hli"]])
points(sites_select, pch = 19)


## ----prepare-----------------------------------------------------------------------------------------
scale_covs <- stack(scale(covariates)) ## Must be a raster stack

surface <- conductance_surface(covariates = scale_covs,
                               coords = sites_select,
                               directions = 8)


## ----fitbasic----------------------------------------------------------------------------------------
fit_mlpe <- radish(ralu.dc ~ cti + err27 + ffp + hli,
                   data = surface, 
                   conductance_model = radish::loglinear_conductance, 
                   measurement_model = radish::mlpe)


## ----summary-----------------------------------------------------------------------------------------
summary(fit_mlpe)


## ----interactions------------------------------------------------------------------------------------
fit_mlpe.int <- radish(ralu.dc ~ cti*hli,
                       data = surface, 
                       conductance_model = radish::loglinear_conductance, 
                       measurement_model = radish::mlpe)


## ----isolation---------------------------------------------------------------------------------------
fit_ibd <- radish(ralu.dc ~ 1,
                  data = surface, 
                  conductance_model = radish::loglinear_conductance, 
                  measurement_model = radish::mlpe)
anova(fit_ibd, fit_mlpe)


## ----visualize---------------------------------------------------------------------------------------
plot(fitted(fit_mlpe, "distance"), ralu.dc, pch = 19,
     xlab = "Optimized resistance distance", ylab = "Dc")


## ----surface-----------------------------------------------------------------------------------------
## Plot fitted conductance surface
fitted_conductance <- conductance(surface, fit_mlpe, quantile = 0.95)

plot(log(fitted_conductance[["est"]]), 
     main = "Fitted conductance surface\n(cti + err27 + hli + ffp)")


## ----effects-----------------------------------------------------------------------------------------
## Get fitted parameter estimates
mod_tab <- summary(fit_mlpe)
mod_tab <- as.data.frame(mod_tab$ztable[,1:2])

## Get ranges of values for covariates
## Using scaled values here
hli_rng <- seq(cellStats(scale_covs$hli, min),
               cellStats(scale_covs$hli, max), length = 100)

## Calculate estimated mean across range of values
plot_hli <- data.frame(mean = exp(mod_tab$Estimate[4] * hli_rng),
                       hli_rng)

library(ggplot2)
(hli_effect <- ggplot(plot_hli, aes(hli_rng, mean)) +
  geom_line(size = 1) + 
  xlab("Heat Load Index") + ylab("Estimated conductance") +
  theme_classic())


## ----wishart-----------------------------------------------------------------------------------------
fit_wish <- radish(ralu.dc ~ cti + err27 + ffp + hli,
                   data = surface, 
                   nu = 8, # msat loci in original data
                   conductance_model = radish::loglinear_conductance, 
                   measurement_model = radish::generalized_wishart)

summary(fit_wish)


## ----leastsquares------------------------------------------------------------------------------------
fit_lsq <- radish(ralu.dc ~ cti + err27 + ffp + hli,
                  data = surface, 
                  conductance_model = radish::loglinear_conductance, 
                  measurement_model = radish::leastsquares)

summary(fit_lsq)


## ----categorical-------------------------------------------------------------------------------------
lc <- terra::classify(covariates[["gsp"]], c(0, 260, 300, Inf), 
                      include.lowest=TRUE, brackets=TRUE)
terra::set.cats(lc, layer=1, c("low precip.", "med precip.", "high precip."), active=1)
terra::cats(lc)


## ----------------------------------------------------------------------------------------------------
terra::plot(lc)


## ----------------------------------------------------------------------------------------------------
gsp_cat <- ratify(raster(lc))
RAT <- levels(gsp_cat)[[1]]
RAT$VALUE <- c("low precip.", "med precip.", "high precip.")
levels(gsp_cat) <- RAT


## ----------------------------------------------------------------------------------------------------
gsp_cat


## ----together----------------------------------------------------------------------------------------
surface_gsp <- radish::conductance_surface(covariates = raster::stack(gsp_cat),
                                   coords = sites_select,
                                   directions = 8)

fit_mlpe_cat <- radish::radish(ralu.dc ~ gsp,
                       data = surface_gsp, 
                       conductance_model = radish::loglinear_conductance, 
                       measurement_model = radish::mlpe)
summary(fit_mlpe_cat)

fitted_conductance_gsp <- conductance(surface_gsp, fit_mlpe_cat)
plot(fitted_conductance_gsp[["est"]], main = "Fitted conductance surface\n(~gsp)")


## ----message=FALSE, warning=TRUE, include=FALSE------------------------------------------------------
LandGenCourse::detachAllPackages()

