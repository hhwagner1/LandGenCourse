## ----message=FALSE, warning=TRUE---------------------------------------------------------------------
library(LandGenCourse)
library(methods)
library(dplyr)
#require(tibble)
#require(poppr)
#require(ade4)
#require(pwr)
#require(effsize)
require(sf)
#require(car) 
library(ggplot2)
library(tmap)


## ----------------------------------------------------------------------------------------------------
data(dd.genind, package = "LandGenCourse")
dd.genind


## ----------------------------------------------------------------------------------------------------
data(dd.site, package = "LandGenCourse")
tibble::as_tibble(dd.site)
?dd.site


## ----------------------------------------------------------------------------------------------------
dd.genind.Cluster <- dd.genind[!is.na(dd.genind@strata$Cluster),]


## ----------------------------------------------------------------------------------------------------
amova.result <- poppr::poppr.amova(dd.genind.Cluster, hier = ~ Cluster/SITE, 
                                   within=FALSE, method = "ade4")
amova.result


## ----------------------------------------------------------------------------------------------------
amova.test <- ade4::randtest(amova.result, nrepet = 999)
amova.test


## ----------------------------------------------------------------------------------------------------
dd.spatial <- dd.site[dd.site$Spatial==TRUE,]


## ----------------------------------------------------------------------------------------------------
dd.df <- st_drop_geometry(dd.spatial)
cor(dd.df[ , c("FST.GESTE", "NLT", "C", "D")], use="pairwise.complete")


## ----fig.height=4, fig.width=8-----------------------------------------------------------------------
NLT.plot <- ggplot(dd.df, aes(x=NLT, y=FST.GESTE)) + 
  geom_point() + 
  geom_smooth(formula = 'y ~ x', method = lm, se = TRUE) +
  geom_text(aes(x=NLT, y=FST.GESTE, label=SITE), size=2, nudge_x=0, nudge_y=0.01, check_overlap=TRUE)

C.plot <- ggplot(dd.df, aes(x=C, y=FST.GESTE)) + 
  geom_point() +
  geom_smooth(formula = 'y ~ x', method = lm, se = TRUE) +
  geom_text(aes(x=C, y=FST.GESTE, label=SITE), size=2.5, nudge_x=0, nudge_y=0.01, check_overlap=TRUE)

cowplot::plot_grid(NLT.plot, C.plot)


## ----------------------------------------------------------------------------------------------------
mod.diff <- lm(scale(FST.GESTE) ~ scale(NLT) + scale(C), 
               data=dd.spatial)
summary(mod.diff)


## ----fig.height=7, fig.width=8-----------------------------------------------------------------------
par(mfrow=c(2,2))
plot(mod.diff, labels.id = dd.spatial$SITE)
par(mfrow=c(1,1))


## ----------------------------------------------------------------------------------------------------
car::vif(mod.diff)


## ----------------------------------------------------------------------------------------------------
dd.spatial$Residuals <- mod.diff$residuals
dd.spatial$Absolute <- abs(mod.diff$residuals)

a <- which(dd.spatial$Absolute > 1.5)


## ----------------------------------------------------------------------------------------------------
tmap_mode("plot")

Map1 <- tm_shape(dd.spatial) + tm_bubbles(size="Absolute",col="Residuals", 
                                  breaks=c(-Inf, 0, Inf), palette=c("red", "blue")) +
  tm_shape(dd.spatial[a,]) + tm_text(text="SITE", size=0.8, just=c(0.7,2.5))

Map1


## ----message=FALSE-----------------------------------------------------------------------------------
#if(!dir.exists(here::here("output"))) dir.create(here::here("output"))
#tmap_save(Map1, file=here::here("output/ResidualMap.pdf"), width = 7, height = 5.5, units = "in",  dpi = 300)


## ----------------------------------------------------------------------------------------------------
#tmap_mode("view")
#Map1


## ----------------------------------------------------------------------------------------------------
mod.diff.minus2 <- lm(scale(FST.GESTE) ~ scale(NLT) + scale(C), 
               data=dd.spatial[-a,])
summary(mod.diff.minus2)


## ----fig.height=7, fig.width=8-----------------------------------------------------------------------
par(mfrow=c(2,2))
plot(mod.diff.minus2, labels.id = dd.spatial$SITE[-a])
par(mfrow=c(1,1))


## ----------------------------------------------------------------------------------------------------
cor(dd.df[, c("RA", "He", "Size", "NLT", "C", "D")],
    use="pairwise.complete")


## ----------------------------------------------------------------------------------------------------
mod.RA <- lm(scale(RA) ~ scale(NLT) + scale(C), data = dd.spatial)
summary(mod.RA)


## ----fig.height=7, fig.width=8-----------------------------------------------------------------------
par(mfrow=c(2,2))
plot(mod.RA, labels.id = dd.spatial$SITE)
par(mfrow=c(1,1))


## ----------------------------------------------------------------------------------------------------
mod.He <- lm(scale(He) ~ scale(NLT) + scale(C), data = dd.spatial)
summary(mod.He)


## ----fig.height=7, fig.width=8-----------------------------------------------------------------------
par(mfrow=c(2,2))
plot(mod.He, labels.id = dd.spatial$SITE)
par(mfrow=c(1,1))


## ----------------------------------------------------------------------------------------------------
cor(dd.site$He, dd.site$FST.GESTE, use = "pairwise.complete")


## ----------------------------------------------------------------------------------------------------
dd.temporal <- dd.site[dd.site$MultiYear==TRUE,]
dd.temporal.df <- sf::st_drop_geometry(dd.temporal)
cor(dd.temporal.df[, c("Fst.temp", "APE", "NLT", "C")], use="pairwise.complete")


## ----------------------------------------------------------------------------------------------------
mod.Fst.temp <- lm(scale(Fst.temp) ~ scale(APE), data=dd.temporal.df)
summary(mod.Fst.temp)

mod.Fst.temp.C <- lm(scale(Fst.temp) ~ scale(APE) + scale(C), 
                     data=dd.temporal.df)
mod.Fst.temp.NLT <- lm(scale(Fst.temp) ~ scale(APE) + scale(NLT), 
                       data=dd.temporal.df)
mod.Fst.temp.both <- lm(scale(Fst.temp) ~ scale(APE) + scale(NLT) + scale(C),  
            data=dd.temporal.df)
AIC(mod.Fst.temp, mod.Fst.temp.C, mod.Fst.temp.NLT, mod.Fst.temp.both)


## ----------------------------------------------------------------------------------------------------
res.Fst.temp <- t.test(Fst.temp ~ APE, data=dd.temporal, alternative = "less")
res.Fst.temp


## ----------------------------------------------------------------------------------------------------
effsize::cohen.d(Fst.temp ~ factor(APE), data=dd.temporal.df)


## ----------------------------------------------------------------------------------------------------
table(dd.temporal$APE[!is.na(dd.temporal.df$Fst.temp)])


## ----------------------------------------------------------------------------------------------------
pwr::pwr.t2n.test(n1=7, n2=5, d=-0.8, alternative = "less")


## ----------------------------------------------------------------------------------------------------
pwr::pwr.t.test(power = 0.8, d = -0.8, alternative = "less")
pwr::pwr.t.test(power = 0.8, d = -0.5, alternative = "less")


## ----message=FALSE, warning=TRUE, include=FALSE------------------------------------------------------
LandGenCourse::detachAllPackages()

