## ----message=FALSE, warning=TRUE-----------------------------------------
library(LandGenCourse)
#library(tibble)
#library(poppr)
#library(ade4)
#library(pwr)
#library(effsize)
#library(sp)
#library(ggmap)
#library(ggplot2)
#library(car)  

## ----message=FALSE, warning=TRUE-----------------------------------------
if(!require(EcoGenetics) || packageVersion("EcoGenetics") < "1.2.1.5")
{
  devtools::install_github("leandroroser/EcoGenetics-devel")
}
library(EcoGenetics)

## ------------------------------------------------------------------------
data(dd.ecogen, package = "LandGenCourse")
dd.ecogen
?dd.ecogen   

## ------------------------------------------------------------------------
data(dd.site, package = "LandGenCourse")
tibble::as.tibble(dd.site)
?dd.site

## ------------------------------------------------------------------------
dd.ecogen.Cluster <- dd.ecogen[!is.na(dd.ecogen[["S"]]$Cluster),]
dd.genind.Cluster <- EcoGenetics::ecogen2genind(dd.ecogen.Cluster)

## ------------------------------------------------------------------------
amova.result <- poppr::poppr.amova(dd.genind.Cluster, hier = ~ Cluster/SITE, 
                                   within=FALSE, method = "ade4")
amova.result

## ------------------------------------------------------------------------
amova.test <- ade4::randtest(amova.result, nrepet = 999)
amova.test

## ------------------------------------------------------------------------
dd.spatial <- dd.site[dd.site@data$Spatial==TRUE, ]

## ------------------------------------------------------------------------
cor(dd.spatial@data[ , c("FST.GESTE", "NLT", "C", "D")], 
    use="pairwise.complete")

## ------------------------------------------------------------------------
mod.diff <- lm(scale(FST.GESTE) ~ scale(NLT) + scale(C), 
               data=dd.spatial)
summary(mod.diff)

## ----fig.height=7, fig.width=8-------------------------------------------
par(mfrow=c(2,2))
plot(mod.diff, labels.id = names(residuals(mod.diff)))
par(mfrow=c(1,1))

## ------------------------------------------------------------------------
car::vif(mod.diff)

## ------------------------------------------------------------------------
dd.spatial@data$Residuals <- mod.diff$residuals
sp::bubble(dd.spatial, zcol = "Residuals", col = c("red", "blue"))

## ----fig.height=4.5, fig.width=7, message=FALSE--------------------------
a <- is.element(rownames(dd.spatial@data), c("32", "42"))
a2 <- c(1:nrow(dd.spatial@data))[a]
myMap <- ggmap::qmplot(Longitude, Latitude,  data = as.data.frame(dd.spatial),
              source = "stamen", maptype = "toner-lite",  
              col = sign(Residuals), size = abs(Residuals))
myMap + ggplot2::geom_text(data = as.data.frame(dd.spatial[a2,]),
                   mapping = ggplot2::aes(Longitude, Latitude, label = SITE),
                   size = 4, col = "black", vjust = 0, nudge_y = -0.015)

## ----message=FALSE-------------------------------------------------------
#require(here)
#if(!dir.exists(paste0(here(),"/output"))) dir.create(paste0(here(),"/output"))
#ggplot2::ggsave(paste0(here(),"/output/ResidualMap.png"), 
#               width = 7, height = 5.5, units = "in",  dpi = 300)

## ------------------------------------------------------------------------
mod.diff.minus2 <- lm(scale(FST.GESTE) ~ scale(NLT) + scale(C), 
               data=dd.spatial[-a2,])
summary(mod.diff.minus2)

## ----fig.height=7, fig.width=8-------------------------------------------
par(mfrow=c(2,2))
plot(mod.diff.minus2, labels.id = names(residuals(mod.diff)))
par(mfrow=c(1,1))

## ------------------------------------------------------------------------
cor(dd.spatial@data[, c("RA", "He", "Size", "NLT", "C", "D")],
    use="pairwise.complete")

## ------------------------------------------------------------------------
mod.RA <- lm(scale(RA) ~ scale(NLT) + scale(C), data = dd.spatial)
summary(mod.RA)

## ----fig.height=7, fig.width=8-------------------------------------------
par(mfrow=c(2,2))
plot(mod.RA)
par(mfrow=c(1,1))

## ------------------------------------------------------------------------
mod.He <- lm(scale(He) ~ scale(NLT) + scale(C), data = dd.spatial)
summary(mod.He)

## ----fig.height=7, fig.width=8-------------------------------------------
par(mfrow=c(2,2))
plot(mod.He)
par(mfrow=c(1,1))

## ------------------------------------------------------------------------
cor(dd.site$He, dd.site$FST.GESTE, use = "pairwise.complete")

## ------------------------------------------------------------------------
dd.temporal <- dd.site[dd.site@data$MultiYear==TRUE,]
cor(dd.temporal@data[, c("Fst.temp", "APE", "NLT", "C")], 
    use="pairwise.complete")

## ------------------------------------------------------------------------
mod.Fst.temp <- lm(scale(Fst.temp) ~ scale(APE), data=dd.temporal)
summary(mod.Fst.temp)

mod.Fst.temp.C <- lm(scale(Fst.temp) ~ scale(APE) + scale(C), 
                     data=dd.temporal)
mod.Fst.temp.NLT <- lm(scale(Fst.temp) ~ scale(APE) + scale(NLT), 
                       data=dd.temporal)
mod.Fst.temp.both <- lm(scale(Fst.temp) ~ scale(APE) + scale(NLT) + scale(C),  
            data=dd.temporal)
AIC(mod.Fst.temp, mod.Fst.temp.C, mod.Fst.temp.NLT, mod.Fst.temp.both)

## ------------------------------------------------------------------------
res.Fst.temp <- t.test(Fst.temp ~ APE, data=dd.temporal, alternative = "less")
res.Fst.temp

## ------------------------------------------------------------------------
effsize::cohen.d(Fst.temp ~ factor(APE), data=dd.temporal)

## ------------------------------------------------------------------------
table(dd.temporal$APE[!is.na(dd.temporal$Fst.temp)])

## ------------------------------------------------------------------------
pwr::pwr.t2n.test(n1=7, n2=5, d=-0.8, alternative = "less")

## ------------------------------------------------------------------------
pwr::pwr.t.test(power = 0.8, d = -0.8, alternative = "less")
pwr::pwr.t.test(power = 0.8, d = -0.5, alternative = "less")

## ----message=FALSE, warning=TRUE, include=FALSE--------------------------
LandGenCourse::detachAllPackages()

