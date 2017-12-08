## ----message=FALSE, warning=TRUE-----------------------------------------
require(LandGenCourse)
require(EcoGenetics)
require(gstudio)
#require(vegan)
#require(dplyr)
#require(pwr)
#require(effsize)

## ------------------------------------------------------------------------
data(dd.ecogen, package = "LandGenCourse")
dd.ecogen
?dd.ecogen   

## ------------------------------------------------------------------------
data(dd.site, package = "LandGenCourse")
tibble::as.tibble(dd.site)
?dd.site

## ------------------------------------------------------------------------
dd.gstudio <- EcoGenetics::ecogen2gstudio(dd.ecogen)
dd.gstudio.Cluster <- dd.gstudio[!is.na(dd.gstudio$Cluster),]
D <- gstudio::dist_amova(dd.gstudio.Cluster)

## ------------------------------------------------------------------------
vegan::adonis(as.dist(D) ~ Cluster/SiteID, data=dd.gstudio.Cluster)
vegan::adonis(as.dist(sqrt(D)) ~ Cluster/SiteID, data=dd.gstudio.Cluster)

## ------------------------------------------------------------------------
cor(dd.site@data[dd.site@data$Spatial==TRUE, c("FST.GESTE", "NLT", "C", "D")], 
    use="pairwise.complete")

## ------------------------------------------------------------------------
mod.diff <- lm(scale(FST.GESTE) ~ scale(NLT) + scale(C), 
               data=dd.site[dd.site$Spatial==TRUE,])
summary(mod.diff)

## ----fig.height=7, fig.width=8-------------------------------------------
par(mfrow=c(2,2))
plot(mod.diff, labels.id = names(residuals(mod.diff)))

## ------------------------------------------------------------------------


## ------------------------------------------------------------------------
mod.diff.minus2 <- lm(scale(FST.GESTE) ~ scale(NLT) + scale(C), 
               data=dd.site[dd.site$Spatial==TRUE & dd.site$SiteID!= "DESB2009"
                            & dd.site$SiteID!= "PTC2009",])
summary(mod.diff.minus2)

## ----fig.height=7, fig.width=8-------------------------------------------
par(mfrow=c(2,2))
plot(mod.diff.minus2, labels.id = names(residuals(mod.diff)))

## ------------------------------------------------------------------------
cor(dd.site@data[dd.site@data$Spatial==TRUE, 
                 c("RA", "He", "Size", "NLT", "C", "D")], use="pairwise.complete")

## ------------------------------------------------------------------------
mod.RA <- lm(scale(RA) ~ scale(NLT) + scale(C), 
               data=dd.site[dd.site$Spatial==TRUE,])
summary(mod.RA)

## ----fig.height=7, fig.width=8-------------------------------------------
par(mfrow=c(2,2))
plot(mod.RA)

## ------------------------------------------------------------------------
mod.He <- lm(scale(He) ~ scale(NLT) + scale(C), 
               data=dd.site[dd.site$Spatial==TRUE,])
summary(mod.He)

## ----fig.height=7, fig.width=8-------------------------------------------
par(mfrow=c(2,2))
plot(mod.He)

## ------------------------------------------------------------------------
cor(dd.site@data[dd.site@data$Spatial==TRUE, c("s", "f", "APA", "NLT", "C")], 
    use="pairwise.complete")

## ------------------------------------------------------------------------
mod.s <- lm(scale(s) ~ scale(APA), data=dd.site[dd.site$Spatial==TRUE,])
summary(mod.s)

mod.s.C <- lm(scale(s) ~ scale(APA) + scale(C),  
            data=dd.site[dd.site$Spatial==TRUE,])
mod.s.NLT <- lm(scale(s) ~ scale(APA) + scale(NLT),  
            data=dd.site[dd.site$Spatial==TRUE,])
mod.s.both <- lm(scale(s) ~ scale(APA) + scale(NLT) + scale(C),  
            data=dd.site[dd.site$Spatial==TRUE,])
AIC(mod.s, mod.s.C, mod.s.NLT, mod.s.both)

## ------------------------------------------------------------------------
summary(mod.s.both)

## ----fig.height=7, fig.width=8-------------------------------------------
par(mfrow=c(2,2))
plot(mod.s.both)

## ------------------------------------------------------------------------
cor(dd.site@data[dd.site@data$MultiYear==TRUE, c("Fst.temp", "APE", "NLT", "C")], 
    use="pairwise.complete")

## ------------------------------------------------------------------------
mod.Fst.temp <- lm(scale(Fst.temp) ~ scale(APE), data=dd.site[dd.site$MultiYear==TRUE
                                                              & dd.site$Spatial==TRUE,])
summary(mod.Fst.temp)

mod.Fst.temp.C <- lm(scale(Fst.temp) ~ scale(APE) + scale(C),  
            data=dd.site[dd.site$MultiYear==TRUE & dd.site$Spatial==TRUE,])
mod.Fst.temp.NLT <- lm(scale(Fst.temp) ~ scale(APE) + scale(NLT),  
            data=dd.site[dd.site$MultiYear==TRUE & dd.site$Spatial==TRUE,])
mod.Fst.temp.both <- lm(scale(Fst.temp) ~ scale(APE) + scale(NLT) + scale(C),  
            data=dd.site[dd.site$MultiYear==TRUE & dd.site$Spatial==TRUE,])
AIC(mod.Fst.temp, mod.Fst.temp.C, mod.Fst.temp.NLT, mod.Fst.temp.both)

## ------------------------------------------------------------------------
res.Fst.temp <- t.test(Fst.temp ~ APE, data=dd.site, alternative = "less")
res.Fst.temp

## ------------------------------------------------------------------------
effsize::cohen.d(Fst.temp ~ factor(APE), data=dd.site[!is.na(dd.site$Fst.temp),])


## ------------------------------------------------------------------------
table(dd.site@data$APE[!is.na(dd.site@data$Fst.temp)])

## ------------------------------------------------------------------------
pwr::pwr.t2n.test(n1=8, n2=4, d=-0.8, alternative = "less")

## ------------------------------------------------------------------------
pwr::pwr.t.test( power=0.8, d=-0.8, alternative = "less")

## ----message=FALSE, warning=TRUE, include=FALSE--------------------------


## ----message=FALSE, warning=TRUE, include=FALSE--------------------------
#detach("package:gstudio", unload=TRUE)
#detach("package:EcoGenetics", unload=TRUE)
LandGenCourse::detachAllPackages()

