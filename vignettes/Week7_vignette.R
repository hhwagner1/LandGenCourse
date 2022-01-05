## ----message=FALSE, warning=TRUE----------------------------------------------
library(LandGenCourse)
#library(here)
#libraryspdep)
library(nlme)
#library(lattice)
#library(MuMIn)
#library(gridExtra)
#library(dplyr)
library(spatialreg)
library(ggplot2)
library(ggmap)
source(system.file("extdata", "panel.cor.r", 
                            package = "LandGenCourse"))


## ----message=FALSE, warning=TRUE----------------------------------------------
if(!require(spmoran)) install.packages("spmoran", repos='http://cran.us.r-project.org')
#require(spmoran)


## -----------------------------------------------------------------------------
data(Dianthus)


## -----------------------------------------------------------------------------
Dianthus.longlat <- sp::spTransform(Dianthus, 
                       CRSobj = tmaptools::get_proj4("longlat")$proj4string)


make_bbox(lon = Longitude, lat = Latitude, data = Dianthus@data, f = 0.2)



## -----------------------------------------------------------------------------
bbox <- make_bbox(lon = Longitude, lat = Latitude, data = Dianthus@data, f = 0.2)


## ----fig.height=4, fig.width=7, message=FALSE---------------------------------
StamenMap.terrain <- ggmap(get_stamenmap(bbox, maptype = "terrain", zoom=12)) + 
  geom_point(aes(x =  Longitude, y = Latitude), data = Dianthus.longlat@data, 
             colour = "black", size = 2)

StamenMap.toner <- ggmap::ggmap(get_stamenmap(bbox, maptype = "toner", zoom=12, force = TRUE)) + 
  geom_point(aes(x =  Longitude, y = Latitude), data = Dianthus.longlat@data, 
             colour = "black", size = 2) 

gridExtra::grid.arrange(StamenMap.terrain, StamenMap.toner, nrow=1)


## ----fig.height=5, fig.width=7------------------------------------------------
Dianthus.df <- data.frame(A=Dianthus@data$A, IBD=Dianthus@data$Eu_pj, 
                          IBR=Dianthus@data$Sheint_pj,
                          PatchSize=log(Dianthus@data$Ha),
                          System=Dianthus@data$System,
                          Longitude=Dianthus@data$Longitude,
                          Latitude=Dianthus@data$Latitude,
                          x=Dianthus@coords[,1], y=Dianthus@coords[,2])

# Define 'System' for ungrazed patches
Dianthus.df$System=as.character(Dianthus@data$System)
Dianthus.df$System[is.na(Dianthus.df$System)] <- "Ungrazed"
Dianthus.df$System <- factor(Dianthus.df$System, 
                             levels=c("Ungrazed", "East", "South", "West"))

# Remove patches with missing values for A
Dianthus.df <- Dianthus.df[!is.na(Dianthus.df$A),]
dim(Dianthus.df)
pairs(Dianthus.df[,-c(5:7)], lower.panel=panel.smooth, 
      upper.panel=panel.cor, diag.panel=panel.hist)


## ----fig.height=3.5, fig.width=7----------------------------------------------

Boxplot1 <- ggplot(Dianthus.df, aes(x=System, y=A)) + 
  geom_boxplot() + xlab("Grazing system") + ylab("Allelic richness (A)") +
  geom_jitter(shape=1, position=position_jitter(0.1), col="blue")

Boxplot2 <- ggplot(Dianthus@data, aes(x=factor(pop09), y=log(Ha))) + 
  geom_boxplot() + xlab("Population size class") + ylab("PatchSize (log(Ha))") +
  geom_jitter(shape=1, position=position_jitter(0.1), col="blue")

gridExtra::grid.arrange(Boxplot1, Boxplot2, nrow=1)


## -----------------------------------------------------------------------------
round(matrix(cor(Dianthus@data$A, Dianthus@data[,15:29], 
                 use="pairwise.complete.obs"), 5, 3, byrow=TRUE, 
           dimnames=list(c("Eu", "Shecte", "Sheint", "Shenu", "Forest"), 
                         c("pj", "Aj", "Nj"))),3)


## -----------------------------------------------------------------------------
mod.lm.IBD <- lm(A ~ IBD, data = Dianthus.df)
summary(mod.lm.IBD)


## -----------------------------------------------------------------------------
mod.lm.IBR <- lm(A ~ IBR, data = Dianthus.df)
summary(mod.lm.IBR)


## ----fig.height=5.5, fig.width=7----------------------------------------------
par(mfrow=c(2,2), mar=c(4,4,2,1))
plot(mod.lm.IBR)
par(mfrow=c(1,1))


## -----------------------------------------------------------------------------
mod.lm.PatchSize <- lm(A ~ PatchSize + IBR, data = Dianthus.df)
summary(mod.lm.PatchSize)


## ----fig.height=5.5, fig.width=7----------------------------------------------
par(mfrow=c(2,2), mar=c(4,4,2,1))
plot(mod.lm.PatchSize)
par(mfrow=c(1,1))


## -----------------------------------------------------------------------------
xy <- data.matrix(Dianthus.df[,c("x", "y")])
nb.gab <- spdep::graph2nb(spdep::gabrielneigh(xy), sym=TRUE)
par(mar=c(0,0,0,0))
plot(nb.gab, xy)
listw.gab <- spdep::nb2listw(nb.gab)

dlist <- spdep::nbdists(nb.gab, xy)
dlist <- lapply(dlist, function(x) 1/x)
listw.d1 <- spdep::nb2listw(nb.gab, style = "W", glist=dlist)
dlist <- lapply(dlist, function(x) 1/x^2)
listw.d2 <- spdep::nb2listw(nb.gab, style = "W", glist=dlist)


## -----------------------------------------------------------------------------
spdep::moran.test(Dianthus.df$A, listw.gab)             

## -----------------------------------------------------------------------------
spdep::moran.test(Dianthus.df$IBD, listw.gab)

## -----------------------------------------------------------------------------
spdep::moran.test(Dianthus.df$IBR, listw.gab) 

## -----------------------------------------------------------------------------
spdep::moran.test(Dianthus.df$PatchSize, listw.gab) 


## -----------------------------------------------------------------------------
spdep::lm.morantest(mod.lm.IBD, listw.gab) 

## -----------------------------------------------------------------------------
spdep::lm.morantest(mod.lm.IBR, listw.gab)          

## -----------------------------------------------------------------------------
spdep::lm.morantest(mod.lm.PatchSize, listw.gab)       


## -----------------------------------------------------------------------------
model.lm <- nlme::gls(A ~ IBR + PatchSize, data = Dianthus.df, method="REML")
semivario <- nlme::Variogram(model.lm, form = ~x  + y, resType = "normalized")


## -----------------------------------------------------------------------------
ggplot(data=semivario, aes(x=dist, y=variog)) + geom_point() + geom_smooth(se=FALSE) +
  geom_hline(yintercept=1) + ylim(c(0,1.3)) + xlab("Distance") + ylab("Semivariance")


## -----------------------------------------------------------------------------
model.lm <- nlme::gls(A ~ IBR + PatchSize, data = Dianthus.df, method="REML")

mod.corExp <- update(model.lm, correlation = nlme::corExp(form = ~ x + y, nugget=T))
mod.corGaus <- update(model.lm, correlation = nlme::corGaus(form = ~ x + y, nugget=T))
mod.corSpher <- update(model.lm, correlation = nlme::corSpher(form = ~ x + y, nugget=T))
mod.corRatio <- update(model.lm, correlation = nlme::corRatio(form = ~ x + y, nugget=T))
#mod.corLin <- update(model.lm, correlation = nlme::corLin(form = ~ x + y, nugget=T))


## -----------------------------------------------------------------------------
MuMIn::model.sel(model.lm, mod.corExp, mod.corGaus, mod.corSpher, mod.corRatio)     


## -----------------------------------------------------------------------------
mod.corExp.ML <- nlme::gls( A ~ PatchSize + IBR, data = Dianthus.df, method="ML",
                            correlation = nlme::corExp(form = ~ x + y, nugget=T))
car::Anova(mod.corExp.ML) 


## -----------------------------------------------------------------------------
summary(lm(A ~ fitted(mod.corExp), data = Dianthus.df))$r.squared
summary(mod.lm.PatchSize)$r.squared


## -----------------------------------------------------------------------------
predictmeans::residplot(mod.corExp)


## -----------------------------------------------------------------------------
semivario <- nlme::Variogram(mod.corExp, form = ~ x + y, 
                             resType = "normalized")
plot(semivario, smooth = TRUE)


## -----------------------------------------------------------------------------
Fitted.variog <- nlme::Variogram(mod.corExp)
class(Fitted.variog)
class(plot(Fitted.variog))
plot(Fitted.variog)


## -----------------------------------------------------------------------------
head(Fitted.variog)


## -----------------------------------------------------------------------------
str(Fitted.variog)


## -----------------------------------------------------------------------------
tibble::as_tibble(attr(Fitted.variog, "modelVariog"))


## -----------------------------------------------------------------------------
ggplot(data=Fitted.variog, aes(x=dist, y=variog)) + geom_point() + 
  ylim(c(0,1.3)) + xlab("Distance") + ylab("Semivariance") + 
  geom_line(data=attr(Fitted.variog, "modelVariog"), aes(x=dist, y=variog), color="blue") +
  geom_hline(yintercept=1,linetype="dashed")


## -----------------------------------------------------------------------------
mod.lme.corExp <- nlme::lme( A ~ PatchSize + IBR, 
                             random = ~ 1 | System, data = Dianthus.df, 
                            correlation = nlme::corExp(form = ~ x + y, nugget=T),
                            method="REML")
summary(mod.lme.corExp)


## -----------------------------------------------------------------------------
MuMIn::r.squaredGLMM(mod.lme.corExp)


## -----------------------------------------------------------------------------
MuMIn::model.sel(model.lm, mod.corExp, mod.corRatio, mod.lme.corExp)  


## -----------------------------------------------------------------------------
mod.sar.IBR.gab <- spatialreg::errorsarlm(A ~ PatchSize + IBR, data = Dianthus.df, 
                                 listw = listw.gab)
mod.sar.IBR.d1 <- spatialreg::errorsarlm(A ~ PatchSize + IBR, data = Dianthus.df, 
                                 listw = listw.d1)
mod.sar.IBR.d2 <- spatialreg::errorsarlm(A ~ PatchSize + IBR, data = Dianthus.df, 
                                 listw = listw.d2)

MuMIn::model.sel(mod.lm.IBR, mod.sar.IBR.gab, mod.sar.IBR.d1, mod.sar.IBR.d2) 


## -----------------------------------------------------------------------------
summary(mod.sar.IBR.d1, Nagelkerke = TRUE)


## -----------------------------------------------------------------------------
# lm model: using truncated distance matrix (max of min spanning tree distance)
meig <- spmoran::meigen(coords=xy)
sfd.res <- spmoran::esf( y=Dianthus.df$A, x=Dianthus.df[,c("PatchSize", "IBR")],
                       meig=meig, fn = "r2" )


## -----------------------------------------------------------------------------
sfd.res$b


## -----------------------------------------------------------------------------
sfd.res$r


## -----------------------------------------------------------------------------
sfd.res$e


## -----------------------------------------------------------------------------
cmat.d1    <- spdep::listw2mat( listw.d1) 
meigw  <- spmoran::meigen( cmat = cmat.d1 )
sfw.res <- spmoran::esf( y=Dianthus.df$A, x=Dianthus.df[,c("PatchSize", "IBR")],
                       meig=meigw, fn = "r2" )
sfw.res$b
tibble::as_tibble(sfw.res$r)
sfw.res$e


## ----fig.height=5, fig.width=7------------------------------------------------
SF <- data.frame(xy, sf=meigw$sf)
names(SF) <- gsub("[.]", "", names(SF))
sp::coordinates(SF) <- ~ x + y
sp::spplot(SF, row.names(sfw.res$r), colorkey = TRUE)


## ----fig.height=3, fig.width=7------------------------------------------------
SF@data$sf <- sfw.res$sf
SF@data$A <- scale(Dianthus.df$A, scale = FALSE)
sp::spplot(SF, c("sf", "A"), colorkey = TRUE)


## -----------------------------------------------------------------------------
cor(Dianthus.df$A, data.frame(sfd=sfd.res$sf, sfw=sfw.res$sf))


## -----------------------------------------------------------------------------
sfr.res <- spmoran::resf( y=Dianthus.df$A, x=Dianthus.df[,c("PatchSize", "IBR")], 
               meig = meigw, method = "reml" ) 
sfr.res$b
tibble::as_tibble(sfr.res$r)
sfr.res$e
sfr.res$s


## -----------------------------------------------------------------------------
rv_res <- spmoran::resf_vc( y=Dianthus.df$A, 
                            x = Dianthus.df[,c("PatchSize", "IBR")], 
                            xconst = NULL, meig = meigw, method = "reml")


## -----------------------------------------------------------------------------
summary( rv_res$b_vc ) 


## -----------------------------------------------------------------------------
summary( rv_res$p_vc )


## -----------------------------------------------------------------------------
Result <- data.frame(Dianthus.df, b=rv_res$b_vc, p=rv_res$p_vc)
names(Result)


## ----fig.show='hold'----------------------------------------------------------
require(ggplot2)
ggplot(as.data.frame(Result), aes(x, y, size=PatchSize)) +
  geom_point(color="darkblue") + coord_fixed()
ggplot(as.data.frame(Result), aes(x, y, col=p.PatchSize < 0.05, size=b.PatchSize)) +
  geom_point() + coord_fixed()


## ----fig.show='hold'----------------------------------------------------------
require(ggplot2)
ggplot(as.data.frame(Result), aes(x, y, size=IBR)) +
  geom_point(color="darkgreen") + coord_fixed()
ggplot(as.data.frame(Result), aes(x, y, col=p.IBR < 0.05, size=b.IBR)) +
  geom_point() + coord_fixed()


## -----------------------------------------------------------------------------
rv_res <- spmoran::resf_vc( y=Dianthus.df$A, 
                            x = Dianthus.df[,c("IBR")], 
                            xconst = NULL, meig = meigw, method = "reml")
summary( rv_res$b_vc ) 


## -----------------------------------------------------------------------------
summary( rv_res$p_vc ) 


## ----fig.height=6, fig.width=7, message=FALSE---------------------------------
Result <- data.frame(Dianthus.df, b=rv_res$b_vc, p=rv_res$p_vc, resid=rv_res$resid)
names(Result)

ggmap::ggmap(get_stamenmap(bbox, maptype = "terrain", color="bw", force=TRUE, zoom=12)) + 
   geom_point(aes(x =  Longitude, y = Latitude, col=p.V1 < 0.05, size=b.V1), data = Result) 
              


## ----message=FALSE------------------------------------------------------------
library(dplyr)

# Dataset with variables 'flower.density' and 'mom.isolation' for each mom:
Moms <- read.csv(system.file("extdata",
                            "pulsatilla_momVariables.csv", 
                            package = "LandGenCourse"))

# Dataset with spatial coordinates of individuals:
Pulsatilla <- read.csv(system.file("extdata",
                            "pulsatilla_genotypes.csv", 
                            package = "LandGenCourse"))
Adults <- Pulsatilla %>% filter(OffID == 0)

# Combine data
Moms <- left_join(Moms, Adults[,1:5])

# Remove replicate flowers sampled from the same mother
Moms <- Moms %>% filter(OffID == 0)


## ----message=FALSE, warning=TRUE, include=FALSE-------------------------------
LandGenCourse::detachAllPackages()

