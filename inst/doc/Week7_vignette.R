## ----message=FALSE, warning=TRUE-----------------------------------------
require(LandGenCourse)
#require(here)
#require(spdep)
#require(nlme)
#require(lattice)
#require(MuMIn)
require(ggplot2)
source(system.file("extdata", "panel.cor.r", 
                            package = "LandGenCourse"))

## ----message=FALSE, warning=TRUE-----------------------------------------
if(!require(spmoran)) install.packages("spmoran", repos='http://cran.us.r-project.org')
#require(spmoran)

## ------------------------------------------------------------------------
data(Dianthus)

## ------------------------------------------------------------------------
ggmap::qmplot(x =  Longitude, y = Latitude, data = Dianthus@data,
              source = "google", maptype = "terrain", zoom = 12)

## ----fig.height=5, fig.width=7-------------------------------------------
Dianthus.df <- data.frame(A=Dianthus@data$A, IBD=Dianthus@data$Eu_pj, 
                          IBR=Dianthus@data$Sheint_pj,
                          PatchSize=log(Dianthus@data$Ha),
                          Longitude=Dianthus@data$Longitude,
                          Latitude=Dianthus@data$Latitude,
                          x=Dianthus@coords[,1], y=Dianthus@coords[,2])

Dianthus.df <- Dianthus.df[!is.na(Dianthus.df$A),]
dim(Dianthus.df)
pairs(Dianthus.df, lower.panel=panel.smooth, upper.panel=panel.cor,
      diag.panel=panel.hist)

## ------------------------------------------------------------------------
boxplot(log(Dianthus$Ha) ~ Dianthus$pop09, ylab="PatchSize (log(Ha))",
        xlab="Population size category")

## ------------------------------------------------------------------------
round(matrix(cor(Dianthus@data$A, Dianthus@data[,15:29], 
                 use="pairwise.complete.obs"), 5, 3, byrow=TRUE, 
           dimnames=list(c("Eu", "Shecte", "Sheint", "Shenu", "Forest"), 
                         c("pj", "Aj", "Nj"))),3)

## ------------------------------------------------------------------------
mod.lm.IBD <- lm(A ~ IBD, data = Dianthus.df)
summary(mod.lm.IBD)

## ------------------------------------------------------------------------
mod.lm.IBR <- lm(A ~ IBR, data = Dianthus.df)
summary(mod.lm.IBR)

## ----fig.height=5.5, fig.width=7-----------------------------------------
par(mfrow=c(2,2), mar=c(4,4,2,1))
plot(mod.lm.IBR)
par(mfrow=c(1,1))

## ------------------------------------------------------------------------
mod.lm.PatchSize <- lm(A ~ PatchSize + IBR, data = Dianthus.df)
summary(mod.lm.PatchSize)

## ----fig.height=5.5, fig.width=7-----------------------------------------
par(mfrow=c(2,2), mar=c(4,4,2,1))
plot(mod.lm.PatchSize)
par(mfrow=c(1,1))

## ------------------------------------------------------------------------
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

## ------------------------------------------------------------------------
spdep::moran.test(Dianthus.df$A, listw.gab)             

## ------------------------------------------------------------------------
spdep::moran.test(Dianthus.df$IBD, listw.gab)

## ------------------------------------------------------------------------
spdep::moran.test(Dianthus.df$IBR, listw.gab) 

## ------------------------------------------------------------------------
spdep::moran.test(Dianthus.df$PatchSize, listw.gab) 

## ------------------------------------------------------------------------
spdep::lm.morantest(mod.lm.IBD, listw.gab) 

## ------------------------------------------------------------------------
spdep::lm.morantest(mod.lm.IBR, listw.gab)          

## ------------------------------------------------------------------------
spdep::lm.morantest(mod.lm.PatchSize, listw.gab)       

## ------------------------------------------------------------------------
model.lm <- nlme::gls(A ~ IBR + PatchSize, data = Dianthus.df)
summary(model.lm)
semivario <- nlme::Variogram(model.lm, form = ~x  + y, resType = "normalized")

## ------------------------------------------------------------------------
ggplot(data=semivario, aes(x=dist, y=variog)) + geom_point() + geom_smooth(se=FALSE) +
  geom_hline(yintercept=1) + ylim(c(0,1.3)) + xlab("Distance") + ylab("Semivariance")

## ------------------------------------------------------------------------
mod.corExp <- nlme::gls( A ~ PatchSize + IBR, data = Dianthus.df, 
                            correlation = nlme::corExp(form = ~ x + y, nugget=T))

mod.corGaus <- nlme::gls( A ~ PatchSize + IBR, data = Dianthus.df, 
                            correlation = nlme::corGaus(form = ~ x + y, nugget=T))

mod.corSpher <- nlme::gls( A ~ PatchSize + IBR, data = Dianthus.df, 
                            correlation = nlme::corSpher(form = ~ x + y, nugget=T))

#mod.corLin <- nlme::gls( A ~ PatchSize + IBR, data = Dianthus.df, 
#                            correlation = nlme::corLin(form = ~ x + y, nugget=T))

mod.corRatio <- nlme::gls( A ~ PatchSize + IBR, data = Dianthus.df, 
                            correlation = nlme::corRatio(form = ~ x + y, nugget=T))

## ------------------------------------------------------------------------
MuMIn::model.sel(model.lm, mod.corExp, mod.corGaus, 
                 mod.corSpher, mod.corRatio)     

## ------------------------------------------------------------------------
summary(mod.corExp)  

## ------------------------------------------------------------------------
summary(lm(A ~ fitted(mod.corExp), data = Dianthus.df))$r.squared
summary(mod.lm.PatchSize)$r.squared

## ----fig.height=3, fig.width=7-------------------------------------------
par(mfrow=c(1,2), mar=c(4,4,1,1))
plot(fitted(mod.corExp), residuals(mod.corExp))
abline(h=0,lty=3)

qqnorm(residuals(mod.corExp), main="")
qqline(residuals(mod.corExp))

## ------------------------------------------------------------------------
semivario <- nlme::Variogram(mod.corExp, form = ~ x + y, 
                             resType = "normalized")
plot(semivario, smooth = TRUE)

## ------------------------------------------------------------------------
Fitted.variog <- nlme::Variogram(mod.corExp)
class(Fitted.variog)
plot(Fitted.variog)

## ------------------------------------------------------------------------
head(Fitted.variog)

## ------------------------------------------------------------------------
str(Fitted.variog)

## ------------------------------------------------------------------------
tibble::as.tibble(attr(Fitted.variog, "modelVariog"))

## ------------------------------------------------------------------------
ggplot(data=Fitted.variog, aes(x=dist, y=variog)) + geom_point() + 
  ylim(c(0,1.3)) + xlab("Distance") + ylab("Semivariance") + 
  geom_line(data=attr(Fitted.variog, "modelVariog"), aes(x=dist, y=variog), color="blue")

## ------------------------------------------------------------------------
mod.sar.IBR.gab <- spdep::errorsarlm(A ~ PatchSize + IBR, data = Dianthus.df, 
                                 listw = listw.gab)
mod.sar.IBR.d1 <- spdep::errorsarlm(A ~ PatchSize + IBR, data = Dianthus.df, 
                                 listw = listw.d1)
mod.sar.IBR.d2 <- spdep::errorsarlm(A ~ PatchSize + IBR, data = Dianthus.df, 
                                 listw = listw.d2)

MuMIn::model.sel(mod.lm.IBR, mod.sar.IBR.gab, mod.sar.IBR.d1, mod.sar.IBR.d2) 

## ------------------------------------------------------------------------
summary(mod.sar.IBR.d1, Nagelkerke = TRUE)

## ------------------------------------------------------------------------
# lm model: using truncated distance matrix (max of min spanning tree distance)
meig <- spmoran::meigen(coords=xy)
sfd.res <- spmoran::esf( y=Dianthus.df$A, x=Dianthus.df[,c("PatchSize", "IBR")],
                       meig=meig, fn = "r2" )

## ------------------------------------------------------------------------
sfd.res$b

## ------------------------------------------------------------------------
sfd.res$r

## ------------------------------------------------------------------------
sfd.res$e

## ------------------------------------------------------------------------
cmat.d1    <- spdep::listw2mat( listw.d1) 
meigw  <- spmoran::meigen( cmat = cmat.d1 )
sfw.res <- spmoran::esf( y=Dianthus.df$A, x=Dianthus.df[,c("PatchSize", "IBR")],
                       meig=meigw, fn = "r2" )
sfw.res$b
tibble::as.tibble(sfw.res$r)
sfw.res$e

## ----fig.height=5, fig.width=7-------------------------------------------
SF <- data.frame(xy, sf=meigw$sf)
names(SF) <- gsub("[.]", "", names(SF))
sp::coordinates(SF) <- ~ x + y
sp::spplot(SF, row.names(sfw.res$r), colorkey = TRUE)

## ----fig.height=3, fig.width=7-------------------------------------------
SF@data$sf <- sfw.res$sf
SF@data$A <- scale(Dianthus.df$A, scale = FALSE)
sp::spplot(SF, c("sf", "A"), colorkey = TRUE)

## ------------------------------------------------------------------------
cor(Dianthus.df$A, data.frame(sfd=sfd.res$sf, sfw=sfw.res$sf))

## ------------------------------------------------------------------------
sfr.res <- spmoran::resf( y=Dianthus.df$A, x=Dianthus.df[,c("PatchSize", "IBR")], 
               meig = meigw, method = "reml" ) 
sfr.res$b
tibble::as.tibble(sfr.res$r)
sfr.res$e
sfr.res$s

## ------------------------------------------------------------------------
rv_res <- spmoran::resf_vc( y=Dianthus.df$A, 
                            x = Dianthus.df[,c("PatchSize", "IBR")], 
                            xconst = NULL, meig = meigw, method = "reml" )

## ------------------------------------------------------------------------
summary( rv_res$b_vc ) 

## ------------------------------------------------------------------------
summary( rv_res$p_vc )

## ------------------------------------------------------------------------
Result <- data.frame(Dianthus.df, b=rv_res$b_vc, p=rv_res$p_vc)
names(Result)

## ----fig.show='hold'-----------------------------------------------------
require(ggplot2)
ggplot(as.data.frame(Result), aes(x, y, size=PatchSize)) +
  geom_point(color="darkblue") + coord_fixed()
ggplot(as.data.frame(Result), aes(x, y, col=p.PatchSize < 0.05, size=b.PatchSize)) +
  geom_point() + coord_fixed()

## ----fig.show='hold'-----------------------------------------------------
require(ggplot2)
ggplot(as.data.frame(Result), aes(x, y, size=IBR)) +
  geom_point(color="darkgreen") + coord_fixed()
ggplot(as.data.frame(Result), aes(x, y, col=p.IBR < 0.05, size=b.IBR)) +
  geom_point() + coord_fixed()

## ------------------------------------------------------------------------
rv_res <- spmoran::resf_vc( y=Dianthus.df$A, 
                            x = Dianthus.df[,c("IBR")], 
                            xconst = NULL, meig = meigw, method = "reml" )
summary( rv_res$b_vc ) 

## ------------------------------------------------------------------------
summary( rv_res$p_vc ) 

## ----fig.height=6, fig.width=7-------------------------------------------
Result <- data.frame(Dianthus.df, b=rv_res$b_vc, p=rv_res$p_vc, resid=rv_res$resid)
names(Result)

ggmap::qmplot(x=Longitude, y=Latitude, data=Result,
              source = "google", maptype = "terrain", zoom = 12,
              col=p.V1 < 0.05, size=b.V1, mapcolor = "bw")

## ----message=FALSE, warning=TRUE, include=FALSE--------------------------
LandGenCourse::detachAllPackages()

