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
if(!require(spmoran)) install.packages("spmoran")
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
mod.gls.PatchSize <- nlme::gls(A ~ IBR + PatchSize, data = Dianthus.df)
summary(mod.gls.PatchSize)


semivario <- nlme::Variogram(mod.gls.PatchSize, form = ~x  + y, 
                             resType = "normalized")
plot(semivario, smooth = TRUE)
lattice::trellis.focus("panel", 1, 1)
lattice::panel.abline(h=1)
lattice::trellis.unfocus() 

## ------------------------------------------------------------------------
exponential.autocor <- nlme::gls( A ~ PatchSize + IBR, data = Dianthus.df, 
                            correlation = nlme::corExp(form = ~ x + y, nugget=T))

gaussian.autocor <- nlme::gls( A ~ PatchSize + IBR, data = Dianthus.df, 
                            correlation = nlme::corGaus(form = ~ x + y, nugget=T))

spherical.autocor <- nlme::gls( A ~ PatchSize + IBR, data = Dianthus.df, 
                            correlation = nlme::corSpher(form = ~ x + y, nugget=T))

#linear.autocor <- nlme::gls( A ~ PatchSize + IBR, data = Dianthus.df, 
#                            correlation = nlme::corLin(form = ~ x + y, nugget=T))

ratio.autocor <- nlme::gls( A ~ PatchSize + IBR, data = Dianthus.df, 
                            correlation = nlme::corRatio(form = ~ x + y, nugget=T))

## ------------------------------------------------------------------------
MuMIn::model.sel(mod.gls.PatchSize, exponential.autocor, gaussian.autocor, 
          spherical.autocor, ratio.autocor)     

## ------------------------------------------------------------------------
summary(exponential.autocor)  

## ------------------------------------------------------------------------
summary(lm(A ~ fitted(exponential.autocor), data = Dianthus.df))$r.squared
summary(mod.lm.PatchSize)$r.squared

## ----fig.show='hold'-----------------------------------------------------
plot(fitted(exponential.autocor), residuals(exponential.autocor))
abline(h=0,lty=3)

qqnorm(residuals(exponential.autocor))
qqline(residuals(exponential.autocor))

## ------------------------------------------------------------------------
semivario <- nlme::Variogram(exponential.autocor, form = ~ x + y, 
                             resType = "normalized")
plot(semivario, smooth = TRUE)
lattice::trellis.focus("panel", 1, 1)
lattice::panel.abline(h=1)
lattice::trellis.unfocus() 

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
e_res <- spmoran::esf( y=Dianthus.df$A, x=Dianthus.df[,c("PatchSize", "IBR")],
                       meig=meig, fn = "r2" )

## ------------------------------------------------------------------------
e_res$b
e_res$r
e_res$e

## ------------------------------------------------------------------------
cmat.d1    <- spdep::listw2mat( listw.d1) 
meigW  <- spmoran::meigen( cmat = cmat.d1 )
e_res <- spmoran::esf( y=Dianthus.df$A, x=Dianthus.df[,c("PatchSize", "IBR")],
                       meig=meigW, fn = "r2" )
e_res$b
e_res$r
e_res$e

## ------------------------------------------------------------------------
r_res <- spmoran::resf( y=Dianthus.df$A, x=Dianthus.df[,c("PatchSize", "IBR")], 
               meig = meigW, method = "reml" ) 
r_res$b
r_res$s
r_res$r
r_res$e

## ------------------------------------------------------------------------
rv_res <- spmoran::resf_vc( y=Dianthus.df$A, 
                            x = Dianthus.df[,c("PatchSize", "IBR")], 
                            xconst = NULL, meig = meigW, method = "reml" )

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
                            xconst = NULL, meig = meigW, method = "reml" )
summary( rv_res$p_vc ) 

## ------------------------------------------------------------------------
summary( rv_res$p_vc ) 

## ------------------------------------------------------------------------
Result <- data.frame(Dianthus.df, b=rv_res$b_vc, p=rv_res$p_vc)
names(Result)

ggplot(as.data.frame(Result), aes(x, y, col=p.V1 < 0.05, size=b.V1)) +
  geom_point() + coord_fixed()

## ----message=FALSE, warning=TRUE, include=FALSE--------------------------
LandGenCourse::detachAllPackages()

