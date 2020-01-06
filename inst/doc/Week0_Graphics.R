## -----------------------------------------------------------------------------
summary(iris)

## -----------------------------------------------------------------------------
library(ggplot2)
library(RColorBrewer)

## -----------------------------------------------------------------------------
x <- c(2, 3, 6, 3, 2, 4)
names(x) <- c("Bob", "Alice", "Jane", "Fred", "Barney", "Lucy")
x

## -----------------------------------------------------------------------------
barplot(x, xlab = "People", ylab = "Thinn-A-Ma-Jigs")

## ----warning=FALSE,message=FALSE----------------------------------------------
library(ggplot2)
df <- data.frame(Thinn_A_Ma_Jigs=x,names=names(x))
ggplot( df, aes(x=names,y=Thinn_A_Ma_Jigs)) + geom_bar(stat="identity")

## -----------------------------------------------------------------------------
h <- hist(iris$Sepal.Length)

## -----------------------------------------------------------------------------
names(h)

## -----------------------------------------------------------------------------
h

## -----------------------------------------------------------------------------
ggplot( iris, aes(x=Sepal.Length)) + geom_bar()

## -----------------------------------------------------------------------------
x <- hist( iris$Sepal.Length,xlab="Sepal Length (cm)", 
           ylab="Count", main="", col="lightblue")

## -----------------------------------------------------------------------------
ggplot( iris, aes(x=Sepal.Length)) + geom_bar(fill="lightblue") + 
  xlab("Sepal Length (cm)") + ylab("Count")

## -----------------------------------------------------------------------------
d <- density( iris$Sepal.Length) 
attributes(d)

## -----------------------------------------------------------------------------
plot( d, col = "red", lwd = 2, 
      xlab = "Value of X", ylab = "Frequency", 
      bty = "n", main = "Density Plot")

## -----------------------------------------------------------------------------
ggplot( iris, aes(x=Sepal.Length)) + geom_density()

## ----eval=FALSE---------------------------------------------------------------
#  by( data, grouping, function )

## -----------------------------------------------------------------------------
by( iris$Sepal.Length, iris$Species, mean)

## -----------------------------------------------------------------------------
d.setosa <- iris$Sepal.Length[ iris$Species=="setosa" ]
d.versicolor <- iris$Sepal.Length[ iris$Species=="versicolor" ]
d.virginica <- iris$Sepal.Length[ iris$Species=="virginica" ]
d.se <- density( d.setosa )
d.ve <- density( d.versicolor )
d.vi <- density( d.virginica )

## -----------------------------------------------------------------------------
plot(d.se,xlim=c(4,8),col="red", lwd=2, bty="n", 
     xlab="Sepal Length (cm)", main="Sepal Lengths")
lines( d.ve, xlim=c(4,8), col="green",lwd=2, bty="n")
lines( d.vi, xlim=c(4,8), col="blue", lwd=2, bty="n")
legend( 6.5,1.1,c("I. setosa", "I. versicolor", "I. virginica"), 
        col=c("red","green","blue"), lwd=2,bty="n")

## -----------------------------------------------------------------------------
ggplot(iris, aes(Sepal.Length,color=Species)) + geom_density()

## -----------------------------------------------------------------------------
ggplot(iris, aes(Sepal.Length,fill=Species)) + geom_density( alpha=0.8)

## -----------------------------------------------------------------------------
breaks <- seq(4,8,by=0.2)
h.se <- hist(d.setosa, breaks=breaks, plot = FALSE)
h.ve <- hist(d.versicolor, breaks=breaks, plot=FALSE)
h.vi <- hist(d.virginica, breaks=breaks, plot=FALSE)
vals <- rbind( h.se$counts, h.ve$counts, h.vi$counts )
rownames(vals) <- levels(iris$Species)
colnames(vals) <- breaks[1:20]
vals

## -----------------------------------------------------------------------------
barplot(vals,xlab="Sepal Length", ylab="Frequency")

## -----------------------------------------------------------------------------
barplot(vals,xlab="Sepal Length", ylab="Frequency", col=c("red","green","blue"), beside=TRUE)
legend(60, 10, c("I. setosa", "I. versicolor", "I. virginica"), fill = c("red", "green", "blue"), bty="n")

## -----------------------------------------------------------------------------
ggplot( iris, aes(x=Sepal.Length, fill=Species)) + geom_histogram()

## -----------------------------------------------------------------------------
ggplot( iris, aes(x=Sepal.Length, fill=Species)) + geom_histogram(position="dodge")

## -----------------------------------------------------------------------------
boxplot(Sepal.Length ~ Species, data=iris, notch=TRUE, 
        xlab="Species", ylab="Sepal Length (cm)", 
        frame.plot=FALSE)

## -----------------------------------------------------------------------------
ggplot(iris, aes(x=Species, y=Sepal.Length)) + 
  geom_boxplot(notch = TRUE) + 
  xlab("Iris Species") + ylab("Sepal Width (cm)")

## -----------------------------------------------------------------------------
plot( iris$Sepal.Length, iris$Sepal.Width, 
      pch=as.numeric(iris$Species), bty="n", 
      xlab="Sepal Length (cm)", ylab="Sepal Width (cm)")
legend( 6.75, 4.3, c("I. setosa", "I. versicolor", "I. virginica"), 
        pch=1:3, bty="n")

## -----------------------------------------------------------------------------
plot(1:25,1:25,pch=1:25, bty="n", xlab="pch", ylab="pch")

## -----------------------------------------------------------------------------
ggplot(iris,aes(x=Sepal.Length,y=Sepal.Width, shape=Species)) + geom_point()

## -----------------------------------------------------------------------------
ggplot( iris, aes(x=Sepal.Width, y=Sepal.Length ) ) + 
  geom_point() + facet_grid(Species~.) + 
  xlab("Sepal Width (cm)") + ylab("Sepal Length")

## -----------------------------------------------------------------------------
names( par() )

## -----------------------------------------------------------------------------
par()$mfrow

## -----------------------------------------------------------------------------
par(mfrow=c(2,2))
plot( lm( Sepal.Length ~ Sepal.Width, data=iris))

## -----------------------------------------------------------------------------
length(colors())

## -----------------------------------------------------------------------------
colors()[ sample.int( length(colors()), size=20) ]

## -----------------------------------------------------------------------------
colors <- c("royalblue1", "orange1", "green3")
cols <- colors[ iris$Species ]
plot( Sepal.Width ~ Sepal.Length, data=iris, 
      col=cols, xlab="Sepal Length (cm)", ylab="Sepal Width (cm)", 
      bty="n", pch=16)

## -----------------------------------------------------------------------------
rainbow(10)

## -----------------------------------------------------------------------------
library(RColorBrewer)
display.brewer.all()

## -----------------------------------------------------------------------------
p <- ggplot(iris,aes(x=Sepal.Length,y=Sepal.Width, color=Species)) + geom_point() 
p <- p + xlab("Sepal Length (cm)") + ylab("Sepal Width (cm)")
p + scale_color_brewer(type="div", palette=6)

## ----eval=FALSE---------------------------------------------------------------
#  png( filename = "MyCoolGraphic.png", quality=300, bg="transparent", width=1080, height=1080)
#  ggplot( df, aes(x=PredictorVar, y=ResponseVar)) + geom_point() + stat_smooth(method="loess") + theme_bw()
#  dev.off()

## ----eval=FALSE---------------------------------------------------------------
#  ggplot( df, aes(x=PredictorVar, y=ResponseVar)) + geom_point() + stat_smooth(method="loess") + theme_bw()
#  dev.copy(device=png,file="MyDevCopyGraphic.png")
#  dev.off()

## ----eval=FALSE---------------------------------------------------------------
#  ggsave(filename = default_name(plot), plot = last_plot(),
#    device = default_device(filename), path = NULL, scale = 1,
#    width = par("din")[1], height = par("din")[2], units = c("in", "cm",
#    "mm"), dpi = 300, limitsize = TRUE, ...)

## ----message=FALSE, warning=TRUE, include=FALSE-------------------------------
LandGenCourse::detachAllPackages()

