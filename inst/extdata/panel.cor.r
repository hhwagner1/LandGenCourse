panel.cor <- function(x, y, digits=2, cex.cor){
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- cor(x, y, use="pairwise.complete.obs")
  txt <- format(c(r, 0.123456789), digits=digits)[1]
  test <- cor.test(x,y)
  Signif <- round(test$p.value,3)##ifelse(round(test$p.value,3)<0.05,"p < 0.05",paste("p =",round(test$p.value,3)))  
  if(test$p.value<0.05 & txt>0.3){
      text(.5, .75, paste("r =",txt), cex=1.8)
      text(0.5, 0.25, Signif, cex=1.8)
  }
  else{
      text(.5, .75, paste("r =",txt), cex=1)
      text(0.5, 0.25, Signif, cex=1)
  }      
}

panel.smooth<-function (x, y, col = "blue", bg = NA, pch = 18, 
                        cex = 0.8, col.smooth = "red", span = 2/3, iter = 3, ...) 
{
  points(x, y, pch = pch, col = col, bg = bg, cex = cex)
  ok <- is.finite(x) & is.finite(y)
  if (any(ok)) 
    lines(stats::lowess(x[ok], y[ok], f = span, iter = iter), 
          col = col.smooth, ...)
}

panel.hist <- function(x, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1.5) )
  h <- hist(x, plot = FALSE)
  breaks <- h$breaks; nB <- length(breaks)
  y <- h$counts; y <- y/max(y)
  rect(breaks[-nB], 0, breaks[-1], y, col="cyan", ...)
}
