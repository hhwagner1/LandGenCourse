## ----message=FALSE, warning=TRUE-----------------------------------------
#require(adegenet)
require(LandGenCourse)
#require(pegas)       
#require(sp)
#require(PopGenReport)
require(dplyr)
require(poppr) 

## ------------------------------------------------------------------------
data(ralu.loci, package="LandGenCourse")
Frogs <- data.frame(FrogID = paste(substr(ralu.loci$Pop, 1, 3), 
                                   row.names(ralu.loci), sep="."), ralu.loci)
Frogs.genind <- adegenet::df2genind(X=Frogs[,c(4:11)], sep=":", ncode=NULL, 
                          ind.names= Frogs$FrogID, loc.names=NULL, 
                          pop=Frogs$Pop, NA.char="NA", ploidy=2, 
                          type="codom", strata=NULL, hierarchy=NULL)
Frogs.genind

## ------------------------------------------------------------------------
summary(Frogs.genind)

## ------------------------------------------------------------------------
round(pegas::hw.test(Frogs.genind, B = 1000), digits = 3)

## ------------------------------------------------------------------------
# Chi-squared test: p-value
HWE.test <- data.frame(sapply(seppop(Frogs.genind), 
                              function(ls) pegas::hw.test(ls, B=0)[,3]))
HWE.test.chisq <- t(data.matrix(HWE.test))
{cat("Chi-squared test (p-values):", "\n")
round(HWE.test.chisq,3)}

## ------------------------------------------------------------------------
# Monte Carlo: p-value
HWE.test <- data.frame(sapply(seppop(Frogs.genind), 
                              function(ls) pegas::hw.test(ls, B=1000)[,4]))
HWE.test.MC <- t(data.matrix(HWE.test))
{cat("MC permuation test (p-values):", "\n")
round(HWE.test.MC,3)}

## ------------------------------------------------------------------------
alpha=0.05
Prop.loci.out.of.HWE <- data.frame(Chisq=apply(HWE.test.chisq<alpha, 2, mean), 
           MC=apply(HWE.test.MC<alpha, 2, mean))
Prop.loci.out.of.HWE             # Type this line again to see results table

## ------------------------------------------------------------------------
Prop.pops.out.of.HWE <- data.frame(Chisq=apply(HWE.test.chisq<alpha, 1, mean), 
           MC=apply(HWE.test.MC<alpha, 1, mean))
Prop.pops.out.of.HWE             

## ------------------------------------------------------------------------

Chisq.fdr <- matrix(p.adjust(HWE.test.chisq,method="fdr"), 
                    nrow=nrow(HWE.test.chisq))
MC.fdr <- matrix(p.adjust(HWE.test.MC, method="fdr"), 
                    nrow=nrow(HWE.test.MC))

Prop.pops.out.of.HWE <- data.frame(Chisq=apply(HWE.test.chisq<alpha, 1, mean), 
           MC=apply(HWE.test.MC<alpha, 1, mean),
           Chisq.fdr=apply(Chisq.fdr<alpha, 1, mean),
           MC.fdr=apply(MC.fdr<alpha, 1, mean))
Prop.pops.out.of.HWE             

## ---- fig.show='hold'----------------------------------------------------
poppr::ia(Frogs.genind, sample=199)
LD.pair <- poppr::pair.ia(Frogs.genind)
LD.pair

## ------------------------------------------------------------------------
# Null alleles: depends on method! See help file.
Null.alleles <- PopGenReport::null.all(Frogs.genind)
Null.alleles$homozygotes$probability.obs

## ------------------------------------------------------------------------
{cat(" summary1 (Chakraborty et al. 1994):", "\n")
round(Null.alleles$null.allele.freq$summary1,2)} 

## ------------------------------------------------------------------------
{cat("summary2 (Brookfield et al. 1996):", "\n")
round(Null.alleles$null.allele.freq$summary2,2)}   

## ------------------------------------------------------------------------
Sum <- summary(Frogs.genind)
names(Sum)

## ---- fig.show='hold'----------------------------------------------------
par(mar=c(5.5, 4.5,1,1))
barplot(Sum$pop.n.all, las=3, 
       xlab = "", ylab = "Number of alleles")
plot(Sum$n.by.pop, Sum$pop.n.all, 
       xlab = "Sample size", ylab = "Number of alleles")
abline(lm(Sum$pop.n.all ~ Sum$n.by.pop), col = "red")

## ------------------------------------------------------------------------
Richness <- PopGenReport::allel.rich(Frogs.genind, min.alleles = NULL)
Richness$alleles.sampled

## ---- fig.show='hold'----------------------------------------------------
par(mar=c(5.5, 4.5,1,1))
barplot(Richness$mean.richness, las=3, ylab="Rarefied allelic richness (Ar)")
plot(colMeans(Richness$pop.sizes), Richness$mean.richness,
     xlab="Valid sample size", 
     ylab="Rarefied allelic richness (Ar)")
abline(lm(Richness$mean.richness ~ colMeans(Richness$pop.sizes)), col="red")

## ------------------------------------------------------------------------
  Sum <- summary(Frogs.genind)
  names(Sum)

## ---- fig.show='hold', fig.height=3, fig.width=4-------------------------
  par(mar=c(3, 4.5,1,1))
  barplot(Sum$Hexp, ylim=c(0,1), ylab="Expected heterozygosity")
  barplot(Sum$Hobs, ylim=c(0,1), ylab="Observed heterozygosity")

## ------------------------------------------------------------------------
  Hobs <- t(sapply(seppop(Frogs.genind), function(ls) summary(ls)$Hobs))
  Hexp <- t(sapply(seppop(Frogs.genind), function(ls) summary(ls)$Hexp))
  {cat("Expected heterozygosity (Hexp):", "\n")
  round(Hexp, 2)
  cat("\n", "Observed heterozygosity (Hobs):", "\n")
  round(Hobs, 2)}
  # 1 - Hobs/Hexp

## ---- fig.show='hold'----------------------------------------------------
  par(mar=c(5.5, 4.5, 1, 1))
  Hobs.pop <- apply(Hobs, MARGIN = 1, FUN = mean)
  Hexp.pop <- apply(Hexp, 1, mean) 
  barplot(Hexp.pop, ylim=c(0,1), las=3, ylab="Expected heterozygosity")
  barplot(Hobs.pop, ylim=c(0,1), las=3, ylab="Observed heterozygosity")

## ------------------------------------------------------------------------
Frogs.diversity <- data.frame(Pop = names(Hobs.pop),
                              n = Sum$n.by.pop,
                              Hobs = Hobs.pop,
                              Hexp = Hexp.pop,
                              Ar = Richness$mean.richness)
Frogs.diversity

## ------------------------------------------------------------------------
#require(here)
#if(!dir.exists(paste0(here(),"/output"))) dir.create(paste0(here(),"/output"))
#save(Frogs.diversity, file = paste0(here(),"/output/Frogs.diversity.RData"))
#load(paste0(here(),"/output/Frogs.diversity.RData"))

## ------------------------------------------------------------------------
Frogs.genpop <- adegenet::genind2genpop(Frogs.genind)

## ------------------------------------------------------------------------
Freq <- adegenet::makefreq(Frogs.genpop)
round(Freq[1:6,1:10], 2)

## ------------------------------------------------------------------------
apply(Freq, MARGIN = 1, FUN = sum)    # Just checking

## ----message=FALSE, warning=TRUE, include=FALSE--------------------------
LandGenCourse::detachAllPackages()

