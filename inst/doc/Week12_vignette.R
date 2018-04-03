## ----packages global_options, include=TRUE, results="hide", message=FALSE, warning=FALSE----
library(LandGenCourse)
library(ggplot2)
#require(lme4)
#require(usdm)

## ------------------------------------------------------------------------
CSFdata <- read.csv(system.file("extdata", "CSF_network.csv", 
                                        package = "LandGenCourse"))
head(CSFdata) 

## ------------------------------------------------------------------------
str(CSFdata)

## ------------------------------------------------------------------------
Zl <- lapply(c("pop1","pop2"), function(nm) 
  Matrix:::fac2sparse(CSFdata[[nm]], "d", drop=FALSE))
ZZ <- Reduce("+", Zl[-1], Zl[[1]])

## ------------------------------------------------------------------------
mod1 <- lme4::lFormula(logDc.km ~ solarinso + forest + ag + shrub + 
                       dev + (1|pop1), data = CSFdata, REML = TRUE)

## ------------------------------------------------------------------------
head(CSFdata)

## ------------------------------------------------------------------------
solarinsoz <- scale(CSFdata$solarinso, center=TRUE, scale=TRUE)

## ------------------------------------------------------------------------
CSFdata <- cbind(CSFdata, solarinsoz)
names(CSFdata)

## ------------------------------------------------------------------------
CSF.df <- with(CSFdata, data.frame(solarinsoz, forest, dev, shrub, ag))
usdm::vif(CSF.df)

## ------------------------------------------------------------------------
usdm::vif(cbind(CSF.df, grass=CSFdata$grass))

## ------------------------------------------------------------------------
cor(CSF.df, CSFdata$grass)

## ------------------------------------------------------------------------
Zl <- lapply(c("pop1","pop2"), function(nm) 
  Matrix:::fac2sparse(CSFdata[[nm]], "d", drop=FALSE))
ZZ <- Reduce("+", Zl[-1], Zl[[1]])

## ------------------------------------------------------------------------
mod_1 <- lme4::lFormula(logDc.km ~ solarinsoz + forest + ag + shrub + 
                        dev + (1|pop1), data = CSFdata, REML = TRUE)

## ------------------------------------------------------------------------
mod_1$reTrms$Zt <- ZZ

## ------------------------------------------------------------------------
dfun <- do.call(lme4::mkLmerDevfun, mod_1)
opt <- lme4::optimizeLmer(dfun)
mod1 <- lme4::mkMerMod(environment(dfun), opt, mod_1$reTrms, fr = mod_1$fr)
summary(mod1)

## ------------------------------------------------------------------------
plot(mod1) 

## ------------------------------------------------------------------------
hist(residuals(mod1)) 

## ------------------------------------------------------------------------
MLPE <- function(variables, data) {
  mod2 <- lme4::lFormula(variables, data = data, REML = TRUE)
  dfun <- do.call(lme4::mkLmerDevfun, mod2)
  opt <- lme4::optimizeLmer(dfun)
  mod_2 <- lme4::mkMerMod(environment(dfun), opt, mod2$reTrms,fr = mod2$fr)
  mod2$reTrms$Zt <- ZZ

# Refit the model
  dfun <- do.call(lme4::mkLmerDevfun, mod2)
  opt <- lme4::optimizeLmer(dfun)
  modelout <- lme4::mkMerMod(environment(dfun), opt, mod2$reTrms,fr = mod2$fr)
  return(modelout)
}

## ------------------------------------------------------------------------
mod2 <- MLPE(logDc.km ~ forest + ag + shrub + dev + (1|pop1), CSFdata)
mod3 <- MLPE(logDc.km ~ ag + dev + (1|pop1), CSFdata)
mod4 <- MLPE(logDc.km ~ slope + shrub + dev + (1|pop1), CSFdata)
mod5 <- MLPE(logDc.km ~ soils + slope + solarinsoz + (1|pop1), CSFdata)

## ------------------------------------------------------------------------
# Alternative code HW:
Models <- list(Full=mod1, Landcover=mod2, HumanFootprint=mod3, 
               EnergyConservation=mod4, Historical=mod5)
CSF.IC <- data.frame(extractAIC = sapply(Models, extractAIC)[2,],
                     BIC = sapply(Models, BIC)) 
CSF.IC

## ------------------------------------------------------------------------
CSF.IC <- data.frame(CSF.IC, k = sapply(Models, function(ls) attr(logLik(ls), "df")))
CSF.IC

## ------------------------------------------------------------------------
CSF.IC$AICc <- CSF.IC$extractAIC + 2*CSF.IC$k*(CSF.IC$k+1)/(48-CSF.IC$k-1)
CSF.IC

## ------------------------------------------------------------------------
AICcmin <- min(CSF.IC$AICc)
RL <- exp(-0.5*(CSF.IC$AICc - AICcmin))
sumRL <- sum(RL)
CSF.IC$AICcmin <- RL/sumRL

## ------------------------------------------------------------------------
BICmin <- min(CSF.IC$BIC)
RL.B <- exp(-0.5*(CSF.IC$BIC - BICmin))
sumRL.B <- sum(RL.B)
CSF.IC$BICew <- RL.B/sumRL.B
round(CSF.IC,3)

## ------------------------------------------------------------------------
#summary(mod2)
summary(Models$Landcover)

## ------------------------------------------------------------------------
#confint(mod2, level = 0.95, method = "Wald")
confint(Models$Landcover, level = 0.95, method = "Wald")

## ----message=FALSE, warning=TRUE, include=FALSE--------------------------
LandGenCourse::detachAllPackages()

