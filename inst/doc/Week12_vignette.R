## ----------------------------------------------------------------------------------------------------
if(!requireNamespace("corMLPE", quietly = TRUE)) 
  remotes::install_github("nspope/corMLPE")


## ----packages global_options, include=TRUE, results="hide", message=FALSE, warning=FALSE-------------
library(LandGenCourse)
library(ggplot2)
#library(nlme)
#library(usdm)
#library(corMLPE)


## ----------------------------------------------------------------------------------------------------
CSFdata <- read.csv(system.file("extdata", "CSF_network.csv", 
                                        package = "LandGenCourse"))
head(CSFdata) 


## ----------------------------------------------------------------------------------------------------
CSFdata$solarinsoz <- scale(CSFdata$solarinso, center=TRUE, scale=TRUE)


## ----------------------------------------------------------------------------------------------------
str(CSFdata)


## ----------------------------------------------------------------------------------------------------
CSF.df <- with(CSFdata, data.frame(solarinsoz, forest, dev, shrub, ag))
usdm::vif(CSF.df)


## ----------------------------------------------------------------------------------------------------
usdm::vif(cbind(CSF.df, grass=CSFdata$grass))


## ----------------------------------------------------------------------------------------------------
cor(CSF.df, CSFdata$grass)


## ----------------------------------------------------------------------------------------------------
mod1 <- nlme::gls(logDc.km ~ solarinsoz + forest + ag + shrub + dev, 
                  correlation=corMLPE::corMLPE(form=~pop1+pop2), 
                  data=CSFdata, method="REML")


## ----------------------------------------------------------------------------------------------------
plot(mod1, abline=c(0,0)) 


## ----fig.height=4, fig.width=7-----------------------------------------------------------------------
par(mfrow=c(1,2))
hist(residuals(mod1)) 
qqnorm(residuals(mod1))
par(mfrow=c(1,1))


## ----------------------------------------------------------------------------------------------------
mod2 <- update(mod1, ~ forest + ag + shrub + dev)
mod3 <- update(mod1, ~ ag + dev)
mod4 <- update(mod1, ~ slope + shrub + dev)
mod5 <- update(mod1, ~ soils + slope + solarinsoz)


## ----------------------------------------------------------------------------------------------------
mod1noREML <- update(mod1, method="ML")
mod2noREML <- update(mod2, method="ML")
mod3noREML <- update(mod3, method="ML")
mod4noREML <- update(mod4, method="ML")
mod5noREML <- update(mod5, method="ML")


## ----------------------------------------------------------------------------------------------------
Models <- list(Full=mod1noREML, Landcover=mod2noREML, HumanFootprint=mod3noREML, 
               EnergyConservation=mod4noREML, Historical=mod5noREML)
CSF.IC <- data.frame(AIC = sapply(Models, AIC),
                     BIC = sapply(Models, BIC)) 
CSF.IC


## ----------------------------------------------------------------------------------------------------
CSF.IC <- data.frame(CSF.IC, k = sapply(Models, function(ls) attr(logLik(ls), "df")))
CSF.IC


## ----------------------------------------------------------------------------------------------------
N = nrow(CSFdata)  # Number of unique pairs
CSF.IC$AICc <- CSF.IC$AIC + 2*CSF.IC$k*(CSF.IC$k+1)/(N-CSF.IC$k-1)
CSF.IC


## ----------------------------------------------------------------------------------------------------
AICcmin <- min(CSF.IC$AICc)
RL <- exp(-0.5*(CSF.IC$AICc - AICcmin))
sumRL <- sum(RL)
CSF.IC$AICcmin <- RL/sumRL


## ----------------------------------------------------------------------------------------------------
BICmin <- min(CSF.IC$BIC)
RL.B <- exp(-0.5*(CSF.IC$BIC - BICmin))
sumRL.B <- sum(RL.B)
CSF.IC$BICew <- RL.B/sumRL.B
round(CSF.IC,3)


## ----------------------------------------------------------------------------------------------------
ModelsREML <- list(Full=mod1, Landcover=mod2, HumanFootprint=mod3,
               EnergyConservation=mod4, Historical=mod5)


## ----------------------------------------------------------------------------------------------------
confint(ModelsREML$Landcover, level = 0.95, method = "Wald")
confint(ModelsREML$HumanFootprint, level = 0.95, method = "Wald")
confint(ModelsREML$EnergyConservation, level = 0.95, method = "Wald")


## ----------------------------------------------------------------------------------------------------
# df <- df[order(df$site, df$pop1, df$pop2),]


## ----------------------------------------------------------------------------------------------------
# m1 <- gls(Genetic.distance ~ Predictor.distance, 
#           correlation = corMLPE(form = ~ pop1+pop2), data = df)
# acf(resid(m1, type='normalized')) ## Autocorrelation function


## ----------------------------------------------------------------------------------------------------
# m2 <- gls(Genetic.distance ~ Predictor.distance, 
#           correlation = corMLPE(form = ~ pop1+pop2, clusters = site), data = df)
# acf(resid(m2, type='normalized')) ## Autocorrelation function


## ----message=FALSE, warning=TRUE, include=FALSE------------------------------------------------------
LandGenCourse::detachAllPackages()

