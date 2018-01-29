## ----message=FALSE, warning=TRUE-----------------------------------------
require(LandGenCourse)
require(lme4)
#require(EcoGenetics)
require(tibble)
#require(lattice)
#require(MuMIn)
#require(predictmeans)
#require(nlme)
#require(QstFstComp)
#require(car)  

#require(ggeffects)     # Not automatically installed with 'LandGenCourse'

source(system.file("extdata", "supplemental_R_functions.R", 
                            package = "LandGenCourse"))
source(system.file("extdata", "panel.cor.r", 
                            package = "LandGenCourse"))

## ------------------------------------------------------------------------
phen <- read.delim(system.file("extdata", "WWP_phenotype_data.txt", 
                            package = "LandGenCourse"), sep = "\t", header = T)
tibble::as.tibble(phen)

## ------------------------------------------------------------------------
phen$family <- as.factor(phen$family)
phen$block <- as.factor(phen$block)
sapply(phen, class)

## ------------------------------------------------------------------------
mod1 <- lm(d13c ~ block, data = phen)
mod2 <- lme4::lmer(d13c ~ 1 + (1|family) + block,data = phen, REML = TRUE)
mod3 <- lme4::lmer(d13c ~ 1 + (1|population/family) + block, 
                   data = phen, REML = TRUE)
mod4 <- lme4::lmer(d13c ~ 1 + (1|population) + (1|family) + block, 
                   data = phen, REML = TRUE)

## ------------------------------------------------------------------------
aic_vals <- c(AIC(mod1), extractAIC(mod2)[2], extractAIC(mod3)[2], 
              extractAIC(mod4)[2])
names(aic_vals) <- c("mod1","mod2","mod3", "mod4")
aic_vals

## ------------------------------------------------------------------------
predictmeans::residplot(mod4, group="population", level=1)


## ------------------------------------------------------------------------
marginal.residuals <- mod4@frame$d13c - predict(mod4, re.form=NA)
plot(mod4@frame$block, marginal.residuals)
predictmeans::CookD(mod4)

## ------------------------------------------------------------------------
MuMIn::r.squaredGLMM(mod3)

## ------------------------------------------------------------------------
summary(mod4)

## ------------------------------------------------------------------------
fam.var <- nlme::VarCorr(mod4)$"family"[1]
prov.var <- nlme::VarCorr(mod4)$"population"[1]
res.var <- summary(mod4)$sigma^2

Components <- data.frame(fam.var, prov.var, res.var)
Components / sum(Components)

## ------------------------------------------------------------------------
#mod.noPop <- update(mod4, .~. -(1 | population))
mod.noPop <- lmer(d13c ~ 1  + (1 | family) + block, data=phen, REML=TRUE)
mod.noFam <- lmer(d13c ~ 1  + (1 | population) + block, data=phen, REML=TRUE)
anova(mod4, mod.noPop, refit=FALSE)
anova(mod4, mod.noFam, refit=FALSE)

## ------------------------------------------------------------------------
mod.noBlock <- lmer(d13c ~ 1  + (1 | population) + (1 | family), data=phen, REML=FALSE)
mod.noFam <- lmer(d13c ~ 1  + (1 | population) + block, data=phen, REML=TRUE)
anova(mod4, mod.noBlock, refit=TRUE, REML=FALSE)

## ------------------------------------------------------------------------
mod4.ML <- lmer(d13c ~ 1  + (1 | population) + (1 | family) + block, 
                data=phen, REML=FALSE)
car::Anova(mod4.ML, type="II", test.statistic="Chisq")

## ------------------------------------------------------------------------
mod4

## ------------------------------------------------------------------------
add_var <- 4*(0.2857^2)
total_wp_var <- (0.2857^2) + (0.3295^2) + (0.8452^2)
h2 <- add_var/total_wp_var
h2

## ------------------------------------------------------------------------
one.over.relatedness <- 1/0.25
h2 <- (one.over.relatedness*Components$fam.var) / sum(Components)
h2

## ------------------------------------------------------------------------
par(mar=c(1, 2, 1, 1))
h2_boot_out <- mod_boot(model = mod4, nboot = 1000)
ci_95 <- quantile(h2_boot_out, probs = c(0.025, 0.50, 0.975))
ci_95
boxplot(h2_boot_out, range=5); abline(h = h2, col = "red") 

## ------------------------------------------------------------------------
num_qst <- 0.3295^2
dem_qst <- 0.3295^2 + (8*(0.2857^2))
qst <- num_qst/dem_qst
qst

## ------------------------------------------------------------------------
num_qst <- Components$prov.var
dem_qst <- Components$prov.var + (8*Components$fam.var)
qst <- num_qst/dem_qst
qst

## ------------------------------------------------------------------------
par(mar=c(1, 2, 1, 1))
qst_boot_out <- mod_boot_qst(model = mod3, nboot = 1000)
ci_95_qst <- quantile(qst_boot_out, probs = c(0.025, 0.50, 0.975)) 
ci_95_qst
boxplot(qst_boot_out); abline(h = qst, col = "red")

## ------------------------------------------------------------------------
data(WWP.ecogen, package="LandGenCourse")

## ------------------------------------------------------------------------
WWP.hierfstat <- EcoGenetics::ecogen2hierfstat(WWP.ecogen, pop='population', 
                                  to_numeric=TRUE, nout=1)
phen_mod <- phen[,-c(2,4)]
QstFst_out <- QstFstComp::QstFstComp(fst.dat = WWP.hierfstat, 
                                     qst.dat = phen_mod, 
                                     numpops = nlevels(WWP@S$population), 
                                     nsim = 10000, 
                                     breeding.design = "half.sib.dam",
                                     dam.offspring.relatedness = 0.25, 
                                     output = "concise_nowrite")

QstFst_out

## ------------------------------------------------------------------------
lme4::fixef(mod4)

## ------------------------------------------------------------------------
# install.packages("ggeffects")

ggeffects::ggeffect(mod4, terms=c("block"))

## ------------------------------------------------------------------------
lattice::dotplot(ranef(mod4,condVar=TRUE))

## ------------------------------------------------------------------------
prov.eff <- lme4::ranef(mod4)$population
fam.eff <- lme4::ranef(mod4)$family
prov.eff
head(fam.eff)

## ------------------------------------------------------------------------
Plot.data <- dplyr::distinct(data.frame(WWP.ecogen@E, 
                    population=WWP.ecogen@S$population))
index <- match(Plot.data$population, row.names(prov.eff))
dat2 <- data.frame(prov.eff=prov.eff[index,1], Plot.data[,-8])

## ----fig.height=7.5, fig.width=7.5---------------------------------------
pairs(dat2, lower.panel=panel.smooth, upper.panel=panel.cor,
      diag.panel=panel.hist)

## ----message=FALSE, warning=TRUE, include=FALSE--------------------------
LandGenCourse::detachAllPackages()

