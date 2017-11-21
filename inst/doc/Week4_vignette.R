## ----message=FALSE, warning=TRUE-----------------------------------------
require(LandGenCourse)
require(lme4)
#require(car)
require(EcoGenetics)
require(tibble)
#require(vegan)

source(system.file("extdata", "supplemental_R_functions.R", 
                            package = "LandGenCourse"))

## ------------------------------------------------------------------------
phen <- read.delim(system.file("extdata", "WWP_phenotype_data.txt", 
                            package = "LandGenCourse"), sep = "\t", header = T)
tibble::as.tibble(phen)

## ------------------------------------------------------------------------
mod1 <- lm(phen$d13c~1+phen$block)
mod2 <- lme4::lmer(d13c~1+(1|family)+block,data = phen, REML = F)
mod3 <- lme4::lmer(d13c ~ 1 + (1|population/family) + block, data = phen, REML = F)

## ------------------------------------------------------------------------
car::Anova(mod1, type="III", test.statistic = "F")
car::Anova(mod2, type="III", test.statistic = "Chisq")
car::Anova(mod3, type="III", test.statistic = "Chisq")

## ------------------------------------------------------------------------
aic_vals <- c(AIC(mod1), AIC(mod2), AIC(mod3))
names(aic_vals) <- c("mod1","mod2","mod3")
aic_vals

## ------------------------------------------------------------------------
aic_out <- aic_weights(aic_vals)
aic_out

## ------------------------------------------------------------------------
mod3_eff <- lme4::ranef(mod3)
head(mod3_eff$family)

## ------------------------------------------------------------------------
mod3_fam_only <- mod3_eff$family + -30.59964
head(mod3_fam_only)

## ------------------------------------------------------------------------
mod3_all_eff <- mod3_fam_only + pop_rep(pop.eff = mod3_eff$population, 
                                        n.fam = nrow(mod3_eff$family), 
                                        fam.eff = mod3_eff$family)
head(mod3_all_eff)

## ------------------------------------------------------------------------
snp <- read.delim(system.file("extdata", "WWP_SNP_genotypes.txt", 
                            package = "LandGenCourse"), sep = "\t", header = T)
               
env <- read.delim(system.file("extdata", "WWP_environmental_data.txt", 
                            package = "LandGenCourse"),sep = "\t", header = T)
trait <- mod3_all_eff
names(trait)[1] <- "d13c"

## ------------------------------------------------------------------------
row.names(snp) <- snp$family   
row.names(env) <- env$family
trait$family <- sapply(strsplit(row.names(trait),":"), 
                              function(ls) ls[[1]])
row.names(trait) <- trait$family

## ------------------------------------------------------------------------
WWP <- EcoGenetics::ecogen(XY = env[,3:4], P = trait, G = snp[,-c(1:2)], 
                           E = env[,-c(1:4)], S = env[,1:2], order.G = FALSE)

## ------------------------------------------------------------------------
#require(here)
#if(!dir.exists(paste0(here(),"/output"))) dir.create(paste0(here(),"/output"))
#save(WWP, file = paste0(here(), "/output/WWP.RData"))
#load(paste0(here(), "/output/WWP.RData"))

## ------------------------------------------------------------------------
mod3

## ------------------------------------------------------------------------
add_var <- 4*(0.2831^2)
total_wp_var <- (0.2831^2) + (0.8509^2)
h2 <- add_var/total_wp_var
h2

## ------------------------------------------------------------------------
par(mar=c(1, 1, 1, 1))
h2_boot_out <- mod_boot(model = mod3, nboot = 1000)
ci_95 <- quantile(h2_boot_out, probs = c(0.025, 0.50, 0.975))
ci_95
boxplot(h2_boot_out, range=5); abline(h = h2, col = "red") 

## ------------------------------------------------------------------------
num_qst <- 0.3088^2
dem_qst <- (0.3088^2) + (8*(0.2831^2))
qst <- num_qst/dem_qst

## ------------------------------------------------------------------------
par(mar=c(1, 1, 1, 1))
qst_boot_out <- mod_boot_qst(model = mod3, nboot = 1000)
ci_95_qst <- quantile(qst_boot_out, probs = c(0.025, 0.50, 0.975)) 
ci_95_qst
boxplot(qst_boot_out); abline(h = qst, col = "red")

## ------------------------------------------------------------------------
phen_env <- data.frame(d13c=scale(WWP@P[,1]), scale(WWP@XY), scale(WWP@E))

## ------------------------------------------------------------------------
round(cor(phen_env), 2)

## ------------------------------------------------------------------------
mod1_env <- lm(d13c ~ longitude + latitude + elev + max_rad + tmax_july + 
                 tmin_jan + ann_ppt + gdd_aug + AWS050, data = phen_env)
summary(mod1_env)

## ------------------------------------------------------------------------
par(mar=c(1, 1, 1, 1))
mod <- vegan::varpart(WWP@P$d13c, WWP@E, WWP@XY, transfo="standardize")
mod
plot(mod)

## ------------------------------------------------------------------------
ab <- vegan::anova.cca(vegan::rda(WWP@P$d13c, WWP@E,  transfo="standardize"))
bc <- vegan::anova.cca(vegan::rda(WWP@P$d13c, WWP@XY, transfo="standardize"))
abc <- vegan::anova.cca(vegan::rda(WWP@P$d13c, data.frame(WWP@E, WWP@XY),
                                   transfo="standardize"))
a <- vegan::anova.cca(vegan::rda(WWP@P$d13c, WWP@E, WWP@XY, transfo="standardize"))
b <- vegan::anova.cca(vegan::rda(WWP@P$d13c, WWP@XY, WWP@E, transfo="standardize"))

## ------------------------------------------------------------------------
c(ab=ab$"Pr(>F)"[1], bc=bc$"Pr(>F)"[1], abc=abc$"Pr(>F)"[1], 
  a=a$"Pr(>F)"[1], b=b$"Pr(>F)"[1])

## ----message=FALSE, warning=TRUE, include=FALSE--------------------------
detach("package:lme4", unload=TRUE)
detach("package:Matrix", unload=TRUE)
detach("package:EcoGenetics", unload=TRUE)

