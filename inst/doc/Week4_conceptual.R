## ----message=FALSE, warning=TRUE, echo=FALSE-----------------------------
require(here)
require(LandGenCourse)
require(lme4)
require(car)
require(EcoGenetics)
require(tibble)
require(vegan)
require(hierfstat)
require(QstFstComp)
source(system.file("extdata", "supplemental_R_functions.R", 
                            package = "LandGenCourse"))

## ---- echo=FALSE---------------------------------------------------------
phen <- read.delim(system.file("extdata", "WWP_phenotype_data.txt", 
                            package = "LandGenCourse"), sep = "\t", header = T)

## ---- echo=TRUE----------------------------------------------------------
mod1<- lm(phen$d13c~1+phen$block)
mod2<- lmer(d13c~1+(1|family)+block,data = phen, REML = F)
mod3 <- lmer(d13c ~ 1 + (1|population/family) + block, data = phen, REML = F)

## ---- echo=FALSE---------------------------------------------------------
cat("\n", "Model 1:", "\n")
Anova(mod1, type="III", test.statistic = "F")
cat("\n", "Model 2:", "\n")
Anova(mod2, type="III", test.statistic = "Chisq")
cat("\n", "Model 3:", "\n")
Anova(mod3, type="III", test.statistic = "Chisq")

## ---- echo=FALSE---------------------------------------------------------
aic_vals <- c(AIC(mod1), AIC(mod2), AIC(mod3))
names(aic_vals) <- c("mod1","mod2","mod3")
cat("\n", "AIC values:", "\n")
aic_vals

## ---- echo=FALSE---------------------------------------------------------
aic_out <- aic_weights(aic_vals)
cat("\n", "Akaike weights:", "\n")
aic_out

## ---- echo=FALSE---------------------------------------------------------
mod3_eff <- ranef(mod3)
mod3_fam_only <- mod3_eff$family + -30.59964
mod3_all_eff <- mod3_fam_only + pop_rep(pop.eff = mod3_eff$population, 
                                        n.fam = nrow(mod3_eff$family), 
                                        fam.eff = mod3_eff$family)

## ---- echo=FALSE---------------------------------------------------------
add_var <- 4*(0.2831^2)
total_wp_var <- (0.2831^2) + (0.8509^2)
h2 <- add_var/total_wp_var
cat("\n", "Estimate of h^2:", "\n")
h2

## ---- echo=FALSE---------------------------------------------------------
h2_boot_out <- mod_boot(model = mod3, nboot = 1000)
ci_95 <- quantile(h2_boot_out, probs = c(0.025, 0.50, 0.975)) ### 95% ci.
cat("\n", "95% bootstrap confidence interval:", "\n")
ci_95
boxplot(h2_boot_out, range=5); abline(h = h2, col = "red") 
## red line: original h2 estimate for comparison to bootstrap distribution.

## ---- echo=FALSE---------------------------------------------------------
num_qst <- 0.3088^2
dem_qst <- (0.3088^2) + (8*(0.2831^2))
qst <- num_qst/dem_qst
cat("\n", "Estimate of QST:", "\n")
qst

## ---- echo=FALSE---------------------------------------------------------
qst_boot_out <- mod_boot_qst(model = mod3, nboot = 1000)
ci_95_qst <- quantile(qst_boot_out, probs = c(0.025, 0.50, 0.975)) ### 95% ci.
cat("\n", "95% bootstrap confidence interval:", "\n")
ci_95_qst
boxplot(qst_boot_out); abline(h = qst, col = "red")

## ---- echo=FALSE---------------------------------------------------------
snp <- read.delim(system.file("extdata", "WWP_SNP_genotypes.txt", 
                            package = "LandGenCourse"), sep = "\t", header = T)
               
env <- read.delim(system.file("extdata", "WWP_environmental_data.txt", 
                            package = "LandGenCourse"),sep = "\t", header = T)
trait <- mod3_all_eff
names(trait)[1] <- "d13c"

row.names(snp) <- snp$family   
row.names(env) <- env$family
trait$family <- sapply(strsplit(row.names(trait),":"), 
                              function(ls) ls[[1]])
row.names(trait) <- trait$family

WWP <- ecogen(XY = env[,3:4], P = trait, G = snp[,-c(1:2)], E = env[,-c(1:4)], S = env[,1:2], order.G = FALSE)

## ---- echo=FALSE---------------------------------------------------------
phen_env <- data.frame(d13c=scale(WWP@P[,1]), scale(WWP@XY), scale(WWP@E))
round(cor(phen_env), 2)

## ---- echo=FALSE---------------------------------------------------------
mod1_env <- lm(d13c ~ longitude + latitude + elev + max_rad + tmax_july + 
                 tmin_jan + ann_ppt + gdd_aug + AWS050, data = phen_env)
summary(mod1_env)

## ---- echo=FALSE---------------------------------------------------------
mod <- vegan::varpart(WWP@P$d13c, WWP@E, WWP@XY, transfo="standardize")
mod

## ---- echo=FALSE---------------------------------------------------------
par(mar=c(1, 1, 1, 1))
plot(mod)

## ---- echo=FALSE---------------------------------------------------------
snp_reformat <- hierfstat_convert(snp = data.frame(WWP@S,WWP@G), 
                                  ids = c(1:ncol(WWP@S)))
mono <- numeric(ncol(snp_reformat))
for (i in 1:ncol(snp_reformat)) 
{
  mono[i] <- length(table(snp_reformat[,i]))
}
snp_reformat2 <- snp_reformat[,-which(mono == 1)]

colnames(snp_reformat2) <- names(WWP@G)[-which(mono == 1)]

## ---- echo=FALSE---------------------------------------------------------
fst <- varcomp.glob(levels = WWP@S$population, loci = snp_reformat2, diploid = T)
fst$F

## ---- fig.show='hold', echo=FALSE----------------------------------------
fst_snp <- fst_persnp(vc = fst$loc, names = colnames(snp_reformat2))
het_out <- het_snp(snp=snp_reformat2, finite.cor= T, names = colnames(snp_reformat2))
plot(het_out, fst_snp)
plot(het_out/(1-fst_snp), fst_snp)

## ---- echo=FALSE---------------------------------------------------------
snp_reformat3 <- data.frame(population=WWP@S$population, snp_reformat2)
phen_mod <- phen[,-c(2,4)]
QstFst_out <- QstFstComp(fst.dat = snp_reformat3, qst.dat = phen_mod, 
                         numpops = nlevels(WWP@S$population), nsim = 10000, 
                         breeding.design = "half.sib.dam", 
                         dam.offspring.relatedness = 0.25, output = "concise")
QstFst_out[[3]]

