## ----message=FALSE, warning=TRUE-----------------------------------------
require(here)
require(hierfstat)
require(QstFstComp)
source(system.file("extdata", "supplemental_R_functions.R", 
                            package = "LandGenCourse"))


## ------------------------------------------------------------------------
if(!exists("WWP")) 
{
  source(system.file("doc", "Week4_vignette.R", package = "LandGenCourse"))
}

## ------------------------------------------------------------------------
snp_reformat <- hierfstat_convert(snp = data.frame(WWP@S,WWP@G), 
                                  ids = c(1:ncol(WWP@S)))

## ------------------------------------------------------------------------
mono <- numeric(ncol(snp_reformat))
for (i in 1:ncol(snp_reformat)) 
{
  mono[i] <- length(table(snp_reformat[,i]))
}
snp_reformat2 <- snp_reformat[,-which(mono == 1)]

## ------------------------------------------------------------------------
colnames(snp_reformat2) <- names(WWP@G)[-which(mono == 1)]

## ------------------------------------------------------------------------
fst <- varcomp.glob(levels = WWP@S$population, loci = snp_reformat2, diploid = T)

## ------------------------------------------------------------------------
snp_reformat3 <- data.frame(population=WWP@S$population, snp_reformat2)
phen_mod <- phen[,-c(2,4)]
QstFst_out <- QstFstComp(fst.dat = snp_reformat3, qst.dat = phen_mod, 
                         numpops = nlevels(WWP@S$population), nsim = 10000, 
                         breeding.design = "half.sib.dam", 
                         dam.offspring.relatedness = 0.25, output = "concise")
QstFst_out

## ------------------------------------------------------------------------
fst_snp <- fst_persnp(vc = fst$loc, names = colnames(snp_reformat2))

## ------------------------------------------------------------------------
het_out <- het_snp(snp=snp_reformat2, finite.cor= T, names = colnames(snp_reformat2))

## ---- fig.show='hold'----------------------------------------------------
plot(het_out, fst_snp)
plot(het_out/(1-fst_snp), fst_snp)

