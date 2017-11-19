\name{dd.site}
\docType{data}
\alias{dd.site}
\title{Genetic data for the freshwater snail \emph{Drepanotrema depressissimum}}
\description{
  Site-level data from Lamy et al. (2012).
}
\usage{dd.site}
\format{A data frame with 42 populations (combinations of site x year) and 22 variables:
\describe{

    \item{SiteID}{Combination of SITE and YEAR}
    \item{SITE}{Site name (abbreviated)}
    \item{YEAR}{Year of sampling}
    \item{Spatial}{Logical: population included in spatial study?}
    \item{MultiYear}{Logical: population sampled in multiple years?}
    \item{APE}{Logical: population extinct between sampling years? Based on Table 2.}
    \item{Cluster}{Cluster name for populations that belong to a cluster.}
    \item{n}{Number of individuals sampled per population.}
    \item{RA}{Allelic richness based on a sample size of 14 individuals. From Table 2.}
    \item{He}{Gene diversity (expected heterozygosity). From Table 2.}
    \item{f}{Inbreeding coefficient. From Table 2}
    \item{s}{Estimate of selfing rate based on a multilocus maximum-likelihood method. From Table 2.}
    \item{Type}{Habitat type (p = pond, r = small river and s = swamp grasslands). From Table 4.}
    \item{FST.GESTE}{Population-specific FST as returned by GESTE. From Table 4.}
    \item{Size}{Patch size: Largest diameter in meter, tranformed  with log(1 + X). From Table 4.}
    \item{V}{Vegetation cover: Fraction of site covered by aquatic vegetation (macrophytes and algae), tranformed with log(1 + X). From Table 4.}
    \item{C}{Connectivity to neighbouring sites, see paper. From Table 4.}
    \item{Stab}{Temporal stability, see paper. From Table 4.}
    \item{D}{Density of favourable habitats: Number of water bodies within a radius of 2 km. From Table 4.}
    \item{APA}{Apparent population age: Time in years since the most recent potential founder effect. From Table 4.}
    \item{NLT}{Long-term population size: Combination of observed density and site size averaged over years. From Table 4.}
    \item{Fst.temp}{Mean pairwise FST between temporal samples within site. From Table 5.}

}
\references{
Lamy, T., Pointier, J. P., Jarne, P. and David, P. (2012), Testing metapopulation dynamics using genetic, demographic and ecological data. Molecular Ecology, 21: 1394–1410. doi:10.1111/j.1365-294X.2012.05478.x}
\keyword{datasets}


