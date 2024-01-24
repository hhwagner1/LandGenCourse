\name{dd.genind}
\docType{data}
\alias{dd.genind}
\title{Genetic data for the freshwater snail \emph{Drepanotrema depressissimum}}
\description{
  Data used in Lamy et al. (2012). See 'dd.ecogen' for a detailed description.
}
\usage{dd.genind}
\format{A 'genind' object with 1271 individuals, 10 loci, and 372 alleles:
\describe{
    \item{tab}{1270 x 372 matrix of allele counts}
    \item{loc.n.all}{number of alleles per locus (range: 18-50)}
    \item{all.names}{list of allele names for each locus}
    \item{ploidy}{ploidy of each individual  (range: 2-2)}
    \item{type}{codominant markers}
    \item{strata}{a data frame with 4 columns ( SiteID, SITE, YEAR, Cluster )}
    \item{other}{a list : \describe{
      \item{xy}{Spatial coordinates (longitude, latitude: WGS84)}
      }}
}}
\references{
Lamy, T., Pointier, J. P., Jarne, P. and David, P. (2012), Testing metapopulation dynamics using genetic, demographic and ecological data. Molecular Ecology, 21: 1394–1410. doi:10.1111/j.1365-294X.2012.05478.x}
\keyword{datasets}


