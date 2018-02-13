\name{Dianthus}
\docType{data}
\alias{Dianthus}
\title{Patch-level data for Carthusian pink (\emph{Dianthus carthusianorum})}
\description{
  Site-level data from Rico et al. 2014.
}
\usage{Dianthus}
\format{A SpatialPointsDataFrame with 65 populations and 31 variables, in the following slots:
\describe{
  \item{coords}{Spatial coordinates: \describe{
    \item{x}{Easting (Gauss-Krueger GK4)}
    \item{y}{Northing (Gauss-Krueger GK4)}
  }}
  \item{proj4string}{CRS("+init=epsg:31468")}
  \item{bbox}{Boundary box: spatial extent}

  \item{data}{Patch-level variables: \describe{
    \item{patch}{Patch ID}
    \item{samples}{Sample size}
    \item{pop09}{Population size (2009) in five categories: "0": absent; "1": 1 - 3; "2": 4 - 39; "3": 40 - 99; "4": 100 or more. In most cases, all individuals were sampled for categories "1" and "2".}
    \item{System}{Grazing system: "East", "South", "West", NA (ungrazed)}
    \item{intensity}{Grazing intensity: "Continuous": grazed 3 - 5 times throughout growing season; "Intermittent": grazed only some years or only late in season; "Ungrazed"}
    \item{Ha}{Patch size in ha}
    \item{Elements2008.4}{Number of dynamic structural element types present in 2008 (rock debris, animal burrows, ant hills, erosion)}
    \item{Dc.89}{Species presence/absence in 1989}
    \item{Dc.09}{Species presence/absence in 2009}
    \item{I}{Shannon diversity index}
    \item{Ho}{Observed heterozygosity}
    \item{He}{Expected heterozygosity}
    \item{A}{Rarefied allelic richness}
    \item{FIS}{Inbreeding coefficient}
    \item{Eu_pj}{Connectivity index Si, calculated based on Euclidean distances and using binary source patch weights (species presence/absence)}
    \item{Eu_Aj}{Connectivity index Si, calculated based on Euclidean distances and using patch weights proportional to patch size (if species present)}
    \item{Eu_Nj}{Connectivity index Si, calculated based on Euclidean distances and using patch weights proportional to the rank of PopSize (ranging from 0 - 4)}
    \item{Shecte_pj}{Connectivity index Si, calculated based on the number of continuously grazed patches between source and focal patch, using binary source patch weights (species presence/absence)}
    \item{Shecte_Aj}{Connectivity index Si, calculated based on the number of continuously grazed patches between source and focal patch, using patch weights proportional to patch size (if species present)}
    \item{Shecte_Nj}{Connectivity index Si, calculated based on the number of continuously grazed patches between source and focal patch, using patch weights proportional to the rank of PopSize (ranging from 0 - 4)}
    \item{Sheint_pj}{Connectivity index Si, calculated based on the number of continuously or intermittently grazed patches between source and focal patch, using binary source patch weights (species presence/absence)}
    \item{Sheint_Aj}{Connectivity index Si, calculated based on the number of continuously or intermittently grazed patches between source and focal patch, using patch weights proportional to patch size (if species present)}
    \item{Sheint_Nj}{Connectivity index Si, calculated based on the number of continuously or intermittently grazed patches between source and focal patch,  using patch weights proportional to the rank of PopSize (ranging from 0 - 4)}
    \item{Shenu_pj}{Connectivity index Si, calculated based on whether or not source and focal patch are in the same grazing system, using binary source patch weights (species presence/absence)}
    \item{Shenu_Aj}{Connectivity index Si, calculated based on whether or not source and focal patch are in the same grazing system, using patch weights proportional to patch size (if species present)}
    \item{Shenu_Nj}{Connectivity index Si, calculated based on whether or not source and focal patch are in the same grazing system, using patch weights proportional to the rank of PopSize (ranging from 0 - 4)}
    \item{Forest_pj}{Connectivity index Si, calculated based on Euclidean distance but only if there is no forest between source and focal patch, using binary source patch weights (species presence/absence)}
    \item{Forest_Aj}{Connectivity index Si, calculated based on Euclidean distance but only if there is no forest between source and focal patch, using patch weights proportional to patch size (if species present)}
    \item{Forest_Nj}{Connectivity index Si, calculated based on Euclidean distance but only if there is no forest between source and focal patch, using patch weights proportional to the rank of PopSize (ranging from 0 - 4)}
    \item{Longitude}{Longitude: CRS("+init=epsg:4326")}
    \item{Latitude}{Latitude: CRS("+init=epsg:4326")}
    }
}}}
\references{
Rico, Y., Holderegger, R., Boehmer, H. J. and Wagner, H. H. (2014), Directed dispersal by rotational shepherding supports landscape genetic connectivity in a calcareous grassland plant. Mol Ecol, 23: 832–842. doi:10.1111/mec.12639}
\keyword{datasets}


