\name{WWP.ecogen}
\docType{data}
\alias{WWP.ecogen}
\title{Genetic, environmental and trait data for Western white pine (\emph{Pinus monticola})}
\description{
  See Maloney et al. (2016) for details of data collection. The phenological trait data are available in external file "WWP_phenotype_data.txt" in package 'LandGenCourse'. The following model was fitted to estimate the heritable d13C trait for mother trees: lmer(d13c ~ 1 + (1 | population) + (1 | family) + block).
}
\usage{WWP.ecogen}
\format{An 'ecogen' object with 157 observations and the following slots:
\describe{
 \item{XY}{Spatial coordinates:
  \describe{
   \item{longitude}{Latitude}
   \item{latitude}{Longitude}
  }}
  \item{G}{Genotypic data for 160 SNP loci unrelated to phenotypic trait (4 monomorphic loci were removed)
  }
  \item{A}{\describe{Allele frequencies derived from slot @G}
  }
  \item{S}{Structures:
   \describe{
    \item{population}{Sampled populations (n = 10)}
    \item{plot}{For each population, trees were sampled within 3 plots (n = 30)}
    \item{family}{Mother trees from which seeds were grown in common garden experiment (n = 157)}
  }}
  \item{P}{Estimates of heritable d13C trait from seedlings grown in common garden experiment:
   \describe{
    \item{Intercept}{Global intercept of LMM (block 1)}
    \item{prov.eff}{Population effect, estimated from LMM}
    \item{fam.eff}{Family effect, estimated from LMM}
  }}
}}
\references{
Maloney, P.E.; Eckert, A.J.; Vogler, D.R.; Jensen, C.E.; Delfino Mix, A.; Neale, D.B. Landscape Biology of Western White Pine: Implications for Conservation of a Widely-Distributed Five-Needle Pine at Its Southern Range Limit. Forests 2016, 7, 93.}
