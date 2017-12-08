\name{detachAllPackages}
\alias{detachAllPackages}
\title{Detach all packages}
\usage{
detachAllPackages()
}
\description{
This function detaches all packages except for a set of basic packages. This helps avoid problems with dll limits e.g. during building of vignettes. Code contributed by 'mjaniec' at:
https://stackoverflow.com/questions/7505547/detach-all-packages-while-working-in-r
}

