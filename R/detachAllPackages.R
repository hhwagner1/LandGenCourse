detachAllPackages <- function() {
# Removes all packages except for basic packages.
# Function contributed by 'mjaniec' at:
# https://stackoverflow.com/questions/7505547/detach-all-packages-while-working-in-r


      basic.packages <- c("package:stats","package:graphics",
                        "package:grDevices","package:utils",
                        "package:datasets","package:methods","package:base")

    package.list <- search()[ifelse(unlist(gregexpr("package:",search()))==1,
                                    TRUE,FALSE)]
    package.list <- setdiff(package.list,basic.packages)

    if (length(package.list)>0)  for (package in package.list)
      detach(package, character.only=TRUE)

  }


