
minVersion <- function(packages = c(), return=c("list", "min", "mine")) {
	# code to collate a list of minimum versions of R
	# required for the listed packages
	return(result) 
}

minVersion("RcppEigen")
# 2.15.1

minVersion(c("RcppEigen","OpenMx"), "mine")
# FALSE


# go back in time
# available versions
# ls -l /Library/Frameworks/R.framework/Versions/

ln -sfn /Library/Frameworks/R.framework/Versions/3.0 /Library/Frameworks/R.framework/Versions/Current
R --version
R --no-save
# turn off graphics and set mirror = 1
chooseCRANmirror(graphics=FALSE)

packageDescription("parallel")
# install our dependencies
# Links
install.packages("RcppEigen")
# Depends
install.packages("BH")
install.packages("digest")
install.packages("methods")
install.packages("MASS")
install.packages("StanHeaders")
install.packages("parallel")
# Suggests
install.packages("snowfall")
install.packages("roxygen2")
install.packages("mvtnorm")
install.packages("rpf")
install.packages("numDeriv")
install.packages("/Users/tim/Dropbox/shared folders/OpenMx_binaries/OpenMx2.2.3-OSX/R3.0maybe/OpenMx_2.2.3-11.tgz")

ln -sfn /Library/Frameworks/R.framework/Versions/3.2 /Library/Frameworks/R.framework/Versions/Current
R --version
R --no-save


minimumRVersion <- function(packageList)
  {
  requirements <- NULL

  for (p in packageList)
    {
    # Get dependencies for the package
    dep <- packageDescription(p, fields = "Depends")

    if (!is.na(dep))
      {
      # dep will be something like:
      # "R (>= 3.1.0), grDevices, graphics, stats, utils"
      # split by comma
      dep <- unlist(strsplit(dep, ","))
      # Find requirement for R (may not exist)
      r.dep <- dep[grep("R \\(", dep)]
      if (!length(r.dep))
          r.dep <- NA
      }
    else
          r.dep <- NA

    requirements <- c(requirements, r.dep)
    }

  requirements
  }