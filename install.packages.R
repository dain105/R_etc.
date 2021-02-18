# How to install R package in internal network where internet access is not available
# create folder for packages
mainDir <- "C:/Users/SAMSUNG/Desktop"
subDir <- "r-pkg"
dir.create(file.path(mainDir, subDir), showWarnings = FALSE)

# install function
getPackages <- function(packs){
  packages <- unlist(
# Find (recursively) dependencies or reverse dependencies of packages.
    tools::package_dependencies(packs, available.packages(),
                                which=c("Depends", "Imports"), recursive=TRUE)
    
  )
  packages <- union(packs, packages)
  return(packages)
}

# dependencies or reverse dependencies of packages.
packages <- getPackages(c("raster"))

#download
download.packages(packages, destdir=file.path(mainDir, subDir))
