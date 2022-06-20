#' Functions to calculate the contribution
#' of the spatial correlations at the population
#' or sub population level.
#' @author Mathieu Fortin - June 2022


jarFilename <- "spatialcorrcalc-0.1.jar"

.welcomeMessage <- function() {
  packageStartupMessage("Welcome to GHMBSpatialCorr!")
  packageStartupMessage("The GHMBSpatialCorr package makes it possible to sum the spatial correlations")
  packageStartupMessage("across a population or a subpopulation.")
  packageStartupMessage("Please, make sure that Java (version 8 or later) is installed on your computer.")
  packageStartupMessage("For more information, visit https://github.com/CWFC-CCFB/GHMB_SpatialCorr/wiki.")
}

.onAttach <- function(libname, pkgname) {
  .welcomeMessage()
}

.getLibraryPath <- function(packageName, myJavaLibrary) {
  filename <- system.file("inst", myJavaLibrary, package = packageName)
  if (file.exists(filename)) {
    filePath <- filename
  }
  else {
    filename <- system.file(myJavaLibrary, package = packageName)
    if (file.exists(filename)) {
      filePath <- filename
    }
    else {
      filePath <- NULL
    }
  }
  return(filePath)
}

.loadLibrary <- function() {
  path <- .getLibraryPath("GHMBSpatialCorr", jarFilename)
  J4R::connectToJava(extensionPath = path)
  if (!J4R::checkIfClasspathContains(jarFilename)) {
    stop(paste("It seems J4R has not been able to load the", jarFilename, "library."))
  }
}

#'
#' Constructor of the SpatialCorrelationCalculator
#'
#' @param rasterResolutionM the raster resolution  (e.g. 30 m for Landsat-8)
#' @param scaleFactor a scaling factor for the correlation function (i.e. 0.001 if the function is based on km)
#' @return a pointer to a Java object of the class SpatialCorrelationCalculator
#'
#' @export
SpatialCorrelationCalculator <- function(rasterResolutionM, scaleFactor) {
  if (!J4R::isConnectedToJava()) {
    .loadLibrary()
  } else if (!J4R::checkIfClasspathContains(jarFilename)) {
    stop(paste("Java is running but the library", jarFilename, "is not part of the classpath! Please shutdown Java through the shutdownClient method first!"))
  }
  corrCalc <- J4R::createJavaObject("spatialcorrcalc.SpatialCorrelationCalculator", rasterResolutionM, scaleFactor)
  return(corrCalc)
}

#'
#' Initialize the calculator
#'
#' @param calculator the pointer to the Java instance of the SpatialCorrelationCalculator class
#' @param FIDs a vector containing the ids of the population units. These ids must be integers.
#' @param UTM_X a vector of UTM longitude
#' @param UTM_Y a vector of UTM latitude
#' @param sample1 a vector of integers standing for the ids of the units in sample 1
#' @param sample2 a vector of integers standing for the ids of the units in sample 2
#' @return nothing
#'
#' @export
initializeCalculator <- function(calculator, FIDs, UTM_X, UTM_Y, sample1, sample2) {
  message("Adding population units. This may take a while...")
  calculator$addPopulationUnit(as.integer(FIDs), UTM_X, UTM_Y)
  listSample1 <- J4R::createJavaObject("java.util.ArrayList")
  listSample1$add(as.integer(sample1))
  listSample2 <- J4R::createJavaObject("java.util.ArrayList")
  listSample2$add(as.integer(sample2))
  calculator$setSample1(listSample1)
  calculator$setSample2(listSample2)
  message(calculator$initializeCalculator())
}




#'
#' Shut down the Java server
#'
#' This method overrides the original function of the J4R package. It only adds
#' a call to the clearCache function before calling the original function of
#' the J4R package.
#'
#' @examples
#' \dontrun{
#' shutdownJava()}
#'
#' @export
shutdownJava <- function() {
  J4R::shutdownJava()
}


#'
#' Shut down the Java server
#'
#' This method overrides the original function of the J4R package. It only adds
#' a call to the clearCache function before calling the original function of
#' the J4R package.
#'
#' @examples
#' \dontrun{
#' shutdownClient()}
#'
#' @export
shutdownClient <- function() {
  J4R::shutdownClient()
}

