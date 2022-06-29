#' Functions to calculate the contribution
#' of the spatial correlations at the population
#' or sub population level.
#' @author Mathieu Fortin - June 2022


jarFilename <- "spatialcorrcalc-0.1.jar"


#'
#' A data.frame object to provide an example
#'
#' @docType data
#'
#' @usage data(popExample)
#'
#' @keywords datasets
#'
#' @examples
#' data(popExample)
"popExample"

#'
#' A vector of containing the IDs of the population units in sample s1
#'
#' @docType data
#'
#' @usage data(FIDs1)
#'
#' @keywords datasets
#'
#' @examples
#' data(FIDs1)
"FIDs1"

#'
#' A vector of containing the IDs of the population units in sample s2
#'
#' @docType data
#'
#' @usage data(FIDs2)
#'
#' @keywords datasets
#'
#' @examples
#' data(FIDs2)
"FIDs2"


.welcomeMessage <- function() {
  packageStartupMessage("Welcome to GHMBSpatialCorr!")
  packageStartupMessage("The GHMBSpatialCorr package makes it possible to sum the spatial correlations")
  packageStartupMessage("across a population or a subpopulation.")
  packageStartupMessage("Please, make sure that Java (version 8 or later) is installed on your computer.")
  packageStartupMessage("For more information, visit https://github.com/CWFC-CCFB/GHMBSpatialCorr/wiki.")
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
  .checkJavaObject(calculator, "spatialcorrcalc.SpatialCorrelationCalculator")
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
#' Compute the sum of the correlations amongst the residual error terms
#'
#' @param calculator the pointer to the Java instance of the SpatialCorrelationCalculator class
#' @param parm the correlation parameter (theta) of a exponential spatial correlation structure, i.e. e^{-d/theta}
#' where d is the distance between two population units
#' @param subpopulation a vector of integers (numerics will be automatically coerced to integers) that stands
#' for the FID (integers) of the units in the subpopulation. A null value means the correlations are calculated
#' across the entire population.
#' @param nbThreads the number of threads to put on the job. An integer between 1 and 8 (by default is set to 3)
#'
#' @export
getCorrelationSumForResidualErrors <- function(calculator, parm, subpopulation = NULL, nbThreads = 3) {
  .checkJavaObject(calculator, "spatialcorrcalc.SpatialCorrelationCalculator")
  nbThreads <- .checkNbThreads(nbThreads)
  if (!is.null(subpopulation)) {
    jSubpopulation <- .checkSubpopulationArgument(subpopulation)
    message("INFO: This may take a while...")
    corrSum <- calculator$getCorrelationSumForResidualError(jSubpopulation, parm, as.integer(nbThreads))
  } else {
    message("INFO: This may take a while...")
    corrSum <- calculator$getCorrelationSumForResidualError(parm, as.integer(nbThreads))
  }
  return(corrSum)
}

.checkNbThreads <- function(nbThreads) {
  if (nbThreads < 1 || nbThreads > 8) {
    return(3)
  } else {
    return(nbThreads)
  }
}

#'
#' Calculate the sums of residual error correlations between the population (or a subpopulation)
#' and the units of the sample.
#'
#' @param calculator the pointer to the Java instance of the SpatialCorrelationCalculator class
#' @param parm the correlation parameter (theta) of a exponential spatial correlation structure, i.e. e^{-d/theta}
#' where d is the distance between two population units
#' @param sample 1 for sample 1 or 2 for sample 2
#' @param subpopulation a vector of integers (numerics will be automatically coerced to integers) that stands
#' for the FID (integers) of the units in the subpopulation. A null value means the correlations are calculated
#' across the entire population.
#' @param nbThreads the number of threads to put on the job. An integer between 1 and 8 (by default is set to 3)
#'
#' @return a matrix of numerics
#'
#' @export
getCorrelationSumsForTheseUnits <- function(calculator, parm, sample = c(1,2), subpopulation = NULL, nbThreads = 3) {
  .checkJavaObject(calculator, "spatialcorrcalc.SpatialCorrelationCalculator")
  nbThreads <- .checkNbThreads(nbThreads)
  if (sample == 1) {
    jSample <- calculator$getSample1()
  } else if (sample == 2) {
    jSample <- calculator$getSample2()
  } else {
    stop("The sample argument should be either 1 or 2!")
  }
  if (!is.null(subpopulation)) {
    jSubpopulation <- .checkSubpopulationArgument(subpopulation)
    message("INFO: This may take a while...")
    corrOmegaArray <- calculator$getCorrelationSumsForTheseUnits(jSample, jSubpopulation, parm, as.integer(nbThreads))
  } else {
    message("INFO: This may take a while...")
    corrOmegaArray <- calculator$getCorrelationSumsForTheseUnits(jSample, parm, as.integer(nbThreads))
  }
  matxCorr <- as.matrix(J4R::getAllValuesFromArray(corrOmegaArray))
  return(matxCorr)
}


.checkSubpopulationArgument <- function(subpopulation) {
  if (class(subpopulation) == "numeric" || class(subpopulation) == "integer") {
    myListFIDs <- J4R::createJavaObject("java.util.ArrayList")
    myListFIDs$add(as.integer(subpopulation))
    return(myListFIDs)
  } else {
    stop("The subpopulation argument should be a vector of integers (or numerics)!")
  }
}

.checkJavaObject <- function(javaObj, class) {
  if (class(javaObj) != "java.object") {
    stop(paste("The calculator argument must be a java.object instance of the", class, "class!"))
  }
  if (javaObj$.class != class) {
    stop(paste("The calculator argument must be a java.object instance of the", class, "class!"))
  }
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

