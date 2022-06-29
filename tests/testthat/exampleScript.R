#'
#' Example
#'

require(GHMBSpatialCorr)
data("popExample") ### the population
data("FIDs1") ### the id of the population units in sample s1
data("FIDs2") ### the id of the population units in sample s2

corrCalc <- SpatialCorrelationCalculator(30, 0.001) ### 30x30 m resolution, 0.001: distances are in km

initializeCalculator(corrCalc, popExample$FID, popExample$UTM_X, popExample$UTM_Y, FIDs1, FIDs2) ### initialize the calculator

theta <- 0.5

matOmegaSum <- getCorrelationSumForResidualErrors(corrCalc, theta)  ### return the product of 1_N^T x Omega x 1_N (see Eq. 5 in the manuscript)

matxD1 <- getCorrelationSumsForTheseUnits(corrCalc, theta, sample = 1) ### returns the product D x 1_N (see Eq. 5 in the manuscript)

shutdownClient()### shutdown the Java server
