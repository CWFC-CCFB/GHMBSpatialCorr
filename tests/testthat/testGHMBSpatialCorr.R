###################################
# Tests
# Mathieu Fortin - June 2022
###################################

require(GHMBSpatialCorr)
data("popExample") ### the population
data("FIDs1") ### the id of the population units in sample s1
data("FIDs2") ### the id of the population units in sample s2

corrCalc <- SpatialCorrelationCalculator(30, 0.001) ### 30x30 m resolution, 0.001: distances are in km

initializeCalculator(corrCalc, popExample$FID, popExample$UTM_X, popExample$UTM_Y, FIDs1, FIDs2) ### initialize the calculator

theta <- 0.5

matOmegaSum <- getCorrelationSumForResidualErrors(corrCalc, theta)  ### return the product of 1_N^T x Omega x 1_N (see Eq. 5 in the manuscript)

test_that("Testing that sum of correlations", {
  expect_equal(matOmegaSum, 84146467.4426222, tolerance = 1E-6)
})


matxD1 <- getCorrelationSumsForTheseUnits(corrCalc, theta, sample = 1) ### returns the product D x 1_N (see Eq. 5 in the manuscript)
test_that("Testing that sum of correlations", {
  expect_equal(matxD1[1], 915.0902, tolerance = 1E-4)
  expect_equal(matxD1[2], 1509.0477, tolerance = 1E-4)
  expect_equal(sum(matxD1), 119917.386517278, tolerance = 1E-8)
})

shutdownClient()### shutdown the Java server
