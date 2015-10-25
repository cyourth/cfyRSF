#' Variance (Population)
#'
#' This function compute the variance of a population x.
#' @param x: a numeric vector.
#' @param na.rm: logical value indicating whether NA values should be stripped before the computation proceeds.
#' @keywords variance
#' @export
#' @examples
#' popData <- c(4, 6, 5, 5, 6, 4, 3, 2, 5, 5, 7, 8, 9, 1, 2)
#' pop.var(popData)
#' pop.var(popData, na.rm = TRUE)
#' @return
#' numeric value: indicating the variance of the population.
pop.var <- function(x, na.rm = FALSE) {
    meanX <- mean(x, na.rm = na.rm)
    sum((x - meanX)^2) / length(x)
}


#' Standard Deviation (Population)
#'
#' This function compute the standard deviation of a population x.
#' @param x: a numeric vector.
#' @param na.rm: logical value indicating whether NA values should be stripped before the computation proceeds.
#' @keywords standard deviation
#' @export
#' @examples
#' popData <- c(4, 6, 5, 5, 6, 4, 3, 2, 5, 5, 7, 8, 9, 1, 2)
#' pop.sd(popData)
#' pop.sd(popData, na.rm = TRUE)
#' @return
#' numeric value: indicating the standard deviation of the population.
pop.sd <- function(x, na.rm = FALSE) {
    meanX <- mean(x, na.rm = na.rm)
    varX <- sum((x - meanX)^2) / length(x)
    sqrt(varX)
}


#' Coefficient of Variation (Population)
#'
#' This function compute the coefficient of variation of a population x.
#' \itemize{
#'    \item compare variation of two or more datasets
#'    \item smaller cofvar indicates a smaller variation
#'    \item ratio of SD to Mean (SD / Mean)
#' }
#' @param dataX: population (numeric vector).
#' @param meanX: mean of x (numeric) - defaults to NULL.
#' @param sdX: standard deviation of x (numeric) - defaults to NULL.
#' @param percentage: convert the results to a percentage (TRUE, FALSE) - defaults to FALSE.
#' @keywords coefficient variation
#' @export
#' @examples
#' popData <- c(4, 6, 5, 5, 6, 4, 3, 2, 5, 5, 7, 8, 9, 1, 2)
#' pop.covar(popData)
#' pop.covar(dataX = popData)
#' pop.covar(meanX = 5.3, sdX = 0.25, percentage = TRUE)
#' @return
#' numeric value: indicating the Coefficient of Variation of the population.
pop.covar <- function(dataX, meanX = NULL, sdX = NULL, percentage = FALSE) {
    covarX <- NULL
    if (!is.null(meanX) && !is.null(sdX)) {
        covarX <- if (percentage) {
            (sdX / meanX) * 100
        } else {
            (sdX / meanX)
        }
    } else {
        meanX <- mean(dataX)
        sdX <- sqrt(sum((dataX - meanX)^2) / length(dataX))
        covarX <- if (percentage) {
            (sdX / meanX) * 100
        } else {
            (sdX / meanX)
        }
    }
    covarX
}


#' Coefficient of Variation (Sample)
#'
#' This function compute the coefficient of variation of a sample x.
#' \itemize{
#'    \item compare variation of two or more datasets
#'    \item smaller cofvar indicates a smaller variation
#'    \item ratio of SD to Mean (SD / Mean)
#' }
#' @param dataX: sample (numeric vector).
#' @param meanX: mean of x (numeric) - defaults to NULL.
#' @param sdX: standard deviation of x (numeric) - defaults to NULL.
#' @param percentage: convert the results to a percentage (TRUE, FALSE) - defaults to FALSE.
#' @keywords coefficient variation
#' @export
#' @examples
#' popData <- c(4, 6, 5, 5, 6, 4, 3, 2, 5, 5, 7, 8, 9, 1, 2)
#' sample.covar(popData)
#' sample.covar(dataX = popData)
#' sample.covar(meanX = 5.3, sdX = 0.25, percentage = TRUE)
#' @return
#' numeric value: indicating the Coefficient of Variation of the sample.
sample.covar <- function(dataX, meanX = NULL, sdX = NULL, percentage = FALSE) {
    covarX <- NULL
    if (!is.null(meanX) && !is.null(sdX)) {
        covarX <- if (percentage) {
            (sdX / meanX) * 100
        } else {
            (sdX / meanX)
        }
    } else {
        meanX <- mean(dataX)
        sdX <- sqrt(sum((dataX - meanX)^2) / (length(dataX) - 1))
        covarX <- if (percentage) {
            (sdX / meanX) * 100
        } else {
            (sdX / meanX)
        }
    }
    covarX
}


#' Determine Z-Score From X
#'
#' This function computes the z-score from data(x), mean(x), and sd(x).
#' @param dataX: x value (numeric).
#' @param meanX: mean of x (numeric).
#' @param sdX: standard deviation of x (numeric).
#' @keywords z-score
#' @export
#' @examples
#' data2zscore(10, 24, 6)
#' data2zscore(dataX=10, meanX=24, sdX=6)
#' @return
#' numeric value: indicating the z-score value.
data2zscore <- function(dataX, meanX, sdX) {
    (dataX - meanX) / sdX
}


#' Determine X From Z-Score
#'
#' This function computes x from z-score(x), mean(x), and sd(x).
#' @param dataZ: z-score of x (numeric).
#' @param meanX: mean of x (numeric).
#' @param sdX: standard deviation of x (numeric).
#' @keywords z-score
#' @export
#' @examples
#' zscore2data(1, 24, 6)
#' zscore2data(dataZ=1, meanX=24, sdX=6)
#' @return
#' numeric value: indicating the x value.
zscore2data <- function(dataZ, meanX, sdX) {
    (dataZ * sdX) + meanX
}


#' Determine Percentile From Z-Score
#'
#' This function computes percentile(x) from z-score(x).
#' @param dataZ: z-score of x (numeric).
#' @keywords z-score percentile
#' @export
#' @examples
#' zscore2percentile(1.5)
#' zscore2percentile(dataZ=1)
#' @return
#' numeric value: indicating the percentile value.
zscore2percentile <- function(dataZ) {
    pnorm(dataZ, lower.tail=TRUE)
}


#' Determine Percentage Between Two Z-Score
#'
#' This function computes percentage between two z-score values.
#' @param leftZ: left z-score of x (numeric).
#' @param rightZ: right z-score of x (numeric).
#' @keywords z-score percentage
#' @export
#' @examples
#' normcdf(1.5, 2)
#' normcdf(leftZ=1, rightZ=2)
#' @return
#' numeric value: indicating the percentage value.
normcdf <- function(leftZ, rightZ) {
    pnorm(rightZ) - pnorm(leftZ)
}


#' Determine Z-Score From Percentile
#'
#' This function computes the z-score from a percentile value.
#' @param percentile: percentile of x (numeric).
#' @keywords percentile z-score
#' @export
#' @examples
#' percentile2zscore(.85)
#' @return
#' numeric value: indicating the z-score value.
percentile2zscore <- function(p) {
    qnorm(p)
}

