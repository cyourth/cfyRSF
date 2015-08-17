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
#' pop.covar(population = popData)
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
#' sampleData <- c(4, 6, 5, 5, 6, 4, 3, 2, 5, 5, 7, 8, 9, 1, 2)
#' sample.covar(popData)
#' sample.covar(population = popData)
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
