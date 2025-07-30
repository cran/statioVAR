#' Trend Removal for Vector Autoregressive Workflows
#'
#' Detrending multivariate time-series to approximate stationarity when dealing 
#' with intensive longitudinal data, prior to Vector Autoregressive (VAR) or 
#' multilevel-VAR estimation. Classical VAR assumes weak stationarity (constant 
#' first two moments), and deterministic trends inflate spurious autocorrelation, 
#' biasing Granger-causality and impulse-response analyses. All functions 
#' operate on raw panel data and write detrended columns back to the data set, 
#' but differ in the level at which the trend is estimated. 
#'
#' The functions are:
#' \itemize{
#'   \item \code{\link{detrender}}: within-person linear detrending, which fits and removes a separate linear trend for each subject on each selected variable.
#'   \item \code{\link{pooled}}: pooled polynomial detrending, which fits and removes a global polynomial trend (up to cubic) and optional cyclic effects across all subjects.
#' }
#' 
#' @section Note:
#' The development of this package was inspired by, and is deeply indebted to, 
#' the works of Eiko Fried, Jonas Haslbeck, Sasha Epskamp, Ria Hoekstra and 
#' Alessandra Mansueto, among others. This software is provided 'as is', 
#' without any express or implied warranties of accuracy or reliability. 
#' For suggestions or to report any issue, please contact the author.
#'
#' @author
#' Giuseppe Corbelli (<giuseppe.corbelli@uniroma1.it>)
#'
#' @docType package
#' @name statioVAR
#' @aliases statioVAR-package statioVAR
"_PACKAGE"