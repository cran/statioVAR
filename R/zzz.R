#' @noRd
.onAttach <- function(libname, pkgname) {
  packageStartupMessage("statioVAR: Trend Removal for Vector Autoregressive Workflows.")
  packageStartupMessage("For more information on getting started, type ?statioVAR.")
  packageStartupMessage(
    "For suggestions or to report issues, please contact Giuseppe Corbelli at \
    giuseppe.corbelli@uninettunouniversity.net."
  )
}
