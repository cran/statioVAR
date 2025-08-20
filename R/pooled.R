#' Pooled polynomial detrending for multivariate panel data
#'
#' Remove study-wide polynomial trend (up to cubic) plus optional cyclic effects
#' from multivariate panel data by fitting a single OLS model on the pooled 
#' series.   
#' Trend terms up to the chosen degree are estimated; those whose two-sided t-tests 
#' are significant at \code{alpha} are retained, non-significant components are 
#' set to 0, and the resulting fitted values are subtracted from every observation 
#' of the raw series.
#'
#' @param df Data frame or tibble (long format). 
#' @param id_var Character string. Unit (subject) identifier column (required).
#' @param time_var Character string. Numeric time index column (if NULL, then \code{cyc_vars} must be specified). If \code{NULL}, no polynomial time terms are included.
#' @param vars_to_detrend Character vector. Column names to detrend (required).
#' @param poly_order Integer in \{1,2,3\}. Maximum degree of the polynomial time trend tested (default: 1):
#'   \itemize{
#'     \item 1 = linear only,
#'     \item 2 = linear + quadratic,
#'     \item 3 = linear + quadratic + cubic.
#'   }
#' @param cyc_vars Character vector. Column names (e.g. "weekend") for categorical cyclicity variables (if NULL, then \code{time_var} must be specified).
#' @param alpha Numeric in (0,1). Significance threshold for retaining polynomial terms (default 0.05).
#' @param miss_thresh Numeric in (0,1). Maximum allowed proportion of missing data per variable (default: 0.30).
#'
#' @return A named list with:
#' \describe{
#'   \item{\code{df}}{Tibble with added \code{<var>_detrended} columns.}
#'   \item{\code{coef_tables}}{Named list of coefficient tables (one per variable), with columns \code{predictor}, \code{estimate}, \code{Std. Error}, \eqn{t}, \eqn{p}, and a logical flag \code{kept}.}
#'   \item{\code{formula_str}}{Character string of the fitted model formula.}
#'   \item{\code{n_units}}{Integer: number of unique units (subjects).}
#' }
#'
#' @examples
#' dat <- data.frame(
#' id = rep(1:3, each=5),
#' time = rep(1:5, 3),
#' cyc = rep(c("A","B"), length.out=15),
#' y1 = rnorm(15, sd = 0.5) + seq(1,15)*1.0
#' )
#' res <- statioVAR::pooled(
#' df = dat,
#' id_var = "id",
#' time_var = "time",
#' vars_to_detrend = "y1",
#' poly_order = 2,
#' cyc_vars = "cyc",
#' alpha = 0.05,
#' miss_thresh = 0.30
#' )
#'
#' @importFrom dplyr as_tibble
#' @importFrom rlang abort
#' @importFrom stats lm predict as.formula model.matrix na.exclude
#' @export
#' 
#' @usage
#' pooled(
#'   df,
#'   id_var,
#'   time_var = NULL,
#'   vars_to_detrend,
#'   poly_order = 1,
#'   cyc_vars = NULL,
#'   alpha = 0.05,
#'   miss_thresh = 0.30
#' )

pooled <- function(
    df,
    id_var,
    time_var      = NULL,
    vars_to_detrend,
    poly_order    = 1L,
    cyc_vars      = NULL,
    alpha         = 0.05,
    miss_thresh   = 0.30
) {
  
  if (!is.data.frame(df))
    rlang::abort("`df` must be a data.frame or tibble.")
  if (!is.character(id_var) || length(id_var) != 1 || !id_var %in% names(df))
    rlang::abort("`id_var` must be a column name in df.")
  if (!is.null(time_var) &&
      (!is.character(time_var) || length(time_var) != 1 || !time_var %in% names(df)))
    rlang::abort("`time_var` must be NULL or a column name in df.")
  if (!is.character(vars_to_detrend) || !all(vars_to_detrend %in% names(df)))
    rlang::abort("All `vars_to_detrend` must be column names in df.")
  if (!poly_order %in% 1:3)
    rlang::abort("`poly_order` must be 1, 2, or 3.")
  if (!is.null(cyc_vars) && !all(cyc_vars %in% names(df)))
    rlang::abort("All `cyc_vars` must be column names in df.")
  if (is.null(time_var) && is.null(cyc_vars))
    rlang::abort("Supply at least one of time_var or cyc_vars.")
  if (!is.numeric(alpha) || alpha <= 0 || alpha >= 1)
    rlang::abort("`alpha` must be between 0 and 1.")
  if (!is.numeric(miss_thresh) || miss_thresh < 0 || miss_thresh > 1)
    rlang::abort("`miss_thresh` must be between 0 and 1.")
  
  df <- as_tibble(df)
  keep <- c(id_var, time_var, cyc_vars, vars_to_detrend)
  df   <- df[, keep, drop = FALSE]
  
  for (v in vars_to_detrend) {
    if (mean(is.na(df[[v]])) > miss_thresh) {
      rlang::abort(sprintf("Variable '%s' exceeds missing threshold.", v))
    }
  }
  if (!is.null(time_var) && mean(is.na(df[[time_var]])) > miss_thresh)
    rlang::abort("time_var exceeds missing threshold.")
  
  terms <- "1"
  if (!is.null(time_var)) {
    terms <- c(terms, time_var)
    if (poly_order >= 2) terms <- c(terms, sprintf("I(%s^2)", time_var))
    if (poly_order >= 3) terms <- c(terms, sprintf("I(%s^3)", time_var))
  }
  if (!is.null(cyc_vars)) {
    terms <- c(terms, sprintf("factor(%s)", cyc_vars))
  }
  formula_str <- paste(terms, collapse = " + ")
  
  # detrending con anche il fitting
  coef_tables <- list()
  df_det      <- df
  n_units  <- length(unique(df[[id_var]]))
  
  for (v in vars_to_detrend) {
    fm  <- as.formula(paste(v, "~", formula_str))
    mod <- stats::lm(fm, data = df, na.action = na.exclude)
    tab <- as.data.frame(summary(mod)$coefficients)
    tab$Predictor <- rownames(tab); rownames(tab) <- NULL
    tab <- tab[tab$Predictor != "(Intercept)", , drop = FALSE]
    tab$kept <- !is.na(tab[["Pr(>|t|)"]]) & tab[["Pr(>|t|)"]] < alpha
    coef_tables[[v]] <- tab
    
    X    <- model.matrix(fm, data = df)
    beta <- setNames(numeric(ncol(X)), colnames(X))
    beta[tab$Predictor] <- ifelse(tab$kept, tab$Estimate, 0)
    fitted <- as.vector(X %*% beta)
    df_det[[paste0(v, "_detrended")]] <- df[[v]] - fitted
  }  
  
  if (interactive()) {
    # cosa finisce in console
  header <- "Pooled polynomial trend removal"
  cat("\n", header, "\n", strrep("-", nchar(header)), "\n", sep = "")
  cat("Polynomial order:", poly_order, "\n")
  cat("Variables processed:", length(vars_to_detrend), "\n\n")
  for (v in vars_to_detrend) {
    kept <- coef_tables[[v]]$Predictor[coef_tables[[v]]$kept]
    cat(sprintf("- %s: retained -> %s\n", v,
                if (length(kept)) paste(kept, collapse = ", ") else "none"))
  }
  }
  
  list(
    df = df_det,
    coef_tables = coef_tables,
    formula_str = formula_str,
    n_units  = n_units)
}