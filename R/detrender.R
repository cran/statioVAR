#' Within-unit linear detrending for multilevel VAR analysis
#'
#' Remove unit-specific linear trends from panel data to approximate stationarity,
#' preparing inputs for multilevel Vector Autoregressive (VAR) modeling (among others). 
#' For each unit (subject) and each selected variable, a linear regression of the variable 
#' on the time index is tested at significance level \code{alpha}; if the slope 
#' is significant, the fitted trend is subtracted and the mean of the unit is 
#' re-added, to produce detrended series while preserving between-unit information.   
#'   
#' Caution: models with lagged outcomes and per-unit intercepts (fixed or random) 
#' are prone to Nickell-type bias when there are fewer than
#' 10 time points (T) per unit; detrending does not remove it. \code{T >= 10} is 
#' recommended (Nickell, 1981; Judson & Owen, 1999).   
#' For VAR(1) with an intercept and linear trend, a minimum of \code{K + 4} 
#' time points per unit (where \code{K} is the number of detrended series) is 
#' required to maintain positive residual degrees of freedom (Lütkepohl, 2005). 
#'
#' @param df Data frame or tibble (long format). 
#' @param id_var Character string. Unit (subject) identifier column (required).
#' @param time_var Character string. Numeric time index column (required). 
#' @param vars_to_detrend Character vector. Column names to detrend within each unit (subject) (required).
#' @param alpha Numeric in (0,1). Significance threshold for retaining a non-zero time slope (default: 0.05).
#' @param min_obs Integer >2. Minimum observations per unit-variable to attempt detrending (default: 3).
#'
#' @return A named list with:
#' \describe{
#'   \item{\code{df}}{Tibble. The original dataset with additional detrended columns.}
#'   \item{\code{n_units}}{Integer. Number of unique units (subjects) processed.}
#'   \item{\code{total_trends}}{Integer. Total number of individual trends removed across all variables.}
#'   \item{\code{summary}}{Tibble. Number of removed linear trends per variable, with columns \code{variable} and \code{removed_trends}.}
#' }
#' 
#' @references
#' Judson, R. A., & Owen, A. L. (1999). Estimating dynamic panel data models: a guide for macroeconomists. Economics letters, 65(1), 9-15. \doi{10.1016/s0165-1765(99)00130-5}     
#' 
#' Lütkepohl, H. (2005). New Introduction to Multiple Time Series Analysis. Springer Berlin Heidelberg. \doi{10.1007/978-3-540-27752-1}   
#' 
#' Nickell, S. (1981). Biases in dynamic models with fixed effects. Econometrica: Journal of the econometric society, 1417-1426. \doi{10.2307/1911408}   
#' 
#' @examples
#' df_example <- data.frame(
#' id = rep(1:2, each = 5),
#' time = rep(1:5, 2),
#' x = rep(1:5, 2) + rnorm(10)
#' )
#' res <- statioVAR::detrender(
#' df = df_example,
#' id_var = "id",
#' time_var = "time",
#' vars_to_detrend = "x",
#' alpha = 0.05,
#' min_obs = 3
#' )
#' res$df[7:9,]
#' res$n_units
#' res$total_trends
#' res$summary
#'
#' @importFrom dplyr as_tibble tibble
#' @importFrom rlang abort
#' @importFrom stats lm predict as.formula setNames
#' @export
#' 
#' @usage
#' detrender(
#'   df,
#'   id_var,
#'   time_var,
#'   vars_to_detrend,
#'   alpha = 0.05,
#'   min_obs = 3
#' )

detrender <- function(df,  
                      id_var,  
                      time_var,  
                      vars_to_detrend,  
                      alpha = 0.05,  
                      min_obs = 3) {

    if (!is.data.frame(df)) {
    rlang::abort("`df` must be a data.frame or tibble.")
  }
  if (!is.character(id_var) || length(id_var) != 1) {
    rlang::abort("`id_var` must be a single character string naming a column in `df`.")
  }
  if (!is.character(time_var) || length(time_var) != 1) {
    rlang::abort("`time_var` must be a single character string naming a column in `df`.")
  }
  if (!all(c(id_var, time_var) %in% names(df))) {
    rlang::abort("Columns specified by `id_var` and `time_var` must exist in `df`.")
  }
  if (!is.character(vars_to_detrend) || length(vars_to_detrend) < 1) {
    rlang::abort("`vars_to_detrend` must be a non-empty character vector of column names.")
  }
  if (!all(vars_to_detrend %in% names(df))) {
    rlang::abort("All `vars_to_detrend` must be existing column names in `df`.")
  }
  if (!is.numeric(alpha) || length(alpha) != 1 || alpha < 0 || alpha > 1) {
    rlang::abort("`alpha` must be a single numeric value between 0 and 1.")
  }
  if (!is.numeric(min_obs) || length(min_obs) != 1 || min_obs < 3) {
    rlang::abort("`min_obs` must be a single numeric value >= 3.")
  }
  
  df <- dplyr::as_tibble(df)
  
  valid_idx <- !is.na(df[[id_var]]) & !is.na(df[[time_var]])
  df_base <- df[valid_idx, , drop = FALSE]
  
  df_det <- df_base
  for (v in vars_to_detrend) {
    df_det[[paste0(v, "_detrended")]] <- df_det[[v]]
  }
  var_trend_count <- setNames(integer(length(vars_to_detrend)), vars_to_detrend)
  
  # detrendare:
  for (current_id in unique(df_base[[id_var]])) {
    id_idx <- df_base[[id_var]] == current_id
    if (sum(id_idx) < min_obs) next
    
    for (v in vars_to_detrend) {
      obs_idx <- id_idx & !is.na(df_base[[v]])
      if (sum(obs_idx) < min_obs) next
      
      subject_values <- df_base[[v]][obs_idx]
      subject_mean <- mean(subject_values, na.rm = TRUE)
      if (is.nan(subject_mean)) next
      
      formula <- as.formula(paste0(v, " ~ ", time_var))
      fit <- stats::lm(formula, data = df_base[obs_idx, , drop = FALSE])
      coefs <- suppressWarnings(summary(fit)$coefficients)
      if (!(time_var %in% rownames(coefs))) next
      
      p_val <- coefs[time_var, "Pr(>|t|)"]
      if (!is.na(p_val) && p_val < alpha) {
        fitted_vals <- stats::predict(fit, newdata = df_base[obs_idx, , drop = FALSE])
        residuals <- df_base[[v]][obs_idx] - fitted_vals
        df_det[[paste0(v, "_detrended")]][obs_idx] <- residuals + subject_mean
        var_trend_count[v] <- var_trend_count[v] + 1L
      }
    }
  }
  
  n_units   <- length(unique(df_base[[id_var]]))
  total_trends <- sum(var_trend_count)
  summary_tbl <- dplyr::tibble(
    variable = names(var_trend_count),
    removed_trends  = as.integer(var_trend_count)
  )
  
  if (interactive()) {
    # cosa finisce in console
  header <- "Within-unit trend removal summary"
  cat("\n", header, "\n", sep = "")
  cat(strrep("-", nchar(header)), "\n", sep = "")
  cat("Number of units (subjects): ", n_units, "\n", sep = "")
  cat("Total linear trends removed: ", total_trends, "\n\n", sep = "")
  
  for (v in names(var_trend_count)) {
    cat(sprintf(
      "- %s: %d removed.\n",
      v,
      var_trend_count[[v]]
    ))
  }
  }
  
  list(
    df = df_det,
    n_units = n_units,
    total_trends = total_trends,
    summary = summary_tbl)
}
