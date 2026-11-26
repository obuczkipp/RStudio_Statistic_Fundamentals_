# =============================================================================
# helpers.R
# Small, well-documented helper functions used by the analysis notebook.
# Every function computes something that base R does not provide directly,
# so the formula is written out explicitly for learning purposes.
# =============================================================================

# ---- Central tendency --------------------------------------------------------

#' Statistical mode: the most frequent value.
#' For continuous data almost every value is unique, so the mode is only
#' meaningful for discrete / categorical variables. Ties return all modes.
stat_mode <- function(x) {
  tab <- table(x)
  modes <- names(tab)[tab == max(tab)]
  if (length(modes) == length(tab)) return(NA_character_)  # no value repeats
  paste(modes, collapse = ", ")
}

#' Geometric mean: exp(mean(log(x))). Appropriate for multiplicative
#' quantities (growth rates, ratios). Defined only for strictly positive x.
geometric_mean <- function(x) if (any(x <= 0)) NA_real_ else exp(mean(log(x)))

#' Harmonic mean: n / sum(1/x). Appropriate for averaging rates
#' (e.g. units per hour). Always <= geometric mean <= arithmetic mean.
harmonic_mean <- function(x) if (any(x <= 0)) NA_real_ else length(x) / sum(1 / x)

# ---- Dispersion --------------------------------------------------------------

#' Coefficient of variation = SD / mean. Unit-free, so it lets us compare
#' variability of variables measured in different units (dollars vs days).
coef_variation <- function(x) sd(x) / mean(x)

#' Standard error of the mean = SD / sqrt(n). How much the sample mean would
#' vary from sample to sample; the basis of confidence intervals for the mean.
std_error <- function(x) sd(x) / sqrt(length(x))

#' Gini coefficient (0 = perfect equality, 1 = one item holds everything).
#' Computed from the sorted values:
#'   G = (2 * sum(i * x_(i))) / (n * sum(x)) - (n + 1) / n
gini <- function(x) {
  x <- sort(x)
  n <- length(x)
  (2 * sum(seq_len(n) * x)) / (n * sum(x)) - (n + 1) / n
}

# ---- Descriptive table -------------------------------------------------------

#' One row of descriptive statistics for a numeric vector.
describe_numeric <- function(x) {
  q <- quantile(x, c(0.05, 0.25, 0.50, 0.75, 0.95), names = FALSE)
  ci <- t.test(x)$conf.int
  data.frame(
    n          = length(x),
    mean       = mean(x),
    trim_mean  = mean(x, trim = 0.10),      # 10% trimmed: robust to outliers
    median     = q[3],
    geo_mean   = geometric_mean(x),
    harm_mean  = harmonic_mean(x),
    sd         = sd(x),
    variance   = var(x),
    se_mean    = std_error(x),
    ci95_low   = ci[1],
    ci95_high  = ci[2],
    cv         = coef_variation(x),
    min        = min(x),
    p05        = q[1],
    q1         = q[2],
    q3         = q[4],
    p95        = q[5],
    max        = max(x),
    range      = diff(range(x)),
    iqr        = IQR(x),
    mad        = mad(x),                     # median absolute deviation (scaled)
    skewness   = e1071::skewness(x, type = 2),
    kurtosis   = e1071::kurtosis(x, type = 2) # EXCESS kurtosis: normal = 0
  )
}

# ---- Outliers ----------------------------------------------------------------

#' Count outliers with three common rules.
#' * Tukey fences: outside [Q1 - 1.5*IQR, Q3 + 1.5*IQR]
#' * z-score: |x - mean| / sd > 3 (assumes approx. normality; the mean and SD
#'   are themselves pulled by outliers, so this rule can "mask" them)
#' * Modified z-score (Iglewicz & Hoaglin): 0.6745 * |x - median| / MAD > 3.5,
#'   robust because median and MAD are not affected by extreme values
outlier_counts <- function(x) {
  q <- quantile(x, c(.25, .75), names = FALSE)
  fence <- 1.5 * diff(q)
  mad_raw <- median(abs(x - median(x)))
  c(
    tukey    = sum(x < q[1] - fence | x > q[2] + fence),
    z_score  = sum(abs(scale(x)) > 3),
    modified = if (mad_raw == 0) NA else sum(0.6745 * abs(x - median(x)) / mad_raw > 3.5)
  )
}

# ---- Categorical -------------------------------------------------------------

#' Shannon entropy (bits) of a categorical variable, and the normalised
#' version (0 = all in one category, 1 = perfectly even spread).
entropy <- function(x) {
  p <- prop.table(table(x))
  h <- -sum(p * log2(p))
  c(entropy_bits = h, normalised = h / log2(length(p)))
}

#' Proportions with Wilson score 95% confidence intervals.
#' The Wilson interval keeps good coverage even for small counts, unlike the
#' textbook Wald interval p +/- 1.96*sqrt(p(1-p)/n), which can go below 0.
wilson_table <- function(x, conf = 0.95) {
  tab <- table(x)
  n <- sum(tab)
  z <- qnorm(1 - (1 - conf) / 2)
  p <- as.numeric(tab) / n
  centre <- (p + z^2 / (2 * n)) / (1 + z^2 / n)
  half   <- z * sqrt(p * (1 - p) / n + z^2 / (4 * n^2)) / (1 + z^2 / n)
  data.frame(level = names(tab), count = as.integer(tab), proportion = p,
             ci_low = centre - half, ci_high = centre + half)
}

# ---- Formatting --------------------------------------------------------------

#' Format a p-value for text: "< 0.001" or three decimals.
fmt_p <- function(p) ifelse(p < 0.001, "< 0.001", sprintf("%.3f", p))

#' Round every numeric column of a data frame.
round_df <- function(df, digits = 3) {
  df[] <- lapply(df, function(col) if (is.numeric(col)) round(col, digits) else col)
  df
}

# ---- Plot theme --------------------------------------------------------------

theme_stats <- function() {
  ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold"),
      plot.subtitle = ggplot2::element_text(colour = "grey35"),
      panel.grid.minor = ggplot2::element_blank(),
      legend.position = "bottom"
    )
}

PALETTE <- c("#2b6cb0", "#dd6b20", "#2f855a", "#805ad5", "#c53030", "#718096")
