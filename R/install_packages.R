# Install every package used by the analysis (run once).
pkgs <- c(
  "dplyr", "tidyr", "ggplot2", "patchwork", "scales",   # data wrangling and plots
  "rmarkdown", "knitr",                                   # reporting
  "e1071", "psych", "nortest", "fitdistrplus", "boot",    # descriptives, normality, bootstrap
  "car", "effectsize", "rstatix", "coin", "lmtest",       # tests and effect sizes
  "sandwich", "performance", "pwr", "vcd", "corrplot"     # regression, power, plots
)
missing <- setdiff(pkgs, rownames(installed.packages()))
if (length(missing)) install.packages(missing, repos = "https://cloud.r-project.org")
invisible(lapply(pkgs, requireNamespace, quietly = TRUE))
message("All ", length(pkgs), " packages available.")
