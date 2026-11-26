# Render the full analysis: analysis/supply_chain_statistics.md + figures/
# Usage (from the project root):  Rscript run_analysis.R
source(file.path("R", "install_packages.R"))
rmarkdown::render(file.path("analysis", "supply_chain_statistics.Rmd"), quiet = TRUE)
message("Done: analysis/supply_chain_statistics.md")
