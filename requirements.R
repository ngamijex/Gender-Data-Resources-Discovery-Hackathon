# ── GDDP Package Requirements ─────────────────────────────────────────────────
# Run this script once to install all required packages.

pkgs <- c(
  "shiny",    # >= 1.7.0  — core framework
  "dplyr",    # >= 1.1.0  — data manipulation
  "tidyr",    # >= 1.3.0  — replace_na, pivoting
  "readr",    # >= 2.1.0  — CSV parsing
  "stringr",  # >= 1.5.0  — string operations
  "plotly"    # >= 4.10.0 — interactive charts
)

to_install <- pkgs[!pkgs %in% rownames(installed.packages())]
if (length(to_install) > 0) {
  install.packages(to_install)
} else {
  message("All required packages are already installed.")
}
