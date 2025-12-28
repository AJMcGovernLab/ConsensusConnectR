# Install all required packages for ConsensusConnectR deployment
# Run this script before deploying to shinyapps.io

cat("Installing ConsensusConnectR dependencies...\n\n")

# CRAN packages
cran_packages <- c(
  # Core Shiny packages
  "shiny", "shinydashboard", "DT", "shinyjs", "colourpicker",

  # Visualization
  "ggplot2", "scales", "viridis", "corrplot", "RColorBrewer",

  # Network analysis
  "igraph",

  # Data manipulation
  "dplyr",

  # Statistical analysis
  "psych", "corpcor", "mice",

  # Optional packages with fallbacks (install to avoid deployment warnings)
  "pracma", "TDA"
)

cat("Installing CRAN packages...\n")
for(pkg in cran_packages) {
  if(!requireNamespace(pkg, quietly = TRUE)) {
    cat(sprintf("  Installing %s...\n", pkg))
    install.packages(pkg, repos = "https://cloud.r-project.org/")
  } else {
    cat(sprintf("  ✓ %s already installed\n", pkg))
  }
}

# Bioconductor packages (required by WGCNA)
cat("\nInstalling Bioconductor packages...\n")

if (!requireNamespace("BiocManager", quietly = TRUE)) {
  install.packages("BiocManager")
}

bioc_packages <- c("impute", "preprocessCore", "GO.db")

for(pkg in bioc_packages) {
  if(!requireNamespace(pkg, quietly = TRUE)) {
    cat(sprintf("  Installing %s from Bioconductor...\n", pkg))
    BiocManager::install(pkg, update = FALSE, ask = FALSE)
  } else {
    cat(sprintf("  ✓ %s already installed\n", pkg))
  }
}

# Install WGCNA (requires Bioconductor dependencies)
cat("\nInstalling WGCNA...\n")
if(!requireNamespace("WGCNA", quietly = TRUE)) {
  install.packages("WGCNA", repos = "https://cloud.r-project.org/")
} else {
  cat("  ✓ WGCNA already installed\n")
}

cat("\n✅ All dependencies installed successfully!\n")
cat("\nYou can now deploy to shinyapps.io using:\n")
cat("  library(rsconnect)\n")
cat("  deployApp()\n")
