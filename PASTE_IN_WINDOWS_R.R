# ═══════════════════════════════════════════════════════════════
# COPY THIS ENTIRE FILE AND PASTE INTO WINDOWS R/RSTUDIO CONSOLE
# ═══════════════════════════════════════════════════════════════

# Set working directory to WSL path
setwd("//wsl.localhost/Ubuntu/home/ajukearth/brain-network-analysis/Persistence3")

# Verify location
cat("Working directory:", getwd(), "\n")
cat("Files in directory:\n")
print(list.files())

# Load rsconnect
library(rsconnect)

# Define ONLY the files that should be deployed
app_files <- c(
  "app.R",
  "modules/analysis_functions.R",
  "modules/visualization_functions.R",
  "modules/ui_components.R",
  "modules/download_simple.R",
  "modules/consensus_analytics.R",
  "modules/consensus_visualizations.R",
  "modules/statistical_tests.R",
  "modules/topological_analysis.R",
  "modules/advanced_networks.R",
  "modules/meta_consensus.R",
  "modules/complete_plot_system.R",
  "modules/export_functions.R"
)

# Deploy to shinyapps.io
cat("\n🚀 Deploying ConsensusConnectR...\n\n")

deployApp(
  appFiles = app_files,
  appName = "ConsensusConnectR",
  appTitle = "ConsensusConnectR - Multimethod Consensus Network Analysis",
  account = "ajmg",
  server = "shinyapps.io",
  forceUpdate = TRUE,
  lint = FALSE
)

cat("\n✅ Deployment complete!\n")
cat("URL: https://ajmg.shinyapps.io/ConsensusConnectR/\n")
