# ConsensusConnectR Windows Deployment Script
# Run this from Windows R/RStudio

cat("ConsensusConnectR Windows Deployment\n")
cat("====================================\n\n")

# Step 1: Set working directory
setwd("//wsl.localhost/Ubuntu/home/ajukearth/brain-network-analysis/Persistence3")
cat("Working directory:", getwd(), "\n\n")

# Step 2: Check required packages
cat("Checking packages...\n")
required_pkgs <- c("shiny", "shinydashboard", "DT", "igraph", "WGCNA", "rsconnect")
missing_pkgs <- required_pkgs[!sapply(required_pkgs, requireNamespace, quietly = TRUE)]

if(length(missing_pkgs) > 0) {
  cat("\n❌ Missing packages:\n")
  cat(paste("  -", missing_pkgs), sep = "\n")
  cat("\nPlease install them first!\n")
  stop("Missing packages")
}

cat("✓ All required packages installed\n\n")

# Step 3: Define files to deploy (excluding unused modules)
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

cat("Files to deploy:\n")
cat(paste("  -", app_files), sep = "\n")
cat("\n")

# Step 4: Confirm deployment
cat("Ready to deploy to shinyapps.io\n")
cat("Account: ajmg\n")
cat("App: ConsensusConnectR\n\n")

response <- readline("Proceed? (y/n): ")

if(tolower(response) != "y") {
  cat("Deployment cancelled.\n")
  quit(save = "no")
}

# Step 5: Deploy
cat("\nDeploying...\n\n")

library(rsconnect)

tryCatch({
  deployApp(
    appFiles = app_files,
    appName = "ConsensusConnectR",
    appTitle = "ConsensusConnectR - Multimethod Consensus Network Analysis",
    account = "ajmg",
    server = "shinyapps.io",
    forceUpdate = TRUE,
    lint = FALSE
  )

  cat("\n✅ Deployment successful!\n")
  cat("URL: https://ajmg.shinyapps.io/ConsensusConnectR/\n")

}, error = function(e) {
  cat("\n❌ Deployment failed:\n")
  cat(e$message, "\n")
})
