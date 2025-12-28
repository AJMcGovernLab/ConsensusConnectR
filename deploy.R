# Automated deployment script for ConsensusConnectR
# This script handles the complete deployment process to shinyapps.io

cat("ConsensusConnectR Deployment Script\n")
cat("===================================\n\n")

# Step 1: Check dependencies
cat("Step 1: Checking dependencies...\n")

required_packages <- c(
  "shiny", "shinydashboard", "DT", "shinyjs", "colourpicker",
  "ggplot2", "scales", "viridis", "corrplot", "igraph",
  "dplyr", "psych", "corpcor", "mice", "WGCNA",
  "pracma", "TDA", "rsconnect"
)

missing_packages <- c()
for(pkg in required_packages) {
  if(!requireNamespace(pkg, quietly = TRUE)) {
    missing_packages <- c(missing_packages, pkg)
  }
}

if(length(missing_packages) > 0) {
  cat("\n❌ Missing packages detected:\n")
  cat(paste("  -", missing_packages, collapse = "\n"), "\n\n")
  cat("Please run: source('install_dependencies.R')\n")
  stop("Installation required before deployment")
}

cat("✓ All required packages installed\n\n")

# Step 2: Check for .rscignore file
cat("Step 2: Checking configuration files...\n")

if(!file.exists(".rscignore")) {
  cat("⚠️  Creating .rscignore file...\n")
  writeLines(c(
    "# Exclude unused modules",
    "modules/shiny_plot_harvester.R",
    "modules/comprehensive_plot_extractor.R",
    "modules/dynamic_plot_generator.R",
    "modules/download_manager.R",
    "modules/download_manager_simple.R",
    "*.backup",
    "*.bak"
  ), ".rscignore")
}

cat("✓ Configuration files ready\n\n")

# Step 3: Test app locally (optional)
cat("Step 3: Local testing...\n")
response <- readline("Test app locally before deploying? (y/n): ")

if(tolower(response) == "y") {
  cat("Starting local test...\n")
  cat("Press Ctrl+C in console to stop the test and continue deployment.\n\n")
  tryCatch({
    shiny::runApp(launch.browser = TRUE)
  }, error = function(e) {
    cat("Local test stopped.\n")
  })
}

# Step 4: Deploy to shinyapps.io
cat("\nStep 4: Deploying to shinyapps.io...\n")

library(rsconnect)

# Check if account is configured
accounts <- rsconnect::accounts()

if(nrow(accounts) == 0) {
  cat("\n❌ No shinyapps.io account configured.\n\n")
  cat("Please configure your account first:\n")
  cat("  rsconnect::setAccountInfo(\n")
  cat("    name = 'your-username',\n")
  cat("    token = 'your-token',\n")
  cat("    secret = 'your-secret'\n")
  cat("  )\n\n")
  cat("Get your credentials from: https://www.shinyapps.io/admin/#/tokens\n")
  stop("Account configuration required")
}

cat(sprintf("✓ Using account: %s\n\n", accounts$name[1]))

# Confirm deployment
cat("Ready to deploy ConsensusConnectR\n")
cat(sprintf("  Account: %s\n", accounts$name[1]))
cat("  Server: shinyapps.io\n")
cat("  App Name: ConsensusConnectR\n\n")

response <- readline("Proceed with deployment? (y/n): ")

if(tolower(response) != "y") {
  cat("Deployment cancelled.\n")
  quit(save = "no")
}

# Deploy
cat("\nDeploying... (this may take 10-15 minutes)\n\n")

tryCatch({
  rsconnect::deployApp(
    appName = "ConsensusConnectR",
    appTitle = "ConsensusConnectR - Multimethod Consensus Network Analysis",
    launch.browser = TRUE,
    forceUpdate = TRUE,
    lint = FALSE  # Skip linting to avoid warnings about fallback packages
  )

  cat("\n✅ Deployment successful!\n\n")
  cat("Your app is now live at:\n")
  cat(sprintf("  https://%s.shinyapps.io/ConsensusConnectR/\n\n", accounts$name[1]))

  cat("Next steps:\n")
  cat("  1. Test the deployed app\n")
  cat("  2. Monitor usage at https://www.shinyapps.io/admin/\n")
  cat("  3. Check application logs if any issues\n")

}, error = function(e) {
  cat("\n❌ Deployment failed:\n")
  cat(e$message, "\n\n")
  cat("Troubleshooting:\n")
  cat("  1. Check your internet connection\n")
  cat("  2. Verify all dependencies are installed\n")
  cat("  3. Review error message above\n")
  cat("  4. Check DEPLOYMENT_GUIDE.md for common issues\n\n")
  cat("If deployment timed out, you can retry - rsconnect will resume.\n")
})
