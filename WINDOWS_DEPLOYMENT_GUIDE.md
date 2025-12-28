# Windows Deployment Guide for ConsensusConnectR

## Problem: Deploying from Windows with Files in WSL

You're deploying from Windows R/RStudio but your app files are in WSL (`//wsl.localhost/Ubuntu/...`). This causes issues with:
1. `.rscignore` not being respected
2. Package detection across Windows/WSL boundary
3. File path incompatibilities

## Solution: Deploy Using Explicit File List

### Step 1: Open R/RStudio on Windows

Open R or RStudio **on your Windows machine** (not in WSL).

### Step 2: Set Working Directory to WSL Path

```r
setwd("//wsl.localhost/Ubuntu/home/ajukearth/brain-network-analysis/Persistence3")
```

Verify you're in the right place:
```r
getwd()
list.files()  # Should show app.R, modules/, etc.
```

### Step 3: Install Required Packages in Windows R

Run this in Windows R console:

```r
# Install required packages
install.packages(c(
  "shiny", "shinydashboard", "DT", "shinyjs", "colourpicker",
  "ggplot2", "scales", "viridis", "corrplot", "igraph",
  "dplyr", "psych", "corpcor", "mice", "RColorBrewer",
  "pracma", "TDA", "rsconnect"
))

# Install Bioconductor packages
if (!requireNamespace("BiocManager", quietly = TRUE))
    install.packages("BiocManager")

BiocManager::install(c("impute", "preprocessCore", "GO.db"))

# Install WGCNA
install.packages("WGCNA")
```

This may take 10-15 minutes. Be patient!

### Step 4: Deploy with Explicit File List

Use this deployment command that explicitly excludes problematic files:

```r
library(rsconnect)

# Define files to deploy (excluding unused modules)
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

# Deploy with explicit file list
deployApp(
  appFiles = app_files,
  appName = "ConsensusConnectR",
  appTitle = "ConsensusConnectR - Multimethod Consensus Network Analysis",
  account = "ajmg",
  server = "shinyapps.io",
  forceUpdate = TRUE,
  lint = FALSE
)
```

### Step 5: Monitor Deployment

The deployment will take 10-15 minutes. You'll see:
```
✔ Bundling 13 files...
✔ Capturing R dependencies...
✔ Uploading bundle...
✔ Deploying application...
```

## Alternative: Deploy from WSL Linux R

If Windows deployment continues to fail, deploy from WSL:

### 1. Install rsconnect in WSL R

```bash
# In WSL terminal
cd /home/ajukearth/brain-network-analysis/Persistence3
R
```

Then in R:
```r
install.packages("rsconnect")
```

### 2. Configure rsconnect in WSL

```r
library(rsconnect)

# Set up your account (one time only)
rsconnect::setAccountInfo(
  name = "ajmg",
  token = "YOUR_TOKEN_HERE",
  secret = "YOUR_SECRET_HERE"
)
```

Get credentials from: https://www.shinyapps.io/admin/#/tokens

### 3. Deploy from WSL

```r
# In R console (WSL)
setwd("/home/ajukearth/brain-network-analysis/Persistence3")

# Define files to deploy
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

library(rsconnect)
deployApp(
  appFiles = app_files,
  appName = "ConsensusConnectR",
  account = "ajmg",
  forceUpdate = TRUE,
  lint = FALSE
)
```

## Troubleshooting

### Error: "pracma/TDA/webshot2 not installed"

**Problem:** These packages are detected but not needed (your code has fallbacks)

**Solution:** Install them anyway to satisfy renv:
```r
install.packages(c("pracma", "TDA"))
```

Skip `webshot2` - we're excluding the file that needs it.

### Error: "impute/preprocessCore not installed"

**Problem:** WGCNA dependencies from Bioconductor

**Solution:**
```r
BiocManager::install(c("impute", "preprocessCore"))
```

### Error: "Package version mismatch"

**Problem:** Windows and WSL have different package versions

**Solution:** Update packages in Windows:
```r
update.packages(ask = FALSE)
```

### Error: "Cannot find .rscignore"

**Problem:** `.rscignore` doesn't work across Windows/WSL boundary

**Solution:** Use explicit `appFiles` parameter (shown above)

### Deployment Succeeds but App Crashes

**Check these:**

1. **File paths are relative:**
   ```r
   # Good
   source("modules/analysis_functions.R")

   # Bad
   source("/home/ajukearth/brain-network-analysis/Persistence3/modules/analysis_functions.R")
   ```

2. **Check application logs** on shinyapps.io dashboard

3. **Test locally first:**
   ```r
   shiny::runApp()
   ```

## Complete Deployment Script (Copy-Paste Ready)

Save this as `deploy_windows.R`:

```r
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

# Step 3: Define files to deploy
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
```

Then run:
```r
source("deploy_windows.R")
```

## Why This Approach Works

1. **Explicit file list** bypasses `.rscignore` issues
2. **Excludes unused modules** that require optional packages
3. **Works across Windows/WSL boundary**
4. **lint = FALSE** skips warnings about optional packages

## Files Being Excluded

These modules are excluded because they're not used by app.R:

- ❌ `modules/shiny_plot_harvester.R` (requires webshot2)
- ❌ `modules/comprehensive_plot_extractor.R` (development tool)
- ❌ `modules/dynamic_plot_generator.R` (development tool)
- ❌ `modules/download_manager.R` (superseded)
- ❌ `modules/download_manager_simple.R` (superseded)

## Success Checklist

After deployment:
- [ ] App loads at https://ajmg.shinyapps.io/ConsensusConnectR/
- [ ] Can upload sample data
- [ ] Analysis runs without errors
- [ ] Plots render correctly
- [ ] Downloads work

## Need Help?

If you still encounter errors:

1. **Copy full error message** from R console
2. **Check shinyapps.io logs** at https://www.shinyapps.io/admin/
3. **Verify file paths** are all relative
4. **Test locally** with `shiny::runApp()` first

---

**Remember:** Deploy from Windows R using explicit file list, OR deploy from WSL R with rsconnect configured there.
