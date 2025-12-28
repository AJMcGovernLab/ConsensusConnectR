# ConsensusConnectR - Deployment Guide for shinyapps.io

## Quick Start

Follow these steps to deploy ConsensusConnectR to shinyapps.io:

### Step 1: Install Dependencies

Run the installation script to install all required packages:

```r
source("install_dependencies.R")
```

This will install:
- Core Shiny packages (shiny, shinydashboard, DT, etc.)
- Network analysis packages (igraph, WGCNA)
- Statistical packages (psych, corpcor, mice)
- Optional packages (pracma, TDA) to avoid deployment warnings
- Bioconductor dependencies (impute, preprocessCore)

### Step 2: Test Locally

Before deploying, test the app locally:

```r
shiny::runApp()
```

Verify that:
- ✓ App loads without errors
- ✓ Sample data can be uploaded
- ✓ Analysis runs successfully
- ✓ Plots render correctly
- ✓ Downloads work

### Step 3: Deploy to shinyapps.io

Deploy using rsconnect:

```r
library(rsconnect)

# First time only: Set up your account
# rsconnect::setAccountInfo(
#   name = "your-username",
#   token = "your-token",
#   secret = "your-secret"
# )

# Deploy the app
rsconnect::deployApp(
  appName = "ConsensusConnectR",
  appTitle = "ConsensusConnectR - Multimethod Consensus Network Analysis",
  launch.browser = TRUE,
  forceUpdate = TRUE
)
```

## Troubleshooting

### Error: "pracma/TDA/webshot2 not installed"

**Solution:** Run `source("install_dependencies.R")` first

The `.rscignore` file excludes unused modules that require these optional packages.

### Error: "impute/preprocessCore not installed"

**Solution:** These are Bioconductor packages required by WGCNA.

```r
BiocManager::install(c("impute", "preprocessCore"))
```

### Error: "Package version mismatch"

**Solution:** Update all packages to latest versions:

```r
update.packages(ask = FALSE, checkBuilt = TRUE)
```

### Deployment is slow/times out

**Causes:**
- Large number of dependencies
- Bioconductor packages compilation

**Solutions:**
1. Deploy from a fast internet connection
2. Be patient - first deployment can take 10-15 minutes
3. If timeout occurs, try again (rsconnect will resume)

### App crashes on shinyapps.io but works locally

**Check:**
1. **File paths:** All paths must be relative, no absolute paths
2. **Case sensitivity:** Linux is case-sensitive (local Windows is not)
3. **Memory limits:** Free tier has 1GB RAM limit
4. **Logs:** Check application logs on shinyapps.io dashboard

## Files Excluded from Deployment

The `.rscignore` file excludes these unused modules:
- `modules/shiny_plot_harvester.R` (requires webshot2, not used)
- `modules/comprehensive_plot_extractor.R` (development tool)
- `modules/dynamic_plot_generator.R` (development tool)
- `modules/download_manager.R` (superseded by download_simple.R)
- `modules/download_manager_simple.R` (superseded by download_simple.R)

## Resource Limits (shinyapps.io Free Tier)

- **Active hours:** 25 hours/month
- **RAM:** 1 GB
- **Concurrent users:** Limited

**Optimization tips:**
- Clear analysis results when switching datasets
- Use reasonable number of permutations (1000-5000, not 10000)
- Monitor usage at shinyapps.io dashboard

## Version Info

- **App Version:** 3.0
- **Shiny:** Uses shinydashboard framework
- **R Version:** ≥ 4.0 recommended

## Support

For deployment issues:
1. Check shinyapps.io application logs
2. Verify all dependencies are installed
3. Test locally with same R version as deployment server
4. Review rsconnect error messages

## Deployment Checklist

Before deploying:
- [ ] All dependencies installed (`install_dependencies.R` completed)
- [ ] App tested locally and working
- [ ] No absolute file paths in code
- [ ] `.rscignore` file present
- [ ] rsconnect account configured
- [ ] App name and title set correctly

After deploying:
- [ ] App loads successfully on shinyapps.io
- [ ] Test basic functionality (upload, analyze, download)
- [ ] Check application logs for warnings/errors
- [ ] Monitor resource usage

## Advanced: Custom Deployment

For more control over deployment:

```r
# Specify which files to include/exclude
rsconnect::deployApp(
  appDir = getwd(),
  appFiles = c(
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
    "modules/meta_consensus.R"
  ),
  appName = "ConsensusConnectR",
  forceUpdate = TRUE
)
```

## shinyapps.io Configuration

Recommended settings in shinyapps.io dashboard:

**Instance Size:** Standard (1 GB RAM)
**R Version:** Latest stable (4.3.x or 4.4.x)
**Startup Timeout:** 60 seconds
**Idle Timeout:** 15 minutes

---

**Need Help?**
- shinyapps.io documentation: https://docs.posit.co/shinyapps.io/
- rsconnect documentation: https://github.com/rstudio/rsconnect
