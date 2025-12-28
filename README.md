# ConsensusConnectR

A Shiny application for network consensus analysis with artificial brain area discovery, designed for neuroimaging connectivity research.

## Features

- **Multi-method Correlation Analysis**: Compare networks using Pearson, Spearman, Kendall, and other correlation methods
- **Consensus Network Construction**: Build consensus networks across multiple correlation approaches
- **Artificial Brain Area Discovery**: Identify combinations of brain regions that maximally contribute to group differences
  - Brute force enumeration of all region combinations
  - Permutation testing with FDR/Bonferroni correction
  - Configurable candidate filtering (effect size, percentile)
- **High-Performance C++ Backend**: OpenMP-parallelized permutation testing for 50x+ speedup
- **Interactive Visualizations**: Heatmaps, network graphs, and contribution plots
- **Flexible Data Export**: Download results in multiple formats

## Installation

### Prerequisites

- R (>= 4.0)
- RStudio (recommended)
- Rtools (Windows) or Xcode Command Line Tools (macOS) for C++ compilation

### Required R Packages

```r
source("install_dependencies.R")
```

Or install manually:

```r
install.packages(c(
  "shiny", "shinydashboard", "shinyjs", "shinyWidgets",
  "DT", "plotly", "ggplot2", "dplyr", "tidyr",
  "igraph", "corrplot", "RColorBrewer",
  "future", "furrr", "promises",
  "Rcpp", "RcppArmadillo"
))
```

### C++ Backend (Optional but Recommended)

The C++ backend provides significant performance improvements for permutation testing. It compiles automatically on first run if Rcpp and RcppArmadillo are installed.

## Usage

1. **Launch the app:**
   ```r
   shiny::runApp()
   ```

2. **Load your data:** Upload correlation matrices or connectivity data

3. **Configure analysis:**
   - Select correlation methods
   - Choose groups to compare
   - Set permutation parameters

4. **Run analysis:**
   - Use the calibration tool to estimate runtime
   - Execute regional contribution or artificial brain area discovery

## Project Structure

```
ConsensusConnectR/
├── app.R                 # Main Shiny application
├── modules/
│   ├── analysis_functions.R      # Core analysis functions
│   ├── statistical_tests.R       # Permutation testing & statistics
│   ├── rcpp_backend.R            # C++ backend wrapper
│   ├── visualization_functions.R # Plotting functions
│   └── ui_components.R           # UI helper functions
├── src/
│   ├── permutation_core.cpp      # C++ OpenMP permutation code
│   ├── Makevars                  # Unix compilation flags
│   └── Makevars.win              # Windows compilation flags
└── install_dependencies.R        # Dependency installer
```

## Performance

The C++ backend with OpenMP provides substantial speedups:

| Cores | Approximate Speedup |
|-------|---------------------|
| 4     | ~20x faster than R  |
| 8     | ~40x faster than R  |
| 16+   | ~50-80x faster than R |

Runtime scales with: `n_candidates × n_permutations × n_matrices`

## Citation

If you use ConsensusConnectR in your research, please cite:

> McGovern, A.J. (2024). ConsensusConnectR: A Shiny application for network consensus analysis. GitHub repository: https://github.com/AJMcGovernLab/ConsensusConnectR

## License

This project is licensed under the MIT License - see the LICENSE file for details.

## Contributing

Contributions are welcome! Please feel free to submit a Pull Request.

## Support

For issues and feature requests, please use the [GitHub Issues](https://github.com/AJMcGovernLab/ConsensusConnectR/issues) page.
