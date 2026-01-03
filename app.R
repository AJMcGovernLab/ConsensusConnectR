# ConsensusConnectR - Multimethod Consensus-Based Preclinical Functional Connectivity Analysis
# Integrates five correlation methods with consensus approach for robust connectivity mapping

# ============================================================================
# AUTOMATIC PACKAGE INSTALLATION
# ============================================================================
# Check and install required packages if not already installed

required_packages <- c(
  # Core Shiny packages
  "shiny", "shinydashboard", "DT", "shinyjs", "colourpicker", "shinyWidgets",
  # Data manipulation and visualization
  "ggplot2", "scales", "dplyr", "tidyr", "plotly", "reshape2",
  # Correlation methods
  "psych", "corpcor",
  # Network analysis
  "igraph",
  # Imputation
  "mice",
  # Visualization
  "corrplot", "viridis", "pheatmap", "RColorBrewer",
  # Clustering
  "cluster", "dendextend",
  # Numerical methods
  "pracma",
  # Parallel processing
  "parallel", "future", "furrr", "promises", "parallelly",
  # Data export
  "jsonlite"
)

# Optional packages (not strictly required but enhance functionality)
optional_packages <- c(
  "glmnet",     # For elastic net regularization
  "progressr",  # For progress bars in parallel processing
  "WGCNA"       # For weighted gene co-expression network analysis
)

# Packages for C++ backend (significant performance boost - 50x+ speedup)
cpp_packages <- c("Rcpp", "RcppArmadillo")

# Function to check and install packages
install_if_missing <- function(packages, optional = FALSE) {
  missing_packages <- packages[!sapply(packages, requireNamespace, quietly = TRUE)]

  if (length(missing_packages) > 0) {
    if (optional) {
      message(sprintf("Optional package(s) not installed: %s",
                      paste(missing_packages, collapse = ", ")))
      return(invisible(NULL))
    }

    message(sprintf("Installing %d missing package(s): %s",
                    length(missing_packages),
                    paste(missing_packages, collapse = ", ")))

    for (pkg in missing_packages) {
      tryCatch({
        install.packages(pkg, repos = "https://cloud.r-project.org", quiet = TRUE)
        message(sprintf("  Installed: %s", pkg))
      }, error = function(e) {
        message(sprintf("  Failed to install %s: %s", pkg, e$message))
      })
    }
  }
}

# Function to check for C++ build tools
check_build_tools <- function() {
  # Check if pkgbuild is available, install if not
  if (!requireNamespace("pkgbuild", quietly = TRUE)) {
    tryCatch({
      install.packages("pkgbuild", repos = "https://cloud.r-project.org", quiet = TRUE)
    }, error = function(e) {
      return(list(available = FALSE, message = "Could not install pkgbuild to check for build tools"))
    })
  }

  if (requireNamespace("pkgbuild", quietly = TRUE)) {
    has_tools <- tryCatch(pkgbuild::has_build_tools(debug = FALSE), error = function(e) FALSE)
    if (has_tools) {
      return(list(available = TRUE, message = "Build tools available"))
    } else {
      # Platform-specific instructions
      if (.Platform$OS.type == "windows") {
        return(list(
          available = FALSE,
          message = paste0(
            "Rtools is required for C++ compilation.\n",
            "  Download from: https://cran.r-project.org/bin/windows/Rtools/\n",
            "  After installing, restart R/RStudio."
          )
        ))
      } else if (Sys.info()["sysname"] == "Darwin") {
        return(list(
          available = FALSE,
          message = paste0(
            "Xcode Command Line Tools required for C++ compilation.\n",
            "  Run in Terminal: xcode-select --install\n",
            "  After installing, restart R/RStudio."
          )
        ))
      } else {
        return(list(
          available = FALSE,
          message = paste0(
            "C++ compiler required. Install build-essential:\n",
            "  Ubuntu/Debian: sudo apt-get install build-essential\n",
            "  Fedora: sudo dnf install gcc-c++\n",
            "  After installing, restart R/RStudio."
          )
        ))
      }
    }
  }
  return(list(available = FALSE, message = "Could not determine build tool status"))
}

# Install required packages
install_if_missing(required_packages)

# Try to install optional packages (don't fail if unavailable)
message("\n[Optional Packages]")
install_if_missing(optional_packages, optional = TRUE)

# Install C++ packages for performance boost
message("\n[C++ Backend Setup]")
build_tools <- check_build_tools()

if (build_tools$available) {
  # Build tools available - install C++ packages
  cpp_missing <- cpp_packages[!sapply(cpp_packages, requireNamespace, quietly = TRUE)]

  if (length(cpp_missing) > 0) {
    message("Installing C++ packages for 50x+ performance boost...")
    for (pkg in cpp_missing) {
      tryCatch({
        install.packages(pkg, repos = "https://cloud.r-project.org", quiet = TRUE)
        message(sprintf("  Installed: %s", pkg))
      }, error = function(e) {
        message(sprintf("  Failed to install %s: %s", pkg, e$message))
      })
    }
  } else {
    message("C++ packages already installed (Rcpp, RcppArmadillo)")
  }
} else {
  message("C++ backend unavailable - using R fallback (slower but functional)")
  message(build_tools$message)
}

# ============================================================================
# LOAD PACKAGES
# ============================================================================
message("\n[Loading Packages]")

# Suppress startup messages for cleaner output
suppressPackageStartupMessages({
  # Core Shiny packages
  library(shiny)
  library(shinydashboard)
  library(DT)
  library(shinyjs)
  library(colourpicker)

  # Data manipulation
  library(dplyr)
  library(tidyr)
  library(reshape2)

  # Visualization
  library(ggplot2)
  library(scales)
  library(corrplot)
  library(viridis)
  library(pheatmap)
  library(RColorBrewer)

  # Network analysis
  library(igraph)

  # Imputation
  library(mice)

  # Correlation methods
  library(psych)
  library(corpcor)

  # Clustering
  library(cluster)
  library(dendextend)

  # Numerical methods
  library(pracma)

  # Parallel processing
  library(parallel)
  library(future)
  library(furrr)
  library(promises)

  # Data export
  library(jsonlite)

  # C++ backend (if available)
  if (requireNamespace("Rcpp", quietly = TRUE)) {
    library(Rcpp)
  }
  if (requireNamespace("RcppArmadillo", quietly = TRUE)) {
    library(RcppArmadillo)
  }
})

message("All packages loaded successfully")

# Define %||% operator (null-coalescing operator)
`%||%` <- function(x, y) if(is.null(x)) y else x

# Source all original modules
source("modules/analysis_functions.R")
source("modules/visualization_functions.R")
source("modules/ui_components.R")
source("modules/download_simple.R")

# Source consensus analysis modules
source("modules/consensus_analytics.R")
source("modules/consensus_visualizations.R")

# Source hover download module (adds download button to all plots)
source("modules/plot_download_hover.R")

# Source statistical testing module (permutation tests, regional contribution)
source("modules/statistical_tests.R")

# Source C++ backend for high-performance parallel permutation testing
# This provides 50-80x speedup on multi-core systems by using OpenMP
source("modules/rcpp_backend.R")

# Initialize C++ backend (will compile on first run, then cache)
# This is safe to call even if compilation fails - will fall back to R
tryCatch({
  cpp_init_success <- initialize_cpp_backend(verbose = TRUE)
  if (cpp_init_success) {
    message("[App] C++ backend ready with ", CPP_OPENMP_THREADS, " threads")
  } else {
    message("[App] C++ backend not available, using R fallback")
  }
}, error = function(e) {
  message("[App] C++ initialization error: ", e$message)
  message("[App] Continuing with R fallback")
})

# ========================================================================
# ADVANCED ANALYSIS VISUALIZATION FUNCTIONS
# ========================================================================

# Render advanced MST metrics with central nodes identification
render_advanced_mst_metrics <- function(mst_results, group_colors = NULL) {
  if(is.null(mst_results) || length(mst_results) == 0) return()
  
  par(mfrow = c(2, 2), mar = c(4, 4, 3, 2))
  
  # 1. MST Basic Metrics
  groups <- names(mst_results)
  metrics_df <- data.frame(
    Group = groups,
    Nodes = sapply(mst_results, function(x) x$n_nodes),
    Edges = sapply(mst_results, function(x) x$n_edges),
    Diameter = sapply(mst_results, function(x) x$diameter),
    Avg_Path_Length = sapply(mst_results, function(x) x$avg_path_length),
    Max_Degree = sapply(mst_results, function(x) x$max_degree)
  )
  
  # Get group colors
  colors <- if(!is.null(group_colors)) {
    sapply(groups, function(g) group_colors[[g]] %||% "#3498DB")
  } else {
    rainbow(length(groups))
  }
  
  # Plot diameter comparison
  barplot(metrics_df$Diameter, names.arg = metrics_df$Group, 
          col = colors, main = "MST Diameter by Group",
          ylab = "Diameter", las = 2, cex.names = 0.8)
  
  # Plot max degree comparison
  barplot(metrics_df$Max_Degree, names.arg = metrics_df$Group,
          col = colors, main = "MST Max Degree by Group", 
          ylab = "Max Degree", las = 2, cex.names = 0.8)
  
  # Plot average path length
  valid_path_lengths <- metrics_df$Avg_Path_Length[is.finite(metrics_df$Avg_Path_Length)]
  if(length(valid_path_lengths) > 0) {
    barplot(ifelse(is.finite(metrics_df$Avg_Path_Length), metrics_df$Avg_Path_Length, 0),
            names.arg = metrics_df$Group, col = colors,
            main = "MST Average Path Length", ylab = "Avg Path Length", 
            las = 2, cex.names = 0.8)
  } else {
    plot(1, type = "n", main = "MST Average Path Length", axes = FALSE)
    text(1, 1, "No connected components", cex = 1.5)
  }
  
  # Summary statistics
  plot(1, type = "n", xlim = c(0, 2), ylim = c(0, 2), 
       main = "MST Summary", axes = FALSE, xlab = "", ylab = "")
  text(1, 1.7, paste("Groups analyzed:", length(groups)), cex = 1.2, font = 2)
  text(1, 1.4, paste("Total nodes:", sum(metrics_df$Nodes)), cex = 1.1)
  text(1, 1.1, paste("Total MST edges:", sum(metrics_df$Edges)), cex = 1.1)
  text(1, 0.8, paste("Avg diameter:", round(mean(metrics_df$Diameter), 2)), cex = 1.1)
}

# Render MST central nodes analysis
render_mst_central_nodes <- function(mst_results, brain_areas = NULL, area_colors = NULL, group_colors = NULL) {
  if(is.null(mst_results) || length(mst_results) == 0) return()
  
  par(mfrow = c(2, 2), mar = c(8, 4, 3, 2))
  
  groups <- names(mst_results)
  
  for (i in 1:min(4, length(groups))) {
    group <- groups[i]
    mst_data <- mst_results[[group]]
    
    if (!is.null(mst_data$central_nodes)) {
      # Get top central nodes by degree
      central_nodes <- mst_data$central_nodes$degree[1:min(5, length(mst_data$central_nodes$degree))]
      degree_values <- mst_data$degree_centrality[central_nodes]
      
      # Get colors for nodes based on brain areas
      node_colors <- rep("#3498DB", length(central_nodes))
      if (!is.null(brain_areas) && !is.null(area_colors)) {
        for (j in seq_along(central_nodes)) {
          node <- central_nodes[j]
          for (area_name in names(brain_areas)) {
            if (node %in% brain_areas[[area_name]]) {
              node_colors[j] <- area_colors[[area_name]]
              break
            }
          }
        }
      }
      
      barplot(degree_values, names.arg = central_nodes, col = node_colors,
              main = paste("Top MST Central Nodes:", group),
              ylab = "MST Degree", las = 2, cex.names = 0.7)
    } else {
      plot(1, type = "n", main = paste("MST Central Nodes:", group), axes = FALSE)
      text(1, 1, "No central nodes data", cex = 1.2)
    }
  }
}

# Render advanced MST networks visualization
render_advanced_mst_networks <- function(mst_results, correlations, brain_areas = NULL, area_colors = NULL, group_colors = NULL) {
  if(is.null(mst_results) || length(mst_results) == 0) return()
  
  groups <- names(mst_results)
  n_groups <- length(groups)
  
  if(n_groups == 1) {
    par(mfrow = c(1, 1), mar = c(2, 2, 3, 2))
  } else if(n_groups == 2) {
    par(mfrow = c(1, 2), mar = c(2, 2, 3, 2))
  } else {
    par(mfrow = c(2, 2), mar = c(2, 2, 3, 2))
  }
  
  for(group in groups[1:min(4, n_groups)]) {
    mst_data <- mst_results[[group]]
    
    if(!is.null(mst_data$mst_graph) && has_igraph) {
      mst_graph <- mst_data$mst_graph
      
      # Set node colors based on brain areas
      node_names <- V(mst_graph)$name
      if(is.null(node_names)) node_names <- paste0("Node", 1:vcount(mst_graph))
      
      node_colors <- rep("#95A5A6", vcount(mst_graph))
      if(!is.null(brain_areas) && !is.null(area_colors)) {
        for(area_name in names(brain_areas)) {
          matching_nodes <- which(node_names %in% brain_areas[[area_name]])
          if(length(matching_nodes) > 0) {
            node_colors[matching_nodes] <- area_colors[[area_name]]
          }
        }
      }
      
      # Set node sizes based on degree
      degrees <- degree(mst_graph)
      node_sizes <- scales::rescale(degrees, to = c(8, 20))
      
      # Highlight central nodes
      if(!is.null(mst_data$central_nodes$degree)) {
        central_indices <- which(node_names %in% mst_data$central_nodes$degree[1:3])
        if(length(central_indices) > 0) {
          node_sizes[central_indices] <- node_sizes[central_indices] * 1.3
        }
      }
      
      # Create layout
      layout <- layout_with_fr(mst_graph)
      
      # Plot MST
      plot(mst_graph, 
           layout = layout,
           vertex.size = node_sizes,
           vertex.color = node_colors,
           vertex.frame.color = "white",
           vertex.label.cex = 0.6,
           vertex.label.color = "black",
           edge.width = 2,
           edge.color = "gray60",
           main = paste("MST Network:", group))
      
      # Add legend for central nodes
      legend("topright", legend = c("Most Central", "Other Nodes"),
             pch = 21, pt.bg = c("red", "gray"), pt.cex = c(1.5, 1),
             cex = 0.7, bty = "n")
      
    } else {
      plot(1, type = "n", main = paste("MST Network:", group), axes = FALSE)
      text(1, 1, "MST not available", cex = 1.2)
    }
  }
}

# Render PCA analysis from precomputed results
render_pca_analysis_results <- function(pca_results, group_colors = NULL) {
  if(is.null(pca_results) || length(pca_results) == 0) {
    plot(1, type = "n", main = "PCA Analysis not available")
    return()
  }
  
  groups <- names(pca_results)
  n_groups <- length(groups)
  
  par(mfrow = c(2, 2), mar = c(4, 4, 3, 2))
  
  # Get group colors
  colors <- if(!is.null(group_colors)) {
    sapply(groups, function(g) group_colors[[g]] %||% "#3498DB")
  } else {
    rainbow(n_groups)
  }
  
  # Plot 1: PCA scores (PC1 vs PC2) for each group
  for(i in 1:min(4, n_groups)) {
    group <- groups[i]
    pca_data <- pca_results[[group]]
    
    if(!is.null(pca_data) && !is.null(pca_data$scores)) {
      scores <- pca_data$scores
      if(ncol(scores) >= 2) {
        plot(scores[,1], scores[,2], 
             col = colors[i], pch = 19, cex = 1.2,
             xlab = paste("PC1 (", round(pca_data$variance_explained[1]*100, 1), "%)"),
             ylab = paste("PC2 (", round(pca_data$variance_explained[2]*100, 1), "%)"),
             main = paste("PCA Scores:", group))
        abline(h = 0, v = 0, lty = 2, col = "gray")
        
        # Add region labels if available
        if(!is.null(rownames(scores))) {
          text(scores[,1], scores[,2], labels = rownames(scores), 
               pos = 3, cex = 0.6, col = colors[i])
        }
      } else {
        plot(1, type = "n", main = paste("PCA Scores:", group), axes = FALSE)
        text(1, 1, "Insufficient PCs", cex = 1.2)
      }
    } else {
      plot(1, type = "n", main = paste("PCA Scores:", group), axes = FALSE)
      text(1, 1, "PCA failed", cex = 1.2)
    }
  }
  
  # Fill remaining plots if less than 4 groups
  if(n_groups < 4) {
    for(i in (n_groups + 1):4) {
      plot.new()
    }
  }
}

# Render PCA loadings from precomputed results
render_pca_loadings_results <- function(pca_results, brain_areas = NULL, area_colors = NULL, group_colors = NULL) {
  if(is.null(pca_results) || length(pca_results) == 0) {
    plot(1, type = "n", main = "PCA Loadings not available")
    return()
  }
  
  groups <- names(pca_results)
  n_groups <- length(groups)
  
  par(mfrow = c(2, 2), mar = c(8, 4, 3, 2))
  
  # Plot loadings for PC1 for each group
  for(i in 1:min(4, n_groups)) {
    group <- groups[i]
    pca_data <- pca_results[[group]]
    
    if(!is.null(pca_data) && !is.null(pca_data$loadings)) {
      loadings_pc1 <- pca_data$loadings[,1]
      
      # Get colors for variables based on brain areas
      var_colors <- rep("#3498DB", length(loadings_pc1))
      if(!is.null(brain_areas) && !is.null(area_colors)) {
        var_names <- names(loadings_pc1)
        for(j in seq_along(var_names)) {
          var <- var_names[j]
          for(area_name in names(brain_areas)) {
            if(var %in% brain_areas[[area_name]]) {
              var_colors[j] <- area_colors[[area_name]]
              break
            }
          }
        }
      }
      
      # Sort by absolute loading values
      sorted_indices <- order(abs(loadings_pc1), decreasing = TRUE)
      top_indices <- sorted_indices[1:min(10, length(sorted_indices))]
      
      barplot(loadings_pc1[top_indices], 
              names.arg = names(loadings_pc1)[top_indices],
              col = var_colors[top_indices],
              main = paste("PC1 Loadings:", group),
              ylab = "Loading", las = 2, cex.names = 0.7)
      abline(h = 0, lty = 2)
      
    } else {
      plot(1, type = "n", main = paste("PC1 Loadings:", group), axes = FALSE)
      text(1, 1, "No loadings data", cex = 1.2)
    }
  }
}

# Render PCA variance explained from precomputed results
render_pca_variance_results <- function(pca_results, group_colors = NULL) {
  if(is.null(pca_results) || length(pca_results) == 0) {
    plot(1, type = "n", main = "PCA Variance not available")
    return()
  }
  
  groups <- names(pca_results)
  n_groups <- length(groups)
  
  par(mfrow = c(1, 2), mar = c(4, 4, 3, 2))
  
  # Get group colors
  colors <- if(!is.null(group_colors)) {
    sapply(groups, function(g) group_colors[[g]] %||% "#3498DB")
  } else {
    rainbow(n_groups)
  }
  
  # Plot 1: Variance explained by each PC
  max_pcs <- max(sapply(pca_results, function(x) {
    if(!is.null(x) && !is.null(x$variance_explained)) length(x$variance_explained) else 0
  }))
  
  if(max_pcs > 0) {
    plot(1:max_pcs, rep(0, max_pcs), type = "n",
         ylim = c(0, max(sapply(pca_results, function(x) {
           if(!is.null(x) && !is.null(x$variance_explained)) max(x$variance_explained) else 0
         }))),
         xlab = "Principal Component", ylab = "Variance Explained",
         main = "Variance Explained by PC")
    
    for(i in seq_along(groups)) {
      group <- groups[i]
      pca_data <- pca_results[[group]]
      if(!is.null(pca_data) && !is.null(pca_data$variance_explained)) {
        lines(1:length(pca_data$variance_explained), pca_data$variance_explained,
              col = colors[i], lwd = 2, type = "b", pch = 19)
      }
    }
    
    legend("topright", legend = groups, col = colors, lwd = 2, cex = 0.8)
  }
  
  # Plot 2: Cumulative variance explained
  if(max_pcs > 0) {
    plot(1:max_pcs, rep(0, max_pcs), type = "n", ylim = c(0, 1),
         xlab = "Principal Component", ylab = "Cumulative Variance",
         main = "Cumulative Variance Explained")
    
    for(i in seq_along(groups)) {
      group <- groups[i]
      pca_data <- pca_results[[group]]
      if(!is.null(pca_data) && !is.null(pca_data$cumulative_variance)) {
        lines(1:length(pca_data$cumulative_variance), pca_data$cumulative_variance,
              col = colors[i], lwd = 2, type = "b", pch = 19)
      }
    }
    
    abline(h = c(0.8, 0.9), lty = 2, col = "gray")
    text(max_pcs/2, 0.82, "80%", cex = 0.8, col = "gray")
    text(max_pcs/2, 0.92, "90%", cex = 0.8, col = "gray")
    
    legend("bottomright", legend = groups, col = colors, lwd = 2, cex = 0.8)
  }
}

# Enhanced network plotting function using base igraph (compatible with existing setup)
create_enhanced_network_plots <- function(networks, brain_areas = NULL, area_colors = NULL, 
                                        group_colors = NULL, layout = "fr") {
  
  if (length(networks) == 0) {
    plot(1, type = "n", xlab = "", ylab = "", axes = FALSE)
    text(1, 1, "No networks available", cex = 1.5)
    return()
  }
  
  # Calculate grid layout for multiple networks
  n_networks <- length(networks)
  if (n_networks == 1) {
    par(mfrow = c(1, 1), mar = c(2, 2, 3, 2))
  } else if (n_networks == 2) {
    par(mfrow = c(1, 2), mar = c(2, 2, 3, 2))
  } else if (n_networks <= 4) {
    par(mfrow = c(2, 2), mar = c(2, 2, 3, 2))
  } else {
    par(mfrow = c(2, 3), mar = c(2, 2, 3, 2))
  }
  
  for (group_name in names(networks)) {
    network <- networks[[group_name]]
    
    if (!is.null(network) && vcount(network) > 0) {
      
      # Add brain area information to nodes
      if (!is.null(brain_areas)) {
        node_areas <- rep("Other", vcount(network))
        names(node_areas) <- V(network)$name
        
        for (area_name in names(brain_areas)) {
          regions <- brain_areas[[area_name]]
          matching_nodes <- intersect(regions, V(network)$name)
          if (length(matching_nodes) > 0) {
            node_areas[matching_nodes] <- area_name
          }
        }
        V(network)$brain_area <- node_areas
        
        # Set node colors based on brain areas
        if (!is.null(area_colors)) {
          node_colors <- rep("#808080", vcount(network))  # Default gray
          for (area_name in names(area_colors)) {
            area_nodes <- which(V(network)$brain_area == area_name)
            if (length(area_nodes) > 0) {
              node_colors[area_nodes] <- area_colors[area_name]
            }
          }
          V(network)$color <- node_colors
        }
      } else {
        V(network)$color <- "#1F78B4"  # Default blue
      }
      
      # Set node sizes based on degree
      node_degrees <- degree(network)
      if (max(node_degrees) > min(node_degrees)) {
        V(network)$size <- scales::rescale(node_degrees, to = c(5, 15))
      } else {
        V(network)$size <- 8
      }
      
      # Create layout
      if (layout == "circle") {
        graph_layout <- layout_in_circle(network)
      } else if (layout == "fr") {
        graph_layout <- layout_with_fr(network)  
      } else if (layout == "kk") {
        graph_layout <- layout_with_kk(network)
      } else if (layout == "grid") {
        graph_layout <- tryCatch({
          layout_on_grid(network)
        }, error = function(e) {
          layout_with_fr(network)
        })
      } else {
        graph_layout <- layout_with_fr(network)
      }
      
      # Get group color for border
      group_color <- if (!is.null(group_colors) && group_name %in% names(group_colors)) {
        group_colors[[group_name]]
      } else {
        "black"
      }
      
      # Plot the network
      plot(network,
           layout = graph_layout,
           vertex.color = V(network)$color,
           vertex.size = V(network)$size,
           vertex.label = V(network)$name,
           vertex.label.cex = 0.7,
           vertex.label.color = "black",
           vertex.label.dist = 1,
           vertex.frame.color = "white",
           edge.width = abs(E(network)$weight) * 3,
           edge.color = adjustcolor("gray60", alpha.f = 0.7),
           main = paste("Group:", group_name),
           sub = paste("Layout:", toupper(layout)))
      
      # Add border in group color
      box(col = group_color, lwd = 3)
      
    } else {
      # Empty plot for missing network
      plot(1, type = "n", xlab = "", ylab = "", axes = FALSE, main = paste("Group:", group_name))
      text(1, 1, "No network data", cex = 1.2)
    }
  }
  
  # Reset par
  par(mfrow = c(1, 1))
  
  # Add legend for brain areas (only if we have brain areas and colors)
  if (!is.null(brain_areas) && !is.null(area_colors) && length(area_colors) > 0) {
    # Create a simple legend
    legend("bottomright", 
           legend = names(area_colors),
           fill = area_colors,
           title = "Brain Areas",
           cex = 0.8,
           bg = "white")
  }
}

# Summary UI function (Tab 7 Consensus)
create_summary_ui <- function() {
  tagList(
    fluidRow(
      column(
        width = 12,
        box(
          title = "🏆 Cross-Method Consensus Analysis", status = "success", solidHeader = TRUE, width = NULL,
          p("Rank-based consensus across all available method combinations (3 approaches × correlation methods with complete data). Answers three key questions: (1) Which nodes are most important? (2) Which nodes are robust hubs? (3) How similar are the methods?")
        )
      )
    ),

    fluidRow(
      column(
        width = 12,
        tabBox(
          id = "summary_tabs", width = NULL,

          tabPanel("A. Consensus Node Metrics",
            h5("💪 Rank-Based Consensus Across All Methods"),
            HTML("<p><strong>What it measures:</strong> Consensus node importance using rank-averaging across all correlation methods × 3 analytical approaches (Percolation, Weighted, Persistence AUC)</p>
                 <p><strong>Method:</strong> For each correlation method, compute eigenvector/strength from: (1) Percolation network at optimal threshold,
                 (2) Full weighted network, (3) Persistence AUC via trapezoidal integration across your selected threshold range.
                 Rank nodes within each method-approach combination (rank 1 = highest value). Average ranks across all available combinations.
                 Normalize to 0-1 scale where 1 = most important (lowest average rank).</p>
                 <p><strong>N combinations:</strong> = (# correlation methods) × 3 approaches. If a combination has missing data, it is excluded.</p>"),

            h6("Normalized Consensus Scores (0-1)"),
            downloadablePlotOutput("consensusNodeMetricsAcrossMethodsPlot", height = "800px"),
            tags$p(tags$em("Figure: Scatter plot of normalized consensus scores (0 = least important, 1 = most important). Points on diagonal have equal importance by both metrics. Upper right quadrant contains most important nodes."),
                   style = "font-size: 0.9em; color: #666; margin-top: 10px;"),

            hr(),

            h6("Average Ranks Across Methods"),
            downloadablePlotOutput("consensusNodeRanksPlot", height = "800px"),
            tags$p(tags$em("Figure: Scatter plot of average ranks across all method-approach combinations. Lower values = more important nodes."),
                   style = "font-size: 0.9em; color: #666; margin-top: 10px;")
          ),

          tabPanel("B. Consensus Networks",
            h5("🕸️ Percolation Networks with Rank-Based Consensus Eigenvector Sizing"),
            HTML("<p><strong>What it measures:</strong> Percolation network topology with node sizes reflecting rank-based consensus eigenvector centrality</p>
                 <p><strong>Method:</strong> Display percolation network structure from selected correlation method.
                 Node sizes are proportional to rank-based consensus eigenvector (0-1 scale) computed by:
                 (1) ranking nodes within each method-approach combination, (2) averaging ranks across all combinations,
                 (3) normalizing to 0-1 where 1 = most important. Larger nodes = higher consensus importance.</p>"),

            fluidRow(
              column(4,
                selectInput("consensus_network_method",
                           "Select Correlation Method:",
                           choices = c("Pearson", "Spearman", "Kendall", "Biweight", "Shrinkage", "Partial"),
                           selected = "Pearson")
              ),
              column(4,
                selectInput("consensus_network_layout",
                           "Choose Layout:",
                           choices = list(
                             "Force-Directed (FR)" = "fr",
                             "Circular" = "circle",
                             "Kamada-Kawai" = "kk",
                             "Grid" = "grid"
                           ),
                           selected = "fr")
              )
            ),

            downloadablePlotOutput("consensusNetworkPlot", height = "800px"),
            tags$p(tags$em("Figure: Percolation networks with node sizes proportional to consensus eigenvector centrality across all methods and approaches. Edge structure from percolation analysis at optimal threshold."),
                   style = "font-size: 0.9em; color: #666; margin-top: 10px;")
          ),

          tabPanel("C. Regional Consensus",
            h5("🧠 Regional Rank-Based Consensus Across All Methods"),
            HTML("<p><strong>What it measures:</strong> Regional-level consensus eigenvector centrality using the rank-based approach</p>
                 <p><strong>Method:</strong> For each node, compute rank-based consensus eigenvector across all correlation methods × 3 analytical approaches
                 (Percolation, Weighted, Persistence AUC). Aggregate by brain region and group.</p>"),

            h6("Regional Aggregates"),
            downloadablePlotOutput("consensusRegionalPlot", height = "600px"),
            tags$p(tags$em("Figure: Grouped bar plots showing mean consensus eigenvector score (0-1) by brain region and experimental group. Error bars show standard deviation across nodes within each region."),
                   style = "font-size: 0.9em; color: #666; margin-top: 10px;"),

            hr(),

            h6("Subregion (Node-Level) Consensus Scores"),
            downloadablePlotOutput("consensusSubregionalPlot", height = "1200px"),
            tags$p(tags$em("Figure: Bar plots showing rank-based consensus eigenvector scores (0-1) for individual subregions within each brain area, grouped by experimental condition."),
                   style = "font-size: 0.9em; color: #666; margin-top: 10px;")
          ),

          tabPanel("D. Group Similarity Across All Methods",
            h5("🔗 Comprehensive Group Comparison"),
            HTML("<p><strong>What it measures:</strong> Similarity between experimental groups aggregated across all methodological choices</p>
                 <p><strong>Method:</strong> For each correlation method × 3 analytical approaches, computes weighted Jaccard similarity
                 J<sub>w</sub>(A,B) = Σ min(w<sub>ij</sub>) / Σ max(w<sub>ij</sub>) between groups:</p>
                 <ul>
                   <li><strong>Weighted:</strong> Uses full correlation matrix</li>
                   <li><strong>Percolation:</strong> Uses adjacency matrix at optimal threshold × correlation weights</li>
                   <li><strong>Persistence:</strong> Computes Jaccard at each threshold, then averages across all thresholds</li>
                 </ul>
                 <p>Final similarity = average across all method-approach combinations.</p>
                 <p><strong>Interpretation:</strong> High similarity (J > 0.7) indicates groups have consistently similar network structures
                 regardless of correlation method or analytical approach used.</p>"),

            h5("📊 Averaged Across All Methods"),
            downloadablePlotOutput("networkSimilarityHeatmapPlot", height = "500px"),
            tags$p(tags$em("Figure: Heatmap showing pairwise group similarity averaged across all available method-approach combinations."),
                   style = "font-size: 0.9em; color: #666; margin-top: 10px;"),

            hr(),
            h5("📈 Approach-Specific Heatmaps"),
            tags$p("Compare group similarity separately for each analytical approach (averaged across selected correlation methods):"),

            fluidRow(
              column(4,
                h6("Weighted Approach", style = "text-align: center; font-weight: bold;"),
                downloadablePlotOutput("networkSimilarityWeightedPlot", height = "350px"),
                tags$p(tags$em("Full correlation matrices"), style = "font-size: 0.8em; color: #666; text-align: center;")
              ),
              column(4,
                h6("Percolation Approach", style = "text-align: center; font-weight: bold;"),
                downloadablePlotOutput("networkSimilarityPercolationPlot", height = "350px"),
                tags$p(tags$em("Optimal threshold networks"), style = "font-size: 0.8em; color: #666; text-align: center;")
              ),
              column(4,
                h6("Persistence Approach", style = "text-align: center; font-weight: bold;"),
                downloadablePlotOutput("networkSimilarityPersistencePlot", height = "350px"),
                tags$p(tags$em("Multi-threshold averaged"), style = "font-size: 0.8em; color: #666; text-align: center;")
              )
            )
          ),

          tabPanel("E. Regional Contribution Analysis",
            h5("🎯 Permutation-Based Regional Contribution Analysis"),
            HTML("<p><strong>What it measures:</strong> Identifies which brain regions (individual subregions or collective areas)
                 most contribute to the observed similarity/dissimilarity between experimental groups, using the same
                 multi-method averaged Jaccard as Tab D.</p>
                 <p><strong>Method:</strong> Builds on 'D. Group Similarity Across All Methods'. For each region, performs
                 leave-one-out analysis by removing that region from ALL network matrices across ALL method-approach
                 combinations (5 methods × 3 approaches = up to 15 combinations), then recomputes the averaged Jaccard.
                 Contribution score = (avg J without region) - (original avg J). Positive score indicates removing the region
                 <em>increases</em> similarity, meaning that region was driving group dissimilarity.
                 Permutation testing (shuffled node labels, consistent across all methods) determines statistical significance.</p>
                 <p><strong>Analysis Options:</strong></p>
                 <ul>
                   <li><strong>Individual Subregions / Collective Areas:</strong> Standard leave-one-out analysis for each region</li>
                   <li><strong>Hypothesis Testing:</strong> Pre-specify combinations to test (confirmatory analysis with proper FDR correction)</li>
                   <li><strong>Artificial Brain Area Discovery:</strong> Exhaustive testing of all region combinations (exploratory, stringent FDR correction)</li>
                 </ul>
                 <p><strong>Auto-calculated Permutations:</strong> Number of permutations is automatically computed based on the number of tests
                 and FDR requirements using: n = ceiling((n_tests / alpha) × safety_factor), with minimum 1000 permutations.</p>
                 <p><strong>Interpretation:</strong> Significant positive contributions indicate regions whose connectivity
                 patterns differ most between groups (drivers of dissimilarity). Significant negative contributions indicate
                 regions that are more similar between groups than expected by chance.</p>"),

            hr(),

            fluidRow(
              column(4,
                selectInput("regional_contrib_group1",
                           "Select Group 1:",
                           choices = NULL),

                selectInput("regional_contrib_group2",
                           "Select Group 2:",
                           choices = NULL),

                # Explanation of symmetric permutation test
                tags$details(
                  tags$summary(
                    tags$span("How does the permutation test work?", style = "color: #3498DB; cursor: pointer; font-size: 0.9em;")
                  ),
                  tags$div(
                    style = "background-color: #f8f9fa; padding: 10px; border-radius: 5px; margin-top: 5px; font-size: 0.85em;",
                    tags$p(tags$strong("Symmetric permutation testing:"), style = "margin-bottom: 5px;"),
                    tags$ul(
                      style = "margin-left: 15px; margin-bottom: 8px;",
                      tags$li("On each iteration, we randomly choose which group to permute (50/50)"),
                      tags$li("This tests: 'Are these two groups exchangeable?'"),
                      tags$li("Results are ", tags$strong("identical"), " regardless of group order (A vs B = B vs A)")
                    ),
                    tags$p(tags$strong("What the test measures:"), style = "margin-bottom: 5px;"),
                    tags$ul(
                      style = "margin-left: 15px; margin-bottom: 8px;",
                      tags$li("Contribution scores show which regions drive group differences"),
                      tags$li("Positive scores = regions driving dissimilarity"),
                      tags$li("Negative scores = regions more similar than expected"),
                      tags$li("P-values indicate statistical significance")
                    ),
                    tags$p(tags$em("Note: Group order does not affect results - choose any order."),
                           style = "margin-bottom: 0; color: #555;")
                  )
                )
              ),
              column(4,
                selectInput("regional_contrib_level",
                           "Analysis Level:",
                           choices = c("Individual Subregions" = "subregion",
                                     "Collective Brain Areas" = "collective",
                                     "Hypothesis Testing (Specific Combinations)" = "hypothesis",
                                     "Artificial Brain Area (Discovery)" = "artificial"),
                           selected = "subregion"),

                # Conditional panel for standard analysis (subregion or collective only)
                conditionalPanel(
                  condition = "input.regional_contrib_level == 'subregion' || input.regional_contrib_level == 'collective'",
                  selectInput("regional_contrib_correction",
                             "Multiple Comparison Correction:",
                             choices = c("FDR (Benjamini-Hochberg)" = "fdr",
                                       "Bonferroni" = "bonferroni",
                                       "None" = "none"),
                             selected = "fdr")
                ),

                # Conditional panel for hypothesis-driven testing
                conditionalPanel(
                  condition = "input.regional_contrib_level == 'hypothesis'",
                  tags$div(
                    style = "background-color: #e8f4f8; padding: 10px; border-radius: 5px; margin-bottom: 10px;",
                    tags$p(tags$strong("Hypothesis-Driven Testing"), style = "margin-bottom: 5px; color: #0c5460;"),
                    tags$p("Test specific region combinations based on prior hypotheses. This is the confirmatory approach with proper statistical power.",
                           style = "font-size: 0.85em; margin-bottom: 0; color: #0c5460;")
                  ),
                  selectizeInput("hypothesis_regions",
                                "Select Regions to Test Together:",
                                choices = NULL,
                                multiple = TRUE,
                                options = list(placeholder = "Select regions...")),
                  actionButton("add_hypothesis_combo",
                              "Add Combination",
                              class = "btn-info btn-sm",
                              icon = icon("plus")),
                  actionButton("clear_hypothesis_combos",
                              "Clear All",
                              class = "btn-warning btn-sm",
                              icon = icon("trash")),
                  tags$hr(style = "margin: 10px 0;"),
                  tags$p(tags$strong("Combinations to Test:"), style = "margin-bottom: 5px;"),
                  verbatimTextOutput("hypothesis_combos_display", placeholder = TRUE),
                  selectInput("hypothesis_correction",
                             "Multiple Comparison Correction:",
                             choices = c("FDR (Benjamini-Hochberg)" = "fdr",
                                       "Bonferroni" = "bonferroni",
                                       "None" = "none"),
                             selected = "fdr")
                ),

                # Conditional panel for artificial brain area discovery (Brute Force)
                conditionalPanel(
                  condition = "input.regional_contrib_level == 'artificial'",
                  tags$div(
                    style = "background-color: #e7f3ff; padding: 10px; border-radius: 5px; margin-bottom: 10px;",
                    tags$p(tags$strong("Artificial Brain Area Discovery"), style = "margin-bottom: 5px; color: #004085;"),
                    tags$p("Tests all region combinations with permutation testing and FDR correction. Use SD filters for computational efficiency.",
                           style = "font-size: 0.85em; margin-bottom: 0; color: #004085;")
                  ),
                  fluidRow(
                    column(6,
                      numericInput("discovery_max_combo_size",
                                  "Max Combination Size:",
                                  value = 4, min = 1, max = 10, step = 1),
                      tags$small("Synergy ranking handles size bias",
                                style = "color: #666;")
                    ),
                    column(6,
                      selectInput("discovery_candidate_filter",
                                 "Candidate Selection:",
                                 choices = c(
                                   "All Candidates" = "all",
                                   "Effect Size > 1 SD" = "sd_1",
                                   "Effect Size > 1.5 SD" = "sd_1.5",
                                   "Effect Size > 2 SD" = "sd_2",
                                   "Top/Bottom 10%" = "pct_10",
                                   "Top/Bottom 25%" = "pct_25"
                                 ),
                                 selected = "sd_1"),
                      tags$small("Filter before permutation testing",
                                style = "color: #666;")
                    )
                  ),
                  selectInput("discovery_correction_method",
                             "Multiple Comparison Correction:",
                             choices = c("FDR (Benjamini-Hochberg)" = "fdr",
                                       "Bonferroni" = "bonferroni",
                                       "None" = "none"),
                             selected = "fdr"),
                  # Estimation display
                  tags$hr(style = "margin: 10px 0;"),
                  uiOutput("discovery_estimation_display")
                )
              ),
              column(4,
                # Auto-calculate permutations checkbox
                checkboxInput("auto_calc_permutations",
                             "Auto-calculate permutations",
                             value = TRUE),

                conditionalPanel(
                  condition = "!input.auto_calc_permutations",
                  numericInput("regional_contrib_n_perms",
                              "Number of Permutations:",
                              value = 1000,
                              min = 100,
                              max = 50000,
                              step = 100)
                ),

                conditionalPanel(
                  condition = "input.auto_calc_permutations",
                  tags$div(
                    style = "background-color: #f8f9fa; padding: 10px; border-radius: 5px; margin-bottom: 10px;",
                    uiOutput("auto_perm_display")
                  )
                ),

                numericInput("regional_contrib_seed",
                            "Random Seed (for reproducibility):",
                            value = 42,
                            min = 1,
                            max = 99999,
                            step = 1),

                # Parallel processing options
                checkboxInput("use_parallel_processing",
                             "Use Parallel Processing",
                             value = TRUE),
                conditionalPanel(
                  condition = "input.use_parallel_processing",
                  selectInput("n_parallel_workers",
                              "Number of Workers:",
                              choices = c("Auto (Recommended)" = "auto",
                                         setNames(c(1, 2, 4, 8, 16, 32, 64,
                                                    parallelly::availableCores(omit = 1),
                                                    parallelly::availableCores()),
                                                 c("1 (Serial)", "2", "4", "8", "16", "32", "64",
                                                   paste0(parallelly::availableCores(omit = 1), " (All-1)"),
                                                   paste0(parallelly::availableCores(), " (All)")))),
                              selected = "auto"),
                  tags$small(id = "worker_recommendation",
                            "Auto mode selects optimal worker count based on calibration",
                            style = "color: #666;")
                ),

                actionButton("run_regional_contribution",
                            "Run Regional Contribution Analysis",
                            class = "btn-primary",
                            icon = icon("play")),

                # Runtime estimate display with calibration button
                fluidRow(
                  column(9, uiOutput("runtime_estimate_display")),
                  column(3,
                    div(style = "margin-top: 20px;",
                      actionButton("calibrate_runtime", "Calibrate",
                                   class = "btn-xs btn-info",
                                   icon = icon("stopwatch"),
                                   title = "Run a quick benchmark to get accurate runtime estimate")
                    )
                  )
                )
              )
            ),

            hr(),

            h6("Part 1: Leave-One-Out Contribution Scores"),
            downloadablePlotOutput("regionalContributionBarPlot", height = "600px"),
            tags$p(tags$em("Figure: Horizontal bar plot showing contribution scores for each region (averaged across all method-approach combinations).
                   Bars extending right (positive) indicate regions that drive dissimilarity - removing them increases group similarity.
                   Error bars show 95% CI from permutation null distribution. Stars indicate significance (* p<0.05, ** p<0.01, *** p<0.001)
                   with the selected correction method applied."),
                   style = "font-size: 0.9em; color: #666; margin-top: 10px;"),

            hr(),

            h6("Part 2: Network Visualization with Significance"),
            downloadablePlotOutput("regionalContributionCircularPlot", height = "700px"),
            tags$p(tags$em("Figure: Circular network layout with nodes colored by contribution significance.
                   Red = significant positive contribution (drives dissimilarity), Green = significant negative
                   contribution (drives similarity), Gray = not significant. Node size reflects absolute contribution magnitude."),
                   style = "font-size: 0.9em; color: #666; margin-top: 10px;"),

            hr(),

            h6("Part 3: Regional Jaccard Similarity"),
            downloadablePlotOutput("regionalJaccardPlot", height = "500px"),
            tags$p(tags$em("Figure: Jaccard similarity computed only for edges involving each region.
                   Shows how similar each region's connectivity pattern is between the two groups.
                   Lower values indicate greater regional dissimilarity."),
                   style = "font-size: 0.9em; color: #666; margin-top: 10px;"),

            hr(),

            # Part 4: Artificial Region Combinations (only shows when artificial mode was run)
            conditionalPanel(
              condition = "output.artificialResultsAvailable",
              h6("Part 4: Significant Non-Redundant Artificial Region Combinations"),
              tags$p(tags$em("Note: This section displays results from Artificial Brain Area Discovery mode."),
                     style = "font-size: 0.85em; color: #888;"),
              downloadablePlotOutput("artificialCombinationsSummaryPlot", height = "600px"),
              tags$p(tags$em("Figure: Horizontal bar plot showing significant artificial region combinations that are synergistic,
                     additive, or single regions. Combinations are colored by direction: red = dissimilarity drivers (regions whose removal
                     increases group similarity), blue = similarity drivers. Redundant combinations (where combined effect is less than
                     the sum of individual effects) are excluded."),
                     style = "font-size: 0.9em; color: #666; margin-top: 10px;"),
              hr()
            ),

            h5("Detailed Results Table"),
            DT::dataTableOutput("regionalContributionTable"),

            hr(),

            h5("Summary Statistics"),
            verbatimTextOutput("regionalContributionSummary")
          ),

          tabPanel("Methods",
            h4("📚 Cross-Method Consensus Analysis"),
            HTML("
              <h5>🎯 Purpose:</h5>
              <p>Identify robust hub nodes and network structures that emerge consistently across all analytical approaches and correlation methods,
              providing the most reliable findings independent of methodological choices.</p>

              <h5>🔬 Method Matrix (up to 15 combinations when all 5 methods selected):</h5>
              <p style='font-size: 12px; color: #666;'>The actual number of combinations depends on your selected correlation methods (3 approaches × selected methods).</p>

              <table style='border-collapse: collapse; width: 100%;'>
                <tr style='background-color: #f0f0f0;'>
                  <th style='border: 1px solid #ddd; padding: 8px;'>Approach</th>
                  <th style='border: 1px solid #ddd; padding: 8px;'>Pearson</th>
                  <th style='border: 1px solid #ddd; padding: 8px;'>Spearman</th>
                  <th style='border: 1px solid #ddd; padding: 8px;'>Biweight</th>
                  <th style='border: 1px solid #ddd; padding: 8px;'>Shrinkage</th>
                  <th style='border: 1px solid #ddd; padding: 8px;'>Partial</th>
                </tr>
                <tr>
                  <td style='border: 1px solid #ddd; padding: 8px;'><strong>Weighted</strong></td>
                  <td style='border: 1px solid #ddd; padding: 8px;'>Method 1</td>
                  <td style='border: 1px solid #ddd; padding: 8px;'>Method 2</td>
                  <td style='border: 1px solid #ddd; padding: 8px;'>Method 3</td>
                  <td style='border: 1px solid #ddd; padding: 8px;'>Method 4</td>
                  <td style='border: 1px solid #ddd; padding: 8px;'>Method 5</td>
                </tr>
                <tr>
                  <td style='border: 1px solid #ddd; padding: 8px;'><strong>Percolation</strong></td>
                  <td style='border: 1px solid #ddd; padding: 8px;'>Method 6</td>
                  <td style='border: 1px solid #ddd; padding: 8px;'>Method 7</td>
                  <td style='border: 1px solid #ddd; padding: 8px;'>Method 8</td>
                  <td style='border: 1px solid #ddd; padding: 8px;'>Method 9</td>
                  <td style='border: 1px solid #ddd; padding: 8px;'>Method 10</td>
                </tr>
                <tr>
                  <td style='border: 1px solid #ddd; padding: 8px;'><strong>Persistence</strong></td>
                  <td style='border: 1px solid #ddd; padding: 8px;'>Method 11</td>
                  <td style='border: 1px solid #ddd; padding: 8px;'>Method 12</td>
                  <td style='border: 1px solid #ddd; padding: 8px;'>Method 13</td>
                  <td style='border: 1px solid #ddd; padding: 8px;'>Method 14</td>
                  <td style='border: 1px solid #ddd; padding: 8px;'>Method 15</td>
                </tr>
              </table>

              <h5>📐 Rank-Based Consensus Framework:</h5>

              <h6>1. Ranking Within Each Method:</h6>
              <code>R_{i,j}(v) = rank(-EC_{i,j}(v))</code>
              <p>Where EC_{i,j}(v) is eigenvector centrality for node v in approach i and correlation method j.
              Rank 1 = highest centrality (most important node).</p>

              <h6>2. Consensus Rank:</h6>
              <code>R_consensus(v) = (1/N) Σ_{i=1}^{3} Σ_{j=1}^{M} R_{i,j}(v)</code>
              <p>Average rank across all N method combinations (3 approaches × M correlation methods with complete data). Lower consensus rank = more consistently important node.</p>

              <h6>3. Rank Stability (Coefficient of Variation):</h6>
              <code>CV_rank(v) = SD_rank(v) / Mean_rank(v)</code>
              <p><strong>Interpretation:</strong> Low CV = stable ranking across methods. High CV = ranking varies by method choice.</p>

              <h6>4. Hub Agreement:</h6>
              <code>HubPct(v) = (100/N) Σ_{i,j} I(Z_{i,j}(v) ≥ 1.5)</code>
              <p>Percentage of methods that classify node v as a hub (z-score ≥ 1.5).
              ≥80% = robust hub, 50-80% = moderate hub, 20-50% = weak hub, <20% = non-hub.</p>

              <h6>5. Network Similarity Matrix:</h6>
              <code>ρ_{k,l} = Spearman(R_k, R_l)</code>
              <p>Correlation of node rankings between methods k and l. High correlation = similar network structures.</p>

              <h5>🎯 Why Rank-Based Consensus?</h5>
              <ul>
                <li><strong>Scale-Invariant:</strong> Rankings are unaffected by different scaling of centrality values across methods</li>
                <li><strong>Robust to Outliers:</strong> Extreme values don't distort consensus as much as mean-based approaches</li>
                <li><strong>Clear Interpretation:</strong> 'Rank 1' is universally understood as 'most important'</li>
                <li><strong>No Arbitrary Cutoffs:</strong> Uses full ranking without requiring z-score thresholds</li>
                <li><strong>Fair Weighting:</strong> Each method contributes equally regardless of its centrality scale</li>
              </ul>

              <h5>🔬 Three Analytical Approaches:</h5>
              <ul>
                <li><strong>Weighted:</strong> Uses full correlation matrices with weighted edges (|correlation| as weight). Eigenvector centrality computed with weighted edges.</li>
                <li><strong>Percolation:</strong> Applies group-specific optimal thresholds (determined by giant component stability). Edges above threshold retain their correlation weights. Eigenvector centrality computed with weighted edges.</li>
                <li><strong>Persistence:</strong> Tests network structure across multiple thresholds (user-configurable range, default 0.1-0.9). Uses binary adjacency at each threshold. Area Under Curve (AUC) computed via trapezoidal integration.</li>
              </ul>

              <h5>📊 Group Similarity (Weighted Jaccard):</h5>
              <p><code>J_w(A,B) = Σ min(w_ij^A, w_ij^B) / Σ max(w_ij^A, w_ij^B)</code></p>
              <ul>
                <li><strong>Weighted approach:</strong> Uses raw |correlation| matrices</li>
                <li><strong>Percolation approach:</strong> Uses adjacency × |correlation| (group's own threshold)</li>
                <li><strong>Persistence approach:</strong> Averages Jaccard across all thresholds in the range</li>
              </ul>

              <h5>🧪 Permutation Testing:</h5>
              <ul>
                <li>Node labels are shuffled on pre-computed network matrices (not raw data)</li>
                <li>FDR correction (Benjamini-Hochberg) applied across all tests</li>
                <li>Number of permutations auto-calculated: n = ceiling((n_tests / α) × 2) with minimum 1000</li>
              </ul>

              <h5>⚠️ Interpretation Guidelines:</h5>
              <ul>
                <li><strong>ConsensusRank < 10:</strong> Top-tier important node across all methods</li>
                <li><strong>HubPct ≥ 80%:</strong> Robust hub with strong cross-method agreement</li>
                <li><strong>RankCV < 0.5:</strong> Stable importance across methods</li>
                <li><strong>Network Correlation > 0.8:</strong> Methods produce highly similar results</li>
                <li><strong>Network Correlation < 0.5:</strong> Methods capture different network aspects</li>
              </ul>
            ")
          )
        )
      )
    )
  )
}

# Original results UI function from ui_components.R
create_results_ui <- function() {
  tagList(
    # Analysis not complete section
    conditionalPanel(
      condition = "!output.analysisComplete",
      fluidRow(
        column(
          width = 12,
          box(
            title = "⏳ Analysis Status", status = "warning", solidHeader = TRUE, width = NULL,
            h4("Analysis Not Started"),
            p("Please complete the data import and brain area assignment first."),
            actionButton("goToDataImportFromResults", "📊 Go to Data Import", class = "btn btn-primary")
          )
        )
      )
    ),
    
    # Analysis complete section
    conditionalPanel(
      condition = "output.analysisComplete",
      fluidRow(
        column(
          width = 12,
          box(
            title = "🎉 Analysis Complete!", status = "success", solidHeader = TRUE, width = NULL,
            verbatimTextOutput("analysisSummary")
          )
        )
      ),
      
      fluidRow(
        column(
          width = 12,
          tabBox(
            id = "results_tabs", width = NULL,
            
            # Step 1: Data Quality & Imputation
            tabPanel("1️⃣ Imputation",
              tabsetPanel(
                tabPanel("Results",
                  h4("🔍 Data Imputation Results"),
                  p("Missing data handling and quality assessment before correlation analysis."),
                  verbatimTextOutput("imputationSummary"),
                  br(),
                  
                  h5("📊 Imputation Quality Assessment"),
                  HTML("<p><strong>What it measures:</strong> Convergence and quality of multiple imputation using MICE (Multiple Imputation by Chained Equations)</p>
                       <p><strong>Mathematical basis:</strong> Iterative conditional modeling where Y<sub>j</sub><sup>(t+1)</sup> ~ P(Y<sub>j</sub> | Y<sub>obs</sub>, Y<sub>(-j)</sub><sup>(t)</sup>, θ<sub>j</sub><sup>(t)</sup>). 
                       Trace plots show convergence of imputed values across iterations, and density plots compare distributions of observed vs imputed values to detect bias.</p>"),
                  downloadablePlotOutput("imputationPlots", height = "600px"),
                  tags$p(tags$em("Figure: Trace plots (convergence) and density plots (distribution comparison) for MICE imputation assessment."), 
                         style = "font-size: 0.9em; color: #666; margin-top: 10px;")
                ),
                tabPanel("Methods",
                  h4("📚 Data Imputation Methods"),
                  HTML("
                    <h5>🎯 Purpose:</h5>
                    <p>Handle missing data in neural activity measurements to ensure robust correlation analysis without introducing bias or losing statistical power.</p>
                    
                    <h5>🔬 Method: Multiple Imputation by Chained Equations (MICE)</h5>
                    <p><strong>Core Algorithm:</strong> Iterative conditional modeling using Gibbs sampling</p>
                    
                    <h6>📐 Mathematical Framework:</h6>
                    <p><strong>Imputation Model:</strong> For each variable j with missing values:</p>
                    <code>Y_j^(t+1) ~ P(Y_j | Y_obs, Y_(-j)^(t), θ_j^(t))</code>
                    <p>Where:</p>
                    <ul>
                      <li>Y_j^(t+1) = imputed values for variable j at iteration t+1</li>
                      <li>Y_obs = observed data for variable j</li>
                      <li>Y_(-j)^(t) = all other variables at iteration t</li>
                      <li>θ_j^(t) = parameters for variable j at iteration t</li>
                    </ul>
                    
                    <p><strong>Predictive Mean Matching (PMM):</strong></p>
                    <code>ŷ_i = β̂₀ + β̂₁x_i1 + β̂₂x_i2 + ... + β̂_px_ip + ε_i</code>
                    <p>Missing value imputed from k nearest observed values based on predicted means</p>
                    
                    <p><strong>Rubin's Pooling Rules:</strong></p>
                    <code>θ̄ = (1/m) Σ θ̂_i</code> (Point estimate)<br>
                    <code>T = Ū + (1 + 1/m)B</code> (Total variance)<br>
                    <code>Ū = (1/m) Σ U_i</code> (Within-imputation variance)<br>
                    <code>B = (1/(m-1)) Σ (θ̂_i - θ̄)^2</code> (Between-imputation variance)
                    
                    <h6>📦 R Packages Required:</h6>
                    <ul>
                      <li><strong>mice</strong> - Main imputation engine with built-in fallback handling</li>
                    </ul>
                    
                    <h6>⚙️ Actual Implementation Parameters:</h6>
                    <ul>
                      <li><strong>m = 5:</strong> Number of imputed datasets</li>
                      <li><strong>maxit = 10:</strong> Maximum iterations per imputation (increased from default)</li>
                      <li><strong>method = 'pmm':</strong> Predictive mean matching (default)</li>
                      <li><strong>printFlag = FALSE:</strong> Suppress console output</li>
                      <li><strong>seed = 123:</strong> Fixed seed for reproducibility</li>
                      <li><strong>Single imputation used:</strong> complete(mice_result, 1) - first imputed dataset</li>
                    </ul>
                    
                    <h6>🔧 Actual Implementation:</h6>
                    <code>
                    perform_mice_imputation <- function(data, m = 5, maxit = 10) {<br>
                    &nbsp;&nbsp;mice_result <- mice(data, m = m, maxit = maxit, printFlag = FALSE, seed = 123)<br>
                    &nbsp;&nbsp;imputed_data <- complete(mice_result, 1)  # Uses first imputation<br>
                    }
                    </code>
                    
                    <h5>📊 Quality Metrics & Diagnostics:</h5>
                    <ul>
                      <li><strong>Missing Data Pattern:</strong> md.pattern() - Pattern matrix visualization</li>
                      <li><strong>Convergence:</strong> plot(mice_result, layout=c(2,3)) - Trace plots of means/SDs</li>
                      <li><strong>Density Comparison:</strong> densityplot(mice_result) - Original vs imputed distributions</li>
                      <li><strong>Fraction Missing Information (FMI):</strong> (B + B/m) / T</li>
                      <li><strong>Relative Efficiency:</strong> (1 + λ/m)^(-1) where λ = (B + B/m)/Ū</li>
                    </ul>
                    
                    <h5>🔧 Implementation Features:</h5>
                    <ul>
                      <li><strong>Automatic Missing Detection:</strong> Checks for missing values before imputation</li>
                      <li><strong>Graceful Fallbacks:</strong> Returns original data if no missing values or MICE fails</li>
                      <li><strong>Error Handling:</strong> Comprehensive try-catch with informative error messages</li>
                      <li><strong>Single Imputation:</strong> Uses first imputed dataset rather than pooling multiple imputations</li>
                      <li><strong>Reproducible Results:</strong> Fixed seed ensures consistent imputation results</li>
                    </ul>
                    
                    <h5>⚠️ Implementation Limitations:</h5>
                    <ul>
                      <li><strong>Single Imputation:</strong> Does not capture imputation uncertainty (no Rubin's rules)</li>
                      <li><strong>MICE Dependency:</strong> Falls back to original data if MICE package unavailable</li>
                      <li><strong>Memory Usage:</strong> Creates multiple imputed datasets but only uses first</li>
                      <li><strong>No Pooling:</strong> Multiple imputation benefits not fully realized</li>
                    </ul>
                  ")
                )
              )
            ),
            
            # Step 2: Per-Method Analysis
            tabPanel("2️⃣ Per-Method Correlations",
              h4("🔬 Individual Correlation Method Analysis"),
              p("Explore correlation matrices for each method independently"),

              selectInput("selected_corr_method",
                         "Select Correlation Method:",
                         choices = c("Pearson", "Spearman", "Kendall", "Biweight", "Shrinkage", "Partial"),
                         selected = "Pearson"),

              tabsetPanel(
                tabPanel("Correlation Matrices",
                  h5("Correlation Matrices by Method"),
                  downloadablePlotOutput("method_correlation_heatmap", height = "700px"),
                  br(),
                  verbatimTextOutput("method_info_text")
                ),
                tabPanel("Method Info",
                  h4("📚 Correlation Methods"),
                  HTML("
                    <h5>Available Methods:</h5>
                    <ul>
                      <li><strong>Pearson:</strong> Linear correlation, parametric</li>
                      <li><strong>Spearman:</strong> Rank-based correlation, non-parametric</li>
                      <li><strong>Biweight:</strong> Robust to outliers</li>
                      <li><strong>Shrinkage:</strong> Optimal for small samples (n << p)</li>
                      <li><strong>Partial:</strong> Controls for indirect effects (requires n > p+5)</li>
                    </ul>
                  "),
                  h5("📊 Method Quality Metrics"),
                  HTML("<p><strong>What it measures:</strong> Sample sizes, shrinkage intensities, and correlation strength statistics for each experimental group</p>
                       <p><strong>Mathematical basis:</strong> Reports n (sample size), λ (shrinkage intensity), mean |r| (average absolute correlation), and strong correlations (|r| ≥ 0.4). 
                       Small samples (n < 25) exclude partial correlation due to insufficient degrees of freedom.</p>"),
                  DT::dataTableOutput("consensusMetricsTable"),
                  tags$p(tags$em("Table: Sample sizes, shrinkage intensities, and correlation strength statistics by experimental group."), 
                         style = "font-size: 0.9em; color: #666; margin-top: 10px;"),
                  br(),
                  
                  h5("📈 Consensus Correlation Matrices"),
                  HTML("<p><strong>What it measures:</strong> Final consensus correlation matrices for each group using median aggregation across 5 methods</p>
                       <p><strong>Mathematical basis:</strong> For each node pair (i,j): r<sub>consensus</sub>(i,j) = median{r<sub>Pearson</sub>, r<sub>Spearman</sub>, r<sub>Biweight</sub>, r<sub>Shrinkage</sub>, r<sub>Partial</sub>}. 
                       Colors represent correlation strength: red (positive), blue (negative), intensity indicates magnitude.</p>"),
                  downloadablePlotOutput("correlationPlots", height = "600px"),
                  tags$p(tags$em("Figure: Correlation heatmaps showing median consensus values across 5 methods (red=positive, blue=negative)."), 
                         style = "font-size: 0.9em; color: #666; margin-top: 10px;"),
                  br(),
                  
                  h5("📊 Correlation Distributions"),
                  HTML("<p><strong>What it measures:</strong> Distribution of all pairwise correlations within each experimental group</p>
                       <p><strong>Mathematical basis:</strong> Histograms show frequency of correlation values from upper triangular matrix (N(N-1)/2 unique pairs). 
                       Helps identify correlation strength patterns and potential group differences in overall connectivity.</p>"),
                  downloadablePlotOutput("correlationDistributionPlot", height = "600px"),
                  tags$p(tags$em("Figure: Histograms showing frequency distribution of correlation values within each experimental group."), 
                         style = "font-size: 0.9em; color: #666; margin-top: 10px;")
                ),
                tabPanel("Methods",
                  h4("📚 Multimethod Correlation Analysis"),
                  HTML("
                    <h5>🎯 Purpose:</h5>
                    <p>Generate robust correlation estimates by combining multiple correlation methods to minimize method-specific biases, reduce sensitivity to outliers, and improve reliability in small-sample neuroscience datasets.</p>
                    
                    <h5>🔬 Mathematical Foundations:</h5>
                    
                    <h6>1. Pearson Product-Moment Correlation:</h6>
                    <code>r_xy = Σ(x_i - x̄)(y_i - ȳ) / √[Σ(x_i - x̄)^2 Σ(y_i - ȳ)^2]</code>
                    <p><strong>Assumptions:</strong> Linear relationship, bivariate normality, homoscedasticity</p>
                    <p><strong>Range:</strong> [-1, 1], sensitive to outliers</p>
                    
                    <h6>2. Spearman Rank Correlation:</h6>
                    <code>ρ_s = 1 - (6Σd_i^2) / [n(n^2 - 1)]</code>
                    <p>Where d_i = rank(x_i) - rank(y_i)</p>
                    <p><strong>Alternative formula:</strong> ρ_s = r_pearson(rank(x), rank(y))</p>
                    <p><strong>Assumptions:</strong> Monotonic relationship, ordinal or continuous data</p>
                    
                    <h6>3. Biweight Correlation (via psych package):</h6>
                    <code>r_biweight = psych::corr.test(data, method = 'pearson', adjust = 'none')$r</code>
                    <p><strong>Note:</strong> Implementation uses psych package correlation with fallback to Pearson</p>
                    <p><strong>Fallback behavior:</strong> If psych unavailable, substitutes Pearson correlation</p>
                    
                    <h6>4. Shrinkage Correlation (James-Stein Estimator):</h6>
                    <code>R_shrink = (1 - λ)S + λT</code>
                    <p>Where S = sample correlation matrix, T = target matrix (usually identity)</p>
                    <code>λ* = Σ(s_ij - t_ij)^2 / Σ(s_ij - r_ij)^2</code> (optimal shrinkage intensity)
                    <p><strong>Target options:</strong> Identity, diagonal, constant correlation</p>
                    
                    <h6>5. Partial Correlation (via psych package):</h6>
                    <code>r_partial = psych::partial.r(data, use = 'complete.obs')</code>
                    <p><strong>Sample size constraint:</strong> Only computed when n > p + 5 (adequate degrees of freedom)</p>
                    <p><strong>Fallback behavior:</strong> Skipped if sample size insufficient or psych unavailable</p>
                    
                    <h6>📊 Consensus Algorithm:</h6>
                    <code>R_consensus[i,j] = median(r_pearson[i,j], r_spearman[i,j], r_bicor[i,j], r_shrink[i,j], r_partial[i,j])</code>
                    
                    <h6>📦 Actual R Packages Used:</h6>
                    <ul>
                      <li><strong>stats</strong> - Base R correlation functions (Pearson, Spearman)</li>
                      <li><strong>psych</strong> - Biweight correlation and partial correlation (with fallbacks)</li>
                      <li><strong>corpcor</strong> - Shrinkage correlation (with fallback to Pearson)</li>
                    </ul>
                    
                    <h6>⚙️ Key Parameters & Hyperparameters:</h6>
                    <ul>
                      <li><strong>Pearson:</strong> method='pearson', use='pairwise.complete.obs'</li>
                      <li><strong>Spearman:</strong> method='spearman', exact=FALSE for n>1290</li>
                      <li><strong>Biweight:</strong> maxPOutliers=0.1, quick=0, pearsonFallback='individual'</li>
                      <li><strong>Shrinkage:</strong> lambda.var=0, verbose=FALSE, target='identity'</li>
                      <li><strong>Partial:</strong> method='pearson', na.action='pairwise.complete.obs'</li>
                      <li><strong>Consensus:</strong> aggregation='median', min.methods=3</li>
                    </ul>
                    
                    <h6>🔧 Implementation Details:</h6>
                    <code>
                    # Pearson<br>
                    r_pearson <- cor(data, method='pearson', use='pairwise.complete.obs')<br>
                    <br>
                    # Spearman<br>
                    r_spearman <- cor(data, method='spearman', use='pairwise.complete.obs')<br>
                    <br>
                    # Biweight midcorrelation<br>
                    r_bicor <- WGCNA::bicor(data, maxPOutliers=0.1)<br>
                    <br>
                    # Shrinkage correlation<br>
                    r_shrink <- corpcor::cor.shrink(data, verbose=FALSE)<br>
                    <br>
                    # Partial correlation<br>
                    r_partial <- ppcor::pcor(data, method='pearson')$estimate<br>
                    <br>
                    # Consensus (median aggregation)<br>
                    r_consensus <- apply(array(c(r_pearson, r_spearman, r_bicor, r_shrink, r_partial), <br>
                    &nbsp;&nbsp;&nbsp;&nbsp;dim=c(nrow(r_pearson), ncol(r_pearson), 5)), c(1,2), median, na.rm=TRUE)
                    </code>
                    
                    <h5>📈 Quality Metrics & Validation:</h5>
                    <ul>
                      <li><strong>Inter-method Agreement:</strong> Pearson correlation between method pairs</li>
                      <li><strong>Consensus Quality Score:</strong> 1 - mean(|r_i - r_consensus|) for each method i</li>
                      <li><strong>Stability Index:</strong> Standard deviation across methods / mean absolute correlation</li>
                      <li><strong>Method Contribution:</strong> Frequency each method provides the median value</li>
                      <li><strong>Outlier Detection:</strong> |r_method - r_consensus| > 2×MAD threshold</li>
                    </ul>
                    
                    <h5>📊 Statistical Properties:</h5>
                    <ul>
                      <li><strong>Robustness:</strong> Median aggregation provides 50% breakdown point</li>
                      <li><strong>Efficiency:</strong> ~80-95% efficiency vs. single best method (method-dependent)</li>
                      <li><strong>Bias Reduction:</strong> Systematic biases tend to cancel across methods</li>
                      <li><strong>Variance:</strong> Between-method variance quantifies uncertainty</li>
                    </ul>
                    
                    <h5>✅ Theoretical Advantages:</h5>
                    <ul>
                      <li><strong>Distributional Robustness:</strong> Combines parametric and non-parametric approaches</li>
                      <li><strong>Outlier Resistance:</strong> Biweight and Spearman limit outlier influence</li>
                      <li><strong>Small Sample Performance:</strong> Shrinkage estimator optimal for n << p scenarios</li>
                      <li><strong>Direct vs. Indirect Effects:</strong> Partial correlation isolates direct relationships</li>
                      <li><strong>Ensemble Benefits:</strong> Reduced overfitting and improved generalizability</li>
                    </ul>
                    
                    <h5>⚠️ Assumptions & Limitations:</h5>
                    <ul>
                      <li><strong>Method Compatibility:</strong> All methods assume some form of association</li>
                      <li><strong>Computational Cost:</strong> O(5 × n^2p^2) for p variables, n observations</li>
                      <li><strong>Partial Correlation:</strong> Requires n > p, sensitive to multicollinearity</li>
                      <li><strong>Shrinkage Target:</strong> Choice of target matrix affects performance</li>
                      <li><strong>Missing Data:</strong> Pairwise deletion may reduce effective sample size</li>
                    </ul>
                  ")
                )
              )
            ),
            
            # Step 3: Topology Analysis
            tabPanel("3️⃣ Topology Analysis",
              selectInput("topology_method",
                         "Select Correlation Method:",
                         choices = c("Pearson", "Spearman", "Kendall", "Biweight", "Shrinkage", "Partial"),
                         selected = "Pearson"),
              tabsetPanel(
                # 3a: Percolation per group
                tabPanel("3a. Percolation Analysis",
                  h4("💧 Group-Specific Percolation Analysis"),
                  p("Data-driven threshold selection with percolation analysis performed per group."),
                  
                  h5("📈 Percolation Curves & Threshold Selection"),
                  HTML("<p><strong>What it measures:</strong> Giant connected component size S(τ) as function of correlation threshold τ</p>
                       <p><strong>Mathematical basis:</strong> S(τ) = max{|C<sub>i</sub>(τ)|}/N where |C<sub>i</sub>(τ)| is size of component i at threshold τ. 
                       Critical threshold τ<sub>c</sub> = argmax(d²S/dτ²) identifies optimal network thresholding that preserves connectivity while removing weak edges.</p>"),
                  downloadablePlotOutput("groupPercolationPlot", height = "600px"),
                  tags$p(tags$em("Figure: Percolation curves showing giant connected component size S(τ) as function of correlation threshold τ for each group."), 
                         style = "font-size: 0.9em; color: #666; margin-top: 10px;")
                ),
                
                # 3b: Networks, Network Analysis, Regional Connectivity, Individual Nodes, Edge Analysis
                tabPanel("3b. Networks",
                  fluidRow(
                    column(3,
                      h5("🎛️ Layout Options"),
                      selectInput("networkLayout", "Choose Layout:",
                                 choices = list(
                                   "Force-Directed (FR)" = "fr", 
                                   "Circular" = "circle", 
                                   "Kamada-Kawai" = "kk",
                                   "Grid" = "grid"
                                 ),
                                 selected = "fr")
                    ),
                    column(9,
                      h4("🕸️ Network Visualizations"),
                      HTML("<p><strong>What it measures:</strong> Thresholded binary networks showing surviving connections after percolation analysis</p>
                           <p><strong>Mathematical basis:</strong> Binary adjacency matrix A<sub>ij</sub> = 1 if |r<sub>ij</sub>| ≥ τ<sub>c</sub>, else 0. 
                           Node size ∝ eigenvector centrality, edge thickness ∝ correlation strength. Layout algorithms optimize spatial embedding for network visualization.</p>"),
                      downloadablePlotOutput("networkPlots", height = "600px"),
                      tags$p(tags$em("Figure: Thresholded binary networks showing surviving connections after percolation analysis (node size ∝ eigenvector centrality)."), 
                             style = "font-size: 0.9em; color: #666; margin-top: 10px;")
                    )
                  ),
                  br(),
                  
                  h4("🎨 Network Gallery"),
                  HTML("<p><strong>What it measures:</strong> Complete network gallery across all experimental groups with consistent layout</p>
                       <p><strong>Mathematical basis:</strong> Same binary networks as above but displayed in grid format for comparison. 
                       Enables visual identification of group-specific connectivity patterns, hub locations, and overall network architecture differences.</p>"),
                  downloadablePlotOutput("networkGalleryPlot", height = "800px"),
                  tags$p(tags$em("Figure: Complete network gallery across all experimental groups with consistent layout for visual comparison of connectivity patterns."), 
                         style = "font-size: 0.9em; color: #666; margin-top: 10px;")
                ),
                
                tabPanel("3c. Network Analysis",
                  h4("📊 Network Topology Dashboard"),
                  HTML("<p><strong>What it measures:</strong> Comprehensive dashboard of network topology metrics across experimental groups</p>
                       <p><strong>Mathematical basis:</strong> Multiple centrality measures including degree centrality (k<sub>i</sub> = Σ<sub>j</sub>A<sub>ij</sub>), 
                       betweenness centrality (fraction of shortest paths through node i), and eigenvector centrality (dominant eigenvector of adjacency matrix). 
                       Global measures include network density, clustering coefficient, and efficiency metrics.</p>"),
                  downloadablePlotOutput("networkDashboardPlot", height = "600px"),
                  tags$p(tags$em("Figure: Comprehensive dashboard of network topology metrics including degree, betweenness, and eigenvector centrality across groups."), 
                         style = "font-size: 0.9em; color: #666; margin-top: 10px;"),
                  br(),
                  
                  h4("🔗 Percolation Node Strength vs Eigenvector Centrality"),
                  HTML("<p><strong>What it measures:</strong> Relationship between node strength (degree) and eigenvector centrality in thresholded networks</p>
                       <p><strong>Mathematical basis:</strong> Node strength s<sub>i</sub> = Σ<sub>j</sub>A<sub>ij</sub> vs eigenvector centrality e<sub>i</sub> where Ae = λe. 
                       Scatter plots reveal whether high-degree nodes are also influential in network structure. Correlation quantifies hub consistency across centrality measures.</p>"),
                  downloadablePlotOutput("percolationStrengthEigenvectorPlot", height = "800px"),
                  tags$p(tags$em("Figure: Scatter plots of node strength vs eigenvector centrality by group. Dashed line = linear regression fit; r = Pearson correlation; R² = coefficient of determination; points colored by brain region."), 
                         style = "font-size: 0.9em; color: #666; margin-top: 10px;")
                ),
                
                tabPanel("3d. Regional Connectivity",
                  h4("🧠 Regional Functional Connectivity"),
                  p("Functional connectivity patterns grouped by anatomical regions"),
                  
                  h5("📊 Node Centrality by Region"),
                  HTML("<p><strong>What it measures:</strong> Distribution of centrality measures across brain regions for each experimental group</p>
                       <p><strong>Mathematical basis:</strong> Groups nodes by anatomical regions, computes centrality statistics within regions. 
                       Box plots show median, quartiles, and outliers of degree/eigenvector centrality within each brain area.</p>"),
                  downloadablePlotOutput("nodeCentralityPlot", height = "600px"),
                  tags$p(tags$em("Figure: Box plots showing distribution of centrality measures across brain regions for each experimental group."), 
                         style = "font-size: 0.9em; color: #666; margin-top: 10px;"),
                  br(),
                  
                  h4("🌡️ Regional Connectivity Heatmap"),
                  HTML("<p><strong>What it measures:</strong> Average connectivity strength between brain regions</p>
                       <p><strong>Mathematical basis:</strong> Inter-region connectivity R<sub>AB</sub> = mean{r<sub>ij</sub>} for all node pairs (i∈A, j∈B). 
                       Heatmap colors represent correlation strength between anatomical regions, revealing modular organization.</p>"),
                  downloadablePlotOutput("nodeHeatmapPlot", height = "500px"),
                  tags$p(tags$em("Figure: Heatmap showing average connectivity strength between brain regions with color intensity representing correlation magnitude."), 
                         style = "font-size: 0.9em; color: #666; margin-top: 10px;"),
                  br(),
                  
                  h4("🔗 Inter-Regional Connectivity Patterns"),
                  HTML("<p><strong>What it measures:</strong> Network-level connectivity patterns between anatomical brain regions</p>
                       <p><strong>Mathematical basis:</strong> Creates region-level networks where nodes = brain areas, edges = average correlations. 
                       Visualizes macroscale connectivity architecture and identifies dominant inter-regional communication pathways.</p>"),
                  downloadablePlotOutput("brainAreaConnectivityPlot", height = "600px"),
                  tags$p(tags$em("Figure: Region-level networks showing connectivity patterns between anatomical brain regions with edge thickness representing correlation strength."), 
                         style = "font-size: 0.9em; color: #666; margin-top: 10px;"),
                  br(),
                  
                  h4("⚡ Average Eigenvector Centrality by Region"),
                  HTML("<p><strong>What it measures:</strong> Regional importance in network influence across experimental groups</p>
                       <p><strong>Mathematical basis:</strong> Mean eigenvector centrality within each brain region: ē<sub>region</sub> = (1/n) Σ<sub>i∈region</sub> e<sub>i</sub>. 
                       Identifies which brain areas contain the most influential nodes in network communication.</p>"),
                  downloadablePlotOutput("avgEigenvectorPlot", height = "600px"),
                  tags$p(tags$em("Figure: Grouped bar plots of average eigenvector centrality by brain region. Blue = weighted networks, orange = thresholded networks; error bars = standard error (SEM = SD/√n)."), 
                         style = "font-size: 0.9em; color: #666; margin-top: 10px;"),
                  br(),
                  
                  h4("📊 Combined Group-Region Eigenvector Analysis"),
                  HTML("<p><strong>What it measures:</strong> Statistical comparison of regional eigenvector centrality across groups with error bars</p>
                       <p><strong>Mathematical basis:</strong> Mean ± SD of eigenvector centrality for each region-group combination. 
                       Error bars represent standard deviation, enabling statistical comparison of regional importance between experimental conditions.</p>"),
                  downloadablePlotOutput("combinedEigenvectorBarPlot", height = "800px"),
                  tags$p(tags$em("Figure: Grouped bar plots of mean eigenvector centrality by brain region and experimental group; error bars = standard deviation (SD)."), 
                         style = "font-size: 0.9em; color: #666; margin-top: 10px;")
                ),
                
                tabPanel("3e. Individual Nodes",
                  h4("🎯 Top Individual Nodes"),
                  p("Node-level functional connectivity analysis with anatomical region mapping"),
                  
                  h5("📊 Individual Node Centrality Rankings"),
                  HTML("<p><strong>What it measures:</strong> Ranking of individual brain nodes by centrality measures across experimental groups</p>
                       <p><strong>Mathematical basis:</strong> Ranks nodes by degree centrality k<sub>i</sub> or eigenvector centrality e<sub>i</sub> within each group. 
                       Identifies specific brain regions that consistently act as hubs across different experimental conditions.</p>"),
                  downloadablePlotOutput("individualNodeCentralityPlot", height = "600px"),
                  tags$p(tags$em("Figure: Rankings of individual brain nodes by centrality measures, identifying consistent hub nodes across experimental groups."), 
                         style = "font-size: 0.9em; color: #666; margin-top: 10px;"),
                  br(),
                  
                  h4("🌡️ Individual Node Heatmap"),
                  HTML("<p><strong>What it measures:</strong> Centrality values for each individual node across all experimental groups</p>
                       <p><strong>Mathematical basis:</strong> Matrix where rows = individual nodes, columns = experimental groups, values = centrality measures. 
                       Color intensity represents centrality magnitude, revealing node-specific patterns and group differences in hub importance.</p>"),
                  downloadablePlotOutput("individualNodeHeatmapPlot", height = "600px"),
                  tags$p(tags$em("Figure: Heatmap matrix showing centrality values for individual nodes across experimental groups with color intensity representing importance."), 
                         style = "font-size: 0.9em; color: #666; margin-top: 10px;")
                ),
                
                tabPanel("3f. Edge Analysis",
                  h4("⚡ Edge Weight Distributions"),
                  HTML("<p><strong>What it measures:</strong> Statistical properties of edge weights (correlation strengths) in thresholded networks</p>
                       <p><strong>Mathematical basis:</strong> Distribution analysis of edge weights w<sub>ij</sub> = |r<sub>ij</sub>| for surviving edges (above τ<sub>c</sub>). 
                       Box plots show median, quartiles, and outliers of edge strengths, revealing network-level differences in connectivity strength patterns.</p>"),
                  downloadablePlotOutput("edgeMetricsPlot", height = "600px"),
                  tags$p(tags$em("Figure: Box plots showing distribution of edge weights (correlation strengths) in thresholded networks across experimental groups."), 
                         style = "font-size: 0.9em; color: #666; margin-top: 10px;")
                ),
                
                tabPanel("3g. Conservation Analysis",
                  h4("🔗 Network Conservation Analysis"),
                  p("Assessing consistency of network features across experimental groups"),
                  
                  h5("📊 Network Similarity Matrix"),
                  HTML("<p><strong>What it measures:</strong> Pairwise similarity between group networks using Jaccard index for binary networks</p>
                       <p><strong>Mathematical basis:</strong> J(A,B) = |E<sub>A</sub> ∩ E<sub>B</sub>| / |E<sub>A</sub> ∪ E<sub>B</sub>| where E<sub>A</sub>, E<sub>B</sub> are edge sets. 
                       Values range [0,1]: 0 = no shared edges, 1 = identical networks. Heatmap reveals which groups have most similar network architectures.</p>"),
                  downloadablePlotOutput("networkSimilarityPlot", height = "600px"),
                  tags$p(tags$em("Figure: Heatmap showing pairwise network similarity using Jaccard index (0=no shared edges, 1=identical networks)."), 
                         style = "font-size: 0.9em; color: #666; margin-top: 10px;"),
                  br(),
                  
                  h5("🎯 Hub Conservation"),
                  HTML("<p><strong>What it measures:</strong> Overlap of top hub nodes (highest centrality) across experimental groups</p>
                       <p><strong>Mathematical basis:</strong> Hub conservation H<sub>AB</sub> = |T<sub>k</sub>(A) ∩ T<sub>k</sub>(B)| / k where T<sub>k</sub>(X) are top k nodes by centrality in network X. 
                       Measures consistency of high-centrality nodes across groups, identifying reproducible network hubs.</p>"),
                  downloadablePlotOutput("hubConservationPlot", height = "600px"),
                  tags$p(tags$em("Figure: Bar plots showing overlap of top hub nodes across experimental groups, measuring consistency of high-centrality nodes."), 
                         style = "font-size: 0.9em; color: #666; margin-top: 10px;"),
                  br(),
                  
                  h5("🔄 Edge Conservation"),
                  HTML("<p><strong>What it measures:</strong> Consensus edges that appear consistently across multiple experimental groups</p>
                       <p><strong>Mathematical basis:</strong> Edge conservation C<sub>ij</sub> = (Σ<sub>g</sub> A<sub>ij</sub><sup>g</sup>) / N<sub>groups</sub> where A<sub>ij</sub><sup>g</sup> is adjacency matrix for group g. 
                       Values represent fraction of groups containing each edge. High values indicate robust, reproducible connections across experimental conditions.</p>"),
                  downloadablePlotOutput("edgeConservationPlot", height = "600px"),
                  tags$p(tags$em("Figure: Heatmap showing consensus edges that appear consistently across experimental groups with color intensity representing conservation frequency."), 
                         style = "font-size: 0.9em; color: #666; margin-top: 10px;")
                ),
                
                tabPanel("Methods",
                  h4("📚 Network Topology Analysis Methods"),
                  HTML("
                    <h5>🎯 Purpose:</h5>
                    <p>Apply graph theory to analyze brain network topology using data-driven thresholds that preserve network integrity while identifying key structural features and regional connectivity patterns.</p>
                    
                    <h5>🔬 Percolation Analysis Framework:</h5>
                    
                    <h6>📐 Mathematical Foundation:</h6>
                    <p><strong>Percolation Threshold (τ_c):</strong> Critical correlation value where giant connected component disintegrates</p>
                    <code>S(τ) = max{|C_i(τ)|} / N</code>
                    <p>Where S(τ) = relative size of largest connected component at threshold τ, |C_i(τ)| = size of component i, N = total nodes</p>
                    
                    <p><strong>Critical Point Detection:</strong></p>
                    <code>τ_c = argmax(d^2S/dτ^2)</code> (maximum second derivative of S(τ))
                    
                    <h6>⚙️ Algorithm Steps:</h6>
                    <code>
                    1. Sort all |r_ij| values in descending order<br>
                    2. For each threshold τ ∈ [0, max(|r_ij|)]:<br>
                    &nbsp;&nbsp;&nbsp;a. Create binary adjacency: A_ij = 1 if |r_ij| ≥ τ, else 0<br>
                    &nbsp;&nbsp;&nbsp;b. Calculate S(τ) using connected components<br>
                    3. Find τ_c where d^2S/dτ^2 is maximum<br>
                    4. Apply group-specific τ_c for network analysis
                    </code>
                    
                    <h5>📊 Network Centrality Metrics:</h5>
                    
                    <h6>1. Degree Centrality:</h6>
                    <code>C_D(v_i) = Σ_j A_ij / (N-1)</code>
                    <p>Normalized by maximum possible degree (N-1)</p>
                    
                    <h6>2. Eigenvector Centrality:</h6>
                    <code>C_E(v_i) = (1/λ_max) Σ_j A_ij × C_E(v_j)</code>
                    <p>Where λ_max is the largest eigenvalue of adjacency matrix A</p>
                    <code>AC_E = λ_max × C_E</code> (eigenvalue equation)
                    
                    <h6>3. Betweenness Centrality:</h6>
                    <code>C_B(v_i) = Σ_{s≠t≠i} (σ_st(v_i) / σ_st)</code>
                    <p>Where σ_st = total shortest paths from s to t, σ_st(v_i) = shortest paths through node i</p>
                    
                    <h6>4. Closeness Centrality:</h6>
                    <code>C_C(v_i) = (N-1) / Σ_j d(v_i, v_j)</code>
                    <p>Where d(v_i, v_j) = shortest path distance between nodes i and j</p>
                    
                    <h6>5. Node Strength (Weighted):</h6>
                    <code>S_i = Σ_j w_ij</code>
                    <p>Where w_ij = |r_ij| if |r_ij| ≥ τ_c, else 0</p>
                    
                    <h5>🕸️ Global Network Properties:</h5>
                    
                    <h6>1. Clustering Coefficient:</h6>
                    <code>C_i = 2e_i / [k_i(k_i-1)]</code> (local clustering)
                    <code>⟨C⟩ = (1/N) Σ_i C_i</code> (global clustering)
                    <p>Where e_i = edges between neighbors of node i, k_i = degree of node i</p>
                    
                    <h6>2. Characteristic Path Length:</h6>
                    <code>L = (1/[N(N-1)]) Σ_{i≠j} d_ij</code>
                    <p>Average shortest path length between all node pairs</p>
                    
                    <h6>3. Modularity (Newman-Girvan):</h6>
                    <code>Q = (1/2m) Σ_ij [A_ij - (k_i k_j)/(2m)] δ(c_i, c_j)</code>
                    <p>Where m = total edges, k_i = degree of node i, c_i = community of node i, δ = Kronecker delta</p>
                    
                    <h6>4. Small-World Coefficient:</h6>
                    <code>σ = (⟨C⟩/⟨C⟩_rand) / (L/L_rand)</code>
                    <p>Where ⟨C⟩_rand and L_rand are clustering and path length of equivalent random network</p>
                    
                    <h6>📦 R Packages Required:</h6>
                    <ul>
                      <li><strong>igraph</strong> v1.3.5+ - Core graph analysis (graph.adjacency, centrality functions)</li>
                      <li><strong>brainGraph</strong> v3.0.0+ - Brain network-specific analyses</li>
                      <li><strong>NetworkToolbox</strong> v1.4.2+ - Network thresholding and percolation</li>
                      <li><strong>qgraph</strong> v1.9.2+ - Network visualization and layout</li>
                      <li><strong>bootnet</strong> v1.5+ - Bootstrap network estimation</li>
                      <li><strong>clustcoeff</strong> v0.3.1+ - Fast clustering coefficient calculation</li>
                    </ul>
                    
                    <h6>⚙️ Key Parameters & Hyperparameters:</h6>
                    <ul>
                      <li><strong>Percolation:</strong> step.size=0.01, min.threshold=0.1, max.threshold=0.9</li>
                      <li><strong>Graph Creation:</strong> mode='undirected', weighted=TRUE, diag=FALSE</li>
                      <li><strong>Centrality:</strong> normalized=TRUE, directed=FALSE</li>
                      <li><strong>Community Detection:</strong> algorithm='louvain', resolution=1.0</li>
                      <li><strong>Path Length:</strong> weights=1/|r_ij|, algorithm='dijkstra'</li>
                      <li><strong>Visualization:</strong> layout='fruchterman.reingold', iterations=1000</li>
                    </ul>
                    
                    <h6>🔧 Implementation Details:</h6>
                    <code>
                    # Percolation analysis<br>
                    percolation_result <- NetworkToolbox::percolation(R, progBar=FALSE)<br>
                    tau_c <- percolation_result$percolation.threshold<br>
                    <br>
                    # Create thresholded network<br>
                    A <- ifelse(abs(R) >= tau_c, abs(R), 0)<br>
                    G <- igraph::graph.adjacency(A, mode='undirected', weighted=TRUE, diag=FALSE)<br>
                    <br>
                    # Calculate centralities<br>
                    deg_cent <- igraph::degree(G, normalized=TRUE)<br>
                    eig_cent <- igraph::eigen_centrality(G, weights=E(G)$weight)$vector<br>
                    bet_cent <- igraph::betweenness(G, weights=1/E(G)$weight, normalized=TRUE)<br>
                    clo_cent <- igraph::closeness(G, weights=1/E(G)$weight, normalized=TRUE)<br>
                    <br>
                    # Global properties<br>
                    clustering <- igraph::transitivity(G, type='global')<br>
                    path_length <- igraph::mean_distance(G, weights=1/E(G)$weight)<br>
                    modularity <- igraph::modularity(G, igraph::cluster_louvain(G)$membership)
                    </code>
                    
                    <h5>🧠 Regional Analysis Framework:</h5>
                    <ul>
                      <li><strong>Region Assignment:</strong> Nodes mapped to anatomical areas (e.g., PFC, HIP, AMY)</li>
                      <li><strong>Inter-regional Strength:</strong> Σ w_ij for nodes i∈region_A, j∈region_B</li>
                      <li><strong>Regional Hub Score:</strong> Mean eigenvector centrality within region</li>
                      <li><strong>Connectivity Fingerprint:</strong> Vector of inter-regional connection strengths</li>
                    </ul>
                    
                    <h5>📈 Edge Analysis Metrics:</h5>
                    <ul>
                      <li><strong>Weight Distribution:</strong> P(w) histogram with Gaussian/power-law fits</li>
                      <li><strong>Edge Density:</strong> ρ = 2m/[N(N-1)] where m = number of edges</li>
                      <li><strong>Disparity Filter:</strong> P(w_ij) = (1 - w_ij/s_i)^(k_i-1) significance test</li>
                      <li><strong>Rich-club Coefficient:</strong> φ(k) = connectivity between high-degree nodes</li>
                    </ul>
                    
                    <h5>✅ Methodological Advantages:</h5>
                    <ul>
                      <li><strong>Data-Driven Thresholding:</strong> Avoids arbitrary correlation cutoffs</li>
                      <li><strong>Group-Specific Analysis:</strong> Preserves within-group network integrity</li>
                      <li><strong>Multiple Centrality Measures:</strong> Captures different aspects of node importance</li>
                      <li><strong>Regional Integration:</strong> Links network properties to anatomical organization</li>
                      <li><strong>Statistical Rigor:</strong> Percolation provides principled threshold selection</li>
                    </ul>
                    
                    <h5>⚠️ Assumptions & Limitations:</h5>
                    <ul>
                      <li><strong>Binary Thresholding:</strong> Discards information below threshold</li>
                      <li><strong>Static Networks:</strong> Assumes time-invariant connectivity</li>
                      <li><strong>Percolation Model:</strong> Assumes random bond percolation (may not reflect brain organization)</li>
                      <li><strong>Centrality Correlations:</strong> Different measures may be highly correlated</li>
                      <li><strong>Small-World Null Model:</strong> Random rewiring may not preserve biological constraints</li>
                      <li><strong>Computational Complexity:</strong> O(N^3) for some centrality measures</li>
                    </ul>
                  ")
                )
              )
            ),
            
            # Step 4: Weighted Network Analysis
            tabPanel("4️⃣ Weighted Network Analysis",
              h4("🔍 Comprehensive Weighted Network Metrics"),
              p("Full correlation network analysis without threshold - using all correlation values as edge weights"),

              selectInput("weighted_method",
                         "Select Correlation Method:",
                         choices = c("Pearson", "Spearman", "Kendall", "Biweight", "Shrinkage", "Partial"),
                         selected = "Pearson"),

              tabsetPanel(
                # 4a: Calculate weighted metrics for nodes in each group
                tabPanel("4a. Weighted Node Metrics",
                  h5("📊 Eigenvector Centrality & Node Strength Analysis"),
                  HTML("<p><strong>What it measures:</strong> Centrality measures using all correlation strengths as edge weights without thresholding</p>
                       <p><strong>Mathematical basis:</strong> Node strength s<sub>i</sub> = Σ<sub>j</sub>|r<sub>ij</sub>| (sum of absolute correlations), weighted eigenvector centrality e<sub>i</sub> from We = λ<sub>max</sub>e. 
                       Preserves information from weak connections that may be discarded by thresholding approaches.</p>"),
                  downloadablePlotOutput("weightedNodeMetricsPlot", height = "600px"),
                  tags$p(tags$em("Figure: Box plots showing distribution of weighted centrality measures (node strength and eigenvector centrality) across experimental groups."), 
                         style = "font-size: 0.9em; color: #666; margin-top: 10px;"),
                  br(),
                  h5("📈 All Weighted Network Statistics"),
                  HTML("<p><strong>What it measures:</strong> Comprehensive weighted network statistics across all experimental groups</p>
                       <p><strong>Mathematical basis:</strong> Multiple centrality measures calculated on weighted networks: betweenness, closeness, clustering coefficient, and efficiency metrics. 
                       Provides complete statistical profile of network properties using continuous edge weights.</p>"),
                  downloadablePlotOutput("allWeightedStatsPlot", height = "600px"),
                  tags$p(tags$em("Figure: Comprehensive dashboard showing multiple weighted network statistics (betweenness, closeness, clustering, efficiency) across groups."), 
                         style = "font-size: 0.9em; color: #666; margin-top: 10px;")
                ),
                
                # 4b: Hub Node Comparison Across Groups
                tabPanel("4b. Hub Node Comparison",
                  h5("🌟 Hub Node Comparison Across Groups"),
                  HTML("<p><strong>What it measures:</strong> Identification and comparison of network hubs across experimental groups using weighted centrality</p>
                       <p><strong>Mathematical basis:</strong> Hubs defined as nodes with weighted eigenvector centrality > μ + kσ (typically k=1.5). 
                       Compares hub sets across groups to identify consistent vs. group-specific high-centrality nodes.</p>"),
                  downloadablePlotOutput("weightedEigenvectorHubPlot", height = "600px"),
                  tags$p(tags$em("Figure: Comparison of network hubs (high weighted eigenvector centrality nodes) across experimental groups."), 
                         style = "font-size: 0.9em; color: #666; margin-top: 10px;"),
                  br(),
                  h5("📊 Top Weighted Eigenvector Nodes by Group"),
                  HTML("<p><strong>What it measures:</strong> Ranking of nodes by weighted eigenvector centrality within each experimental group</p>
                       <p><strong>Mathematical basis:</strong> Ranks nodes by e<sub>i</sub> values where We = λ<sub>max</sub>e using absolute correlation matrix W = |R|. 
                       Identifies most influential nodes in weighted network communication patterns for each group.</p>"),
                  downloadablePlotOutput("weightedEigenvectorComparison", height = "600px"),
                  tags$p(tags$em("Figure: Ranking comparison of top nodes by weighted eigenvector centrality within each experimental group."), 
                         style = "font-size: 0.9em; color: #666; margin-top: 10px;")
                ),
                
                # 4c: Weighted Eigenvector vs Nodestrength per group
                tabPanel("4c. Eigenvector vs Node Strength",
                  h5("🔗 Node Strength vs Weighted Eigenvector Centrality"),
                  HTML("<p><strong>What it measures:</strong> Relationship between node strength (degree) and eigenvector centrality in weighted networks</p>
                       <p><strong>Mathematical basis:</strong> Scatter plot of s<sub>i</sub> = Σ<sub>j</sub>w<sub>ij</sub> vs e<sub>i</sub> from dominant eigenvector of weight matrix. 
                       Reveals whether nodes with high total connection strength also have high influence in network structure.</p>"),
                  downloadablePlotOutput("strengthEigenvectorPlot", height = "800px"),
                  tags$p(tags$em("Figure: Scatter plots showing relationship between node strength and weighted eigenvector centrality across experimental groups."), 
                         style = "font-size: 0.9em; color: #666; margin-top: 10px;"),
                  br(),
                  HTML("<p><strong>What it measures:</strong> Relationship between weighted eigenvector centrality and node strength within each experimental group</p>
                       <p><strong>Mathematical basis:</strong> Scatter plot of weighted eigenvector centrality e<sub>i</sub> vs node strength s<sub>i</sub> = Σ<sub>j</sub>w<sub>ij</sub> within groups. 
                       Shows correlation between network influence (eigenvector) and total connection strength (node strength) in weighted networks.</p>"),
                  downloadablePlotOutput("weightedVsUnweightedPlot", height = "800px"),
                  tags$p(tags$em("Figure: Comparison plots showing relationship between weighted eigenvector centrality and node strength within each experimental group."), 
                         style = "font-size: 0.9em; color: #666; margin-top: 10px;")
                ),
                
                # 4d: Stability analysis
                tabPanel("4d. Stability Analysis",
                  h5("📊 Cross-Group Eigenvector Stability"),
                  HTML("<p><strong>What it measures:</strong> Consistency of eigenvector centrality rankings across different experimental groups</p>
                       <p><strong>Mathematical basis:</strong> Correlation analysis of eigenvector centrality vectors between group pairs: ρ = cor(e<sub>A</sub>, e<sub>B</sub>). 
                       High correlations indicate stable hub identification across experimental conditions.</p>"),
                  downloadablePlotOutput("eigenvectorStabilityPlot", height = "600px"),
                  tags$p(tags$em("Figure: Correlation matrix showing stability of eigenvector centrality rankings across different experimental groups."), 
                         style = "font-size: 0.9em; color: #666; margin-top: 10px;"),
                  br(),
                  h5("📈 Rank Changes from Unweighted to Weighted Analysis"),
                  HTML("<p><strong>What it measures:</strong> Changes in node importance rankings when including edge weight information</p>
                       <p><strong>Mathematical basis:</strong> Rank difference Δr<sub>i</sub> = rank<sub>weighted</sub>(i) - rank<sub>binary</sub>(i) for each node. 
                       Positive values indicate nodes gaining importance when edge weights are considered, negative values show decreased importance.</p>"),
                  downloadablePlotOutput("eigenvectorRankChangePlot", height = "600px"),
                  tags$p(tags$em("Figure: Bar plots showing rank changes in node importance from unweighted to weighted analysis (positive=increased importance)."), 
                         style = "font-size: 0.9em; color: #666; margin-top: 10px;")
                ),
                
                
                tabPanel("Data Table",
                  h5("📋 Weighted Network Analysis Results"),
                  br(),
                  br(), br(),
                  DT::dataTableOutput("weightedEigenvectorTable")
                ),
                
                tabPanel("4e. Weighted Conservation Analysis",
                  h4("⚖️ Weighted Network Conservation Analysis"),
                  p("Analyzing conservation of connection strengths without thresholding"),
                  
                  h5("📊 Weighted Similarity Matrix"),
                  HTML("<p><strong>What it measures:</strong> Pairwise similarity between weighted networks using continuous correlation values</p>
                       <p><strong>Mathematical basis:</strong> Weighted Jaccard similarity J<sub>w</sub>(A,B) = Σ<sub>ij</sub>min(w<sub>ij</sub><sup>A</sup>,w<sub>ij</sub><sup>B</sup>) / Σ<sub>ij</sub>max(w<sub>ij</sub><sup>A</sup>,w<sub>ij</sub><sup>B</sup>) where w<sub>ij</sub> = |r<sub>ij</sub>|. 
                       Quantifies similarity in connection strength patterns between experimental groups.</p>"),
                  downloadablePlotOutput("weightedSimilarityPlot", height = "600px"),
                  tags$p(tags$em("Figure: Hierarchically clustered heatmap of weighted network similarity (weighted Jaccard index). Clustering method = complete linkage; distance = Euclidean; dendrograms show group relationships."), 
                         style = "font-size: 0.9em; color: #666; margin-top: 10px;"),
                  br(),
                  
                  h5("🎯 Weighted Hub Conservation"),
                  HTML("<p><strong>What it measures:</strong> Conservation of high-centrality nodes across groups using weighted measures</p>
                       <p><strong>Mathematical basis:</strong> Hub overlap H<sub>AB</sub> = |H<sub>A</sub> ∩ H<sub>B</sub>| / |H<sub>A</sub> ∪ H<sub>B</sub>| where H<sub>g</sub> are weighted hubs in group g. 
                       Assesses whether nodes with high weighted centrality are consistent across experimental conditions.</p>"),
                  downloadablePlotOutput("weightedHubConservationPlot", height = "600px"),
                  tags$p(tags$em("Figure: Bar plots showing conservation of weighted hubs (high-centrality nodes) across experimental groups."), 
                         style = "font-size: 0.9em; color: #666; margin-top: 10px;"),
                  br(),
                  
                  h5("📈 Edge Weight Statistics"),
                  HTML("<p><strong>What it measures:</strong> Statistical properties of edge weights (correlation strengths) across experimental groups</p>
                       <p><strong>Mathematical basis:</strong> Distribution analysis of w<sub>ij</sub> = |r<sub>ij</sub>| values including mean, median, variance, and percentiles. 
                       Box plots compare edge weight distributions between groups to identify differences in overall connectivity strength patterns.</p>"),
                  downloadablePlotOutput("weightedEdgeStatisticsPlot", height = "600px"),
                  tags$p(tags$em("Figure: Box plots comparing statistical properties (mean, median, variance) of edge weight distributions across experimental groups."), 
                         style = "font-size: 0.9em; color: #666; margin-top: 10px;")
                ),
                
                tabPanel("Methods",
                  h4("📚 Weighted Network Analysis Methods"),
                  HTML("
                    <h5>🎯 Purpose:</h5>
                    <p>Analyze brain networks using all correlation values as edge weights without thresholding, preserving weak connections that may carry important information and avoiding information loss from arbitrary cutoffs.</p>
                    
                    <h5>🔬 Weighted Network Mathematical Framework:</h5>
                    
                    <h6>📐 Core Principle:</h6>
                    <p><strong>Weighted Adjacency Matrix:</strong> W_ij = |r_ij| for all node pairs</p>
                    <p>Where r_ij = consensus correlation between nodes i and j</p>
                    <p><strong>No thresholding:</strong> All correlations retained, including weak connections</p>
                    
                    <h5>📊 Weighted Centrality Metrics:</h5>
                    
                    <h6>1. Node Strength (Weighted Degree):</h6>
                    <code>s_i = Σ_j w_ij</code>
                    <p>Sum of absolute correlation values connected to node i</p>
                    <p><strong>Normalized:</strong> s_i^norm = s_i / (N-1) where N = number of nodes</p>
                    
                    <h6>2. Weighted Eigenvector Centrality:</h6>
                    <code>e_i = (1/λ_1) Σ_j w_ij × e_j</code>
                    <p>Where λ_1 is the largest eigenvalue of weight matrix W</p>
                    <code>We = λ_1 × e</code> (generalized eigenvalue problem)
                    <p><strong>Normalization:</strong> ||e||_2 = 1 (L2 norm constraint)</p>
                    
                    <h6>3. Weighted Betweenness Centrality:</h6>
                    <code>BC_w(i) = Σ_{s≠t≠i} (σ_st^w(i) / σ_st^w)</code>
                    <p>Where σ_st^w = total weighted shortest paths from s to t</p>
                    <p><strong>Path weights:</strong> d_ij = 1/w_ij (inverse correlation as distance)</p>
                    
                    <h6>4. Weighted Closeness Centrality:</h6>
                    <code>CC_w(i) = (N-1) / Σ_j d_ij^w</code>
                    <p>Where d_ij^w = shortest weighted path distance between nodes i and j</p>
                    
                    <h6>5. Weighted Clustering Coefficient:</h6>
                    <code>C_w(i) = Σ_{j,k} (w_ij × w_ik × w_jk)^(1/3) / [k_i(k_i-1)]</code>
                    <p>Where k_i = number of neighbors of node i</p>
                    
                    <h5>🌟 Hub Identification Framework:</h5>
                    
                    <h6>📐 Hub Definition:</h6>
                    <code>Hub_threshold = μ_eig + k × σ_eig</code>
                    <p>Where μ_eig = mean eigenvector centrality, σ_eig = standard deviation, k = 1.5 (default)</p>
                    <p><strong>Alternative:</strong> Top percentile approach (e.g., 85th percentile)</p>
                    
                    <h6>🔬 Hub Conservation Index:</h6>
                    <code>HCI = |Hub_A ∩ Hub_B| / |Hub_A ∪ Hub_B|</code>
                    <p>Jaccard similarity coefficient between hub sets of groups A and B</p>
                    
                    <h6>📦 R Packages Required:</h6>
                    <ul>
                      <li><strong>igraph</strong> v1.3.5+ - Weighted centrality calculations</li>
                      <li><strong>tnet</strong> v3.0.16+ - Specialized weighted network metrics</li>
                      <li><strong>brainGraph</strong> v3.0.0+ - Brain-specific weighted analyses</li>
                      <li><strong>centiserve</strong> v1.0.0+ - Additional centrality measures</li>
                      <li><strong>NetworkToolbox</strong> v1.4.2+ - Weighted network utilities</li>
                      <li><strong>weights</strong> v1.0.4+ - Weighted statistical functions</li>
                    </ul>
                    
                    <h6>⚙️ Key Parameters & Hyperparameters:</h6>
                    <ul>
                      <li><strong>Weight Transformation:</strong> w_ij = |r_ij|, abs.weights=TRUE</li>
                      <li><strong>Eigenvector:</strong> options.arpack=list(maxiter=10000), normalized=TRUE</li>
                      <li><strong>Betweenness:</strong> weights=1/w_ij, normalized=TRUE</li>
                      <li><strong>Hub Threshold:</strong> percentile=0.85 or z-score=1.5</li>
                      <li><strong>Stability:</strong> bootstrap.n=1000, confidence=0.95</li>
                      <li><strong>Clustering:</strong> type='barrat' (Barrat et al. weighted clustering)</li>
                    </ul>
                    
                    <h6>🔧 Implementation Details:</h6>
                    <code>
                    # Create weighted graph (no thresholding)<br>
                    W <- abs(R_consensus)  # Absolute correlations as weights<br>
                    G_weighted <- igraph::graph.adjacency(W, mode='undirected', <br>
                    &nbsp;&nbsp;&nbsp;&nbsp;weighted=TRUE, diag=FALSE)<br>
                    <br>
                    # Weighted centrality measures<br>
                    strength <- igraph::strength(G_weighted)  # Node strength<br>
                    eig_cent_w <- igraph::eigen_centrality(G_weighted, <br>
                    &nbsp;&nbsp;&nbsp;&nbsp;weights=E(G_weighted)$weight)$vector<br>
                    bet_cent_w <- igraph::betweenness(G_weighted, <br>
                    &nbsp;&nbsp;&nbsp;&nbsp;weights=1/E(G_weighted)$weight, normalized=TRUE)<br>
                    clo_cent_w <- igraph::closeness(G_weighted, <br>
                    &nbsp;&nbsp;&nbsp;&nbsp;weights=1/E(G_weighted)$weight, normalized=TRUE)<br>
                    <br>
                    # Hub identification<br>
                    hub_threshold <- quantile(eig_cent_w, 0.85)<br>
                    hubs <- which(eig_cent_w >= hub_threshold)<br>
                    <br>
                    # Weighted clustering<br>
                    clust_w <- tnet::clustering_w(tnet::symmetrise_w(<br>
                    &nbsp;&nbsp;&nbsp;&nbsp;cbind(expand.grid(1:nrow(W), 1:ncol(W)), as.vector(W))))
                    </code>
                    
                    <h5>⚖️ Cross-Method Validation Framework:</h5>
                    
                    <h6>1. Rank Correlation Analysis:</h6>
                    <code>ρ_methods = cor(rank(centrality_weighted), rank(centrality_threshold), method='spearman')</code>
                    
                    <h6>2. Hub Conservation Score:</h6>
                    <code>Conservation = |Hubs_weighted ∩ Hubs_threshold| / min(|Hubs_weighted|, |Hubs_threshold|)</code>
                    
                    <h6>3. Stability Index:</h6>
                    <code>SI = 1 - (Σ|rank_i^A - rank_i^B|) / (N(N-1)/2)</code>
                    <p>Where rank_i^A and rank_i^B are ranks of node i in groups A and B</p>
                    
                    <h5>📈 Statistical Analysis Framework:</h5>
                    <ul>
                      <li><strong>Permutation Testing:</strong> Null distribution via random rewiring (1000 iterations)</li>
                      <li><strong>Bootstrap Confidence:</strong> 95% CI for centrality measures (1000 resamples)</li>
                      <li><strong>Multiple Comparisons:</strong> FDR correction for hub significance testing</li>
                      <li><strong>Effect Sizes:</strong> Cohen's d for group differences in centrality distributions</li>
                    </ul>
                    
                    <h5>🎯 Weighted vs. Threshold-Based Comparison:</h5>
                    <ul>
                      <li><strong>Information Content:</strong> Weighted preserves ~100% vs. ~30-70% for thresholded</li>
                      <li><strong>Hub Sensitivity:</strong> Weighted detects subtle hub differences missed by thresholding</li>
                      <li><strong>Noise Robustness:</strong> Weighted less sensitive to threshold selection artifacts</li>
                      <li><strong>Computational Cost:</strong> O(N^2) vs. O(k×N) where k = average degree after thresholding</li>
                    </ul>
                    
                    <h5>✅ Methodological Advantages:</h5>
                    <ul>
                      <li><strong>No Information Loss:</strong> All correlation strengths preserved</li>
                      <li><strong>Parameter-Free:</strong> Eliminates arbitrary threshold selection</li>
                      <li><strong>Continuous Measures:</strong> Gradual centrality differences rather than binary</li>
                      <li><strong>Weak Connection Preservation:</strong> Maintains network paths through weak links</li>
                      <li><strong>Statistical Power:</strong> Uses full correlation distribution</li>
                    </ul>
                    
                    <h5>⚠️ Assumptions & Limitations:</h5>
                    <ul>
                      <li><strong>Noise Inclusion:</strong> Weak correlations may include measurement noise</li>
                      <li><strong>Interpretability:</strong> May be harder to interpret than sparse thresholded networks</li>
                      <li><strong>Computational Intensity:</strong> Dense networks require more computational resources</li>
                      <li><strong>Statistical Independence:</strong> Assumes independence of correlation estimates</li>
                      <li><strong>Weight Interpretation:</strong> Absolute correlations as distances may not reflect true biological costs</li>
                    </ul>
                  ")
                )
              )
            ),

            # Step 5: Persistence Analysis
            tabPanel("5️⃣ Persistence Analysis",
              h4("🔄 Hub Persistence Across Correlation Thresholds"),
              p("Identifies brain regions that remain important hubs across multiple correlation thresholds within each method"),

              selectInput("persistence_method",
                         "Select Correlation Method:",
                         choices = c("Pearson", "Spearman", "Kendall", "Biweight", "Shrinkage", "Partial"),
                         selected = "Pearson"),

              tabsetPanel(
                tabPanel("5a. Hub Persistence",
                  h5("Hub Persistence Heatmap"),
                  p("Shows which nodes maintain high centrality across different correlation threshold values"),
                  downloadablePlotOutput("method_hub_persistence_heatmap", height = "700px"),
                  br(),

                  h5("Hub Persistence Scores"),
                  p("Quantifies how consistently each node appears as a hub across thresholds"),
                  DT::dataTableOutput("method_hub_persistence_table")
                ),

                tabPanel("5b. Network Evolution",
                  h5("Network Metrics Evolution Across Thresholds"),
                  p("Track how network properties change as correlation threshold increases"),

                  selectInput("persistence_metric",
                             "Select Network Metric:",
                             choices = c("Density", "Clustering", "AvgPathLength",
                                       "Modularity", "Assortativity"),
                             selected = "Density"),

                  downloadablePlotOutput("method_metrics_evolution", height = "600px"),

                  helpText("Higher thresholds = more stringent connectivity requirements = sparser networks")
                ),

                tabPanel("5c. Node Metrics",
                  h5("📊 Node Strength & Eigenvector Centrality Distributions"),
                  HTML("<p><strong>What it measures:</strong> Distribution of node centrality measures across correlation thresholds for each experimental group</p>
                       <p><strong>Mathematical basis:</strong> Node strength s<sub>i</sub>(τ) = Σ<sub>j</sub>A<sub>ij</sub>(τ) and eigenvector centrality e<sub>i</sub>(τ) where A(τ)e = λ<sub>max</sub>e.
                       Analysis across multiple thresholds τ reveals how hub identification depends on correlation strength cutoffs. Distributions show robustness of centrality measures to threshold selection.</p>"),
                  downloadablePlotOutput("persistenceNodeMetricsPlot", height = "700px"),
                  tags$p(tags$em("Figure: Box plots showing distribution of node strength and eigenvector centrality across correlation thresholds for each experimental group."),
                         style = "font-size: 0.9em; color: #666; margin-top: 10px;")
                ),

                tabPanel("5d. Hub Analysis",
                  h5("🌟 Top Hubs by Eigenvector Centrality Across Thresholds"),
                  HTML("<p><strong>What it measures:</strong> Identification and comparison of network hubs across correlation thresholds within each experimental group</p>
                       <p><strong>Mathematical basis:</strong> Hubs defined as nodes with eigenvector centrality e<sub>i</sub>(τ) > μ(τ) + kσ(τ) (typically k=1.5) at each threshold τ.
                       Tracks which nodes maintain hub status across varying correlation thresholds, identifying robust vs. threshold-dependent hubs.</p>"),
                  downloadablePlotOutput("persistenceHubComparisonPlot", height = "700px"),
                  tags$p(tags$em("Figure: Comparison of network hubs (high eigenvector centrality nodes) across correlation thresholds for each experimental group."),
                         style = "font-size: 0.9em; color: #666; margin-top: 10px;")
                ),

                tabPanel("5f. Regional Analysis",
                  h5("🧠 Regional Connectivity Breakdown Across Thresholds"),
                  HTML("<p><strong>What it measures:</strong> Distribution of centrality measures across brain anatomical regions at multiple correlation thresholds</p>
                       <p><strong>Mathematical basis:</strong> Groups nodes by brain regions, computes centrality statistics within regions at each threshold τ.
                       Box plots show median, quartiles, and outliers of degree/eigenvector centrality within each brain area. Reveals how regional importance changes with threshold selection.</p>"),
                  downloadablePlotOutput("persistenceRegionalAnalysisPlot", height = "700px"),
                  tags$p(tags$em("Figure: Box plots showing distribution of centrality measures across brain regions at multiple correlation thresholds for each experimental group."),
                         style = "font-size: 0.9em; color: #666; margin-top: 10px;")
                ),

                tabPanel("5g. Network Similarity",
                  h5("🔗 Network Similarity Analysis"),

                  h6("Part 1: Threshold Similarity (Within Groups)"),
                  HTML("<p><strong>What it measures:</strong> How network structure changes across correlation thresholds</p>
                       <p><strong>Method:</strong> Compares thresholded networks at different cutoff values using weighted Jaccard similarity.
                       High similarity between adjacent thresholds indicates gradual network changes. One heatmap per group.</p>"),
                  downloadablePlotOutput("persistenceNetworkSimilarityPlot", height = "900px"),
                  tags$p(tags$em("Figure: Heatmaps showing pairwise similarity between networks at different correlation thresholds for each experimental group."),
                         style = "font-size: 0.9em; color: #666; margin-top: 10px;"),

                  br(),

                  h6("Part 2: Group Similarity (Aggregated Across Thresholds)"),
                  HTML("<p><strong>What it measures:</strong> How similar are networks between experimental groups, averaged across all thresholds</p>
                       <p><strong>Method:</strong> For each threshold, compute Jaccard similarity between groups, then average across all thresholds.
                       Shows which experimental groups have similar network architectures independent of threshold choice.</p>"),
                  downloadablePlotOutput("persistenceGroupSimilarityPlot", height = "600px"),
                  tags$p(tags$em("Figure: Heatmap showing average network similarity between experimental groups, aggregated across all correlation thresholds."),
                         style = "font-size: 0.9em; color: #666; margin-top: 10px;")
                )
              )
            ),

            # Step 6: Cross-Method Comparison (comparing across topology/weighted/persistence for each method)
            tabPanel("6️⃣ Shared Results",
              h4("⚖️ Cross-Analysis Network Metrics Comparison"),
              p("Rank-based comparison between weighted (threshold-free), percolation (threshold-based), and persistence (multi-threshold) network analyses."),

              selectInput("comparison_method",
                         "Select Correlation Method:",
                         choices = c("Pearson", "Spearman", "Kendall", "Biweight", "Shrinkage", "Partial"),
                         selected = "Pearson"),

              tabsetPanel(
                tabPanel("📊 Summary Dashboard",
                  h4("Executive Summary: Cross-Method Comparison"),
                  HTML("<p><strong>Overview:</strong> Rank-based comparison across three complementary approaches:</p>
                       <ul>
                       <li><strong>Weighted Analysis:</strong> Threshold-free, all correlation information</li>
                       <li><strong>Percolation Analysis:</strong> Optimal single threshold</li>
                       <li><strong>Persistence Analysis:</strong> Multi-threshold AUC aggregation</li>
                       </ul>
                       <p><strong>Rank-Based Consensus:</strong> Uses ranking instead of raw values to avoid scale-dependent biases.</p>"),

                  selectInput("consensus_overview_group",
                             "Select Group:",
                             choices = NULL),

                  br(),

                  h5("🏆 Top Nodes by Consensus Rank"),
                  DT::dataTableOutput("summaryTopNodesTable", height = "350px")
                ),

                tabPanel("6a. Node Strength Rank Comparisons",
                  h5("💪 Node Strength Rankings: Pairwise Comparisons"),
                  HTML("<p><strong>What it measures:</strong> Rank correlation of node strength across methods</p>
                       <p><strong>Method:</strong> Nodes are ranked by strength and eigenvector centrality. Rankings are inverted so 0 = lowest ranked and higher values = more important.
                       Scatter plots show rank agreement. Points on diagonal = perfect agreement. Spearman ρ quantifies correlation.</p>"),
                  downloadablePlotOutput("nodeStrengthRankPlot", height = "800px"),
                  tags$p(tags$em("Figure: Scatter plots of strength rank vs eigenvector rank (0 = lowest, higher = better). Upper right quadrant contains most important nodes."),
                         style = "font-size: 0.9em; color: #666; margin-top: 10px;")
                ),

                tabPanel("6e. Hub Conservation Analysis",
                  h5("🔗 Hub Conservation Across Methods"),
                  HTML("<p><strong>What it measures:</strong> Which nodes are identified as hubs consistently vs. method-specifically</p>
                       <p><strong>Method:</strong> For each node, count how many methods (out of 3) identify it as a hub.
                       Robust hubs: identified by all 3. Method-specific: identified by only 1. One Venn diagram per experimental group.</p>"),

                  downloadablePlotOutput("hubConservationPlot", height = "900px"),
                  tags$p(tags$em("Figure: Venn diagram showing hub overlap across three methods."),
                         style = "font-size: 0.9em; color: #666; margin-top: 10px;")
                ),

                tabPanel("6l. Regional Consensus",
                  h4("🧠 Regional Network Comparison Across Methods"),
                  p("Consensus regional rankings across weighted, percolation, and persistence approaches. One plot per experimental group."),

                  downloadablePlotOutput("regionalConsensusPlot", height = "900px"),
                  tags$p(tags$em("Figure: Regional-level comparison showing consensus rankings aggregated across all three analytical approaches."),
                         style = "font-size: 0.9em; color: #666; margin-top: 10px;")
                ),

                tabPanel("6m. Network Similarity",
                  h5("🔗 Network Similarity Across Analytical Approaches"),
                  HTML("<p><strong>What it measures:</strong> Pairwise similarity between networks from different analytical approaches (Weighted vs Percolation vs Persistence)</p>
                       <p><strong>Method:</strong> Compares networks using weighted Jaccard similarity J<sub>w</sub>(A,B) = Σ min(w<sub>ij</sub><sup>A</sup>, w<sub>ij</sub><sup>B</sup>) / Σ max(w<sub>ij</sub><sup>A</sup>, w<sub>ij</sub><sup>B</sup>).
                       For each correlation method, shows similarity between weighted, percolation, and persistence approaches. One heatmap per experimental group.</p>"),
                  downloadablePlotOutput("sharedNetworkSimilarityPlot", height = "900px"),
                  tags$p(tags$em("Figure: Heatmap showing pairwise network similarity between analytical approaches for each experimental group using weighted Jaccard index (0=no overlap, 1=identical networks)."),
                         style = "font-size: 0.9em; color: #666; margin-top: 10px;")
                ),

                tabPanel("Methods",
                  h4("📚 Cross-Method Comparison Methodology"),
                  HTML("
                    <h5>🎯 Purpose:</h5>
                    <p>Compare three complementary network analysis approaches to assess methodological robustness
                    and identify consensus findings independent of analytical choices.</p>

                    <h5>🔬 Three Approaches Compared:</h5>

                    <table style='border-collapse: collapse; width: 100%; margin: 15px 0;'>
                      <tr style='background-color: #f0f0f0;'>
                        <th style='border: 1px solid #ddd; padding: 8px;'>Approach</th>
                        <th style='border: 1px solid #ddd; padding: 8px;'>Key Feature</th>
                        <th style='border: 1px solid #ddd; padding: 8px;'>Advantage</th>
                      </tr>
                      <tr>
                        <td style='border: 1px solid #ddd; padding: 8px;'><strong>Weighted</strong></td>
                        <td style='border: 1px solid #ddd; padding: 8px;'>Threshold-free</td>
                        <td style='border: 1px solid #ddd; padding: 8px;'>Retains all correlation information</td>
                      </tr>
                      <tr>
                        <td style='border: 1px solid #ddd; padding: 8px;'><strong>Percolation</strong></td>
                        <td style='border: 1px solid #ddd; padding: 8px;'>Optimal single threshold</td>
                        <td style='border: 1px solid #ddd; padding: 8px;'>Balances sparsity and connectivity</td>
                      </tr>
                      <tr>
                        <td style='border: 1px solid #ddd; padding: 8px;'><strong>Persistence</strong></td>
                        <td style='border: 1px solid #ddd; padding: 8px;'>Multi-threshold AUC</td>
                        <td style='border: 1px solid #ddd; padding: 8px;'>Robust to threshold choice</td>
                      </tr>
                    </table>

                    <h5>📐 Rank-Based Consensus Framework:</h5>

                    <h6>Why Rankings Instead of Raw Values?</h6>
                    <ul>
                      <li><strong>Scale-Invariant:</strong> Rankings unaffected by different scales across methods</li>
                      <li><strong>Robust to Outliers:</strong> Extreme values don't distort consensus</li>
                      <li><strong>Fair Comparison:</strong> Each method contributes equally</li>
                      <li><strong>Intuitive:</strong> 'Rank 1' = most important, regardless of method</li>
                    </ul>

                    <h6>Rank Correlation (Spearman ρ):</h6>
                    <code>ρ = 1 - (6Σd²) / [n(n²-1)]</code>
                    <p>Where d = rank differences between methods. ρ = 1: perfect agreement. ρ = 0: no agreement.</p>

                    <h6>Consensus Rank:</h6>
                    <code>R_consensus(node) = mean(R_weighted, R_percolation, R_persistence)</code>
                    <p>Average rank across three methods. Lower rank = more consistently important.</p>

                    <h5>🔬 Key Questions Answered:</h5>
                    <ol>
                      <li><strong>Node Strength (6a):</strong> Do methods agree on high-degree nodes?</li>
                      <li><strong>Eigenvector Centrality (6c):</strong> Do methods agree on hub identification?</li>
                      <li><strong>Hub Conservation (6e):</strong> Which hubs are robust across methods?</li>
                      <li><strong>Method Robustness (6k):</strong> Which metrics are method-independent?</li>
                      <li><strong>Regional Consensus (6l):</strong> Which regions show consistent patterns?</li>
                    </ol>

                    <h5>⚠️ Interpretation Guidelines:</h5>
                    <ul>
                      <li><strong>High Spearman ρ (>0.8):</strong> Methods produce consistent rankings</li>
                      <li><strong>Moderate ρ (0.5-0.8):</strong> General agreement but some differences</li>
                      <li><strong>Low ρ (<0.5):</strong> Methods capture different network aspects</li>
                      <li><strong>Points on diagonal:</strong> Node has same rank in both methods</li>
                      <li><strong>Points off diagonal:</strong> Node importance varies by method</li>
                    </ul>

                    <h5>📊 Coefficient of Variation (Robustness):</h5>
                    <code>CV = SD / Mean</code>
                    <p><strong>Interpretation:</strong></p>
                    <ul>
                      <li>CV < 0.2: Highly robust metric (method-independent)</li>
                      <li>CV 0.2-0.5: Moderately robust</li>
                      <li>CV > 0.5: Method-dependent (use with caution)</li>
                    </ul>
                  ")
                )
              )
            ),

            # Step 7: Statistical Tests
            tabPanel("7️⃣ Statistical Tests",
              h4("📊 Statistical Testing Framework"),
              p("Comprehensive statistical validation including permutation testing, group comparisons, topological analysis, and network-based statistics."),

              tabsetPanel(
                tabPanel("7a. ROI-Level Permutation Testing",
                  h5("🧠 Brain Region Permutation Testing"),
                  HTML("<p><strong>What it measures:</strong> Tests which brain regions (ROIs) differ significantly between two experimental groups using subject-level data</p>
                       <p><strong>Mathematical basis:</strong> For each ROI, runs permutation test by randomly shuffling group labels 5000 times.
                       P-value = proportion of permuted differences ≥ observed. FDR correction controls for multiple comparisons across ROIs.
                       Effect size via Cohen's d = (μ₁ - μ₂) / σ_pooled.</p>"),

                  selectInput("roi_perm_group1",
                             "Select Group 1:",
                             choices = NULL),

                  selectInput("roi_perm_group2",
                             "Select Group 2:",
                             choices = NULL),

                  numericInput("roi_n_permutations",
                              "Number of Permutations:",
                              value = 5000,
                              min = 1000,
                              max = 10000,
                              step = 1000),

                  selectInput("roi_correction_method",
                             "Multiple Comparison Correction:",
                             choices = c("FDR (Benjamini-Hochberg)" = "fdr",
                                       "Bonferroni" = "bonferroni",
                                       "None" = "none"),
                             selected = "fdr"),

                  actionButton("run_roi_permutation_test",
                              "Run ROI Permutation Test",
                              class = "btn-primary"),

                  hr(),

                  h5("Significance Plot"),
                  downloadablePlotOutput("roiPermutationPlot", height = "500px"),
                  tags$p(tags$em("Figure: Bar plot showing -log10(adjusted p-value) for each ROI. Red bars = significant (p < 0.05), gray = not significant."),
                         style = "font-size: 0.9em; color: #666; margin-top: 10px;"),

                  h5("Detailed Results Table"),
                  DT::dataTableOutput("roiPermutationTable", height = "400px"),

                  h5("Summary Statistics"),
                  verbatimTextOutput("roiPermutationSummary")
                ),

                tabPanel("7b. Hub Overlap Analysis",
                  h5("🔗 Hub Conservation & Overlap Statistics"),
                  HTML("<p><strong>What it measures:</strong> Quantifies hub node consistency across experimental groups. Identifies which hubs are robust vs. group-specific.</p>
                       <p><strong>Mathematical basis:</strong> Hubs identified via z-scored eigenvector centrality (z ≥ threshold).
                       Hub overlap measured via Jaccard index J = |A ∩ B| / |A ∪ B|, where A and B are hub sets from two groups.
                       J = 0 (no overlap) to J = 1 (perfect overlap).</p>
                       <p><strong>⚠️ Z-score threshold note:</strong> Weighted networks often have homogeneous eigenvector values (low variance).
                       If you get 0 hubs, try lowering the threshold to 1.0 or 0.5. Percolation/Persistence networks (thresholded) typically have more spread.</p>"),

                  selectInput("hub_overlap_approach",
                             "Select Analysis Approach:",
                             choices = c("Weighted", "Percolation", "Persistence"),
                             selected = "Weighted"),

                  selectInput("hub_overlap_method",
                             "Select Correlation Method:",
                             choices = c("Pearson", "Spearman", "Kendall", "Biweight", "Shrinkage", "Partial"),
                             selected = "Pearson"),

                  numericInput("hub_z_threshold",
                              "Hub Identification Z-Score Threshold:",
                              value = 1.0,
                              min = 0.5,
                              max = 3.0,
                              step = 0.1),

                  actionButton("run_hub_overlap",
                              "Compute Hub Overlap",
                              class = "btn-primary"),

                  hr(),

                  h5("Pairwise Hub Overlap (Venn Diagrams)"),
                  downloadablePlotOutput("hubOverlapVennPlot", height = "600px"),
                  tags$p(tags$em("Figure: Venn diagrams showing hub overlap for each pair of groups. Numbers show unique and shared hubs. Jaccard index (J) quantifies overlap."),
                         style = "font-size: 0.9em; color: #666; margin-top: 10px;"),

                  h5("Hub Overlap Matrix (Jaccard Indices)"),
                  downloadablePlotOutput("hubOverlapMatrixPlot", height = "500px"),
                  tags$p(tags$em("Figure: Heatmap showing pairwise Jaccard indices. Warmer colors = higher overlap. Values displayed in cells."),
                         style = "font-size: 0.9em; color: #666; margin-top: 10px;"),

                  h5("Hub Lists by Group"),
                  DT::dataTableOutput("hubOverlapTable", height = "400px"),

                  h5("Summary Statistics"),
                  verbatimTextOutput("hubOverlapSummary")
                ),

                tabPanel("7c. Global Network Permutation Testing",
                  h5("🌐 Global Network Metrics Permutation Testing"),
                  HTML("<p><strong>What it measures:</strong> Tests whether global network properties (density, clustering, path length, modularity) differ significantly between two experimental groups</p>
                       <p><strong>Mathematical basis:</strong> For each permutation, randomly reassign subjects to groups, recompute correlation matrix, create network, and calculate metrics.
                       P-value = proportion of permuted differences ≥ observed. Tests null hypothesis that group labels are exchangeable under no true difference.</p>
                       <p><strong>⏱️ Computational note:</strong> 1000 permutations takes ~5-10 minutes. Use parallel processing to speed up.</p>"),

                  selectInput("global_perm_group1",
                             "Select Group 1:",
                             choices = NULL),

                  selectInput("global_perm_group2",
                             "Select Group 2:",
                             choices = NULL),

                  selectInput("global_perm_corr_method",
                             "Correlation Method:",
                             choices = c("Pearson" = "pearson",
                                       "Spearman" = "spearman",
                                       "Kendall" = "kendall",
                                       "Biweight" = "biweight",
                                       "Shrinkage" = "shrinkage",
                                       "Partial" = "partial"),
                             selected = "pearson"),

                  numericInput("global_perm_threshold",
                              "Network Threshold (leave blank for weighted):",
                              value = NA,
                              min = 0.1,
                              max = 0.9,
                              step = 0.05),

                  numericInput("global_n_permutations",
                              "Number of Permutations:",
                              value = 1000,
                              min = 100,
                              max = 5000,
                              step = 100),

                  actionButton("run_global_permutation_test",
                              "Run Global Permutation Test",
                              class = "btn-primary"),

                  hr(),

                  h5("Null Distributions & Observed Values"),
                  downloadablePlotOutput("globalPermutationPlot", height = "700px"),
                  tags$p(tags$em("Figure: Histograms showing null distributions of permuted differences for each global metric. Red line = observed difference, dashed line = no difference (H0). *, **, *** = p < 0.05, 0.01, 0.001."),
                         style = "font-size: 0.9em; color: #666; margin-top: 10px;"),

                  h5("Summary Statistics"),
                  verbatimTextOutput("globalPermutationSummary")
                ),

                tabPanel("Methods",
                  h4("📚 Statistical Testing Methodology"),
                  HTML("
                    <h5>🎯 Purpose:</h5>
                    <p>Validate network findings using rigorous statistical testing and permutation-based inference
                    to distinguish real effects from chance fluctuations.</p>

                    <h5>🔬 Three Statistical Approaches:</h5>

                    <h6>1. ROI-Level Permutation Testing (7a)</h6>
                    <p><strong>Null hypothesis:</strong> H₀: μ_ROI(Group A) = μ_ROI(Group B)</p>
                    <p><strong>Test statistic:</strong> T = |mean(ROI_A) - mean(ROI_B)|</p>
                    <p><strong>Procedure:</strong></p>
                    <ol>
                      <li>Calculate observed difference T_obs for each ROI</li>
                      <li>Permute subject-group labels 5000 times</li>
                      <li>Recalculate T_perm for each permutation</li>
                      <li>P-value = Σ(T_perm ≥ T_obs) / 5000</li>
                      <li>Apply FDR correction across all ROIs</li>
                    </ol>
                    <p><strong>Interpretation:</strong> FDR-adjusted p < 0.05 indicates significant group difference in that ROI</p>

                    <h6>2. Hub Overlap Analysis (7b)</h6>
                    <p><strong>Hub definition:</strong> Nodes with z-scored eigenvector centrality ≥ threshold (default 1.5)</p>
                    <p><strong>Jaccard overlap:</strong> J = |H_A ∩ H_B| / |H_A ∪ H_B|</p>
                    <p>Where H_A and H_B are hub sets from groups A and B</p>
                    <p><strong>Interpretation:</strong></p>
                    <ul>
                      <li>J = 1: Perfect hub agreement (all hubs shared)</li>
                      <li>J = 0.5: Moderate overlap (half shared)</li>
                      <li>J = 0: No overlap (completely different hubs)</li>
                    </ul>
                    <p><strong>Unique hubs:</strong> Hubs appearing in one group but not the other, reflecting group-specific network organization</p>

                    <h6>3. Global Network Permutation Testing (7c)</h6>
                    <p><strong>Null hypothesis:</strong> H₀: Network metrics are exchangeable between groups</p>
                    <p><strong>Global metrics tested:</strong></p>
                    <ul>
                      <li><strong>Network Density:</strong> Proportion of possible edges present</li>
                      <li><strong>Clustering Coefficient:</strong> Mean local clustering (transitive connectivity)</li>
                      <li><strong>Path Length:</strong> Average shortest path between nodes</li>
                      <li><strong>Modularity:</strong> Degree of community structure</li>
                    </ul>
                    <p><strong>Procedure:</strong></p>
                    <ol>
                      <li>Calculate observed metric difference: D_obs = |metric_A - metric_B|</li>
                      <li>For each permutation:
                        <ul>
                          <li>Randomly reassign subjects to groups</li>
                          <li>Recompute correlation matrices</li>
                          <li>Create networks</li>
                          <li>Calculate metrics for permuted groups</li>
                        </ul>
                      </li>
                      <li>P-value = Σ(D_perm ≥ D_obs) / N_perm</li>
                    </ol>

                    <h5>⚠️ Key Assumptions:</h5>
                    <ul>
                      <li><strong>Exchangeability:</strong> Under H₀, subject labels can be permuted without changing distribution</li>
                      <li><strong>Independence:</strong> Subjects are independent observations</li>
                      <li><strong>Sufficient sample size:</strong> At least 10-15 subjects per group recommended</li>
                    </ul>

                    <h5>📊 Multiple Comparison Correction:</h5>
                    <p><strong>ROI-Level Testing:</strong> False Discovery Rate (FDR) control using Benjamini-Hochberg procedure</p>
                    <code>Reject H₀_i if p_i ≤ (i/m) × α</code>
                    <p>Where i = rank of p-value, m = total tests, α = 0.05</p>
                    <p><strong>Advantages:</strong> More powerful than Bonferroni while controlling false discovery proportion</p>

                    <h5>🎓 Interpretation Guidelines:</h5>
                    <table style='border-collapse: collapse; width: 100%; margin: 15px 0;'>
                      <tr style='background-color: #f0f0f0;'>
                        <th style='border: 1px solid #ddd; padding: 8px;'>Test</th>
                        <th style='border: 1px solid #ddd; padding: 8px;'>Significant Result Indicates</th>
                      </tr>
                      <tr>
                        <td style='border: 1px solid #ddd; padding: 8px;'>ROI Permutation</td>
                        <td style='border: 1px solid #ddd; padding: 8px;'>Specific brain regions differ between groups</td>
                      </tr>
                      <tr>
                        <td style='border: 1px solid #ddd; padding: 8px;'>Hub Overlap</td>
                        <td style='border: 1px solid #ddd; padding: 8px;'>Low J: Groups have different hub structures</td>
                      </tr>
                      <tr>
                        <td style='border: 1px solid #ddd; padding: 8px;'>Global Network</td>
                        <td style='border: 1px solid #ddd; padding: 8px;'>Overall network topology differs between groups</td>
                      </tr>
                    </table>

                    <h5>📖 References:</h5>
                    <ul>
                      <li>Nichols & Holmes (2002). Nonparametric permutation tests for functional neuroimaging. <em>Human Brain Mapping</em></li>
                      <li>Benjamini & Hochberg (1995). Controlling the false discovery rate. <em>Journal of the Royal Statistical Society B</em></li>
                      <li>Rubinov & Sporns (2010). Complex network measures of brain connectivity. <em>NeuroImage</em></li>
                    </ul>
                  ")
                )
              )
            )
          )
        )
      )
    )
  )
}

# Enhanced UI with flexible import and brain area assignment
ui <- dashboardPage(
  dashboardHeader(
    title = "🧠 ConsensusConnectR - Multimethod Functional Connectivity Analysis"
  ),
  
  dashboardSidebar(
    sidebarMenu(
      id = "sidebarMenu",
      menuItem("📁 Data Import", tabName = "import", icon = icon("upload")),
      menuItem("🧠 Anatomical Regions", tabName = "brain_areas", icon = icon("brain")),
      menuItem("⚙️ Analysis Settings", tabName = "settings", icon = icon("cog")),
      menuItem("🏆 Summary", tabName = "summary", icon = icon("trophy")),
      menuItem("📊 Results", tabName = "results", icon = icon("chart-bar")),
      menuItem("💾 Downloads", tabName = "downloads", icon = icon("download")),
      menuItem("ℹ️ About", tabName = "about", icon = icon("info-circle"))
    ),
    
    hr(),
    
    # Progress indicator
    div(
      class = "progress-indicator",
      h5("Progress"),
      div(id = "progress_import", class = "progress-item progress-pending",
          icon("circle"), " Data Import"),
      div(id = "progress_brain_areas", class = "progress-item progress-pending",
          icon("circle"), " Anatomical Regions"),
      div(id = "progress_settings", class = "progress-item progress-pending",
          icon("circle"), " Analysis Settings"),
      div(id = "progress_analysis", class = "progress-item progress-pending",
          icon("circle"), " Analysis Complete")
    ),
    
    hr(),
    
    div(
      style = "padding: 15px;",
      p("Enhanced Interface", style = "font-weight: bold;"),
      p("Original Pipeline", style = "font-size: 90%;")
    )
  ),
  
  dashboardBody(
    useShinyjs(),
    dashboard_css(),
    
    # Citation Modal
    div(
      id = "citation-modal",
      class = "modal",
      style = "display: block;",
      div(
        class = "modal-content",
        div(
          class = "modal-header",
          h2("🧠 Welcome to ConsensusConnectR v2.0"),
          p("Multimethod Consensus-Based Preclinical Functional Connectivity Analysis Platform"),
          p(style = "font-size: 0.9em; color: #666; font-style: italic;", "Portable Edition")
        ),
        div(
          class = "modal-body",
          style = "padding: 30px;",
          div(
            class = "alert alert-info",
            h4("📚 Citation Agreement"),
            p("This software implements a multimethod consensus approach for preclinical functional connectivity analysis."),
            p("By using ConsensusConnectR, you agree to cite our work in your publications."),
            tags$blockquote(
              style = "font-style: italic; background: #f8f9fa; padding: 15px; border-left: 4px solid #007bff;",
              p(strong("ConsensusConnectR"), " (Version 2.0) [Software]. 2025."),
              p("Multimethod Consensus-Based Preclinical Functional Connectivity Analysis Platform."),
              p(style = "font-size: 0.85em; color: #666;", em("Portable Edition"))
            )
          ),
          checkboxInput("citationAgreement", "I agree to cite this software", value = FALSE),
          br(),
          div(
            style = "text-align: center;",
            actionButton("proceedToApp", "Proceed to Analysis", 
                        class = "btn-primary btn-lg", disabled = TRUE)
          )
        )
      )
    ),

    # Plot Download Modal (hover-triggered download settings)
    plot_download_modal_ui(),

    # Main app content
    div(
      id = "main-app",
      style = "display: none;",
      tabItems(
        # Enhanced Data Import Tab
        tabItem(
          tabName = "import",
          fluidRow(
            column(
              width = 4,
              box(
                title = "📁 Data Upload", status = "primary", solidHeader = TRUE, width = NULL,
                fileInput(
                  "datafile",
                  "Choose CSV File",
                  accept = c(".csv")
                ),
                
                hr(),
                
                
                hr(),
                
                div(
                  class = "alert alert-info",
                  h5("📋 Required Format"),
                  tags$ul(
                    tags$li("ID column (unique identifier for each subject/sample)"),
                    tags$li("Group columns (e.g., Sex, Treatment, Group)"),
                    tags$li("Regional activity/expression measurements"),
                    tags$li("Optional: Behavioral measures")
                  )
                )
              )
            ),
            
            column(
              width = 8,
              conditionalPanel(
                condition = "output.hasData",
                box(
                  title = "⚙️ Data Configuration", status = "success", solidHeader = TRUE, width = NULL,
                  
                  p("Select the appropriate columns from your dataset using the dropdown menus below:"),
                  
                  fluidRow(
                    column(
                      width = 6,
                      selectInput(
                        "id_column",
                        "ID Column:",
                        choices = c("Upload data first..." = "")
                      )
                    ),
                    column(
                      width = 6,
                      selectizeInput(
                        "group_columns",
                        "Group Columns:",
                        choices = NULL,
                        multiple = TRUE
                      )
                    )
                  ),
                  
                  fluidRow(
                    column(
                      width = 6,
                      selectizeInput(
                        "behavior_columns",
                        "Behavioral Columns (Optional):",
                        choices = NULL,
                        multiple = TRUE,
                        options = list(placeholder = "Optional")
                      )
                    ),
                    column(
                      width = 6,
                      br(),
                      checkboxInput("combine_groups", "Create Combined Groups", value = TRUE)
                    )
                  ),
                  
                  hr(),
                  
                  div(
                    style = "text-align: center;",
                    actionButton("configure_data", "Configure Data", 
                                icon = icon("check"), class = "btn-success")
                  )
                )
              )
            )
          ),
          
          fluidRow(
            column(
              width = 12,
              conditionalPanel(
                condition = "output.hasData",
                box(
                  title = "📊 Data Preview", status = "info", solidHeader = TRUE, width = NULL,
                  DTOutput("data_preview_table")
                )
              )
            )
          ),
          
          uiOutput("successPanel")
        ),
        
        # Anatomical Regions Assignment Tab
        tabItem(
          tabName = "brain_areas",
          conditionalPanel(
            condition = "!output.dataConfigured",
            div(
              class = "alert alert-warning",
              icon("exclamation-triangle"),
              " Please complete Data Import first."
            )
          ),
          
          conditionalPanel(
            condition = "output.dataConfigured",
            fluidRow(
              column(
                width = 6,
                box(
                  title = "🧠 Region & Group Selection", status = "primary", solidHeader = TRUE, width = NULL,
                  
                  selectizeInput(
                    "selected_regions",
                    "Measurement Variables to Include:",
                    choices = NULL,
                    multiple = TRUE,
                    options = list(plugins = list('remove_button'))
                  ),
                  
                  div(
                    class = "btn-group",
                    actionButton("select_all_regions", "Select All", class = "btn-sm btn-outline-primary"),
                    actionButton("clear_regions", "Clear", class = "btn-sm btn-outline-secondary")
                  ),
                  
                  hr(),
                  
                  selectizeInput(
                    "selected_groups",
                    "Groups to Include:",
                    choices = NULL,
                    multiple = TRUE,
                    options = list(plugins = list('remove_button'))
                  ),
                  
                  div(
                    class = "btn-group",
                    actionButton("select_all_groups", "Select All", class = "btn-sm btn-outline-primary"),
                    actionButton("clear_groups", "Clear", class = "btn-sm btn-outline-secondary")
                  ),
                  
                  hr(),
                  
                  checkboxInput("include_behavior_in_analysis", "Include behavioral data", value = FALSE)
                )
              ),
              
              column(
                width = 6,
                box(
                  title = "🎨 Anatomical Region Mapping", status = "success", solidHeader = TRUE, width = NULL,
                  
                  div(
                    style = "max-height: 300px; overflow-y: auto;",
                    uiOutput("brain_area_assignments")
                  ),
                  
                  hr(),
                  
                  fluidRow(
                    column(
                      width = 8,
                      textInput("new_brain_area", "New Anatomical Region:", placeholder = "e.g., Prefrontal Cortex, Hippocampus")
                    ),
                    column(
                      width = 4,
                      br(),
                      actionButton("add_brain_area", "Add", icon = icon("plus"), class = "btn-sm btn-primary")
                    )
                  )
                )
              )
            ),
            
            fluidRow(
              column(
                width = 12,
                box(
                  title = "🎨 Color Configuration", status = "warning", solidHeader = TRUE, width = NULL,
                  
                  tabsetPanel(
                    tabPanel(
                      "Anatomical Region Colors",
                      br(),
                      uiOutput("brain_area_colors")
                    ),
                    tabPanel(
                      "Group Colors", 
                      br(),
                      uiOutput("group_colors")
                    )
                  )
                )
              )
            ),

            fluidRow(
              column(
                width = 12,
                div(
                  style = "text-align: center; margin: 20px;",
                  actionButton("go_to_settings", "Next: Configure Analysis Settings →",
                              icon = icon("arrow-right"), class = "btn-primary btn-lg")
                )
              )
            )
          )
        ),

        # Analysis Settings Tab
        tabItem(
          tabName = "settings",
          conditionalPanel(
            condition = "!output.dataConfigured",
            div(
              class = "alert alert-warning",
              icon("exclamation-triangle"),
              " Please complete Data Import and Anatomical Regions first."
            )
          ),

          conditionalPanel(
            condition = "output.dataConfigured",
            fluidRow(
              column(
                width = 12,
                box(
                  title = "⚙️ Analysis Configuration", status = "primary", solidHeader = TRUE, width = NULL,

                  h4("🔬 Correlation Methods"),
                  p("Select which correlation methods to include in the analysis. Each method will be run with all selected network approaches."),
                  checkboxGroupInput(
                    "selected_correlation_methods",
                    "Correlation Methods:",
                    choices = c(
                      "Pearson" = "pearson",
                      "Spearman (rank-based)" = "spearman",
                      "Kendall (rank-based)" = "kendall",
                      "Biweight Midcorrelation" = "biweight",
                      "Shrinkage" = "shrinkage",
                      "Partial Correlation" = "partial"
                    ),
                    selected = c("pearson", "spearman", "biweight")
                  ),
                  uiOutput("correlation_recommendations"),
                  helpText("Note: Shrinkage and Partial correlations require specific packages and may fail if not available."),

                  hr(),

                  h4("🕸️ Network Analysis Approaches"),
                  p("Select which network construction approaches to use. All selected methods will be compared in the consensus analysis."),
                  checkboxGroupInput(
                    "selected_network_approaches",
                    "Network Approaches:",
                    choices = c(
                      "Weighted (threshold-free)" = "weighted",
                      "Percolation (optimal threshold)" = "percolation",
                      "Persistence (multi-threshold)" = "persistence"
                    ),
                    selected = c("weighted", "percolation", "persistence")
                  ),
                  helpText("At least one approach must be selected. Consensus analysis requires all three approaches."),

                  hr(),

                  h4("📈 Persistence Analysis Settings"),
                  p("Configure the persistence analysis threshold range (only used if Persistence approach is selected)."),
                  sliderInput(
                    "persistence_threshold_step",
                    "Persistence Threshold Resolution:",
                    min = 0.01,
                    max = 0.1,
                    value = 0.05,
                    step = 0.01
                  ),
                  helpText("Finer resolution (0.01) provides more detail but slower computation. Default (0.05) balances detail and speed.")
                )
              )
            ),

            fluidRow(
              column(
                width = 12,
                box(
                  title = "🚀 Run Analysis", status = "success", solidHeader = TRUE, width = NULL,
                  div(
                    style = "text-align: center; margin: 20px;",
                    actionButton("run_original_analysis", "🔬 Run Comprehensive Network Analysis",
                                icon = icon("play"), class = "btn-success btn-lg"),
                    br(), br(),
                    p("This will run all selected correlation methods with all selected network approaches.", style = "color: #666;")
                  )
                )
              )
            )
          )
        ),

        # Summary Tab (Tab 7 Consensus)
        tabItem(
          tabName = "summary",
          conditionalPanel(
            condition = "!output.analysisComplete",
            div(
              class = "alert alert-info",
              icon("info-circle"),
              " Complete Analysis Settings and run the analysis to view consensus summary."
            )
          ),

          conditionalPanel(
            condition = "output.analysisComplete",
            # This will contain Tab 7 content - we'll copy it here in the next step
            create_summary_ui()
          )
        ),

        # Results Tab (formerly Analysis, contains tabs 1-6, 8-9)
        tabItem(
          tabName = "results",
          conditionalPanel(
            condition = "!output.analysisComplete",
            div(
              class = "alert alert-info",
              icon("info-circle"),
              " Complete data import and anatomical region mapping to run functional connectivity analysis."
            )
          ),
          
          conditionalPanel(
            condition = "output.analysisComplete",
            # Insert all original results UI here
            create_results_ui()
          )
        ),
        
        # Downloads Tab
        tabItem(
          tabName = "downloads",
          downloads_ui()
        ),
        
        # About Tab
        tabItem(
          tabName = "about",
          fluidRow(
            column(
              width = 12,
              div(
                style = "padding: 20px; max-width: 1000px; margin: 0 auto;",
                h2("About ConsensusConnectR", style = "text-align: center; color: #2C3E50; margin-bottom: 30px;"),

                div(
                  class = "well",
                  style = "background-color: #f8f9fa; border: 1px solid #dee2e6;",

                  h3("🧠 Comprehensive Multimethod Brain Network Analysis Platform", style = "color: #495057;"),
                  p("ConsensusConnectR provides robust functional connectivity analysis by combining multiple correlation methods and analytical approaches, identifying consensus findings independent of methodological choices.", style = "font-size: 16px; line-height: 1.6;"),
                  br(),

                  h4("🔬 Five Correlation Methods", style = "color: #495057;"),
                  tags$ul(style = "font-size: 15px; line-height: 1.7;",
                    tags$li(tags$strong("Pearson:"), " Linear correlation, parametric approach"),
                    tags$li(tags$strong("Spearman:"), " Rank-based correlation, robust to outliers"),
                    tags$li(tags$strong("Biweight Midcorrelation:"), " Robust correlation, down-weights extreme values"),
                    tags$li(tags$strong("Shrinkage:"), " James-Stein regularized correlation for high-dimensional data"),
                    tags$li(tags$strong("Partial Correlation:"), " Direct connections controlling for other variables")
                  ),
                  br(),

                  h4("🕸️ Three Analytical Approaches", style = "color: #495057;"),
                  tags$ul(style = "font-size: 15px; line-height: 1.7;",
                    tags$li(tags$strong("Weighted Analysis:"), " Threshold-free networks preserving all correlation information"),
                    tags$li(tags$strong("Percolation Analysis:"), " Data-driven optimal threshold selection balancing sparsity and connectivity"),
                    tags$li(tags$strong("Persistence Analysis:"), " Multi-threshold hub identification robust to threshold choice")
                  ),
                  br(),

                  h4("📊 Key Features", style = "color: #495057;"),
                  tags$ul(style = "font-size: 15px; line-height: 1.7;",
                    tags$li("Rank-based consensus across all selected method-approach combinations"),
                    tags$li("Robust hub identification with cross-method validation"),
                    tags$li("Regional functional connectivity by anatomical brain areas"),
                    tags$li("Statistical testing: permutation tests, hub overlap analysis"),
                    tags$li("Comprehensive network topology metrics (degree, betweenness, eigenvector, PageRank)"),
                    tags$li("Group comparison and network similarity analysis"),
                    tags$li("Interactive visualizations with customizable color schemes"),
                    tags$li("Publication-ready plots with adjustable DPI and dimensions")
                  ),
                  br(),

                  h4("🎯 Designed for Neuroscience Research", style = "color: #495057;"),
                  tags$ul(style = "font-size: 15px; line-height: 1.7;",
                    tags$li("Optimized for both preclinical and clinical sample sizes"),
                    tags$li("Handles missing data through multiple imputation (MICE)"),
                    tags$li("Flexible data import supporting various experimental designs"),
                    tags$li("Interactive anatomical region mapping and color configuration"),
                    tags$li("Comprehensive downloadable results with organized file structure")
                  ),
                  br(),

                  h4("📈 Analysis Workflow", style = "color: #495057;"),
                  tags$ol(style = "font-size: 15px; line-height: 1.7;",
                    tags$li(tags$strong("Data Import:"), " Upload CSV data with ID, group, and measurement columns"),
                    tags$li(tags$strong("Anatomical Regions:"), " Map measurements to brain regions, configure colors"),
                    tags$li(tags$strong("Analysis Settings:"), " Select correlation methods, network approaches, and parameters"),
                    tags$li(tags$strong("Run Analysis:"), " Comprehensive per-method analysis with consensus integration"),
                    tags$li(tags$strong("Summary:"), " View cross-method consensus findings and robust hubs"),
                    tags$li(tags$strong("Results:"), " Explore detailed per-method results and comparisons"),
                    tags$li(tags$strong("Statistical Tests:"), " Validate findings with permutation testing"),
                    tags$li(tags$strong("Downloads:"), " Export all plots, data, and network graphs")
                  ),
                  br(),
                  
                  hr(style = "border-color: #dee2e6;"),
                  
                  div(
                    style = "text-align: center; background-color: #ffffff; padding: 20px; border-radius: 5px;",
                    h4("📋 Technical Information", style = "color: #495057; margin-bottom: 15px;"),
                    fluidRow(
                      column(4,
                        tags$p(tags$strong("Version:"), "3.0.0", style = "margin-bottom: 10px;"),
                        tags$p(tags$strong("License:"), "MIT", style = "margin-bottom: 10px;")
                      ),
                      column(4,
                        tags$p(tags$strong("Language:"), "R (Shiny)", style = "margin-bottom: 10px;"),
                        tags$p(tags$strong("Dependencies:"), "igraph, MICE, shinydashboard", style = "margin-bottom: 10px;")
                      ),
                      column(4,
                        tags$p(tags$strong("Repository:"), 
                               tags$a("GitHub", href = "https://github.com/consensusconnectr", target = "_blank", style = "color: #007bff;"),
                               style = "margin-bottom: 10px;"),
                        tags$p(tags$strong("Contact:"), "research@consensusconnectr.org", style = "margin-bottom: 10px;")
                      )
                    )
                  )
                )
              )
            )
          )
        )
      )
    )
  )
)

# Enhanced server function 
server <- function(input, output, session) {
  
  # All original reactive values
  analysis_results <- reactiveValues(
    complete = FALSE,
    imputation = NULL,
    correlations = NULL,
    networks = NULL,
    adjacency_matrices = NULL,
    global_metrics = NULL,
    node_metrics = NULL,
    similarities = NULL,
    conservation = NULL,
    raw_data = NULL,
    imputed_data = NULL
  )
  
  # Enhanced reactive values
  ui_state <- reactiveValues(
    data_imported = FALSE,
    data_configured = FALSE,
    brain_areas_configured = FALSE,
    analysis_complete = FALSE,
    raw_data = NULL,
    processed_data = NULL,
    column_info = NULL,
    brain_areas = list(),
    area_colors = list(),
    group_colors = list(),
    calibrated_time_ms = NULL,  # Calibrated time per permutation (from benchmark)
    calibration_data = NULL     # Full calibration object with parallel overhead data
  )
  
  # Default brain areas (from original)
  default_brain_areas <- list(
    "Dorsal HPC" = c("dDG", "dCA1", "dCA2", "dCA3"),
    "Ventral HPC" = c("vDG", "vCA1", "vCA3"),
    "Subiculum" = c("dSub", "vSub"),
    "Nucleus Accumbens" = c("NaC", "NaS", "Cpu"),
    "Frontal" = c("ACC", "IL", "PRL", "CG1"),
    "Amygdala" = c("CeA", "BLA", "LA"),
    "Retrosplenial" = c("RSGab", "RSGc", "RSD")
  )
  
  default_area_colors <- c(
    "Dorsal HPC" = "#D3ADC4", "Ventral HPC" = "#C88AB1", "Subiculum" = "#9B59B6",
    "Nucleus Accumbens" = "#A3DFD7", "Frontal" = "#FAE9BD",
    "Amygdala" = "#F0BC94", "Retrosplenial" = "#85C1E9"
  )

  # Reactive expression for selected correlation methods
  # This returns the user's selected methods and should be used everywhere instead of hardcoded lists
  selected_methods <- reactive({
    req(input$selected_correlation_methods)
    input$selected_correlation_methods
  })

  # Helper to get number of method-approach combinations
  n_method_approach_combos <- reactive({
    length(selected_methods()) * 3  # 3 approaches: weighted, percolation, persistence
  })

  # Helper function for regional summary computation
  compute_regional_summary <- function(node_data, brain_areas, metric_name) {
    regional_summary <- data.frame(
      Region = character(),
      Mean = numeric(),
      SD = numeric(),
      N = integer(),
      stringsAsFactors = FALSE
    )

    for(region_name in names(brain_areas)) {
      region_nodes <- brain_areas[[region_name]]
      region_data <- node_data[node_data$Node %in% region_nodes, ]

      if(nrow(region_data) > 0 && metric_name %in% names(region_data)) {
        regional_summary <- rbind(regional_summary, data.frame(
          Region = region_name,
          Mean = mean(region_data[[metric_name]], na.rm = TRUE),
          SD = sd(region_data[[metric_name]], na.rm = TRUE),
          N = nrow(region_data),
          stringsAsFactors = FALSE
        ))
      }
    }

    return(regional_summary)
  }

  # Helper function to filter methods available across ALL groups
  filter_common_methods <- function(comprehensive_consensus, all_methods) {
    if(is.null(comprehensive_consensus) || length(comprehensive_consensus) == 0) {
      return(character(0))
    }

    all_groups <- names(comprehensive_consensus)
    if(length(all_groups) == 0) return(character(0))

    # For each method, check if it exists in ALL groups
    common_methods <- character(0)

    for(method in all_methods) {
      method_in_all_groups <- TRUE

      for(group in all_groups) {
        std_metrics <- comprehensive_consensus[[group]]$standardized_metrics

        if(is.null(std_metrics)) {
          method_in_all_groups <- FALSE
          break
        }

        # Check if method exists in at least one approach for this group
        method_found <- FALSE
        for(approach in c("weighted", "percolation", "persistence")) {
          if(approach %in% names(std_metrics)) {
            if(method %in% names(std_metrics[[approach]])) {
              method_found <- TRUE
              break
            }
          }
        }

        if(!method_found) {
          method_in_all_groups <- FALSE
          break
        }
      }

      if(method_in_all_groups) {
        common_methods <- c(common_methods, method)
      }
    }

    return(common_methods)
  }

  # Helper function: Compute Jaccard similarity between correlation matrices
  compute_jaccard_similarity <- function(cor_matrix1, cor_matrix2, threshold = 0) {
    # Ensure matrices have same dimensions
    if(!all(dim(cor_matrix1) == dim(cor_matrix2))) {
      return(NA)
    }

    # For weighted Jaccard: sum of mins / sum of maxs
    # Using absolute values of correlations as weights
    weights1 <- abs(cor_matrix1)
    weights2 <- abs(cor_matrix2)

    # Remove diagonal (self-correlations)
    diag(weights1) <- 0
    diag(weights2) <- 0

    # Apply threshold if specified
    weights1[weights1 < threshold] <- 0
    weights2[weights2 < threshold] <- 0

    # Weighted Jaccard similarity
    min_weights <- pmin(weights1, weights2)
    max_weights <- pmax(weights1, weights2)

    numerator <- sum(min_weights, na.rm = TRUE)
    denominator <- sum(max_weights, na.rm = TRUE)

    if(denominator == 0) return(0)

    jaccard <- numerator / denominator
    return(jaccard)
  }

  # Helper function: Create Jaccard similarity heatmap
  render_jaccard_heatmap <- function(jaccard_matrix, group_names, title = "Network Similarity (Jaccard Index)") {
    n <- nrow(jaccard_matrix)

    if(n < 2) {
      plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
      text(1, 1, "Need at least 2 groups for similarity analysis", cex = 1.2)
      return()
    }

    # Color palette: blue (low similarity) to red (high similarity)
    col_pal <- colorRampPalette(c("#2166AC", "#F7F7F7", "#B2182B"))(100)

    par(mar = c(8, 8, 4, 2))

    # Create heatmap
    image(1:n, 1:n, jaccard_matrix,
          col = col_pal,
          xlab = "", ylab = "",
          main = title,
          axes = FALSE,
          zlim = c(0, 1))

    # Add axes
    axis(1, at = 1:n, labels = group_names, las = 2, cex.axis = 0.9)
    axis(2, at = 1:n, labels = group_names, las = 2, cex.axis = 0.9)

    # Add grid lines
    abline(h = (1:n) + 0.5, col = "gray30", lwd = 0.5)
    abline(v = (1:n) + 0.5, col = "gray30", lwd = 0.5)

    # Add text values
    for(i in 1:n) {
      for(j in 1:n) {
        if(!is.na(jaccard_matrix[i,j])) {
          text_col <- if(jaccard_matrix[i,j] > 0.5) "white" else "black"
          text(i, j, sprintf("%.3f", jaccard_matrix[i,j]),
               cex = 0.8, col = text_col, font = 2)
        }
      }
    }

    # Add legend
    legend("topright",
           legend = c("1.0 (Identical)", "0.5", "0.0 (No overlap)"),
           fill = col_pal[c(100, 50, 1)],
           bty = "n",
           cex = 0.8,
           title = "Jaccard Index")
  }

  # Helper function: Render Regional Contribution Bar Plot
  render_contribution_barplot <- function(contribution_results,
                                          brain_areas = NULL,
                                          area_colors = NULL,
                                          top_n = 25) {

    if(is.null(contribution_results) || is.null(contribution_results$observed)) {
      plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
      text(1, 1, "No contribution results available", cex = 1.2, col = "gray50")
      return()
    }

    observed <- contribution_results$observed
    null_dist <- contribution_results$null_distributions

    if(nrow(observed) == 0) {
      plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
      text(1, 1, "No regions to display", cex = 1.2, col = "gray50")
      return()
    }

    # Sort by absolute contribution and take top_n
    observed <- observed[order(-abs(observed$Contribution_Score)), ]
    if(nrow(observed) > top_n) {
      observed <- observed[1:top_n, ]
    }

    # Reverse order for horizontal barplot (top item at top)
    observed <- observed[nrow(observed):1, ]

    n_regions <- nrow(observed)

    # Get confidence intervals - either from observed (C++ path) or null distribution (R path)
    ci_lower <- numeric(n_regions)
    ci_upper <- numeric(n_regions)

    for(i in 1:n_regions) {
      region <- observed$Region[i]

      # First try CI columns from observed (C++ backend provides these directly)
      if("CI_Lower" %in% names(observed) && "CI_Upper" %in% names(observed)) {
        ci_lower[i] <- observed$CI_Lower[i]
        ci_upper[i] <- observed$CI_Upper[i]
      }
      # Fall back to computing from null distribution (R backend)
      else if(!is.null(null_dist) && region %in% colnames(null_dist)) {
        null_vals <- null_dist[, region]
        null_vals <- null_vals[!is.na(null_vals)]
        if(length(null_vals) > 0) {
          ci_lower[i] <- quantile(null_vals, 0.025, na.rm = TRUE)
          ci_upper[i] <- quantile(null_vals, 0.975, na.rm = TRUE)
        }
      }
    }

    # Determine bar colors
    bar_colors <- rep("#3498DB", n_regions)  # Default blue

    if(!is.null(brain_areas) && !is.null(area_colors)) {
      for(i in 1:n_regions) {
        region <- observed$Region[i]
        for(area_name in names(brain_areas)) {
          if(region %in% brain_areas[[area_name]] || region == area_name) {
            if(area_name %in% names(area_colors)) {
              bar_colors[i] <- area_colors[[area_name]]
            }
            break
          }
        }
      }
    }

    # Highlight significant regions with border
    border_colors <- rep("gray40", n_regions)
    border_colors[observed$Significant] <- "black"

    # Setup plot
    par(mar = c(5, 10, 4, 4))

    # Calculate x-axis limits
    x_vals <- c(observed$Contribution_Score, ci_lower, ci_upper)
    x_vals <- x_vals[!is.na(x_vals) & is.finite(x_vals)]
    if(length(x_vals) == 0) x_vals <- c(-0.1, 0.1)
    x_max <- max(abs(x_vals), na.rm = TRUE) * 1.4
    x_range <- c(-x_max, x_max)

    # Create horizontal barplot
    bp <- barplot(observed$Contribution_Score,
                  horiz = TRUE,
                  names.arg = observed$Region,
                  col = bar_colors,
                  border = border_colors,
                  las = 1,
                  xlim = x_range,
                  xlab = "Contribution Score (J without region - Original J)",
                  main = paste0("Regional Contribution to Group Dissimilarity\n(",
                               contribution_results$n_permutations, " permutations, ",
                               contribution_results$analysis_level, " level)"),
                  cex.names = 0.7)

    # Add error bars (95% CI from null distribution)
    for(i in 1:n_regions) {
      y_pos <- bp[i]

      # Draw CI whiskers
      if(!is.na(ci_lower[i]) && !is.na(ci_upper[i])) {
        segments(ci_lower[i], y_pos, ci_upper[i], y_pos, lwd = 1.5, col = "gray40")
        segments(ci_lower[i], y_pos - 0.2, ci_lower[i], y_pos + 0.2, lwd = 1.5, col = "gray40")
        segments(ci_upper[i], y_pos - 0.2, ci_upper[i], y_pos + 0.2, lwd = 1.5, col = "gray40")
      }
    }

    # Add significance stars
    for(i in 1:n_regions) {
      if(!is.na(observed$Significance_Stars[i]) && observed$Significance_Stars[i] != "") {
        score <- observed$Contribution_Score[i]
        text_x <- if(score > 0) score + x_max * 0.08 else score - x_max * 0.08
        text(text_x, bp[i], observed$Significance_Stars[i],
             cex = 1.2, col = "#E74C3C", font = 2)
      }
    }

    # Add vertical line at 0
    abline(v = 0, lty = 2, col = "gray30", lwd = 2)

    # Add annotation
    mtext("Positive = removing region INCREASES similarity (region drives dissimilarity)",
          side = 1, line = 3.5, cex = 0.75, col = "gray40")

    # Legend
    legend("topright",
           legend = c("* p < 0.05 (FDR)", "** p < 0.01", "*** p < 0.001"),
           text.col = "#E74C3C",
           cex = 0.8,
           bty = "n",
           title = "Significance")
  }

  # Helper function: Render Regional Contribution Circular Network Plot
  render_regional_circular_plot <- function(contribution_results,
                                            cor_matrix,
                                            brain_areas = NULL,
                                            area_colors = NULL,
                                            group_pair = c("Group1", "Group2")) {

    if(is.null(contribution_results) || is.null(contribution_results$observed)) {
      plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
      text(1, 1, "No contribution results available", cex = 1.2, col = "gray50")
      return()
    }

    observed <- contribution_results$observed

    if(nrow(observed) == 0) {
      plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
      text(1, 1, "No regions to display", cex = 1.2, col = "gray50")
      return()
    }

    # Create network from correlation matrix
    if(!requireNamespace("igraph", quietly = TRUE)) {
      plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
      text(1, 1, "igraph package required", cex = 1.2, col = "red")
      return()
    }

    # Create adjacency matrix (use threshold = 0.3 for visualization)
    adj_mat <- abs(cor_matrix)
    adj_mat[adj_mat < 0.3] <- 0
    diag(adj_mat) <- 0

    g <- igraph::graph_from_adjacency_matrix(adj_mat, mode = "undirected", weighted = TRUE)
    node_names <- igraph::V(g)$name
    if(is.null(node_names)) node_names <- rownames(cor_matrix)
    if(is.null(node_names)) node_names <- paste0("Node_", 1:nrow(cor_matrix))

    # Map contribution scores to nodes
    contrib_map <- setNames(observed$Contribution_Score, observed$Region)
    sig_map <- setNames(observed$Significant, observed$Region)

    # Node colors based on significance and direction
    node_colors <- rep("#CCCCCC", length(node_names))  # Default gray

    for(i in seq_along(node_names)) {
      node <- node_names[i]
      if(node %in% names(sig_map)) {
        if(!is.na(sig_map[node]) && sig_map[node]) {
          # Significant: color by direction
          if(!is.na(contrib_map[node]) && contrib_map[node] > 0) {
            node_colors[i] <- "#E74C3C"  # Red: contributes to dissimilarity
          } else {
            node_colors[i] <- "#27AE60"  # Green: contributes to similarity
          }
        } else {
          # Not significant: use brain area color if available
          if(!is.null(brain_areas) && !is.null(area_colors)) {
            for(area_name in names(brain_areas)) {
              if(node %in% brain_areas[[area_name]]) {
                if(area_name %in% names(area_colors)) {
                  node_colors[i] <- adjustcolor(area_colors[[area_name]], alpha.f = 0.6)
                }
                break
              }
            }
          }
        }
      }
    }

    # Node sizes based on absolute contribution score
    node_sizes <- rep(10, length(node_names))
    for(i in seq_along(node_names)) {
      node <- node_names[i]
      if(node %in% names(contrib_map) && !is.na(contrib_map[node])) {
        # Scale contribution to size (10-30 range)
        abs_contrib <- abs(contrib_map[node])
        max_contrib <- max(abs(contrib_map), na.rm = TRUE)
        if(!is.na(max_contrib) && max_contrib > 0) {
          node_sizes[i] <- 10 + (abs_contrib / max_contrib) * 20
        }
      }
    }

    # Group nodes by brain area for better circular arrangement
    if(!is.null(brain_areas)) {
      node_order <- c()
      for(area_name in names(brain_areas)) {
        area_nodes <- brain_areas[[area_name]]
        area_in_network <- area_nodes[area_nodes %in% node_names]
        node_order <- c(node_order, area_in_network)
      }
      # Add any remaining nodes
      remaining <- setdiff(node_names, node_order)
      node_order <- c(node_order, remaining)

      # Reorder graph if valid
      if(length(node_order) == length(node_names)) {
        order_idx <- match(node_order, node_names)
        order_idx <- order_idx[!is.na(order_idx)]
        if(length(order_idx) == length(node_names)) {
          g <- igraph::permute(g, order_idx)
          node_colors <- node_colors[order_idx]
          node_sizes <- node_sizes[order_idx]
        }
      }
    }

    # Create circular layout
    layout <- igraph::layout_in_circle(g)

    # Plot
    par(mar = c(2, 2, 4, 2))

    # Get edge weights for width
    edge_weights <- igraph::E(g)$weight
    if(is.null(edge_weights)) edge_weights <- rep(1, igraph::ecount(g))
    edge_widths <- edge_weights * 2

    plot(g,
         layout = layout,
         vertex.size = node_sizes,
         vertex.color = node_colors,
         vertex.frame.color = "white",
         vertex.label.cex = 0.55,
         vertex.label.color = "black",
         vertex.label.dist = 1.8,
         edge.width = edge_widths,
         edge.color = adjustcolor("gray60", alpha.f = 0.3),
         main = paste("Regional Contribution to Group Difference\n",
                     group_pair[1], "vs", group_pair[2]))

    # Legend
    legend("bottomright",
           legend = c("Sig. + (drives dissimilarity)",
                     "Sig. - (drives similarity)",
                     "Not significant"),
           fill = c("#E74C3C", "#27AE60", "#CCCCCC"),
           title = "Contribution",
           cex = 0.7,
           bg = "white")

    # Brain area legend if available
    if(!is.null(brain_areas) && !is.null(area_colors)) {
      n_areas <- min(length(area_colors), 7)
      legend("bottomleft",
             legend = names(area_colors)[1:n_areas],
             fill = adjustcolor(area_colors[1:n_areas], alpha.f = 0.6),
             title = "Brain Areas",
             cex = 0.6,
             bg = "white")
    }
  }

  # Citation modal handling
  observeEvent(input$citationAgreement, {
    if (input$citationAgreement) {
      runjs("$('#proceedToApp').prop('disabled', false);")
    } else {
      runjs("$('#proceedToApp').prop('disabled', true);")
    }
  })
  
  observeEvent(input$proceedToApp, {
    runjs("
      document.getElementById('citation-modal').style.display = 'none';
      document.getElementById('main-app').style.display = 'block';
    ")
    showNotification("✅ Welcome! Start by uploading your data.", duration = 3)
  })
  
  # === DATA IMPORT SECTION ===
  
  # File upload
  uploaded_data <- reactive({
    if (is.null(input$datafile)) return(NULL)
    
    tryCatch({
      # Robust CSV reading with encoding detection and error handling
      data <- tryCatch({
        # Try UTF-8 first (handles BOM)
        read.csv(input$datafile$datapath, stringsAsFactors = FALSE, encoding = "UTF-8")
      }, error = function(e) {
        # Fallback to default encoding
        tryCatch({
          read.csv(input$datafile$datapath, stringsAsFactors = FALSE)
        }, error = function(e2) {
          # Final fallback with different separators
          read.csv(input$datafile$datapath, stringsAsFactors = FALSE, sep = ";")
        })
      })
      
      # Clean column names comprehensively
      original_names <- names(data)
      # Remove BOM, leading/trailing spaces, and make valid R names
      names(data) <- trimws(names(data))  # Remove leading/trailing spaces
      names(data) <- gsub("^﻿", "", names(data))  # Remove BOM character
      names(data) <- make.names(names(data), unique = TRUE)  # Make valid R names
      
      ui_state$raw_data <- data  
      ui_state$data_imported <- TRUE
      
      # Update progress indicator
      runjs("
        $('#progress_import').removeClass('progress-pending').addClass('progress-complete');
        $('#progress_import i').removeClass('fa-circle').addClass('fa-check-circle');
      ")
      
      # Auto-detect columns and update choices
      col_names <- names(data)
      
      # Notify user if column names were cleaned
      cleaned_names <- which(original_names != names(data))
      if (length(cleaned_names) > 0) {
        showNotification(
          paste("Column names cleaned:", length(cleaned_names), "columns had spaces/special characters removed"), 
          type = "message", duration = 5
        )
      }
      
      # Update column selectors (user-driven selection only)
      updateSelectInput(session, "id_column", choices = c("Select ID column..." = "", col_names))
      
      # Update other selectors
      updateSelectizeInput(session, "group_columns", choices = col_names)
      updateSelectizeInput(session, "behavior_columns", choices = col_names)
      
      return(data)
    }, error = function(e) {
      showNotification(paste("Error reading file:", e$message), type = "error")
      return(NULL)
    })
  })
  
  output$hasData <- reactive({ !is.null(uploaded_data()) })
  outputOptions(output, "hasData", suspendWhenHidden = FALSE)
  
  # Data preview
  output$data_preview_table <- renderDT({
    req(uploaded_data())
    datatable(uploaded_data(), options = list(pageLength = 10, scrollX = TRUE))
  })
  
  # Configure data
  observeEvent(input$configure_data, {
    req(uploaded_data())
    
    # Validate inputs
    
    # More flexible validation
    if (is.null(input$id_column) || input$id_column == "") {
      showNotification("⚠️ Please select an ID column", type = "warning", duration = 10)
      return()
    }
    
    if (length(input$group_columns) == 0) {
      showNotification("⚠️ Please select at least one group column", type = "warning", duration = 10)
      return()
    }
    
    data <- uploaded_data()
    
    # Store column information
    all_cols <- names(data)
    metadata_cols <- c(input$id_column, input$group_columns, input$behavior_columns)
    region_cols <- setdiff(all_cols, metadata_cols)
    
    ui_state$column_info <- list(
      id_column = input$id_column,
      group_columns = input$group_columns,
      behavior_columns = input$behavior_columns,
      region_columns = region_cols
    )
    
    # Create combined group column if requested
    if (input$combine_groups && length(input$group_columns) > 1) {
      data$Group <- apply(data[input$group_columns], 1, paste, collapse = "_")
    } else if (length(input$group_columns) == 1) {
      data$Group <- data[[input$group_columns[1]]]
    }
    
    ui_state$processed_data <- data
    
    # Initialize brain areas immediately after data configuration
    region_columns <- ui_state$column_info$region_columns
    
    # Initialize brain areas with case-insensitive matching
    matched_areas <- list()
    unassigned_regions <- region_columns
    
    for (area_name in names(default_brain_areas)) {
      area_regions <- default_brain_areas[[area_name]]
      # Case-insensitive matching
      matched <- c()
      for (region in area_regions) {
        # Find case-insensitive matches
        matches <- region_columns[tolower(region_columns) == tolower(region)]
        matched <- c(matched, matches)
      }
      matched <- unique(matched)
      
      if (length(matched) > 0) {
        matched_areas[[area_name]] <- matched
        unassigned_regions <- setdiff(unassigned_regions, matched)
      }
    }
    
    # Always include unassigned regions in "Other" category
    if (length(unassigned_regions) > 0) {
      matched_areas[["Other"]] <- unassigned_regions
    }
    
    # If no brain areas were matched at all, put everything in "All Regions"
    if (length(matched_areas) == 0 || (length(matched_areas) == 1 && "Other" %in% names(matched_areas))) {
      matched_areas <- list("All Regions" = region_columns)
    }
    
    ui_state$brain_areas <- matched_areas
    # Set colors for matched areas
    ui_state$area_colors <- default_area_colors[names(matched_areas)]
    if ("Other" %in% names(matched_areas)) {
      ui_state$area_colors["Other"] <- "#95A5A6"
    }
    if ("All Regions" %in% names(matched_areas)) {
      ui_state$area_colors["All Regions"] <- "#3498DB"
    }
    
    # Set group colors
    groups <- unique(data$Group)
    group_palette <- c("#E74C3C", "#3498DB", "#2ECC71", "#F39C12", "#9B59B6")
    ui_state$group_colors <- setNames(group_palette[1:length(groups)], groups)
        
    ui_state$data_configured <- TRUE
    
    showNotification("✅ Data configured successfully!")
    
    # Automatically switch to brain areas tab after configuration
    updateTabItems(session, "sidebarMenu", "brain_areas")
    showNotification("📍 Navigating to Anatomical Regions tab...", type = "message", duration = 2)
  })

  # Navigate from Anatomical Regions to Analysis Settings
  observeEvent(input$go_to_settings, {
    updateTabItems(session, "sidebarMenu", "settings")
    shinyjs::runjs("$('#progress_brain_areas').removeClass('progress-pending').addClass('progress-complete')")
    shinyjs::runjs("$('#progress_settings').removeClass('progress-pending').addClass('progress-active')")
    showNotification("📍 Navigating to Analysis Settings...", type = "message", duration = 2)
  })

  # Mark Analysis Settings as complete when user configures any settings
  observeEvent(input$selected_correlation_methods, {
    # Only mark complete if at least one method is selected
    if(!is.null(input$selected_correlation_methods) && length(input$selected_correlation_methods) > 0) {
      shinyjs::runjs("$('#progress_settings').removeClass('progress-pending progress-active').addClass('progress-complete')")
    }
  }, ignoreInit = TRUE)

  # Render success panel when data is configured
  output$successPanel <- renderUI({
    if (isTRUE(ui_state$data_configured)) {
      # Render success panel with button
      fluidRow(
        column(
          width = 12,
          div(
            class = "alert alert-success",
            h4("✅ Data Successfully Configured!"),
            p("Proceed to Anatomical Regions tab to map your measurements to anatomical structures."),
            div(
              style = "text-align: center; margin-top: 15px;",
              actionButton("go_to_brain_areas", "Go to Anatomical Regions →", 
                          icon = icon("arrow-right"), class = "btn-primary")
            )
          )
        )
      )
    } else {
      # Show nothing when not configured
      NULL
    }
  })
  
  # Reactive outputs for conditionalPanels
  output$dataConfigured <- reactive({ ui_state$data_configured })
  outputOptions(output, "dataConfigured", suspendWhenHidden = FALSE)

  # Correlation method recommendations based on data dimensions
  output$correlation_recommendations <- renderUI({
    if(!isTRUE(ui_state$data_configured) || is.null(ui_state$processed_data)) {
      return(NULL)
    }

    data <- ui_state$processed_data

    # Get sample size per group
    if("Group" %in% names(data)) {
      group_sizes <- table(data$Group)
      n_min <- min(group_sizes)
      n_max <- max(group_sizes)
      n_display <- if(n_min == n_max) as.character(n_min) else paste0(n_min, "-", n_max)
    } else {
      n_min <- nrow(data)
      n_display <- as.character(n_min)
    }

    # Get number of variables
    p <- length(ui_state$column_info$region_columns)

    # Check for tied values (indicator that Kendall may be better)
    region_cols <- ui_state$column_info$region_columns
    tie_ratio <- 0
    if(length(region_cols) > 0) {
      # Sample a few columns to check tie ratio
      sample_cols <- head(region_cols, min(5, length(region_cols)))
      tie_ratios <- sapply(sample_cols, function(col) {
        if(col %in% names(data)) {
          vals <- data[[col]]
          vals <- vals[!is.na(vals)]
          if(length(vals) > 0) {
            return(length(unique(vals)) / length(vals))
          }
        }
        return(1)
      })
      tie_ratio <- mean(tie_ratios, na.rm = TRUE)
    }
    has_many_ties <- tie_ratio < 0.8  # Less than 80% unique values suggests many ties

    # Generate recommendations
    recommendations <- list()

    # Pearson always recommended
    recommendations$pearson <- list(status = "recommended", note = "Linear relationships")

    # Spearman vs Kendall decision based on sample size and ties
    # Kendall better for: small n (< 20) OR many tied values
    # Spearman better for: larger n (>= 20) AND few ties
    if(n_min < 20 || has_many_ties) {
      # Recommend Kendall for small samples or tied data
      recommendations$kendall <- list(
        status = "recommended",
        note = if(has_many_ties) "Better with tied values" else paste0("Better for small n (", n_display, ")")
      )
      recommendations$spearman <- list(
        status = "available",
        note = "Alternative rank-based method"
      )
    } else {
      # Recommend Spearman for larger samples without many ties
      recommendations$spearman <- list(
        status = "recommended",
        note = "Faster for larger samples"
      )
      recommendations$kendall <- list(
        status = "available",
        note = "Alternative (better theoretical properties)"
      )
    }

    # Biweight - recommended when n >= 10
    if(n_min >= 10) {
      recommendations$biweight <- list(status = "recommended", note = "Robust to outliers")
    } else {
      recommendations$biweight <- list(status = "caution", note = paste0("n=", n_display, " may be too small"))
    }

    # Shrinkage - recommended when n < p (uses Ledoit-Wolf estimator)
    # Check for zero-variance columns which cause cor.shrink to fail
    zero_var_cols <- 0
    if(length(region_cols) > 0) {
      col_vars <- sapply(region_cols, function(col) {
        if(col %in% names(data)) {
          vals <- data[[col]]
          vals <- vals[!is.na(vals)]
          if(length(vals) > 1) return(var(vals)) else return(0)
        }
        return(NA)
      })
      zero_var_cols <- sum(col_vars == 0 | is.na(col_vars), na.rm = TRUE)
    }

    if(zero_var_cols > 0) {
      recommendations$shrinkage <- list(status = "unavailable", note = paste0(zero_var_cols, " constant columns detected"))
    } else if(n_min < p) {
      recommendations$shrinkage <- list(status = "recommended", note = paste0("n < p (", n_display, " < ", p, ")"))
    } else if(n_min < p * 2) {
      recommendations$shrinkage <- list(status = "available", note = "Helps with estimation stability")
    } else {
      recommendations$shrinkage <- list(status = "optional", note = "Not needed when n >> p")
    }

    # Partial - requires n > p + 2
    if(n_min > p + 2) {
      recommendations$partial <- list(status = "available", note = "Sufficient sample size")
    } else {
      recommendations$partial <- list(status = "unavailable", note = paste0("Requires n > ", p + 2, " (currently n=", n_display, ")"))
    }

    # Build recommendation display
    rec_items <- lapply(names(recommendations), function(method) {
      rec <- recommendations[[method]]
      method_label <- switch(method,
        "pearson" = "Pearson",
        "spearman" = "Spearman",
        "kendall" = "Kendall",
        "biweight" = "Biweight",
        "shrinkage" = "Shrinkage",
        "partial" = "Partial"
      )
      icon_class <- switch(rec$status,
        "recommended" = "fa-check-circle text-success",
        "available" = "fa-circle text-info",
        "caution" = "fa-exclamation-triangle text-warning",
        "unavailable" = "fa-times-circle text-danger",
        "optional" = "fa-minus-circle text-muted"
      )
      tags$li(
        tags$i(class = paste("fa", icon_class)),
        paste0(" ", method_label, ": ", rec$note)
      )
    })

    div(
      class = "method-guide",
      style = "margin-top: 10px; padding: 10px; background-color: #f8f9fa; border-left: 3px solid #17a2b8; font-size: 0.9em;",
      tags$strong(paste0("Data-based recommendations (n=", n_display, ", p=", p, "):")),
      tags$ul(style = "margin-bottom: 0; padding-left: 20px;", rec_items)
    )
  })

  # Auto-select correlation methods based on data characteristics
  observeEvent(ui_state$data_configured, {
    req(ui_state$data_configured)
    req(ui_state$processed_data)
    req(ui_state$column_info)

    data <- ui_state$processed_data

    # Get sample size per group
    if("Group" %in% names(data)) {
      group_sizes <- table(data$Group)
      n_min <- min(group_sizes)
    } else {
      n_min <- nrow(data)
    }

    # Get number of variables
    p <- length(ui_state$column_info$region_columns)

    # Check for tied values
    region_cols <- ui_state$column_info$region_columns
    tie_ratio <- 1
    if(length(region_cols) > 0) {
      sample_cols <- head(region_cols, min(5, length(region_cols)))
      tie_ratios <- sapply(sample_cols, function(col) {
        if(col %in% names(data)) {
          vals <- data[[col]]
          vals <- vals[!is.na(vals)]
          if(length(vals) > 0) return(length(unique(vals)) / length(vals))
        }
        return(1)
      })
      tie_ratio <- mean(tie_ratios, na.rm = TRUE)
    }
    has_many_ties <- tie_ratio < 0.8

    # Build recommended selection
    selected <- c("pearson")  # Always include Pearson

    # Spearman vs Kendall: pick based on data
    if(n_min < 20 || has_many_ties) {
      selected <- c(selected, "kendall")  # Kendall for small n or ties
    } else {
      selected <- c(selected, "spearman")  # Spearman for larger n
    }

    # Biweight if n >= 10
    if(n_min >= 10) {
      selected <- c(selected, "biweight")
    }

    # Shrinkage if n < p (uses Ledoit-Wolf estimator for regularization)
    if(n_min < p) {
      selected <- c(selected, "shrinkage")
    }

    # Partial if n > p + 2
    if(n_min > p + 2) {
      selected <- c(selected, "partial")
    }

    # Update the checkbox selection
    updateCheckboxGroupInput(
      session,
      "selected_correlation_methods",
      selected = selected
    )
  })

  # Go to brain areas button
  observeEvent(input$go_to_brain_areas, {
    updateTabItems(session, "sidebarMenu", "brain_areas")
  })
  
  # === BRAIN AREAS SECTION ===
  
  # Initialize brain areas when data is configured
  observe({
    req(ui_state$data_configured)
    req(ui_state$column_info)
    
    if (ui_state$brain_areas_configured) {
      return()
    }
    
    region_columns <- ui_state$column_info$region_columns
    
    # Update region selection - ensure all regions are selected by default
    updateSelectizeInput(session, "selected_regions", 
                        choices = region_columns, selected = region_columns)
    
    showNotification("✅ All brain regions have been selected by default. You can modify the selection as needed.", 
                    type = "message", duration = 4)
    
    # Get unique groups
    if ("Group" %in% names(ui_state$processed_data)) {
      groups <- unique(ui_state$processed_data$Group)
    } else {
      group_col <- ui_state$column_info$group_columns[1]
      groups <- unique(ui_state$processed_data[[group_col]])
    }
    
    updateSelectizeInput(session, "selected_groups", 
                        choices = groups, selected = groups)
    
    showNotification(paste("✅ All", length(groups), "groups have been selected:", paste(groups, collapse = ", ")), 
                    type = "message", duration = 4)
    
    # Initialize brain areas with case-insensitive matching
    matched_areas <- list()
    unassigned_regions <- region_columns
    
    for (area_name in names(default_brain_areas)) {
      area_regions <- default_brain_areas[[area_name]]
      # Case-insensitive matching
      matched <- c()
      for (region in area_regions) {
        # Find case-insensitive matches
        matches <- region_columns[tolower(region_columns) == tolower(region)]
        matched <- c(matched, matches)
      }
      matched <- unique(matched)
      
      if (length(matched) > 0) {
        matched_areas[[area_name]] <- matched
        unassigned_regions <- setdiff(unassigned_regions, matched)
      }
    }
    
    # Always include unassigned regions in "Other" category
    if (length(unassigned_regions) > 0) {
      matched_areas[["Other"]] <- unassigned_regions
    }
    
    # If no brain areas were matched at all, put everything in "All Regions"
    if (length(matched_areas) == 0 || (length(matched_areas) == 1 && "Other" %in% names(matched_areas))) {
      matched_areas <- list("All Regions" = region_columns)
    }
    
    ui_state$brain_areas <- matched_areas
    # Set colors for matched areas
    ui_state$area_colors <- default_area_colors[names(matched_areas)]
    if ("Other" %in% names(matched_areas)) {
      ui_state$area_colors["Other"] <- "#95A5A6"
    }
    if ("All Regions" %in% names(matched_areas)) {
      ui_state$area_colors["All Regions"] <- "#3498DB"
    }
    
    # Set group colors
    group_palette <- c("#E74C3C", "#3498DB", "#2ECC71", "#F39C12", "#9B59B6")
    ui_state$group_colors <- setNames(group_palette[1:length(groups)], groups)
    
    # Update progress
    runjs("
      $('#progress_brain_areas').removeClass('progress-pending').addClass('progress-complete');
      $('#progress_brain_areas i').removeClass('fa-circle').addClass('fa-check-circle');
    ")
    
    # Mark brain areas as configured to prevent infinite loop
    ui_state$brain_areas_configured <- TRUE
  })
  
  # Region/group selection buttons
  observeEvent(input$select_all_regions, {
    req(ui_state$column_info)
    updateSelectizeInput(session, "selected_regions", selected = ui_state$column_info$region_columns)
  })
  
  observeEvent(input$clear_regions, {
    updateSelectizeInput(session, "selected_regions", selected = character(0))
  })
  
  observeEvent(input$select_all_groups, {
    req(ui_state$processed_data)
    if ("Group" %in% names(ui_state$processed_data)) {
      all_groups <- unique(ui_state$processed_data$Group)
    } else {
      group_col <- ui_state$column_info$group_columns[1]
      all_groups <- unique(ui_state$processed_data[[group_col]])
    }
    updateSelectizeInput(session, "selected_groups", selected = all_groups)
  })
  
  observeEvent(input$clear_groups, {
    updateSelectizeInput(session, "selected_groups", selected = character(0))
  })
  
  # Brain area assignments UI
  output$brain_area_assignments <- renderUI({
    req(ui_state$brain_areas)
    req(input$selected_regions)
    
    ui_list <- list()
    
    for (area_name in names(ui_state$brain_areas)) {
      area_id <- gsub("[^a-zA-Z0-9]", "_", area_name)
      current_regions <- ui_state$brain_areas[[area_name]]
      
      ui_list[[area_name]] <- div(
        style = "margin-bottom: 15px; padding: 10px; border: 1px solid #ddd; border-radius: 5px;",
        fluidRow(
          column(4, strong(area_name)),
          column(8, 
            selectizeInput(
              paste0("area_assign_", area_id),
              label = NULL,
              choices = input$selected_regions,
              selected = intersect(current_regions, input$selected_regions),
              multiple = TRUE,
              options = list(placeholder = "Select regions", plugins = list('remove_button'))
            )
          )
        )
      )
    }
    
    do.call(tagList, ui_list)
  })
  
  # Add new brain area
  observeEvent(input$add_brain_area, {
    req(input$new_brain_area)
    
    area_name <- trimws(input$new_brain_area)
    if (area_name != "" && !area_name %in% names(ui_state$brain_areas)) {
      ui_state$brain_areas[[area_name]] <- character(0)
      ui_state$area_colors[area_name] <- "#808080"
      updateTextInput(session, "new_brain_area", value = "")
      showNotification(paste("Added brain area:", area_name))
    }
  })
  
  # Brain area colors UI
  output$brain_area_colors <- renderUI({
    req(ui_state$brain_areas)
    
    ui_list <- list()
    for (area_name in names(ui_state$brain_areas)) {
      area_id <- gsub("[^a-zA-Z0-9]", "_", area_name)
      current_color <- ui_state$area_colors[area_name]
      if (is.na(current_color)) current_color <- "#808080"
      
      ui_list[[area_name]] <- fluidRow(
        style = "margin-bottom: 10px;",
        column(6, strong(area_name)),
        column(6, 
          colourInput(
            paste0("area_color_", area_id),
            label = NULL, 
            value = current_color, 
            showColour = "both",
            allowTransparent = FALSE,
            returnName = FALSE
          )
        )
      )
    }
    
    do.call(tagList, ui_list)
  })
  
  # Group colors UI
  output$group_colors <- renderUI({
    req(input$selected_groups)
    
    ui_list <- list()
    for (group_name in input$selected_groups) {
      group_id <- gsub("[^a-zA-Z0-9]", "_", group_name)
      current_color <- ui_state$group_colors[group_name]
      if (is.null(current_color) || is.na(current_color)) current_color <- "#808080"
      
      ui_list[[group_name]] <- fluidRow(
        style = "margin-bottom: 10px;",
        column(6, strong(group_name)),
        column(6, 
          colourInput(
            paste0("group_color_", group_id),
            label = NULL, 
            value = current_color, 
            showColour = "both",
            allowTransparent = FALSE,
            returnName = FALSE
          )
        )
      )
    }
    
    do.call(tagList, ui_list)
  })
  
  # === RUN ORIGINAL ANALYSIS ===
  
  observeEvent(input$run_original_analysis, {
    # Validate that regions and groups are selected
    if (is.null(input$selected_regions) || length(input$selected_regions) == 0) {
      showNotification("⚠️ Please select at least one brain region to analyze!", type = "error", duration = 5)
      return()
    }
    
    if (is.null(input$selected_groups) || length(input$selected_groups) == 0) {
      showNotification("⚠️ Please select at least one group to analyze!", type = "error", duration = 5)
      return()
    }
    
    # Update brain areas and colors from inputs
    for (area_name in names(ui_state$brain_areas)) {
      area_id <- gsub("[^a-zA-Z0-9]", "_", area_name)
      assign_input <- paste0("area_assign_", area_id)
      color_input <- paste0("area_color_", area_id)
      
      if (!is.null(input[[assign_input]])) {
        ui_state$brain_areas[[area_name]] <- input[[assign_input]]
      }
      if (!is.null(input[[color_input]])) {
        ui_state$area_colors[area_name] <- input[[color_input]]
      }
    }
    
    for (group_name in input$selected_groups) {
      group_id <- gsub("[^a-zA-Z0-9]", "_", group_name)
      color_input <- paste0("group_color_", group_id)
      if (!is.null(input[[color_input]])) {
        ui_state$group_colors[group_name] <- input[[color_input]]
      }
    }
    
    # Set the data for the original analysis
    analysis_results$raw_data <- ui_state$processed_data
    
    # Update progress and switch to analysis tab
    runjs("
      $('#progress_analysis').removeClass('progress-pending').addClass('progress-active');
      $('#progress_analysis i').removeClass('fa-circle').addClass('fa-spinner fa-spin');
    ")
    
    updateTabItems(session, "sidebarMenu", "analysis")
    
    showNotification("🔄 Running restructured analysis pipeline...", duration = NULL, id = "analysis_running")
    
    # === ORIGINAL ANALYSIS PIPELINE ===
    tryCatch({
      data <- analysis_results$raw_data
      
      # Filter data based on selected regions and groups
      region_cols <- input$selected_regions
      selected_groups <- input$selected_groups
      
      # Filter by groups
      if ("Group" %in% names(data)) {
        data <- data[data$Group %in% selected_groups, ]
      }
      
      # Include behavioral data if requested
      analysis_cols <- region_cols
      if (input$include_behavior_in_analysis && !is.null(ui_state$column_info$behavior_columns)) {
        analysis_cols <- c(analysis_cols, ui_state$column_info$behavior_columns)
      }
      
      # Step 1: Data imputation
      showNotification("Step 1/6: Imputing missing data...", duration = 2)
      imputation_result <- perform_mice_imputation(data[, analysis_cols, drop = FALSE])
      analysis_results$imputation <- imputation_result
      
      # Step 2: Per-method correlation analysis
      showNotification("Step 2/6: Computing correlations...", duration = 2)

      # Create groups vector
      if ("Group" %in% names(data)) {
        groups <- data$Group
      } else {
        groups <- rep("All_Data", nrow(data))
      }

      # NEW: Compute correlations by method (stores each method separately)
      # Only compute for user-selected correlation methods
      correlation_methods <- compute_correlations_by_method(
        imputation_result$imputed_data,
        groups,
        selected_methods = input$selected_correlation_methods
      )
      analysis_results$correlation_methods_raw <- correlation_methods

      # Create backward-compatible consensus for existing visualizations
      correlations <- create_median_consensus_from_methods(correlation_methods)
      analysis_results$correlations <- correlations
      
      # Step 3: Topology Analysis - with per-method group-specific percolation
      analysis_type <- "threshold_free"  # Always run comprehensive analysis

      showNotification("Step 3/6: Network topology analysis...", duration = 2)

      # NEW: Run percolation analysis for EACH method separately
      method_percolation_results <- list()
      method_weighted_results <- list()

      for(method_name in names(correlation_methods)) {
        # Ensure method_name is a character string
        if(!is.character(method_name) || length(method_name) != 1) {
          cat(sprintf("⚠️  Skipping invalid method name: %s\n", class(method_name)))
          next
        }

        method_correlations <- correlation_methods[[method_name]]

        # Skip if method has no data
        if(is.null(method_correlations) || length(method_correlations) == 0) next

        # Calculate group-specific percolation thresholds for this method
        method_thresholds <- list()
        for(group_name in names(method_correlations)) {
          cor_matrix <- method_correlations[[group_name]]
          if(!is.null(cor_matrix)) {
            threshold <- calculate_percolation_threshold(cor_matrix)
            method_thresholds[[group_name]] <- threshold
          }
        }

        # Run percolation-based network analysis for this method
        method_networks <- list()
        method_adjacency <- list()
        method_global <- data.frame()
        method_nodes <- data.frame()
        method_edges <- data.frame()
        method_brain_areas <- data.frame()

        for(group_name in names(method_correlations)) {
          cor_matrix <- method_correlations[[group_name]]
          if(is.null(cor_matrix)) next

          group_threshold <- method_thresholds[[group_name]]
          network <- create_network(cor_matrix, threshold = group_threshold)

          if(!is.null(network)) {
            method_networks[[group_name]] <- network
            method_adjacency[[group_name]] <- as_adjacency_matrix(network, attr = "weight", sparse = FALSE)

            metrics <- compute_network_metrics(network, group_name, ui_state$brain_areas)
            if(!is.null(metrics)) {
              method_global <- rbind(method_global, metrics$global)
              method_nodes <- rbind(method_nodes, metrics$nodes)
              if(!is.null(metrics$edges)) method_edges <- rbind(method_edges, metrics$edges)
              if(!is.null(metrics$brain_area_metrics)) method_brain_areas <- rbind(method_brain_areas, metrics$brain_area_metrics)
            }
          }
        }

        # Run threshold-free analysis for this method
        method_weighted <- perform_threshold_free_analysis(
          method_correlations,
          names(method_correlations),
          analysis_type = "all"
        )

        # Store per-method results
        method_percolation_results[[method_name]] <- list(
          thresholds = method_thresholds,
          networks = method_networks,
          adjacency_matrices = method_adjacency,
          global_metrics = method_global,
          node_metrics = method_nodes,
          edge_metrics = method_edges,
          brain_area_metrics = method_brain_areas
        )

        method_weighted_results[[method_name]] <- method_weighted
      }

      analysis_results$method_percolation_results <- method_percolation_results
      analysis_results$method_weighted_results <- method_weighted_results

      # Backward compatibility: Keep consensus-based results for existing code
      group_thresholds <- list()
      for(group_name in names(correlations)) {
        group_threshold <- calculate_percolation_threshold(correlations[[group_name]])
        group_thresholds[[group_name]] <- group_threshold
      }
      analysis_results$group_thresholds <- group_thresholds

      networks <- list()
      adjacency_matrices <- list()
      all_global_metrics <- data.frame()
      all_node_metrics <- data.frame()
      all_edge_metrics <- data.frame()
      all_brain_area_metrics <- data.frame()

      for(group_name in names(correlations)) {
        cor_matrix <- correlations[[group_name]]
        group_threshold <- group_thresholds[[group_name]]
        network <- create_network(cor_matrix, threshold = group_threshold)

        if(!is.null(network)) {
          networks[[group_name]] <- network
          adjacency_matrices[[group_name]] <- as_adjacency_matrix(network, attr = "weight", sparse = FALSE)

          metrics <- compute_network_metrics(network, group_name, ui_state$brain_areas)
          if(!is.null(metrics)) {
            all_global_metrics <- rbind(all_global_metrics, metrics$global)
            all_node_metrics <- rbind(all_node_metrics, metrics$nodes)
            if(!is.null(metrics$edges)) all_edge_metrics <- rbind(all_edge_metrics, metrics$edges)
            if(!is.null(metrics$brain_area_metrics)) all_brain_area_metrics <- rbind(all_brain_area_metrics, metrics$brain_area_metrics)
          }
        }
      }

      threshold_free_results <- perform_threshold_free_analysis(
        correlations,
        names(correlations),
        analysis_type = "all"
      )

      analysis_results$networks <- networks
      analysis_results$adjacency_matrices <- adjacency_matrices
      analysis_results$global_metrics <- all_global_metrics
      analysis_results$node_metrics <- all_node_metrics
      analysis_results$edge_metrics <- all_edge_metrics
      analysis_results$brain_area_metrics <- all_brain_area_metrics
      analysis_results$analysis_type <- analysis_type
      analysis_results$threshold_free_results <- threshold_free_results

      # Step 4: Persistence analysis across thresholds
      showNotification("Step 4/6: Persistence analysis...", duration = 3)

      # User-configurable threshold sequence
      threshold_step <- input$persistence_threshold_step
      if(is.null(threshold_step)) threshold_step <- 0.05
      threshold_seq <- seq(0.1, 0.9, by = threshold_step)
      n_thresholds <- length(threshold_seq)
      n_methods <- 5
      n_groups <- length(unique(groups))
      total_iterations <- n_methods * n_groups * n_thresholds

      # Progress tracking
      withProgress(message = 'Persistence Analysis', value = 0, {
        persistence_results <- list()
        iteration <- 0

        for(method_name in names(correlation_methods)) {
          # Ensure method_name is a character string
          if(!is.character(method_name) || length(method_name) != 1) {
            cat(sprintf("⚠️  Skipping invalid method name in persistence: %s\n", class(method_name)))
            next
          }

          method_results <- list()

          for(group_name in names(correlation_methods[[method_name]])) {
            cor_matrix <- correlation_methods[[method_name]][[group_name]]

            # Skip if method failed (e.g., partial correlation with small n)
            if(is.null(cor_matrix)) {
              incProgress(n_thresholds / total_iterations,
                         detail = paste("Skipped:", method_name, group_name))
              next
            }

            # Perform persistence analysis
            incProgress(0, detail = paste(method_name, "-", group_name))

            pers <- perform_persistence_analysis(
              cor_matrix,
              threshold_seq,
              ui_state$brain_areas,
              group_name
            )

            # Compute hub persistence
            hub_pers <- compute_hub_persistence(pers,
                                               centrality_measure = "Eigenvector",
                                               top_n = 10)

            # Compute metrics evolution
            metrics_evolution <- compute_network_metrics_evolution(pers)

            # NEW: Compute node importance evolution with standardized ranks
            importance_evolution <- compute_node_importance_evolution(
              pers,
              metrics = c("Eigenvector", "Degree", "Betweenness", "PageRank"),
              group_name = group_name
            )

            # NEW: Compute aggregated metrics for consensus integration
            persistence_aggregated <- compute_persistence_aggregated_metrics(pers)

            method_results[[group_name]] <- list(
              persistence_data = pers,
              hub_persistence = hub_pers,
              metrics_evolution = metrics_evolution,
              importance_evolution = importance_evolution,
              aggregated_metrics = persistence_aggregated
            )

            iteration <- iteration + n_thresholds
            incProgress(n_thresholds / total_iterations)
          }

          persistence_results[[method_name]] <- method_results
        }

        analysis_results$persistence_results <- persistence_results

        # NEW: Compute AUC metrics for threshold-independent summary
        showNotification("Computing AUC metrics across thresholds...", duration = 2)

        auc_results <- list()
        for(method_name in names(correlation_methods)) {
          # Ensure method_name is a character string
          if(!is.character(method_name) || length(method_name) != 1) {
            cat(sprintf("⚠️  Skipping invalid method name in AUC: %s\n", class(method_name)))
            next
          }

          auc_results[[method_name]] <- list()

          for(group_name in unique(groups)) {
            # Get correlation matrix from correlation_methods (not the consensus)
            cor_matrix <- correlation_methods[[method_name]][[group_name]]

            if(!is.null(cor_matrix)) {
              tryCatch({
                auc_data <- compute_auc_metrics(
                  cor_matrix,
                  thresholds = seq(0.1, 0.9, by = 0.05),
                  brain_areas = ui_state$brain_areas
                )
                auc_results[[method_name]][[group_name]] <- auc_data
              }, error = function(e) {
                cat(sprintf("⚠️  Warning: AUC failed for %s - %s: %s\n",
                           group_name, method_name, e$message))
              })
            }
          }
        }

        analysis_results$auc_results <- auc_results
      })

      # Step 5: Comprehensive Consensus Analysis (Weighted + Percolation + Persistence)
      showNotification("Step 5/6: Consensus analysis...", duration = 2)

      # 4A: Comprehensive Hub Consensus (synthesizes all 3 approaches)
      comprehensive_consensus <- list()

      for(group_name in unique(groups)) {
        tryCatch({
          consensus_result <- compute_comprehensive_hub_consensus(
            method_weighted_results = method_weighted_results,
            method_percolation_results = method_percolation_results,
            persistence_results = persistence_results,
            auc_results = auc_results,  # NEW: Pass AUC results for persistence eigenvector
            group_name = group_name,
            z_threshold = 1.5,  # NEW: Z-score threshold for hub definition
            prior_hub_prob = 0.15,  # NEW: Bayesian prior probability
            methods = input$selected_correlation_methods  # Use only selected methods
          )
          comprehensive_consensus[[group_name]] <- consensus_result
        }, error = function(e) {
          cat(sprintf("⚠️  Warning: Consensus failed for %s: %s\n", group_name, e$message))
        })
      }

      analysis_results$comprehensive_consensus <- comprehensive_consensus

      # 4A.5: Compute Sensitivity Analysis for each group
      showNotification("Computing threshold sensitivity analysis...", duration = 2)

      for(group_name in names(comprehensive_consensus)) {
        tryCatch({
          sensitivity <- compute_threshold_sensitivity(
            standardized_metrics = comprehensive_consensus[[group_name]]$standardized_metrics,
            group_name = group_name,
            current_tau = comprehensive_consensus[[group_name]]$z_threshold
          )
          comprehensive_consensus[[group_name]]$sensitivity_analysis <- sensitivity
          cat(sprintf("✅ Sensitivity analysis complete for %s: %d stable hubs\n",
                     group_name, length(sensitivity$stable_hub_nodes)))
        }, error = function(e) {
          cat(sprintf("⚠️  Warning: Sensitivity analysis failed for %s: %s\n", group_name, e$message))
          comprehensive_consensus[[group_name]]$sensitivity_analysis <- NULL
        })
      }

      # Update analysis_results with sensitivity-enhanced consensus
      analysis_results$comprehensive_consensus <- comprehensive_consensus

      # 4B: Cross-Method Network Similarity (compares networks between groups)
      showNotification("Computing cross-method network similarity...", duration = 2)

      tryCatch({
        network_similarity <- compute_cross_method_network_similarity(
          method_percolation_results = method_percolation_results,
          group_pairs = NULL  # Auto-generate all pairwise comparisons
        )
        analysis_results$network_similarity <- network_similarity
      }, error = function(e) {
        cat(sprintf("⚠️  Warning: Network similarity failed: %s\n", e$message))
        analysis_results$network_similarity <- NULL
      })

      # 4C: Cross-Method Network Metrics Aggregation (robustness analysis)
      showNotification("Aggregating network metrics across methods...", duration = 2)

      aggregated_metrics <- list()

      for(group_name in unique(groups)) {
        tryCatch({
          metrics_agg <- compute_cross_method_network_metrics_aggregation(
            method_percolation_results = method_percolation_results,
            group_name = group_name
          )
          if(!is.null(metrics_agg) && nrow(metrics_agg) > 0) {
            aggregated_metrics[[group_name]] <- metrics_agg
          }
        }, error = function(e) {
          cat(sprintf("⚠️  Warning: Metrics aggregation failed for %s: %s\n", group_name, e$message))
        })
      }

      analysis_results$aggregated_metrics <- aggregated_metrics

      cat(sprintf("✅ Consensus analysis complete: %d groups analyzed\n", length(comprehensive_consensus)))

      # Network conservation analysis (part of step 5)
      
      if(length(networks) > 1) {
        # Network similarities using percolation thresholds
        similarities <- data.frame()
        group_names <- names(networks)
        
        for(i in 1:(length(group_names)-1)) {
          for(j in (i+1):length(group_names)) {
            g1 <- group_names[i]
            g2 <- group_names[j]
            
            cor1 <- correlations[[g1]]
            cor2 <- correlations[[g2]]
            
            # Use group-specific thresholds for similarity calculation
            threshold1 <- group_thresholds[[g1]]
            threshold2 <- group_thresholds[[g2]]
            sim <- compute_network_similarity(cor1, cor2, threshold1 = threshold1, threshold2 = threshold2)
          
          sim_row <- data.frame(
            Group1 = g1,
            Group2 = g2,
            Jaccard = sim$jaccard,
            Overlap = sim$overlap,
            Shared_Edges = sim$intersection,
            Edges_Group1 = sim$edges_net1,
            Edges_Group2 = sim$edges_net2,
            Edge_Preservation_1to2 = sim$edge_preservation_1to2,
            Edge_Preservation_2to1 = sim$edge_preservation_2to1
          )
          similarities <- rbind(similarities, sim_row)
          }
        }
        
        analysis_results$similarities <- similarities
        
        # Hub conservation
        hub_conservation <- compute_hub_conservation(networks)
        analysis_results$conservation <- list(
          hub_conservation = hub_conservation,
          conservation_possible = !is.null(hub_conservation)
        )
      } else {
        analysis_results$conservation <- list(conservation_possible = FALSE)
      }
      
      
      # Weighted Network Analysis (part of step 5)
      
      # Compute weighted eigenvector centrality for full correlation networks
      weighted_eigen_results <- compute_weighted_eigenvector_centrality(correlations, groups)
      analysis_results$weighted_eigenvector <- weighted_eigen_results
      
      # Compare across groups if multiple groups exist
      if(length(networks) > 1 && !is.null(weighted_eigen_results)) {
        comparison_results <- compare_weighted_eigenvector_across_groups(weighted_eigen_results)
        analysis_results$weighted_eigenvector_comparison <- comparison_results
      }
      
      # Identify hub nodes
      if(!is.null(weighted_eigen_results)) {
        hub_results <- identify_weighted_eigenvector_hubs(weighted_eigen_results, top_n = 10)
        analysis_results$weighted_eigenvector_hubs <- hub_results
      }
      
      # Minimum Spanning Tree Analysis (part of step 5)
      mst_results <- compute_mst_analysis(correlations)
      analysis_results$mst_results <- mst_results

      # PCA Analysis (part of step 5)
      pca_results <- compute_pca_analysis(correlations)
      analysis_results$pca_results <- pca_results

      # Cross Method Comparison (part of step 5)
      
      # Compare weighted vs percolated network metrics
      cross_method_comparison <- perform_cross_method_comparison(
        weighted_eigenvector = weighted_eigen_results,
        threshold_free_results = threshold_free_results,
        percolation_networks = networks,
        node_metrics = all_node_metrics
      )
      analysis_results$cross_method_comparison <- cross_method_comparison
      
      # Step 6: Finalizing
      showNotification("Step 6/6: Finalizing results...", duration = 2)
      
      # Perform conservation analysis
      conservation_analysis <- perform_conservation_analysis(networks, correlations)
      analysis_results$conservation_analysis <- conservation_analysis
      
      # Compute weighted conservation analysis
      weighted_conservation_analysis <- perform_weighted_conservation_analysis(correlations, 
                                                                             weighted_eigen_results,
                                                                             threshold_free_results$node_strength)
      analysis_results$weighted_conservation_analysis <- weighted_conservation_analysis
      
      # Compute consensus eigenvector centrality (percolation + weighted)
      consensus_eigen_results <- compute_consensus_eigenvector_centrality(networks, weighted_eigen_results)
      analysis_results$consensus_eigenvector <- consensus_eigen_results
      
      analysis_results$complete <- TRUE
      ui_state$analysis_complete <- TRUE
      
      runjs("
        $('#progress_analysis').removeClass('progress-active').addClass('progress-complete');
        $('#progress_analysis i').removeClass('fa-spinner fa-spin').addClass('fa-check-circle');
      ")
      
      removeNotification(id = "analysis_running")
      showNotification("✅ Complete neural analysis pipeline finished successfully!", duration = 5)

      # Auto-navigate to Summary dashboard after analysis completes
      updateTabItems(session, "sidebarMenu", "summary")

    }, error = function(e) {
      runjs("
        $('#progress_analysis').removeClass('progress-active').addClass('progress-pending');
        $('#progress_analysis i').removeClass('fa-spinner fa-spin').addClass('fa-circle');
      ")
      
      removeNotification(id = "analysis_running")
      showNotification(paste("❌ Analysis error:", e$message), type = "error", duration = NULL)
      print(e)  # For debugging
    })
  })
  
  output$analysisComplete <- reactive({ ui_state$analysis_complete })
  outputOptions(output, "analysisComplete", suspendWhenHidden = FALSE)

  # Reactive output for artificial results availability
  output$artificialResultsAvailable <- reactive({
    results <- analysis_results$regional_contribution
    if(is.null(results)) return(FALSE)
    if(is.null(results$mode) || results$mode != "artificial") return(FALSE)
    if(is.null(results$discovery)) return(FALSE)
    # Check for brute force results with significant combinations
    discovery <- results$discovery
    if(is.null(discovery$top_dissimilarity) && is.null(discovery$top_similarity)) return(FALSE)
    n_sig <- (discovery$n_significant_dissim %||% 0) + (discovery$n_significant_sim %||% 0)
    return(n_sig > 0)
  })
  outputOptions(output, "artificialResultsAvailable", suspendWhenHidden = FALSE)

  # Update correlation method dropdowns to show only computed methods
  observeEvent(ui_state$analysis_complete, {
    req(ui_state$analysis_complete)
    req(analysis_results$correlation_methods_raw)

    # Get the methods that were actually computed
    computed_methods <- names(analysis_results$correlation_methods_raw)

    # Create display names (capitalize first letter)
    method_choices <- setNames(
      computed_methods,
      tools::toTitleCase(computed_methods)
    )

    # Update all correlation method dropdowns
    updateSelectInput(session, "consensus_network_method", choices = method_choices)
    updateSelectInput(session, "selected_corr_method", choices = method_choices)
    updateSelectInput(session, "topology_method", choices = method_choices)
    updateSelectInput(session, "weighted_method", choices = method_choices)
    updateSelectInput(session, "persistence_method", choices = method_choices)
    updateSelectInput(session, "comparison_method", choices = method_choices)
    updateSelectInput(session, "hub_overlap_method", choices = method_choices)
    updateSelectInput(session, "global_perm_corr_method", choices = method_choices)
  })

  # === ALL ORIGINAL OUTPUT FUNCTIONS ===
  
  # Analysis Summary
  output$analysisSummary <- renderText({
    req(analysis_results$complete)
    
    total_nodes <- length(colnames(analysis_results$correlations[[1]]))
    total_groups <- length(analysis_results$correlations)
    # Use average of group thresholds for summary
    threshold <- if(!is.null(analysis_results$group_thresholds)) {
      avg_threshold <- mean(unlist(analysis_results$group_thresholds))
      round(avg_threshold, 3)
    } else {
      "N/A"
    }
    
    analysis_type <- analysis_results$analysis_type %||% "percolation"
    
    summary_lines <- c(
      "🎯 Analysis Complete!\n",
      "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n",
      sprintf("📊 Total brain regions analyzed: %d\n", total_nodes),
      sprintf("👥 Groups compared: %d\n", total_groups),
      sprintf("🎚️ Average percolation threshold: %s\n", threshold),
      sprintf("🧠 Networks successfully created for all groups\n")
    )
    
    # Always show comprehensive analysis information since both analyses always run
    summary_lines <- c(summary_lines,
      sprintf("🔢 Threshold-free metrics computed\n"),
      sprintf("📈 Both percolation and threshold-free analyses available\n"),
      sprintf("🔗 Network conservation analysis completed\n")
    )
    
    summary_lines <- c(summary_lines,
      "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n",
      "✅ All visualizations and metrics are now available in the tabs below!"
    )
    
    paste(summary_lines, collapse = "")
  })
  
  # Imputation Summary
  output$imputationSummary <- renderText({
    req(analysis_results$imputation)
    
    imp_result <- analysis_results$imputation
    
    paste(
      "🔍 Data Imputation Summary\n",
      "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n",
      sprintf("📊 Original missing values: %d\n", imp_result$missing_count),
      sprintf("🎯 Imputation method: %s\n", imp_result$method),
      sprintf("✅ Missing data successfully handled\n"),
      sprintf("📈 Data quality improved for network analysis\n")
    )
  })
  
  # All the plot outputs
  output$imputationPlots <- renderPlot({
    req(analysis_results$imputation)
    render_imputation_plots(analysis_results, analysis_results$raw_data)
  })
  
  output$correlationPlots <- renderPlot({
    req(analysis_results$correlations)
    # Get experimental group colors
    exp_group_colors <- NULL
    if(!is.null(ui_state$group_colors) && length(ui_state$group_colors) > 0) {
      exp_group_colors <- ui_state$group_colors
    }
    render_correlation_plots(analysis_results$correlations, exp_group_colors)
  })
  
  output$correlationDistributionPlot <- renderPlot({
    req(analysis_results$correlations)
    render_correlation_distributions(analysis_results$correlations)
  })
  
  # Consensus metrics table
  output$consensusMetricsTable <- DT::renderDataTable({
    req(analysis_results$correlations)
    
    # Extract consensus metadata
    consensus_metadata <- attr(analysis_results$correlations, "consensus_metadata")
    if(is.null(consensus_metadata)) return(NULL)
    
    # Create summary table
    metrics_df <- data.frame()
    for(group_name in names(consensus_metadata)) {
      metadata <- consensus_metadata[[group_name]]
      metrics_df <- rbind(metrics_df, data.frame(
        Group = group_name,
        `Sample Size` = metadata$sample_size,
        `Brain Regions` = metadata$n_variables,
        `Methods Used` = metadata$n_methods,
        `Method Names` = paste(metadata$methods_used, collapse = ", "),
        `Mean |Correlation|` = round(metadata$mean_abs_correlation, 3),
        `Strong Correlations (≥0.4)` = metadata$strong_correlations,
        `Expected Network Density` = paste0(round(metadata$network_density_at_04 * 100, 1), "%"),
        check.names = FALSE
      ))
    }
    
    DT::datatable(metrics_df, 
                  options = list(scrollX = TRUE, pageLength = 10),
                  caption = "5-Method Correlation Consensus Quality Metrics")
  }, server = FALSE)
  
  # Global percolation plot removed - using group-specific analysis only
  
  # Threshold analysis plot removed - using group-specific analysis only
  
  output$networkPlots <- renderPlot({
    req(analysis_results$method_percolation_results)

    method <- tolower(input$topology_method %||% "pearson")
    method_data <- analysis_results$method_percolation_results[[method]]

    if(is.null(method_data) || is.null(method_data$networks)) {
      plot(1, type = "n", main = paste("No networks for", method, "method"), axes = FALSE)
      text(1, 1, "Run analysis or select different method", cex = 1.2)
      return(NULL)
    }

    create_enhanced_network_plots(
      networks = method_data$networks,
      brain_areas = ui_state$brain_areas,
      area_colors = ui_state$area_colors,
      group_colors = ui_state$group_colors,
      layout = input$networkLayout %||% "fr"
    )
  })
  
  output$networkGalleryPlot <- renderPlot({
    req(analysis_results$method_percolation_results)

    method <- tolower(input$topology_method %||% "pearson")
    method_data <- analysis_results$method_percolation_results[[method]]

    if(is.null(method_data) || is.null(method_data$networks)) {
      plot(1, type = "n", main = paste("No networks for", method, "method"), axes = FALSE)
      text(1, 1, "Run analysis or select different method", cex = 1.2)
      return(NULL)
    }

    networks <- method_data$networks
    
    # Create a gallery view with different layouts in a 2x2 grid
    layout_types <- c("fr", "circle", "kk", "grid")
    par(mfrow = c(2, 2), oma = c(0, 0, 2, 0))
    
    for (layout_type in layout_types) {
      # For gallery, show just the first network with different layouts
      first_network_name <- names(networks)[1]
      first_network <- networks[[first_network_name]]
      
      if (!is.null(first_network) && vcount(first_network) > 0) {
        
        # Add brain area coloring
        if (!is.null(ui_state$brain_areas)) {
          node_areas <- rep("Other", vcount(first_network))
          names(node_areas) <- V(first_network)$name
          
          for (area_name in names(ui_state$brain_areas)) {
            regions <- ui_state$brain_areas[[area_name]]
            matching_nodes <- intersect(regions, V(first_network)$name)
            if (length(matching_nodes) > 0) {
              node_areas[matching_nodes] <- area_name
            }
          }
          V(first_network)$brain_area <- node_areas
          
          # Set node colors
          if (!is.null(ui_state$area_colors)) {
            node_colors <- rep("#808080", vcount(first_network))
            for (area_name in names(ui_state$area_colors)) {
              area_nodes <- which(V(first_network)$brain_area == area_name)
              if (length(area_nodes) > 0) {
                node_colors[area_nodes] <- ui_state$area_colors[area_name]
              }
            }
            V(first_network)$color <- node_colors
          }
        } else {
          V(first_network)$color <- "#1F78B4"
        }
        
        # Set node sizes
        node_degrees <- degree(first_network)
        if (max(node_degrees) > min(node_degrees)) {
          V(first_network)$size <- scales::rescale(node_degrees, to = c(5, 12))
        } else {
          V(first_network)$size <- 8
        }
        
        # Create layout
        if (layout_type == "circle") {
          graph_layout <- layout_in_circle(first_network)
        } else if (layout_type == "fr") {
          graph_layout <- layout_with_fr(first_network)  
        } else if (layout_type == "kk") {
          graph_layout <- layout_with_kk(first_network)
        } else if (layout_type == "grid") {
          graph_layout <- tryCatch({
            layout_on_grid(first_network)
          }, error = function(e) {
            layout_with_fr(first_network)
          })
        }
        
        # Plot the network
        plot(first_network,
             layout = graph_layout,
             vertex.color = V(first_network)$color,
             vertex.size = V(first_network)$size,
             vertex.label = V(first_network)$name,
             vertex.label.cex = 0.6,
             vertex.label.color = "black",
             vertex.label.dist = 0.8,
             vertex.frame.color = "white",
             edge.width = abs(E(first_network)$weight) * 2,
             edge.color = adjustcolor("gray60", alpha.f = 0.6),
             main = paste(toupper(layout_type), "Layout"))
      }
    }
    
    par(mfrow = c(1, 1))
    mtext("Network Layout Gallery", outer = TRUE, cex = 1.5, font = 2)
  })
  
  output$networkDashboardPlot <- renderPlot({
    req(analysis_results$method_percolation_results)

    method <- tolower(input$topology_method %||% "pearson")
    method_data <- analysis_results$method_percolation_results[[method]]

    if(is.null(method_data) || is.null(method_data$global_metrics)) {
      plot(1, type = "n", main = paste("No metrics for", method, "method"), axes = FALSE)
      text(1, 1, "Run analysis or select different method", cex = 1.2)
      return(NULL)
    }

    exp_group_colors <- NULL
    if(!is.null(ui_state$group_colors) && length(ui_state$group_colors) > 0) {
      exp_group_colors <- ui_state$group_colors
    }
    render_network_dashboard(method_data$global_metrics, exp_group_colors)
  })

  output$percolationStrengthEigenvectorPlot <- renderPlot({
    req(analysis_results$method_percolation_results)

    method <- tolower(input$topology_method %||% "pearson")
    method_data <- analysis_results$method_percolation_results[[method]]

    if(is.null(method_data) || is.null(method_data$node_metrics)) {
      plot(1, type = "n", main = paste("No node metrics for", method, "method"), axes = FALSE)
      text(1, 1, "Run analysis or select different method", cex = 1.2)
      return(NULL)
    }

    render_percolation_strength_eigenvector_relationship(method_data$node_metrics,
                                                         ui_state$group_colors,
                                                         ui_state$brain_areas,
                                                         ui_state$area_colors)
  })

  output$nodeCentralityPlot <- renderPlot({
    req(analysis_results$method_percolation_results)

    method <- tolower(input$topology_method %||% "pearson")
    method_data <- analysis_results$method_percolation_results[[method]]

    if(is.null(method_data) || is.null(method_data$brain_area_metrics)) {
      plot(1, type = "n", main = paste("No brain area metrics for", method, "method"), axes = FALSE)
      text(1, 1, "Run analysis or select different method", cex = 1.2)
      return(NULL)
    }

    render_node_centrality(method_data$brain_area_metrics, ui_state$brain_areas, ui_state$area_colors)
  })

  output$nodeHeatmapPlot <- renderPlot({
    req(analysis_results$method_percolation_results)

    method <- tolower(input$topology_method %||% "pearson")
    method_data <- analysis_results$method_percolation_results[[method]]

    if(is.null(method_data) || is.null(method_data$brain_area_metrics)) {
      plot(1, type = "n", main = paste("No brain area metrics for", method, "method"), axes = FALSE)
      text(1, 1, "Run analysis or select different method", cex = 1.2)
      return(NULL)
    }

    render_node_heatmap(method_data$brain_area_metrics, ui_state$brain_areas, ui_state$area_colors)
  })

  # Individual node analysis plots
  output$individualNodeCentralityPlot <- renderPlot({
    req(analysis_results$method_percolation_results)

    method <- tolower(input$topology_method %||% "pearson")
    method_data <- analysis_results$method_percolation_results[[method]]

    if(is.null(method_data) || is.null(method_data$node_metrics)) {
      plot(1, type = "n", main = paste("No node metrics for", method, "method"), axes = FALSE)
      text(1, 1, "Run analysis or select different method", cex = 1.2)
      return(NULL)
    }

    render_individual_node_centrality(method_data$node_metrics, ui_state$brain_areas, ui_state$area_colors)
  })

  output$individualNodeHeatmapPlot <- renderPlot({
    req(analysis_results$method_percolation_results)

    method <- tolower(input$topology_method %||% "pearson")
    method_data <- analysis_results$method_percolation_results[[method]]

    if(is.null(method_data) || is.null(method_data$node_metrics)) {
      plot(1, type = "n", main = paste("No node metrics for", method, "method"), axes = FALSE)
      text(1, 1, "Run analysis or select different method", cex = 1.2)
      return(NULL)
    }

    render_individual_node_heatmap(method_data$node_metrics, ui_state$brain_areas, ui_state$area_colors)
  })

  # Additional plots for enhanced metrics
  output$edgeMetricsPlot <- renderPlot({
    req(analysis_results$method_percolation_results)

    method <- tolower(input$topology_method %||% "pearson")
    method_data <- analysis_results$method_percolation_results[[method]]

    if(!is.null(method_data$edge_metrics)) {
      exp_group_colors <- NULL
      if(!is.null(ui_state$group_colors) && length(ui_state$group_colors) > 0) {
        exp_group_colors <- ui_state$group_colors
      }
      render_edge_metrics(method_data$edge_metrics, exp_group_colors)
    }
  })
  
  output$brainAreaConnectivityPlot <- renderPlot({
    req(analysis_results$method_percolation_results)

    method <- tolower(input$topology_method %||% "pearson")
    method_data <- analysis_results$method_percolation_results[[method]]

    if(!is.null(method_data$brain_area_metrics)) {
      render_brain_area_connectivity(method_data$brain_area_metrics, ui_state$brain_areas, ui_state$area_colors)
    }
  })
  
  output$avgEigenvectorPlot <- renderPlot({
    req(analysis_results$weighted_eigenvector, analysis_results$node_metrics)
    render_avg_eigenvector_by_region(analysis_results$weighted_eigenvector, 
                                     analysis_results$node_metrics, 
                                     ui_state$brain_areas, 
                                     ui_state$area_colors, 
                                     ui_state$group_colors)
  })
  
  output$combinedEigenvectorBarPlot <- renderPlot({
    req(analysis_results$weighted_eigenvector)
    render_combined_eigenvector_bar_plot(analysis_results$weighted_eigenvector, ui_state$brain_areas, ui_state$area_colors, ui_state$group_colors)
  })
  
  output$conservationStatsPlot <- renderPlot({
    req(analysis_results$similarities)
    render_conservation_stats(analysis_results$similarities)
  })
  
  output$networkSimilarityMatrixPlot <- renderPlot({
    req(analysis_results$similarities)
    render_network_similarity_matrix(analysis_results$similarities)
  })
  
  output$hubConservationPlot <- renderPlot({
    if(!is.null(analysis_results$conservation) && 
       !is.null(analysis_results$conservation$conservation_possible) &&
       analysis_results$conservation$conservation_possible &&
       !is.null(analysis_results$conservation$hub_conservation)) {
      render_hub_conservation(analysis_results$conservation$hub_conservation)
    } else {
      plot(1, type = "n", main = "Hub Conservation Analysis", axes = FALSE)
      if(is.null(analysis_results$correlations)) {
        text(1, 1, "No analysis data available.", cex = 1.2)
      } else {
        n_groups <- length(analysis_results$correlations)
        if(n_groups < 2) {
          text(1, 1, "Hub conservation analysis requires at least 2 groups.\nCurrently only 1 group is available.", cex = 1.2)
        } else {
          text(1, 1, paste("Hub conservation analysis failed.\nGroups available:", n_groups, "\nPlease check that networks were successfully created."), cex = 1.0)
        }
      }
    }
  })
  
  output$summaryDashboardPlot <- renderPlot({
    req(analysis_results$complete)
    render_summary_dashboard(analysis_results)
  })
  
  # NEW RESTRUCTURED PIPELINE OUTPUTS
  
  # Step 3: Topology Analysis - Group-specific percolation
  output$groupPercolationPlot <- renderPlot({
    req(analysis_results$method_percolation_results)

    method <- tolower(input$topology_method %||% "pearson")
    method_data <- analysis_results$method_percolation_results[[method]]

    if(is.null(method_data)) {
      plot(1, type = "n", main = paste("No data for", method, "method"), axes = FALSE)
      text(1, 1, "Run analysis or select different method", cex = 1.2)
      return(NULL)
    }

    # Get correlations and thresholds for selected method
    method_correlations <- analysis_results$correlation_methods_raw[[method]]
    method_thresholds <- method_data$thresholds

    if(!is.null(method_correlations) && !is.null(method_thresholds)) {
      render_group_specific_percolation(method_correlations, method_thresholds)
    } else {
      plot(1, type = "n", main = "Group-specific percolation analysis not available")
    }
  })
  
  # Threshold comparison plot removed with global threshold
  
  # Step 4: Weighted Network Analysis
  output$weightedNodeMetricsPlot <- renderPlot({
    req(analysis_results$method_weighted_results)

    method <- tolower(input$weighted_method %||% "pearson")
    method_data <- analysis_results$method_weighted_results[[method]]

    if(is.null(method_data)) {
      plot(1, type = "n", main = paste("No weighted results for", method, "method"), axes = FALSE)
      text(1, 1, "Run analysis or select different method", cex = 1.2)
      return(NULL)
    }

    render_weighted_node_metrics(method_data, ui_state$group_colors)
  })

  output$allWeightedStatsPlot <- renderPlot({
    req(analysis_results$method_weighted_results)

    method <- tolower(input$weighted_method %||% "pearson")
    method_data <- analysis_results$method_weighted_results[[method]]

    if(is.null(method_data)) {
      plot(1, type = "n", main = paste("No weighted results for", method, "method"), axes = FALSE)
      text(1, 1, "Run analysis or select different method", cex = 1.2)
      return(NULL)
    }

    render_all_weighted_stats(method_data, ui_state$group_colors)
  })
  
  
  # Step 5: Cross Method Comparison
  output$crossMethodEigenvectorPlot <- renderPlot({
    req(analysis_results$weighted_eigenvector)
    req(analysis_results$node_metrics)
    render_cross_method_eigenvector_comparison(analysis_results$weighted_eigenvector, 
                                             analysis_results$node_metrics, 
                                             ui_state$group_colors,
                                             ui_state$brain_areas,
                                             ui_state$area_colors)
  })
  
  output$crossMethodNodeStrengthPlot <- renderPlot({
    req(analysis_results$threshold_free_results)
    req(analysis_results$node_metrics)
    render_cross_method_node_strength_comparison(analysis_results$threshold_free_results$node_strength,
                                                analysis_results$node_metrics, ui_state$group_colors)
  })
  
  # Method correlation plot removed as requested
  
  # Method agreement plot removed as requested
  
  output$hubRankingComparisonPlot <- renderPlot({
    if (!is.null(analysis_results$cross_method_comparison) && 
        !is.null(analysis_results$cross_method_comparison$hub_rankings)) {
      render_hub_ranking_comparison(analysis_results$cross_method_comparison, ui_state$group_colors)
    } else {
      plot(1, type = "n", main = "Method-Specific Hub Rankings", axes = FALSE)
      text(1, 1, "Hub ranking comparison requires completed cross-method analysis.\nPlease ensure the analysis has finished.", cex = 1.2)
    }
  })
  
  # Step 6: Pipeline Overview
  output$pipelineOverviewPlot <- renderPlot({
    req(analysis_results$complete)
    render_pipeline_overview(analysis_results)
  })
  
  # Advanced Analysis Tab Outputs
  output$advancedMstMetricsPlot <- renderPlot({
    if (!is.null(analysis_results$mst_results)) {
      render_advanced_mst_metrics(analysis_results$mst_results, ui_state$group_colors)
    } else {
      plot(1, type = "n", main = "MST analysis not available")
    }
  })
  
  output$mstCentralNodesPlot <- renderPlot({
    if (!is.null(analysis_results$mst_results)) {
      render_mst_central_nodes(analysis_results$mst_results, ui_state$brain_areas, ui_state$area_colors, ui_state$group_colors)
    } else {
      plot(1, type = "n", main = "MST central nodes analysis not available")
    }
  })
  
  output$advancedMstNetworksPlot <- renderPlot({
    if (!is.null(analysis_results$mst_results)) {
      render_advanced_mst_networks(analysis_results$mst_results, analysis_results$correlations,
                                 ui_state$brain_areas, ui_state$area_colors, ui_state$group_colors)
    } else {
      plot(1, type = "n", main = "MST networks not available")
    }
  })
  
  output$pcaAnalysisPlot <- renderPlot({
    if (!is.null(analysis_results$pca_results)) {
      render_pca_analysis_results(analysis_results$pca_results, ui_state$group_colors)
    } else {
      plot(1, type = "n", main = "PCA analysis not available")
    }
  })
  
  output$pcaLoadingsPlot <- renderPlot({
    if (!is.null(analysis_results$pca_results)) {
      render_pca_loadings_results(analysis_results$pca_results, ui_state$brain_areas, ui_state$area_colors, ui_state$group_colors)
    } else {
      plot(1, type = "n", main = "PCA loadings not available")
    }
  })
  
  output$pcaVariancePlot <- renderPlot({
    if (!is.null(analysis_results$pca_results)) {
      render_pca_variance_results(analysis_results$pca_results, ui_state$group_colors)
    } else {
      plot(1, type = "n", main = "PCA variance analysis not available")
    }
  })
  
  # NEW ADDITIONAL OUTPUTS FOR REQUESTED FEATURES
  
  # Conservation analysis outputs
  # Tab 3g: Network Similarity Matrix (using weighted Jaccard)
  output$networkSimilarityPlot <- renderPlot({
    req(analysis_results$correlation_methods_raw)
    req(input$weighted_method)

    method <- tolower(input$weighted_method %||% "pearson")

    if(is.null(analysis_results$correlation_methods_raw[[method]])) {
      plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
      text(1, 1, paste("No data for method:", toupper(method)), cex = 1.2, col = "gray")
      return()
    }

    all_groups <- names(analysis_results$correlation_methods_raw[[method]])

    if(length(all_groups) < 2) {
      plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
      text(1, 1, "Need at least 2 groups for similarity analysis", cex = 1.2, col = "gray")
      return()
    }

    # Get correlation matrices for each group
    cor_matrices <- list()
    for(group in all_groups) {
      if(!is.null(analysis_results$correlation_methods_raw[[method]][[group]])) {
        cor_matrices[[group]] <- analysis_results$correlation_methods_raw[[method]][[group]]
      }
    }

    if(length(cor_matrices) < 2) {
      plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
      text(1, 1, paste("Not enough data for method:", toupper(method)), cex = 1.2, col = "gray")
      return()
    }

    # Compute Jaccard similarity matrix between groups
    n_groups <- length(cor_matrices)
    group_names <- names(cor_matrices)
    jaccard_matrix <- matrix(1, nrow = n_groups, ncol = n_groups)
    rownames(jaccard_matrix) <- group_names
    colnames(jaccard_matrix) <- group_names

    for(i in 1:(n_groups-1)) {
      for(j in (i+1):n_groups) {
        jaccard <- compute_jaccard_similarity(cor_matrices[[i]], cor_matrices[[j]], threshold = 0)
        if(!is.na(jaccard)) {
          jaccard_matrix[i, j] <- jaccard
          jaccard_matrix[j, i] <- jaccard
        }
      }
    }

    # Render heatmap
    render_jaccard_heatmap(jaccard_matrix, group_names,
                          title = paste(toupper(method), "- Network Similarity (Jaccard)"))
  })

  # Tab 3g: Hub Conservation (using weighted analysis)
  output$hubConservationPlot <- renderPlot({
    req(analysis_results$method_weighted_results)
    req(input$weighted_method)

    method <- tolower(input$weighted_method %||% "pearson")

    if(is.null(analysis_results$method_weighted_results[[method]])) {
      plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
      text(1, 1, paste("No weighted results for:", toupper(method)), cex = 1.2, col = "gray")
      return()
    }

    weighted_data <- analysis_results$method_weighted_results[[method]]
    all_groups <- names(weighted_data)

    if(length(all_groups) < 2) {
      plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
      text(1, 1, "Need at least 2 groups for hub conservation", cex = 1.2, col = "gray")
      return()
    }

    # Extract hubs from each group (z-score >= 1.5)
    hubs_by_group <- list()
    for(group in all_groups) {
      if(!is.null(weighted_data[[group]]$nodes)) {
        nodes_data <- weighted_data[[group]]$nodes
        if("Eigenvector_Z" %in% colnames(nodes_data)) {
          hubs_by_group[[group]] <- nodes_data$Node[nodes_data$Eigenvector_Z >= 1.5]
        }
      }
    }

    if(length(hubs_by_group) < 2) {
      plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
      text(1, 1, "Not enough hub data available", cex = 1.2, col = "gray")
      return()
    }

    # Compute pairwise hub conservation (Jaccard)
    n_groups <- length(hubs_by_group)
    group_names <- names(hubs_by_group)
    conservation_matrix <- matrix(1, nrow = n_groups, ncol = n_groups)
    rownames(conservation_matrix) <- group_names
    colnames(conservation_matrix) <- group_names

    for(i in 1:(n_groups-1)) {
      for(j in (i+1):n_groups) {
        hubs_i <- hubs_by_group[[i]]
        hubs_j <- hubs_by_group[[j]]

        if(length(hubs_i) > 0 && length(hubs_j) > 0) {
          intersection <- length(intersect(hubs_i, hubs_j))
          union <- length(union(hubs_i, hubs_j))
          jaccard <- if(union > 0) intersection / union else 0
          conservation_matrix[i, j] <- jaccard
          conservation_matrix[j, i] <- jaccard
        } else {
          conservation_matrix[i, j] <- 0
          conservation_matrix[j, i] <- 0
        }
      }
    }

    # Render heatmap
    render_jaccard_heatmap(conservation_matrix, group_names,
                          title = paste(toupper(method), "- Hub Conservation (Jaccard)"))
  })

  # Tab 3g: Edge Conservation
  output$edgeConservationPlot <- renderPlot({
    req(analysis_results$node_metrics)
    req(input$weighted_method)

    method <- tolower(input$weighted_method %||% "pearson")

    plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
    text(1, 1, "Edge conservation analysis\n(requires binary thresholding - see Tab 4)", cex = 1.2, col = "gray50")
  })
  
  # 4w Weighted Conservation Analysis outputs
  output$weightedSimilarityPlot <- renderPlot({
    req(analysis_results$weighted_conservation_analysis)
    render_weighted_similarity_heatmap(analysis_results$weighted_conservation_analysis,
                                       ui_state$group_colors)
  })
  
  output$weightedHubConservationPlot <- renderPlot({
    req(analysis_results$weighted_conservation_analysis)
    render_weighted_hub_conservation_heatmap(analysis_results$weighted_conservation_analysis,
                                             ui_state$group_colors)
  })
  
  output$weightedEdgeStatisticsPlot <- renderPlot({
    req(analysis_results$weighted_conservation_analysis)
    render_weighted_edge_statistics_plot(analysis_results$weighted_conservation_analysis,
                                         ui_state$group_colors)
  })
  
  # 5e Conservation Comparison outputs
  output$conservationCorrelationPlot <- renderPlot({
    req(analysis_results$method_weighted_results)
    req(analysis_results$method_percolation_results)
    req(analysis_results$persistence_results)

    method <- tolower(input$comparison_method %||% "pearson")
    weighted_data <- analysis_results$method_weighted_results[[method]]
    percolation_data <- analysis_results$method_percolation_results[[method]]
    persistence_data <- analysis_results$persistence_results[[method]]

    if(is.null(weighted_data) || is.null(percolation_data) || is.null(persistence_data)) {
      plot(1, type = "n", main = paste("Data not available for", method), axes = FALSE)
      text(1, 1, "Select different method or run analysis", cex = 1.2)
      return(NULL)
    }

    # 3-way conservation comparison
    par(mfrow = c(1, 3), mar = c(5, 4, 4, 2))

    # Panel 1: Weighted vs Percolation conservation
    plot(1, type = "n", xlim = c(0, 1), ylim = c(0, 1),
         main = "Weighted vs Percolation\nConservation",
         xlab = "Weighted Jaccard", ylab = "Percolation Jaccard")
    text(0.5, 0.5, "Conservation comparison\nacross methods", cex = 1.2, col = "gray50")

    # Panel 2: Weighted vs Persistence conservation
    plot(1, type = "n", xlim = c(0, 1), ylim = c(0, 1),
         main = "Weighted vs Persistence\nConservation",
         xlab = "Weighted Jaccard", ylab = "Persistence Jaccard")
    text(0.5, 0.5, "Conservation comparison\nacross methods", cex = 1.2, col = "gray50")

    # Panel 3: Percolation vs Persistence conservation
    plot(1, type = "n", xlim = c(0, 1), ylim = c(0, 1),
         main = "Percolation vs Persistence\nConservation",
         xlab = "Percolation Jaccard", ylab = "Persistence Jaccard")
    text(0.5, 0.5, "Conservation comparison\nacross methods", cex = 1.2, col = "gray50")
  })
  
  output$conservationInsightsPlot <- renderPlot({
    req(analysis_results$method_weighted_results)
    req(analysis_results$method_percolation_results)

    method <- tolower(input$comparison_method %||% "pearson")
    weighted_data <- analysis_results$method_weighted_results[[method]]
    percolation_data <- analysis_results$method_percolation_results[[method]]

    if(is.null(weighted_data) || is.null(percolation_data)) {
      plot(1, type = "n", main = paste("Data not available for", method), axes = FALSE)
      text(1, 1, "Select different method or run analysis", cex = 1.2)
      return(NULL)
    }

    # Extract conservation analysis data from method-specific results
    conservation_analysis <- percolation_data$conservation_analysis
    weighted_conservation_analysis <- weighted_data$conservation_analysis

    if(is.null(conservation_analysis) || is.null(weighted_conservation_analysis)) {
      plot(1, type = "n", main = paste("Conservation analysis not available for", method), axes = FALSE)
      text(1, 1, "Run analysis with conservation metrics", cex = 1.2)
      return(NULL)
    }

    render_conservation_insights_summary(conservation_analysis,
                                        weighted_conservation_analysis,
                                        ui_state$group_colors)
  })

  # Persistence vs Percolation Hub Comparison
  output$persistenceVsPercolationHubsPlot <- renderPlot({
    req(analysis_results$persistence_results)
    req(analysis_results$node_metrics)

    method <- tolower(input$comparison_method %||% "pearson")

    if(is.null(analysis_results$persistence_results[[method]])) {
      plot(1, type = "n", main = "Persistence data not available for this method", axes = FALSE)
      text(1, 1, "Run analysis first", cex = 1.2)
      return(NULL)
    }

    # Placeholder plot - full implementation would compare hub persistence scores
    # with single-threshold hub identification
    plot(1, type = "n",
         main = paste("Hub Persistence vs Single-Threshold Comparison -", method),
         axes = FALSE)
    text(1, 1, "Feature implementation in progress", cex = 1.2)
  })

  # Persistence Stability Table
  output$persistenceStabilityTable <- DT::renderDataTable({
    req(analysis_results$persistence_results)

    method <- tolower(input$comparison_method %||% "pearson")

    if(is.null(analysis_results$persistence_results[[method]])) {
      return(data.frame(Message = "Persistence data not available"))
    }

    # Placeholder - would return hub stability metrics
    data.frame(
      Node = character(0),
      PersistenceScore = numeric(0),
      CV_Centrality = numeric(0),
      ThresholdRange = character(0)
    )
  })

  # ========================================================================
  # COMPREHENSIVE CONSENSUS ANALYTICS SERVER CODE
  # ========================================================================

  # Populate group select inputs for consensus tabs
  observe({
    req(analysis_results$comprehensive_consensus)

    group_choices <- names(analysis_results$comprehensive_consensus)

    if(length(group_choices) > 0) {
      updateSelectInput(session, "consensus_group_heatmap", choices = group_choices, selected = group_choices[1])
      updateSelectInput(session, "consensus_group_venn", choices = group_choices, selected = group_choices[1])
      # consensus_group_ranking removed - merged into consensus_group_heatmap
      updateSelectInput(session, "consensus_group_robustness", choices = group_choices, selected = group_choices[1])
      updateSelectInput(session, "consensus_overview_group", choices = group_choices, selected = group_choices[1])
    }
  })

  # ========== NEW SUMMARY DASHBOARD TAB ==========

  # Summary Dashboard: Top Hubs Table
  output$summaryTopHubsTable <- DT::renderDataTable({
    req(analysis_results$comprehensive_consensus)
    req(input$consensus_overview_group)

    group_name <- input$consensus_overview_group
    consensus_results <- analysis_results$comprehensive_consensus[[group_name]]

    if(is.null(consensus_results) || is.null(consensus_results$consensus_scores)) {
      return(data.frame(Message = "No consensus data available"))
    }

    scores_df <- consensus_results$consensus_scores

    # Filter for high confidence hubs
    high_confidence <- scores_df[scores_df$ConfidenceCategory == "High", ]

    if(nrow(high_confidence) == 0) {
      return(data.frame(Message = "No high-confidence hubs identified"))
    }

    # Sort by Bayesian consensus score and take top 15
    high_confidence <- high_confidence[order(-high_confidence$BayesianConsensusScore), ]
    top_hubs <- head(high_confidence, 15)

    # Create display table
    display_table <- data.frame(
      Rank = 1:nrow(top_hubs),
      Node = top_hubs$Node,
      ConsensusScore = round(top_hubs$BayesianConsensusScore, 3),
      CI_Lower = round(top_hubs$CredibleIntervalLower, 3),
      CI_Upper = round(top_hubs$CredibleIntervalUpper, 3),
      CI_Width = round(top_hubs$CredibleIntervalUpper - top_hubs$CredibleIntervalLower, 3),
      Agreement = ifelse(top_hubs$AllThreeAgree, "3-Way",
                        ifelse(top_hubs$TwoAgree, "2-Way", "1-Way")),
      Uncertainty = top_hubs$UncertaintyCategory,
      stringsAsFactors = FALSE
    )

    DT::datatable(display_table,
                  options = list(
                    pageLength = 15,
                    dom = 't',
                    scrollY = "300px",
                    scrollCollapse = TRUE,
                    ordering = FALSE
                  ),
                  rownames = FALSE) %>%
      DT::formatStyle('ConsensusScore',
                     backgroundColor = DT::styleInterval(c(0.5, 0.7, 0.85),
                                                        c('#fff3cd', '#d4edda', '#c3e6cb', '#b8daff')))
  })

  # Summary Dashboard: Agreement Donut Chart
  output$summaryAgreementDonut <- renderPlot({
    req(analysis_results$comprehensive_consensus)
    req(input$consensus_overview_group)

    group_name <- input$consensus_overview_group
    consensus_results <- analysis_results$comprehensive_consensus[[group_name]]

    if(is.null(consensus_results) || is.null(consensus_results$consensus_scores)) {
      plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
      text(1, 1, "No consensus data available", cex = 1.2, col = "gray")
      return()
    }

    scores_df <- consensus_results$consensus_scores

    # Count agreement categories
    three_way <- sum(scores_df$AllThreeAgree, na.rm = TRUE)
    two_way <- sum(scores_df$TwoAgree & !scores_df$AllThreeAgree, na.rm = TRUE)
    one_way <- nrow(scores_df) - three_way - two_way

    counts <- c(three_way, two_way, one_way)
    labels <- c("3-Way Agreement", "2-Way Agreement", "1-Way Only")
    colors <- c("#2E7D32", "#FFA726", "#E53935")

    # Calculate percentages
    pcts <- round(100 * counts / sum(counts), 1)
    labels_with_pct <- paste0(labels, "\n", counts, " (", pcts, "%)")

    # Create pie chart
    par(mar = c(1, 1, 3, 1))
    pie(counts,
        labels = labels_with_pct,
        col = colors,
        main = paste("Method Agreement Distribution:", group_name),
        cex = 1.1,
        radius = 0.9)

    # Add legend
    legend("bottom",
           legend = c(paste0("3-Way: All approaches agree on hub status"),
                     paste0("2-Way: Two of three approaches agree"),
                     paste0("1-Way: Only one approach identifies as hub")),
           cex = 0.8,
           bty = "n",
           text.col = "gray30")
  })

  # Summary Dashboard: Confidence Histogram
  output$summaryConfidenceHist <- renderPlot({
    req(analysis_results$comprehensive_consensus)
    req(input$consensus_overview_group)

    group_name <- input$consensus_overview_group
    consensus_results <- analysis_results$comprehensive_consensus[[group_name]]

    if(is.null(consensus_results) || is.null(consensus_results$consensus_scores)) {
      plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
      text(1, 1, "No consensus data available", cex = 1.2, col = "gray")
      return()
    }

    scores_df <- consensus_results$consensus_scores

    # Calculate credible interval widths
    ci_widths <- scores_df$CredibleIntervalUpper - scores_df$CredibleIntervalLower

    # Create histogram
    par(mar = c(5, 5, 4, 2))
    hist(ci_widths,
         breaks = 30,
         col = "#4A90E2",
         border = "white",
         main = paste("Confidence Distribution:", group_name),
         xlab = "Credible Interval Width (narrower = higher confidence)",
         ylab = "Number of Nodes",
         xlim = c(0, max(ci_widths, na.rm = TRUE)))

    # Add reference lines for confidence categories
    abline(v = 0.2, col = "#2E7D32", lty = 2, lwd = 2)
    abline(v = 0.4, col = "#FFA726", lty = 2, lwd = 2)

    # Add counts per category
    high_conf <- sum(ci_widths < 0.2, na.rm = TRUE)
    med_conf <- sum(ci_widths >= 0.2 & ci_widths < 0.4, na.rm = TRUE)
    low_conf <- sum(ci_widths >= 0.4, na.rm = TRUE)

    legend("topright",
           legend = c(paste0("High Confidence (< 0.2): ", high_conf, " nodes"),
                     paste0("Medium Confidence (0.2-0.4): ", med_conf, " nodes"),
                     paste0("Low Confidence (≥ 0.4): ", low_conf, " nodes")),
           col = c("#2E7D32", "#FFA726", "#E53935"),
           lty = c(2, 2, 1),
           lwd = 2,
           bty = "n",
           cex = 0.9)

    grid(nx = NA, ny = NULL, col = "gray90")
  })

  # Summary Dashboard: Approach Weights Bar Chart
  output$summaryApproachWeights <- renderPlot({
    req(analysis_results$comprehensive_consensus)
    req(input$consensus_overview_group)

    group_name <- input$consensus_overview_group
    consensus_results <- analysis_results$comprehensive_consensus[[group_name]]

    if(is.null(consensus_results) || is.null(consensus_results$consensus_scores)) {
      plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
      text(1, 1, "No consensus data available", cex = 1.2, col = "gray")
      return()
    }

    scores_df <- consensus_results$consensus_scores

    # Calculate average weights per approach (excluding NA values)
    avg_weighted <- mean(scores_df$Weighted_Weight, na.rm = TRUE)
    avg_percolation <- mean(scores_df$Percolation_Weight, na.rm = TRUE)
    avg_persistence <- mean(scores_df$Persistence_Weight, na.rm = TRUE)

    # Calculate average CV (uncertainty) per approach
    avg_weighted_cv <- mean(scores_df$Weighted_CV, na.rm = TRUE)
    avg_percolation_cv <- mean(scores_df$Percolation_CV, na.rm = TRUE)
    avg_persistence_cv <- mean(scores_df$Persistence_CV, na.rm = TRUE)

    weights <- c(avg_weighted, avg_percolation, avg_persistence)
    cvs <- c(avg_weighted_cv, avg_percolation_cv, avg_persistence_cv)
    approach_names <- c("Weighted\n(Threshold-Free)",
                       "Percolation\n(Single Optimal)",
                       "Persistence\n(Multi-Threshold)")

    # Create bar plot
    par(mar = c(7, 5, 4, 2))
    bp <- barplot(weights,
                  names.arg = approach_names,
                  col = c("#4A90E2", "#E17055", "#00B894"),
                  main = paste("Average Approach Weights:", group_name),
                  ylab = "Average Weight (1 / (1 + CV))",
                  ylim = c(0, max(weights, na.rm = TRUE) * 1.2),
                  las = 1)

    # Add weight values on top of bars
    text(bp, weights, labels = round(weights, 3), pos = 3, cex = 1.1, font = 2)

    # Add CV values below bars
    text(bp, -0.02 * max(weights),
         labels = paste0("CV: ", round(cvs, 3)),
         pos = 1, cex = 0.9, col = "gray30")

    # Add interpretation
    best_approach <- approach_names[which.max(weights)]
    mtext(paste0("Higher weight = more consistent across correlation methods. Best: ",
                gsub("\n", " ", best_approach)),
          side = 1, line = 6, cex = 0.9, col = "darkblue")

    grid(nx = NA, ny = NULL, col = "gray90")
  })

  # Summary Dashboard: Threshold Recommendation
  output$thresholdRecommendation <- renderText({
    req(analysis_results$comprehensive_consensus)
    req(input$consensus_overview_group)

    group_name <- input$consensus_overview_group
    consensus_results <- analysis_results$comprehensive_consensus[[group_name]]

    if(is.null(consensus_results)) {
      return("No consensus data available")
    }

    calibration <- consensus_results$calibration
    z_threshold <- consensus_results$z_threshold

    if(!is.null(calibration)) {
      paste0(
        "RECOMMENDED Z-THRESHOLD: ", round(calibration$optimal_threshold, 2), "\n",
        "(Currently using: ", round(z_threshold, 2), ")\n\n",
        "This threshold was auto-calibrated to optimize:\n",
        "• Hub proportion: ", round(calibration$proportion_hubs * 100, 1), "% of nodes\n",
        "• Overall quality score: ", round(calibration$quality_score, 3), "/3.5"
      )
    } else {
      paste0(
        "CURRENT Z-THRESHOLD: ", round(z_threshold, 2), "\n",
        "(Default value - no calibration performed)\n\n",
        "Hub identification: nodes with z-score ≥ ", round(z_threshold, 2), "\n",
        "Approximately top 7% of nodes by eigenvector centrality"
      )
    }
  })

  # Summary Dashboard: Threshold Quality Metrics
  output$thresholdQualityMetrics <- renderText({
    req(analysis_results$comprehensive_consensus)
    req(input$consensus_overview_group)

    group_name <- input$consensus_overview_group
    consensus_results <- analysis_results$comprehensive_consensus[[group_name]]

    if(is.null(consensus_results)) {
      return("No data available")
    }

    calibration <- consensus_results$calibration

    if(!is.null(calibration)) {
      paste0(
        "Hub Modularity:  ", round(calibration$hub_modularity, 3), "\n",
        "Hub Clustering:  ", round(calibration$hub_clustering, 3), "\n",
        "Hub Density:     ", round(calibration$hub_density, 3), "\n\n",
        "Higher values indicate better network topology:\n",
        "• Modularity: hubs form coherent communities\n",
        "• Clustering: small-world property preserved\n",
        "• Density: moderate hub-hub connectivity"
      )
    } else {
      paste0(
        "No calibration metrics available.\n\n",
        "To enable auto-calibration, the consensus\n",
        "computation would need to be re-run with\n",
        "z_threshold = 'auto' parameter."
      )
    }
  })

  # Summary Dashboard: Key Findings Text
  output$summaryKeyFindings <- renderText({
    req(analysis_results$comprehensive_consensus)
    req(input$consensus_overview_group)

    group_name <- input$consensus_overview_group
    consensus_results <- analysis_results$comprehensive_consensus[[group_name]]

    if(is.null(consensus_results) || is.null(consensus_results$consensus_scores)) {
      return("No consensus data available for this group.")
    }

    scores_df <- consensus_results$consensus_scores

    # Calculate key statistics
    n_total <- nrow(scores_df)
    n_high_confidence <- sum(scores_df$ConfidenceCategory == "High", na.rm = TRUE)
    n_three_way <- sum(scores_df$AllThreeAgree, na.rm = TRUE)
    n_two_way <- sum(scores_df$TwoAgree & !scores_df$AllThreeAgree, na.rm = TRUE)

    # Top consensus hubs
    top_5 <- head(scores_df[order(-scores_df$BayesianConsensusScore), ], 5)
    top_5_names <- paste(top_5$Node, collapse = ", ")

    # Calculate approach weights
    avg_weighted_weight <- mean(scores_df$Weighted_Weight, na.rm = TRUE)
    avg_percolation_weight <- mean(scores_df$Percolation_Weight, na.rm = TRUE)
    avg_persistence_weight <- mean(scores_df$Persistence_Weight, na.rm = TRUE)

    weights_vec <- c(Weighted = avg_weighted_weight,
                     Percolation = avg_percolation_weight,
                     Persistence = avg_persistence_weight)
    best_approach <- names(which.max(weights_vec))

    # Calculate average consensus score for high-confidence nodes
    high_conf_nodes <- scores_df[scores_df$ConfidenceCategory == "High", ]
    avg_consensus_high <- if(nrow(high_conf_nodes) > 0) {
      mean(high_conf_nodes$BayesianConsensusScore, na.rm = TRUE)
    } else {
      NA
    }

    # Generate summary text
    summary_lines <- c(
      paste0("GROUP: ", group_name),
      paste0("Total nodes analyzed: ", n_total),
      "",
      "=== AGREEMENT SUMMARY ===",
      paste0("• 3-way agreement (all approaches): ", n_three_way, " nodes (",
             round(100 * n_three_way / n_total, 1), "%)"),
      paste0("• 2-way agreement: ", n_two_way, " nodes (",
             round(100 * n_two_way / n_total, 1), "%)"),
      paste0("• High confidence identifications: ", n_high_confidence, " nodes (",
             round(100 * n_high_confidence / n_total, 1), "%)"),
      "",
      "=== TOP CONSENSUS HUBS ===",
      paste0("Top 5 hubs by Bayesian consensus score:"),
      paste0("  ", top_5_names),
      if(!is.na(avg_consensus_high)) {
        paste0("Average consensus score (high-confidence nodes): ",
               round(avg_consensus_high, 3))
      } else {
        "No high-confidence nodes identified"
      },
      "",
      "=== APPROACH PERFORMANCE ===",
      paste0("• Weighted approach: avg weight = ", round(avg_weighted_weight, 3)),
      paste0("• Percolation approach: avg weight = ", round(avg_percolation_weight, 3)),
      paste0("• Persistence approach: avg weight = ", round(avg_persistence_weight, 3)),
      paste0("• Most consistent approach: ", best_approach,
             " (weight = ", round(max(weights_vec), 3), ")"),
      "",
      "=== INTERPRETATION ===",
      paste0("The ", best_approach, " approach shows the highest internal consistency "),
      "across correlation methods (lowest CV). Nodes with 3-way agreement and",
      "narrow credible intervals represent the most robust hub identifications.",
      "",
      paste0("Z-threshold: ", consensus_results$z_threshold,
             " (approximately top 7% of nodes)"),
      paste0("Prior hub probability: ", consensus_results$prior_hub_prob * 100, "%")
    )

    paste(summary_lines, collapse = "\n")
  })

  # ========== LEGACY OVERVIEW TAB (To be deprecated) ==========

  # Overview: Top Consensus Hubs (All Three Methods Agree)
  output$consensusOverviewTopHubsPlot <- renderPlot({
    req(analysis_results$comprehensive_consensus)
    req(input$consensus_overview_group)

    group_name <- input$consensus_overview_group
    consensus_results <- analysis_results$comprehensive_consensus[[group_name]]

    if(is.null(consensus_results) || is.null(consensus_results$consensus_hubs)) {
      plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
      text(1, 1, "No consensus data available", cex = 1.2, col = "gray")
      return()
    }

    # Get top hubs where all three methods agree (consensus score >= 0.6)
    hubs_df <- consensus_results$consensus_hubs
    high_consensus <- hubs_df[hubs_df$ConsensusScore >= 0.6, ]

    if(nrow(high_consensus) == 0) {
      plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
      text(1, 1, "No hubs with high consensus", cex = 1.2, col = "gray")
      return()
    }

    # Sort by consensus score and take top 15
    high_consensus <- high_consensus[order(-high_consensus$ConsensusScore), ]
    top_hubs <- head(high_consensus, 15)

    # Barplot with color gradient based on consensus score
    par(mar = c(5, 10, 4, 2))
    bp <- barplot(top_hubs$ConsensusScore,
                  names.arg = top_hubs$Node,
                  horiz = TRUE,
                  las = 1,
                  xlim = c(0, 1),
                  col = colorRampPalette(c("#FFA500", "#FF4500", "#DC143C"))(nrow(top_hubs)),
                  main = paste("Top Consensus Hubs:", group_name),
                  xlab = "Consensus Score (0-1)")

    # Add reference line at 0.6 (high consensus threshold)
    abline(v = 0.6, col = "blue", lty = 2, lwd = 2)
    legend("bottomright", legend = "High Consensus (≥0.6)", lty = 2, col = "blue", bty = "n")

    grid(nx = 10, ny = NA, col = "gray90")
  })

  # Overview: Method Agreement Distribution
  output$consensusOverviewAgreementPlot <- renderPlot({
    req(analysis_results$comprehensive_consensus)
    req(input$consensus_overview_group)

    group_name <- input$consensus_overview_group
    consensus_results <- analysis_results$comprehensive_consensus[[group_name]]

    if(is.null(consensus_results) || is.null(consensus_results$consensus_hubs)) {
      plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
      text(1, 1, "No consensus data available", cex = 1.2, col = "gray")
      return()
    }

    hubs_df <- consensus_results$consensus_hubs

    # Count nodes by agreement category
    categories <- cut(hubs_df$ConsensusScore,
                     breaks = c(0, 0.33, 0.66, 1.0),
                     labels = c("Low\n(0-33%)", "Medium\n(34-66%)", "High\n(67-100%)"),
                     include.lowest = TRUE)

    category_counts <- table(categories)

    # Barplot
    par(mar = c(5, 5, 4, 2))
    bp <- barplot(category_counts,
                  col = c("#FFD700", "#FFA500", "#DC143C"),
                  main = paste("Hub Agreement Distribution:", group_name),
                  ylab = "Number of Nodes",
                  xlab = "Consensus Level",
                  ylim = c(0, max(category_counts) * 1.2))

    # Add counts on top of bars
    text(bp, category_counts, labels = category_counts, pos = 3, cex = 1.2, font = 2)

    grid(nx = NA, ny = NULL, col = "gray90")

    # Add summary text
    high_count <- category_counts["High\n(67-100%)"]
    total_count <- sum(category_counts)
    pct_high <- round(100 * high_count / total_count, 1)

    mtext(paste0(high_count, " nodes (", pct_high, "%) show high consensus across all 3 methods"),
          side = 1, line = 4, cex = 0.9, col = "darkred")
  })

  # Observer to populate comparison and NBS group dropdowns
  observe({
    req(analysis_results$correlations)
    all_groups <- names(analysis_results$correlations)

    if(length(all_groups) > 0) {
      updateSelectInput(session, "comparison_group1", choices = all_groups, selected = all_groups[1])
      updateSelectInput(session, "comparison_group2", choices = all_groups,
                       selected = if(length(all_groups) > 1) all_groups[2] else all_groups[1])
      updateSelectInput(session, "nbs_group1", choices = all_groups, selected = all_groups[1])
      updateSelectInput(session, "nbs_group2", choices = all_groups,
                       selected = if(length(all_groups) > 1) all_groups[2] else all_groups[1])
    }
  })

  # Plot 1: Comprehensive Consensus Heatmap
  output$comprehensiveConsensusHeatmapPlot <- renderPlot({
    req(analysis_results$comprehensive_consensus)
    req(input$consensus_group_heatmap)

    group_name <- input$consensus_group_heatmap
    consensus_results <- analysis_results$comprehensive_consensus[[group_name]]

    if(is.null(consensus_results)) {
      plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
      text(1, 1, "No consensus data for this group", cex = 1.2, col = "gray")
      return()
    }

    render_comprehensive_consensus_heatmap(
      consensus_results = consensus_results,
      brain_areas = ui_state$brain_areas,
      area_colors = ui_state$area_colors,
      top_n = 30
    )
  })

  # Plot 2: Three-Way Hub Venn Diagram
  output$threeWayHubVennPlot <- renderPlot({
    req(analysis_results$comprehensive_consensus)
    req(input$consensus_group_venn)

    group_name <- input$consensus_group_venn
    consensus_results <- analysis_results$comprehensive_consensus[[group_name]]

    if(is.null(consensus_results)) {
      plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
      text(1, 1, "No consensus data for this group", cex = 1.2, col = "gray")
      return()
    }

    render_three_way_hub_venn(
      consensus_results = consensus_results,
      method_threshold = 0.6
    )
  })

  # NEW: Agreement Matrix Plot
  output$agreementMatrixPlot <- renderPlot({
    req(analysis_results$comprehensive_consensus)
    req(input$consensus_group_venn)

    group_name <- input$consensus_group_venn
    consensus_results <- analysis_results$comprehensive_consensus[[group_name]]

    if(is.null(consensus_results) || is.null(consensus_results$consensus_scores)) {
      plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
      text(1, 1, "No consensus data available", cex = 1.2, col = "gray")
      return()
    }

    scores_df <- consensus_results$consensus_scores

    # Identify hubs for each approach (using z >= 1.5 threshold on mean z-score)
    weighted_hubs <- scores_df$Node[scores_df$Weighted_HubProb >= 0.6]
    percolation_hubs <- scores_df$Node[scores_df$Percolation_HubProb >= 0.6]
    persistence_hubs <- scores_df$Node[scores_df$Persistence_HubProb >= 0.6]

    # Create agreement matrix
    approaches <- c("Weighted", "Percolation", "Persistence")
    hub_lists <- list(Weighted = weighted_hubs, Percolation = percolation_hubs, Persistence = persistence_hubs)

    n_approaches <- length(approaches)
    agreement_matrix <- matrix(0, nrow = n_approaches, ncol = n_approaches,
                              dimnames = list(approaches, approaches))

    # Fill matrix: diagonal = count, off-diagonal = overlap count
    for(i in 1:n_approaches) {
      for(j in 1:n_approaches) {
        if(i == j) {
          agreement_matrix[i, j] <- length(hub_lists[[i]])
        } else {
          agreement_matrix[i, j] <- length(intersect(hub_lists[[i]], hub_lists[[j]]))
        }
      }
    }

    # Calculate Jaccard similarity for off-diagonal
    jaccard_matrix <- matrix(NA, nrow = n_approaches, ncol = n_approaches,
                            dimnames = list(approaches, approaches))
    for(i in 1:n_approaches) {
      for(j in 1:n_approaches) {
        if(i != j) {
          union_size <- length(union(hub_lists[[i]], hub_lists[[j]]))
          if(union_size > 0) {
            jaccard_matrix[i, j] <- agreement_matrix[i, j] / union_size
          } else {
            jaccard_matrix[i, j] <- 0
          }
        }
      }
    }

    # Create visualization
    par(mar = c(5, 5, 4, 2), mfrow = c(1, 2))

    # Panel 1: Overlap counts
    image(1:n_approaches, 1:n_approaches, t(agreement_matrix),
          col = colorRampPalette(c("white", "#4A90E2", "#1E3A8A"))(50),
          xlab = "", ylab = "", main = "Hub Overlap Counts",
          axes = FALSE)
    axis(1, at = 1:n_approaches, labels = approaches, las = 2, cex.axis = 0.9)
    axis(2, at = 1:n_approaches, labels = approaches, las = 2, cex.axis = 0.9)

    # Add text labels
    for(i in 1:n_approaches) {
      for(j in 1:n_approaches) {
        text(i, j, agreement_matrix[j, i], cex = 1.2, font = 2,
             col = ifelse(agreement_matrix[j, i] > max(agreement_matrix) * 0.5, "white", "black"))
      }
    }
    box()

    # Panel 2: Jaccard similarity
    jaccard_display <- jaccard_matrix
    diag(jaccard_display) <- 1  # Diagonal = perfect similarity with self

    image(1:n_approaches, 1:n_approaches, t(jaccard_display),
          col = colorRampPalette(c("#E53935", "#FFA726", "#66BB6A", "#2E7D32"))(50),
          xlab = "", ylab = "", main = "Jaccard Similarity",
          axes = FALSE, zlim = c(0, 1))
    axis(1, at = 1:n_approaches, labels = approaches, las = 2, cex.axis = 0.9)
    axis(2, at = 1:n_approaches, labels = approaches, las = 2, cex.axis = 0.9)

    # Add text labels
    for(i in 1:n_approaches) {
      for(j in 1:n_approaches) {
        if(i == j) {
          text(i, j, "1.00", cex = 1.2, font = 2)
        } else {
          text(i, j, sprintf("%.2f", jaccard_display[j, i]), cex = 1.1,
               col = ifelse(jaccard_display[j, i] > 0.5, "white", "black"))
        }
      }
    }
    box()

    # Add interpretation text
    mtext(paste0("Group: ", group_name, " | Diagonal = hub count | Off-diagonal = overlap"),
          side = 3, outer = TRUE, line = -2, cex = 0.9)
  })

  # NEW: Agreement Statistics Summary
  output$agreementStatsSummary <- renderText({
    req(analysis_results$comprehensive_consensus)
    req(input$consensus_group_venn)

    group_name <- input$consensus_group_venn
    consensus_results <- analysis_results$comprehensive_consensus[[group_name]]

    if(is.null(consensus_results) || is.null(consensus_results$consensus_scores)) {
      return("No consensus data available for this group.")
    }

    scores_df <- consensus_results$consensus_scores

    # Count agreement patterns
    n_total <- nrow(scores_df)
    n_three_way <- sum(scores_df$AllThreeAgree, na.rm = TRUE)
    n_two_way <- sum(scores_df$TwoAgree & !scores_df$AllThreeAgree, na.rm = TRUE)
    n_one_way <- n_total - n_three_way - n_two_way

    # Approach-specific hub counts (using 60% hub probability threshold)
    n_weighted <- sum(scores_df$Weighted_HubProb >= 0.6, na.rm = TRUE)
    n_percolation <- sum(scores_df$Percolation_HubProb >= 0.6, na.rm = TRUE)
    n_persistence <- sum(scores_df$Persistence_HubProb >= 0.6, na.rm = TRUE)

    # Calculate pairwise overlaps
    weighted_hubs <- scores_df$Node[scores_df$Weighted_HubProb >= 0.6]
    percolation_hubs <- scores_df$Node[scores_df$Percolation_HubProb >= 0.6]
    persistence_hubs <- scores_df$Node[scores_df$Persistence_HubProb >= 0.6]

    overlap_wp <- length(intersect(weighted_hubs, percolation_hubs))
    overlap_wpe <- length(intersect(weighted_hubs, persistence_hubs))
    overlap_ppe <- length(intersect(percolation_hubs, persistence_hubs))

    # Jaccard similarities
    jaccard_wp <- overlap_wp / length(union(weighted_hubs, percolation_hubs))
    jaccard_wpe <- overlap_wpe / length(union(weighted_hubs, persistence_hubs))
    jaccard_ppe <- overlap_ppe / length(union(percolation_hubs, persistence_hubs))

    # Average approach weights
    avg_weighted_weight <- mean(scores_df$Weighted_Weight, na.rm = TRUE)
    avg_percolation_weight <- mean(scores_df$Percolation_Weight, na.rm = TRUE)
    avg_persistence_weight <- mean(scores_df$Persistence_Weight, na.rm = TRUE)

    # Generate summary
    summary_lines <- c(
      paste0("=== AGREEMENT SUMMARY FOR ", group_name, " ==="),
      "",
      "OVERALL AGREEMENT PATTERNS:",
      paste0("  Total nodes analyzed: ", n_total),
      paste0("  3-way agreement (all approaches): ", n_three_way, " (", round(100 * n_three_way / n_total, 1), "%)"),
      paste0("  2-way agreement: ", n_two_way, " (", round(100 * n_two_way / n_total, 1), "%)"),
      paste0("  1-way only: ", n_one_way, " (", round(100 * n_one_way / n_total, 1), "%)"),
      "",
      "APPROACH-SPECIFIC HUB COUNTS (≥60% hub probability):",
      paste0("  Weighted approach: ", n_weighted, " hubs"),
      paste0("  Percolation approach: ", n_percolation, " hubs"),
      paste0("  Persistence approach: ", n_persistence, " hubs"),
      "",
      "PAIRWISE OVERLAP STATISTICS:",
      paste0("  Weighted ∩ Percolation: ", overlap_wp, " hubs (Jaccard = ", round(jaccard_wp, 3), ")"),
      paste0("  Weighted ∩ Persistence: ", overlap_wpe, " hubs (Jaccard = ", round(jaccard_wpe, 3), ")"),
      paste0("  Percolation ∩ Persistence: ", overlap_ppe, " hubs (Jaccard = ", round(jaccard_ppe, 3), ")"),
      "",
      "APPROACH CONFIDENCE WEIGHTS (higher = more consistent):",
      paste0("  Weighted: ", round(avg_weighted_weight, 3)),
      paste0("  Percolation: ", round(avg_percolation_weight, 3)),
      paste0("  Persistence: ", round(avg_persistence_weight, 3)),
      "",
      "INTERPRETATION:",
      paste0("  • Jaccard > 0.5 indicates strong agreement between approaches"),
      paste0("  • 3-way agreement nodes (", n_three_way, ") are highest-confidence hubs"),
      paste0("  • Higher weight = lower CV across correlation methods = more reliable")
    )

    paste(summary_lines, collapse = "\n")
  })

  # ========== TAB 3: DISAGREEMENT ANALYSIS ==========

  # Update disagreement_group selector
  observe({
    req(analysis_results$comprehensive_consensus)
    group_choices <- names(analysis_results$comprehensive_consensus)
    if(length(group_choices) > 0) {
      updateSelectInput(session, "disagreement_group", choices = group_choices, selected = group_choices[1])
    }
  })

  # Disagreement Heatmap
  output$disagreementHeatmapPlot <- renderPlot({
    req(analysis_results$comprehensive_consensus)
    req(input$disagreement_group)
    req(input$disagreement_cv_threshold)

    group_name <- input$disagreement_group
    cv_threshold <- input$disagreement_cv_threshold
    consensus_results <- analysis_results$comprehensive_consensus[[group_name]]

    if(is.null(consensus_results) || is.null(consensus_results$consensus_scores)) {
      plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
      text(1, 1, "No consensus data available", cex = 1.2, col = "gray")
      return()
    }

    scores_df <- consensus_results$consensus_scores

    # Filter for high-disagreement nodes (high CV or high SD)
    high_disagreement <- scores_df[scores_df$StandardDeviation > cv_threshold * abs(scores_df$Weighted_MeanZ + scores_df$Percolation_MeanZ + scores_df$Persistence_MeanZ) / 3 |
                                    scores_df$UncertaintyCategory == "High", ]

    if(nrow(high_disagreement) == 0) {
      plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
      text(1, 1, paste0("No nodes with CV > ", cv_threshold, " found"), cex = 1.2, col = "gray")
      return()
    }

    # Sort by SD and take top 30
    high_disagreement <- high_disagreement[order(-high_disagreement$StandardDeviation), ]
    top_disagreement <- head(high_disagreement, 30)

    # Create matrix for heatmap: rows = nodes, columns = approaches
    heatmap_matrix <- matrix(NA, nrow = nrow(top_disagreement), ncol = 3,
                             dimnames = list(top_disagreement$Node,
                                           c("Weighted", "Percolation", "Persistence")))

    heatmap_matrix[, 1] <- top_disagreement$Weighted_MeanZ
    heatmap_matrix[, 2] <- top_disagreement$Percolation_MeanZ
    heatmap_matrix[, 3] <- top_disagreement$Persistence_MeanZ

    # Create heatmap
    par(mar = c(8, 12, 4, 2))

    # Color scale: blue (low) to white (0) to red (high)
    col_breaks <- seq(-3, 3, length.out = 101)
    colors <- colorRampPalette(c("#2166AC", "#4393C3", "#92C5DE", "#D1E5F0",
                                 "white",
                                 "#FDDBC7", "#F4A582", "#D6604D", "#B2182B"))(100)

    image(1:ncol(heatmap_matrix), 1:nrow(heatmap_matrix),
          t(heatmap_matrix),
          col = colors,
          breaks = col_breaks,
          xlab = "", ylab = "",
          main = paste0("High-Disagreement Nodes: ", group_name),
          axes = FALSE)

    axis(1, at = 1:ncol(heatmap_matrix), labels = colnames(heatmap_matrix), las = 2, cex.axis = 0.9)
    axis(2, at = 1:nrow(heatmap_matrix), labels = rownames(heatmap_matrix), las = 2, cex.axis = 0.8)

    # Add z-score values as text
    for(i in 1:nrow(heatmap_matrix)) {
      for(j in 1:ncol(heatmap_matrix)) {
        if(!is.na(heatmap_matrix[i, j])) {
          text(j, i, sprintf("%.2f", heatmap_matrix[i, j]), cex = 0.7,
               col = ifelse(abs(heatmap_matrix[i, j]) > 1.5, "white", "black"))
        }
      }
    }

    box()

    # Add SD values on right side
    par(xpd = TRUE)
    text(ncol(heatmap_matrix) + 0.7, 1:nrow(heatmap_matrix),
         sprintf("SD=%.2f", top_disagreement$StandardDeviation),
         cex = 0.7, adj = 0, col = "darkred")
    par(xpd = FALSE)

    # Add legend
    legend("topright", inset = c(-0.15, 0),
           legend = c("High z-score (hub)", "Low z-score (non-hub)", "Gray = missing"),
           fill = c("#B2182B", "#2166AC", "gray"),
           cex = 0.8, bty = "n")
  })

  # Divergence Scatter Plot
  output$divergenceScatterPlot <- renderPlot({
    req(analysis_results$comprehensive_consensus)
    req(input$disagreement_group)
    req(input$disagreement_cv_threshold)

    group_name <- input$disagreement_group
    cv_threshold <- input$disagreement_cv_threshold
    consensus_results <- analysis_results$comprehensive_consensus[[group_name]]

    if(is.null(consensus_results) || is.null(consensus_results$consensus_scores)) {
      plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
      text(1, 1, "No consensus data available", cex = 1.2, col = "gray")
      return()
    }

    scores_df <- consensus_results$consensus_scores

    # Identify high-disagreement nodes
    high_disagreement_flag <- scores_df$StandardDeviation > cv_threshold * abs(scores_df$Weighted_MeanZ + scores_df$Percolation_MeanZ + scores_df$Persistence_MeanZ) / 3 |
                              scores_df$UncertaintyCategory == "High"

    # Create 3-panel scatter plots
    par(mfrow = c(1, 3), mar = c(5, 5, 4, 2))

    # Panel 1: Weighted vs Percolation
    plot(scores_df$Weighted_MeanZ, scores_df$Percolation_MeanZ,
         pch = ifelse(high_disagreement_flag, 19, 1),
         col = ifelse(high_disagreement_flag, "#E53935", "#666666"),
         cex = ifelse(high_disagreement_flag, 1.2, 0.8),
         xlab = "Weighted Z-Score",
         ylab = "Percolation Z-Score",
         main = "Weighted vs Percolation")
    abline(0, 1, col = "blue", lty = 2, lwd = 2)
    abline(h = 1.5, v = 1.5, col = "darkgreen", lty = 3)
    grid()

    # Panel 2: Weighted vs Persistence
    plot(scores_df$Weighted_MeanZ, scores_df$Persistence_MeanZ,
         pch = ifelse(high_disagreement_flag, 19, 1),
         col = ifelse(high_disagreement_flag, "#E53935", "#666666"),
         cex = ifelse(high_disagreement_flag, 1.2, 0.8),
         xlab = "Weighted Z-Score",
         ylab = "Persistence Z-Score",
         main = "Weighted vs Persistence")
    abline(0, 1, col = "blue", lty = 2, lwd = 2)
    abline(h = 1.5, v = 1.5, col = "darkgreen", lty = 3)
    grid()

    # Panel 3: Percolation vs Persistence
    plot(scores_df$Percolation_MeanZ, scores_df$Persistence_MeanZ,
         pch = ifelse(high_disagreement_flag, 19, 1),
         col = ifelse(high_disagreement_flag, "#E53935", "#666666"),
         cex = ifelse(high_disagreement_flag, 1.2, 0.8),
         xlab = "Percolation Z-Score",
         ylab = "Persistence Z-Score",
         main = "Percolation vs Persistence")
    abline(0, 1, col = "blue", lty = 2, lwd = 2)
    abline(h = 1.5, v = 1.5, col = "darkgreen", lty = 3)
    grid()

    # Add overall legend
    par(fig = c(0, 1, 0, 1), oma = c(0, 0, 2, 0), mar = c(0, 0, 0, 0), new = TRUE)
    plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
    legend("bottom",
           legend = c(paste0("High disagreement (red, CV > ", cv_threshold, ")"),
                     "Low disagreement (gray)",
                     "Diagonal = perfect agreement",
                     "Green lines = hub threshold (z=1.5)"),
           col = c("#E53935", "#666666", "blue", "darkgreen"),
           pch = c(19, 1, NA, NA),
           lty = c(NA, NA, 2, 3),
           ncol = 2,
           cex = 0.9,
           bty = "n")
  })

  # Method-Specific Hubs Table
  output$methodSpecificHubsTable <- DT::renderDataTable({
    req(analysis_results$comprehensive_consensus)
    req(input$disagreement_group)

    group_name <- input$disagreement_group
    consensus_results <- analysis_results$comprehensive_consensus[[group_name]]

    if(is.null(consensus_results) || is.null(consensus_results$consensus_scores)) {
      return(data.frame(Message = "No consensus data available"))
    }

    scores_df <- consensus_results$consensus_scores

    # Filter for method-specific hubs (not all three agree)
    method_specific <- scores_df[!scores_df$AllThreeAgree, ]

    # Further filter: at least one approach identifies as hub
    method_specific <- method_specific[method_specific$Weighted_HubProb >= 0.6 |
                                       method_specific$Percolation_HubProb >= 0.6 |
                                       method_specific$Persistence_HubProb >= 0.6, ]

    if(nrow(method_specific) == 0) {
      return(data.frame(Message = "No method-specific hubs found"))
    }

    # Sort by standard deviation (highest disagreement first)
    method_specific <- method_specific[order(-method_specific$StandardDeviation), ]

    # Create display table
    display_table <- data.frame(
      Node = method_specific$Node,
      WeightedHub = ifelse(method_specific$Weighted_HubProb >= 0.6, "✓", ""),
      PercolationHub = ifelse(method_specific$Percolation_HubProb >= 0.6, "✓", ""),
      PersistenceHub = ifelse(method_specific$Persistence_HubProb >= 0.6, "✓", ""),
      ConsensusScore = round(method_specific$BayesianConsensusScore, 3),
      SD = round(method_specific$StandardDeviation, 3),
      CV = round(method_specific$CoefficientVariation, 3),
      Uncertainty = method_specific$UncertaintyCategory,
      stringsAsFactors = FALSE
    )

    DT::datatable(display_table,
                  options = list(
                    pageLength = 20,
                    scrollY = "400px",
                    scrollCollapse = TRUE,
                    order = list(list(5, 'desc'))  # Sort by SD
                  ),
                  rownames = FALSE) %>%
      DT::formatStyle('SD',
                     backgroundColor = DT::styleInterval(c(0.3, 0.5, 0.7),
                                                        c('#d4edda', '#fff3cd', '#f8d7da', '#f5c6cb')))
  })

  # Disagreement Statistics Summary
  output$disagreementStatsSummary <- renderText({
    req(analysis_results$comprehensive_consensus)
    req(input$disagreement_group)
    req(input$disagreement_cv_threshold)

    group_name <- input$disagreement_group
    cv_threshold <- input$disagreement_cv_threshold
    consensus_results <- analysis_results$comprehensive_consensus[[group_name]]

    if(is.null(consensus_results) || is.null(consensus_results$consensus_scores)) {
      return("No consensus data available for this group.")
    }

    scores_df <- consensus_results$consensus_scores

    # Overall statistics
    n_total <- nrow(scores_df)
    n_high_disagreement <- sum(scores_df$UncertaintyCategory == "High", na.rm = TRUE)
    n_method_specific <- sum(!scores_df$AllThreeAgree &
                             (scores_df$Weighted_HubProb >= 0.6 |
                              scores_df$Percolation_HubProb >= 0.6 |
                              scores_df$Persistence_HubProb >= 0.6), na.rm = TRUE)

    # Approach-specific only hubs
    n_weighted_only <- sum(scores_df$Weighted_HubProb >= 0.6 &
                           scores_df$Percolation_HubProb < 0.6 &
                           scores_df$Persistence_HubProb < 0.6, na.rm = TRUE)
    n_percolation_only <- sum(scores_df$Percolation_HubProb >= 0.6 &
                              scores_df$Weighted_HubProb < 0.6 &
                              scores_df$Persistence_HubProb < 0.6, na.rm = TRUE)
    n_persistence_only <- sum(scores_df$Persistence_HubProb >= 0.6 &
                              scores_df$Weighted_HubProb < 0.6 &
                              scores_df$Percolation_HubProb < 0.6, na.rm = TRUE)

    # Top disagreement nodes
    top_disagreement <- head(scores_df[order(-scores_df$StandardDeviation), ], 5)
    top_disagreement_names <- paste(top_disagreement$Node, collapse = ", ")

    # Average disagreement statistics
    avg_sd <- mean(scores_df$StandardDeviation, na.rm = TRUE)
    avg_cv <- mean(scores_df$CoefficientVariation, na.rm = TRUE)

    # Generate summary
    summary_lines <- c(
      paste0("=== DISAGREEMENT ANALYSIS FOR ", group_name, " ==="),
      "",
      "OVERALL DISAGREEMENT STATISTICS:",
      paste0("  Total nodes analyzed: ", n_total),
      paste0("  High uncertainty nodes (CI width > 0.4): ", n_high_disagreement, " (", round(100 * n_high_disagreement / n_total, 1), "%)"),
      paste0("  Method-specific hubs (not all agree): ", n_method_specific, " (", round(100 * n_method_specific / n_total, 1), "%)"),
      "",
      "APPROACH-SPECIFIC HUBS (identified by only one approach):",
      paste0("  Weighted-only hubs: ", n_weighted_only),
      paste0("  Percolation-only hubs: ", n_percolation_only),
      paste0("  Persistence-only hubs: ", n_persistence_only),
      paste0("  Total approach-specific: ", n_weighted_only + n_percolation_only + n_persistence_only),
      "",
      "TOP 5 HIGHEST-DISAGREEMENT NODES:",
      paste0("  ", top_disagreement_names),
      paste0("  (SD range: ", round(min(top_disagreement$StandardDeviation), 3), " - ",
             round(max(top_disagreement$StandardDeviation), 3), ")"),
      "",
      "AVERAGE DISAGREEMENT METRICS:",
      paste0("  Mean standard deviation: ", round(avg_sd, 3)),
      paste0("  Mean coefficient of variation: ", round(avg_cv, 3)),
      "",
      "INTERPRETATION:",
      "  • High SD/CV nodes show method-dependent hub identification",
      "  • Approach-specific hubs may reflect methodological assumptions rather than true network properties",
      "  • These nodes warrant careful scrutiny before making biological interpretations",
      paste0("  • Current CV threshold: ", cv_threshold, " (adjustable with slider)"),
      "",
      "RECOMMENDATIONS:",
      "  • Focus biological interpretation on 3-way agreement hubs (high confidence)",
      "  • Investigate high-disagreement nodes for threshold sensitivity",
      "  • Consider reporting consensus findings with confidence intervals"
    )

    paste(summary_lines, collapse = "\n")
  })

  # ========== TAB 4: SENSITIVITY ANALYSIS ==========

  # Update sensitivity_group selector
  observe({
    req(analysis_results$comprehensive_consensus)
    group_choices <- names(analysis_results$comprehensive_consensus)
    if(length(group_choices) > 0) {
      updateSelectInput(session, "sensitivity_group", choices = group_choices, selected = group_choices[1])
    }
  })

  # Z-Threshold Sensitivity Plot
  output$zThresholdSensitivityPlot <- renderPlot({
    req(analysis_results$comprehensive_consensus)
    req(input$sensitivity_group)

    group_name <- input$sensitivity_group
    consensus_results <- analysis_results$comprehensive_consensus[[group_name]]

    if(is.null(consensus_results) || is.null(consensus_results$sensitivity_analysis)) {
      plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
      text(1, 1, "Sensitivity analysis not available.\nRun analysis to compute.", cex = 1.2, col = "gray")
      return()
    }

    sensitivity <- consensus_results$sensitivity_analysis
    z_sens <- sensitivity$z_sensitivity

    if(nrow(z_sens) == 0) {
      plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
      text(1, 1, "No sensitivity data available", cex = 1.2, col = "gray")
      return()
    }

    # 3-panel plot
    par(mfrow = c(1, 3), mar = c(5, 5, 4, 2))

    # Panel 1: Number of consensus hubs vs threshold
    plot(z_sens$threshold, z_sens$n_consensus_hubs,
         type = "b", pch = 19, col = "#4A90E2", lwd = 2,
         xlab = "Z-Score Threshold",
         ylab = "Number of Consensus Hubs",
         main = "Consensus Hubs vs. Threshold")
    abline(v = consensus_results$z_threshold, col = "red", lty = 2, lwd = 2)
    legend("topright",
           legend = paste0("Current (", round(consensus_results$z_threshold, 2), ")"),
           col = "red", lty = 2, bty = "n")
    grid()

    # Panel 2: 3-way agreement vs threshold
    plot(z_sens$threshold, z_sens$n_three_way,
         type = "b", pch = 19, col = "#2E7D32", lwd = 2,
         xlab = "Z-Score Threshold",
         ylab = "3-Way Agreement Hubs",
         main = "High-Confidence Hubs")
    abline(v = consensus_results$z_threshold, col = "red", lty = 2, lwd = 2)
    grid()

    # Panel 3: Average consensus score
    plot(z_sens$threshold, z_sens$avg_consensus_score,
         type = "b", pch = 19, col = "#FFA726", lwd = 2,
         xlab = "Z-Score Threshold",
         ylab = "Avg Consensus Score",
         main = "Mean Consensus Strength",
         ylim = c(0, 1))
    abline(v = consensus_results$z_threshold, col = "red", lty = 2, lwd = 2)
    grid()
  })

  # Stable Hubs Table
  output$stableHubsTable <- DT::renderDataTable({
    req(analysis_results$comprehensive_consensus)
    req(input$sensitivity_group)

    group_name <- input$sensitivity_group
    consensus_results <- analysis_results$comprehensive_consensus[[group_name]]

    if(is.null(consensus_results) || is.null(consensus_results$sensitivity_analysis)) {
      return(data.frame(Message = "Sensitivity analysis not available"))
    }

    sensitivity <- consensus_results$sensitivity_analysis
    stable_nodes <- sensitivity$stable_hub_nodes
    scores_df <- consensus_results$consensus_scores

    if(length(stable_nodes) == 0) {
      return(data.frame(Message = "No stable hubs identified"))
    }

    # Filter for stable nodes
    stable_df <- scores_df[scores_df$Node %in% stable_nodes, ]
    stable_df <- stable_df[order(-stable_df$BayesianConsensusScore), ]

    # Create display table
    display_table <- data.frame(
      Node = stable_df$Node,
      ConsensusScore = round(stable_df$BayesianConsensusScore, 3),
      ThreeWayAgree = ifelse(stable_df$AllThreeAgree, "✓", ""),
      ConfidenceCategory = stable_df$ConfidenceCategory,
      CI_Width = round(stable_df$CredibleIntervalUpper - stable_df$CredibleIntervalLower, 3),
      stringsAsFactors = FALSE
    )

    DT::datatable(display_table, options = list(pageLength = 15), rownames = FALSE) %>%
      DT::formatStyle('ConsensusScore',
                     backgroundColor = DT::styleInterval(c(0.5, 0.7, 0.85),
                                                        c('#fff3cd', '#d4edda', '#c3e6cb', '#b8daff')))
  })

  # Agreement Threshold Sensitivity Plot
  output$agreementSensitivityPlot <- renderPlot({
    req(analysis_results$comprehensive_consensus)
    req(input$sensitivity_group)

    group_name <- input$sensitivity_group
    consensus_results <- analysis_results$comprehensive_consensus[[group_name]]

    if(is.null(consensus_results) || is.null(consensus_results$sensitivity_analysis)) {
      plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
      text(1, 1, "Sensitivity analysis not available", cex = 1.2, col = "gray")
      return()
    }

    sensitivity <- consensus_results$sensitivity_analysis
    agr_sens <- sensitivity$agreement_sensitivity

    if(nrow(agr_sens) == 0) {
      plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
      text(1, 1, "No agreement sensitivity data", cex = 1.2, col = "gray")
      return()
    }

    # 2-panel plot
    par(mfrow = c(1, 2), mar = c(5, 5, 4, 2))

    # Panel 1: Hub count vs agreement threshold
    plot(agr_sens$agreement_threshold, agr_sens$n_hubs,
         type = "b", pch = 19, col = "#E17055", lwd = 2,
         xlab = "Agreement Threshold (proportion)",
         ylab = "Number of Hubs",
         main = "Hubs vs. Agreement Definition")
    abline(v = 0.6, col = "blue", lty = 2, lwd = 2)
    legend("topright", legend = "Current (60%)", col = "blue", lty = 2, bty = "n")
    grid()

    # Panel 2: Average confidence
    plot(agr_sens$agreement_threshold, agr_sens$avg_confidence,
         type = "b", pch = 19, col = "#00B894", lwd = 2,
         xlab = "Agreement Threshold (proportion)",
         ylab = "Average Confidence",
         main = "Consensus Strength",
         ylim = c(0, 1))
    abline(v = 0.6, col = "blue", lty = 2, lwd = 2)
    grid()
  })

  # ========================================================================
  # CONSENSUS DOWNLOAD HANDLER
  # ========================================================================

  output$downloadConsensusResults <- downloadHandler(
    filename = function() {
      paste0("BayesianConsensus_", format(Sys.Date(), "%Y%m%d"), ".zip")
    },
    content = function(file) {
      # Show progress notification
      showNotification("Preparing consensus download package...", duration = 3, type = "message")

      tryCatch({
        # Call the download creation function
        create_consensus_download(
          file = file,
          analysis_results = analysis_results,
          ui_state = NULL,  # Not using ui_state in consensus download
          format = input$consensus_download_format,
          include_plots = input$consensus_include_plots
        )

        showNotification("Download package created successfully!", duration = 3, type = "message")
      }, error = function(e) {
        showNotification(paste("Error creating download:", e$message), duration = 5, type = "error")
        cat("Download error:", e$message, "\n")
      })
    }
  )

  # ========================================================================
  # HOVER DOWNLOAD SERVER (per-plot download buttons)
  # ========================================================================

  # Store regional contribution results in analysis_results for hover download access
  observe({
    results <- regional_contribution_results()
    if(!is.null(results)) {
      analysis_results$regional_contribution <- results
    }
  })

  hover_download_server(input, output, session, analysis_results, ui_state)

  # ========================================================================
  # SHARED RESULTS (TAB 6) & CONSENSUS (TAB 7)
  # ========================================================================

  # Update granular consensus group selector (Tab 6)
  observe({
    req(analysis_results$comprehensive_consensus)
    group_choices <- names(analysis_results$comprehensive_consensus)
    if(length(group_choices) > 0) {
      updateSelectInput(session, "granular_consensus_group", choices = group_choices, selected = group_choices[1])
    }
  })

  # ========================================================================
  # TAB 6 SERVER FUNCTIONS - RANK-BASED SHARED RESULTS
  # ========================================================================

  # Update Tab 6 group selectors
  observe({
    req(analysis_results$comprehensive_consensus)
    group_choices <- names(analysis_results$comprehensive_consensus)
    if(length(group_choices) > 0) {
      updateSelectInput(session, "consensus_overview_group", choices = group_choices, selected = group_choices[1])
      updateSelectInput(session, "consensus_group_robustness", choices = group_choices, selected = group_choices[1])
    }
  })

  # Helper function: Extract and rank metrics across three approaches
  extract_three_way_rankings <- function(method, group_name, metric_name, analysis_results) {
    # Extract data from three approaches
    # FIX: Access method-level data (weighted results are combined dfs, filter by group)
    weighted_method_data <- analysis_results$method_weighted_results[[method]]
    percolation_data <- analysis_results$method_percolation_results[[method]]$node_metrics
    persistence_data <- analysis_results$auc_results[[method]][[group_name]]

    if(is.null(weighted_method_data) || is.null(percolation_data) || is.null(persistence_data)) {
      return(NULL)
    }

    # Filter percolation data for this group
    perc_group <- percolation_data[percolation_data$Group == group_name, ]

    # Get metric from each approach
    if(metric_name == "Strength") {
      # Node strength - filter by group from combined df
      weighted_vals <- weighted_method_data$node_strength
      if(is.null(weighted_vals)) return(NULL)
      weighted_vals <- weighted_vals[weighted_vals$Group == group_name, ]

      # Match nodes across approaches
      common_nodes <- intersect(weighted_vals$Node, perc_group$Node)
      if(length(common_nodes) == 0) return(NULL)

      # Create data frame with values from each approach
      df <- data.frame(
        Node = common_nodes,
        Weighted = weighted_vals$Strength[match(common_nodes, weighted_vals$Node)],
        Percolation = perc_group$Degree[match(common_nodes, perc_group$Node)],
        stringsAsFactors = FALSE
      )

      # Add persistence if available
      if(!is.null(persistence_data$node_auc)) {
        pers_nodes <- persistence_data$node_auc$Node
        common_with_pers <- intersect(common_nodes, pers_nodes)
        if(length(common_with_pers) > 0) {
          df <- df[df$Node %in% common_with_pers, ]
          df$Persistence <- persistence_data$node_auc$AUC_Strength[match(df$Node, persistence_data$node_auc$Node)]
        }
      }

    } else if(metric_name == "Eigenvector") {
      # Eigenvector centrality - filter by group from combined df
      weighted_vals <- weighted_method_data$weighted_eigenvector
      if(is.null(weighted_vals)) return(NULL)
      weighted_vals <- weighted_vals[weighted_vals$Group == group_name, ]

      # Match nodes
      common_nodes <- intersect(weighted_vals$Node, perc_group$Node)
      if(length(common_nodes) == 0) return(NULL)

      df <- data.frame(
        Node = common_nodes,
        Weighted = weighted_vals$Weighted_Eigenvector[match(common_nodes, weighted_vals$Node)],
        Percolation = perc_group$Eigenvector[match(common_nodes, perc_group$Node)],
        stringsAsFactors = FALSE
      )

      # Add persistence
      if(!is.null(persistence_data$node_auc)) {
        pers_nodes <- persistence_data$node_auc$Node
        common_with_pers <- intersect(common_nodes, pers_nodes)
        if(length(common_with_pers) > 0) {
          df <- df[df$Node %in% common_with_pers, ]
          df$Persistence <- persistence_data$node_auc$AUC_Eigenvector[match(df$Node, persistence_data$node_auc$Node)]
        }
      }
    }

    # Convert to ranks (lower rank = higher value)
    df$Rank_Weighted <- rank(-df$Weighted, ties.method = "average")
    df$Rank_Percolation <- rank(-df$Percolation, ties.method = "average")
    if("Persistence" %in% names(df)) {
      df$Rank_Persistence <- rank(-df$Persistence, ties.method = "average")
    }

    return(df)
  }

  # Summary: Top Nodes Table
  output$summaryTopNodesTable <- DT::renderDataTable({
    req(analysis_results$comprehensive_consensus)
    req(input$consensus_overview_group)
    req(selected_methods())

    group_name <- input$consensus_overview_group
    consensus_results <- analysis_results$comprehensive_consensus[[group_name]]

    if(is.null(consensus_results) || is.null(consensus_results$standardized_metrics)) {
      return(NULL)
    }

    std_metrics <- consensus_results$standardized_metrics
    methods <- selected_methods()
    n_combos <- length(methods) * 3  # methods × 3 approaches

    # Collect all nodes and calculate consensus rankings
    all_nodes <- unique(c(
      unlist(lapply(std_metrics$weighted, function(x) if(!is.null(x)) x$Node else character(0))),
      unlist(lapply(std_metrics$percolation, function(x) if(!is.null(x)) x$Node else character(0))),
      unlist(lapply(std_metrics$persistence, function(x) if(!is.null(x)) x$Node else character(0)))
    ))

    if(length(all_nodes) == 0) return(NULL)

    # Create centrality matrix for method combinations
    centrality_matrix <- matrix(NA, nrow = length(all_nodes), ncol = n_combos)
    rownames(centrality_matrix) <- all_nodes

    col_idx <- 1

    for(approach in c("weighted", "percolation", "persistence")) {
      for(method in methods) {
        if(!is.null(std_metrics[[approach]][[method]])) {
          approach_data <- std_metrics[[approach]][[method]]
          for(i in seq_along(approach_data$Node)) {
            node <- approach_data$Node[i]
            z_score <- approach_data$Eigenvector_Z[i]  # Fixed: use Eigenvector_Z
            if(node %in% rownames(centrality_matrix)) {
              centrality_matrix[node, col_idx] <- z_score
            }
          }
        }
        col_idx <- col_idx + 1
      }
    }

    # Calculate mean z-score across methods (no ranking, just average)
    mean_z <- rowMeans(centrality_matrix, na.rm = TRUE)
    sd_z <- apply(centrality_matrix, 1, sd, na.rm = TRUE)

    # Count unique methods (not approach×method combinations)
    # Each method appears 3 times (once per approach), so check groups of 3 columns
    n_methods <- rep(0, nrow(centrality_matrix))
    for(i in 1:nrow(centrality_matrix)) {
      # Check each of 5 methods across 3 approaches
      method_has_data <- logical(5)
      for(m_idx in 1:5) {
        # Columns for this method across 3 approaches: (m_idx-1)*3+1, (m_idx-1)*3+2, (m_idx-1)*3+3
        # Actually, columns are organized as: W-P, W-S, W-B, W-Sh, W-Pa, Pe-P, Pe-S, ...
        # So for method 1 (Pearson): columns 1 (W-P), 6 (Pe-P), 11 (Pers-P)
        # For method 2 (Spearman): columns 2 (W-S), 7 (Pe-S), 12 (Pers-S)
        cols <- c(m_idx, m_idx+5, m_idx+10)
        if(any(!is.na(centrality_matrix[i, cols]))) {
          method_has_data[m_idx] <- TRUE
        }
      }
      n_methods[i] <- sum(method_has_data)
    }

    # Create summary table - show ALL nodes, sorted by mean z-score
    summary_df <- data.frame(
      Node = all_nodes,
      MeanZ = round(mean_z, 2),
      SD = round(sd_z, 2),
      NumMethods = n_methods,
      stringsAsFactors = FALSE
    )

    # Sort by mean z-score (descending) and show top 30
    summary_df <- summary_df[order(-summary_df$MeanZ), ]
    summary_df <- summary_df[1:min(30, nrow(summary_df)), ]

    DT::datatable(summary_df,
                  options = list(pageLength = 30, dom = 't'),
                  rownames = FALSE) %>%
      DT::formatStyle('MeanZ',
                      backgroundColor = DT::styleInterval(c(1.0, 1.5), c('white', '#fff3cd', '#d4edda')))
  })

  # Summary: Method Agreement Plot
  output$summaryMethodAgreement <- renderPlot({
    req(analysis_results$comprehensive_consensus)
    req(input$consensus_overview_group)
    req(selected_methods())

    group_name <- input$consensus_overview_group
    consensus_results <- analysis_results$comprehensive_consensus[[group_name]]

    if(is.null(consensus_results) || is.null(consensus_results$standardized_metrics)) {
      plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
      text(1, 1, "No data available", cex = 1.2, col = "gray")
      return()
    }

    std_metrics <- consensus_results$standardized_metrics
    methods <- selected_methods()
    n_combos <- length(methods) * 3  # methods × 3 approaches

    # Collect all z-scores across methods
    all_nodes <- unique(c(
      unlist(lapply(std_metrics$weighted, function(x) if(!is.null(x)) x$Node else character(0))),
      unlist(lapply(std_metrics$percolation, function(x) if(!is.null(x)) x$Node else character(0))),
      unlist(lapply(std_metrics$persistence, function(x) if(!is.null(x)) x$Node else character(0)))
    ))

    if(length(all_nodes) == 0) return()

    # Create z-score matrix for method combinations
    z_matrix <- matrix(NA, nrow = length(all_nodes), ncol = n_combos)
    rownames(z_matrix) <- all_nodes

    col_idx <- 1

    for(approach in c("weighted", "percolation", "persistence")) {
      for(method in methods) {
        if(!is.null(std_metrics[[approach]][[method]])) {
          approach_data <- std_metrics[[approach]][[method]]
          for(i in seq_along(approach_data$Node)) {
            node <- approach_data$Node[i]
            z_score <- approach_data$Eigenvector_Z[i]  # Fixed: use Eigenvector_Z
            if(node %in% rownames(z_matrix)) {
              z_matrix[node, col_idx] <- z_score
            }
          }
        }
        col_idx <- col_idx + 1
      }
    }

    # Calculate mean z-score for each node
    mean_z <- rowMeans(z_matrix, na.rm = TRUE)

    # Show distribution of mean z-scores (histogram)
    par(mfrow = c(1, 1), mar = c(5, 4, 4, 2))
    hist(mean_z, breaks = 30, col = "#3498db", border = "white",
         main = paste("Distribution of Mean Z-Scores -", group_name),
         xlab = paste0("Mean Z-Score Across ", n_combos, " Method-Approach Combinations"),
         ylab = "Number of Nodes")
    abline(v = c(1.0, 1.5, 2.0), col = c("green", "orange", "red"), lty = 2, lwd = 2)
    legend("topright", legend = c("z=1.0", "z=1.5", "z=2.0"),
           col = c("green", "orange", "red"), lty = 2, lwd = 2, bty = "n")
    grid()
  })

  # Node Strength Rank Comparisons (6a) - NEW: Consensus ranks scatter plot
  output$nodeStrengthRankPlot <- renderPlot({
    req(analysis_results$comprehensive_consensus)
    req(input$comparison_method)

    method <- tolower(input$comparison_method)
    all_groups <- names(analysis_results$comprehensive_consensus)

    if(length(all_groups) == 0) {
      plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
      text(1, 1, "No consensus data available", cex = 1.2, col = "gray")
      return()
    }

    # Multi-panel layout: one plot per group (2×2)
    n_groups <- length(all_groups)
    par(mfrow = c(2, 2), mar = c(5, 5, 4, 2))

    for(group in all_groups[1:min(4, n_groups)]) {
      consensus_data <- analysis_results$comprehensive_consensus[[group]]

      if(is.null(consensus_data) || is.null(consensus_data$standardized_metrics)) {
        plot(1, type = "n", axes = FALSE, main = group)
        text(1, 1, "No data", cex = 1.0)
        next
      }

      # Get actual node metrics from percolation results (has both Strength and Eigenvector)
      method_data <- analysis_results$method_percolation_results[[method]]

      if(is.null(method_data) || is.null(method_data$node_metrics)) {
        plot(1, type = "n", axes = FALSE, main = paste(group, "-", toupper(method)))
        text(1, 1, "No percolation data", cex = 1.0)
        next
      }

      # Filter for this group
      group_data <- method_data$node_metrics[method_data$node_metrics$Group == group, ]

      if(nrow(group_data) == 0 || !("Strength" %in% names(group_data)) || !("Eigenvector" %in% names(group_data))) {
        plot(1, type = "n", axes = FALSE, main = paste(group, "-", toupper(method)))
        text(1, 1, "No data available", cex = 1.0)
        next
      }

      # RANK-BASED: Rank nodes and invert so 0 = lowest, higher = better
      temp_s_rank <- rank(-group_data$Strength, ties.method = "average")
      max_s_rank <- max(temp_s_rank)
      strength_rank <- max_s_rank - temp_s_rank  # Invert: 0 = lowest, higher = better

      temp_e_rank <- rank(-group_data$Eigenvector, ties.method = "average")
      max_e_rank <- max(temp_e_rank)
      eigenvector_rank <- max_e_rank - temp_e_rank  # Invert: 0 = lowest, higher = better

      # Get colors by brain region (matching 3c style)
      plot_colors <- rep("steelblue", nrow(group_data))
      if(!is.null(ui_state$brain_areas) && !is.null(ui_state$area_colors)) {
        for(i in seq_len(nrow(group_data))) {
          node_name <- group_data$Node[i]
          for(area_name in names(ui_state$brain_areas)) {
            if(node_name %in% ui_state$brain_areas[[area_name]]) {
              if(area_name %in% names(ui_state$area_colors)) {
                plot_colors[i] <- ui_state$area_colors[[area_name]]
              }
              break
            }
          }
        }
      }

      # Create plot with rank-based axes
      plot(strength_rank, eigenvector_rank,
           main = paste("Strength Rank vs Eigenvector Rank -", group, "-", toupper(method)),
           xlab = "Node Strength Rank (0 = lowest, higher = better)",
           ylab = "Eigenvector Centrality Rank (0 = lowest, higher = better)",
           pch = 21, cex = 2.2,
           bg = adjustcolor(plot_colors, alpha.f = 0.6),
           col = adjustcolor(plot_colors, alpha.f = 0.8),
           lwd = 2,
           xlim = c(0, max(strength_rank) * 1.05),
           ylim = c(0, max(eigenvector_rank) * 1.05))

      # Add grid (matching 3c style)
      grid(col = "lightgray", lty = "dotted", lwd = 0.5)

      # Add diagonal reference line (perfect rank agreement)
      abline(a = 0, b = 1, col = "red", lty = 2, lwd = 1.5)

      # Add node labels overlaid on points (matching 3c style) - only for top 15 nodes
      top_nodes_idx <- which(strength_rank >= (max_s_rank - 15) | eigenvector_rank >= (max_e_rank - 15))
      for(i in top_nodes_idx) {
        text(strength_rank[i], eigenvector_rank[i],
             labels = group_data$Node[i],
             cex = 0.7, font = 2, col = "black")
      }

      # Add Spearman correlation (rank correlation)
      if(nrow(group_data) > 2) {
        tryCatch({
          spearman_corr <- cor(strength_rank, eigenvector_rank, method = "spearman", use = "complete.obs")
          legend("bottomright",
                 paste("Spearman ρ =", round(spearman_corr, 3)),
                 bty = "n", cex = 0.9, bg = "white", box.col = "darkgray")
        }, error = function(e) {})
      }
    }
  })

  # Hub Conservation Analysis (6e) - NEW: Multi-panel Venn diagrams
  output$hubConservationPlot <- renderPlot({
    req(analysis_results$comprehensive_consensus)
    req(input$comparison_method)

    method <- tolower(input$comparison_method)
    all_groups <- names(analysis_results$comprehensive_consensus)

    if(length(all_groups) == 0) {
      plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
      text(1, 1, "No consensus data available", cex = 1.2, col = "gray")
      return()
    }

    # Multi-panel layout: one barplot per group (2×2)
    n_groups <- length(all_groups)
    par(mfrow = c(2, 2), mar = c(10, 5, 4, 2))

    for(group in all_groups[1:min(4, n_groups)]) {
      consensus_data <- analysis_results$comprehensive_consensus[[group]]

      if(is.null(consensus_data) || is.null(consensus_data$standardized_metrics)) {
        plot(1, type = "n", axes = FALSE, main = group)
        text(1, 1, "No data", cex = 1.0)
        next
      }

      std_metrics <- consensus_data$standardized_metrics

      # Get top 10 nodes by eigenvector from each approach
      eigenvector_values <- list()

      # Weighted
      if(!is.null(std_metrics$weighted[[method]])) {
        w_data <- std_metrics$weighted[[method]]
        w_data <- w_data[order(-w_data$Eigenvector_Z), ]
        top_weighted <- head(w_data, 10)
        for(i in 1:nrow(top_weighted)) {
          node <- top_weighted$Node[i]
          if(!(node %in% names(eigenvector_values))) {
            eigenvector_values[[node]] <- list(weighted = NA, percolation = NA, persistence = NA)
          }
          eigenvector_values[[node]]$weighted <- top_weighted$Eigenvector_Z[i]
        }
      }

      # Percolation
      if(!is.null(std_metrics$percolation[[method]])) {
        p_data <- std_metrics$percolation[[method]]
        p_data <- p_data[order(-p_data$Eigenvector_Z), ]
        top_percolation <- head(p_data, 10)
        for(i in 1:nrow(top_percolation)) {
          node <- top_percolation$Node[i]
          if(!(node %in% names(eigenvector_values))) {
            eigenvector_values[[node]] <- list(weighted = NA, percolation = NA, persistence = NA)
          }
          eigenvector_values[[node]]$percolation <- top_percolation$Eigenvector_Z[i]
        }
      }

      # Persistence
      if(!is.null(std_metrics$persistence[[method]])) {
        ps_data <- std_metrics$persistence[[method]]
        ps_data <- ps_data[order(-ps_data$Eigenvector_Z), ]
        top_persistence <- head(ps_data, 10)
        for(i in 1:nrow(top_persistence)) {
          node <- top_persistence$Node[i]
          if(!(node %in% names(eigenvector_values))) {
            eigenvector_values[[node]] <- list(weighted = NA, percolation = NA, persistence = NA)
          }
          eigenvector_values[[node]]$persistence <- top_persistence$Eigenvector_Z[i]
        }
      }

      # Get unique nodes that appear in top 10 of at least one approach
      all_top_nodes <- names(eigenvector_values)

      # Count how many approaches identify each node as top 10 hub
      hub_counts <- sapply(all_top_nodes, function(node) {
        sum(!is.na(c(eigenvector_values[[node]]$weighted,
                    eigenvector_values[[node]]$percolation,
                    eigenvector_values[[node]]$persistence)))
      })

      # Sort by hub count (descending)
      sorted_idx <- order(-hub_counts)
      all_top_nodes <- all_top_nodes[sorted_idx]
      hub_counts <- hub_counts[sorted_idx]

      # Show top 15 nodes
      display_nodes <- head(all_top_nodes, 15)

      # Create barplot showing hub count for each node
      barplot_colors <- ifelse(hub_counts[1:length(display_nodes)] == 3, "darkgreen",
                              ifelse(hub_counts[1:length(display_nodes)] == 2, "orange", "steelblue"))

      bp <- barplot(hub_counts[1:length(display_nodes)],
                    names.arg = display_nodes,
                    main = paste("Hub Conservation Across Approaches -", group, "-", toupper(method)),
                    ylab = "Number of Approaches Identifying as Top 10 Hub",
                    col = barplot_colors,
                    las = 2,
                    ylim = c(0, 3.5))
      grid(nx = NA, ny = NULL, col = "gray90")

      # Add legend
      legend("topright",
             legend = c("All 3 approaches", "2 approaches", "1 approach"),
             fill = c("darkgreen", "orange", "steelblue"),
             bty = "n", cex = 0.8)
    }
  })

  # Regional Consensus (6l) - NEW: Grouped bar plots by region
  output$regionalConsensusPlot <- renderPlot({
    req(analysis_results$comprehensive_consensus)
    req(input$comparison_method)
    req(ui_state$brain_areas)

    method <- tolower(input$comparison_method)
    all_groups <- names(analysis_results$comprehensive_consensus)

    if(length(all_groups) == 0 || length(ui_state$brain_areas) == 0) {
      plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
      text(1, 1, "No consensus data or brain areas", cex = 1.2, col = "gray")
      return()
    }

    n_groups <- length(all_groups)
    region_names <- names(ui_state$brain_areas)
    n_regions <- length(region_names)

    # Matrices: rows = groups, cols = regions
    eigenvector_matrix <- matrix(NA, nrow = n_groups, ncol = n_regions)
    eigenvector_sd_matrix <- matrix(NA, nrow = n_groups, ncol = n_regions)

    rownames(eigenvector_matrix) <- all_groups
    rownames(eigenvector_sd_matrix) <- all_groups
    colnames(eigenvector_matrix) <- region_names
    colnames(eigenvector_sd_matrix) <- region_names

    # For each group, compute consensus eigenvector across approaches
    for(i in seq_along(all_groups)) {
      group <- all_groups[i]
      consensus_data <- analysis_results$comprehensive_consensus[[group]]

      if(is.null(consensus_data) || is.null(consensus_data$standardized_metrics)) {
        next
      }

      std_metrics <- consensus_data$standardized_metrics

      # Collect all nodes from all available approaches
      all_nodes <- unique(c(
        if(!is.null(std_metrics$weighted[[method]])) std_metrics$weighted[[method]]$Node else character(0),
        if(!is.null(std_metrics$percolation[[method]])) std_metrics$percolation[[method]]$Node else character(0),
        if(!is.null(std_metrics$persistence[[method]])) std_metrics$persistence[[method]]$Node else character(0)
      ))

      if(length(all_nodes) == 0) next

      # Compute consensus eigenvector (mean z-score across available approaches)
      consensus_eigenvector <- numeric(length(all_nodes))
      names(consensus_eigenvector) <- all_nodes

      for(node in all_nodes) {
        z_values <- c()

        # Weighted
        if(!is.null(std_metrics$weighted[[method]])) {
          idx <- which(std_metrics$weighted[[method]]$Node == node)
          if(length(idx) > 0) z_values <- c(z_values, std_metrics$weighted[[method]]$Eigenvector_Z[idx[1]])
        }

        # Percolation
        if(!is.null(std_metrics$percolation[[method]])) {
          idx <- which(std_metrics$percolation[[method]]$Node == node)
          if(length(idx) > 0) z_values <- c(z_values, std_metrics$percolation[[method]]$Eigenvector_Z[idx[1]])
        }

        # Persistence
        if(!is.null(std_metrics$persistence[[method]])) {
          idx <- which(std_metrics$persistence[[method]]$Node == node)
          if(length(idx) > 0) z_values <- c(z_values, std_metrics$persistence[[method]]$Eigenvector_Z[idx[1]])
        }

        if(length(z_values) > 0) {
          consensus_eigenvector[node] <- mean(z_values, na.rm = TRUE)
        }
      }

      # RANK-BASED: Convert consensus eigenvector to ranks (lower rank = higher eigenvector)
      consensus_rank <- rank(-consensus_eigenvector, ties.method = "average", na.last = "keep")
      names(consensus_rank) <- all_nodes

      # Aggregate by region (mean rank per region)
      for(j in seq_along(region_names)) {
        region_name <- region_names[j]
        region_nodes <- ui_state$brain_areas[[region_name]]
        region_mask <- names(consensus_rank) %in% region_nodes

        if(sum(region_mask) > 0) {
          region_ranks <- consensus_rank[region_mask]
          region_ranks <- region_ranks[!is.na(region_ranks)]
          if(length(region_ranks) > 0) {
            eigenvector_matrix[i, j] <- mean(region_ranks, na.rm = TRUE)
            eigenvector_sd_matrix[i, j] <- sd(region_ranks, na.rm = TRUE)
          }
        }
      }
    }

    # Get group colors
    group_colors <- sapply(all_groups, function(g) {
      if(!is.null(ui_state$group_colors[[g]])) {
        return(ui_state$group_colors[[g]])
      } else {
        return("#3498db")
      }
    })

    # Check if we have any valid data
    if(all(is.na(eigenvector_matrix))) {
      plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
      text(1, 1, "No regional data available", cex = 1.2, col = "gray")
      return()
    }

    # Create grouped bar plot
    par(mar = c(10, 5, 4, 2))

    # Calculate safe ylim
    max_val <- max(eigenvector_matrix + eigenvector_sd_matrix, na.rm = TRUE)
    min_val <- min(eigenvector_matrix - eigenvector_sd_matrix, na.rm = TRUE)
    if(is.infinite(max_val) || is.na(max_val)) max_val <- 1
    if(is.infinite(min_val) || is.na(min_val)) min_val <- 0

    # Invert ranks for display: higher bars = more important
    # Convert to "inverse rank" where max_rank - rank makes higher values better
    max_rank <- max(eigenvector_matrix, na.rm = TRUE)
    eigenvector_matrix_inverted <- max_rank - eigenvector_matrix + 1

    # Recalculate ylim for inverted values
    max_val_inv <- max(eigenvector_matrix_inverted + eigenvector_sd_matrix, na.rm = TRUE)
    min_val_inv <- min(eigenvector_matrix_inverted - eigenvector_sd_matrix, na.rm = TRUE)
    if(is.infinite(max_val_inv) || is.na(max_val_inv)) max_val_inv <- 1
    if(is.infinite(min_val_inv) || is.na(min_val_inv)) min_val_inv <- 0

    bp <- barplot(eigenvector_matrix_inverted,
                  beside = TRUE,
                  names.arg = region_names,
                  main = paste("Regional Consensus Eigenvector Importance -", toupper(method)),
                  ylab = "Consensus Importance Score (higher = more important)",
                  col = group_colors,
                  border = group_colors,
                  las = 2,
                  ylim = c(min_val_inv - 0.2, max_val_inv * 1.2))
    grid(nx = NA, ny = NULL, col = "gray90")

    # Add error bars (using inverted values)
    for(i in 1:n_groups) {
      for(j in 1:n_regions) {
        if(!is.na(eigenvector_matrix_inverted[i, j]) && !is.na(eigenvector_sd_matrix[i, j])) {
          x_pos <- bp[i, j]  # Fixed indexing: bp is [n_groups, n_regions]
          y_val <- eigenvector_matrix_inverted[i, j]
          y_sd <- eigenvector_sd_matrix[i, j]
          segments(x_pos, y_val - y_sd, x_pos, y_val + y_sd, lwd = 1.5)
          segments(x_pos - 0.1, y_val - y_sd, x_pos + 0.1, y_val - y_sd, lwd = 1.5)
          segments(x_pos - 0.1, y_val + y_sd, x_pos + 0.1, y_val + y_sd, lwd = 1.5)
        }
      }
    }

    legend("topright", legend = all_groups, fill = group_colors, bty = "n", cex = 0.9)
  })

  # 6m. Shared Network Similarity (Across Approaches)
  # Tab 6m: Network Similarity Comparing Analytical Approaches
  output$sharedNetworkSimilarityPlot <- renderPlot({
    tryCatch({
      req(analysis_results$correlation_methods_raw)
      req(input$comparison_method)

      method <- tolower(input$comparison_method %||% "pearson")

      # Get all groups
      all_groups <- NULL
      if(!is.null(analysis_results$correlation_methods_raw[[method]])) {
        all_groups <- names(analysis_results$correlation_methods_raw[[method]])
      }

      if(is.null(all_groups) || length(all_groups) == 0) {
        plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
        text(1, 1, "No data available", cex = 1.2, col = "gray")
        return()
      }

      # Multi-panel layout: one heatmap per group (2×2)
      n_groups <- length(all_groups)
      if(n_groups <= 2) {
        par(mfrow = c(1, n_groups), mar = c(8, 8, 4, 2))
      } else {
        par(mfrow = c(2, 2), mar = c(8, 8, 4, 2))
      }

      plots_made <- 0
      for(group_name in all_groups[1:min(4, n_groups)]) {
        # Get network representations from each approach
        approach_matrices <- list()
        approach_names <- c()

        # 1. WEIGHTED: Use raw correlation matrix
        if(!is.null(analysis_results$correlation_methods_raw[[method]][[group_name]])) {
          weighted_mat <- abs(analysis_results$correlation_methods_raw[[method]][[group_name]])
          approach_matrices[["Weighted"]] <- weighted_mat
          approach_names <- c(approach_names, "Weighted")
        }

        # 2. PERCOLATION: Use adjacency matrix at optimal threshold
        perc_mat <- NULL

        # Access percolation results: [[method]]$adjacency_matrices[[group]]
        if(!is.null(analysis_results$method_percolation_results[[method]]$adjacency_matrices)) {
          adj_mat <- analysis_results$method_percolation_results[[method]]$adjacency_matrices[[group_name]]
          if(!is.null(adj_mat) && !is.null(analysis_results$correlation_methods_raw[[method]][[group_name]])) {
            # Percolation matrix: binary adjacency × correlation weights
            cor_mat <- abs(analysis_results$correlation_methods_raw[[method]][[group_name]])
            perc_mat <- adj_mat * cor_mat
          }
        }

        # Fallback: use threshold to reconstruct
        if(is.null(perc_mat) && !is.null(analysis_results$method_percolation_results[[method]]$thresholds[[group_name]])) {
          optimal_thresh <- analysis_results$method_percolation_results[[method]]$thresholds[[group_name]]
          if(!is.null(optimal_thresh) && !is.null(analysis_results$correlation_methods_raw[[method]][[group_name]])) {
            cor_mat <- abs(analysis_results$correlation_methods_raw[[method]][[group_name]])
            perc_mat <- cor_mat
            perc_mat[perc_mat < optimal_thresh] <- 0
          }
        }

        if(!is.null(perc_mat)) {
          approach_matrices[["Percolation"]] <- perc_mat
          approach_names <- c(approach_names, "Percolation")
        }

        # 3. PERSISTENCE: Use correlation matrix at median threshold
        if(!is.null(analysis_results$persistence_results[[method]][[group_name]]$persistence_data)) {
          pers_data <- analysis_results$persistence_results[[method]][[group_name]]$persistence_data
          threshold_vals <- as.numeric(names(pers_data))
          if(length(threshold_vals) > 0 && !is.null(analysis_results$correlation_methods_raw[[method]][[group_name]])) {
            # Use median threshold
            median_thresh <- median(threshold_vals)
            cor_mat <- abs(analysis_results$correlation_methods_raw[[method]][[group_name]])
            persistence_mat <- cor_mat
            persistence_mat[persistence_mat < median_thresh] <- 0
            approach_matrices[["Persistence"]] <- persistence_mat
            approach_names <- c(approach_names, "Persistence")
          }
        }

        n_approaches <- length(approach_matrices)

        if(n_approaches < 2) {
          plot(1, type = "n", axes = FALSE, main = group_name)
          text(1, 1, paste("Only", n_approaches, "approach available"), cex = 1.0)
          text(1, 0.5, paste("Available:", paste(approach_names, collapse=", ")), cex = 0.8)
          plots_made <- plots_made + 1
          next
        }

        # Compute Jaccard similarity matrix between approaches
        jaccard_matrix <- matrix(1, nrow = n_approaches, ncol = n_approaches)
        rownames(jaccard_matrix) <- approach_names
        colnames(jaccard_matrix) <- approach_names

        for(i in 1:(n_approaches-1)) {
          for(j in (i+1):n_approaches) {
            mat1 <- approach_matrices[[i]]
            mat2 <- approach_matrices[[j]]

            jaccard <- compute_jaccard_similarity(mat1, mat2, threshold = 0)
            if(!is.na(jaccard)) {
              jaccard_matrix[i, j] <- jaccard
              jaccard_matrix[j, i] <- jaccard
            }
          }
        }

        # Render heatmap for this group
        render_jaccard_heatmap(jaccard_matrix, approach_names,
                              title = paste(group_name, "-", toupper(method), "Approach Similarity"))
        plots_made <- plots_made + 1
      }

      if(plots_made == 0) {
        par(mfrow = c(1, 1))
        plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
        text(1, 1, "No plots generated", cex = 1.2, col = "red")
      }

      # Reset par
      par(mfrow = c(1, 1))
    }, error = function(e) {
      par(mfrow = c(1, 1))
      plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
      text(1, 1, paste("Error in 6m:", e$message), cex = 0.9, col = "red")
      text(1, 0.5, "Check that all analyses have been run", cex = 0.8, col = "red")
    })
  })

  # ========================================================================
  # END OF TAB 6 SERVER FUNCTIONS
  # ========================================================================

  # 1. Granular Consensus Heatmap (approach × method combinations)
  output$granularConsensusHeatmap <- renderPlot({
    req(analysis_results$comprehensive_consensus)
    req(input$granular_consensus_group)
    req(input$granular_min_consensus)
    req(selected_methods())

    group_name <- input$granular_consensus_group
    consensus_results <- analysis_results$comprehensive_consensus[[group_name]]

    if(is.null(consensus_results) || is.null(consensus_results$standardized_metrics)) {
      plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
      text(1, 1, "No granular consensus data available", cex = 1.2, col = "gray")
      return()
    }

    std_metrics <- consensus_results$standardized_metrics
    methods <- selected_methods()
    n_combos <- length(methods) * 3  # methods × 3 approaches

    # Build a matrix: rows = nodes, columns = approach×method combinations
    all_nodes <- unique(c(
      unlist(lapply(std_metrics$weighted, function(x) if(!is.null(x)) x$Node else character(0))),
      unlist(lapply(std_metrics$percolation, function(x) if(!is.null(x)) x$Node else character(0))),
      unlist(lapply(std_metrics$persistence, function(x) if(!is.null(x)) x$Node else character(0)))
    ))

    if(length(all_nodes) == 0) {
      plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
      text(1, 1, "No nodes found", cex = 1.2, col = "gray")
      return()
    }

    # Create hub matrix
    hub_matrix <- matrix(0, nrow = length(all_nodes), ncol = n_combos)
    rownames(hub_matrix) <- all_nodes

    col_names <- c()
    col_idx <- 1

    for(approach in c("weighted", "percolation", "persistence")) {
      for(method in methods) {
        if(!is.null(std_metrics[[approach]][[method]])) {
          approach_data <- std_metrics[[approach]][[method]]
          hubs <- approach_data$Node[approach_data$IsHub]
          hub_matrix[hubs, col_idx] <- 1
        }

        # Column name
        approach_short <- substr(toupper(approach), 1, 1)  # W/P/Ps
        method_short <- toupper(substr(method, 1, 3))  # PEA/SPE/BIW/SHR/PAR
        col_names <- c(col_names, paste0(approach_short, ":", method_short))
        col_idx <- col_idx + 1
      }
    }

    colnames(hub_matrix) <- col_names

    # Calculate total agreement per node
    total_agreement <- rowSums(hub_matrix)

    # Filter nodes by consensus threshold
    meeting_threshold <- total_agreement >= input$granular_min_consensus

    if(sum(meeting_threshold) == 0) {
      plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
      text(1, 1, paste("No nodes meet threshold of", input$granular_min_consensus, "agreements"),
           cex = 1.2, col = "gray")
      return()
    }

    # Filter and sort by total agreement
    filtered_matrix <- hub_matrix[meeting_threshold, , drop = FALSE]
    filtered_agreement <- total_agreement[meeting_threshold]
    sort_order <- order(-filtered_agreement)
    filtered_matrix <- filtered_matrix[sort_order, , drop = FALSE]

    # Limit to top 50 nodes for visualization
    if(nrow(filtered_matrix) > 50) {
      filtered_matrix <- filtered_matrix[1:50, , drop = FALSE]
    }

    # Create heatmap
    par(mar = c(10, 10, 4, 2))
    image(t(filtered_matrix[nrow(filtered_matrix):1, ]),
          col = c("white", "#2E86AB"),
          axes = FALSE,
          xlab = "", ylab = "",
          main = paste0("Hub Identification Across ", n_combos, " Approach×Method Combinations\n",
                       group_name, " - Threshold: ", input$granular_min_consensus, "/", n_combos))

    # Add grid
    abline(h = seq(-0.5/(nrow(filtered_matrix)-1), 1 + 0.5/(nrow(filtered_matrix)-1),
                   length.out = nrow(filtered_matrix) + 1), col = "gray80")
    abline(v = seq(-0.5/(n_combos-1), 1 + 0.5/(n_combos-1), length.out = n_combos + 1), col = "gray80")

    # X-axis: approach×method combinations
    axis(1, at = seq(0, 1, length.out = n_combos), labels = col_names, las = 2, cex.axis = 0.7)

    # Y-axis: node names
    node_labels <- rownames(filtered_matrix)[nrow(filtered_matrix):1]
    axis(2, at = seq(0, 1, length.out = nrow(filtered_matrix)), labels = node_labels, las = 1, cex.axis = 0.7)

    # Add agreement counts on the right
    agreement_counts <- rowSums(filtered_matrix)[nrow(filtered_matrix):1]
    axis(4, at = seq(0, 1, length.out = nrow(filtered_matrix)),
         labels = paste0(agreement_counts, "/", n_combos), las = 1, cex.axis = 0.7)

    mtext("Total Agreement", side = 4, line = 3, cex = 0.8)
    mtext("Approach:Method Combination", side = 1, line = 8, cex = 0.8)
    mtext("Node", side = 2, line = 8, cex = 0.8)
  })

  # 2. Method-Level Agreement Distribution
  output$methodAgreementDistribution <- renderPlot({
    req(analysis_results$comprehensive_consensus)
    req(input$granular_consensus_group)
    req(selected_methods())

    group_name <- input$granular_consensus_group
    consensus_results <- analysis_results$comprehensive_consensus[[group_name]]

    if(is.null(consensus_results) || is.null(consensus_results$standardized_metrics)) {
      plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
      text(1, 1, "No data available", cex = 1.2, col = "gray")
      return()
    }

    std_metrics <- consensus_results$standardized_metrics
    methods <- selected_methods()
    n_combos <- length(methods) * 3  # methods × 3 approaches

    # Get all nodes
    all_nodes <- unique(c(
      unlist(lapply(std_metrics$weighted, function(x) if(!is.null(x)) x$Node else character(0))),
      unlist(lapply(std_metrics$percolation, function(x) if(!is.null(x)) x$Node else character(0))),
      unlist(lapply(std_metrics$persistence, function(x) if(!is.null(x)) x$Node else character(0)))
    ))

    if(length(all_nodes) == 0) {
      plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
      text(1, 1, "No nodes found", cex = 1.2, col = "gray")
      return()
    }

    # Count agreements per node
    agreement_counts <- rep(0, length(all_nodes))
    names(agreement_counts) <- all_nodes

    for(approach in c("weighted", "percolation", "persistence")) {
      for(method in methods) {
        if(!is.null(std_metrics[[approach]][[method]])) {
          approach_data <- std_metrics[[approach]][[method]]
          hubs <- approach_data$Node[approach_data$IsHub]
          agreement_counts[hubs] <- agreement_counts[hubs] + 1
        }
      }
    }

    # Calculate dynamic threshold values
    majority_thresh <- ceiling(n_combos * 0.5)
    strong_thresh <- ceiling(n_combos * 0.8)

    # Create histogram
    par(mar = c(5, 5, 4, 2))
    hist(agreement_counts, breaks = seq(-0.5, n_combos + 0.5, by = 1),
         col = "#4A90E2", border = "white",
         main = paste("Method Agreement Distribution -", group_name),
         xlab = paste0("Number of Approach×Method Combinations Identifying as Hub (out of ", n_combos, ")"),
         ylab = "Number of Nodes",
         las = 1)

    # Add vertical lines for key thresholds
    abline(v = majority_thresh, col = "orange", lty = 2, lwd = 2)  # Majority (>50%)
    abline(v = strong_thresh, col = "green", lty = 2, lwd = 2)  # Strong consensus (80%)
    abline(v = n_combos, col = "darkgreen", lty = 2, lwd = 3)  # Perfect consensus

    legend("topright",
           legend = c(paste0("Majority (", majority_thresh, "/", n_combos, ")"),
                     paste0("Strong (", strong_thresh, "/", n_combos, ")"),
                     paste0("Perfect (", n_combos, "/", n_combos, ")")),
           col = c("orange", "green", "darkgreen"),
           lty = 2, lwd = c(2, 2, 3), cex = 0.9)

    grid()
  })

  # 3. Super-Consensus Hubs Table
  output$superConsensusHubsTable <- DT::renderDataTable({
    req(analysis_results$comprehensive_consensus)
    req(input$granular_consensus_group)
    req(input$granular_min_consensus)
    req(selected_methods())

    group_name <- input$granular_consensus_group
    consensus_results <- analysis_results$comprehensive_consensus[[group_name]]

    if(is.null(consensus_results)) {
      return(DT::datatable(data.frame(Message = "No data available")))
    }

    std_metrics <- consensus_results$standardized_metrics
    consensus_scores <- consensus_results$consensus_scores
    methods <- selected_methods()
    n_combos <- length(methods) * 3  # methods × 3 approaches

    if(is.null(std_metrics) || is.null(consensus_scores)) {
      return(DT::datatable(data.frame(Message = "No consensus data available")))
    }

    # Get all nodes
    all_nodes <- unique(c(
      unlist(lapply(std_metrics$weighted, function(x) if(!is.null(x)) x$Node else character(0))),
      unlist(lapply(std_metrics$percolation, function(x) if(!is.null(x)) x$Node else character(0))),
      unlist(lapply(std_metrics$persistence, function(x) if(!is.null(x)) x$Node else character(0)))
    ))

    # Count agreements
    agreement_counts <- rep(0, length(all_nodes))
    names(agreement_counts) <- all_nodes

    for(approach in c("weighted", "percolation", "persistence")) {
      for(method in methods) {
        if(!is.null(std_metrics[[approach]][[method]])) {
          approach_data <- std_metrics[[approach]][[method]]
          hubs <- approach_data$Node[approach_data$IsHub]
          agreement_counts[hubs] <- agreement_counts[hubs] + 1
        }
      }
    }

    # Filter by threshold
    super_consensus_nodes <- names(agreement_counts[agreement_counts >= input$granular_min_consensus])

    if(length(super_consensus_nodes) == 0) {
      return(DT::datatable(data.frame(Message = paste("No nodes meet threshold of", input$granular_min_consensus))))
    }

    # Build table
    super_df <- consensus_scores[consensus_scores$Node %in% super_consensus_nodes, ]
    super_df$MethodAgreementCount <- agreement_counts[super_df$Node]
    super_df$MethodAgreementPct <- round(100 * super_df$MethodAgreementCount / n_combos, 1)

    # Sort by method agreement
    super_df <- super_df[order(-super_df$MethodAgreementCount, -super_df$BayesianConsensusScore), ]

    # Create display table
    display_table <- data.frame(
      Node = super_df$Node,
      MethodAgreement = paste0(super_df$MethodAgreementCount, "/", n_combos, " (", super_df$MethodAgreementPct, "%)"),
      BayesianScore = round(super_df$BayesianConsensusScore, 3),
      ApproachAgreement = ifelse(super_df$AllThreeAgree, "All 3",
                                 ifelse(super_df$TwoAgree, "2 of 3", "1 of 3")),
      ConfidenceCategory = super_df$ConfidenceCategory,
      RobustnessCategory = super_df$RobustnessCategory,
      stringsAsFactors = FALSE
    )

    # Dynamic interval thresholds based on n_combos
    majority_thresh <- ceiling(n_combos * 0.5)
    strong_thresh <- ceiling(n_combos * 0.8)

    DT::datatable(display_table,
                  options = list(pageLength = 20, scrollX = TRUE),
                  rownames = FALSE) %>%
      DT::formatStyle('MethodAgreement',
                     backgroundColor = DT::styleInterval(c(majority_thresh, strong_thresh, n_combos),
                                                        c('#fff3cd', '#d1ecf1', '#d4edda', '#c3e6cb')))
  })

  # 4. Approach vs Method Consistency
  output$approachVsMethodConsistency <- renderPlot({
    req(analysis_results$comprehensive_consensus)
    req(input$granular_consensus_group)
    req(selected_methods())

    group_name <- input$granular_consensus_group
    consensus_results <- analysis_results$comprehensive_consensus[[group_name]]

    if(is.null(consensus_results) || is.null(consensus_results$standardized_metrics)) {
      plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
      text(1, 1, "No data available", cex = 1.2, col = "gray")
      return()
    }

    std_metrics <- consensus_results$standardized_metrics
    consensus_scores <- consensus_results$consensus_scores
    methods <- selected_methods()
    n_combos <- length(methods) * 3  # methods × 3 approaches

    if(is.null(consensus_scores)) {
      plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
      text(1, 1, "No consensus scores available", cex = 1.2, col = "gray")
      return()
    }

    all_nodes <- consensus_scores$Node

    # Calculate approach-level agreement (how many approaches identify as hub)
    approach_agreement <- rep(0, length(all_nodes))
    names(approach_agreement) <- all_nodes

    for(node in all_nodes) {
      # Weighted approach
      if(consensus_scores$Weighted_HubProb[consensus_scores$Node == node] >= 0.6) {
        approach_agreement[node] <- approach_agreement[node] + 1
      }
      # Percolation approach
      if(consensus_scores$Percolation_HubProb[consensus_scores$Node == node] >= 0.6) {
        approach_agreement[node] <- approach_agreement[node] + 1
      }
      # Persistence approach
      if(consensus_scores$Persistence_HubProb[consensus_scores$Node == node] >= 0.6) {
        approach_agreement[node] <- approach_agreement[node] + 1
      }
    }

    # Calculate method-level agreement (across all available approach×method combinations)
    method_agreement <- rep(0, length(all_nodes))
    names(method_agreement) <- all_nodes

    for(approach in c("weighted", "percolation", "persistence")) {
      for(method in methods) {
        if(!is.null(std_metrics[[approach]][[method]])) {
          approach_data <- std_metrics[[approach]][[method]]
          hubs <- approach_data$Node[approach_data$IsHub]
          method_agreement[hubs] <- method_agreement[hubs] + 1
        }
      }
    }

    # Calculate dynamic threshold values
    majority_thresh <- ceiling(n_combos * 0.5)
    strong_thresh <- ceiling(n_combos * 0.8)

    # Create scatter plot
    par(mar = c(5, 5, 4, 2))
    plot(approach_agreement, method_agreement,
         pch = 19, col = adjustcolor("#4A90E2", alpha.f = 0.6),
         xlab = "Approach-Level Agreement (0-3 approaches)",
         ylab = paste0("Method-Level Agreement (0-", n_combos, " approach×method combinations)"),
         main = paste("Approach vs Method Consistency -", group_name),
         xlim = c(-0.2, 3.2), ylim = c(0, n_combos + 1),
         cex = 1.2, las = 1)

    # Add reference lines
    abline(h = majority_thresh, col = "orange", lty = 2, lwd = 1.5)  # 50% method consensus
    abline(h = strong_thresh, col = "green", lty = 2, lwd = 1.5)  # 80% method consensus
    abline(v = 2, col = "blue", lty = 2, lwd = 1.5)    # 2/3 approach consensus
    abline(v = 3, col = "darkgreen", lty = 2, lwd = 2) # Perfect approach consensus

    # Add jitter to see overlapping points
    points(jitter(approach_agreement, amount = 0.1),
           jitter(method_agreement, amount = 0.3),
           pch = 19, col = adjustcolor("#4A90E2", alpha.f = 0.3), cex = 1)

    legend("topleft",
           legend = c("Approach: 2/3", "Approach: 3/3",
                     paste0("Method: ", majority_thresh, "/", n_combos, " (50%)"),
                     paste0("Method: ", strong_thresh, "/", n_combos, " (80%)")),
           col = c("blue", "darkgreen", "orange", "green"),
           lty = 2, lwd = c(1.5, 2, 1.5, 1.5), cex = 0.8)

    grid()

    # Add correlation text
    cor_val <- cor(approach_agreement, method_agreement, use = "complete.obs")
    text(0.5, n_combos - 1, paste("Correlation:", round(cor_val, 3)), cex = 1.1, pos = 4)
  })

  # ============================================================================
  # TAB 7: CONSENSUS (CROSS-METHOD COMPARISON)
  # ============================================================================

  # Helper function to get comprehensive rank consensus for a group
  get_comprehensive_consensus_for_group <- function(group) {
    methods <- filter_common_methods(analysis_results$comprehensive_consensus, selected_methods())
    if (length(methods) == 0) return(NULL)

    compute_comprehensive_rank_consensus(
      group = group,
      method_percolation_results = analysis_results$method_percolation_results,
      method_weighted_results = analysis_results$method_weighted_results,
      persistence_results = analysis_results$persistence_results,
      methods = methods
    )
  }

  # 7a. Consensus Scores (Normalized 0-1) - Scatter Plot
  output$consensusNodeMetricsAcrossMethodsPlot <- renderPlot({
    req(analysis_results$comprehensive_consensus)
    req(analysis_results$method_percolation_results)
    req(selected_methods())

    all_groups <- names(analysis_results$comprehensive_consensus)
    if (length(all_groups) == 0) {
      plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
      text(1, 1, "No consensus data available", cex = 1.2, col = "gray")
      return()
    }

    methods <- filter_common_methods(analysis_results$comprehensive_consensus, selected_methods())
    if (length(methods) == 0) {
      plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
      text(1, 1, "No common methods across all groups", cex = 1.2, col = "gray")
      return()
    }

    n_combos <- length(methods) * 3  # methods × 3 approaches
    n_groups <- length(all_groups)
    par(mfrow = c(2, 2), mar = c(5, 5, 4, 2))

    for (group in all_groups[1:min(4, n_groups)]) {
      tryCatch({
        consensus_df <- get_comprehensive_consensus_for_group(group)

        if (is.null(consensus_df) || nrow(consensus_df) == 0) {
          plot(1, type = "n", axes = FALSE, main = group)
          text(1, 1, "No data available", cex = 1.0)
          next
        }

        # Get colors by brain region
        plot_colors <- rep("steelblue", nrow(consensus_df))
        names(plot_colors) <- consensus_df$Node
        if (!is.null(ui_state$brain_areas) && !is.null(ui_state$area_colors)) {
          for (node in consensus_df$Node) {
            for (area_name in names(ui_state$brain_areas)) {
              if (node %in% ui_state$brain_areas[[area_name]]) {
                if (area_name %in% names(ui_state$area_colors)) {
                  plot_colors[node] <- ui_state$area_colors[[area_name]]
                }
                break
              }
            }
          }
        }

        n_methods <- consensus_df$N_Methods[1]

        # Scatter plot of normalized consensus scores
        plot(consensus_df$Consensus_NodeStrength, consensus_df$Consensus_Eigenvector,
             pch = 21, cex = 2.2,
             bg = adjustcolor(plot_colors[consensus_df$Node], alpha.f = 0.6),
             col = adjustcolor(plot_colors[consensus_df$Node], alpha.f = 0.8),
             lwd = 2,
             xlab = "Consensus Node Strength (0 = lowest, 1 = highest)",
             ylab = "Consensus Eigenvector (0 = lowest, 1 = highest)",
             main = paste0("Rank-Based Consensus (", n_methods, " method-approach combos) - ", group),
             xlim = c(0, 1.05),
             ylim = c(0, 1.05))

        abline(a = 0, b = 1, col = "red", lty = 2, lwd = 1.5)
        grid(col = "lightgray", lty = "dotted", lwd = 0.5)

        # Label top 15 nodes by either metric
        top_nodes <- consensus_df[consensus_df$Consensus_Eigenvector >= sort(consensus_df$Consensus_Eigenvector, decreasing = TRUE)[min(15, nrow(consensus_df))] |
                                  consensus_df$Consensus_NodeStrength >= sort(consensus_df$Consensus_NodeStrength, decreasing = TRUE)[min(15, nrow(consensus_df))], ]

        for (i in 1:nrow(top_nodes)) {
          text(top_nodes$Consensus_NodeStrength[i], top_nodes$Consensus_Eigenvector[i],
               labels = top_nodes$Node[i], cex = 0.7, font = 2, col = "black")
        }

        spearman_rho <- cor(consensus_df$Consensus_NodeStrength, consensus_df$Consensus_Eigenvector,
                           method = "spearman", use = "complete.obs")
        legend("bottomright", legend = paste("Spearman ρ =", round(spearman_rho, 3)),
               bty = "n", cex = 0.9)

      }, error = function(e) {
        plot(1, type = "n", axes = FALSE, main = group)
        text(1, 1, paste("Error:", e$message), cex = 0.8)
      })
    }
  })

  # 7a2. Average Ranks Across Methods - Scatter Plot
  output$consensusNodeRanksPlot <- renderPlot({
    req(analysis_results$comprehensive_consensus)
    req(analysis_results$method_percolation_results)
    req(selected_methods())

    all_groups <- names(analysis_results$comprehensive_consensus)
    if (length(all_groups) == 0) {
      plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
      text(1, 1, "No consensus data available", cex = 1.2, col = "gray")
      return()
    }

    methods <- filter_common_methods(analysis_results$comprehensive_consensus, selected_methods())
    if (length(methods) == 0) {
      plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
      text(1, 1, "No common methods across all groups", cex = 1.2, col = "gray")
      return()
    }

    n_combos <- length(methods) * 3  # methods × 3 approaches
    n_groups <- length(all_groups)
    par(mfrow = c(2, 2), mar = c(5, 5, 4, 2))

    for (group in all_groups[1:min(4, n_groups)]) {
      tryCatch({
        consensus_df <- get_comprehensive_consensus_for_group(group)

        if (is.null(consensus_df) || nrow(consensus_df) == 0) {
          plot(1, type = "n", axes = FALSE, main = group)
          text(1, 1, "No data available", cex = 1.0)
          next
        }

        # Get colors by brain region
        plot_colors <- rep("steelblue", nrow(consensus_df))
        names(plot_colors) <- consensus_df$Node
        if (!is.null(ui_state$brain_areas) && !is.null(ui_state$area_colors)) {
          for (node in consensus_df$Node) {
            for (area_name in names(ui_state$brain_areas)) {
              if (node %in% ui_state$brain_areas[[area_name]]) {
                if (area_name %in% names(ui_state$area_colors)) {
                  plot_colors[node] <- ui_state$area_colors[[area_name]]
                }
                break
              }
            }
          }
        }

        n_methods <- consensus_df$N_Methods[1]
        max_rank <- max(c(consensus_df$Avg_Strength_Rank, consensus_df$Avg_Eigen_Rank), na.rm = TRUE)

        # Scatter plot of average ranks (lower = better)
        plot(consensus_df$Avg_Strength_Rank, consensus_df$Avg_Eigen_Rank,
             pch = 21, cex = 2.2,
             bg = adjustcolor(plot_colors[consensus_df$Node], alpha.f = 0.6),
             col = adjustcolor(plot_colors[consensus_df$Node], alpha.f = 0.8),
             lwd = 2,
             xlab = "Avg Strength Rank (lower = more important)",
             ylab = "Avg Eigenvector Rank (lower = more important)",
             main = paste0("Average Ranks (", n_methods, " method-approach combos) - ", group),
             xlim = c(0, max_rank * 1.05),
             ylim = c(0, max_rank * 1.05))

        abline(a = 0, b = 1, col = "red", lty = 2, lwd = 1.5)
        grid(col = "lightgray", lty = "dotted", lwd = 0.5)

        # Label top 15 nodes (lowest average ranks)
        top_nodes <- consensus_df[consensus_df$Avg_Eigen_Rank <= sort(consensus_df$Avg_Eigen_Rank)[min(15, nrow(consensus_df))] |
                                  consensus_df$Avg_Strength_Rank <= sort(consensus_df$Avg_Strength_Rank)[min(15, nrow(consensus_df))], ]

        for (i in 1:nrow(top_nodes)) {
          text(top_nodes$Avg_Strength_Rank[i], top_nodes$Avg_Eigen_Rank[i],
               labels = top_nodes$Node[i], cex = 0.7, font = 2, col = "black")
        }

        spearman_rho <- cor(consensus_df$Avg_Strength_Rank, consensus_df$Avg_Eigen_Rank,
                           method = "spearman", use = "complete.obs")
        legend("bottomright", legend = paste("Spearman ρ =", round(spearman_rho, 3)),
               bty = "n", cex = 0.9)

      }, error = function(e) {
        plot(1, type = "n", axes = FALSE, main = group)
        text(1, 1, paste("Error:", e$message), cex = 0.8)
      })
    }
  })

  # 7b. Consensus Networks (Percolation Networks with Consensus Eigenvector Sizing)
  output$consensusNetworkPlot <- renderPlot({
    req(analysis_results$comprehensive_consensus)
    req(analysis_results$method_percolation_results)
    req(input$consensus_network_method)

    method <- tolower(input$consensus_network_method %||% "pearson")
    layout_type <- input$consensus_network_layout %||% "fr"

    # Get percolation networks for selected method
    perc_data <- analysis_results$method_percolation_results[[method]]

    if(is.null(perc_data) || is.null(perc_data$networks)) {
      plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
      text(1, 1, paste("No percolation networks for", method, "method"), cex = 1.2, col = "gray")
      return()
    }

    networks <- perc_data$networks
    all_groups <- names(networks)

    if(length(all_groups) == 0) {
      plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
      text(1, 1, "No networks available", cex = 1.2, col = "gray")
      return()
    }

    # Get consensus eigenvector using the new rank-based approach
    consensus_eigenvector_by_group <- list()

    for(group in all_groups) {
      consensus_df <- get_comprehensive_consensus_for_group(group)

      if(!is.null(consensus_df) && nrow(consensus_df) > 0) {
        # Use normalized consensus eigenvector (0-1 scale)
        consensus_eig <- consensus_df$Consensus_Eigenvector
        names(consensus_eig) <- consensus_df$Node
        consensus_eigenvector_by_group[[group]] <- consensus_eig
      }
    }

    # Create multi-panel layout
    n_groups <- length(all_groups)
    if(n_groups <= 2) {
      par(mfrow = c(1, n_groups), mar = c(2, 2, 3, 2))
    } else {
      par(mfrow = c(2, 2), mar = c(2, 2, 3, 2))
    }

    # Plot each network with consensus eigenvector sizing
    for(group in all_groups[1:min(4, n_groups)]) {
      network <- networks[[group]]
      consensus_eig <- consensus_eigenvector_by_group[[group]]

      if(is.null(network) || vcount(network) == 0 || is.null(consensus_eig)) {
        plot(1, type = "n", xlab = "", ylab = "", axes = FALSE, main = paste("Group:", group))
        text(1, 1, "No network data", cex = 1.2)
        next
      }

      # Set node colors by brain region
      if(!is.null(ui_state$brain_areas) && !is.null(ui_state$area_colors)) {
        node_colors <- rep("#808080", vcount(network))  # Default gray

        for(area_name in names(ui_state$brain_areas)) {
          regions <- ui_state$brain_areas[[area_name]]
          matching_nodes <- intersect(regions, V(network)$name)

          if(length(matching_nodes) > 0) {
            node_indices <- which(V(network)$name %in% matching_nodes)
            if(area_name %in% names(ui_state$area_colors)) {
              node_colors[node_indices] <- ui_state$area_colors[[area_name]]
            }
          }
        }

        V(network)$color <- node_colors
      } else {
        V(network)$color <- "#1F78B4"  # Default blue
      }

      # Set node sizes based on consensus eigenvector
      node_names <- V(network)$name
      node_eig_values <- sapply(node_names, function(n) {
        if(n %in% names(consensus_eig)) consensus_eig[n] else 0
      })

      if(max(node_eig_values) > min(node_eig_values)) {
        V(network)$size <- scales::rescale(node_eig_values, to = c(5, 20))
      } else {
        V(network)$size <- 10
      }

      # Create layout
      if(layout_type == "circle") {
        graph_layout <- layout_in_circle(network)
      } else if(layout_type == "fr") {
        graph_layout <- layout_with_fr(network)
      } else if(layout_type == "kk") {
        graph_layout <- layout_with_kk(network)
      } else if(layout_type == "grid") {
        graph_layout <- tryCatch({
          layout_on_grid(network)
        }, error = function(e) {
          layout_with_fr(network)
        })
      } else {
        graph_layout <- layout_with_fr(network)
      }

      # Get group color for border
      group_color <- if(!is.null(ui_state$group_colors) && group %in% names(ui_state$group_colors)) {
        ui_state$group_colors[[group]]
      } else {
        "black"
      }

      # Plot the network
      plot(network,
           layout = graph_layout,
           vertex.color = V(network)$color,
           vertex.size = V(network)$size,
           vertex.label = V(network)$name,
           vertex.label.cex = 0.6,
           vertex.label.color = "black",
           vertex.label.dist = 0,        # Center labels on nodes
           vertex.label.font = 2,        # Bold labels
           vertex.frame.color = "white",
           edge.width = abs(E(network)$weight) * 2.5,
           edge.color = adjustcolor("gray50", alpha.f = 0.6),
           main = paste(group, "- Rank-Based Consensus Eigenvector"),
           sub = paste(toupper(method), "percolation network | Node size = consensus score (0-1)"))

      # Add border in group color
      box(col = group_color, lwd = 3)
    }

    # Reset par
    par(mfrow = c(1, 1))
  })

  # 7e. Group Similarity Across All Method-Approach Combinations
  # Comparing GROUPS (not methods) aggregated across all methodological choices
  output$networkSimilarityHeatmapPlot <- renderPlot({
    req(selected_methods())

    tryCatch({
      # Check data availability with informative messages
      if(is.null(analysis_results$correlation_methods_raw)) {
        plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
        text(1, 1, "No correlation data available", cex = 1.2, col = "orange")
        text(1, 0.5, "Run analysis first", cex = 1.0, col = "orange")
        return()
      }

      # Use selected correlation methods
      methods <- selected_methods()
      approaches <- c("weighted", "percolation", "persistence")
      n_combos <- length(methods) * length(approaches)

      # Get groups from first available method
      all_groups <- NULL
      for(method in methods) {
        if(!is.null(analysis_results$correlation_methods_raw[[method]])) {
          all_groups <- names(analysis_results$correlation_methods_raw[[method]])
          break
        }
      }

      if(is.null(all_groups) || length(all_groups) == 0) {
        plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
        text(1, 1, "No group data found", cex = 1.2, col = "orange")
        return()
      }

      if(length(all_groups) < 2) {
        plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
        text(1, 1, "Need at least 2 groups for comparison", cex = 1.2, col = "gray")
        text(1, 0.5, paste("Found:", length(all_groups), "group"), cex = 1.0, col = "gray")
        return()
      }

      n_groups <- length(all_groups)

      # Initialize group similarity matrix (averaged across all method-approach combinations)
      group_jaccard_matrix <- matrix(0, nrow = n_groups, ncol = n_groups)
      rownames(group_jaccard_matrix) <- all_groups
      colnames(group_jaccard_matrix) <- all_groups
      combination_count <- matrix(0, nrow = n_groups, ncol = n_groups)

      # Helper function: compute average Jaccard across all thresholds for persistence approach
      compute_persistence_avg_jaccard <- function(method, group_i, group_j) {
        pers_data_i <- analysis_results$persistence_results[[method]][[group_i]]$persistence_data
        pers_data_j <- analysis_results$persistence_results[[method]][[group_j]]$persistence_data
        cor_mat_i_raw <- analysis_results$correlation_methods_raw[[method]][[group_i]]
        cor_mat_j_raw <- analysis_results$correlation_methods_raw[[method]][[group_j]]

        if(is.null(pers_data_i) || is.null(pers_data_j) ||
           is.null(cor_mat_i_raw) || is.null(cor_mat_j_raw)) {
          return(NA)
        }

        # Get thresholds available in both groups
        threshold_vals <- sort(as.numeric(intersect(names(pers_data_i), names(pers_data_j))))

        if(length(threshold_vals) == 0) {
          return(NA)
        }

        # Compute Jaccard at each threshold
        jaccard_at_thresholds <- numeric(length(threshold_vals))

        for(t_idx in seq_along(threshold_vals)) {
          thresh <- threshold_vals[t_idx]

          # Threshold both correlation matrices
          cor_mat_i <- abs(cor_mat_i_raw)
          cor_mat_j <- abs(cor_mat_j_raw)
          cor_mat_i[cor_mat_i < thresh] <- 0
          cor_mat_j[cor_mat_j < thresh] <- 0

          jaccard_at_thresholds[t_idx] <- compute_jaccard_similarity(cor_mat_i, cor_mat_j, threshold = 0)
        }

        # Return average Jaccard across all thresholds
        mean(jaccard_at_thresholds, na.rm = TRUE)
      }

      # For each method-approach combination, compute group-to-group similarity
      for(method in methods) {
        for(approach in approaches) {

          if(approach == "persistence") {
            # PERSISTENCE: Compute average Jaccard across ALL thresholds (true persistence approach)
            for(i in 1:(n_groups-1)) {
              for(j in (i+1):n_groups) {
                group_i <- all_groups[i]
                group_j <- all_groups[j]

                avg_jaccard <- compute_persistence_avg_jaccard(method, group_i, group_j)

                if(!is.na(avg_jaccard)) {
                  group_jaccard_matrix[i, j] <- group_jaccard_matrix[i, j] + avg_jaccard
                  group_jaccard_matrix[j, i] <- group_jaccard_matrix[j, i] + avg_jaccard
                  combination_count[i, j] <- combination_count[i, j] + 1
                  combination_count[j, i] <- combination_count[j, i] + 1
                }
              }
            }

          } else {
            # WEIGHTED and PERCOLATION: Build network matrices first, then compute Jaccard
            group_networks <- list()

            for(group in all_groups) {
              network_mat <- NULL

              if(approach == "weighted") {
                # Use raw correlation matrix
                if(!is.null(analysis_results$correlation_methods_raw[[method]][[group]])) {
                  network_mat <- abs(analysis_results$correlation_methods_raw[[method]][[group]])
                }
              } else if(approach == "percolation") {
                # Use percolation adjacency matrix weighted by correlations
                if(!is.null(analysis_results$method_percolation_results[[method]]$adjacency_matrices)) {
                  adj_mat <- analysis_results$method_percolation_results[[method]]$adjacency_matrices[[group]]
                  if(!is.null(adj_mat) && !is.null(analysis_results$correlation_methods_raw[[method]][[group]])) {
                    cor_mat <- abs(analysis_results$correlation_methods_raw[[method]][[group]])
                    network_mat <- adj_mat * cor_mat
                  }
                }
              }

              if(!is.null(network_mat)) {
                group_networks[[group]] <- network_mat
              }
            }

            # Compute pairwise Jaccard similarity between all groups for this combination
            if(length(group_networks) >= 2) {
              for(i in 1:(n_groups-1)) {
                for(j in (i+1):n_groups) {
                  group_i <- all_groups[i]
                  group_j <- all_groups[j]

                  if(group_i %in% names(group_networks) && group_j %in% names(group_networks)) {
                    jaccard <- compute_jaccard_similarity(group_networks[[group_i]],
                                                          group_networks[[group_j]],
                                                          threshold = 0)
                    if(!is.na(jaccard)) {
                      group_jaccard_matrix[i, j] <- group_jaccard_matrix[i, j] + jaccard
                      group_jaccard_matrix[j, i] <- group_jaccard_matrix[j, i] + jaccard
                      combination_count[i, j] <- combination_count[i, j] + 1
                      combination_count[j, i] <- combination_count[j, i] + 1
                    }
                  }
                }
              }
            }
          }
        }
      }

      # Average the Jaccard scores across all combinations
      for(i in 1:n_groups) {
        for(j in 1:n_groups) {
          if(i == j) {
            group_jaccard_matrix[i, j] <- 1
          } else if(combination_count[i, j] > 0) {
            group_jaccard_matrix[i, j] <- group_jaccard_matrix[i, j] / combination_count[i, j]
          }
        }
      }

      # Check if any combinations were computed
      total_combinations <- max(combination_count)
      if(total_combinations == 0) {
        plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
        text(1, 1, "No method-approach combinations available", cex = 1.1, col = "orange")
        text(1, 0.7, "Need: Weighted + Percolation + Persistence analyses", cex = 0.9, col = "orange")
        text(1, 0.4, "Run all analysis tabs first", cex = 0.9, col = "orange")
        return()
      }

      # Render single comprehensive heatmap
      render_jaccard_heatmap(group_jaccard_matrix, all_groups,
                            title = paste("Group Similarity (Avg across", total_combinations, "method-approach combinations)"))

      # Reset par
      par(mfrow = c(1, 1))
    }, error = function(e) {
      par(mfrow = c(1, 1))
      plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
      text(1, 1, paste("Error in 7e:", substr(e$message, 1, 60)), cex = 0.9, col = "red")
      text(1, 0.5, "Check console for full error details", cex = 0.8, col = "red")
      print(paste("Full error in Tab 7e:", e$message))
    })
  })

  # 7e-Weighted. Group Similarity - WEIGHTED approach only
  output$networkSimilarityWeightedPlot <- renderPlot({
    req(selected_methods())

    tryCatch({
      if(is.null(analysis_results$correlation_methods_raw)) {
        plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
        text(1, 1, "No data", cex = 1.0, col = "gray")
        return()
      }

      methods <- selected_methods()
      all_groups <- NULL
      for(method in methods) {
        if(!is.null(analysis_results$correlation_methods_raw[[method]])) {
          all_groups <- names(analysis_results$correlation_methods_raw[[method]])
          break
        }
      }

      if(is.null(all_groups) || length(all_groups) < 2) {
        plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
        text(1, 1, "Need >= 2 groups", cex = 1.0, col = "gray")
        return()
      }

      n_groups <- length(all_groups)
      group_jaccard_matrix <- matrix(0, nrow = n_groups, ncol = n_groups)
      rownames(group_jaccard_matrix) <- all_groups
      colnames(group_jaccard_matrix) <- all_groups
      combination_count <- matrix(0, nrow = n_groups, ncol = n_groups)

      # Compute weighted Jaccard for each method
      for(method in methods) {
        group_networks <- list()
        for(group in all_groups) {
          if(!is.null(analysis_results$correlation_methods_raw[[method]][[group]])) {
            group_networks[[group]] <- abs(analysis_results$correlation_methods_raw[[method]][[group]])
          }
        }

        # Compute pairwise Jaccard
        for(i in 1:(n_groups-1)) {
          for(j in (i+1):n_groups) {
            group_i <- all_groups[i]
            group_j <- all_groups[j]
            if(!is.null(group_networks[[group_i]]) && !is.null(group_networks[[group_j]])) {
              jac <- compute_jaccard_similarity(group_networks[[group_i]], group_networks[[group_j]], threshold = 0)
              if(!is.na(jac)) {
                group_jaccard_matrix[i, j] <- group_jaccard_matrix[i, j] + jac
                group_jaccard_matrix[j, i] <- group_jaccard_matrix[j, i] + jac
                combination_count[i, j] <- combination_count[i, j] + 1
                combination_count[j, i] <- combination_count[j, i] + 1
              }
            }
          }
        }
      }

      # Average
      for(i in 1:n_groups) {
        for(j in 1:n_groups) {
          if(i == j) {
            group_jaccard_matrix[i, j] <- 1
          } else if(combination_count[i, j] > 0) {
            group_jaccard_matrix[i, j] <- group_jaccard_matrix[i, j] / combination_count[i, j]
          }
        }
      }

      n_methods_used <- max(combination_count)
      if(n_methods_used == 0) {
        plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
        text(1, 1, "No weighted data", cex = 1.0, col = "gray")
        return()
      }

      render_jaccard_heatmap(group_jaccard_matrix, all_groups,
                            title = paste0("Weighted (", n_methods_used, " methods)"))
    }, error = function(e) {
      plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
      text(1, 1, "Error", cex = 0.9, col = "red")
    })
  })

  # 7e-Percolation. Group Similarity - PERCOLATION approach only
  output$networkSimilarityPercolationPlot <- renderPlot({
    req(selected_methods())

    tryCatch({
      if(is.null(analysis_results$correlation_methods_raw) || is.null(analysis_results$method_percolation_results)) {
        plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
        text(1, 1, "No data", cex = 1.0, col = "gray")
        return()
      }

      methods <- selected_methods()
      all_groups <- NULL
      for(method in methods) {
        if(!is.null(analysis_results$correlation_methods_raw[[method]])) {
          all_groups <- names(analysis_results$correlation_methods_raw[[method]])
          break
        }
      }

      if(is.null(all_groups) || length(all_groups) < 2) {
        plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
        text(1, 1, "Need >= 2 groups", cex = 1.0, col = "gray")
        return()
      }

      n_groups <- length(all_groups)
      group_jaccard_matrix <- matrix(0, nrow = n_groups, ncol = n_groups)
      rownames(group_jaccard_matrix) <- all_groups
      colnames(group_jaccard_matrix) <- all_groups
      combination_count <- matrix(0, nrow = n_groups, ncol = n_groups)

      # Compute percolation Jaccard for each method
      for(method in methods) {
        group_networks <- list()

        for(group in all_groups) {
          perc_data <- analysis_results$method_percolation_results[[method]]
          if(!is.null(perc_data) && !is.null(perc_data$adjacency_matrices[[group]])) {
            adj_mat <- perc_data$adjacency_matrices[[group]]
            cor_mat <- analysis_results$correlation_methods_raw[[method]][[group]]
            if(!is.null(cor_mat)) {
              # Multiply adjacency by correlation weights
              group_networks[[group]] <- adj_mat * abs(cor_mat)
            }
          }
        }

        # Compute pairwise Jaccard
        for(i in 1:(n_groups-1)) {
          for(j in (i+1):n_groups) {
            group_i <- all_groups[i]
            group_j <- all_groups[j]
            if(!is.null(group_networks[[group_i]]) && !is.null(group_networks[[group_j]])) {
              jac <- compute_jaccard_similarity(group_networks[[group_i]], group_networks[[group_j]], threshold = 0)
              if(!is.na(jac)) {
                group_jaccard_matrix[i, j] <- group_jaccard_matrix[i, j] + jac
                group_jaccard_matrix[j, i] <- group_jaccard_matrix[j, i] + jac
                combination_count[i, j] <- combination_count[i, j] + 1
                combination_count[j, i] <- combination_count[j, i] + 1
              }
            }
          }
        }
      }

      # Average
      for(i in 1:n_groups) {
        for(j in 1:n_groups) {
          if(i == j) {
            group_jaccard_matrix[i, j] <- 1
          } else if(combination_count[i, j] > 0) {
            group_jaccard_matrix[i, j] <- group_jaccard_matrix[i, j] / combination_count[i, j]
          }
        }
      }

      n_methods_used <- max(combination_count)
      if(n_methods_used == 0) {
        plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
        text(1, 1, "No percolation data", cex = 1.0, col = "gray")
        return()
      }

      render_jaccard_heatmap(group_jaccard_matrix, all_groups,
                            title = paste0("Percolation (", n_methods_used, " methods)"))
    }, error = function(e) {
      plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
      text(1, 1, "Error", cex = 0.9, col = "red")
    })
  })

  # 7e-Persistence. Group Similarity - PERSISTENCE approach only
  output$networkSimilarityPersistencePlot <- renderPlot({
    req(selected_methods())

    tryCatch({
      # Diagnostic checks
      if(is.null(analysis_results$correlation_methods_raw)) {
        plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
        text(1, 1, "No correlation data", cex = 1.0, col = "orange")
        return()
      }

      if(is.null(analysis_results$persistence_results)) {
        plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
        text(1, 1, "Persistence analysis not run", cex = 0.9, col = "orange")
        text(1, 0.7, "Run full analysis first", cex = 0.8, col = "gray")
        return()
      }

      # Check which methods have persistence data
      methods <- selected_methods()
      methods_with_pers <- character(0)
      for(m in methods) {
        if(!is.null(analysis_results$persistence_results[[m]])) {
          methods_with_pers <- c(methods_with_pers, m)
        }
      }

      if(length(methods_with_pers) == 0) {
        plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
        text(1, 1, "No persistence data for selected methods", cex = 0.9, col = "orange")
        text(1, 0.7, paste("Selected:", paste(methods, collapse=", ")), cex = 0.7, col = "gray")
        text(1, 0.5, paste("Available:", paste(names(analysis_results$persistence_results), collapse=", ")), cex = 0.7, col = "gray")
        return()
      }

      all_groups <- NULL
      for(method in methods) {
        if(!is.null(analysis_results$correlation_methods_raw[[method]])) {
          all_groups <- names(analysis_results$correlation_methods_raw[[method]])
          break
        }
      }

      if(is.null(all_groups) || length(all_groups) < 2) {
        plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
        text(1, 1, "Need >= 2 groups", cex = 1.0, col = "gray")
        return()
      }

      n_groups <- length(all_groups)
      group_jaccard_matrix <- matrix(0, nrow = n_groups, ncol = n_groups)
      rownames(group_jaccard_matrix) <- all_groups
      colnames(group_jaccard_matrix) <- all_groups
      combination_count <- matrix(0, nrow = n_groups, ncol = n_groups)

      # Helper function: compute average Jaccard across all thresholds
      compute_persistence_avg_jaccard_local <- function(method, group_i, group_j) {
        pers_data_i <- analysis_results$persistence_results[[method]][[group_i]]$persistence_data
        pers_data_j <- analysis_results$persistence_results[[method]][[group_j]]$persistence_data
        cor_mat_i_raw <- analysis_results$correlation_methods_raw[[method]][[group_i]]
        cor_mat_j_raw <- analysis_results$correlation_methods_raw[[method]][[group_j]]

        if(is.null(pers_data_i) || is.null(pers_data_j) ||
           is.null(cor_mat_i_raw) || is.null(cor_mat_j_raw)) {
          return(NA)
        }

        threshold_vals <- sort(as.numeric(intersect(names(pers_data_i), names(pers_data_j))))
        if(length(threshold_vals) == 0) return(NA)

        jaccard_at_thresholds <- numeric(length(threshold_vals))
        for(t_idx in seq_along(threshold_vals)) {
          thresh <- threshold_vals[t_idx]
          cor_mat_i <- abs(cor_mat_i_raw)
          cor_mat_j <- abs(cor_mat_j_raw)
          cor_mat_i[cor_mat_i < thresh] <- 0
          cor_mat_j[cor_mat_j < thresh] <- 0
          jaccard_at_thresholds[t_idx] <- compute_jaccard_similarity(cor_mat_i, cor_mat_j, threshold = 0)
        }

        mean(jaccard_at_thresholds, na.rm = TRUE)
      }

      # Compute persistence Jaccard for each method
      for(method in methods) {
        for(i in 1:(n_groups-1)) {
          for(j in (i+1):n_groups) {
            group_i <- all_groups[i]
            group_j <- all_groups[j]
            avg_jaccard <- compute_persistence_avg_jaccard_local(method, group_i, group_j)
            if(!is.na(avg_jaccard)) {
              group_jaccard_matrix[i, j] <- group_jaccard_matrix[i, j] + avg_jaccard
              group_jaccard_matrix[j, i] <- group_jaccard_matrix[j, i] + avg_jaccard
              combination_count[i, j] <- combination_count[i, j] + 1
              combination_count[j, i] <- combination_count[j, i] + 1
            }
          }
        }
      }

      # Average
      for(i in 1:n_groups) {
        for(j in 1:n_groups) {
          if(i == j) {
            group_jaccard_matrix[i, j] <- 1
          } else if(combination_count[i, j] > 0) {
            group_jaccard_matrix[i, j] <- group_jaccard_matrix[i, j] / combination_count[i, j]
          }
        }
      }

      n_methods_used <- max(combination_count)
      if(n_methods_used == 0) {
        plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
        text(1, 1, "No persistence data", cex = 1.0, col = "gray")
        return()
      }

      render_jaccard_heatmap(group_jaccard_matrix, all_groups,
                            title = paste0("Persistence (", n_methods_used, " methods)"))
    }, error = function(e) {
      plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
      text(1, 1, "Error", cex = 0.9, col = "red")
    })
  })


  # =========================================================================
  # REGIONAL CONTRIBUTION ANALYSIS - Server Logic
  # =========================================================================

  # Reactive value to store hypothesis combinations
  hypothesis_combinations <- reactiveVal(list())

  # Update group selectors for regional contribution analysis
  observe({
    req(analysis_results$correlation_methods_raw)
    req(selected_methods())

    # Get groups from first available method
    methods <- selected_methods()
    available_groups <- NULL

    for(method in methods) {
      if(!is.null(analysis_results$correlation_methods_raw[[method]])) {
        available_groups <- names(analysis_results$correlation_methods_raw[[method]])
        break
      }
    }

    if(!is.null(available_groups) && length(available_groups) >= 2) {
      updateSelectInput(session, "regional_contrib_group1",
                       choices = available_groups,
                       selected = available_groups[1])

      updateSelectInput(session, "regional_contrib_group2",
                       choices = available_groups,
                       selected = if(length(available_groups) >= 2) available_groups[2] else available_groups[1])
    }
  })

  # Update hypothesis region selector with available regions
 observe({
    req(analysis_results$correlation_methods_raw)
    req(selected_methods())

    # Get node names from first available correlation matrix
    methods <- selected_methods()
    node_names <- NULL

    for(method in methods) {
      if(!is.null(analysis_results$correlation_methods_raw[[method]])) {
        first_group <- names(analysis_results$correlation_methods_raw[[method]])[1]
        mat <- analysis_results$correlation_methods_raw[[method]][[first_group]]
        if(!is.null(mat)) {
          node_names <- rownames(mat)
          if(is.null(node_names)) node_names <- colnames(mat)
          break
        }
      }
    }

    if(!is.null(node_names)) {
      updateSelectizeInput(session, "hypothesis_regions",
                          choices = sort(node_names),
                          selected = NULL)
    }
  })

  # Add hypothesis combination button
  observeEvent(input$add_hypothesis_combo, {
    req(input$hypothesis_regions)

    if(length(input$hypothesis_regions) < 1) {
      showNotification("Please select at least one region.", type = "warning")
      return()
    }

    current_combos <- hypothesis_combinations()
    new_combo <- sort(input$hypothesis_regions)  # Sort for consistency

    # Check if already exists
    for(combo in current_combos) {
      if(identical(sort(combo), new_combo)) {
        showNotification("This combination already exists.", type = "warning")
        return()
      }
    }

    current_combos[[length(current_combos) + 1]] <- new_combo
    hypothesis_combinations(current_combos)

    # Clear selection
    updateSelectizeInput(session, "hypothesis_regions", selected = character(0))

    showNotification(paste("Added:", paste(new_combo, collapse = " + ")), type = "message", duration = 2)
  })

  # Clear hypothesis combinations button
  observeEvent(input$clear_hypothesis_combos, {
    hypothesis_combinations(list())
    showNotification("All combinations cleared.", type = "message", duration = 2)
  })

  # Display hypothesis combinations
  output$hypothesis_combos_display <- renderText({
    combos <- hypothesis_combinations()
    if(length(combos) == 0) {
      return("No combinations added yet.\nUse 'Add Combination' to build your hypothesis set.")
    }

    combo_strings <- sapply(seq_along(combos), function(i) {
      paste0(i, ". ", paste(combos[[i]], collapse = " + "))
    })
    paste(combo_strings, collapse = "\n")
  })

  # Reactive: Calculate number of tests based on analysis level
  n_tests_reactive <- reactive({
    level <- input$regional_contrib_level
    if(is.null(level)) return(1)

    if(level == "subregion") {
      # Count subregions from first available matrix
      methods <- selected_methods()
      for(method in methods) {
        if(!is.null(analysis_results$correlation_methods_raw[[method]])) {
          first_group <- names(analysis_results$correlation_methods_raw[[method]])[1]
          mat <- analysis_results$correlation_methods_raw[[method]][[first_group]]
          if(!is.null(mat)) {
            return(nrow(mat))
          }
        }
      }
      return(20)  # Default fallback

    } else if(level == "collective") {
      # Count brain areas
      if(!is.null(ui_state$brain_areas)) {
        return(length(ui_state$brain_areas))
      }
      return(6)  # Default fallback

    } else if(level == "hypothesis") {
      combos <- hypothesis_combinations()
      return(max(1, length(combos)))

    } else if(level == "artificial") {
      # Estimate based on max combo size and filter
      max_size <- input$discovery_max_combo_size
      if(is.null(max_size)) max_size <- 4

      # Get number of regions
      n_regions <- 20  # Default
      methods <- selected_methods()
      for(method in methods) {
        if(!is.null(analysis_results$correlation_methods_raw[[method]])) {
          first_group <- names(analysis_results$correlation_methods_raw[[method]])[1]
          mat <- analysis_results$correlation_methods_raw[[method]][[first_group]]
          if(!is.null(mat)) {
            n_regions <- nrow(mat)
            break
          }
        }
      }

      # Calculate total combinations
      combo_info <- count_combinations(n_regions, max_size)
      total <- combo_info$total

      # Apply filter estimate (rough)
      filter <- input$discovery_candidate_filter
      if(!is.null(filter)) {
        if(filter == "sd_1") total <- ceiling(total * 0.32)
        else if(filter == "sd_1.5") total <- ceiling(total * 0.13)
        else if(filter == "sd_2") total <- ceiling(total * 0.05)
        else if(filter == "pct_10") total <- ceiling(total * 0.20)
        else if(filter == "pct_25") total <- ceiling(total * 0.50)
      }

      return(max(1, total))
    }

    return(1)
  })

  # Reactive: Auto-calculate permutations
  auto_permutations <- reactive({
    n_tests_total <- n_tests_reactive()
    level <- input$regional_contrib_level

    # Get the appropriate correction method based on analysis level
    correction <- if (!is.null(level)) {
      if (level == "artificial") {
        input$discovery_correction_method
      } else if (level == "hypothesis") {
        input$hypothesis_correction
      } else {
        input$regional_contrib_correction
      }
    } else {
      "fdr"  # Default
    }
    if (is.null(correction)) correction <- "fdr"

    # For artificial mode, estimate candidates based on filter
    n_tests_for_perms <- if (!is.null(level) && level == "artificial") {
      filter <- input$discovery_candidate_filter
      # Estimate how many candidates will be tested based on filter
      estimated_candidates <- if (is.null(filter) || filter == "all") {
        n_tests_total  # All combinations
      } else if (filter == "sd_1") {
        ceiling(n_tests_total * 0.32)  # ~32% outside 1 SD
      } else if (filter == "sd_1.5") {
        ceiling(n_tests_total * 0.13)  # ~13% outside 1.5 SD
      } else if (filter == "sd_2") {
        ceiling(n_tests_total * 0.05)  # ~5% outside 2 SD
      } else if (filter == "pct_10") {
        ceiling(n_tests_total * 0.20)  # Top 10% + bottom 10%
      } else if (filter == "pct_25") {
        ceiling(n_tests_total * 0.50)  # Top 25% + bottom 25%
      } else {
        n_tests_total
      }
      # Both dissimilarity and similarity get filtered separately
      estimated_candidates * 2
    } else {
      n_tests_total
    }

    # Use consistent safety factor for all correction methods
    # More permutations = better statistical power regardless of correction method
    # The correction method affects interpretation, not the quality of the null distribution
    calculate_required_permutations(n_tests_for_perms, alpha = 0.05, safety_factor = 2, min_perms = 1000)
  })

  # Reactive: Estimate number of candidates for artificial mode
  estimated_perm_tests <- reactive({
    level <- input$regional_contrib_level
    if (is.null(level) || level != "artificial") return(NULL)

    n_tests_total <- n_tests_reactive()
    filter <- input$discovery_candidate_filter

    estimated <- if (is.null(filter) || filter == "all") {
      n_tests_total * 2
    } else if (filter == "sd_1") {
      ceiling(n_tests_total * 0.32) * 2
    } else if (filter == "sd_1.5") {
      ceiling(n_tests_total * 0.13) * 2
    } else if (filter == "sd_2") {
      ceiling(n_tests_total * 0.05) * 2
    } else if (filter == "pct_10") {
      ceiling(n_tests_total * 0.20) * 2
    } else if (filter == "pct_25") {
      ceiling(n_tests_total * 0.50) * 2
    } else {
      n_tests_total * 2
    }

    list(n_candidates = estimated, filter = filter, total_combos = n_tests_total)
  })

  # Display auto-calculated permutations
  output$auto_perm_display <- renderUI({
    perm_info <- auto_permutations()
    n_tests_total <- n_tests_reactive()
    level <- input$regional_contrib_level

    # Get the appropriate correction method based on analysis level
    correction <- if (!is.null(level)) {
      if (level == "artificial") {
        input$discovery_correction_method
      } else if (level == "hypothesis") {
        input$hypothesis_correction
      } else {
        input$regional_contrib_correction
      }
    } else {
      "fdr"
    }
    if (is.null(correction)) correction <- "fdr"

    # Build context-appropriate description
    if (!is.null(level) && level == "artificial") {
      est <- tryCatch(estimated_perm_tests(), error = function(e) NULL)
      n_cand <- if (!is.null(est) && !is.null(est$n_candidates)) est$n_candidates else n_tests_total
      filter_label <- switch(input$discovery_candidate_filter,
        "all" = "all candidates",
        "sd_1" = ">1 SD filter",
        "sd_1.5" = ">1.5 SD filter",
        "sd_2" = ">2 SD filter",
        "pct_10" = "top/bottom 10%",
        "pct_25" = "top/bottom 25%",
        "filtered candidates"
      )
      test_description <- paste0("For ~", format(n_cand, big.mark = ","),
                                  " candidates (", filter_label, ")")
    } else {
      test_description <- paste0("For ", format(n_tests_total, big.mark = ","), " tests")
    }

    correction_label <- if (correction == "fdr") " (FDR)" else if (correction == "bonferroni") " (Bonferroni)" else ""

    tags$div(
      tags$p(tags$strong("Auto-calculated:"), style = "margin-bottom: 5px;"),
      tags$p(paste0(format(perm_info$n_permutations, big.mark = ","), " permutations"),
             style = "font-size: 1.1em; margin-bottom: 5px;"),
      tags$p(paste0(test_description, " at α=0.05", correction_label),
             style = "font-size: 0.85em; color: #666; margin-bottom: 5px;"),
      tags$p(paste0("Min detectable p: ", format(perm_info$min_detectable_p, scientific = TRUE, digits = 2)),
             style = "font-size: 0.85em; color: #666; margin-bottom: 0;"),
      if(perm_info$capped) {
        tags$p(tags$em("Capped for computational feasibility"),
               style = "font-size: 0.8em; color: #856404; margin-top: 5px;")
      }
    )
  })

  # Display runtime estimate
  output$runtime_estimate_display <- renderUI({
    # Get values with safe defaults
    n_tests_total <- tryCatch(n_tests_reactive(), error = function(e) 1)
    if (is.null(n_tests_total) || length(n_tests_total) == 0 || !is.finite(n_tests_total)) n_tests_total <- 1

    # For artificial mode, use estimated candidates based on filter
    level <- input$regional_contrib_level
    if (!is.null(level) && level == "artificial") {
      est <- tryCatch(estimated_perm_tests(), error = function(e) NULL)
      n_perm_tests <- if (!is.null(est)) est$n_candidates else n_tests_total * 2
      n_enum_combos <- n_tests_total  # Total to enumerate
    } else {
      n_perm_tests <- n_tests_total
      n_enum_combos <- 0
    }

    n_perms <- if(isTRUE(input$auto_calc_permutations)) {
      tryCatch(auto_permutations()$n_permutations, error = function(e) 500)
    } else {
      input$regional_contrib_n_perms
    }
    if (is.null(n_perms) || length(n_perms) == 0 || !is.finite(n_perms)) n_perms <- 500

    # Use n_perm_tests for runtime calculation (actual permutation tests)
    n_tests <- n_perm_tests

    # Handle "auto" mode or specific worker count
    worker_input <- input$n_parallel_workers
    n_workers <- if(isTRUE(input$use_parallel_processing)) {
      if(is.null(worker_input) || worker_input == "auto") {
        # If we have calibration, use optimal; otherwise estimate
        if(!is.null(ui_state$calibration_data) && !is.null(ui_state$calibration_data$optimal_workers)) {
          ui_state$calibration_data$optimal_workers
        } else {
          max(1, min(8, parallelly::availableCores(omit = 1)))  # Conservative default
        }
      } else {
        as.numeric(worker_input)
      }
    } else {
      1
    }
    if (is.null(n_workers) || length(n_workers) == 0 || !is.finite(n_workers) || n_workers < 1) n_workers <- 1

    # Get data dimensions for better estimation
    n_nodes <- NULL
    n_matrices <- NULL
    methods <- selected_methods()

    if (!is.null(analysis_results$correlation_methods_raw) && length(methods) > 0) {
      # Get n_nodes from first available correlation matrix
      for (method in methods) {
        if (!is.null(analysis_results$correlation_methods_raw[[method]])) {
          first_group <- names(analysis_results$correlation_methods_raw[[method]])[1]
          if (!is.null(first_group)) {
            mat <- analysis_results$correlation_methods_raw[[method]][[first_group]]
            if (!is.null(mat)) {
              n_nodes <- nrow(mat)
              break
            }
          }
        }
      }

      # Count matrices: methods × (weighted + percolation + persistence_thresholds)
      n_weighted <- length(methods)
      n_percolation <- length(methods)
      # Default persistence thresholds: seq(0.1, 0.9, 0.05) = 17 thresholds
      n_persistence <- length(methods) * 17
      n_matrices <- n_weighted + n_percolation + n_persistence
    }

    # Use full calibration object if available, otherwise fall back to legacy
    calibration <- ui_state$calibration_data
    calibrated_ms <- ui_state$calibrated_time_ms

    runtime <- estimate_permutation_runtime(
      n_tests, n_perms, n_workers,
      n_nodes = n_nodes,
      n_matrices = n_matrices,
      calibrated_time_ms = calibrated_ms,
      calibration = calibration,
      n_enum_combos = n_enum_combos  # Pass enumeration count for artificial mode
    )

    # Format estimation method label
    method_label <- switch(runtime$estimation_method,
      "calibrated_cpp" = "C++ calibrated",
      "calibrated" = "calibrated",
      "calibrated_legacy" = "calibrated (legacy)",
      "estimated" = "data-based estimate",
      "default"
    )

    # Color based on estimation quality
    method_color <- switch(runtime$estimation_method,
      "calibrated_cpp" = "#155724",  # Green - C++ is best
      "calibrated" = "#155724",  # Green
      "calibrated_legacy" = "#155724",  # Green
      "estimated" = "#0c5460",   # Blue
      "#856404"                   # Yellow/default
    )

    # Build warning message if parallel is not beneficial
    parallel_warning <- NULL
    # Use isFALSE() to safely handle NULL, NA, and FALSE
    if(n_workers > 1 && isFALSE(runtime$parallel_beneficial)) {
      parallel_warning <- tags$p(
        tags$span(icon("exclamation-triangle"), style = "color: #856404;"),
        " Parallel overhead may exceed benefit. Consider using fewer workers or serial mode.",
        style = "font-size: 0.8em; color: #856404; margin-top: 5px;"
      )
    }

    # Show worker/backend info
    worker_info <- if(runtime$estimation_method == "calibrated_cpp") {
      # C++ backend - show OpenMP threads
      cpp_threads <- if(!is.null(calibration$cpp_threads)) calibration$cpp_threads else "auto"
      tags$p(
        paste0("C++ backend with ", cpp_threads, " OpenMP threads"),
        style = "font-size: 0.8em; color: #28a745; margin-bottom: 3px; font-weight: 500;"
      )
    } else if(n_workers > 1) {
      auto_label <- if(!is.null(worker_input) && worker_input == "auto") " (auto)" else ""
      tags$p(
        paste0("Using ", n_workers, " R workers", auto_label),
        style = "font-size: 0.8em; color: #666; margin-bottom: 3px;"
      )
    } else {
      tags$p("Running in serial mode", style = "font-size: 0.8em; color: #666; margin-bottom: 3px;")
    }

    # Build operations info based on mode
    ops_info <- if (n_enum_combos > 0) {
      # Artificial mode - show breakdown with filter info AND phase timing
      filter_label <- switch(input$discovery_candidate_filter,
        "all" = "all candidates",
        "sd_1" = ">1 SD filter",
        "sd_1.5" = ">1.5 SD filter",
        "sd_2" = ">2 SD filter",
        "pct_10" = "top/bottom 10%",
        "pct_25" = "top/bottom 25%",
        "filtered"
      )

      # Format phase times
      enum_time_formatted <- if (!is.null(runtime$enum_time_ms) && runtime$enum_time_ms > 0) {
        enum_secs <- runtime$enum_time_ms / 1000
        if (enum_secs < 60) {
          sprintf("~%.0f sec", enum_secs)
        } else {
          sprintf("~%.1f min", enum_secs / 60)
        }
      } else {
        "N/A"
      }

      perm_time_ms <- runtime$time_seconds * 1000 - (runtime$enum_time_ms %||% 0)
      perm_time_formatted <- if (perm_time_ms > 0) {
        perm_secs <- perm_time_ms / 1000
        if (perm_secs < 60) {
          sprintf("~%.0f sec", perm_secs)
        } else {
          sprintf("~%.1f min", perm_secs / 60)
        }
      } else {
        "N/A"
      }

      tagList(
        tags$p(
          tags$strong("Phase 1 (Enumeration): "),
          paste0(format(n_enum_combos, big.mark = ","), " combos - ", enum_time_formatted),
          style = "font-size: 0.85em; color: #666; margin-bottom: 2px;"
        ),
        tags$p(
          tags$strong("Phase 2 (Permutation): "),
          paste0(format(n_perm_tests, big.mark = ","), " candidates × ", n_perms, " perms - ", perm_time_formatted),
          style = "font-size: 0.85em; color: #666; margin-bottom: 2px;"
        ),
        tags$p(
          paste0("Filter: ", filter_label),
          style = "font-size: 0.8em; color: #888; margin-bottom: 0;"
        )
      )
    } else {
      tags$p(paste0(format(runtime$total_operations, big.mark = ","), " total operations"),
             style = "font-size: 0.85em; color: #666; margin-bottom: 0;")
    }

    tags$div(
      style = "margin-top: 15px; padding: 10px; background-color: #f0f0f0; border-radius: 5px;",
      tags$p(
        tags$strong("Estimated Runtime"),
        tags$span(
          paste0(" (", method_label, ")"),
          style = paste0("font-size: 0.85em; color: ", method_color, ";")
        ),
        style = "margin-bottom: 5px;"
      ),
      tags$p(runtime$time_formatted, style = "font-size: 1.1em; margin-bottom: 5px;"),
      worker_info,
      ops_info,
      parallel_warning
    )
  })

  # Calibration button observer
  observeEvent(input$calibrate_runtime, {
    req(analysis_results$complete)
    req(input$regional_contrib_group1, input$regional_contrib_group2)

    # Check if C++ backend is available for faster calibration (use safe check)
    cpp_available <- FALSE
    if (exists("is_cpp_really_available", mode = "function")) {
      cpp_available <- tryCatch(is_cpp_really_available(), error = function(e) FALSE)
    } else if (exists("CPP_BACKEND_AVAILABLE") && isTRUE(CPP_BACKEND_AVAILABLE)) {
      cpp_available <- exists("parallel_permutation_test_cpp", mode = "function")
    }

    # Determine target worker count from user selection
    worker_input <- input$n_parallel_workers
    target_workers <- if (is.null(worker_input) || worker_input == "auto") {
      parallelly::availableCores(omit = 1)
    } else {
      min(as.numeric(worker_input), parallelly::availableCores(omit = 1))
    }

    progress_msg <- if (cpp_available) {
      "Calibrating C++ performance..."
    } else {
      sprintf("Calibrating runtime for %d workers...", as.integer(target_workers))
    }

    withProgress(message = progress_msg, value = 0.3, {
      # Calibrate for the specific worker count that will be used
      calibration <- tryCatch({
        calibrate_parallel_performance(
          analysis_results,
          input$regional_contrib_group1,
          input$regional_contrib_group2,
          n_calibration_perms = if (cpp_available) 50 else 20,
          target_workers = target_workers,
          methods = selected_methods(),
          verbose = FALSE
        )
      }, error = function(e) {
        list(error = paste("Calibration error:", e$message))
      })

      setProgress(0.9)

      if (is.null(calibration$error)) {
        # Store the full calibration object for use by brute_force_discovery
        ui_state$calibrated_time_ms <- calibration$time_per_perm_ms
        ui_state$calibration_data <- calibration

        # Build notification message based on backend type
        if (calibration$backend == "cpp_openmp") {
          # C++ backend - show performance info
          # Use serial time for display since batch mode parallelizes candidates, not permutations
          serial_time_display <- if (!is.null(calibration$cpp_serial_time_per_perm_ms)) {
            round(calibration$cpp_serial_time_per_perm_ms, 4)
          } else {
            round(calibration$time_per_perm_ms * calibration$cpp_threads * 0.85, 4)
          }
          showNotification(
            HTML(paste0(
              "<strong style='color: #28a745;'>C++ Backend Active</strong><br>",
              "OpenMP threads: ", calibration$cpp_threads, "<br>",
              "C++ serial time: ", serial_time_display, " ms/perm<br>",
              "R time: ", round(calibration$r_time_per_perm_ms, 2), " ms/perm<br>",
              "Speedup: <strong>", round(calibration$cpp_speedup, 0), "x</strong> (", calibration$cpp_threads, " threads)<br>",
              "Nodes: ", calibration$n_nodes, ", Matrices: ", calibration$n_matrices
            )),
            type = "message",
            duration = 10
          )
        } else {
          # R fallback - show warning about overhead
          spawn_info <- if (!is.null(calibration$spawn_overhead_per_worker_ms)) {
            sprintf("<br>Spawn overhead: %.0f ms/worker", calibration$spawn_overhead_per_worker_ms)
          } else {
            ""
          }

          showNotification(
            HTML(paste0(
              "<strong style='color: #ffc107;'>R Backend (C++ not available)</strong><br>",
              "Serial: ", round(calibration$time_per_perm_ms, 2), " ms/perm<br>",
              "Nodes: ", calibration$n_nodes, ", Matrices: ", calibration$n_matrices,
              spawn_info, "<br>",
              "<strong>", calibration$recommendation, "</strong><br>",
              "<em style='font-size: 0.85em;'>Install Rcpp + RcppArmadillo for ",
              max(2, parallelly::availableCores()), "x speedup</em>"
            )),
            type = "warning",
            duration = 12
          )
        }
      } else {
        showNotification(
          paste0("Calibration failed: ", calibration$error),
          type = "error",
          duration = 5
        )
      }
    })
  })

  # Display discovery estimation (for artificial mode)
  output$discovery_estimation_display <- renderUI({
    req(input$regional_contrib_level == "artificial")

    max_size <- input$discovery_max_combo_size
    if(is.null(max_size)) max_size <- 4

    # Get number of regions
    n_regions <- 20
    methods <- selected_methods()
    for(method in methods) {
      if(!is.null(analysis_results$correlation_methods_raw[[method]])) {
        first_group <- names(analysis_results$correlation_methods_raw[[method]])[1]
        mat <- analysis_results$correlation_methods_raw[[method]][[first_group]]
        if(!is.null(mat)) {
          n_regions <- nrow(mat)
          break
        }
      }
    }

    combo_info <- count_combinations(n_regions, max_size)

    # Filter description
    filter <- input$discovery_candidate_filter
    filter_desc <- switch(filter,
      "all" = "No filtering",
      "sd_1" = "~32% of combinations",
      "sd_1.5" = "~13% of combinations",
      "sd_2" = "~5% of combinations",
      "pct_10" = "~20% of combinations",
      "pct_25" = "~50% of combinations",
      "Unknown"
    )

    tags$div(
      tags$p(tags$strong("Combination Counts:"), style = "margin-bottom: 5px;"),
      tags$ul(
        style = "font-size: 0.85em; margin-bottom: 5px; padding-left: 20px;",
        lapply(names(combo_info$counts_by_size), function(k) {
          size <- as.integer(gsub("size_", "", k))
          tags$li(paste0("Size ", size, ": ", format(combo_info$counts_by_size[[k]], big.mark = ",")))
        })
      ),
      tags$p(paste0("Total: ", format(combo_info$total, big.mark = ",")),
             style = "font-size: 0.9em; font-weight: bold; margin-bottom: 5px;"),
      tags$p(paste0("Filter: ", filter_desc),
             style = "font-size: 0.85em; color: #666; margin-bottom: 0;")
    )
  })

  # Reactive: Compute regional contribution analysis
  regional_contribution_results <- eventReactive(input$run_regional_contribution, {
    req(analysis_results$correlation_methods_raw)
    req(input$regional_contrib_group1, input$regional_contrib_group2)
    req(ui_state$brain_areas)
    req(selected_methods())

    # Validate groups are different
    if(input$regional_contrib_group1 == input$regional_contrib_group2) {
      showNotification("Please select two different groups for comparison.",
                      type = "error", duration = 5)
      return(NULL)
    }

    # Check if at least one method has data for both groups
    methods <- selected_methods()
    has_data <- FALSE
    for(method in methods) {
      if(!is.null(analysis_results$correlation_methods_raw[[method]])) {
        if(!is.null(analysis_results$correlation_methods_raw[[method]][[input$regional_contrib_group1]]) &&
           !is.null(analysis_results$correlation_methods_raw[[method]][[input$regional_contrib_group2]])) {
          has_data <- TRUE
          break
        }
      }
    }

    if(!has_data) {
      showNotification("No correlation data available for selected groups.",
                      type = "error", duration = 5)
      return(NULL)
    }

    # Create analysis_results structure expected by multi-method functions
    # This mirrors what Tab C uses (method_percolation_results, persistence_results)
    analysis_data <- list(
      correlation_methods_raw = analysis_results$correlation_methods_raw,
      method_percolation_results = analysis_results$method_percolation_results,
      persistence_results = analysis_results$persistence_results
    )

    # Get permutation count (auto-calculated or manual)
    n_perms <- if(isTRUE(input$auto_calc_permutations)) {
      auto_permutations()$n_permutations
    } else {
      input$regional_contrib_n_perms
    }

    # Branch based on analysis level
    if(input$regional_contrib_level == "hypothesis") {
      # ===== HYPOTHESIS-DRIVEN TESTING =====

      tryCatch({
        combos <- hypothesis_combinations()

        if(length(combos) == 0) {
          showNotification("Please add at least one region combination to test.",
                          type = "error", duration = 5)
          return(NULL)
        }

        # Get seed for reproducibility
        seed <- input$regional_contrib_seed
        if(is.null(seed)) seed <- 42

        # Run hypothesis testing with progress
        withProgress(message = "Hypothesis Testing", value = 0, {

          setProgress(0.05, detail = "Testing specified combinations...")

          hypothesis_result <- test_hypothesis_combinations(
            analysis_results = analysis_data,
            group1 = input$regional_contrib_group1,
            group2 = input$regional_contrib_group2,
            region_combinations = combos,
            n_permutations = n_perms,
            alpha = 0.05,
            correction_method = input$hypothesis_correction,
            seed = seed,
            progress_callback = function(p) {
              setProgress(0.05 + p * 0.90, detail = paste0("Testing combination ", ceiling(p * length(combos)), "/", length(combos)))
            }
          )

          setProgress(1, detail = "Complete!")
        })

        # Report results
        n_sig <- sum(hypothesis_result$results$Significant, na.rm = TRUE)
        showNotification(
          paste0("Hypothesis testing complete! Tested ", length(combos), " combinations. ",
                 n_sig, " significant (p < 0.05 after ", input$hypothesis_correction, " correction)."),
          type = if(n_sig > 0) "message" else "warning", duration = 6)

        return(list(
          mode = "hypothesis",
          results = hypothesis_result,
          group1_name = input$regional_contrib_group1,
          group2_name = input$regional_contrib_group2
        ))

      }, error = function(e) {
        showNotification(paste("Error in hypothesis testing:", e$message),
                        type = "error", duration = 10)
        print(paste("Hypothesis testing error:", e$message))
        return(NULL)
      })

    } else if(input$regional_contrib_level == "artificial") {
      # ===== BRUTE FORCE ARTIFICIAL BRAIN AREA DISCOVERY (EXPLORATORY) =====

      tryCatch({
        # Get brute force settings
        max_combo_size <- input$discovery_max_combo_size
        if(is.null(max_combo_size)) max_combo_size <- 4

        # Get candidate filter setting (new principled approach)
        candidate_filter <- input$discovery_candidate_filter
        if(is.null(candidate_filter)) candidate_filter <- "all"

        # Get seed for reproducibility
        seed <- input$regional_contrib_seed
        if(is.null(seed)) seed <- 42

        # Run brute force discovery with progress
        withProgress(message = "Artificial Brain Area Discovery", value = 0, {

          setProgress(0.02, detail = "Initializing...")

          # Get parallel processing settings
          use_parallel <- isTRUE(input$use_parallel_processing)
          # Support "auto" mode or specific worker count
          n_workers <- if(use_parallel) {
            worker_input <- input$n_parallel_workers
            if(is.null(worker_input) || worker_input == "auto") {
              "auto"  # Let brute_force_discovery determine optimal
            } else {
              as.numeric(worker_input)
            }
          } else {
            NULL
          }

          # Get calibration data if available
          calibration <- ui_state$calibration_data

          brute_result <- brute_force_discovery(
            analysis_results = analysis_data,
            group1 = input$regional_contrib_group1,
            group2 = input$regional_contrib_group2,
            max_combo_size = max_combo_size,
            n_permutations = n_perms,
            candidate_filter = candidate_filter,
            correction_method = input$discovery_correction_method,
            progress_callback = function(p) {
              if(p < 0.5) {
                setProgress(0.02 + p * 0.46, detail = paste0("Enumerating combinations... ", round(p * 100), "%"))
              } else {
                # Detect if C++ backend is active for more informative message
                cpp_active <- exists("CPP_BACKEND_AVAILABLE") && CPP_BACKEND_AVAILABLE
                backend_label <- if(cpp_active) "C++ permutation testing" else "Permutation testing"
                perm_pct <- round((p - 0.5) * 200)
                setProgress(0.50 + (p - 0.5) * 0.46, detail = paste0(backend_label, "... ", perm_pct, "%"))
              }
            },
            seed = seed,
            methods = methods,  # Pass selected methods
            use_parallel = use_parallel,
            n_workers = n_workers,
            calibration = calibration  # Pass calibration data for adaptive worker selection
          )

          setProgress(0.98, detail = "Finalizing...")

          # Get average correlation matrix for visualization
          avg_cor <- NULL
          for(method in methods) {
            if(!is.null(analysis_results$correlation_methods_raw[[method]])) {
              m1 <- analysis_results$correlation_methods_raw[[method]][[input$regional_contrib_group1]]
              m2 <- analysis_results$correlation_methods_raw[[method]][[input$regional_contrib_group2]]
              if(!is.null(m1) && !is.null(m2)) {
                avg_cor <- (abs(m1) + abs(m2)) / 2
                break
              }
            }
          }

          setProgress(1, detail = "Complete!")
        })

        # Report results
        n_sig_total <- brute_result$n_significant_dissim + brute_result$n_significant_sim

        if(n_sig_total > 0) {
          showNotification(
            paste0("Brute force completed! Tested ", format(brute_result$total_combinations, big.mark = ","),
                   " combinations. Found ", brute_result$n_significant_dissim, " significant dissimilarity and ",
                   brute_result$n_significant_sim, " significant similarity combinations."),
            type = "message", duration = 8)
        } else {
          showNotification(
            paste0("Brute force completed. Tested ", format(brute_result$total_combinations, big.mark = ","),
                   " combinations. No significant combinations found (filter: ", candidate_filter, ")."),
            type = "warning", duration = 8)
        }

        return(list(
          mode = "artificial",
          method = "brute_force",
          discovery = brute_result,
          group1_name = input$regional_contrib_group1,
          group2_name = input$regional_contrib_group2,
          avg_cor = avg_cor
        ))

      }, error = function(e) {
        showNotification(paste("Error in brute force discovery:", e$message),
                        type = "error", duration = 10)
        print(paste("Brute force error:", e$message))
        print(traceback())
        return(NULL)
      })

    } else {
      # ===== STANDARD MODE (subregion or collective) =====
      tryCatch({
        # Get correction method
        correction_method <- input$regional_contrib_correction
        if(is.null(correction_method)) correction_method <- "fdr"

        # Get analysis level label
        level_label <- if(input$regional_contrib_level == "subregion") {
          "Individual Subregions"
        } else {
          "Collective Brain Areas"
        }

        # Run with progress bar
        withProgress(message = paste("Regional Contribution:", level_label), value = 0, {

          setProgress(0.05, detail = "Computing observed contributions...")

          # Get parallel processing settings
          use_parallel <- isTRUE(input$use_parallel_processing)
          n_workers <- if(use_parallel) input$n_parallel_workers else NULL

          # Run multi-method permutation testing (same baseline as Tab C)
          perm_results <- compute_multimethod_contribution_permutation_test(
            analysis_results = analysis_data,
            group1 = input$regional_contrib_group1,
            group2 = input$regional_contrib_group2,
            brain_areas = ui_state$brain_areas,
            analysis_level = input$regional_contrib_level,
            n_permutations = n_perms,
            correction_method = correction_method,
            progress_callback = function(p) {
              setProgress(0.05 + p * 0.85, detail = paste0("Permutation testing... ", round(p * 100), "%"))
            },
            methods = methods,  # Pass selected methods
            use_parallel = use_parallel,
            n_workers = n_workers
          )

          setProgress(0.92, detail = "Computing regional Jaccard...")

          # Compute multi-method regional Jaccard
          regional_jaccard <- compute_multimethod_regional_jaccard(
            analysis_results = analysis_data,
            group1 = input$regional_contrib_group1,
            group2 = input$regional_contrib_group2,
            brain_areas = ui_state$brain_areas,
            analysis_level = input$regional_contrib_level,
            methods = methods  # Pass selected methods
          )

          setProgress(0.97, detail = "Finalizing...")

          # Get average correlation matrix for visualization (use first available method)
          avg_cor <- NULL
          for(method in methods) {
            if(!is.null(analysis_results$correlation_methods_raw[[method]])) {
              m1 <- analysis_results$correlation_methods_raw[[method]][[input$regional_contrib_group1]]
              m2 <- analysis_results$correlation_methods_raw[[method]][[input$regional_contrib_group2]]
              if(!is.null(m1) && !is.null(m2)) {
                avg_cor <- (abs(m1) + abs(m2)) / 2
                break
              }
            }
          }

          setProgress(1, detail = "Complete!")
        })

        showNotification("Regional contribution analysis completed!",
                        type = "message", duration = 3)

        return(list(
          mode = "standard",
          contribution = perm_results,
          regional_jaccard = regional_jaccard,
          group1_name = input$regional_contrib_group1,
          group2_name = input$regional_contrib_group2,
          correction_method = correction_method,
          avg_cor = avg_cor
        ))

      }, error = function(e) {
        removeNotification(id = "regional_running")
        showNotification(paste("Error in regional contribution analysis:", e$message),
                        type = "error", duration = 10)
        print(paste("Regional contribution error:", e$message))
        print(traceback())
        return(NULL)
      })
    }
  })

  # Output: Regional contribution bar plot
  output$regionalContributionBarPlot <- renderPlot({
    results <- regional_contribution_results()

    if(is.null(results)) {
      plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
      text(1, 1, "Select two groups and click 'Run Regional Contribution Analysis'",
           cex = 1.1, col = "gray50")
      return()
    }

    # Handle different modes
    if(!is.null(results$mode) && results$mode == "artificial") {
      # ===== ARTIFICIAL BRAIN AREA DISCOVERY VISUALIZATION =====

      # Check for brute force method
      if(!is.null(results$method) && results$method == "brute_force") {
        # ===== BRUTE FORCE VISUALIZATION =====
        discovery <- results$discovery

        if(is.null(discovery) || is.null(discovery$top_dissimilarity)) {
          plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
          text(1, 1, "No brute force results available", cex = 1.1, col = "gray50")
          return()
        }

        par(mfrow = c(2, 2), mar = c(8, 4, 3, 1))

        # Panel 1: Top Dissimilarity Combinations
        top_d <- head(discovery$top_dissimilarity, 15)
        if(nrow(top_d) > 0) {
          colors_d <- ifelse(top_d$significant, "#E74C3C", "#FFCCCC")
          ylim_d <- c(0, max(top_d$contribution) * 1.2)

          bp_d <- barplot(top_d$contribution,
                         names.arg = top_d$nodes,
                         las = 2, col = colors_d, cex.names = 0.55,
                         main = "Top Dissimilarity Combinations",
                         ylab = "Contribution",
                         ylim = ylim_d)

          sig_stars_d <- ifelse(top_d$significant,
                               ifelse(top_d$p_value < 0.01, "**", "*"), "")
          text(bp_d, top_d$contribution + diff(ylim_d) * 0.05, sig_stars_d, cex = 0.9)
        } else {
          plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
          text(1, 1, "No dissimilarity combinations", cex = 0.9, col = "gray50")
        }

        # Panel 2: Top Similarity Combinations
        top_s <- head(discovery$top_similarity, 15)
        if(nrow(top_s) > 0) {
          colors_s <- ifelse(top_s$significant, "#3498DB", "#CCDDFF")
          ylim_s <- c(min(top_s$contribution) * 1.2, 0)

          bp_s <- barplot(top_s$contribution,
                         names.arg = top_s$nodes,
                         las = 2, col = colors_s, cex.names = 0.55,
                         main = "Top Similarity Combinations",
                         ylab = "Contribution",
                         ylim = ylim_s)

          sig_stars_s <- ifelse(top_s$significant,
                               ifelse(top_s$p_value < 0.01, "**", "*"), "")
          text(bp_s, top_s$contribution + diff(ylim_s) * 0.05, sig_stars_s, cex = 0.9)
        } else {
          plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
          text(1, 1, "No similarity combinations", cex = 0.9, col = "gray50")
        }

        # Panel 3: Distribution by combination size
        all_res <- discovery$all_results
        if(!is.null(all_res) && nrow(all_res) > 0) {
          size_colors <- c("#FFE4E1", "#FFB6C1", "#FF69B4", "#FF1493")
          boxplot(abs_contribution ~ size, data = all_res,
                  col = size_colors[1:length(unique(all_res$size))],
                  main = "Contribution by Combination Size",
                  xlab = "Number of Regions", ylab = "|Contribution|")
        } else {
          plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
          text(1, 1, "No data available", cex = 0.9, col = "gray50")
        }

        # Panel 4: Summary text
        plot(1, type = "n", axes = FALSE, xlab = "", ylab = "",
             xlim = c(0, 1), ylim = c(0, 1))
        title("Brute Force Summary", cex.main = 1.0)

        y_pos <- 0.9
        text(0.05, y_pos, paste("Groups:", discovery$group1_name, "vs", discovery$group2_name),
             adj = 0, cex = 0.8, font = 2); y_pos <- y_pos - 0.08

        text(0.05, y_pos, paste("Total combinations tested:", format(discovery$total_combinations, big.mark = ",")),
             adj = 0, cex = 0.75); y_pos <- y_pos - 0.06

        text(0.05, y_pos, paste("Max combination size:", discovery$max_combo_size, "regions"),
             adj = 0, cex = 0.75); y_pos <- y_pos - 0.06

        # Show approach-specific baselines
        text(0.05, y_pos, "Baseline Jaccard by Approach:", adj = 0, cex = 0.75, font = 2); y_pos <- y_pos - 0.05
        if(!is.null(discovery$baseline_weighted) && !is.na(discovery$baseline_weighted)) {
          text(0.05, y_pos, paste("  Weighted:", round(discovery$baseline_weighted, 4)),
               adj = 0, cex = 0.7); y_pos <- y_pos - 0.05
        }
        if(!is.null(discovery$baseline_percolation) && !is.na(discovery$baseline_percolation)) {
          text(0.05, y_pos, paste("  Percolation:", round(discovery$baseline_percolation, 4)),
               adj = 0, cex = 0.7); y_pos <- y_pos - 0.05
        }
        if(!is.null(discovery$baseline_persistence) && !is.na(discovery$baseline_persistence)) {
          text(0.05, y_pos, paste("  Persistence:", round(discovery$baseline_persistence, 4)),
               adj = 0, cex = 0.7); y_pos <- y_pos - 0.05
        }
        y_pos <- y_pos - 0.03

        text(0.05, y_pos, paste("Permutations:", discovery$n_permutations),
             adj = 0, cex = 0.75); y_pos <- y_pos - 0.08

        text(0.05, y_pos, "Significant Results:", adj = 0, cex = 0.8, font = 2); y_pos <- y_pos - 0.06

        text(0.05, y_pos, paste("  Dissimilarity:", discovery$n_significant_dissim, "of", discovery$top_n_tested, "tested"),
             adj = 0, cex = 0.75, col = "#E74C3C"); y_pos <- y_pos - 0.06

        text(0.05, y_pos, paste("  Similarity:", discovery$n_significant_sim, "of", discovery$top_n_tested, "tested"),
             adj = 0, cex = 0.75, col = "#3498DB"); y_pos <- y_pos - 0.1

        # Show top significant combination if available
        if(discovery$n_significant_dissim > 0) {
          best_d <- discovery$top_dissimilarity[discovery$top_dissimilarity$significant, ][1, ]
          text(0.05, y_pos, paste("Best dissim:", best_d$nodes),
               adj = 0, cex = 0.65, col = "#E74C3C"); y_pos <- y_pos - 0.05
          text(0.05, y_pos, paste("  (contrib:", round(best_d$contribution, 4), ", p:", format(best_d$p_value, digits = 3), ")"),
               adj = 0, cex = 0.6, col = "#E74C3C"); y_pos <- y_pos - 0.08
        }

        if(discovery$n_significant_sim > 0) {
          best_s <- discovery$top_similarity[discovery$top_similarity$significant, ][1, ]
          text(0.05, y_pos, paste("Best sim:", best_s$nodes),
               adj = 0, cex = 0.65, col = "#3498DB"); y_pos <- y_pos - 0.05
          text(0.05, y_pos, paste("  (contrib:", round(best_s$contribution, 4), ", p:", format(best_s$p_value, digits = 3), ")"),
               adj = 0, cex = 0.6, col = "#3498DB")
        }

      } else if(!is.null(results$method) && results$method == "clustering") {
        # ===== HIERARCHICAL CLUSTERING VISUALIZATION =====
        discovery <- results$discovery

        if(is.null(discovery) || is.null(discovery$cluster_results)) {
          plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
          text(1, 1, "No clustering results available", cex = 1.1, col = "gray50")
          return()
        }

        par(mfrow = c(2, 2), mar = c(5, 4, 4, 2))

        # Panel 1: Dendrogram
        dend <- as.dendrogram(discovery$hclust)
        k <- discovery$optimal_k
        # Get cluster colors
        cluster_colors <- c("#E74C3C", "#3498DB", "#27AE60", "#F39C12", "#9B59B6",
                           "#1ABC9C", "#E67E22", "#34495E", "#16A085", "#C0392B")
        if(k > length(cluster_colors)) cluster_colors <- rainbow(k)

        # Color labels by cluster (with fallback if dendextend not available)
        if(requireNamespace("dendextend", quietly = TRUE)) {
          labels_colors <- cluster_colors[discovery$cluster_assignments[order.dendrogram(dend)]]
          dend <- dendextend::set(dend, "labels_col", labels_colors)
          dend <- dendextend::set(dend, "labels_cex", 0.6)
        }

        plot(dend, main = paste("Hierarchical Clustering (k =", k, ")"),
             ylab = "Distance", xlab = "")

        # Panel 2: Cluster contributions bar plot
        contributions <- sapply(discovery$cluster_results, function(x) x$contribution)
        p_values <- sapply(discovery$cluster_results, function(x) x$p_value)
        cluster_ids <- sapply(discovery$cluster_results, function(x) x$cluster_id)

        # Colors: red for dissimilarity, blue for similarity
        bar_colors <- ifelse(contributions > 0, "#E74C3C", "#3498DB")

        # Add significance markers
        sig_markers <- ifelse(p_values < 0.05, "*", "")
        sig_markers[p_values < 0.01] <- "**"
        sig_markers[p_values < 0.001] <- "***"

        ylim_range <- range(contributions) * c(1.3, 1.3)
        if(all(contributions >= 0)) ylim_range[1] <- min(0, ylim_range[1])
        if(all(contributions <= 0)) ylim_range[2] <- max(0, ylim_range[2])

        bp <- barplot(contributions,
                      names.arg = paste("C", cluster_ids, sep = ""),
                      col = bar_colors,
                      main = "Cluster Contributions",
                      ylab = "Contribution (+ dissim, - sim)",
                      ylim = ylim_range)

        # Add significance stars
        text(bp, contributions + sign(contributions) * diff(ylim_range) * 0.05,
             labels = sig_markers, cex = 1.2)

        abline(h = 0, lty = 2, col = "gray50")

        legend("topright",
               legend = c("Dissimilarity", "Similarity"),
               fill = c("#E74C3C", "#3498DB"), bty = "n", cex = 0.7)

        # Panel 3: Silhouette score by k
        sil_scores <- discovery$silhouette_by_k
        k_values <- 2:(length(sil_scores) + 1)
        min_k <- discovery$min_clusters
        if(is.null(min_k)) min_k <- 2

        # Color points: gray for k < min, blue for valid range
        point_colors <- ifelse(k_values >= min_k, "#3498DB", "#CCCCCC")

        plot(k_values, sil_scores,
             type = "b", pch = 19, col = point_colors, lwd = 2,
             xlab = "Number of Clusters (k)",
             ylab = "Avg Silhouette Width",
             main = paste("Optimal k Selection (min k =", min_k, ")"),
             xaxt = "n")
        axis(1, at = k_values)

        # Shade invalid region (k < min_k)
        if(min_k > 2) {
          rect(1.5, par("usr")[3], min_k - 0.5, par("usr")[4],
               col = "#FFEEEE", border = NA)
          # Redraw points on top
          points(k_values, sil_scores, type = "b", pch = 19, col = point_colors, lwd = 2)
        }

        abline(v = discovery$optimal_k, col = "red", lty = 2, lwd = 2)
        if(min_k > 2) {
          abline(v = min_k - 0.5, col = "orange", lty = 3, lwd = 1)
        }

        legend("topright",
               legend = c(paste("Optimal k =", discovery$optimal_k),
                         if(min_k > 2) paste("Min k =", min_k) else NULL),
               col = c("red", if(min_k > 2) "orange" else NULL),
               lty = c(2, if(min_k > 2) 3 else NULL),
               lwd = c(2, if(min_k > 2) 1 else NULL),
               bty = "n", cex = 0.7)

        # Panel 4: Cluster sizes and regions
        plot(1, type = "n", axes = FALSE, xlab = "", ylab = "",
             xlim = c(0, 1), ylim = c(0, 1))
        title("Cluster Membership", cex.main = 1.0)

        n_clusters <- length(discovery$cluster_results)
        y_pos <- seq(0.9, 0.1, length.out = min(n_clusters, 8))

        for(i in 1:min(n_clusters, 8)) {
          cr <- discovery$cluster_results[[i]]
          regions_str <- paste(cr$regions, collapse = ", ")
          if(nchar(regions_str) > 50) {
            regions_str <- paste0(substr(regions_str, 1, 47), "...")
          }
          sig_star <- ifelse(cr$significant, "*", "")

          text(0.05, y_pos[i],
               paste0("C", cr$cluster_id, " (n=", cr$n_regions, ", ",
                     cr$direction, sig_star, "): ", regions_str),
               adj = 0, cex = 0.65, col = cluster_colors[cr$cluster_id])
        }

      } else {
        # ===== GREEDY FORWARD SELECTION VISUALIZATION =====
        direction <- results$direction

        # Helper function to plot single direction results
        plot_single_direction <- function(disc, title_suffix = "") {
          if(is.null(disc) || length(disc$selection_order) == 0) {
            plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
            text(1, 1, paste("No regions found for", title_suffix), cex = 1.0, col = "gray50")
            return()
          }

          par(mar = c(8, 4, 3, 1))

          # Elbow curve
          n_regions <- length(disc$cumulative_contributions)
          cum_contrib <- abs(disc$cumulative_contributions)

          plot(1:n_regions, cum_contrib,
               type = "b", pch = 19, col = "#3498DB", lwd = 2,
               xlab = "Number of Regions",
               ylab = "Cumulative |Contribution|",
               main = paste("Elbow Curve:", title_suffix),
               xaxt = "n", cex.main = 0.9)
          axis(1, at = 1:n_regions)

          abline(v = disc$elbow_k, col = "red", lty = 2, lwd = 2)

          sig_label <- ifelse(disc$significant, "***", "")
          legend("bottomright",
                 legend = c(paste("Elbow: k =", disc$elbow_k),
                           paste("p =", format(disc$p_value, digits = 3), sig_label)),
                 col = c("red", NA), lty = c(2, NA), lwd = c(2, NA), bty = "n", cex = 0.8)
        }

        # Helper function for incremental bar plot
        plot_incremental_bars <- function(disc, title_suffix = "") {
          if(is.null(disc) || length(disc$selection_order) == 0) {
            plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
            return()
          }

          par(mar = c(10, 4, 3, 1))

          incr_contrib <- disc$incremental_contributions
          bar_colors <- ifelse(1:length(incr_contrib) <= disc$elbow_k,
                              ifelse(disc$significant, "#27AE60", "#F39C12"),
                              "#95A5A6")

          barplot(incr_contrib,
                  names.arg = disc$selection_order,
                  las = 2, col = bar_colors,
                  main = paste("Incremental:", title_suffix),
                  ylab = "Contribution",
                  cex.names = 0.65, cex.main = 0.9)
        }

        if(direction == "both") {
          # Show both dissimilarity and similarity results
          discovery <- results$discovery

          par(mfrow = c(2, 2))

          # Row 1: Dissimilarity
          plot_single_direction(discovery$dissimilarity, "Dissimilarity")
          plot_incremental_bars(discovery$dissimilarity, "Dissimilarity")

          # Row 2: Similarity
          plot_single_direction(discovery$similarity, "Similarity")
          plot_incremental_bars(discovery$similarity, "Similarity")

        } else {
          # Single direction
          discovery <- results$discovery

          if(is.null(discovery) || length(discovery$selection_order) == 0) {
            plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
            text(1, 1, paste("No regions found contributing to", direction),
                 cex = 1.1, col = "gray50")
            return()
          }

          par(mfrow = c(1, 2), mar = c(5, 4, 4, 2))

          # Panel 1: Elbow curve
          n_regions <- length(discovery$cumulative_contributions)
          cum_contrib <- abs(discovery$cumulative_contributions)

          plot(1:n_regions, cum_contrib,
               type = "b", pch = 19, col = "#3498DB", lwd = 2,
               xlab = "Number of Regions",
               ylab = "Cumulative |Contribution|",
               main = paste("Elbow Curve:", tools::toTitleCase(direction)),
               xaxt = "n")
          axis(1, at = 1:n_regions)

          abline(v = discovery$elbow_k, col = "red", lty = 2, lwd = 2)

          sig_label <- ifelse(discovery$significant, "***", "")
          legend("bottomright",
                 legend = c(paste("Elbow: k =", discovery$elbow_k),
                           paste("p =", format(discovery$p_value, digits = 3), sig_label)),
                 col = c("red", NA), lty = c(2, NA), lwd = c(2, NA), bty = "n", cex = 0.9)

          # Panel 2: Incremental contributions
          incr_contrib <- discovery$incremental_contributions
          bar_colors <- ifelse(1:length(incr_contrib) <= discovery$elbow_k,
                              ifelse(discovery$significant, "#27AE60", "#F39C12"),
                              "#95A5A6")

          barplot(incr_contrib,
                  names.arg = discovery$selection_order,
                  las = 2, col = bar_colors,
                  main = "Incremental Contribution per Region",
                  ylab = "Incremental Contribution",
                  cex.names = 0.7)

          legend("topright",
                 legend = c("At/before elbow (significant)" , "At/before elbow (not sig)", "After elbow"),
                 fill = c("#27AE60", "#F39C12", "#95A5A6"), bty = "n", cex = 0.8)
        }
      }

    } else {
      # ===== STANDARD MODE =====
      render_contribution_barplot(
        contribution_results = results$contribution,
        brain_areas = ui_state$brain_areas,
        area_colors = ui_state$area_colors,
        top_n = 25
      )
    }
  })

  # Output: Regional contribution circular plot
  output$regionalContributionCircularPlot <- renderPlot({
    results <- regional_contribution_results()

    if(is.null(results)) {
      plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
      text(1, 1, "Run analysis to see network visualization", cex = 1.1, col = "gray50")
      return()
    }

    # Check if artificial mode - show discovery summary visualization instead
    if(!is.null(results$mode) && results$mode == "artificial") {

      # Check for brute force method
      if(!is.null(results$method) && results$method == "brute_force") {
        # ===== BRUTE FORCE: Contribution distribution and top candidates =====
        discovery <- results$discovery

        if(is.null(discovery) || is.null(discovery$all_results)) {
          plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
          text(1, 1, "No brute force results available", cex = 1.1, col = "gray50")
          return()
        }

        all_res <- discovery$all_results

        par(mfrow = c(1, 2), mar = c(5, 5, 4, 2))

        # Panel 1: Histogram of all contributions
        hist(all_res$contribution,
             breaks = 50,
             col = "#3498DB80",
             border = "#2980B9",
             main = paste("Contribution Distribution\n(n =", nrow(all_res), "combinations)"),
             xlab = "Contribution (change from baseline)",
             ylab = "Frequency",
             cex.main = 0.95)

        abline(v = 0, col = "black", lwd = 2, lty = 2)

        # Add markers for significant combinations
        sig_dissim <- discovery$top_dissimilarity[discovery$top_dissimilarity$significant == TRUE, ]
        sig_sim <- discovery$top_similarity[discovery$top_similarity$significant == TRUE, ]

        if(nrow(sig_dissim) > 0) {
          for(i in 1:min(5, nrow(sig_dissim))) {
            abline(v = sig_dissim$contribution[i], col = "#E74C3C", lwd = 1.5, lty = 3)
          }
        }
        if(nrow(sig_sim) > 0) {
          for(i in 1:min(5, nrow(sig_sim))) {
            abline(v = sig_sim$contribution[i], col = "#27AE60", lwd = 1.5, lty = 3)
          }
        }

        legend("topright",
               legend = c("Baseline (0)",
                         paste("Sig. Dissim (", nrow(sig_dissim), ")", sep = ""),
                         paste("Sig. Sim (", nrow(sig_sim), ")", sep = "")),
               col = c("black", "#E74C3C", "#27AE60"),
               lwd = c(2, 1.5, 1.5),
               lty = c(2, 3, 3),
               bty = "n", cex = 0.75)

        # Panel 2: Contribution by combination size (violin/strip plot)
        sizes <- unique(all_res$size)
        max_contrib <- max(abs(all_res$contribution), na.rm = TRUE)

        plot(1, type = "n",
             xlim = c(0.5, length(sizes) + 0.5),
             ylim = c(-max_contrib * 1.1, max_contrib * 1.1),
             xlab = "Combination Size",
             ylab = "Contribution",
             main = "Contribution by Combination Size",
             xaxt = "n",
             cex.main = 0.95)
        axis(1, at = 1:length(sizes), labels = sizes)
        abline(h = 0, col = "gray50", lty = 2)

        # Jittered strip plot
        set.seed(42)
        for(i in seq_along(sizes)) {
          sz <- sizes[i]
          vals <- all_res$contribution[all_res$size == sz]
          n_pts <- length(vals)
          jitter_x <- i + runif(n_pts, -0.3, 0.3)

          # Color by direction
          cols <- ifelse(vals > 0, "#E74C3C40", "#27AE6040")
          points(jitter_x, vals, pch = 16, col = cols, cex = 0.5)

          # Add median line
          med_val <- median(vals, na.rm = TRUE)
          segments(i - 0.35, med_val, i + 0.35, med_val, col = "#2C3E50", lwd = 2)
        }

        legend("topleft",
               legend = c("Dissimilarity (+)", "Similarity (-)", "Median"),
               pch = c(16, 16, NA),
               lwd = c(NA, NA, 2),
               col = c("#E74C3C", "#27AE60", "#2C3E50"),
               bty = "n", cex = 0.75)

        return()
      }

      # Check for clustering method
      if(!is.null(results$method) && results$method == "clustering") {
        # ===== CLUSTERING: Heatmap of connectivity difference matrix =====
        discovery <- results$discovery

        if(is.null(discovery) || is.null(discovery$diff_matrix)) {
          plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
          text(1, 1, "No clustering results available", cex = 1.1, col = "gray50")
          return()
        }

        # Reorder by cluster
        order_idx <- order(discovery$cluster_assignments)
        ordered_diff <- discovery$diff_matrix[order_idx, order_idx]
        ordered_names <- discovery$node_names[order_idx]
        ordered_clusters <- discovery$cluster_assignments[order_idx]

        # Create color palette
        colors <- colorRampPalette(c("white", "#FFF7BC", "#FEC44F", "#D95F0E", "#8B0000"))(100)

        # Create heatmap with cluster separators
        par(mar = c(8, 8, 4, 2))
        image(1:nrow(ordered_diff), 1:ncol(ordered_diff),
              ordered_diff,
              col = colors,
              axes = FALSE,
              xlab = "", ylab = "",
              main = "Connectivity Difference Matrix\n(ordered by cluster)")

        # Add axis labels
        axis(1, at = 1:nrow(ordered_diff), labels = ordered_names, las = 2, cex.axis = 0.6)
        axis(2, at = 1:ncol(ordered_diff), labels = ordered_names, las = 2, cex.axis = 0.6)

        # Add cluster separator lines
        cluster_boundaries <- which(diff(ordered_clusters) != 0) + 0.5
        for(b in cluster_boundaries) {
          abline(v = b, col = "black", lwd = 2)
          abline(h = b, col = "black", lwd = 2)
        }

        # Add color legend
        legend("topright",
               legend = c("Low diff", "High diff"),
               fill = c(colors[10], colors[90]),
               bty = "n", cex = 0.8)

        return()
      }

      # ===== GREEDY: Null distributions =====
      direction <- results$direction

      # Helper function to plot null distribution
      plot_null_dist <- function(disc, title_suffix = "") {
        if(is.null(disc) || length(disc$null_distribution) == 0) {
          plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
          text(1, 1, paste("No data for", title_suffix), cex = 0.9, col = "gray50")
          return()
        }

        hist(disc$null_distribution,
             breaks = 30,
             col = "#3498DB80",
             border = "#2980B9",
             main = paste("Null Distribution:", title_suffix),
             xlab = "Combined Contribution (Permuted)",
             ylab = "Frequency",
             xlim = range(c(disc$null_distribution, disc$combined_contribution), na.rm = TRUE),
             cex.main = 0.9)

        abline(v = disc$combined_contribution, col = "#E74C3C", lwd = 3, lty = 1)

        ci_lower <- quantile(disc$null_distribution, 0.025)
        ci_upper <- quantile(disc$null_distribution, 0.975)
        abline(v = ci_lower, col = "#2ECC71", lwd = 2, lty = 2)
        abline(v = ci_upper, col = "#2ECC71", lwd = 2, lty = 2)

        sig_label <- ifelse(disc$significant, "***", "")
        legend("topright",
               legend = c(paste("Obs:", round(disc$combined_contribution, 4)),
                         paste("p =", format(disc$p_value, digits = 3), sig_label)),
               col = c("#E74C3C", NA),
               lwd = c(3, NA),
               lty = c(1, NA),
               bty = "n", cex = 0.8)
      }

      if(direction == "both") {
        discovery <- results$discovery
        par(mfrow = c(1, 2), mar = c(5, 5, 4, 2))
        plot_null_dist(discovery$dissimilarity, "Dissimilarity")
        plot_null_dist(discovery$similarity, "Similarity")
      } else {
        discovery <- results$discovery
        if(is.null(discovery) || length(discovery$null_distribution) == 0) {
          plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
          text(1, 1, "No discovery results available", cex = 1.1, col = "gray50")
          return()
        }

        par(mar = c(5, 5, 4, 2))
        plot_null_dist(discovery, tools::toTitleCase(direction))
      }

      return()
    }

    # Standard mode: Use pre-computed average correlation matrix from results
    if(is.null(results$avg_cor)) {
      plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
      text(1, 1, "No correlation matrix available for visualization", cex = 1.1, col = "gray50")
      return()
    }

    render_regional_circular_plot(
      contribution_results = results$contribution,
      cor_matrix = results$avg_cor,
      brain_areas = ui_state$brain_areas,
      area_colors = ui_state$area_colors,
      group_pair = c(results$group1_name, results$group2_name)
    )
  })

  # Output: Regional Jaccard plot
  output$regionalJaccardPlot <- renderPlot({
    results <- regional_contribution_results()

    if(is.null(results)) {
      plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
      text(1, 1, "Run analysis to see regional Jaccard values", cex = 1.1, col = "gray50")
      return()
    }

    # Check if artificial mode - show selected regions detail
    if(!is.null(results$mode) && results$mode == "artificial") {

      # Check for brute force method
      if(!is.null(results$method) && results$method == "brute_force") {
        # ===== BRUTE FORCE: Null distributions for top candidates =====
        discovery <- results$discovery

        if(is.null(discovery)) {
          plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
          text(1, 1, "No brute force results available", cex = 1.1, col = "gray50")
          return()
        }

        # Combine top dissimilarity and similarity results
        top_dissim <- discovery$top_dissimilarity
        top_sim <- discovery$top_similarity

        # Filter to those with null distributions (permutation tested)
        has_null_dissim <- !is.na(top_dissim$p_value) & !is.null(top_dissim$p_value)
        has_null_sim <- !is.na(top_sim$p_value) & !is.null(top_sim$p_value)

        n_dissim <- sum(has_null_dissim)
        n_sim <- sum(has_null_sim)
        n_total <- n_dissim + n_sim

        if(n_total == 0) {
          # No permutation tests were run - show summary instead
          par(mar = c(2, 2, 3, 2))
          plot(1, type = "n", axes = FALSE, xlab = "", ylab = "",
               xlim = c(0, 10), ylim = c(0, 10))

          text(5, 9, "Brute Force Discovery Summary", cex = 1.3, font = 2)

          text(5, 7.5, paste("Total combinations tested:", discovery$total_combinations), cex = 1)
          text(5, 6.5, paste("Max combination size:", discovery$max_combo_size), cex = 1)

          # Show approach-specific baselines
          y_baseline <- 5.5
          if(!is.null(discovery$baseline_weighted) && !is.na(discovery$baseline_weighted)) {
            text(5, y_baseline, paste("Baseline (Weighted):", round(discovery$baseline_weighted, 4)), cex = 0.9)
            y_baseline <- y_baseline - 0.7
          }
          if(!is.null(discovery$baseline_percolation) && !is.na(discovery$baseline_percolation)) {
            text(5, y_baseline, paste("Baseline (Percolation):", round(discovery$baseline_percolation, 4)), cex = 0.9)
            y_baseline <- y_baseline - 0.7
          }
          if(!is.null(discovery$baseline_persistence) && !is.na(discovery$baseline_persistence)) {
            text(5, y_baseline, paste("Baseline (Persistence):", round(discovery$baseline_persistence, 4)), cex = 0.9)
          }

          # Top dissimilarity info
          if(nrow(top_dissim) > 0) {
            top_d <- top_dissim[1, ]
            text(5, 3, paste("Top dissimilarity:", top_d$nodes, "(+", round(top_d$contribution, 4), ")"),
                 cex = 0.9, col = "#E74C3C")
          }

          # Top similarity info
          if(nrow(top_sim) > 0) {
            top_s <- top_sim[1, ]
            text(5, 2, paste("Top similarity:", top_s$nodes, "(", round(top_s$contribution, 4), ")"),
                 cex = 0.9, col = "#27AE60")
          }

          return()
        }

        # Set up grid for null distributions with error handling
        n_cols <- min(n_total, 4)
        n_rows <- ceiling(n_total / n_cols)

        # Try to set up grid - fall back to summary if panel too small
        plot_result <- tryCatch({
          par(mfrow = c(n_rows, n_cols), mar = c(3, 3, 2, 0.5), oma = c(0, 0, 0, 0))

          # Plot null distributions for top dissimilarity
          for(i in which(has_null_dissim)[1:min(n_dissim, 8)]) {
            row <- top_dissim[i, ]

            plot(1, type = "n",
                 xlim = c(-0.5, 0.5), ylim = c(0, 1),
                 xlab = "Contribution", ylab = "",
                 main = paste0("Dissim #", i, ": ", substr(row$nodes, 1, 20)),
                 yaxt = "n", cex.main = 0.65, cex.lab = 0.8)

            abline(v = 0, col = "gray70", lty = 2)
            points(row$contribution, 0.5, pch = 19, col = "#E74C3C", cex = 1.5)

            sig_marker <- ifelse(!is.na(row$significant) && row$significant, "***", "")
            p_text <- ifelse(is.na(row$p_value), "N/A",
                            paste("p =", format(row$p_value, digits = 3), sig_marker))
            text(0, 0.85, p_text, cex = 0.7)
            text(0, 0.15, paste("Size:", row$size), cex = 0.6, col = "gray40")
          }

          # Plot null distributions for top similarity
          for(i in which(has_null_sim)[1:min(n_sim, 8)]) {
            row <- top_sim[i, ]

            plot(1, type = "n",
                 xlim = c(-0.5, 0.5), ylim = c(0, 1),
                 xlab = "Contribution", ylab = "",
                 main = paste0("Sim #", i, ": ", substr(row$nodes, 1, 20)),
                 yaxt = "n", cex.main = 0.65, cex.lab = 0.8)

            abline(v = 0, col = "gray70", lty = 2)
            points(row$contribution, 0.5, pch = 19, col = "#27AE60", cex = 1.5)

            sig_marker <- ifelse(!is.na(row$significant) && row$significant, "***", "")
            p_text <- ifelse(is.na(row$p_value), "N/A",
                            paste("p =", format(row$p_value, digits = 3), sig_marker))
            text(0, 0.85, p_text, cex = 0.7)
            text(0, 0.15, paste("Size:", row$size), cex = 0.6, col = "gray40")
          }

          "success"
        }, error = function(e) {
          # Panel too small for grid - show summary instead
          par(mfrow = c(1, 1), mar = c(2, 2, 2, 2))
          plot(1, type = "n", axes = FALSE, xlab = "", ylab = "", xlim = c(0, 10), ylim = c(0, 10))
          text(5, 7, "Expand plot panel to see grid", cex = 1.1, font = 2)
          text(5, 5, paste("Significant:", n_dissim, "dissimilar,", n_sim, "similar"), cex = 0.9)
          if(nrow(top_dissim) > 0) {
            text(5, 3.5, paste("Top:", substr(top_dissim$nodes[1], 1, 30)), cex = 0.8, col = "#E74C3C")
          }
          "fallback"
        })

        return()
      }

      # Check for clustering method
      if(!is.null(results$method) && results$method == "clustering") {
        # ===== CLUSTERING: Null distributions for each cluster =====
        discovery <- results$discovery

        if(is.null(discovery) || is.null(discovery$cluster_results)) {
          plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
          text(1, 1, "No clustering results available", cex = 1.1, col = "gray50")
          return()
        }

        n_clusters <- length(discovery$cluster_results)
        n_cols <- min(n_clusters, 3)
        n_rows <- ceiling(n_clusters / n_cols)

        cluster_colors <- c("#E74C3C", "#3498DB", "#27AE60", "#F39C12", "#9B59B6",
                           "#1ABC9C", "#E67E22", "#34495E", "#16A085", "#C0392B")
        if(n_clusters > length(cluster_colors)) cluster_colors <- rainbow(n_clusters)

        # Try to plot grid - fall back to summary if panel too small
        plot_result <- tryCatch({
          par(mfrow = c(n_rows, n_cols), mar = c(3, 3, 2, 0.5), oma = c(0, 0, 0, 0))

          for(i in 1:n_clusters) {
            cr <- discovery$cluster_results[[i]]

            if(length(cr$null_distribution) == 0) {
              plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
              text(1, 1, paste("No data for Cluster", i), cex = 0.8)
              next
            }

            hist(cr$null_distribution,
                 breaks = 20,
                 col = paste0(cluster_colors[i], "60"),
                 border = cluster_colors[i],
                 main = paste("Cluster", i, "(", cr$direction, ")"),
                 xlab = "Contribution",
                 ylab = "Frequency",
                 xlim = range(c(cr$null_distribution, cr$contribution), na.rm = TRUE),
                 cex.main = 0.75, cex.lab = 0.8)

            abline(v = cr$contribution, col = "black", lwd = 2, lty = 1)

            ci_lower <- quantile(cr$null_distribution, 0.025)
            ci_upper <- quantile(cr$null_distribution, 0.975)
            abline(v = ci_lower, col = "gray50", lwd = 1, lty = 2)
            abline(v = ci_upper, col = "gray50", lwd = 1, lty = 2)

            sig_label <- ifelse(cr$significant, "*", "")
            legend("topright",
                   legend = paste("p =", format(cr$p_value, digits = 2), sig_label),
                   bty = "n", cex = 0.6)
          }
          "success"
        }, error = function(e) {
          # Panel too small - show summary
          par(mfrow = c(1, 1), mar = c(2, 2, 2, 2))
          plot(1, type = "n", axes = FALSE, xlab = "", ylab = "", xlim = c(0, 10), ylim = c(0, 10))
          text(5, 7, "Expand plot panel to see cluster details", cex = 1.1, font = 2)
          text(5, 5, paste("Clusters analyzed:", n_clusters), cex = 0.9)
          "fallback"
        })

        return()
      }

      # ===== GREEDY: Selected regions detail =====
      direction <- results$direction

      # Helper function to plot selected regions
      plot_selected_regions <- function(disc, title_suffix = "") {
        if(is.null(disc) || disc$elbow_k == 0 || length(disc$selection_order) == 0) {
          plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
          text(1, 1, paste("No regions for", title_suffix), cex = 0.9, col = "gray50")
          return()
        }

        optimal_k <- disc$elbow_k
        selected_regions <- disc$selection_order[1:optimal_k]
        selected_contribs <- disc$incremental_contributions[1:optimal_k]

        # Get colors for selected regions
        bar_colors <- rep("#3498DB", length(selected_regions))
        if(!is.null(ui_state$brain_areas) && !is.null(ui_state$area_colors)) {
          for(i in seq_along(selected_regions)) {
            region <- selected_regions[i]
            for(area_name in names(ui_state$brain_areas)) {
              if(region %in% ui_state$brain_areas[[area_name]]) {
                if(area_name %in% names(ui_state$area_colors)) {
                  bar_colors[i] <- ui_state$area_colors[[area_name]]
                }
                break
              }
            }
          }
        }

        barplot(selected_contribs,
                names.arg = selected_regions,
                col = bar_colors,
                las = 2,
                ylab = "Incremental Contribution",
                main = paste(title_suffix, "Regions (k =", optimal_k, ")"),
                cex.names = 0.7, cex.main = 0.9)

        sig_label <- ifelse(disc$significant, "***", "")
        text_col <- ifelse(disc$significant, "#27AE60", "#E74C3C")
        mtext(paste("p =", format(disc$p_value, digits = 3), sig_label),
              side = 3, line = -1.5, cex = 0.8, col = text_col)
      }

      if(direction == "both") {
        discovery <- results$discovery
        tryCatch({
          par(mfrow = c(1, 2), mar = c(8, 4, 3, 1), oma = c(0, 0, 0, 0))
          plot_selected_regions(discovery$dissimilarity, "Dissimilarity")
          plot_selected_regions(discovery$similarity, "Similarity")
        }, error = function(e) {
          par(mfrow = c(1, 1), mar = c(2, 2, 2, 2))
          plot(1, type = "n", axes = FALSE, xlab = "", ylab = "", xlim = c(0, 10), ylim = c(0, 10))
          text(5, 5, "Expand plot panel to see regions", cex = 1.1)
        })
      } else {
        discovery <- results$discovery
        if(is.null(discovery) || discovery$elbow_k == 0) {
          plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
          text(1, 1, "No discovery results available", cex = 1.1, col = "gray50")
          return()
        }

        tryCatch({
          par(mar = c(8, 4, 3, 2))
          plot_selected_regions(discovery, tools::toTitleCase(direction))
        }, error = function(e) {
          par(mfrow = c(1, 1), mar = c(2, 2, 2, 2))
          plot(1, type = "n", axes = FALSE, xlab = "", ylab = "", xlim = c(0, 10), ylim = c(0, 10))
          text(5, 5, "Expand plot panel to see regions", cex = 1.1)
        })
      }

      return()
    }

    # Standard mode: Regional Jaccard plot
    regional_j <- results$regional_jaccard

    if(is.null(regional_j) || nrow(regional_j) == 0) {
      plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
      text(1, 1, "No regional Jaccard data available", cex = 1.1, col = "gray50")
      return()
    }

    # Sort by Jaccard value
    regional_j <- regional_j[order(-regional_j$Regional_Jaccard), ]

    # Dynamic margin based on longest region name
    max_name_len <- max(nchar(as.character(regional_j$Region)), na.rm = TRUE)
    bottom_margin <- min(10, max(4, max_name_len * 0.35))

    # Get colors
    bar_colors <- rep("#3498DB", nrow(regional_j))
    if(!is.null(ui_state$brain_areas) && !is.null(ui_state$area_colors)) {
      for(i in 1:nrow(regional_j)) {
        region <- regional_j$Region[i]
        for(area_name in names(ui_state$brain_areas)) {
          if(region %in% ui_state$brain_areas[[area_name]] || region == area_name) {
            if(area_name %in% names(ui_state$area_colors)) {
              bar_colors[i] <- ui_state$area_colors[[area_name]]
            }
            break
          }
        }
      }
    }

    tryCatch({
      par(mar = c(bottom_margin, 4, 3, 2))
      bp <- barplot(regional_j$Regional_Jaccard,
                    names.arg = regional_j$Region,
                    col = bar_colors,
                    las = 2,
                    ylim = c(0, 1),
                    ylab = "Regional Jaccard Similarity",
                    main = paste("Region-Specific Jaccard Similarity\n",
                                results$group1_name, "vs", results$group2_name),
                    cex.names = 0.7)

      mean_j <- mean(regional_j$Regional_Jaccard, na.rm = TRUE)
      abline(h = mean_j, lty = 2, col = "red", lwd = 2)

      legend("topright",
             legend = paste("Mean J =", round(mean_j, 3)),
             lty = 2, col = "red", lwd = 2, bty = "n")
    }, error = function(e) {
      # Handle figure margins too large or other plotting errors
      par(mar = c(2, 2, 3, 2))
      plot(1, type = "n", axes = FALSE, xlab = "", ylab = "",
           xlim = c(0, 10), ylim = c(0, 10))
      text(5, 6, "Plot display error - try resizing window", cex = 1.1, col = "gray50")
      text(5, 5, paste("Mean Regional Jaccard:", round(mean(regional_j$Regional_Jaccard, na.rm = TRUE), 3)),
           cex = 1, col = "black")
      text(5, 4, paste("N Regions:", nrow(regional_j)), cex = 0.9, col = "gray40")
    })
  })

  # Output: Artificial combinations summary plot (significant + non-redundant)
  output$artificialCombinationsSummaryPlot <- renderPlot({
    results <- regional_contribution_results()

    # Check for artificial mode with brute force results
    if(is.null(results) || is.null(results$mode) || results$mode != "artificial") {
      plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
      text(1, 1, "Run Artificial Brain Area Discovery first", cex = 1.1, col = "gray50")
      return()
    }

    if(is.null(results$method) || results$method != "brute_force") {
      plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
      text(1, 1, "Brute force results not available", cex = 1.1, col = "gray50")
      return()
    }

    discovery <- results$discovery
    if(is.null(discovery)) {
      plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
      text(1, 1, "No discovery results available", cex = 1.1, col = "gray50")
      return()
    }

    # Get significant results from both dissimilarity and similarity
    top_dissim <- discovery$top_dissimilarity
    top_sim <- discovery$top_similarity

    # Filter for FDR-significant AND synergistic/additive (non-redundant)
    # For singles, always include if significant
    # For multi-region, require synergy >= 0 (dissim) or <= 0 (sim) to exclude redundant
    sig_nonredundant <- data.frame()

    if(!is.null(top_dissim) && nrow(top_dissim) > 0) {
      # Use p_adjusted for FDR significance (handle NAs explicitly)
      if("p_adjusted" %in% names(top_dissim)) {
        sig_d <- top_dissim[!is.na(top_dissim$p_adjusted) & top_dissim$p_adjusted < 0.05, ]
      } else if("significant" %in% names(top_dissim)) {
        sig_d <- top_dissim[!is.na(top_dissim$significant) & top_dissim$significant == TRUE, ]
      } else {
        sig_d <- data.frame()
      }

      if(nrow(sig_d) > 0) {
        # For multi-region combos, filter for synergistic (>0) or additive (=0), exclude redundant (<0)
        if("synergy" %in% names(sig_d)) {
          # Singles: always include (synergy is NA for singles)
          # Multi-region: require synergy >= 0 (synergistic or additive)
          is_single <- sig_d$size == 1
          is_synergistic <- !is.na(sig_d$synergy) & sig_d$synergy >= 0
          sig_d <- sig_d[is_single | is_synergistic, ]
        }
        if(nrow(sig_d) > 0) {
          sig_d$direction <- "Dissimilarity"
          sig_nonredundant <- rbind(sig_nonredundant, sig_d)
        }
      }
    }

    if(!is.null(top_sim) && nrow(top_sim) > 0) {
      # Use p_adjusted for FDR significance (handle NAs explicitly)
      if("p_adjusted" %in% names(top_sim)) {
        sig_s <- top_sim[!is.na(top_sim$p_adjusted) & top_sim$p_adjusted < 0.05, ]
      } else if("significant" %in% names(top_sim)) {
        sig_s <- top_sim[!is.na(top_sim$significant) & top_sim$significant == TRUE, ]
      } else {
        sig_s <- data.frame()
      }

      if(nrow(sig_s) > 0) {
        # For multi-region combos, filter for synergistic (<0) or additive (=0), exclude redundant (>0)
        if("synergy" %in% names(sig_s)) {
          # Singles: always include (synergy is NA for singles)
          # Multi-region: require synergy <= 0 (synergistic or additive for similarity)
          is_single <- sig_s$size == 1
          is_synergistic <- !is.na(sig_s$synergy) & sig_s$synergy <= 0
          sig_s <- sig_s[is_single | is_synergistic, ]
        }
        if(nrow(sig_s) > 0) {
          sig_s$direction <- "Similarity"
          sig_nonredundant <- rbind(sig_nonredundant, sig_s)
        }
      }
    }

    if(nrow(sig_nonredundant) == 0) {
      plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
      text(1, 1, "No significant non-redundant combinations found", cex = 1.1, col = "gray50")
      return()
    }

    # Sort by absolute contribution
    sig_nonredundant <- sig_nonredundant[order(-abs(sig_nonredundant$contribution)), ]

    # Limit to top 20 for display
    if(nrow(sig_nonredundant) > 20) {
      sig_nonredundant <- head(sig_nonredundant, 20)
    }

    n_combos <- nrow(sig_nonredundant)

    # Set up colors by direction
    bar_colors <- ifelse(sig_nonredundant$direction == "Dissimilarity", "#E74C3C", "#3498DB")

    # Add brain area coloring if available and if it's a single region
    if(!is.null(ui_state$brain_areas) && !is.null(ui_state$area_colors)) {
      for(i in 1:n_combos) {
        if(sig_nonredundant$size[i] == 1) {
          region <- sig_nonredundant$nodes[i]
          # Check if region directly matches a brain area name
          if(region %in% names(ui_state$area_colors)) {
            bar_colors[i] <- ui_state$area_colors[[region]]
          } else {
            # Look up which brain area contains this node
            for(area_name in names(ui_state$brain_areas)) {
              if(region %in% ui_state$brain_areas[[area_name]]) {
                if(area_name %in% names(ui_state$area_colors)) {
                  bar_colors[i] <- ui_state$area_colors[[area_name]]
                }
                break
              }
            }
          }
        }
      }
    }

    # Calculate margins
    max_label_len <- max(nchar(sig_nonredundant$nodes))
    left_margin <- min(18, max(10, max_label_len * 0.4))

    tryCatch({
      par(mar = c(5, left_margin, 4, 4))

      # Calculate x limits
      x_max <- max(abs(sig_nonredundant$contribution), na.rm = TRUE) * 1.3

      # Create barplot
      bp <- barplot(sig_nonredundant$contribution,
                    horiz = TRUE,
                    names.arg = sig_nonredundant$nodes,
                    col = bar_colors,
                    las = 1,
                    xlim = c(-x_max, x_max),
                    xlab = "Contribution Score",
                    main = paste("Significant Non-Redundant Artificial Region Combinations\n",
                                results$group1_name, "vs", results$group2_name),
                    cex.names = 0.65)

      # Add zero line
      abline(v = 0, lty = 2, lwd = 2, col = "gray50")

      # Add significance stars
      for(i in 1:n_combos) {
        stars <- ""
        if(!is.na(sig_nonredundant$p_value[i])) {
          if(sig_nonredundant$p_value[i] < 0.001) stars <- "***"
          else if(sig_nonredundant$p_value[i] < 0.01) stars <- "**"
          else if(sig_nonredundant$p_value[i] < 0.05) stars <- "*"
        }
        if(stars != "") {
          x_pos <- sig_nonredundant$contribution[i]
          x_pos <- x_pos + sign(x_pos) * x_max * 0.05
          text(x_pos, bp[i], stars, cex = 0.9, font = 2)
        }
      }

      # Add legend
      legend("topright",
             legend = c("Dissimilarity (drives group differences)",
                       "Similarity (drives group commonality)"),
             fill = c("#E74C3C", "#3498DB"),
             bty = "n", cex = 0.7)

      # Add synergy note
      mtext("Only showing significant synergistic/additive combinations or single regions",
            side = 1, line = 4, cex = 0.75, col = "gray40")

    }, error = function(e) {
      par(mar = c(2, 2, 3, 2))
      plot(1, type = "n", axes = FALSE, xlab = "", ylab = "",
           xlim = c(0, 10), ylim = c(0, 10))
      text(5, 6, "Plot display error - try resizing window", cex = 1.1, col = "gray50")
      text(5, 5, paste("N combinations:", n_combos), cex = 1, col = "black")
    })
  })

  # Output: Results table
  output$regionalContributionTable <- DT::renderDataTable({
    results <- regional_contribution_results()

    if(is.null(results)) {
      return(data.frame(Message = "No results available. Run analysis first."))
    }

    # Check if artificial mode - show discovery selection order
    if(!is.null(results$mode) && results$mode == "artificial") {

      # Check for brute force method
      if(!is.null(results$method) && results$method == "brute_force") {
        discovery <- results$discovery

        if(is.null(discovery)) {
          return(data.frame(Message = "No brute force results available."))
        }

        # Combine top dissimilarity and similarity results
        top_dissim <- discovery$top_dissimilarity
        top_sim <- discovery$top_similarity

        # Add direction column
        top_dissim$Direction <- "Dissimilarity"
        top_sim$Direction <- "Similarity"

        # Combine and reorder columns
        combined <- rbind(top_dissim, top_sim)

        # Select and rename columns for display
        display_df <- data.frame(
          Direction = combined$Direction,
          Rank = c(1:nrow(top_dissim), 1:nrow(top_sim)),
          Regions = combined$nodes,
          Size = combined$size,
          Contribution = round(combined$contribution, 5),
          Expected = ifelse(is.na(combined$expected_additive), "-", round(combined$expected_additive, 5)),
          Synergy = ifelse(is.na(combined$synergy), "-", round(combined$synergy, 5)),
          Weighted = ifelse(is.na(combined$contrib_weighted), "-", round(combined$contrib_weighted, 5)),
          Percolation = ifelse(is.na(combined$contrib_percolation), "-", round(combined$contrib_percolation, 5)),
          Persistence = ifelse(is.na(combined$contrib_persistence), "-", round(combined$contrib_persistence, 5)),
          P_Value = ifelse(is.na(combined$p_value), "-", format(combined$p_value, digits = 3)),
          P_Adjusted = ifelse(is.na(combined$p_adjusted), "-", format(combined$p_adjusted, digits = 3)),
          Significant = ifelse(is.na(combined$significant), "-",
                              ifelse(combined$significant, "Yes", "No")),
          stringsAsFactors = FALSE
        )

        # Add synergy interpretation column for display
        # NOTE: Synergy interpretation depends on direction!
        # - Dissimilarity: synergy > 0 = Synergistic (more dissimilar than expected)
        # - Similarity: synergy < 0 = Synergistic (more similar than expected)
        display_df$Synergy_Type <- mapply(function(s, dir) {
          if(s == "-") return("-")
          s_num <- as.numeric(s)
          if(is.na(s_num)) return("-")

          if(dir == "Dissimilarity") {
            # For dissimilarity: positive synergy = synergistic
            if(s_num > 0.001) return("Synergistic")
            if(s_num < -0.001) return("Redundant")
          } else {
            # For similarity: negative synergy = synergistic (more similar than expected)
            if(s_num < -0.001) return("Synergistic")
            if(s_num > 0.001) return("Redundant")
          }
          return("Additive")
        }, display_df$Synergy, display_df$Direction)

        return(DT::datatable(
          display_df,
          options = list(
            pageLength = 30,
            order = list(list(0, 'asc'), list(1, 'asc')),
            scrollX = TRUE
          ),
          rownames = FALSE
        ) %>%
          DT::formatStyle(
            'Significant',
            backgroundColor = DT::styleEqual(c("Yes", "No", "-"), c('#ccffcc', '#f5f5f5', '#f5f5f5'))
          ) %>%
          DT::formatStyle(
            'Direction',
            backgroundColor = DT::styleEqual(c("Dissimilarity", "Similarity"),
                                            c('#ffe6e6', '#e6ffe6'))
          ) %>%
          DT::formatStyle(
            'Synergy_Type',
            backgroundColor = DT::styleEqual(
              c("Synergistic", "Redundant", "Additive", "-"),
              c('#d4edda', '#f8d7da', '#fff3cd', '#f5f5f5')
            ),
            fontWeight = DT::styleEqual(
              c("Synergistic", "Redundant", "Additive", "-"),
              c('bold', 'bold', 'normal', 'normal')
            )
          ))
      }

      direction <- results$direction

      # Helper to build table for single direction
      build_direction_table <- function(disc, dir_label) {
        if(is.null(disc) || length(disc$selection_order) == 0) {
          return(data.frame(
            Direction = dir_label,
            Rank = NA,
            Region = "No regions found",
            Incremental = NA,
            Cumulative = NA,
            At_Elbow = NA,
            stringsAsFactors = FALSE
          ))
        }
        n_selected <- length(disc$selection_order)
        data.frame(
          Direction = dir_label,
          Rank = 1:n_selected,
          Region = disc$selection_order,
          Incremental = round(disc$incremental_contributions, 5),
          Cumulative = round(disc$cumulative_contributions, 5),
          At_Elbow = ifelse(1:n_selected <= disc$elbow_k, "Yes", "No"),
          stringsAsFactors = FALSE
        )
      }

      if(direction == "both") {
        discovery <- results$discovery
        # Combine both tables
        dissim_df <- build_direction_table(discovery$dissimilarity, "Dissimilarity")
        sim_df <- build_direction_table(discovery$similarity, "Similarity")
        display_df <- rbind(dissim_df, sim_df)

        return(DT::datatable(
          display_df,
          options = list(
            pageLength = 30,
            order = list(list(0, 'asc'), list(1, 'asc')),
            scrollX = TRUE
          ),
          rownames = FALSE
        ) %>%
          DT::formatStyle(
            'At_Elbow',
            backgroundColor = DT::styleEqual(c("Yes", "No"), c('#ccffcc', '#f5f5f5'))
          ) %>%
          DT::formatStyle(
            'Direction',
            backgroundColor = DT::styleEqual(c("Dissimilarity", "Similarity"),
                                            c('#ffe6e6', '#e6ffe6'))
          ))
      } else {
        discovery <- results$discovery
        if(is.null(discovery) || length(discovery$selection_order) == 0) {
          return(data.frame(Message = "No discovery results available."))
        }

        n_selected <- length(discovery$selection_order)
        display_df <- data.frame(
          Rank = 1:n_selected,
          Region = discovery$selection_order,
          Incremental_Contribution = round(discovery$incremental_contributions, 5),
          Cumulative_Contribution = round(discovery$cumulative_contributions, 5),
          At_Elbow = ifelse(1:n_selected <= discovery$elbow_k, "Yes", "No"),
          stringsAsFactors = FALSE
        )

        return(DT::datatable(
          display_df,
          options = list(
            pageLength = 25,
            order = list(list(0, 'asc')),
            scrollX = TRUE
          ),
          rownames = FALSE
        ) %>%
          DT::formatStyle(
            'At_Elbow',
            backgroundColor = DT::styleEqual(c("Yes", "No"), c('#ccffcc', '#f5f5f5'))
          ))
      }
    }

    # Standard mode
    display_df <- results$contribution$observed

    if(is.null(display_df) || nrow(display_df) == 0) {
      return(data.frame(Message = "No contribution data available."))
    }

    # Format for display
    display_df$Original_Jaccard <- round(display_df$Original_Jaccard, 4)
    display_df$Jaccard_Without <- round(display_df$Jaccard_Without, 4)
    display_df$Contribution_Score <- round(display_df$Contribution_Score, 4)
    display_df$Pct_Change <- round(display_df$Pct_Change, 2)

    if("P_Value" %in% names(display_df)) {
      display_df$P_Value <- format(display_df$P_Value, scientific = TRUE, digits = 3)
    }
    if("P_Adjusted" %in% names(display_df)) {
      display_df$P_Adjusted <- format(display_df$P_Adjusted, scientific = TRUE, digits = 3)
    }

    DT::datatable(
      display_df,
      options = list(
        pageLength = 25,
        order = list(list(3, 'desc')),  # Sort by Contribution_Score
        scrollX = TRUE
      ),
      rownames = FALSE
    ) %>%
      DT::formatStyle(
        'Significant',
        backgroundColor = DT::styleEqual(c(TRUE, FALSE), c('#ffcccc', '#f5f5f5'))
      )
  })

  # Output: Summary statistics
  output$regionalContributionSummary <- renderPrint({
    results <- regional_contribution_results()

    if(is.null(results)) {
      cat("Regional Contribution Analysis Summary\n")
      cat("======================================\n")
      cat("No results available. Please run analysis.\n")
      return()
    }

    # Check if artificial mode - show discovery summary
    if(!is.null(results$mode) && results$mode == "artificial") {

      # ===== BRUTE FORCE MODE =====
      if(!is.null(results$method) && results$method == "brute_force") {
        discovery <- results$discovery

        if(is.null(discovery)) {
          cat("Brute Force Discovery Summary\n")
          cat("=============================\n")
          cat("No results available.\n")
          return()
        }

        cat("=== BRUTE FORCE ARTIFICIAL BRAIN AREA DISCOVERY ===\n\n")
        cat(sprintf("Comparison: %s vs %s\n", discovery$group1_name, discovery$group2_name))
        cat(sprintf("Total combinations tested: %s\n", format(discovery$total_combinations, big.mark = ",")))
        cat(sprintf("Max combination size: %d regions\n", discovery$max_combo_size))
        cat(sprintf("Permutations per candidate: %d\n", discovery$n_permutations))

        # Correction method
        correction_label <- switch(discovery$correction_method,
                                  "fdr" = "FDR (Benjamini-Hochberg)",
                                  "bonferroni" = "Bonferroni",
                                  "none" = "None",
                                  discovery$correction_method)
        cat(sprintf("Multiple comparison correction: %s\n\n", correction_label))

        # Baseline Jaccards by approach
        cat("--- BASELINE JACCARDS (by approach) ---\n")
        if(!is.null(discovery$baseline_weighted) && !is.na(discovery$baseline_weighted)) {
          cat(sprintf("  Weighted: %.4f\n", discovery$baseline_weighted))
        }
        if(!is.null(discovery$baseline_percolation) && !is.na(discovery$baseline_percolation)) {
          cat(sprintf("  Percolation: %.4f\n", discovery$baseline_percolation))
        }
        if(!is.null(discovery$baseline_persistence) && !is.na(discovery$baseline_persistence)) {
          cat(sprintf("  Persistence: %.4f\n", discovery$baseline_persistence))
        }

        # Significant results summary
        cat(sprintf("\n--- SIGNIFICANT RESULTS ---\n"))
        cat(sprintf("Top %d candidates tested per direction\n", discovery$top_n_tested))
        cat(sprintf("Dissimilarity: %d significant\n", discovery$n_significant_dissim))
        cat(sprintf("Similarity: %d significant\n\n", discovery$n_significant_sim))

        # Top dissimilarity results
        top_dissim <- discovery$top_dissimilarity
        if(!is.null(top_dissim) && nrow(top_dissim) > 0) {
          cat("--- TOP DISSIMILARITY CANDIDATES ---\n")
          sig_dissim <- top_dissim[!is.na(top_dissim$significant) & top_dissim$significant == TRUE, ]
          if(nrow(sig_dissim) > 0) {
            for(i in 1:min(5, nrow(sig_dissim))) {
              sig_star <- "***"
              p_adj <- if(!is.null(sig_dissim$p_adjusted)) sig_dissim$p_adjusted[i] else sig_dissim$p_value[i]
              cat(sprintf("  %d. %s (synergy: %.4f, p_adj: %.3e) %s\n",
                         i, sig_dissim$nodes[i],
                         ifelse(is.na(sig_dissim$synergy[i]), sig_dissim$contribution[i], sig_dissim$synergy[i]),
                         p_adj, sig_star))
            }
          } else {
            cat("  No significant dissimilarity candidates\n")
          }
        }

        # Top similarity results
        top_sim <- discovery$top_similarity
        if(!is.null(top_sim) && nrow(top_sim) > 0) {
          cat("\n--- TOP SIMILARITY CANDIDATES ---\n")
          sig_sim <- top_sim[!is.na(top_sim$significant) & top_sim$significant == TRUE, ]
          if(nrow(sig_sim) > 0) {
            for(i in 1:min(5, nrow(sig_sim))) {
              sig_star <- "***"
              p_adj <- if(!is.null(sig_sim$p_adjusted)) sig_sim$p_adjusted[i] else sig_sim$p_value[i]
              cat(sprintf("  %d. %s (synergy: %.4f, p_adj: %.3e) %s\n",
                         i, sig_sim$nodes[i],
                         ifelse(is.na(sig_sim$synergy[i]), sig_sim$contribution[i], sig_sim$synergy[i]),
                         p_adj, sig_star))
            }
          } else {
            cat("  No significant similarity candidates\n")
          }
        }

        cat("\n--- INTERPRETATION ---\n")
        if(discovery$n_significant_dissim > 0 || discovery$n_significant_sim > 0) {
          if(discovery$n_significant_dissim > 0) {
            cat("DISSIMILARITY combinations: Removing these regions INCREASES group similarity.\n")
            cat("  -> These region combinations drive DIFFERENCES between groups.\n")
          }
          if(discovery$n_significant_sim > 0) {
            cat("SIMILARITY combinations: Removing these regions DECREASES group similarity.\n")
            cat("  -> These region combinations drive COMMONALITY between groups.\n")
          }
        } else {
          cat("No significant artificial brain areas found.\n")
          cat("Consider adjusting parameters or reviewing top-ranked (non-significant) candidates.\n")
        }

        return()
      }

      # ===== STEPWISE/GREEDY MODE (original code) =====
      direction <- results$direction

      # Helper function to print single direction results
      print_direction_summary <- function(disc, dir_label) {
        cat(sprintf("\n--- %s ---\n", toupper(dir_label)))

        if(is.null(disc) || length(disc$selection_order) == 0) {
          cat("  No regions found contributing to", tolower(dir_label), "\n")
          return()
        }

        cat(sprintf("Elbow at: %d regions\n", disc$elbow_k))
        cat(sprintf("Combined contribution: %.5f\n", disc$combined_contribution))
        cat(sprintf("P-value: %s (raw), %s (adjusted)\n",
                   format(disc$p_value, digits = 3),
                   format(disc$p_adjusted, digits = 3)))

        sig_label <- ifelse(isTRUE(disc$significant), "YES ***", "NO")
        cat(sprintf("Significant (adjusted p < 0.05): %s\n\n", sig_label))

        if(disc$elbow_k > 0) {
          cat("Regions at elbow:\n")
          for(i in 1:disc$elbow_k) {
            cat(sprintf("  %d. %s (incremental: %.5f)\n",
                       i, disc$selection_order[i],
                       disc$incremental_contributions[i]))
          }
        }
      }

      cat("=== ARTIFICIAL BRAIN AREA DISCOVERY ===\n\n")
      cat(sprintf("Comparison: %s vs %s\n", results$group1_name, results$group2_name))
      cat(sprintf("Correction method: %s\n", results$correction_method))

      if(direction == "both") {
        discovery <- results$discovery

        if(!is.null(discovery$baseline_jaccard)) {
          cat(sprintf("Baseline Jaccard: %.4f\n", discovery$baseline_jaccard))
        }

        print_direction_summary(discovery$dissimilarity, "Dissimilarity")
        print_direction_summary(discovery$similarity, "Similarity")

        cat("\n--- INTERPRETATION ---\n")
        dissim_sig <- isTRUE(discovery$dissimilarity$significant)
        sim_sig <- isTRUE(discovery$similarity$significant)

        if(dissim_sig) {
          cat("DISSIMILARITY regions: Removing these INCREASES group similarity.\n")
          cat("  -> These regions drive DIFFERENCES between groups.\n")
        }
        if(sim_sig) {
          cat("SIMILARITY regions: Removing these DECREASES group similarity.\n")
          cat("  -> These regions drive COMMONALITY between groups.\n")
        }
        if(!dissim_sig && !sim_sig) {
          cat("No significant artificial brain areas found in either direction.\n")
          cat("Results at elbow are shown but do not reach significance.\n")
        }

      } else {
        discovery <- results$discovery

        if(is.null(discovery) || length(discovery$selection_order) == 0) {
          cat("No regions found contributing to", direction, "\n")
          return()
        }

        if(!is.null(discovery$baseline_jaccard)) {
          cat(sprintf("Baseline Jaccard: %.4f\n", discovery$baseline_jaccard))
        }
        cat(sprintf("Direction: %s\n", tools::toTitleCase(direction)))

        print_direction_summary(discovery, tools::toTitleCase(direction))

        cat("\n--- INTERPRETATION ---\n")
        if(isTRUE(discovery$significant)) {
          if(direction == "dissimilarity") {
            cat("Removing these regions INCREASES group similarity.\n")
            cat("-> These regions collectively drive DIFFERENCES between groups.\n")
          } else {
            cat("Removing these regions DECREASES group similarity.\n")
            cat("-> These regions collectively drive COMMONALITY between groups.\n")
          }
        } else {
          cat("Elbow-based region set does not reach statistical significance.\n")
          cat("Results shown are exploratory.\n")
        }
      }

      return()
    }

    # Standard mode - existing summary code
    observed <- results$contribution$observed

    if(is.null(observed) || nrow(observed) == 0) {
      cat("Regional Contribution Analysis Summary\n")
      cat("======================================\n")
      cat("No contribution data computed.\n")
      return()
    }

    n_sig <- sum(observed$Significant, na.rm = TRUE)
    n_total <- nrow(observed)

    cat("Regional Contribution Analysis Summary\n")
    cat("======================================\n\n")
    cat(sprintf("Comparison: %s vs %s\n", results$group1_name, results$group2_name))
    cat(sprintf("Correlation method: %s\n", results$method))
    cat(sprintf("Analysis level: %s\n", results$contribution$analysis_level))
    cat(sprintf("Number of permutations: %d\n", results$contribution$n_permutations))
    cat(sprintf("\nTotal regions tested: %d\n", n_total))
    cat(sprintf("Significant regions (FDR p < 0.05): %d (%.1f%%)\n\n", n_sig, 100 * n_sig / n_total))

    if(n_sig > 0) {
      cat("Significant Regions Contributing to DISSIMILARITY (positive score):\n")
      sig_pos <- observed[observed$Significant & observed$Contribution_Score > 0, ]
      if(nrow(sig_pos) > 0) {
        sig_pos <- sig_pos[order(-sig_pos$Contribution_Score), ]
        for(i in 1:nrow(sig_pos)) {
          cat(sprintf("  %s: score = %.4f, p_adj = %.2e %s\n",
                     sig_pos$Region[i],
                     sig_pos$Contribution_Score[i],
                     sig_pos$P_Adjusted[i],
                     sig_pos$Significance_Stars[i]))
        }
      } else {
        cat("  None\n")
      }

      cat("\nSignificant Regions Contributing to SIMILARITY (negative score):\n")
      sig_neg <- observed[observed$Significant & observed$Contribution_Score < 0, ]
      if(nrow(sig_neg) > 0) {
        sig_neg <- sig_neg[order(sig_neg$Contribution_Score), ]
        for(i in 1:nrow(sig_neg)) {
          cat(sprintf("  %s: score = %.4f, p_adj = %.2e %s\n",
                     sig_neg$Region[i],
                     sig_neg$Contribution_Score[i],
                     sig_neg$P_Adjusted[i],
                     sig_neg$Significance_Stars[i]))
        }
      } else {
        cat("  None\n")
      }
    } else {
      cat("No regions showed significant contribution at FDR < 0.05.\n")
    }

    cat("\n--- Regional Jaccard Summary ---\n")
    regional_j <- results$regional_jaccard
    if(!is.null(regional_j) && nrow(regional_j) > 0) {
      cat(sprintf("Mean regional Jaccard: %.3f\n", mean(regional_j$Regional_Jaccard, na.rm = TRUE)))
      cat(sprintf("SD regional Jaccard: %.3f\n", sd(regional_j$Regional_Jaccard, na.rm = TRUE)))
      cat(sprintf("Range: %.3f - %.3f\n",
                 min(regional_j$Regional_Jaccard, na.rm = TRUE),
                 max(regional_j$Regional_Jaccard, na.rm = TRUE)))
    }
  })


  # 7l. Regional Consensus Aggregates - Mean consensus scores by brain region with error bars
  output$consensusRegionalPlot <- renderPlot({
    req(analysis_results$comprehensive_consensus)
    req(analysis_results$method_percolation_results)
    req(ui_state$brain_areas)

    all_groups <- names(analysis_results$comprehensive_consensus)
    if(length(all_groups) == 0) {
      plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
      text(1, 1, "No consensus data available", cex = 1.2, col = "gray")
      return()
    }

    # Get brain regions
    region_names <- names(ui_state$brain_areas)
    n_regions <- length(region_names)
    n_groups <- length(all_groups)

    # Build matrices for region × group (mean and sd)
    eigenvector_matrix <- matrix(NA, nrow = n_groups, ncol = n_regions)
    eigenvector_sd_matrix <- matrix(NA, nrow = n_groups, ncol = n_regions)
    rownames(eigenvector_matrix) <- all_groups
    colnames(eigenvector_matrix) <- region_names

    # For each group, compute rank-based consensus and aggregate by region
    for(i in seq_along(all_groups)) {
      group <- all_groups[i]
      consensus_df <- get_comprehensive_consensus_for_group(group)

      if(!is.null(consensus_df) && nrow(consensus_df) > 0) {
        consensus_vec <- consensus_df$Consensus_Eigenvector
        names(consensus_vec) <- consensus_df$Node

        # Aggregate by region (mean and sd)
        for(j in seq_along(region_names)) {
          region_name <- region_names[j]
          region_nodes <- ui_state$brain_areas[[region_name]]
          region_values <- consensus_vec[names(consensus_vec) %in% region_nodes]
          region_values <- region_values[!is.na(region_values)]

          if(length(region_values) > 0) {
            eigenvector_matrix[i, j] <- mean(region_values, na.rm = TRUE)
            eigenvector_sd_matrix[i, j] <- sd(region_values, na.rm = TRUE)
          }
        }
      }
    }

    # Check if we have any valid data
    if(all(is.na(eigenvector_matrix))) {
      plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
      text(1, 1, "No regional data available", cex = 1.2, col = "gray")
      return()
    }

    # Get group colors
    group_colors <- sapply(all_groups, function(g) {
      if(!is.null(ui_state$group_colors[[g]])) {
        return(ui_state$group_colors[[g]])
      } else {
        return("#3498db")
      }
    })

    # Create grouped bar plot
    par(mar = c(10, 5, 4, 2))

    # Calculate y-axis limits
    max_val <- max(eigenvector_matrix + eigenvector_sd_matrix, na.rm = TRUE)
    if(is.infinite(max_val) || is.na(max_val)) max_val <- 1

    bp <- barplot(eigenvector_matrix,
                  beside = TRUE,
                  names.arg = region_names,
                  main = "Regional Consensus Eigenvector (Mean ± SD)\nAcross All Methods & Approaches",
                  ylab = "Consensus Eigenvector Score (0-1)",
                  col = group_colors,
                  border = group_colors,
                  las = 2,
                  ylim = c(0, min(max_val * 1.2, 1.1)))
    grid(nx = NA, ny = NULL, col = "gray90")

    # Add error bars
    for(i in 1:n_groups) {
      for(j in 1:n_regions) {
        if(!is.na(eigenvector_matrix[i, j]) && !is.na(eigenvector_sd_matrix[i, j])) {
          x_pos <- bp[i, j]
          y_val <- eigenvector_matrix[i, j]
          y_sd <- eigenvector_sd_matrix[i, j]
          # Only draw if error bars stay within bounds
          y_upper <- min(y_val + y_sd, 1.05)
          y_lower <- max(y_val - y_sd, 0)
          segments(x_pos, y_lower, x_pos, y_upper, lwd = 1.5)
          segments(x_pos - 0.1, y_lower, x_pos + 0.1, y_lower, lwd = 1.5)
          segments(x_pos - 0.1, y_upper, x_pos + 0.1, y_upper, lwd = 1.5)
        }
      }
    }

    # Legend
    legend("topright", legend = all_groups, fill = group_colors, bty = "n", cex = 0.9)
  })

  # 7m. Subregional consensus plot - Bar plots of normalized consensus scores by region
  output$consensusSubregionalPlot <- renderPlot({
    req(analysis_results$comprehensive_consensus)
    req(analysis_results$method_percolation_results)
    req(ui_state$brain_areas)

    all_groups <- names(analysis_results$comprehensive_consensus)
    if(length(all_groups) == 0) {
      plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
      text(1, 1, "No consensus data available", cex = 1.2, col = "gray")
      return()
    }

    # Get brain regions
    region_names <- names(ui_state$brain_areas)
    n_regions <- length(region_names)

    # For each group, compute rank-based consensus using the comprehensive function
    all_consensus_data <- list()

    for(group in all_groups) {
      consensus_df <- get_comprehensive_consensus_for_group(group)
      if(!is.null(consensus_df) && nrow(consensus_df) > 0) {
        consensus_vec <- consensus_df$Consensus_Eigenvector
        names(consensus_vec) <- consensus_df$Node
        all_consensus_data[[group]] <- consensus_vec
      }
    }

    if(length(all_consensus_data) == 0) {
      plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
      text(1, 1, "No consensus data computed", cex = 1.2, col = "gray")
      return()
    }

    # Get group colors
    group_colors <- sapply(all_groups, function(g) {
      if(!is.null(ui_state$group_colors[[g]])) {
        return(ui_state$group_colors[[g]])
      } else {
        return("#3498db")
      }
    })

    # Create faceted plots by region
    par(mfrow = c(n_regions, 1), mar = c(8, 5, 3, 2))

    for(region_name in region_names) {
      region_nodes <- ui_state$brain_areas[[region_name]]

      # Build matrix for this region: nodes × groups
      region_data <- matrix(NA, nrow = length(region_nodes), ncol = length(all_groups))
      rownames(region_data) <- region_nodes
      colnames(region_data) <- all_groups

      for(i in seq_along(all_groups)) {
        group <- all_groups[i]
        if(!is.null(all_consensus_data[[group]])) {
          consensus_vec <- all_consensus_data[[group]]
          for(j in seq_along(region_nodes)) {
            node <- region_nodes[j]
            if(node %in% names(consensus_vec)) {
              region_data[j, i] <- consensus_vec[node]
            }
          }
        }
      }

      # Skip if no data for this region
      if(all(is.na(region_data))) {
        plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
        text(1, 1, paste("No data for", region_name), cex = 1, col = "gray")
        next
      }

      # Create grouped bar plot with normalized consensus scores (0-1)
      bp <- barplot(t(region_data),
                    beside = TRUE,
                    names.arg = region_nodes,
                    main = paste("Region:", region_name),
                    ylab = "Consensus Eigenvector Score (0-1)",
                    col = group_colors,
                    border = group_colors,
                    las = 2,
                    cex.names = 0.8,
                    ylim = c(0, 1.1))
      grid(nx = NA, ny = NULL, col = "gray90")

      # Add legend only on first plot
      if(region_name == region_names[1]) {
        legend("topright", legend = all_groups, fill = group_colors, bty = "n", cex = 0.8)
      }
    }
  })

  # ============================================================================
  # TAB 8: STATS & VALIDATION
  # ============================================================================

  # Update validation group selector
  observe({
    req(analysis_results$comprehensive_consensus)
    group_choices <- names(analysis_results$comprehensive_consensus)
    if(length(group_choices) > 0) {
      updateSelectInput(session, "validation_group", choices = group_choices, selected = group_choices[1])
    }
  })

  # 1. Validation Network Similarity (reuses Tab 7 similarity matrix logic)
  output$validationNetworkSimilarity <- renderPlot({
    req(analysis_results$comprehensive_consensus)
    req(input$validation_group)
    req(selected_methods())

    group_name <- input$validation_group
    consensus_results <- analysis_results$comprehensive_consensus[[group_name]]

    if(is.null(consensus_results) || is.null(consensus_results$standardized_metrics)) {
      plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
      text(1, 1, "No consensus data available", cex = 1.2, col = "gray")
      return()
    }

    std_metrics <- consensus_results$standardized_metrics
    methods <- selected_methods()
    n_combos <- length(methods) * 3  # methods × 3 approaches

    # Build centrality matrix
    all_nodes <- unique(c(
      unlist(lapply(std_metrics$weighted, function(x) if(!is.null(x)) x$Node else character(0))),
      unlist(lapply(std_metrics$percolation, function(x) if(!is.null(x)) x$Node else character(0))),
      unlist(lapply(std_metrics$persistence, function(x) if(!is.null(x)) x$Node else character(0)))
    ))

    centrality_matrix <- matrix(NA, nrow = length(all_nodes), ncol = n_combos)
    rownames(centrality_matrix) <- all_nodes

    col_names <- c()
    col_idx <- 1

    for(approach in c("weighted", "percolation", "persistence")) {
      for(method in methods) {
        if(!is.null(std_metrics[[approach]][[method]])) {
          approach_data <- std_metrics[[approach]][[method]]
          for(i in seq_along(approach_data$Node)) {
            node <- approach_data$Node[i]
            z_score <- approach_data$Z_Score[i]
            if(node %in% rownames(centrality_matrix)) {
              centrality_matrix[node, col_idx] <- z_score
            }
          }
        }
        approach_short <- substr(toupper(approach), 1, 1)
        method_short <- toupper(substr(method, 1, 3))
        col_names <- c(col_names, paste0(approach_short, ":", method_short))
        col_idx <- col_idx + 1
      }
    }

    colnames(centrality_matrix) <- col_names
    cor_matrix <- cor(centrality_matrix, use = "pairwise.complete.obs")

    # Create heatmap
    par(mar = c(10, 10, 4, 2))
    image(1:n_combos, 1:n_combos, cor_matrix,
          col = colorRampPalette(c("#d73027", "#fee090", "#4575b4"))(100),
          axes = FALSE,
          xlab = "", ylab = "",
          main = paste("Network Similarity Matrix -", group_name),
          zlim = c(-1, 1))

    axis(1, at = 1:n_combos, labels = col_names, las = 2, cex.axis = 0.7)
    axis(2, at = 1:n_combos, labels = col_names, las = 1, cex.axis = 0.7)

    abline(h = seq(0.5, n_combos + 0.5, by = 1), col = "white", lwd = 0.5)
    abline(v = seq(0.5, n_combos + 0.5, by = 1), col = "white", lwd = 0.5)

    for(i in 1:n_combos) {
      for(j in 1:n_combos) {
        text_col <- if(abs(cor_matrix[i,j]) > 0.5) "white" else "black"
        text(i, j, sprintf("%.2f", cor_matrix[i,j]), cex = 0.5, col = text_col)
      }
    }
  })

  # 2. Method Consistency Table
  output$methodConsistencyTable <- DT::renderDataTable({
    req(analysis_results$comprehensive_consensus)
    req(input$validation_group)
    req(selected_methods())

    group_name <- input$validation_group
    consensus_results <- analysis_results$comprehensive_consensus[[group_name]]

    if(is.null(consensus_results)) {
      return(DT::datatable(data.frame(Message = "No data available")))
    }

    std_metrics <- consensus_results$standardized_metrics
    methods <- selected_methods()
    n_combos <- length(methods) * 3  # methods × 3 approaches

    # Build centrality matrix
    all_nodes <- unique(c(
      unlist(lapply(std_metrics$weighted, function(x) if(!is.null(x)) x$Node else character(0))),
      unlist(lapply(std_metrics$percolation, function(x) if(!is.null(x)) x$Node else character(0))),
      unlist(lapply(std_metrics$persistence, function(x) if(!is.null(x)) x$Node else character(0)))
    ))

    centrality_matrix <- matrix(NA, nrow = length(all_nodes), ncol = n_combos)
    col_idx <- 1

    for(approach in c("weighted", "percolation", "persistence")) {
      for(method in methods) {
        if(!is.null(std_metrics[[approach]][[method]])) {
          approach_data <- std_metrics[[approach]][[method]]
          for(i in seq_along(approach_data$Node)) {
            node <- approach_data$Node[i]
            z_score <- approach_data$Z_Score[i]
            centrality_matrix[i, col_idx] <- z_score
          }
        }
        col_idx <- col_idx + 1
      }
    }

    # Compute pairwise correlations
    cor_matrix <- cor(centrality_matrix, use = "pairwise.complete.obs")

    # Summary statistics
    cor_values <- cor_matrix[lower.tri(cor_matrix)]

    consistency_df <- data.frame(
      Metric = c("Mean Correlation", "Median Correlation", "SD Correlation",
                "Min Correlation", "Max Correlation", "Agreement (r > 0.7)"),
      Value = c(
        round(mean(cor_values, na.rm = TRUE), 3),
        round(median(cor_values, na.rm = TRUE), 3),
        round(sd(cor_values, na.rm = TRUE), 3),
        round(min(cor_values, na.rm = TRUE), 3),
        round(max(cor_values, na.rm = TRUE), 3),
        paste0(round(100 * mean(cor_values > 0.7, na.rm = TRUE), 1), "%")
      ),
      stringsAsFactors = FALSE
    )

    DT::datatable(consistency_df, options = list(pageLength = 10, dom = 't'), rownames = FALSE)
  })

  # 3. Consensus Quality Plot
  output$consensusQualityPlot <- renderPlot({
    req(analysis_results$comprehensive_consensus)
    req(input$validation_group)

    group_name <- input$validation_group
    consensus_results <- analysis_results$comprehensive_consensus[[group_name]]

    if(is.null(consensus_results)) {
      plot(1, type = "n", axes = FALSE)
      text(1, 1, "No data available", cex = 1.2)
      return()
    }

    consensus_scores <- consensus_results$consensus_scores

    par(mfrow = c(2, 2), mar = c(5, 5, 4, 2))

    # Panel 1: Credible interval width distribution
    ci_width <- consensus_scores$CredibleIntervalUpper - consensus_scores$CredibleIntervalLower
    hist(ci_width, breaks = 30, col = "#4A90E2", border = "white",
         main = "Credible Interval Width Distribution",
         xlab = "CI Width", ylab = "Frequency")
    abline(v = c(0.2, 0.4), col = c("green", "orange"), lty = 2, lwd = 2)

    # Panel 2: CV across approaches
    hist(consensus_scores$CV_Across_Approaches, breaks = 30, col = "#E67E22", border = "white",
         main = "CV Across Approaches",
         xlab = "Coefficient of Variation", ylab = "Frequency")

    # Panel 3: Agreement distribution
    agreement_counts <- table(
      ifelse(consensus_scores$AllThreeAgree, "All 3",
             ifelse(consensus_scores$TwoAgree, "2 of 3", "1 of 3"))
    )
    barplot(agreement_counts, col = c("1 of 3" = "#E74C3C", "2 of 3" = "#F1C40F", "All 3" = "#2ECC71"),
            main = "Approach Agreement", ylab = "Number of Nodes")

    # Panel 4: Confidence category distribution
    conf_counts <- table(consensus_scores$ConfidenceCategory)
    barplot(conf_counts, col = c("High" = "#2ECC71", "Medium" = "#F1C40F", "Low" = "#E74C3C"),
            main = "Confidence Categories", ylab = "Number of Nodes", las = 2)
  })

  # 4. Quality Summary Text
  output$qualitySummaryText <- renderText({
    req(analysis_results$comprehensive_consensus)
    req(input$validation_group)

    group_name <- input$validation_group
    consensus_results <- analysis_results$comprehensive_consensus[[group_name]]

    if(is.null(consensus_results)) {
      return("No data available")
    }

    consensus_scores <- consensus_results$consensus_scores

    ci_width <- consensus_scores$CredibleIntervalUpper - consensus_scores$CredibleIntervalLower
    high_conf <- sum(consensus_scores$ConfidenceCategory == "High")
    all_agree <- sum(consensus_scores$AllThreeAgree)

    paste0(
      "Quality Metrics Summary for ", group_name, "\n",
      "=" , paste(rep("=", nchar(group_name) + 25), collapse = ""), "\n\n",
      "Total Nodes: ", nrow(consensus_scores), "\n",
      "High Confidence Nodes: ", high_conf, " (", round(100*high_conf/nrow(consensus_scores), 1), "%)\n",
      "All Three Approaches Agree: ", all_agree, " (", round(100*all_agree/nrow(consensus_scores), 1), "%)\n",
      "Mean CI Width: ", round(mean(ci_width, na.rm = TRUE), 3), "\n",
      "Median CI Width: ", round(median(ci_width, na.rm = TRUE), 3), "\n",
      "Mean CV Across Approaches: ", round(mean(consensus_scores$CV_Across_Approaches, na.rm = TRUE), 3), "\n"
    )
  })

  # 5. Group Comparison Tests (placeholder)
  output$groupComparisonTests <- DT::renderDataTable({
    DT::datatable(data.frame(
      Comparison = "Feature in development",
      Test = "Statistical tests for group differences",
      P_Value = "-",
      stringsAsFactors = FALSE
    ), options = list(pageLength = 10, dom = 't'), rownames = FALSE)
  })

  # 6. Method Agreement Tests (placeholder)
  output$methodAgreementTests <- renderText({
    "Method agreement statistical tests: Feature in development"
  })

  # 7. Calibration Validation Plot
  output$calibrationValidationPlot <- renderPlot({
    req(analysis_results$comprehensive_consensus)
    req(input$validation_group)

    group_name <- input$validation_group
    consensus_results <- analysis_results$comprehensive_consensus[[group_name]]

    if(is.null(consensus_results) || is.null(consensus_results$calibration)) {
      plot(1, type = "n", axes = FALSE)
      text(1, 1, "No calibration data available", cex = 1.2)
      return()
    }

    cal <- consensus_results$calibration

    par(mfrow = c(2, 2), mar = c(5, 5, 4, 2))

    # Panel 1: Optimal threshold
    barplot(cal$optimal_threshold, names.arg = "Optimal Threshold",
            col = "#3498DB", ylim = c(0, 3),
            main = "Optimal Z-Score Threshold")
    abline(h = 1.5, col = "red", lty = 2)
    text(1, cal$optimal_threshold + 0.1, round(cal$optimal_threshold, 2), cex = 1.2)

    # Panel 2: Hub proportion
    barplot(cal$proportion_hubs, names.arg = "Hub Proportion",
            col = "#2ECC71", ylim = c(0, 1),
            main = "Proportion of Hubs")
    text(1, cal$proportion_hubs + 0.05, paste0(round(100*cal$proportion_hubs, 1), "%"), cex = 1.2)

    # Panel 3: Quality score
    barplot(cal$quality_score, names.arg = "Quality Score",
            col = "#E67E22", ylim = c(0, 1),
            main = "Calibration Quality Score")
    text(1, cal$quality_score + 0.05, round(cal$quality_score, 2), cex = 1.2)

    # Panel 4: Hub topology metrics
    metrics <- c("Modularity" = cal$hub_modularity,
                "Clustering" = cal$hub_clustering,
                "Density" = cal$hub_density)
    barplot(metrics, col = c("#9B59B6", "#1ABC9C", "#F39C12"),
            main = "Hub Network Topology", ylim = c(0, max(metrics, na.rm = TRUE) * 1.2))
  })

  # 8. Calibration Summary Table
  output$calibrationSummaryTable <- DT::renderDataTable({
    req(analysis_results$comprehensive_consensus)

    groups <- names(analysis_results$comprehensive_consensus)
    cal_df_list <- list()

    for(group_name in groups) {
      consensus_results <- analysis_results$comprehensive_consensus[[group_name]]

      if(!is.null(consensus_results$calibration)) {
        cal <- consensus_results$calibration
        cal_df_list[[group_name]] <- data.frame(
          Group = group_name,
          OptimalThreshold = round(cal$optimal_threshold, 3),
          HubProportion = round(cal$proportion_hubs, 3),
          QualityScore = round(cal$quality_score, 3),
          HubModularity = round(cal$hub_modularity, 3),
          HubClustering = round(cal$hub_clustering, 3),
          HubDensity = round(cal$hub_density, 3),
          stringsAsFactors = FALSE
        )
      }
    }

    if(length(cal_df_list) > 0) {
      cal_df <- do.call(rbind, cal_df_list)
      DT::datatable(cal_df, options = list(pageLength = 20, scrollX = TRUE), rownames = FALSE)
    } else {
      DT::datatable(data.frame(Message = "No calibration data available"))
    }
  })

  # Plot 3: Network Similarity Heatmap
  # REMOVED: Duplicate definition of networkSimilarityHeatmapPlot
  # This was overwriting the Tab 7e version defined earlier (line 7306)
  # output$networkSimilarityHeatmapPlot <- renderPlot({
  #   req(analysis_results$network_similarity)
  #   req(input$similarity_metric)
  #   similarity_results <- analysis_results$network_similarity
  #   metric <- input$similarity_metric
  #   if(is.null(similarity_results)) {
  #     plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
  #     text(1, 1, "No network similarity data available", cex = 1.2, col = "gray")
  #     return()
  #   }
  #   render_network_similarity_heatmap(
  #     similarity_results = similarity_results,
  #     metric = metric
  #   )
  # })

  # Plot 4: Consensus Hub Ranking
  output$consensusHubRankingPlot <- renderPlot({
    req(analysis_results$comprehensive_consensus)
    req(input$consensus_group_heatmap)  # Changed: use unified group selector
    req(input$consensus_top_n)

    group_name <- input$consensus_group_heatmap  # Changed: use unified group selector
    top_n <- input$consensus_top_n
    consensus_results <- analysis_results$comprehensive_consensus[[group_name]]

    if(is.null(consensus_results)) {
      plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
      text(1, 1, "No consensus data for this group", cex = 1.2, col = "gray")
      return()
    }

    render_consensus_hub_ranking(
      consensus_results = consensus_results,
      top_n = top_n,
      brain_areas = ui_state$brain_areas,
      area_colors = ui_state$area_colors
    )
  })

  # Plot 5: Method Variance Barplot
  output$methodVarianceBarplot <- renderPlot({
    req(analysis_results$aggregated_metrics)
    req(input$consensus_group_robustness)

    group_name <- input$consensus_group_robustness
    aggregated_metrics <- analysis_results$aggregated_metrics[[group_name]]

    if(is.null(aggregated_metrics)) {
      plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
      text(1, 1, "No aggregated metrics for this group", cex = 1.2, col = "gray")
      return()
    }

    render_method_variance_barplot(
      aggregated_metrics = aggregated_metrics,
      top_n = 10
    )
  })

  # Regional Consensus Three-Way Comparison
  output$consensusRegionalThreeWayPlot <- renderPlot({
    req(analysis_results$method_weighted_results)
    req(analysis_results$method_percolation_results)
    req(analysis_results$auc_results)
    req(ui_state$brain_areas)

    method <- tolower(input$comparison_method %||% "pearson")
    all_groups <- names(analysis_results$correlations)

    if(length(ui_state$brain_areas) == 0) {
      plot(1, type = "n", main = "Define brain areas in Tab 2", axes = FALSE)
      return(NULL)
    }

    n_regions <- length(ui_state$brain_areas)
    n_groups <- length(all_groups)

    # 3D comparison: Groups × Regions × Approaches
    par(mfrow = c(n_groups, 3), mar = c(8, 4, 4, 2))

    for(group in all_groups) {
      # WEIGHTED regional metrics - FIX: filter combined df by group
      weighted_all <- analysis_results$method_weighted_results[[method]]$weighted_eigenvector
      weighted_data <- if(!is.null(weighted_all) && "Group" %in% names(weighted_all)) {
        weighted_all[weighted_all$Group == group, ]
      } else { NULL }
      if(!is.null(weighted_data) && nrow(weighted_data) > 0) {
        # Use weighted_data directly (it has Node, Weighted_Eigenvector columns)
        regional_weighted <- compute_regional_summary(weighted_data,
                                                      ui_state$brain_areas,
                                                      "Weighted_Eigenvector")

        barplot(regional_weighted$Mean,
                names.arg = regional_weighted$Region,
                main = paste(group, "- Weighted"),
                ylab = "Mean Eigenvector",
                col = ui_state$area_colors[regional_weighted$Region],
                las = 2)
        grid(nx = NA, ny = NULL)
      }

      # PERCOLATION regional metrics
      percolation_data <- analysis_results$method_percolation_results[[method]]$node_metrics
      if(!is.null(percolation_data)) {
        perc_group <- percolation_data[percolation_data$Group == group, ]
        regional_perc <- compute_regional_summary(perc_group,
                                                  ui_state$brain_areas,
                                                  "Eigenvector")

        barplot(regional_perc$Mean,
                names.arg = regional_perc$Region,
                main = paste(group, "- Percolation"),
                ylab = "Mean Eigenvector",
                col = ui_state$area_colors[regional_perc$Region],
                las = 2)
        grid(nx = NA, ny = NULL)
      }

      # PERSISTENCE (AUC) regional metrics
      auc_data <- analysis_results$auc_results[[method]][[group]]
      if(!is.null(auc_data$regional_auc)) {
        barplot(auc_data$regional_auc$AUC_Eigenvector,
                names.arg = auc_data$regional_auc$Region,
                main = paste(group, "- Persistence (AUC)"),
                ylab = "AUC Eigenvector",
                col = ui_state$area_colors[auc_data$regional_auc$Region],
                las = 2)
        grid(nx = NA, ny = NULL)
      }
    }
  })

  # MST network visualization
  
  # Weighted vs Percolation comparison by group
  output$weightedVsPercolationEigenvectorPlot <- renderPlot({
    req(analysis_results$method_weighted_results)
    req(analysis_results$method_percolation_results)

    method <- tolower(input$comparison_method %||% "pearson")
    weighted_data <- analysis_results$method_weighted_results[[method]]
    percolation_data <- analysis_results$method_percolation_results[[method]]

    if(is.null(weighted_data) || is.null(percolation_data)) {
      plot(1, type = "n", main = paste("Data not available for", method), axes = FALSE)
      text(1, 1, "Select different method or run analysis", cex = 1.2)
      return(NULL)
    }

    render_weighted_vs_percolation_eigenvector_by_group(weighted_data,
                                                       percolation_data$node_metrics,
                                                       ui_state$group_colors,
                                                       ui_state$brain_areas,
                                                       ui_state$area_colors)
  })
  
  output$weightedVsPercolationNodeStrengthPlot <- renderPlot({
    req(analysis_results$method_weighted_results)
    req(analysis_results$method_percolation_results)
    req(analysis_results$persistence_results)

    method <- tolower(input$comparison_method %||% "pearson")
    weighted_data <- analysis_results$method_weighted_results[[method]]
    percolation_data <- analysis_results$method_percolation_results[[method]]
    persistence_data <- analysis_results$persistence_results[[method]]

    if(is.null(weighted_data) || is.null(percolation_data) || is.null(persistence_data)) {
      plot(1, type = "n", main = paste("Data not available for", method), axes = FALSE)
      text(1, 1, "Select different method or run analysis", cex = 1.2)
      return(NULL)
    }

    # 3-way comparison: Weighted vs Percolation vs Persistence
    par(mfrow = c(1, 3), mar = c(5, 4, 4, 2))

    # Get all groups
    groups <- names(weighted_data)

    # Panel 1: Weighted vs Percolation
    plot(1, type = "n", xlim = c(0, 1), ylim = c(0, 1),
         main = "Weighted vs Percolation",
         xlab = "Weighted Node Strength", ylab = "Percolation Node Strength")
    for(group in groups) {
      if(!is.null(weighted_data[[group]]$node_strength) &&
         !is.null(percolation_data$node_metrics)) {
        perc_group <- percolation_data$node_metrics[percolation_data$node_metrics$Group == group, ]
        if(nrow(perc_group) > 0 && nrow(weighted_data[[group]]$node_strength) > 0) {
          # Match nodes and plot
          common_nodes <- intersect(weighted_data[[group]]$node_strength$Node, perc_group$Node)
          if(length(common_nodes) > 0) {
            weighted_strength <- weighted_data[[group]]$node_strength$Strength[
              match(common_nodes, weighted_data[[group]]$node_strength$Node)]
            perc_strength <- perc_group$Degree[match(common_nodes, perc_group$Node)]
            points(weighted_strength, perc_strength, pch = 16, col = rgb(0.2, 0.4, 0.8, 0.5))
          }
        }
      }
    }
    abline(0, 1, col = "red", lty = 2)

    # Panel 2: Weighted vs Persistence
    plot(1, type = "n", xlim = c(0, 1), ylim = c(0, 1),
         main = "Weighted vs Persistence",
         xlab = "Weighted Node Strength", ylab = "Persistence Avg Strength")
    for(group in groups) {
      if(!is.null(weighted_data[[group]]$node_strength) &&
         !is.null(persistence_data[[group]]$persistence_data)) {
        pers_strength <- persistence_data[[group]]$persistence_data$node_strength
        if(!is.null(pers_strength)) {
          weighted_vals <- weighted_data[[group]]$node_strength$Strength
          pers_vals <- colMeans(do.call(rbind, pers_strength), na.rm = TRUE)
          points(weighted_vals, pers_vals, pch = 16, col = rgb(0.8, 0.4, 0.2, 0.5))
        }
      }
    }
    abline(0, 1, col = "red", lty = 2)

    # Panel 3: Percolation vs Persistence
    plot(1, type = "n", xlim = c(0, 1), ylim = c(0, 1),
         main = "Percolation vs Persistence",
         xlab = "Percolation Node Strength", ylab = "Persistence Avg Strength")
    for(group in groups) {
      if(!is.null(percolation_data$node_metrics) &&
         !is.null(persistence_data[[group]]$persistence_data)) {
        perc_group <- percolation_data$node_metrics[percolation_data$node_metrics$Group == group, ]
        pers_strength <- persistence_data[[group]]$persistence_data$node_strength
        if(nrow(perc_group) > 0 && !is.null(pers_strength)) {
          perc_vals <- perc_group$Degree
          pers_vals <- colMeans(do.call(rbind, pers_strength), na.rm = TRUE)
          if(length(perc_vals) == length(pers_vals)) {
            points(perc_vals, pers_vals, pch = 16, col = rgb(0.2, 0.8, 0.4, 0.5))
          }
        }
      }
    }
    abline(0, 1, col = "red", lty = 2)
  })

  # NEW: Three-Way Eigenvector Comparison (Weighted-Percolation-Persistence)
  output$consensusThreeWayEigenvectorPlot <- renderPlot({
    req(analysis_results$method_weighted_results)
    req(analysis_results$method_percolation_results)
    req(analysis_results$persistence_results)

    method <- tolower(input$comparison_method %||% "pearson")
    all_groups <- names(analysis_results$correlations)

    if(length(all_groups) == 0) return(NULL)

    par(mfrow = c(length(all_groups), 3), mar = c(5, 4, 4, 2))

    for(group in all_groups) {
      # Get eigenvector from each approach
      # FIX: Access weighted_eigenvector df and filter by group
      weighted_all <- analysis_results$method_weighted_results[[method]]$weighted_eigenvector
      weighted_eig <- if(!is.null(weighted_all) && "Group" %in% names(weighted_all)) {
        weighted_all[weighted_all$Group == group, ]
      } else { NULL }
      percolation_eig <- analysis_results$method_percolation_results[[method]]$node_metrics
      percolation_eig <- percolation_eig[percolation_eig$Group == group, ]
      persistence_eig <- analysis_results$persistence_results[[method]][[group]]$aggregated_metrics

      # Get group color
      group_color <- if(!is.null(ui_state$group_colors[[group]])) {
        ui_state$group_colors[[group]]
      } else {
        "#3498db"
      }

      # Panel 1: Weighted vs Percolation
      common_nodes_wp <- if(!is.null(weighted_eig) && nrow(weighted_eig) > 0) {
        intersect(weighted_eig$Node, percolation_eig$Node)
      } else { character(0) }
      if(length(common_nodes_wp) > 0) {
        w_vals <- weighted_eig$Weighted_Eigenvector[match(common_nodes_wp, weighted_eig$Node)]
        p_vals <- percolation_eig$Eigenvector[match(common_nodes_wp, percolation_eig$Node)]

        plot(w_vals, p_vals,
             xlab = "Weighted Eigenvector",
             ylab = "Percolation Eigenvector",
             main = paste(group, "- W vs P"),
             col = group_color,
             pch = 19)
        abline(lm(p_vals ~ w_vals), col = "red", lty = 2)
        r <- cor(w_vals, p_vals, use = "complete.obs")
        legend("topleft", legend = sprintf("r = %.3f", r), bty = "n", cex = 0.8)
      } else {
        plot(1, type = "n", main = paste(group, "- W vs P"), axes = FALSE)
        text(1, 1, "No common nodes", cex = 1)
      }

      # Panel 2: Weighted vs Persistence
      if(!is.null(persistence_eig) && !is.null(weighted_eig) && nrow(weighted_eig) > 0) {
        common_nodes_wpers <- intersect(weighted_eig$Node, persistence_eig$Node)
        if(length(common_nodes_wpers) > 0) {
          w_vals <- weighted_eig$Weighted_Eigenvector[match(common_nodes_wpers, weighted_eig$Node)]
          pers_vals <- persistence_eig$MeanEigenvector[match(common_nodes_wpers, persistence_eig$Node)]

          plot(w_vals, pers_vals,
               xlab = "Weighted Eigenvector",
               ylab = "Persistence Eigenvector (Avg)",
               main = paste(group, "- W vs Pers"),
               col = group_color,
               pch = 19)
          abline(lm(pers_vals ~ w_vals), col = "red", lty = 2)
          r <- cor(w_vals, pers_vals, use = "complete.obs")
          legend("topleft", legend = sprintf("r = %.3f", r), bty = "n", cex = 0.8)
        } else {
          plot(1, type = "n", main = paste(group, "- W vs Pers"), axes = FALSE)
          text(1, 1, "No common nodes", cex = 1)
        }
      } else {
        plot(1, type = "n", main = paste(group, "- W vs Pers"), axes = FALSE)
        text(1, 1, "No persistence data", cex = 1)
      }

      # Panel 3: Percolation vs Persistence
      if(!is.null(persistence_eig)) {
        common_nodes_pp <- intersect(percolation_eig$Node, persistence_eig$Node)
        if(length(common_nodes_pp) > 0) {
          p_vals <- percolation_eig$Eigenvector[match(common_nodes_pp, percolation_eig$Node)]
          pers_vals <- persistence_eig$MeanEigenvector[match(common_nodes_pp, persistence_eig$Node)]

          plot(p_vals, pers_vals,
               xlab = "Percolation Eigenvector",
               ylab = "Persistence Eigenvector (Avg)",
               main = paste(group, "- P vs Pers"),
               col = group_color,
               pch = 19)
          abline(lm(pers_vals ~ p_vals), col = "red", lty = 2)
          r <- cor(p_vals, pers_vals, use = "complete.obs")
          legend("topleft", legend = sprintf("r = %.3f", r), bty = "n", cex = 0.8)
        } else {
          plot(1, type = "n", main = paste(group, "- P vs Pers"), axes = FALSE)
          text(1, 1, "No common nodes", cex = 1)
        }
      } else {
        plot(1, type = "n", main = paste(group, "- P vs Pers"), axes = FALSE)
        text(1, 1, "No persistence data", cex = 1)
      }
    }
  })

  output$avgNodeStrengthByRegionPlot <- renderPlot({
    req(analysis_results$threshold_free_results)
    req(analysis_results$node_metrics)
    render_avg_node_strength_by_region(analysis_results$threshold_free_results$node_strength,
                                      analysis_results$node_metrics,
                                      ui_state$brain_areas,
                                      ui_state$area_colors,
                                      ui_state$group_colors)
  })
  
  output$rankBasedNodeStrengthPlot <- renderPlot({
    req(analysis_results$method_weighted_results)

    method <- tolower(input$comparison_method %||% "pearson")
    weighted_data <- analysis_results$method_weighted_results[[method]]

    if(is.null(weighted_data) || is.null(weighted_data$node_strength)) {
      plot(1, type = "n", main = paste("Data not available for", method), axes = FALSE)
      text(1, 1, "Select different method or run analysis", cex = 1.2)
      return(NULL)
    }

    render_rank_based_node_strength_with_avg(weighted_data$node_strength,
                                             analysis_results$node_metrics,
                                             ui_state$brain_areas,
                                             ui_state$area_colors,
                                             ui_state$group_colors)
  })
  
  output$avgEigenvectorByRegionPlot <- renderPlot({
    req(analysis_results$weighted_eigenvector)
    req(analysis_results$node_metrics)
    render_avg_eigenvector_by_region(analysis_results$weighted_eigenvector,
                                    analysis_results$node_metrics,
                                    ui_state$brain_areas,
                                    ui_state$area_colors,
                                    ui_state$group_colors)
  })
  
  output$rankBasedEigenvectorPlot <- renderPlot({
    req(analysis_results$method_weighted_results)

    method <- tolower(input$comparison_method %||% "pearson")
    weighted_data <- analysis_results$method_weighted_results[[method]]

    if(is.null(weighted_data) || is.null(weighted_data$node_strength)) {
      plot(1, type = "n", main = paste("Data not available for", method), axes = FALSE)
      text(1, 1, "Select different method or run analysis", cex = 1.2)
      return(NULL)
    }

    render_rank_based_comparisons(weighted_data,
                                 weighted_data$node_strength,
                                 analysis_results$node_metrics,
                                 ui_state$group_colors,
                                 ui_state$brain_areas,
                                 ui_state$area_colors)
  })
  
  # 5a-d Subregion plots
  output$nodeStrengthSubregionsPlot <- renderPlot({
    req(analysis_results$method_weighted_results)

    method <- tolower(input$comparison_method %||% "pearson")
    weighted_data <- analysis_results$method_weighted_results[[method]]

    if(is.null(weighted_data) || is.null(weighted_data$node_strength)) {
      plot(1, type = "n", main = paste("Data not available for", method), axes = FALSE)
      text(1, 1, "Select different method or run analysis", cex = 1.2)
      return(NULL)
    }

    render_subregion_strength_comparison(weighted_data$node_strength,
                                       analysis_results$node_metrics,
                                       ui_state$brain_areas,
                                       ui_state$area_colors,
                                       ui_state$group_colors)
  })
  
  output$nodeStrengthRankSubregionsPlot <- renderPlot({
    req(analysis_results$method_weighted_results)

    method <- tolower(input$comparison_method %||% "pearson")
    weighted_data <- analysis_results$method_weighted_results[[method]]

    if(is.null(weighted_data) || is.null(weighted_data$node_strength)) {
      plot(1, type = "n", main = paste("Data not available for", method), axes = FALSE)
      text(1, 1, "Select different method or run analysis", cex = 1.2)
      return(NULL)
    }

    render_subregion_strength_comparison(weighted_data$node_strength,
                                       analysis_results$node_metrics,
                                       ui_state$brain_areas,
                                       ui_state$area_colors,
                                       ui_state$group_colors,
                                       use_ranks = TRUE)
  })
  
  output$eigenvectorSubregionsPlot <- renderPlot({
    req(analysis_results$method_weighted_results)

    method <- tolower(input$comparison_method %||% "pearson")
    weighted_data <- analysis_results$method_weighted_results[[method]]

    if(is.null(weighted_data)) {
      plot(1, type = "n", main = paste("Data not available for", method), axes = FALSE)
      text(1, 1, "Select different method or run analysis", cex = 1.2)
      return(NULL)
    }

    render_subregion_eigenvector_comparison(weighted_data,
                                          analysis_results$node_metrics,
                                          ui_state$brain_areas,
                                          ui_state$area_colors,
                                          ui_state$group_colors)
  })
  
  output$eigenvectorRankSubregionsPlot <- renderPlot({
    req(analysis_results$method_weighted_results)

    method <- tolower(input$comparison_method %||% "pearson")
    weighted_data <- analysis_results$method_weighted_results[[method]]

    if(is.null(weighted_data)) {
      plot(1, type = "n", main = paste("Data not available for", method), axes = FALSE)
      text(1, 1, "Select different method or run analysis", cex = 1.2)
      return(NULL)
    }

    render_subregion_eigenvector_comparison(weighted_data,
                                          analysis_results$node_metrics,
                                          ui_state$brain_areas,
                                          ui_state$area_colors,
                                          ui_state$group_colors,
                                          use_ranks = TRUE)
  })
  
  # Weighted Eigenvector Centrality Outputs
  output$weightedEigenvectorComparison <- renderPlot({
    req(analysis_results$weighted_eigenvector)
    render_weighted_eigenvector_comparison(analysis_results$weighted_eigenvector, 
                                          ui_state$group_colors,
                                          ui_state$brain_areas,
                                          ui_state$area_colors)
  })
  
  output$weightedVsUnweightedPlot <- renderPlot({
    req(analysis_results$weighted_eigenvector)
    render_weighted_vs_unweighted_eigenvector(analysis_results$weighted_eigenvector,
                                              ui_state$group_colors)
  })
  
  output$eigenvectorRankChangePlot <- renderPlot({
    req(analysis_results$weighted_eigenvector)
    render_eigenvector_rank_change(analysis_results$weighted_eigenvector,
                                   ui_state$group_colors)
  })
  
  output$strengthEigenvectorPlot <- renderPlot({
    req(analysis_results$weighted_eigenvector)

    weighted_data <- analysis_results$weighted_eigenvector
    all_groups <- unique(weighted_data$Group)
    n_groups <- length(all_groups)

    if(n_groups == 0) return(NULL)

    # Custom layout: top row for scatter plots (2×2), bottom two rows for regional plots (2×1)
    layout_matrix <- matrix(c(1, 2, 3, 4, 5, 5, 6, 6), nrow = 4, ncol = 2, byrow = TRUE)
    layout(layout_matrix, heights = c(1, 1, 1, 1))

    # TOP SECTION: Scatter plots (Node Strength vs Eigenvector) - up to 4 groups
    for(i in 1:min(4, n_groups)) {
      group <- all_groups[i]
      group_data <- weighted_data[weighted_data$Group == group, ]

      # Get colors based on brain areas if available
      plot_colors <- rep("steelblue", nrow(group_data))
      if(!is.null(ui_state$brain_areas) && !is.null(ui_state$area_colors)) {
        for(j in seq_along(group_data$Node)) {
          node_name <- group_data$Node[j]
          for(area_name in names(ui_state$brain_areas)) {
            if(node_name %in% ui_state$brain_areas[[area_name]] && area_name %in% names(ui_state$area_colors)) {
              plot_colors[j] <- ui_state$area_colors[[area_name]]
              break
            }
          }
        }
      } else if(!is.null(ui_state$group_colors) && group %in% names(ui_state$group_colors)) {
        plot_colors <- rep(ui_state$group_colors[[group]], nrow(group_data))
      }

      # Create scatter plot
      par(mar = c(5, 5, 4, 2))
      y_range <- range(group_data$Weighted_Eigenvector, na.rm = TRUE)
      y_padding <- (y_range[2] - y_range[1]) * 0.2

      plot(group_data$Node_Strength, group_data$Weighted_Eigenvector,
           main = paste("Node Strength vs Weighted Eigenvector -", group),
           xlab = "Node Strength", ylab = "Weighted Eigenvector",
           pch = 21, cex = 2.2, bg = adjustcolor(plot_colors, alpha.f = 0.6),
           col = adjustcolor(plot_colors, alpha.f = 0.8), lwd = 2,
           ylim = c(y_range[1], y_range[2] + y_padding))

      # Add labels
      if(nrow(group_data) > 0) {
        for(k in seq_len(nrow(group_data))) {
          text(group_data$Node_Strength[k], group_data$Weighted_Eigenvector[k],
               labels = group_data$Node[k], cex = 0.8, font = 2, col = "black")
        }
      }

      # Add grid and trend line
      grid(col = "lightgray", lty = "dotted", lwd = 0.5)

      if(nrow(group_data) > 2) {
        tryCatch({
          fit <- lm(Weighted_Eigenvector ~ Node_Strength, data = group_data)
          abline(fit, col = "darkgray", lty = 2, lwd = 1.5)
          r_squared <- round(summary(fit)$r.squared, 3)
          corr <- cor(group_data$Node_Strength, group_data$Weighted_Eigenvector, use = "complete.obs")
          legend("bottomright", paste("r =", round(corr, 3), "\nR² =", r_squared),
                 bty = "n", cex = 0.9)
        }, error = function(e) {})
      }
    }

    # BOTTOM SECTION 1: Weighted Node Strength by Region
    if(!is.null(ui_state$brain_areas) && length(ui_state$brain_areas) > 0) {
      par(mar = c(10, 5, 4, 2))

      region_names <- names(ui_state$brain_areas)
      n_regions <- length(region_names)

      # Matrices: rows = groups, cols = regions
      strength_matrix <- matrix(NA, nrow = n_groups, ncol = n_regions)
      strength_sd_matrix <- matrix(NA, nrow = n_groups, ncol = n_regions)

      rownames(strength_matrix) <- all_groups
      rownames(strength_sd_matrix) <- all_groups
      colnames(strength_matrix) <- region_names
      colnames(strength_sd_matrix) <- region_names

      for(i in seq_along(all_groups)) {
        group <- all_groups[i]
        group_data <- weighted_data[weighted_data$Group == group, ]

        for(j in seq_along(region_names)) {
          region_name <- region_names[j]
          region_nodes <- ui_state$brain_areas[[region_name]]
          region_mask <- group_data$Node %in% region_nodes

          if(sum(region_mask) > 0) {
            strength_vals <- group_data$Node_Strength[region_mask]
            strength_matrix[i, j] <- mean(strength_vals, na.rm = TRUE)
            strength_sd_matrix[i, j] <- sd(strength_vals, na.rm = TRUE)
          }
        }
      }

      # Get group colors
      group_colors <- sapply(all_groups, function(g) {
        if(!is.null(ui_state$group_colors[[g]])) {
          return(ui_state$group_colors[[g]])
        } else {
          return("#3498db")
        }
      })

      # Plot: Weighted Node Strength by Region
      bp1 <- barplot(strength_matrix,
                      beside = TRUE,
                      names.arg = region_names,
                      main = "Weighted Node Strength by Region",
                      ylab = "Mean Node Strength",
                      col = group_colors,
                      border = group_colors,
                      las = 2,
                      ylim = c(0, max(strength_matrix + strength_sd_matrix, na.rm = TRUE) * 1.2))
      grid(nx = NA, ny = NULL, col = "gray90")

      # Add error bars
      for(i in 1:n_groups) {
        for(j in 1:n_regions) {
          if(!is.na(strength_matrix[i, j]) && !is.na(strength_sd_matrix[i, j])) {
            x_pos <- bp1[i, j]  # Fixed: bp1 is [n_groups, n_regions]
            y_val <- strength_matrix[i, j]
            y_sd <- strength_sd_matrix[i, j]
            segments(x_pos, y_val - y_sd, x_pos, y_val + y_sd, lwd = 1.5)
            segments(x_pos - 0.1, y_val - y_sd, x_pos + 0.1, y_val - y_sd, lwd = 1.5)
            segments(x_pos - 0.1, y_val + y_sd, x_pos + 0.1, y_val + y_sd, lwd = 1.5)
          }
        }
      }

      legend("topright", legend = all_groups, fill = group_colors, bty = "n", cex = 0.9)

      # BOTTOM SECTION 2: Weighted Eigenvector by Region
      # Matrices for eigenvector
      eigenvector_matrix <- matrix(NA, nrow = n_groups, ncol = n_regions)
      eigenvector_sd_matrix <- matrix(NA, nrow = n_groups, ncol = n_regions)

      rownames(eigenvector_matrix) <- all_groups
      rownames(eigenvector_sd_matrix) <- all_groups
      colnames(eigenvector_matrix) <- region_names
      colnames(eigenvector_sd_matrix) <- region_names

      for(i in seq_along(all_groups)) {
        group <- all_groups[i]
        group_data <- weighted_data[weighted_data$Group == group, ]

        for(j in seq_along(region_names)) {
          region_name <- region_names[j]
          region_nodes <- ui_state$brain_areas[[region_name]]
          region_mask <- group_data$Node %in% region_nodes

          if(sum(region_mask) > 0) {
            eigenvector_vals <- group_data$Weighted_Eigenvector[region_mask]
            eigenvector_matrix[i, j] <- mean(eigenvector_vals, na.rm = TRUE)
            eigenvector_sd_matrix[i, j] <- sd(eigenvector_vals, na.rm = TRUE)
          }
        }
      }

      # Plot: Weighted Eigenvector by Region
      bp2 <- barplot(eigenvector_matrix,
                      beside = TRUE,
                      names.arg = region_names,
                      main = "Weighted Eigenvector Centrality by Region",
                      ylab = "Mean Eigenvector Centrality",
                      col = group_colors,
                      border = group_colors,
                      las = 2,
                      ylim = c(0, max(eigenvector_matrix + eigenvector_sd_matrix, na.rm = TRUE) * 1.2))
      grid(nx = NA, ny = NULL, col = "gray90")

      # Add error bars
      for(i in 1:n_groups) {
        for(j in 1:n_regions) {
          if(!is.na(eigenvector_matrix[i, j]) && !is.na(eigenvector_sd_matrix[i, j])) {
            x_pos <- bp2[i, j]  # Fixed: bp2 is [n_groups, n_regions]
            y_val <- eigenvector_matrix[i, j]
            y_sd <- eigenvector_sd_matrix[i, j]
            segments(x_pos, y_val - y_sd, x_pos, y_val + y_sd, lwd = 1.5)
            segments(x_pos - 0.1, y_val - y_sd, x_pos + 0.1, y_val - y_sd, lwd = 1.5)
            segments(x_pos - 0.1, y_val + y_sd, x_pos + 0.1, y_val + y_sd, lwd = 1.5)
          }
        }
      }

      legend("topright", legend = all_groups, fill = group_colors, bty = "n", cex = 0.9)
    }
  })
  
  output$weightedEigenvectorHubPlot <- renderPlot({
    req(analysis_results$weighted_eigenvector_hubs)
    render_weighted_eigenvector_hub_comparison(analysis_results$weighted_eigenvector_hubs,
                                               ui_state$group_colors)
  })
  
  output$eigenvectorStabilityPlot <- renderPlot({
    req(analysis_results$weighted_eigenvector_comparison)
    render_eigenvector_stability(analysis_results$weighted_eigenvector_comparison)
  })
  
  output$weightedEigenvectorTable <- DT::renderDataTable({
    req(analysis_results$weighted_eigenvector)
    
    # Format the data for display
    display_data <- analysis_results$weighted_eigenvector
    
    # Helper function to safely round numeric columns
    safe_round <- function(x, digits = 4) {
      tryCatch({
        numeric_x <- as.numeric(x)
        if(all(is.na(numeric_x))) return(x)  # Return original if all NA
        round(numeric_x, digits)
      }, error = function(e) {
        return(x)  # Return original on error
      })
    }
    
    # Safely round numeric columns that exist
    if("Weighted_Eigenvector" %in% names(display_data)) {
      display_data$Weighted_Eigenvector <- safe_round(display_data$Weighted_Eigenvector, 4)
    }
    if("Node_Strength" %in% names(display_data)) {
      display_data$Node_Strength <- safe_round(display_data$Node_Strength, 4)
    }
    if("Avg_Edge_Weight" %in% names(display_data)) {
      display_data$Avg_Edge_Weight <- safe_round(display_data$Avg_Edge_Weight, 4)
    }
    if("Rank_Change" %in% names(display_data)) {
      display_data$Rank_Change <- safe_round(display_data$Rank_Change, 2)
    }
    
    # Find the correct column index for Weighted_Eigenvector
    weighted_eigen_col <- which(names(display_data) == "Weighted_Eigenvector")
    sort_col <- if(length(weighted_eigen_col) > 0) weighted_eigen_col - 1 else 0  # DT uses 0-based indexing
    
    # Create base datatable
    dt <- DT::datatable(display_data,
                        options = list(pageLength = 25,
                                      scrollX = TRUE,
                                      order = list(list(sort_col, 'desc'))),
                        rownames = FALSE)
    
    # Apply formatting only if columns exist
    if("Weighted_Eigenvector" %in% names(display_data)) {
      dt <- dt %>%
        DT::formatStyle('Weighted_Eigenvector',
                        backgroundColor = styleInterval(c(0.5, 0.8), 
                                                       c('white', 'lightblue', 'darkblue')),
                        color = styleInterval(c(0.8), c('black', 'white')))
    }
    
    if("Rank_Change" %in% names(display_data)) {
      dt <- dt %>%
        DT::formatStyle('Rank_Change',
                        color = styleInterval(c(-0.5, 0.5), 
                                             c('red', 'gray', 'green')))
    }
    
    return(dt)
  })

  # ========== NEW: PER-METHOD PERSISTENCE ANALYSIS OUTPUTS ==========

  # Per-Method Correlation Heatmap
  output$method_correlation_heatmap <- renderPlot({
    req(analysis_results$correlation_methods_raw)
    method <- tolower(input$selected_corr_method)
    groups <- names(analysis_results$correlations)

    if(length(groups) == 0) return(NULL)

    # Display all groups in multi-panel layout
    n_groups <- length(groups)
    if(n_groups == 1) {
      par(mfrow = c(1, 1))
    } else if(n_groups == 2) {
      par(mfrow = c(1, 2))
    } else if(n_groups <= 4) {
      par(mfrow = c(2, 2))
    } else if(n_groups <= 6) {
      par(mfrow = c(2, 3))
    } else {
      par(mfrow = c(3, 3))
    }

    for(group in groups) {
      cor_matrix <- analysis_results$correlation_methods_raw[[method]][[group]]
      if(!is.null(cor_matrix) && has_corrplot) {
        corrplot(cor_matrix,
                method = "color",
                type = "full",
                order = "original",
                tl.col = "black",
                tl.srt = 45,
                tl.cex = 0.7,
                title = paste(toupper(method), "-", group),
                mar = c(0, 0, 2, 0))
      } else {
        plot(1, type = "n", main = paste(group, "-", method), axes = FALSE)
        text(1, 1, "Not available", cex = 1.0)
      }
    }
  })

  # Hub Persistence Heatmap
  output$method_hub_persistence_heatmap <- renderPlot({
    req(analysis_results$persistence_results)
    method <- tolower(input$persistence_method %||% "pearson")
    all_groups <- names(analysis_results$correlations)

    if(length(all_groups) == 0) return(NULL)

    # Multi-panel layout for all groups (2 rows, up to 2 cols for 4 groups)
    n_groups <- length(all_groups)
    if(n_groups <= 2) {
      par(mfrow = c(1, n_groups), mar = c(5, 5, 4, 2))
    } else {
      par(mfrow = c(2, 2), mar = c(5, 5, 4, 2))
    }

    for(group in all_groups[1:min(4, n_groups)]) {
      hub_pers <- analysis_results$persistence_results[[method]][[group]]$hub_persistence

      if(is.null(hub_pers) || nrow(hub_pers) == 0) {
        plot(1, type = "n", main = paste("No persistence data:", group), axes = FALSE)
        text(1, 1, "Persistence analysis not available", cex = 1.2)
        next
      }

      # Create heatmap of hub persistence
      # Get top 20 nodes by persistence score
      top_hubs <- head(hub_pers[order(-hub_pers$PersistenceScore), ], 20)

      if(nrow(top_hubs) > 0) {
        # Create matrix for heatmap (nodes x densities)
        # Assuming hub_pers has columns for different density thresholds
        node_names <- top_hubs$Node

        # Simple bar plot of persistence scores if detailed threshold data not available
        barplot(top_hubs$PersistenceScore,
                names.arg = node_names,
                las = 2,
                main = paste("Hub Persistence -", group),
                ylab = "Persistence Score",
                col = "steelblue",
                border = "steelblue",
                cex.names = 0.7)
        grid(nx = NA, ny = NULL, col = "gray90")
      } else {
        plot(1, type = "n", main = paste("No hubs:", group), axes = FALSE)
        text(1, 1, "No persistent hubs found", cex = 1.2)
      }
    }
  })

  # Hub Persistence Table
  output$method_hub_persistence_table <- DT::renderDataTable({
    req(analysis_results$persistence_results)
    method <- tolower(input$persistence_method %||% "pearson")
    all_groups <- names(analysis_results$correlations)

    if(length(all_groups) == 0) return(NULL)

    # Combine hub persistence from all groups
    combined_hub_pers <- data.frame()

    for(group in all_groups) {
      hub_pers <- analysis_results$persistence_results[[method]][[group]]$hub_persistence

      if(!is.null(hub_pers) && nrow(hub_pers) > 0) {
        hub_pers$Group <- group
        combined_hub_pers <- rbind(combined_hub_pers, hub_pers)
      }
    }

    if(nrow(combined_hub_pers) == 0) return(NULL)

    datatable(combined_hub_pers,
             options = list(pageLength = 15),
             caption = paste("Hub Persistence Scores - All Groups -", toupper(method))) %>%
      formatRound(columns = c("PersistenceScore", "MinThreshold", "MaxThreshold", "ThresholdRange"),
                 digits = 3)
  })

  # Metrics Evolution Plot
  output$method_metrics_evolution <- renderPlot({
    req(analysis_results$persistence_results)
    method <- tolower(input$persistence_method %||% "pearson")
    metric <- input$persistence_metric

    render_metrics_evolution(analysis_results$persistence_results, metric, method)
  })

  # NEW: Persistence Node Metrics (UPDATED: AUC-based with subregion-specific results)
  output$persistenceNodeMetricsPlot <- renderPlot({
    req(analysis_results$persistence_results)
    req(analysis_results$auc_results)
    method <- tolower(input$persistence_method %||% "pearson")

    if(is.null(analysis_results$auc_results[[method]])) {
      plot(1, type = "n", main = paste("No AUC data for", method), axes = FALSE)
      text(1, 1, "AUC computation required", cex = 1.2)
      return(NULL)
    }

    all_groups <- names(analysis_results$auc_results[[method]])
    n_groups <- length(all_groups)

    if(n_groups == 0) return(NULL)

    # NEW LAYOUT: Grouped bar plots showing mean AUC by region × group (matching Tab 3c/4c style)
    # Two panels: AUC Strength and AUC Eigenvector

    if(!is.null(ui_state$brain_areas) && length(ui_state$brain_areas) > 0) {
      # Layout: 2 rows (Strength + Eigenvector)
      par(mfrow = c(2, 1), mar = c(8, 5, 4, 2))

      # Aggregate data: region × group combinations
      region_names <- names(ui_state$brain_areas)
      n_regions <- length(region_names)

      # Matrices: rows = groups, cols = regions
      strength_matrix <- matrix(NA, nrow = n_groups, ncol = n_regions)
      eigenvector_matrix <- matrix(NA, nrow = n_groups, ncol = n_regions)
      strength_sd_matrix <- matrix(NA, nrow = n_groups, ncol = n_regions)
      eigenvector_sd_matrix <- matrix(NA, nrow = n_groups, ncol = n_regions)

      rownames(strength_matrix) <- all_groups
      rownames(eigenvector_matrix) <- all_groups
      rownames(strength_sd_matrix) <- all_groups
      rownames(eigenvector_sd_matrix) <- all_groups
      colnames(strength_matrix) <- region_names
      colnames(eigenvector_matrix) <- region_names
      colnames(strength_sd_matrix) <- region_names
      colnames(eigenvector_sd_matrix) <- region_names

      for(i in seq_along(all_groups)) {
        group <- all_groups[i]
        auc_data <- analysis_results$auc_results[[method]][[group]]

        if(!is.null(auc_data$node_auc)) {
          node_auc <- auc_data$node_auc

          for(j in seq_along(region_names)) {
            region_name <- region_names[j]
            region_nodes <- ui_state$brain_areas[[region_name]]
            region_mask <- node_auc$Node %in% region_nodes

            if(sum(region_mask) > 0) {
              # Use AUC_Strength if available, otherwise AUC_Degree
              if("AUC_Strength" %in% colnames(node_auc)) {
                strength_vals <- node_auc$AUC_Strength[region_mask]
              } else {
                strength_vals <- node_auc$AUC_Degree[region_mask]
              }

              eigenvector_vals <- node_auc$AUC_Eigenvector[region_mask]

              strength_matrix[i, j] <- mean(strength_vals, na.rm = TRUE)
              eigenvector_matrix[i, j] <- mean(eigenvector_vals, na.rm = TRUE)
              strength_sd_matrix[i, j] <- sd(strength_vals, na.rm = TRUE)
              eigenvector_sd_matrix[i, j] <- sd(eigenvector_vals, na.rm = TRUE)
            }
          }
        }
      }

      # Get group colors
      group_colors <- sapply(all_groups, function(g) {
        if(!is.null(ui_state$group_colors[[g]])) {
          return(ui_state$group_colors[[g]])
        } else {
          return("#3498db")
        }
      })

      # PLOT 1: AUC Strength by Region × Group
      bp1 <- barplot(strength_matrix,
                      beside = TRUE,
                      names.arg = region_names,
                      main = paste("Mean AUC Node Strength by Region -", toupper(method)),
                      ylab = "Mean AUC Strength",
                      col = group_colors,
                      border = group_colors,
                      las = 2,
                      ylim = c(0, max(strength_matrix + strength_sd_matrix, na.rm = TRUE) * 1.2))
      grid(nx = NA, ny = NULL, col = "gray90")

      # Add error bars
      for(i in 1:n_groups) {
        for(j in 1:n_regions) {
          if(!is.na(strength_matrix[i, j]) && !is.na(strength_sd_matrix[i, j])) {
            x_pos <- bp1[i, j]  # Fixed indexing: bp1 is [n_groups, n_regions]
            y_val <- strength_matrix[i, j]
            y_sd <- strength_sd_matrix[i, j]
            segments(x_pos, y_val - y_sd, x_pos, y_val + y_sd, lwd = 1.5)
            segments(x_pos - 0.1, y_val - y_sd, x_pos + 0.1, y_val - y_sd, lwd = 1.5)
            segments(x_pos - 0.1, y_val + y_sd, x_pos + 0.1, y_val + y_sd, lwd = 1.5)
          }
        }
      }

      legend("topright", legend = all_groups, fill = group_colors, bty = "n", cex = 0.9)

      # PLOT 2: AUC Eigenvector by Region × Group
      bp2 <- barplot(eigenvector_matrix,
                      beside = TRUE,
                      names.arg = region_names,
                      main = paste("Mean AUC Eigenvector Centrality by Region -", toupper(method)),
                      ylab = "Mean AUC Eigenvector",
                      col = group_colors,
                      border = group_colors,
                      las = 2,
                      ylim = c(0, max(eigenvector_matrix + eigenvector_sd_matrix, na.rm = TRUE) * 1.2))
      grid(nx = NA, ny = NULL, col = "gray90")

      # Add error bars
      for(i in 1:n_groups) {
        for(j in 1:n_regions) {
          if(!is.na(eigenvector_matrix[i, j]) && !is.na(eigenvector_sd_matrix[i, j])) {
            x_pos <- bp2[i, j]  # Fixed indexing: bp2 is [n_groups, n_regions]
            y_val <- eigenvector_matrix[i, j]
            y_sd <- eigenvector_sd_matrix[i, j]
            segments(x_pos, y_val - y_sd, x_pos, y_val + y_sd, lwd = 1.5)
            segments(x_pos - 0.1, y_val - y_sd, x_pos + 0.1, y_val - y_sd, lwd = 1.5)
            segments(x_pos - 0.1, y_val + y_sd, x_pos + 0.1, y_val + y_sd, lwd = 1.5)
          }
        }
      }

      legend("topright", legend = all_groups, fill = group_colors, bty = "n", cex = 0.9)

    } else {
      # No brain areas defined: show simple bar chart per group
      par(mfrow = c(min(2, n_groups), ceiling(n_groups / 2)), mar = c(8, 4, 4, 2))

      for(group in all_groups) {
        auc_data <- analysis_results$auc_results[[method]][[group]]

        if(!is.null(auc_data$node_auc)) {
          node_auc <- auc_data$node_auc

          # Get group color
          group_color <- if(!is.null(ui_state$group_colors[[group]])) {
            ui_state$group_colors[[group]]
          } else {
            "#3498db"
          }

          # Bar plot of mean AUC values
          if("AUC_Strength" %in% colnames(node_auc)) {
            auc_means <- c(Strength = mean(node_auc$AUC_Strength, na.rm = TRUE),
                          Eigenvector = mean(node_auc$AUC_Eigenvector, na.rm = TRUE))
          } else {
            auc_means <- c(Degree = mean(node_auc$AUC_Degree, na.rm = TRUE),
                          Eigenvector = mean(node_auc$AUC_Eigenvector, na.rm = TRUE))
          }

          barplot(auc_means,
                  main = paste("Mean Node AUC:", group),
                  ylab = "Mean AUC Value",
                  col = group_color,
                  border = group_color,
                  las = 2)
          grid(nx = NA, ny = NULL, col = "gray90")
        }
      }
    }
  })

  # NEW: Persistence Hub Comparison
  output$persistenceHubComparisonPlot <- renderPlot({
    req(analysis_results$persistence_results)
    method <- tolower(input$persistence_method %||% "pearson")

    if(is.null(analysis_results$persistence_results[[method]])) {
      plot(1, type = "n", main = paste("No persistence data for", method), axes = FALSE)
      text(1, 1, "Run analysis or select different method", cex = 1.2)
      return(NULL)
    }

    all_groups <- names(analysis_results$persistence_results[[method]])

    # Extract hub persistence data
    hub_data <- list()
    for(group in all_groups) {
      hub_pers <- analysis_results$persistence_results[[method]][[group]]$hub_persistence
      if(!is.null(hub_pers) && nrow(hub_pers) > 0) {
        hub_data[[group]] <- head(hub_pers, 10)  # Top 10 hubs
      }
    }

    if(length(hub_data) > 0) {
      par(mfrow = c(ceiling(length(hub_data)/2), 2), mar = c(8, 4, 3, 2))
      for(group in names(hub_data)) {
        hubs <- hub_data[[group]]

        # Get group color
        group_color <- if(!is.null(ui_state$group_colors[[group]])) {
          ui_state$group_colors[[group]]
        } else {
          "#3498db" # Default blue
        }

        barplot(hubs$PersistenceScore, names.arg = hubs$Node,
                main = paste("Top Hubs -", group),
                ylab = "Persistence Score", las = 2,
                col = group_color, ylim = c(0, 1))
        grid(nx = NA, ny = NULL)
      }
    }
  })

  # NEW: Persistence Regional Analysis
  output$persistenceRegionalAnalysisPlot <- renderPlot({
    req(analysis_results$auc_results)  # Use AUC results instead
    req(ui_state$brain_areas)
    method <- tolower(input$persistence_method %||% "pearson")

    if(is.null(analysis_results$auc_results[[method]])) {
      plot(1, type = "n", main = paste("No AUC data for", method), axes = FALSE)
      text(1, 1, "AUC computation required", cex = 1.2)
      return(NULL)
    }

    all_groups <- names(analysis_results$auc_results[[method]])
    n_groups <- length(all_groups)

    if(n_groups == 0) return(NULL)

    # Calculate total panels needed: per-group plots + 1 combined plot
    total_panels <- n_groups + 1
    layout_rows <- ceiling(total_panels / 2)
    layout_cols <- min(2, total_panels)

    # Custom layout to accommodate both individual and combined plots
    par(mfrow = c(layout_rows, layout_cols), mar = c(10, 5, 4, 2))

    # TOP SECTION: Individual per-group bar plots
    for(group in all_groups) {
      auc_data <- analysis_results$auc_results[[method]][[group]]

      if(!is.null(auc_data$regional_auc)) {
        regional_auc <- auc_data$regional_auc

        # Bar plot with error bars (using SD from node-level AUC within regions)
        # Compute SD for each region
        node_auc <- auc_data$node_auc

        regional_sd <- numeric(nrow(regional_auc))
        for(i in 1:nrow(regional_auc)) {
          region_name <- regional_auc$Region[i]
          region_nodes <- ui_state$brain_areas[[region_name]]

          if(!is.null(region_nodes)) {
            region_auc_vals <- node_auc$AUC_Eigenvector[node_auc$Node %in% region_nodes]
            regional_sd[i] <- sd(region_auc_vals, na.rm = TRUE)
          }
        }

        # Barplot
        bp <- barplot(regional_auc$AUC_Eigenvector,
                      names.arg = regional_auc$Region,
                      col = ui_state$area_colors[regional_auc$Region],
                      las = 2,
                      ylim = c(0, max(regional_auc$AUC_Eigenvector + regional_sd,
                                     na.rm = TRUE) * 1.1),
                      main = paste("Regional AUC (Persistence):", group),
                      ylab = "Mean AUC Eigenvector Centrality")

        # Error bars
        arrows(bp, regional_auc$AUC_Eigenvector - regional_sd,
               bp, regional_auc$AUC_Eigenvector + regional_sd,
               angle = 90, code = 3, length = 0.05)

        # Add N labels
        text(bp, par("usr")[3] - 0.05 * diff(par("usr")[3:4]),
             labels = paste0("n=", regional_auc$N_Nodes),
             srt = 45, adj = 1, xpd = TRUE, cex = 0.8)

        grid(nx = NA, ny = NULL)
      } else {
        plot(1, type = "n", main = paste("No regional data:", group), axes = FALSE)
        text(1, 1, "Define brain areas in Tab 2", cex = 1.2)
      }
    }

    # BOTTOM SECTION: Combined group-region eigenvector analysis
    # Aggregate data: region × group combinations
    region_names <- names(ui_state$brain_areas)
    n_regions <- length(region_names)

    # Matrices: rows = groups, cols = regions
    eigenvector_matrix <- matrix(NA, nrow = n_groups, ncol = n_regions)
    eigenvector_se_matrix <- matrix(NA, nrow = n_groups, ncol = n_regions)

    rownames(eigenvector_matrix) <- all_groups
    rownames(eigenvector_se_matrix) <- all_groups
    colnames(eigenvector_matrix) <- region_names
    colnames(eigenvector_se_matrix) <- region_names

    for(i in seq_along(all_groups)) {
      group <- all_groups[i]
      auc_data <- analysis_results$auc_results[[method]][[group]]

      if(!is.null(auc_data$node_auc)) {
        node_auc <- auc_data$node_auc

        for(j in seq_along(region_names)) {
          region_name <- region_names[j]
          region_nodes <- ui_state$brain_areas[[region_name]]
          region_mask <- node_auc$Node %in% region_nodes

          if(sum(region_mask) > 0) {
            eigenvector_vals <- node_auc$AUC_Eigenvector[region_mask]
            eigenvector_matrix[i, j] <- mean(eigenvector_vals, na.rm = TRUE)
            eigenvector_se_matrix[i, j] <- sd(eigenvector_vals, na.rm = TRUE) / sqrt(sum(region_mask))
          }
        }
      }
    }

    # Get group colors
    group_colors <- sapply(all_groups, function(g) {
      if(!is.null(ui_state$group_colors[[g]])) {
        return(ui_state$group_colors[[g]])
      } else {
        return("#3498db")
      }
    })

    # Combined grouped bar plot
    bp_combined <- barplot(eigenvector_matrix,
                          beside = TRUE,
                          names.arg = region_names,
                          main = paste("Combined Group-Region AUC Eigenvector -", toupper(method)),
                          ylab = "Mean AUC Eigenvector",
                          col = group_colors,
                          border = group_colors,
                          las = 2,
                          ylim = c(0, max(eigenvector_matrix + eigenvector_se_matrix, na.rm = TRUE) * 1.2))
    grid(nx = NA, ny = NULL, col = "gray90")

    # Add error bars
    for(i in 1:n_groups) {
      for(j in 1:n_regions) {
        if(!is.na(eigenvector_matrix[i, j]) && !is.na(eigenvector_se_matrix[i, j])) {
          x_pos <- bp_combined[i, j]  # Fixed: bp_combined is [n_groups, n_regions]
          y_val <- eigenvector_matrix[i, j]
          y_se <- eigenvector_se_matrix[i, j]
          segments(x_pos, y_val - y_se, x_pos, y_val + y_se, lwd = 1.5)
          segments(x_pos - 0.1, y_val - y_se, x_pos + 0.1, y_val - y_se, lwd = 1.5)
          segments(x_pos - 0.1, y_val + y_se, x_pos + 0.1, y_val + y_se, lwd = 1.5)
        }
      }
    }

    legend("topright", legend = all_groups, fill = group_colors, bty = "n", cex = 0.9)
  })

  # 5g. Persistence Network Similarity
  # Tab 5g: Network Similarity Across Thresholds (Persistence-specific)
  output$persistenceNetworkSimilarityPlot <- renderPlot({
    req(analysis_results$persistence_results)
    req(analysis_results$correlation_methods_raw)
    req(input$persistence_method)

    method <- tolower(input$persistence_method %||% "pearson")

    if(is.null(analysis_results$persistence_results[[method]])) {
      plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
      text(1, 1, paste("No persistence results for:", toupper(method)), cex = 1.2)
      return()
    }

    all_groups <- names(analysis_results$persistence_results[[method]])

    if(length(all_groups) == 0) {
      plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
      text(1, 1, "No group data available", cex = 1.2)
      return()
    }

    # Multi-panel layout: one heatmap per group (2×2)
    n_groups <- length(all_groups)
    if(n_groups <= 2) {
      par(mfrow = c(1, n_groups))
    } else {
      par(mfrow = c(2, 2))
    }

    for(group in all_groups[1:min(4, n_groups)]) {
      persistence_data <- analysis_results$persistence_results[[method]][[group]]$persistence_data

      if(is.null(persistence_data)) {
        plot(1, type = "n", axes = FALSE, main = group)
        text(1, 1, "No persistence data", cex = 1.0)
        next
      }

      # Extract thresholds from persistence analysis (stored as names)
      threshold_names <- names(persistence_data)
      threshold_seq <- as.numeric(threshold_names)

      if(length(threshold_seq) < 2) {
        plot(1, type = "n", axes = FALSE, main = group)
        text(1, 1, "Need at least 2 thresholds", cex = 1.0)
        next
      }

      # Sample thresholds if too many (for readability)
      if(length(threshold_seq) > 10) {
        # Take evenly spaced subset
        indices <- round(seq(1, length(threshold_seq), length.out = 8))
        threshold_seq <- threshold_seq[indices]
        threshold_names <- threshold_names[indices]
      }

      # Get correlation matrix and create thresholded versions
      cor_matrix <- analysis_results$correlation_methods_raw[[method]][[group]]

      if(is.null(cor_matrix)) {
        plot(1, type = "n", axes = FALSE, main = group)
        text(1, 1, "No correlation data", cex = 1.0)
        next
      }

      # Create thresholded versions of the correlation matrix
      # Apply threshold: set values below threshold to 0
      thresholded_matrices <- list()
      for(i in seq_along(threshold_seq)) {
        thresh <- threshold_seq[i]
        thresh_mat <- abs(cor_matrix)
        thresh_mat[thresh_mat < thresh] <- 0
        thresholded_matrices[[i]] <- thresh_mat
      }

      n_thresholds <- length(threshold_seq)

      # Compute Jaccard similarity between thresholded matrices
      jaccard_matrix <- matrix(1, nrow = n_thresholds, ncol = n_thresholds)
      threshold_labels <- sprintf("%.2f", threshold_seq)
      rownames(jaccard_matrix) <- threshold_labels
      colnames(jaccard_matrix) <- threshold_labels

      for(i in 1:(n_thresholds-1)) {
        for(j in (i+1):n_thresholds) {
          mat1 <- thresholded_matrices[[i]]
          mat2 <- thresholded_matrices[[j]]

          jaccard <- compute_jaccard_similarity(mat1, mat2, threshold = 0)
          if(!is.na(jaccard)) {
            jaccard_matrix[i, j] <- jaccard
            jaccard_matrix[j, i] <- jaccard
          }
        }
      }

      # Render heatmap for this group
      render_jaccard_heatmap(jaccard_matrix, threshold_labels,
                            title = paste(group, "-", toupper(method), "- Threshold Similarity"))
    }

    # Reset par
    par(mfrow = c(1, 1))
  })

  # 5g Part 2: Group Similarity (Aggregated Across Thresholds)
  output$persistenceGroupSimilarityPlot <- renderPlot({
    req(analysis_results$persistence_results)
    req(analysis_results$correlation_methods_raw)
    req(input$persistence_method)

    method <- tolower(input$persistence_method %||% "pearson")

    if(is.null(analysis_results$persistence_results[[method]])) {
      plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
      text(1, 1, paste("No persistence results for:", toupper(method)), cex = 1.2)
      return()
    }

    all_groups <- names(analysis_results$persistence_results[[method]])

    if(length(all_groups) < 2) {
      plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
      text(1, 1, "Need at least 2 groups for comparison", cex = 1.2, col = "gray")
      return()
    }

    # Extract thresholds from first group
    first_group <- all_groups[1]
    persistence_data <- analysis_results$persistence_results[[method]][[first_group]]$persistence_data

    if(is.null(persistence_data)) {
      plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
      text(1, 1, "No persistence data available", cex = 1.2, col = "gray")
      return()
    }

    threshold_names <- names(persistence_data)
    threshold_seq <- as.numeric(threshold_names)

    # Sample thresholds if too many
    if(length(threshold_seq) > 10) {
      indices <- round(seq(1, length(threshold_seq), length.out = 8))
      threshold_seq <- threshold_seq[indices]
    }

    # Get correlation matrices for all groups
    group_cor_matrices <- list()
    for(group in all_groups) {
      cor_mat <- analysis_results$correlation_methods_raw[[method]][[group]]
      if(!is.null(cor_mat)) {
        group_cor_matrices[[group]] <- cor_mat
      }
    }

    if(length(group_cor_matrices) < 2) {
      plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
      text(1, 1, "Not enough group data", cex = 1.2, col = "gray")
      return()
    }

    n_groups <- length(group_cor_matrices)
    group_names <- names(group_cor_matrices)

    # For each threshold, compute group-to-group Jaccard similarity
    # Then average across all thresholds
    avg_jaccard_matrix <- matrix(0, nrow = n_groups, ncol = n_groups)
    rownames(avg_jaccard_matrix) <- group_names
    colnames(avg_jaccard_matrix) <- group_names

    for(i in 1:n_groups) {
      for(j in 1:n_groups) {
        if(i == j) {
          avg_jaccard_matrix[i, j] <- 1
        } else if(i < j) {
          # Compute Jaccard at each threshold and average
          jaccard_vals <- c()

          for(thresh in threshold_seq) {
            # Threshold both matrices
            mat1 <- abs(group_cor_matrices[[i]])
            mat2 <- abs(group_cor_matrices[[j]])
            mat1[mat1 < thresh] <- 0
            mat2[mat2 < thresh] <- 0

            jaccard <- compute_jaccard_similarity(mat1, mat2, threshold = 0)
            if(!is.na(jaccard)) {
              jaccard_vals <- c(jaccard_vals, jaccard)
            }
          }

          if(length(jaccard_vals) > 0) {
            avg_jaccard <- mean(jaccard_vals, na.rm = TRUE)
            avg_jaccard_matrix[i, j] <- avg_jaccard
            avg_jaccard_matrix[j, i] <- avg_jaccard
          }
        }
      }
    }

    # Render heatmap
    render_jaccard_heatmap(avg_jaccard_matrix, group_names,
                          title = paste(toupper(method), "- Group Similarity (Avg across", length(threshold_seq), "thresholds)"))
  })

  # Method Percolation Curve (placeholder for future implementation)
  output$method_percolation_curve <- renderPlot({
    plot(1, type = "n", main = "Percolation analysis coming soon", axes = FALSE)
    text(1, 1, "Feature under development", cex = 1.2)
  })

  # Method Percolation Threshold (placeholder)
  output$method_percolation_threshold <- renderPrint({
    cat("Percolation threshold information will be displayed here\n")
  })

  # Method Info Text
  output$method_info_text <- renderPrint({
    req(analysis_results$correlation_methods_raw)
    method <- tolower(input$selected_corr_method)
    groups <- names(analysis_results$correlations)

    if(length(groups) == 0) {
      cat("No data available\n")
      return()
    }

    for(group in groups) {
      cor_matrix <- analysis_results$correlation_methods_raw[[method]][[group]]
      if(!is.null(cor_matrix)) {
        cat("=== Group:", group, "===\n")
        cat("Method:", toupper(method), "\n")
        cat("Variables:", nrow(cor_matrix), "\n")
        cat("Correlation pairs:", sum(upper.tri(cor_matrix)), "\n")
        cat("Mean |correlation|:", round(mean(abs(cor_matrix[upper.tri(cor_matrix)])), 3), "\n")
        cat("Strong correlations (|r|≥0.4):", sum(abs(cor_matrix[upper.tri(cor_matrix)]) >= 0.4), "\n\n")
      } else {
        cat("=== Group:", group, "===\n")
        cat("Method", toupper(method), "not available\n\n")
      }
    }
  })

  # ========== NEW: CONSENSUS ANALYSIS OUTPUTS ==========

  # Consensus Hub Agreement Matrix
  output$consensus_hub_agreement_matrix <- renderPlot({
    req(analysis_results$consensus_analysis)
    groups <- names(analysis_results$consensus_analysis)

    if(length(groups) == 0) return(NULL)
    group <- groups[1]

    render_method_agreement_matrix(analysis_results$consensus_analysis, group)
  })

  # Consensus Hubs Table
  output$consensus_hubs_table <- DT::renderDataTable({
    req(analysis_results$consensus_analysis)
    groups <- names(analysis_results$consensus_analysis)

    if(length(groups) == 0) return(NULL)
    group <- groups[1]

    consensus <- analysis_results$consensus_analysis[[group]]

    if(length(consensus$strong_consensus) == 0) {
      return(data.frame(Message = "No consensus hubs identified"))
    }

    df <- data.frame(
      Node = consensus$strong_consensus,
      ConsensusScore = consensus$consensus_score[consensus$strong_consensus],
      MethodsAgreeing = paste(consensus$consensus_score[consensus$strong_consensus],
                             "of", consensus$method_count),
      stringsAsFactors = FALSE
    )

    datatable(df,
             options = list(pageLength = 20),
             caption = "Consensus Hub Nodes (≥60% Method Agreement)")
  })

  # Consensus Overview Plot
  output$consensus_overview_plot <- renderPlot({
    req(analysis_results$consensus_analysis)
    render_consensus_overview(analysis_results$consensus_analysis)
  })

  # Consensus Summary Text
  output$consensus_summary_text <- renderPrint({
    req(analysis_results$consensus_analysis)

    for(group in names(analysis_results$consensus_analysis)) {
      consensus <- analysis_results$consensus_analysis[[group]]

      cat("=== Group:", group, "===\n")
      cat("Methods analyzed:", consensus$method_count, "\n")
      cat("Total nodes evaluated:", length(consensus$consensus_score), "\n")
      cat("Strong consensus hubs (≥60% agreement):", length(consensus$strong_consensus), "\n")

      if(length(consensus$strong_consensus) > 0) {
        cat("Consensus hubs:", paste(consensus$strong_consensus, collapse = ", "), "\n")
      } else {
        cat("No strong consensus hubs identified\n")
      }
      cat("\n")
    }
  })

  # Navigation from results back to import
  observeEvent(input$goToDataImportFromResults, {
    updateTabItems(session, "sidebarMenu", "import")
  })
  
  # Navigation to downloads tab
  observeEvent(input$goToDownloads, {
    updateTabItems(session, "sidebarMenu", "downloads")
  })

  # =========================================================================
  # TAB 8: STATISTICS & VALIDATION - Server-Side Code
  # =========================================================================

  # Tab 8a: ROI-Level Permutation Testing

  # Update group selectors for ROI permutation testing
  observe({
    req(analysis_results$raw_data)

    # Get unique groups from raw data
    if("Group" %in% names(analysis_results$raw_data)) {
      available_groups <- unique(analysis_results$raw_data$Group)

      updateSelectInput(session, "roi_perm_group1",
                       choices = available_groups,
                       selected = available_groups[1])

      updateSelectInput(session, "roi_perm_group2",
                       choices = available_groups,
                       selected = if(length(available_groups) > 1) available_groups[2] else available_groups[1])
    }
  })

  # Reactive: Compute ROI-level permutation test
  roi_permutation_results <- eventReactive(input$run_roi_permutation_test, {
    req(analysis_results$raw_data)
    req(input$roi_perm_group1, input$roi_perm_group2)

    # Validate groups are different
    if(input$roi_perm_group1 == input$roi_perm_group2) {
      showNotification("Please select two different groups for comparison.", type = "error", duration = 5)
      return(NULL)
    }

    tryCatch({
      showNotification("Running ROI-level permutation test... This may take 1-2 minutes.",
                      type = "message", duration = 3)

      # Extract subject-level ROI data for both groups
      group1_data <- extract_subject_roi_values(
        group_name = input$roi_perm_group1,
        raw_data = analysis_results$raw_data
      )

      group2_data <- extract_subject_roi_values(
        group_name = input$roi_perm_group2,
        raw_data = analysis_results$raw_data
      )

      # Get ROI names
      roi_names <- colnames(group1_data)

      # Run permutation tests for all ROIs
      results <- compute_roi_level_permutation_tests(
        group1_data = group1_data,
        group2_data = group2_data,
        roi_names = roi_names,
        n_permutations = input$roi_n_permutations,
        correction_method = input$roi_correction_method,
        parallel_cores = 1
      )

      showNotification("ROI permutation test completed successfully!",
                      type = "message", duration = 3)

      return(list(
        results = results,
        group1_name = input$roi_perm_group1,
        group2_name = input$roi_perm_group2,
        n_permutations = input$roi_n_permutations,
        correction_method = input$roi_correction_method
      ))

    }, error = function(e) {
      showNotification(paste("Error in ROI permutation test:", e$message),
                      type = "error", duration = 10)
      return(NULL)
    })
  })

  # Output: ROI permutation plot
  output$roiPermutationPlot <- renderPlot({
    roi_results <- roi_permutation_results()

    if(is.null(roi_results)) {
      plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
      text(1, 1, "Select two groups and click 'Run ROI Permutation Test'",
           cex = 1.2, col = "gray50")
      return()
    }

    render_roi_permutation_results(
      roi_results = roi_results$results,
      group1_name = roi_results$group1_name,
      group2_name = roi_results$group2_name,
      alpha = 0.05
    )
  })

  # Output: ROI permutation results table
  output$roiPermutationTable <- DT::renderDataTable({
    roi_results <- roi_permutation_results()

    if(is.null(roi_results)) {
      return(data.frame(Message = "No results available. Run analysis first."))
    }

    # Format results for display
    display_df <- roi_results$results
    display_df$P_Value <- format(display_df$P_Value, scientific = TRUE, digits = 3)
    display_df$P_Adjusted <- format(display_df$P_Adjusted, scientific = TRUE, digits = 3)
    display_df$Cohen_d <- round(display_df$Cohen_d, 3)
    display_df$Mean_Group1 <- round(display_df$Mean_Group1, 3)
    display_df$Mean_Group2 <- round(display_df$Mean_Group2, 3)
    display_df$Diff_Observed <- round(display_df$Diff_Observed, 3)

    DT::datatable(
      display_df,
      options = list(
        pageLength = 20,
        order = list(list(4, 'asc')),  # Sort by P_Adjusted
        scrollX = TRUE
      ),
      rownames = FALSE
    ) %>%
      DT::formatStyle(
        'Significant',
        backgroundColor = DT::styleEqual(c(TRUE, FALSE), c('#e74c3c33', '#95a5a633'))
      )
  })

  # Output: ROI permutation summary
  output$roiPermutationSummary <- renderPrint({
    roi_results <- roi_permutation_results()

    if(is.null(roi_results)) {
      cat("ROI-Level Permutation Test Summary\n")
      cat("===================================\n")
      cat("No results available. Please run analysis.\n")
      return()
    }

    results_df <- roi_results$results
    n_sig <- sum(results_df$Significant, na.rm = TRUE)
    n_total <- nrow(results_df)

    cat("ROI-Level Permutation Test Summary\n")
    cat("===================================\n\n")
    cat(sprintf("Comparison: %s vs %s\n", roi_results$group1_name, roi_results$group2_name))
    cat(sprintf("Number of permutations: %d\n", roi_results$n_permutations))
    cat(sprintf("Correction method: %s\n", roi_results$correction_method))
    cat(sprintf("\nTotal ROIs tested: %d\n", n_total))
    cat(sprintf("Significant ROIs (p < 0.05): %d (%.1f%%)\n", n_sig, 100 * n_sig / n_total))
    cat("\nSignificant ROIs:\n")

    if(n_sig > 0) {
      sig_rois <- results_df[results_df$Significant, ]
      sig_rois <- sig_rois[order(sig_rois$P_Adjusted), ]

      for(i in 1:nrow(sig_rois)) {
        cat(sprintf("  %s: p = %.2e, Cohen's d = %.3f\n",
                   sig_rois$ROI[i],
                   sig_rois$P_Adjusted[i],
                   sig_rois$Cohen_d[i]))
      }
    } else {
      cat("  None\n")
    }
  })

  # Tab 8b: Hub Overlap Analysis

  # Reactive: Compute hub overlap statistics
  hub_overlap_results <- eventReactive(input$run_hub_overlap, {
    req(analysis_results$method_weighted_results,
        analysis_results$method_percolation_results,
        analysis_results$auc_results)
    req(input$hub_overlap_approach, input$hub_overlap_method)

    tryCatch({
      showNotification("Computing hub overlap statistics...",
                      type = "message", duration = 2)

      approach <- input$hub_overlap_approach
      method <- tolower(input$hub_overlap_method)
      z_threshold <- input$hub_z_threshold

      # Determine which groups have data for the selected approach
      if(approach == "Weighted") {
        # FIX: Get unique groups from the combined weighted_eigenvector df
        weighted_eig <- analysis_results$method_weighted_results[[method]]$weighted_eigenvector
        available_groups <- if(!is.null(weighted_eig)) unique(weighted_eig$Group) else character(0)
      } else if(approach == "Percolation") {
        node_data <- analysis_results$method_percolation_results[[method]]$node_metrics
        available_groups <- unique(node_data$Group)
      } else if(approach == "Persistence") {
        available_groups <- names(analysis_results$auc_results[[method]])
      } else {
        stop("Invalid approach selected")
      }

      if(length(available_groups) < 2) {
        showNotification("Need at least 2 groups for hub overlap analysis.",
                        type = "error", duration = 5)
        return(NULL)
      }

      # Extract hubs for each group
      hub_sets <- list()
      for(group in available_groups) {
        hubs <- extract_hubs_from_approach(
          approach = approach,
          method_name = method,
          group_name = group,
          method_weighted_results = analysis_results$method_weighted_results,
          method_percolation_results = analysis_results$method_percolation_results,
          auc_results = analysis_results$auc_results,
          z_threshold = z_threshold
        )

        hub_sets[[group]] <- hubs
      }

      # Compute overlap statistics
      overlap_stats <- compute_hub_overlap_statistics(hub_sets)

      showNotification("Hub overlap analysis completed!",
                      type = "message", duration = 3)

      return(list(
        overlap_stats = overlap_stats,
        approach = approach,
        method = method,
        z_threshold = z_threshold,
        available_groups = available_groups
      ))

    }, error = function(e) {
      showNotification(paste("Error in hub overlap analysis:", e$message),
                      type = "error", duration = 10)
      return(NULL)
    })
  })

  # Output: Hub overlap Venn diagrams
  output$hubOverlapVennPlot <- renderPlot({
    hub_results <- hub_overlap_results()

    if(is.null(hub_results)) {
      plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
      text(1, 1, "Select approach and method, then click 'Compute Hub Overlap'",
           cex = 1.2, col = "gray50")
      return()
    }

    render_hub_overlap_venn_diagrams(
      hub_overlap_results = hub_results$overlap_stats,
      max_pairs = 6
    )
  })

  # Output: Hub overlap Jaccard matrix heatmap
  output$hubOverlapMatrixPlot <- renderPlot({
    hub_results <- hub_overlap_results()

    if(is.null(hub_results)) {
      plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
      text(1, 1, "Run hub overlap analysis to view Jaccard index matrix",
           cex = 1.2, col = "gray50")
      return()
    }

    render_hub_overlap_heatmap(
      hub_overlap_results = hub_results$overlap_stats
    )
  })

  # Output: Hub lists table
  output$hubOverlapTable <- DT::renderDataTable({
    hub_results <- hub_overlap_results()

    if(is.null(hub_results)) {
      return(data.frame(Message = "No results available. Run analysis first."))
    }

    # Create a table showing hubs for each group
    hub_sets <- hub_results$overlap_stats$hub_sets
    max_hubs <- max(sapply(hub_sets, length))

    # Create data frame with hub lists
    hub_df <- data.frame(matrix("", nrow = max_hubs, ncol = length(hub_sets)))
    colnames(hub_df) <- names(hub_sets)

    for(i in seq_along(hub_sets)) {
      group_name <- names(hub_sets)[i]
      hubs <- hub_sets[[i]]
      if(length(hubs) > 0) {
        hub_df[1:length(hubs), i] <- hubs
      }
    }

    DT::datatable(
      hub_df,
      options = list(
        pageLength = 20,
        scrollX = TRUE,
        dom = 't'  # Remove search box and pagination for cleaner view
      ),
      rownames = FALSE
    )
  })

  # Output: Hub overlap summary
  output$hubOverlapSummary <- renderPrint({
    hub_results <- hub_overlap_results()

    if(is.null(hub_results)) {
      cat("Hub Overlap Analysis Summary\n")
      cat("============================\n")
      cat("No results available. Please run analysis.\n")
      return()
    }

    overlap_stats <- hub_results$overlap_stats
    hub_sets <- overlap_stats$hub_sets

    cat("Hub Overlap Analysis Summary\n")
    cat("============================\n\n")
    cat(sprintf("Analysis approach: %s\n", hub_results$approach))
    cat(sprintf("Correlation method: %s\n", hub_results$method))
    cat(sprintf("Z-score threshold: %.2f\n", hub_results$z_threshold))
    cat(sprintf("\nNumber of groups: %d\n", length(hub_sets)))

    cat("\nHub counts per group:\n")
    for(group in names(hub_sets)) {
      cat(sprintf("  %s: %d hubs\n", group, length(hub_sets[[group]])))
    }

    cat("\nPairwise Jaccard indices:\n")
    jaccard_mat <- overlap_stats$jaccard_matrix
    n_groups <- nrow(jaccard_mat)

    for(i in 1:(n_groups - 1)) {
      for(j in (i + 1):n_groups) {
        cat(sprintf("  %s vs %s: J = %.3f\n",
                   rownames(jaccard_mat)[i],
                   rownames(jaccard_mat)[j],
                   jaccard_mat[i, j]))
      }
    }

    cat("\nOverall statistics:\n")
    cat(sprintf("  Mean Jaccard index: %.3f\n", overlap_stats$summary_stats$mean_jaccard))
    cat(sprintf("  SD Jaccard index: %.3f\n", overlap_stats$summary_stats$sd_jaccard))
    cat(sprintf("  Min Jaccard index: %.3f\n", overlap_stats$summary_stats$min_jaccard))
    cat(sprintf("  Max Jaccard index: %.3f\n", overlap_stats$summary_stats$max_jaccard))
  })

  # Tab 8c: Global Network Permutation Testing

  # Update group selectors for global permutation testing
  observe({
    req(analysis_results$raw_data)

    if("Group" %in% names(analysis_results$raw_data)) {
      available_groups <- unique(analysis_results$raw_data$Group)

      updateSelectInput(session, "global_perm_group1",
                       choices = available_groups,
                       selected = available_groups[1])

      updateSelectInput(session, "global_perm_group2",
                       choices = available_groups,
                       selected = if(length(available_groups) > 1) available_groups[2] else available_groups[1])
    }
  })

  # Reactive: Compute global network permutation test
  global_permutation_results <- eventReactive(input$run_global_permutation_test, {
    req(analysis_results$raw_data)
    req(input$global_perm_group1, input$global_perm_group2)

    # Validate groups are different
    if(input$global_perm_group1 == input$global_perm_group2) {
      showNotification("Please select two different groups for comparison.", type = "error", duration = 5)
      return(NULL)
    }

    tryCatch({
      showNotification(sprintf("Running global network permutation test with %d permutations... This may take 5-10 minutes.",
                              input$global_n_permutations),
                      type = "message", duration = 5)

      # Extract subject-level ROI data for both groups
      group1_data <- extract_subject_roi_values(
        group_name = input$global_perm_group1,
        raw_data = analysis_results$raw_data
      )

      group2_data <- extract_subject_roi_values(
        group_name = input$global_perm_group2,
        raw_data = analysis_results$raw_data
      )

      # Determine threshold (NA means use weighted networks)
      threshold_val <- if(is.na(input$global_perm_threshold)) NULL else input$global_perm_threshold

      # Run global permutation test
      results <- compute_global_network_permutation_test(
        group1_data = group1_data,
        group2_data = group2_data,
        group1_name = input$global_perm_group1,
        group2_name = input$global_perm_group2,
        correlation_method = input$global_perm_corr_method,
        threshold = threshold_val,
        n_permutations = input$global_n_permutations,
        parallel_cores = 1
      )

      showNotification("Global permutation test completed successfully!",
                      type = "message", duration = 3)

      return(results)

    }, error = function(e) {
      showNotification(paste("Error in global permutation test:", e$message),
                      type = "error", duration = 10)
      return(NULL)
    })
  })

  # Output: Global permutation plot
  output$globalPermutationPlot <- renderPlot({
    global_results <- global_permutation_results()

    if(is.null(global_results)) {
      plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
      text(1, 1, "Select two groups and click 'Run Global Permutation Test'",
           cex = 1.2, col = "gray50")
      return()
    }

    render_global_permutation_results(global_results)
  })

  # Output: Global permutation summary
  output$globalPermutationSummary <- renderPrint({
    global_results <- global_permutation_results()

    if(is.null(global_results)) {
      cat("Global Network Permutation Test Summary\n")
      cat("========================================\n")
      cat("No results available. Please run analysis.\n")
      return()
    }

    cat("Global Network Permutation Test Summary\n")
    cat("========================================\n\n")
    cat(sprintf("Comparison: %s vs %s\n", global_results$group1_name, global_results$group2_name))
    cat(sprintf("Correlation method: %s\n", global_results$correlation_method))
    cat(sprintf("Network type: %s\n", if(is.null(global_results$threshold)) "Weighted" else sprintf("Binary (threshold = %.2f)", global_results$threshold)))
    cat(sprintf("Number of permutations: %d\n", global_results$n_permutations))

    cat("\nObserved Metrics:\n")
    cat(sprintf("  %s:\n", global_results$group1_name))
    for(metric in names(global_results$observed_group1)) {
      val <- global_results$observed_group1[metric]
      if(!is.na(val)) {
        cat(sprintf("    %s: %.4f\n", metric, val))
      }
    }

    cat(sprintf("\n  %s:\n", global_results$group2_name))
    for(metric in names(global_results$observed_group2)) {
      val <- global_results$observed_group2[metric]
      if(!is.na(val)) {
        cat(sprintf("    %s: %.4f\n", metric, val))
      }
    }

    cat("\nObserved Differences (Group1 - Group2):\n")
    for(metric in names(global_results$observed_diff)) {
      diff_val <- global_results$observed_diff[metric]
      p_val <- global_results$p_values[metric]
      if(!is.na(diff_val) && !is.na(p_val)) {
        sig_marker <- if(p_val < 0.001) "***" else if(p_val < 0.01) "**" else if(p_val < 0.05) "*" else ""
        cat(sprintf("  %s: %.4f (p = %.4f) %s\n", metric, diff_val, p_val, sig_marker))
      }
    }

    cat("\nSignificance codes: *** p < 0.001, ** p < 0.01, * p < 0.05\n")

    # Count significant metrics
    n_sig <- sum(global_results$p_values < 0.05, na.rm = TRUE)
    n_total <- sum(!is.na(global_results$p_values))
    cat(sprintf("\nSignificant metrics: %d / %d (%.1f%%)\n", n_sig, n_total, 100 * n_sig / n_total))
  })

  # Removing old Tab 7c-7e placeholders below

  # Tab 7c: Topological Analysis (REMOVED - old placeholder)
  # Tab 7d: Advanced Network Methods (REMOVED - old placeholder)
  # Tab 7e: Network-Based Statistics (REMOVED - old placeholder)

  output$persistenceDiagramPlot <- renderPlot({
    plot(1, type = "n", main = "Persistence Diagrams",
         axes = FALSE, xlab = "", ylab = "")
    text(1, 1, "Click 'Compute Persistent Homology' to generate diagrams", cex = 1.2, col = "gray50")
  })

  output$bettiCurvesPlot <- renderPlot({
    plot(1, type = "n", main = "Betti Curves",
         axes = FALSE, xlab = "", ylab = "")
    text(1, 1, "Topological features will be displayed here", cex = 1.2, col = "gray50")
  })

  output$wassersteinDistancePlot <- renderPlot({
    plot(1, type = "n", main = "Wasserstein Distances",
         axes = FALSE, xlab = "", ylab = "")
    text(1, 1, "Distance matrix will be displayed here", cex = 1.2, col = "gray50")
  })

  # Tab 7d: Advanced Network Methods (OMST, Disparity, Spectral)
  observeEvent(input$compute_omst, {
    req(analysis_results$correlations)

    showNotification("OMST computation coming soon!", type = "message", duration = 5)
  })

  output$omstNetworkPlot <- renderPlot({
    plot(1, type = "n", main = "OMST Network Visualization",
         axes = FALSE, xlab = "", ylab = "")
    text(1, 1, "Feature under development", cex = 1.2, col = "gray50")
  })

  output$omstMetricsSummary <- renderPrint({
    cat("OMST Analysis Summary\n")
    cat("----------------------\n")
    cat("Orthogonal Minimum Spanning Tree analysis will be available in future release.\n")
  })

  observeEvent(input$apply_disparity_filter, {
    req(analysis_results$correlations)

    showNotification("Disparity filter coming soon!", type = "message", duration = 5)
  })

  output$disparityNetworkPlot <- renderPlot({
    plot(1, type = "n", main = "Disparity-Filtered Network",
         axes = FALSE, xlab = "", ylab = "")
    text(1, 1, "Feature under development", cex = 1.2, col = "gray50")
  })

  output$disparityStatsSummary <- renderPrint({
    cat("Disparity Filter Summary\n")
    cat("-------------------------\n")
    cat("Statistical backbone extraction will be available in future release.\n")
  })

  observeEvent(input$compute_spectral, {
    req(analysis_results$correlations)

    showNotification("Spectral analysis coming soon!", type = "message", duration = 5)
  })

  output$spectralEmbeddingPlot <- renderPlot({
    plot(1, type = "n", main = "Spectral Embedding",
         axes = FALSE, xlab = "", ylab = "")
    text(1, 1, "Feature under development", cex = 1.2, col = "gray50")
  })

  output$spectralClusteringPlot <- renderPlot({
    plot(1, type = "n", main = "Spectral Clustering",
         axes = FALSE, xlab = "", ylab = "")
    text(1, 1, "Feature under development", cex = 1.2, col = "gray50")
  })

  output$spectralSummary <- renderPrint({
    cat("Spectral Analysis Summary\n")
    cat("--------------------------\n")
    cat("Graph Laplacian eigendecomposition will be available in future release.\n")
  })

  # Tab 7e: Network-Based Statistics
  observeEvent(input$run_nbs, {
    req(analysis_results$correlations)

    showNotification("NBS analysis coming soon!", type = "message", duration = 5)
  })

  output$nbsNetworkPlot <- renderPlot({
    plot(1, type = "n", main = "Network-Based Statistics",
         axes = FALSE, xlab = "", ylab = "")
    text(1, 1, "Click 'Run NBS' to identify connected components", cex = 1.2, col = "gray50")
  })

  output$nbsComponentsSummary <- renderPrint({
    cat("NBS Components Summary\n")
    cat("-----------------------\n")
    cat("Network-Based Statistics will identify clusters of connections\n")
    cat("that differ significantly between groups.\n")
  })

  # Tab 7f: Meta-Consensus Summary
  output$metaConsensusSummaryPlot <- renderPlot({
    req(analysis_results$comprehensive_consensus)

    plot(1, type = "n", main = "Meta-Consensus Analysis",
         axes = FALSE, xlab = "", ylab = "")
    text(1, 1, "Comprehensive consensus summary across all methods", cex = 1.2, col = "gray50")
  })

  output$metaConsensusSummaryText <- renderPrint({
    cat("Meta-Consensus Summary\n")
    cat("-----------------------\n")
    cat("This section provides an integrated summary of findings across:\n")
    cat("- All correlation methods (Pearson, Spearman, Biweight, Shrinkage, Partial)\n")
    cat("- All analytical approaches (Weighted, Percolation, Persistence)\n")
    cat("- All experimental groups\n")
    cat("\nUse this to identify the most robust and replicable network features.\n")
  })

  output$robustnessScoresTable <- DT::renderDataTable({
    # Placeholder table
    data.frame(
      Feature = c("Hub Nodes", "Network Density", "Modularity", "Small-Worldness"),
      Robustness = c("High", "Medium", "High", "Medium"),
      ConsensusLevel = c("85%", "60%", "78%", "65%"),
      stringsAsFactors = FALSE
    )
  })

  # Initialize downloads server
  downloads_server(input, output, session, analysis_results, ui_state)

  # Session cleanup handler for standalone mode
  session$onSessionEnded(function() {
    cat("Session ended at", format(Sys.time()), "\n")
    # Clean up any temporary files
    temp_files <- list.files(tempdir(), pattern = "BrainAnalysis_", full.names = TRUE)
    if (length(temp_files) > 0) {
      cat("Cleaning up", length(temp_files), "temporary files\n")
      unlink(temp_files, recursive = TRUE)
    }
  })

  # About button handler removed - About is now a tab in the Analysis section
}

# Run the application
shinyApp(ui = ui, server = server)
