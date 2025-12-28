# Analysis Functions Module
# Contains all core analysis functions with fixes

# Load required packages
has_mice <- requireNamespace("mice", quietly = TRUE)
has_igraph <- requireNamespace("igraph", quietly = TRUE)
has_dplyr <- requireNamespace("dplyr", quietly = TRUE)
has_pracma <- requireNamespace("pracma", quietly = TRUE)

# Track if warnings have been shown (to avoid repetitive messages)
.warning_shown <- new.env(parent = emptyenv())
.warning_shown$pracma <- FALSE

if(has_mice) library(mice)
if(has_igraph) library(igraph)
if(has_dplyr) library(dplyr)

# Data imputation function
perform_mice_imputation <- function(data, m = 5, maxit = 10) {
  if(!has_mice) {
    return(list(imputed_data = data, imputation_needed = FALSE, method = "No MICE available"))
  }
  
  missing_count <- sum(is.na(data))
  if(missing_count == 0) {
    return(list(imputed_data = data, imputation_needed = FALSE, missing_count = 0))
  }
  
  tryCatch({
    mice_result <- mice(data, m = m, maxit = maxit, printFlag = FALSE, seed = 123)
    imputed_data <- complete(mice_result, 1)
    
    return(list(
      imputed_data = imputed_data,
      imputation_needed = TRUE,
      missing_count = missing_count,
      method = "MICE with PMM"
    ))
  }, error = function(e) {
    return(list(imputed_data = data, imputation_needed = FALSE, error = e$message))
  })
}

# FIX 3: Percolation threshold calculation - matching original pipeline methodology
calculate_percolation_threshold <- function(correlation_matrix) {
  if(!has_igraph) return(0.4) # Use original fallback
  
  # Use exact methodology from percolation_network_analysis.R
  n_nodes <- nrow(correlation_matrix)
  threshold_range <- seq(0.01, 0.95, by = 0.01)
  
  percolation_data <- data.frame(
    threshold = numeric(),
    giant_component_size = numeric(),
    n_components = numeric(),
    edge_density = numeric()
  )
  
  for(thresh in threshold_range) {
    # Create network at this threshold
    adj_matrix <- (abs(correlation_matrix) >= thresh) * 1
    diag(adj_matrix) <- 0
    
    if(sum(adj_matrix) == 0) {
      # Empty network
      percolation_data <- rbind(percolation_data, data.frame(
        threshold = thresh,
        giant_component_size = 0,
        n_components = n_nodes,
        edge_density = 0
      ))
    } else {
      # Create network
      g <- graph_from_adjacency_matrix(adj_matrix, mode = "undirected", diag = FALSE)
      
      # Get component information
      comp <- components(g)
      giant_size <- max(comp$csize)
      n_components <- comp$no
      edge_density <- edge_density(g)
      
      percolation_data <- rbind(percolation_data, data.frame(
        threshold = thresh,
        giant_component_size = giant_size / n_nodes,  # Normalize by total nodes
        n_components = n_components,
        edge_density = edge_density
      ))
    }
  }
  
  # Find last stable threshold before giant component fragmentation
  if(nrow(percolation_data) > 10) {
    # Find the peak of the giant component
    max_giant <- max(percolation_data$giant_component_size)
    peak_idx <- which.max(percolation_data$giant_component_size)
    
    # Find the last threshold that maintains peak connectivity
    stable_threshold <- NULL
    
    if(max_giant == 1.0) {
      # When peak is full connectivity (1.0), find highest threshold with exactly 1.0
      exact_peak_indices <- which(percolation_data$giant_component_size == 1.0)
      if(length(exact_peak_indices) > 0) {
        last_stable_idx <- max(exact_peak_indices)
        stable_threshold <- percolation_data$threshold[last_stable_idx]
        return(stable_threshold)
      }
    } else {
      # When peak is partial connectivity, use tolerance approach
      tolerance <- 0.02
      stable_threshold_value <- max_giant * (1 - tolerance)
      
      # Find all points that are "stable" (within tolerance of peak)
      stable_indices <- which(percolation_data$giant_component_size >= stable_threshold_value)
      
      if(length(stable_indices) > 0) {
        # Find the highest threshold among stable points
        last_stable_idx <- max(stable_indices)
        stable_threshold <- percolation_data$threshold[last_stable_idx]
        return(stable_threshold)
      }
    }
    
    # If no stable threshold found, fall back to previous methods
    if(is.null(stable_threshold)) {
      # Fallback: use second derivative method if no clear fragmentation point
      tryCatch({
        giant_smooth <- smooth.spline(percolation_data$threshold, 
                                      percolation_data$giant_component_size, 
                                      spar = 0.5)
        second_deriv <- predict(giant_smooth, percolation_data$threshold, deriv = 2)
        critical_idx <- which.min(second_deriv$y)
        critical_threshold <- percolation_data$threshold[critical_idx]
        return(critical_threshold)

      }, error = function(e) {
        return(0.4)
      })
    }
  }
  
  # Final fallback
  return(0.4)
}

# Enhanced 5-method correlation consensus computation by groups
compute_correlations <- function(data, groups) {
  results <- list()
  consensus_metadata <- list()
  
  for(group in unique(groups)) {
    group_data <- data[groups == group, ]
    if(nrow(group_data) > 1 && ncol(group_data) > 1) {
      
      # Compute multiple correlation methods
      correlation_methods <- compute_correlation_methods(as.matrix(group_data))
      
      # Create consensus matrix using median
      consensus_matrix <- compute_consensus_correlation(correlation_methods, method = "median")
      
      # Quality metrics
      n_total_pairs <- sum(upper.tri(consensus_matrix))
      mean_abs_corr <- mean(abs(consensus_matrix[upper.tri(consensus_matrix)]))
      strong_corrs <- sum(abs(consensus_matrix[upper.tri(consensus_matrix)]) >= 0.4)
      
      # Store consensus metadata
      consensus_metadata[[group]] <- list(
        methods_used = names(correlation_methods),
        n_methods = length(correlation_methods),
        sample_size = nrow(group_data),
        n_variables = ncol(group_data),
        mean_abs_correlation = mean_abs_corr,
        strong_correlations = strong_corrs,
        total_pairs = n_total_pairs,
        network_density_at_04 = strong_corrs / n_total_pairs
      )
      
      results[[group]] <- consensus_matrix
    }
  }
  
  # Store metadata as attribute for access in UI
  attr(results, "consensus_metadata") <- consensus_metadata
  return(results)
}

# Compute multiple correlation methods for robust consensus
compute_correlation_methods <- function(data_matrix, selected_methods = c("pearson", "spearman", "kendall", "biweight", "shrinkage", "partial")) {
  n_vars <- ncol(data_matrix)
  n_obs <- nrow(data_matrix)
  methods <- list()

  # 1. Pearson correlation (parametric)
  if("pearson" %in% selected_methods) {
    methods$pearson <- cor(data_matrix, use = "complete.obs", method = "pearson")
  }

  # 2. Spearman correlation (non-parametric, rank-based)
  if("spearman" %in% selected_methods) {
    methods$spearman <- cor(data_matrix, use = "complete.obs", method = "spearman")
  }

  # 3. Kendall correlation (non-parametric, rank-based, better for small n and ties)
  if("kendall" %in% selected_methods) {
    methods$kendall <- cor(data_matrix, use = "complete.obs", method = "kendall")
  }

  # 4. Biweight midcorrelation (robust to outliers)
  # Use psych package if available, otherwise fallback to Pearson
  if("biweight" %in% selected_methods) {
    tryCatch({
      if(requireNamespace("psych", quietly = TRUE)) {
        library(psych)
        methods$biweight <- corr.test(data_matrix, method = "pearson", adjust = "none")$r
      } else {
        warning("psych package not available, using Pearson as biweight substitute")
        # Need pearson computed for fallback
        if(is.null(methods$pearson)) {
          methods$biweight <- cor(data_matrix, use = "complete.obs", method = "pearson")
        } else {
          methods$biweight <- methods$pearson
        }
      }
    }, error = function(e) {
      warning("Biweight correlation failed, using Pearson substitute")
      if(is.null(methods$pearson)) {
        methods$biweight <- cor(data_matrix, use = "complete.obs", method = "pearson")
      } else {
        methods$biweight <- methods$pearson
      }
    })
  }

  # 4. Shrinkage correlation (optimal for small samples)
  if("shrinkage" %in% selected_methods) {
    tryCatch({
      if(requireNamespace("corpcor", quietly = TRUE)) {
        library(corpcor)
        shrink_result <- cor.shrink(data_matrix, verbose = FALSE)
        methods$shrinkage <- shrink_result
      } else {
        warning("corpcor package not available, using Pearson as shrinkage substitute")
        if(is.null(methods$pearson)) {
          methods$shrinkage <- cor(data_matrix, use = "complete.obs", method = "pearson")
        } else {
          methods$shrinkage <- methods$pearson
        }
      }
    }, error = function(e) {
      warning("Shrinkage correlation failed, using Pearson substitute")
      if(is.null(methods$pearson)) {
        methods$shrinkage <- cor(data_matrix, use = "complete.obs", method = "pearson")
      } else {
        methods$shrinkage <- methods$pearson
      }
    })
  }

  # 5. Partial correlation (removes indirect effects)
  # Only compute if sample size is adequate (n > p + 5)
  if("partial" %in% selected_methods) {
    if(n_obs > n_vars + 5) {
      tryCatch({
        if(requireNamespace("psych", quietly = TRUE)) {
          library(psych)
          # Use partial correlation via regression residuals
          partial_result <- partial.r(data_matrix, use = "complete.obs")
          methods$partial <- partial_result
        } else {
          warning("psych package not available, skipping partial correlation")
        }
      }, error = function(e) {
        warning("Partial correlation failed (likely sample size), skipping")
      })
    } else {
      warning(sprintf("Sample size (%d) too small for partial correlation (need > %d), skipping",
                  n_obs, n_vars + 5))
    }
  }

  return(methods)
}

# Compute consensus correlation matrix from multiple methods
compute_consensus_correlation <- function(methods_list, method = "median") {
  if(length(methods_list) == 0) {
    stop("No correlation methods provided")
  }
  
  # Get dimensions from first method
  first_method <- methods_list[[1]]
  n_vars <- nrow(first_method)
  var_names <- rownames(first_method)
  
  # Create 3D array: vars x vars x methods
  methods_array <- array(NA, dim = c(n_vars, n_vars, length(methods_list)))
  dimnames(methods_array) <- list(var_names, var_names, names(methods_list))
  
  # Fill array with correlation matrices
  for(i in seq_along(methods_list)) {
    methods_array[,,i] <- methods_list[[i]]
  }
  
  # Compute consensus
  if(method == "median") {
    consensus_matrix <- apply(methods_array, c(1,2), median, na.rm = TRUE)
  } else if(method == "mean") {
    consensus_matrix <- apply(methods_array, c(1,2), mean, na.rm = TRUE)
  } else {
    stop("Unknown consensus method: ", method)
  }
  
  # Ensure symmetric matrix with diagonal = 1
  consensus_matrix <- (consensus_matrix + t(consensus_matrix)) / 2
  diag(consensus_matrix) <- 1
  
  # Preserve variable names
  rownames(consensus_matrix) <- colnames(consensus_matrix) <- var_names
  
  return(consensus_matrix)
}

# NEW: Compute correlations by method (stores each method separately)
compute_correlations_by_method <- function(data, groups, selected_methods = c("pearson", "spearman", "kendall", "biweight", "shrinkage", "partial")) {
  # Initialize results only for selected methods
  results <- list()
  for(method_name in selected_methods) {
    results[[method_name]] <- list()
  }

  for(group in unique(groups)) {
    group_data <- data[groups == group, ]
    if(nrow(group_data) > 1 && ncol(group_data) > 1) {

      # Compute only selected correlation methods
      methods <- compute_correlation_methods(as.matrix(group_data), selected_methods)

      # Store each method separately (only for selected methods)
      for(method_name in selected_methods) {
        if(!is.null(methods[[method_name]])) {
          results[[method_name]][[group]] <- methods[[method_name]]
        }
      }
    }
  }

  return(results)
}

# NEW: Create median consensus from per-method results (backward compatibility)
create_median_consensus_from_methods <- function(correlation_methods) {
  results <- list()
  consensus_metadata <- list()

  # Get all groups
  groups <- unique(unlist(lapply(correlation_methods, names)))

  for(group in groups) {
    # Collect available methods for this group
    methods_list <- list()

    for(method_name in names(correlation_methods)) {
      method_matrix <- correlation_methods[[method_name]][[group]]
      if(!is.null(method_matrix)) {
        methods_list[[method_name]] <- method_matrix
      }
    }

    if(length(methods_list) > 0) {
      # Create consensus matrix using median
      consensus_matrix <- compute_consensus_correlation(methods_list, method = "median")

      # Quality metrics
      n_total_pairs <- sum(upper.tri(consensus_matrix))
      mean_abs_corr <- mean(abs(consensus_matrix[upper.tri(consensus_matrix)]))
      strong_corrs <- sum(abs(consensus_matrix[upper.tri(consensus_matrix)]) >= 0.4)

      # Store consensus metadata
      consensus_metadata[[group]] <- list(
        methods_used = names(methods_list),
        n_methods = length(methods_list),
        sample_size = nrow(correlation_methods$pearson[[group]]),
        n_variables = ncol(correlation_methods$pearson[[group]]),
        mean_abs_correlation = mean_abs_corr,
        strong_correlations = strong_corrs,
        total_pairs = n_total_pairs,
        network_density_at_04 = strong_corrs / n_total_pairs
      )

      results[[group]] <- consensus_matrix
    }
  }

  # Store metadata as attribute for access in UI
  attr(results, "consensus_metadata") <- consensus_metadata
  return(results)
}

# NEW: Perform persistence analysis across multiple thresholds
perform_persistence_analysis <- function(correlation_matrix,
                                        threshold_seq,
                                        brain_areas,
                                        group_name) {
  results <- list()

  for(thresh in threshold_seq) {
    # Create binary thresholded network
    g <- create_network(correlation_matrix, threshold = thresh)

    # Only compute metrics if network has edges
    if(!is.null(g) && vcount(g) > 0 && ecount(g) > 0) {
      metrics <- compute_network_metrics(g, group_name, brain_areas)

      results[[as.character(thresh)]] <- list(
        threshold = thresh,
        global = metrics$global,
        nodes = metrics$nodes,
        edges = metrics$edges
        # Don't store graph object to save memory
      )
    } else {
      # Store placeholder for empty networks at high thresholds
      results[[as.character(thresh)]] <- list(
        threshold = thresh,
        global = NULL,
        nodes = NULL,
        edges = NULL
      )
    }
  }

  return(results)
}

# NEW: Compute hub persistence scores
compute_hub_persistence <- function(persistence_results,
                                   centrality_measure = "Eigenvector",
                                   top_n = 10) {
  # Track which nodes remain hubs across thresholds
  hub_tracking <- list()

  for(thresh_name in names(persistence_results)) {
    nodes_df <- persistence_results[[thresh_name]]$nodes

    # Skip empty networks
    if(is.null(nodes_df)) next

    # Get top N nodes by specified centrality - use base R if dplyr not available
    if(has_dplyr) {
      top_nodes <- nodes_df %>%
        arrange(desc(.data[[centrality_measure]])) %>%
        head(top_n) %>%
        pull(Node)
    } else {
      # Base R fallback
      nodes_df <- nodes_df[order(-nodes_df[[centrality_measure]]), ]
      top_nodes <- head(nodes_df$Node, top_n)
    }

    # Track threshold appearances for each node
    for(node in top_nodes) {
      if(is.null(hub_tracking[[node]])) hub_tracking[[node]] <- c()
      hub_tracking[[node]] <- c(hub_tracking[[node]], as.numeric(thresh_name))
    }
  }

  # Calculate persistence metrics
  total_thresholds <- sum(!sapply(persistence_results, function(x) is.null(x$nodes)))

  if(length(hub_tracking) == 0 || total_thresholds == 0) {
    return(data.frame(
      Node = character(0),
      PersistenceScore = numeric(0),
      MinThreshold = numeric(0),
      MaxThreshold = numeric(0),
      ThresholdRange = numeric(0)
    ))
  }

  persistence_df <- data.frame(
    Node = names(hub_tracking),
    PersistenceScore = sapply(hub_tracking, length) / total_thresholds,
    MinThreshold = sapply(hub_tracking, min),
    MaxThreshold = sapply(hub_tracking, max),
    ThresholdRange = sapply(hub_tracking, max) - sapply(hub_tracking, min),
    stringsAsFactors = FALSE
  )

  # Sort by persistence score - use base R if dplyr not available
  if(has_dplyr) {
    persistence_df <- persistence_df %>% arrange(desc(PersistenceScore))
  } else {
    persistence_df <- persistence_df[order(-persistence_df$PersistenceScore), ]
  }

  return(persistence_df)
}

# NEW: Compute node importance evolution across thresholds with standardized ranks
compute_node_importance_evolution <- function(persistence_results,
                                             metrics = c("Eigenvector", "Degree", "Betweenness", "PageRank"),
                                             group_name = "Group1") {
  # Extract all thresholds
  thresholds <- as.numeric(names(persistence_results))
  thresholds <- sort(thresholds)

  # Collect all unique nodes
  all_nodes <- unique(unlist(lapply(persistence_results, function(res) {
    if(!is.null(res$nodes)) res$nodes$Node else character(0)
  })))

  if(length(all_nodes) == 0 || length(thresholds) == 0) {
    return(NULL)
  }

  # Initialize results list
  results <- list()

  for(metric in metrics) {
    # Create matrix: nodes × thresholds (raw values)
    raw_matrix <- matrix(NA,
                        nrow = length(all_nodes),
                        ncol = length(thresholds),
                        dimnames = list(all_nodes, paste0("T", thresholds)))

    # Create matrix for ranks
    rank_matrix <- matrix(NA,
                         nrow = length(all_nodes),
                         ncol = length(thresholds),
                         dimnames = list(all_nodes, paste0("T", thresholds)))

    # Create matrix for standardized scores (0-1 scale)
    std_matrix <- matrix(NA,
                        nrow = length(all_nodes),
                        ncol = length(thresholds),
                        dimnames = list(all_nodes, paste0("T", thresholds)))

    # Fill matrices
    for(i in seq_along(thresholds)) {
      thresh <- as.character(thresholds[i])
      nodes_df <- persistence_results[[thresh]]$nodes

      if(!is.null(nodes_df) && metric %in% names(nodes_df)) {
        # Extract values for this threshold
        metric_values <- nodes_df[[metric]]
        names(metric_values) <- nodes_df$Node

        # Raw values
        matching_nodes <- intersect(all_nodes, nodes_df$Node)
        raw_matrix[matching_nodes, i] <- metric_values[matching_nodes]

        # Rank values (higher value = better rank = lower number)
        # Rank 1 = highest metric value
        ranks <- rank(-metric_values, ties.method = "average", na.last = "keep")
        names(ranks) <- nodes_df$Node
        rank_matrix[matching_nodes, i] <- ranks[matching_nodes]

        # Standardized scores (0-1, where 1 = best)
        # Rank 1 gets score 1.0, last rank gets score 0.0
        n_nodes <- length(metric_values)
        if(n_nodes > 1) {
          std_scores <- 1 - ((ranks - 1) / (n_nodes - 1))
          names(std_scores) <- nodes_df$Node
          std_matrix[matching_nodes, i] <- std_scores[matching_nodes]
        } else {
          # Single node gets score 1.0
          std_matrix[matching_nodes, i] <- 1.0
        }
      }
    }

    # Calculate summary statistics per node
    node_stats <- data.frame(
      Node = all_nodes,
      Group = group_name,
      Metric = metric,
      MeanRank = rowMeans(rank_matrix, na.rm = TRUE),
      MedianRank = apply(rank_matrix, 1, median, na.rm = TRUE),
      BestRank = apply(rank_matrix, 1, min, na.rm = TRUE),
      WorstRank = apply(rank_matrix, 1, max, na.rm = TRUE),
      MeanStdScore = rowMeans(std_matrix, na.rm = TRUE),
      MedianStdScore = apply(std_matrix, 1, median, na.rm = TRUE),
      StabilityScore = apply(std_matrix, 1, function(x) {
        # How stable is this node's importance? (1 = very stable, 0 = very unstable)
        valid_vals <- x[!is.na(x)]
        if(length(valid_vals) < 2) return(NA)
        1 - sd(valid_vals)  # Lower SD = more stable
      }),
      PersistenceRate = rowSums(!is.na(rank_matrix)) / ncol(rank_matrix),  # % of thresholds where node appears
      FirstAppearance = apply(rank_matrix, 1, function(x) {
        idx <- which(!is.na(x))[1]
        if(length(idx) > 0) thresholds[idx] else NA
      }),
      LastAppearance = apply(rank_matrix, 1, function(x) {
        idx <- max(which(!is.na(x)))
        if(length(idx) > 0 && idx > 0) thresholds[idx] else NA
      }),
      stringsAsFactors = FALSE
    )

    # Sort by mean standardized score (best nodes first)
    node_stats <- node_stats[order(-node_stats$MeanStdScore), ]

    results[[metric]] <- list(
      raw_values = raw_matrix,
      ranks = rank_matrix,
      standardized_scores = std_matrix,
      summary = node_stats,
      thresholds = thresholds
    )
  }

  return(results)
}

# NEW: Compute network metrics evolution across thresholds
compute_network_metrics_evolution <- function(persistence_results) {
  # Extract time series of global metrics across thresholds
  thresholds <- names(persistence_results)

  evolution <- data.frame(
    Threshold = numeric(),
    Density = numeric(),
    Clustering = numeric(),
    AvgPathLength = numeric(),
    Modularity = numeric(),
    Assortativity = numeric(),
    Components = numeric()
  )

  for(thresh in thresholds) {
    global <- persistence_results[[thresh]]$global

    if(!is.null(global)) {
      evolution <- rbind(evolution, data.frame(
        Threshold = as.numeric(thresh),
        Density = ifelse(!is.null(global$Density), global$Density, NA),
        Clustering = ifelse(!is.null(global$Clustering), global$Clustering, NA),
        AvgPathLength = ifelse(!is.null(global$Path_Length), global$Path_Length, NA),
        Modularity = ifelse(!is.null(global$Modularity), global$Modularity, NA),
        Assortativity = ifelse(!is.null(global$Assortativity), global$Assortativity, NA),
        Components = ifelse(!is.null(global$Components), global$Components, 1)
      ))
    }
  }

  return(evolution)
}

# Network creation with percolation threshold
create_network <- function(correlation_matrix, threshold) {
  if(!has_igraph) return(NULL)
  
  # Threshold is required and should be calculated once on full dataset
  if(is.null(threshold)) {
    stop("Threshold must be provided - should be calculated on full dataset")
  }
  
  adj_matrix <- (abs(correlation_matrix) >= threshold) * 1
  diag(adj_matrix) <- 0
  
  weighted_adj <- abs(correlation_matrix) * adj_matrix
  g <- graph_from_adjacency_matrix(weighted_adj, mode = "undirected", 
                                   weighted = TRUE, diag = FALSE)
  
  # Store threshold in graph
  g$threshold <- threshold
  
  return(g)
}



# Enhanced network metrics function with comprehensive node and edge metrics
compute_network_metrics <- function(g, group_name, brain_areas = NULL) {
  if(!has_igraph || is.null(g)) return(NULL)
  
  if(vcount(g) == 0 || ecount(g) == 0) {
    return(list(
      global = data.frame(
        Group = group_name,
        Nodes = vcount(g),
        Edges = 0,
        Density = 0,
        Connected = FALSE,
        Clustering = 0,
        Path_Length = Inf,
        Small_World_Sigma = 0,
        Modularity = 0,
        Assortativity = 0,
        Diameter = 0,
        Threshold = ifelse(is.null(g$threshold), 0.3, g$threshold)
      ),
      nodes = data.frame(
        Group = group_name,
        Node = character(0),
        Brain_Area = character(0),
        Degree = numeric(0),
        Betweenness = numeric(0),
        Closeness = numeric(0),
        Eigenvector = numeric(0),
        PageRank = numeric(0),
        Clustering_Coeff = numeric(0),
        Strength = numeric(0)
      ),
      edges = data.frame(
        Group = group_name,
        From = character(0),
        To = character(0),
        Weight = numeric(0),
        Betweenness = numeric(0)
      ),
      brain_area_metrics = data.frame(
        Group = group_name,
        Brain_Area = character(0),
        Nodes_Count = numeric(0),
        Avg_Degree = numeric(0),
        Avg_Betweenness = numeric(0),
        Avg_Clustering = numeric(0),
        Internal_Edges = numeric(0),
        External_Edges = numeric(0)
      )
    ))
  }
  
  # Global metrics
  n_nodes <- vcount(g)
  n_edges <- ecount(g)
  density <- edge_density(g)
  is_connected <- is_connected(g)
  clustering <- transitivity(g, type = "global")
  if(is.na(clustering)) clustering <- 0
  
  # Path length (only for connected networks)
  if(is_connected && n_edges > 0) {
    path_length <- tryCatch(mean_distance(g), error = function(e) Inf)
    diameter <- tryCatch(diameter(g), error = function(e) 0)
  } else {
    path_length <- Inf
    diameter <- 0
  }
  
  # Modularity
  modularity_score <- 0
  if(n_edges > 0) {
    tryCatch({
      community <- cluster_louvain(g)
      modularity_score <- modularity(community)
    }, error = function(e) modularity_score <- 0)
  }
  
  # Assortativity
  assortativity_score <- 0
  if(n_edges > 0) {
    tryCatch({
      assortativity_score <- assortativity_degree(g)
      if(is.na(assortativity_score)) assortativity_score <- 0
    }, error = function(e) assortativity_score <- 0)
  }
  
  # Small-world assessment
  small_world_sigma <- 0
  if(is_connected && clustering > 0 && path_length < Inf && n_edges > 0) {
    tryCatch({
      random_g <- sample_gnm(n_nodes, n_edges, directed = FALSE)
      random_clustering <- transitivity(random_g, type = "global")
      random_path <- mean_distance(random_g)
      
      if(!is.na(random_clustering) && random_clustering > 0 && random_path > 0) {
        small_world_sigma <- (clustering/random_clustering) / (path_length/random_path)
      }
    }, error = function(e) {
      small_world_sigma <- 0
    })
  }
  
  # Node-level metrics
  node_names <- if(is.null(V(g)$name)) paste0("Node", 1:vcount(g)) else V(g)$name
  
  # Comprehensive centrality measures
  degree_cent <- degree(g)
  betweenness_cent <- tryCatch(betweenness(g, normalized = TRUE), 
                               error = function(e) rep(0, vcount(g)))
  
  # Closeness (handle disconnected networks)
  if(is_connected) {
    closeness_cent <- tryCatch(closeness(g, normalized = TRUE), 
                               error = function(e) rep(0, vcount(g)))
  } else {
    closeness_cent <- rep(0, vcount(g))
  }
  
  # Eigenvector centrality (weighted - uses edge weights from thresholded network)
  eigen_cent <- tryCatch(eigen_centrality(g, weights = E(g)$weight)$vector,
                         error = function(e) rep(0, vcount(g)))
  
  # PageRank centrality
  pagerank_cent <- tryCatch(page_rank(g)$vector, 
                           error = function(e) rep(0, vcount(g)))
  
  # Local clustering coefficient
  local_clustering <- tryCatch(transitivity(g, type = "local"), 
                              error = function(e) rep(0, vcount(g)))
  local_clustering[is.na(local_clustering)] <- 0
  
  # Node strength (sum of edge weights)
  strength_cent <- tryCatch(strength(g), 
                           error = function(e) rep(0, vcount(g)))
  
  # Assign brain areas to nodes
  node_brain_areas <- assign_nodes_to_brain_areas(node_names, brain_areas)
  
  # Edge-level metrics
  edge_list <- as_edgelist(g, names = TRUE)
  edge_weights <- E(g)$weight
  if(is.null(edge_weights)) edge_weights <- rep(1, nrow(edge_list))
  
  edge_betweenness <- tryCatch(edge_betweenness(g), 
                              error = function(e) rep(0, nrow(edge_list)))
  
  edge_metrics <- data.frame(
    Group = group_name,
    From = edge_list[,1],
    To = edge_list[,2],
    Weight = edge_weights,
    Betweenness = edge_betweenness
  )
  
  # Brain area aggregated metrics
  brain_area_metrics <- compute_brain_area_metrics(g, node_names, node_brain_areas, brain_areas, group_name)
  
  # Enhanced global metrics
  global_metrics <- data.frame(
    Group = group_name,
    Nodes = n_nodes,
    Edges = n_edges,
    Density = density,
    Connected = is_connected,
    Clustering = clustering,
    Path_Length = path_length,
    Small_World_Sigma = small_world_sigma,
    Modularity = modularity_score,
    Assortativity = assortativity_score,
    Diameter = diameter,
    Threshold = ifelse(is.null(g$threshold), 0.3, g$threshold)
  )
  
  # Enhanced node metrics
  node_metrics <- data.frame(
    Group = group_name,
    Node = node_names,
    Brain_Area = node_brain_areas,
    Degree = degree_cent,
    Betweenness = betweenness_cent,
    Closeness = closeness_cent,
    Eigenvector = eigen_cent,
    PageRank = pagerank_cent,
    Clustering_Coeff = local_clustering,
    Strength = strength_cent
  )
  
  return(list(
    global = global_metrics, 
    nodes = node_metrics,
    edges = edge_metrics,
    brain_area_metrics = brain_area_metrics
  ))
}

# Helper function to assign nodes to brain areas
assign_nodes_to_brain_areas <- function(node_names, brain_areas) {
  if(is.null(brain_areas) || length(brain_areas) == 0) {
    return(rep("Unassigned", length(node_names)))
  }
  
  node_brain_areas <- rep("Unassigned", length(node_names))
  
  for(area_name in names(brain_areas)) {
    area_info <- brain_areas[[area_name]]
    
    # Handle different data structures
    if(is.list(area_info) && "regions" %in% names(area_info)) {
      # Structure: brain_areas$area_name$regions
      regions <- area_info$regions
    } else if(is.character(area_info)) {
      # Structure: brain_areas$area_name is directly a character vector
      regions <- area_info
    } else {
      # Skip if unknown structure
      next
    }
    
    # Find matching nodes (case-insensitive)
    matching_indices <- c()
    for (region in regions) {
      matches <- which(tolower(node_names) == tolower(region))
      matching_indices <- c(matching_indices, matches)
    }
    matching_indices <- unique(matching_indices)
    
    if(length(matching_indices) > 0) {
      node_brain_areas[matching_indices] <- area_name
    }
  }
  
  return(node_brain_areas)
}

# Compute brain area aggregated metrics
compute_brain_area_metrics <- function(g, node_names, node_brain_areas, brain_areas, group_name) {
  if(is.null(brain_areas) || length(brain_areas) == 0) {
    return(data.frame(
      Group = character(0),
      Brain_Area = character(0),
      Nodes_Count = numeric(0),
      Avg_Degree = numeric(0),
      Avg_Betweenness = numeric(0),
      Avg_Clustering = numeric(0),
      Internal_Edges = numeric(0),
      External_Edges = numeric(0)
    ))
  }
  
  # Get node metrics
  degrees <- degree(g)
  betweenness <- tryCatch(betweenness(g, normalized = TRUE), error = function(e) rep(0, vcount(g)))
  clustering <- tryCatch(transitivity(g, type = "local"), error = function(e) rep(0, vcount(g)))
  clustering[is.na(clustering)] <- 0
  
  # Get edge list
  edge_list <- as_edgelist(g, names = TRUE)
  
  brain_area_results <- data.frame()
  
  for(area_name in names(brain_areas)) {
    area_nodes <- which(node_brain_areas == area_name)
    
    if(length(area_nodes) > 0) {
      # Basic metrics
      nodes_count <- length(area_nodes)
      avg_degree <- mean(degrees[area_nodes], na.rm = TRUE)
      avg_betweenness <- mean(betweenness[area_nodes], na.rm = TRUE)
      avg_clustering <- mean(clustering[area_nodes], na.rm = TRUE)
      
      # Count internal and external edges
      area_node_names <- node_names[area_nodes]
      internal_edges <- 0
      external_edges <- 0
      
      for(i in 1:nrow(edge_list)) {
        from_node <- edge_list[i, 1]
        to_node <- edge_list[i, 2]
        
        from_in_area <- from_node %in% area_node_names
        to_in_area <- to_node %in% area_node_names
        
        if(from_in_area && to_in_area) {
          internal_edges <- internal_edges + 1
        } else if(from_in_area || to_in_area) {
          external_edges <- external_edges + 1
        }
      }
      
      brain_area_results <- rbind(brain_area_results, data.frame(
        Group = group_name,
        Brain_Area = area_name,
        Nodes_Count = nodes_count,
        Avg_Degree = avg_degree,
        Avg_Betweenness = avg_betweenness,
        Avg_Clustering = avg_clustering,
        Internal_Edges = internal_edges,
        External_Edges = external_edges
      ))
    }
  }
  
  return(brain_area_results)
}

# ==============================================================================
# WEIGHTED EIGENVECTOR CENTRALITY ANALYSIS (FULL CORRELATION NETWORKS)
# ==============================================================================

# Create full weighted network without threshold
create_full_weighted_network <- function(correlation_matrix) {
  if(!has_igraph) return(NULL)
  
  # Use absolute correlation values as weights
  weighted_adj <- abs(correlation_matrix)
  diag(weighted_adj) <- 0  # Remove self-connections
  
  # Create fully connected weighted network
  g <- graph_from_adjacency_matrix(weighted_adj, mode = "undirected", 
                                   weighted = TRUE, diag = FALSE)
  
  # Add network properties
  g$network_type <- "full_weighted"
  g$threshold <- "none"
  
  return(g)
}

# Compute weighted eigenvector centrality for full networks
compute_weighted_eigenvector_centrality <- function(correlation_matrices, groups) {
  if(!has_igraph) return(NULL)
  
  results <- list()
  
  for(group in names(correlation_matrices)) {
    cor_matrix <- correlation_matrices[[group]]
    
    # Create full weighted network
    g <- create_full_weighted_network(cor_matrix)
    
    if(!is.null(g) && vcount(g) > 0) {
      # Get node names
      node_names <- if(is.null(V(g)$name)) rownames(cor_matrix) else V(g)$name
      
      # Compute weighted eigenvector centrality
      eigen_cent <- tryCatch({
        eigen_centrality(g, weights = E(g)$weight)$vector
      }, error = function(e) {
        warning(paste("Eigenvector centrality failed for group", group, ":", e$message))
        rep(0, vcount(g))
      })
      
      
      # Get edge weights statistics for each node
      node_strength <- strength(g, mode = "all", weights = E(g)$weight)
      
      # Average weight of edges connected to each node
      avg_edge_weight <- sapply(1:vcount(g), function(i) {
        incident_edges <- incident(g, i)
        if(length(incident_edges) > 0) {
          mean(E(g)$weight[incident_edges])
        } else {
          0
        }
      })
      
      results[[group]] <- data.frame(
        Group = group,
        Node = node_names,
        Weighted_Eigenvector = eigen_cent,
        Node_Strength = node_strength,
        Avg_Edge_Weight = avg_edge_weight,
        stringsAsFactors = FALSE
      )
    }
  }
  
  # Combine all results
  if(length(results) > 0) {
    all_results <- do.call(rbind, results)
    
    # Add ranking within each group
    all_results <- do.call(rbind, lapply(split(all_results, all_results$Group), function(df) {
      df$Weighted_Rank <- rank(-df$Weighted_Eigenvector, ties.method = "min")
      df
    }))
    
    return(all_results)
  }
  
  return(NULL)
}

# Compare weighted eigenvector centrality across groups
compare_weighted_eigenvector_across_groups <- function(weighted_eigen_results) {
  if(is.null(weighted_eigen_results)) return(NULL)
  
  # Get unique nodes and groups
  nodes <- unique(weighted_eigen_results$Node)
  groups <- unique(weighted_eigen_results$Group)
  
  # Create comparison matrix
  comparison_results <- data.frame()
  
  for(node in nodes) {
    node_data <- weighted_eigen_results[weighted_eigen_results$Node == node, ]
    
    if(nrow(node_data) > 1) {
      # Calculate variance and range across groups
      eigen_values <- node_data$Weighted_Eigenvector
      
      comparison_results <- rbind(comparison_results, data.frame(
        Node = node,
        Mean_Weighted_Eigenvector = mean(eigen_values),
        SD_Weighted_Eigenvector = sd(eigen_values),
        CV_Weighted_Eigenvector = sd(eigen_values) / mean(eigen_values),
        Min_Weighted_Eigenvector = min(eigen_values),
        Max_Weighted_Eigenvector = max(eigen_values),
        Range_Weighted_Eigenvector = max(eigen_values) - min(eigen_values),
        Groups_Present = nrow(node_data),
        stringsAsFactors = FALSE
      ))
    }
  }
  
  # Sort by mean eigenvector centrality
  if(nrow(comparison_results) > 0) {
    comparison_results <- comparison_results[order(-comparison_results$Mean_Weighted_Eigenvector), ]
  }
  
  return(comparison_results)
}

# Identify hub nodes based on weighted eigenvector centrality
identify_weighted_eigenvector_hubs <- function(weighted_eigen_results, top_n = 10, threshold = 0.8) {
  if(is.null(weighted_eigen_results)) return(NULL)
  
  hubs <- list()
  
  # For each group, identify top hub nodes
  for(group in unique(weighted_eigen_results$Group)) {
    group_data <- weighted_eigen_results[weighted_eigen_results$Group == group, ]
    
    # Sort by weighted eigenvector centrality
    group_data <- group_data[order(-group_data$Weighted_Eigenvector), ]
    
    # Get top N nodes or those above threshold
    top_nodes <- head(group_data, top_n)
    threshold_nodes <- group_data[group_data$Weighted_Eigenvector >= threshold, ]
    
    # Combine and remove duplicates
    hub_nodes <- unique(rbind(top_nodes, threshold_nodes))
    
    hubs[[group]] <- hub_nodes
  }
  
  return(hubs)
}

# Compute consensus eigenvector centrality between percolation-based and threshold-free approaches
compute_consensus_eigenvector_centrality <- function(percolation_networks, weighted_eigen_results) {
  if(is.null(percolation_networks) || is.null(weighted_eigen_results)) return(NULL)
  
  consensus_results <- list()
  
  for(group_name in names(percolation_networks)) {
    network <- percolation_networks[[group_name]]
    
    if(!is.null(network) && vcount(network) > 0) {
      # Get percolation-based weighted eigenvector centrality
      percolation_eigen <- tryCatch({
        eigen_centrality(network, weights = E(network)$weight)$vector
      }, error = function(e) {
        rep(0, vcount(network))
      })
      
      # Get node names
      node_names <- if(is.null(V(network)$name)) {
        paste0("Node", 1:vcount(network))
      } else {
        V(network)$name
      }
      
      # Get corresponding weighted eigenvector centrality from full network
      weighted_data <- weighted_eigen_results[weighted_eigen_results$Group == group_name, ]
      
      if(nrow(weighted_data) > 0) {
        # Match nodes between percolation and weighted results
        weighted_eigen <- rep(0, length(node_names))
        for(i in seq_along(node_names)) {
          match_idx <- which(weighted_data$Node == node_names[i])
          if(length(match_idx) > 0) {
            weighted_eigen[i] <- weighted_data$Weighted_Eigenvector[match_idx[1]]
          }
        }
        
        # Compute consensus: geometric mean of the two approaches
        # Use geometric mean to avoid issues with zero values
        consensus_eigen <- sqrt(percolation_eigen * weighted_eigen)

        # Weighted average favoring the approach with higher variation
        percolation_var <- var(percolation_eigen)
        weighted_var <- var(weighted_eigen)
        total_var <- percolation_var + weighted_var
        
        if(total_var > 0) {
          percolation_weight <- percolation_var / total_var
          weighted_weight <- weighted_var / total_var
          consensus_eigen_weighted <- (percolation_eigen * percolation_weight + 
                                     weighted_eigen * weighted_weight)
        } else {
          consensus_eigen_weighted <- consensus_eigen
        }
        
        consensus_results[[group_name]] <- data.frame(
          Group = group_name,
          Node = node_names,
          Percolation_Eigenvector = percolation_eigen,
          Weighted_Eigenvector = weighted_eigen,
          Consensus_Eigenvector_Geometric = consensus_eigen,
          Consensus_Eigenvector_Weighted = consensus_eigen_weighted,
          Percolation_Rank = rank(-percolation_eigen, ties.method = "min"),
          Weighted_Rank = rank(-weighted_eigen, ties.method = "min"),
          Consensus_Rank = rank(-consensus_eigen, ties.method = "min"),
          Rank_Difference = rank(-percolation_eigen, ties.method = "min") - 
                          rank(-weighted_eigen, ties.method = "min"),
          stringsAsFactors = FALSE
        )
      }
    }
  }
  
  # Combine all results
  if(length(consensus_results) > 0) {
    return(do.call(rbind, consensus_results))
  }
  
  return(NULL)
}

# ==============================================================================
# THRESHOLD-FREE NETWORK METRICS (NEW ANALYSIS OPTIONS)
# ==============================================================================

# 1. Node Strength Analysis
compute_node_strength <- function(correlation_matrices, groups) {
  results <- list()
  
  for(group in names(correlation_matrices)) {
    cor_matrix <- correlation_matrices[[group]]
    
    # Remove diagonal (self-connections)
    diag(cor_matrix) <- 0
    
    # Calculate node strength (sum of absolute correlations)
    node_strength <- rowSums(abs(cor_matrix))
    
    # Get node names
    node_names <- rownames(cor_matrix)
    if(is.null(node_names)) {
      node_names <- paste0("Node", 1:nrow(cor_matrix))
    }
    
    results[[group]] <- data.frame(
      Group = group,
      Node = node_names,
      Node_Strength = node_strength,
      Normalized_Strength = node_strength / (nrow(cor_matrix) - 1), # Normalize by max possible connections
      stringsAsFactors = FALSE
    )
  }
  
  # Combine all results
  if(length(results) > 0) {
    all_results <- do.call(rbind, results)
    
    # Add ranking within each group
    all_results <- do.call(rbind, lapply(split(all_results, all_results$Group), function(df) {
      df$Strength_Rank <- rank(-df$Node_Strength, ties.method = "min")
      df
    }))
    
    return(all_results)
  }
  
  return(NULL)
}

# Combined threshold-free analysis function
perform_threshold_free_analysis <- function(correlation_matrices, groups, analysis_type = "all") {
  results <- list()

  # Perform requested analyses
  if(analysis_type == "all" || analysis_type == "node_strength") {
    results$node_strength <- compute_node_strength(correlation_matrices, groups)
  }

  # Weighted eigenvector centrality (needed for standardized consensus)
  if(analysis_type == "all" || analysis_type == "weighted_eigenvector") {
    results$weighted_eigenvector <- compute_weighted_eigenvector_centrality(correlation_matrices, groups)
  }

  return(results)
}

# ==============================================================================
# NEW RESTRUCTURED PIPELINE FUNCTIONS
# ==============================================================================

# Minimum Spanning Tree Analysis
compute_mst_analysis <- function(correlation_matrices) {
  if(!has_igraph) return(NULL)
  
  results <- list()
  
  for(group in names(correlation_matrices)) {
    cor_matrix <- correlation_matrices[[group]]
    
    # Convert correlation to distance (1 - |correlation|)
    dist_matrix <- 1 - abs(cor_matrix)
    diag(dist_matrix) <- 0
    
    # Create graph from distance matrix
    g <- graph_from_adjacency_matrix(dist_matrix, mode = "undirected", weighted = TRUE, diag = FALSE)
    
    # Compute MST
    mst <- mst(g)
    
    # Get node names
    node_names <- if(is.null(V(mst)$name)) rownames(cor_matrix) else V(mst)$name
    
    # Compute centrality measures for MST
    degree_cent <- degree(mst)
    betweenness_cent <- tryCatch(betweenness(mst), error = function(e) rep(0, vcount(mst)))
    closeness_cent <- tryCatch(closeness(mst), error = function(e) rep(0, vcount(mst)))
    
    # Identify most central nodes (top 5 by different measures)
    central_nodes <- list(
      degree = names(sort(degree_cent, decreasing = TRUE))[1:min(5, length(degree_cent))],
      betweenness = names(sort(betweenness_cent, decreasing = TRUE))[1:min(5, length(betweenness_cent))],
      closeness = names(sort(closeness_cent, decreasing = TRUE))[1:min(5, length(closeness_cent))]
    )
    
    # MST metrics
    mst_metrics <- list(
      group = group,
      mst_graph = mst,
      n_nodes = vcount(mst),
      n_edges = ecount(mst),
      diameter = tryCatch(diameter(mst), error = function(e) 0),
      avg_path_length = tryCatch(mean_distance(mst), error = function(e) Inf),
      max_degree = max(degree_cent),
      degree_centrality = degree_cent,
      betweenness_centrality = betweenness_cent,
      closeness_centrality = closeness_cent,
      central_nodes = central_nodes,
      total_weight = sum(E(mst)$weight),
      avg_weight = mean(E(mst)$weight),
      node_names = node_names
    )
    
    results[[group]] <- mst_metrics
  }
  
  return(results)
}

# PCA Analysis function
compute_pca_analysis <- function(correlation_matrices) {
  results <- list()
  
  for(group in names(correlation_matrices)) {
    cor_matrix <- correlation_matrices[[group]]
    
    # Perform PCA on correlation matrix
    tryCatch({
      pca_result <- prcomp(cor_matrix, center = TRUE, scale. = TRUE)
      
      # Calculate variance explained
      variance_explained <- pca_result$sdev^2 / sum(pca_result$sdev^2)
      
      results[[group]] <- list(
        pca = pca_result,
        variance_explained = variance_explained,
        cumulative_variance = cumsum(variance_explained),
        loadings = pca_result$rotation,
        scores = pca_result$x
      )
    }, error = function(e) {
      warning(sprintf("PCA failed for group %s: %s", group, e$message))
      results[[group]] <- NULL
    })
  }
  
  return(results)
}

# Cross-method comparison analysis
perform_cross_method_comparison <- function(weighted_eigenvector, threshold_free_results, 
                                          percolation_networks, node_metrics) {
  comparison_results <- list()
  
  # Compare eigenvector centrality between methods
  if(!is.null(weighted_eigenvector) && !is.null(node_metrics)) {
    eigenvector_comparison <- data.frame()
    
    for(group in unique(weighted_eigenvector$Group)) {
      weighted_data <- weighted_eigenvector[weighted_eigenvector$Group == group, ]
      percolation_data <- node_metrics[node_metrics$Group == group, ]
      
      # Match nodes
      common_nodes <- intersect(weighted_data$Node, percolation_data$Node)
      
      if(length(common_nodes) > 0) {
        for(node in common_nodes) {
          weighted_eigen <- weighted_data$Weighted_Eigenvector[weighted_data$Node == node]
          percolation_eigen <- percolation_data$Eigenvector[percolation_data$Node == node]
          
          eigenvector_comparison <- rbind(eigenvector_comparison, data.frame(
            Group = group,
            Node = node,
            Weighted_Eigenvector = weighted_eigen,
            Percolation_Eigenvector = percolation_eigen,
            Difference = weighted_eigen - percolation_eigen,
            Ratio = ifelse(percolation_eigen > 0, weighted_eigen / percolation_eigen, NA),
            stringsAsFactors = FALSE
          ))
        }
      }
    }
    
    comparison_results$eigenvector_comparison <- eigenvector_comparison
  }
  
  # Compare node strength between methods
  if(!is.null(threshold_free_results$node_strength) && !is.null(node_metrics)) {
    strength_comparison <- data.frame()
    
    for(group in unique(threshold_free_results$node_strength$Group)) {
      strength_data <- threshold_free_results$node_strength[threshold_free_results$node_strength$Group == group, ]
      percolation_data <- node_metrics[node_metrics$Group == group, ]
      
      # Match nodes
      common_nodes <- intersect(strength_data$Node, percolation_data$Node)
      
      if(length(common_nodes) > 0) {
        for(node in common_nodes) {
          threshold_free_strength <- strength_data$Node_Strength[strength_data$Node == node]
          percolation_strength <- percolation_data$Strength[percolation_data$Node == node]
          
          strength_comparison <- rbind(strength_comparison, data.frame(
            Group = group,
            Node = node,
            Threshold_Free_Strength = threshold_free_strength,
            Percolation_Strength = percolation_strength,
            Difference = threshold_free_strength - percolation_strength,
            Ratio = ifelse(percolation_strength > 0, threshold_free_strength / percolation_strength, NA),
            stringsAsFactors = FALSE
          ))
        }
      }
    }
    
    comparison_results$strength_comparison <- strength_comparison
  }
  
  # Method correlation analysis
  if(!is.null(comparison_results$eigenvector_comparison)) {
    method_correlations <- list()
    
    for(group in unique(comparison_results$eigenvector_comparison$Group)) {
      group_data <- comparison_results$eigenvector_comparison[comparison_results$eigenvector_comparison$Group == group, ]
      
      # Correlation between weighted and percolation eigenvector centrality
      if(nrow(group_data) > 2) {
        cor_result <- tryCatch({
          cor.test(group_data$Weighted_Eigenvector, group_data$Percolation_Eigenvector)
        }, error = function(e) NULL)
        
        if(!is.null(cor_result)) {
          method_correlations[[group]] <- list(
            correlation = cor_result$estimate,
            p_value = cor_result$p.value,
            ci_lower = cor_result$conf.int[1],
            ci_upper = cor_result$conf.int[2]
          )
        }
      }
    }
    
    comparison_results$method_correlations <- method_correlations
  }
  
  return(comparison_results)
}

# ==============================================================================
# NETWORK CONSERVATION ANALYSIS FUNCTIONS
# ==============================================================================

# Function to compute network similarity metrics
compute_network_similarity <- function(adj_matrix1, adj_matrix2, threshold1 = 0.5, threshold2 = 0.5) {
  # Ensure matrices have same dimensions
  common_nodes <- intersect(rownames(adj_matrix1), rownames(adj_matrix2))
  if(length(common_nodes) < 2) return(NULL)
  
  adj1 <- adj_matrix1[common_nodes, common_nodes]
  adj2 <- adj_matrix2[common_nodes, common_nodes]
  
  # Convert to binary adjacency matrices using group-specific thresholds
  adj1_bin <- (abs(adj1) > threshold1) * 1  
  adj2_bin <- (abs(adj2) > threshold2) * 1  
  
  # Remove diagonal
  diag(adj1_bin) <- 0
  diag(adj2_bin) <- 0
  
  # Get upper triangular parts (undirected networks)
  adj1_upper <- adj1_bin[upper.tri(adj1_bin)]
  adj2_upper <- adj2_bin[upper.tri(adj2_bin)]
  
  # Compute similarity metrics
  intersection <- sum(adj1_upper & adj2_upper)
  union <- sum(adj1_upper | adj2_upper)
  
  jaccard <- ifelse(union > 0, intersection / union, 0)
  
  # Overlap coefficient
  min_edges <- min(sum(adj1_upper), sum(adj2_upper))
  overlap <- ifelse(min_edges > 0, intersection / min_edges, 0)
  
  # Edge preservation
  total_edges_net1 <- sum(adj1_upper)
  total_edges_net2 <- sum(adj2_upper)
  
  edge_preservation_1to2 <- ifelse(total_edges_net1 > 0, intersection / total_edges_net1, 0)
  edge_preservation_2to1 <- ifelse(total_edges_net2 > 0, intersection / total_edges_net2, 0)
  
  return(list(
    jaccard = jaccard,
    overlap = overlap,
    intersection = intersection,
    union = union,
    edges_net1 = total_edges_net1,
    edges_net2 = total_edges_net2,
    edge_preservation_1to2 = edge_preservation_1to2,
    edge_preservation_2to1 = edge_preservation_2to1
  ))
}

# Function to compute hub conservation
compute_hub_conservation <- function(graph_list, top_n = 5) {
  # Compute degree centrality for each network
  degree_lists <- lapply(graph_list, function(g) {
    if(vcount(g) > 0) {
      deg <- degree(g)
      return(sort(deg, decreasing = TRUE))
    } else {
      return(numeric(0))
    }
  })
  
  # Get top hubs for each network
  top_hubs <- lapply(degree_lists, function(deg) {
    if(length(deg) >= top_n) {
      return(names(deg)[1:top_n])
    } else {
      return(names(deg))
    }
  })
  
  # Compute pairwise hub overlap
  group_names <- names(graph_list)
  hub_conservation <- matrix(0, nrow = length(group_names), ncol = length(group_names))
  rownames(hub_conservation) <- group_names
  colnames(hub_conservation) <- group_names
  
  for(i in 1:length(group_names)) {
    for(j in 1:length(group_names)) {
      if(i != j) {
        hubs_i <- top_hubs[[i]]
        hubs_j <- top_hubs[[j]]
        if(length(hubs_i) > 0 && length(hubs_j) > 0) {
          overlap <- length(intersect(hubs_i, hubs_j))
          max_possible <- min(length(hubs_i), length(hubs_j))
          hub_conservation[i, j] <- overlap / max_possible
        }
      } else {
        hub_conservation[i, j] <- 1  # Perfect conservation with self
      }
    }
  }
  
  return(list(
    hub_conservation_matrix = hub_conservation,
    top_hubs = top_hubs,
    degree_lists = degree_lists
  ))
}

# Function to identify conserved edges
identify_conserved_edges <- function(adj_matrices) {
  if(length(adj_matrices) < 2) {
    return(NULL)
  }
  
  # Get common nodes across all networks
  all_nodes <- Reduce(intersect, lapply(adj_matrices, rownames))
  
  if(length(all_nodes) < 2) {
    return(NULL)
  }
  
  # Subset matrices to common nodes
  adj_common <- lapply(adj_matrices, function(adj) {
    adj[all_nodes, all_nodes]
  })
  
  # Convert to binary and get consensus
  adj_binary <- lapply(adj_common, function(adj) {
    binary <- (adj != 0) * 1
    diag(binary) <- 0
    return(binary)
  })
  
  # Find edges present in all networks
  consensus_matrix <- Reduce("+", adj_binary)
  n_networks <- length(adj_binary)
  
  # Different conservation levels
  fully_conserved <- (consensus_matrix == n_networks) * 1
  majority_conserved <- (consensus_matrix >= ceiling(n_networks/2)) * 1
  
  # Convert to edge lists
  fully_conserved_edges <- which(fully_conserved == 1 & upper.tri(fully_conserved), arr.ind = TRUE)
  majority_conserved_edges <- which(majority_conserved == 1 & upper.tri(majority_conserved), arr.ind = TRUE)
  
  # Create data frames with node names
  create_edge_df <- function(edges, nodes) {
    if(nrow(edges) > 0) {
      data.frame(
        Node1 = nodes[edges[,1]],
        Node2 = nodes[edges[,2]],
        stringsAsFactors = FALSE
      )
    } else {
      data.frame(Node1 = character(), Node2 = character())
    }
  }
  
  return(list(
    consensus_matrix = consensus_matrix / n_networks,  # Normalize to 0-1
    fully_conserved = create_edge_df(fully_conserved_edges, all_nodes),
    majority_conserved = create_edge_df(majority_conserved_edges, all_nodes),
    conservation_proportion = sum(fully_conserved) / sum(upper.tri(fully_conserved))
  ))
}

# Main conservation analysis function
perform_conservation_analysis <- function(networks, correlations, thresholds = NULL) {
  conservation_results <- list()
  
  # Get valid networks (with nodes)
  valid_groups <- names(networks)[sapply(networks, function(g) vcount(g) > 0)]
  
  if(length(valid_groups) < 2) {
    return(NULL)
  }
  
  valid_networks <- networks[valid_groups]
  valid_correlations <- correlations[valid_groups]
  
  # Extract percolation thresholds from networks if available, otherwise use provided thresholds
  group_thresholds <- rep(0.5, length(valid_groups))  # default
  names(group_thresholds) <- valid_groups
  
  if(!is.null(thresholds)) {
    for(group in valid_groups) {
      if(!is.null(thresholds[[group]])) {
        group_thresholds[group] <- thresholds[[group]]
      }
    }
  } else {
    # Try to extract thresholds from network objects
    for(i in seq_along(valid_networks)) {
      group <- valid_groups[i]
      if(!is.null(valid_networks[[i]]$threshold)) {
        group_thresholds[group] <- valid_networks[[i]]$threshold
      }
    }
  }
  
  # 1. Network similarity analysis using group-specific thresholds
  similarity_matrix <- matrix(0, nrow = length(valid_groups), ncol = length(valid_groups))
  rownames(similarity_matrix) <- valid_groups
  colnames(similarity_matrix) <- valid_groups
  
  similarity_details <- list()
  
  for(i in 1:length(valid_groups)) {
    for(j in 1:length(valid_groups)) {
      if(i != j) {
        group1 <- valid_groups[i]
        group2 <- valid_groups[j]
        threshold1 <- group_thresholds[group1]
        threshold2 <- group_thresholds[group2]
        
        sim_result <- compute_network_similarity(valid_correlations[[i]], valid_correlations[[j]], 
                                               threshold1, threshold2)
        if(!is.null(sim_result)) {
          similarity_matrix[i, j] <- sim_result$jaccard
          similarity_details[[paste(group1, "vs", group2)]] <- sim_result
        }
      } else {
        similarity_matrix[i, j] <- 1.0
      }
    }
  }
  
  conservation_results$similarity_matrix <- similarity_matrix
  conservation_results$similarity_details <- similarity_details
  
  # 2. Hub conservation analysis
  hub_conservation <- compute_hub_conservation(valid_networks, top_n = 5)
  conservation_results$hub_conservation <- hub_conservation
  
  # 3. Edge conservation analysis using group-specific thresholds
  binary_adjacency <- list()
  for(i in seq_along(valid_correlations)) {
    group <- valid_groups[i]
    threshold <- group_thresholds[group]
    adj <- valid_correlations[[i]]
    binary <- (abs(adj) > threshold) * 1
    diag(binary) <- 0
    binary_adjacency[[group]] <- binary
  }
  
  edge_conservation <- identify_conserved_edges(binary_adjacency)
  conservation_results$edge_conservation <- edge_conservation
  conservation_results$group_thresholds <- group_thresholds
  
  return(conservation_results)
}

# ==============================================================================
# WEIGHTED NETWORK CONSERVATION ANALYSIS FUNCTIONS
# ==============================================================================

# Function to compute weighted network similarity metrics
compute_weighted_network_similarity <- function(corr_matrix1, corr_matrix2) {
  # Ensure matrices have same dimensions
  common_nodes <- intersect(rownames(corr_matrix1), rownames(corr_matrix2))
  if(length(common_nodes) < 2) return(NULL)
  
  corr1 <- corr_matrix1[common_nodes, common_nodes]
  corr2 <- corr_matrix2[common_nodes, common_nodes]
  
  # Remove diagonal
  diag(corr1) <- 0
  diag(corr2) <- 0
  
  # Get upper triangular parts (undirected networks)
  corr1_upper <- corr1[upper.tri(corr1)]
  corr2_upper <- corr2[upper.tri(corr2)]
  
  # Use absolute values for weight-based similarity
  abs_corr1 <- abs(corr1_upper)
  abs_corr2 <- abs(corr2_upper)
  
  # Weighted Jaccard similarity: sum(min) / sum(max)
  min_weights <- pmin(abs_corr1, abs_corr2)
  max_weights <- pmax(abs_corr1, abs_corr2)
  
  weighted_jaccard <- ifelse(sum(max_weights) > 0, sum(min_weights) / sum(max_weights), 0)
  
  # Weighted overlap coefficient: sum(min) / min(sum1, sum2)
  sum_weights1 <- sum(abs_corr1)
  sum_weights2 <- sum(abs_corr2)
  min_sum_weights <- min(sum_weights1, sum_weights2)
  
  weighted_overlap <- ifelse(min_sum_weights > 0, sum(min_weights) / min_sum_weights, 0)
  
  # Correlation between weight vectors
  weight_correlation <- ifelse(length(abs_corr1) > 1, cor(abs_corr1, abs_corr2, use = "complete.obs"), 0)
  if(is.na(weight_correlation)) weight_correlation <- 0
  
  # Edge weight preservation (proportion of weight preserved)
  weight_preservation_1to2 <- ifelse(sum_weights1 > 0, sum(min_weights) / sum_weights1, 0)
  weight_preservation_2to1 <- ifelse(sum_weights2 > 0, sum(min_weights) / sum_weights2, 0)
  
  # Mean absolute difference in weights
  mean_weight_diff <- mean(abs(abs_corr1 - abs_corr2))
  
  return(list(
    weighted_jaccard = weighted_jaccard,
    weighted_overlap = weighted_overlap,
    weight_correlation = weight_correlation,
    weight_preservation_1to2 = weight_preservation_1to2,
    weight_preservation_2to1 = weight_preservation_2to1,
    mean_weight_difference = mean_weight_diff,
    total_weight_1 = sum_weights1,
    total_weight_2 = sum_weights2,
    shared_weight = sum(min_weights)
  ))
}

# Function to compute weighted hub conservation
compute_weighted_hub_conservation <- function(weighted_eigenvector, node_strength_results, top_n = 5) {
  if(is.null(weighted_eigenvector) || is.null(node_strength_results)) return(NULL)
  
  # Get unique groups
  groups <- unique(weighted_eigenvector$Group)
  if(length(groups) < 2) return(NULL)
  
  # Create hub conservation matrices for different metrics
  n_groups <- length(groups)
  
  # Eigenvector-based hub conservation
  eigenvector_conservation <- matrix(0, nrow = n_groups, ncol = n_groups)
  rownames(eigenvector_conservation) <- groups
  colnames(eigenvector_conservation) <- groups
  
  # Node strength-based hub conservation  
  strength_conservation <- matrix(0, nrow = n_groups, ncol = n_groups)
  rownames(strength_conservation) <- groups
  colnames(strength_conservation) <- groups
  
  for(i in 1:n_groups) {
    for(j in 1:n_groups) {
      group1 <- groups[i]
      group2 <- groups[j]
      
      if(i != j) {
        # Eigenvector hub conservation
        eig_data1 <- weighted_eigenvector[weighted_eigenvector$Group == group1, ]
        eig_data2 <- weighted_eigenvector[weighted_eigenvector$Group == group2, ]
        
        if(nrow(eig_data1) >= top_n && nrow(eig_data2) >= top_n) {
          # Get top hubs by weighted eigenvector centrality
          top_hubs1 <- head(eig_data1[order(-eig_data1$Weighted_Eigenvector), "Node"], top_n)
          top_hubs2 <- head(eig_data2[order(-eig_data2$Weighted_Eigenvector), "Node"], top_n)
          
          # Calculate overlap
          overlap_eig <- length(intersect(top_hubs1, top_hubs2))
          eigenvector_conservation[i, j] <- overlap_eig / top_n
        }
        
        # Node strength hub conservation
        str_data1 <- node_strength_results[node_strength_results$Group == group1, ]
        str_data2 <- node_strength_results[node_strength_results$Group == group2, ]
        
        if(nrow(str_data1) >= top_n && nrow(str_data2) >= top_n) {
          # Get top hubs by node strength
          top_hubs1 <- head(str_data1[order(-str_data1$Node_Strength), "Node"], top_n)
          top_hubs2 <- head(str_data2[order(-str_data2$Node_Strength), "Node"], top_n)
          
          # Calculate overlap
          overlap_str <- length(intersect(top_hubs1, top_hubs2))
          strength_conservation[i, j] <- overlap_str / top_n
        }
        
      } else {
        # Perfect self-conservation
        eigenvector_conservation[i, j] <- 1.0
        strength_conservation[i, j] <- 1.0
      }
    }
  }
  
  return(list(
    eigenvector_hub_conservation = eigenvector_conservation,
    strength_hub_conservation = strength_conservation,
    top_n = top_n
  ))
}

# Function to analyze weighted edge statistics
analyze_weighted_edge_statistics <- function(correlations, groups) {
  if(length(correlations) < 2) return(NULL)
  
  edge_stats <- data.frame()
  
  for(group in names(correlations)) {
    corr_matrix <- correlations[[group]]
    
    # Remove diagonal
    diag(corr_matrix) <- 0
    
    # Get upper triangular weights
    weights <- abs(corr_matrix[upper.tri(corr_matrix)])
    
    # Calculate statistics
    stats <- data.frame(
      Group = group,
      Mean_Weight = mean(weights),
      Median_Weight = median(weights),
      SD_Weight = sd(weights),
      Min_Weight = min(weights),
      Max_Weight = max(weights),
      Total_Weight = sum(weights),
      N_Edges = length(weights),
      Strong_Edges_03 = sum(weights > 0.3),
      Strong_Edges_05 = sum(weights > 0.5),
      Strong_Edges_07 = sum(weights > 0.7),
      stringsAsFactors = FALSE
    )
    
    edge_stats <- rbind(edge_stats, stats)
  }
  
  return(edge_stats)
}

# Main weighted conservation analysis function
perform_weighted_conservation_analysis <- function(correlations, weighted_eigenvector, node_strength_results) {
  weighted_conservation_results <- list()
  
  # Get valid groups
  valid_groups <- names(correlations)
  
  if(length(valid_groups) < 2) {
    return(NULL)
  }
  
  # 1. Weighted network similarity analysis
  similarity_matrix <- matrix(0, nrow = length(valid_groups), ncol = length(valid_groups))
  rownames(similarity_matrix) <- valid_groups
  colnames(similarity_matrix) <- valid_groups
  
  similarity_details <- list()
  
  for(i in 1:length(valid_groups)) {
    for(j in 1:length(valid_groups)) {
      group1 <- valid_groups[i]
      group2 <- valid_groups[j]
      
      if(i != j) {
        sim_result <- compute_weighted_network_similarity(correlations[[group1]], correlations[[group2]])
        if(!is.null(sim_result)) {
          similarity_matrix[i, j] <- sim_result$weighted_jaccard
          similarity_details[[paste(group1, "vs", group2)]] <- sim_result
        }
      } else {
        similarity_matrix[i, j] <- 1.0
      }
    }
  }
  
  weighted_conservation_results$similarity_matrix = similarity_matrix
  weighted_conservation_results$similarity_details = similarity_details
  
  # 2. Weighted hub conservation analysis
  if(!is.null(weighted_eigenvector) && !is.null(node_strength_results)) {
    weighted_hub_conservation <- compute_weighted_hub_conservation(weighted_eigenvector, node_strength_results, top_n = 5)
    weighted_conservation_results$hub_conservation = weighted_hub_conservation
  }
  
  # 3. Weighted edge statistics analysis
  edge_statistics <- analyze_weighted_edge_statistics(correlations, valid_groups)
  weighted_conservation_results$edge_statistics = edge_statistics
  
  return(weighted_conservation_results)
}

# ============================================================================
# PERSISTENCE AGGREGATED METRICS
# ============================================================================

#' Compute Persistence Aggregated Metrics
#'
#' Derives single-value node strength and eigenvector centrality from persistence
#' data across thresholds using stability-weighted averaging
#'
#' @param persistence_data List of threshold data (named by threshold value)
#' @return Data frame with aggregated metrics per node
#'
compute_persistence_aggregated_metrics <- function(persistence_data) {

  if(is.null(persistence_data) || length(persistence_data) == 0) {
    return(NULL)
  }

  thresholds <- names(persistence_data)

  # Collect all unique nodes across all thresholds
  all_nodes <- unique(unlist(lapply(persistence_data, function(t) {
    if(!is.null(t$nodes)) t$nodes$Node else character(0)
  })))

  if(length(all_nodes) == 0) {
    return(NULL)
  }

  # Initialize results data frame
  results <- data.frame(
    Node = all_nodes,
    MeanNodeStrength = 0,
    MeanEigenvector = 0,
    StabilityScore = 0,
    AppearanceRate = 0,
    stringsAsFactors = FALSE
  )

  # Compute aggregated metrics for each node
  for(node in all_nodes) {
    strength_vals <- c()
    eig_vals <- c()
    stability_weights <- c()

    # Collect values across all thresholds
    for(thresh in thresholds) {
      thresh_data <- persistence_data[[thresh]]

      if(!is.null(thresh_data$nodes)) {
        node_row <- thresh_data$nodes[thresh_data$nodes$Node == node, ]

        if(nrow(node_row) > 0) {
          # Collect node metrics
          strength_vals <- c(strength_vals, node_row$Strength)
          eig_vals <- c(eig_vals, node_row$Eigenvector)

          # Compute network quality weight (clustering × density)
          if(!is.null(thresh_data$global)) {
            clustering <- ifelse(!is.null(thresh_data$global$Clustering) && is.finite(thresh_data$global$Clustering),
                                thresh_data$global$Clustering, 0)
            density <- ifelse(!is.null(thresh_data$global$Density) && is.finite(thresh_data$global$Density),
                             thresh_data$global$Density, 0)

            weight <- clustering * density
            weight <- ifelse(is.finite(weight) && weight > 0, weight, 0.01) # Minimum weight
            stability_weights <- c(stability_weights, weight)
          } else {
            stability_weights <- c(stability_weights, 0.01) # Minimal weight if no global data
          }
        }
      }
    }

    # Compute stability-weighted averages
    if(length(strength_vals) > 0 && length(stability_weights) > 0) {
      # Ensure no NA/Inf values
      valid_idx <- is.finite(strength_vals) & is.finite(eig_vals) & is.finite(stability_weights)

      if(sum(valid_idx) > 0) {
        results$MeanNodeStrength[results$Node == node] <-
          weighted.mean(strength_vals[valid_idx], stability_weights[valid_idx], na.rm = TRUE)

        results$MeanEigenvector[results$Node == node] <-
          weighted.mean(eig_vals[valid_idx], stability_weights[valid_idx], na.rm = TRUE)

        results$StabilityScore[results$Node == node] <-
          mean(stability_weights[valid_idx], na.rm = TRUE)

        results$AppearanceRate[results$Node == node] <-
          length(strength_vals) / length(thresholds)
      }
    }
  }

  # Sort by mean eigenvector (descending)
  results <- results[order(-results$MeanEigenvector), ]
  rownames(results) <- NULL

  return(results)
}


#' Compute Area Under Curve (AUC) Metrics Across Thresholds
#'
#' Integrates network metrics across multiple correlation thresholds
#' using trapezoidal rule for threshold-independent summary statistics
#'
#' @param correlation_matrix Correlation matrix
#' @param thresholds Vector of thresholds to evaluate
#' @param brain_areas List of brain area definitions (optional)
#' @return List containing global and node-level AUC metrics
compute_auc_metrics <- function(correlation_matrix,
                               thresholds = seq(0.05, 0.5, by = 0.025),
                               brain_areas = NULL) {


  # Check if pracma package available for trapezoidal integration
  if(!has_pracma && !.warning_shown$pracma) {
    warning("pracma package not available, using simplified AUC computation")
    .warning_shown$pracma <- TRUE
  }

  n_nodes <- nrow(correlation_matrix)
  node_names <- rownames(correlation_matrix)

  # Initialize storage for metrics at each threshold
  metrics_by_threshold <- list()

  for(tau in thresholds) {
    # Create network at this threshold
    adj_matrix <- (abs(correlation_matrix) >= tau) * 1
    diag(adj_matrix) <- 0

    g <- graph_from_adjacency_matrix(adj_matrix, mode = "undirected", diag = FALSE)

    if(ecount(g) > 0) {
      V(g)$name <- node_names

      # Global metrics
      global <- list(
        Threshold = tau,
        Density = edge_density(g),
        Clustering = transitivity(g, type = "global"),
        N_Edges = ecount(g)
      )

      # Try to compute path length (can fail for disconnected networks)
      tryCatch({
        if(is_connected(g)) {
          global$PathLength <- mean_distance(g, directed = FALSE)
        } else {
          # Use largest component
          comps <- components(g)
          largest_comp <- which.max(comps$csize)
          g_largest <- induced_subgraph(g, which(comps$membership == largest_comp))
          if(vcount(g_largest) > 1) {
            global$PathLength <- mean_distance(g_largest, directed = FALSE)
          } else {
            global$PathLength <- 0
          }
        }
      }, error = function(e) {
        global$PathLength <<- 0
      })

      # Modularity
      tryCatch({
        comm <- cluster_louvain(g)
        global$Modularity <- modularity(comm)
      }, error = function(e) {
        global$Modularity <<- 0
      })

      # Node-level metrics
      node_metrics <- data.frame(
        Node = V(g)$name,
        Degree = degree(g),
        Strength = degree(g),  # For binary, same as degree
        Eigenvector = eigen_centrality(g)$vector,
        stringsAsFactors = FALSE
      )

      # Betweenness (can be slow for large networks)
      if(vcount(g) <= 100) {
        node_metrics$Betweenness <- betweenness(g, normalized = TRUE)
      }

      metrics_by_threshold[[as.character(tau)]] <- list(
        threshold = tau,
        global = global,
        nodes = node_metrics
      )
    }
  }

  if(length(metrics_by_threshold) == 0) {
    warning("No valid networks across thresholds")
    return(NULL)
  }

  # Extract threshold sequence that had valid networks
  valid_thresholds <- sapply(metrics_by_threshold, function(m) m$threshold)

  # Compute AUC for global metrics using trapezoidal integration
  auc_results <- list()

  # Global AUC
  global_metrics <- c("Density", "Clustering", "PathLength", "Modularity", "N_Edges")

  global_auc <- data.frame(
    Metric = global_metrics,
    AUC = NA,
    Mean = NA,
    SD = NA,
    stringsAsFactors = FALSE
  )

  for(metric in global_metrics) {
    values <- sapply(metrics_by_threshold, function(m) {
      if(!is.null(m$global[[metric]])) {
        return(m$global[[metric]])
      } else {
        return(0)
      }
    })

    # Compute AUC
    if(has_pracma) {
      auc_val <- pracma::trapz(valid_thresholds, values)
    } else {
      # Simplified trapezoidal rule
      auc_val <- sum(diff(valid_thresholds) * (head(values, -1) + tail(values, -1)) / 2)
    }

    global_auc$AUC[global_auc$Metric == metric] <- auc_val
    global_auc$Mean[global_auc$Metric == metric] <- mean(values, na.rm = TRUE)
    global_auc$SD[global_auc$Metric == metric] <- sd(values, na.rm = TRUE)
  }

  # Node-level AUC
  node_metrics <- c("Degree", "Strength", "Eigenvector")

  if(!is.null(metrics_by_threshold[[1]]$nodes$Betweenness)) {
    node_metrics <- c(node_metrics, "Betweenness")
  }

  node_auc <- data.frame(
    Node = node_names,
    stringsAsFactors = FALSE
  )

  for(metric in node_metrics) {
    auc_col <- paste0("AUC_", metric)
    node_auc[[auc_col]] <- 0

    for(node in node_names) {
      # Collect values across thresholds
      values <- sapply(metrics_by_threshold, function(m) {
        if(!is.null(m$nodes)) {
          node_row <- m$nodes[m$nodes$Node == node, ]
          if(nrow(node_row) > 0 && metric %in% names(node_row)) {
            return(node_row[[metric]])
          }
        }
        return(0)
      })

      # Compute AUC
      if(has_pracma) {
        auc_val <- pracma::trapz(valid_thresholds, values)
      } else {
        auc_val <- sum(diff(valid_thresholds) * (head(values, -1) + tail(values, -1)) / 2)
      }

      node_auc[node_auc$Node == node, auc_col] <- auc_val
    }
  }

  # Regional AUC (if brain areas provided)
  regional_auc <- NULL

  if(!is.null(brain_areas) && length(brain_areas) > 0) {
    regional_auc <- data.frame(
      Region = character(),
      AUC_Degree = numeric(),
      AUC_Eigenvector = numeric(),
      N_Nodes = integer(),
      stringsAsFactors = FALSE
    )

    for(region_name in names(brain_areas)) {
      region_nodes <- brain_areas[[region_name]]
      region_nodes <- intersect(region_nodes, node_names)

      if(length(region_nodes) > 0) {
        # Mean AUC across nodes in region
        region_data <- node_auc[node_auc$Node %in% region_nodes, ]

        regional_auc <- rbind(regional_auc, data.frame(
          Region = region_name,
          AUC_Degree = mean(region_data$AUC_Degree, na.rm = TRUE),
          AUC_Eigenvector = mean(region_data$AUC_Eigenvector, na.rm = TRUE),
          N_Nodes = length(region_nodes),
          stringsAsFactors = FALSE
        ))
      }
    }
  }

  return(list(
    global_auc = global_auc,
    node_auc = node_auc,
    regional_auc = regional_auc,
    threshold_range = range(valid_thresholds),
    n_thresholds = length(valid_thresholds),
    metrics_by_threshold = metrics_by_threshold
  ))
}


#' Compute Persistence AUC Metrics for Eigenvector and Node Strength
#'
#' Calculates Area Under the Curve (AUC) for eigenvector centrality and node strength
#' across multiple correlation thresholds using trapezoidal integration.
#' Missing nodes at any threshold are treated as having 0 eigenvector/strength.
#'
#' @param persistence_data List of persistence analysis results (keyed by threshold)
#' @param all_nodes Character vector of all node names to include
#' @return Data frame with Node, Eigenvector_AUC, Strength_AUC columns
compute_persistence_auc_metrics <- function(persistence_data, all_nodes) {

  if (is.null(persistence_data) || length(persistence_data) == 0) {
    return(data.frame(
      Node = all_nodes,
      Eigenvector_AUC = rep(0, length(all_nodes)),
      Strength_AUC = rep(0, length(all_nodes)),
      stringsAsFactors = FALSE
    ))
  }

  # Get sorted thresholds
  thresholds <- sort(as.numeric(names(persistence_data)))

  if (length(thresholds) < 2) {
    warning("Less than 2 thresholds available for AUC calculation")
    return(data.frame(
      Node = all_nodes,
      Eigenvector_AUC = rep(0, length(all_nodes)),
      Strength_AUC = rep(0, length(all_nodes)),
      stringsAsFactors = FALSE
    ))
  }

  # Initialize results
  results <- data.frame(
    Node = all_nodes,
    Eigenvector_AUC = rep(0, length(all_nodes)),
    Strength_AUC = rep(0, length(all_nodes)),
    stringsAsFactors = FALSE
  )

  # For each node, collect values across thresholds and compute AUC
  for (i in seq_along(all_nodes)) {
    node <- all_nodes[i]

    eigen_values <- numeric(length(thresholds))
    strength_values <- numeric(length(thresholds))

    # Collect eigenvector and strength at each threshold
    for (j in seq_along(thresholds)) {
      thresh <- thresholds[j]
      thresh_key <- as.character(thresh)

      if (!is.null(persistence_data[[thresh_key]]) &&
          !is.null(persistence_data[[thresh_key]]$nodes)) {

        node_data <- persistence_data[[thresh_key]]$nodes
        node_row <- node_data[node_data$Node == node, ]

        if (nrow(node_row) > 0) {
          if ("Eigenvector" %in% names(node_row)) {
            eigen_val <- node_row$Eigenvector[1]
            eigen_values[j] <- ifelse(is.finite(eigen_val), eigen_val, 0)
          }
          if ("Strength" %in% names(node_row)) {
            strength_val <- node_row$Strength[1]
            strength_values[j] <- ifelse(is.finite(strength_val), strength_val, 0)
          }
        }
      }
    }

    # Compute AUC using trapezoidal rule
    if (length(thresholds) >= 2) {
      delta_x <- diff(thresholds)
      results$Eigenvector_AUC[i] <- sum(delta_x * (eigen_values[-length(eigen_values)] + eigen_values[-1]) / 2)
      results$Strength_AUC[i] <- sum(delta_x * (strength_values[-length(strength_values)] + strength_values[-1]) / 2)
    }
  }

  return(results)
}


#' Compute Rank-Based Consensus for Eigenvector and Node Strength
#'
#' Computes consensus metrics by ranking nodes within each method (Percolation,
#' Weighted, Persistence AUC), averaging ranks across methods, and normalizing
#' to a 0-1 scale where 1 = most important.
#'
#' @param percolation_metrics Data frame with Node and Eigenvector columns
#' @param weighted_metrics Data frame with Node and Weighted_Eigenvector, Node_Strength columns
#' @param persistence_auc_metrics Data frame with Node, Eigenvector_AUC, Strength_AUC columns
#' @return Data frame with all intermediate ranks and final consensus scores
compute_rank_based_consensus <- function(percolation_metrics, weighted_metrics, persistence_auc_metrics) {

  all_nodes <- unique(c(
    as.character(percolation_metrics$Node),
    as.character(weighted_metrics$Node),
    as.character(persistence_auc_metrics$Node)
  ))

  n_nodes <- length(all_nodes)

  results <- data.frame(Node = all_nodes, stringsAsFactors = FALSE)

  # EIGENVECTOR CONSENSUS
  perc_eigen <- sapply(all_nodes, function(node) {
    row <- percolation_metrics[percolation_metrics$Node == node, ]
    if (nrow(row) > 0 && "Eigenvector" %in% names(row)) {
      val <- row$Eigenvector[1]
      return(ifelse(is.finite(val), val, 0))
    }
    return(0)
  })

  weighted_eigen <- sapply(all_nodes, function(node) {
    row <- weighted_metrics[weighted_metrics$Node == node, ]
    if (nrow(row) > 0 && "Weighted_Eigenvector" %in% names(row)) {
      val <- row$Weighted_Eigenvector[1]
      return(ifelse(is.finite(val), val, 0))
    }
    return(0)
  })

  pers_eigen <- sapply(all_nodes, function(node) {
    row <- persistence_auc_metrics[persistence_auc_metrics$Node == node, ]
    if (nrow(row) > 0 && "Eigenvector_AUC" %in% names(row)) {
      val <- row$Eigenvector_AUC[1]
      return(ifelse(is.finite(val), val, 0))
    }
    return(0)
  })

  perc_eigen_rank <- rank(-perc_eigen, ties.method = "min")
  weighted_eigen_rank <- rank(-weighted_eigen, ties.method = "min")
  pers_eigen_rank <- rank(-pers_eigen, ties.method = "min")
  avg_eigen_rank <- (perc_eigen_rank + weighted_eigen_rank + pers_eigen_rank) / 3

  # Min-max normalization: best node (lowest avg rank) = 1, worst = 0
  min_eigen_rank <- min(avg_eigen_rank, na.rm = TRUE)
  max_eigen_rank <- max(avg_eigen_rank, na.rm = TRUE)
  if (max_eigen_rank > min_eigen_rank) {
    consensus_eigenvector <- (max_eigen_rank - avg_eigen_rank) / (max_eigen_rank - min_eigen_rank)
  } else {
    consensus_eigenvector <- rep(1, length(avg_eigen_rank))  # All tied
  }

  results$Percolation_Eigenvector <- perc_eigen
  results$Percolation_Eigen_Rank <- perc_eigen_rank
  results$Weighted_Eigenvector <- weighted_eigen
  results$Weighted_Eigen_Rank <- weighted_eigen_rank
  results$Persistence_Eigenvector_AUC <- pers_eigen
  results$Persistence_Eigen_Rank <- pers_eigen_rank
  results$Avg_Eigen_Rank <- avg_eigen_rank
  results$Consensus_Eigenvector <- consensus_eigenvector

  # NODE STRENGTH CONSENSUS
  perc_strength <- sapply(all_nodes, function(node) {
    row <- percolation_metrics[percolation_metrics$Node == node, ]
    if (nrow(row) > 0 && "Strength" %in% names(row)) {
      val <- row$Strength[1]
      return(ifelse(is.finite(val), val, 0))
    }
    return(0)
  })

  weighted_strength <- sapply(all_nodes, function(node) {
    row <- weighted_metrics[weighted_metrics$Node == node, ]
    if (nrow(row) > 0 && "Node_Strength" %in% names(row)) {
      val <- row$Node_Strength[1]
      return(ifelse(is.finite(val), val, 0))
    }
    return(0)
  })

  pers_strength <- sapply(all_nodes, function(node) {
    row <- persistence_auc_metrics[persistence_auc_metrics$Node == node, ]
    if (nrow(row) > 0 && "Strength_AUC" %in% names(row)) {
      val <- row$Strength_AUC[1]
      return(ifelse(is.finite(val), val, 0))
    }
    return(0)
  })

  perc_strength_rank <- rank(-perc_strength, ties.method = "min")
  weighted_strength_rank <- rank(-weighted_strength, ties.method = "min")
  pers_strength_rank <- rank(-pers_strength, ties.method = "min")
  avg_strength_rank <- (perc_strength_rank + weighted_strength_rank + pers_strength_rank) / 3

  # Min-max normalization: best node (lowest avg rank) = 1, worst = 0
  min_strength_rank <- min(avg_strength_rank, na.rm = TRUE)
  max_strength_rank <- max(avg_strength_rank, na.rm = TRUE)
  if (max_strength_rank > min_strength_rank) {
    consensus_strength <- (max_strength_rank - avg_strength_rank) / (max_strength_rank - min_strength_rank)
  } else {
    consensus_strength <- rep(1, length(avg_strength_rank))  # All tied
  }

  results$Percolation_Strength <- perc_strength
  results$Percolation_Strength_Rank <- perc_strength_rank
  results$Weighted_Strength <- weighted_strength
  results$Weighted_Strength_Rank <- weighted_strength_rank
  results$Persistence_Strength_AUC <- pers_strength
  results$Persistence_Strength_Rank <- pers_strength_rank
  results$Avg_Strength_Rank <- avg_strength_rank
  results$Consensus_NodeStrength <- consensus_strength

  results <- results[order(-results$Consensus_Eigenvector), ]
  rownames(results) <- NULL

  return(results)
}


#' Compute Comprehensive Rank-Based Consensus Across All Methods
#'
#' Calculates consensus metrics by ranking nodes within each correlation method
#' AND analytical approach (Percolation, Weighted, Persistence AUC), then
#' averaging ranks across ALL method-approach combinations.
#'
#' @param group The group name to analyze
#' @param method_percolation_results List of percolation results by method
#' @param method_weighted_results List of weighted results by method
#' @param persistence_results List of persistence results by method and group
#' @param methods Character vector of correlation methods to include
#' @return Data frame with consensus scores and all intermediate ranks
compute_comprehensive_rank_consensus <- function(group,
                                                  method_percolation_results,
                                                  method_weighted_results,
                                                  persistence_results,
                                                  methods = c("pearson", "spearman", "biweight", "shrinkage", "partial")) {

  # Collect all nodes from all methods
  all_nodes <- character(0)

  for (method in methods) {
    # From percolation
    if (!is.null(method_percolation_results[[method]]$node_metrics)) {
      group_data <- method_percolation_results[[method]]$node_metrics
      group_data <- group_data[group_data$Group == group, ]
      if (nrow(group_data) > 0) {
        all_nodes <- union(all_nodes, as.character(group_data$Node))
      }
    }

    # From weighted (FIX: access weighted_eigenvector and filter by group)
    weighted_all <- method_weighted_results[[method]]$weighted_eigenvector
    if (!is.null(weighted_all) && "Group" %in% names(weighted_all)) {
      weighted_data <- weighted_all[weighted_all$Group == group, ]
      if (nrow(weighted_data) > 0 && "Node" %in% names(weighted_data)) {
        all_nodes <- union(all_nodes, as.character(weighted_data$Node))
      }
    }
  }

  if (length(all_nodes) == 0) {
    return(NULL)
  }

  n_nodes <- length(all_nodes)

  # Storage for all ranks (each column = one method-approach combination)
  eigen_ranks_list <- list()
  strength_ranks_list <- list()
  method_approach_names <- character(0)
  n_combinations <- 0

  for (method in methods) {
    # === PERCOLATION ===
    perc_data <- method_percolation_results[[method]]$node_metrics
    if (!is.null(perc_data)) {
      perc_group <- perc_data[perc_data$Group == group, ]
      if (nrow(perc_group) > 0 && "Eigenvector" %in% names(perc_group) && "Strength" %in% names(perc_group)) {
        n_combinations <- n_combinations + 1
        combo_name <- paste0(method, "_percolation")
        method_approach_names <- c(method_approach_names, combo_name)

        # Get values for all nodes (0 if missing)
        eigen_vals <- sapply(all_nodes, function(node) {
          row <- perc_group[perc_group$Node == node, ]
          if (nrow(row) > 0 && is.finite(row$Eigenvector[1])) row$Eigenvector[1] else 0
        })
        strength_vals <- sapply(all_nodes, function(node) {
          row <- perc_group[perc_group$Node == node, ]
          if (nrow(row) > 0 && is.finite(row$Strength[1])) row$Strength[1] else 0
        })

        # Rank (1 = highest value = most important)
        eigen_ranks_list[[combo_name]] <- rank(-eigen_vals, ties.method = "min")
        strength_ranks_list[[combo_name]] <- rank(-strength_vals, ties.method = "min")
      }
    }

    # === WEIGHTED ===
    # Access weighted_eigenvector df and filter by group (all groups are combined)
    weighted_all <- method_weighted_results[[method]]$weighted_eigenvector
    weighted_data <- if (!is.null(weighted_all) && "Group" %in% names(weighted_all)) {
      weighted_all[weighted_all$Group == group, ]
    } else {
      NULL
    }
    if (!is.null(weighted_data) && nrow(weighted_data) > 0) {
      if ("Weighted_Eigenvector" %in% names(weighted_data) && "Node_Strength" %in% names(weighted_data)) {
        n_combinations <- n_combinations + 1
        combo_name <- paste0(method, "_weighted")
        method_approach_names <- c(method_approach_names, combo_name)

        eigen_vals <- sapply(all_nodes, function(node) {
          row <- weighted_data[weighted_data$Node == node, ]
          if (nrow(row) > 0 && is.finite(row$Weighted_Eigenvector[1])) row$Weighted_Eigenvector[1] else 0
        })
        strength_vals <- sapply(all_nodes, function(node) {
          row <- weighted_data[weighted_data$Node == node, ]
          if (nrow(row) > 0 && is.finite(row$Node_Strength[1])) row$Node_Strength[1] else 0
        })

        eigen_ranks_list[[combo_name]] <- rank(-eigen_vals, ties.method = "min")
        strength_ranks_list[[combo_name]] <- rank(-strength_vals, ties.method = "min")
      }
    }

    # === PERSISTENCE AUC ===
    pers_check <- persistence_results[[method]][[group]]$persistence_data

    if (!is.null(pers_check)) {
      persistence_data <- pers_check
      persistence_auc <- compute_persistence_auc_metrics(persistence_data, all_nodes)

      if (!is.null(persistence_auc) && nrow(persistence_auc) > 0) {
        n_combinations <- n_combinations + 1
        combo_name <- paste0(method, "_persistence")
        method_approach_names <- c(method_approach_names, combo_name)

        eigen_vals <- sapply(all_nodes, function(node) {
          row <- persistence_auc[persistence_auc$Node == node, ]
          if (nrow(row) > 0 && is.finite(row$Eigenvector_AUC[1])) row$Eigenvector_AUC[1] else 0
        })
        strength_vals <- sapply(all_nodes, function(node) {
          row <- persistence_auc[persistence_auc$Node == node, ]
          if (nrow(row) > 0 && is.finite(row$Strength_AUC[1])) row$Strength_AUC[1] else 0
        })

        eigen_ranks_list[[combo_name]] <- rank(-eigen_vals, ties.method = "min")
        strength_ranks_list[[combo_name]] <- rank(-strength_vals, ties.method = "min")
      }
    }
  }

  if (n_combinations == 0) {
    return(NULL)
  }

  # Compute average ranks across all method-approach combinations
  eigen_rank_matrix <- do.call(cbind, eigen_ranks_list)
  strength_rank_matrix <- do.call(cbind, strength_ranks_list)

  avg_eigen_rank <- rowMeans(eigen_rank_matrix, na.rm = TRUE)
  avg_strength_rank <- rowMeans(strength_rank_matrix, na.rm = TRUE)

  # Min-max normalization: best node (lowest avg rank) = 1, worst = 0
  min_eigen_rank <- min(avg_eigen_rank, na.rm = TRUE)
  max_eigen_rank <- max(avg_eigen_rank, na.rm = TRUE)
  if (max_eigen_rank > min_eigen_rank) {
    consensus_eigenvector <- (max_eigen_rank - avg_eigen_rank) / (max_eigen_rank - min_eigen_rank)
  } else {
    consensus_eigenvector <- rep(1, n_nodes)
  }

  min_strength_rank <- min(avg_strength_rank, na.rm = TRUE)
  max_strength_rank <- max(avg_strength_rank, na.rm = TRUE)
  if (max_strength_rank > min_strength_rank) {
    consensus_strength <- (max_strength_rank - avg_strength_rank) / (max_strength_rank - min_strength_rank)
  } else {
    consensus_strength <- rep(1, n_nodes)
  }

  # Build results data frame
  results <- data.frame(
    Node = all_nodes,
    Avg_Eigen_Rank = avg_eigen_rank,
    Consensus_Eigenvector = consensus_eigenvector,
    Avg_Strength_Rank = avg_strength_rank,
    Consensus_NodeStrength = consensus_strength,
    N_Methods = n_combinations,
    stringsAsFactors = FALSE
  )

  results <- results[order(-results$Consensus_Eigenvector), ]
  rownames(results) <- NULL

  return(results)
}