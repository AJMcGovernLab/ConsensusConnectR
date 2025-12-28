# Consensus Visualization Functions
# Visualizations for comprehensive consensus analytics across all three approaches

# ============================================================================
# Visualization 1: Comprehensive Consensus Heatmap
# ============================================================================

render_comprehensive_consensus_heatmap <- function(consensus_results,
                                                  brain_areas = NULL,
                                                  area_colors = NULL,
                                                  top_n = 30) {

  if(is.null(consensus_results) || is.null(consensus_results$consensus_scores)) {
    plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
    text(1, 1, "No consensus data available", cex = 1.2, col = "gray")
    return()
  }

  scores <- consensus_results$consensus_scores

  if(nrow(scores) == 0) {
    plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
    text(1, 1, "No nodes found in consensus analysis", cex = 1.2, col = "gray")
    return()
  }

  top_nodes <- head(scores$Node, min(top_n, nrow(scores)))

  # Create matrix: nodes × approaches
  plot_matrix <- as.matrix(scores[scores$Node %in% top_nodes,
                                 c("WeightedScore", "PercolationScore", "PersistenceScore")])
  rownames(plot_matrix) <- top_nodes
  colnames(plot_matrix) <- c("Weighted", "Percolation", "Persistence")

  par(mar = c(8, 10, 4, 8))

  # Color palette: white (0) to dark red (1)
  col_palette <- colorRampPalette(c("white", "#fee5d9", "#fcae91", "#fb6a4a", "#de2d26", "#a50f15"))(100)

  image(1:ncol(plot_matrix), 1:nrow(plot_matrix), t(plot_matrix),
        col = col_palette,
        xlab = "", ylab = "",
        main = paste("Consensus Hub Heatmap:", consensus_results$group),
        axes = FALSE)

  axis(1, at = 1:ncol(plot_matrix), labels = colnames(plot_matrix), las = 2, cex.axis = 0.9)
  axis(2, at = 1:nrow(plot_matrix), labels = rownames(plot_matrix), las = 2, cex.axis = 0.8)

  # Add text values
  for(i in 1:nrow(plot_matrix)) {
    for(j in 1:ncol(plot_matrix)) {
      text(j, i, sprintf("%.2f", plot_matrix[i, j]),
           col = ifelse(plot_matrix[i, j] > 0.5, "white", "black"),
           cex = 0.7)
    }
  }

  # Legend
  legend("right", legend = c("1.0", "0.75", "0.5", "0.25", "0"),
         fill = colorRampPalette(col_palette)(5),
         title = "Score",
         xpd = TRUE, inset = c(-0.2, 0), cex = 0.8)
}


# ============================================================================
# Visualization 2: Three-Way Hub Venn Diagram
# ============================================================================

render_three_way_hub_venn <- function(consensus_results,
                                     method_threshold = 0.6) {

  if(is.null(consensus_results) || is.null(consensus_results$consensus_scores)) {
    plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
    text(1, 1, "No consensus data available", cex = 1.2, col = "gray")
    return()
  }

  scores <- consensus_results$consensus_scores

  if(nrow(scores) == 0) {
    plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
    text(1, 1, "No nodes found in consensus analysis", cex = 1.2, col = "gray")
    return()
  }

  # Define hub sets (≥60% method agreement)
  weighted_hubs <- scores$Node[scores$WeightedScore >= method_threshold]
  percolation_hubs <- scores$Node[scores$PercolationScore >= method_threshold]
  persistence_hubs <- scores$Node[scores$PersistenceScore >= method_threshold]

  # Calculate overlaps
  w_only <- setdiff(setdiff(weighted_hubs, percolation_hubs), persistence_hubs)
  p_only <- setdiff(setdiff(percolation_hubs, weighted_hubs), persistence_hubs)
  pers_only <- setdiff(setdiff(persistence_hubs, weighted_hubs), percolation_hubs)

  w_p <- setdiff(intersect(weighted_hubs, percolation_hubs), persistence_hubs)
  w_pers <- setdiff(intersect(weighted_hubs, persistence_hubs), percolation_hubs)
  p_pers <- setdiff(intersect(percolation_hubs, persistence_hubs), weighted_hubs)

  all_three <- intersect(intersect(weighted_hubs, percolation_hubs), persistence_hubs)

  # Create Venn diagram manually
  par(mar = c(2, 2, 4, 2))
  plot(1, type = "n", xlim = c(0, 10), ylim = c(0, 10), axes = FALSE, xlab = "", ylab = "",
       main = paste("Hub Overlap Across Approaches\n", consensus_results$group))

  # Draw circles
  if(requireNamespace("graphics", quietly = TRUE)) {
    symbols(c(3.5, 6.5, 5), c(6, 6, 4), circles = c(2.5, 2.5, 2.5),
            inches = FALSE, add = TRUE, fg = c("#3498db", "#e74c3c", "#2ecc71"), lwd = 3)
  }

  # Add labels
  text(2, 8, "Weighted", col = "#3498db", font = 2, cex = 1.2)
  text(8, 8, "Percolation", col = "#e74c3c", font = 2, cex = 1.2)
  text(5, 1.5, "Persistence", col = "#2ecc71", font = 2, cex = 1.2)

  # Add counts
  text(2.5, 6, length(w_only), cex = 1.2, font = 2)
  text(7.5, 6, length(p_only), cex = 1.2, font = 2)
  text(5, 3, length(pers_only), cex = 1.2, font = 2)
  text(5, 6.5, length(w_p), cex = 1.2, font = 2)
  text(3.5, 4.5, length(w_pers), cex = 1.2, font = 2)
  text(6.5, 4.5, length(p_pers), cex = 1.2, font = 2)
  text(5, 5, length(all_three), cex = 1.5, font = 2, col = "darkred")

  # Summary text
  text(5, 0.5,
       sprintf("All Three Agree: %d hubs | Two Agree: %d hubs | One Only: %d hubs",
               length(all_three),
               length(w_p) + length(w_pers) + length(p_pers),
               length(w_only) + length(p_only) + length(pers_only)),
       cex = 0.9, col = "gray30")
}


# ============================================================================
# Visualization 3: Network Similarity Heatmap
# ============================================================================

render_network_similarity_heatmap <- function(similarity_results,
                                             metric = "JaccardIndex") {

  if(is.null(similarity_results) || is.null(similarity_results$cross_method_average)) {
    plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
    text(1, 1, "No similarity data available", cex = 1.2, col = "gray")
    return()
  }

  # Extract metric from cross-method average
  avg_sim <- similarity_results$cross_method_average

  if(nrow(avg_sim) == 0) {
    plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
    text(1, 1, "No pairwise group comparisons available", cex = 1.2, col = "gray")
    return()
  }

  # Get unique groups
  all_groups <- unique(c(avg_sim$Group1, avg_sim$Group2))
  n_groups <- length(all_groups)

  if(n_groups < 2) {
    plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
    text(1, 1, "Need at least 2 groups for similarity", cex = 1.2, col = "gray")
    return()
  }

  # Create similarity matrix
  sim_matrix <- matrix(1, nrow = n_groups, ncol = n_groups)
  rownames(sim_matrix) <- all_groups
  colnames(sim_matrix) <- all_groups

  metric_col <- switch(metric,
                      "JaccardIndex" = "MeanJaccard",
                      "DiceCoefficient" = "MeanDice",
                      "DegreeCorrelation" = "MeanDegreeCorr",
                      "MeanJaccard")

  for(i in 1:nrow(avg_sim)) {
    g1_idx <- which(all_groups == avg_sim$Group1[i])
    g2_idx <- which(all_groups == avg_sim$Group2[i])

    value <- avg_sim[[metric_col]][i]
    if(!is.na(value)) {
      sim_matrix[g1_idx, g2_idx] <- value
      sim_matrix[g2_idx, g1_idx] <- value
    }
  }

  par(mar = c(8, 8, 4, 6))

  col_palette <- colorRampPalette(c("#d7191c", "#fdae61", "#ffffbf", "#abd9e9", "#2c7bb6"))(100)

  image(1:n_groups, 1:n_groups, sim_matrix,
        col = col_palette,
        xlab = "", ylab = "",
        main = paste("Network Similarity:", metric),
        axes = FALSE)

  axis(1, at = 1:n_groups, labels = all_groups, las = 2, cex.axis = 0.9)
  axis(2, at = 1:n_groups, labels = all_groups, las = 2, cex.axis = 0.9)

  # Add text values
  for(i in 1:n_groups) {
    for(j in 1:n_groups) {
      if(i != j) {
        text(j, i, sprintf("%.2f", sim_matrix[i, j]), cex = 0.7)
      }
    }
  }

  legend("right", legend = sprintf("%.2f", seq(0, 1, by = 0.25)),
         fill = colorRampPalette(col_palette)(5),
         title = metric,
         xpd = TRUE, inset = c(-0.2, 0), cex = 0.8)
}


# ============================================================================
# Visualization 4: Consensus Hub Ranking
# ============================================================================

render_consensus_hub_ranking <- function(consensus_results,
                                        top_n = 20,
                                        brain_areas = NULL,
                                        area_colors = NULL) {

  if(is.null(consensus_results) || is.null(consensus_results$consensus_scores)) {
    plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
    text(1, 1, "No consensus data available", cex = 1.2, col = "gray")
    return()
  }

  scores <- consensus_results$consensus_scores

  if(nrow(scores) == 0) {
    plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
    text(1, 1, "No nodes found in consensus analysis", cex = 1.2, col = "gray")
    return()
  }

  scores <- head(scores, min(top_n, nrow(scores)))

  par(mar = c(8, 5, 4, 2))

  # Color by robustness category
  robustness_colors <- c(
    "Highly Stable" = "#27ae60",
    "Stable" = "#3498db",
    "Moderately Stable" = "#f39c12",
    "Unstable" = "#e74c3c"
  )

  bar_colors <- robustness_colors[as.character(scores$RobustnessCategory)]

  bp <- barplot(scores$ConsensusScore,
                names.arg = scores$Node,
                col = bar_colors,
                border = NA,
                las = 2,
                ylim = c(0, 1.1),
                main = paste("Top Consensus Hubs:", consensus_results$group),
                ylab = "Consensus Score (0-1)")

  # Add error bars (SD across approaches)
  approach_sd <- apply(scores[, c("WeightedScore", "PercolationScore", "PersistenceScore")],
                      1, sd, na.rm = TRUE)

  arrows(bp, pmax(0, scores$ConsensusScore - approach_sd),
         bp, pmin(1, scores$ConsensusScore + approach_sd),
         angle = 90, code = 3, length = 0.05, lwd = 1.5)

  grid(nx = NA, ny = NULL)

  # Legend
  legend("topright",
         legend = c("Highly Stable (≥80%)", "Stable (60-79%)",
                   "Moderately Stable (40-59%)", "Unstable (<40%)"),
         fill = robustness_colors,
         cex = 0.8,
         title = "Robustness")

  # Add marker for "all three agree"
  if(any(scores$AllThreeAgree)) {
    points(bp[scores$AllThreeAgree], scores$ConsensusScore[scores$AllThreeAgree],
           pch = 8, cex = 2, col = "darkred", lwd = 2)

    text(bp[1], 1.05, "★ = All 3 approaches agree", pos = 4, cex = 0.8, col = "darkred")
  }
}


# ============================================================================
# Visualization 5: Method Variance Barplot
# ============================================================================

render_method_variance_barplot <- function(aggregated_metrics,
                                          top_n = 10) {

  if(is.null(aggregated_metrics) || nrow(aggregated_metrics) == 0) {
    plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
    text(1, 1, "No aggregated metrics available", cex = 1.2, col = "gray")
    return()
  }

  # Sort by CV (coefficient of variation)
  sorted <- aggregated_metrics[order(aggregated_metrics$CV), ]

  # Filter out NA or Inf values
  sorted <- sorted[is.finite(sorted$CV), ]

  if(nrow(sorted) == 0) {
    plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
    text(1, 1, "No valid CV values available", cex = 1.2, col = "gray")
    return()
  }

  n_show <- min(top_n, nrow(sorted))

  top_robust <- head(sorted, n_show)  # Lowest CV = most robust
  top_variable <- tail(sorted, n_show)  # Highest CV = most variable

  par(mfrow = c(1, 2), mar = c(8, 5, 4, 2))

  # Most Robust Metrics
  if(nrow(top_robust) > 0) {
    barplot(top_robust$CV,
            names.arg = top_robust$Metric,
            col = "#27ae60",
            border = NA,
            las = 2,
            main = "Most Robust Metrics\n(Low Cross-Method Variance)",
            ylab = "Coefficient of Variation",
            ylim = c(0, max(aggregated_metrics$CV, na.rm = TRUE)))
    grid(nx = NA, ny = NULL)
  }

  # Most Variable Metrics
  if(nrow(top_variable) > 0) {
    barplot(top_variable$CV,
            names.arg = top_variable$Metric,
            col = "#e74c3c",
            border = NA,
            las = 2,
            main = "Most Variable Metrics\n(High Cross-Method Variance)",
            ylab = "Coefficient of Variation",
            ylim = c(0, max(aggregated_metrics$CV, na.rm = TRUE)))
    grid(nx = NA, ny = NULL)
  }
}


# ============================================================================
# Helper Function: Filter Common Methods
# ============================================================================

filter_common_methods <- function(comprehensive_consensus, all_methods = c("pearson", "spearman", "biweight", "shrinkage", "partial")) {
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


# ============================================================================
# Additional Consensus Visualizations for Download System
# ============================================================================

# Plot 1: Consensus Node Metrics Across Methods (Summary Tab A)
render_consensus_node_metrics_plot <- function(comprehensive_consensus,
                                               method_percolation_results,
                                               brain_areas = NULL,
                                               area_colors = NULL,
                                               group_colors = NULL) {

  all_groups <- names(comprehensive_consensus)
  if(length(all_groups) == 0) {
    plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
    text(1, 1, "No consensus data available", cex = 1.2, col = "gray")
    return()
  }

  n_groups <- length(all_groups)
  par(mfrow = c(2, 2), mar = c(5, 5, 4, 2))

  methods <- filter_common_methods(comprehensive_consensus)

  if(length(methods) == 0) {
    plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
    text(1, 1, "No common methods across all groups", cex = 1.2, col = "gray")
    return()
  }

  for(group in all_groups[1:min(4, n_groups)]) {
    all_nodes <- character(0)
    strength_by_method <- list()
    eigenvector_by_method <- list()
    n_methods_with_data <- 0

    for(method in methods) {
      method_data <- method_percolation_results[[method]]

      if(!is.null(method_data) && !is.null(method_data$node_metrics)) {
        group_data <- method_data$node_metrics[method_data$node_metrics$Group == group, ]

        if(nrow(group_data) > 0 && "Strength" %in% names(group_data) && "Eigenvector" %in% names(group_data)) {
          n_methods_with_data <- n_methods_with_data + 1

          for(i in 1:nrow(group_data)) {
            node <- group_data$Node[i]
            if(!(node %in% all_nodes)) {
              all_nodes <- c(all_nodes, node)
              strength_by_method[[node]] <- c()
              eigenvector_by_method[[node]] <- c()
            }
            strength_by_method[[node]] <- c(strength_by_method[[node]], group_data$Strength[i])
            eigenvector_by_method[[node]] <- c(eigenvector_by_method[[node]], group_data$Eigenvector[i])
          }
        }
      }
    }

    if(length(all_nodes) == 0 || n_methods_with_data == 0) {
      plot(1, type = "n", axes = FALSE, main = group)
      text(1, 1, "No data available", cex = 1.0)
      next
    }

    consensus_strength <- sapply(strength_by_method, function(x) if(length(x) > 0) mean(x, na.rm = TRUE) else NA)
    consensus_eigenvector <- sapply(eigenvector_by_method, function(x) if(length(x) > 0) mean(x, na.rm = TRUE) else NA)

    valid_idx <- !is.na(consensus_strength)
    strength_rank <- rep(NA, length(all_nodes))
    names(strength_rank) <- all_nodes
    if(sum(valid_idx) > 0) {
      temp_rank <- rank(-consensus_strength[valid_idx], ties.method = "average")
      max_rank <- max(temp_rank)
      strength_rank[valid_idx] <- max_rank - temp_rank
    }

    valid_idx <- !is.na(consensus_eigenvector)
    eigenvector_rank <- rep(NA, length(all_nodes))
    names(eigenvector_rank) <- all_nodes
    if(sum(valid_idx) > 0) {
      temp_rank <- rank(-consensus_eigenvector[valid_idx], ties.method = "average")
      max_rank <- max(temp_rank)
      eigenvector_rank[valid_idx] <- max_rank - temp_rank
    }

    plot_colors <- rep("steelblue", length(all_nodes))
    names(plot_colors) <- all_nodes
    if(!is.null(brain_areas) && !is.null(area_colors)) {
      for(node in all_nodes) {
        for(area_name in names(brain_areas)) {
          if(node %in% brain_areas[[area_name]]) {
            if(area_name %in% names(area_colors)) {
              plot_colors[node] <- area_colors[[area_name]]
            }
            break
          }
        }
      }
    }

    valid_idx <- !is.na(strength_rank) & !is.na(eigenvector_rank)

    if(sum(valid_idx) > 0) {
      valid_nodes <- all_nodes[valid_idx]

      plot(strength_rank[valid_idx], eigenvector_rank[valid_idx],
           pch = 21, cex = 2.2,
           bg = adjustcolor(plot_colors[valid_nodes], alpha.f = 0.6),
           col = adjustcolor(plot_colors[valid_nodes], alpha.f = 0.8),
           lwd = 2,
           xlab = "Consensus Strength Rank (0 = lowest, higher = better)",
           ylab = "Consensus Eigenvector Rank (0 = lowest, higher = better)",
           main = paste("Consensus Across", n_methods_with_data, "Methods -", group),
           xlim = c(0, max(strength_rank, na.rm = TRUE) * 1.05),
           ylim = c(0, max(eigenvector_rank, na.rm = TRUE) * 1.05))

      abline(a = 0, b = 1, col = "red", lty = 2, lwd = 1.5)
      grid(col = "lightgray", lty = "dotted", lwd = 0.5)

      max_s_rank <- max(strength_rank, na.rm = TRUE)
      max_e_rank <- max(eigenvector_rank, na.rm = TRUE)
      top_nodes_idx <- which(strength_rank >= (max_s_rank - 15) | eigenvector_rank >= (max_e_rank - 15))
      top_nodes_idx <- top_nodes_idx[!is.na(strength_rank[top_nodes_idx]) & !is.na(eigenvector_rank[top_nodes_idx])]

      for(i in top_nodes_idx) {
        text(strength_rank[i], eigenvector_rank[i],
             labels = all_nodes[i],
             cex = 0.7, font = 2, col = "black")
      }

      spearman_rho <- cor(strength_rank[valid_idx], eigenvector_rank[valid_idx],
                         method = "spearman", use = "complete.obs")
      legend("bottomright",
             legend = paste("Spearman ρ =", round(spearman_rho, 3)),
             bty = "n", cex = 0.9, bg = "white", box.col = "darkgray")
    } else {
      plot(1, type = "n", axes = FALSE, main = group)
      text(1, 1, "No valid data", cex = 1.0)
    }
  }
}

# Plot 2: Consensus Regional Plot (Summary Tab D)
render_consensus_regional_plot <- function(comprehensive_consensus,
                                           brain_areas,
                                           area_colors = NULL,
                                           group_colors = NULL) {

  all_groups <- names(comprehensive_consensus)
  if(length(all_groups) == 0 || is.null(brain_areas)) {
    plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
    text(1, 1, "No consensus data available", cex = 1.2, col = "gray")
    return()
  }

  methods <- filter_common_methods(comprehensive_consensus)

  if(length(methods) == 0) {
    plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
    text(1, 1, "No common methods across all groups", cex = 1.2, col = "gray")
    return()
  }

  region_names <- names(brain_areas)
  n_regions <- length(region_names)
  n_groups <- length(all_groups)

  eigenvector_matrix <- matrix(NA, nrow = n_groups, ncol = n_regions)
  eigenvector_sd_matrix <- matrix(NA, nrow = n_groups, ncol = n_regions)
  rownames(eigenvector_matrix) <- all_groups
  colnames(eigenvector_matrix) <- region_names

  for(i in seq_along(all_groups)) {
    group <- all_groups[i]
    std_metrics <- comprehensive_consensus[[group]]$standardized_metrics

    all_nodes <- unique(c(
      unlist(lapply(std_metrics$weighted, function(x) if(!is.null(x)) x$Node else character(0))),
      unlist(lapply(std_metrics$percolation, function(x) if(!is.null(x)) x$Node else character(0))),
      unlist(lapply(std_metrics$persistence, function(x) if(!is.null(x)) x$Node else character(0)))
    ))

    consensus_eigenvector <- rep(NA, length(all_nodes))
    names(consensus_eigenvector) <- all_nodes

    for(node in all_nodes) {
      z_values <- c()
      for(method in methods) {
        for(approach in c("weighted", "percolation", "persistence")) {
          approach_data <- std_metrics[[approach]][[method]]
          if(!is.null(approach_data)) {
            node_idx <- which(approach_data$Node == node)
            if(length(node_idx) > 0) {
              z_values <- c(z_values, approach_data$Eigenvector_Z[node_idx[1]])
            }
          }
        }
      }
      if(length(z_values) > 0) {
        consensus_eigenvector[node] <- mean(z_values, na.rm = TRUE)
      }
    }

    consensus_rank <- rank(-consensus_eigenvector, ties.method = "average", na.last = "keep")
    names(consensus_rank) <- all_nodes

    for(j in seq_along(region_names)) {
      region_name <- region_names[j]
      region_nodes <- brain_areas[[region_name]]
      region_mask <- all_nodes %in% region_nodes
      region_ranks <- consensus_rank[region_mask]
      region_ranks <- region_ranks[!is.na(region_ranks)]

      if(length(region_ranks) > 0) {
        eigenvector_matrix[i, j] <- mean(region_ranks, na.rm = TRUE)
        eigenvector_sd_matrix[i, j] <- sd(region_ranks, na.rm = TRUE)
      }
    }
  }

  if(all(is.na(eigenvector_matrix))) {
    plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
    text(1, 1, "No regional data available", cex = 1.2, col = "gray")
    return()
  }

  max_rank <- max(eigenvector_matrix, na.rm = TRUE)
  eigenvector_matrix_inverted <- max_rank - eigenvector_matrix + 1

  max_val_inv <- max(eigenvector_matrix_inverted + eigenvector_sd_matrix, na.rm = TRUE)
  min_val_inv <- min(eigenvector_matrix_inverted - eigenvector_sd_matrix, na.rm = TRUE)
  if(is.infinite(max_val_inv) || is.na(max_val_inv)) max_val_inv <- 1
  if(is.infinite(min_val_inv) || is.na(min_val_inv)) min_val_inv <- 0

  grp_colors <- sapply(all_groups, function(g) {
    if(!is.null(group_colors[[g]])) {
      return(group_colors[[g]])
    } else {
      return("#3498db")
    }
  })

  par(mar = c(10, 5, 4, 2))

  bp <- barplot(eigenvector_matrix_inverted,
                beside = TRUE,
                names.arg = region_names,
                main = "Regional Consensus Eigenvector Importance\nAcross All Methods & Approaches",
                ylab = "Consensus Importance Score (higher = more important)",
                col = grp_colors,
                border = grp_colors,
                las = 2,
                ylim = c(min_val_inv - 0.2, max_val_inv * 1.2))
  grid(nx = NA, ny = NULL, col = "gray90")

  for(i in 1:n_groups) {
    for(j in 1:n_regions) {
      if(!is.na(eigenvector_matrix_inverted[i, j]) && !is.na(eigenvector_sd_matrix[i, j])) {
        x_pos <- bp[i, j]
        y_val <- eigenvector_matrix_inverted[i, j]
        y_sd <- eigenvector_sd_matrix[i, j]
        segments(x_pos, y_val - y_sd, x_pos, y_val + y_sd, lwd = 1.5)
        segments(x_pos - 0.1, y_val - y_sd, x_pos + 0.1, y_val - y_sd, lwd = 1.5)
        segments(x_pos - 0.1, y_val + y_sd, x_pos + 0.1, y_val + y_sd, lwd = 1.5)
      }
    }
  }

  legend("topright", legend = all_groups, fill = grp_colors, bty = "n", cex = 0.9)
}


# Plot 3: Consensus Subregional Plot (Summary Tab D - Individual Nodes)
render_consensus_subregional_plot <- function(comprehensive_consensus,
                                              brain_areas,
                                              area_colors = NULL,
                                              group_colors = NULL) {

  all_groups <- names(comprehensive_consensus)
  if(length(all_groups) == 0 || is.null(brain_areas)) {
    plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
    text(1, 1, "No consensus data available", cex = 1.2, col = "gray")
    return()
  }

  methods <- filter_common_methods(comprehensive_consensus)

  if(length(methods) == 0) {
    plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
    text(1, 1, "No common methods across all groups", cex = 1.2, col = "gray")
    return()
  }

  region_names <- names(brain_areas)
  n_regions <- length(region_names)

  all_consensus_data <- list()

  for(group in all_groups) {
    std_metrics <- comprehensive_consensus[[group]]$standardized_metrics

    all_nodes <- unique(c(
      unlist(lapply(std_metrics$weighted, function(x) if(!is.null(x)) x$Node else character(0))),
      unlist(lapply(std_metrics$percolation, function(x) if(!is.null(x)) x$Node else character(0))),
      unlist(lapply(std_metrics$persistence, function(x) if(!is.null(x)) x$Node else character(0)))
    ))

    consensus_eigenvector <- rep(NA, length(all_nodes))
    names(consensus_eigenvector) <- all_nodes

    for(node in all_nodes) {
      z_values <- c()
      for(method in methods) {
        for(approach in c("weighted", "percolation", "persistence")) {
          approach_data <- std_metrics[[approach]][[method]]
          if(!is.null(approach_data)) {
            node_idx <- which(approach_data$Node == node)
            if(length(node_idx) > 0) {
              z_values <- c(z_values, approach_data$Eigenvector_Z[node_idx[1]])
            }
          }
        }
      }
      if(length(z_values) > 0) {
        consensus_eigenvector[node] <- mean(z_values, na.rm = TRUE)
      }
    }

    all_consensus_data[[group]] <- consensus_eigenvector
  }

  grp_colors <- sapply(all_groups, function(g) {
    if(!is.null(group_colors[[g]])) {
      return(group_colors[[g]])
    } else {
      return("#3498db")
    }
  })

  par(mfrow = c(n_regions, 1), mar = c(8, 5, 3, 2))

  for(region_name in region_names) {
    region_nodes <- brain_areas[[region_name]]

    region_data <- matrix(NA, nrow = length(region_nodes), ncol = length(all_groups))
    rownames(region_data) <- region_nodes
    colnames(region_data) <- all_groups

    for(i in seq_along(all_groups)) {
      group <- all_groups[i]
      consensus_vec <- all_consensus_data[[group]]
      for(j in seq_along(region_nodes)) {
        node <- region_nodes[j]
        if(node %in% names(consensus_vec)) {
          region_data[j, i] <- consensus_vec[node]
        }
      }
    }

    if(all(is.na(region_data))) {
      plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
      text(1, 1, paste("No data for", region_name), cex = 1, col = "gray")
      next
    }

    bp <- barplot(t(region_data),
                  beside = TRUE,
                  names.arg = region_nodes,
                  main = paste("Region:", region_name),
                  ylab = "Consensus Eigenvector Centrality",
                  col = grp_colors,
                  border = grp_colors,
                  las = 2,
                  cex.names = 0.8)
    grid(nx = NA, ny = NULL, col = "gray90")
    abline(h = 0, lty = 2, col = "gray50")

    if(region_name == region_names[1]) {
      legend("topright", legend = all_groups, fill = grp_colors, bty = "n", cex = 0.8)
    }
  }
}

# Plot 4: Overview Top Hubs Plot (Summary Dashboard)
render_consensus_overview_top_hubs_plot <- function(comprehensive_consensus,
                                                     brain_areas = NULL,
                                                     area_colors = NULL,
                                                     group_colors = NULL) {

  all_groups <- names(comprehensive_consensus)
  if(length(all_groups) == 0) {
    plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
    text(1, 1, "No consensus data available", cex = 1.2, col = "gray")
    return()
  }

  par(mfrow = c(min(4, length(all_groups)), 1), mar = c(5, 10, 4, 2))

  for(group_name in all_groups[1:min(4, length(all_groups))]) {
    consensus_results <- comprehensive_consensus[[group_name]]

    if(is.null(consensus_results) || is.null(consensus_results$consensus_hubs)) {
      plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
      text(1, 1, paste("No data:", group_name), cex = 1, col = "gray")
      next
    }

    hubs_df <- consensus_results$consensus_hubs
    high_consensus <- hubs_df[hubs_df$ConsensusScore >= 0.6, ]

    if(nrow(high_consensus) == 0) {
      plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
      text(1, 1, paste("No hubs:", group_name), cex = 1, col = "gray")
      next
    }

    high_consensus <- high_consensus[order(-high_consensus$ConsensusScore), ]
    top_hubs <- head(high_consensus, 15)

    bp <- barplot(top_hubs$ConsensusScore,
                  names.arg = top_hubs$Node,
                  horiz = TRUE,
                  las = 1,
                  xlim = c(0, 1),
                  col = colorRampPalette(c("#FFA500", "#FF4500", "#DC143C"))(nrow(top_hubs)),
                  main = paste("Top Consensus Hubs:", group_name),
                  xlab = "Consensus Score (0-1)")

    abline(v = 0.6, col = "blue", lty = 2, lwd = 2)
    grid(nx = 10, ny = NA, col = "gray90")
  }
}


# Plot 5: Overview Agreement Plot (Summary Dashboard)
render_consensus_overview_agreement_plot <- function(comprehensive_consensus,
                                                     group_colors = NULL) {

  all_groups <- names(comprehensive_consensus)
  if(length(all_groups) == 0) {
    plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
    text(1, 1, "No consensus data available", cex = 1.2, col = "gray")
    return()
  }

  par(mfrow = c(min(4, length(all_groups)), 1), mar = c(5, 5, 4, 2))

  for(group_name in all_groups[1:min(4, length(all_groups))]) {
    consensus_results <- comprehensive_consensus[[group_name]]

    if(is.null(consensus_results) || is.null(consensus_results$consensus_hubs)) {
      plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
      text(1, 1, paste("No data:", group_name), cex = 1, col = "gray")
      next
    }

    hubs_df <- consensus_results$consensus_hubs

    categories <- cut(hubs_df$ConsensusScore,
                     breaks = c(0, 0.33, 0.66, 1.0),
                     labels = c("Low\n(0-33%)", "Medium\n(34-66%)", "High\n(67-100%)"),
                     include.lowest = TRUE)

    category_counts <- table(categories)

    bp <- barplot(category_counts,
                  col = c("#FFD700", "#FFA500", "#DC143C"),
                  main = paste("Hub Agreement:", group_name),
                  ylab = "Number of Nodes",
                  xlab = "Consensus Level",
                  ylim = c(0, max(category_counts) * 1.2))

    text(bp, category_counts, labels = category_counts, pos = 3, cex = 1.2, font = 2)
    grid(nx = NA, ny = NULL, col = "gray90")

    high_count <- category_counts["High\n(67-100%)"]
    total_count <- sum(category_counts)
    pct_high <- round(100 * high_count / total_count, 1)

    mtext(paste0(high_count, " nodes (", pct_high, "%) high consensus"),
          side = 1, line = 4, cex = 0.8, col = "darkred")
  }
}


# Plot 6: Network Similarity Heatmap (Tab C)
# Requires helper functions
compute_jaccard_similarity <- function(cor_matrix1, cor_matrix2, threshold = 0) {
  if(!all(dim(cor_matrix1) == dim(cor_matrix2))) {
    return(NA)
  }

  weights1 <- abs(cor_matrix1)
  weights2 <- abs(cor_matrix2)

  diag(weights1) <- 0
  diag(weights2) <- 0

  weights1[weights1 < threshold] <- 0
  weights2[weights2 < threshold] <- 0

  min_weights <- pmin(weights1, weights2)
  max_weights <- pmax(weights1, weights2)

  numerator <- sum(min_weights, na.rm = TRUE)
  denominator <- sum(max_weights, na.rm = TRUE)

  if(denominator == 0) return(0)

  jaccard <- numerator / denominator
  return(jaccard)
}

render_jaccard_heatmap <- function(jaccard_matrix, group_names, title = "Network Similarity (Jaccard Index)") {
  n <- nrow(jaccard_matrix)

  if(n < 2) {
    plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
    text(1, 1, "Need at least 2 groups for similarity analysis", cex = 1.2)
    return()
  }

  col_pal <- colorRampPalette(c("#2166AC", "#F7F7F7", "#B2182B"))(100)

  par(mar = c(8, 8, 4, 2))

  image(1:n, 1:n, jaccard_matrix,
        col = col_pal,
        xlab = "", ylab = "",
        main = title,
        axes = FALSE,
        zlim = c(0, 1))

  axis(1, at = 1:n, labels = group_names, las = 2, cex.axis = 0.9)
  axis(2, at = 1:n, labels = group_names, las = 2, cex.axis = 0.9)

  abline(h = (1:n) + 0.5, col = "gray30", lwd = 0.5)
  abline(v = (1:n) + 0.5, col = "gray30", lwd = 0.5)

  for(i in 1:n) {
    for(j in 1:n) {
      if(!is.na(jaccard_matrix[i,j])) {
        text_col <- if(jaccard_matrix[i,j] > 0.5) "white" else "black"
        text(i, j, sprintf("%.3f", jaccard_matrix[i,j]),
             cex = 0.8, col = text_col, font = 2)
      }
    }
  }

  legend("topright",
         legend = c("1.0 (Identical)", "0.5", "0.0 (No overlap)"),
         fill = col_pal[c(100, 50, 1)],
         bty = "n",
         cex = 0.8,
         inset = c(-0.15, 0),
         xpd = TRUE)
}

render_network_similarity_heatmap_plot <- function(correlation_methods_raw,
                                                    method_percolation_results,
                                                    persistence_results) {

  if(is.null(correlation_methods_raw)) {
    plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
    text(1, 1, "No correlation data available", cex = 1.2, col = "orange")
    return()
  }

  methods <- c("pearson", "spearman", "biweight", "shrinkage", "partial")
  approaches <- c("weighted", "percolation", "persistence")

  all_groups <- NULL
  for(method in methods) {
    if(!is.null(correlation_methods_raw[[method]])) {
      all_groups <- names(correlation_methods_raw[[method]])
      break
    }
  }

  if(is.null(all_groups) || length(all_groups) < 2) {
    plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
    text(1, 1, "Need at least 2 groups for comparison", cex = 1.2, col = "gray")
    return()
  }

  n_groups <- length(all_groups)

  group_jaccard_matrix <- matrix(0, nrow = n_groups, ncol = n_groups)
  rownames(group_jaccard_matrix) <- all_groups
  colnames(group_jaccard_matrix) <- all_groups
  combination_count <- matrix(0, nrow = n_groups, ncol = n_groups)

  for(method in methods) {
    for(approach in approaches) {
      group_networks <- list()

      for(group in all_groups) {
        network_mat <- NULL

        if(approach == "weighted") {
          if(!is.null(correlation_methods_raw[[method]][[group]])) {
            network_mat <- abs(correlation_methods_raw[[method]][[group]])
          }
        } else if(approach == "percolation") {
          if(!is.null(method_percolation_results[[method]]$adjacency_matrices)) {
            adj_mat <- method_percolation_results[[method]]$adjacency_matrices[[group]]
            if(!is.null(adj_mat) && !is.null(correlation_methods_raw[[method]][[group]])) {
              cor_mat <- abs(correlation_methods_raw[[method]][[group]])
              network_mat <- adj_mat * cor_mat
            }
          }
        } else if(approach == "persistence") {
          if(!is.null(persistence_results[[method]][[group]]$persistence_data) &&
             !is.null(correlation_methods_raw[[method]][[group]])) {
            pers_data <- persistence_results[[method]][[group]]$persistence_data
            threshold_vals <- as.numeric(names(pers_data))
            if(length(threshold_vals) > 0) {
              median_thresh <- median(threshold_vals)
              cor_mat <- abs(correlation_methods_raw[[method]][[group]])
              network_mat <- cor_mat
              network_mat[network_mat < median_thresh] <- 0
            }
          }
        }

        if(!is.null(network_mat)) {
          group_networks[[group]] <- network_mat
        }
      }

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

  for(i in 1:n_groups) {
    for(j in 1:n_groups) {
      if(i == j) {
        group_jaccard_matrix[i, j] <- 1
      } else if(combination_count[i, j] > 0) {
        group_jaccard_matrix[i, j] <- group_jaccard_matrix[i, j] / combination_count[i, j]
      }
    }
  }

  total_combinations <- max(combination_count)
  if(total_combinations == 0) {
    plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
    text(1, 1, "No method-approach combinations available", cex = 1.1, col = "orange")
    return()
  }

  render_jaccard_heatmap(group_jaccard_matrix, all_groups,
                        title = paste("Group Similarity (Avg across", total_combinations, "combinations)"))
}
