# Visualization Functions Module
# Contains all plot rendering functions

# Load required packages
has_corrplot <- requireNamespace("corrplot", quietly = TRUE)
has_igraph <- requireNamespace("igraph", quietly = TRUE)
has_viridis <- requireNamespace("viridis", quietly = TRUE)

if(has_corrplot) library(corrplot)
if(has_igraph) library(igraph)
if(has_viridis) library(viridis)

# Enhanced Imputation Plots
render_imputation_plots <- function(analysis_results, uploaded_data) {
  if(is.null(analysis_results$imputation)) return(NULL)
  
  par(mfrow = c(2, 2), mar = c(4, 4, 3, 2))
  
  # Missing pattern visualization
  if(!is.null(uploaded_data)) {
    missing_data <- is.na(uploaded_data)
    if(sum(missing_data) > 0) {
      image(1:ncol(missing_data), 1:nrow(missing_data), t(missing_data),
            main = "Missing Data Pattern", xlab = "Variables", ylab = "Observations",
            col = c("white", "red"))
    } else {
      plot(1, type="n", main = "No Missing Data", axes = FALSE)
      text(1, 1, "✓ Complete Data", cex = 2, col = "green")
    }
  }
  
  # Imputation method summary
  imp <- analysis_results$imputation
  if(imp$imputation_needed) {
    plot(1, type="n", main = "Imputation Method", axes = FALSE, xlim = c(0, 2), ylim = c(0, 2))
    text(1, 1.5, imp$method, cex = 1.2, col = "blue")
    text(1, 1, paste("Missing values:", imp$missing_count), cex = 1)
    text(1, 0.5, "✓ Successfully imputed", cex = 1, col = "green")
  } else {
    plot(1, type="n", main = "No Imputation Needed", axes = FALSE)
    text(1, 1, "✓ Complete Dataset", cex = 1.5, col = "green")
  }
  
  # Data distribution comparison
  if(!is.null(uploaded_data) && imp$imputation_needed) {
    raw_numeric <- uploaded_data[sapply(uploaded_data, is.numeric)]
    if(ncol(raw_numeric) > 0) {
      hist(unlist(raw_numeric), main = "Before Imputation", 
           xlab = "Values", col = "red", breaks = 20)
    }
  }
  
  if(!is.null(imp$imputed_data)) {
    imputed_numeric <- imp$imputed_data[sapply(imp$imputed_data, is.numeric)]
    if(ncol(imputed_numeric) > 0) {
      hist(unlist(imputed_numeric), main = "After Imputation", 
           xlab = "Values", col = "blue", breaks = 20)
    }
  }
}

# Correlation plots (standard colors)
render_correlation_plots <- function(correlations, experimental_group_colors = NULL) {
  if(is.null(correlations)) return()
  
  n_groups <- length(correlations)
  par(mfrow = c(ceiling(sqrt(n_groups)), ceiling(sqrt(n_groups))))
  
  for(group_name in names(correlations)) {
    cor_matrix <- correlations[[group_name]]
    if(has_corrplot) {
      corrplot(cor_matrix, method = "color", 
              title = paste("📊", group_name),
              tl.cex = 0.8, mar = c(0,0,2,0),
              col = colorRampPalette(c("blue", "white", "red"))(100))
    } else {
      image(cor_matrix, main = paste("📊", group_name))
    }
  }
}

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
  
  for(i in 1:min(4, length(groups))) {
    group <- groups[i]
    mst_data <- mst_results[[group]]
    
    if(!is.null(mst_data$central_nodes)) {
      # Get top central nodes by degree
      central_nodes <- mst_data$central_nodes$degree[1:min(5, length(mst_data$central_nodes$degree))]
      degree_values <- mst_data$degree_centrality[central_nodes]
      
      # Get colors for nodes based on brain areas
      node_colors <- rep("#3498DB", length(central_nodes))
      if(!is.null(brain_areas) && !is.null(area_colors)) {
        for(j in seq_along(central_nodes)) {
          node <- central_nodes[j]
          for(area_name in names(brain_areas)) {
            if(node %in% brain_areas[[area_name]]) {
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

# NEW: Render persistence heatmap showing hub nodes across thresholds
render_persistence_heatmap <- function(persistence_data, top_n = 20, main_title = NULL) {
  # Extract centrality across thresholds
  all_thresholds <- names(persistence_data)

  # Skip if no valid data
  if(length(all_thresholds) == 0) {
    plot(1, type = "n", main = "No persistence data available", axes = FALSE)
    text(1, 1, "No data", cex = 1.2)
    return(NULL)
  }

  # Get nodes from first threshold
  first_valid <- NULL
  for(thresh in all_thresholds) {
    if(!is.null(persistence_data[[thresh]]$nodes)) {
      first_valid <- thresh
      break
    }
  }

  if(is.null(first_valid)) {
    plot(1, type = "n", main = "No valid network data", axes = FALSE)
    text(1, 1, "No networks at any threshold", cex = 1.2)
    return(NULL)
  }

  all_nodes <- persistence_data[[first_valid]]$nodes$Node

  # Matrix: nodes × thresholds (eigenvector centrality values)
  heat_matrix <- matrix(0,
                       nrow = length(all_nodes),
                       ncol = length(all_thresholds),
                       dimnames = list(all_nodes, all_thresholds))

  for(thresh in all_thresholds) {
    nodes_df <- persistence_data[[thresh]]$nodes
    if(!is.null(nodes_df)) {
      heat_matrix[nodes_df$Node, thresh] <- nodes_df$Eigenvector
    }
  }

  # Calculate persistence score and keep top N
  persistence_score <- rowSums(heat_matrix > 0.1) / ncol(heat_matrix)
  top_nodes <- names(sort(persistence_score, decreasing = TRUE))[1:min(top_n, length(all_nodes))]

  # Create heatmap using pheatmap if available
  plot_title <- if(!is.null(main_title)) {
    paste("Hub Persistence:", main_title)
  } else {
    "Hub Persistence Across Correlation Thresholds"
  }

  if(requireNamespace("pheatmap", quietly = TRUE)) {
    pheatmap::pheatmap(
      heat_matrix[top_nodes, ],
      cluster_cols = FALSE,
      cluster_rows = TRUE,
      color = colorRampPalette(c("white", "yellow", "orange", "red"))(100),
      main = plot_title,
      fontsize = 10,
      cellwidth = 20,
      cellheight = 15
    )
  } else {
    # Fallback to base R heatmap
    heatmap(heat_matrix[top_nodes, ],
           Colv = NA,
           scale = "none",
           col = colorRampPalette(c("white", "yellow", "orange", "red"))(100),
           main = plot_title)
  }
}

# NEW: Render method agreement matrix
render_method_agreement_matrix <- function(consensus_analysis, group) {
  if(is.null(consensus_analysis) || is.null(consensus_analysis[[group]])) {
    plot(1, type = "n", main = "No consensus data available", axes = FALSE)
    text(1, 1, "No data", cex = 1.2)
    return(NULL)
  }

  agreement <- consensus_analysis[[group]]$agreement_matrix
  consensus_scores <- consensus_analysis[[group]]$consensus_score

  if(nrow(agreement) == 0) {
    plot(1, type = "n", main = "No hub nodes identified", axes = FALSE)
    text(1, 1, "No hubs", cex = 1.2)
    return(NULL)
  }

  # Sort by consensus score
  node_order <- names(sort(consensus_scores, decreasing = TRUE))
  agreement_sorted <- agreement[node_order, , drop = FALSE]

  # Convert to long format for ggplot
  if(requireNamespace("reshape2", quietly = TRUE) && requireNamespace("ggplot2", quietly = TRUE)) {
    library(reshape2)
    library(ggplot2)

    df_long <- melt(agreement_sorted, varnames = c("Node", "Method"), value.name = "IsHub")
    df_long$ConsensusScore <- consensus_scores[df_long$Node]

    # Plot
    p <- ggplot(df_long, aes(x = Method, y = Node, fill = factor(IsHub))) +
      geom_tile(color = "white", size = 0.5) +
      scale_fill_manual(values = c("0" = "white", "1" = "darkgreen"),
                       labels = c("Not Hub", "Hub"),
                       name = "Hub Status") +
      theme_minimal(base_size = 12) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            axis.text.y = element_text(size = 9)) +
      labs(title = paste("Method Agreement on Hub Identification -", group),
           x = "Correlation Method",
           y = "Node")

    print(p)
  } else {
    # Fallback to base R visualization
    image(t(agreement_sorted[nrow(agreement_sorted):1, ]),
         col = c("white", "darkgreen"),
         xlab = "Method", ylab = "Node",
         main = paste("Method Agreement -", group),
         axes = FALSE)
    axis(1, at = seq(0, 1, length.out = ncol(agreement_sorted)),
         labels = colnames(agreement_sorted), las = 2)
    axis(2, at = seq(0, 1, length.out = nrow(agreement_sorted)),
         labels = rev(rownames(agreement_sorted)), las = 2)
  }
}

# NEW: Render metrics evolution plot
render_metrics_evolution <- function(persistence_results,
                                    metric_name,
                                    method_name) {
  if(is.null(persistence_results) || is.null(persistence_results[[method_name]])) {
    plot(1, type = "n", main = "No persistence data available", axes = FALSE)
    text(1, 1, "No data", cex = 1.2)
    return(NULL)
  }

  all_data <- list()

  for(group in names(persistence_results[[method_name]])) {
    metrics_df <- persistence_results[[method_name]][[group]]$metrics_evolution

    if(!is.null(metrics_df) && metric_name %in% colnames(metrics_df)) {
      metrics_df$Group <- group
      all_data[[group]] <- metrics_df[, c("Threshold", metric_name, "Group")]
    }
  }

  if(length(all_data) == 0) {
    plot(1, type = "n", main = paste(metric_name, "not available"), axes = FALSE)
    text(1, 1, "No data", cex = 1.2)
    return(NULL)
  }

  combined <- do.call(rbind, all_data)
  colnames(combined)[2] <- "MetricValue"

  if(requireNamespace("ggplot2", quietly = TRUE)) {
    library(ggplot2)

    p <- ggplot(combined, aes(x = Threshold, y = MetricValue, color = Group)) +
      geom_line(size = 1.5) +
      geom_point(size = 2.5) +
      labs(title = paste(metric_name, "vs Correlation Threshold -", method_name),
           x = "Correlation Threshold",
           y = metric_name) +
      theme_minimal(base_size = 14) +
      theme(legend.position = "bottom")

    print(p)
  } else {
    # Fallback to base R
    groups <- unique(combined$Group)
    colors <- rainbow(length(groups))

    plot(combined$Threshold, combined$MetricValue,
         type = "n",
         main = paste(metric_name, "vs Threshold -", method_name),
         xlab = "Correlation Threshold",
         ylab = metric_name)

    for(i in seq_along(groups)) {
      group_data <- combined[combined$Group == groups[i], ]
      lines(group_data$Threshold, group_data$MetricValue,
           col = colors[i], lwd = 2)
      points(group_data$Threshold, group_data$MetricValue,
            col = colors[i], pch = 19)
    }

    legend("topright", legend = groups, col = colors, lwd = 2, pch = 19)
  }
}

# NEW: Render consensus overview plot
render_consensus_overview <- function(consensus_analysis) {
  if(is.null(consensus_analysis) || length(consensus_analysis) == 0) {
    plot(1, type = "n", main = "No consensus data available", axes = FALSE)
    text(1, 1, "No data", cex = 1.2)
    return(NULL)
  }

  # Create summary visualization of consensus across all groups
  all_groups_data <- list()

  for(group in names(consensus_analysis)) {
    consensus <- consensus_analysis[[group]]

    if(!is.null(consensus$consensus_score) && length(consensus$consensus_score) > 0) {
      df <- data.frame(
        Group = group,
        Node = names(consensus$consensus_score),
        ConsensusScore = consensus$consensus_score,
        MethodCount = consensus$method_count,
        stringsAsFactors = FALSE
      )

      all_groups_data[[group]] <- df
    }
  }

  if(length(all_groups_data) == 0) {
    plot(1, type = "n", main = "No consensus scores available", axes = FALSE)
    text(1, 1, "No data", cex = 1.2)
    return(NULL)
  }

  combined <- do.call(rbind, all_groups_data)

  if(requireNamespace("dplyr", quietly = TRUE) && requireNamespace("ggplot2", quietly = TRUE)) {
    library(dplyr)
    library(ggplot2)

    # Keep only top nodes per group
    top_nodes <- combined %>%
      group_by(Group) %>%
      top_n(15, ConsensusScore) %>%
      ungroup()

    p <- ggplot(top_nodes, aes(x = reorder(Node, ConsensusScore),
                         y = ConsensusScore,
                         fill = Group)) +
      geom_col() +
      coord_flip() +
      facet_wrap(~ Group, scales = "free_y") +
      labs(title = "Top Consensus Hub Nodes Across Groups",
           x = "Node",
           y = "Consensus Score (# Methods Agreeing)") +
      theme_minimal(base_size = 12) +
      theme(legend.position = "none")

    print(p)
  } else {
    # Fallback to base R
    groups <- unique(combined$Group)
    n_groups <- length(groups)

    par(mfrow = c(ceiling(n_groups/2), 2), mar = c(8, 4, 3, 2))

    for(group in groups) {
      group_data <- combined[combined$Group == group, ]
      group_data <- group_data[order(group_data$ConsensusScore, decreasing = TRUE), ]
      group_data <- head(group_data, 15)

      barplot(group_data$ConsensusScore,
             names.arg = group_data$Node,
             main = paste("Top Hubs -", group),
             ylab = "Consensus Score",
             las = 2,
             col = rainbow(nrow(group_data)))
    }

    par(mfrow = c(1, 1))
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

# Network Dashboard Plot with experimental group colors
render_network_dashboard <- function(global_metrics, experimental_group_colors = NULL) {
  if(is.null(global_metrics)) return(NULL)
  
  metrics <- global_metrics
  group_colors <- get_experimental_group_colors(metrics$Group, experimental_group_colors)
  
  par(mfrow = c(2, 3), mar = c(5, 4, 4, 2))
  
  # Network size comparison
  barplot(metrics$Nodes, names.arg = metrics$Group, main = "Network Size (Nodes)",
          ylab = "Number of Nodes", col = group_colors, border = "white")
  
  # Edge density
  barplot(metrics$Density * 100, names.arg = metrics$Group, main = "Network Density",
          ylab = "Density (%)", col = group_colors, border = "white")
  
  # Clustering coefficient
  barplot(metrics$Clustering, names.arg = metrics$Group, main = "Clustering Coefficient",
          ylab = "Clustering", col = group_colors, border = "white")
  
  # Path length (handle infinite values)
  path_lengths <- ifelse(is.infinite(metrics$Path_Length), NA, metrics$Path_Length)
  barplot(path_lengths, names.arg = metrics$Group, main = "Average Path Length",
          ylab = "Path Length", col = group_colors, border = "white")
  
  # Small-world index
  barplot(metrics$Small_World_Sigma, names.arg = metrics$Group, main = "Small-World Index",
          ylab = "Sigma", col = group_colors, border = "white")
  abline(h = 1, lty = 2, col = "red")
  
  # Connectivity status
  connectivity <- as.numeric(metrics$Connected)
  barplot(connectivity, names.arg = metrics$Group, main = "Network Connectivity",
          ylab = "Connected (1=Yes, 0=No)", col = group_colors, border = "white", ylim = c(0, 1.2))
}

# Enhanced Brain Area Centrality Plot with user-defined colors
render_node_centrality <- function(brain_area_metrics, brain_areas = NULL, area_colors = NULL) {
  if(is.null(brain_area_metrics) || nrow(brain_area_metrics) == 0) return(NULL)
  
  groups <- unique(brain_area_metrics$Group)
  
  par(mfrow = c(2, 2), mar = c(8, 4, 4, 2))
  
  # Plot brain area metrics for each group with error bars
  for(group in groups[1:min(4, length(groups))]) {
    group_data <- brain_area_metrics[brain_area_metrics$Group == group, ]
    if(nrow(group_data) > 0) {
      # Get brain area colors from UI state
      brain_area_colors <- get_brain_area_colors(group_data$Brain_Area, brain_areas, area_colors)
      
      # Create bar plot with error bars
      means <- group_data$Avg_Degree
      stdevs <- if("StdDev_Degree" %in% names(group_data)) group_data$StdDev_Degree else rep(0, length(means))
      
      # Create the barplot
      bp <- barplot(means, 
                    names.arg = group_data$Brain_Area, 
                    main = paste("Average Degree by Brain Area -", group),
                    ylab = "Average Degree", las = 2, cex.names = 0.8,
                    col = brain_area_colors,
                    border = "white",
                    ylim = c(0, max(means + stdevs, na.rm = TRUE) * 1.1))
      
      # Add error bars
      if(sum(stdevs, na.rm = TRUE) > 0) {
        segments(bp, means - stdevs, bp, means + stdevs, lwd = 2)
        segments(bp - 0.1, means - stdevs, bp + 0.1, means - stdevs, lwd = 1)
        segments(bp - 0.1, means + stdevs, bp + 0.1, means + stdevs, lwd = 1)
      }
    }
  }
}

# Enhanced Brain Area Heatmap Plot with user-defined colors
render_node_heatmap <- function(brain_area_metrics, brain_areas = NULL, area_colors = NULL) {
  if(is.null(brain_area_metrics) || nrow(brain_area_metrics) == 0) return(NULL)
  
  # Create brain area centrality matrix
  groups <- unique(brain_area_metrics$Group)
  brain_area_names <- unique(brain_area_metrics$Brain_Area)
  
  if(length(brain_area_names) == 0) return(NULL)
  
  # Create matrix: rows = groups, columns = brain areas
  heatmap_data <- matrix(0, nrow = length(groups), ncol = length(brain_area_names))
  rownames(heatmap_data) <- groups
  colnames(heatmap_data) <- brain_area_names
  
  # Fill matrix with average degree values
  for(i in seq_along(groups)) {
    group_data <- brain_area_metrics[brain_area_metrics$Group == groups[i], ]
    for(j in seq_along(brain_area_names)) {
      area_data <- group_data[group_data$Brain_Area == brain_area_names[j], ]
      if(nrow(area_data) > 0) {
        heatmap_data[i, j] <- area_data$Avg_Degree[1]
      }
    }
  }
  
  # Create custom color palette based on brain area colors
  if(!is.null(brain_areas) && length(brain_areas) > 0) {
    area_colors <- get_brain_area_colors(brain_area_names, brain_areas)
    # Create a gradient from white to each brain area color
    color_palette <- colorRampPalette(c("white", "lightblue", "darkblue"))(100)
  } else {
    color_palette <- colorRampPalette(c("white", "lightblue", "darkblue"))(100)
  }
  
  if(has_corrplot) {
    corrplot(heatmap_data, is.corr = FALSE, method = "color",
             tl.col = "black", tl.srt = 45,
             main = "Brain Area Average Degree by Group",
             col = color_palette)
  } else {
    image(t(heatmap_data), main = "Brain Area Average Degree by Group",
          axes = FALSE, col = color_palette)
    axis(1, at = seq(0, 1, length.out = ncol(heatmap_data)), 
         labels = colnames(heatmap_data))
    axis(2, at = seq(0, 1, length.out = nrow(heatmap_data)), 
         labels = rownames(heatmap_data))
  }
}

# Enhanced Edge Metrics Plot with experimental group colors
render_edge_metrics <- function(edge_metrics, experimental_group_colors = NULL) {
  if(is.null(edge_metrics) || nrow(edge_metrics) == 0) return(NULL)
  
  groups <- unique(edge_metrics$Group)
  group_colors <- get_experimental_group_colors(groups, experimental_group_colors)
  
  par(mfrow = c(2, 2), mar = c(5, 4, 4, 2))
  
  # Edge weight distribution
  for(group in groups[1:min(4, length(groups))]) {
    group_data <- edge_metrics[edge_metrics$Group == group, ]
    if(nrow(group_data) > 0) {
      group_color <- group_colors[group]
      hist(group_data$Weight, main = paste("Edge Weights -", group),
           xlab = "Edge Weight", ylab = "Frequency", 
           col = adjustcolor(group_color, alpha.f = 0.7), 
           border = group_color, breaks = 20)
      abline(v = mean(group_data$Weight), col = "darkred", lty = 2, lwd = 2)
      legend("topright", paste("Mean:", round(mean(group_data$Weight), 3)), 
             col = "darkred", lty = 2, cex = 0.8)
    }
  }
}

# Brain Area Connectivity Plot with user-defined colors
render_brain_area_connectivity <- function(brain_area_metrics, brain_areas = NULL, area_colors = NULL) {
  if(is.null(brain_area_metrics) || nrow(brain_area_metrics) == 0) return(NULL)
  
  groups <- unique(brain_area_metrics$Group)
  par(mfrow = c(length(groups), 2), mar = c(8, 4, 4, 2))
  
  for(group in groups) {
    group_data <- brain_area_metrics[brain_area_metrics$Group == group, ]
    if(nrow(group_data) > 0) {
      # Get brain area colors from UI state
      brain_area_colors <- get_brain_area_colors(group_data$Brain_Area, brain_areas, area_colors)
      
      # Internal vs External edges with brain area colors and error bars
      edge_means <- rbind(group_data$Internal_Edges, group_data$External_Edges)
      
      bp <- barplot(edge_means,
                    names.arg = group_data$Brain_Area,
                    main = paste("Internal vs External Edges -", group),
                    ylab = "Edge Count", las = 2, cex.names = 0.7,
                    col = c(adjustcolor(brain_area_colors, alpha.f = 0.8), 
                           adjustcolor(brain_area_colors, alpha.f = 0.4)),
                    legend = c("Internal", "External"),
                    beside = TRUE)
      
      # Node count per brain area with user colors and error bars
      means <- group_data$Nodes_Count
      stdevs <- if("StdDev_Nodes" %in% names(group_data)) group_data$StdDev_Nodes else rep(0, length(means))
      
      bp2 <- barplot(means,
                     names.arg = group_data$Brain_Area,
                     main = paste("Nodes per Brain Area -", group),
                     ylab = "Node Count", las = 2, cex.names = 0.7,
                     col = brain_area_colors,
                     border = "white",
                     ylim = c(0, max(means + stdevs, na.rm = TRUE) * 1.1))
      
      # Add error bars for node count
      if(sum(stdevs, na.rm = TRUE) > 0) {
        segments(bp2, means - stdevs, bp2, means + stdevs, lwd = 2)
        segments(bp2 - 0.1, means - stdevs, bp2 + 0.1, means - stdevs, lwd = 1)
        segments(bp2 - 0.1, means + stdevs, bp2 + 0.1, means + stdevs, lwd = 1)
      }
    }
  }
}

# Combined Group-Region Eigenvector Bar Plot
render_combined_eigenvector_bar_plot <- function(weighted_eigenvector_results, brain_areas = NULL, area_colors = NULL, group_colors = NULL) {
  if(is.null(weighted_eigenvector_results) || nrow(weighted_eigenvector_results) == 0) return(NULL)
  
  groups <- unique(weighted_eigenvector_results$Group)
  
  if(is.null(brain_areas) || length(brain_areas) == 0) {
    plot(1, type = "n", main = "No Brain Area Assignments Available")
    text(1, 1, "Please assign brain areas to view regional analysis")
    return()
  }
  
  # Prepare data structure
  all_stats <- data.frame(
    Group = character(),
    Region = character(),
    Mean_Eigenvector = numeric(),
    StdDev_Eigenvector = numeric(),
    stringsAsFactors = FALSE
  )
  
  # Calculate stats for each group and region
  for(group in groups) {
    group_data <- weighted_eigenvector_results[weighted_eigenvector_results$Group == group, ]
    
    for(region_name in names(brain_areas)) {
      region_nodes <- intersect(group_data$Node, brain_areas[[region_name]])
      if(length(region_nodes) > 0) {
        region_values <- group_data$Weighted_Eigenvector[group_data$Node %in% region_nodes]
        all_stats <- rbind(all_stats, data.frame(
          Group = group,
          Region = region_name,
          Mean_Eigenvector = mean(region_values, na.rm = TRUE),
          StdDev_Eigenvector = sd(region_values, na.rm = TRUE),
          stringsAsFactors = FALSE
        ))
      }
    }
  }
  
  if(nrow(all_stats) == 0) {
    plot(1, type = "n", main = "No Data Available for Regional Analysis")
    return()
  }
  
  # Create matrix for grouped bar plot
  regions <- unique(all_stats$Region)
  plot_matrix <- matrix(0, nrow = length(groups), ncol = length(regions))
  error_matrix <- matrix(0, nrow = length(groups), ncol = length(regions))
  rownames(plot_matrix) <- groups
  colnames(plot_matrix) <- regions
  rownames(error_matrix) <- groups
  colnames(error_matrix) <- regions
  
  for(i in 1:nrow(all_stats)) {
    row_idx <- which(groups == all_stats$Group[i])
    col_idx <- which(regions == all_stats$Region[i])
    plot_matrix[row_idx, col_idx] <- all_stats$Mean_Eigenvector[i]
    error_matrix[row_idx, col_idx] <- all_stats$StdDev_Eigenvector[i]
  }
  
  # Get group colors
  plot_colors <- if(!is.null(group_colors)) {
    sapply(groups, function(g) group_colors[[g]] %||% "steelblue")
  } else {
    rainbow(length(groups))
  }
  
  # Create grouped bar plot
  bp <- barplot(plot_matrix,
                beside = TRUE,
                main = "Average Eigenvector Centrality by Group and Region",
                ylab = "Average Weighted Eigenvector Centrality",
                las = 2, cex.names = 0.8,
                col = plot_colors,
                border = "white",
                legend.text = groups,
                args.legend = list(x = "topright", bty = "n"),
                ylim = c(0, max(plot_matrix + error_matrix, na.rm = TRUE) * 1.2))
  
  # Add error bars
  for(i in 1:nrow(plot_matrix)) {
    for(j in 1:ncol(plot_matrix)) {
      if(plot_matrix[i, j] > 0 && error_matrix[i, j] > 0) {
        x_pos <- bp[i, j]
        y_pos <- plot_matrix[i, j]
        error_val <- error_matrix[i, j]
        
        segments(x_pos, y_pos - error_val, x_pos, y_pos + error_val, lwd = 1.5)
        segments(x_pos - 0.1, y_pos - error_val, x_pos + 0.1, y_pos - error_val, lwd = 1)
        segments(x_pos - 0.1, y_pos + error_val, x_pos + 0.1, y_pos + error_val, lwd = 1)
      }
    }
  }
}

# Individual Node Analysis Functions
render_individual_node_centrality <- function(node_metrics, brain_areas = NULL, area_colors = NULL) {
  if(is.null(node_metrics) || nrow(node_metrics) == 0) return(NULL)
  
  groups <- unique(node_metrics$Group)
  par(mfrow = c(2, 2), mar = c(8, 4, 4, 2))
  
  # Plot top nodes for each group
  for(group in groups[1:min(4, length(groups))]) {
    group_data <- node_metrics[node_metrics$Group == group, ]
    if(nrow(group_data) > 0) {
      # Get top 10 nodes by degree
      top_nodes <- head(group_data[order(group_data$Degree, decreasing = TRUE), ], 10)
      
      # Get colors based on brain areas
      if(!is.null(top_nodes$Brain_Area)) {
        node_colors <- get_brain_area_colors(top_nodes$Brain_Area, brain_areas, area_colors)
      } else {
        node_colors <- rainbow(nrow(top_nodes))
      }
      
      barplot(top_nodes$Degree, 
              names.arg = top_nodes$Node,
              main = paste("Top Degree Nodes -", group),
              ylab = "Degree", las = 2, cex.names = 0.6,
              col = node_colors,
              border = "white")
    }
  }
}

render_individual_node_heatmap <- function(node_metrics, brain_areas = NULL, area_colors = NULL) {
  if(is.null(node_metrics) || nrow(node_metrics) == 0) return(NULL)
  
  # Create node centrality matrix (nodes x centrality measures)
  centrality_metrics <- c("Degree", "Betweenness", "Closeness", "Eigenvector", "PageRank")
  available_metrics <- intersect(centrality_metrics, names(node_metrics))
  
  if(length(available_metrics) < 2) return(NULL)
  
  # Get top nodes across all groups
  top_nodes_per_group <- 5
  groups <- unique(node_metrics$Group)
  
  selected_nodes <- c()
  for(group in groups) {
    group_data <- node_metrics[node_metrics$Group == group, ]
    top_nodes <- head(group_data[order(group_data$Degree, decreasing = TRUE), ], top_nodes_per_group)
    selected_nodes <- c(selected_nodes, paste(top_nodes$Node, group, sep = "_"))
  }
  
  # Create matrix
  node_data <- node_metrics[node_metrics$Group %in% groups, ]
  node_data$Node_Group <- paste(node_data$Node, node_data$Group, sep = "_")
  selected_node_data <- node_data[node_data$Node_Group %in% selected_nodes, ]
  
  if(nrow(selected_node_data) == 0) return(NULL)
  
  # Create heatmap matrix
  heatmap_matrix <- as.matrix(selected_node_data[, available_metrics, drop = FALSE])
  rownames(heatmap_matrix) <- selected_node_data$Node_Group
  
  # Normalize for better visualization
  heatmap_matrix_norm <- apply(heatmap_matrix, 2, function(x) (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE)))
  
  if(has_corrplot) {
    corrplot(t(heatmap_matrix_norm), is.corr = FALSE, method = "color",
             tl.col = "black", tl.srt = 45, tl.cex = 0.7,
             main = "Individual Node Centralities (Normalized)")
  } else {
    image(t(heatmap_matrix_norm), main = "Individual Node Centralities", axes = FALSE)
    axis(1, at = seq(0, 1, length.out = nrow(heatmap_matrix_norm)), 
         labels = rownames(heatmap_matrix_norm), las = 2, cex.axis = 0.7)
    axis(2, at = seq(0, 1, length.out = ncol(heatmap_matrix_norm)), 
         labels = colnames(heatmap_matrix_norm))
  }
}

# Helper function to get brain area colors from UI state
get_brain_area_colors <- function(brain_area_names, brain_areas = NULL, area_colors = NULL) {
  if(is.null(brain_area_names) || length(brain_area_names) == 0) {
    return(character(0))
  }
  
  # Initialize with default colors
  colors <- rainbow(length(brain_area_names))
  names(colors) <- brain_area_names
  
  # If area_colors is provided (from ui_state$area_colors), use those
  if(!is.null(area_colors) && is.character(area_colors)) {
    for(area_name in brain_area_names) {
      if(area_name %in% names(area_colors) && !is.na(area_colors[area_name])) {
        colors[area_name] <- area_colors[area_name]
      }
    }
  }
  
  return(colors)
}

# Helper function to get experimental group colors
get_experimental_group_colors <- function(group_names, experimental_group_colors = NULL) {
  if(is.null(experimental_group_colors) || length(experimental_group_colors) == 0) {
    # Default group colors
    default_colors <- c("#3498db", "#e74c3c", "#27ae60", "#9b59b6", "#e67e22", "#1abc9c")
    colors <- default_colors[((seq_along(group_names) - 1) %% length(default_colors)) + 1]
    names(colors) <- group_names
    return(colors)
  }
  
  colors <- rep("#3498db", length(group_names))  # Default blue
  names(colors) <- group_names
  
  for(group_name in group_names) {
    if(group_name %in% names(experimental_group_colors)) {
      colors[group_name] <- experimental_group_colors[[group_name]]
    }
  }
  
  return(colors)
}

# FIX 2: Conservation Statistics Plot
render_conservation_stats <- function(similarities) {
  if(is.null(similarities)) return(NULL)
  
  par(mfrow = c(2, 2), mar = c(5, 4, 4, 2))
  
  # Jaccard similarity
  jaccard_values <- similarities$Jaccard
  names(jaccard_values) <- paste(similarities$Group1, "vs", similarities$Group2)
  barplot(jaccard_values, main = "Network Similarity (Jaccard Index)",
          ylab = "Jaccard Index", las = 2, col = "lightcoral")
  
  # Overlap coefficient
  overlap_values <- similarities$Overlap
  names(overlap_values) <- paste(similarities$Group1, "vs", similarities$Group2)
  barplot(overlap_values, main = "Network Overlap Coefficient",
          ylab = "Overlap Coefficient", las = 2, col = "lightblue")
  
  # Shared edges
  shared_values <- similarities$Shared_Edges
  names(shared_values) <- paste(similarities$Group1, "vs", similarities$Group2)
  barplot(shared_values, main = "Shared Edges Between Networks",
          ylab = "Number of Shared Edges", las = 2, col = "lightgreen")
  
  # Edge preservation
  if("Edge_Preservation_1to2" %in% names(similarities)) {
    preservation_data <- c(similarities$Edge_Preservation_1to2, similarities$Edge_Preservation_2to1)
    preservation_labels <- c(paste(similarities$Group1, "→", similarities$Group2),
                            paste(similarities$Group2, "→", similarities$Group1))
    barplot(preservation_data, names.arg = preservation_labels,
            main = "Edge Preservation", ylab = "Preservation Rate",
            las = 2, col = rainbow(length(preservation_data)), cex.names = 0.7)
  }
}

# Network Similarity Matrix Plot
render_network_similarity_matrix <- function(similarities) {
  if(is.null(similarities)) return(NULL)
  
  groups <- unique(c(similarities$Group1, similarities$Group2))
  
  # Create similarity matrix
  sim_matrix <- matrix(1, nrow = length(groups), ncol = length(groups))
  rownames(sim_matrix) <- groups
  colnames(sim_matrix) <- groups
  
  for(i in 1:nrow(similarities)) {
    g1 <- similarities$Group1[i]
    g2 <- similarities$Group2[i]
    jaccard <- similarities$Jaccard[i]
    
    idx1 <- which(groups == g1)
    idx2 <- which(groups == g2)
    sim_matrix[idx1, idx2] <- jaccard
    sim_matrix[idx2, idx1] <- jaccard
  }
  
  if(has_corrplot) {
    corrplot(sim_matrix, method = "color", type = "full",
             addCoef.col = "black", number.cex = 0.8,
             tl.col = "black", tl.srt = 45,
             col = colorRampPalette(c("white", "lightblue", "darkblue"))(100),
             main = "Network Similarity Matrix (Jaccard Index)")
  } else {
    image(sim_matrix, main = "Network Similarity Matrix", axes = FALSE)
    axis(1, at = seq(0, 1, length.out = length(groups)), labels = groups)
    axis(2, at = seq(0, 1, length.out = length(groups)), labels = groups)
  }
}

# Hub Conservation Plot
render_hub_conservation <- function(conservation) {
  if(is.null(conservation)) {
    plot(1, type = "n", main = "Hub Conservation Analysis", axes = FALSE)
    text(1, 1, "No conservation data available", cex = 1.5, col = "gray")
    return()
  }
  
  if(!is.logical(conservation$conservation_possible) || !conservation$conservation_possible) {
    plot(1, type = "n", main = "Hub Conservation Analysis", axes = FALSE)
    text(1, 1, "Not enough groups for\nconservation analysis", cex = 1.5, col = "gray")
    return()
  }
  
  hub_conservation <- conservation$hub_conservation
  if(!is.null(hub_conservation) && !is.null(hub_conservation$hub_conservation_matrix)) {
    if(has_corrplot) {
      corrplot(hub_conservation$hub_conservation_matrix, method = "color", type = "full",
               addCoef.col = "black", number.cex = 0.8,
               tl.col = "black", tl.srt = 45,
               col = colorRampPalette(c("white", "lightcoral", "darkred"))(100),
               main = "Hub Conservation Between Networks")
    } else {
      image(hub_conservation$hub_conservation_matrix, main = "Hub Conservation Matrix")
    }
  } else {
    plot(1, type = "n", main = "Hub Conservation Analysis", axes = FALSE)
    text(1, 1, "No hub conservation\nmatrix available", cex = 1.5, col = "gray")
  }
}

# Correlation Distribution Plot
render_correlation_distributions <- function(correlations) {
  if(is.null(correlations) || length(correlations) == 0) return(NULL)
  
  n_groups <- length(correlations)
  # Adjust layout to show all groups properly
  if(n_groups <= 4) {
    par(mfrow = c(2, 2), mar = c(4, 4, 3, 2))
  } else if(n_groups <= 6) {
    par(mfrow = c(2, 3), mar = c(4, 4, 3, 2))
  } else if(n_groups <= 9) {
    par(mfrow = c(3, 3), mar = c(4, 4, 3, 2))
  } else {
    # For more than 9 groups, use a 4x3 layout
    par(mfrow = c(3, 4), mar = c(4, 4, 3, 2))
  }
  
  for(group_name in names(correlations)) {
    # Handle both matrix and list formats
    if(is.matrix(correlations[[group_name]])) {
      cor_matrix <- correlations[[group_name]]
    } else if(is.list(correlations[[group_name]]) && "consensus" %in% names(correlations[[group_name]])) {
      cor_matrix <- correlations[[group_name]]$consensus
    } else {
      cor_matrix <- correlations[[group_name]]
    }
    
    if(!is.null(cor_matrix) && is.matrix(cor_matrix)) {
      # Get upper triangular correlations (excluding diagonal)
      cor_values <- cor_matrix[upper.tri(cor_matrix)]
      
      # Create histogram
      hist(cor_values, main = paste("Correlation Distribution -", group_name),
           xlab = "Correlation Coefficient", ylab = "Frequency",
           col = "skyblue", breaks = 30, xlim = c(-1, 1))
      
      # Add vertical lines for quartiles
      quartiles <- quantile(cor_values, c(0.25, 0.5, 0.75), na.rm = TRUE)
      abline(v = quartiles, col = c("blue", "red", "blue"), lty = c(2, 1, 2))
      
      # Add statistics
      legend("topright", 
             c(sprintf("Mean: %.3f", mean(cor_values, na.rm = TRUE)),
               sprintf("Median: %.3f", median(cor_values, na.rm = TRUE)),
               sprintf("SD: %.3f", sd(cor_values, na.rm = TRUE))),
             bty = "n", cex = 0.8)
    }
  }
}

# Summary Dashboard Plot
render_summary_dashboard <- function(analysis_results) {
  if(!analysis_results$complete) return(NULL)
  
  par(mfrow = c(2, 2), mar = c(5, 4, 4, 2))
  
  # Analysis overview with consensus information
  plot(1, type = "n", main = "Enhanced Analysis Overview", axes = FALSE, xlim = c(0, 2), ylim = c(0, 2))
  
  n_groups <- length(analysis_results$correlations)
  n_networks <- length(analysis_results$networks)
  n_comparisons <- if(!is.null(analysis_results$similarities)) nrow(analysis_results$similarities) else 0
  
  # Get consensus metadata if available
  consensus_metadata <- attr(analysis_results$correlations, "consensus_metadata")
  avg_methods <- if(!is.null(consensus_metadata)) {
    round(mean(sapply(consensus_metadata, function(x) x$n_methods)), 1)
  } else 1
  
  text(1, 1.8, "🧠 Multimethod Consensus Functional Connectivity", cex = 1.4, col = "darkblue")
  text(1, 1.5, "🔬 5-Method Correlation Integration", cex = 1.1, col = "purple")
  text(1, 1.2, paste("Groups analyzed:", n_groups), cex = 1.1)
  text(1, 0.9, paste("Avg correlation methods:", avg_methods), cex = 1.1)
  text(1, 0.6, paste("Networks created:", n_networks), cex = 1.1)
  text(1, 0.3, "✅ Analysis Complete", cex = 1.2, col = "green")
  
  # Data quality summary
  if(!is.null(analysis_results$imputation)) {
    plot(1, type = "n", main = "Data Quality", axes = FALSE, xlim = c(0, 2), ylim = c(0, 2))
    
    imp <- analysis_results$imputation
    if(imp$imputation_needed) {
      text(1, 1.5, "📊 Data Imputation", cex = 1.3, col = "blue")
      text(1, 1.1, paste("Missing values:", imp$missing_count), cex = 1)
      text(1, 0.8, paste("Method:", imp$method), cex = 1)
      text(1, 0.4, "✅ Successfully imputed", cex = 1, col = "green")
    } else {
      text(1, 1.2, "📊 Data Quality", cex = 1.3, col = "green")
      text(1, 0.8, "✅ No missing values", cex = 1.2, col = "green")
      text(1, 0.4, "Original data used", cex = 1)
    }
  }
  
  # Network metrics overview
  if(!is.null(analysis_results$global_metrics)) {
    metrics <- analysis_results$global_metrics
    plot(1, type = "n", main = "Network Properties", axes = FALSE, xlim = c(0, 2), ylim = c(0, 2))
    
    avg_density <- mean(metrics$Density, na.rm = TRUE)
    avg_clustering <- mean(metrics$Clustering, na.rm = TRUE)
    connected_networks <- sum(metrics$Connected, na.rm = TRUE)
    
    text(1, 1.7, "🌐 Network Summary", cex = 1.3, col = "darkgreen")
    text(1, 1.3, sprintf("Avg density: %.3f", avg_density), cex = 1)
    text(1, 1.0, sprintf("Avg clustering: %.3f", avg_clustering), cex = 1)
    text(1, 0.7, sprintf("Connected: %d/%d", connected_networks, nrow(metrics)), cex = 1)
    text(1, 0.3, "📈 Metrics computed", cex = 1, col = "green")
  }
  
  # Conservation summary
  if(!is.null(analysis_results$similarities)) {
    similarities <- analysis_results$similarities
    plot(1, type = "n", main = "Network Conservation", axes = FALSE, xlim = c(0, 2), ylim = c(0, 2))
    
    avg_jaccard <- mean(similarities$Jaccard, na.rm = TRUE)
    avg_overlap <- mean(similarities$Overlap, na.rm = TRUE)
    
    text(1, 1.7, "🔗 Conservation Analysis", cex = 1.3, col = "purple")
    text(1, 1.3, sprintf("Avg Jaccard: %.3f", avg_jaccard), cex = 1)
    text(1, 1.0, sprintf("Avg overlap: %.3f", avg_overlap), cex = 1)
    text(1, 0.7, sprintf("Comparisons: %d", nrow(similarities)), cex = 1)
    text(1, 0.3, "🎯 Conservation measured", cex = 1, col = "green")
  } else {
    plot(1, type = "n", main = "Network Conservation", axes = FALSE, xlim = c(0, 2), ylim = c(0, 2))
    text(1, 1, "Single group analysis\nNo conservation computed", cex = 1.2, col = "gray")
  }
}

# ==============================================================================
# WEIGHTED EIGENVECTOR CENTRALITY VISUALIZATIONS
# ==============================================================================

# Render weighted eigenvector centrality comparison plot
render_weighted_eigenvector_comparison <- function(weighted_eigen_results, experimental_group_colors = NULL, brain_areas = NULL, area_colors = NULL) {
  if(is.null(weighted_eigen_results)) return(NULL)
  
  groups <- unique(weighted_eigen_results$Group)
  group_colors <- get_experimental_group_colors(groups, experimental_group_colors)
  
  par(mfrow = c(2, 2), mar = c(8, 4, 4, 2))
  
  # 1. Top nodes by weighted eigenvector centrality for each group
  for(i in 1:min(4, length(groups))) {
    group <- groups[i]
    group_data <- weighted_eigen_results[weighted_eigen_results$Group == group, ]
    
    # Get top 15 nodes
    top_nodes <- head(group_data[order(-group_data$Weighted_Eigenvector), ], 15)
    
    # Get colors based on brain areas if available, otherwise use group colors
    plot_colors <- rep(adjustcolor(group_colors[group], alpha.f = 0.8), nrow(top_nodes))
    if (!is.null(brain_areas) && !is.null(area_colors)) {
      for (j in seq_along(top_nodes$Node)) {
        node_name <- top_nodes$Node[j]
        for (area_name in names(brain_areas)) {
          if (node_name %in% brain_areas[[area_name]] && area_name %in% names(area_colors)) {
            plot_colors[j] <- adjustcolor(area_colors[[area_name]], alpha.f = 0.8)
            break
          }
        }
      }
    }
    
    barplot(top_nodes$Weighted_Eigenvector,
            names.arg = top_nodes$Node,
            main = paste("Top Weighted Eigenvector Nodes -", group),
            ylab = "Weighted Eigenvector Centrality",
            las = 2, cex.names = 0.6,
            col = plot_colors,
            border = "white")
    
    # Add reference line at 0.8 (typical hub threshold)
    abline(h = 0.8, lty = 2, col = "red")
  }
}

# Render weighted vs unweighted eigenvector scatter plot
render_weighted_vs_unweighted_eigenvector <- function(weighted_eigen_results, experimental_group_colors = NULL) {
  if(is.null(weighted_eigen_results)) return(NULL)
  
  groups <- unique(weighted_eigen_results$Group)
  group_colors <- get_experimental_group_colors(groups, experimental_group_colors)
  
  par(mfrow = c(2, 2), mar = c(5, 4, 4, 2))
  
  for(i in 1:min(4, length(groups))) {
    group <- groups[i]
    group_data <- weighted_eigen_results[weighted_eigen_results$Group == group, ]
    
    # Create scatter plot of Weighted Eigenvector vs Node Strength with 5a-d style
    plot(group_data$Node_Strength, group_data$Weighted_Eigenvector,
         main = paste("Weighted Eigenvector vs Node Strength -", group),
         xlab = "Node Strength",
         ylab = "Weighted Eigenvector Centrality",
         pch = 21, cex = 2.2, 
         bg = adjustcolor(group_colors[group], alpha.f = 0.6),
         col = adjustcolor(group_colors[group], alpha.f = 0.8),
         lwd = 2)
    
    # Add grid to match 5a-d style
    grid(col = "lightgray", lty = "dotted", lwd = 0.5)
    
    # Add labels overlaid on points (matching 5a-d style)
    if (nrow(group_data) > 0) {
      for (i in seq_len(nrow(group_data))) {
        text(group_data$Node_Strength[i], group_data$Weighted_Eigenvector[i], 
             labels = group_data$Node[i], 
             cex = 0.8, font = 2, col = "black")
      }
    }
    
    # Add trend line
    if(nrow(group_data) > 2) {
      lm_fit <- lm(Weighted_Eigenvector ~ Node_Strength, data = group_data)
      abline(lm_fit, col = "darkgray", lty = 2, lwd = 1.5)
    }
    
    # Add correlation
    cor_val <- cor(group_data$Node_Strength, group_data$Weighted_Eigenvector, use = "complete.obs")
    legend("bottomright", paste("r =", round(cor_val, 3)), bty = "n", cex = 0.9, bg = "white", box.col = "darkgray")
  }
}

# Render rank change analysis
render_eigenvector_rank_change <- function(weighted_eigen_results, experimental_group_colors = NULL) {
  if(is.null(weighted_eigen_results)) return(NULL)
  
  groups <- unique(weighted_eigen_results$Group)
  group_colors <- get_experimental_group_colors(groups, experimental_group_colors)
  
  par(mfrow = c(2, 2), mar = c(8, 4, 4, 2))
  
  for(i in 1:min(4, length(groups))) {
    group <- groups[i]
    group_data <- weighted_eigen_results[weighted_eigen_results$Group == group, ]
    
    # Get top nodes by weighted eigenvector centrality
    top_nodes <- head(group_data[order(-group_data$Weighted_Eigenvector), ], 15)
    
    # Create bar plot of weighted eigenvector centrality
    barplot(top_nodes$Weighted_Eigenvector,
            names.arg = top_nodes$Node,
            main = paste("Top Weighted Eigenvector Centrality -", group),
            ylab = "Weighted Eigenvector Centrality",
            las = 2, cex.names = 0.6,
            col = adjustcolor(group_colors[group], alpha.f = 0.7),
            border = "white")
  }
}

# Render hub comparison across groups
render_weighted_eigenvector_hub_comparison <- function(hub_results, experimental_group_colors = NULL) {
  if(is.null(hub_results) || length(hub_results) == 0) return(NULL)
  
  # Extract top 5 hubs from each group
  all_hubs <- data.frame()
  
  for(group in names(hub_results)) {
    group_hubs <- head(hub_results[[group]], 5)
    group_hubs$Rank <- 1:nrow(group_hubs)
    all_hubs <- rbind(all_hubs, group_hubs)
  }
  
  # Get unique hub nodes across all groups
  unique_hubs <- unique(all_hubs$Node)
  
  # Create hub presence matrix
  hub_matrix <- matrix(0, nrow = length(unique_hubs), ncol = length(names(hub_results)))
  rownames(hub_matrix) <- unique_hubs
  colnames(hub_matrix) <- names(hub_results)
  
  for(i in 1:nrow(all_hubs)) {
    node <- all_hubs$Node[i]
    group <- all_hubs$Group[i]
    value <- all_hubs$Weighted_Eigenvector[i]
    hub_matrix[node, group] <- value
  }
  
  # Plot heatmap
  par(mar = c(10, 8, 4, 2))
  
  if(has_corrplot) {
    corrplot(hub_matrix, is.corr = FALSE, method = "color",
             tl.col = "black", tl.srt = 45, tl.cex = 0.8,
             main = "Weighted Eigenvector Hub Nodes Across Groups",
             col = colorRampPalette(c("white", "lightblue", "darkblue"))(100),
             addCoef.col = "black", number.cex = 0.6)
  } else {
    image(t(hub_matrix), main = "Weighted Eigenvector Hub Nodes",
          axes = FALSE, col = heat.colors(100))
    axis(1, at = seq(0, 1, length.out = ncol(hub_matrix)), 
         labels = colnames(hub_matrix), las = 2)
    axis(2, at = seq(0, 1, length.out = nrow(hub_matrix)), 
         labels = rownames(hub_matrix), las = 2)
  }
}

# Render cross-group eigenvector stability
render_eigenvector_stability <- function(comparison_results) {
  if(is.null(comparison_results) || nrow(comparison_results) == 0) return(NULL)
  
  par(mfrow = c(2, 2), mar = c(8, 4, 4, 2))
  
  # 1. Top stable nodes (low CV)
  stable_nodes <- head(comparison_results[order(comparison_results$CV_Weighted_Eigenvector), ], 20)
  
  barplot(stable_nodes$CV_Weighted_Eigenvector,
          names.arg = stable_nodes$Node,
          main = "Most Stable Nodes Across Groups (Low CV)",
          ylab = "Coefficient of Variation",
          las = 2, cex.names = 0.6,
          col = "lightgreen",
          border = "darkgreen")
  
  # 2. Most variable nodes (high CV)
  variable_nodes <- head(comparison_results[order(-comparison_results$CV_Weighted_Eigenvector), ], 20)
  
  barplot(variable_nodes$CV_Weighted_Eigenvector,
          names.arg = variable_nodes$Node,
          main = "Most Variable Nodes Across Groups (High CV)",
          ylab = "Coefficient of Variation",
          las = 2, cex.names = 0.6,
          col = "lightcoral",
          border = "darkred")
  
  # 3. Range of eigenvector values
  high_range_nodes <- head(comparison_results[order(-comparison_results$Range_Weighted_Eigenvector), ], 20)
  
  barplot(high_range_nodes$Range_Weighted_Eigenvector,
          names.arg = high_range_nodes$Node,
          main = "Nodes with Largest Eigenvector Range",
          ylab = "Range (Max - Min)",
          las = 2, cex.names = 0.6,
          col = "lightyellow",
          border = "orange")
  
  # 4. Distribution of mean eigenvector centrality
  hist(comparison_results$Mean_Weighted_Eigenvector,
       main = "Distribution of Mean Eigenvector Centrality",
       xlab = "Mean Weighted Eigenvector Centrality",
       ylab = "Frequency",
       col = "lightblue",
       breaks = 30)
  
  # Add vertical lines for percentiles
  percentiles <- quantile(comparison_results$Mean_Weighted_Eigenvector, c(0.9, 0.95, 0.99))
  abline(v = percentiles, col = c("blue", "red", "darkred"), lty = 2)
  legend("topright", c("90th", "95th", "99th percentile"), 
         col = c("blue", "red", "darkred"), lty = 2, cex = 0.8)
}

# ==============================================================================
# NEW RESTRUCTURED PIPELINE VISUALIZATION FUNCTIONS
# ==============================================================================

# Group-specific percolation analysis
render_group_specific_percolation <- function(correlations, group_thresholds) {
  par(mfrow = c(2, 2), mar = c(4, 4, 3, 2))
  
  # 1. Threshold comparison across groups
  thresholds <- unlist(group_thresholds)
  barplot(thresholds, 
          main = "Group-Specific Percolation Thresholds",
          ylab = "Threshold Value",
          col = rainbow(length(thresholds)),
          las = 2, cex.names = 0.8)
  
  # 2. Threshold distribution
  hist(thresholds, 
       main = "Threshold Distribution",
       xlab = "Threshold Value",
       ylab = "Frequency",
       col = "lightblue",
       breaks = 10)
  
  # 3. Group comparison
  group_names <- names(group_thresholds)
  plot(1:length(group_names), thresholds,
       type = "b", pch = 19, col = "red",
       main = "Threshold Variation Across Groups",
       xlab = "Group", ylab = "Threshold",
       xaxt = "n")
  axis(1, at = 1:length(group_names), labels = group_names, las = 2)
  
  # 4. Summary statistics
  plot.new()
  text(0.5, 0.7, paste("Summary Statistics:"), cex = 1.5, font = 2)
  text(0.5, 0.6, paste("Mean:", round(mean(thresholds), 3)), cex = 1.2)
  text(0.5, 0.5, paste("SD:", round(sd(thresholds), 3)), cex = 1.2)
  text(0.5, 0.4, paste("Range:", round(max(thresholds) - min(thresholds), 3)), cex = 1.2)
  text(0.5, 0.3, paste("CV:", round(sd(thresholds)/mean(thresholds), 3)), cex = 1.2)
}

# Weighted node metrics visualization
render_weighted_node_metrics <- function(threshold_free_results, group_colors) {
  if(is.null(threshold_free_results$node_strength)) return()
  
  par(mfrow = c(2, 2), mar = c(4, 4, 3, 2))
  
  # 1. Node strength by group
  node_strength <- threshold_free_results$node_strength
  groups <- unique(node_strength$Group)
  
  boxplot(Node_Strength ~ Group, data = node_strength,
          main = "Node Strength by Group",
          ylab = "Node Strength",
          col = group_colors[groups])
  
  # 2. Top nodes by strength
  top_nodes <- head(node_strength[order(-node_strength$Node_Strength), ], 20)
  barplot(top_nodes$Node_Strength,
          names.arg = top_nodes$Node,
          main = "Top 20 Nodes by Strength",
          ylab = "Node Strength",
          col = "lightgreen",
          las = 2, cex.names = 0.6)
  
  # 3. Strength distribution
  hist(node_strength$Node_Strength,
       main = "Distribution of Node Strengths",
       xlab = "Node Strength",
       ylab = "Frequency",
       col = "lightblue",
       breaks = 30)
  
  # 4. Group comparison
  if(length(groups) > 1) {
    group_means <- aggregate(Node_Strength ~ Group, data = node_strength, mean)
    barplot(group_means$Node_Strength,
            names.arg = group_means$Group,
            main = "Mean Node Strength by Group",
            ylab = "Mean Node Strength",
            col = group_colors[group_means$Group])
  }
}

# All weighted statistics visualization
render_all_weighted_stats <- function(threshold_free_results, group_colors) {
  par(mfrow = c(2, 3), mar = c(4, 4, 3, 1))
  
  # Node strength
  if(!is.null(threshold_free_results$node_strength)) {
    node_strength <- threshold_free_results$node_strength
    boxplot(Node_Strength ~ Group, data = node_strength,
            main = "Node Strength", ylab = "Strength",
            col = group_colors)
  }
  
  # Mean connectivity
  if(!is.null(threshold_free_results$mean_connectivity)) {
    mean_conn <- threshold_free_results$mean_connectivity
    barplot(mean_conn$Mean_Connectivity,
            names.arg = mean_conn$Group,
            main = "Mean Connectivity", ylab = "Connectivity",
            col = group_colors)
  }
  
  # Network strength
  if(!is.null(threshold_free_results$network_strength)) {
    net_strength <- threshold_free_results$network_strength
    barplot(net_strength$Network_Strength,
            names.arg = net_strength$Group,
            main = "Network Strength", ylab = "Strength",
            col = group_colors)
  }
  
  # Weighted clustering
  if(!is.null(threshold_free_results$weighted_clustering)) {
    clustering <- threshold_free_results$weighted_clustering
    boxplot(Weighted_Clustering ~ Group, data = clustering,
            main = "Weighted Clustering", ylab = "Clustering",
            col = group_colors)
  }
  
  # Additional summary plots
  plot.new()
  text(0.5, 0.5, "All Weighted\nStatistics\nSummary", cex = 1.5, font = 2)
}


# Cross-method eigenvector comparison
render_cross_method_eigenvector_comparison <- function(weighted_eigenvector, node_metrics, group_colors, brain_areas = NULL, area_colors = NULL) {
  par(mfrow = c(2, 2), mar = c(4, 4, 3, 2))
  
  # Create comparison data
  comparison_data <- data.frame()
  
  for(group in unique(weighted_eigenvector$Group)) {
    weighted_data <- weighted_eigenvector[weighted_eigenvector$Group == group, ]
    percolation_data <- node_metrics[node_metrics$Group == group, ]
    
    common_nodes <- intersect(weighted_data$Node, percolation_data$Node)
    
    if(length(common_nodes) > 0) {
      for(node in common_nodes) {
        weighted_eigen <- weighted_data$Weighted_Eigenvector[weighted_data$Node == node]
        percolation_eigen <- percolation_data$Eigenvector[percolation_data$Node == node]
        
        comparison_data <- rbind(comparison_data, data.frame(
          Group = group,
          Node = node,
          Weighted = weighted_eigen,
          Percolation = percolation_eigen
        ))
      }
    }
  }
  
  if(nrow(comparison_data) > 0) {
    # Get colors based on brain areas if available, otherwise use group colors
    plot_colors <- group_colors[comparison_data$Group]
    if (!is.null(brain_areas) && !is.null(area_colors)) {
      for (i in seq_len(nrow(comparison_data))) {
        node_name <- comparison_data$Node[i]
        for (area_name in names(brain_areas)) {
          if (node_name %in% brain_areas[[area_name]] && area_name %in% names(area_colors)) {
            plot_colors[i] <- area_colors[[area_name]]
            break
          }
        }
      }
    }
    
    # 1. Scatter plot
    plot(comparison_data$Weighted, comparison_data$Percolation,
         main = "Weighted vs Percolation Eigenvector",
         xlab = "Weighted Eigenvector",
         ylab = "Percolation Eigenvector",
         col = plot_colors,
         pch = 19, cex = 1.2)
    abline(0, 1, lty = 2, col = "red")
    
    # Add labels with improved overlap prevention (same as 4c)
    n_labels <- min(12, nrow(comparison_data))  # Limit to 12 labels max
    top_idx <- order(comparison_data$Weighted, decreasing = TRUE)[1:n_labels]
    
    # Use smart positioning to prevent overlaps
    for (i in seq_along(top_idx)) {
      idx <- top_idx[i]
      x_pos <- comparison_data$Weighted[idx]
      y_pos <- comparison_data$Percolation[idx]
      
      # Smart position selection based on plot quadrant
      x_mid <- mean(comparison_data$Weighted)
      y_mid <- mean(comparison_data$Percolation)
      
      if (x_pos > x_mid && y_pos > y_mid) {
        pos_val <- if(i %% 2 == 0) 2 else 4  # Right or top
      } else if (x_pos <= x_mid && y_pos > y_mid) {
        pos_val <- if(i %% 2 == 0) 4 else 1  # Top or left
      } else if (x_pos <= x_mid && y_pos <= y_mid) {
        pos_val <- if(i %% 2 == 0) 1 else 3  # Left or bottom
      } else {
        pos_val <- if(i %% 2 == 0) 3 else 2  # Bottom or right
      }
      
      offset_val <- 0.3 + (i %% 3) * 0.1
      
      text(x_pos, y_pos, 
           labels = comparison_data$Node[idx], 
           pos = pos_val, cex = 0.9, col = "darkblue",
           offset = offset_val, font = 1)
    }
    
    # 2. Correlation by group
    if(length(unique(comparison_data$Group)) > 1) {
      correlations <- by(comparison_data, comparison_data$Group, function(x) {
        if(nrow(x) > 2) cor(x$Weighted, x$Percolation) else NA
      })
      
      valid_cors <- correlations[!is.na(correlations)]
      if(length(valid_cors) > 0) {
        barplot(unlist(valid_cors),
                names.arg = names(valid_cors),
                main = "Method Correlation by Group",
                ylab = "Correlation",
                col = group_colors[names(valid_cors)])
      }
    }
  }
}

# Cross-method node strength comparison
render_cross_method_node_strength_comparison <- function(node_strength, node_metrics, group_colors) {
  par(mfrow = c(2, 2), mar = c(4, 4, 3, 2))
  
  # Similar implementation to eigenvector comparison but for node strength
  comparison_data <- data.frame()
  
  for(group in unique(node_strength$Group)) {
    strength_data <- node_strength[node_strength$Group == group, ]
    percolation_data <- node_metrics[node_metrics$Group == group, ]
    
    common_nodes <- intersect(strength_data$Node, percolation_data$Node)
    
    if(length(common_nodes) > 0) {
      for(node in common_nodes) {
        threshold_free_strength <- strength_data$Node_Strength[strength_data$Node == node]
        percolation_strength <- percolation_data$Strength[percolation_data$Node == node]
        
        comparison_data <- rbind(comparison_data, data.frame(
          Group = group,
          Node = node,
          ThresholdFree = threshold_free_strength,
          Percolation = percolation_strength
        ))
      }
    }
  }
  
  if(nrow(comparison_data) > 0) {
    # Scatter plot
    plot(comparison_data$ThresholdFree, comparison_data$Percolation,
         main = "Threshold-Free vs Percolation Strength",
         xlab = "Threshold-Free Strength",
         ylab = "Percolation Strength",
         col = group_colors[comparison_data$Group],
         pch = 19)
    abline(0, 1, lty = 2, col = "red")
  }
}

# Pipeline overview
render_pipeline_overview <- function(analysis_results) {
  par(mfrow = c(2, 3), mar = c(3, 3, 3, 1))
  
  # Step 1: Imputation
  if(!is.null(analysis_results$imputation)) {
    plot.new()
    text(0.5, 0.7, "Step 1", cex = 1.5, font = 2, col = "blue")
    text(0.5, 0.5, "IMPUTATION", cex = 1.2, font = 2)
    text(0.5, 0.3, "✓ Complete", cex = 1, col = "green")
  }
  
  # Step 2: MultiMethod Correlation
  if(!is.null(analysis_results$correlations)) {
    plot.new()
    text(0.5, 0.7, "Step 2", cex = 1.5, font = 2, col = "blue")
    text(0.5, 0.5, "MULTIMETHOD\nCORRELATION", cex = 1.2, font = 2)
    text(0.5, 0.3, "✓ Complete", cex = 1, col = "green")
  }
  
  # Step 3: Topology Analysis
  if(!is.null(analysis_results$networks)) {
    plot.new()
    text(0.5, 0.7, "Step 3", cex = 1.5, font = 2, col = "blue")
    text(0.5, 0.5, "TOPOLOGY\nANALYSIS", cex = 1.2, font = 2)
    text(0.5, 0.3, "✓ Complete", cex = 1, col = "green")
  }
  
  # Step 4: Weighted Network Analysis
  if(!is.null(analysis_results$weighted_eigenvector)) {
    plot.new()
    text(0.5, 0.7, "Step 4", cex = 1.5, font = 2, col = "blue")
    text(0.5, 0.5, "WEIGHTED\nNETWORK", cex = 1.2, font = 2)
    text(0.5, 0.3, "✓ Complete", cex = 1, col = "green")
  }
  
  # Step 5: Cross Method Comparison
  if(!is.null(analysis_results$cross_method_comparison)) {
    plot.new()
    text(0.5, 0.7, "Step 5", cex = 1.5, font = 2, col = "blue")
    text(0.5, 0.5, "CROSS METHOD\nCOMPARISON", cex = 1.2, font = 2)
    text(0.5, 0.3, "✓ Complete", cex = 1, col = "green")
  }
  
  # Step 6: Summary
  plot.new()
  text(0.5, 0.7, "Step 6", cex = 1.5, font = 2, col = "blue")
  text(0.5, 0.5, "SUMMARY &\nDOWNLOAD", cex = 1.2, font = 2)
  text(0.5, 0.3, "✓ Ready", cex = 1, col = "green")
}

# ==============================================================================
# NEW ADDITIONAL VISUALIZATION FUNCTIONS FOR REQUESTED FEATURES
# ==============================================================================

# Regional summary statistics
render_regional_summary_stats <- function(weighted_eigenvector, node_strength, brain_areas, group_colors) {
  if(is.null(brain_areas) || length(brain_areas) == 0) return()
  
  par(mfrow = c(2, 2), mar = c(4, 4, 3, 2))
  
  # Combine data and assign brain areas
  combined_data <- merge(weighted_eigenvector, node_strength, 
                        by = c("Group", "Node"), all = TRUE, suffixes = c("_eigen", "_strength"))
  
  combined_data$Brain_Area <- "Unassigned"
  for(area_name in names(brain_areas)) {
    area_regions <- brain_areas[[area_name]]
    matching_nodes <- which(combined_data$Node %in% area_regions)
    if(length(matching_nodes) > 0) {
      combined_data$Brain_Area[matching_nodes] <- area_name
    }
  }
  
  # Summary statistics plots
  if("Weighted_Eigenvector" %in% names(combined_data)) {
    boxplot(Weighted_Eigenvector ~ Brain_Area, data = combined_data,
            main = "Eigenvector Distribution by Brain Area",
            ylab = "Eigenvector Centrality",
            las = 2, cex.axis = 0.8)
  }
  
  if("Node_Strength" %in% names(combined_data)) {
    boxplot(Node_Strength ~ Brain_Area, data = combined_data,
            main = "Node Strength Distribution by Brain Area", 
            ylab = "Node Strength",
            las = 2, cex.axis = 0.8)
  }
}

# Weighted vs Percolation Eigenvector comparison by group
render_weighted_vs_percolation_eigenvector_by_group <- function(weighted_eigenvector, node_metrics, group_colors, brain_areas = NULL, area_colors = NULL) {
  if (is.null(weighted_eigenvector) || is.null(node_metrics)) return()
  
  groups <- unique(weighted_eigenvector$Group)
  n_groups <- length(groups)
  
  # Set up layout: top plots for groups (smaller) + bottom plot gets more space
  if (n_groups <= 2) {
    layout(matrix(c(1:n_groups, rep(max(1:n_groups) + 1, n_groups)), nrow = 2, byrow = TRUE), 
           heights = c(1, 1.5))
    par(mar = c(4, 4, 3, 2), bg = "white")
  } else {
    layout(matrix(c(1, 2, 3, 4, 5, 5), nrow = 3, byrow = TRUE), 
           heights = c(1.5, 1.5, 1.8))
    par(mar = c(4, 4, 3, 2), bg = "white")
  }
  
  # Top plots: Individual group correlation plots
  for (i in 1:min(4, n_groups)) {
    group <- groups[i]
    weighted_group <- weighted_eigenvector[weighted_eigenvector$Group == group, ]
    percolation_group <- node_metrics[node_metrics$Group == group, ]
    
    # Match nodes
    common_nodes <- intersect(weighted_group$Node, percolation_group$Node)
    
    if (length(common_nodes) > 0) {
      weighted_vals <- sapply(common_nodes, function(node) {
        weighted_group$Weighted_Eigenvector[weighted_group$Node == node]
      })
      percolation_vals <- sapply(common_nodes, function(node) {
        percolation_group$Eigenvector[percolation_group$Node == node]
      })
      
      # Get colors based on brain areas for this group
      plot_colors <- rep("steelblue", length(common_nodes))
      if (!is.null(group_colors) && group %in% names(group_colors)) {
        plot_colors <- rep(group_colors[[group]], length(common_nodes))
      }
      if (!is.null(brain_areas) && !is.null(area_colors)) {
        for (j in seq_along(common_nodes)) {
          node_name <- common_nodes[j]
          for (area_name in names(brain_areas)) {
            if (node_name %in% brain_areas[[area_name]] && area_name %in% names(area_colors)) {
              plot_colors[j] <- area_colors[[area_name]]
              break
            }
          }
        }
      }
      
      # Create scatter plot for this group
      plot(weighted_vals, percolation_vals,
           main = paste("Eigenvector Centrality:", group),
           xlab = "Weighted Eigenvector",
           ylab = "Percolation Eigenvector",
           col = adjustcolor(plot_colors, alpha.f = 0.8),
           pch = 21, cex = 2.2, bg = adjustcolor(plot_colors, alpha.f = 0.6),
           lwd = 2)
      
      abline(0, 1, lty = 2, col = "darkgray", lwd = 1.5)
      grid(col = "lightgray", lty = "dotted", lwd = 0.5)
      
      # Add correlation
      if (length(weighted_vals) > 2) {
        corr <- cor(weighted_vals, percolation_vals, use = "complete.obs")
        legend("bottomright", paste("r =", round(corr, 3)), bty = "n", cex = 0.9, bg = "white", box.col = "darkgray")
      }
      
      # Add brain region labels for all nodes (improved labeling with connections)
      if (!is.null(brain_areas) && length(common_nodes) > 0) {
        # Label all nodes for better visibility
        n_labels <- length(common_nodes)
        
        for (idx in seq_along(common_nodes)) {
          node_name <- common_nodes[idx]
          x_pos <- weighted_vals[idx]
          y_pos <- percolation_vals[idx]
          
          # Use subregion name (node name)
          node_label <- node_name
          
          # Improved positioning to prevent overlap and cropping
          x_range <- max(weighted_vals) - min(weighted_vals)
          y_range <- max(percolation_vals) - min(percolation_vals)
          
          # Overlay labels directly on points
          text(x_pos, y_pos, node_label, 
               cex = 0.8, font = 2, col = "black")
        }
      }
    }
  }
  
  # Bottom plot: Combined weighted+percolation multibar plot by region showing group differences
  par(mar = c(8, 5, 4, 2))  # More space for labels
  render_combined_eigenvector_by_region_multibar(weighted_eigenvector, node_metrics, brain_areas, area_colors, group_colors)
  
  # Reset layout
  layout(1)
  par(mar = c(5, 4, 4, 2))
}

# Helper function for 5c - Combined weighted+percolation eigenvector multibar by region  
render_combined_eigenvector_by_region_multibar <- function(weighted_eigenvector, percolation_node_metrics, brain_areas, area_colors, group_colors) {
  if (is.null(weighted_eigenvector) || is.null(percolation_node_metrics) || is.null(brain_areas)) return()
  
  # Create summary data by region and group  
  summary_data <- data.frame()
  groups <- unique(weighted_eigenvector$Group)
  
  for (group in groups) {
    weighted_group <- weighted_eigenvector[weighted_eigenvector$Group == group, ]
    percolation_group <- percolation_node_metrics[percolation_node_metrics$Group == group, ]
    
    # Calculate regional averages for raw eigenvector values (not ranks)
    for (area_name in names(brain_areas)) {
      area_nodes <- brain_areas[[area_name]]
      
      # Get data for nodes in this area
      weighted_area <- weighted_group[weighted_group$Node %in% area_nodes, ]
      percolation_area <- percolation_group[percolation_group$Node %in% area_nodes, ]
      
      if (nrow(weighted_area) > 0 && nrow(percolation_area) > 0) {
        # Get the raw eigenvector values for nodes in this area
        weighted_vals <- weighted_area$Weighted_Eigenvector
        percolation_vals <- percolation_area$Eigenvector
        
        # Combined average of both weighted and percolation values
        combined_avg <- (mean(weighted_vals, na.rm = TRUE) + mean(percolation_vals, na.rm = TRUE)) / 2
        combined_se <- sd(c(weighted_vals, percolation_vals), na.rm = TRUE) / sqrt(length(c(weighted_vals, percolation_vals)))
        
        summary_data <- rbind(summary_data, data.frame(
          Group = group,
          Region = area_name,
          Combined_Avg_Value = combined_avg,
          Combined_SE = combined_se,
          stringsAsFactors = FALSE
        ))
      }
    }
  }
  
  if (nrow(summary_data) > 0) {
    # Prepare data for grouped bar plot (exactly like 5b style)
    regions <- unique(summary_data$Region)
    n_groups <- length(groups)
    
    # Create matrix for barplot
    plot_matrix <- matrix(NA, nrow = n_groups, ncol = length(regions))
    se_matrix <- matrix(NA, nrow = n_groups, ncol = length(regions))
    rownames(plot_matrix) <- groups
    colnames(plot_matrix) <- regions
    rownames(se_matrix) <- groups
    colnames(se_matrix) <- regions
    
    for (i in 1:nrow(summary_data)) {
      group <- summary_data$Group[i]
      region <- summary_data$Region[i]
      plot_matrix[group, region] <- summary_data$Combined_Avg_Value[i]
      se_matrix[group, region] <- summary_data$Combined_SE[i]
    }
    
    # Get group colors
    bar_colors <- rep("steelblue", n_groups)
    if (!is.null(group_colors)) {
      bar_colors <- sapply(groups, function(g) ifelse(g %in% names(group_colors), group_colors[[g]], "steelblue"))
    }
    
    # Create grouped bar plot
    bar_pos <- barplot(plot_matrix,
                      beside = TRUE,
                      main = "Average Eigenvector Centrality by Region\n(Combined Weighted + Percolation)",
                      xlab = "Brain Region",
                      ylab = "Combined Average Eigenvector Centrality",
                      col = bar_colors,
                      las = 2,
                      cex.names = 0.9,
                      ylim = c(0, max(plot_matrix + se_matrix, na.rm = TRUE) * 1.1),
                      space = c(0.1, 1))
    
    # Add error bars
    for (i in 1:nrow(plot_matrix)) {
      for (j in 1:ncol(plot_matrix)) {
        if (!is.na(plot_matrix[i, j]) && !is.na(se_matrix[i, j])) {
          segments(bar_pos[i, j], plot_matrix[i, j] - se_matrix[i, j],
                   bar_pos[i, j], plot_matrix[i, j] + se_matrix[i, j])
          segments(bar_pos[i, j] - 0.1, plot_matrix[i, j] - se_matrix[i, j],
                   bar_pos[i, j] + 0.1, plot_matrix[i, j] - se_matrix[i, j])
          segments(bar_pos[i, j] - 0.1, plot_matrix[i, j] + se_matrix[i, j],
                   bar_pos[i, j] + 0.1, plot_matrix[i, j] + se_matrix[i, j])
        }
      }
    }
    
    # Add legend
    legend("topright", legend = groups, fill = bar_colors, 
           bty = "n", cex = 0.9, bg = "white", box.col = "darkgray")
  }
}

# Helper function for eigenvector regional subplot
render_avg_eigenvector_by_region_subplot <- function(weighted_eigenvector, node_metrics, brain_areas, area_colors, group_colors) {
  if (is.null(weighted_eigenvector) || is.null(node_metrics) || is.null(brain_areas)) return()
  
  # Create summary data by region and group
  summary_data <- data.frame()
  
  for (group in unique(weighted_eigenvector$Group)) {
    weighted_group <- weighted_eigenvector[weighted_eigenvector$Group == group, ]
    percolation_group <- node_metrics[node_metrics$Group == group, ]
    
    for (area_name in names(brain_areas)) {
      area_nodes <- brain_areas[[area_name]]
      
      # Get weighted values for nodes in this area
      weighted_vals <- weighted_group$Weighted_Eigenvector[weighted_group$Node %in% area_nodes]
      percolation_vals <- percolation_group$Eigenvector[percolation_group$Node %in% area_nodes]
      
      if (length(weighted_vals) > 0 && length(percolation_vals) > 0) {
        # Average of weighted and percolation
        combined_mean <- mean(c(weighted_vals, percolation_vals), na.rm = TRUE)
        combined_se <- sd(c(weighted_vals, percolation_vals), na.rm = TRUE) / sqrt(length(c(weighted_vals, percolation_vals)))
        
        summary_data <- rbind(summary_data, data.frame(
          Group = group,
          Region = area_name,
          Combined_Mean = combined_mean,
          Combined_SE = combined_se
        ))
      }
    }
  }
  
  if (nrow(summary_data) > 0) {
    # Reshape data for plotting by region
    regions <- unique(summary_data$Region)
    groups <- unique(summary_data$Group)
    
    # Create matrix for grouped barplot
    plot_matrix <- matrix(NA, nrow = length(groups), ncol = length(regions))
    rownames(plot_matrix) <- groups
    colnames(plot_matrix) <- regions
    
    se_matrix <- matrix(NA, nrow = length(groups), ncol = length(regions))
    
    for (i in 1:nrow(summary_data)) {
      group_idx <- which(groups == summary_data$Group[i])
      region_idx <- which(regions == summary_data$Region[i])
      plot_matrix[group_idx, region_idx] <- summary_data$Combined_Mean[i]
      se_matrix[group_idx, region_idx] <- summary_data$Combined_SE[i]
    }
    
    # Get group colors
    bar_colors <- group_colors[groups]
    
    # Create grouped bar plot
    bar_pos <- barplot(plot_matrix,
                      beside = TRUE,
                      names.arg = regions,
                      main = "Average Eigenvector by Region (Combined Weighted + Percolation)",
                      ylab = "Average Eigenvector",
                      col = bar_colors,
                      las = 2, cex.names = 0.8)
    
    # Add error bars
    for (i in 1:length(groups)) {
      for (j in 1:length(regions)) {
        if (!is.na(plot_matrix[i, j])) {
          segments(bar_pos[i, j], plot_matrix[i, j] - se_matrix[i, j],
                   bar_pos[i, j], plot_matrix[i, j] + se_matrix[i, j], lwd = 2, col = "black")
        }
      }
    }
    
    legend("topright", groups, fill = bar_colors, title = "Groups", 
           bty = "n", cex = 1.0, bg = "white", box.col = "darkgray")
  }
}

# Weighted vs Percolation Node Strength comparison by group - IMPROVED CORRELATION PLOTS
render_weighted_vs_percolation_node_strength_by_group <- function(node_strength, node_metrics, group_colors, brain_areas = NULL, area_colors = NULL) {
  if (is.null(node_strength) || is.null(node_metrics)) return()
  
  groups <- unique(node_strength$Group)
  n_groups <- length(groups)
  
  # Set up layout: top plots for groups (smaller) + bottom plot gets more space
  if (n_groups <= 2) {
    layout(matrix(c(1:n_groups, rep(max(1:n_groups) + 1, n_groups)), nrow = 2, byrow = TRUE), 
           heights = c(1, 1.5))
    par(mar = c(4, 4, 3, 2), bg = "white")
  } else {
    layout(matrix(c(1, 2, 3, 4, 5, 5), nrow = 3, byrow = TRUE), 
           heights = c(1.5, 1.5, 1.8))
    par(mar = c(4, 4, 3, 2), bg = "white")
  }
  
  # Top plots: Individual group correlation plots
  for (i in 1:min(4, n_groups)) {
    group <- groups[i]
    weighted_group <- node_strength[node_strength$Group == group, ]
    percolation_group <- node_metrics[node_metrics$Group == group, ]
    
    # Match nodes
    common_nodes <- intersect(weighted_group$Node, percolation_group$Node)
    
    if (length(common_nodes) > 0) {
      weighted_vals <- sapply(common_nodes, function(node) {
        weighted_group$Node_Strength[weighted_group$Node == node]
      })
      percolation_vals <- sapply(common_nodes, function(node) {
        percolation_group$Strength[percolation_group$Node == node]
      })
      
      # Get colors based on brain areas for this group
      plot_colors <- rep("steelblue", length(common_nodes))
      if (!is.null(group_colors) && group %in% names(group_colors)) {
        plot_colors <- rep(group_colors[[group]], length(common_nodes))
      }
      if (!is.null(brain_areas) && !is.null(area_colors)) {
        for (j in seq_along(common_nodes)) {
          node_name <- common_nodes[j]
          for (area_name in names(brain_areas)) {
            if (node_name %in% brain_areas[[area_name]] && area_name %in% names(area_colors)) {
              plot_colors[j] <- area_colors[[area_name]]
              break
            }
          }
        }
      }
      
      # Create scatter plot for this group
      plot(weighted_vals, percolation_vals,
           main = paste("Node Strength:", group),
           xlab = "Weighted Node Strength",
           ylab = "Percolation Node Strength",
           col = adjustcolor(plot_colors, alpha.f = 0.8),
           pch = 21, cex = 2.2, bg = adjustcolor(plot_colors, alpha.f = 0.6),
           lwd = 2)
      
      abline(0, 1, lty = 2, col = "darkgray", lwd = 1.5)
      grid(col = "lightgray", lty = "dotted", lwd = 0.5)
      
      # Add correlation
      if (length(weighted_vals) > 2) {
        corr <- cor(weighted_vals, percolation_vals, use = "complete.obs")
        legend("bottomright", paste("r =", round(corr, 3)), bty = "n", cex = 0.9, bg = "white", box.col = "darkgray")
      }
      
      # Add brain region labels for all nodes (improved labeling with connections)
      if (!is.null(brain_areas) && length(common_nodes) > 0) {
        # Label all nodes for better visibility
        n_labels <- length(common_nodes)
        
        for (idx in seq_along(common_nodes)) {
          node_name <- common_nodes[idx]
          x_pos <- weighted_vals[idx]
          y_pos <- percolation_vals[idx]
          
          # Use subregion name (node name)
          node_label <- node_name
          
          # Improved positioning to prevent overlap and cropping
          x_range <- max(weighted_vals) - min(weighted_vals)
          y_range <- max(percolation_vals) - min(percolation_vals)
          
          # Overlay labels directly on points
          text(x_pos, y_pos, node_label, 
               cex = 0.8, font = 2, col = "black")
        }
      }
    }
  }
  
  # Bottom plot: Combined weighted+percolation multibar plot by region showing group differences
  par(mar = c(8, 5, 4, 2))  # More space for labels
  render_combined_node_strength_by_region_multibar(node_strength, node_metrics, brain_areas, area_colors, group_colors)
  
  # Reset layout
  layout(1)
  par(mar = c(5, 4, 4, 2))
}

# Helper function for 5a - Combined weighted+percolation node strength multibar by region
render_combined_node_strength_by_region_multibar <- function(weighted_node_strength, percolation_node_strength, brain_areas, area_colors, group_colors) {
  if (is.null(weighted_node_strength) || is.null(percolation_node_strength) || is.null(brain_areas)) return()
  
  # Create summary data by region and group  
  summary_data <- data.frame()
  groups <- unique(weighted_node_strength$Group)
  
  for (group in groups) {
    weighted_group <- weighted_node_strength[weighted_node_strength$Group == group, ]
    percolation_group <- percolation_node_strength[percolation_node_strength$Group == group, ]
    
    # Calculate regional averages for raw values (not ranks like 5b)
    for (area_name in names(brain_areas)) {
      area_nodes <- brain_areas[[area_name]]
      
      # Get data for nodes in this area
      weighted_area <- weighted_group[weighted_group$Node %in% area_nodes, ]
      percolation_area <- percolation_group[percolation_group$Node %in% area_nodes, ]
      
      if (nrow(weighted_area) > 0 && nrow(percolation_area) > 0) {
        # Get the raw values for nodes in this area
        weighted_vals <- weighted_area$Node_Strength
        percolation_vals <- percolation_area$Strength
        
        # Combined average of both weighted and percolation values
        combined_avg <- (mean(weighted_vals, na.rm = TRUE) + mean(percolation_vals, na.rm = TRUE)) / 2
        combined_se <- sd(c(weighted_vals, percolation_vals), na.rm = TRUE) / sqrt(length(c(weighted_vals, percolation_vals)))
        
        summary_data <- rbind(summary_data, data.frame(
          Group = group,
          Region = area_name,
          Combined_Avg_Value = combined_avg,
          Combined_SE = combined_se,
          stringsAsFactors = FALSE
        ))
      }
    }
  }
  
  if (nrow(summary_data) > 0) {
    # Prepare data for grouped bar plot (exactly like 5b style)
    regions <- unique(summary_data$Region)
    n_groups <- length(groups)
    
    # Create matrix for barplot
    plot_matrix <- matrix(NA, nrow = n_groups, ncol = length(regions))
    se_matrix <- matrix(NA, nrow = n_groups, ncol = length(regions))
    rownames(plot_matrix) <- groups
    colnames(plot_matrix) <- regions
    rownames(se_matrix) <- groups
    colnames(se_matrix) <- regions
    
    for (i in 1:nrow(summary_data)) {
      group <- summary_data$Group[i]
      region <- summary_data$Region[i]
      plot_matrix[group, region] <- summary_data$Combined_Avg_Value[i]
      se_matrix[group, region] <- summary_data$Combined_SE[i]
    }
    
    # Get group colors
    bar_colors <- rep("steelblue", n_groups)
    if (!is.null(group_colors)) {
      bar_colors <- sapply(groups, function(g) ifelse(g %in% names(group_colors), group_colors[[g]], "steelblue"))
    }
    
    # Create grouped bar plot
    bar_pos <- barplot(plot_matrix,
                      beside = TRUE,
                      main = "Average Node Strength by Region\n(Combined Weighted + Percolation)",
                      xlab = "Brain Region",
                      ylab = "Combined Average Node Strength",
                      col = bar_colors,
                      las = 2,
                      cex.names = 0.9,
                      ylim = c(0, max(plot_matrix + se_matrix, na.rm = TRUE) * 1.1),
                      space = c(0.1, 1))
    
    # Add error bars
    for (i in 1:nrow(plot_matrix)) {
      for (j in 1:ncol(plot_matrix)) {
        if (!is.na(plot_matrix[i, j]) && !is.na(se_matrix[i, j])) {
          segments(bar_pos[i, j], plot_matrix[i, j] - se_matrix[i, j],
                   bar_pos[i, j], plot_matrix[i, j] + se_matrix[i, j])
          segments(bar_pos[i, j] - 0.1, plot_matrix[i, j] - se_matrix[i, j],
                   bar_pos[i, j] + 0.1, plot_matrix[i, j] - se_matrix[i, j])
          segments(bar_pos[i, j] - 0.1, plot_matrix[i, j] + se_matrix[i, j],
                   bar_pos[i, j] + 0.1, plot_matrix[i, j] + se_matrix[i, j])
        }
      }
    }
    
    # Add legend
    legend("topright", legend = groups, fill = bar_colors, 
           bty = "n", cex = 0.9, bg = "white", box.col = "darkgray")
  }
}


# Helper function for regional subplot
render_avg_node_strength_by_region_subplot <- function(weighted_node_strength, percolation_node_strength, brain_areas, area_colors, group_colors) {
  if (is.null(weighted_node_strength) || is.null(percolation_node_strength) || is.null(brain_areas)) return()
  
  # Create summary data by region and group
  summary_data <- data.frame()
  
  for (group in unique(weighted_node_strength$Group)) {
    weighted_group <- weighted_node_strength[weighted_node_strength$Group == group, ]
    percolation_group <- percolation_node_strength[percolation_node_strength$Group == group, ]
    
    for (area_name in names(brain_areas)) {
      area_nodes <- brain_areas[[area_name]]
      
      # Get weighted values for nodes in this area
      weighted_vals <- weighted_group$Node_Strength[weighted_group$Node %in% area_nodes]
      percolation_vals <- percolation_group$Strength[percolation_group$Node %in% area_nodes]
      
      if (length(weighted_vals) > 0 && length(percolation_vals) > 0) {
        # Average of weighted and percolation
        combined_mean <- mean(c(weighted_vals, percolation_vals), na.rm = TRUE)
        combined_se <- sd(c(weighted_vals, percolation_vals), na.rm = TRUE) / sqrt(length(c(weighted_vals, percolation_vals)))
        
        summary_data <- rbind(summary_data, data.frame(
          Group = group,
          Region = area_name,
          Combined_Mean = combined_mean,
          Combined_SE = combined_se
        ))
      }
    }
  }
  
  if (nrow(summary_data) > 0) {
    # Reshape data for plotting by region
    regions <- unique(summary_data$Region)
    groups <- unique(summary_data$Group)
    
    # Create matrix for grouped barplot
    plot_matrix <- matrix(NA, nrow = length(groups), ncol = length(regions))
    rownames(plot_matrix) <- groups
    colnames(plot_matrix) <- regions
    
    se_matrix <- matrix(NA, nrow = length(groups), ncol = length(regions))
    
    for (i in 1:nrow(summary_data)) {
      group_idx <- which(groups == summary_data$Group[i])
      region_idx <- which(regions == summary_data$Region[i])
      plot_matrix[group_idx, region_idx] <- summary_data$Combined_Mean[i]
      se_matrix[group_idx, region_idx] <- summary_data$Combined_SE[i]
    }
    
    # Get group colors
    bar_colors <- group_colors[groups]
    
    # Create grouped bar plot
    bar_pos <- barplot(plot_matrix,
                      beside = TRUE,
                      names.arg = regions,
                      main = "Average Node Strength by Region (Combined Weighted + Percolation)",
                      ylab = "Average Node Strength",
                      col = bar_colors,
                      las = 2, cex.names = 0.8)
    
    # Add error bars
    for (i in 1:length(groups)) {
      for (j in 1:length(regions)) {
        if (!is.na(plot_matrix[i, j])) {
          segments(bar_pos[i, j], plot_matrix[i, j] - se_matrix[i, j],
                   bar_pos[i, j], plot_matrix[i, j] + se_matrix[i, j], lwd = 2, col = "black")
        }
      }
    }
    
    legend("topright", groups, fill = bar_colors, title = "Groups", 
           bty = "n", cex = 1.0, bg = "white", box.col = "darkgray")
  }
}

# ============================================================================
# ADDITIONAL RENDER FUNCTIONS FOR COMPREHENSIVE DOWNLOAD
# ============================================================================

# Rank-based comparison of weighted vs percolation metrics
render_rank_based_comparisons <- function(weighted_eigenvector, node_strength, node_metrics, group_colors, brain_areas = NULL, area_colors = NULL) {
  groups <- unique(weighted_eigenvector$Group)
  n_groups <- length(groups)
  
  # Set up layout: top plots for groups + bottom plot for regional averages
  if (n_groups <= 2) {
    layout(matrix(c(1:n_groups, rep(max(1:n_groups) + 1, n_groups)), nrow = 2, byrow = TRUE), 
           heights = c(1, 1.5))
    par(mar = c(4, 4, 3, 2), bg = "white")
  } else {
    layout(matrix(c(1, 2, 3, 4, 5, 5), nrow = 3, byrow = TRUE), 
           heights = c(1.5, 1.5, 1.8))
    par(mar = c(4, 4, 3, 2), bg = "white")
  }
  
  # Top plots: Individual group rank comparison plots
  for(i in 1:min(4, n_groups)) {
    group <- groups[i]
    
    # Get eigenvector data
    weighted_eig_data <- weighted_eigenvector[weighted_eigenvector$Group == group, ]
    perc_eig_data <- node_metrics[node_metrics$Group == group, ]
    
    # Get node strength data
    weighted_str_data <- node_strength[node_strength$Group == group, ]
    perc_str_data <- node_metrics[node_metrics$Group == group, ]
    
    # Match nodes for eigenvector
    common_eig_nodes <- intersect(weighted_eig_data$Node, perc_eig_data$Node)
    
    if(length(common_eig_nodes) > 5) {  # Need at least 5 nodes for meaningful ranks
      # Get eigenvector values and ranks
      weighted_eig_vals <- sapply(common_eig_nodes, function(node) {
        weighted_eig_data$Weighted_Eigenvector[weighted_eig_data$Node == node]
      })
      perc_eig_vals <- sapply(common_eig_nodes, function(node) {
        perc_eig_data$Eigenvector[perc_eig_data$Node == node]
      })
      
      # Convert to ranks and normalize to 0-1 (inverted: rank 1 = n/n, rank 2 = (n-1)/n)
      n_nodes <- length(weighted_eig_vals)
      weighted_eig_ranks <- (n_nodes - rank(-weighted_eig_vals) + 1) / n_nodes  # Inverted ranking
      perc_eig_ranks <- (n_nodes - rank(-perc_eig_vals) + 1) / n_nodes
      
      # Get colors based on brain areas if available
      plot_colors <- rep("steelblue", length(common_eig_nodes))
      if (!is.null(group_colors) && group %in% names(group_colors)) {
        plot_colors <- rep(group_colors[[group]], length(common_eig_nodes))
      }
      if (!is.null(brain_areas) && !is.null(area_colors)) {
        for (j in seq_along(common_eig_nodes)) {
          node_name <- common_eig_nodes[j]
          for (area_name in names(brain_areas)) {
            if (node_name %in% brain_areas[[area_name]] && area_name %in% names(area_colors)) {
              plot_colors[j] <- area_colors[[area_name]]
              break
            }
          }
        }
      }
      
      # Plot eigenvector ranks
      plot(weighted_eig_ranks, perc_eig_ranks,
           main = paste("Eigenvector Rank:", group),
           xlab = "Weighted Rank (1=best)",
           ylab = "Percolation Rank (1=best)",
           col = adjustcolor(plot_colors, alpha.f = 0.8),
           pch = 21, cex = 2.2, bg = adjustcolor(plot_colors, alpha.f = 0.6),
           lwd = 2, xlim = c(0, 1), ylim = c(0, 1))
      
      # Add identity line and grid
      abline(0, 1, lty = 2, col = "darkgray", lwd = 1.5)
      grid(col = "lightgray", lty = "dotted", lwd = 0.5)
      
      # Add correlation
      if(length(weighted_eig_ranks) > 2) {
        corr <- cor(weighted_eig_ranks, perc_eig_ranks, use = "complete.obs", method = "spearman")
        legend("bottomright", paste("ρ =", round(corr, 3)), bty = "n", cex = 0.9, bg = "white", box.col = "darkgray")
      }
      
      # Add subregion labels for all nodes (improved labeling with connections)
      if (!is.null(brain_areas) && length(common_eig_nodes) > 0) {
        # Label all nodes for better visibility
        n_labels <- length(common_eig_nodes)
        
        for (idx in seq_along(common_eig_nodes)) {
          node_name <- common_eig_nodes[idx]
          x_pos <- weighted_eig_ranks[idx]
          y_pos <- perc_eig_ranks[idx]
          
          # Use subregion name (node name)
          node_label <- node_name
          
          # Improved positioning to prevent overlap and cropping
          x_range <- max(weighted_eig_ranks) - min(weighted_eig_ranks)
          y_range <- max(perc_eig_ranks) - min(perc_eig_ranks)
          
          # More sophisticated offset calculation for better distribution
          angle <- (idx * 137.5) * pi / 180  # Golden angle for better distribution
          radius <- 0.03 * min(x_range, y_range)  # Reduced radius for closer labels
          x_jitter <- radius * cos(angle)
          y_jitter <- radius * sin(angle)
          
          # Overlay labels directly on points
          text(x_pos, y_pos, node_label, 
               cex = 0.8, font = 2, col = "black")
        }
      }
    } else {
      plot(1, type = "n", main = paste("Insufficient Data:", group), xlab = "", ylab = "")
      text(1, 1, "Need >5 nodes for ranking", cex = 1.0)
    }
  }
  
  # Bottom plot: Combined weighted+percolation multibar plot by region showing group differences (RANKED)
  par(mar = c(8, 5, 4, 2))  # More space for labels
  render_combined_eigenvector_rank_by_region_multibar(weighted_eigenvector, node_metrics, brain_areas, area_colors, group_colors)
  
  # Reset layout
  layout(1)
  par(mar = c(5, 4, 4, 2))
}

# Helper function for 5d - Combined weighted+percolation eigenvector RANK multibar by region
render_combined_eigenvector_rank_by_region_multibar <- function(weighted_eigenvector, percolation_node_metrics, brain_areas, area_colors, group_colors) {
  if (is.null(weighted_eigenvector) || is.null(percolation_node_metrics) || is.null(brain_areas)) return()
  
  # Create summary data by region and group  
  summary_data <- data.frame()
  groups <- unique(weighted_eigenvector$Group)
  
  for (group in groups) {
    weighted_group <- weighted_eigenvector[weighted_eigenvector$Group == group, ]
    percolation_group <- percolation_node_metrics[percolation_node_metrics$Group == group, ]
    
    # First, rank ALL nodes within this group (across all regions) - like 5b
    n_weighted_all <- length(weighted_group$Weighted_Eigenvector)
    n_percolation_all <- length(percolation_group$Eigenvector)
    
    # Create global ranks for this group
    weighted_global_ranks <- (n_weighted_all - rank(-weighted_group$Weighted_Eigenvector) + 1) / n_weighted_all
    percolation_global_ranks <- (n_percolation_all - rank(-percolation_group$Eigenvector) + 1) / n_percolation_all
    
    # Add ranks to data frames
    weighted_group$Rank <- weighted_global_ranks
    percolation_group$Rank <- percolation_global_ranks
    
    # Now calculate regional averages of RANKS
    for (area_name in names(brain_areas)) {
      area_nodes <- brain_areas[[area_name]]
      
      # Get data for nodes in this area
      weighted_area <- weighted_group[weighted_group$Node %in% area_nodes, ]
      percolation_area <- percolation_group[percolation_group$Node %in% area_nodes, ]
      
      if (nrow(weighted_area) > 0 && nrow(percolation_area) > 0) {
        # Get the ranks for nodes in this area (already calculated globally)
        weighted_ranks <- weighted_area$Rank
        percolation_ranks <- percolation_area$Rank
        
        # Combined average of both weighted and percolation ranks
        combined_avg <- (mean(weighted_ranks, na.rm = TRUE) + mean(percolation_ranks, na.rm = TRUE)) / 2
        combined_se <- sd(c(weighted_ranks, percolation_ranks), na.rm = TRUE) / sqrt(length(c(weighted_ranks, percolation_ranks)))
        
        summary_data <- rbind(summary_data, data.frame(
          Group = group,
          Region = area_name,
          Combined_Avg_Rank = combined_avg,
          Combined_SE = combined_se,
          stringsAsFactors = FALSE
        ))
      }
    }
  }
  
  if (nrow(summary_data) > 0) {
    # Prepare data for grouped bar plot (exactly like 5b style)
    regions <- unique(summary_data$Region)
    n_groups <- length(groups)
    
    # Create matrix for barplot
    plot_matrix <- matrix(NA, nrow = n_groups, ncol = length(regions))
    se_matrix <- matrix(NA, nrow = n_groups, ncol = length(regions))
    rownames(plot_matrix) <- groups
    colnames(plot_matrix) <- regions
    rownames(se_matrix) <- groups
    colnames(se_matrix) <- regions
    
    for (i in 1:nrow(summary_data)) {
      group <- summary_data$Group[i]
      region <- summary_data$Region[i]
      plot_matrix[group, region] <- summary_data$Combined_Avg_Rank[i]
      se_matrix[group, region] <- summary_data$Combined_SE[i]
    }
    
    # Get group colors
    bar_colors <- rep("steelblue", n_groups)
    if (!is.null(group_colors)) {
      bar_colors <- sapply(groups, function(g) ifelse(g %in% names(group_colors), group_colors[[g]], "steelblue"))
    }
    
    # Create grouped bar plot
    bar_pos <- barplot(plot_matrix,
                      beside = TRUE,
                      main = "Average Eigenvector Rank by Region\n(Combined Weighted + Percolation)",
                      xlab = "Brain Region",
                      ylab = "Combined Average Rank (1=best, 0=worst)",
                      col = bar_colors,
                      las = 2,
                      cex.names = 0.9,
                      ylim = c(0, max(plot_matrix + se_matrix, na.rm = TRUE) * 1.1),
                      space = c(0.1, 1))
    
    # Add error bars
    for (i in 1:nrow(plot_matrix)) {
      for (j in 1:ncol(plot_matrix)) {
        if (!is.na(plot_matrix[i, j]) && !is.na(se_matrix[i, j])) {
          segments(bar_pos[i, j], plot_matrix[i, j] - se_matrix[i, j],
                   bar_pos[i, j], plot_matrix[i, j] + se_matrix[i, j])
          segments(bar_pos[i, j] - 0.1, plot_matrix[i, j] - se_matrix[i, j],
                   bar_pos[i, j] + 0.1, plot_matrix[i, j] - se_matrix[i, j])
          segments(bar_pos[i, j] - 0.1, plot_matrix[i, j] + se_matrix[i, j],
                   bar_pos[i, j] + 0.1, plot_matrix[i, j] + se_matrix[i, j])
        }
      }
    }
    
    # Add legend
    legend("topright", legend = groups, fill = bar_colors, 
           bty = "n", cex = 0.9, bg = "white", box.col = "darkgray")
  }
}


# Average node strength by region
render_avg_node_strength_by_region <- function(weighted_node_strength, percolation_node_strength, brain_areas, area_colors, group_colors) {
  if (is.null(weighted_node_strength) || is.null(percolation_node_strength) || is.null(brain_areas)) return()
  
  par(mfrow = c(1, 1), mar = c(8, 5, 4, 2), bg = "white")
  
  # Create summary data by region and group
  summary_data <- data.frame()
  
  for (group in unique(weighted_node_strength$Group)) {
    weighted_group <- weighted_node_strength[weighted_node_strength$Group == group, ]
    percolation_group <- percolation_node_strength[percolation_node_strength$Group == group, ]
    
    for (area_name in names(brain_areas)) {
      area_nodes <- brain_areas[[area_name]]
      
      # Get weighted values for nodes in this area
      weighted_vals <- weighted_group$Node_Strength[weighted_group$Node %in% area_nodes]
      percolation_vals <- percolation_group$Strength[percolation_group$Node %in% area_nodes]
      
      if (length(weighted_vals) > 0 && length(percolation_vals) > 0) {
        # Average of weighted and percolation
        combined_mean <- mean(c(weighted_vals, percolation_vals), na.rm = TRUE)
        combined_se <- sd(c(weighted_vals, percolation_vals), na.rm = TRUE) / sqrt(length(c(weighted_vals, percolation_vals)))
        
        summary_data <- rbind(summary_data, data.frame(
          Group = group,
          Region = area_name,
          Combined_Mean = combined_mean,
          Combined_SE = combined_se
        ))
      }
    }
  }
  
  if (nrow(summary_data) > 0) {
    # Reshape data for plotting by region
    regions <- unique(summary_data$Region)
    groups <- unique(summary_data$Group)
    
    # Create matrix for grouped barplot
    plot_matrix <- matrix(NA, nrow = length(groups), ncol = length(regions))
    rownames(plot_matrix) <- groups
    colnames(plot_matrix) <- regions
    
    se_matrix <- matrix(NA, nrow = length(groups), ncol = length(regions))
    
    for (i in 1:nrow(summary_data)) {
      group_idx <- which(groups == summary_data$Group[i])
      region_idx <- which(regions == summary_data$Region[i])
      plot_matrix[group_idx, region_idx] <- summary_data$Combined_Mean[i]
      se_matrix[group_idx, region_idx] <- summary_data$Combined_SE[i]
    }
    
    # Get group colors
    bar_colors <- group_colors[groups]
    
    # Create grouped bar plot
    bar_pos <- barplot(plot_matrix,
                      beside = TRUE,
                      names.arg = regions,
                      main = "Average Node Strength by Region (Combined Weighted + Percolation)",
                      ylab = "Average Node Strength",
                      col = bar_colors,
                      las = 2, cex.names = 0.8)
    
    # Add error bars
    for (i in 1:length(groups)) {
      for (j in 1:length(regions)) {
        if (!is.na(plot_matrix[i, j])) {
          segments(bar_pos[i, j], plot_matrix[i, j] - se_matrix[i, j],
                   bar_pos[i, j], plot_matrix[i, j] + se_matrix[i, j])
        }
      }
    }
    
    legend("topright", groups, fill = bar_colors, title = "Groups", 
           bty = "n", cex = 1.0, bg = "white", box.col = "darkgray")
  }
}

# Rank-based node strength comparison by group with average rank by region
render_rank_based_node_strength_with_avg <- function(weighted_node_strength, percolation_node_strength, brain_areas, area_colors, group_colors) {
  if (is.null(weighted_node_strength) || is.null(percolation_node_strength)) return()
  
  groups <- unique(weighted_node_strength$Group)
  n_groups <- length(groups)
  
  # Set up layout: top plots for groups (smaller) + bottom plot gets more space
  if (n_groups <= 2) {
    layout(matrix(c(1:n_groups, rep(max(1:n_groups) + 1, n_groups)), nrow = 2, byrow = TRUE), 
           heights = c(1, 1.5))
    par(mar = c(4, 4, 3, 2), bg = "white")
  } else {
    layout(matrix(c(1, 2, 3, 4, 5, 5), nrow = 3, byrow = TRUE), 
           heights = c(1.5, 1.5, 1.8))
    par(mar = c(4, 4, 3, 2), bg = "white")
  }
  
  # Top plots: Individual group correlation plots
  for (i in 1:min(4, n_groups)) {
    group <- groups[i]
    weighted_group <- weighted_node_strength[weighted_node_strength$Group == group, ]
    percolation_group <- percolation_node_strength[percolation_node_strength$Group == group, ]
    
    # Match nodes
    common_nodes <- intersect(weighted_group$Node, percolation_group$Node)
    
    if (length(common_nodes) > 0) {
      weighted_vals <- sapply(common_nodes, function(node) {
        weighted_group$Node_Strength[weighted_group$Node == node]
      })
      percolation_vals <- sapply(common_nodes, function(node) {
        percolation_group$Strength[percolation_group$Node == node]
      })
      
      # Convert to ranks and normalize to 0-1 (inverted: rank 1 = n/n, rank 2 = (n-1)/n)
      n_vals <- length(weighted_vals)
      weighted_ranks <- (n_vals - rank(-weighted_vals) + 1) / n_vals
      percolation_ranks <- (n_vals - rank(-percolation_vals) + 1) / n_vals
      
      # Get colors based on brain areas for this group
      plot_colors <- rep("steelblue", length(common_nodes))
      if (!is.null(brain_areas) && !is.null(area_colors)) {
        for (j in seq_along(common_nodes)) {
          node_name <- common_nodes[j]
          for (area_name in names(brain_areas)) {
            if (node_name %in% brain_areas[[area_name]] && area_name %in% names(area_colors)) {
              plot_colors[j] <- area_colors[[area_name]]
              break
            }
          }
        }
      } else if (!is.null(group_colors) && group %in% names(group_colors)) {
        plot_colors <- rep(group_colors[[group]], length(common_nodes))
      }
      
      # Create scatter plot for this group
      plot(weighted_ranks, percolation_ranks,
           main = paste("Node Strength Rank:", group),
           xlab = "Weighted Rank (1=best)",
           ylab = "Percolation Rank (1=best)",
           col = adjustcolor(plot_colors, alpha.f = 0.8),
           pch = 21, cex = 2.2, bg = adjustcolor(plot_colors, alpha.f = 0.6),
           lwd = 2, xlim = c(0, 1), ylim = c(0, 1))
      
      abline(0, 1, lty = 2, col = "darkgray", lwd = 1.5)
      grid(col = "lightgray", lty = "dotted", lwd = 0.5)
      
      # Add correlation
      if (length(weighted_ranks) > 2) {
        corr <- cor(weighted_ranks, percolation_ranks, use = "complete.obs", method = "spearman")
        legend("bottomright", paste("ρ =", round(corr, 3)), bty = "n", cex = 0.9, bg = "white", box.col = "darkgray")
      }
      
      # Add subregion labels for all nodes (improved labeling with connections)
      if (!is.null(brain_areas) && length(common_nodes) > 0) {
        # Label all nodes for better visibility
        n_labels <- length(common_nodes)
        
        for (idx in seq_along(common_nodes)) {
          node_name <- common_nodes[idx]
          x_pos <- weighted_ranks[idx]
          y_pos <- percolation_ranks[idx]
          
          # Use subregion name (node name)
          node_label <- node_name
          
          # Improved positioning to prevent overlap and cropping
          x_range <- max(weighted_ranks) - min(weighted_ranks)
          y_range <- max(percolation_ranks) - min(percolation_ranks)
          
          # More sophisticated offset calculation for better distribution
          angle <- (idx * 137.5) * pi / 180  # Golden angle for better distribution
          radius <- 0.03 * min(x_range, y_range)  # Reduced radius for closer labels
          x_jitter <- radius * cos(angle)
          y_jitter <- radius * sin(angle)
          
          # Overlay labels directly on points
          text(x_pos, y_pos, node_label, 
               cex = 0.8, font = 2, col = "black")
        }
      }
    }
  }
  
  # Bottom plot: Regional averages showing combined weighted+percolation average
  par(mar = c(8, 5, 4, 2))  # More space for labels
  render_combined_rank_by_region_node_strength(weighted_node_strength, percolation_node_strength, brain_areas, area_colors, group_colors)
  
  # Reset layout
  layout(1)
  par(mar = c(5, 4, 4, 2))
}

# Helper function for 5b regional averages - combined weighted+percolation
render_combined_rank_by_region_node_strength <- function(weighted_node_strength, percolation_node_strength, brain_areas, area_colors, group_colors) {
  if (is.null(weighted_node_strength) || is.null(percolation_node_strength) || is.null(brain_areas)) return()
  
  # Create summary data by region and group  
  summary_data <- data.frame()
  groups <- unique(weighted_node_strength$Group)
  
  for (group in groups) {
    weighted_group <- weighted_node_strength[weighted_node_strength$Group == group, ]
    percolation_group <- percolation_node_strength[percolation_node_strength$Group == group, ]
    
    # First, rank ALL nodes within this group (across all regions)
    n_weighted_all <- length(weighted_group$Node_Strength)
    n_percolation_all <- length(percolation_group$Strength)
    
    # Create global ranks for this group
    weighted_global_ranks <- (n_weighted_all - rank(-weighted_group$Node_Strength) + 1) / n_weighted_all
    percolation_global_ranks <- (n_percolation_all - rank(-percolation_group$Strength) + 1) / n_percolation_all
    
    # Add ranks to data frames
    weighted_group$Rank <- weighted_global_ranks
    percolation_group$Rank <- percolation_global_ranks
    
    # Now calculate regional averages
    for (area_name in names(brain_areas)) {
      area_nodes <- brain_areas[[area_name]]
      
      # Get data for nodes in this area
      weighted_area <- weighted_group[weighted_group$Node %in% area_nodes, ]
      percolation_area <- percolation_group[percolation_group$Node %in% area_nodes, ]
      
      if (nrow(weighted_area) > 0 && nrow(percolation_area) > 0) {
        # Get the ranks for nodes in this area (already calculated globally)
        weighted_ranks <- weighted_area$Rank
        percolation_ranks <- percolation_area$Rank
        
        # Combined average of both weighted and percolation ranks
        combined_avg <- (mean(weighted_ranks, na.rm = TRUE) + mean(percolation_ranks, na.rm = TRUE)) / 2
        combined_se <- sd(c(weighted_ranks, percolation_ranks), na.rm = TRUE) / sqrt(length(c(weighted_ranks, percolation_ranks)))
        
        summary_data <- rbind(summary_data, data.frame(
          Group = group,
          Region = area_name,
          Combined_Avg_Rank = combined_avg,
          Combined_SE = combined_se,
          stringsAsFactors = FALSE
        ))
      }
    }
  }
  
  if (nrow(summary_data) > 0) {
    # Prepare data for grouped bar plot (similar to 5a style)
    regions <- unique(summary_data$Region)
    n_groups <- length(groups)
    
    # Create matrix for barplot
    plot_matrix <- matrix(NA, nrow = n_groups, ncol = length(regions))
    se_matrix <- matrix(NA, nrow = n_groups, ncol = length(regions))
    rownames(plot_matrix) <- groups
    colnames(plot_matrix) <- regions
    rownames(se_matrix) <- groups
    colnames(se_matrix) <- regions
    
    for (i in 1:nrow(summary_data)) {
      group <- summary_data$Group[i]
      region <- summary_data$Region[i]
      plot_matrix[group, region] <- summary_data$Combined_Avg_Rank[i]
      se_matrix[group, region] <- summary_data$Combined_SE[i]
    }
    
    # Get group colors
    bar_colors <- rep("steelblue", n_groups)
    if (!is.null(group_colors)) {
      bar_colors <- sapply(groups, function(g) ifelse(g %in% names(group_colors), group_colors[[g]], "steelblue"))
    }
    
    # Create grouped bar plot
    bar_pos <- barplot(plot_matrix,
                      beside = TRUE,
                      main = "Average Node Strength Rank by Region\n(Combined Weighted + Percolation)",
                      xlab = "Brain Region",
                      ylab = "Combined Average Rank (1=best, 0=worst)",
                      col = bar_colors,
                      las = 2,
                      cex.names = 0.9,
                      ylim = c(0, max(plot_matrix + se_matrix, na.rm = TRUE) * 1.1),
                      space = c(0.1, 1))
    
    # Add error bars
    for (i in 1:nrow(plot_matrix)) {
      for (j in 1:ncol(plot_matrix)) {
        if (!is.na(plot_matrix[i, j]) && !is.na(se_matrix[i, j])) {
          segments(bar_pos[i, j], plot_matrix[i, j] - se_matrix[i, j],
                   bar_pos[i, j], plot_matrix[i, j] + se_matrix[i, j])
          segments(bar_pos[i, j] - 0.1, plot_matrix[i, j] - se_matrix[i, j],
                   bar_pos[i, j] + 0.1, plot_matrix[i, j] - se_matrix[i, j])
          segments(bar_pos[i, j] - 0.1, plot_matrix[i, j] + se_matrix[i, j],
                   bar_pos[i, j] + 0.1, plot_matrix[i, j] + se_matrix[i, j])
        }
      }
    }
    
    # Add legend
    legend("topright", legend = groups, fill = bar_colors, 
           bty = "n", cex = 0.9, bg = "white", box.col = "darkgray")
  }
}

# Average eigenvector by region
render_avg_eigenvector_by_region <- function(weighted_eigenvector, node_metrics, brain_areas = NULL, area_colors = NULL, group_colors = NULL) {
  if (is.null(weighted_eigenvector) || is.null(node_metrics) || is.null(brain_areas)) return()
  
  par(mfrow = c(1, 1), mar = c(8, 5, 4, 2), bg = "white")
  
  # Create summary data by region and group
  summary_data <- data.frame()
  
  for (group in unique(weighted_eigenvector$Group)) {
    weighted_group <- weighted_eigenvector[weighted_eigenvector$Group == group, ]
    percolation_group <- node_metrics[node_metrics$Group == group, ]
    
    for (area_name in names(brain_areas)) {
      area_nodes <- brain_areas[[area_name]]
      
      # Get weighted values for nodes in this area
      weighted_vals <- weighted_group$Weighted_Eigenvector[weighted_group$Node %in% area_nodes]
      percolation_vals <- percolation_group$Eigenvector[percolation_group$Node %in% area_nodes]
      
      if (length(weighted_vals) > 0 && length(percolation_vals) > 0) {
        summary_data <- rbind(summary_data, data.frame(
          Group = group,
          Region = area_name,
          Weighted_Mean = mean(weighted_vals, na.rm = TRUE),
          Weighted_SE = sd(weighted_vals, na.rm = TRUE) / sqrt(length(weighted_vals)),
          Percolation_Mean = mean(percolation_vals, na.rm = TRUE),
          Percolation_SE = sd(percolation_vals, na.rm = TRUE) / sqrt(length(percolation_vals))
        ))
      }
    }
  }
  
  if (nrow(summary_data) > 0) {
    # Create combined labels
    summary_data$Label <- paste(summary_data$Group, summary_data$Region, sep = "_")
    
    # Create grouped bar plot
    bar_pos <- barplot(rbind(summary_data$Weighted_Mean, summary_data$Percolation_Mean),
                      beside = TRUE,
                      names.arg = summary_data$Label,
                      main = "Average Eigenvector Centrality by Region and Group",
                      ylab = "Average Eigenvector Centrality",
                      col = c("steelblue", "darkorange2"),
                      las = 2, cex.names = 0.8)
    
    # Add error bars
    for (i in 1:nrow(summary_data)) {
      # Weighted error bars
      segments(bar_pos[1, i], summary_data$Weighted_Mean[i] - summary_data$Weighted_SE[i],
               bar_pos[1, i], summary_data$Weighted_Mean[i] + summary_data$Weighted_SE[i], lwd = 2, col = "black")
      # Percolation error bars  
      segments(bar_pos[2, i], summary_data$Percolation_Mean[i] - summary_data$Percolation_SE[i],
               bar_pos[2, i], summary_data$Percolation_Mean[i] + summary_data$Percolation_SE[i])
    }
    
    legend("topright", c("Weighted", "Percolation"), fill = c("steelblue", "darkorange2"), 
           bty = "n", cex = 1.0, bg = "white", box.col = "darkgray")
  }
}

# Helper function for 5c regional single bar per region
render_single_bar_per_region_eigenvector <- function(weighted_eigenvector, node_metrics, brain_areas, area_colors, group_colors) {
  if (is.null(weighted_eigenvector) || is.null(node_metrics) || is.null(brain_areas)) return()
  
  # Create summary data by region (averaged across all groups)
  summary_data <- data.frame()
  
  for (area_name in names(brain_areas)) {
    area_nodes <- brain_areas[[area_name]]
    
    # Get all data for nodes in this area across all groups
    weighted_area_vals <- c()
    percolation_area_vals <- c()
    
    for (group in unique(weighted_eigenvector$Group)) {
      weighted_group <- weighted_eigenvector[weighted_eigenvector$Group == group, ]
      percolation_group <- node_metrics[node_metrics$Group == group, ]
      
      # Get weighted values for nodes in this area
      weighted_vals <- weighted_group$Weighted_Eigenvector[weighted_group$Node %in% area_nodes]
      percolation_vals <- percolation_group$Eigenvector[percolation_group$Node %in% area_nodes]
      
      weighted_area_vals <- c(weighted_area_vals, weighted_vals)
      percolation_area_vals <- c(percolation_area_vals, percolation_vals)
    }
    
    if (length(weighted_area_vals) > 0 && length(percolation_area_vals) > 0) {
      # Combined average of both weighted and percolation values
      combined_avg <- (mean(weighted_area_vals, na.rm = TRUE) + mean(percolation_area_vals, na.rm = TRUE)) / 2
      combined_se <- sd(c(weighted_area_vals, percolation_area_vals), na.rm = TRUE) / sqrt(length(c(weighted_area_vals, percolation_area_vals)))
      
      summary_data <- rbind(summary_data, data.frame(
        Region = area_name,
        Combined_Avg = combined_avg,
        Combined_SE = combined_se,
        stringsAsFactors = FALSE
      ))
    }
  }
  
  if (nrow(summary_data) > 0) {
    # Sort by average value for better visualization
    summary_data <- summary_data[order(-summary_data$Combined_Avg), ]
    
    # Get region colors
    bar_colors <- rep("steelblue", nrow(summary_data))
    if (!is.null(area_colors)) {
      bar_colors <- sapply(summary_data$Region, function(region) {
        ifelse(region %in% names(area_colors), area_colors[[region]], "steelblue")
      })
    }
    
    # Create single bar plot
    bar_pos <- barplot(summary_data$Combined_Avg,
                      names.arg = summary_data$Region,
                      main = "Average Eigenvector by Region\n(Combined Weighted + Percolation)",
                      xlab = "Brain Region",
                      ylab = "Combined Average Eigenvector",
                      col = bar_colors,
                      las = 2,
                      cex.names = 0.9,
                      ylim = c(0, max(summary_data$Combined_Avg + summary_data$Combined_SE, na.rm = TRUE) * 1.1))
    
    # Add error bars
    for (i in 1:nrow(summary_data)) {
      segments(bar_pos[i], summary_data$Combined_Avg[i] - summary_data$Combined_SE[i],
               bar_pos[i], summary_data$Combined_Avg[i] + summary_data$Combined_SE[i])
      segments(bar_pos[i] - 0.1, summary_data$Combined_Avg[i] - summary_data$Combined_SE[i],
               bar_pos[i] + 0.1, summary_data$Combined_Avg[i] - summary_data$Combined_SE[i])
      segments(bar_pos[i] - 0.1, summary_data$Combined_Avg[i] + summary_data$Combined_SE[i],
               bar_pos[i] + 0.1, summary_data$Combined_Avg[i] + summary_data$Combined_SE[i])
    }
  }
}

# Percolation-based strength vs eigenvector relationship (matching 4c and 5a-d style)
render_percolation_strength_eigenvector_relationship <- function(node_metrics, group_colors, brain_areas = NULL, area_colors = NULL) {
  if (is.null(node_metrics)) return()
  
  par(mfrow = c(2, 2), mar = c(5, 5, 4, 3))
  
  groups <- unique(node_metrics$Group)
  
  for (group in groups[1:min(4, length(groups))]) {
    group_data <- node_metrics[node_metrics$Group == group, ]
    
    if (nrow(group_data) > 0 && "Strength" %in% names(group_data) && "Eigenvector" %in% names(group_data)) {
      # Get colors based on brain areas if available, otherwise use group colors
      plot_colors <- rep("steelblue", nrow(group_data))
      if (!is.null(brain_areas) && !is.null(area_colors)) {
        for (i in seq_along(group_data$Node)) {
          node_name <- group_data$Node[i]
          for (area_name in names(brain_areas)) {
            if (node_name %in% brain_areas[[area_name]] && area_name %in% names(area_colors)) {
              plot_colors[i] <- area_colors[[area_name]]
              break
            }
          }
        }
      } else if(!is.null(group_colors) && group %in% names(group_colors)) {
        plot_colors <- rep(group_colors[[group]], nrow(group_data))
      }
      
      # Create plot with expanded y-axis to accommodate labels
      y_range <- range(group_data$Eigenvector)
      y_padding <- (y_range[2] - y_range[1]) * 0.2
      
      plot(group_data$Strength, group_data$Eigenvector,
           main = paste("Percolation: Strength vs Eigenvector -", group),
           xlab = "Percolation Node Strength", ylab = "Percolation Eigenvector",
           pch = 21, cex = 2.2, bg = adjustcolor(plot_colors, alpha.f = 0.6),
           col = adjustcolor(plot_colors, alpha.f = 0.8), lwd = 2,
           ylim = c(y_range[1], y_range[2] + y_padding))
      
      # Add grid to match 5a-d style
      grid(col = "lightgray", lty = "dotted", lwd = 0.5)
      
      # Add labels overlaid directly on points (matching 5a-d style)
      if (nrow(group_data) > 0) {
        for (i in seq_len(nrow(group_data))) {
          x_pos <- group_data$Strength[i]
          y_pos <- group_data$Eigenvector[i]
          
          # Overlay labels directly on points
          text(x_pos, y_pos, 
               labels = group_data$Node[i], 
               cex = 0.8, font = 2, col = "black")
        }
      }
      
      # Add trend line
      if (nrow(group_data) > 2) {
        tryCatch({
          fit <- lm(Eigenvector ~ Strength, data = group_data)
          abline(fit, col = "darkgray", lty = 2, lwd = 1.5)
          # Add R-squared and correlation
          r_squared <- round(summary(fit)$r.squared, 3)
          corr <- cor(group_data$Strength, group_data$Eigenvector, use = "complete.obs")
          legend("bottomright", paste("r =", round(corr, 3), "\nR² =", r_squared), bty = "n", cex = 0.9, bg = "white", box.col = "darkgray")
        }, error = function(e) {})
      }
    } else {
      plot(1, type = "n", main = paste("No Data -", group))
      text(1, 1, "Missing percolation metrics")
    }
  }
}

render_hub_ranking_comparison <- function(cross_method_comparison, group_colors) {
  if (is.null(cross_method_comparison) || is.null(cross_method_comparison$hub_rankings)) return()
  
  hub_data <- cross_method_comparison$hub_rankings
  
  par(mfrow = c(1, 1), mar = c(8, 4, 4, 2))
  
  if (is.data.frame(hub_data) && nrow(hub_data) > 0) {
    # Simple hub ranking visualization
    if ("Hub_Score" %in% names(hub_data) && "Node" %in% names(hub_data)) {
      top_hubs <- head(hub_data[order(hub_data$Hub_Score, decreasing = TRUE), ], 10)
      
      barplot(top_hubs$Hub_Score,
              names.arg = top_hubs$Node,
              main = "Top Hub Nodes (Cross-Method)",
              ylab = "Hub Score",
              col = "coral",
              las = 2, cex.names = 0.8)
    } else {
      plot(1, type = "n", main = "Hub Rankings")
      text(1, 1, "Hub ranking data format not recognized")
    }
  } else {
    plot(1, type = "n", main = "Hub Rankings")
    text(1, 1, "No hub ranking data available")
  }
}

# ==============================================================================
# CONSERVATION ANALYSIS VISUALIZATIONS
# ==============================================================================

# Render network similarity heatmap
render_network_similarity_heatmap <- function(conservation_results) {
  if(is.null(conservation_results) || is.null(conservation_results$similarity_matrix)) {
    plot.new()
    text(0.5, 0.5, "No conservation analysis available\n(Requires at least 2 groups)", cex = 1.2)
    return()
  }
  
  similarity_matrix <- conservation_results$similarity_matrix
  
  # Check if we have variance for clustering
  upper_tri_values <- similarity_matrix[upper.tri(similarity_matrix)]
  has_variance <- FALSE
  
  if(length(upper_tri_values) > 1) {
    similarity_var <- var(upper_tri_values, na.rm = TRUE)
    has_variance <- !is.na(similarity_var) && similarity_var > 0.001
  }
  
  # Use pheatmap if available, otherwise base heatmap
  if(requireNamespace("pheatmap", quietly = TRUE)) {
    if(has_variance) {
      pheatmap::pheatmap(similarity_matrix,
                         main = "Network Similarity (Jaccard Index)",
                         color = colorRampPalette(c("white", "lightblue", "darkblue"))(100),
                         display_numbers = TRUE,
                         number_format = "%.3f",
                         cluster_rows = TRUE,
                         cluster_cols = TRUE,
                         fontsize = 12)
    } else {
      pheatmap::pheatmap(similarity_matrix,
                         main = "Network Similarity (Jaccard Index)",
                         color = colorRampPalette(c("white", "lightblue", "darkblue"))(100),
                         display_numbers = TRUE,
                         number_format = "%.3f",
                         cluster_rows = FALSE,
                         cluster_cols = FALSE,
                         fontsize = 12)
    }
  } else {
    # Fallback to base R heatmap
    heatmap(similarity_matrix,
            main = "Network Similarity (Jaccard Index)",
            col = colorRampPalette(c("white", "lightblue", "darkblue"))(100),
            scale = "none",
            margins = c(10, 10))
  }
}

# Render hub conservation heatmap
render_hub_conservation_heatmap <- function(conservation_results) {
  if(is.null(conservation_results) || is.null(conservation_results$hub_conservation)) {
    plot.new()
    text(0.5, 0.5, "No hub conservation analysis available", cex = 1.2)
    return()
  }
  
  hub_matrix <- conservation_results$hub_conservation$hub_conservation_matrix
  
  # Check variance
  hub_upper_tri <- hub_matrix[upper.tri(hub_matrix)]
  hub_has_variance <- FALSE
  
  if(length(hub_upper_tri) > 1) {
    hub_var <- var(hub_upper_tri, na.rm = TRUE)
    hub_has_variance <- !is.na(hub_var) && hub_var > 0.001
  }
  
  if(requireNamespace("pheatmap", quietly = TRUE)) {
    if(hub_has_variance) {
      pheatmap::pheatmap(hub_matrix,
                         main = "Hub Conservation (Top 5 Hubs Overlap)",
                         color = colorRampPalette(c("white", "orange", "red"))(100),
                         display_numbers = TRUE,
                         number_format = "%.2f",
                         cluster_rows = TRUE,
                         cluster_cols = TRUE,
                         fontsize = 12)
    } else {
      pheatmap::pheatmap(hub_matrix,
                         main = "Hub Conservation (Top 5 Hubs Overlap)",
                         color = colorRampPalette(c("white", "orange", "red"))(100),
                         display_numbers = TRUE,
                         number_format = "%.2f",
                         cluster_rows = FALSE,
                         cluster_cols = FALSE,
                         fontsize = 12)
    }
  } else {
    heatmap(hub_matrix,
            main = "Hub Conservation (Top 5 Hubs Overlap)",
            col = colorRampPalette(c("white", "orange", "red"))(100),
            scale = "none",
            margins = c(10, 10))
  }
}

# Render edge conservation consensus heatmap
render_edge_conservation_heatmap <- function(conservation_results) {
  if(is.null(conservation_results) || is.null(conservation_results$edge_conservation)) {
    plot.new()
    text(0.5, 0.5, "No edge conservation analysis available", cex = 1.2)
    return()
  }
  
  consensus_matrix <- conservation_results$edge_conservation$consensus_matrix
  
  if(is.null(consensus_matrix) || nrow(consensus_matrix) == 0) {
    plot.new()
    text(0.5, 0.5, "No conserved edges found", cex = 1.2)
    return()
  }
  
  # Check variance
  consensus_values <- as.vector(consensus_matrix)
  consensus_has_variance <- FALSE
  
  if(length(consensus_values) > 1) {
    consensus_var <- var(consensus_values, na.rm = TRUE)
    consensus_has_variance <- !is.na(consensus_var) && consensus_var > 0.001
  }
  
  if(requireNamespace("pheatmap", quietly = TRUE)) {
    if(consensus_has_variance) {
      pheatmap::pheatmap(consensus_matrix,
                         main = "Edge Conservation Consensus",
                         color = colorRampPalette(c("white", "yellow", "orange", "red"))(100),
                         cluster_rows = TRUE,
                         cluster_cols = TRUE,
                         show_rownames = TRUE,
                         show_colnames = TRUE,
                         fontsize = 8)
    } else {
      pheatmap::pheatmap(consensus_matrix,
                         main = "Edge Conservation Consensus",
                         color = colorRampPalette(c("white", "yellow", "orange", "red"))(100),
                         cluster_rows = FALSE,
                         cluster_cols = FALSE,
                         show_rownames = TRUE,
                         show_colnames = TRUE,
                         fontsize = 8)
    }
  } else {
    heatmap(consensus_matrix,
            main = "Edge Conservation Consensus",
            col = colorRampPalette(c("white", "yellow", "orange", "red"))(100),
            scale = "none",
            margins = c(8, 8))
  }
}

# ==============================================================================
# SUBREGION PLOTS FOR CROSS-METHOD COMPARISON (5a-d)
# ==============================================================================

# Render combined subregion plot for eigenvector comparison
render_subregion_eigenvector_comparison <- function(weighted_eigenvector, node_metrics, brain_areas, area_colors, group_colors, use_ranks = FALSE) {
  if(is.null(weighted_eigenvector) || is.null(node_metrics) || is.null(brain_areas)) {
    plot.new()
    text(0.5, 0.5, "Data not available for subregion comparison", cex = 1.2)
    return()
  }
  
  # Combine data
  combined_data <- data.frame()
  
  for(group in unique(weighted_eigenvector$Group)) {
    weighted_data <- weighted_eigenvector[weighted_eigenvector$Group == group, ]
    percolation_data <- node_metrics[node_metrics$Group == group, ]
    
    common_nodes <- intersect(weighted_data$Node, percolation_data$Node)
    
    for(node in common_nodes) {
      weighted_val <- weighted_data$Weighted_Eigenvector[weighted_data$Node == node]
      percolation_val <- percolation_data$Eigenvector[percolation_data$Node == node]
      
      # Find brain area for this node
      brain_area <- "Other"
      for(area_name in names(brain_areas)) {
        if(node %in% brain_areas[[area_name]]) {
          brain_area <- area_name
          break
        }
      }
      
      combined_data <- rbind(combined_data, data.frame(
        Group = group,
        Node = node,
        Brain_Area = brain_area,
        Weighted = weighted_val,
        Percolation = percolation_val,
        stringsAsFactors = FALSE
      ))
    }
  }
  
  if(nrow(combined_data) == 0) {
    plot.new()
    text(0.5, 0.5, "No common nodes for comparison", cex = 1.2)
    return()
  }
  
  # Convert to ranks if requested
  if(use_ranks) {
    for(group in unique(combined_data$Group)) {
      group_data <- combined_data[combined_data$Group == group, ]
      if(nrow(group_data) > 1) {
        n_nodes <- nrow(group_data)
        # Convert to ranks and normalize to 0-1 (inverted: rank 1 = n/n, rank 2 = (n-1)/n)
        combined_data$Weighted[combined_data$Group == group] <- (n_nodes - rank(-group_data$Weighted) + 1) / n_nodes
        combined_data$Percolation[combined_data$Group == group] <- (n_nodes - rank(-group_data$Percolation) + 1) / n_nodes
      }
    }
  }
  
  # Create faceted barplot
  par(mfrow = c(2, 1), mar = c(6, 4, 3, 2))
  
  # Plot 1: Weighted eigenvector by subregion
  node_order <- unique(combined_data$Node)
  brain_area_order <- names(brain_areas)
  
  # Order nodes by brain area
  combined_data$Node <- factor(combined_data$Node, levels = node_order)
  combined_data$Brain_Area <- factor(combined_data$Brain_Area, levels = brain_area_order)
  combined_data <- combined_data[order(combined_data$Brain_Area, combined_data$Node), ]
  
  # Create grouped barplot for weighted
  groups <- unique(combined_data$Group)
  n_groups <- length(groups)
  nodes <- unique(combined_data$Node)
  n_nodes <- length(nodes)
  
  # Prepare matrix for barplot
  weighted_matrix <- matrix(NA, nrow = n_groups, ncol = n_nodes)
  rownames(weighted_matrix) <- groups
  colnames(weighted_matrix) <- nodes
  
  for(i in 1:nrow(combined_data)) {
    weighted_matrix[combined_data$Group[i], combined_data$Node[i]] <- combined_data$Weighted[i]
  }
  
  # Plot weighted
  barplot(weighted_matrix, beside = TRUE,
          main = "Weighted Eigenvector Across All Brain Regions",
          xlab = "", ylab = "Weighted Eigenvector",
          col = group_colors[rownames(weighted_matrix)],
          las = 2, cex.names = 0.7,
          legend.text = rownames(weighted_matrix),
          args.legend = list(x = "topright", bty = "n", cex = 0.8))
  
  # Add brain area labels
  area_positions <- tapply(seq_along(nodes), 
                          combined_data$Brain_Area[match(nodes, combined_data$Node)],
                          mean)
  axis(1, at = area_positions * (n_groups + 1) - n_groups/2, 
       labels = names(area_positions), line = 3, lwd = 0, cex.axis = 0.9)
  
  # Plot 2: Percolation eigenvector
  percolation_matrix <- matrix(NA, nrow = n_groups, ncol = n_nodes)
  rownames(percolation_matrix) <- groups
  colnames(percolation_matrix) <- nodes
  
  for(i in 1:nrow(combined_data)) {
    percolation_matrix[combined_data$Group[i], combined_data$Node[i]] <- combined_data$Percolation[i]
  }
  
  barplot(percolation_matrix, beside = TRUE,
          main = "Percolation Eigenvector Across All Brain Regions",
          xlab = "", ylab = "Percolation Eigenvector",
          col = group_colors[rownames(percolation_matrix)],
          las = 2, cex.names = 0.7,
          legend.text = rownames(percolation_matrix),
          args.legend = list(x = "topright", bty = "n", cex = 0.8))
  
  # Add brain area labels
  axis(1, at = area_positions * (n_groups + 1) - n_groups/2,
       labels = names(area_positions), line = 3, lwd = 0, cex.axis = 0.9)
}

# Render combined subregion plot for node strength comparison
render_subregion_strength_comparison <- function(node_strength, node_metrics, brain_areas, area_colors, group_colors, use_ranks = FALSE) {
  if(is.null(node_strength) || is.null(node_metrics) || is.null(brain_areas)) {
    plot.new()
    text(0.5, 0.5, "Data not available for subregion comparison", cex = 1.2)
    return()
  }
  
  # Similar implementation but for node strength
  combined_data <- data.frame()
  
  for(group in unique(node_strength$Group)) {
    strength_data <- node_strength[node_strength$Group == group, ]
    percolation_data <- node_metrics[node_metrics$Group == group, ]
    
    common_nodes <- intersect(strength_data$Node, percolation_data$Node)
    
    for(node in common_nodes) {
      weighted_val <- strength_data$Node_Strength[strength_data$Node == node]
      percolation_val <- percolation_data$Strength[percolation_data$Node == node]
      
      # Find brain area
      brain_area <- "Other"
      for(area_name in names(brain_areas)) {
        if(node %in% brain_areas[[area_name]]) {
          brain_area <- area_name
          break
        }
      }
      
      combined_data <- rbind(combined_data, data.frame(
        Group = group,
        Node = node,
        Brain_Area = brain_area,
        Weighted = weighted_val,
        Percolation = percolation_val,
        stringsAsFactors = FALSE
      ))
    }
  }
  
  if(nrow(combined_data) == 0) {
    plot.new()
    text(0.5, 0.5, "No common nodes for comparison", cex = 1.2)
    return()
  }
  
  # Convert to ranks if requested
  if(use_ranks) {
    for(group in unique(combined_data$Group)) {
      group_data <- combined_data[combined_data$Group == group, ]
      if(nrow(group_data) > 1) {
        n_nodes <- nrow(group_data)
        # Convert to ranks and normalize to 0-1 (inverted: rank 1 = n/n, rank 2 = (n-1)/n)
        combined_data$Weighted[combined_data$Group == group] <- (n_nodes - rank(-group_data$Weighted) + 1) / n_nodes
        combined_data$Percolation[combined_data$Group == group] <- (n_nodes - rank(-group_data$Percolation) + 1) / n_nodes
      }
    }
  }
  
  # Create similar plots for node strength
  par(mfrow = c(2, 1), mar = c(6, 4, 3, 2))
  
  # Order and prepare data
  node_order <- unique(combined_data$Node)
  brain_area_order <- names(brain_areas)
  combined_data$Node <- factor(combined_data$Node, levels = node_order)
  combined_data$Brain_Area <- factor(combined_data$Brain_Area, levels = brain_area_order)
  combined_data <- combined_data[order(combined_data$Brain_Area, combined_data$Node), ]
  
  groups <- unique(combined_data$Group)
  n_groups <- length(groups)
  nodes <- unique(combined_data$Node)
  n_nodes <- length(nodes)
  
  # Weighted strength matrix
  weighted_matrix <- matrix(NA, nrow = n_groups, ncol = n_nodes)
  rownames(weighted_matrix) <- groups
  colnames(weighted_matrix) <- nodes
  
  for(i in 1:nrow(combined_data)) {
    weighted_matrix[combined_data$Group[i], combined_data$Node[i]] <- combined_data$Weighted[i]
  }
  
  barplot(weighted_matrix, beside = TRUE,
          main = "Weighted Node Strength Across All Brain Regions",
          xlab = "", ylab = "Weighted Node Strength",
          col = group_colors[rownames(weighted_matrix)],
          las = 2, cex.names = 0.7,
          legend.text = rownames(weighted_matrix),
          args.legend = list(x = "topright", bty = "n", cex = 0.8))
  
  # Add brain area labels
  area_positions <- tapply(seq_along(nodes),
                          combined_data$Brain_Area[match(nodes, combined_data$Node)],
                          mean)
  axis(1, at = area_positions * (n_groups + 1) - n_groups/2,
       labels = names(area_positions), line = 3, lwd = 0, cex.axis = 0.9)
  
  # Percolation strength matrix
  percolation_matrix <- matrix(NA, nrow = n_groups, ncol = n_nodes)
  rownames(percolation_matrix) <- groups
  colnames(percolation_matrix) <- nodes
  
  for(i in 1:nrow(combined_data)) {
    percolation_matrix[combined_data$Group[i], combined_data$Node[i]] <- combined_data$Percolation[i]
  }
  
  barplot(percolation_matrix, beside = TRUE,
          main = "Percolation Node Strength Across All Brain Regions",
          xlab = "", ylab = "Percolation Node Strength",
          col = group_colors[rownames(percolation_matrix)],
          las = 2, cex.names = 0.7,
          legend.text = rownames(percolation_matrix),
          args.legend = list(x = "topright", bty = "n", cex = 0.8))
  
  # Add brain area labels
  axis(1, at = area_positions * (n_groups + 1) - n_groups/2,
       labels = names(area_positions), line = 3, lwd = 0, cex.axis = 0.9)
}
# ==============================================================================
# WEIGHTED CONSERVATION VISUALIZATION FUNCTIONS
# ==============================================================================

# Render weighted similarity heatmap
render_weighted_similarity_heatmap <- function(weighted_conservation_results, group_colors) {
  if(is.null(weighted_conservation_results) || is.null(weighted_conservation_results$similarity_matrix)) {
    plot.new()
    text(0.5, 0.5, "Weighted conservation analysis not available", cex = 1.2)
    return()
  }
  
  similarity_matrix <- weighted_conservation_results$similarity_matrix
  
  if(nrow(similarity_matrix) < 2) {
    plot.new()
    text(0.5, 0.5, "Need at least 2 groups for similarity analysis", cex = 1.2)
    return()
  }
  
  # Create heatmap using pheatmap
  if(!require(pheatmap, quietly = TRUE)) {
    # Fallback to base R heatmap
    heatmap(similarity_matrix, 
            main = "Weighted Network Similarity (Jaccard)",
            col = colorRampPalette(c("white", "lightblue", "darkblue"))(100),
            symm = TRUE)
  } else {
    pheatmap(similarity_matrix,
             main = "Weighted Network Similarity (Jaccard)",
             color = colorRampPalette(c("white", "lightblue", "darkblue"))(100),
             display_numbers = TRUE,
             number_format = "%.3f",
             cluster_rows = TRUE,
             cluster_cols = TRUE,
             clustering_distance_rows = "euclidean",
             clustering_distance_cols = "euclidean", 
             clustering_method = "complete",
             fontsize = 12,
             fontsize_number = 10)
  }
}

# Render weighted hub conservation heatmap
render_weighted_hub_conservation_heatmap <- function(weighted_conservation_results, group_colors) {
  if(is.null(weighted_conservation_results) || is.null(weighted_conservation_results$hub_conservation)) {
    plot.new()
    text(0.5, 0.5, "Weighted hub conservation analysis not available", cex = 1.2)
    return()
  }
  
  hub_conservation <- weighted_conservation_results$hub_conservation
  
  if(is.null(hub_conservation$eigenvector_hub_conservation) && 
     is.null(hub_conservation$strength_hub_conservation)) {
    plot.new()
    text(0.5, 0.5, "Hub conservation matrices not available", cex = 1.2)
    return()
  }
  
  # Create side-by-side plots
  par(mfrow = c(1, 2), mar = c(4, 4, 3, 2))
  
  # Eigenvector hub conservation
  if(!is.null(hub_conservation$eigenvector_hub_conservation)) {
    eig_matrix <- hub_conservation$eigenvector_hub_conservation
    
    if(!require(pheatmap, quietly = TRUE)) {
      heatmap(eig_matrix, 
              main = "Eigenvector Hub Conservation",
              col = colorRampPalette(c("white", "lightgreen", "darkgreen"))(100),
              symm = TRUE)
    } else {
      pheatmap(eig_matrix,
               main = "Eigenvector Hub Conservation",
               color = colorRampPalette(c("white", "lightgreen", "darkgreen"))(100),
               display_numbers = TRUE,
               number_format = "%.3f",
               cluster_rows = FALSE,
               cluster_cols = FALSE,
               fontsize = 10,
               fontsize_number = 8)
    }
  }
  
  # Node strength hub conservation
  if(!is.null(hub_conservation$strength_hub_conservation)) {
    str_matrix <- hub_conservation$strength_hub_conservation
    
    if(!require(pheatmap, quietly = TRUE)) {
      heatmap(str_matrix, 
              main = "Node Strength Hub Conservation",
              col = colorRampPalette(c("white", "orange", "red"))(100),
              symm = TRUE)
    } else {
      pheatmap(str_matrix,
               main = "Node Strength Hub Conservation",
               color = colorRampPalette(c("white", "orange", "red"))(100),
               display_numbers = TRUE,
               number_format = "%.3f",
               cluster_rows = FALSE,
               cluster_cols = FALSE,
               fontsize = 10,
               fontsize_number = 8)
    }
  }
  
  par(mfrow = c(1, 1))
}

# Render weighted edge statistics plot
render_weighted_edge_statistics_plot <- function(weighted_conservation_results, group_colors) {
  if(is.null(weighted_conservation_results) || is.null(weighted_conservation_results$edge_statistics)) {
    plot.new()
    text(0.5, 0.5, "Weighted edge statistics not available", cex = 1.2)
    return()
  }
  
  edge_stats <- weighted_conservation_results$edge_statistics
  
  if(nrow(edge_stats) == 0) {
    plot.new()
    text(0.5, 0.5, "No edge statistics available", cex = 1.2)
    return()
  }
  
  # Create multi-panel plot
  par(mfrow = c(2, 2), mar = c(4, 4, 3, 2))
  
  # Plot 1: Mean edge weights by group
  barplot(edge_stats$Mean_Weight, 
          names.arg = edge_stats$Group,
          main = "Mean Edge Weights",
          ylab = "Mean |Correlation|",
          col = group_colors[edge_stats$Group],
          las = 2)
  
  # Plot 2: Total edge weight by group
  barplot(edge_stats$Total_Weight, 
          names.arg = edge_stats$Group,
          main = "Total Edge Weight",
          ylab = "Sum of |Correlations|",
          col = group_colors[edge_stats$Group],
          las = 2)
  
  # Plot 3: Distribution of strong edges
  strong_edge_matrix <- as.matrix(edge_stats[, c("Strong_Edges_03", "Strong_Edges_05", "Strong_Edges_07")])
  rownames(strong_edge_matrix) <- edge_stats$Group
  colnames(strong_edge_matrix) <- c(">0.3", ">0.5", ">0.7")
  
  barplot(t(strong_edge_matrix), 
          beside = TRUE,
          main = "Strong Edge Counts",
          ylab = "Number of Edges",
          col = c("lightblue", "blue", "darkblue"),
          legend.text = TRUE,
          args.legend = list(x = "topright", bty = "n", cex = 0.8),
          las = 2,
          names.arg = edge_stats$Group)
  
  # Plot 4: Edge weight variability
  barplot(edge_stats$SD_Weight, 
          names.arg = edge_stats$Group,
          main = "Edge Weight Variability",
          ylab = "SD of |Correlations|",
          col = group_colors[edge_stats$Group],
          las = 2)
  
  par(mfrow = c(1, 1))
}

# ==============================================================================  
# CONSERVATION COMPARISON VISUALIZATION FUNCTIONS
# ==============================================================================

# Render conservation method comparison plots
render_conservation_comparison_plots <- function(conservation_results, weighted_conservation_results, group_colors) {
  if(is.null(conservation_results) || is.null(weighted_conservation_results)) {
    plot.new()
    text(0.5, 0.5, "Conservation analysis results not available", cex = 1.2)
    return()
  }
  
  # Create side-by-side similarity matrix comparison
  par(mfrow = c(1, 2), mar = c(4, 4, 3, 2))
  
  # Thresholded conservation similarity
  if(!is.null(conservation_results$similarity_matrix)) {
    threshold_matrix <- conservation_results$similarity_matrix
    
    if(!require(pheatmap, quietly = TRUE)) {
      heatmap(threshold_matrix, 
              main = "Thresholded Conservation",
              col = colorRampPalette(c("white", "lightcoral", "red"))(100),
              symm = TRUE)
    } else {
      pheatmap(threshold_matrix,
               main = "Thresholded Conservation",
               color = colorRampPalette(c("white", "lightcoral", "red"))(100),
               display_numbers = TRUE,
               number_format = "%.3f",
               cluster_rows = FALSE,
               cluster_cols = FALSE,
               fontsize = 10,
               fontsize_number = 8)
    }
  }
  
  # Weighted conservation similarity  
  if(!is.null(weighted_conservation_results$similarity_matrix)) {
    weighted_matrix <- weighted_conservation_results$similarity_matrix
    
    if(!require(pheatmap, quietly = TRUE)) {
      heatmap(weighted_matrix, 
              main = "Weighted Conservation",
              col = colorRampPalette(c("white", "lightblue", "blue"))(100),
              symm = TRUE)
    } else {
      pheatmap(weighted_matrix,
               main = "Weighted Conservation", 
               color = colorRampPalette(c("white", "lightblue", "blue"))(100),
               display_numbers = TRUE,
               number_format = "%.3f",
               cluster_rows = FALSE,
               cluster_cols = FALSE,
               fontsize = 10,
               fontsize_number = 8)
    }
  }
  
  par(mfrow = c(1, 1))
}

# Render conservation correlation analysis
render_conservation_correlation_analysis <- function(conservation_results, weighted_conservation_results, group_colors) {
  if(is.null(conservation_results) || is.null(weighted_conservation_results)) {
    plot.new()
    text(0.5, 0.5, "Conservation analysis results not available", cex = 1.2)
    return()
  }
  
  threshold_matrix <- conservation_results$similarity_matrix
  weighted_matrix <- weighted_conservation_results$similarity_matrix
  
  if(is.null(threshold_matrix) || is.null(weighted_matrix)) {
    plot.new()
    text(0.5, 0.5, "Similarity matrices not available", cex = 1.2)
    return()
  }
  
  # Extract upper triangular values (excluding diagonal)
  n_groups <- nrow(threshold_matrix)
  if(n_groups != nrow(weighted_matrix) || n_groups < 2) {
    plot.new()
    text(0.5, 0.5, "Incompatible similarity matrices", cex = 1.2)
    return()
  }
  
  # Get upper triangular indices
  upper_tri_indices <- which(upper.tri(threshold_matrix), arr.ind = TRUE)
  
  threshold_values <- threshold_matrix[upper.tri(threshold_matrix)]
  weighted_values <- weighted_matrix[upper.tri(weighted_matrix)]
  
  if(length(threshold_values) != length(weighted_values)) {
    plot.new()
    text(0.5, 0.5, "Mismatched similarity matrix dimensions", cex = 1.2)
    return()
  }
  
  # Create group pair labels
  group_pairs <- paste(rownames(threshold_matrix)[upper_tri_indices[,1]], 
                      "vs", 
                      rownames(threshold_matrix)[upper_tri_indices[,2]])
  
  # Identify most similar and most different pairs
  avg_similarity <- (threshold_values + weighted_values) / 2
  most_similar_idx <- which.max(avg_similarity)
  most_different_idx <- which.min(avg_similarity)
  
  most_similar_pair <- group_pairs[most_similar_idx]
  most_different_pair <- group_pairs[most_different_idx]
  
  # Create plots with improved margins to prevent cropping
  par(mfrow = c(2, 1), mar = c(6, 5, 4, 3), oma = c(0, 0, 0, 0))
  
  # Scatter plot with better spacing
  plot(threshold_values, weighted_values,
       xlab = "Thresholded Conservation (Jaccard)",
       ylab = "Weighted Conservation (Jaccard)",
       main = "Conservation Method Correlation",
       pch = 21, cex = 1.8,
       bg = "lightblue", col = "darkblue",
       xlim = c(min(threshold_values) - 0.05, max(threshold_values) + 0.05),
       ylim = c(min(weighted_values) - 0.05, max(weighted_values) + 0.05))
  
  # Highlight most similar and different points
  points(threshold_values[most_similar_idx], weighted_values[most_similar_idx], 
         pch = 21, cex = 2.2, bg = "lightgreen", col = "darkgreen", lwd = 2)
  points(threshold_values[most_different_idx], weighted_values[most_different_idx], 
         pch = 21, cex = 2.2, bg = "lightcoral", col = "darkred", lwd = 2)
  
  # Add point labels with offset to prevent overlap
  text(threshold_values, weighted_values, 
       labels = group_pairs, 
       pos = 3, offset = 0.8, cex = 0.8, col = "black")
  
  # Add correlation line and statistics
  if(length(threshold_values) > 2) {
    abline(lm(weighted_values ~ threshold_values), col = "red", lwd = 2)
    
    # Calculate correlation
    cor_val <- cor(threshold_values, weighted_values, use = "complete.obs")
    if(!is.na(cor_val)) {
      legend("topleft", 
             legend = c(paste("r =", round(cor_val, 3)),
                       paste("Most Similar:", most_similar_pair),
                       paste("Most Different:", most_different_pair)),
             bty = "n", cex = 0.9, text.col = c("black", "darkgreen", "darkred"))
    }
  }
  
  # Difference plot with better formatting
  differences <- weighted_values - threshold_values
  
  # Truncate long labels for better display
  short_labels <- sapply(group_pairs, function(x) {
    if(nchar(x) > 15) {
      parts <- strsplit(x, " vs ")[[1]]
      paste(substr(parts[1], 1, 6), "vs", substr(parts[2], 1, 6))
    } else {
      x
    }
  })
  
  barplot(differences,
          names.arg = short_labels,
          main = "Conservation Method Differences (Weighted - Thresholded)",
          ylab = "Difference in Jaccard Similarity",
          las = 2,
          col = ifelse(differences > 0, "lightblue", "lightcoral"),
          cex.names = 0.8)
  abline(h = 0, lty = 2, col = "gray", lwd = 2)
  
  # Add text annotation for extreme differences
  text(which.max(abs(differences)), differences[which.max(abs(differences))] + 
       sign(differences[which.max(abs(differences))]) * 0.01,
       labels = "Largest Difference", cex = 0.8, col = "darkred")
  
  par(mfrow = c(1, 1), mar = c(5, 4, 4, 2) + 0.1, oma = c(0, 0, 0, 0))
}

# Render conservation insights summary
render_conservation_insights_summary <- function(conservation_results, weighted_conservation_results, group_colors) {
  if(is.null(conservation_results) || is.null(weighted_conservation_results)) {
    plot.new()
    text(0.5, 0.5, "Conservation analysis results not available", cex = 1.2)
    return()
  }
  
  # Create summary statistics table plot
  par(mar = c(2, 2, 3, 2))
  plot.new()
  
  # Calculate summary statistics
  threshold_matrix <- conservation_results$similarity_matrix
  weighted_matrix <- weighted_conservation_results$similarity_matrix
  
  if(!is.null(threshold_matrix) && !is.null(weighted_matrix)) {
    threshold_values <- threshold_matrix[upper.tri(threshold_matrix)]
    weighted_values <- weighted_matrix[upper.tri(weighted_matrix)]
    
    # Calculate metrics
    threshold_mean <- round(mean(threshold_values, na.rm = TRUE), 3)
    weighted_mean <- round(mean(weighted_values, na.rm = TRUE), 3)
    
    threshold_sd <- round(sd(threshold_values, na.rm = TRUE), 3)
    weighted_sd <- round(sd(weighted_values, na.rm = TRUE), 3)
    
    correlation <- round(cor(threshold_values, weighted_values, use = "complete.obs"), 3)
    
    mean_difference <- round(mean(weighted_values - threshold_values, na.rm = TRUE), 3)
    
    # Create text summary
    title("Conservation Analysis Insights", cex.main = 1.5, font.main = 2)
    
    insights_text <- paste0(
      "CONSERVATION METHOD COMPARISON\n\n",
      "Thresholded Conservation:\n",
      "  Mean similarity: ", threshold_mean, "\n",
      "  SD: ", threshold_sd, "\n\n",
      "Weighted Conservation:\n", 
      "  Mean similarity: ", weighted_mean, "\n",
      "  SD: ", weighted_sd, "\n\n",
      "Method Correlation: r = ", correlation, "\n",
      "Mean difference (W-T): ", mean_difference, "\n\n",
      "INTERPRETATION:\n",
      if(correlation > 0.7) "Strong agreement between methods" else
      if(correlation > 0.4) "Moderate agreement between methods" else
      "Low agreement between methods", "\n",
      if(mean_difference > 0.1) "Weighted method shows higher conservation" else
      if(mean_difference < -0.1) "Thresholded method shows higher conservation" else
      "Similar conservation levels between methods"
    )
    
    text(0.1, 0.9, insights_text,
         adj = c(0, 1), cex = 0.9, family = "mono")
  } else {
    text(0.5, 0.5, "Insufficient data for insights", cex = 1.2)
  }
}

# ============================================================================
# NODE IMPORTANCE EVOLUTION VISUALIZATIONS
# ============================================================================

# Heatmap of node importance ranks across thresholds
render_node_importance_heatmap <- function(importance_evolution,
                                          metric = "Eigenvector",
                                          top_n = 20,
                                          use_standardized = TRUE,
                                          brain_areas = NULL,
                                          area_colors = NULL) {
  if(is.null(importance_evolution) || !metric %in% names(importance_evolution)) {
    plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
    text(1, 1, "No importance evolution data available", cex = 1.2, col = "gray")
    return()
  }

  metric_data <- importance_evolution[[metric]]

  # Get top N nodes by mean standardized score
  top_nodes <- head(metric_data$summary$Node, top_n)

  # Select matrix to display
  if(use_standardized) {
    plot_matrix <- metric_data$standardized_scores[top_nodes, , drop = FALSE]
    main_title <- paste("Node Importance Evolution:", metric, "(Standardized 0-1)")
    legend_label <- "Score"
  } else {
    plot_matrix <- metric_data$ranks[top_nodes, , drop = FALSE]
    main_title <- paste("Node Rank Evolution:", metric)
    legend_label <- "Rank"
  }

  # Remove columns (thresholds) that are all NA
  non_na_cols <- colSums(!is.na(plot_matrix)) > 0
  plot_matrix <- plot_matrix[, non_na_cols, drop = FALSE]

  if(ncol(plot_matrix) == 0 || nrow(plot_matrix) == 0) {
    plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
    text(1, 1, "Insufficient data for visualization", cex = 1.2, col = "gray")
    return()
  }

  # Create color palette
  if(use_standardized) {
    # High scores (red) = important, low scores (blue) = less important
    col_palette <- colorRampPalette(c("blue", "white", "red"))(100)
  } else {
    # Low ranks (red) = important, high ranks (blue) = less important
    col_palette <- colorRampPalette(c("red", "white", "blue"))(100)
  }

  par(mar = c(8, 10, 4, 6))

  # Plot heatmap
  image(1:ncol(plot_matrix), 1:nrow(plot_matrix), t(plot_matrix),
        col = col_palette,
        xlab = "", ylab = "",
        main = main_title,
        axes = FALSE)

  # Add axes
  axis(1, at = 1:ncol(plot_matrix), labels = colnames(plot_matrix),
       las = 2, cex.axis = 0.7)
  axis(2, at = 1:nrow(plot_matrix), labels = rownames(plot_matrix),
       las = 2, cex.axis = 0.8)
  mtext("Threshold", side = 1, line = 6.5)
  mtext("Node", side = 2, line = 8.5)

  # Add color scale legend
  legend_range <- range(plot_matrix, na.rm = TRUE)
  legend_vals <- seq(legend_range[1], legend_range[2], length.out = 5)
  legend("right", legend = sprintf("%.2f", legend_vals),
         fill = colorRampPalette(col_palette)(5),
         title = legend_label,
         xpd = TRUE, inset = c(-0.15, 0), cex = 0.8)
}

# Line plot showing rank trajectories for top nodes
render_rank_trajectories <- function(importance_evolution,
                                    metric = "Eigenvector",
                                    top_n = 10,
                                    brain_areas = NULL,
                                    area_colors = NULL) {
  if(is.null(importance_evolution) || !metric %in% names(importance_evolution)) {
    plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
    text(1, 1, "No importance evolution data available", cex = 1.2, col = "gray")
    return()
  }

  metric_data <- importance_evolution[[metric]]
  thresholds <- metric_data$thresholds

  # Get top N nodes
  top_nodes <- head(metric_data$summary$Node, top_n)
  rank_matrix <- metric_data$ranks[top_nodes, , drop = FALSE]

  par(mar = c(5, 5, 4, 8), xpd = FALSE)

  # Set up plot
  plot(NULL, xlim = range(thresholds), ylim = rev(range(rank_matrix, na.rm = TRUE)),
       xlab = "Threshold", ylab = "Rank (1 = Best)",
       main = paste("Node Rank Trajectories:", metric))
  grid()

  # Generate colors
  if(!is.null(brain_areas) && !is.null(area_colors)) {
    node_colors <- sapply(top_nodes, function(node) {
      area <- brain_areas[[node]]
      if(!is.null(area) && area %in% names(area_colors)) {
        area_colors[[area]]
      } else {
        "#333333"
      }
    })
  } else {
    node_colors <- rainbow(top_n)
  }

  # Plot trajectories
  for(i in 1:length(top_nodes)) {
    node <- top_nodes[i]
    ranks <- rank_matrix[node, ]
    valid_idx <- !is.na(ranks)

    if(sum(valid_idx) > 1) {
      lines(thresholds[valid_idx], ranks[valid_idx],
            col = node_colors[i], lwd = 2)
      points(thresholds[valid_idx], ranks[valid_idx],
             col = node_colors[i], pch = 19, cex = 0.8)
    }
  }

  # Add legend
  par(xpd = TRUE)
  legend("topright", legend = top_nodes, col = node_colors, lwd = 2,
         cex = 0.7, inset = c(-0.25, 0), title = "Node")
  par(xpd = FALSE)
}

# Stability vs. Importance scatter plot
render_stability_vs_importance <- function(importance_evolution,
                                          metric = "Eigenvector",
                                          brain_areas = NULL,
                                          area_colors = NULL) {
  if(is.null(importance_evolution) || !metric %in% names(importance_evolution)) {
    plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
    text(1, 1, "No importance evolution data available", cex = 1.2, col = "gray")
    return()
  }

  summary_df <- importance_evolution[[metric]]$summary

  # Filter valid data
  valid_data <- summary_df[!is.na(summary_df$StabilityScore) &
                           !is.na(summary_df$MeanStdScore), ]

  if(nrow(valid_data) == 0) {
    plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
    text(1, 1, "Insufficient data for stability analysis", cex = 1.2, col = "gray")
    return()
  }

  par(mar = c(5, 5, 4, 2))

  # Get node colors by brain area
  if(!is.null(brain_areas) && !is.null(area_colors)) {
    node_colors <- sapply(valid_data$Node, function(node) {
      area <- brain_areas[[node]]
      if(!is.null(area) && area %in% names(area_colors)) {
        area_colors[[area]]
      } else {
        "#333333"
      }
    })
  } else {
    node_colors <- "#3498db"
  }

  # Plot
  plot(valid_data$StabilityScore, valid_data$MeanStdScore,
       xlab = "Stability Score (Higher = More Stable)",
       ylab = "Mean Importance Score (0-1)",
       main = paste("Node Stability vs. Importance:", metric),
       pch = 19, col = adjustcolor(node_colors, alpha.f = 0.6),
       cex = 1.5)
  grid()

  # Add quadrant lines
  abline(h = median(valid_data$MeanStdScore, na.rm = TRUE),
         lty = 2, col = "gray40")
  abline(v = median(valid_data$StabilityScore, na.rm = TRUE),
         lty = 2, col = "gray40")

  # Label top nodes (high importance + high stability)
  top_stable_important <- valid_data[valid_data$MeanStdScore >
                                      quantile(valid_data$MeanStdScore, 0.75) &
                                      valid_data$StabilityScore >
                                      quantile(valid_data$StabilityScore, 0.75, na.rm = TRUE), ]

  if(nrow(top_stable_important) > 0) {
    text(top_stable_important$StabilityScore,
         top_stable_important$MeanStdScore,
         labels = top_stable_important$Node,
         pos = 4, cex = 0.7, col = "black")
  }
}

# Summary table of top persistent nodes
render_persistent_nodes_summary <- function(importance_evolution,
                                           metrics = c("Eigenvector", "Degree", "Betweenness"),
                                           top_n = 15) {
  if(is.null(importance_evolution)) {
    plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
    text(1, 1, "No importance evolution data available", cex = 1.2, col = "gray")
    return()
  }

  par(mar = c(2, 2, 3, 2))
  plot(1, type = "n", axes = FALSE, xlab = "", ylab = "",
       xlim = c(0, 10), ylim = c(0, 10),
       main = "Top Persistent Nodes Summary")

  # Collect top nodes from each metric
  all_top_nodes <- list()
  for(metric in metrics) {
    if(metric %in% names(importance_evolution)) {
      top_nodes <- head(importance_evolution[[metric]]$summary, top_n)
      all_top_nodes[[metric]] <- top_nodes
    }
  }

  if(length(all_top_nodes) == 0) {
    text(5, 5, "No data available", cex = 1.2, col = "gray")
    return()
  }

  # Display summary info
  y_pos <- 9
  y_step <- 0.6

  for(metric in names(all_top_nodes)) {
    top_df <- all_top_nodes[[metric]]

    # Metric header
    text(0.5, y_pos, paste(metric, "Centrality:"),
         adj = 0, cex = 1.1, font = 2, col = "#2c3e50")
    y_pos <- y_pos - y_step

    # Top 5 nodes
    display_nodes <- head(top_df, 5)
    for(i in 1:nrow(display_nodes)) {
      node_text <- sprintf("%d. %s (Score: %.2f, Stability: %.2f)",
                          i,
                          display_nodes$Node[i],
                          display_nodes$MeanStdScore[i],
                          ifelse(is.na(display_nodes$StabilityScore[i]), 0,
                                display_nodes$StabilityScore[i]))
      text(1, y_pos, node_text, adj = 0, cex = 0.8, col = "#34495e")
      y_pos <- y_pos - (y_step * 0.7)
    }

    y_pos <- y_pos - y_step * 0.5

    if(y_pos < 1) break
  }
}

# ============================================================================
# Persistence Analysis Render Functions (for Export System)
# ============================================================================

render_persistence_node_metrics <- function(persistence_results, group_colors = NULL, method = "pearson") {
  # Multi-panel: 2 columns (Strength, Eigenvector) × N groups rows
  all_groups <- names(persistence_results[[method]])
  n_groups <- length(all_groups)
  par(mfrow = c(n_groups, 2), mar = c(5, 4, 4, 2))

  for(group in all_groups) {
    pers_data <- persistence_results[[method]][[group]]

    # Get group color
    group_color <- if(!is.null(group_colors) && group %in% names(group_colors)) {
      group_colors[[group]]
    } else {
      "#3498db"
    }

    # STRENGTH distribution across thresholds
    strength_data <- list()
    if(!is.null(pers_data$persistence_data)) {
      for(thresh_name in names(pers_data$persistence_data)) {
        thresh <- pers_data$persistence_data[[thresh_name]]
        if(!is.null(thresh$nodes)) {
          strength_data[[thresh_name]] <- thresh$nodes$Strength
        }
      }
    }

    if(length(strength_data) > 0) {
      boxplot(strength_data,
              main = paste("Node Strength:", group),
              ylab = "Strength",
              las = 2,
              col = group_color,
              border = group_color,
              outcol = group_color)
      grid(nx = NA, ny = NULL, col = "gray90")
    }

    # EIGENVECTOR distribution
    eig_data <- list()
    if(!is.null(pers_data$persistence_data)) {
      for(thresh_name in names(pers_data$persistence_data)) {
        thresh <- pers_data$persistence_data[[thresh_name]]
        if(!is.null(thresh$nodes)) {
          eig_data[[thresh_name]] <- thresh$nodes$Eigenvector
        }
      }
    }

    if(length(eig_data) > 0) {
      boxplot(eig_data,
              main = paste("Eigenvector Centrality:", group),
              ylab = "Eigenvector",
              las = 2,
              col = group_color,
              border = group_color,
              outcol = group_color)
      grid(nx = NA, ny = NULL, col = "gray90")
    }
  }
}

render_persistence_hub_comparison <- function(persistence_results, group_colors = NULL, method = "pearson") {
  hub_data <- list()
  for(group in names(persistence_results[[method]])) {
    pers_data <- persistence_results[[method]][[group]]
    if(!is.null(pers_data$hub_persistence)) {
      hub_data[[group]] <- pers_data$hub_persistence
    }
  }

  if(length(hub_data) > 0) {
    par(mfrow = c(ceiling(length(hub_data)/2), 2), mar = c(8, 4, 3, 2))
    for(group in names(hub_data)) {
      hubs <- hub_data[[group]]

      group_color <- if(!is.null(group_colors) && group %in% names(group_colors)) {
        group_colors[[group]]
      } else {
        "#3498db"
      }

      barplot(hubs$PersistenceScore, names.arg = hubs$Node,
              main = paste("Top Hubs -", group),
              ylab = "Persistence Score", las = 2,
              col = group_color, ylim = c(0, 1))
      grid(nx = NA, ny = NULL)
    }
  }
}

render_persistence_regional_analysis <- function(persistence_results, brain_areas, area_colors = NULL, group_colors = NULL, method = "pearson") {
  if(is.null(brain_areas)) {
    plot(1, type="n", main="Regional Analysis")
    text(1, 1, "No brain areas defined", cex=1.5)
    return()
  }

  all_groups <- names(persistence_results[[method]])
  n_groups <- length(all_groups)
  par(mfrow = c(min(2, n_groups), ceiling(n_groups / 2)), mar = c(8, 5, 4, 2))

  for(group in all_groups) {
    pers_data <- persistence_results[[method]][[group]]

    if(!is.null(pers_data$aggregated_metrics)) {
      metrics <- pers_data$aggregated_metrics

      region_metrics <- data.frame(
        Region = character(),
        MeanCentrality = numeric(),
        SD = numeric(),
        N = integer(),
        stringsAsFactors = FALSE
      )

      for(region_name in names(brain_areas)) {
        region_nodes <- brain_areas[[region_name]]
        region_data <- metrics[metrics$Node %in% region_nodes, ]

        if(nrow(region_data) > 0) {
          region_metrics <- rbind(region_metrics, data.frame(
            Region = region_name,
            MeanCentrality = mean(region_data$MeanEigenvector, na.rm = TRUE),
            SD = sd(region_data$MeanEigenvector, na.rm = TRUE),
            N = nrow(region_data),
            stringsAsFactors = FALSE
          ))
        }
      }

      if(nrow(region_metrics) > 0) {
        colors <- if(!is.null(area_colors)) {
          area_colors[region_metrics$Region]
        } else {
          rainbow(nrow(region_metrics))
        }

        bp <- barplot(region_metrics$MeanCentrality,
                      names.arg = region_metrics$Region,
                      col = colors,
                      las = 2,
                      ylim = c(0, max(region_metrics$MeanCentrality +
                                      region_metrics$SD, na.rm = TRUE) * 1.1),
                      main = paste("Regional Centrality (Persistence):", group),
                      ylab = "Mean Eigenvector Centrality")

        arrows(bp, region_metrics$MeanCentrality - region_metrics$SD,
               bp, region_metrics$MeanCentrality + region_metrics$SD,
               angle = 90, code = 3, length = 0.05)

        grid(nx = NA, ny = NULL)
      }
    }
  }
}

# ============================================================================
# Consensus Analysis Render Functions (for Export System)
# ============================================================================

render_consensus_three_way_eigenvector <- function(method_weighted_results, method_percolation_results, persistence_results, group_colors = NULL, method = "pearson") {
  # FIX: Get groups from the combined weighted_eigenvector df
  weighted_all <- method_weighted_results[[method]]$weighted_eigenvector
  all_groups <- if(!is.null(weighted_all)) unique(weighted_all$Group) else character(0)

  if(length(all_groups) == 0) return(NULL)

  par(mfrow = c(length(all_groups), 3), mar = c(5, 4, 4, 2))

  for(group in all_groups) {
    # FIX: Filter combined df by group
    weighted_eig <- weighted_all[weighted_all$Group == group, ]
    percolation_eig <- method_percolation_results[[method]]$node_metrics
    percolation_eig <- percolation_eig[percolation_eig$Group == group, ]
    persistence_eig <- persistence_results[[method]][[group]]$aggregated_metrics

    group_color <- if(!is.null(group_colors) && group %in% names(group_colors)) {
      group_colors[[group]]
    } else {
      "#3498db"
    }

    # Panel 1: Weighted vs Percolation
    common_nodes_wp <- if(nrow(weighted_eig) > 0) {
      intersect(weighted_eig$Node, percolation_eig$Node)
    } else { character(0) }
    if(length(common_nodes_wp) > 0) {
      w_vals <- weighted_eig$Weighted_Eigenvector[match(common_nodes_wp, weighted_eig$Node)]
      p_vals <- percolation_eig$Eigenvector[match(common_nodes_wp, percolation_eig$Node)]

      plot(w_vals, p_vals,
           xlab = "Weighted Eigenvector",
           ylab = "Percolation Eigenvector",
           main = paste(group, "- W vs P"),
           col = group_color, pch = 19)
      abline(lm(p_vals ~ w_vals), col = "red", lty = 2)
      r <- cor(w_vals, p_vals, use = "complete.obs")
      legend("topleft", legend = sprintf("r = %.3f", r), bty = "n", cex = 0.8)
    }

    # Panel 2: Weighted vs Persistence
    common_nodes_wpers <- if(!is.null(persistence_eig) && nrow(weighted_eig) > 0) {
      intersect(weighted_eig$Node, persistence_eig$Node)
    } else { character(0) }
    if(length(common_nodes_wpers) > 0) {
      w_vals <- weighted_eig$Weighted_Eigenvector[match(common_nodes_wpers, weighted_eig$Node)]
      pers_vals <- persistence_eig$MeanEigenvector[match(common_nodes_wpers, persistence_eig$Node)]

      plot(w_vals, pers_vals,
           xlab = "Weighted Eigenvector",
           ylab = "Persistence Eigenvector",
           main = paste(group, "- W vs Pers"),
           col = group_color, pch = 19)
      abline(lm(pers_vals ~ w_vals), col = "red", lty = 2)
      r <- cor(w_vals, pers_vals, use = "complete.obs")
      legend("topleft", legend = sprintf("r = %.3f", r), bty = "n", cex = 0.8)
    }

    # Panel 3: Percolation vs Persistence
    common_nodes_ppers <- intersect(percolation_eig$Node, persistence_eig$Node)
    if(length(common_nodes_ppers) > 0) {
      p_vals <- percolation_eig$Eigenvector[match(common_nodes_ppers, percolation_eig$Node)]
      pers_vals <- persistence_eig$MeanEigenvector[match(common_nodes_ppers, persistence_eig$Node)]

      plot(p_vals, pers_vals,
           xlab = "Percolation Eigenvector",
           ylab = "Persistence Eigenvector",
           main = paste(group, "- P vs Pers"),
           col = group_color, pch = 19)
      abline(lm(pers_vals ~ p_vals), col = "red", lty = 2)
      r <- cor(p_vals, pers_vals, use = "complete.obs")
      legend("topleft", legend = sprintf("r = %.3f", r), bty = "n", cex = 0.8)
    }
  }
}

render_consensus_hub_ranking <- function(consensus_hub_results, group_colors = NULL) {
  if(is.null(consensus_hub_results$consensus_scores)) {
    plot(1, type="n", main="Consensus Hub Ranking")
    text(1, 1, "No consensus data available", cex=1.5)
    return()
  }

  scores <- consensus_hub_results$consensus_scores
  top_hubs <- head(scores[order(-scores$ConsensusScore), ], 20)

  par(mfrow = c(1, 1), mar = c(8, 5, 4, 2))

  colors <- rainbow(nrow(top_hubs))

  barplot(top_hubs$ConsensusScore,
          names.arg = top_hubs$Node,
          las = 2,
          col = colors,
          main = "Top 20 Consensus Hubs",
          ylab = "Consensus Score",
          ylim = c(0, 1))
  grid(nx = NA, ny = NULL)
}

render_consensus_heatmap <- function(consensus_hub_results, group_colors = NULL) {
  if(is.null(consensus_hub_results$consensus_scores)) {
    plot(1, type="n", main="Consensus Heatmap")
    text(1, 1, "No consensus data available", cex=1.5)
    return()
  }

  scores <- consensus_hub_results$consensus_scores
  top_hubs <- head(scores[order(-scores$ConsensusScore), ], 30)

  par(mfrow = c(1, 1), mar = c(8, 8, 4, 2))

  heatmap_data <- as.matrix(top_hubs[, c("WeightedScore", "PercolationScore", "PersistenceScore")])
  rownames(heatmap_data) <- top_hubs$Node

  image(t(heatmap_data),
        main = "Consensus Hub Heatmap",
        axes = FALSE,
        col = colorRampPalette(c("white", "yellow", "red"))(100))

  axis(1, at = seq(0, 1, length.out = 3),
       labels = c("Weighted", "Percolation", "Persistence"),
       las = 2, cex.axis = 0.8)

  axis(2, at = seq(0, 1, length.out = nrow(heatmap_data)),
       labels = rownames(heatmap_data),
       las = 2, cex.axis = 0.7)
}

# ========================================================================
# TAB 8 VISUALIZATION FUNCTIONS - STATISTICAL VALIDATION
# ========================================================================

#' Render ROI-Level Permutation Test Results
#'
#' Creates a bar plot showing -log10(adjusted p-values) for ROI-level comparisons
#' between two groups. Includes a significance threshold line at -log10(0.05) = 1.3.
#'
#' @param roi_results Data frame from compute_roi_level_permutation_tests with columns:
#'   ROI, Mean_Group1, Mean_Group2, Diff_Observed, P_Value, P_Adjusted, Cohen_d, Significant
#' @param group1_name Character string name of first group (e.g., "Adult_HC")
#' @param group2_name Character string name of second group (e.g., "Adult_VEH")
#' @param alpha Significance threshold (default: 0.05)
#'
#' @return NULL (plots to current graphics device)
#'
#' @details
#' - Bars colored by significance (red = significant, gray = not significant)
#' - Horizontal dashed line at -log10(alpha) shows significance threshold
#' - Higher bars = more significant differences
#' - ROI names displayed on x-axis at 45-degree angle for readability
#'
render_roi_permutation_results <- function(roi_results, group1_name, group2_name, alpha = 0.05) {
  # Input validation
  if(is.null(roi_results) || nrow(roi_results) == 0) {
    plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
    text(1, 1, "No ROI permutation results available.\nRun analysis first.", cex = 1.2)
    return()
  }

  required_cols <- c("ROI", "P_Adjusted", "Significant")
  if(!all(required_cols %in% names(roi_results))) {
    plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
    text(1, 1, "Invalid ROI results format.\nMissing required columns.", cex = 1.2)
    return()
  }

  # Calculate -log10(p-values) for visualization
  roi_results$NegLogP <- -log10(roi_results$P_Adjusted)

  # Replace Inf values (p = 0) with a large finite value
  max_finite <- max(roi_results$NegLogP[is.finite(roi_results$NegLogP)])
  roi_results$NegLogP[is.infinite(roi_results$NegLogP)] <- max_finite * 1.2

  # Sort by significance for better visualization
  roi_results <- roi_results[order(-roi_results$NegLogP), ]

  # Color bars by significance
  bar_colors <- ifelse(roi_results$Significant, "#E74C3C", "#95A5A6")

  # Create bar plot
  par(mar = c(10, 5, 4, 2))

  barplot(roi_results$NegLogP,
          names.arg = roi_results$ROI,
          col = bar_colors,
          border = NA,
          las = 2,
          ylab = expression(-log[10](italic(p)[adjusted])),
          main = sprintf("ROI-Level Permutation Test:\n%s vs %s", group1_name, group2_name),
          ylim = c(0, max(roi_results$NegLogP) * 1.1),
          cex.names = 0.8,
          cex.lab = 1.1,
          cex.main = 1.2)

  # Add significance threshold line
  threshold_line <- -log10(alpha)
  abline(h = threshold_line, col = "#34495E", lty = 2, lwd = 2)

  # Add threshold label
  text(x = 1, y = threshold_line * 1.1,
       labels = sprintf("p = %.2f", alpha),
       pos = 4, col = "#34495E", cex = 0.9)

  # Add legend
  legend("topright",
         legend = c("Significant", "Not Significant", "Threshold"),
         fill = c("#E74C3C", "#95A5A6", NA),
         border = c("black", "black", NA),
         lty = c(NA, NA, 2),
         lwd = c(NA, NA, 2),
         col = c(NA, NA, "#34495E"),
         bty = "n",
         cex = 0.9)

  # Add summary text
  n_sig <- sum(roi_results$Significant, na.rm = TRUE)
  n_total <- nrow(roi_results)
  mtext(sprintf("%d / %d ROIs significant (FDR-corrected)", n_sig, n_total),
        side = 3, line = 0.5, cex = 0.9, col = "#34495E")
}


#' Render Hub Overlap Venn Diagrams
#'
#' Creates multi-panel Venn diagrams showing hub overlap for pairwise group comparisons.
#' Displays up to 6 pairwise comparisons in a 2x3 grid layout.
#'
#' @param hub_overlap_results List from compute_hub_overlap_statistics containing:
#'   - jaccard_matrix: Matrix of Jaccard indices
#'   - pairwise_details: List of pairwise comparison details
#'   - hub_sets: Named list of hub node vectors per group
#' @param max_pairs Maximum number of pairwise comparisons to display (default: 6)
#'
#' @return NULL (plots to current graphics device)
#'
#' @details
#' - Each Venn diagram shows overlap between two groups
#' - Numbers indicate: unique to A, shared, unique to B
#' - Jaccard index (J) shown for each pair
#' - Sorted by Jaccard index (highest overlap first)
#'
render_hub_overlap_venn_diagrams <- function(hub_overlap_results, max_pairs = 6) {
  # Input validation
  if(is.null(hub_overlap_results) || is.null(hub_overlap_results$pairwise_details)) {
    plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
    text(1, 1, "No hub overlap results available.\nRun analysis first.", cex = 1.2)
    return()
  }

  pairwise <- hub_overlap_results$pairwise_details

  if(length(pairwise) == 0) {
    plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
    text(1, 1, "No pairwise comparisons available.\nNeed at least 2 groups.", cex = 1.2)
    return()
  }

  # Sort pairs by Jaccard index (descending)
  jaccard_values <- sapply(pairwise, function(x) x$jaccard_index)
  sorted_pairs <- pairwise[order(-jaccard_values)]

  # Limit to max_pairs
  n_pairs <- min(length(sorted_pairs), max_pairs)
  sorted_pairs <- sorted_pairs[1:n_pairs]

  # Determine grid layout
  if(n_pairs <= 2) {
    par(mfrow = c(1, n_pairs), mar = c(2, 2, 3, 2))
  } else if(n_pairs <= 4) {
    par(mfrow = c(2, 2), mar = c(2, 2, 3, 2))
  } else {
    par(mfrow = c(2, 3), mar = c(2, 2, 3, 2))
  }

  # Draw each Venn diagram
  for(i in 1:n_pairs) {
    pair_name <- names(sorted_pairs)[i]
    pair_data <- sorted_pairs[[i]]

    # Extract group names from pair key (format: "GroupA_vs_GroupB")
    group_names <- strsplit(pair_name, "_vs_")[[1]]
    group1 <- group_names[1]
    group2 <- group_names[2]

    # Extract counts
    n_shared <- length(pair_data$common_hubs)
    n_unique1 <- length(pair_data$unique_to_group1)
    n_unique2 <- length(pair_data$unique_to_group2)
    jaccard <- pair_data$jaccard_index

    # Draw Venn diagram manually (simple 2-circle version)
    plot(0, 0, type = "n", xlim = c(-2, 2), ylim = c(-1.5, 1.5),
         axes = FALSE, xlab = "", ylab = "",
         main = sprintf("%s vs %s\nJ = %.3f", group1, group2, jaccard),
         cex.main = 0.9)

    # Draw circles
    theta <- seq(0, 2*pi, length.out = 100)

    # Circle 1 (left)
    x1 <- -0.5 + 0.8 * cos(theta)
    y1 <- 0.8 * sin(theta)
    polygon(x1, y1, col = rgb(0.2, 0.5, 0.8, 0.3), border = "#3498DB", lwd = 2)

    # Circle 2 (right)
    x2 <- 0.5 + 0.8 * cos(theta)
    y2 <- 0.8 * sin(theta)
    polygon(x2, y2, col = rgb(0.9, 0.4, 0.3, 0.3), border = "#E74C3C", lwd = 2)

    # Add counts
    text(-0.9, 0, labels = n_unique1, cex = 1.5, font = 2, col = "#3498DB")
    text(0, 0, labels = n_shared, cex = 1.5, font = 2, col = "#27AE60")
    text(0.9, 0, labels = n_unique2, cex = 1.5, font = 2, col = "#E74C3C")

    # Add group labels
    text(-0.9, -1.2, labels = group1, cex = 0.8, col = "#3498DB")
    text(0.9, -1.2, labels = group2, cex = 0.8, col = "#E74C3C")
  }

  # Reset to single plot
  par(mfrow = c(1, 1))
}


#' Render Hub Overlap Jaccard Index Heatmap
#'
#' Creates a heatmap showing Jaccard indices for all pairwise group comparisons.
#' Color intensity indicates degree of hub overlap (0 = no overlap, 1 = perfect overlap).
#'
#' @param hub_overlap_results List from compute_hub_overlap_statistics containing:
#'   - jaccard_matrix: Symmetric matrix of Jaccard indices
#'   - hub_sets: Named list of hub node vectors per group
#'
#' @return NULL (plots to current graphics device)
#'
#' @details
#' - Diagonal = 1 (perfect self-overlap)
#' - Off-diagonal values show between-group overlap
#' - Color scale: white (J=0) to dark red (J=1)
#' - Values displayed in each cell
#'
render_hub_overlap_heatmap <- function(hub_overlap_results) {
  # Input validation
  if(is.null(hub_overlap_results) || is.null(hub_overlap_results$jaccard_matrix)) {
    plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
    text(1, 1, "No Jaccard matrix available.\nRun hub overlap analysis first.", cex = 1.2)
    return()
  }

  jaccard_mat <- hub_overlap_results$jaccard_matrix

  if(nrow(jaccard_mat) < 2) {
    plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
    text(1, 1, "Need at least 2 groups for heatmap.", cex = 1.2)
    return()
  }

  # Create heatmap
  n_groups <- nrow(jaccard_mat)
  group_names <- rownames(jaccard_mat)

  # Set up plotting area
  par(mar = c(8, 8, 4, 3))

  # Create color palette
  col_palette <- colorRampPalette(c("white", "#FADBD8", "#F1948A", "#E74C3C", "#922B21"))(100)

  # Draw heatmap
  image(1:n_groups, 1:n_groups, t(jaccard_mat),
        col = col_palette,
        axes = FALSE,
        xlab = "", ylab = "",
        main = "Hub Overlap Jaccard Index Heatmap",
        cex.main = 1.3)

  # Add axes
  axis(1, at = 1:n_groups, labels = group_names, las = 2, cex.axis = 0.9)
  axis(2, at = 1:n_groups, labels = group_names, las = 2, cex.axis = 0.9)

  # Add grid lines
  abline(h = (1:n_groups) + 0.5, col = "gray70", lwd = 0.5)
  abline(v = (1:n_groups) + 0.5, col = "gray70", lwd = 0.5)

  # Add Jaccard index values to cells
  for(i in 1:n_groups) {
    for(j in 1:n_groups) {
      jaccard_val <- jaccard_mat[i, j]

      # Choose text color based on background
      text_col <- if(jaccard_val > 0.5) "white" else "black"

      text(j, i, sprintf("%.2f", jaccard_val),
           col = text_col, cex = 0.9, font = 2)
    }
  }

  # Add color scale legend
  legend_vals <- seq(0, 1, length.out = 5)
  legend_cols <- col_palette[c(1, 25, 50, 75, 100)]

  legend("right",
         legend = sprintf("%.2f", legend_vals),
         fill = legend_cols,
         title = "Jaccard\nIndex",
         bty = "n",
         cex = 0.8,
         y.intersp = 1.2)

  # Add summary statistics
  off_diag <- jaccard_mat[lower.tri(jaccard_mat)]
  mean_jaccard <- mean(off_diag, na.rm = TRUE)

  mtext(sprintf("Mean pairwise Jaccard: %.3f", mean_jaccard),
        side = 3, line = 0.5, cex = 0.9, col = "#34495E")
}


#' Render Global Network Permutation Test Results
#'
#' Creates a multi-panel visualization showing null distributions and observed
#' differences for global network metrics between two groups.
#'
#' @param global_perm_results List from compute_global_network_permutation_test containing:
#'   - group1_name, group2_name: Character strings
#'   - observed_diff: Named vector of observed differences
#'   - p_values: Named vector of p-values
#'   - null_distributions: Matrix of permuted differences
#'
#' @return NULL (plots to current graphics device)
#'
#' @details
#' Creates a 2x2 panel plot showing:
#' - Top panels: Histograms of null distributions for each metric with observed value marked
#' - Bottom left: Bar plot of -log10(p-values) with significance threshold
#' - Bottom right: Comparison of observed metrics between groups
#'
render_global_permutation_results <- function(global_perm_results) {
  # Input validation
  if(is.null(global_perm_results)) {
    plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
    text(1, 1, "No global permutation results available.\nRun analysis first.", cex = 1.2)
    return()
  }

  observed_diff <- global_perm_results$observed_diff
  p_values <- global_perm_results$p_values
  null_dists <- global_perm_results$null_distributions
  group1_name <- global_perm_results$group1_name
  group2_name <- global_perm_results$group2_name

  # Identify available metrics (not all NA)
  available_metrics <- names(observed_diff)[!is.na(observed_diff)]

  if(length(available_metrics) == 0) {
    plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
    text(1, 1, "No valid metrics computed.\nCheck network connectivity.", cex = 1.2)
    return()
  }

  # Set up 2x2 panel layout
  par(mfrow = c(2, 2), mar = c(4, 4, 3, 2))

  # Plot null distributions for each available metric
  for(i in 1:min(4, length(available_metrics))) {
    metric <- available_metrics[i]
    obs_val <- observed_diff[metric]
    p_val <- p_values[metric]
    null_vals <- null_dists[, metric]

    # Remove NAs
    null_vals <- null_vals[!is.na(null_vals)]

    if(length(null_vals) == 0) next

    # Histogram of null distribution
    hist(null_vals,
         breaks = 30,
         col = "#95A5A6",
         border = "white",
         main = sprintf("%s: p = %.4f", metric, p_val),
         xlab = sprintf("Difference (%s - %s)", group1_name, group2_name),
         ylab = "Frequency",
         cex.main = 1.1)

    # Add observed value line
    abline(v = obs_val, col = "#E74C3C", lwd = 3, lty = 1)

    # Add zero line
    abline(v = 0, col = "#34495E", lwd = 2, lty = 2)

    # Add legend
    legend("topright",
           legend = c("Observed", "Null (H0)"),
           col = c("#E74C3C", "#34495E"),
           lty = c(1, 2),
           lwd = c(3, 2),
           bty = "n",
           cex = 0.8)

    # Add significance annotation
    if(p_val < 0.05) {
      sig_text <- if(p_val < 0.001) "***" else if(p_val < 0.01) "**" else "*"
      text(obs_val, max(hist(null_vals, breaks = 30, plot = FALSE)$counts) * 0.9,
           labels = sig_text, col = "#E74C3C", cex = 2, font = 2)
    }
  }

  # If fewer than 4 metrics, add summary plots
  if(length(available_metrics) < 4) {
    # Bottom left: P-value bar plot
    neg_log_p <- -log10(p_values[available_metrics])
    bar_colors <- ifelse(p_values[available_metrics] < 0.05, "#E74C3C", "#95A5A6")

    barplot(neg_log_p,
            names.arg = available_metrics,
            col = bar_colors,
            border = NA,
            main = "Statistical Significance",
            ylab = expression(-log[10](italic(p))),
            las = 2,
            cex.names = 0.8)

    abline(h = -log10(0.05), col = "#34495E", lty = 2, lwd = 2)
    text(0.5, -log10(0.05) * 1.1, "p = 0.05", pos = 4, col = "#34495E", cex = 0.8)

    # Bottom right: Observed metrics comparison
    if(length(available_metrics) >= 2) {
      obs_group1 <- global_perm_results$observed_group1[available_metrics]
      obs_group2 <- global_perm_results$observed_group2[available_metrics]

      # Normalize for comparison (z-scores within each metric)
      metrics_mat <- rbind(obs_group1, obs_group2)
      rownames(metrics_mat) <- c(group1_name, group2_name)

      barplot(metrics_mat,
              beside = TRUE,
              col = c("#3498DB", "#E67E22"),
              border = NA,
              main = "Observed Network Metrics",
              ylab = "Metric Value",
              las = 2,
              cex.names = 0.8,
              legend.text = c(group1_name, group2_name),
              args.legend = list(bty = "n", cex = 0.8))
    }
  }

  par(mfrow = c(1, 1))
}
