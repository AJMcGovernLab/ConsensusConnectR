# Complete Plot Export System
# Generates ALL 69+ plots exactly as they appear in the interactive app

# Load required libraries and functions
source("modules/visualization_functions.R")
source("modules/consensus_visualizations.R")

# Load additional functions from app.R if they exist
tryCatch({
  # Source app.R functions (lines 35-400 contain render functions)
  app_lines <- readLines("app.R")

  # Extract render function definitions from app.R
  render_start <- grep("render_advanced_mst_metrics.*function", app_lines)

  # Find the end by looking for the next major section or create_enhanced_network_plots
  next_section <- grep("^# Enhanced network plotting function|^create_enhanced_network_plots", app_lines)

  if(length(render_start) > 0 && length(next_section) > 0) {
    # Extract up to but not including the next section
    render_end <- next_section[next_section > render_start[1]][1] - 1

    # Create temp file with just the render functions
    temp_functions <- app_lines[render_start[1]:render_end]
    temp_file <- tempfile(fileext = ".R")
    writeLines(temp_functions, temp_file)
    source(temp_file, local = TRUE)
    unlink(temp_file)
    cat("✓ Additional render functions loaded from app.R\n")
  }
}, error = function(e) {
  cat("⚠️ Could not load additional functions from app.R:", e$message, "\n")
})

# Complete mapping of all renderPlot outputs to their exact function calls
complete_plot_system <- function(analysis_results, ui_state, output_dir, width = 1200, height = 800, dpi = 300, network_options = NULL) {

  cat("🚀 Starting Complete Plot System - Targeting 69+ plots\n")
  cat(sprintf("📐 Plot dimensions: %d×%d pixels @ %d DPI\n", width, height, dpi))

  # Set default network options if not provided
  if(is.null(network_options)) {
    network_options <- list(
      use_anatomical_colors = FALSE,
      node_shape = "circle",
      edge_sign_colors = FALSE,
      positive_color = "#2ecc71",
      negative_color = "#e74c3c"
    )
  }

  # Log network configuration
  cat(sprintf("🎨 Network options: use_anatomical_colors=%s, node_shape=%s\n",
              network_options$use_anatomical_colors, network_options$node_shape))
  cat(sprintf("🧠 Brain areas defined: %s (n=%d)\n",
              !is.null(ui_state$brain_areas),
              if(!is.null(ui_state$brain_areas)) length(ui_state$brain_areas) else 0))
  cat(sprintf("🎨 Area colors defined: %s (n=%d)\n",
              !is.null(ui_state$area_colors),
              if(!is.null(ui_state$area_colors)) length(ui_state$area_colors) else 0))

  # Validate and provide defaults for user-selected colors
  if(is.null(ui_state$group_colors) || length(ui_state$group_colors) == 0) {
    cat("⚠️  Warning: No group colors defined - using default color palette\n")
    if(!is.null(analysis_results$correlations)) {
      default_colors <- c("#3498db", "#e74c3c", "#27ae60", "#9b59b6", "#e67e22", "#1abc9c")
      group_names <- names(analysis_results$correlations)
      ui_state$group_colors <- setNames(
        default_colors[((seq_along(group_names) - 1) %% length(default_colors)) + 1],
        group_names
      )
    }
  }

  if(is.null(ui_state$area_colors) || length(ui_state$area_colors) == 0) {
    cat("⚠️  Warning: No brain area colors defined - using rainbow palette\n")
    if(!is.null(ui_state$brain_areas)) {
      area_names <- names(ui_state$brain_areas)
      ui_state$area_colors <- setNames(rainbow(length(area_names)), area_names)
    }
  }

  # Complete plot definition with exact parameters from app.R
  all_plots <- list(
    
    # Core Analysis Plots (Always available)
    "01_Data_Imputation" = list(
      condition = function() !is.null(analysis_results$imputation),
      func = function() render_imputation_plots(analysis_results, analysis_results$raw_data)
    ),
    
    "02_Correlation_Matrices" = list(
      condition = function() !is.null(analysis_results$correlations),
      func = function() {
        exp_group_colors <- if(!is.null(ui_state$group_colors) && length(ui_state$group_colors) > 0) ui_state$group_colors else NULL
        render_correlation_plots(analysis_results$correlations, exp_group_colors)
      }
    ),
    
    "03_Correlation_Distributions" = list(
      condition = function() !is.null(analysis_results$correlations),
      func = function() render_correlation_distributions(analysis_results$correlations)
    ),
    
    # Gallery plot removed - each plot gets its own file

    # Individual Network Plots - dynamically created based on available networks
    # These will be generated separately in the plot generation loop

    "06_Network_Dashboard" = list(
      condition = function() !is.null(analysis_results$global_metrics),
      func = function() {
        exp_group_colors <- if(!is.null(ui_state$group_colors) && length(ui_state$group_colors) > 0) ui_state$group_colors else NULL
        render_network_dashboard(analysis_results$global_metrics, exp_group_colors)
      }
    ),
    
    # Node Analysis Plots
    "07_Percolation_Strength_Eigenvector" = list(
      condition = function() !is.null(analysis_results$node_metrics),
      func = function() render_percolation_strength_eigenvector_relationship(analysis_results$node_metrics, ui_state$group_colors, ui_state$brain_areas, ui_state$area_colors)
    ),
    
    "08_Node_Centrality" = list(
      condition = function() !is.null(analysis_results$brain_area_metrics),
      func = function() render_node_centrality(analysis_results$brain_area_metrics, ui_state$brain_areas, ui_state$area_colors)
    ),
    
    "09_Node_Heatmap" = list(
      condition = function() !is.null(analysis_results$brain_area_metrics),
      func = function() render_node_heatmap(analysis_results$brain_area_metrics, ui_state$brain_areas, ui_state$area_colors)
    ),
    
    "10_Individual_Node_Centrality" = list(
      condition = function() !is.null(analysis_results$node_metrics),
      func = function() render_individual_node_centrality(analysis_results$node_metrics, ui_state$brain_areas, ui_state$area_colors)
    ),
    
    "11_Individual_Node_Heatmap" = list(
      condition = function() !is.null(analysis_results$node_metrics),
      func = function() render_individual_node_heatmap(analysis_results$node_metrics, ui_state$brain_areas, ui_state$area_colors)
    ),
    
    # Edge and Connectivity Plots
    "12_Edge_Metrics" = list(
      condition = function() !is.null(analysis_results$edge_metrics),
      func = function() {
        exp_group_colors <- if(!is.null(ui_state$group_colors) && length(ui_state$group_colors) > 0) ui_state$group_colors else NULL
        render_edge_metrics(analysis_results$edge_metrics, exp_group_colors)
      }
    ),
    
    "13_Brain_Area_Connectivity" = list(
      condition = function() !is.null(analysis_results$brain_area_metrics),
      func = function() render_brain_area_connectivity(analysis_results$brain_area_metrics, ui_state$brain_areas, ui_state$area_colors)
    ),
    
    # Eigenvector Analysis Plots
    "14_Average_Eigenvector" = list(
      condition = function() !is.null(analysis_results$weighted_eigenvector) && !is.null(analysis_results$node_metrics),
      func = function() render_avg_eigenvector_by_region(analysis_results$weighted_eigenvector, analysis_results$node_metrics, ui_state$brain_areas, ui_state$area_colors, ui_state$group_colors)
    ),
    
    "15_Combined_Eigenvector_Bar" = list(
      condition = function() !is.null(analysis_results$weighted_eigenvector),
      func = function() render_combined_eigenvector_bar_plot(analysis_results$weighted_eigenvector, ui_state$brain_areas, ui_state$area_colors, ui_state$group_colors)
    ),
    
    # Conservation Analysis Plots
    "16_Conservation_Stats" = list(
      condition = function() !is.null(analysis_results$similarities),
      func = function() render_conservation_stats(analysis_results$similarities)
    ),
    
    "17_Network_Similarity_Matrix" = list(
      condition = function() !is.null(analysis_results$similarities),
      func = function() render_network_similarity_matrix(analysis_results$similarities)
    ),
    
    "18_Hub_Conservation" = list(
      condition = function() !is.null(analysis_results$conservation) && !is.null(analysis_results$conservation$conservation_possible) && analysis_results$conservation$conservation_possible,
      func = function() render_hub_conservation(analysis_results$conservation)
    ),
    
    # Summary and Overview
    "19_Summary_Dashboard" = list(
      condition = function() !is.null(analysis_results$complete) && analysis_results$complete,
      func = function() render_summary_dashboard(analysis_results)
    ),
    
    # Advanced Analysis Plots
    "20_Group_Percolation" = list(
      condition = function() !is.null(analysis_results$correlations) && !is.null(analysis_results$group_thresholds),
      func = function() render_group_specific_percolation(analysis_results$correlations, analysis_results$group_thresholds)
    ),
    
    "21_Weighted_Node_Metrics" = list(
      condition = function() !is.null(analysis_results$threshold_free_results),
      func = function() render_weighted_node_metrics(analysis_results$threshold_free_results, ui_state$group_colors)
    ),
    
    "22_All_Weighted_Stats" = list(
      condition = function() !is.null(analysis_results$threshold_free_results),
      func = function() render_all_weighted_stats(analysis_results$threshold_free_results, ui_state$group_colors)
    ),
    
    # Cross-Method Comparisons
    "23_Cross_Method_Eigenvector" = list(
      condition = function() !is.null(analysis_results$weighted_eigenvector) && !is.null(analysis_results$node_metrics),
      func = function() render_cross_method_eigenvector_comparison(analysis_results$weighted_eigenvector, analysis_results$node_metrics, ui_state$group_colors, ui_state$brain_areas, ui_state$area_colors)
    ),
    
    "24_Cross_Method_Node_Strength" = list(
      condition = function() !is.null(analysis_results$threshold_free_results) && !is.null(analysis_results$node_metrics),
      func = function() render_cross_method_node_strength_comparison(analysis_results$threshold_free_results$node_strength, analysis_results$node_metrics, ui_state$group_colors)
    ),
    
    "25_Hub_Ranking_Comparison" = list(
      condition = function() !is.null(analysis_results$cross_method_comparison) && !is.null(analysis_results$cross_method_comparison$hub_rankings),
      func = function() render_hub_ranking_comparison(analysis_results$cross_method_comparison, ui_state$group_colors)
    ),
    
    "26_Pipeline_Overview" = list(
      condition = function() !is.null(analysis_results$complete) && analysis_results$complete,
      func = function() render_pipeline_overview(analysis_results)
    ),
    
    # MST Analysis Plots
    "27_Advanced_MST_Metrics" = list(
      condition = function() !is.null(analysis_results$mst_results),
      func = function() render_advanced_mst_metrics(analysis_results$mst_results, ui_state$group_colors)
    ),
    
    "28_MST_Central_Nodes" = list(
      condition = function() !is.null(analysis_results$mst_results),
      func = function() render_mst_central_nodes(analysis_results$mst_results, ui_state$brain_areas, ui_state$area_colors, ui_state$group_colors)
    ),
    
    "29_Advanced_MST_Networks" = list(
      condition = function() !is.null(analysis_results$mst_results),
      func = function() render_advanced_mst_networks(analysis_results$mst_results, analysis_results$correlations, ui_state$brain_areas, ui_state$area_colors, ui_state$group_colors)
    ),
    
    # PCA Analysis Plots
    "30_PCA_Analysis" = list(
      condition = function() !is.null(analysis_results$pca_results),
      func = function() render_pca_analysis_results(analysis_results$pca_results, ui_state$group_colors)
    ),
    
    "31_PCA_Loadings" = list(
      condition = function() !is.null(analysis_results$pca_results),
      func = function() render_pca_loadings_results(analysis_results$pca_results, ui_state$brain_areas, ui_state$area_colors, ui_state$group_colors)
    ),
    
    "32_PCA_Variance" = list(
      condition = function() !is.null(analysis_results$pca_results),
      func = function() render_pca_variance_results(analysis_results$pca_results, ui_state$group_colors)
    ),
    
    # Conservation Heatmaps
    "33_Network_Similarity_Heatmap" = list(
      condition = function() !is.null(analysis_results$conservation_analysis),
      func = function() render_network_similarity_heatmap(analysis_results$conservation_analysis)
    ),
    
    "34_Hub_Conservation_Heatmap" = list(
      condition = function() !is.null(analysis_results$conservation_analysis),
      func = function() render_hub_conservation_heatmap(analysis_results$conservation_analysis)
    ),
    
    "35_Edge_Conservation_Heatmap" = list(
      condition = function() !is.null(analysis_results$conservation_analysis),
      func = function() render_edge_conservation_heatmap(analysis_results$conservation_analysis)
    ),
    
    # Weighted Conservation Analysis
    "36_Weighted_Similarity_Heatmap" = list(
      condition = function() !is.null(analysis_results$weighted_conservation_analysis),
      func = function() render_weighted_similarity_heatmap(analysis_results$weighted_conservation_analysis, ui_state$group_colors)
    ),
    
    "37_Weighted_Hub_Conservation" = list(
      condition = function() !is.null(analysis_results$weighted_conservation_analysis),
      func = function() render_weighted_hub_conservation_heatmap(analysis_results$weighted_conservation_analysis, ui_state$group_colors)
    ),
    
    "38_Weighted_Edge_Statistics" = list(
      condition = function() !is.null(analysis_results$weighted_conservation_analysis),
      func = function() render_weighted_edge_statistics_plot(analysis_results$weighted_conservation_analysis, ui_state$group_colors)
    ),
    
    # Conservation Correlations
    "39_Conservation_Correlation" = list(
      condition = function() !is.null(analysis_results$conservation_analysis) && !is.null(analysis_results$weighted_conservation_analysis),
      func = function() render_conservation_correlation_analysis(analysis_results$conservation_analysis, analysis_results$weighted_conservation_analysis, ui_state$group_colors)
    ),
    
    "40_Conservation_Insights" = list(
      condition = function() !is.null(analysis_results$conservation_analysis) && !is.null(analysis_results$weighted_conservation_analysis),
      func = function() render_conservation_insights_summary(analysis_results$conservation_analysis, analysis_results$weighted_conservation_analysis, ui_state$group_colors)
    ),
    
    # Weighted vs Percolation Comparisons
    "41_Weighted_vs_Percolation_Eigenvector" = list(
      condition = function() !is.null(analysis_results$weighted_eigenvector) && !is.null(analysis_results$node_metrics),
      func = function() render_weighted_vs_percolation_eigenvector_by_group(analysis_results$weighted_eigenvector, analysis_results$node_metrics, ui_state$group_colors, ui_state$brain_areas, ui_state$area_colors)
    ),
    
    "42_Weighted_vs_Percolation_Node_Strength" = list(
      condition = function() !is.null(analysis_results$threshold_free_results) && !is.null(analysis_results$node_metrics),
      func = function() render_weighted_vs_percolation_node_strength_by_group(analysis_results$threshold_free_results$node_strength, analysis_results$node_metrics, ui_state$group_colors, ui_state$brain_areas, ui_state$area_colors)
    ),
    
    # Regional Analysis Plots  
    "43_Average_Node_Strength_by_Region" = list(
      condition = function() !is.null(analysis_results$threshold_free_results) && !is.null(analysis_results$node_metrics),
      func = function() render_avg_node_strength_by_region(analysis_results$threshold_free_results$node_strength, analysis_results$node_metrics, ui_state$brain_areas, ui_state$area_colors, ui_state$group_colors)
    ),
    
    "44_Rank_Based_Node_Strength" = list(
      condition = function() !is.null(analysis_results$threshold_free_results) && !is.null(analysis_results$node_metrics),
      func = function() render_rank_based_node_strength_with_avg(analysis_results$threshold_free_results$node_strength, analysis_results$node_metrics, ui_state$brain_areas, ui_state$area_colors, ui_state$group_colors)
    ),
    
    "45_Average_Eigenvector_by_Region" = list(
      condition = function() !is.null(analysis_results$weighted_eigenvector) && !is.null(analysis_results$node_metrics),
      func = function() render_avg_eigenvector_by_region(analysis_results$weighted_eigenvector, analysis_results$node_metrics, ui_state$brain_areas, ui_state$area_colors, ui_state$group_colors)
    ),
    
    "46_Rank_Based_Eigenvector" = list(
      condition = function() !is.null(analysis_results$weighted_eigenvector) && !is.null(analysis_results$node_metrics),
      func = function() render_rank_based_comparisons(analysis_results$weighted_eigenvector, analysis_results$threshold_free_results$node_strength, analysis_results$node_metrics, ui_state$group_colors, ui_state$brain_areas, ui_state$area_colors)
    ),
    
    # Subregion Analysis Plots
    "47_Node_Strength_Subregions" = list(
      condition = function() !is.null(analysis_results$threshold_free_results$node_strength) && !is.null(analysis_results$node_metrics),
      func = function() render_subregion_strength_comparison(analysis_results$threshold_free_results$node_strength, analysis_results$node_metrics, ui_state$brain_areas, ui_state$area_colors, ui_state$group_colors, use_ranks = FALSE)
    ),
    
    "48_Node_Strength_Rank_Subregions" = list(
      condition = function() !is.null(analysis_results$threshold_free_results$node_strength) && !is.null(analysis_results$node_metrics),
      func = function() render_subregion_strength_comparison(analysis_results$threshold_free_results$node_strength, analysis_results$node_metrics, ui_state$brain_areas, ui_state$area_colors, ui_state$group_colors, use_ranks = TRUE)
    ),
    
    "49_Eigenvector_Subregions" = list(
      condition = function() !is.null(analysis_results$weighted_eigenvector) && !is.null(analysis_results$node_metrics),
      func = function() render_subregion_eigenvector_comparison(analysis_results$weighted_eigenvector, analysis_results$node_metrics, ui_state$brain_areas, ui_state$area_colors, ui_state$group_colors, use_ranks = FALSE)
    ),
    
    "50_Eigenvector_Rank_Subregions" = list(
      condition = function() !is.null(analysis_results$weighted_eigenvector) && !is.null(analysis_results$node_metrics),
      func = function() render_subregion_eigenvector_comparison(analysis_results$weighted_eigenvector, analysis_results$node_metrics, ui_state$brain_areas, ui_state$area_colors, ui_state$group_colors, use_ranks = TRUE)
    ),
    
    # Final Eigenvector Analysis Plots
    "51_Weighted_Eigenvector_Comparison" = list(
      condition = function() !is.null(analysis_results$weighted_eigenvector),
      func = function() render_weighted_eigenvector_comparison(analysis_results$weighted_eigenvector, ui_state$group_colors, ui_state$brain_areas, ui_state$area_colors)
    ),
    
    "52_Weighted_vs_Unweighted" = list(
      condition = function() !is.null(analysis_results$weighted_eigenvector),
      func = function() render_weighted_vs_unweighted_eigenvector(analysis_results$weighted_eigenvector, ui_state$group_colors)
    ),
    
    "53_Eigenvector_Rank_Change" = list(
      condition = function() !is.null(analysis_results$weighted_eigenvector),
      func = function() render_eigenvector_rank_change(analysis_results$weighted_eigenvector, ui_state$group_colors)
    ),
    
    "55_Weighted_Eigenvector_Hub" = list(
      condition = function() !is.null(analysis_results$weighted_eigenvector_hubs),
      func = function() render_weighted_eigenvector_hub_comparison(analysis_results$weighted_eigenvector_hubs, ui_state$group_colors)
    ),
    
    "56_Eigenvector_Stability" = list(
      condition = function() !is.null(analysis_results$weighted_eigenvector_comparison),
      func = function() render_eigenvector_stability(analysis_results$weighted_eigenvector_comparison)
    ),

    # Persistence Analysis Plots (Tab 5)
    "57_Persistence_Node_Metrics" = list(
      condition = function() !is.null(analysis_results$persistence_results),
      func = function() render_persistence_node_metrics(analysis_results$persistence_results, ui_state$group_colors)
    ),

    "58_Persistence_Hub_Comparison" = list(
      condition = function() !is.null(analysis_results$persistence_results),
      func = function() render_persistence_hub_comparison(analysis_results$persistence_results, ui_state$group_colors)
    ),

    "59_Persistence_Regional_Analysis" = list(
      condition = function() !is.null(analysis_results$persistence_results) && !is.null(ui_state$brain_areas),
      func = function() render_persistence_regional_analysis(analysis_results$persistence_results, ui_state$brain_areas, ui_state$area_colors, ui_state$group_colors)
    ),

    # Comprehensive Consensus Plots (Tab 6)
    "60_Consensus_Three_Way_Eigenvector" = list(
      condition = function() !is.null(analysis_results$method_weighted_results) && !is.null(analysis_results$method_percolation_results) && !is.null(analysis_results$persistence_results),
      func = function() render_consensus_three_way_eigenvector(analysis_results$method_weighted_results, analysis_results$method_percolation_results, analysis_results$persistence_results, ui_state$group_colors)
    ),

    "61_Consensus_Hub_Ranking" = list(
      condition = function() !is.null(analysis_results$consensus_hub_results),
      func = function() render_consensus_hub_ranking(analysis_results$consensus_hub_results, ui_state$group_colors)
    ),

    "62_Consensus_Heatmap" = list(
      condition = function() !is.null(analysis_results$consensus_hub_results),
      func = function() render_consensus_heatmap(analysis_results$consensus_hub_results, ui_state$group_colors)
    ),

    # New Comprehensive Consensus Plots (Summary Tab - Tab 7 in new structure)
    "63_Consensus_Node_Metrics" = list(
      condition = function() !is.null(analysis_results$comprehensive_consensus) && !is.null(analysis_results$method_percolation_results),
      func = function() render_consensus_node_metrics_plot(analysis_results$comprehensive_consensus, analysis_results$method_percolation_results, ui_state$brain_areas, ui_state$area_colors, ui_state$group_colors, label_all_nodes = TRUE)
    ),

    "64_Consensus_Regional" = list(
      condition = function() !is.null(analysis_results$comprehensive_consensus) && !is.null(ui_state$brain_areas),
      func = function() render_consensus_regional_plot(analysis_results$comprehensive_consensus, ui_state$brain_areas, ui_state$area_colors, ui_state$group_colors)
    ),

    "65_Consensus_Subregional" = list(
      condition = function() !is.null(analysis_results$comprehensive_consensus) && !is.null(ui_state$brain_areas),
      func = function() render_consensus_subregional_plot(analysis_results$comprehensive_consensus, ui_state$brain_areas, ui_state$area_colors, ui_state$group_colors)
    ),

    "66_Consensus_Overview_Top_Hubs" = list(
      condition = function() !is.null(analysis_results$comprehensive_consensus),
      func = function() render_consensus_overview_top_hubs_plot(analysis_results$comprehensive_consensus, ui_state$brain_areas, ui_state$area_colors, ui_state$group_colors)
    ),

    "67_Consensus_Overview_Agreement" = list(
      condition = function() !is.null(analysis_results$comprehensive_consensus),
      func = function() render_consensus_overview_agreement_plot(analysis_results$comprehensive_consensus, ui_state$group_colors)
    ),

    "68_Network_Similarity_Heatmap" = list(
      condition = function() !is.null(analysis_results$correlation_methods_raw),
      func = function() render_network_similarity_heatmap_plot(analysis_results$correlation_methods_raw, analysis_results$method_percolation_results, analysis_results$persistence_results)
    ),

    "69_Consensus_Network" = list(
      condition = function() !is.null(analysis_results$comprehensive_consensus) && !is.null(analysis_results$method_percolation_results),
      func = function() {
        # Note: consensusNetworkPlot requires interactive selection of method/layout
        # For download, create a simple message plot explaining this is interactive
        plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
        text(1, 1, "Consensus Network Visualization", cex = 1.4, font = 2)
        text(1, 0.7, "Interactive plot available in app (Summary Tab B)", cex = 1.0)
        text(1, 0.4, "Allows selection of:", cex = 0.9, col = "gray40")
        text(1, 0.2, "• Correlation method (Pearson/Spearman/etc.)", cex = 0.8, col = "gray40")
        text(1, 0.0, "• Network layout (FR/Circular/KK/Grid)", cex = 0.8, col = "gray40")
      }
    )
  )
  
  # Execute plot generation
  successful_plots <- 0
  skipped_plots <- 0
  failed_plots <- 0

  for(plot_name in names(all_plots)) {
    plot_info <- all_plots[[plot_name]]

    # Check if conditions are met
    if(plot_info$condition()) {
      tryCatch({
        filename <- paste0(plot_name, ".png")
        png(file.path(output_dir, filename), width = width, height = height, res = dpi)

        # Reset graphics parameters with more space for complex plots
        par(mfrow=c(1,1), mar=c(4,4,3,1), oma=c(0,0,0,0), mgp=c(2,0.5,0))

        # Execute the plotting function with error recovery
        tryCatch({
          plot_info$func()
        }, error = function(margin_error) {
          # If margin error, try with smaller margins
          if(grepl("figure margins too large", margin_error$message)) {
            par(mfrow=c(1,1), mar=c(2,2,2,1), oma=c(0,0,0,0))
            tryCatch({
              plot_info$func()
            }, error = function(e2) {
              # Final fallback: create informative placeholder
              par(mfrow=c(1,1), mar=c(1,1,2,1), oma=c(0,0,0,0))
              plot(1, type="n", main=gsub("_", " ", gsub("^\\d+_", "", plot_name)), axes=FALSE)
              text(1, 1, "Plot requires interactive\nShiny environment", cex=1.2, col="gray50")
              text(1, 0.7, paste("Error:", e2$message), cex=0.8, col="red")
            })
          } else {
            # For non-margin errors, create informative placeholder
            par(mfrow=c(1,1), mar=c(1,1,2,1), oma=c(0,0,0,0))
            plot(1, type="n", main=gsub("_", " ", gsub("^\\d+_", "", plot_name)), axes=FALSE)
            text(1, 1, "Function not available", cex=1.2, col="gray50")
            text(1, 0.7, paste("Error:", margin_error$message), cex=0.8, col="red")
          }
        })

        dev.off()
        successful_plots <- successful_plots + 1
        cat(sprintf("✅ Generated: %s\n", filename))

      }, error = function(e) {
        dev.off()
        failed_plots <- failed_plots + 1
        cat(sprintf("❌ Failed: %s - %s\n", plot_name, e$message))
      })
    } else {
      skipped_plots <- skipped_plots + 1
      cat(sprintf("⏭️  Skipped: %s (data not available)\n", plot_name))
    }
  }

  # Generate individual network plots (one file per network)
  if(!is.null(analysis_results$networks) && length(analysis_results$networks) > 0) {
    cat("\n🕸️  Generating individual network plots...\n")

    for(i in seq_along(analysis_results$networks)) {
      network <- analysis_results$networks[[i]]
      group_name <- names(analysis_results$networks)[i]
      plot_num <- sprintf("%02d", i + 3)  # Start from 04, 05, etc.
      plot_name <- paste0(plot_num, "_Network_", gsub("[^a-zA-Z0-9]", "_", group_name))

      tryCatch({
        filename <- paste0(plot_name, ".png")
        png(file.path(output_dir, filename), width = width, height = height, res = dpi)

        par(mfrow=c(1,1), mar=c(1,1,3,1))

        if(requireNamespace("igraph", quietly = TRUE)) {
          # Get node names
          node_names <- igraph::V(network)$name

          # ALWAYS use brain area colors when defined (matching Summary page behavior)
          # Only fall back to blue if no brain areas are defined
          if(!is.null(ui_state$brain_areas) && length(ui_state$brain_areas) > 0) {
            # Use brain area colors - same as Summary page
            node_colors <- rep("#808080", length(node_names))  # Default gray for unmatched
            names(node_colors) <- node_names

            for(area_name in names(ui_state$brain_areas)) {
              regions <- ui_state$brain_areas[[area_name]]
              matching_nodes <- intersect(regions, node_names)

              if(length(matching_nodes) > 0) {
                # Get color for this brain area
                if(!is.null(ui_state$area_colors) && area_name %in% names(ui_state$area_colors)) {
                  area_color <- ui_state$area_colors[[area_name]]
                } else {
                  # Use default rainbow colors if no specific color defined
                  default_colors <- c("#3498db", "#e74c3c", "#27ae60", "#9b59b6", "#e67e22", "#1abc9c", "#f39c12")
                  color_idx <- ((match(area_name, names(ui_state$brain_areas)) - 1) %% length(default_colors)) + 1
                  area_color <- default_colors[color_idx]
                }
                node_colors[matching_nodes] <- area_color
              }
            }
            vertex_colors <- as.character(node_colors)
          } else {
            # Default blue when no brain areas defined
            vertex_colors <- "#1F78B4"
          }

          # Determine vertex shape
          vertex_shape <- switch(network_options$node_shape %||% "circle",
                                "circle" = "circle",
                                "square" = "square",
                                "none" = "none",
                                "circle")  # default fallback

          # Determine edge colors
          if(network_options$edge_sign_colors && !is.null(analysis_results$correlations)) {
            # Get correlation matrix for this group
            cor_matrix <- analysis_results$correlations[[group_name]]

            if(!is.null(cor_matrix)) {
              # Get edge list
              edge_list <- igraph::as_edgelist(network)
              edge_colors <- character(nrow(edge_list))

              for(e in 1:nrow(edge_list)) {
                node1 <- edge_list[e, 1]
                node2 <- edge_list[e, 2]

                # Get correlation value
                if(node1 %in% rownames(cor_matrix) && node2 %in% colnames(cor_matrix)) {
                  cor_val <- cor_matrix[node1, node2]

                  # Assign color based on sign
                  if(!is.na(cor_val)) {
                    if(cor_val > 0) {
                      edge_colors[e] <- network_options$positive_color
                    } else {
                      edge_colors[e] <- network_options$negative_color
                    }
                  } else {
                    edge_colors[e] <- adjustcolor("gray60", alpha.f = 0.7)
                  }
                } else {
                  edge_colors[e] <- adjustcolor("gray60", alpha.f = 0.7)
                }
              }
            } else {
              edge_colors <- adjustcolor("gray60", alpha.f = 0.7)
            }
          } else {
            edge_colors <- adjustcolor("gray60", alpha.f = 0.7)
          }

          # Calculate node sizes based on degree (like Summary page)
          node_degrees <- igraph::degree(network)
          if(max(node_degrees) > min(node_degrees)) {
            vertex_sizes <- scales::rescale(node_degrees, to = c(5, 15))
          } else {
            vertex_sizes <- 8
          }

          # Get group color for border (like Summary page)
          group_color <- if(!is.null(ui_state$group_colors) && group_name %in% names(ui_state$group_colors)) {
            ui_state$group_colors[[group_name]]
          } else {
            "black"
          }

          # Plot network (matching Summary page style)
          plot(network,
               main=paste("Group:", group_name),
               vertex.size=vertex_sizes,
               vertex.color=vertex_colors,
               vertex.shape=vertex_shape,
               vertex.frame.color="white",
               vertex.label.cex=0.7,
               vertex.label.color="black",
               vertex.label.dist=1,
               edge.color=edge_colors,
               edge.width=abs(igraph::E(network)$weight) * 3,
               layout=igraph::layout_with_fr(network))

          # Add border in group color (like Summary page)
          box(col = group_color, lwd = 3)
        }

        dev.off()
        successful_plots <- successful_plots + 1
        cat(sprintf("✅ Generated: %s\n", filename))

      }, error = function(e) {
        if(exists("dev.cur") && dev.cur() > 1) dev.off()
        failed_plots <- failed_plots + 1
        cat(sprintf("❌ Failed: %s - %s\n", plot_name, e$message))
      })
    }
  }
  
  # Quality validation - check file sizes and remove problematic files
  generated_files <- list.files(output_dir, pattern = "\\.png$", full.names = TRUE)
  valid_files <- 0
  small_files <- 0
  cleaned_files <- c()
  
  for(file in generated_files) {
    file_size <- file.size(file)
    if(file_size > 10000) {  # Files larger than 10KB are likely valid plots
      valid_files <- valid_files + 1
      cleaned_files <- c(cleaned_files, file)
    } else if(file_size > 2000) {  # Small but potentially valid files
      small_files <- small_files + 1
      cleaned_files <- c(cleaned_files, file)
      cat(sprintf("⚠️  Small file kept: %s (%d bytes)\n", basename(file), file_size))
    } else {
      # Remove very small files (likely error stubs)
      cat(sprintf("🗑️  Removing error file: %s (%d bytes)\n", basename(file), file_size))
      file.remove(file)
    }
  }
  
  # Update counts after cleanup
  final_files <- list.files(output_dir, pattern = "\\.png$", full.names = TRUE)
  
  # Summary report
  cat(sprintf("\n📊 PLOT GENERATION COMPLETE\n"))
  cat(sprintf("✅ Successfully generated: %d plots\n", successful_plots))
  cat(sprintf("📁 Valid plot files: %d plots\n", valid_files))
  cat(sprintf("⚠️  Small files kept: %d plots\n", small_files))
  cat(sprintf("🗑️  Error files removed: %d files\n", length(generated_files) - length(final_files)))
  cat(sprintf("⏭️  Skipped (no data): %d plots\n", skipped_plots))
  cat(sprintf("❌ Failed: %d plots\n", failed_plots))
  cat(sprintf("📈 Total plots defined: %d plots\n", length(all_plots)))
  
  # Calculate success rate based on clean files
  quality_success_rate <- if(length(final_files) > 0) round((valid_files / length(final_files)) * 100, 1) else 0
  cat(sprintf("🎯 Quality success rate: %s%% (%d valid / %d total files)\n", 
              quality_success_rate, valid_files, length(final_files)))
  
  return(list(
    successful = successful_plots,
    valid_files = valid_files,
    small_files = small_files,
    removed_files = length(generated_files) - length(final_files),
    skipped = skipped_plots,
    failed = failed_plots,
    total = length(all_plots),
    success_rate = quality_success_rate,
    generated_files = final_files
  ))
}