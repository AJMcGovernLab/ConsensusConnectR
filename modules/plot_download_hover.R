# Plot Download on Hover Module
# Adds hover-triggered download buttons to all plots with customizable DPI/dimensions/format

library(shiny)
library(shinyjs)

# ============================================================================
# CSS FOR HOVER DOWNLOAD FUNCTIONALITY
# ============================================================================
hover_download_css <- function() {
  tags$style(HTML("
    /* Container for plot with hover overlay */
    .downloadable-plot-container {
      position: relative;
      display: inline-block;
      width: 100%;
    }

    /* Download button - hidden by default, appears on hover */
    .plot-download-btn {
      position: absolute;
      top: 10px;
      right: 10px;
      width: 36px;
      height: 36px;
      background: rgba(52, 152, 219, 0.85);
      border: none;
      border-radius: 6px;
      cursor: pointer;
      opacity: 0;
      transition: opacity 0.25s ease, transform 0.2s ease, background 0.2s ease;
      z-index: 100;
      display: flex;
      align-items: center;
      justify-content: center;
      padding: 0;
      box-shadow: 0 2px 8px rgba(0,0,0,0.15);
    }

    .plot-download-btn:hover {
      background: rgba(41, 128, 185, 1);
      transform: scale(1.1);
    }

    /* Show button on container hover */
    .downloadable-plot-container:hover .plot-download-btn {
      opacity: 1;
    }

    /* Download icon */
    .plot-download-btn .fa,
    .plot-download-btn .glyphicon {
      color: white;
      font-size: 16px;
    }

    /* =========================================
       DOWNLOAD MODAL STYLES
       ========================================= */
    .plot-download-modal {
      display: none;
      position: fixed;
      z-index: 2000;
      left: 0;
      top: 0;
      width: 100%;
      height: 100%;
      background-color: rgba(0,0,0,0.6);
      backdrop-filter: blur(2px);
    }

    .plot-download-modal-content {
      background-color: white;
      margin: 4% auto;
      padding: 0;
      border-radius: 12px;
      width: 500px;
      max-width: 90%;
      max-height: 90vh;
      overflow-y: auto;
      box-shadow: 0 15px 50px rgba(0,0,0,0.3);
      animation: plotModalSlideIn 0.3s ease;
    }

    @keyframes plotModalSlideIn {
      from {
        opacity: 0;
        transform: translateY(-30px) scale(0.95);
      }
      to {
        opacity: 1;
        transform: translateY(0) scale(1);
      }
    }

    .plot-download-modal-header {
      background: linear-gradient(135deg, #3498db, #2980b9);
      color: white;
      padding: 18px 24px;
      border-radius: 12px 12px 0 0;
      display: flex;
      align-items: center;
      gap: 10px;
    }

    .plot-download-modal-header h4 {
      margin: 0;
      font-size: 1.25em;
      font-weight: 600;
    }

    .plot-download-modal-header .plot-name-display {
      font-size: 0.85em;
      opacity: 0.9;
      margin-top: 4px;
      font-weight: normal;
    }

    .plot-download-modal-body {
      padding: 24px;
    }

    .plot-download-modal-footer {
      padding: 16px 24px;
      background: #f8f9fa;
      border-radius: 0 0 12px 12px;
      display: flex;
      justify-content: flex-end;
      gap: 12px;
      border-top: 1px solid #e9ecef;
    }

    /* Form styling within modal */
    .download-form-group {
      margin-bottom: 18px;
    }

    .download-form-group label {
      display: block;
      margin-bottom: 6px;
      font-weight: 600;
      color: #333;
      font-size: 0.9em;
    }

    .download-form-group .form-control {
      border-radius: 6px;
      border: 1px solid #ced4da;
      padding: 8px 12px;
    }

    .download-form-group .form-control:focus {
      border-color: #3498db;
      box-shadow: 0 0 0 3px rgba(52, 152, 219, 0.15);
    }

    .download-form-row {
      display: flex;
      gap: 16px;
    }

    .download-form-row .download-form-group {
      flex: 1;
    }

    /* Radio buttons inline styling */
    .download-format-options {
      display: flex;
      gap: 20px;
      margin-top: 8px;
    }

    .download-format-options .radio-inline {
      padding-left: 0;
    }

    /* Cancel button */
    .btn-cancel-download {
      background: #6c757d;
      color: white;
      border: none;
      padding: 10px 20px;
      border-radius: 6px;
      cursor: pointer;
      font-weight: 500;
      transition: background 0.2s ease;
    }

    .btn-cancel-download:hover {
      background: #5a6268;
    }

    /* Download button in modal */
    .btn-execute-download,
    .plot-download-modal-footer .shiny-download-link,
    .plot-download-modal-footer .btn,
    #execute_hover_download {
      background: linear-gradient(135deg, #27ae60, #2ecc71) !important;
      color: white !important;
      border: none !important;
      padding: 10px 24px !important;
      border-radius: 6px !important;
      cursor: pointer !important;
      font-weight: 600 !important;
      transition: transform 0.2s ease, box-shadow 0.2s ease !important;
      box-shadow: 0 2px 8px rgba(39, 174, 96, 0.3) !important;
      text-decoration: none !important;
      display: inline-block !important;
      pointer-events: auto !important;
      position: relative !important;
      z-index: 100 !important;
    }

    .btn-execute-download:hover,
    .plot-download-modal-footer .shiny-download-link:hover,
    #execute_hover_download:hover {
      transform: translateY(-1px);
      box-shadow: 0 4px 12px rgba(39, 174, 96, 0.4) !important;
      color: white !important;
      background: linear-gradient(135deg, #219a52, #27ae60) !important;
    }

    /* Ensure modal footer buttons are clickable */
    .plot-download-modal-footer {
      position: relative;
      z-index: 10;
    }

    .plot-download-modal-footer * {
      pointer-events: auto;
    }

    /* Shiny selectInput and numericInput overrides for modal */
    .plot-download-modal-body .selectize-input {
      border-radius: 6px !important;
    }

    .plot-download-modal-body input[type='number'] {
      border-radius: 6px;
    }

    /* =========================================
       PLOT PREVIEW STYLES
       ========================================= */
    .plot-preview-container {
      background: #f8f9fa;
      border: 1px solid #e9ecef;
      border-radius: 8px;
      padding: 8px;
      margin-bottom: 20px;
      text-align: center;
    }

    .plot-preview-container .plot-preview-label {
      font-size: 0.8em;
      color: #6c757d;
      margin-bottom: 8px;
      font-weight: 500;
    }

    .plot-preview-container .shiny-plot-output {
      background: white;
      border-radius: 6px;
      overflow: hidden;
      box-shadow: 0 1px 3px rgba(0,0,0,0.1);
    }

    /* Loading state for preview */
    .plot-preview-loading {
      display: flex;
      align-items: center;
      justify-content: center;
      height: 150px;
      color: #6c757d;
    }

    .plot-preview-loading::after {
      content: 'Loading preview...';
      animation: pulse 1.5s ease-in-out infinite;
    }

    @keyframes pulse {
      0%, 100% { opacity: 0.5; }
      50% { opacity: 1; }
    }
  "))
}

# ============================================================================
# JAVASCRIPT FOR HOVER DOWNLOAD FUNCTIONALITY
# ============================================================================
hover_download_js <- function() {
  tags$script(HTML("
    // Store current plot ID for download
    var currentDownloadPlotId = null;

    // Open download modal
    function openPlotDownloadModal(plotId) {
      currentDownloadPlotId = plotId;
      console.log('[Plot Download] Opening modal for: ' + plotId);
      var modal = document.getElementById('plot-download-modal');
      if (modal) {
        modal.style.display = 'block';
        // Update plot name display
        var plotNameEl = document.getElementById('download_plot_name');
        if (plotNameEl) {
          // Format plot ID for display (convert camelCase to Title Case)
          var displayName = plotId
            .replace(/([A-Z])/g, ' $1')
            .replace(/^./, function(str){ return str.toUpperCase(); })
            .replace('Plot', '')
            .trim();
          plotNameEl.innerText = displayName;
        }
        // Send plot ID to Shiny with event priority
        Shiny.setInputValue('hover_download_plot_id', plotId, {priority: 'event'});

        // Force a resize event after a short delay to help render hidden elements
        setTimeout(function() {
          window.dispatchEvent(new Event('resize'));
          // Also trigger Shiny binding refresh
          if (typeof Shiny !== 'undefined' && Shiny.bindAll) {
            Shiny.bindAll(modal);
          }
          console.log('[Plot Download] Modal initialized');
        }, 100);
      }
    }

    // Close download modal
    function closePlotDownloadModal() {
      var modal = document.getElementById('plot-download-modal');
      if (modal) {
        modal.style.display = 'none';
      }
      currentDownloadPlotId = null;
    }

    // Close modal on outside click
    document.addEventListener('click', function(event) {
      var modal = document.getElementById('plot-download-modal');
      if (event.target === modal) {
        closePlotDownloadModal();
      }
    });

    // Close modal on Escape key
    document.addEventListener('keydown', function(event) {
      if (event.key === 'Escape') {
        closePlotDownloadModal();
      }
    });

    // Custom message handler for closing modal after download
    Shiny.addCustomMessageHandler('close_plot_download_modal', function(message) {
      closePlotDownloadModal();
    });

    // Custom message handler for showing error
    Shiny.addCustomMessageHandler('plot_download_error', function(message) {
      alert('Download Error: ' + message.error);
    });

    // Custom message handler for confirming plot is ready
    Shiny.addCustomMessageHandler('plot_download_ready', function(message) {
      console.log('[Plot Download] Ready to render: ' + message.plot_id);
    });

  "))
}

# ============================================================================
# WRAPPER FUNCTION FOR DOWNLOADABLE PLOT OUTPUT
# ============================================================================
#' Create a plot output with hover-triggered download button
#'
#' @param outputId The output ID for the plot
#' @param height Height of the plot (default "400px")
#' @param width Width of the plot (default "100%")
#' @return A div containing the plot and download button
downloadablePlotOutput <- function(outputId, height = "400px", width = "100%") {
  div(
    class = "downloadable-plot-container",
    plotOutput(outputId, height = height, width = width),
    tags$button(
      class = "plot-download-btn",
      onclick = sprintf("openPlotDownloadModal('%s')", outputId),
      title = "Download this plot",
      icon("download")
    )
  )
}

# ============================================================================
# MODAL UI FOR DOWNLOAD SETTINGS
# ============================================================================
plot_download_modal_ui <- function() {
  div(
    id = "plot-download-modal",
    class = "plot-download-modal",
    div(
      class = "plot-download-modal-content",
      # Header
      div(
        class = "plot-download-modal-header",
        div(
          icon("download", style = "font-size: 1.3em;"),
          div(
            tags$h4("Download Plot"),
            div(id = "download_plot_name", class = "plot-name-display")
          )
        )
      ),
      # Body - Form inputs
      div(
        class = "plot-download-modal-body",
        # Plot Preview - renders at actual dimensions, scaled to fit
        div(
          class = "plot-preview-container",
          div(class = "plot-preview-label", "Preview (actual render)"),
          imageOutput("hover_download_preview", height = "auto")
        ),
        # DPI Selection
        div(
          class = "download-form-group",
          tags$label("Resolution (DPI)"),
          selectInput(
            "hover_download_dpi",
            NULL,
            choices = c(
              "72 - Screen/Web" = "72",
              "150 - Draft Print" = "150",
              "300 - Publication Quality" = "300",
              "600 - High Resolution" = "600"
            ),
            selected = "300",
            width = "100%"
          )
        ),
        # Dimensions Row
        div(
          class = "download-form-row",
          div(
            class = "download-form-group",
            tags$label("Width (inches)"),
            numericInput(
              "hover_download_width",
              NULL,
              value = 10,
              min = 2,
              max = 24,
              step = 0.5,
              width = "100%"
            )
          ),
          div(
            class = "download-form-group",
            tags$label("Height (inches)"),
            numericInput(
              "hover_download_height",
              NULL,
              value = 8,
              min = 2,
              max = 24,
              step = 0.5,
              width = "100%"
            )
          )
        ),
        # Format Selection
        div(
          class = "download-form-group",
          tags$label("Format"),
          radioButtons(
            "hover_download_format",
            NULL,
            choices = c("PNG" = "png", "PDF" = "pdf"),
            selected = "png",
            inline = TRUE
          )
        )
      ),
      # Footer - Buttons
      div(
        class = "plot-download-modal-footer",
        tags$button(
          class = "btn-cancel-download",
          onclick = "closePlotDownloadModal()",
          "Cancel"
        ),
        # Direct download button - simplest approach
        downloadButton(
          "execute_hover_download",
          "Download",
          class = "btn-execute-download"
        )
      )
    )
  )
}

# ============================================================================
# COMPLETE PLOT REGISTRY
# Maps all plot IDs to their render functions
# ============================================================================
create_hover_plot_registry <- function(analysis_results, ui_state, input) {

  # Helper function to get group colors safely
  get_group_colors <- function() {
    if(!is.null(ui_state$group_colors) && length(ui_state$group_colors) > 0) {
      return(ui_state$group_colors)
    }
    return(NULL)
  }

  # Helper to get current method
  get_current_method <- function() {
    tolower(input$topology_method %||% "pearson")
  }

  # Helper to get current layout
  get_current_layout <- function() {
    input$networkLayout %||% "fr"
  }

  list(
    # =========================================================================
    # IMPUTATION & DATA QUALITY PLOTS
    # =========================================================================
    "imputationPlots" = list(
      condition = function() !is.null(analysis_results$imputation),
      render = function() render_imputation_plots(analysis_results, analysis_results$raw_data)
    ),

    # =========================================================================
    # CORRELATION ANALYSIS PLOTS
    # =========================================================================
    "correlationPlots" = list(
      condition = function() !is.null(analysis_results$correlations),
      render = function() render_correlation_plots(analysis_results$correlations, get_group_colors())
    ),

    "correlationDistributionPlot" = list(
      condition = function() !is.null(analysis_results$correlations),
      render = function() render_correlation_distributions(analysis_results$correlations)
    ),

    "method_correlation_heatmap" = list(
      condition = function() !is.null(analysis_results$correlation_methods_raw),
      render = function() {
        render_method_correlation_heatmap(
          analysis_results$correlation_methods_raw,
          get_group_colors()
        )
      }
    ),

    "method_percolation_curve" = list(
      condition = function() !is.null(analysis_results$method_percolation_results),
      render = function() {
        render_method_percolation_curves(
          analysis_results$method_percolation_results,
          get_group_colors()
        )
      }
    ),

    # =========================================================================
    # NETWORK TOPOLOGY PLOTS
    # =========================================================================
    "groupPercolationPlot" = list(
      condition = function() !is.null(analysis_results$correlations) && !is.null(analysis_results$group_thresholds),
      render = function() render_group_specific_percolation(analysis_results$correlations, analysis_results$group_thresholds)
    ),

    "networkPlots" = list(
      condition = function() {
        method <- get_current_method()
        method_data <- analysis_results$method_percolation_results[[method]]
        !is.null(method_data) && !is.null(method_data$networks)
      },
      render = function() {
        method <- get_current_method()
        method_data <- analysis_results$method_percolation_results[[method]]
        create_enhanced_network_plots(
          networks = method_data$networks,
          brain_areas = ui_state$brain_areas,
          area_colors = ui_state$area_colors,
          group_colors = ui_state$group_colors,
          layout = get_current_layout()
        )
      }
    ),

    "networkGalleryPlot" = list(
      condition = function() {
        method <- get_current_method()
        method_data <- analysis_results$method_percolation_results[[method]]
        !is.null(method_data) && !is.null(method_data$networks)
      },
      render = function() {
        method <- get_current_method()
        method_data <- analysis_results$method_percolation_results[[method]]
        render_network_gallery(
          method_data$networks,
          ui_state$brain_areas,
          ui_state$area_colors
        )
      }
    ),

    "networkDashboardPlot" = list(
      condition = function() {
        method <- get_current_method()
        method_data <- analysis_results$method_percolation_results[[method]]
        !is.null(method_data) && !is.null(method_data$global_metrics)
      },
      render = function() {
        method <- get_current_method()
        method_data <- analysis_results$method_percolation_results[[method]]
        render_network_dashboard(method_data$global_metrics, get_group_colors())
      }
    ),

    "percolationStrengthEigenvectorPlot" = list(
      condition = function() !is.null(analysis_results$method_percolation_results),
      render = function() {
        method <- get_current_method()
        method_data <- analysis_results$method_percolation_results[[method]]
        if(!is.null(method_data) && !is.null(method_data$node_metrics)) {
          render_percolation_strength_eigenvector_relationship(
            method_data$node_metrics,
            get_group_colors(),
            ui_state$brain_areas,
            ui_state$area_colors
          )
        }
      }
    ),

    # =========================================================================
    # NODE CENTRALITY PLOTS
    # =========================================================================
    "nodeCentralityPlot" = list(
      condition = function() !is.null(analysis_results$method_percolation_results),
      render = function() {
        method <- get_current_method()
        method_data <- analysis_results$method_percolation_results[[method]]
        if(!is.null(method_data) && !is.null(method_data$brain_area_metrics)) {
          render_node_centrality(method_data$brain_area_metrics, ui_state$brain_areas, ui_state$area_colors)
        }
      }
    ),

    "nodeHeatmapPlot" = list(
      condition = function() !is.null(analysis_results$method_percolation_results),
      render = function() {
        method <- get_current_method()
        method_data <- analysis_results$method_percolation_results[[method]]
        if(!is.null(method_data) && !is.null(method_data$brain_area_metrics)) {
          render_node_heatmap(method_data$brain_area_metrics, ui_state$brain_areas, ui_state$area_colors)
        }
      }
    ),

    "individualNodeCentralityPlot" = list(
      condition = function() !is.null(analysis_results$method_percolation_results),
      render = function() {
        method <- get_current_method()
        method_data <- analysis_results$method_percolation_results[[method]]
        if(!is.null(method_data) && !is.null(method_data$node_metrics)) {
          render_individual_node_centrality(method_data$node_metrics, ui_state$brain_areas, ui_state$area_colors)
        }
      }
    ),

    "individualNodeHeatmapPlot" = list(
      condition = function() !is.null(analysis_results$method_percolation_results),
      render = function() {
        method <- get_current_method()
        method_data <- analysis_results$method_percolation_results[[method]]
        if(!is.null(method_data) && !is.null(method_data$node_metrics)) {
          render_individual_node_heatmap(method_data$node_metrics, ui_state$brain_areas, ui_state$area_colors)
        }
      }
    ),

    "brainAreaConnectivityPlot" = list(
      condition = function() !is.null(analysis_results$method_percolation_results),
      render = function() {
        method <- get_current_method()
        method_data <- analysis_results$method_percolation_results[[method]]
        if(!is.null(method_data) && !is.null(method_data$brain_area_metrics)) {
          render_brain_area_connectivity(method_data$brain_area_metrics, ui_state$brain_areas, ui_state$area_colors)
        }
      }
    ),

    "avgEigenvectorPlot" = list(
      condition = function() !is.null(analysis_results$method_percolation_results),
      render = function() {
        method <- get_current_method()
        method_data <- analysis_results$method_percolation_results[[method]]
        if(!is.null(method_data) && !is.null(method_data$node_metrics)) {
          render_avg_eigenvector_by_region(
            method_data$weighted_eigenvector,
            method_data$node_metrics,
            ui_state$brain_areas,
            ui_state$area_colors,
            get_group_colors()
          )
        }
      }
    ),

    "combinedEigenvectorBarPlot" = list(
      condition = function() !is.null(analysis_results$method_percolation_results),
      render = function() {
        method <- get_current_method()
        method_data <- analysis_results$method_percolation_results[[method]]
        if(!is.null(method_data) && !is.null(method_data$weighted_eigenvector)) {
          render_combined_eigenvector_bar_plot(
            method_data$weighted_eigenvector,
            ui_state$brain_areas,
            ui_state$area_colors,
            get_group_colors()
          )
        }
      }
    ),

    # =========================================================================
    # EDGE METRICS PLOTS
    # =========================================================================
    "edgeMetricsPlot" = list(
      condition = function() !is.null(analysis_results$method_percolation_results),
      render = function() {
        method <- get_current_method()
        method_data <- analysis_results$method_percolation_results[[method]]
        if(!is.null(method_data) && !is.null(method_data$edge_metrics)) {
          render_edge_metrics(method_data$edge_metrics, get_group_colors())
        }
      }
    ),

    # =========================================================================
    # NETWORK SIMILARITY PLOTS
    # =========================================================================
    "networkSimilarityPlot" = list(
      condition = function() !is.null(analysis_results$method_percolation_results),
      render = function() {
        method <- get_current_method()
        method_data <- analysis_results$method_percolation_results[[method]]
        if(!is.null(method_data) && !is.null(method_data$similarities)) {
          render_network_similarity_matrix(method_data$similarities)
        }
      }
    ),

    "networkSimilarityHeatmapPlot" = list(
      condition = function() !is.null(analysis_results$correlation_methods_raw) && !is.null(analysis_results$method_percolation_results),
      render = function() {
        render_network_similarity_heatmap_plot(
          analysis_results$correlation_methods_raw,
          analysis_results$method_percolation_results,
          analysis_results$persistence_results
        )
      }
    ),

    "networkSimilarityWeightedPlot" = list(
      condition = function() !is.null(analysis_results$method_weighted_results),
      render = function() {
        render_network_similarity_weighted(analysis_results$method_weighted_results)
      }
    ),

    "networkSimilarityPercolationPlot" = list(
      condition = function() !is.null(analysis_results$method_percolation_results),
      render = function() {
        render_network_similarity_percolation(analysis_results$method_percolation_results)
      }
    ),

    "networkSimilarityPersistencePlot" = list(
      condition = function() !is.null(analysis_results$persistence_results),
      render = function() {
        render_network_similarity_persistence(analysis_results$persistence_results)
      }
    ),

    "networkSimilarityMatrixPlot" = list(
      condition = function() !is.null(analysis_results$method_percolation_results),
      render = function() {
        method <- get_current_method()
        method_data <- analysis_results$method_percolation_results[[method]]
        if(!is.null(method_data) && !is.null(method_data$similarities)) {
          render_network_similarity_matrix(method_data$similarities)
        }
      }
    ),

    # =========================================================================
    # HUB CONSERVATION PLOTS
    # =========================================================================
    "hubConservationPlot" = list(
      condition = function() {
        method <- get_current_method()
        method_data <- analysis_results$method_percolation_results[[method]]
        !is.null(method_data) && !is.null(method_data$conservation) &&
        !is.null(method_data$conservation$conservation_possible) &&
        method_data$conservation$conservation_possible
      },
      render = function() {
        method <- get_current_method()
        method_data <- analysis_results$method_percolation_results[[method]]
        render_hub_conservation(method_data$conservation)
      }
    ),

    "edgeConservationPlot" = list(
      condition = function() !is.null(analysis_results$conservation_analysis),
      render = function() {
        render_network_similarity_heatmap(analysis_results$conservation_analysis)
      }
    ),

    "conservationStatsPlot" = list(
      condition = function() !is.null(analysis_results$method_percolation_results),
      render = function() {
        method <- get_current_method()
        method_data <- analysis_results$method_percolation_results[[method]]
        if(!is.null(method_data) && !is.null(method_data$similarities)) {
          render_conservation_stats(method_data$similarities)
        }
      }
    ),

    # =========================================================================
    # WEIGHTED NETWORK ANALYSIS PLOTS
    # =========================================================================
    "weightedNodeMetricsPlot" = list(
      condition = function() !is.null(analysis_results$method_weighted_results),
      render = function() {
        method <- get_current_method()
        method_data <- analysis_results$method_weighted_results[[method]]
        if(!is.null(method_data)) {
          render_weighted_node_metrics(method_data, get_group_colors())
        }
      }
    ),

    "allWeightedStatsPlot" = list(
      condition = function() !is.null(analysis_results$method_weighted_results),
      render = function() {
        render_all_weighted_stats(analysis_results$method_weighted_results, get_group_colors())
      }
    ),

    "weightedEigenvectorHubPlot" = list(
      condition = function() !is.null(analysis_results$method_weighted_results),
      render = function() {
        method <- get_current_method()
        method_data <- analysis_results$method_weighted_results[[method]]
        if(!is.null(method_data)) {
          render_weighted_eigenvector_hubs(method_data, ui_state$brain_areas, ui_state$area_colors)
        }
      }
    ),

    "weightedEigenvectorComparison" = list(
      condition = function() !is.null(analysis_results$method_weighted_results),
      render = function() {
        render_weighted_eigenvector_comparison(
          analysis_results$method_weighted_results,
          ui_state$brain_areas,
          ui_state$area_colors,
          get_group_colors()
        )
      }
    ),

    "strengthEigenvectorPlot" = list(
      condition = function() !is.null(analysis_results$method_weighted_results),
      render = function() {
        render_strength_eigenvector_relationship(
          analysis_results$method_weighted_results,
          get_group_colors()
        )
      }
    ),

    "weightedVsUnweightedPlot" = list(
      condition = function() !is.null(analysis_results$method_weighted_results) && !is.null(analysis_results$method_percolation_results),
      render = function() {
        render_weighted_vs_unweighted_comparison(
          analysis_results$method_weighted_results,
          analysis_results$method_percolation_results,
          get_group_colors()
        )
      }
    ),

    "eigenvectorStabilityPlot" = list(
      condition = function() !is.null(analysis_results$method_weighted_results),
      render = function() {
        render_eigenvector_stability(analysis_results$method_weighted_results, get_group_colors())
      }
    ),

    "eigenvectorRankChangePlot" = list(
      condition = function() !is.null(analysis_results$method_weighted_results),
      render = function() {
        render_eigenvector_rank_change(
          analysis_results$method_weighted_results,
          ui_state$brain_areas,
          ui_state$area_colors
        )
      }
    ),

    "weightedSimilarityPlot" = list(
      condition = function() !is.null(analysis_results$method_weighted_results),
      render = function() {
        render_weighted_similarity(analysis_results$method_weighted_results)
      }
    ),

    "weightedHubConservationPlot" = list(
      condition = function() !is.null(analysis_results$method_weighted_results),
      render = function() {
        render_weighted_hub_conservation(analysis_results$method_weighted_results)
      }
    ),

    "weightedEdgeStatisticsPlot" = list(
      condition = function() !is.null(analysis_results$method_weighted_results),
      render = function() {
        render_weighted_edge_statistics(analysis_results$method_weighted_results, get_group_colors())
      }
    ),

    # =========================================================================
    # CONSENSUS ANALYSIS PLOTS
    # =========================================================================
    "consensusNodeMetricsAcrossMethodsPlot" = list(
      condition = function() !is.null(analysis_results$comprehensive_consensus) && !is.null(analysis_results$method_percolation_results),
      render = function() {
        selected_methods <- input$selected_correlation_methods
        all_groups <- names(analysis_results$comprehensive_consensus)
        if(length(all_groups) == 0) {
          plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
          text(1, 1, "No consensus data available", cex = 1.2, col = "gray")
          return()
        }

        methods <- filter_common_methods(analysis_results$comprehensive_consensus, selected_methods)
        if(length(methods) == 0) {
          plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
          text(1, 1, "No common methods across all groups", cex = 1.2, col = "gray")
          return()
        }

        n_combos <- length(methods) * 3
        n_groups <- length(all_groups)
        par(mfrow = c(2, 2), mar = c(5, 5, 4, 2))

        for(group in all_groups[1:min(4, n_groups)]) {
          tryCatch({
            consensus_df <- compute_comprehensive_rank_consensus(
              group = group,
              method_percolation_results = analysis_results$method_percolation_results,
              method_weighted_results = analysis_results$method_weighted_results,
              persistence_results = analysis_results$persistence_results,
              methods = methods
            )

            if(is.null(consensus_df) || nrow(consensus_df) == 0) {
              plot(1, type = "n", axes = FALSE, main = group)
              text(1, 1, "No data available", cex = 1.0)
              next
            }

            # Get colors by brain region
            plot_colors <- rep("steelblue", nrow(consensus_df))
            names(plot_colors) <- consensus_df$Node
            if(!is.null(ui_state$brain_areas) && !is.null(ui_state$area_colors)) {
              for(node in consensus_df$Node) {
                for(area_name in names(ui_state$brain_areas)) {
                  if(node %in% ui_state$brain_areas[[area_name]]) {
                    if(area_name %in% names(ui_state$area_colors)) {
                      plot_colors[node] <- ui_state$area_colors[[area_name]]
                    }
                    break
                  }
                }
              }
            }

            n_methods <- consensus_df$N_Methods[1]
            plot(consensus_df$Consensus_NodeStrength, consensus_df$Consensus_Eigenvector,
                 pch = 21, cex = 2.2,
                 bg = adjustcolor(plot_colors[consensus_df$Node], alpha.f = 0.6),
                 col = adjustcolor(plot_colors[consensus_df$Node], alpha.f = 0.8),
                 lwd = 2,
                 xlab = "Consensus Node Strength (0 = lowest, 1 = highest)",
                 ylab = "Consensus Eigenvector (0 = lowest, 1 = highest)",
                 main = paste0("Rank-Based Consensus (", n_methods, " method-approach combos) - ", group),
                 xlim = c(0, 1.05), ylim = c(0, 1.05))

            # Add node labels
            top_nodes <- consensus_df[order(consensus_df$Consensus_Eigenvector, decreasing = TRUE)[1:min(10, nrow(consensus_df))], ]
            if(nrow(top_nodes) > 0 && requireNamespace("ggrepel", quietly = TRUE)) {
              text(top_nodes$Consensus_NodeStrength, top_nodes$Consensus_Eigenvector,
                   labels = top_nodes$Node, pos = 3, cex = 0.7, font = 2, col = "black")
            }
            grid(col = "gray90")
            abline(0, 1, lty = 2, col = "gray70")
          }, error = function(e) {
            plot(1, type = "n", axes = FALSE, main = group)
            text(1, 1, paste("Error:", substr(e$message, 1, 30)), cex = 0.8)
          })
        }
      }
    ),

    "consensusNodeRanksPlot" = list(
      condition = function() !is.null(analysis_results$comprehensive_consensus),
      render = function() {
        render_consensus_node_ranks_plot(
          analysis_results$comprehensive_consensus,
          ui_state$brain_areas,
          ui_state$area_colors,
          get_group_colors()
        )
      }
    ),

    "consensusNetworkPlot" = list(
      condition = function() !is.null(analysis_results$comprehensive_consensus) && !is.null(analysis_results$method_percolation_results),
      render = function() {
        method <- tolower(input$consensus_network_method %||% "pearson")
        layout_type <- input$consensus_network_layout %||% "fr"
        selected_methods <- input$selected_correlation_methods

        perc_data <- analysis_results$method_percolation_results[[method]]
        if(is.null(perc_data) || is.null(perc_data$networks)) {
          plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
          text(1, 1, paste("No percolation networks for", method), cex = 1.2, col = "gray")
          return()
        }

        networks <- perc_data$networks
        all_groups <- names(networks)
        if(length(all_groups) == 0) {
          plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
          text(1, 1, "No networks available", cex = 1.2, col = "gray")
          return()
        }

        # Get consensus eigenvector for each group
        methods <- filter_common_methods(analysis_results$comprehensive_consensus, selected_methods)
        consensus_eigenvector_by_group <- list()

        for(group in all_groups) {
          consensus_df <- compute_comprehensive_rank_consensus(
            group = group,
            method_percolation_results = analysis_results$method_percolation_results,
            method_weighted_results = analysis_results$method_weighted_results,
            persistence_results = analysis_results$persistence_results,
            methods = methods
          )
          if(!is.null(consensus_df) && nrow(consensus_df) > 0) {
            consensus_eig <- consensus_df$Consensus_Eigenvector
            names(consensus_eig) <- consensus_df$Node
            consensus_eigenvector_by_group[[group]] <- consensus_eig
          }
        }

        n_groups <- length(all_groups)
        if(n_groups <= 2) {
          par(mfrow = c(1, n_groups), mar = c(2, 2, 3, 2))
        } else {
          par(mfrow = c(2, 2), mar = c(2, 2, 3, 2))
        }

        for(group in all_groups[1:min(4, n_groups)]) {
          network <- networks[[group]]
          consensus_eig <- consensus_eigenvector_by_group[[group]]

          if(is.null(network) || igraph::vcount(network) == 0 || is.null(consensus_eig)) {
            plot(1, type = "n", xlab = "", ylab = "", axes = FALSE, main = paste("Group:", group))
            text(1, 1, "No network data", cex = 1.2)
            next
          }

          # Set node colors
          if(!is.null(ui_state$brain_areas) && !is.null(ui_state$area_colors)) {
            node_colors <- rep("#808080", igraph::vcount(network))
            for(area_name in names(ui_state$brain_areas)) {
              regions <- ui_state$brain_areas[[area_name]]
              matching_nodes <- intersect(regions, igraph::V(network)$name)
              if(length(matching_nodes) > 0) {
                node_indices <- which(igraph::V(network)$name %in% matching_nodes)
                if(area_name %in% names(ui_state$area_colors)) {
                  node_colors[node_indices] <- ui_state$area_colors[[area_name]]
                }
              }
            }
            igraph::V(network)$color <- node_colors
          } else {
            igraph::V(network)$color <- "#1F78B4"
          }

          # Set node sizes based on consensus eigenvector
          node_names <- igraph::V(network)$name
          node_eig_values <- sapply(node_names, function(n) {
            if(n %in% names(consensus_eig)) consensus_eig[n] else 0
          })
          if(max(node_eig_values) > min(node_eig_values)) {
            igraph::V(network)$size <- scales::rescale(node_eig_values, to = c(5, 20))
          } else {
            igraph::V(network)$size <- 10
          }

          # Create layout
          if(layout_type == "circle") {
            graph_layout <- igraph::layout_in_circle(network)
          } else if(layout_type == "kk") {
            graph_layout <- igraph::layout_with_kk(network)
          } else {
            graph_layout <- igraph::layout_with_fr(network)
          }

          group_color <- if(!is.null(ui_state$group_colors) && group %in% names(ui_state$group_colors)) {
            ui_state$group_colors[[group]]
          } else { "black" }

          plot(network, layout = graph_layout,
               vertex.color = igraph::V(network)$color,
               vertex.size = igraph::V(network)$size,
               vertex.label = igraph::V(network)$name,
               vertex.label.cex = 0.6, vertex.label.color = "black",
               vertex.label.dist = 0, vertex.label.font = 2,
               vertex.frame.color = "white",
               edge.width = abs(igraph::E(network)$weight) * 2.5,
               edge.color = adjustcolor("gray50", alpha.f = 0.6),
               main = paste(group, "- Rank-Based Consensus Eigenvector"),
               sub = paste(toupper(method), "percolation network | Node size = consensus score (0-1)"))
          box(col = group_color, lwd = 3)
        }
        par(mfrow = c(1, 1))
      }
    ),

    "consensusRegionalPlot" = list(
      condition = function() !is.null(analysis_results$comprehensive_consensus) && !is.null(ui_state$brain_areas),
      render = function() {
        render_consensus_regional_plot(
          analysis_results$comprehensive_consensus,
          ui_state$brain_areas,
          ui_state$area_colors,
          get_group_colors(),
          selected_methods = input$selected_correlation_methods
        )
      }
    ),

    "consensusSubregionalPlot" = list(
      condition = function() !is.null(analysis_results$comprehensive_consensus) && !is.null(ui_state$brain_areas),
      render = function() {
        selected_methods <- input$selected_correlation_methods
        all_groups <- names(analysis_results$comprehensive_consensus)
        if(length(all_groups) == 0) {
          plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
          text(1, 1, "No consensus data available", cex = 1.2, col = "gray")
          return()
        }

        region_names <- names(ui_state$brain_areas)
        n_regions <- length(region_names)
        methods <- filter_common_methods(analysis_results$comprehensive_consensus, selected_methods)

        # Compute consensus for each group
        all_consensus_data <- list()
        for(group in all_groups) {
          consensus_df <- compute_comprehensive_rank_consensus(
            group = group,
            method_percolation_results = analysis_results$method_percolation_results,
            method_weighted_results = analysis_results$method_weighted_results,
            persistence_results = analysis_results$persistence_results,
            methods = methods
          )
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
          if(!is.null(ui_state$group_colors[[g]])) ui_state$group_colors[[g]] else "#3498db"
        })

        par(mfrow = c(n_regions, 1), mar = c(8, 5, 3, 2))

        for(region_name in region_names) {
          region_nodes <- ui_state$brain_areas[[region_name]]
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

          if(all(is.na(region_data))) {
            plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
            text(1, 1, paste("No data for", region_name), cex = 1, col = "gray")
            next
          }

          bp <- barplot(t(region_data), beside = TRUE, names.arg = region_nodes,
                        main = paste("Region:", region_name),
                        ylab = "Consensus Eigenvector Score (0-1)",
                        col = group_colors, border = group_colors,
                        las = 2, cex.names = 0.8, ylim = c(0, 1.1))
          grid(nx = NA, ny = NULL, col = "gray90")
          if(region_name == region_names[1]) {
            legend("topright", legend = all_groups, fill = group_colors, bty = "n", cex = 0.8)
          }
        }
      }
    ),

    "comprehensiveConsensusHeatmapPlot" = list(
      condition = function() !is.null(analysis_results$comprehensive_consensus),
      render = function() {
        render_comprehensive_consensus_heatmap(
          analysis_results$comprehensive_consensus,
          get_group_colors()
        )
      }
    ),

    "threeWayHubVennPlot" = list(
      condition = function() !is.null(analysis_results$comprehensive_consensus),
      render = function() {
        render_three_way_hub_venn(
          analysis_results$comprehensive_consensus,
          get_group_colors()
        )
      }
    ),

    "agreementMatrixPlot" = list(
      condition = function() !is.null(analysis_results$comprehensive_consensus),
      render = function() {
        render_agreement_matrix_plot(analysis_results$comprehensive_consensus)
      }
    ),

    "consensusOverviewTopHubsPlot" = list(
      condition = function() !is.null(analysis_results$comprehensive_consensus),
      render = function() {
        render_consensus_overview_top_hubs_plot(
          analysis_results$comprehensive_consensus,
          ui_state$brain_areas,
          ui_state$area_colors,
          get_group_colors()
        )
      }
    ),

    "consensusOverviewAgreementPlot" = list(
      condition = function() !is.null(analysis_results$comprehensive_consensus),
      render = function() {
        render_consensus_overview_agreement_plot(
          analysis_results$comprehensive_consensus,
          get_group_colors()
        )
      }
    ),

    "consensusHubRankingPlot" = list(
      condition = function() !is.null(analysis_results$consensus_hub_results),
      render = function() {
        render_consensus_hub_ranking(analysis_results$consensus_hub_results, get_group_colors())
      }
    ),

    "consensusRegionalThreeWayPlot" = list(
      condition = function() !is.null(analysis_results$comprehensive_consensus),
      render = function() {
        render_consensus_regional_three_way(
          analysis_results$comprehensive_consensus,
          ui_state$brain_areas,
          ui_state$area_colors,
          get_group_colors()
        )
      }
    ),

    "consensusThreeWayEigenvectorPlot" = list(
      condition = function() !is.null(analysis_results$method_weighted_results) && !is.null(analysis_results$method_percolation_results),
      render = function() {
        render_consensus_three_way_eigenvector(
          analysis_results$method_weighted_results,
          analysis_results$method_percolation_results,
          analysis_results$persistence_results,
          get_group_colors()
        )
      }
    ),

    "granularConsensusHeatmap" = list(
      condition = function() !is.null(analysis_results$comprehensive_consensus),
      render = function() {
        render_granular_consensus_heatmap(
          analysis_results$comprehensive_consensus,
          get_group_colors()
        )
      }
    ),

    "consensus_hub_agreement_matrix" = list(
      condition = function() !is.null(analysis_results$comprehensive_consensus),
      render = function() {
        render_consensus_hub_agreement_matrix(
          analysis_results$comprehensive_consensus
        )
      }
    ),

    "consensus_overview_plot" = list(
      condition = function() !is.null(analysis_results$comprehensive_consensus),
      render = function() {
        render_consensus_overview(
          analysis_results$comprehensive_consensus,
          get_group_colors()
        )
      }
    ),

    # =========================================================================
    # REGIONAL CONTRIBUTION ANALYSIS PLOTS
    # Uses analysis_results$regional_contribution stored by observer in app.R
    # =========================================================================
    "regionalContributionBarPlot" = list(
      condition = function() !is.null(analysis_results$regional_contribution),
      render = function() {
        results <- analysis_results$regional_contribution
        if(is.null(results)) {
          plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
          text(1, 1, "Run Regional Contribution Analysis first", cex = 1.1, col = "gray50")
          return()
        }

        # Handle artificial/brute force mode
        if(!is.null(results$mode) && results$mode == "artificial") {
          if(!is.null(results$method) && results$method == "brute_force") {
            discovery <- results$discovery
            if(!is.null(discovery) && !is.null(discovery$top_dissimilarity)) {
              par(mfrow = c(1, 2), mar = c(8, 4, 3, 1))
              top_d <- head(discovery$top_dissimilarity, 10)
              if(nrow(top_d) > 0) {
                colors_d <- ifelse(top_d$significant, "#E74C3C", "#FFCCCC")
                barplot(top_d$contribution, names.arg = top_d$nodes, las = 2,
                        col = colors_d, cex.names = 0.6, main = "Top Dissimilarity",
                        ylab = "Contribution")
              }
              top_s <- head(discovery$top_similarity, 10)
              if(nrow(top_s) > 0) {
                colors_s <- ifelse(top_s$significant, "#3498DB", "#CCDDFF")
                barplot(top_s$contribution, names.arg = top_s$nodes, las = 2,
                        col = colors_s, cex.names = 0.6, main = "Top Similarity",
                        ylab = "Contribution")
              }
            }
          } else {
            # Greedy discovery
            if(!is.null(results$discovery) && !is.null(results$discovery$best_result)) {
              par(mar = c(8, 4, 3, 2))
              best <- results$discovery$best_result
              nodes <- strsplit(best$nodes, ", ")[[1]]
              barplot(rep(best$contribution / length(nodes), length(nodes)),
                      names.arg = nodes, las = 2, col = "#E74C3C",
                      main = paste("Best Artificial Region (n=", length(nodes), ")", sep = ""),
                      ylab = "Contribution per Node")
            }
          }
        } else if(!is.null(results$mode) && results$mode == "hypothesis") {
          # Hypothesis testing mode
          if(!is.null(results$results) && !is.null(results$results$results)) {
            par(mar = c(10, 4, 3, 2))
            res_df <- results$results$results
            colors <- ifelse(res_df$Significant, "#E74C3C", "#CCCCCC")
            barplot(res_df$Contribution, names.arg = res_df$Regions, las = 2,
                    col = colors, cex.names = 0.7, main = "Hypothesis Test Results",
                    ylab = "Contribution")
          }
        } else {
          # Standard brain area mode - use contribution results with CI for error bars
          if(!is.null(results$contribution) && !is.null(results$contribution$observed)) {
            observed <- results$contribution$observed
            null_dist <- results$contribution$null_distributions

            n_regions <- nrow(observed)
            par(mar = c(5, 12, 4, 4))

            # Get CI values - either from observed (C++ backend) or null distribution (R backend)
            ci_lower <- numeric(n_regions)
            ci_upper <- numeric(n_regions)

            for(i in 1:n_regions) {
              region <- observed$Region[i]
              if("CI_Lower" %in% names(observed) && "CI_Upper" %in% names(observed)) {
                ci_lower[i] <- observed$CI_Lower[i]
                ci_upper[i] <- observed$CI_Upper[i]
              } else if(!is.null(null_dist) && region %in% colnames(null_dist)) {
                null_vals <- null_dist[, region]
                null_vals <- null_vals[!is.na(null_vals)]
                if(length(null_vals) > 0) {
                  ci_lower[i] <- quantile(null_vals, 0.025, na.rm = TRUE)
                  ci_upper[i] <- quantile(null_vals, 0.975, na.rm = TRUE)
                }
              }
            }

            # Determine colors - use brain area colors if available
            bar_colors <- rep("#3498DB", n_regions)
            if(!is.null(ui_state$area_colors) && !is.null(ui_state$brain_areas)) {
              for(i in 1:n_regions) {
                region <- observed$Region[i]
                # Check if region directly matches a brain area name (collective mode)
                if(region %in% names(ui_state$area_colors)) {
                  bar_colors[i] <- ui_state$area_colors[[region]]
                } else {
                  # Individual subregion mode - find which brain area contains this node
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
            # Darken/lighten non-significant bars
            for(i in 1:n_regions) {
              if(!observed$Significant[i]) {
                bar_colors[i] <- adjustcolor(bar_colors[i], alpha.f = 0.4)
              }
            }

            # Calculate x limits
            x_vals <- c(observed$Contribution_Score, ci_lower, ci_upper)
            x_vals <- x_vals[!is.na(x_vals) & is.finite(x_vals)]
            x_max <- max(abs(x_vals), na.rm = TRUE) * 1.3

            # Create barplot
            bp <- barplot(observed$Contribution_Score, horiz = TRUE, names.arg = observed$Region,
                          col = bar_colors, las = 1, xlim = c(-x_max, x_max),
                          xlab = "Contribution Score", main = "Regional Contribution Analysis",
                          cex.names = 0.7)

            # Add error bars
            for(i in 1:n_regions) {
              if(!is.na(ci_lower[i]) && !is.na(ci_upper[i])) {
                segments(ci_lower[i], bp[i], ci_upper[i], bp[i], lwd = 1.5, col = "gray40")
                segments(ci_lower[i], bp[i] - 0.2, ci_lower[i], bp[i] + 0.2, lwd = 1.5, col = "gray40")
                segments(ci_upper[i], bp[i] - 0.2, ci_upper[i], bp[i] + 0.2, lwd = 1.5, col = "gray40")
              }
            }

            abline(v = 0, lty = 2, lwd = 2, col = "gray50")

            # Add significance stars
            for(i in 1:n_regions) {
              if(!is.na(observed$Significance_Stars[i]) && observed$Significance_Stars[i] != "") {
                x_pos <- observed$Contribution_Score[i]
                x_pos <- x_pos + sign(x_pos) * x_max * 0.05
                text(x_pos, bp[i], observed$Significance_Stars[i], cex = 1.0, font = 2)
              }
            }

            # Add legend for brain areas if colors are available
            if(!is.null(ui_state$area_colors) && length(ui_state$area_colors) > 0) {
              legend("topright",
                     legend = c(names(ui_state$area_colors), "Not significant (faded)"),
                     fill = c(unlist(ui_state$area_colors), adjustcolor("#808080", alpha.f = 0.4)),
                     bty = "n", cex = 0.7, title = "Brain Areas")
            }
          } else if(!is.null(results$regional_results)) {
            # Fallback to old format - also use brain area colors
            par(mar = c(10, 5, 3, 2))
            reg <- results$regional_results
            colors <- rep("#3498DB", nrow(reg))
            if(!is.null(ui_state$area_colors) && !is.null(ui_state$brain_areas)) {
              for(i in 1:nrow(reg)) {
                region <- reg$region[i]
                # Check if region directly matches a brain area name (collective mode)
                if(region %in% names(ui_state$area_colors)) {
                  colors[i] <- ui_state$area_colors[[region]]
                } else {
                  # Individual subregion mode - find which brain area contains this node
                  for(area_name in names(ui_state$brain_areas)) {
                    if(region %in% ui_state$brain_areas[[area_name]]) {
                      if(area_name %in% names(ui_state$area_colors)) {
                        colors[i] <- ui_state$area_colors[[area_name]]
                      }
                      break
                    }
                  }
                }
              }
              colors <- ifelse(reg$significant, colors, adjustcolor(colors, alpha.f = 0.4))
            } else {
              colors <- ifelse(reg$significant, ifelse(reg$contribution > 0, "#E74C3C", "#3498DB"), "#CCCCCC")
            }
            barplot(reg$contribution, names.arg = reg$region, las = 2, horiz = TRUE,
                    col = colors, cex.names = 0.8, main = "Regional Contribution Analysis",
                    xlab = "Contribution Score")
            abline(v = 0, lty = 2, col = "gray50")
          }
        }
      }
    ),

    "regionalContributionCircularPlot" = list(
      condition = function() !is.null(analysis_results$regional_contribution),
      render = function() {
        results <- analysis_results$regional_contribution
        if(is.null(results)) {
          plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
          text(1, 1, "Run Regional Contribution Analysis first", cex = 1.1, col = "gray50")
          return()
        }

        # For circular plot, show a summary visualization
        par(mar = c(2, 2, 3, 2))
        if(!is.null(results$regional_results)) {
          reg <- results$regional_results
          n <- nrow(reg)
          if(n > 0) {
            # Create a simple circular representation
            angles <- seq(0, 2*pi, length.out = n + 1)[1:n]
            x <- cos(angles)
            y <- sin(angles)

            plot(x, y, type = "n", axes = FALSE, xlab = "", ylab = "",
                 xlim = c(-1.5, 1.5), ylim = c(-1.5, 1.5), asp = 1,
                 main = "Regional Contribution Network")

            # Draw nodes
            colors <- ifelse(reg$significant,
                           ifelse(reg$contribution > 0, "#E74C3C", "#27AE60"),
                           "#AAAAAA")
            sizes <- abs(reg$contribution)
            sizes <- 1 + (sizes / max(sizes)) * 2

            points(x, y, pch = 19, col = colors, cex = sizes * 1.5)
            text(x * 1.25, y * 1.25, reg$region, cex = 0.6, srt = angles * 180/pi)
          }
        } else {
          plot(1, type = "n", axes = FALSE, xlab = "", ylab = "", main = "Regional Network")
          text(1, 1, "Standard brain area mode required", cex = 0.9, col = "gray50")
        }
      }
    ),

    "regionalJaccardPlot" = list(
      condition = function() !is.null(analysis_results$regional_contribution),
      render = function() {
        results <- analysis_results$regional_contribution
        if(is.null(results)) {
          plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
          text(1, 1, "Run Regional Contribution Analysis first", cex = 1.1, col = "gray50")
          return()
        }

        par(mar = c(10, 5, 3, 2))
        if(!is.null(results$regional_results) && "jaccard" %in% names(results$regional_results)) {
          reg <- results$regional_results
          colors <- colorRampPalette(c("#E74C3C", "#F39C12", "#27AE60"))(100)
          color_idx <- pmax(1, pmin(100, round(reg$jaccard * 100)))

          barplot(reg$jaccard, names.arg = reg$region, las = 2,
                  col = colors[color_idx], cex.names = 0.8,
                  main = "Regional Jaccard Similarity",
                  ylab = "Jaccard Index", ylim = c(0, 1))
          abline(h = 0.5, lty = 2, col = "gray50")
        } else {
          plot(1, type = "n", axes = FALSE, xlab = "", ylab = "", main = "Regional Jaccard")
          text(1, 1, "Jaccard data not available in current mode", cex = 0.9, col = "gray50")
        }
      }
    ),

    "artificialCombinationsSummaryPlot" = list(
      condition = function() {
        results <- analysis_results$regional_contribution
        if(is.null(results)) return(FALSE)
        if(is.null(results$mode) || results$mode != "artificial") return(FALSE)
        if(is.null(results$discovery)) return(FALSE)
        discovery <- results$discovery
        if(is.null(discovery$top_dissimilarity) && is.null(discovery$top_similarity)) return(FALSE)
        n_sig <- (discovery$n_significant_dissim %||% 0) + (discovery$n_significant_sim %||% 0)
        return(n_sig > 0)
      },
      render = function() {
        results <- analysis_results$regional_contribution
        if(is.null(results) || is.null(results$mode) || results$mode != "artificial") {
          plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
          text(1, 1, "Run Artificial Brain Area Discovery first", cex = 1.1, col = "gray50")
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

        # Filter for significant AND non-redundant (positive synergy for multi-region combos)
        sig_nonredundant <- data.frame()

        if(!is.null(top_dissim) && nrow(top_dissim) > 0) {
          sig_d <- top_dissim[top_dissim$significant == TRUE, ]
          if(nrow(sig_d) > 0) {
            if("synergy" %in% names(sig_d)) {
              sig_d <- sig_d[sig_d$size == 1 | (!is.na(sig_d$synergy) & sig_d$synergy > 0), ]
            }
            if(nrow(sig_d) > 0) {
              sig_d$direction <- "Dissimilarity"
              sig_nonredundant <- rbind(sig_nonredundant, sig_d)
            }
          }
        }

        if(!is.null(top_sim) && nrow(top_sim) > 0) {
          sig_s <- top_sim[top_sim$significant == TRUE, ]
          if(nrow(sig_s) > 0) {
            if("synergy" %in% names(sig_s)) {
              sig_s <- sig_s[sig_s$size == 1 | (!is.na(sig_s$synergy) & sig_s$synergy < 0), ]
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

        # Sort and limit
        sig_nonredundant <- sig_nonredundant[order(-abs(sig_nonredundant$contribution)), ]
        if(nrow(sig_nonredundant) > 20) sig_nonredundant <- head(sig_nonredundant, 20)

        n_combos <- nrow(sig_nonredundant)
        bar_colors <- ifelse(sig_nonredundant$direction == "Dissimilarity", "#E74C3C", "#3498DB")

        # Brain area coloring for singles
        if(!is.null(ui_state$brain_areas) && !is.null(ui_state$area_colors)) {
          for(i in 1:n_combos) {
            if(sig_nonredundant$size[i] == 1) {
              region <- sig_nonredundant$nodes[i]
              if(region %in% names(ui_state$area_colors)) {
                bar_colors[i] <- ui_state$area_colors[[region]]
              } else {
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

        max_label_len <- max(nchar(sig_nonredundant$nodes))
        left_margin <- min(18, max(10, max_label_len * 0.4))
        par(mar = c(5, left_margin, 4, 4))

        x_max <- max(abs(sig_nonredundant$contribution), na.rm = TRUE) * 1.3

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

        abline(v = 0, lty = 2, lwd = 2, col = "gray50")

        # Significance stars
        for(i in 1:n_combos) {
          stars <- ""
          if(!is.na(sig_nonredundant$p_value[i])) {
            if(sig_nonredundant$p_value[i] < 0.001) stars <- "***"
            else if(sig_nonredundant$p_value[i] < 0.01) stars <- "**"
            else if(sig_nonredundant$p_value[i] < 0.05) stars <- "*"
          }
          if(stars != "") {
            x_pos <- sig_nonredundant$contribution[i] + sign(sig_nonredundant$contribution[i]) * x_max * 0.05
            text(x_pos, bp[i], stars, cex = 0.9, font = 2)
          }
        }

        legend("topright",
               legend = c("Dissimilarity (drives differences)", "Similarity (drives commonality)"),
               fill = c("#E74C3C", "#3498DB"),
               bty = "n", cex = 0.7)

        mtext("Only showing combinations with synergistic effects (non-redundant)",
              side = 1, line = 4, cex = 0.75, col = "gray40")
      }
    ),

    "avgNodeStrengthByRegionPlot" = list(
      condition = function() !is.null(analysis_results$method_percolation_results) && !is.null(ui_state$brain_areas),
      render = function() {
        method <- get_current_method()
        method_data <- analysis_results$method_percolation_results[[method]]
        if(!is.null(method_data) && !is.null(method_data$node_metrics)) {
          render_avg_node_strength_by_region(
            method_data$node_metrics,
            ui_state$brain_areas,
            ui_state$area_colors,
            get_group_colors()
          )
        }
      }
    ),

    "rankBasedNodeStrengthPlot" = list(
      condition = function() !is.null(analysis_results$method_percolation_results) && !is.null(ui_state$brain_areas),
      render = function() {
        method <- get_current_method()
        method_data <- analysis_results$method_percolation_results[[method]]
        if(!is.null(method_data) && !is.null(method_data$node_metrics)) {
          render_rank_based_node_strength(
            method_data$node_metrics,
            ui_state$brain_areas,
            ui_state$area_colors
          )
        }
      }
    ),

    "avgEigenvectorByRegionPlot" = list(
      condition = function() !is.null(analysis_results$method_percolation_results) && !is.null(ui_state$brain_areas),
      render = function() {
        method <- get_current_method()
        method_data <- analysis_results$method_percolation_results[[method]]
        if(!is.null(method_data)) {
          render_avg_eigenvector_by_region(
            method_data$weighted_eigenvector,
            method_data$node_metrics,
            ui_state$brain_areas,
            ui_state$area_colors,
            get_group_colors()
          )
        }
      }
    ),

    "rankBasedEigenvectorPlot" = list(
      condition = function() !is.null(analysis_results$method_percolation_results) && !is.null(ui_state$brain_areas),
      render = function() {
        method <- get_current_method()
        method_data <- analysis_results$method_percolation_results[[method]]
        if(!is.null(method_data) && !is.null(method_data$node_metrics)) {
          render_rank_based_eigenvector(
            method_data$node_metrics,
            ui_state$brain_areas,
            ui_state$area_colors
          )
        }
      }
    ),

    "nodeStrengthSubregionsPlot" = list(
      condition = function() !is.null(analysis_results$method_percolation_results) && !is.null(ui_state$brain_areas),
      render = function() {
        method <- get_current_method()
        method_data <- analysis_results$method_percolation_results[[method]]
        if(!is.null(method_data) && !is.null(method_data$node_metrics)) {
          render_node_strength_subregions(
            method_data$node_metrics,
            ui_state$brain_areas,
            ui_state$area_colors,
            get_group_colors()
          )
        }
      }
    ),

    "nodeStrengthRankSubregionsPlot" = list(
      condition = function() !is.null(analysis_results$method_percolation_results) && !is.null(ui_state$brain_areas),
      render = function() {
        method <- get_current_method()
        method_data <- analysis_results$method_percolation_results[[method]]
        if(!is.null(method_data) && !is.null(method_data$node_metrics)) {
          render_node_strength_rank_subregions(
            method_data$node_metrics,
            ui_state$brain_areas,
            ui_state$area_colors
          )
        }
      }
    ),

    "eigenvectorSubregionsPlot" = list(
      condition = function() !is.null(analysis_results$method_percolation_results) && !is.null(ui_state$brain_areas),
      render = function() {
        method <- get_current_method()
        method_data <- analysis_results$method_percolation_results[[method]]
        if(!is.null(method_data) && !is.null(method_data$node_metrics)) {
          render_eigenvector_subregions(
            method_data$node_metrics,
            ui_state$brain_areas,
            ui_state$area_colors,
            get_group_colors()
          )
        }
      }
    ),

    "eigenvectorRankSubregionsPlot" = list(
      condition = function() !is.null(analysis_results$method_percolation_results) && !is.null(ui_state$brain_areas),
      render = function() {
        method <- get_current_method()
        method_data <- analysis_results$method_percolation_results[[method]]
        if(!is.null(method_data) && !is.null(method_data$node_metrics)) {
          render_eigenvector_rank_subregions(
            method_data$node_metrics,
            ui_state$brain_areas,
            ui_state$area_colors
          )
        }
      }
    ),

    # =========================================================================
    # CROSS-METHOD COMPARISON PLOTS
    # =========================================================================
    "crossMethodEigenvectorPlot" = list(
      condition = function() !is.null(analysis_results$method_percolation_results),
      render = function() {
        render_cross_method_eigenvector(
          analysis_results$method_percolation_results,
          ui_state$brain_areas,
          ui_state$area_colors,
          get_group_colors()
        )
      }
    ),

    "crossMethodNodeStrengthPlot" = list(
      condition = function() !is.null(analysis_results$method_percolation_results),
      render = function() {
        render_cross_method_node_strength(
          analysis_results$method_percolation_results,
          ui_state$brain_areas,
          ui_state$area_colors,
          get_group_colors()
        )
      }
    ),

    "hubRankingComparisonPlot" = list(
      condition = function() !is.null(analysis_results$method_percolation_results),
      render = function() {
        render_hub_ranking_comparison(
          analysis_results$method_percolation_results,
          ui_state$brain_areas,
          ui_state$area_colors
        )
      }
    ),

    "methodVarianceBarplot" = list(
      condition = function() !is.null(analysis_results$method_percolation_results),
      render = function() {
        render_method_variance_barplot(
          analysis_results$method_percolation_results,
          get_group_colors()
        )
      }
    ),

    "methodAgreementDistribution" = list(
      condition = function() !is.null(analysis_results$method_percolation_results),
      render = function() {
        render_method_agreement_distribution(analysis_results$method_percolation_results)
      }
    ),

    "approachVsMethodConsistency" = list(
      condition = function() !is.null(analysis_results$comprehensive_consensus),
      render = function() {
        render_approach_vs_method_consistency(
          analysis_results$comprehensive_consensus,
          get_group_colors()
        )
      }
    ),

    # =========================================================================
    # PERSISTENCE ANALYSIS PLOTS
    # =========================================================================
    "method_hub_persistence_heatmap" = list(
      condition = function() !is.null(analysis_results$persistence_results),
      render = function() {
        render_method_hub_persistence_heatmap(
          analysis_results$persistence_results,
          ui_state$brain_areas,
          ui_state$area_colors
        )
      }
    ),

    "method_metrics_evolution" = list(
      condition = function() !is.null(analysis_results$persistence_results),
      render = function() {
        render_method_metrics_evolution(
          analysis_results$persistence_results,
          get_group_colors()
        )
      }
    ),

    "persistenceNodeMetricsPlot" = list(
      condition = function() !is.null(analysis_results$persistence_results),
      render = function() {
        render_persistence_node_metrics(
          analysis_results$persistence_results,
          ui_state$brain_areas,
          ui_state$area_colors,
          get_group_colors()
        )
      }
    ),

    "persistenceHubComparisonPlot" = list(
      condition = function() !is.null(analysis_results$persistence_results),
      render = function() {
        render_persistence_hub_comparison(
          analysis_results$persistence_results,
          ui_state$brain_areas,
          ui_state$area_colors
        )
      }
    ),

    "persistenceRegionalAnalysisPlot" = list(
      condition = function() !is.null(analysis_results$persistence_results) && !is.null(ui_state$brain_areas),
      render = function() {
        render_persistence_regional_analysis(
          analysis_results$persistence_results,
          ui_state$brain_areas,
          ui_state$area_colors,
          get_group_colors()
        )
      }
    ),

    "persistenceNetworkSimilarityPlot" = list(
      condition = function() !is.null(analysis_results$persistence_results),
      render = function() {
        render_persistence_network_similarity(analysis_results$persistence_results)
      }
    ),

    "persistenceGroupSimilarityPlot" = list(
      condition = function() !is.null(analysis_results$persistence_results),
      render = function() {
        render_persistence_group_similarity(
          analysis_results$persistence_results,
          get_group_colors()
        )
      }
    ),

    "persistenceDiagramPlot" = list(
      condition = function() !is.null(analysis_results$persistence_results),
      render = function() {
        render_persistence_diagram(
          analysis_results$persistence_results,
          get_group_colors()
        )
      }
    ),

    "persistenceVsPercolationHubsPlot" = list(
      condition = function() !is.null(analysis_results$persistence_results) && !is.null(analysis_results$method_percolation_results),
      render = function() {
        render_persistence_vs_percolation_hubs(
          analysis_results$persistence_results,
          analysis_results$method_percolation_results,
          ui_state$brain_areas,
          ui_state$area_colors
        )
      }
    ),

    "validationNetworkSimilarity" = list(
      condition = function() !is.null(analysis_results$persistence_results),
      render = function() {
        render_validation_network_similarity(analysis_results$persistence_results)
      }
    ),

    # =========================================================================
    # ADVANCED TOPOLOGY (MST & SPECTRAL) PLOTS
    # =========================================================================
    "advancedMstMetricsPlot" = list(
      condition = function() !is.null(analysis_results$mst_results),
      render = function() {
        render_advanced_mst_metrics(analysis_results$mst_results, get_group_colors())
      }
    ),

    "mstCentralNodesPlot" = list(
      condition = function() !is.null(analysis_results$mst_results),
      render = function() {
        render_mst_central_nodes(
          analysis_results$mst_results,
          ui_state$brain_areas,
          ui_state$area_colors,
          get_group_colors()
        )
      }
    ),

    "advancedMstNetworksPlot" = list(
      condition = function() !is.null(analysis_results$mst_results),
      render = function() {
        render_advanced_mst_networks(
          analysis_results$mst_results,
          ui_state$brain_areas,
          ui_state$area_colors,
          get_group_colors()
        )
      }
    ),

    "pipelineOverviewPlot" = list(
      condition = function() !is.null(analysis_results$complete) && analysis_results$complete,
      render = function() {
        render_pipeline_overview(analysis_results)
      }
    ),

    "pcaAnalysisPlot" = list(
      condition = function() !is.null(analysis_results$pca_results),
      render = function() {
        render_pca_analysis(analysis_results$pca_results, get_group_colors())
      }
    ),

    "pcaLoadingsPlot" = list(
      condition = function() !is.null(analysis_results$pca_results),
      render = function() {
        render_pca_loadings(
          analysis_results$pca_results,
          ui_state$brain_areas,
          ui_state$area_colors
        )
      }
    ),

    "pcaVariancePlot" = list(
      condition = function() !is.null(analysis_results$pca_results),
      render = function() {
        render_pca_variance(analysis_results$pca_results)
      }
    ),

    "spectralEmbeddingPlot" = list(
      condition = function() !is.null(analysis_results$spectral_results),
      render = function() {
        render_spectral_embedding(
          analysis_results$spectral_results,
          ui_state$brain_areas,
          ui_state$area_colors,
          get_group_colors()
        )
      }
    ),

    "spectralClusteringPlot" = list(
      condition = function() !is.null(analysis_results$spectral_results),
      render = function() {
        render_spectral_clustering(
          analysis_results$spectral_results,
          ui_state$brain_areas,
          ui_state$area_colors
        )
      }
    ),

    "omstNetworkPlot" = list(
      condition = function() !is.null(analysis_results$omst_results),
      render = function() {
        render_omst_network(
          analysis_results$omst_results,
          ui_state$brain_areas,
          ui_state$area_colors,
          get_group_colors()
        )
      }
    ),

    "disparityNetworkPlot" = list(
      condition = function() !is.null(analysis_results$disparity_results),
      render = function() {
        render_disparity_network(
          analysis_results$disparity_results,
          ui_state$brain_areas,
          ui_state$area_colors,
          get_group_colors()
        )
      }
    ),

    "nbsNetworkPlot" = list(
      condition = function() !is.null(analysis_results$nbs_results),
      render = function() {
        render_nbs_network(
          analysis_results$nbs_results,
          ui_state$brain_areas,
          ui_state$area_colors,
          get_group_colors()
        )
      }
    ),

    # =========================================================================
    # PERMUTATION & STATISTICAL TESTING PLOTS
    # =========================================================================
    "roiPermutationPlot" = list(
      condition = function() !is.null(analysis_results$permutation_results),
      render = function() {
        render_roi_permutation_results(
          analysis_results$permutation_results,
          ui_state$brain_areas,
          ui_state$area_colors
        )
      }
    ),

    "globalPermutationPlot" = list(
      condition = function() !is.null(analysis_results$permutation_results),
      render = function() {
        render_global_permutation_results(
          analysis_results$permutation_results,
          get_group_colors()
        )
      }
    ),

    "hubOverlapVennPlot" = list(
      condition = function() !is.null(analysis_results$hub_overlap_analysis),
      render = function() {
        render_hub_overlap_venn(
          analysis_results$hub_overlap_analysis,
          get_group_colors()
        )
      }
    ),

    "hubOverlapMatrixPlot" = list(
      condition = function() !is.null(analysis_results$hub_overlap_analysis),
      render = function() {
        render_hub_overlap_matrix(analysis_results$hub_overlap_analysis)
      }
    ),

    # =========================================================================
    # SUMMARY & DASHBOARD PLOTS
    # =========================================================================
    "summaryDashboardPlot" = list(
      condition = function() !is.null(analysis_results$complete) && analysis_results$complete,
      render = function() {
        render_summary_dashboard(analysis_results)
      }
    ),

    "summaryAgreementDonut" = list(
      condition = function() !is.null(analysis_results$comprehensive_consensus),
      render = function() {
        render_summary_agreement_donut(
          analysis_results$comprehensive_consensus,
          get_group_colors()
        )
      }
    ),

    "summaryConfidenceHist" = list(
      condition = function() !is.null(analysis_results$comprehensive_consensus),
      render = function() {
        render_summary_confidence_hist(analysis_results$comprehensive_consensus)
      }
    ),

    "summaryApproachWeights" = list(
      condition = function() !is.null(analysis_results$comprehensive_consensus),
      render = function() {
        render_summary_approach_weights(analysis_results$comprehensive_consensus)
      }
    ),

    "summaryMethodAgreement" = list(
      condition = function() !is.null(analysis_results$comprehensive_consensus),
      render = function() {
        render_summary_method_agreement(analysis_results$comprehensive_consensus)
      }
    ),

    "consensusQualityPlot" = list(
      condition = function() !is.null(analysis_results$comprehensive_consensus),
      render = function() {
        render_consensus_quality(
          analysis_results$comprehensive_consensus,
          get_group_colors()
        )
      }
    ),

    "calibrationValidationPlot" = list(
      condition = function() !is.null(analysis_results$comprehensive_consensus),
      render = function() {
        render_calibration_validation(analysis_results$comprehensive_consensus)
      }
    ),

    "disagreementHeatmapPlot" = list(
      condition = function() !is.null(analysis_results$comprehensive_consensus),
      render = function() {
        render_disagreement_heatmap(
          analysis_results$comprehensive_consensus,
          ui_state$brain_areas,
          ui_state$area_colors
        )
      }
    ),

    "divergenceScatterPlot" = list(
      condition = function() !is.null(analysis_results$comprehensive_consensus),
      render = function() {
        render_divergence_scatter(
          analysis_results$comprehensive_consensus,
          ui_state$brain_areas,
          ui_state$area_colors,
          get_group_colors()
        )
      }
    ),

    "zThresholdSensitivityPlot" = list(
      condition = function() !is.null(analysis_results$comprehensive_consensus),
      render = function() {
        render_z_threshold_sensitivity(
          analysis_results$comprehensive_consensus,
          get_group_colors()
        )
      }
    ),

    "agreementSensitivityPlot" = list(
      condition = function() !is.null(analysis_results$comprehensive_consensus),
      render = function() {
        render_agreement_sensitivity(
          analysis_results$comprehensive_consensus,
          get_group_colors()
        )
      }
    ),

    "conservationCorrelationPlot" = list(
      condition = function() !is.null(analysis_results$comprehensive_consensus),
      render = function() {
        render_conservation_correlation(
          analysis_results$comprehensive_consensus,
          get_group_colors()
        )
      }
    ),

    "conservationInsightsPlot" = list(
      condition = function() !is.null(analysis_results$conservation_analysis),
      render = function() {
        render_conservation_insights(analysis_results$conservation_analysis)
      }
    ),

    "metaConsensusSummaryPlot" = list(
      condition = function() !is.null(analysis_results$comprehensive_consensus),
      render = function() {
        render_meta_consensus_summary(
          analysis_results$comprehensive_consensus,
          get_group_colors()
        )
      }
    ),

    "nodeStrengthRankPlot" = list(
      condition = function() !is.null(analysis_results$method_percolation_results),
      render = function() {
        method <- get_current_method()
        method_data <- analysis_results$method_percolation_results[[method]]
        if(!is.null(method_data) && !is.null(method_data$node_metrics)) {
          render_node_strength_rank(
            method_data$node_metrics,
            ui_state$brain_areas,
            ui_state$area_colors,
            get_group_colors()
          )
        }
      }
    ),

    "sharedNetworkSimilarityPlot" = list(
      condition = function() !is.null(analysis_results$method_percolation_results),
      render = function() {
        render_shared_network_similarity(
          analysis_results$method_percolation_results,
          get_group_colors()
        )
      }
    )
  )
}

# ============================================================================
# SERVER LOGIC FOR HOVER DOWNLOADS
# ============================================================================
hover_download_server <- function(input, output, session, analysis_results, ui_state) {

  # Create reactive plot registry
  plot_registry <- reactive({
    create_hover_plot_registry(analysis_results, ui_state, input)
  })

  # Track the current plot being downloaded
  current_plot_id <- reactiveVal(NULL)

  # Update current plot when modal opens
  observeEvent(input$hover_download_plot_id, {
    req(input$hover_download_plot_id)
    message(sprintf("[Plot Download] Modal opened for plot: %s", input$hover_download_plot_id))
    current_plot_id(input$hover_download_plot_id)

    # Force preview update by invalidating
    session$sendCustomMessage("plot_download_ready", list(plot_id = input$hover_download_plot_id))
  }, ignoreNULL = TRUE, ignoreInit = TRUE)

  # Reactive for preview height based on aspect ratio
  preview_height <- reactive({
    width_in <- as.numeric(input$hover_download_width %||% 10)
    height_in <- as.numeric(input$hover_download_height %||% 8)

    # Calculate preview height maintaining aspect ratio
    # Preview container is ~450px wide, so calculate proportional height
    preview_width <- 450
    aspect_ratio <- height_in / width_in
    calculated_height <- round(preview_width * aspect_ratio)

    # Clamp between reasonable bounds
    max(150, min(400, calculated_height))
  })

  # Render plot preview in modal - renders to temp file at actual dimensions then displays
  output$hover_download_preview <- renderImage({
    plot_id <- current_plot_id()

    # Force reactivity on dimension inputs
    width_in <- as.numeric(input$hover_download_width %||% 10)
    height_in <- as.numeric(input$hover_download_height %||% 8)
    dpi <- as.numeric(input$hover_download_dpi %||% 300)

    # Force reactivity on analysis_results
    regional_contrib <- analysis_results$regional_contribution
    consensus <- analysis_results$comprehensive_consensus

    message(sprintf("[Preview] Rendering for: %s (%.1f x %.1f @ %d DPI)",
                    if(is.null(plot_id)) "NULL" else plot_id, width_in, height_in, dpi))

    # Create temp file for preview
    temp_file <- tempfile(fileext = ".png")

    # Helper to create placeholder image
    create_placeholder <- function(msg, bg_col = "#f8f9fa", text_col = "#6c757d") {
      png(temp_file, width = 450, height = preview_height(), bg = bg_col)
      par(mar = c(0, 0, 0, 0))
      plot(1, type = "n", axes = FALSE, xlab = "", ylab = "", main = "",
           xlim = c(0, 1), ylim = c(0, 1))
      text(0.5, 0.5, msg, col = text_col, cex = 1.0)
      dev.off()
    }

    # Show placeholder if no plot selected
    if(is.null(plot_id) || plot_id == "") {
      create_placeholder("Select a plot to preview")
      return(list(src = temp_file, contentType = "image/png",
                  width = 450, height = preview_height(),
                  alt = "No plot selected"))
    }

    # Get registry and check plot exists
    registry <- plot_registry()
    if(!plot_id %in% names(registry)) {
      create_placeholder(paste("Plot not found:", plot_id), "#f8d7da", "#dc3545")
      return(list(src = temp_file, contentType = "image/png",
                  width = 450, height = preview_height(),
                  alt = "Plot not found"))
    }

    plot_info <- registry[[plot_id]]

    # Check if data is available
    condition_met <- tryCatch(plot_info$condition(), error = function(e) FALSE)
    if(!condition_met) {
      create_placeholder("Run analysis first", "#fff3cd", "#856404")
      return(list(src = temp_file, contentType = "image/png",
                  width = 450, height = preview_height(),
                  alt = "Analysis required"))
    }

    # Render at actual dimensions to temp file (use lower DPI for preview speed)
    preview_dpi <- min(100, dpi)  # Cap preview DPI for performance
    tryCatch({
      png(temp_file, width = width_in * preview_dpi, height = height_in * preview_dpi, res = preview_dpi)
      plot_info$render()
      dev.off()
    }, error = function(e) {
      message(sprintf("[Preview] Render error: %s", e$message))
      tryCatch(dev.off(), error = function(x) {})
      create_placeholder("Preview error\nDownload will work", "#f8d7da", "#721c24")
    })

    # Return scaled image maintaining aspect ratio
    list(
      src = temp_file,
      contentType = "image/png",
      width = 450,
      height = preview_height(),
      alt = sprintf("Preview: %.1f x %.1f inches", width_in, height_in)
    )
  }, deleteFile = TRUE)

  # Ensure preview renders even when modal is hidden
  outputOptions(output, "hover_download_preview", suspendWhenHidden = FALSE)

  # Download handler
  output$execute_hover_download <- downloadHandler(
    filename = function() {
      plot_id <- current_plot_id()
      format <- input$hover_download_format %||% "png"

      # Create safe filename
      safe_name <- gsub("[^a-zA-Z0-9_]", "_", plot_id)
      timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")

      paste0(safe_name, "_", timestamp, ".", format)
    },
    content = function(file) {
      plot_id <- current_plot_id()
      registry <- plot_registry()

      # Check if plot exists in registry
      if(is.null(plot_id) || !plot_id %in% names(registry)) {
        # Try to render a placeholder error
        png(file, width = 400, height = 300)
        plot(1, type = "n", main = "Plot not found", axes = FALSE, xlab = "", ylab = "")
        text(1, 1, paste("Plot ID:", plot_id, "not in registry"), cex = 1.2)
        dev.off()
        session$sendCustomMessage("plot_download_error", list(error = paste("Plot", plot_id, "not found in registry")))
        return()
      }

      plot_info <- registry[[plot_id]]

      # Check if data is available
      if(!plot_info$condition()) {
        png(file, width = 400, height = 300)
        plot(1, type = "n", main = "Data not available", axes = FALSE, xlab = "", ylab = "")
        text(1, 1, "Run analysis first", cex = 1.2)
        dev.off()
        session$sendCustomMessage("plot_download_error", list(error = "Plot data not available. Please run the analysis first."))
        return()
      }

      # Get settings from modal inputs
      width_in <- as.numeric(input$hover_download_width %||% 10)
      height_in <- as.numeric(input$hover_download_height %||% 8)
      dpi <- as.numeric(input$hover_download_dpi %||% 300)
      format <- input$hover_download_format %||% "png"

      # Validate inputs
      width_in <- max(2, min(24, width_in))
      height_in <- max(2, min(24, height_in))
      dpi <- max(72, min(600, dpi))

      # Create the plot file
      tryCatch({
        if(format == "png") {
          png(file, width = width_in * dpi, height = height_in * dpi, res = dpi)
        } else {
          pdf(file, width = width_in, height = height_in)
        }

        # Render the plot
        plot_info$render()

        dev.off()

        # Close modal after successful download
        session$sendCustomMessage("close_plot_download_modal", list())

      }, error = function(e) {
        # Make sure device is closed
        tryCatch(dev.off(), error = function(e2) {})

        err_msg <- conditionMessage(e)

        # Create error placeholder
        if(format == "png") {
          png(file, width = 400, height = 300)
        } else {
          pdf(file, width = 5, height = 4)
        }

        # Check if this is a missing function error
        if(grepl("could not find function", err_msg)) {
          plot(1, type = "n", main = "Download Not Available", axes = FALSE, xlab = "", ylab = "")
          text(1, 1, "This plot type does not support\nindividual download yet.", cex = 1.0)
          dev.off()
          session$sendCustomMessage("plot_download_error",
            list(error = "This plot type does not support individual download. Use the main export feature instead."))
        } else {
          plot(1, type = "n", main = "Error rendering plot", axes = FALSE, xlab = "", ylab = "")
          text(1, 1, err_msg, cex = 0.8)
          dev.off()
          session$sendCustomMessage("plot_download_error", list(error = err_msg))
        }
      })
    }
  )
}
