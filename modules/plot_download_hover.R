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
        render_consensus_node_metrics_plot(
          analysis_results$comprehensive_consensus,
          analysis_results$method_percolation_results,
          ui_state$brain_areas,
          ui_state$area_colors,
          get_group_colors(),
          selected_methods = input$selected_correlation_methods
        )
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
      condition = function() !is.null(analysis_results$comprehensive_consensus),
      render = function() {
        render_consensus_network_plot(
          analysis_results$comprehensive_consensus,
          ui_state$brain_areas,
          ui_state$area_colors,
          get_group_colors()
        )
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
        render_consensus_subregional_plot(
          analysis_results$comprehensive_consensus,
          ui_state$brain_areas,
          ui_state$area_colors,
          get_group_colors(),
          selected_methods = input$selected_correlation_methods
        )
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

            # Determine colors
            bar_colors <- rep("#3498DB", n_regions)
            bar_colors[observed$Contribution_Score > 0 & observed$Significant] <- "#E74C3C"
            bar_colors[observed$Contribution_Score <= 0 & observed$Significant] <- "#27AE60"

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
          } else if(!is.null(results$regional_results)) {
            # Fallback to old format
            par(mar = c(10, 5, 3, 2))
            reg <- results$regional_results
            colors <- ifelse(reg$significant, ifelse(reg$contribution > 0, "#E74C3C", "#3498DB"), "#CCCCCC")
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
