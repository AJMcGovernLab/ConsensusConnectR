# Simple Single Download System - Updated for Per-Method Analysis
# One button, one package with everything

# Load all required modules
source("modules/visualization_functions.R")
source("modules/complete_plot_system.R")

# ==========================================
# PLOT CUSTOMIZER REGISTRY
# Maps plot IDs to their render functions and conditions
# Note: Title customization is handled via par(cex.main) since original
# render functions have internal titles. Custom title text replaces the
# entire plot's title via mtext overlay when specified.
# ==========================================
create_summary_plot_registry <- function(analysis_results, ui_state) {
  list(
    "Consensus Node Metrics" = list(
      id = "consensus_node_metrics",
      condition = function() !is.null(analysis_results$comprehensive_consensus) && !is.null(analysis_results$method_percolation_results),
      render = function(title_override = NULL, title_size = 1.2) {
        render_consensus_node_metrics_plot(
          analysis_results$comprehensive_consensus,
          analysis_results$method_percolation_results,
          ui_state$brain_areas,
          ui_state$area_colors,
          ui_state$group_colors
        )
      },
      default_title = "Consensus Node Metrics Across Methods"
    ),

    "Consensus Regional Analysis" = list(
      id = "consensus_regional",
      condition = function() !is.null(analysis_results$comprehensive_consensus) && !is.null(ui_state$brain_areas),
      render = function(title_override = NULL, title_size = 1.2) {
        render_consensus_regional_plot(
          analysis_results$comprehensive_consensus,
          ui_state$brain_areas,
          ui_state$area_colors,
          ui_state$group_colors
        )
      },
      default_title = "Regional Consensus Analysis"
    ),

    "Consensus Subregional Analysis" = list(
      id = "consensus_subregional",
      condition = function() !is.null(analysis_results$comprehensive_consensus) && !is.null(ui_state$brain_areas),
      render = function(title_override = NULL, title_size = 1.2) {
        render_consensus_subregional_plot(
          analysis_results$comprehensive_consensus,
          ui_state$brain_areas,
          ui_state$area_colors,
          ui_state$group_colors
        )
      },
      default_title = "Subregional Consensus Analysis"
    ),

    "Consensus Top Hubs" = list(
      id = "consensus_top_hubs",
      condition = function() !is.null(analysis_results$comprehensive_consensus),
      render = function(title_override = NULL, title_size = 1.2) {
        render_consensus_overview_top_hubs_plot(
          analysis_results$comprehensive_consensus,
          ui_state$brain_areas,
          ui_state$area_colors,
          ui_state$group_colors
        )
      },
      default_title = "Top Consensus Hubs by Group"
    ),

    "Consensus Agreement Matrix" = list(
      id = "consensus_agreement",
      condition = function() !is.null(analysis_results$comprehensive_consensus),
      render = function(title_override = NULL, title_size = 1.2) {
        render_consensus_overview_agreement_plot(
          analysis_results$comprehensive_consensus,
          ui_state$group_colors
        )
      },
      default_title = "Method-Approach Agreement Matrix"
    ),

    "Network Similarity Heatmap" = list(
      id = "network_similarity_heatmap",
      condition = function() !is.null(analysis_results$correlation_methods_raw) && !is.null(analysis_results$method_percolation_results),
      render = function(title_override = NULL, title_size = 1.2) {
        render_network_similarity_heatmap_plot(
          analysis_results$correlation_methods_raw,
          analysis_results$method_percolation_results,
          analysis_results$persistence_results
        )
      },
      default_title = "Network Similarity Across Methods"
    ),

    "Conservation Heatmap" = list(
      id = "conservation_heatmap",
      condition = function() !is.null(analysis_results$conservation_analysis),
      render = function(title_override = NULL, title_size = 1.2) {
        render_network_similarity_heatmap(
          analysis_results$conservation_analysis
        )
      },
      default_title = "Edge Conservation Heatmap"
    ),

    "Three-Way Eigenvector Comparison" = list(
      id = "three_way_eigenvector",
      condition = function() !is.null(analysis_results$method_weighted_results) && !is.null(analysis_results$method_percolation_results),
      render = function(title_override = NULL, title_size = 1.2) {
        render_consensus_three_way_eigenvector(
          analysis_results$method_weighted_results,
          analysis_results$method_percolation_results,
          analysis_results$persistence_results,
          ui_state$group_colors
        )
      },
      default_title = "Eigenvector Centrality: Weighted vs Percolation vs Persistence"
    ),

    "Hub Ranking Comparison" = list(
      id = "hub_ranking",
      condition = function() !is.null(analysis_results$consensus_hub_results),
      render = function(title_override = NULL, title_size = 1.2) {
        render_consensus_hub_ranking(
          analysis_results$consensus_hub_results,
          ui_state$group_colors
        )
      },
      default_title = "Hub Ranking by Consensus Score"
    ),

    "Consensus Heatmap" = list(
      id = "consensus_heatmap",
      condition = function() !is.null(analysis_results$consensus_hub_results),
      render = function(title_override = NULL, title_size = 1.2) {
        render_consensus_heatmap(
          analysis_results$consensus_hub_results,
          ui_state$group_colors
        )
      },
      default_title = "Consensus Hub Agreement Heatmap"
    )
  )
}

# Helper function to organize plots into categorical folders
organize_plots_by_category <- function(source_dir, dest_dirs) {
  all_plots <- list.files(source_dir, pattern = "\\.png$", full.names = TRUE)

  # Mapping of plot numbers to categories
  plot_categories <- list(
    imputation = c("01"),
    permethod = c("02", "03"),
    topology = c("04", "05", "06", "07", "08", "09", "10", "11", "12", "13", "14", "15", "20"),  # Individual network plots 04-0X + dashboard plots
    weighted = c("21", "22"),
    persistence = c(),  # Will be added based on method
    crossmethod = c("23", "24", "25", "26", "41", "42"),
    advanced = c("27", "28", "29", "30", "31", "32"),
    consensus = c("33", "34", "35", "36", "37", "38", "39", "40"),
    summary = c("19", "43", "44", "45", "46", "47", "48", "49", "50", "51", "52", "53", "54", "55", "56")
  )

  for(plot_file in all_plots) {
    plot_name <- basename(plot_file)
    plot_num <- sub("_.*", "", plot_name)

    # Find which category this plot belongs to
    category_found <- FALSE
    for(cat_name in names(plot_categories)) {
      if(plot_num %in% plot_categories[[cat_name]]) {
        dest_file <- file.path(dest_dirs[[cat_name]], plot_name)
        file.copy(plot_file, dest_file, overwrite = TRUE)
        category_found <- TRUE
        break
      }
    }

    # If not categorized, put in summary
    if(!category_found) {
      dest_file <- file.path(dest_dirs$summary, plot_name)
      file.copy(plot_file, dest_file, overwrite = TRUE)
    }
  }

  # Remove original files from base directory
  file.remove(all_plots)
}

# Helper function to export networks as JSON
export_networks_json <- function(analysis_results, networks_dir) {
  if(!requireNamespace("jsonlite", quietly = TRUE)) {
    cat("⚠️  jsonlite package not available, skipping JSON export\n")
    return(0)
  }

  if(!requireNamespace("igraph", quietly = TRUE)) {
    cat("⚠️  igraph package not available, skipping JSON export\n")
    return(0)
  }

  count <- 0

  # Helper function to export a single network in Cytoscape.js format
  export_single_network <- function(network, method_name, group_name, cor_matrix = NULL) {
    if(is.null(network) || !igraph::is.igraph(network)) return(FALSE)

    tryCatch({
      # Convert igraph network to Cytoscape.js JSON format
      node_names <- igraph::V(network)$name
      if(is.null(node_names)) node_names <- paste0("Node", 1:igraph::vcount(network))

      # Build nodes in Cytoscape format: { data: { id: "...", name: "..." } }
      nodes_list <- lapply(node_names, function(n) {
        list(data = list(id = n, name = n))
      })

      # Build edges in Cytoscape format
      edge_list <- igraph::as_edgelist(network)
      edges_list <- list()

      if(nrow(edge_list) > 0) {
        edge_weights <- if("weight" %in% igraph::edge_attr_names(network)) {
          igraph::E(network)$weight
        } else {
          rep(1, nrow(edge_list))
        }

        for(i in 1:nrow(edge_list)) {
          n1 <- edge_list[i, 1]
          n2 <- edge_list[i, 2]

          edge_data <- list(
            id = paste0("e", i),
            source = n1,
            target = n2,
            weight = round(edge_weights[i], 4)
          )

          # Add correlation if available
          if(!is.null(cor_matrix) && n1 %in% rownames(cor_matrix) && n2 %in% colnames(cor_matrix)) {
            edge_data$correlation <- round(cor_matrix[n1, n2], 4)
          }

          edges_list[[i]] <- list(data = edge_data)
        }
      }

      # Cytoscape.js format
      network_data <- list(
        format_version = "1.0",
        generated_by = "ConsensusConnectR",
        target_cytoscapejs_version = "3.x",
        data = list(
          name = paste(method_name, group_name, sep = "_"),
          method = method_name,
          group = group_name
        ),
        elements = list(
          nodes = nodes_list,
          edges = edges_list
        )
      )

      safe_method <- gsub("[^a-zA-Z0-9]", "_", method_name)
      safe_group <- gsub("[^a-zA-Z0-9]", "_", group_name)
      filename <- paste0("Network_", safe_method, "_", safe_group, ".json")

      jsonlite::write_json(network_data,
                         file.path(networks_dir, filename),
                         pretty = TRUE,
                         auto_unbox = TRUE)
      return(TRUE)
    }, error = function(e) {
      cat(sprintf("⚠️  Failed to export network %s-%s: %s\n",
                 method_name, group_name, conditionMessage(e)))
      return(FALSE)
    })
  }

  # Export main networks (from analysis_results$networks)
  if(!is.null(analysis_results$networks)) {
    cat("   Exporting main networks...\n")
    for(group_name in names(analysis_results$networks)) {
      network <- analysis_results$networks[[group_name]]
      cor_matrix <- if(!is.null(analysis_results$correlations)) {
        analysis_results$correlations[[group_name]]
      } else NULL

      if(export_single_network(network, "main", group_name, cor_matrix)) {
        count <- count + 1
      }
    }
  }

  # Export per-method percolation networks (build from adjacency matrices)
  if(!is.null(analysis_results$method_percolation_results)) {
    cat("   Exporting per-method networks...\n")
    for(method_name in names(analysis_results$method_percolation_results)) {
      method_data <- analysis_results$method_percolation_results[[method_name]]

      # Try to get networks from adjacency matrices
      if(!is.null(method_data$adjacency_matrices)) {
        for(group_name in names(method_data$adjacency_matrices)) {
          adj_mat <- method_data$adjacency_matrices[[group_name]]

          if(!is.null(adj_mat) && is.matrix(adj_mat)) {
            # Get correlation matrix for this method/group
            cor_matrix <- NULL
            if(!is.null(analysis_results$correlation_methods_raw[[method_name]][[group_name]])) {
              cor_matrix <- analysis_results$correlation_methods_raw[[method_name]][[group_name]]
            }

            # Create weighted adjacency (adjacency × correlations)
            if(!is.null(cor_matrix)) {
              weighted_adj <- adj_mat * abs(cor_matrix)
            } else {
              weighted_adj <- adj_mat
            }

            # Convert to igraph
            network <- igraph::graph_from_adjacency_matrix(
              weighted_adj,
              mode = "undirected",
              weighted = TRUE,
              diag = FALSE
            )

            if(export_single_network(network, method_name, group_name, cor_matrix)) {
              count <- count + 1
            }
          }
        }
      }
    }
  }

  return(count)
}

# Simple downloads UI - tabbed interface with Download Packages and Plot Customizer
downloads_ui <- function() {
  fluidRow(
    column(12,
      div(
        style = "padding: 20px;",

        # Tab navigation
        tabsetPanel(
          id = "downloads_tabs",
          type = "pills",

          # ==========================================
          # TAB 1: DOWNLOAD PACKAGES (existing functionality)
          # ==========================================
          tabPanel(
            title = "📦 Download Packages",
            value = "download_packages",
            div(
              style = "text-align: center; padding: 30px;",
              h3("📦 Download Complete Analysis"),
              p("Get all your plots and data in one convenient package."),

              conditionalPanel(
                condition = "!output.analysisComplete",
                div(
                  class = "alert alert-warning",
                  style = "max-width: 600px; margin: 20px auto;",
                  h4("⚠️ Analysis Required"),
                  p("Please complete your analysis first to download results."),
                  actionButton("goToAnalysisFromDownloads", "Go to Analysis", class = "btn btn-primary btn-lg")
                )
              ),

              conditionalPanel(
                condition = "output.analysisComplete",
                div(
                  style = "max-width: 700px; margin: 20px auto;",

                  # Plot Customization Panel
                  div(
                    style = "padding: 20px; border: 1px solid #ddd; border-radius: 8px; background-color: #f9f9f9; margin-bottom: 20px;",
                    h4("🎨 Batch Plot Customization"),
                    p("Adjust plot dimensions, resolution, and text sizes for bulk downloads:"),

                    fluidRow(
                      column(3,
                        numericInput("download_plot_width",
                                   "Width (inches):",
                                   value = 12,
                                   min = 6,
                                   max = 20,
                                   step = 1)
                      ),
                      column(3,
                        numericInput("download_plot_height",
                                   "Height (inches):",
                                   value = 8,
                                   min = 4,
                                   max = 16,
                                   step = 1)
                      ),
                      column(3,
                        selectInput("download_plot_dpi",
                                  "Resolution (DPI):",
                                  choices = c("150 (Draft)" = 150,
                                            "300 (Standard)" = 300,
                                            "600 (High Quality)" = 600),
                                  selected = 300)
                      ),
                      column(3,
                        numericInput("download_plot_cex",
                                   "Text Size:",
                                   value = 1.0,
                                   min = 0.5,
                                   max = 2.0,
                                   step = 0.1)
                      )
                    ),

                    # Network Visualization Customization
                    div(
                      style = "margin-top: 15px; padding: 15px; background-color: #fff; border: 1px solid #ddd; border-radius: 4px;",
                      h5("🎨 Network Graph Customization"),
                      fluidRow(
                        column(4,
                          selectInput("download_color_scheme",
                                    "Node Color Scheme:",
                                    choices = c("Default (Blue)" = "default",
                                              "Brain Areas" = "brain_areas"),
                                    selected = "default")
                        ),
                        column(4,
                          selectInput("network_node_shape",
                                    "Node Shape:",
                                    choices = c("Circle" = "circle",
                                              "Square" = "square",
                                              "None (point)" = "none"),
                                    selected = "circle")
                        ),
                        column(4,
                          checkboxInput("network_edge_sign_colors",
                                      "Color edges by correlation sign",
                                      value = FALSE)
                        )
                      ),
                      conditionalPanel(
                        condition = "input.network_edge_sign_colors == true",
                        fluidRow(
                          column(6,
                            div(style = "padding: 5px;",
                              tags$label("Positive Correlations:"),
                              tags$input(type = "color", id = "network_positive_color",
                                        value = "#2ecc71",
                                        style = "width: 100%; height: 35px; border: 1px solid #ccc; border-radius: 4px;")
                            )
                          ),
                          column(6,
                            div(style = "padding: 5px;",
                              tags$label("Negative Correlations:"),
                              tags$input(type = "color", id = "network_negative_color",
                                        value = "#e74c3c",
                                        style = "width: 100%; height: 35px; border: 1px solid #ccc; border-radius: 4px;")
                            )
                          )
                        )
                      ),
                      p(style = "font-size: 0.85em; color: #666; margin-top: 10px;",
                        "💡 Tip: Brain Areas coloring uses your anatomical region definitions from the Data tab.")
                    ),

                    # Preview selector and plot
                    div(
                      style = "margin-top: 20px; padding: 15px; background-color: white; border: 1px solid #ddd; border-radius: 4px;",
                      h5("📊 Live Preview"),
                      fluidRow(
                        column(6,
                          selectInput("download_preview_type",
                                    "Select plot to preview:",
                                    choices = c(
                                      "Network Density (Bar Chart)" = "density",
                                      "Correlation Matrix (Heatmap)" = "correlation",
                                      "Node Centrality (Bar Chart)" = "centrality",
                                      "Network Graph" = "network",
                                      "Clustering Coefficient (Bar)" = "clustering",
                                      "Path Length (Bar)" = "pathlength",
                                      "Settings & Aspect Ratio" = "settings"
                                    ),
                                    selected = "density")
                        ),
                        column(6,
                          p(style = "font-size: 0.85em; color: #666; margin-top: 25px;",
                            "👆 Select different plots to preview your customization settings")
                        )
                      ),
                      hr(style = "margin: 10px 0;"),
                      uiOutput("download_preview_plot_ui"),
                      p(style = "font-size: 0.85em; color: #666; margin-top: 10px;",
                        textOutput("download_preview_info"))
                    ),

                    p(style = "font-size: 0.9em; color: #666; margin-top: 15px;",
                      "💡 Tip: Use 12×8 inches at 300 DPI for standard publications. Adjust text size if labels appear too small or large.")
                  ),

                  # Download Options
                  fluidRow(
                    # Summary Only Download
                    column(6,
                      div(
                        style = "padding: 25px; border: 2px solid #3498db; border-radius: 10px; background-color: #f0f8ff; height: 100%;",
                        h4("📊 Summary Only", style = "color: #2980b9;"),
                        p("Quick download of key consensus findings:"),
                        tags$ul(
                          style = "text-align: left; font-size: 0.9em;",
                          tags$li("Consensus hub rankings"),
                          tags$li("Regional analysis plots"),
                          tags$li("Network visualizations"),
                          tags$li("Similarity heatmaps"),
                          tags$li("Comprehensive consensus data")
                        ),
                        br(),
                        downloadButton("download_summary_only",
                                     "📊 Download Summary Package",
                                     class = "btn btn-info btn-lg",
                                     style = "font-size: 16px; padding: 12px 25px; width: 100%;")
                      )
                    ),
                    # Complete Package Download
                    column(6,
                      div(
                        style = "padding: 25px; border: 2px solid #28a745; border-radius: 10px; background-color: #f8fff9; height: 100%;",
                        h4("📦 Complete Package", style = "color: #27ae60;"),
                        p("Full analysis with all results:"),
                        tags$ul(
                          style = "text-align: left; font-size: 0.9em;",
                          tags$li("55+ publication-ready plots"),
                          tags$li("Network graph files (JSON)"),
                          tags$li("Per-method correlation data"),
                          tags$li("Persistence analysis results"),
                          tags$li("All data tables as CSV")
                        ),
                        br(),
                        downloadButton("download_everything",
                                     "📦 Download Complete Package",
                                     class = "btn btn-success btn-lg",
                                     style = "font-size: 16px; padding: 12px 25px; width: 100%;")
                      )
                    )
                  )
                )
              )
            )
          ),

          # ==========================================
          # TAB 2: PLOT CUSTOMIZER (new functionality)
          # ==========================================
          tabPanel(
            title = "🎨 Plot Customizer",
            value = "plot_customizer",
            div(
              style = "padding: 20px;",

              conditionalPanel(
                condition = "!output.analysisComplete",
                div(
                  class = "alert alert-warning",
                  style = "max-width: 600px; margin: 20px auto; text-align: center;",
                  h4("⚠️ Analysis Required"),
                  p("Please complete your analysis first to customize and download individual plots."),
                  actionButton("goToAnalysisFromCustomizer", "Go to Analysis", class = "btn btn-primary btn-lg")
                )
              ),

              conditionalPanel(
                condition = "output.analysisComplete",
                div(
                  style = "max-width: 1200px; margin: 0 auto;",

                  # Header
                  div(
                    style = "text-align: center; margin-bottom: 20px;",
                    h3("🎨 Plot Customizer"),
                    p("Customize individual Summary plots with live preview and download")
                  ),

                  # Main content - two columns
                  fluidRow(
                    # Left column: Plot selection and preview
                    column(8,
                      div(
                        style = "padding: 20px; border: 1px solid #ddd; border-radius: 8px; background-color: #f9f9f9;",

                        # Plot selector
                        fluidRow(
                          column(12,
                            selectInput("customizer_plot_select",
                                      "Select Plot:",
                                      choices = c(
                                        "Consensus Node Metrics" = "Consensus Node Metrics",
                                        "Consensus Regional Analysis" = "Consensus Regional Analysis",
                                        "Consensus Subregional Analysis" = "Consensus Subregional Analysis",
                                        "Consensus Top Hubs" = "Consensus Top Hubs",
                                        "Consensus Agreement Matrix" = "Consensus Agreement Matrix",
                                        "Network Similarity Heatmap" = "Network Similarity Heatmap",
                                        "Conservation Heatmap" = "Conservation Heatmap",
                                        "Three-Way Eigenvector Comparison" = "Three-Way Eigenvector Comparison",
                                        "Hub Ranking Comparison" = "Hub Ranking Comparison",
                                        "Consensus Heatmap" = "Consensus Heatmap"
                                      ),
                                      selected = "Consensus Node Metrics",
                                      width = "100%")
                          )
                        ),

                        hr(),

                        # Live Preview with exact aspect ratio
                        div(
                          style = "background-color: white; padding: 10px; border: 1px solid #ccc; border-radius: 4px;",
                          h5("📊 Live Preview", style = "margin-top: 0;"),
                          p(style = "font-size: 0.85em; color: #666; margin-bottom: 10px;",
                            "Preview shows exact appearance at download. Aspect ratio matches your dimension settings."),
                          div(
                            style = "text-align: center;",
                            uiOutput("customizer_preview_container")
                          ),
                          div(
                            style = "text-align: center; margin-top: 10px;",
                            textOutput("customizer_preview_info")
                          )
                        )
                      )
                    ),

                    # Right column: Customization controls
                    column(4,
                      div(
                        style = "padding: 20px; border: 1px solid #ddd; border-radius: 8px; background-color: #fff;",

                        # Dimensions section
                        div(
                          style = "margin-bottom: 20px;",
                          h5("📐 Dimensions", style = "border-bottom: 1px solid #ddd; padding-bottom: 10px;"),
                          numericInput("customizer_width",
                                     "Width (inches):",
                                     value = 12,
                                     min = 4,
                                     max = 24,
                                     step = 0.5,
                                     width = "100%"),
                          numericInput("customizer_height",
                                     "Height (inches):",
                                     value = 8,
                                     min = 3,
                                     max = 18,
                                     step = 0.5,
                                     width = "100%"),
                          selectInput("customizer_dpi",
                                    "Resolution (DPI):",
                                    choices = c("150 (Draft)" = 150,
                                              "300 (Standard)" = 300,
                                              "600 (High Quality)" = 600),
                                    selected = 300,
                                    width = "100%")
                        ),

                        # Text section
                        div(
                          style = "margin-bottom: 20px;",
                          h5("📝 Text Settings", style = "border-bottom: 1px solid #ddd; padding-bottom: 10px;"),
                          textInput("customizer_title",
                                  "Custom Title:",
                                  value = "",
                                  placeholder = "(Use default title)",
                                  width = "100%"),
                          p(style = "font-size: 0.8em; color: #888; margin-top: -10px; margin-bottom: 15px;",
                            "Leave blank to use default title"),
                          sliderInput("customizer_title_size",
                                    "Title Size:",
                                    min = 0.8,
                                    max = 2.0,
                                    value = 1.2,
                                    step = 0.1,
                                    width = "100%"),
                          sliderInput("customizer_text_size",
                                    "Text Size:",
                                    min = 0.5,
                                    max = 2.0,
                                    value = 1.0,
                                    step = 0.1,
                                    width = "100%")
                        ),

                        # Download section
                        div(
                          style = "margin-top: 20px; padding-top: 15px; border-top: 2px solid #27ae60;",
                          h5("📥 Download", style = "color: #27ae60;"),
                          p(style = "font-size: 0.85em; color: #666;",
                            "Download this plot as a PNG file with your custom settings."),
                          downloadButton("customizer_download",
                                       "📥 Download This Plot",
                                       class = "btn btn-success btn-lg",
                                       style = "width: 100%; font-size: 16px; padding: 12px;")
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
}

# Simple downloads server - rebuilt for per-method analysis
downloads_server <- function(input, output, session, analysis_results, ui_state) {

  # Navigation handlers
  observeEvent(input$goToAnalysisFromDownloads, {
    updateTabItems(session, "sidebarMenu", "settings")
  })

  observeEvent(input$goToAnalysisFromCustomizer, {
    updateTabItems(session, "sidebarMenu", "settings")
  })

  # ==========================================
  # PLOT CUSTOMIZER SERVER LOGIC
  # ==========================================

  # Create reactive plot registry
  plot_registry <- reactive({
    create_summary_plot_registry(analysis_results, ui_state)
  })

  # Get available plots (those with data)
  available_plots <- reactive({
    registry <- plot_registry()
    available <- sapply(names(registry), function(name) {
      tryCatch({
        registry[[name]]$condition()
      }, error = function(e) FALSE)
    })
    names(registry)[available]
  })

  # Update plot choices based on what's available
  observe({
    available <- available_plots()
    if(length(available) > 0) {
      updateSelectInput(session, "customizer_plot_select",
                       choices = setNames(available, available),
                       selected = available[1])
    }
  })

  # Get current title (custom or default)
  get_current_title <- reactive({
    custom_title <- input$customizer_title %||% ""
    if(nchar(trimws(custom_title)) == 0) {
      selected <- input$customizer_plot_select %||% "Consensus Node Metrics"
      registry <- plot_registry()
      if(selected %in% names(registry)) {
        return(registry[[selected]]$default_title)
      }
    }
    return(custom_title)
  })

  # Dynamic preview container - creates plotOutput with correct aspect ratio
  output$customizer_preview_container <- renderUI({
    plot_width <- as.numeric(input$customizer_width %||% 12)
    plot_height <- as.numeric(input$customizer_height %||% 8)

    # Calculate preview size (scale to fit available space while maintaining aspect ratio)
    max_preview_width <- 700  # Max width in pixels for preview
    aspect_ratio <- plot_width / plot_height
    preview_width <- max_preview_width
    preview_height <- preview_width / aspect_ratio

    # Cap height to prevent overly tall previews
    max_preview_height <- 500
    if(preview_height > max_preview_height) {
      preview_height <- max_preview_height
      preview_width <- preview_height * aspect_ratio
    }

    plotOutput("customizer_preview_plot",
               width = paste0(preview_width, "px"),
               height = paste0(preview_height, "px"))
  })

  # Preview info text
  output$customizer_preview_info <- renderText({
    plot_width <- as.numeric(input$customizer_width %||% 12)
    plot_height <- as.numeric(input$customizer_height %||% 8)
    plot_dpi <- as.numeric(input$customizer_dpi %||% 300)
    text_size <- as.numeric(input$customizer_text_size %||% 1.0)
    title_size <- as.numeric(input$customizer_title_size %||% 1.2)

    sprintf("Output: %.1f × %.1f inches @ %d DPI | Text: %.1f× | Title: %.1f× | Aspect: %.2f:1",
            plot_width, plot_height, plot_dpi, text_size, title_size, plot_width/plot_height)
  })

  # Preview plot renderer - uses debounce to avoid excessive re-renders
  customizer_inputs <- reactive({
    list(
      plot_select = input$customizer_plot_select,
      width = input$customizer_width,
      height = input$customizer_height,
      dpi = input$customizer_dpi,
      title = input$customizer_title,
      title_size = input$customizer_title_size,
      text_size = input$customizer_text_size
    )
  })

  # Debounce the inputs (300ms delay)
  customizer_inputs_debounced <- debounce(customizer_inputs, 300)

  output$customizer_preview_plot <- renderPlot({
    # Get debounced inputs
    inputs <- customizer_inputs_debounced()

    selected_plot <- inputs$plot_select %||% "Consensus Node Metrics"
    text_size <- as.numeric(inputs$text_size %||% 1.0)
    title_size <- as.numeric(inputs$title_size %||% 1.2)
    custom_title <- inputs$title %||% ""

    # Check if analysis is complete
    if(is.null(analysis_results$comprehensive_consensus)) {
      par(mar = c(1, 1, 3, 1))
      plot(1, type = "n", axes = FALSE, xlab = "", ylab = "", main = "Plot Preview")
      text(1, 1, "Run analysis first to see preview", cex = 1.5, col = "gray50")
      return()
    }

    # Get registry and check if plot is available
    registry <- plot_registry()

    if(!selected_plot %in% names(registry)) {
      par(mar = c(1, 1, 3, 1))
      plot(1, type = "n", axes = FALSE, xlab = "", ylab = "", main = "Plot Not Found")
      text(1, 1, "Selected plot not available", cex = 1.2, col = "gray50")
      return()
    }

    plot_info <- registry[[selected_plot]]

    # Check if plot condition is met
    if(!plot_info$condition()) {
      par(mar = c(1, 1, 3, 1))
      plot(1, type = "n", axes = FALSE, xlab = "", ylab = "", main = selected_plot)
      text(1, 1, "Required data not available for this plot", cex = 1.2, col = "gray50")
      text(1, 0.85, "Run the appropriate analysis first", cex = 0.9, col = "#666")
      return()
    }

    # Apply text size globally
    par(cex = text_size, cex.main = title_size, cex.lab = text_size, cex.axis = text_size * 0.9)

    # Render the plot
    tryCatch({
      # Determine title to use
      title_to_use <- if(nchar(trimws(custom_title)) > 0) custom_title else NULL

      # Call the render function
      plot_info$render(title_override = title_to_use, title_size = title_size)

    }, error = function(e) {
      par(mar = c(1, 1, 3, 1))
      plot(1, type = "n", axes = FALSE, xlab = "", ylab = "", main = "Preview Error")
      text(1, 1, "Error rendering plot", cex = 1.2, col = "#e74c3c")
      text(1, 0.85, paste("Details:", substr(e$message, 1, 60)), cex = 0.8, col = "#666")
    })
  })

  # Single plot download handler
  output$customizer_download <- downloadHandler(
    filename = function() {
      selected_plot <- input$customizer_plot_select %||% "Plot"
      safe_name <- gsub("[^a-zA-Z0-9]", "_", selected_plot)
      paste0(safe_name, "_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")
    },
    content = function(file) {
      # Get settings
      plot_width <- as.numeric(input$customizer_width %||% 12)
      plot_height <- as.numeric(input$customizer_height %||% 8)
      plot_dpi <- as.numeric(input$customizer_dpi %||% 300)
      text_size <- as.numeric(input$customizer_text_size %||% 1.0)
      title_size <- as.numeric(input$customizer_title_size %||% 1.2)
      custom_title <- input$customizer_title %||% ""
      selected_plot <- input$customizer_plot_select %||% "Consensus Node Metrics"

      # Convert inches to pixels
      width_px <- plot_width * plot_dpi
      height_px <- plot_height * plot_dpi

      # Get registry
      registry <- plot_registry()

      if(!selected_plot %in% names(registry)) {
        stop("Selected plot not available")
      }

      plot_info <- registry[[selected_plot]]

      if(!plot_info$condition()) {
        stop("Required data not available for this plot")
      }

      # Create PNG file
      png(file, width = width_px, height = height_px, res = plot_dpi)

      tryCatch({
        # Apply text size
        par(cex = text_size, cex.main = title_size, cex.lab = text_size, cex.axis = text_size * 0.9)

        # Determine title to use
        title_to_use <- if(nchar(trimws(custom_title)) > 0) custom_title else NULL

        # Call the render function
        plot_info$render(title_override = title_to_use, title_size = title_size)

        dev.off()
      }, error = function(e) {
        dev.off()
        stop(paste("Error generating plot:", e$message))
      })
    },
    contentType = "image/png"
  )

  # Dynamic plot output UI - creates plotOutput with correct aspect ratio
  output$download_preview_plot_ui <- renderUI({
    plot_width <- as.numeric(input$download_plot_width %||% 12)
    plot_height <- as.numeric(input$download_plot_height %||% 8)

    # Calculate preview size (scale to fit screen while maintaining aspect ratio)
    preview_width <- 700  # Max width in pixels for preview
    aspect_ratio <- plot_width / plot_height
    preview_height <- preview_width / aspect_ratio

    plotOutput("download_preview_plot",
               width = paste0(preview_width, "px"),
               height = paste0(preview_height, "px"))
  })

  # Preview info text - shows current settings
  output$download_preview_info <- renderText({
    cex_setting <- as.numeric(input$download_plot_cex %||% 1.0)
    plot_width <- as.numeric(input$download_plot_width %||% 12)
    plot_height <- as.numeric(input$download_plot_height %||% 8)
    plot_dpi <- as.numeric(input$download_plot_dpi %||% 300)

    sprintf("Current settings: %.0f×%.0f inches @ %.0f DPI, Text size: %.1f×  |  Aspect ratio: %.2f:1",
            plot_width, plot_height, plot_dpi, cex_setting, plot_width/plot_height)
  })

  # Preview plot renderer - FULLY REACTIVE to all inputs
  output$download_preview_plot <- renderPlot({
    # REACTIVE DEPENDENCIES - any change triggers re-render
    preview_type <- input$download_preview_type %||% "density"
    cex_setting <- as.numeric(input$download_plot_cex %||% 1.0)
    plot_width <- as.numeric(input$download_plot_width %||% 12)
    plot_height <- as.numeric(input$download_plot_height %||% 8)
    plot_dpi <- as.numeric(input$download_plot_dpi %||% 300)

    # Check if analysis is complete
    if(is.null(analysis_results$global_metrics) || nrow(analysis_results$global_metrics) == 0) {
      par(mar = c(1, 1, 3, 1))
      plot(1, type = "n", axes = FALSE, xlab = "", ylab = "", main = "Analysis Preview")
      text(1, 1, "Run analysis first to see preview", cex = 1.5, col = "gray50")
      return()
    }

    # Apply text size globally
    par(cex = cex_setting, cex.main = cex_setting * 1.3,
        cex.lab = cex_setting * 1.1, cex.axis = cex_setting * 0.9)

    # Get data and colors
    metrics <- analysis_results$global_metrics
    groups <- unique(metrics$Group)
    colors <- if(!is.null(ui_state$group_colors)) {
      sapply(groups, function(g) ui_state$group_colors[[g]] %||% "#3498DB")
    } else {
      rainbow(length(groups))
    }

    # Render based on selection - INDIVIDUAL PLOTS ONLY
    tryCatch({

      if(preview_type == "density") {
        # Network Density Bar Chart
        par(mar = c(8, 5, 4, 2))
        if("Density" %in% names(metrics)) {
          barplot(metrics$Density,
                  names.arg = metrics$Group,
                  col = colors,
                  main = "Network Density by Group",
                  ylab = "Network Density",
                  las = 2,
                  border = NA,
                  ylim = c(0, max(metrics$Density, na.rm = TRUE) * 1.1))
          grid(nx = NA, ny = NULL, col = "gray90", lty = 1)
        }

      } else if(preview_type == "correlation") {
        # Correlation Heatmap
        par(mar = c(5, 5, 4, 2))
        if(!is.null(analysis_results$correlations) && length(analysis_results$correlations) > 0) {
          cor_matrix <- analysis_results$correlations[[1]]
          if(!is.null(cor_matrix) && is.matrix(cor_matrix)) {
            n <- min(nrow(cor_matrix), 15)  # Show max 15x15 for preview
            cor_subset <- cor_matrix[1:n, 1:n]

            image(1:n, 1:n, cor_subset,
                  main = "Correlation Matrix (Sample)",
                  xlab = "Variables", ylab = "Variables",
                  col = colorRampPalette(c("blue", "white", "red"))(100),
                  axes = FALSE)
            axis(1, at = 1:n, labels = colnames(cor_subset), las = 2, cex.axis = cex_setting * 0.7)
            axis(2, at = 1:n, labels = rownames(cor_subset), las = 2, cex.axis = cex_setting * 0.7)

            # Add color scale legend
            legend("topright", legend = c("High", "Medium", "Low"),
                   fill = c("red", "white", "blue"),
                   cex = cex_setting * 0.8, bg = "white")
          }
        } else {
          plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
          text(1, 1, "No correlation data available", cex = 1.2, col = "gray")
        }

      } else if(preview_type == "centrality") {
        # Node Centrality Plot
        par(mar = c(8, 5, 4, 2))
        if(!is.null(analysis_results$node_metrics) && nrow(analysis_results$node_metrics) > 0) {
          node_data <- analysis_results$node_metrics

          # Get top 10 nodes by degree
          top_nodes <- head(node_data[order(-node_data$Degree), ], 10)

          barplot(top_nodes$Degree,
                  names.arg = top_nodes$Node,
                  col = colorRampPalette(c("#3498db", "#e74c3c"))(10),
                  main = "Top 10 Nodes by Degree Centrality",
                  ylab = "Degree",
                  las = 2,
                  border = NA)
          grid(nx = NA, ny = NULL, col = "gray90", lty = 1)
        } else {
          plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
          text(1, 1, "No node metrics available", cex = 1.2, col = "gray")
        }

      } else if(preview_type == "network") {
        # Network Graph
        par(mar = c(1, 1, 3, 1))
        if(!is.null(analysis_results$networks) && length(analysis_results$networks) > 0) {
          network <- analysis_results$networks[[1]]
          group_name <- names(analysis_results$networks)[1]

          if(requireNamespace("igraph", quietly = TRUE)) {
            # Determine vertex colors based on user selection
            color_scheme <- input$download_color_scheme %||% "default"
            node_names <- igraph::V(network)$name

            if(color_scheme == "brain_areas" && !is.null(ui_state$brain_areas)) {
              # Use brain area colors
              source("modules/export_functions.R", local = TRUE)
              vertex_colors <- apply_brain_area_colors(node_names, ui_state$brain_areas, ui_state$area_colors)
            } else {
              vertex_colors <- "#3498db"
            }

            plot(network,
                 main = paste("🌐", group_name, "Network"),
                 vertex.size = 8,
                 vertex.color = vertex_colors,
                 vertex.frame.color = "#e74c3c",
                 vertex.frame.width = 2,
                 vertex.label.cex = 0.6 * cex_setting,
                 vertex.label.color = "black",
                 edge.color = "gray70",
                 edge.width = 1.2,
                 layout = igraph::layout_with_fr(network))
          }
        } else {
          plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
          text(1, 1, "No network available", cex = 1.2, col = "gray")
        }

      } else if(preview_type == "clustering") {
        # Clustering Coefficient
        par(mar = c(8, 5, 4, 2))
        if("Clustering" %in% names(metrics)) {
          barplot(metrics$Clustering,
                  names.arg = metrics$Group,
                  col = colors,
                  main = "Clustering Coefficient by Group",
                  ylab = "Clustering Coefficient",
                  las = 2,
                  border = NA,
                  ylim = c(0, max(metrics$Clustering, na.rm = TRUE) * 1.1))
          grid(nx = NA, ny = NULL, col = "gray90", lty = 1)
        } else {
          plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
          text(1, 1, "No clustering data available", cex = 1.2, col = "gray")
        }

      } else if(preview_type == "pathlength") {
        # Path Length
        par(mar = c(8, 5, 4, 2))
        if("Avg_Path_Length" %in% names(metrics)) {
          valid_paths <- metrics$Avg_Path_Length[is.finite(metrics$Avg_Path_Length)]
          if(length(valid_paths) > 0) {
            barplot(ifelse(is.finite(metrics$Avg_Path_Length), metrics$Avg_Path_Length, 0),
                    names.arg = metrics$Group,
                    col = colors,
                    main = "Average Path Length by Group",
                    ylab = "Path Length",
                    las = 2,
                    border = NA)
            grid(nx = NA, ny = NULL, col = "gray90", lty = 1)
          }
        } else {
          plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
          text(1, 1, "No path length data available", cex = 1.2, col = "gray")
        }

      } else if(preview_type == "settings") {
        # Settings Display
        par(mar = c(2, 2, 3, 2))
        plot(1, type = "n", axes = FALSE, xlab = "", ylab = "",
             xlim = c(0, 10), ylim = c(0, 10),
             main = "Download Settings Preview")

        # Display settings with visual examples
        rect(2, 7, 8, 9, col = "#ecf0f1", border = "#34495e", lwd = 2)
        text(5, 8, paste("Dimensions:", plot_width, "×", plot_height, "inches"),
             cex = cex_setting * 1.2, col = "#2c3e50", font = 2)

        rect(2, 5, 8, 6.5, col = "#e8f5e9", border = "#27ae60", lwd = 2)
        text(5, 5.75, paste("Resolution:", plot_dpi, "DPI"),
             cex = cex_setting * 1.2, col = "#27ae60", font = 2)

        rect(2, 3, 8, 4.5, col = "#fff3e0", border = "#f39c12", lwd = 2)
        text(5, 3.75, paste("Text Size:", cex_setting, "×"),
             cex = cex_setting * 1.2, col = "#f39c12", font = 2)

        # Aspect ratio visualization
        aspect_ratio <- plot_width / plot_height
        rect_width <- 3
        rect_height <- rect_width / aspect_ratio
        rect(3.5, 0.5, 3.5 + rect_width, 0.5 + rect_height,
             col = "#3498db", border = "#2c3e50", lwd = 2)
        text(5, 0.2, sprintf("Aspect: %.2f:1", aspect_ratio),
             cex = cex_setting * 0.9, col = "#34495e")
      }

    }, error = function(e) {
      par(mar = c(1, 1, 3, 1))
      plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
      text(1, 1, "Preview unavailable", cex = 1.2, col = "gray50")
      text(1, 0.8, paste("Error:", e$message), cex = 0.8, col = "red")
    })
  })

  # Single download handler for everything
  output$download_everything <- downloadHandler(
    filename = function() {
      paste0("BrainAnalysis_Complete_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".zip")
    },
    content = function(file) {
      # Create temporary directory with unique timestamp
      temp_base <- tempfile(pattern = "BrainAnalysis_")
      dir.create(temp_base, recursive = TRUE, showWarnings = FALSE)

      # Create subdirectories
      data_dir <- file.path(temp_base, "Data")
      data_permethod_dir <- file.path(data_dir, "PerMethod")
      data_persistence_dir <- file.path(data_dir, "Persistence")
      data_consensus_dir <- file.path(data_dir, "Consensus")
      docs_dir <- file.path(temp_base, "Documentation")

      dir.create(data_dir, recursive = TRUE, showWarnings = FALSE)
      dir.create(data_permethod_dir, recursive = TRUE, showWarnings = FALSE)
      dir.create(data_persistence_dir, recursive = TRUE, showWarnings = FALSE)
      dir.create(data_consensus_dir, recursive = TRUE, showWarnings = FALSE)
      dir.create(docs_dir, recursive = TRUE, showWarnings = FALSE)

      tryCatch({

        cat("📦 Creating download package...\n")

        # ==========================================
        # GENERATE ALL PLOTS
        # ==========================================

        cat("📊 Generating visualizations...\n")

        # Create organized plot directory structure matching app tabs
        plots_base <- file.path(temp_base, "Plots")
        dir.create(plots_base, recursive = TRUE, showWarnings = FALSE)

        # Create subdirectories for each analysis section
        plots_dirs <- list(
          imputation = file.path(plots_base, "01_Imputation"),
          permethod = file.path(plots_base, "02_PerMethod_Correlations"),
          topology = file.path(plots_base, "03_Topology_Analysis"),
          weighted = file.path(plots_base, "04_Weighted_Network"),
          persistence = file.path(plots_base, "05_Persistence_Analysis"),
          crossmethod = file.path(plots_base, "06_CrossMethod_Comparison"),
          advanced = file.path(plots_base, "07_Advanced_Analysis"),
          consensus = file.path(plots_base, "08_Consensus"),
          summary = file.path(plots_base, "09_Summary")
        )

        for(dir in plots_dirs) {
          dir.create(dir, recursive = TRUE, showWarnings = FALSE)
        }

        # Get plot customization settings from input
        plot_width <- as.numeric(input$download_plot_width %||% 12)
        plot_height <- as.numeric(input$download_plot_height %||% 8)
        plot_dpi <- as.numeric(input$download_plot_dpi %||% 300)
        plot_cex <- as.numeric(input$download_plot_cex %||% 1.0)

        cat(sprintf("📐 Plot settings: %d×%d inches @ %d DPI, text size: %.1f\n",
                   plot_width, plot_height, plot_dpi, plot_cex))

        # Convert inches to pixels for png device
        plot_width_px <- plot_width * plot_dpi
        plot_height_px <- plot_height * plot_dpi

        # Generate plots with custom settings
        tryCatch({
          # Temporarily set device and graphics parameters for all plots
          old_res <- getOption("shiny.plot.res", 96)
          old_cex <- par("cex")

          options(shiny.plot.res = plot_dpi)
          par(cex = plot_cex)

          # Prepare network customization options
          network_options <- list(
            use_anatomical_colors = !is.null(input$download_color_scheme) && input$download_color_scheme == "brain_areas",
            node_shape = input$network_node_shape %||% "circle",
            edge_sign_colors = !is.null(input$network_edge_sign_colors) && input$network_edge_sign_colors,
            positive_color = input$network_positive_color %||% "#2ecc71",
            negative_color = input$network_negative_color %||% "#e74c3c"
          )

          plot_results <- complete_plot_system(analysis_results, ui_state, plots_base,
                                               width = plot_width_px, height = plot_height_px, dpi = plot_dpi,
                                               network_options = network_options)

          # Restore options
          options(shiny.plot.res = old_res)
          par(cex = old_cex)

          cat(sprintf("✅ Generated %d plots successfully\n", plot_results$successful))
          if(plot_results$skipped > 0) {
            cat(sprintf("⏭️  Skipped %d plots (data not available)\n", plot_results$skipped))
          }

          # Organize plots into subdirectories
          cat("📁 Organizing plots into folders...\n")
          organize_plots_by_category(plots_base, plots_dirs)

        }, error = function(e) {
          cat("⚠️ Warning: Plot generation encountered errors\n")
          cat(sprintf("   Error: %s\n", conditionMessage(e)))
        })

        # ==========================================
        # EXPORT NETWORK GRAPHS AS JSON
        # ==========================================

        cat("🕸️  Exporting network graphs as JSON...\n")

        networks_dir <- file.path(data_dir, "Networks_JSON")
        dir.create(networks_dir, recursive = TRUE, showWarnings = FALSE)

        network_count <- export_networks_json(analysis_results, networks_dir)
        cat(sprintf("✅ Exported %d network graphs\n", network_count))

        # ==========================================
        # SAVE PER-METHOD DATA
        # ==========================================

        cat("💾 Saving per-method correlation data...\n")

        # Per-method correlations
        if(!is.null(analysis_results$correlation_methods_raw)) {
          for(method_name in names(analysis_results$correlation_methods_raw)) {
            method_data <- analysis_results$correlation_methods_raw[[method_name]]

            if(!is.null(method_data) && length(method_data) > 0) {
              for(group_name in names(method_data)) {
                cor_matrix <- method_data[[group_name]]
                if(!is.null(cor_matrix) && is.matrix(cor_matrix)) {
                  safe_method <- gsub("[^a-zA-Z0-9]", "_", method_name)
                  safe_group <- gsub("[^a-zA-Z0-9]", "_", group_name)
                  filename <- paste0("Correlation_", safe_method, "_", safe_group, ".csv")
                  write.csv(cor_matrix, file.path(data_permethod_dir, filename), row.names = TRUE)
                }
              }
            }
          }
        }

        # Per-method network metrics
        cat("💾 Saving per-method network metrics...\n")

        if(!is.null(analysis_results$method_percolation_results)) {
          for(method_name in names(analysis_results$method_percolation_results)) {
            method_data <- analysis_results$method_percolation_results[[method_name]]
            safe_method <- gsub("[^a-zA-Z0-9]", "_", method_name)

            # Save node metrics
            if(!is.null(method_data$node_metrics) && nrow(method_data$node_metrics) > 0) {
              filename <- paste0("NodeMetrics_", safe_method, ".csv")
              write.csv(method_data$node_metrics,
                       file.path(data_permethod_dir, filename),
                       row.names = FALSE)
            }

            # Save global metrics
            if(!is.null(method_data$global_metrics) && nrow(method_data$global_metrics) > 0) {
              filename <- paste0("GlobalMetrics_", safe_method, ".csv")
              write.csv(method_data$global_metrics,
                       file.path(data_permethod_dir, filename),
                       row.names = FALSE)
            }

            # Save thresholds
            if(!is.null(method_data$thresholds) && length(method_data$thresholds) > 0) {
              thresh_df <- data.frame(
                Group = names(method_data$thresholds),
                Threshold = unlist(method_data$thresholds),
                row.names = NULL
              )
              filename <- paste0("Thresholds_", safe_method, ".csv")
              write.csv(thresh_df,
                       file.path(data_permethod_dir, filename),
                       row.names = FALSE)
            }
          }
        }

        # Per-method weighted analysis
        if(!is.null(analysis_results$method_weighted_results)) {
          for(method_name in names(analysis_results$method_weighted_results)) {
            method_data <- analysis_results$method_weighted_results[[method_name]]
            safe_method <- gsub("[^a-zA-Z0-9]", "_", method_name)

            if(!is.null(method_data$node_strength) && nrow(method_data$node_strength) > 0) {
              filename <- paste0("WeightedNodeStrength_", safe_method, ".csv")
              write.csv(method_data$node_strength,
                       file.path(data_permethod_dir, filename),
                       row.names = FALSE)
            }
          }
        }

        # ==========================================
        # SAVE PERSISTENCE DATA
        # ==========================================

        cat("💾 Saving persistence analysis results...\n")

        if(!is.null(analysis_results$persistence_results)) {
          for(method_name in names(analysis_results$persistence_results)) {
            method_pers <- analysis_results$persistence_results[[method_name]]
            safe_method <- gsub("[^a-zA-Z0-9]", "_", method_name)

            if(!is.null(method_pers) && length(method_pers) > 0) {
              for(group_name in names(method_pers)) {
                group_data <- method_pers[[group_name]]
                safe_group <- gsub("[^a-zA-Z0-9]", "_", group_name)

                # Save hub persistence
                if(!is.null(group_data$hub_persistence) && nrow(group_data$hub_persistence) > 0) {
                  filename <- paste0("HubPersistence_", safe_method, "_", safe_group, ".csv")
                  write.csv(group_data$hub_persistence,
                           file.path(data_persistence_dir, filename),
                           row.names = FALSE)
                }

                # Save metrics evolution
                if(!is.null(group_data$metrics_evolution) && nrow(group_data$metrics_evolution) > 0) {
                  filename <- paste0("MetricsEvolution_", safe_method, "_", safe_group, ".csv")
                  write.csv(group_data$metrics_evolution,
                           file.path(data_persistence_dir, filename),
                           row.names = FALSE)
                }
              }
            }
          }
        }

        # ==========================================
        # SAVE CONSENSUS DATA
        # ==========================================

        cat("💾 Saving consensus analysis results...\n")

        if(!is.null(analysis_results$consensus_analysis)) {
          for(group_name in names(analysis_results$consensus_analysis)) {
            consensus <- analysis_results$consensus_analysis[[group_name]]
            safe_group <- gsub("[^a-zA-Z0-9]", "_", group_name)

            # Save consensus hubs
            if(!is.null(consensus$strong_consensus) && length(consensus$strong_consensus) > 0) {
              consensus_df <- data.frame(
                Node = consensus$strong_consensus,
                ConsensusScore = consensus$consensus_score[consensus$strong_consensus],
                MethodCount = consensus$method_count,
                row.names = NULL
              )
              filename <- paste0("ConsensusHubs_", safe_group, ".csv")
              write.csv(consensus_df,
                       file.path(data_consensus_dir, filename),
                       row.names = FALSE)
            }

            # Save agreement matrix
            if(!is.null(consensus$agreement_matrix) && nrow(consensus$agreement_matrix) > 0) {
              filename <- paste0("MethodAgreement_", safe_group, ".csv")
              write.csv(consensus$agreement_matrix,
                       file.path(data_consensus_dir, filename),
                       row.names = TRUE)
            }
          }
        }

        # ==========================================
        # SAVE COMPREHENSIVE CONSENSUS ANALYTICS
        # ==========================================

        if(!is.null(analysis_results$comprehensive_consensus)) {
          cat("💎 Exporting comprehensive consensus analytics...\n")

          data_comprehensive_dir <- file.path(data_dir, "ComprehensiveConsensus")
          dir.create(data_comprehensive_dir, showWarnings = FALSE, recursive = TRUE)

          for(group_name in names(analysis_results$comprehensive_consensus)) {
            group_data <- analysis_results$comprehensive_consensus[[group_name]]
            safe_group <- gsub("[^a-zA-Z0-9]", "_", group_name)

            if(!is.null(group_data$consensus_scores) && nrow(group_data$consensus_scores) > 0) {
              # Main consensus scores table
              filename <- paste0("ComprehensiveConsensus_", safe_group, ".csv")
              write.csv(group_data$consensus_scores,
                       file.path(data_comprehensive_dir, filename),
                       row.names = FALSE)

              # Hub lists by approach and method
              hubs_dir <- file.path(data_comprehensive_dir, paste0(safe_group, "_HubLists"))
              dir.create(hubs_dir, showWarnings = FALSE)

              # Weighted hubs by method
              if(!is.null(group_data$weighted_hubs_by_method)) {
                for(method in names(group_data$weighted_hubs_by_method)) {
                  hubs <- group_data$weighted_hubs_by_method[[method]]
                  if(length(hubs) > 0) {
                    filename <- paste0("Weighted_", method, ".csv")
                    write.csv(data.frame(Node = hubs),
                             file.path(hubs_dir, filename),
                             row.names = FALSE)
                  }
                }
              }

              # Percolation hubs by method
              if(!is.null(group_data$percolation_hubs_by_method)) {
                for(method in names(group_data$percolation_hubs_by_method)) {
                  hubs <- group_data$percolation_hubs_by_method[[method]]
                  if(length(hubs) > 0) {
                    filename <- paste0("Percolation_", method, ".csv")
                    write.csv(data.frame(Node = hubs),
                             file.path(hubs_dir, filename),
                             row.names = FALSE)
                  }
                }
              }

              # Persistence hubs by method
              if(!is.null(group_data$persistence_hubs_by_method)) {
                for(method in names(group_data$persistence_hubs_by_method)) {
                  hubs <- group_data$persistence_hubs_by_method[[method]]
                  if(length(hubs) > 0) {
                    filename <- paste0("Persistence_", method, ".csv")
                    write.csv(data.frame(Node = hubs),
                             file.path(hubs_dir, filename),
                             row.names = FALSE)
                  }
                }
              }
            }
          }
        }

        # Save network similarity results
        if(!is.null(analysis_results$network_similarity)) {
          cat("🔗 Exporting network similarity metrics...\n")

          similarity_data <- analysis_results$network_similarity

          # Cross-method average similarities
          if(!is.null(similarity_data$cross_method_average) &&
             nrow(similarity_data$cross_method_average) > 0) {
            filename <- "NetworkSimilarity_CrossMethodAverage.csv"
            write.csv(similarity_data$cross_method_average,
                     file.path(data_comprehensive_dir, filename),
                     row.names = FALSE)
          }

          # Per-method similarities
          if(!is.null(similarity_data$by_method)) {
            similarity_permethod_dir <- file.path(data_comprehensive_dir, "NetworkSimilarity_ByMethod")
            dir.create(similarity_permethod_dir, showWarnings = FALSE)

            for(method in names(similarity_data$by_method)) {
              method_sim <- similarity_data$by_method[[method]]
              if(!is.null(method_sim) && nrow(method_sim) > 0) {
                safe_method <- gsub("[^a-zA-Z0-9]", "_", method)
                filename <- paste0("Similarity_", safe_method, ".csv")
                write.csv(method_sim,
                         file.path(similarity_permethod_dir, filename),
                         row.names = FALSE)
              }
            }
          }
        }

        # Save aggregated network metrics
        if(!is.null(analysis_results$aggregated_metrics)) {
          cat("📊 Exporting aggregated network metrics...\n")

          for(group_name in names(analysis_results$aggregated_metrics)) {
            group_data <- analysis_results$aggregated_metrics[[group_name]]
            safe_group <- gsub("[^a-zA-Z0-9]", "_", group_name)

            if(!is.null(group_data) && nrow(group_data) > 0) {
              filename <- paste0("AggregatedMetrics_", safe_group, ".csv")
              write.csv(group_data,
                       file.path(data_comprehensive_dir, filename),
                       row.names = FALSE)
            }
          }
        }

        # ==========================================
        # SAVE LEGACY/BACKWARD COMPATIBILITY DATA
        # ==========================================

        # Raw data
        if(!is.null(analysis_results$raw_data)) {
          write.csv(analysis_results$raw_data,
                   file.path(data_dir, "RawData.csv"),
                   row.names = FALSE)
        }

        # Imputed data
        if(!is.null(analysis_results$imputation$imputed_data)) {
          write.csv(analysis_results$imputation$imputed_data,
                   file.path(data_dir, "ImputedData.csv"),
                   row.names = FALSE)
        }

        # ==========================================
        # CREATE DOCUMENTATION
        # ==========================================

        cat("📝 Creating documentation...\n")

        # Analysis summary
        summary_content <- c(
          "BRAIN NETWORK ANALYSIS - COMPLETE RESULTS",
          "==========================================",
          "",
          paste("Analysis Date:", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
          paste("Software: ConsensusConnectR v3.0 - Multimethod Consensus Analysis"),
          "",
          "ANALYSIS APPROACH:",
          "",
          "This analysis uses a comprehensive multimethod approach combining 5 correlation",
          "methods with 3 analytical approaches (15 method-approach combinations), followed",
          "by rank-based consensus identification of robust findings:",
          "",
          "CORRELATION METHODS ANALYZED:",
          "  1. Pearson correlation (linear relationships)",
          "  2. Spearman rank correlation (monotonic relationships)",
          "  3. Biweight midcorrelation (robust to outliers)",
          "  4. Shrinkage (James-Stein) correlation (regularized)",
          "  5. Partial correlation (direct connections)",
          "",
          "ANALYTICAL APPROACHES:",
          "  1. Weighted Analysis - Threshold-free networks",
          "  2. Percolation Analysis - Data-driven optimal thresholding",
          "  3. Persistence Analysis - Multi-threshold hub robustness",
          "",
          "ANALYSIS WORKFLOW:",
          "  1. Data Import: Upload CSV with measurements and group assignments",
          "  2. Anatomical Regions: Map measurements to brain regions",
          "  3. Analysis Settings: Select methods and approaches to run",
          "  4. Run Analysis: Comprehensive 15-combination analysis",
          "  5. Summary: Cross-method consensus findings",
          "  6. Results: Detailed per-method and per-approach results",
          "  7. Statistical Tests: Permutation testing and validation",
          "  8. Downloads: Export all plots and data",
          "",
          "FOLDER STRUCTURE:",
          "",
          "📁 Plots/ - Organized by analysis section (matching Results tab structure)",
          "   ├── 01_Imputation/ - Data quality and imputation analysis",
          "   ├── 02_PerMethod_Correlations/ - Individual correlation method results",
          "   ├── 03_Topology_Analysis/ - Percolation and network topology",
          "   ├── 04_Weighted_Network/ - Threshold-free weighted analysis",
          "   ├── 05_Persistence_Analysis/ - Multi-threshold hub persistence",
          "   ├── 06_CrossMethod_Comparison/ - Cross-method shared results",
          "   ├── 07_Advanced_Analysis/ - Statistical tests and validation",
          "   ├── 08_Consensus/ - Rank-based consensus across all methods",
          "   └── 09_Summary/ - Overview dashboards and summary visualizations",
          "",
          "📁 Data/",
          "   ├── RawData.csv - Original uploaded data",
          "   ├── ImputedData.csv - Data after imputation",
          "   │",
          "   ├── 📁 Networks_JSON/ - Network graphs in JSON format",
          "   │   └── Network_[Method]_[Group].json",
          "   │",
          "   ├── 📁 PerMethod/",
          "   │   ├── Correlation_[Method]_[Group].csv",
          "   │   ├── NodeMetrics_[Method].csv",
          "   │   ├── GlobalMetrics_[Method].csv",
          "   │   ├── Thresholds_[Method].csv",
          "   │   └── WeightedNodeStrength_[Method].csv",
          "   │",
          "   ├── 📁 Persistence/",
          "   │   ├── HubPersistence_[Method]_[Group].csv",
          "   │   └── MetricsEvolution_[Method]_[Group].csv",
          "   │",
          "   └── 📁 Consensus/",
          "       ├── ConsensusHubs_[Group].csv",
          "       └── MethodAgreement_[Group].csv",
          "",
          "📁 Documentation/",
          "   ├── Analysis_Summary.txt (this file)",
          "   ├── Methods_Description.txt",
          "   ├── Data_Dictionary.txt",
          "   └── Plot_Manifest.txt",
          "",
          "KEY FEATURES:",
          "",
          "✓ Multimethod Approach: 5 correlation methods × 3 analytical approaches",
          "✓ Rank-Based Consensus: Identifies robust findings across 15 combinations",
          "✓ Three Analytical Perspectives: Weighted, Percolation, and Persistence",
          "✓ Statistical Validation: Permutation testing and hub overlap analysis",
          "✓ Organized Navigation: Separate Summary and Results sections",
          "✓ Publication-Ready: High-resolution plots and CSV data exports",
          "",
          "NAVIGATION STRUCTURE:",
          "",
          "The application is organized into 7 main sections:",
          "  1. Data Import - Upload and configure your dataset",
          "  2. Anatomical Regions - Map measurements to brain regions",
          "  3. Analysis Settings - Select methods and parameters",
          "  4. Summary - Cross-method consensus findings (main results)",
          "  5. Results - Detailed per-method and per-approach analyses",
          "  6. Downloads - Export all plots, data, and networks",
          "  7. About - Software information and workflow overview",
          "",
          "CITATION:",
          "",
          "Please cite: ConsensusConnectR v3.0 (Multimethod Consensus Analysis) [Software]. 2025.",
          "",
          "For questions or support, refer to the ConsensusConnectR documentation."
        )

        writeLines(summary_content, file.path(docs_dir, "Analysis_Summary.txt"))

        # Methods documentation
        methods_content <- c(
          "METHODS DOCUMENTATION",
          "=====================",
          "",
          "MULTIMETHOD CONSENSUS APPROACH:",
          "",
          "This analysis combines 5 correlation methods with 3 analytical approaches,",
          "creating 15 method-approach combinations. Rank-based consensus identifies",
          "findings that emerge consistently across methodological choices.",
          "",
          "1. CORRELATION METHODS (5 options):",
          "",
          "   Pearson: Linear correlation, parametric",
          "   Spearman: Rank-based correlation, robust to outliers",
          "   Biweight: Robust correlation, down-weights extreme values",
          "   Shrinkage: James-Stein regularized correlation",
          "   Partial: Direct connections controlling for other variables",
          "",
          "2. ANALYTICAL APPROACHES (3 options):",
          "",
          "   A. WEIGHTED ANALYSIS (threshold-free):",
          "      - Uses all correlation values weighted by strength",
          "      - No arbitrary thresholding",
          "      - Preserves full correlation structure",
          "",
          "   B. PERCOLATION ANALYSIS (optimal single threshold):",
          "      - Data-driven threshold selection per method and group",
          "      - Maximizes giant connected component size",
          "      - Balances network sparsity and connectivity",
          "      - Creates binary networks at optimal threshold",
          "",
          "   C. PERSISTENCE ANALYSIS (multi-threshold):",
          "      - Tracks hub nodes across threshold range (0.1 to 0.9)",
          "      - Calculates Area Under Curve (AUC) for each metric",
          "      - Identifies hubs robust to threshold choice",
          "      - Provides stability assessment",
          "",
          "3. RANK-BASED CONSENSUS FRAMEWORK:",
          "",
          "   - Each method-approach combination ranks nodes by importance",
          "   - Consensus rank = average rank across all 15 combinations",
          "   - Lower consensus rank = more consistently important node",
          "   - Avoids scale-dependent biases between methods",
          "   - Identifies robust hubs agreed upon by most combinations",
          "",
          "4. NETWORK METRICS:",
          "",
          "   Node-level:",
          "   - Degree centrality (number of connections)",
          "   - Betweenness centrality (bridge nodes)",
          "   - Eigenvector centrality (influence in network)",
          "   - Closeness centrality (average distance to others)",
          "   - PageRank (importance based on connections)",
          "",
          "   Global:",
          "   - Network density",
          "   - Clustering coefficient",
          "   - Average path length",
          "   - Modularity",
          "   - Assortativity",
          "",
          "5. STATISTICAL VALIDATION:",
          "",
          "   A. ROI-Level Permutation Testing:",
          "      - Tests which brain regions differ between groups",
          "      - 5000 permutations with FDR correction",
          "      - Identifies significantly different regions",
          "",
          "   B. Hub Overlap Analysis:",
          "      - Quantifies hub consistency across groups",
          "      - Jaccard index: J = |A ∩ B| / |A ∪ B|",
          "      - Identifies shared vs. unique hubs",
          "",
          "   C. Global Network Permutation (optional):",
          "      - Tests if network topology differs between groups",
          "      - Metrics: density, clustering, path length, modularity",
          "",
          "6. APPLICATION WORKFLOW:",
          "",
          "   Step 1: Data Import",
          "      - Upload CSV file (Subject × ROI matrix)",
          "      - Configure ID, group, and measurement columns",
          "",
          "   Step 2: Anatomical Regions",
          "      - Map measurements to brain regions",
          "      - Assign colors to regions and groups",
          "",
          "   Step 3: Analysis Settings",
          "      - Select correlation methods to use",
          "      - Select analytical approaches to apply",
          "      - Set persistence threshold resolution",
          "",
          "   Step 4: Run Analysis",
          "      - Comprehensive analysis across all combinations",
          "      - Progress tracking and error handling",
          "",
          "   Step 5: Summary Tab",
          "      - View consensus findings (main results)",
          "      - Tabs A-D: Node metrics, networks, similarity, regional",
          "",
          "   Step 6: Results Tab",
          "      - Explore detailed per-method results",
          "      - Tabs 1-7: All individual analyses",
          "",
          "   Step 7: Statistical Tests",
          "      - Validate findings with permutation testing",
          "      - Tabs 7a-7c: ROI tests, hub overlap, global tests",
          "",
          "   Step 8: Downloads",
          "      - Export all plots and data",
          "      - Customize plot dimensions and resolution",
          "",
          "SOFTWARE PACKAGES:",
          "   - R statistical software (≥4.0)",
          "   - igraph: network analysis",
          "   - WGCNA: weighted correlation",
          "   - mice: multiple imputation",
          "   - corpcor: shrinkage estimation",
          "",
          "QUALITY CONTROL:",
          "   - Data validation and missing data handling",
          "   - Method-specific error handling (e.g., partial correlation with small n)",
          "   - Graceful degradation if methods fail",
          "   - Comprehensive result validation"
        )

        writeLines(methods_content, file.path(docs_dir, "Methods_Description.txt"))

        # Data dictionary
        dict_content <- c(
          "DATA DICTIONARY",
          "===============",
          "",
          "NETWORK JSON FILES:",
          "",
          "Network_[Method]_[Group].json:",
          "  - Network graph in JSON format",
          "  - Contains: nodes (id, name), edges (source, target, weight)",
          "  - Includes basic network metrics (n_nodes, n_edges, density)",
          "  - Can be imported into Cytoscape, Gephi, or custom visualization tools",
          "  - Compatible with JavaScript network libraries (D3.js, vis.js, etc.)",
          "",
          "PER-METHOD FILES:",
          "",
          "Correlation_[Method]_[Group].csv:",
          "  - Correlation matrix for specified method and group",
          "  - Rows and columns are brain regions",
          "  - Values are correlation coefficients (-1 to 1)",
          "",
          "NodeMetrics_[Method].csv:",
          "  - Columns: Group, Node, Degree, Betweenness, Eigenvector, Closeness, PageRank",
          "  - Network centrality measures for each node",
          "  - Calculated from method-specific networks",
          "",
          "GlobalMetrics_[Method].csv:",
          "  - Columns: Group, Density, Clustering, AvgPathLength, Modularity, etc.",
          "  - Network-level properties",
          "  - One row per group",
          "",
          "Thresholds_[Method].csv:",
          "  - Columns: Group, Threshold",
          "  - Optimal percolation threshold for each group",
          "  - Determined by percolation analysis",
          "",
          "WeightedNodeStrength_[Method].csv:",
          "  - Columns: Group, Node, NodeStrength, AvgEdgeWeight",
          "  - Weighted network metrics (threshold-free)",
          "  - NodeStrength = sum of weighted connections",
          "",
          "PERSISTENCE FILES:",
          "",
          "HubPersistence_[Method]_[Group].csv:",
          "  - Columns: Node, PersistenceScore, MinThreshold, MaxThreshold",
          "  - PersistenceScore: fraction of thresholds where node is hub (0-1)",
          "  - MinThreshold: lowest threshold where node appears as hub",
          "  - MaxThreshold: highest threshold where node appears as hub",
          "",
          "MetricsEvolution_[Method]_[Group].csv:",
          "  - Columns: Threshold, Density, Clustering, AvgPathLength, Modularity, etc.",
          "  - How network metrics change as threshold increases",
          "  - One row per threshold value",
          "",
          "CONSENSUS FILES:",
          "",
          "ConsensusHubs_[Group].csv:",
          "  - Columns: Node, ConsensusScore, MethodCount",
          "  - Nodes identified as hubs by ≥60% of methods",
          "  - ConsensusScore: number of methods agreeing",
          "  - MethodCount: total methods analyzed",
          "",
          "MethodAgreement_[Group].csv:",
          "  - Rows: Nodes, Columns: Methods (Pearson, Spearman, etc.)",
          "  - Values: 1 if method identifies node as hub, 0 otherwise",
          "  - Shows which methods agree on each hub",
          "",
          "MAIN DATA FILES:",
          "",
          "RawData.csv:",
          "  - Original uploaded data",
          "  - Includes Group column and all brain region columns",
          "",
          "ImputedData.csv:",
          "  - Data after missing value imputation (if applicable)",
          "  - Same structure as RawData.csv"
        )

        writeLines(dict_content, file.path(docs_dir, "Data_Dictionary.txt"))

        # Create README
        readme_content <- c(
          "BRAIN NETWORK ANALYSIS - COMPLETE PACKAGE",
          "==========================================",
          "",
          paste("Generated:", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
          paste("Software: ConsensusConnectR v3.0 - Multimethod Consensus Analysis"),
          "",
          "This package contains all results from your comprehensive multimethod brain",
          "network analysis combining 5 correlation methods with 3 analytical approaches.",
          "",
          "QUICK START:",
          "",
          "1. Read Documentation/Analysis_Summary.txt for complete overview and workflow",
          "2. Browse Plots/ folder - organized by analysis section (01-09)",
          "   - Focus on 08_Consensus/ for main cross-method findings",
          "3. Check Data/ComprehensiveConsensus/ for rank-based consensus results",
          "4. Review Data/Networks_JSON/ for importable network graphs",
          "5. Explore Data/PerMethod/ for individual correlation method results",
          "6. Check Documentation/Methods_Description.txt for analytical details",
          "7. Review Documentation/Plot_Manifest.txt for complete plot listing",
          "",
          "KEY RESULTS LOCATIONS:",
          "",
          "Main Findings:",
          "  - Plots/08_Consensus/ - Cross-method consensus visualizations",
          "  - Data/ComprehensiveConsensus/ - Consensus scores and hub lists",
          "",
          "Supporting Analyses:",
          "  - Plots/01_Imputation/ - Data quality assessment",
          "  - Plots/02-05/ - Individual method results",
          "  - Plots/06_CrossMethod_Comparison/ - Pairwise comparisons",
          "  - Plots/07_Advanced_Analysis/ - Statistical validation",
          "",
          "All CSV files can be opened in Excel, R, Python, or any statistical software.",
          "JSON network files are compatible with Cytoscape, Gephi, D3.js, and vis.js.",
          "",
          "For questions or support, refer to the ConsensusConnectR documentation.",
          "",
          "Thank you for using ConsensusConnectR v3.0!"
        )

        writeLines(readme_content, file.path(temp_base, "README.txt"))

        # Create plot manifest if plots were generated
        if(exists("plot_results") && !is.null(plot_results$generated_files)) {
          manifest_content <- c(
            "PLOT MANIFEST - GENERATED VISUALIZATIONS",
            "=========================================",
            "",
            paste("Generation Date:", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
            paste("Total Plots Generated:", plot_results$successful),
            paste("Plots Skipped (no data):", plot_results$skipped),
            paste("Success Rate:", paste0(plot_results$success_rate, "%")),
            "",
            "All plots are high-resolution PNG files (300 DPI) suitable for publication.",
            "",
            "GENERATED PLOTS:",
            ""
          )

          # Add list of generated files with sizes
          for(plot_file in plot_results$generated_files) {
            if(file.exists(plot_file)) {
              file_size <- round(file.size(plot_file) / 1024, 1)  # Size in KB
              file_name <- basename(plot_file)
              manifest_content <- c(manifest_content,
                                  paste("✓", file_name, paste0("(", file_size, " KB)")))
            }
          }

          manifest_content <- c(manifest_content,
            "",
            "PLOT CATEGORIES:",
            "",
            "Data Quality:",
            "  - Imputation analysis and missing data patterns",
            "",
            "Per-Method Analysis:",
            "  - Correlation matrices for each method",
            "  - Network visualizations per method",
            "  - Node and global metrics per method",
            "",
            "Persistence Analysis:",
            "  - Hub persistence across thresholds",
            "  - Network metrics evolution",
            "",
            "Cross-Method Comparisons:",
            "  - Weighted vs. percolation comparisons",
            "  - Method agreement visualizations",
            "",
            "Consensus Analysis:",
            "  - Hub agreement matrices",
            "  - Consensus overview plots",
            "",
            "Advanced Analysis:",
            "  - MST analysis and PCA visualizations",
            "  - Conservation analysis",
            "  - Summary dashboards"
          )

          writeLines(manifest_content, file.path(docs_dir, "Plot_Manifest.txt"))
        }

        # ==========================================
        # CREATE ZIP FILE (FIXED METHOD)
        # ==========================================

        cat("📦 Creating ZIP archive...\n")

        # Ensure all files are properly closed and written
        Sys.sleep(0.5)

        # Get current directory and switch to temp directory for zipping
        current_dir <- getwd()
        on.exit(setwd(current_dir), add = TRUE)

        # Use utils::zip with proper parameters
        setwd(dirname(temp_base))
        base_name <- basename(temp_base)

        # Create list of all files to include
        all_files <- list.files(base_name, recursive = TRUE, full.names = FALSE)

        if(length(all_files) == 0) {
          stop("No files to archive - analysis may have failed")
        }

        # Create the ZIP file
        zip_result <- utils::zip(
          zipfile = file,
          files = file.path(base_name, all_files),
          flags = "-q"  # Quiet mode
        )

        if(zip_result != 0) {
          cat("⚠️ Standard ZIP failed, trying alternative method...\n")

          # Alternative: use tar.gz instead
          tar_file <- sub("\\.zip$", ".tar.gz", file)
          tar_result <- utils::tar(
            tarfile = tar_file,
            files = base_name,
            compression = "gzip"
          )

          if(tar_result == 0) {
            file.rename(tar_file, file)
            cat("✅ Created compressed archive successfully\n")
          } else {
            stop("Failed to create archive with both ZIP and TAR methods")
          }
        } else {
          cat("✅ ZIP archive created successfully\n")
        }

        # Verify the created file
        if(!file.exists(file) || file.size(file) < 1000) {
          stop("Archive file is missing or too small - creation may have failed")
        }

        cat(sprintf("📊 Final archive size: %.2f MB\n", file.size(file) / 1024^2))
        cat("✅ Download package ready!\n")

      }, error = function(e) {
        cat("❌ Error creating download package:\n")
        cat(conditionMessage(e), "\n")
        stop(paste("Download failed:", conditionMessage(e)))
      }, finally = {
        # Clean up temporary directory
        if(dir.exists(temp_base)) {
          unlink(temp_base, recursive = TRUE)
        }
      })
    },
    contentType = "application/zip"
  )

  # ==========================================
  # SUMMARY ONLY DOWNLOAD HANDLER
  # ==========================================
  output$download_summary_only <- downloadHandler(
    filename = function() {
      paste0("BrainAnalysis_Summary_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".zip")
    },
    content = function(file) {
      # Create temporary directory
      temp_base <- tempfile(pattern = "BrainAnalysis_Summary_")
      dir.create(temp_base, recursive = TRUE, showWarnings = FALSE)

      # Create subdirectories
      plots_dir <- file.path(temp_base, "Plots")
      data_dir <- file.path(temp_base, "Data")
      networks_dir <- file.path(data_dir, "Networks_JSON")

      dir.create(plots_dir, recursive = TRUE, showWarnings = FALSE)
      dir.create(data_dir, recursive = TRUE, showWarnings = FALSE)
      dir.create(networks_dir, recursive = TRUE, showWarnings = FALSE)

      tryCatch({
        cat("📊 Creating Summary-Only download package...\n")

        # Get plot customization settings
        plot_width <- as.numeric(input$download_plot_width %||% 12)
        plot_height <- as.numeric(input$download_plot_height %||% 8)
        plot_dpi <- as.numeric(input$download_plot_dpi %||% 300)
        plot_cex <- as.numeric(input$download_plot_cex %||% 1.0)

        plot_width_px <- plot_width * plot_dpi
        plot_height_px <- plot_height * plot_dpi

        cat(sprintf("📐 Plot settings: %d×%d inches @ %d DPI\n", plot_width, plot_height, plot_dpi))

        # Network options
        network_options <- list(
          use_anatomical_colors = TRUE,
          node_shape = input$network_node_shape %||% "circle",
          edge_sign_colors = !is.null(input$network_edge_sign_colors) && input$network_edge_sign_colors,
          positive_color = input$network_positive_color %||% "#2ecc71",
          negative_color = input$network_negative_color %||% "#e74c3c"
        )

        # ==========================================
        # GENERATE SUMMARY PLOTS ONLY
        # ==========================================
        cat("📊 Generating Summary plots...\n")

        successful_plots <- 0
        failed_plots <- 0

        # Define Summary-only plots
        summary_plots <- list(
          # Consensus Node Metrics (Tab A)
          "01_Consensus_Node_Metrics" = list(
            condition = function() !is.null(analysis_results$comprehensive_consensus) && !is.null(analysis_results$method_percolation_results),
            func = function() render_consensus_node_metrics_plot(analysis_results$comprehensive_consensus, analysis_results$method_percolation_results, ui_state$brain_areas, ui_state$area_colors, ui_state$group_colors)
          ),

          # Consensus Regional (Tab A)
          "02_Consensus_Regional" = list(
            condition = function() !is.null(analysis_results$comprehensive_consensus) && !is.null(ui_state$brain_areas),
            func = function() render_consensus_regional_plot(analysis_results$comprehensive_consensus, ui_state$brain_areas, ui_state$area_colors, ui_state$group_colors)
          ),

          # Consensus Subregional (Tab A)
          "03_Consensus_Subregional" = list(
            condition = function() !is.null(analysis_results$comprehensive_consensus) && !is.null(ui_state$brain_areas),
            func = function() render_consensus_subregional_plot(analysis_results$comprehensive_consensus, ui_state$brain_areas, ui_state$area_colors, ui_state$group_colors)
          ),

          # Consensus Top Hubs (Tab A)
          "04_Consensus_Top_Hubs" = list(
            condition = function() !is.null(analysis_results$comprehensive_consensus),
            func = function() render_consensus_overview_top_hubs_plot(analysis_results$comprehensive_consensus, ui_state$brain_areas, ui_state$area_colors, ui_state$group_colors)
          ),

          # Consensus Agreement (Tab A)
          "05_Consensus_Agreement" = list(
            condition = function() !is.null(analysis_results$comprehensive_consensus),
            func = function() render_consensus_overview_agreement_plot(analysis_results$comprehensive_consensus, ui_state$group_colors)
          ),

          # Network Similarity Heatmap (Tab C)
          "06_Network_Similarity_Heatmap" = list(
            condition = function() !is.null(analysis_results$correlation_methods_raw) && !is.null(analysis_results$method_percolation_results),
            func = function() render_network_similarity_heatmap_plot(analysis_results$correlation_methods_raw, analysis_results$method_percolation_results, analysis_results$persistence_results)
          ),

          # Conservation Heatmap (Tab C)
          "07_Conservation_Heatmap" = list(
            condition = function() !is.null(analysis_results$conservation_analysis),
            func = function() render_network_similarity_heatmap(analysis_results$conservation_analysis)
          ),

          # Three-Way Eigenvector Comparison
          "08_Three_Way_Eigenvector" = list(
            condition = function() !is.null(analysis_results$method_weighted_results) && !is.null(analysis_results$method_percolation_results),
            func = function() render_consensus_three_way_eigenvector(analysis_results$method_weighted_results, analysis_results$method_percolation_results, analysis_results$persistence_results, ui_state$group_colors)
          ),

          # Hub Ranking
          "09_Hub_Ranking" = list(
            condition = function() !is.null(analysis_results$consensus_hub_results),
            func = function() render_consensus_hub_ranking(analysis_results$consensus_hub_results, ui_state$group_colors)
          ),

          # Consensus Heatmap
          "10_Consensus_Heatmap" = list(
            condition = function() !is.null(analysis_results$consensus_hub_results),
            func = function() render_consensus_heatmap(analysis_results$consensus_hub_results, ui_state$group_colors)
          )
        )

        # Generate each summary plot
        for(plot_name in names(summary_plots)) {
          plot_info <- summary_plots[[plot_name]]

          if(plot_info$condition()) {
            tryCatch({
              filename <- paste0(plot_name, ".png")
              png(file.path(plots_dir, filename), width = plot_width_px, height = plot_height_px, res = plot_dpi)

              par(cex = plot_cex, cex.main = plot_cex * 1.2, cex.lab = plot_cex, cex.axis = plot_cex * 0.9)

              plot_info$func()

              dev.off()
              successful_plots <- successful_plots + 1
              cat(sprintf("✅ Generated: %s\n", filename))

            }, error = function(e) {
              if(dev.cur() > 1) dev.off()
              failed_plots <- failed_plots + 1
              cat(sprintf("❌ Failed: %s - %s\n", plot_name, e$message))
            })
          } else {
            cat(sprintf("⏭️  Skipped: %s (data not available)\n", plot_name))
          }
        }

        # ==========================================
        # GENERATE NETWORK PLOTS (with brain area colors)
        # ==========================================
        if(!is.null(analysis_results$networks) && length(analysis_results$networks) > 0) {
          cat("\n🕸️  Generating network plots...\n")

          for(i in seq_along(analysis_results$networks)) {
            network <- analysis_results$networks[[i]]
            group_name <- names(analysis_results$networks)[i]
            plot_name <- paste0("Network_", gsub("[^a-zA-Z0-9]", "_", group_name))

            tryCatch({
              filename <- paste0(plot_name, ".png")
              png(file.path(plots_dir, filename), width = plot_width_px, height = plot_height_px, res = plot_dpi)

              par(mfrow=c(1,1), mar=c(1,1,3,1))

              if(requireNamespace("igraph", quietly = TRUE)) {
                node_names <- igraph::V(network)$name

                # Always use brain area colors
                if(!is.null(ui_state$brain_areas) && length(ui_state$brain_areas) > 0) {
                  node_colors <- rep("#808080", length(node_names))
                  names(node_colors) <- node_names

                  for(area_name in names(ui_state$brain_areas)) {
                    regions <- ui_state$brain_areas[[area_name]]
                    matching_nodes <- intersect(regions, node_names)

                    if(length(matching_nodes) > 0) {
                      if(!is.null(ui_state$area_colors) && area_name %in% names(ui_state$area_colors)) {
                        area_color <- ui_state$area_colors[[area_name]]
                      } else {
                        default_colors <- c("#3498db", "#e74c3c", "#27ae60", "#9b59b6", "#e67e22", "#1abc9c", "#f39c12")
                        color_idx <- ((match(area_name, names(ui_state$brain_areas)) - 1) %% length(default_colors)) + 1
                        area_color <- default_colors[color_idx]
                      }
                      node_colors[matching_nodes] <- area_color
                    }
                  }
                  vertex_colors <- as.character(node_colors)
                } else {
                  vertex_colors <- "#1F78B4"
                }

                # Node sizes based on degree
                node_degrees <- igraph::degree(network)
                if(max(node_degrees) > min(node_degrees)) {
                  vertex_sizes <- scales::rescale(node_degrees, to = c(5, 15))
                } else {
                  vertex_sizes <- 8
                }

                # Group color for border
                group_color <- if(!is.null(ui_state$group_colors) && group_name %in% names(ui_state$group_colors)) {
                  ui_state$group_colors[[group_name]]
                } else {
                  "black"
                }

                plot(network,
                     main=paste("Group:", group_name),
                     vertex.size=vertex_sizes,
                     vertex.color=vertex_colors,
                     vertex.frame.color="white",
                     vertex.label.cex=0.7,
                     vertex.label.color="black",
                     vertex.label.dist=1,
                     edge.color=adjustcolor("gray60", alpha.f = 0.7),
                     edge.width=abs(igraph::E(network)$weight) * 3,
                     layout=igraph::layout_with_fr(network))

                box(col = group_color, lwd = 3)
              }

              dev.off()
              successful_plots <- successful_plots + 1
              cat(sprintf("✅ Generated: %s\n", filename))

            }, error = function(e) {
              if(dev.cur() > 1) dev.off()
              failed_plots <- failed_plots + 1
              cat(sprintf("❌ Failed: %s - %s\n", plot_name, e$message))
            })
          }
        }

        cat(sprintf("\n📊 Summary plots: %d generated, %d failed\n", successful_plots, failed_plots))

        # ==========================================
        # EXPORT SUMMARY DATA
        # ==========================================
        cat("\n💾 Saving Summary data...\n")

        # Comprehensive consensus data
        if(!is.null(analysis_results$comprehensive_consensus)) {
          for(group_name in names(analysis_results$comprehensive_consensus)) {
            group_data <- analysis_results$comprehensive_consensus[[group_name]]
            safe_group <- gsub("[^a-zA-Z0-9]", "_", group_name)

            if(!is.null(group_data$consensus_scores) && nrow(group_data$consensus_scores) > 0) {
              filename <- paste0("ConsensusScores_", safe_group, ".csv")
              write.csv(group_data$consensus_scores,
                       file.path(data_dir, filename),
                       row.names = FALSE)
              cat(sprintf("✅ Saved: %s\n", filename))
            }
          }
        }

        # Network similarity data
        if(!is.null(analysis_results$network_similarity)) {
          if(!is.null(analysis_results$network_similarity$cross_method_average)) {
            write.csv(analysis_results$network_similarity$cross_method_average,
                     file.path(data_dir, "NetworkSimilarity_CrossMethod.csv"),
                     row.names = FALSE)
            cat("✅ Saved: NetworkSimilarity_CrossMethod.csv\n")
          }
        }

        # Aggregated metrics
        if(!is.null(analysis_results$aggregated_metrics)) {
          for(group_name in names(analysis_results$aggregated_metrics)) {
            group_data <- analysis_results$aggregated_metrics[[group_name]]
            safe_group <- gsub("[^a-zA-Z0-9]", "_", group_name)

            if(!is.null(group_data) && nrow(group_data) > 0) {
              filename <- paste0("AggregatedMetrics_", safe_group, ".csv")
              write.csv(group_data,
                       file.path(data_dir, filename),
                       row.names = FALSE)
            }
          }
        }

        # Export network JSON files
        cat("\n🕸️  Exporting network graphs as JSON...\n")
        network_count <- export_networks_json(analysis_results, networks_dir)
        cat(sprintf("✅ Exported %d network graphs\n", network_count))

        # ==========================================
        # CREATE README
        # ==========================================
        readme_content <- c(
          "BRAIN NETWORK ANALYSIS - SUMMARY PACKAGE",
          "=========================================",
          "",
          paste("Generated:", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
          paste("Software: ConsensusConnectR v3.0"),
          "",
          "This package contains the key Summary findings from your analysis.",
          "",
          "CONTENTS:",
          "",
          "📁 Plots/",
          "   - Consensus node metrics and rankings",
          "   - Regional and subregional analysis",
          "   - Network similarity heatmaps",
          "   - Network visualizations with brain area colors",
          "",
          "📁 Data/",
          "   - ConsensusScores_[Group].csv - Rank-based consensus scores",
          "   - NetworkSimilarity_CrossMethod.csv - Method agreement metrics",
          "   - AggregatedMetrics_[Group].csv - Summary statistics",
          "",
          "📁 Data/Networks_JSON/",
          "   - Network graphs in Cytoscape.js format",
          "   - Import into Cytoscape via File > Import > Network from File",
          "",
          "For complete analysis results, use 'Download Complete Package'."
        )

        writeLines(readme_content, file.path(temp_base, "README.txt"))

        # ==========================================
        # CREATE ZIP FILE
        # ==========================================
        cat("\n📦 Creating ZIP archive...\n")

        Sys.sleep(0.3)

        current_dir <- getwd()
        on.exit(setwd(current_dir), add = TRUE)

        setwd(dirname(temp_base))
        base_name <- basename(temp_base)

        all_files <- list.files(base_name, recursive = TRUE, full.names = FALSE)

        if(length(all_files) == 0) {
          stop("No files to archive")
        }

        zip_result <- utils::zip(
          zipfile = file,
          files = file.path(base_name, all_files),
          flags = "-q"
        )

        if(zip_result != 0) {
          stop("Failed to create ZIP archive")
        }

        cat(sprintf("📊 Summary package size: %.2f MB\n", file.size(file) / 1024^2))
        cat("✅ Summary download ready!\n")

      }, error = function(e) {
        cat("❌ Error creating summary package:\n")
        cat(conditionMessage(e), "\n")
        stop(paste("Download failed:", conditionMessage(e)))
      }, finally = {
        if(dir.exists(temp_base)) {
          unlink(temp_base, recursive = TRUE)
        }
      })
    },
    contentType = "application/zip"
  )
}
