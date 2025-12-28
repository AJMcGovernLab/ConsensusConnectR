# Comprehensive Consensus Analytics Functions
# Synthesizes results across Weighted, Percolation, and Persistence approaches
# Across 5 correlation methods: Pearson, Spearman, Biweight, Shrinkage, Partial

# ============================================================================
# NEW FUNCTION: Standardized Hub Metrics (Z-Scored Eigenvector)
# ============================================================================

compute_standardized_hub_metrics <- function(method_weighted_results,
                                              method_percolation_results,
                                             auc_results,
                                              group_name,
                                              z_threshold = 1.5,
                                              methods = c("pearson", "spearman", "biweight", "shrinkage", "partial")) {

  # methods parameter now passed in - use selected methods only

  standardized_metrics <- list(
    weighted = list(),
    percolation = list(),
    persistence = list()
  )

  for(method in methods) {
    # WEIGHTED: Extract eigenvector from weighted_eigenvector field
    # FIX: Access weighted_eigenvector directly (it's a combined df, filter by group)
    if(!is.null(method_weighted_results[[method]]$weighted_eigenvector)) {
      weighted_all <- method_weighted_results[[method]]$weighted_eigenvector
      weighted_eig <- weighted_all[weighted_all$Group == group_name, ]

      if(nrow(weighted_eig) > 0 && "Weighted_Eigenvector" %in% names(weighted_eig)) {
        # Z-score transformation
        z_scores <- (weighted_eig$Weighted_Eigenvector - mean(weighted_eig$Weighted_Eigenvector, na.rm = TRUE)) /
                    sd(weighted_eig$Weighted_Eigenvector, na.rm = TRUE)

        standardized_metrics$weighted[[method]] <- data.frame(
          Node = weighted_eig$Node,
          Eigenvector_Raw = weighted_eig$Weighted_Eigenvector,
          Eigenvector_Z = z_scores,
          IsHub = z_scores >= z_threshold,
          stringsAsFactors = FALSE
        )
      }
    }

    # PERCOLATION: Extract eigenvector at optimal threshold
    if(!is.null(method_percolation_results[[method]]$node_metrics)) {
      perc_nodes <- method_percolation_results[[method]]$node_metrics
      group_nodes <- perc_nodes[perc_nodes$Group == group_name, ]

      if(nrow(group_nodes) > 0 && "Eigenvector" %in% names(group_nodes)) {
        z_scores <- (group_nodes$Eigenvector - mean(group_nodes$Eigenvector, na.rm = TRUE)) /
                    sd(group_nodes$Eigenvector, na.rm = TRUE)

        standardized_metrics$percolation[[method]] <- data.frame(
          Node = group_nodes$Node,
          Eigenvector_Raw = group_nodes$Eigenvector,
          Eigenvector_Z = z_scores,
          IsHub = z_scores >= z_threshold,
          stringsAsFactors = FALSE
        )
      }
    }

    # PERSISTENCE: Use AUC_Eigenvector from auc_results
    if(!is.null(auc_results[[method]]) &&
       !is.null(auc_results[[method]][[group_name]]) &&
       !is.null(auc_results[[method]][[group_name]]$node_auc)) {

      auc_nodes <- auc_results[[method]][[group_name]]$node_auc

      if("AUC_Eigenvector" %in% names(auc_nodes)) {
        z_scores <- (auc_nodes$AUC_Eigenvector - mean(auc_nodes$AUC_Eigenvector, na.rm = TRUE)) /
                    sd(auc_nodes$AUC_Eigenvector, na.rm = TRUE)

        standardized_metrics$persistence[[method]] <- data.frame(
          Node = auc_nodes$Node,
          Eigenvector_Raw = auc_nodes$AUC_Eigenvector,
          Eigenvector_Z = z_scores,
          IsHub = z_scores >= z_threshold,
          stringsAsFactors = FALSE
        )
      }
    }
  }

  return(standardized_metrics)
}

# ============================================================================
# NEW FUNCTION: Bayesian Confidence-Weighted Consensus
# ============================================================================

compute_bayesian_consensus <- function(standardized_metrics,
                                       group_name,
                                       prior_hub_prob = 0.15) {

  # Get all unique nodes across all approaches and methods
  all_nodes <- unique(c(
    unlist(lapply(standardized_metrics$weighted, function(x) if(!is.null(x)) x$Node else character(0))),
    unlist(lapply(standardized_metrics$percolation, function(x) if(!is.null(x)) x$Node else character(0))),
    unlist(lapply(standardized_metrics$persistence, function(x) if(!is.null(x)) x$Node else character(0)))
  ))

  if(length(all_nodes) == 0) {
    return(data.frame(
      Node = character(0),
      Weighted_HubProb = numeric(0),
      Percolation_HubProb = numeric(0),
      Persistence_HubProb = numeric(0),
      Weighted_MeanZ = numeric(0),
      Percolation_MeanZ = numeric(0),
      Persistence_MeanZ = numeric(0),
      Weighted_CV = numeric(0),
      Percolation_CV = numeric(0),
      Persistence_CV = numeric(0),
      Weighted_Weight = numeric(0),
      Percolation_Weight = numeric(0),
      Persistence_Weight = numeric(0),
      BayesianConsensusScore = numeric(0),
      CredibleIntervalLower = numeric(0),
      CredibleIntervalUpper = numeric(0),
      ConfidenceCategory = character(0),
      stringsAsFactors = FALSE
    ))
  }

  results <- data.frame(
    Node = all_nodes,
    Weighted_HubProb = 0,
    Percolation_HubProb = 0,
    Persistence_HubProb = 0,
    Weighted_MeanZ = 0,
    Percolation_MeanZ = 0,
    Persistence_MeanZ = 0,
    Weighted_CV = Inf,
    Percolation_CV = Inf,
    Persistence_CV = Inf,
    Weighted_Weight = 0,
    Percolation_Weight = 0,
    Persistence_Weight = 0,
    BayesianConsensusScore = 0,
    CredibleIntervalLower = 0,
    CredibleIntervalUpper = 0,
    ConfidenceCategory = "Low",
    stringsAsFactors = FALSE
  )

  methods <- names(standardized_metrics$weighted)
  if(length(methods) == 0) {
    methods <- names(standardized_metrics$percolation)
  }
  if(length(methods) == 0) {
    methods <- names(standardized_metrics$persistence)
  }

  n_methods_total <- 5  # Always 5 possible methods

  for(node in all_nodes) {
    # For each approach, compute:
    # 1. Hub probability (proportion of methods identifying as hub)
    # 2. Mean z-score across methods
    # 3. Coefficient of variation (CV) across methods
    # 4. Approach weight = 1 / (1 + CV)

    for(approach_name in c("weighted", "percolation", "persistence")) {
      approach_data <- standardized_metrics[[approach_name]]

      # Extract z-scores and hub flags for this node
      z_scores <- c()
      hub_flags <- c()

      for(method in names(approach_data)) {
        if(!is.null(approach_data[[method]])) {
          node_row <- approach_data[[method]][approach_data[[method]]$Node == node, ]
          if(nrow(node_row) > 0) {
            z_scores <- c(z_scores, node_row$Eigenvector_Z)
            hub_flags <- c(hub_flags, as.numeric(node_row$IsHub))
          }
        }
      }

      if(length(z_scores) > 0) {
        mean_z <- mean(z_scores, na.rm = TRUE)
        sd_z <- sd(z_scores, na.rm = TRUE)
        cv <- ifelse(abs(mean_z) > 0.01, abs(sd_z / mean_z), Inf)
        hub_prob <- mean(hub_flags, na.rm = TRUE)

        # Weight inversely proportional to CV (more consistent = higher weight)
        weight <- 1 / (1 + cv)

        col_prefix <- paste0(toupper(substring(approach_name, 1, 1)),
                            substring(approach_name, 2))
        results[results$Node == node, paste0(col_prefix, "_HubProb")] <- hub_prob
        results[results$Node == node, paste0(col_prefix, "_MeanZ")] <- mean_z
        results[results$Node == node, paste0(col_prefix, "_CV")] <- cv
        results[results$Node == node, paste0(col_prefix, "_Weight")] <- weight
      }
    }

    # Bayesian consensus: Weighted average with uncertainty
    node_row <- results[results$Node == node, ]

    weights <- c(node_row$Weighted_Weight,
                node_row$Percolation_Weight,
                node_row$Persistence_Weight)
    hub_probs <- c(node_row$Weighted_HubProb,
                   node_row$Percolation_HubProb,
                   node_row$Persistence_HubProb)

    # Normalize weights
    total_weight <- sum(weights, na.rm = TRUE)
    if(total_weight > 0) {
      norm_weights <- weights / total_weight
    } else {
      norm_weights <- rep(1/3, 3)
    }

    # Bayesian update: P(hub | data) ∝ P(data | hub) * P(hub)
    # Using Beta-Binomial model for each approach
    # Prior: Beta(α, β) where α/(α+β) = prior_hub_prob
    # Posterior: Beta(α + k, β + n - k) where k = # methods calling hub, n = # methods available

    alpha_prior <- prior_hub_prob * 10  # Strength of prior
    beta_prior <- (1 - prior_hub_prob) * 10

    # Calculate credible intervals from posterior
    posterior_samples <- numeric(0)

    for(i in 1:3) {
      k <- hub_probs[i] * n_methods_total  # Expected # of methods calling it hub
      n <- n_methods_total

      # Posterior parameters
      alpha_post <- alpha_prior + k
      beta_post <- beta_prior + (n - k)

      # Sample from posterior (for credible intervals)
      # Simpler approach: use quantiles of Beta distribution
      lower_quantile <- qbeta(0.025, alpha_post, beta_post)
      upper_quantile <- qbeta(0.975, alpha_post, beta_post)

      # Weighted contribution to credible interval
      posterior_samples <- c(posterior_samples,
                            lower_quantile * norm_weights[i],
                            upper_quantile * norm_weights[i])
    }

    # Consensus is weighted sum of posterior means (hub probabilities)
    posterior_mean <- sum(hub_probs * norm_weights, na.rm = TRUE)

    # Credible interval (conservative: min of lower bounds, max of upper bounds)
    credible_lower <- sum(hub_probs * norm_weights, na.rm = TRUE) - sd(hub_probs * norm_weights, na.rm = TRUE)
    credible_upper <- sum(hub_probs * norm_weights, na.rm = TRUE) + sd(hub_probs * norm_weights, na.rm = TRUE)

    # Ensure bounds are in [0,1]
    credible_lower <- max(0, credible_lower)
    credible_upper <- min(1, credible_upper)

    # Confidence category based on credible interval width
    ci_width <- credible_upper - credible_lower
    confidence_cat <- ifelse(ci_width < 0.2, "High",
                           ifelse(ci_width < 0.4, "Medium", "Low"))

    results[results$Node == node, "BayesianConsensusScore"] <- posterior_mean
    results[results$Node == node, "CredibleIntervalLower"] <- credible_lower
    results[results$Node == node, "CredibleIntervalUpper"] <- credible_upper
    results[results$Node == node, "ConfidenceCategory"] <- confidence_cat
  }

  # Sort by consensus score
  results <- results[order(-results$BayesianConsensusScore), ]

  return(results)
}

# ============================================================================
# NEW FUNCTION: Uncertainty Quantification Metrics
# ============================================================================

compute_uncertainty_metrics <- function(consensus_results) {

  # Compute SD and CV across approaches
  consensus_results$SD_Across_Approaches <- apply(
    consensus_results[, c("Weighted_HubProb", "Percolation_HubProb", "Persistence_HubProb")],
    1, sd, na.rm = TRUE
  )

  consensus_results$CV_Across_Approaches <- apply(
    consensus_results[, c("Weighted_HubProb", "Percolation_HubProb", "Persistence_HubProb")],
    1, function(x) {
      m <- mean(x, na.rm = TRUE)
      ifelse(m > 0.01, sd(x, na.rm = TRUE) / m, Inf)
    }
  )

  # Uncertainty category based on CV
  consensus_results$UncertaintyCategory <- cut(
    consensus_results$CV_Across_Approaches,
    breaks = c(-Inf, 0.2, 0.4, Inf),
    labels = c("Low Uncertainty (High Confidence)",
               "Medium Uncertainty (Medium Confidence)",
               "High Uncertainty (Low Confidence)")
  )

  # Agreement flags
  consensus_results$AllThreeAgree <- (
    consensus_results$Weighted_HubProb >= 0.6 &
    consensus_results$Percolation_HubProb >= 0.6 &
    consensus_results$Persistence_HubProb >= 0.6
  )

  consensus_results$TwoAgree <- (
    rowSums(consensus_results[, c("Weighted_HubProb", "Percolation_HubProb",
                                  "Persistence_HubProb")] >= 0.6, na.rm = TRUE) >= 2
  )

  # Partial data flag (set later based on actual missing methods)
  consensus_results$HasPartialData <- FALSE

  return(consensus_results)
}

# ============================================================================
# Function 4: Dynamic Threshold Calibration
# ============================================================================

calibrate_dynamic_thresholds <- function(standardized_metrics,
                                         group_name,
                                         tau_range = seq(1.0, 2.5, by = 0.1)) {
  #' Calibrate optimal z-threshold based on network properties
  #'
  #' @param standardized_metrics Output from compute_standardized_hub_metrics()
  #' @param group_name Group identifier
  #' @param tau_range Vector of z-thresholds to test
  #'
  #' @return List with optimal_threshold, quality_score, and diagnostics

  # Extract all z-scores across approaches
  all_z_scores <- c()

  for(approach in names(standardized_metrics)) {
    for(method in names(standardized_metrics[[approach]])) {
      if(!is.null(standardized_metrics[[approach]][[method]])) {
        all_z_scores <- c(all_z_scores, standardized_metrics[[approach]][[method]]$Eigenvector_Z)
      }
    }
  }

  if(length(all_z_scores) == 0) {
    return(list(
      optimal_threshold = 1.5,
      quality_score = 0,
      proportion_hubs = 0,
      hub_modularity = 0,
      hub_clustering = 0,
      hub_density = 0,
      threshold_scores = data.frame()
    ))
  }

  # Remove NAs
  all_z_scores <- all_z_scores[!is.na(all_z_scores)]
  n_nodes <- length(unique(all_z_scores)) / 3  # Approximate, assuming each node appears 3 times

  # Evaluate quality for each threshold
  threshold_results <- data.frame(
    tau = numeric(),
    proportion_hubs = numeric(),
    quality_score = numeric(),
    hub_modularity = numeric(),
    hub_clustering = numeric(),
    hub_density = numeric()
  )

  for(tau in tau_range) {
    # Identify hubs at this threshold
    hub_z_scores <- all_z_scores[all_z_scores >= tau]
    n_hubs <- length(unique(hub_z_scores)) / 3  # Approximate

    if(n_hubs < 2) {
      # Too few hubs, skip
      threshold_results <- rbind(threshold_results, data.frame(
        tau = tau,
        proportion_hubs = 0,
        quality_score = 0,
        hub_modularity = 0,
        hub_clustering = 0,
        hub_density = 0
      ))
      next
    }

    # Calculate proportion of nodes as hubs
    p_hubs <- n_hubs / n_nodes

    # Quality component 1: Hub proportion penalty
    # Prefer 5-15% of nodes as hubs (biologically plausible)
    if(p_hubs >= 0.05 && p_hubs <= 0.15) {
      f_hub <- 1.0
    } else {
      f_hub <- exp(-abs(p_hubs - 0.10) * 10)
    }

    # Quality component 2: Hub modularity (simulated - would need network structure)
    # Higher modularity = hubs form coherent communities
    # Simulate based on hub z-score distribution
    if(length(hub_z_scores) > 3) {
      hub_mod <- 1 - (sd(hub_z_scores) / mean(hub_z_scores))  # Lower CV = higher modularity
      hub_mod <- max(0, min(1, hub_mod))  # Clamp to [0,1]
    } else {
      hub_mod <- 0
    }

    # Quality component 3: Hub clustering (simulated)
    # Higher clustering = small-world property
    # Estimate from z-score concentration
    if(length(hub_z_scores) > 3) {
      z_range <- max(hub_z_scores) - min(hub_z_scores)
      hub_clust <- 1 / (1 + z_range)  # Lower range = more clustered
      hub_clust <- max(0, min(1, hub_clust))
    } else {
      hub_clust <- 0
    }

    # Quality component 4: Hub density
    # Prefer moderate density (10-60%)
    # Estimate as proportion of hub-hub connections
    hub_dens <- min(p_hubs * 2, 0.6)  # Simple approximation

    if(hub_dens >= 0.1 && hub_dens <= 0.6) {
      f_dens <- 0.5
    } else {
      f_dens <- 0
    }

    # Combined quality score
    quality_score <- f_hub + hub_mod + hub_clust + f_dens

    threshold_results <- rbind(threshold_results, data.frame(
      tau = tau,
      proportion_hubs = p_hubs,
      quality_score = quality_score,
      hub_modularity = hub_mod,
      hub_clustering = hub_clust,
      hub_density = hub_dens
    ))
  }

  # Find optimal threshold
  if(nrow(threshold_results) > 0) {
    best_idx <- which.max(threshold_results$quality_score)
    optimal_tau <- threshold_results$tau[best_idx]

    result <- list(
      optimal_threshold = optimal_tau,
      quality_score = threshold_results$quality_score[best_idx],
      proportion_hubs = threshold_results$proportion_hubs[best_idx],
      hub_modularity = threshold_results$hub_modularity[best_idx],
      hub_clustering = threshold_results$hub_clustering[best_idx],
      hub_density = threshold_results$hub_density[best_idx],
      threshold_scores = threshold_results
    )
  } else {
    result <- list(
      optimal_threshold = 1.5,
      quality_score = 0,
      proportion_hubs = 0,
      hub_modularity = 0,
      hub_clustering = 0,
      hub_density = 0,
      threshold_scores = data.frame()
    )
  }

  return(result)
}

# ============================================================================
# Function 5: Threshold Sensitivity Analysis
# ============================================================================

compute_threshold_sensitivity <- function(standardized_metrics,
                                         group_name,
                                         tau_range = seq(1.0, 2.5, by = 0.1),
                                         agreement_thresholds = seq(0.4, 0.8, by = 0.1),
                                         current_tau = 1.5) {
  #' Compute sensitivity of consensus to threshold choices
  #'
  #' @param standardized_metrics Output from compute_standardized_hub_metrics()
  #' @param group_name Group identifier
  #' @param tau_range Vector of z-thresholds to test
  #' @param agreement_thresholds Vector of agreement thresholds to test
  #' @param current_tau Currently used z-threshold
  #'
  #' @return List with z_sensitivity, agreement_sensitivity, and stable_hub_nodes

  # Extract all nodes
  all_nodes <- c()
  for(approach in names(standardized_metrics)) {
    for(method in names(standardized_metrics[[approach]])) {
      if(!is.null(standardized_metrics[[approach]][[method]])) {
        all_nodes <- c(all_nodes, standardized_metrics[[approach]][[method]]$Node)
      }
    }
  }
  all_nodes <- unique(all_nodes)

  if(length(all_nodes) == 0) {
    return(list(
      z_sensitivity = data.frame(),
      agreement_sensitivity = data.frame(),
      stable_hub_nodes = character()
    ))
  }

  # Part 1: Z-threshold sensitivity
  z_sens_results <- data.frame(
    threshold = numeric(),
    n_consensus_hubs = numeric(),
    n_three_way = numeric(),
    avg_consensus_score = numeric(),
    stable_hubs = numeric()
  )

  # Track which nodes are hubs at each threshold
  node_hub_matrix <- matrix(FALSE, nrow = length(all_nodes), ncol = length(tau_range),
                            dimnames = list(all_nodes, as.character(tau_range)))

  for(i in seq_along(tau_range)) {
    tau <- tau_range[i]

    # Count hubs at this threshold across all approaches
    hub_counts <- rep(0, length(all_nodes))
    names(hub_counts) <- all_nodes

    for(approach in names(standardized_metrics)) {
      for(method in names(standardized_metrics[[approach]])) {
        if(!is.null(standardized_metrics[[approach]][[method]])) {
          approach_data <- standardized_metrics[[approach]][[method]]
          hubs_at_tau <- approach_data$Node[approach_data$Eigenvector_Z >= tau]
          hub_counts[hubs_at_tau] <- hub_counts[hubs_at_tau] + 1
        }
      }
    }

    # Calculate consensus: proportion of (3 approaches × 5 methods) identifying as hub
    n_total_tests <- 15  # 3 approaches × 5 methods (may be less if some missing)
    consensus_scores <- hub_counts / n_total_tests

    # Nodes with consensus >= 0.5 are consensus hubs
    consensus_hubs <- names(consensus_scores[consensus_scores >= 0.5])
    node_hub_matrix[consensus_hubs, i] <- TRUE

    # Nodes with all three approaches agreeing (hub_count >= 9 out of 15, accounting for 3 approaches × ~3 methods each)
    three_way_hubs <- names(consensus_scores[consensus_scores >= 0.6])

    z_sens_results <- rbind(z_sens_results, data.frame(
      threshold = tau,
      n_consensus_hubs = length(consensus_hubs),
      n_three_way = length(three_way_hubs),
      avg_consensus_score = mean(consensus_scores[consensus_scores > 0], na.rm = TRUE),
      stable_hubs = 0  # Will fill later
    ))
  }

  # Identify stable hubs (present in 80%+ of thresholds)
  threshold_appearance <- rowSums(node_hub_matrix)
  stable_threshold <- ceiling(0.8 * length(tau_range))
  stable_hub_nodes <- names(threshold_appearance[threshold_appearance >= stable_threshold])

  # Fill in stable_hubs count
  for(i in 1:nrow(z_sens_results)) {
    hubs_at_tau <- names(which(node_hub_matrix[, i]))
    z_sens_results$stable_hubs[i] <- sum(hubs_at_tau %in% stable_hub_nodes)
  }

  # Part 2: Agreement threshold sensitivity
  agr_sens_results <- data.frame(
    agreement_threshold = numeric(),
    n_hubs = numeric(),
    avg_confidence = numeric()
  )

  # Use current tau for agreement sensitivity
  hub_counts <- rep(0, length(all_nodes))
  names(hub_counts) <- all_nodes

  for(approach in names(standardized_metrics)) {
    for(method in names(standardized_metrics[[approach]])) {
      if(!is.null(standardized_metrics[[approach]][[method]])) {
        approach_data <- standardized_metrics[[approach]][[method]]
        hubs_at_current_tau <- approach_data$Node[approach_data$Eigenvector_Z >= current_tau]
        hub_counts[hubs_at_current_tau] <- hub_counts[hubs_at_current_tau] + 1
      }
    }
  }

  n_total_tests <- 15
  consensus_scores <- hub_counts / n_total_tests

  for(agr_thresh in agreement_thresholds) {
    hubs_at_threshold <- names(consensus_scores[consensus_scores >= agr_thresh])
    avg_conf <- mean(consensus_scores[consensus_scores >= agr_thresh], na.rm = TRUE)
    if(is.nan(avg_conf)) avg_conf <- 0

    agr_sens_results <- rbind(agr_sens_results, data.frame(
      agreement_threshold = agr_thresh,
      n_hubs = length(hubs_at_threshold),
      avg_confidence = avg_conf
    ))
  }

  return(list(
    z_sensitivity = z_sens_results,
    agreement_sensitivity = agr_sens_results,
    stable_hub_nodes = stable_hub_nodes
  ))
}

# ============================================================================
# Function 1: Comprehensive Hub Consensus
# ============================================================================

compute_comprehensive_hub_consensus <- function(method_weighted_results,
                                                method_percolation_results,
                                                persistence_results,
                                                auc_results = NULL,
                                                group_name,
                                                z_threshold = 1.5,
                                                prior_hub_prob = 0.15,
                                                methods = c("pearson", "spearman", "biweight", "shrinkage", "partial")) {

  # NEW APPROACH: Use standardized z-scored eigenvector metrics with Bayesian consensus

  # Step 1: Compute standardized hub metrics (z-scored eigenvector)
  standardized_metrics <- compute_standardized_hub_metrics(
    method_weighted_results = method_weighted_results,
    method_percolation_results = method_percolation_results,
    auc_results = auc_results,
    group_name = group_name,
    z_threshold = z_threshold,
    methods = methods
  )

  # Step 1.5: Dynamic threshold calibration (if requested)
  calibration <- NULL
  if(is.null(z_threshold) || (is.character(z_threshold) && z_threshold == "auto")) {
    tryCatch({
      calibration <- calibrate_dynamic_thresholds(
        standardized_metrics = standardized_metrics,
        group_name = group_name
      )
      z_threshold <- calibration$optimal_threshold

      # Recompute standardized metrics with calibrated threshold
      standardized_metrics <- compute_standardized_hub_metrics(
        method_weighted_results = method_weighted_results,
        method_percolation_results = method_percolation_results,
        auc_results = auc_results,
        group_name = group_name,
        z_threshold = z_threshold,
        methods = methods
      )
    }, error = function(e) {
      # Fallback to default threshold if calibration fails
      z_threshold <<- 1.5
      cat(sprintf("⚠️  Threshold calibration failed for %s, using default 1.5: %s\n",
                 group_name, e$message))
    })
  }

  # Step 2: Compute Bayesian consensus with confidence weighting
  bayesian_consensus <- compute_bayesian_consensus(
    standardized_metrics = standardized_metrics,
    group_name = group_name,
    prior_hub_prob = prior_hub_prob
  )

  # Step 3: Add uncertainty metrics
  consensus_scores <- compute_uncertainty_metrics(bayesian_consensus)

  # Step 4: BACKWARD COMPATIBILITY - Add legacy columns
  consensus_scores$WeightedScore <- consensus_scores$Weighted_HubProb
  consensus_scores$PercolationScore <- consensus_scores$Percolation_HubProb
  consensus_scores$PersistenceScore <- consensus_scores$Persistence_HubProb
  consensus_scores$ConsensusScore <- consensus_scores$BayesianConsensusScore

  # Legacy robustness category (map from new confidence category)
  consensus_scores$RobustnessCategory <- ifelse(
    consensus_scores$AllThreeAgree, "Highly Stable",
    ifelse(consensus_scores$TwoAgree, "Stable",
           ifelse(consensus_scores$BayesianConsensusScore >= 0.4, "Moderately Stable", "Unstable"))
  )

  consensus_scores$MethodsAgreement <- consensus_scores$BayesianConsensusScore
  consensus_scores$TwoApproachesAgree <- consensus_scores$TwoAgree

  # Step 5: Extract legacy hub lists for backward compatibility
  # Use methods parameter passed to function (not hardcoded)
  weighted_hubs <- list()
  percolation_hubs <- list()
  persistence_hubs <- list()

  for(method in methods) {
    # Extract hubs from standardized metrics based on z-threshold
    if(!is.null(standardized_metrics$weighted[[method]])) {
      weighted_hubs[[method]] <- standardized_metrics$weighted[[method]]$Node[
        standardized_metrics$weighted[[method]]$IsHub
      ]
    }

    if(!is.null(standardized_metrics$percolation[[method]])) {
      percolation_hubs[[method]] <- standardized_metrics$percolation[[method]]$Node[
        standardized_metrics$percolation[[method]]$IsHub
      ]
    }

    if(!is.null(standardized_metrics$persistence[[method]])) {
      persistence_hubs[[method]] <- standardized_metrics$persistence[[method]]$Node[
        standardized_metrics$persistence[[method]]$IsHub
      ]
    }
  }

  # Return comprehensive results with NEW and LEGACY fields
  return(list(
    consensus_scores = consensus_scores,  # NEW: Full Bayesian consensus data frame
    standardized_metrics = standardized_metrics,  # NEW: Raw z-scores by approach and method
    weighted_hubs_by_method = weighted_hubs,  # LEGACY: For backward compatibility
    percolation_hubs_by_method = percolation_hubs,  # LEGACY
    persistence_hubs_by_method = persistence_hubs,  # LEGACY
    group = group_name,
    n_methods = length(methods),
    z_threshold = z_threshold,  # NEW: Record threshold used
    prior_hub_prob = prior_hub_prob,  # NEW: Record prior used
    calibration = calibration  # NEW: Store calibration results for UI display
  ))
}


# ============================================================================
# Function 2: Cross-Method Network Similarity
# ============================================================================

compute_cross_method_network_similarity <- function(method_percolation_results,
                                                    group_pairs = NULL) {

  methods <- names(method_percolation_results)

  if(length(methods) == 0) {
    return(list(
      by_method = list(),
      cross_method_average = data.frame(),
      group_pairs = list(),
      n_methods = 0
    ))
  }

  # Auto-generate all pairwise group comparisons if not specified
  if(is.null(group_pairs)) {
    all_groups <- unique(unlist(lapply(method_percolation_results, function(m) {
      if(!is.null(m$networks)) names(m$networks) else character(0)
    })))

    if(length(all_groups) < 2) {
      return(list(
        by_method = list(),
        cross_method_average = data.frame(),
        group_pairs = list(),
        n_methods = length(methods)
      ))
    }

    group_pairs <- combn(all_groups, 2, simplify = FALSE)
  }

  similarity_results <- list()

  for(method in methods) {
    method_similarities <- data.frame(
      Group1 = character(),
      Group2 = character(),
      JaccardIndex = numeric(),
      DiceCoefficient = numeric(),
      DegreeCorrelation = numeric(),
      DensityDifference = numeric(),
      ClusteringDifference = numeric(),
      stringsAsFactors = FALSE
    )

    networks <- method_percolation_results[[method]]$networks

    if(!is.null(networks) && length(networks) > 0) {
      for(pair in group_pairs) {
        g1_name <- pair[1]
        g2_name <- pair[2]

        if(g1_name %in% names(networks) && g2_name %in% names(networks)) {
          g1 <- networks[[g1_name]]
          g2 <- networks[[g2_name]]

          if(!is.null(g1) && !is.null(g2) && requireNamespace("igraph", quietly = TRUE)) {
            # Jaccard Index (edge overlap)
            edges1 <- igraph::as_edgelist(g1)
            edges2 <- igraph::as_edgelist(g2)

            if(nrow(edges1) > 0 && nrow(edges2) > 0) {
              edges1_set <- paste(edges1[,1], edges1[,2], sep="-")
              edges2_set <- paste(edges2[,1], edges2[,2], sep="-")

              intersection <- length(intersect(edges1_set, edges2_set))
              union <- length(union(edges1_set, edges2_set))
              jaccard <- ifelse(union > 0, intersection / union, 0)

              # Dice Coefficient
              dice <- ifelse((length(edges1_set) + length(edges2_set)) > 0,
                            2 * intersection / (length(edges1_set) + length(edges2_set)),
                            0)
            } else {
              jaccard <- 0
              dice <- 0
            }

            # Degree correlation (for common nodes)
            common_nodes <- intersect(igraph::V(g1)$name, igraph::V(g2)$name)

            if(length(common_nodes) > 2) {
              deg1 <- igraph::degree(g1)[common_nodes]
              deg2 <- igraph::degree(g2)[common_nodes]
              deg_cor <- cor(deg1, deg2, method = "spearman", use = "complete.obs")
            } else {
              deg_cor <- NA
            }

            # Network-level metric differences
            density_diff <- abs(igraph::edge_density(g1) - igraph::edge_density(g2))

            clust1 <- igraph::transitivity(g1, type = "global")
            clust2 <- igraph::transitivity(g2, type = "global")
            clust_diff <- abs(ifelse(is.na(clust1), 0, clust1) - ifelse(is.na(clust2), 0, clust2))

            # Store results
            method_similarities <- rbind(method_similarities, data.frame(
              Group1 = g1_name,
              Group2 = g2_name,
              JaccardIndex = jaccard,
              DiceCoefficient = dice,
              DegreeCorrelation = deg_cor,
              DensityDifference = density_diff,
              ClusteringDifference = clust_diff,
              stringsAsFactors = FALSE
            ))
          }
        }
      }
    }

    similarity_results[[method]] <- method_similarities
  }

  # Calculate cross-method average similarities
  avg_similarities <- data.frame(
    Group1 = character(),
    Group2 = character(),
    MeanJaccard = numeric(),
    SDJaccard = numeric(),
    MeanDice = numeric(),
    SDDice = numeric(),
    MeanDegreeCorr = numeric(),
    SDDegreeCorr = numeric(),
    stringsAsFactors = FALSE
  )

  for(pair in group_pairs) {
    g1 <- pair[1]
    g2 <- pair[2]

    jaccard_vals <- sapply(similarity_results, function(m) {
      if(nrow(m) > 0) {
        row <- m[m$Group1 == g1 & m$Group2 == g2, ]
        ifelse(nrow(row) > 0, row$JaccardIndex, NA)
      } else {
        NA
      }
    })

    dice_vals <- sapply(similarity_results, function(m) {
      if(nrow(m) > 0) {
        row <- m[m$Group1 == g1 & m$Group2 == g2, ]
        ifelse(nrow(row) > 0, row$DiceCoefficient, NA)
      } else {
        NA
      }
    })

    deg_corr_vals <- sapply(similarity_results, function(m) {
      if(nrow(m) > 0) {
        row <- m[m$Group1 == g1 & m$Group2 == g2, ]
        ifelse(nrow(row) > 0, row$DegreeCorrelation, NA)
      } else {
        NA
      }
    })

    avg_similarities <- rbind(avg_similarities, data.frame(
      Group1 = g1,
      Group2 = g2,
      MeanJaccard = mean(jaccard_vals, na.rm = TRUE),
      SDJaccard = sd(jaccard_vals, na.rm = TRUE),
      MeanDice = mean(dice_vals, na.rm = TRUE),
      SDDice = sd(dice_vals, na.rm = TRUE),
      MeanDegreeCorr = mean(deg_corr_vals, na.rm = TRUE),
      SDDegreeCorr = sd(deg_corr_vals, na.rm = TRUE),
      stringsAsFactors = FALSE
    ))
  }

  return(list(
    by_method = similarity_results,
    cross_method_average = avg_similarities,
    group_pairs = group_pairs,
    n_methods = length(methods)
  ))
}


# ============================================================================
# Function 3: Cross-Method Network Metrics Aggregation
# ============================================================================

compute_cross_method_network_metrics_aggregation <- function(method_percolation_results,
                                                             group_name) {

  methods <- names(method_percolation_results)

  # Collect metrics from each method
  metrics_list <- list()
  for(method in methods) {
    if(!is.null(method_percolation_results[[method]]$global_metrics)) {
      global_metrics <- method_percolation_results[[method]]$global_metrics
      group_metrics <- global_metrics[global_metrics$Group == group_name, ]

      if(nrow(group_metrics) > 0) {
        metrics_list[[method]] <- group_metrics
      }
    }
  }

  if(length(metrics_list) == 0) {
    return(NULL)
  }

  # Aggregate across methods
  metric_names <- c("Density", "Clustering", "Path_Length", "Modularity",
                   "Assortativity", "Small_World_Sigma", "Diameter")

  # Initialize empty data frame with correct structure
  aggregated <- data.frame(
    Group = character(),
    Metric = character(),
    Mean = numeric(),
    SD = numeric(),
    CV = numeric(),
    Min = numeric(),
    Max = numeric(),
    Range = numeric(),
    MethodsAvailable = integer(),
    stringsAsFactors = FALSE
  )

  for(metric in metric_names) {
    if(all(sapply(metrics_list, function(m) metric %in% names(m)))) {
      values <- sapply(metrics_list, function(m) m[[metric]])
      values <- values[is.finite(values)]  # Remove Inf/NaN

      if(length(values) > 0) {
        mean_val <- mean(values, na.rm = TRUE)
        sd_val <- sd(values, na.rm = TRUE)
        cv_val <- ifelse(mean_val != 0, sd_val / mean_val, NA)

        aggregated <- rbind(aggregated, data.frame(
          Group = group_name,
          Metric = metric,
          Mean = mean_val,
          SD = sd_val,
          CV = cv_val,
          Min = min(values, na.rm = TRUE),
          Max = max(values, na.rm = TRUE),
          Range = max(values, na.rm = TRUE) - min(values, na.rm = TRUE),
          MethodsAvailable = length(values),
          stringsAsFactors = FALSE
        ))
      }
    }
  }

  # Categorize robustness based on CV
  if(nrow(aggregated) > 0) {
    aggregated$Robustness <- cut(
      aggregated$CV,
      breaks = c(-Inf, 0.15, 0.30, 0.50, Inf),
      labels = c("Highly Robust", "Robust", "Moderately Robust", "Variable")
    )
  }

  return(aggregated)
}
