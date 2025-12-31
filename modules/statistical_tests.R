# Statistical Testing Module
# Permutation testing, group comparisons, and statistical validation
# Part of ConsensusConnectR Persistence2 - Advanced Statistical Methods

suppressPackageStartupMessages({
  library(parallel)
  library(igraph)
})

# ==============================================================================
# AUTOMATED PERMUTATION COUNT CALCULATION
# ==============================================================================

#' Calculate Required Number of Permutations
#'
#' Automatically determines the number of permutations needed based on the
#' number of statistical tests being performed. Ensures the minimum detectable
#' p-value is small enough to survive FDR correction.
#'
#' Formula: n = ceiling((n_tests / alpha) * safety_factor)
#'
#' @param n_tests Number of statistical tests to be performed
#' @param alpha Significance threshold (default: 0.05)
#' @param safety_factor Multiplier for conservative estimation (default: 2)
#' @param min_perms Minimum number of permutations (default: 1000)
#' @param max_perms Maximum number of permutations for computational limits (default: NULL, no limit)
#' @return Named list with n_permutations, min_detectable_p, and explanation
#' @export
calculate_required_permutations <- function(n_tests,
                                             alpha = 0.05,
                                             safety_factor = 2,
                                             min_perms = 1000,
                                             max_perms = NULL) {
  # Calculate required permutations
  n_required <- ceiling((n_tests / alpha) * safety_factor)

  # Apply minimum bound
  n_permutations <- max(min_perms, n_required)

  # Apply maximum bound if specified
  capped <- FALSE
  if(!is.null(max_perms) && n_permutations > max_perms) {
    n_permutations <- max_perms
    capped <- TRUE
  }


  # Calculate minimum detectable p-value
  min_detectable_p <- 1 / (n_permutations + 1)

  # Most stringent FDR-corrected threshold (Bonferroni-like worst case)
  stringent_threshold <- alpha / n_tests

  # Check if we have sufficient resolution
  sufficient_resolution <- min_detectable_p < stringent_threshold

  # Ensure integer values for sprintf %d format
  n_tests_int <- as.integer(n_tests)
  n_permutations_int <- as.integer(n_permutations)
  safety_factor_int <- as.integer(safety_factor)
  n_required_int <- as.integer(n_required)

  # Generate explanation
  explanation <- sprintf(
    "Testing %d comparisons at alpha=%.2f requires %d permutations (formula: ceiling(%d/%.2f)*%d = %d). Min detectable p-value: %.2e. %s",
    n_tests_int, alpha, n_permutations_int,
    n_tests_int, alpha, safety_factor_int, n_required_int,
    min_detectable_p,
    ifelse(capped, sprintf("CAPPED from %d due to computational limits.", n_required_int),
           ifelse(sufficient_resolution, "Sufficient resolution for FDR correction.",
                  "WARNING: May have insufficient resolution for stringent FDR correction."))
  )

  return(list(
    n_permutations = n_permutations,
    n_tests = n_tests,
    n_required_uncapped = n_required,
    min_detectable_p = min_detectable_p,
    stringent_threshold = stringent_threshold,
    sufficient_resolution = sufficient_resolution,
    capped = capped,
    explanation = explanation
  ))
}

#' Estimate Permutation Test Runtime
#'
#' Estimates the time required to run permutation tests based on the number
#' of tests, permutations, and available computational resources. Uses a
#' data-driven formula when network dimensions are provided, or calibrated
#' timing from benchmark runs.
#'
#' @param n_tests Number of statistical tests
#' @param n_permutations Number of permutations per test
#' @param n_workers Number of parallel workers (default: 1)
#' @param n_nodes Number of nodes in the network (optional, for formula-based estimation)
#' @param n_matrices Number of network matrices being compared (optional)
#' @param calibrated_time_ms Time per permutation from calibration benchmark (optional)
#' @return Named list with estimated time in seconds, minutes, formatted string,
#'   time_per_perm_ms used, and estimation_method (calibrated/estimated/default)
#' @export
estimate_permutation_runtime <- function(n_tests,
                                          n_permutations,
                                          n_workers = 1,
                                          n_nodes = NULL,
                                          n_matrices = NULL,
                                          calibrated_time_ms = NULL,
                                          calibration = NULL) {
  # Validate inputs - return safe defaults if missing/invalid

  if (is.null(n_tests) || length(n_tests) == 0 || !is.numeric(n_tests) || n_tests <= 0) {
    n_tests <- 1
  }
  if (is.null(n_permutations) || length(n_permutations) == 0 || !is.numeric(n_permutations) || n_permutations <= 0) {
    n_permutations <- 500
  }
  if (is.null(n_workers) || length(n_workers) == 0 || !is.numeric(n_workers) || n_workers < 1) {
    n_workers <- 1
  }

  # Determine time per permutation based on available information
  estimation_method <- "default"
  spawn_overhead_per_worker <- 3000  # Default Windows spawn overhead (ms)
  using_cpp_backend <- FALSE

  # Use comprehensive calibration object if available
  if (!is.null(calibration) && !is.null(calibration$time_per_perm_ms)) {
    time_per_perm_ms <- calibration$time_per_perm_ms
    spawn_overhead_per_worker <- if (!is.null(calibration$spawn_overhead_per_worker_ms)) {
      calibration$spawn_overhead_per_worker_ms
    } else {
      0  # C++ doesn't have spawn overhead
    }
    estimation_method <- "calibrated"

    # Check if using C++ backend - time is ALREADY parallelized with OpenMP
    if (!is.null(calibration$backend) && calibration$backend == "cpp_openmp") {
      using_cpp_backend <- TRUE
      estimation_method <- "calibrated_cpp"
    }
  } else if (!is.null(calibrated_time_ms) && calibrated_time_ms > 0) {
    # Legacy: Use simple calibrated time if available
    time_per_perm_ms <- calibrated_time_ms
    estimation_method <- "calibrated_legacy"
  } else if (!is.null(n_nodes) && !is.null(n_matrices) && n_nodes > 0 && n_matrices > 0) {
    # Formula-based estimation using data dimensions
    # Complexity: O(n_nodes^2 × n_matrices) per permutation
    # Base overhead ~0.5ms, plus scaling with matrix operations
    base_ms <- 0.5
    scaling_factor <- 0.00005  # Empirically tuned coefficient
    time_per_perm_ms <- base_ms + (n_nodes^2 * n_matrices * scaling_factor)
    estimation_method <- "estimated"
  } else {
    # Conservative fallback when no dimension info available
    time_per_perm_ms <- 5
    estimation_method <- "default"
  }

  # Total permutation operations
  total_ops <- n_tests * n_permutations

  # Handle C++ backend specially - time_per_perm_ms is ALREADY parallelized
  if (using_cpp_backend) {
    # C++ OpenMP: time_per_perm_ms already includes parallelization
    # No R process spawn overhead, no serialization overhead
    # Just multiply total operations by the already-parallel time

    # For batch processing, there's small overhead per batch (~10 batches for progress updates)
    n_batches <- ceiling(n_tests / max(1, ceiling(n_tests / 10)))
    batch_overhead_ms <- n_batches * 5  # ~5ms overhead per C++ call

    time_ms <- total_ops * time_per_perm_ms + batch_overhead_ms
    serial_compute_ms <- time_ms  # For C++, "serial" time IS the parallel time (already optimized)
    parallel_beneficial <- TRUE  # C++ is always beneficial
    overhead_breakdown <- list(
      spawn_ms = 0,
      serialization_ms = 0,
      batch_overhead_ms = batch_overhead_ms,
      compute_ms = total_ops * time_per_perm_ms
    )

  } else if (n_workers > 1) {
    # R parallel: Calculate with spawn and serialization overhead
    # Serial computation time (what it would take without parallelization)
    serial_compute_ms <- total_ops * time_per_perm_ms

    # Process spawn overhead (one-time cost on Windows)
    spawn_overhead_ms <- spawn_overhead_per_worker * n_workers

    # Serialization overhead (proportional to data size and chunks)
    n_chunks <- ceiling(n_tests / n_workers)
    if (!is.null(n_nodes) && !is.null(n_matrices)) {
      serialization_ms <- n_chunks * (50 + n_nodes^2 * n_matrices * 0.001)
    } else {
      serialization_ms <- n_chunks * 100  # Conservative default
    }

    # Parallel compute time
    parallel_compute_ms <- serial_compute_ms / n_workers

    # Total time with overhead
    time_ms <- spawn_overhead_ms + parallel_compute_ms + serialization_ms

    # Check if parallel is beneficial
    parallel_beneficial <- time_ms < serial_compute_ms
    overhead_breakdown <- list(
      spawn_ms = spawn_overhead_ms,
      serialization_ms = serialization_ms,
      compute_ms = parallel_compute_ms
    )
  } else {
    # Serial R execution
    serial_compute_ms <- total_ops * time_per_perm_ms
    time_ms <- serial_compute_ms
    parallel_beneficial <- FALSE
    overhead_breakdown <- list(
      spawn_ms = 0,
      serialization_ms = 0,
      compute_ms = serial_compute_ms
    )
  }

  # Ensure time_ms is valid before calculations
 if (is.null(time_ms) || length(time_ms) == 0 || !is.finite(time_ms)) {
    time_ms <- 1000  # Default to 1 second
  }

  time_seconds <- time_ms / 1000
  time_minutes <- time_seconds / 60

  # Format time string with safety checks
  if (is.null(time_minutes) || length(time_minutes) == 0 || !is.finite(time_minutes)) {
    time_formatted <- "Calculating..."
  } else if (time_minutes < 1) {
    time_formatted <- sprintf("%.0f seconds", time_seconds)
  } else if (time_minutes < 60) {
    time_formatted <- sprintf("%.1f minutes", time_minutes)
  } else {
    time_formatted <- sprintf("%.1f hours", time_minutes / 60)
  }

  return(list(
    time_seconds = time_seconds,
    time_minutes = time_minutes,
    time_formatted = time_formatted,
    total_operations = total_ops,
    n_workers = n_workers,
    time_per_perm_ms = time_per_perm_ms,
    estimation_method = estimation_method,
    parallel_beneficial = parallel_beneficial,
    serial_time_ms = serial_compute_ms,
    overhead_breakdown = overhead_breakdown
  ))
}

#' Calibrate Permutation Time
#'
#' Runs a small benchmark to measure actual permutation time on the current
#' machine with the current dataset. This provides a more accurate estimate
#' than the formula-based approach.
#'
#' @param analysis_results The analysis results object
#' @param group1 Name of group 1
#' @param group2 Name of group 2
#' @param n_calibration_perms Number of permutations to run for calibration (default: 10)
#' @param methods Vector of correlation methods to include (optional)
#' @return Named list with calibrated time_per_perm_ms, n_nodes, n_matrices
#' @export
calibrate_permutation_time <- function(analysis_results,
                                        group1,
                                        group2,
                                        n_calibration_perms = 10,
                                        methods = NULL) {
  # Extract networks using the same function used in actual permutation tests
  networks_g1 <- extract_all_network_matrices(group1, analysis_results, methods)
  networks_g2 <- extract_all_network_matrices(group2, analysis_results, methods)

  if (length(networks_g1) == 0 || length(networks_g2) == 0) {
    return(list(
      time_per_perm_ms = 5,  # Fallback to default
      n_nodes = NA,
      n_matrices = 0,
      n_calibration_perms = 0,
      error = "Could not extract network matrices"
    ))
  }

  # Get dimensions
  node_names <- rownames(networks_g1[[1]])
  n_nodes <- length(node_names)
  n_matrices <- length(networks_g1)

  # Time a small number of permutation iterations
  start_time <- Sys.time()

  for (i in 1:n_calibration_perms) {
    perm_idx <- sample(n_nodes)

    # Permute all matrices (the expensive operation)
    permuted_g1 <- lapply(networks_g1, function(m) {
      m[perm_idx, perm_idx]
    })
    permuted_g2 <- lapply(networks_g2, function(m) {
      m[perm_idx, perm_idx]
    })

    # Compute Jaccard (also expensive, included in actual permutation tests)
    compute_averaged_jaccard(permuted_g1, permuted_g2, exclude_nodes = NULL)
  }

  elapsed_ms <- as.numeric(difftime(Sys.time(), start_time, units = "secs")) * 1000
  time_per_perm_ms <- elapsed_ms / n_calibration_perms

  return(list(
    time_per_perm_ms = time_per_perm_ms,
    n_nodes = n_nodes,
    n_matrices = n_matrices,
    n_calibration_perms = n_calibration_perms,
    elapsed_ms = elapsed_ms
  ))
}

#' Calibrate Parallel Performance
#'
#' Comprehensive calibration that measures computation time and parallel performance.
#' Automatically detects and uses C++ backend with OpenMP if available, otherwise
#' falls back to measuring R parallel overhead.
#'
#' @param analysis_results The analysis results object
#' @param group1 Name of group 1
#' @param group2 Name of group 2
#' @param n_calibration_perms Number of permutations to run for calibration (default: 20)
#' @param target_workers Specific worker count to calibrate for (default: NULL = auto-detect)
#' @param methods Vector of correlation methods to include (optional)
#' @param verbose Print diagnostic messages (default: FALSE)
#' @return Named list with calibrated timing, backend info, and performance estimates
#' @export
calibrate_parallel_performance <- function(analysis_results,
                                            group1,
                                            group2,
                                            n_calibration_perms = 20,
                                            target_workers = NULL,
                                            methods = NULL,
                                            verbose = FALSE) {
  # Extract networks
  networks_g1 <- extract_all_network_matrices(group1, analysis_results, methods)
  networks_g2 <- extract_all_network_matrices(group2, analysis_results, methods)

  if (length(networks_g1) == 0 || length(networks_g2) == 0) {
    return(list(
      error = "Could not extract network matrices",
      time_per_perm_ms = 5,
      optimal_workers = 1,
      use_parallel = FALSE,
      backend = "none"
    ))
  }

  node_names <- rownames(networks_g1[[1]])
  n_nodes <- length(node_names)
  n_matrices <- length(networks_g1)

  if (verbose) message("Calibrating with ", n_nodes, " nodes and ", n_matrices, " matrices...")

  # Check for C++ backend availability - thorough check to prevent crashes
  cpp_available <- FALSE
  cpp_threads <- 1

  # Use the safer check function if available
  if (exists("is_cpp_really_available", mode = "function")) {
    cpp_available <- tryCatch(is_cpp_really_available(), error = function(e) FALSE)
  } else if (exists("CPP_BACKEND_AVAILABLE") && isTRUE(CPP_BACKEND_AVAILABLE)) {
    # Fallback: check if the actual C++ function exists
    cpp_available <- exists("parallel_permutation_test_cpp", mode = "function")
  }

  if (exists("CPP_OPENMP_THREADS")) {
    cpp_threads <- CPP_OPENMP_THREADS
  }

  if (cpp_available && verbose) {
    message("[Calibration] C++ backend detected with ", cpp_threads, " OpenMP threads")
  }

  # =========================================================================
  # C++ BACKEND CALIBRATION (preferred path)
  # =========================================================================
  if (cpp_available && exists("test_combined_contribution_fast", mode = "function")) {
    if (verbose) message("Using C++ backend for calibration...")

    # Pick a random region to test with
    test_region <- sample(node_names, 1)

    # Time C++ execution
    start_cpp <- Sys.time()
    tryCatch({
      cpp_result <- test_combined_contribution_fast(
        networks_g1 = networks_g1,
        networks_g2 = networks_g2,
        node_names = node_names,
        selected_regions = test_region,
        n_permutations = n_calibration_perms,
        n_threads = 0,  # Auto-detect
        seed = 42
      )
      cpp_elapsed_ms <- as.numeric(difftime(Sys.time(), start_cpp, units = "secs")) * 1000
      cpp_time_per_perm <- cpp_elapsed_ms / n_calibration_perms

      # Also measure R serial for comparison
      start_r <- Sys.time()
      for (i in 1:min(5, n_calibration_perms)) {
        perm_idx <- sample(n_nodes)
        permuted_g1 <- lapply(networks_g1, function(m) m[perm_idx, perm_idx])
        permuted_g2 <- lapply(networks_g2, function(m) m[perm_idx, perm_idx])
        compute_averaged_jaccard(permuted_g1, permuted_g2, exclude_nodes = NULL)
      }
      r_elapsed_ms <- as.numeric(difftime(Sys.time(), start_r, units = "secs")) * 1000
      r_time_per_perm <- r_elapsed_ms / min(5, n_calibration_perms)

      # Calculate speedup
      cpp_speedup <- r_time_per_perm / cpp_time_per_perm

      if (verbose) {
        message("  C++ time per permutation: ", round(cpp_time_per_perm, 3), " ms")
        message("  R serial time per permutation: ", round(r_time_per_perm, 2), " ms")
        message("  C++ speedup: ", round(cpp_speedup, 1), "x")
        message("  OpenMP threads: ", cpp_threads)
      }

      recommendation <- sprintf("C++ backend with %d OpenMP threads (%.0fx faster than R)",
                                 cpp_threads, cpp_speedup)

      return(list(
        time_per_perm_ms = cpp_time_per_perm,
        r_time_per_perm_ms = r_time_per_perm,
        serial_time_ms = cpp_elapsed_ms,
        n_nodes = n_nodes,
        n_matrices = n_matrices,
        n_calibration_perms = n_calibration_perms,
        backend = "cpp_openmp",
        cpp_threads = cpp_threads,
        cpp_speedup = cpp_speedup,
        optimal_workers = cpp_threads,
        optimal_speedup = cpp_speedup,
        use_parallel = TRUE,
        recommendation = recommendation,
        calibration_timestamp = Sys.time()
      ))

    }, error = function(e) {
      if (verbose) message("  C++ calibration failed: ", e$message, " - falling back to R")
      # Fall through to R calibration below
    })
  }

  # =========================================================================
  # R FALLBACK CALIBRATION (when C++ not available)
  # =========================================================================
  if (verbose) message("Using R backend for calibration...")

  # Determine target worker count
  max_available_cores <- parallel::detectCores()
  if (is.null(target_workers) || target_workers == "auto") {
    # Default: use all cores minus 1 (leave one for system)
    target_workers <- max(1, max_available_cores - 1)
  } else {
    target_workers <- as.integer(target_workers)
  }
  # Ensure target_workers doesn't exceed available cores

  target_workers <- min(target_workers, max_available_cores)

  if (verbose) message("Target worker count: ", target_workers)

  # Phase 1: Measure serial computation time
  if (verbose) message("Phase 1: Measuring R serial computation time...")

  start_serial <- Sys.time()
  for (i in 1:n_calibration_perms) {
    perm_idx <- sample(n_nodes)
    permuted_g1 <- lapply(networks_g1, function(m) m[perm_idx, perm_idx])
    permuted_g2 <- lapply(networks_g2, function(m) m[perm_idx, perm_idx])
    compute_averaged_jaccard(permuted_g1, permuted_g2, exclude_nodes = NULL)
  }
  serial_elapsed_ms <- as.numeric(difftime(Sys.time(), start_serial, units = "secs")) * 1000
  time_per_perm_ms <- serial_elapsed_ms / n_calibration_perms

  if (verbose) message("  R serial time per permutation: ", round(time_per_perm_ms, 2), " ms")

  # Phase 2: Measure parallel overhead for target worker count only
  parallel_results <- list()

  if (PARALLEL_AVAILABLE && target_workers > 1) {
    if (verbose) message("Phase 2: Measuring R parallel overhead for ", target_workers, " workers...")

    n_w <- target_workers
    if (verbose) message("  Testing ", n_w, " workers...")

      tryCatch({
        # Time worker startup
        start_spawn <- Sys.time()
        old_plan <- future::plan()
        future::plan(future::multisession, workers = n_w)

        # Force worker initialization with minimal task (measures spawn time)
        warmup <- furrr::future_map(1:n_w, function(i) Sys.getpid(),
                                     .options = furrr::furrr_options(seed = TRUE))
        spawn_time_ms <- as.numeric(difftime(Sys.time(), start_spawn, units = "secs")) * 1000

        # Time actual parallel execution with same workload
        start_parallel <- Sys.time()
        parallel_results_inner <- furrr::future_map_dbl(
          1:n_calibration_perms,
          function(i) {
            perm_idx <- sample(n_nodes)
            permuted_g1 <- lapply(networks_g1, function(m) m[perm_idx, perm_idx])
            permuted_g2 <- lapply(networks_g2, function(m) m[perm_idx, perm_idx])
            result <- compute_averaged_jaccard(permuted_g1, permuted_g2, exclude_nodes = NULL)
            return(result$avg_jaccard)
          },
          .options = furrr::furrr_options(
            seed = TRUE,
            globals = list(
              compute_averaged_jaccard = compute_averaged_jaccard,
              compute_jaccard_for_contribution = compute_jaccard_for_contribution,
              networks_g1 = networks_g1,
              networks_g2 = networks_g2,
              n_nodes = n_nodes
            )
          )
        )
        parallel_time_ms <- as.numeric(difftime(Sys.time(), start_parallel, units = "secs")) * 1000

        future::plan(old_plan)

        total_time_ms <- spawn_time_ms + parallel_time_ms
        effective_speedup <- serial_elapsed_ms / total_time_ms

        parallel_results[[as.character(n_w)]] <- list(
          workers = n_w,
          spawn_time_ms = spawn_time_ms,
          parallel_time_ms = parallel_time_ms,
          total_time_ms = total_time_ms,
          effective_speedup = effective_speedup,
          spawn_per_worker_ms = spawn_time_ms / n_w
        )

        if (verbose) {
          message(sprintf("    spawn=%.0fms (%.0fms/worker), compute=%.0fms, total=%.0fms, speedup=%.2fx",
                          spawn_time_ms, spawn_time_ms/n_w, parallel_time_ms, total_time_ms, effective_speedup))
        }
      }, error = function(e) {
        if (verbose) message("    Error testing ", n_w, " workers: ", e$message)
        # Restore plan on error
        tryCatch(future::plan(old_plan), error = function(e2) NULL)
      })
  } else {
    if (verbose) message("Phase 2: Skipped (parallel not available or target_workers = 1)")
  }

  # Get results for target worker count (if tested)
  target_key <- as.character(target_workers)
  target_result <- parallel_results[[target_key]]

  # Calculate spawn overhead per worker
  spawn_overhead_per_worker <- if (!is.null(target_result)) {
    target_result$spawn_per_worker_ms
  } else {
    3000  # Default 3 seconds per worker on Windows
  }

  # Determine if parallel is beneficial (target workers must beat serial by >10%)
  use_parallel <- !is.null(target_result) && target_result$effective_speedup > 1.1
  effective_speedup <- if (!is.null(target_result)) target_result$effective_speedup else 1.0

  recommendation <- if (!use_parallel) {
    "R serial (parallel overhead too high) - consider installing Rcpp/RcppArmadillo for C++ backend"
  } else {
    sprintf("R parallel: %d workers for %.1fx speedup", target_workers, effective_speedup)
  }

  # Always return target_workers as optimal (that's what user selected/will use)
  optimal_workers <- if (use_parallel) target_workers else 1

  if (verbose) {
    message("\nCalibration complete:")
    message("  R serial time per perm: ", round(time_per_perm_ms, 2), " ms")
    message("  Target workers: ", target_workers)
    message("  Spawn overhead/worker: ", round(spawn_overhead_per_worker, 0), " ms")
    message("  Parallel beneficial: ", use_parallel)
    message("  Recommendation: ", recommendation)
  }

  list(
    time_per_perm_ms = time_per_perm_ms,
    serial_time_ms = serial_elapsed_ms,
    n_nodes = n_nodes,
    n_matrices = n_matrices,
    n_calibration_perms = n_calibration_perms,
    backend = "r_fallback",
    parallel_results = parallel_results,
    target_workers = target_workers,
    optimal_workers = optimal_workers,
    optimal_speedup = effective_speedup,
    use_parallel = use_parallel,
    spawn_overhead_per_worker_ms = spawn_overhead_per_worker,
    recommendation = recommendation,
    calibration_timestamp = Sys.time()
  )
}

#' Determine Optimal Worker Count
#'
#' Calculates the optimal number of parallel workers based on problem size,
#' matrix dimensions, and calibration data. For Windows systems, accounts for
#' the significant overhead of spawning R processes.
#'
#' @param n_candidates Number of candidates to test
#' @param n_permutations Number of permutations per candidate
#' @param n_nodes Number of nodes in network matrices
#' @param n_matrices Number of network matrices
#' @param calibration Calibration object from calibrate_parallel_performance() (optional)
#' @return Named list with optimal_workers, should_use_parallel, recommendation
#' @export
determine_optimal_workers <- function(n_candidates,
                                       n_permutations,
                                       n_nodes,
                                       n_matrices,
                                       calibration = NULL) {
  # Estimate per-permutation computation time
  if (!is.null(calibration) && !is.null(calibration$time_per_perm_ms)) {
    compute_time_ms <- calibration$time_per_perm_ms
    spawn_overhead_ms <- calibration$spawn_overhead_per_worker_ms
  } else {
    # Formula-based estimate
    compute_time_ms <- 0.5 + (n_nodes^2 * n_matrices * 0.00005)
    spawn_overhead_ms <- 3000  # Conservative Windows default
  }

  # Total serial computation time
  total_compute_ms <- n_candidates * n_permutations * compute_time_ms

  # Available cores

  max_cores <- min(parallel::detectCores() - 1, n_candidates)
  max_cores <- max(1, max_cores)

  # If we have calibration data with actual measurements, use best measured config
  if (!is.null(calibration) && !is.null(calibration$optimal_workers)) {
    # Scale the calibration results to the actual workload
    # Calibration was done with n_calibration_perms, actual has n_candidates * n_permutations
    scale_factor <- (n_candidates * n_permutations) / calibration$n_calibration_perms

    # Estimate time with calibrated optimal workers
    if (calibration$optimal_workers > 1 && length(calibration$parallel_results) > 0) {
      opt_key <- as.character(calibration$optimal_workers)
      if (!is.null(calibration$parallel_results[[opt_key]])) {
        parallel_compute_ms <- calibration$parallel_results[[opt_key]]$parallel_time_ms * scale_factor
        spawn_ms <- calibration$parallel_results[[opt_key]]$spawn_time_ms
        parallel_time_ms <- spawn_ms + parallel_compute_ms
        serial_time_ms <- total_compute_ms

        return(list(
          optimal_workers = calibration$optimal_workers,
          serial_time_ms = serial_time_ms,
          parallel_time_ms = parallel_time_ms,
          expected_speedup = serial_time_ms / parallel_time_ms,
          should_use_parallel = parallel_time_ms < serial_time_ms * 0.9,
          recommendation = calibration$recommendation,
          estimation_method = "calibrated"
        ))
      }
    } else {
      # Calibration says serial is best
      return(list(
        optimal_workers = 1,
        serial_time_ms = total_compute_ms,
        parallel_time_ms = total_compute_ms,
        expected_speedup = 1.0,
        should_use_parallel = FALSE,
        recommendation = calibration$recommendation,
        estimation_method = "calibrated"
      ))
    }
  }

  # No calibration - use heuristic model
  # Find optimal workers by modeling total time
  best_workers <- 1
  best_time <- total_compute_ms

  # Test worker counts (powers of 2 up to max_cores)
  test_workers <- c(2, 4, 8, 16, 32, 64)
  test_workers <- test_workers[test_workers <= max_cores]

  for (w in test_workers) {
    # Process startup cost (one-time)
    process_startup <- w * spawn_overhead_ms

    # Parallel computation time
    parallel_compute <- total_compute_ms / w

    # Serialization overhead (proportional to data size and number of chunks)
    n_chunks <- ceiling(n_candidates / w)
    serialization <- n_chunks * (50 + n_nodes^2 * n_matrices * 0.001) * w

    total_time <- process_startup + parallel_compute + serialization

    if (total_time < best_time) {
      best_time <- total_time
      best_workers <- w
    }
  }

  # Determine if parallel is beneficial (at least 10% improvement)
  should_use_parallel <- best_workers > 1 && best_time < total_compute_ms * 0.9

  recommendation <- if (!should_use_parallel) {
    "Serial execution recommended - parallel overhead exceeds benefit for this workload"
  } else {
    sprintf("Use %d workers (expected %.1fx speedup)", best_workers, total_compute_ms / best_time)
  }

  list(
    optimal_workers = best_workers,
    serial_time_ms = total_compute_ms,
    parallel_time_ms = best_time,
    expected_speedup = total_compute_ms / best_time,
    should_use_parallel = should_use_parallel,
    recommendation = recommendation,
    estimation_method = "heuristic"
  )
}

#' Count Combinations for Cluster Analysis
#'
#' Calculates the number of region combinations for a given cluster size range.
#'
#' @param n_regions Total number of regions
#' @param max_size Maximum cluster size
#' @param min_size Minimum cluster size (default: 1)
#' @return Named list with counts per size and total
#' @export
count_combinations <- function(n_regions, max_size, min_size = 1) {
  counts <- list()
  total <- 0

  for(k in min_size:max_size) {
    if(k <= n_regions) {
      count_k <- choose(n_regions, k)
      counts[[paste0("size_", k)]] <- count_k
      total <- total + count_k
    }
  }

  return(list(
    counts_by_size = counts,
    total = total,
    n_regions = n_regions,
    min_size = min_size,
    max_size = max_size
  ))
}


#' Hypothesis Test for Specific Region Combinations
#'
#' Tests user-specified region combinations for their contribution to group
#' differences. This is the confirmatory/hypothesis-driven approach.
#'
#' @param analysis_results The analysis_results reactive containing all data
#' @param group1 Name of group 1
#' @param group2 Name of group 2
#' @param region_combinations List of character vectors, each specifying regions to test together
#' @param n_permutations Number of permutations (auto-calculated if NULL)
#' @param alpha Significance threshold (default: 0.05)
#' @param correction_method "fdr", "bonferroni", or "none"
#' @param seed Random seed for reproducibility
#' @param progress_callback Optional progress callback function
#' @return Data frame with results for each combination
#' @export
test_hypothesis_combinations <- function(analysis_results,
                                          group1,
                                          group2,
                                          region_combinations,
                                          n_permutations = NULL,
                                          alpha = 0.05,
                                          correction_method = "fdr",
                                          seed = 42,
                                          progress_callback = NULL) {
  # Set seed for reproducibility
  if(!is.null(seed)) {
    set.seed(seed)
  }

  # Number of tests = number of combinations provided
  n_tests <- length(region_combinations)

  if(n_tests == 0) {
    stop("No region combinations provided for testing")
  }

  # Auto-calculate permutations if not specified
  if(is.null(n_permutations)) {
    perm_calc <- calculate_required_permutations(n_tests, alpha = alpha)
    n_permutations <- perm_calc$n_permutations
    message(sprintf("Auto-calculated permutations: %d (for %d tests)", n_permutations, n_tests))
  }

  # Extract networks
  networks_g1 <- extract_all_network_matrices(group1, analysis_results)
  networks_g2 <- extract_all_network_matrices(group2, analysis_results)

  if(length(networks_g1) == 0 || length(networks_g2) == 0) {
    stop("Could not extract network matrices for one or both groups")
  }

  # Compute baseline Jaccard
  baseline_result <- compute_averaged_jaccard(networks_g1, networks_g2, exclude_nodes = NULL)
  baseline_jaccard <- baseline_result$avg_jaccard

  if(is.na(baseline_jaccard)) {
    stop("Could not compute baseline Jaccard similarity")
  }

  # Get node names for permutation
  first_mat <- networks_g1[[1]]
  node_names <- rownames(first_mat)
  if(is.null(node_names)) node_names <- colnames(first_mat)
  n_nodes <- length(node_names)

  # Initialize results
  results <- data.frame(
    Combination_ID = integer(),
    Regions = character(),
    N_Regions = integer(),
    Baseline_Jaccard = numeric(),
    Jaccard_Without = numeric(),
    Contribution = numeric(),
    P_Value = numeric(),
    stringsAsFactors = FALSE
  )

  # Test each combination
  for(i in seq_along(region_combinations)) {
    combo <- region_combinations[[i]]
    combo_name <- paste(combo, collapse = " + ")

    # Update progress
    if(!is.null(progress_callback)) {
      progress_callback(i / n_tests)
    }

    # Compute observed contribution (Jaccard without these regions)
    loo_result <- compute_averaged_jaccard(networks_g1, networks_g2, exclude_nodes = combo)
    jaccard_without <- loo_result$avg_jaccard

    if(is.na(jaccard_without)) {
      # Skip if we can't compute
      next
    }

    observed_contribution <- jaccard_without - baseline_jaccard

    # Run permutation test
    null_distribution <- numeric(n_permutations)

    for(perm in 1:n_permutations) {
      # Shuffle node labels for both groups independently
      perm_idx_g1 <- sample(n_nodes)
      perm_idx_g2 <- sample(n_nodes)

      # Apply permutation to all network matrices
      permuted_g1 <- list()
      for(key in names(networks_g1)) {
        perm_mat <- networks_g1[[key]][perm_idx_g1, perm_idx_g1]
        rownames(perm_mat) <- node_names
        colnames(perm_mat) <- node_names
        permuted_g1[[key]] <- perm_mat
      }

      permuted_g2 <- list()
      for(key in names(networks_g2)) {
        perm_mat <- networks_g2[[key]][perm_idx_g2, perm_idx_g2]
        rownames(perm_mat) <- node_names
        colnames(perm_mat) <- node_names
        permuted_g2[[key]] <- perm_mat
      }

      # Compute contribution under permutation
      perm_baseline <- compute_averaged_jaccard(permuted_g1, permuted_g2, exclude_nodes = NULL)$avg_jaccard
      perm_without <- compute_averaged_jaccard(permuted_g1, permuted_g2, exclude_nodes = combo)$avg_jaccard

      if(!is.na(perm_baseline) && !is.na(perm_without)) {
        null_distribution[perm] <- perm_without - perm_baseline
      } else {
        null_distribution[perm] <- NA
      }
    }

    # Compute p-value (two-tailed)
    null_valid <- null_distribution[!is.na(null_distribution)]
    if(length(null_valid) > 0) {
      p_value <- (sum(abs(null_valid) >= abs(observed_contribution)) + 1) / (length(null_valid) + 1)
    } else {
      p_value <- NA
    }

    # Add to results
    results <- rbind(results, data.frame(
      Combination_ID = i,
      Regions = combo_name,
      N_Regions = length(combo),
      Baseline_Jaccard = baseline_jaccard,
      Jaccard_Without = jaccard_without,
      Contribution = observed_contribution,
      P_Value = p_value,
      stringsAsFactors = FALSE
    ))
  }

  # Apply multiple comparison correction
  if(nrow(results) > 0 && correction_method != "none") {
    if(correction_method == "fdr") {
      results$P_Adjusted <- p.adjust(results$P_Value, method = "BH")
    } else if(correction_method == "bonferroni") {
      results$P_Adjusted <- p.adjust(results$P_Value, method = "bonferroni")
    }
  } else {
    results$P_Adjusted <- results$P_Value
  }

  # Add significance indicators
  results$Significant <- !is.na(results$P_Adjusted) & results$P_Adjusted < alpha
  results$Significance_Stars <- ""
  results$Significance_Stars[!is.na(results$P_Adjusted) & results$P_Adjusted < 0.05] <- "*"
  results$Significance_Stars[!is.na(results$P_Adjusted) & results$P_Adjusted < 0.01] <- "**"
  results$Significance_Stars[!is.na(results$P_Adjusted) & results$P_Adjusted < 0.001] <- "***"

  return(list(
    results = results,
    baseline_jaccard = baseline_jaccard,
    n_permutations = n_permutations,
    n_tests = n_tests,
    correction_method = correction_method,
    group1 = group1,
    group2 = group2
  ))
}

# Check for glmnet availability (for Lasso regression)
GLMNET_AVAILABLE <- requireNamespace("glmnet", quietly = TRUE)
if(GLMNET_AVAILABLE) {
  suppressPackageStartupMessages(library(glmnet))
}

# Check for parallel processing packages (future/furrr/progressr)
PARALLEL_AVAILABLE <- requireNamespace("future", quietly = TRUE) &&
                       requireNamespace("furrr", quietly = TRUE)
PROGRESSR_AVAILABLE <- requireNamespace("progressr", quietly = TRUE)

if(PARALLEL_AVAILABLE) {
  suppressPackageStartupMessages({
    library(future)
    library(furrr)
  })
}
if(PROGRESSR_AVAILABLE) {
  suppressPackageStartupMessages({
    library(progressr)
  })
}

#' Compute Permutation Test for Network Metrics
#'
#' Performs permutation testing to assess statistical significance of
#' differences in network metrics between two groups.
#'
#' @param group1_data Numeric vector of metric values for group 1
#' @param group2_data Numeric vector of metric values for group 2
#' @param n_permutations Number of permutations (default: 5000)
#' @param metric_name Name of the metric being tested
#' @param alternative "two.sided", "greater", or "less"
#' @param parallel_cores Number of cores for parallel processing (default: 1)
#' @return List containing test statistic, p-value, and null distribution
compute_permutation_test <- function(group1_data,
                                     group2_data,
                                     n_permutations = 5000,
                                     metric_name = "Metric",
                                     alternative = "two.sided",
                                     parallel_cores = 1) {

  # Remove NA values
  group1_data <- group1_data[!is.na(group1_data)]
  group2_data <- group2_data[!is.na(group2_data)]

  if(length(group1_data) == 0 || length(group2_data) == 0) {
    return(list(
      metric = metric_name,
      observed_difference = NA,
      p_value = NA,
      null_distribution = NULL,
      effect_size = NA,
      ci_lower = NA,
      ci_upper = NA
    ))
  }

  # Observed test statistic (mean difference)
  observed_diff <- mean(group1_data) - mean(group2_data)

  # Combine data for permutation
  combined_data <- c(group1_data, group2_data)
  n1 <- length(group1_data)
  n_total <- length(combined_data)

  # Generate permutation function
  permute_once <- function(seed = NULL) {
    if(!is.null(seed)) set.seed(seed)

    # Randomly permute labels
    perm_indices <- sample(n_total)
    perm_group1 <- combined_data[perm_indices[1:n1]]
    perm_group2 <- combined_data[perm_indices[(n1+1):n_total]]

    # Compute permuted difference
    mean(perm_group1) - mean(perm_group2)
  }

  # Run permutations (parallel if cores > 1)
  if(parallel_cores > 1) {
    cl <- makeCluster(min(parallel_cores, detectCores() - 1))
    clusterExport(cl, c("combined_data", "n1", "n_total"), envir = environment())

    null_distribution <- parSapply(cl, 1:n_permutations, function(i) {
      perm_indices <- sample(n_total)
      perm_group1 <- combined_data[perm_indices[1:n1]]
      perm_group2 <- combined_data[perm_indices[(n1+1):n_total]]
      mean(perm_group1) - mean(perm_group2)
    })

    stopCluster(cl)
  } else {
    null_distribution <- replicate(n_permutations, permute_once())
  }

  # Compute p-value based on alternative hypothesis
  if(alternative == "two.sided") {
    p_value <- mean(abs(null_distribution) >= abs(observed_diff))
  } else if(alternative == "greater") {
    p_value <- mean(null_distribution >= observed_diff)
  } else if(alternative == "less") {
    p_value <- mean(null_distribution <= observed_diff)
  }

  # Compute effect size (Cohen's d)
  pooled_sd <- sqrt(((length(group1_data) - 1) * var(group1_data) +
                      (length(group2_data) - 1) * var(group2_data)) /
                     (length(group1_data) + length(group2_data) - 2))
  cohens_d <- observed_diff / pooled_sd

  # Confidence interval from permutation distribution
  ci_lower <- quantile(null_distribution, 0.025)
  ci_upper <- quantile(null_distribution, 0.975)

  return(list(
    metric = metric_name,
    observed_difference = observed_diff,
    p_value = p_value,
    null_distribution = null_distribution,
    effect_size = cohens_d,
    ci_lower = ci_lower,
    ci_upper = ci_upper,
    n_permutations = n_permutations,
    alternative = alternative
  ))
}


#' Compute Network-Based Statistics (NBS)
#'
#' Identifies connected subnetworks that differ significantly between groups
#' using permutation testing with family-wise error (FWE) control.
#'
#' @param correlation_matrices_group1 List of correlation matrices for group 1
#' @param correlation_matrices_group2 List of correlation matrices for group 2
#' @param threshold T-statistic threshold for initial edge selection
#' @param n_permutations Number of permutations for FWE correction
#' @param parallel_cores Number of cores for parallel processing
#' @return List containing significant components and statistics
compute_network_based_statistics <- function(correlation_matrices_group1,
                                             correlation_matrices_group2,
                                             threshold = 3.0,
                                             n_permutations = 5000,
                                             parallel_cores = 1) {

  if(length(correlation_matrices_group1) == 0 || length(correlation_matrices_group2) == 0) {
    return(NULL)
  }

  # Get dimensions
  n_nodes <- nrow(correlation_matrices_group1[[1]])
  node_names <- rownames(correlation_matrices_group1[[1]])

  # Initialize edge-wise t-statistic matrix
  t_matrix <- matrix(0, nrow = n_nodes, ncol = n_nodes)
  p_matrix <- matrix(1, nrow = n_nodes, ncol = n_nodes)

  # Compute edge-wise t-tests
  cat("Computing edge-wise t-tests...\n")
  for(i in 1:(n_nodes - 1)) {
    for(j in (i + 1):n_nodes) {
      # Extract edge values across subjects
      edge_vals_g1 <- sapply(correlation_matrices_group1, function(m) m[i, j])
      edge_vals_g2 <- sapply(correlation_matrices_group2, function(m) m[i, j])

      # Perform t-test
      tryCatch({
        test <- t.test(edge_vals_g1, edge_vals_g2)
        t_matrix[i, j] <- test$statistic
        t_matrix[j, i] <- test$statistic
        p_matrix[i, j] <- test$p.value
        p_matrix[j, i] <- test$p.value
      }, error = function(e) {
        # Keep as 0 if test fails
      })
    }
  }

  # Threshold t-matrix and identify components
  suprathreshold_matrix <- (abs(t_matrix) > threshold) * 1
  diag(suprathreshold_matrix) <- 0

  g_supra <- graph_from_adjacency_matrix(suprathreshold_matrix,
                                         mode = "undirected",
                                         diag = FALSE)
  V(g_supra)$name <- node_names

  components <- clusters(g_supra)
  max_component_size <- max(components$csize)

  cat(sprintf("Found %d components, largest has %d edges\n",
              components$no, max_component_size))

  # Permutation testing for FWE correction
  cat(sprintf("Running %d permutations for FWE correction...\n", n_permutations))

  # Combine data for permutation
  all_matrices <- c(correlation_matrices_group1, correlation_matrices_group2)
  n1 <- length(correlation_matrices_group1)
  n_total <- length(all_matrices)

  # Permutation function
  permute_nbs <- function(seed = NULL) {
    if(!is.null(seed)) set.seed(seed)

    # Permute group labels
    perm_indices <- sample(n_total)
    perm_g1_indices <- perm_indices[1:n1]
    perm_g2_indices <- perm_indices[(n1+1):n_total]

    perm_g1 <- all_matrices[perm_g1_indices]
    perm_g2 <- all_matrices[perm_g2_indices]

    # Compute permuted t-matrix
    perm_t_matrix <- matrix(0, nrow = n_nodes, ncol = n_nodes)
    for(i in 1:(n_nodes - 1)) {
      for(j in (i + 1):n_nodes) {
        edge_vals_g1 <- sapply(perm_g1, function(m) m[i, j])
        edge_vals_g2 <- sapply(perm_g2, function(m) m[i, j])

        tryCatch({
          test <- t.test(edge_vals_g1, edge_vals_g2)
          perm_t_matrix[i, j] <- test$statistic
          perm_t_matrix[j, i] <- test$statistic
        }, error = function(e) {})
      }
    }

    # Find largest component in permuted data
    perm_supra <- (abs(perm_t_matrix) > threshold) * 1
    diag(perm_supra) <- 0
    perm_g <- graph_from_adjacency_matrix(perm_supra, mode = "undirected", diag = FALSE)
    perm_components <- clusters(perm_g)

    max(perm_components$csize)
  }

  # Run permutations (parallel if requested)
  if(parallel_cores > 1) {
    cl <- makeCluster(min(parallel_cores, detectCores() - 1))
    clusterExport(cl, c("all_matrices", "n1", "n_total", "n_nodes", "threshold"),
                  envir = environment())
    clusterEvalQ(cl, library(igraph))

    permuted_max_sizes <- parSapply(cl, 1:n_permutations, function(i) {
      perm_indices <- sample(n_total)
      perm_g1_indices <- perm_indices[1:n1]
      perm_g2_indices <- perm_indices[(n1+1):n_total]

      perm_g1 <- all_matrices[perm_g1_indices]
      perm_g2 <- all_matrices[perm_g2_indices]

      perm_t_matrix <- matrix(0, nrow = n_nodes, ncol = n_nodes)
      for(i in 1:(n_nodes - 1)) {
        for(j in (i + 1):n_nodes) {
          edge_vals_g1 <- sapply(perm_g1, function(m) m[i, j])
          edge_vals_g2 <- sapply(perm_g2, function(m) m[i, j])
          tryCatch({
            test <- t.test(edge_vals_g1, edge_vals_g2)
            perm_t_matrix[i, j] <- test$statistic
            perm_t_matrix[j, i] <- test$statistic
          }, error = function(e) {})
        }
      }

      perm_supra <- (abs(perm_t_matrix) > threshold) * 1
      diag(perm_supra) <- 0
      perm_g <- graph_from_adjacency_matrix(perm_supra, mode = "undirected", diag = FALSE)
      perm_components <- clusters(perm_g)
      max(perm_components$csize)
    })

    stopCluster(cl)
  } else {
    permuted_max_sizes <- replicate(n_permutations, permute_nbs())
  }

  # Compute FWE-corrected p-value
  fwe_p_value <- mean(permuted_max_sizes >= max_component_size)

  # Extract significant components (those exceeding 95th percentile of null)
  null_threshold <- quantile(permuted_max_sizes, 0.95)
  significant_component_ids <- which(components$csize > null_threshold)

  # Build result list
  significant_components <- list()
  for(comp_id in significant_component_ids) {
    # Get nodes in this component
    comp_nodes <- node_names[components$membership == comp_id]

    # Get edges in this component
    comp_edges <- data.frame()
    for(i in 1:(length(comp_nodes) - 1)) {
      for(j in (i + 1):length(comp_nodes)) {
        node1 <- comp_nodes[i]
        node2 <- comp_nodes[j]

        idx1 <- which(node_names == node1)
        idx2 <- which(node_names == node2)

        if(suprathreshold_matrix[idx1, idx2] == 1) {
          comp_edges <- rbind(comp_edges, data.frame(
            Node1 = node1,
            Node2 = node2,
            T_statistic = t_matrix[idx1, idx2],
            P_value = p_matrix[idx1, idx2],
            stringsAsFactors = FALSE
          ))
        }
      }
    }

    significant_components[[paste0("Component_", comp_id)]] <- list(
      nodes = comp_nodes,
      edges = comp_edges,
      size = components$csize[comp_id]
    )
  }

  return(list(
    t_matrix = t_matrix,
    p_matrix = p_matrix,
    threshold = threshold,
    components = components,
    max_component_size = max_component_size,
    fwe_p_value = fwe_p_value,
    null_distribution = permuted_max_sizes,
    null_threshold = null_threshold,
    significant_components = significant_components,
    n_significant = length(significant_components)
  ))
}


#' Compute Group Comparison Statistics
#'
#' Comprehensive statistical comparison of network metrics between groups
#'
#' @param results_group1 Analysis results for group 1
#' @param results_group2 Analysis results for group 2
#' @param group1_name Name of group 1
#' @param group2_name Name of group 2
#' @param n_permutations Number of permutations for testing
#' @param parallel_cores Number of cores for parallel processing
#' @return List of statistical comparisons for each metric
compute_group_comparison_statistics <- function(results_group1,
                                               results_group2,
                                               group1_name = "Group1",
                                               group2_name = "Group2",
                                               n_permutations = 5000,
                                               parallel_cores = 1) {

  comparison_results <- list()

  # Global network metrics comparison
  if(!is.null(results_group1$global) && !is.null(results_group2$global)) {
    global_metrics <- c("Density", "Transitivity", "MeanPathLength",
                       "Assortativity", "Modularity", "SmallWorldness")

    global_comparisons <- list()
    for(metric in global_metrics) {
      if(metric %in% names(results_group1$global) &&
         metric %in% names(results_group2$global)) {

        # For single-value metrics, create distributions via bootstrapping
        val1 <- results_group1$global[[metric]]
        val2 <- results_group2$global[[metric]]

        if(length(val1) > 1 && length(val2) > 1) {
          # Already have distributions
          test_result <- compute_permutation_test(
            val1, val2,
            n_permutations = n_permutations,
            metric_name = metric,
            parallel_cores = parallel_cores
          )
          global_comparisons[[metric]] <- test_result
        }
      }
    }

    comparison_results$global_metrics <- global_comparisons
  }

  # Node-level metrics comparison
  if(!is.null(results_group1$nodes) && !is.null(results_group2$nodes)) {
    node_metrics <- c("Strength", "Betweenness", "Closeness", "Eigenvector")

    node_comparisons <- list()
    for(metric in node_metrics) {
      if(metric %in% names(results_group1$nodes) &&
         metric %in% names(results_group2$nodes)) {

        test_result <- compute_permutation_test(
          results_group1$nodes[[metric]],
          results_group2$nodes[[metric]],
          n_permutations = n_permutations,
          metric_name = metric,
          parallel_cores = parallel_cores
        )

        node_comparisons[[metric]] <- test_result
      }
    }

    comparison_results$node_metrics <- node_comparisons
  }

  # Hub overlap analysis
  if(!is.null(results_group1$hubs) && !is.null(results_group2$hubs)) {
    hubs1 <- results_group1$hubs
    hubs2 <- results_group2$hubs

    n_common <- length(intersect(hubs1, hubs2))
    n_unique_g1 <- length(setdiff(hubs1, hubs2))
    n_unique_g2 <- length(setdiff(hubs2, hubs1))

    # Jaccard index
    jaccard <- n_common / length(union(hubs1, hubs2))

    comparison_results$hub_overlap <- list(
      group1_hubs = hubs1,
      group2_hubs = hubs2,
      common_hubs = intersect(hubs1, hubs2),
      unique_to_group1 = setdiff(hubs1, hubs2),
      unique_to_group2 = setdiff(hubs2, hubs1),
      jaccard_index = jaccard,
      n_common = n_common,
      n_unique_g1 = n_unique_g1,
      n_unique_g2 = n_unique_g2
    )
  }

  return(comparison_results)
}


#' Compute Multiple Comparison Correction
#'
#' Applies FDR correction to multiple p-values
#'
#' @param p_values Vector of p-values
#' @param method Correction method: "fdr" (default) or "bonferroni"
#' @return Vector of adjusted p-values
compute_multiple_comparison_correction <- function(p_values, method = "fdr") {
  if(method == "fdr") {
    # Benjamini-Hochberg FDR
    return(p.adjust(p_values, method = "BH"))
  } else if(method == "bonferroni") {
    # Bonferroni correction
    return(p.adjust(p_values, method = "bonferroni"))
  } else {
    return(p_values)
  }
}


#' Compute Effect Size Matrix
#'
#' Computes Cohen's d for all pairwise group comparisons
#'
#' @param group_results List of group results
#' @param metric_name Name of metric to compute effect sizes for
#' @return Matrix of effect sizes
compute_effect_size_matrix <- function(group_results, metric_name = "Strength") {
  group_names <- names(group_results)
  n_groups <- length(group_names)

  effect_size_matrix <- matrix(0, nrow = n_groups, ncol = n_groups)
  rownames(effect_size_matrix) <- group_names
  colnames(effect_size_matrix) <- group_names

  for(i in 1:(n_groups - 1)) {
    for(j in (i + 1):n_groups) {
      group1 <- group_names[i]
      group2 <- group_names[j]

      if(!is.null(group_results[[group1]]$nodes) &&
         !is.null(group_results[[group2]]$nodes)) {

        vals1 <- group_results[[group1]]$nodes[[metric_name]]
        vals2 <- group_results[[group2]]$nodes[[metric_name]]

        # Compute Cohen's d
        mean_diff <- mean(vals1, na.rm = TRUE) - mean(vals2, na.rm = TRUE)
        pooled_sd <- sqrt(((length(vals1) - 1) * var(vals1, na.rm = TRUE) +
                          (length(vals2) - 1) * var(vals2, na.rm = TRUE)) /
                         (length(vals1) + length(vals2) - 2))

        cohens_d <- mean_diff / pooled_sd

        effect_size_matrix[i, j] <- cohens_d
        effect_size_matrix[j, i] <- -cohens_d
      }
    }
  }

  return(effect_size_matrix)
}


# ========================================================================
# TAB 8 STATISTICAL VALIDATION - HELPER FUNCTIONS
# ========================================================================

#' Extract Subject-Level ROI Values for a Specific Group
#'
#' Extracts raw ROI values for all subjects in a given group
#'
#' @param group_name Name of the group to extract
#' @param raw_data The raw subject x ROI data frame (with Group column)
#' @param roi_columns Vector of ROI column names to extract
#' @return Matrix of subject x ROI values (rows = subjects, cols = ROIs)
extract_subject_roi_values <- function(group_name, raw_data, roi_columns = NULL) {

  if(is.null(raw_data) || !"Group" %in% names(raw_data)) {
    stop("raw_data must contain a 'Group' column")
  }

  # Filter to group
  group_data <- raw_data[raw_data$Group == group_name, ]

  if(nrow(group_data) == 0) {
    stop(sprintf("No subjects found for group: %s", group_name))
  }

  # Auto-detect ROI columns if not provided
  if(is.null(roi_columns)) {
    # Exclude non-ROI columns (Group, Subject, ID, behavioral, etc.)
    exclude_patterns <- c("^Group$", "^Subject", "^ID", "^Age", "^Sex",
                         "^Treatment", "^Weight", "^Behavior", "^Loco")
    roi_columns <- names(group_data)

    for(pattern in exclude_patterns) {
      roi_columns <- roi_columns[!grepl(pattern, roi_columns, ignore.case = TRUE)]
    }
  }

  # Extract ROI data only
  roi_matrix <- as.matrix(group_data[, roi_columns, drop = FALSE])

  return(roi_matrix)
}


#' Compute ROI-Level Permutation Tests
#'
#' Performs permutation testing for each ROI independently, comparing two groups
#'
#' @param group1_data Matrix of subject x ROI for group 1
#' @param group2_data Matrix of subject x ROI for group 2
#' @param roi_names Names of ROIs (column names)
#' @param n_permutations Number of permutations (default: 5000)
#' @param correction_method "fdr", "bonferroni", or "none"
#' @param parallel_cores Number of cores for parallel processing
#' @return Data frame with columns: ROI, Mean_Group1, Mean_Group2, Difference,
#'         P_Value, P_Adjusted, Cohen_D, Significant
compute_roi_level_permutation_tests <- function(group1_data,
                                                group2_data,
                                                roi_names,
                                                n_permutations = 5000,
                                                correction_method = "fdr",
                                                parallel_cores = 1) {

  if(ncol(group1_data) != ncol(group2_data)) {
    stop("Group 1 and Group 2 must have the same number of ROIs")
  }

  if(ncol(group1_data) != length(roi_names)) {
    stop("Number of ROI names must match number of columns")
  }

  n_rois <- length(roi_names)
  results_list <- vector("list", n_rois)

  cat(sprintf("Testing %d ROIs with %d permutations...\n", n_rois, n_permutations))

  # Run permutation test for each ROI
  for(i in 1:n_rois) {
    roi_name <- roi_names[i]

    group1_roi <- group1_data[, i]
    group2_roi <- group2_data[, i]

    # Remove NAs
    group1_roi <- group1_roi[!is.na(group1_roi)]
    group2_roi <- group2_roi[!is.na(group2_roi)]

    if(length(group1_roi) == 0 || length(group2_roi) == 0) {
      results_list[[i]] <- data.frame(
        ROI = roi_name,
        Mean_Group1 = NA,
        Mean_Group2 = NA,
        Difference = NA,
        P_Value = NA,
        Cohen_D = NA,
        stringsAsFactors = FALSE
      )
      next
    }

    # Compute permutation test
    perm_result <- compute_permutation_test(
      group1_data = group1_roi,
      group2_data = group2_roi,
      n_permutations = n_permutations,
      metric_name = roi_name,
      alternative = "two.sided",
      parallel_cores = parallel_cores
    )

    results_list[[i]] <- data.frame(
      ROI = roi_name,
      Mean_Group1 = mean(group1_roi, na.rm = TRUE),
      Mean_Group2 = mean(group2_roi, na.rm = TRUE),
      Difference = perm_result$observed_difference,
      P_Value = perm_result$p_value,
      Cohen_D = perm_result$effect_size,
      stringsAsFactors = FALSE
    )
  }

  # Combine results
  results_df <- do.call(rbind, results_list)

  # Apply multiple comparison correction
  if(correction_method != "none") {
    results_df$P_Adjusted <- compute_multiple_comparison_correction(
      results_df$P_Value,
      method = correction_method
    )
  } else {
    results_df$P_Adjusted <- results_df$P_Value
  }

  # Mark significant ROIs (adjusted p < 0.05)
  results_df$Significant <- results_df$P_Adjusted < 0.05

  # Sort by adjusted p-value
  results_df <- results_df[order(results_df$P_Adjusted), ]

  return(results_df)
}


#' Extract Hub Nodes from Analysis Results
#'
#' Extracts hub nodes (z-scored eigenvector centrality >= threshold)
#' for specified approach and method
#'
#' @param approach "Weighted", "Percolation", or "Persistence"
#' @param method_name "pearson", "spearman", "biweight", "shrinkage", or "partial"
#' @param group_name Name of the group
#' @param method_weighted_results Weighted analysis results
#' @param method_percolation_results Percolation analysis results
#' @param auc_results Persistence (AUC) analysis results
#' @param z_threshold Z-score threshold for hub identification (default: 1.5)
#' @return Character vector of hub node names
extract_hubs_from_approach <- function(approach,
                                       method_name,
                                       group_name,
                                       method_weighted_results,
                                       method_percolation_results,
                                       auc_results,
                                       z_threshold = 1.5) {

  hubs <- character(0)

  if(approach == "Weighted") {
    # Extract from weighted_eigenvector - FIX: filter combined df by group
    if(!is.null(method_weighted_results[[method_name]]$weighted_eigenvector)) {
      weighted_all <- method_weighted_results[[method_name]]$weighted_eigenvector
      eig_data <- weighted_all[weighted_all$Group == group_name, ]

      if(nrow(eig_data) > 0 && "Weighted_Eigenvector" %in% names(eig_data)) {
        # Z-score
        z_scores <- (eig_data$Weighted_Eigenvector - mean(eig_data$Weighted_Eigenvector, na.rm = TRUE)) /
                    sd(eig_data$Weighted_Eigenvector, na.rm = TRUE)

        hubs <- eig_data$Node[z_scores >= z_threshold]
      }
    }

  } else if(approach == "Percolation") {
    # Extract from percolation node_metrics
    if(!is.null(method_percolation_results[[method_name]]) &&
       !is.null(method_percolation_results[[method_name]]$node_metrics)) {

      nodes_data <- method_percolation_results[[method_name]]$node_metrics
      group_nodes <- nodes_data[nodes_data$Group == group_name, ]

      if(nrow(group_nodes) > 0 && "Eigenvector" %in% names(group_nodes)) {
        # Z-score
        z_scores <- (group_nodes$Eigenvector - mean(group_nodes$Eigenvector, na.rm = TRUE)) /
                    sd(group_nodes$Eigenvector, na.rm = TRUE)

        hubs <- group_nodes$Node[z_scores >= z_threshold]
      }
    }

  } else if(approach == "Persistence") {
    # Extract from AUC_Eigenvector
    if(!is.null(auc_results[[method_name]]) &&
       !is.null(auc_results[[method_name]][[group_name]]) &&
       !is.null(auc_results[[method_name]][[group_name]]$node_auc)) {

      auc_data <- auc_results[[method_name]][[group_name]]$node_auc

      if("AUC_Eigenvector" %in% names(auc_data)) {
        # Z-score
        z_scores <- (auc_data$AUC_Eigenvector - mean(auc_data$AUC_Eigenvector, na.rm = TRUE)) /
                    sd(auc_data$AUC_Eigenvector, na.rm = TRUE)

        hubs <- auc_data$Node[z_scores >= z_threshold]
      }
    }
  }

  return(hubs)
}


#' Compute Hub Overlap Statistics
#'
#' Computes Jaccard indices and overlap statistics for hub sets
#'
#' @param hub_sets Named list of character vectors (hub names per group)
#' @return List containing jaccard_matrix, pairwise_details, and summary_stats
compute_hub_overlap_statistics <- function(hub_sets) {

  group_names <- names(hub_sets)
  n_groups <- length(group_names)

  if(n_groups < 2) {
    stop("Need at least 2 groups to compute overlap")
  }

  # Initialize Jaccard matrix
  jaccard_matrix <- matrix(1, nrow = n_groups, ncol = n_groups)
  rownames(jaccard_matrix) <- group_names
  colnames(jaccard_matrix) <- group_names

  # Initialize pairwise details list
  pairwise_details <- list()

  # Compute pairwise Jaccard indices
  for(i in 1:(n_groups - 1)) {
    for(j in (i + 1):n_groups) {
      group1 <- group_names[i]
      group2 <- group_names[j]

      hubs1 <- hub_sets[[group1]]
      hubs2 <- hub_sets[[group2]]

      # Compute overlap
      common_hubs <- intersect(hubs1, hubs2)
      unique_to_g1 <- setdiff(hubs1, hubs2)
      unique_to_g2 <- setdiff(hubs2, hubs1)
      union_hubs <- union(hubs1, hubs2)

      # Jaccard index
      if(length(union_hubs) == 0) {
        jaccard <- 0
      } else {
        jaccard <- length(common_hubs) / length(union_hubs)
      }

      jaccard_matrix[i, j] <- jaccard
      jaccard_matrix[j, i] <- jaccard

      # Store details
      pair_key <- paste(group1, group2, sep = "_vs_")
      pairwise_details[[pair_key]] <- list(
        group1 = group1,
        group2 = group2,
        hubs1 = hubs1,
        hubs2 = hubs2,
        common_hubs = common_hubs,
        unique_to_group1 = unique_to_g1,
        unique_to_group2 = unique_to_g2,
        n_common = length(common_hubs),
        n_unique_g1 = length(unique_to_g1),
        n_unique_g2 = length(unique_to_g2),
        jaccard_index = jaccard
      )
    }
  }

  # Summary statistics
  all_jaccard <- jaccard_matrix[lower.tri(jaccard_matrix)]
  summary_stats <- list(
    mean_jaccard = mean(all_jaccard, na.rm = TRUE),
    median_jaccard = median(all_jaccard, na.rm = TRUE),
    min_jaccard = min(all_jaccard, na.rm = TRUE),
    max_jaccard = max(all_jaccard, na.rm = TRUE),
    n_comparisons = length(all_jaccard)
  )

  return(list(
    jaccard_matrix = jaccard_matrix,
    pairwise_details = pairwise_details,
    summary_stats = summary_stats,
    hub_sets = hub_sets
  ))
}


#' Compute Global Network Permutation Test
#'
#' Tests whether global network metrics differ significantly between two groups
#' using permutation testing. This recomputes correlation matrices and networks
#' for each permutation.
#'
#' @param group1_data Matrix of subject × ROI data for group 1 (from extract_subject_roi_values)
#' @param group2_data Matrix of subject × ROI data for group 2
#' @param group1_name Character string name of group 1
#' @param group2_name Character string name of group 2
#' @param correlation_method Character: "pearson", "spearman", "biweight", "shrinkage", or "partial"
#' @param threshold Numeric threshold for binarizing networks (if NULL, uses weighted networks)
#' @param n_permutations Integer number of permutations (default: 1000, max recommended: 5000)
#' @param parallel_cores Integer number of CPU cores for parallel processing (default: 1)
#'
#' @return List containing:
#'   - metrics: Character vector of metric names
#'   - observed: Named vector of observed group differences
#'   - p_values: Named vector of permutation p-values
#'   - null_distributions: Matrix of permuted differences (n_permutations × n_metrics)
#'
#' @details
#' For each permutation:
#' 1. Pool subjects from both groups
#' 2. Randomly reassign to new groups (maintaining original group sizes)
#' 3. Compute correlation matrix for each permuted group
#' 4. Create network and compute global metrics:
#'    - Density: proportion of edges / possible edges
#'    - Clustering: average local clustering coefficient
#'    - Path Length: average shortest path length (weighted networks)
#'    - Modularity: community structure strength (binary networks)
#' 5. Store difference: metric_group1 - metric_group2
#' 6. P-value = proportion of permuted differences ≥ observed
#'
#' @note Computationally expensive: 1000 permutations takes ~5-10 minutes
#'
compute_global_network_permutation_test <- function(group1_data,
                                                     group2_data,
                                                     group1_name,
                                                     group2_name,
                                                     correlation_method = "pearson",
                                                     threshold = NULL,
                                                     n_permutations = 1000,
                                                     parallel_cores = 1) {

  # Input validation
  if(!is.matrix(group1_data)) group1_data <- as.matrix(group1_data)
  if(!is.matrix(group2_data)) group2_data <- as.matrix(group2_data)

  if(ncol(group1_data) != ncol(group2_data)) {
    stop("group1_data and group2_data must have same number of ROIs")
  }

  n1 <- nrow(group1_data)
  n2 <- nrow(group2_data)
  n_total <- n1 + n2

  if(n1 < 3 || n2 < 3) {
    stop("Each group must have at least 3 subjects for network construction")
  }

  # Pool all subjects
  pooled_data <- rbind(group1_data, group2_data)

  # Helper function to compute global network metrics
  compute_network_metrics <- function(subject_data, method, thresh = NULL) {
    # Compute correlation matrix
    cor_mat <- tryCatch({
      if(method == "pearson") {
        cor(subject_data, use = "pairwise.complete.obs")
      } else if(method == "spearman") {
        cor(subject_data, use = "pairwise.complete.obs", method = "spearman")
      } else if(method == "biweight") {
        # Use biweight midcorrelation if available
        if(requireNamespace("WGCNA", quietly = TRUE)) {
          WGCNA::bicor(subject_data, use = "pairwise.complete.obs")
        } else {
          cor(subject_data, use = "pairwise.complete.obs")  # Fallback
        }
      } else if(method == "shrinkage") {
        # Use shrinkage correlation if available
        if(requireNamespace("corpcor", quietly = TRUE)) {
          corpcor::cor.shrink(subject_data, verbose = FALSE)
        } else {
          cor(subject_data, use = "pairwise.complete.obs")  # Fallback
        }
      } else if(method == "partial") {
        # Partial correlation requires n > p + 5
        if(nrow(subject_data) > ncol(subject_data) + 5) {
          if(requireNamespace("corpcor", quietly = TRUE)) {
            # Compute partial correlations from precision matrix
            precision_mat <- corpcor::cor2pcor(cor(subject_data, use = "pairwise.complete.obs"))
            -cov2cor(precision_mat)  # Convert precision to partial correlation
          } else {
            cor(subject_data, use = "pairwise.complete.obs")  # Fallback
          }
        } else {
          cor(subject_data, use = "pairwise.complete.obs")  # Fallback
        }
      } else {
        cor(subject_data, use = "pairwise.complete.obs")  # Default fallback
      }
    }, error = function(e) {
      cor(subject_data, use = "pairwise.complete.obs")  # Fallback on error
    })

    # Set diagonal to 0 (no self-loops)
    diag(cor_mat) <- 0

    # Convert to absolute values for network construction
    abs_cor_mat <- abs(cor_mat)

    # Create network
    if(!is.null(thresh)) {
      # Binary network: threshold
      adj_mat <- ifelse(abs_cor_mat >= thresh, 1, 0)
      diag(adj_mat) <- 0
    } else {
      # Weighted network
      adj_mat <- abs_cor_mat
      diag(adj_mat) <- 0
    }

    # Compute metrics
    n_nodes <- nrow(adj_mat)
    n_possible_edges <- n_nodes * (n_nodes - 1) / 2

    # Density
    if(!is.null(thresh)) {
      n_edges <- sum(adj_mat > 0) / 2  # Binary
    } else {
      n_edges <- sum(abs_cor_mat > 0.1) / 2  # Count edges > 0.1 for weighted
    }
    density <- n_edges / n_possible_edges

    # Clustering coefficient (weighted)
    clustering <- mean(compute_local_clustering(adj_mat), na.rm = TRUE)

    # Average path length (for connected networks)
    path_length <- tryCatch({
      # Create igraph object for path length calculation
      if(requireNamespace("igraph", quietly = TRUE)) {
        g <- igraph::graph_from_adjacency_matrix(adj_mat, mode = "undirected",
                                                  weighted = if(is.null(thresh)) TRUE else NULL)
        if(igraph::is_connected(g)) {
          igraph::mean_distance(g, directed = FALSE)
        } else {
          NA_real_  # Disconnected network
        }
      } else {
        NA_real_
      }
    }, error = function(e) NA_real_)

    # Modularity (binary networks only, using Louvain algorithm)
    modularity <- tryCatch({
      if(!is.null(thresh) && requireNamespace("igraph", quietly = TRUE)) {
        g <- igraph::graph_from_adjacency_matrix(adj_mat, mode = "undirected", weighted = NULL)
        if(igraph::vcount(g) > 2 && igraph::ecount(g) > 0) {
          community <- igraph::cluster_louvain(g)
          igraph::modularity(community)
        } else {
          NA_real_
        }
      } else {
        NA_real_
      }
    }, error = function(e) NA_real_)

    return(c(
      Density = density,
      Clustering = clustering,
      Path_Length = path_length,
      Modularity = modularity
    ))
  }

  # Helper function: compute local clustering coefficient
  compute_local_clustering <- function(adj_mat) {
    n <- nrow(adj_mat)
    clustering <- numeric(n)

    for(i in 1:n) {
      neighbors <- which(adj_mat[i, ] > 0)
      k <- length(neighbors)

      if(k < 2) {
        clustering[i] <- 0
        next
      }

      # Count connections among neighbors (weighted)
      neighbor_weights <- sum(adj_mat[neighbors, neighbors]) / 2
      possible_connections <- k * (k - 1) / 2

      clustering[i] <- neighbor_weights / possible_connections
    }

    return(clustering)
  }

  # Compute observed metrics
  obs_group1 <- compute_network_metrics(group1_data, correlation_method, threshold)
  obs_group2 <- compute_network_metrics(group2_data, correlation_method, threshold)
  observed_diff <- obs_group1 - obs_group2

  # Initialize storage for permutation results
  metric_names <- names(observed_diff)
  null_diffs <- matrix(NA, nrow = n_permutations, ncol = length(metric_names))
  colnames(null_diffs) <- metric_names

  # Run permutations (with optional parallel processing)
  if(parallel_cores > 1 && requireNamespace("parallel", quietly = TRUE)) {
    # Parallel version
    cl <- parallel::makeCluster(parallel_cores)
    parallel::clusterExport(cl, c("compute_network_metrics", "compute_local_clustering",
                                   "pooled_data", "n1", "correlation_method", "threshold"),
                           envir = environment())

    perm_results <- parallel::parLapply(cl, 1:n_permutations, function(i) {
      # Randomly permute group assignments
      perm_indices <- sample(1:nrow(pooled_data))
      perm_group1 <- pooled_data[perm_indices[1:n1], , drop = FALSE]
      perm_group2 <- pooled_data[perm_indices[(n1+1):nrow(pooled_data)], , drop = FALSE]

      # Compute metrics
      perm_metrics1 <- compute_network_metrics(perm_group1, correlation_method, threshold)
      perm_metrics2 <- compute_network_metrics(perm_group2, correlation_method, threshold)

      return(perm_metrics1 - perm_metrics2)
    })

    parallel::stopCluster(cl)

    # Convert list to matrix
    for(i in 1:n_permutations) {
      null_diffs[i, ] <- perm_results[[i]]
    }

  } else {
    # Sequential version
    for(i in 1:n_permutations) {
      # Randomly permute group assignments
      perm_indices <- sample(1:nrow(pooled_data))
      perm_group1 <- pooled_data[perm_indices[1:n1], , drop = FALSE]
      perm_group2 <- pooled_data[perm_indices[(n1+1):nrow(pooled_data)], , drop = FALSE]

      # Compute metrics
      perm_metrics1 <- compute_network_metrics(perm_group1, correlation_method, threshold)
      perm_metrics2 <- compute_network_metrics(perm_group2, correlation_method, threshold)

      null_diffs[i, ] <- perm_metrics1 - perm_metrics2
    }
  }

  # Compute p-values (two-tailed)
  p_values <- numeric(length(metric_names))
  names(p_values) <- metric_names

  for(j in 1:length(metric_names)) {
    metric <- metric_names[j]
    obs <- observed_diff[metric]
    null_vals <- null_diffs[, metric]

    # Remove NAs from null distribution
    null_vals <- null_vals[!is.na(null_vals)]

    if(length(null_vals) > 0) {
      # Two-tailed p-value
      p_values[metric] <- sum(abs(null_vals) >= abs(obs)) / length(null_vals)
    } else {
      p_values[metric] <- NA_real_
    }
  }

  return(list(
    group1_name = group1_name,
    group2_name = group2_name,
    correlation_method = correlation_method,
    threshold = threshold,
    n_permutations = n_permutations,
    observed_group1 = obs_group1,
    observed_group2 = obs_group2,
    observed_diff = observed_diff,
    p_values = p_values,
    null_distributions = null_diffs
  ))
}


# ========================================================================
# REGIONAL CONTRIBUTION ANALYSIS - Multi-Method Leave-One-Out Permutation Testing
# Mirrors the computation in Tab C "Group Similarity Across All Methods"
# ========================================================================

#' Compute Jaccard Similarity Between Two Network Matrices
#'
#' Internal helper function for regional contribution analysis.
#' Uses weighted Jaccard: sum(min) / sum(max) on absolute values.
#'
#' @param mat1 Network matrix for group 1
#' @param mat2 Network matrix for group 2
#' @return Numeric Jaccard similarity value (0-1)
compute_jaccard_for_contribution <- function(mat1, mat2) {
  if(!all(dim(mat1) == dim(mat2))) {
    return(NA)
  }

  weights1 <- abs(mat1)
  weights2 <- abs(mat2)

  diag(weights1) <- 0
  diag(weights2) <- 0

  min_weights <- pmin(weights1, weights2)
  max_weights <- pmax(weights1, weights2)

  numerator <- sum(min_weights, na.rm = TRUE)
  denominator <- sum(max_weights, na.rm = TRUE)

  if(denominator == 0) return(0)
  return(numerator / denominator)
}


#' Extract Network Matrices for All Method-Approach Combinations
#'
#' Internal helper that mirrors Tab C's network extraction logic.
#' Returns a list of network matrices for a given group across all available
#' method-approach combinations.
#'
#' @param group Character name of the group
#' @param analysis_results The analysis_results reactive containing all data
#' @return List of network matrices keyed by "method_approach"
extract_all_network_matrices <- function(group, analysis_results, methods = NULL) {
  # Use provided methods or default to all 6
  if(is.null(methods)) {
    methods <- c("pearson", "spearman", "kendall", "biweight", "shrinkage", "partial")
  }
  approaches <- c("weighted", "percolation", "persistence")

  networks <- list()

  for(method in methods) {
    for(approach in approaches) {
      network_mat <- NULL
      key <- paste(method, approach, sep = "_")

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
      } else if(approach == "persistence") {
        # PROPER PERSISTENCE: Create network at EACH threshold, not just median
        # This allows averaging Jaccard across all thresholds (true persistence approach)
        if(!is.null(analysis_results$persistence_results[[method]][[group]]$persistence_data) &&
           !is.null(analysis_results$correlation_methods_raw[[method]][[group]])) {
          pers_data <- analysis_results$persistence_results[[method]][[group]]$persistence_data
          threshold_vals <- as.numeric(names(pers_data))
          cor_mat <- abs(analysis_results$correlation_methods_raw[[method]][[group]])

          # Create a separate network for each threshold
          for(thresh in threshold_vals) {
            thresh_mat <- cor_mat
            thresh_mat[thresh_mat < thresh] <- 0
            # Key includes threshold: e.g., "pearson_persistence_0.15"
            thresh_key <- paste(method, approach, thresh, sep = "_")
            networks[[thresh_key]] <- thresh_mat
          }
        }
        # Skip the normal key assignment for persistence (handled above)
        next
      }

      if(!is.null(network_mat)) {
        networks[[key]] <- network_mat
      }
    }
  }

  return(networks)
}


#' Compute Approach-Weighted Averaged Jaccard
#'
#' Computes Jaccard similarity with equal weighting per approach (33% each).
#' Each approach (weighted, percolation, persistence) contributes equally,
#' regardless of how many networks it contains.
#'
#' @param networks_group1 List of network matrices for group 1
#' @param networks_group2 List of network matrices for group 2
#' @param exclude_nodes Optional vector of node names to exclude (for leave-one-out)
#' @return List with avg_jaccard, n_combinations, and n_approaches
compute_averaged_jaccard <- function(networks_group1, networks_group2, exclude_nodes = NULL) {
  # Find common method-approach combinations
  common_keys <- intersect(names(networks_group1), names(networks_group2))

  if(length(common_keys) == 0) {
    return(list(avg_jaccard = NA, n_combinations = 0, n_approaches = 0))
  }

  # Group keys by approach type (reactive - finds whatever keys exist)
  weighted_keys <- grep("_weighted$", common_keys, value = TRUE)
  percolation_keys <- grep("_percolation$", common_keys, value = TRUE)
  persistence_keys <- grep("_persistence_", common_keys, value = TRUE)

  # Helper function to compute average Jaccard for a set of keys
  compute_approach_avg <- function(keys) {
    if(length(keys) == 0) return(NA)

    jaccards <- c()
    for(key in keys) {
      mat1 <- networks_group1[[key]]
      mat2 <- networks_group2[[key]]

      # Apply node exclusion if specified
      if(!is.null(exclude_nodes) && length(exclude_nodes) > 0) {
        node_names <- rownames(mat1)
        if(is.null(node_names)) node_names <- colnames(mat1)
        if(!is.null(node_names)) {
          keep_idx <- which(!(node_names %in% exclude_nodes))
          if(length(keep_idx) >= 3) {
            mat1 <- mat1[keep_idx, keep_idx, drop = FALSE]
            mat2 <- mat2[keep_idx, keep_idx, drop = FALSE]
          } else {
            next  # Skip if too few nodes remain
          }
        }
      }

      j <- compute_jaccard_for_contribution(mat1, mat2)
      if(!is.na(j)) jaccards <- c(jaccards, j)
    }

    if(length(jaccards) == 0) return(NA)
    return(mean(jaccards, na.rm = TRUE))
  }

  # Compute average Jaccard for each approach
  weighted_avg <- compute_approach_avg(weighted_keys)
  percolation_avg <- compute_approach_avg(percolation_keys)
  persistence_avg <- compute_approach_avg(persistence_keys)

  # Equal weight per approach (33% each)
  approach_avgs <- c(weighted_avg, percolation_avg, persistence_avg)
  approach_avgs <- approach_avgs[!is.na(approach_avgs)]

  if(length(approach_avgs) == 0) {
    return(list(avg_jaccard = NA, n_combinations = 0, n_approaches = 0))
  }

  # Count total combinations for reporting
  n_total <- length(weighted_keys) + length(percolation_keys) + length(persistence_keys)

  return(list(
    avg_jaccard = mean(approach_avgs, na.rm = TRUE),
    n_combinations = n_total,
    n_approaches = length(approach_avgs)
  ))
}


#' Compute Multi-Method Leave-One-Out Contribution
#'
#' For each region, removes it from ALL network matrices across ALL method-approach
#' combinations, then recomputes the averaged Jaccard. This mirrors Tab C's
#' multi-method aggregation approach.
#'
#' @param analysis_results The analysis_results reactive containing all data
#' @param group1 Name of group 1
#' @param group2 Name of group 2
#' @param brain_areas Named list mapping collective regions to subregions
#' @param analysis_level "subregion" or "collective"
#' @return Data frame with Region, Original_Jaccard, Jaccard_Without, Contribution_Score, Pct_Change, N_Combinations
compute_multimethod_leave_one_out_contribution <- function(analysis_results,
                                                            group1,
                                                            group2,
                                                            brain_areas,
                                                            analysis_level = "subregion",
                                                            methods = NULL) {

  # Extract all network matrices for both groups (using selected methods)
  networks_g1 <- extract_all_network_matrices(group1, analysis_results, methods = methods)
  networks_g2 <- extract_all_network_matrices(group2, analysis_results, methods = methods)

  if(length(networks_g1) == 0 || length(networks_g2) == 0) {
    return(data.frame(
      Region = character(),
      Original_Jaccard = numeric(),
      Jaccard_Without = numeric(),
      Contribution_Score = numeric(),
      Pct_Change = numeric(),
      N_Combinations = integer(),
      stringsAsFactors = FALSE
    ))
  }

  # Get node names from first available matrix
  first_mat <- networks_g1[[1]]
  node_names <- rownames(first_mat)
  if(is.null(node_names)) node_names <- colnames(first_mat)
  if(is.null(node_names)) node_names <- paste0("Node_", 1:nrow(first_mat))

  # Determine regions to analyze
  if(analysis_level == "subregion") {
    regions_to_test <- node_names
  } else {
    regions_to_test <- names(brain_areas)
  }

  # Compute original averaged Jaccard (no exclusion)
  original_result <- compute_averaged_jaccard(networks_g1, networks_g2, exclude_nodes = NULL)
  original_jaccard <- original_result$avg_jaccard
  n_combinations <- original_result$n_combinations

  if(is.na(original_jaccard)) {
    return(data.frame(
      Region = character(),
      Original_Jaccard = numeric(),
      Jaccard_Without = numeric(),
      Contribution_Score = numeric(),
      Pct_Change = numeric(),
      N_Combinations = integer(),
      stringsAsFactors = FALSE
    ))
  }

  # Initialize results
  results <- data.frame(
    Region = character(),
    Original_Jaccard = numeric(),
    Jaccard_Without = numeric(),
    Contribution_Score = numeric(),
    Pct_Change = numeric(),
    N_Combinations = integer(),
    stringsAsFactors = FALSE
  )

  for(region in regions_to_test) {
    # Determine nodes to exclude
    if(analysis_level == "subregion") {
      exclude_nodes <- region
    } else {
      exclude_nodes <- brain_areas[[region]]
      if(is.null(exclude_nodes)) next
    }

    # Compute averaged Jaccard without this region
    loo_result <- compute_averaged_jaccard(networks_g1, networks_g2, exclude_nodes = exclude_nodes)
    jaccard_without <- loo_result$avg_jaccard

    if(is.na(jaccard_without)) next

    # Contribution score: positive = removing region INCREASES similarity
    # = region was contributing to dissimilarity
    contribution <- jaccard_without - original_jaccard
    pct_change <- if(original_jaccard > 0) (contribution / original_jaccard) * 100 else 0

    results <- rbind(results, data.frame(
      Region = region,
      Original_Jaccard = original_jaccard,
      Jaccard_Without = jaccard_without,
      Contribution_Score = contribution,
      Pct_Change = pct_change,
      N_Combinations = n_combinations,
      stringsAsFactors = FALSE
    ))
  }

  # Sort by absolute contribution (most impactful first)
  if(nrow(results) > 0) {
    results <- results[order(-abs(results$Contribution_Score)), ]
  }

  return(results)
}


#' Multi-Method Regional Jaccard
#'
#' Computes averaged regional Jaccard across all method-approach combinations.
#'
#' @param analysis_results The analysis_results reactive
#' @param group1 Name of group 1
#' @param group2 Name of group 2
#' @param brain_areas Named list mapping collective regions to subregions
#' @param analysis_level "subregion" or "collective"
#' @return Data frame with Region, Regional_Jaccard
compute_multimethod_regional_jaccard <- function(analysis_results,
                                                  group1,
                                                  group2,
                                                  brain_areas,
                                                  analysis_level = "subregion",
                                                  methods = NULL) {

  # Extract all network matrices for both groups (using selected methods)
  networks_g1 <- extract_all_network_matrices(group1, analysis_results, methods = methods)
  networks_g2 <- extract_all_network_matrices(group2, analysis_results, methods = methods)

  if(length(networks_g1) == 0 || length(networks_g2) == 0) {
    return(data.frame(Region = character(), Regional_Jaccard = numeric(), stringsAsFactors = FALSE))
  }

  # Get node names from first available matrix
  first_mat <- networks_g1[[1]]
  node_names <- rownames(first_mat)
  if(is.null(node_names)) node_names <- colnames(first_mat)
  if(is.null(node_names)) node_names <- paste0("Node_", 1:nrow(first_mat))

  # Determine regions to analyze
  if(analysis_level == "subregion") {
    regions_to_test <- node_names
  } else {
    regions_to_test <- names(brain_areas)
  }

  common_keys <- intersect(names(networks_g1), names(networks_g2))

  results <- data.frame(
    Region = character(),
    Regional_Jaccard = numeric(),
    stringsAsFactors = FALSE
  )

  for(region in regions_to_test) {
    if(analysis_level == "subregion") {
      region_nodes <- region
    } else {
      region_nodes <- brain_areas[[region]]
      if(is.null(region_nodes)) next
    }

    region_idx <- which(node_names %in% region_nodes)
    if(length(region_idx) == 0) next

    # Compute regional Jaccard for each method-approach and average
    regional_jaccards <- numeric()

    for(key in common_keys) {
      mat1 <- networks_g1[[key]]
      mat2 <- networks_g2[[key]]

      # Extract edges involving this region
      weights1_region <- abs(mat1[region_idx, , drop = FALSE])
      weights2_region <- abs(mat2[region_idx, , drop = FALSE])

      # Zero out self-connections
      for(i in seq_along(region_idx)) {
        weights1_region[i, region_idx[i]] <- 0
        weights2_region[i, region_idx[i]] <- 0
      }

      min_weights <- pmin(weights1_region, weights2_region)
      max_weights <- pmax(weights1_region, weights2_region)

      numerator <- sum(min_weights, na.rm = TRUE)
      denominator <- sum(max_weights, na.rm = TRUE)

      if(denominator > 0) {
        regional_jaccards <- c(regional_jaccards, numerator / denominator)
      }
    }

    if(length(regional_jaccards) > 0) {
      results <- rbind(results, data.frame(
        Region = region,
        Regional_Jaccard = mean(regional_jaccards),
        stringsAsFactors = FALSE
      ))
    }
  }

  return(results)
}


#' Multi-Method Permutation Test for Regional Contribution
#'
#' Performs permutation testing on the multi-method averaged leave-one-out
#' contribution scores. Permutes node labels consistently across all matrices.
#'
#' @param analysis_results The analysis_results reactive
#' @param group1 Name of group 1
#' @param group2 Name of group 2
#' @param brain_areas Named list mapping collective regions to subregions
#' @param analysis_level "subregion" or "collective"
#' @param n_permutations Number of permutations (default: 1000)
#' @param correction_method "fdr", "bonferroni", or "none"
#' @return List with observed contributions, null distributions, p-values
compute_multimethod_contribution_permutation_test <- function(analysis_results,
                                                               group1,
                                                               group2,
                                                               brain_areas,
                                                               analysis_level = "subregion",
                                                               n_permutations = 1000,
                                                               correction_method = "fdr",
                                                               progress_callback = NULL,
                                                               methods = NULL,
                                                               use_parallel = FALSE,
                                                               n_workers = NULL) {

  # Setup parallel processing if requested
  if(use_parallel && PARALLEL_AVAILABLE) {
    # Determine number of workers
    if(is.null(n_workers)) {
      n_workers <- max(1, parallel::detectCores() - 1)
    }
    # Set up future plan
    old_plan <- future::plan()
    future::plan(future::multisession, workers = n_workers)
    # Ensure we restore the old plan when function exits
    on.exit(future::plan(old_plan), add = TRUE)
  }

  # SYMMETRIC: Sort groups alphabetically for internal processing

  # This ensures the same seed produces identical results regardless of input order
  groups_sorted <- sort(c(group1, group2))
  internal_g1 <- groups_sorted[1]
  internal_g2 <- groups_sorted[2]

  # Compute observed contribution scores (using original order - Jaccard is symmetric)
  observed <- compute_multimethod_leave_one_out_contribution(
    analysis_results, group1, group2, brain_areas, analysis_level,
    methods = methods
  )

  if(nrow(observed) == 0) {
    return(list(
      observed = observed,
      null_distributions = NULL,
      n_permutations = n_permutations,
      n_combinations = 0,
      analysis_level = analysis_level,
      correction_method = correction_method
    ))
  }

  regions <- observed$Region
  n_regions <- length(regions)
  n_combinations <- observed$N_Combinations[1]

  # Initialize null distribution storage
  null_distributions <- matrix(NA, nrow = n_permutations, ncol = n_regions)
  colnames(null_distributions) <- regions

  # Extract all network matrices for both groups (using sorted order for symmetry)
  networks_g1 <- extract_all_network_matrices(internal_g1, analysis_results, methods)
  networks_g2 <- extract_all_network_matrices(internal_g2, analysis_results, methods)

  # Get node names and count
  first_mat <- networks_g1[[1]]
  node_names <- rownames(first_mat)
  if(is.null(node_names)) node_names <- colnames(first_mat)
  n_nodes <- length(node_names)

  # Helper function for single permutation
  run_single_perm_contribution <- function(perm_idx, networks_g1, networks_g2,
                                            node_names, n_nodes, regions,
                                            analysis_level, brain_areas) {
    # Generate INDEPENDENT permutation indices for each group
    perm_idx_g1 <- sample(n_nodes)
    perm_idx_g2 <- sample(n_nodes)

    # Permute Group 1
    permuted_g1 <- list()
    for(key in names(networks_g1)) {
      perm_mat <- networks_g1[[key]][perm_idx_g1, perm_idx_g1]
      rownames(perm_mat) <- node_names
      colnames(perm_mat) <- node_names
      permuted_g1[[key]] <- perm_mat
    }

    # Permute Group 2
    permuted_g2 <- list()
    for(key in names(networks_g2)) {
      perm_mat <- networks_g2[[key]][perm_idx_g2, perm_idx_g2]
      rownames(perm_mat) <- node_names
      colnames(perm_mat) <- node_names
      permuted_g2[[key]] <- perm_mat
    }

    # Compute original averaged Jaccard for permuted data
    perm_original <- compute_averaged_jaccard(permuted_g1, permuted_g2, exclude_nodes = NULL)$avg_jaccard

    # Compute leave-one-out for each region
    perm_results <- numeric(length(regions))
    names(perm_results) <- regions

    for(j in seq_along(regions)) {
      region <- regions[j]
      if(analysis_level == "subregion") {
        exclude_nodes <- region
      } else {
        exclude_nodes <- brain_areas[[region]]
        if(is.null(exclude_nodes)) {
          perm_results[j] <- NA
          next
        }
      }

      perm_loo <- compute_averaged_jaccard(permuted_g1, permuted_g2, exclude_nodes = exclude_nodes)$avg_jaccard

      if(!is.na(perm_original) && !is.na(perm_loo)) {
        perm_results[j] <- perm_loo - perm_original
      } else {
        perm_results[j] <- NA
      }
    }
    return(perm_results)
  }

  # Run permutations (SYMMETRIC: permute BOTH groups independently each iteration)
  # This ensures A vs B gives IDENTICAL results to B vs A
  # Note: Parallelization at this level has high overhead; keep serial
  for(perm in 1:n_permutations) {
    # Update progress if callback provided
    if(!is.null(progress_callback)) {
      progress_callback(perm / n_permutations)
    }
    null_distributions[perm, ] <- run_single_perm_contribution(
      perm, networks_g1, networks_g2, node_names, n_nodes, regions,
      analysis_level, brain_areas
    )
  }

  # Compute p-values (two-tailed)
  p_values <- numeric(n_regions)
  names(p_values) <- regions

  for(i in seq_along(regions)) {
    region <- regions[i]
    obs_val <- observed$Contribution_Score[observed$Region == region]
    null_vals <- null_distributions[, region]
    null_vals <- null_vals[!is.na(null_vals)]

    if(length(null_vals) > 0 && length(obs_val) > 0) {
      p_values[region] <- (sum(abs(null_vals) >= abs(obs_val)) + 1) / (length(null_vals) + 1)
    } else {
      p_values[region] <- NA
    }
  }

  # Apply correction
  if(correction_method == "fdr") {
    p_adjusted <- p.adjust(p_values, method = "BH")
  } else if(correction_method == "bonferroni") {
    p_adjusted <- p.adjust(p_values, method = "bonferroni")
  } else {
    p_adjusted <- p_values  # No correction
  }

  # Add to observed results
  observed$P_Value <- p_values[observed$Region]
  observed$P_Adjusted <- p_adjusted[observed$Region]
  observed$Significant <- !is.na(observed$P_Adjusted) & observed$P_Adjusted < 0.05

  # Determine significance stars
  observed$Significance_Stars <- ""
  observed$Significance_Stars[!is.na(observed$P_Adjusted) & observed$P_Adjusted < 0.05] <- "*"
  observed$Significance_Stars[!is.na(observed$P_Adjusted) & observed$P_Adjusted < 0.01] <- "**"
  observed$Significance_Stars[!is.na(observed$P_Adjusted) & observed$P_Adjusted < 0.001] <- "***"

  return(list(
    observed = observed,
    null_distributions = null_distributions,
    n_permutations = n_permutations,
    n_combinations = n_combinations,
    analysis_level = analysis_level,
    correction_method = correction_method
  ))
}


# ========================================================================
# ARTIFICIAL BRAIN AREA DISCOVERY
# Greedy Forward Selection with Elbow Detection
# ========================================================================

#' Find Elbow Using Second Derivative (Kneedle) Method
#'
#' Finds the point of maximum perpendicular distance from the line
#' connecting the first and last points of the curve.
#'
#' @param values Numeric vector of cumulative contribution values
#' @return Integer index of the elbow point
find_elbow_second_derivative <- function(values) {
  n <- length(values)
  if(n < 3) return(n)

  x <- 1:n
  y <- abs(values)  # Use absolute contributions

  # Scale to unit square for fair distance computation
  x_range <- max(x) - min(x)
  y_range <- max(y) - min(y)

  if(x_range == 0) x_range <- 1e-10
  if(y_range == 0) return(1)  # All values same, return first

  x_norm <- (x - min(x)) / x_range
  y_norm <- (y - min(y)) / y_range

  # Line from first point to last point
  # Line equation: ax + by + c = 0
  a <- y_norm[n] - y_norm[1]
  b <- x_norm[1] - x_norm[n]  # effectively -1
  c <- x_norm[n] * y_norm[1] - x_norm[1] * y_norm[n]

  # Perpendicular distance from each point to the line
  denom <- sqrt(a^2 + b^2)
  if(denom == 0) return(1)

  distances <- abs(a * x_norm + b * y_norm + c) / denom

  # Elbow is the point with maximum distance
  elbow_idx <- which.max(distances)

  return(elbow_idx)
}


#' Test Combined Contribution Significance
#'
#' Permutation test for the combined contribution of selected regions.
#'
#' @param networks_g1 List of network matrices for group 1
#' @param networks_g2 List of network matrices for group 2
#' @param node_names Character vector of node names
#' @param selected_regions Character vector of selected region names
#' @param n_permutations Number of permutations
#' @return List with observed contribution, null distribution, and p-value
test_combined_contribution <- function(networks_g1, networks_g2, node_names,
                                       selected_regions, n_permutations) {
  # Compute observed combined contribution
  baseline_J <- compute_averaged_jaccard(networks_g1, networks_g2)$avg_jaccard
  J_without <- compute_averaged_jaccard(networks_g1, networks_g2,
                                        exclude_nodes = selected_regions)$avg_jaccard
  observed_contribution <- J_without - baseline_J

  # Build null distribution by permuting node labels
  # SYMMETRIC TEST: permute BOTH groups independently each iteration
  # This ensures A vs B gives IDENTICAL results to B vs A
  n_nodes <- length(node_names)

  # Helper function for single permutation
  run_single_permutation <- function(perm_idx, networks_g1, networks_g2,
                                      node_names, n_nodes, selected_regions) {
    # Generate INDEPENDENT permutation indices for each group
    perm_idx_g1 <- sample(n_nodes)
    perm_idx_g2 <- sample(n_nodes)

    # Permute Group 1
    permuted_g1 <- list()
    for(key in names(networks_g1)) {
      perm_mat <- networks_g1[[key]][perm_idx_g1, perm_idx_g1]
      rownames(perm_mat) <- node_names
      colnames(perm_mat) <- node_names
      permuted_g1[[key]] <- perm_mat
    }

    # Permute Group 2
    permuted_g2 <- list()
    for(key in names(networks_g2)) {
      perm_mat <- networks_g2[[key]][perm_idx_g2, perm_idx_g2]
      rownames(perm_mat) <- node_names
      colnames(perm_mat) <- node_names
      permuted_g2[[key]] <- perm_mat
    }

    # Compute contribution under permutation
    perm_baseline <- compute_averaged_jaccard(permuted_g1, permuted_g2)$avg_jaccard
    perm_without <- compute_averaged_jaccard(permuted_g1, permuted_g2,
                                             exclude_nodes = selected_regions)$avg_jaccard
    return(perm_without - perm_baseline)
  }

  # Run permutations (always serial - parallelization happens at candidate level)
  null_distribution <- numeric(n_permutations)
  for(perm in 1:n_permutations) {
    null_distribution[perm] <- run_single_permutation(
      perm, networks_g1, networks_g2, node_names, n_nodes, selected_regions
    )
  }

  # Two-tailed p-value
  p_value <- (sum(abs(null_distribution) >= abs(observed_contribution)) + 1) / (n_permutations + 1)

  return(list(
    observed = observed_contribution,
    null_distribution = null_distribution,
    p_value = p_value
  ))
}


#' Greedy Forward Selection of Regions
#'
#' Iteratively selects regions based on contribution direction.
#' - "dissimilarity": selects regions where removing them INCREASES Jaccard
#'   (positive contribution = these regions drive differences between groups)
#' - "similarity": selects regions where removing them DECREASES Jaccard
#'   (negative contribution = these regions drive similarity between groups)
#'
#' @param networks_g1 List of network matrices for group 1
#' @param networks_g2 List of network matrices for group 2
#' @param node_names Character vector of all node names
#' @param max_regions Maximum number of regions to select
#' @param baseline_J Baseline Jaccard similarity
#' @param direction "dissimilarity" or "similarity" - which type of contribution to maximize
#' @return List with selection_order, cumulative_contributions, incremental_contributions
greedy_forward_selection <- function(networks_g1, networks_g2, node_names,
                                     max_regions, baseline_J,
                                     direction = "dissimilarity") {
  selected <- character(0)
  candidates <- node_names

  cumulative_contributions <- numeric(0)
  incremental_contributions <- numeric(0)
  selection_order <- character(0)

 # Cap max_regions at available nodes
  max_regions <- min(max_regions, length(node_names) - 2)  # Leave at least 2 nodes

  for(step in 1:max_regions) {
    # Current Jaccard with selected regions removed
    if(length(selected) == 0) {
      current_J <- baseline_J
    } else {
      current_J <- compute_averaged_jaccard(networks_g1, networks_g2,
                                            exclude_nodes = selected)$avg_jaccard
    }

    # Find best next region based on direction
    best_contrib <- NULL
    best_region <- NULL

    for(region in candidates) {
      test_set <- c(selected, region)
      test_J <- compute_averaged_jaccard(networks_g1, networks_g2,
                                         exclude_nodes = test_set)$avg_jaccard

      if(is.na(test_J)) next

      contrib <- test_J - current_J  # Incremental contribution

      # Select based on direction
      if(direction == "dissimilarity") {
        # Looking for POSITIVE contributions (removing region increases similarity)
        # These regions contribute to dissimilarity
        if(is.null(best_contrib) || contrib > best_contrib) {
          best_contrib <- contrib
          best_region <- region
        }
      } else {
        # Looking for NEGATIVE contributions (removing region decreases similarity)
        # These regions contribute to similarity
        if(is.null(best_contrib) || contrib < best_contrib) {
          best_contrib <- contrib
          best_region <- region
        }
      }
    }

    # If no valid region found or contribution goes wrong direction, stop
    if(is.null(best_region)) break

    # Stop if contribution is in wrong direction
    if(direction == "dissimilarity" && best_contrib <= 0) break
    if(direction == "similarity" && best_contrib >= 0) break

    # Add best region
    selected <- c(selected, best_region)
    candidates <- setdiff(candidates, best_region)
    selection_order <- c(selection_order, best_region)
    incremental_contributions <- c(incremental_contributions, best_contrib)

    # Cumulative contribution from baseline
    final_J <- compute_averaged_jaccard(networks_g1, networks_g2,
                                        exclude_nodes = selected)$avg_jaccard
    cumulative_contributions <- c(cumulative_contributions, final_J - baseline_J)
  }

  return(list(
    selection_order = selection_order,
    cumulative_contributions = cumulative_contributions,
    incremental_contributions = incremental_contributions,
    direction = direction
  ))
}


#' Discover Artificial Brain Area
#'
#' Uses greedy forward selection with elbow detection to find an optimal
#' combination of subregions that together contribute to group similarity or
#' dissimilarity. The discovered combination is validated with permutation testing.
#'
#' @param analysis_results The analysis_results reactive containing all data
#' @param group1 Name of group 1
#' @param group2 Name of group 2
#' @param max_regions Maximum number of regions to consider (default: 10)
#' @param n_permutations Number of permutations for significance testing (default: 500)
#' @param direction "dissimilarity" or "similarity" - which contribution type to find
#' @param correction_method "fdr", "bonferroni", or "none" for p-value correction
#' @return List with optimal_regions, combined_contribution, p_value, etc.
discover_artificial_brain_area <- function(analysis_results,
                                           group1,
                                           group2,
                                           max_regions = 10,
                                           n_permutations = 500,
                                           direction = "dissimilarity",
                                           correction_method = "none",
                                           methods = NULL) {

  # Extract networks (same as existing multi-method approach)
  networks_g1 <- extract_all_network_matrices(group1, analysis_results, methods)
  networks_g2 <- extract_all_network_matrices(group2, analysis_results, methods)

  empty_result <- list(
    optimal_regions = character(0),
    combined_contribution = NA,
    p_value = NA,
    p_adjusted = NA,
    significant = FALSE,
    elbow_k = 0,
    final_k = 0,
    selection_order = character(0),
    cumulative_contributions = numeric(0),
    incremental_contributions = numeric(0),
    null_distribution = numeric(0),
    baseline_jaccard = NA,
    direction = direction,
    correction_method = correction_method,
    error = NULL
  )

  if(length(networks_g1) == 0 || length(networks_g2) == 0) {
    empty_result$error <- "No network data available"
    return(empty_result)
  }

  # Get node names from first matrix
  first_mat <- networks_g1[[1]]
  node_names <- rownames(first_mat)
  if(is.null(node_names)) node_names <- colnames(first_mat)
  if(is.null(node_names)) node_names <- paste0("Node_", 1:nrow(first_mat))

  baseline_J <- compute_averaged_jaccard(networks_g1, networks_g2)$avg_jaccard

  if(is.na(baseline_J)) {
    empty_result$error <- "Could not compute baseline Jaccard"
    return(empty_result)
  }

  # Run greedy forward selection with specified direction
  selection_result <- greedy_forward_selection(networks_g1, networks_g2, node_names,
                                               max_regions, baseline_J,
                                               direction = direction)

  if(length(selection_result$selection_order) == 0) {
    empty_result$baseline_jaccard <- baseline_J
    empty_result$error <- paste("No regions found contributing to", direction)
    return(empty_result)
  }

  # Find elbow using second derivative method
  elbow_k <- find_elbow_second_derivative(selection_result$cumulative_contributions)

  # Get optimal set at elbow (do NOT iterate to find p<0.05 - that's p-hacking)
  optimal_k <- elbow_k
  optimal_regions <- selection_result$selection_order[1:optimal_k]

  # Permutation test for significance of combined contribution (test ONCE at elbow)
  perm_result <- test_combined_contribution(networks_g1, networks_g2, node_names,
                                            optimal_regions, n_permutations)

  # Apply multiple comparison correction if requested
  # Note: This is for when running multiple discovery analyses (e.g., both directions)
  p_adjusted <- perm_result$p_value
  if(correction_method == "bonferroni") {
    # Typically correcting for 2 tests (similarity + dissimilarity)
    p_adjusted <- min(1, perm_result$p_value * 2)
  } else if(correction_method == "fdr") {
    # For single test, FDR doesn't change much, but we include it for consistency
    p_adjusted <- perm_result$p_value
  }

  return(list(
    optimal_regions = optimal_regions,
    combined_contribution = selection_result$cumulative_contributions[optimal_k],
    p_value = perm_result$p_value,
    p_adjusted = p_adjusted,
    significant = p_adjusted < 0.05,
    elbow_k = elbow_k,
    final_k = optimal_k,
    selection_order = selection_result$selection_order,
    cumulative_contributions = selection_result$cumulative_contributions,
    incremental_contributions = selection_result$incremental_contributions,
    null_distribution = perm_result$null_distribution,
    baseline_jaccard = baseline_J,
    direction = direction,
    correction_method = correction_method,
    error = NULL
  ))
}


#' Discover Both Similarity and Dissimilarity Artificial Brain Areas
#'
#' Runs discovery for both directions and returns combined results.
#' Use this when you want to find regions contributing to both similarity
#' and dissimilarity between groups.
#'
#' @param analysis_results The analysis_results reactive containing all data
#' @param group1 Name of group 1
#' @param group2 Name of group 2
#' @param max_regions Maximum number of regions to consider (default: 10)
#' @param n_permutations Number of permutations for significance testing (default: 500)
#' @param correction_method "fdr", "bonferroni", or "none" for p-value correction
#' @return List with dissimilarity_result, similarity_result, and combined summary
discover_both_directions <- function(analysis_results,
                                     group1,
                                     group2,
                                     max_regions = 10,
                                     n_permutations = 500,
                                     correction_method = "bonferroni") {

  # Run both directions
  dissim_result <- discover_artificial_brain_area(
    analysis_results, group1, group2,
    max_regions, n_permutations,
    direction = "dissimilarity",
    correction_method = correction_method
  )

  sim_result <- discover_artificial_brain_area(
    analysis_results, group1, group2,
    max_regions, n_permutations,
    direction = "similarity",
    correction_method = correction_method
  )

  # If using Bonferroni, adjust p-values for 2 tests
  if(correction_method == "bonferroni") {
    dissim_result$p_adjusted <- min(1, dissim_result$p_value * 2)
    dissim_result$significant <- dissim_result$p_adjusted < 0.05

    sim_result$p_adjusted <- min(1, sim_result$p_value * 2)
    sim_result$significant <- sim_result$p_adjusted < 0.05
  }

  return(list(
    dissimilarity = dissim_result,
    similarity = sim_result,
    baseline_jaccard = dissim_result$baseline_jaccard
  ))
}


# =============================================================================
# HIERARCHICAL CLUSTERING APPROACH FOR ARTIFICIAL BRAIN AREA DISCOVERY
# =============================================================================

#' Compute Connectivity Difference Matrix Between Groups
#'
#' For each pair of regions, computes the average absolute difference in
#' connectivity between groups, averaged across all method-approach combinations.
#' Each region's row represents its "difference profile" - how its connections
#' to other regions differ between groups.
#'
#' @param networks_g1 List of network matrices for group 1 (from extract_all_network_matrices)
#' @param networks_g2 List of network matrices for group 2
#' @param node_names Character vector of node names
#' @return Matrix where diff[i,j] = average |g1[i,j] - g2[i,j]| across methods
compute_connectivity_difference_matrix <- function(networks_g1, networks_g2, node_names) {
  n_nodes <- length(node_names)
  method_keys <- intersect(names(networks_g1), names(networks_g2))

  if(length(method_keys) == 0) {
    warning("No common method-approach combinations found")
    return(matrix(0, nrow = n_nodes, ncol = n_nodes,
                  dimnames = list(node_names, node_names)))
  }

  # Accumulate differences across all method-approach combinations
  total_diff <- matrix(0, nrow = n_nodes, ncol = n_nodes,
                       dimnames = list(node_names, node_names))
  n_valid <- 0

  for(key in method_keys) {
    mat_g1 <- networks_g1[[key]]
    mat_g2 <- networks_g2[[key]]

    # Ensure matching dimensions
    if(nrow(mat_g1) != n_nodes || nrow(mat_g2) != n_nodes) {
      next
    }

    # Absolute difference in connectivity
    diff_mat <- abs(mat_g1 - mat_g2)
    total_diff <- total_diff + diff_mat
    n_valid <- n_valid + 1
  }

  # Average across methods
  if(n_valid > 0) {
    avg_diff <- total_diff / n_valid
  } else {
    avg_diff <- total_diff
  }

  return(avg_diff)
}


#' Find Optimal Number of Clusters Using Silhouette Analysis
#'
#' Tests different values of k and returns the one with maximum average
#' silhouette width, respecting minimum cluster constraints.
#'
#' @param hclust_result Result from hclust()
#' @param dist_matrix Distance matrix used for clustering
#' @param max_k Maximum number of clusters to test
#' @param min_k Minimum number of clusters to consider (default: 2)
#' @return Integer: optimal number of clusters
find_optimal_clusters <- function(hclust_result, dist_matrix, max_k, min_k = 2) {
  n <- attr(dist_matrix, "Size")
  if(is.null(n)) n <- nrow(as.matrix(dist_matrix))

  # Enforce minimum of 2

  if(min_k < 2) min_k <- 2

  # Cap max_k at n-1
  if(max_k > n - 1) max_k <- n - 1
  if(max_k < min_k) return(min_k)

  # Compute silhouette scores for all k values
  silhouette_scores <- numeric(max_k - 1)
  names(silhouette_scores) <- 2:max_k

  for(k in 2:max_k) {
    cluster_assignments <- cutree(hclust_result, k = k)

    # Check if we have valid clustering (all clusters have members)
    if(length(unique(cluster_assignments)) < k) {
      silhouette_scores[k - 1] <- -1  # Invalid
      next
    }

    # Compute silhouette
    sil <- cluster::silhouette(cluster_assignments, dist_matrix)
    silhouette_scores[k - 1] <- mean(sil[, "sil_width"])
  }

  # Only consider k values >= min_k
  valid_indices <- which(2:max_k >= min_k)
  valid_scores <- silhouette_scores[valid_indices]

  # Find best k among valid options
  if(length(valid_scores) == 0 || all(valid_scores <= -1)) {
    return(min_k)
  }

  # Get the k with maximum silhouette score (among k >= min_k)
  best_idx <- which.max(valid_scores)
  optimal_k <- (min_k:max_k)[best_idx]

  return(optimal_k)
}


#' Cluster-Based Artificial Brain Area Discovery
#'
#' Uses hierarchical clustering to group regions based on how their connectivity
#' patterns differ between groups. Regions that change similarly between groups
#' cluster together, forming natural "functional units".
#'
#' @param analysis_results The analysis_results reactive containing all data
#' @param group1 Name of group 1
#' @param group2 Name of group 2
#' @param max_clusters Maximum number of clusters to test (default: 5)
#' @param min_clusters Minimum number of clusters to consider (default: 3)
#' @param n_permutations Number of permutations for significance testing (default: 500)
#' @param linkage_method Hierarchical clustering linkage method (default: "ward.D2")
#' @return List containing clustering results and cluster contribution tests
cluster_based_discovery <- function(analysis_results,
                                    group1,
                                    group2,
                                    max_clusters = 5,
                                    min_clusters = 3,
                                    n_permutations = 500,
                                    linkage_method = "ward.D2",
                                    methods = NULL) {

  # Extract networks for both groups
  networks_g1 <- extract_all_network_matrices(group1, analysis_results, methods)
  networks_g2 <- extract_all_network_matrices(group2, analysis_results, methods)

  if(length(networks_g1) == 0 || length(networks_g2) == 0) {
    stop("Could not extract network matrices for one or both groups")
  }

  # Get node names from first matrix
  first_mat <- networks_g1[[1]]
  node_names <- rownames(first_mat)
  if(is.null(node_names)) node_names <- colnames(first_mat)
  if(is.null(node_names)) {
    node_names <- paste0("Node", 1:nrow(first_mat))
  }
  n_nodes <- length(node_names)

  # Validate min/max clusters
  if(min_clusters < 2) min_clusters <- 2
  if(max_clusters < min_clusters) max_clusters <- min_clusters

  # Step 1 & 2: Compute averaged connectivity difference matrix
  diff_matrix <- compute_connectivity_difference_matrix(networks_g1, networks_g2, node_names)

  # Step 3: Hierarchical clustering on difference profiles
  # Each region's row in diff_matrix is its "difference profile"
  region_dist <- dist(diff_matrix, method = "euclidean")
  hclust_result <- hclust(region_dist, method = linkage_method)

  # Step 4: Find optimal k using silhouette (respecting min_clusters)
  optimal_k <- find_optimal_clusters(hclust_result, region_dist, max_clusters, min_clusters)
  cluster_assignments <- cutree(hclust_result, k = optimal_k)
  names(cluster_assignments) <- node_names

  # Compute silhouette scores for all tested k values (for visualization)
  # Start from 2 to show full picture, but mark which are valid
  silhouette_by_k <- numeric(max_clusters - 1)
  for(k in 2:max_clusters) {
    assignments_k <- cutree(hclust_result, k = k)
    if(length(unique(assignments_k)) == k) {
      sil <- cluster::silhouette(assignments_k, region_dist)
      silhouette_by_k[k - 1] <- mean(sil[, "sil_width"])
    } else {
      silhouette_by_k[k - 1] <- NA
    }
  }

  # Step 5: Test each cluster's contribution
  baseline_J <- compute_averaged_jaccard(networks_g1, networks_g2)$avg_jaccard
  cluster_results <- list()

  for(k in 1:optimal_k) {
    cluster_regions <- node_names[cluster_assignments == k]

    # Compute contribution (Jaccard change when cluster is removed)
    J_without <- compute_averaged_jaccard(networks_g1, networks_g2,
                                          exclude_nodes = cluster_regions)$avg_jaccard
    contribution <- J_without - baseline_J

    # Permutation test for significance
    perm_result <- test_combined_contribution(networks_g1, networks_g2, node_names,
                                              cluster_regions, n_permutations)

    cluster_results[[k]] <- list(
      cluster_id = k,
      regions = cluster_regions,
      n_regions = length(cluster_regions),
      contribution = contribution,
      direction = ifelse(contribution > 0, "dissimilarity", "similarity"),
      p_value = perm_result$p_value,
      significant = perm_result$p_value < 0.05,
      null_distribution = perm_result$null_distribution
    )
  }

  # Compute mean difference profile magnitude for each cluster
  # (indicates how much the cluster's connectivity changes between groups)
  cluster_diff_magnitudes <- numeric(optimal_k)
  for(k in 1:optimal_k) {
    cluster_regions <- node_names[cluster_assignments == k]
    cluster_idx <- which(node_names %in% cluster_regions)
    if(length(cluster_idx) > 0) {
      cluster_diff_magnitudes[k] <- mean(diff_matrix[cluster_idx, ])
    }
  }

  return(list(
    method = "hierarchical_clustering",
    hclust = hclust_result,
    optimal_k = optimal_k,
    cluster_assignments = cluster_assignments,
    diff_matrix = diff_matrix,
    region_dist = region_dist,
    baseline_jaccard = baseline_J,
    cluster_results = cluster_results,
    cluster_diff_magnitudes = cluster_diff_magnitudes,
    silhouette_by_k = silhouette_by_k,
    node_names = node_names,
    linkage_method = linkage_method,
    min_clusters = min_clusters,
    max_clusters = max_clusters,
    group1_name = group1,
    group2_name = group2
  ))
}


# =============================================================================
# BRUTE FORCE ARTIFICIAL BRAIN AREA DISCOVERY
# Exhaustive enumeration of all region combinations (1, 2, 3, 4 regions)
# =============================================================================

#' Extract Networks Organized by Approach
#'
#' Extracts network matrices for a group, organized by approach type (weighted,
#' percolation, persistence). This allows separate analysis of each approach.
#'
#' @param group Character name of the group
#' @param analysis_results The analysis_results reactive containing all data
#' @return List with $weighted, $percolation, $persistence sublists
extract_networks_by_approach <- function(group, analysis_results, methods = NULL) {
  # Use provided methods or default to all 6
  if(is.null(methods)) {
    methods <- c("pearson", "spearman", "kendall", "biweight", "shrinkage", "partial")
  }

  result <- list(
    weighted = list(),      # Up to 6 matrices (one per method)
    percolation = list(),   # Up to 6 matrices (one per method)
    persistence = list()    # Up to 6 lists of matrices (one per method, each with threshold matrices)
  )

  for(method in methods) {
    # WEIGHTED: Full correlation matrix (no threshold)
    if(!is.null(analysis_results$correlation_methods_raw[[method]][[group]])) {
      result$weighted[[method]] <- abs(analysis_results$correlation_methods_raw[[method]][[group]])
    }

    # PERCOLATION: Adjacency at optimal threshold × correlations
    if(!is.null(analysis_results$method_percolation_results[[method]]$adjacency_matrices[[group]])) {
      adj_mat <- analysis_results$method_percolation_results[[method]]$adjacency_matrices[[group]]
      cor_mat <- abs(analysis_results$correlation_methods_raw[[method]][[group]])
      result$percolation[[method]] <- adj_mat * cor_mat
    }

    # PERSISTENCE: Matrix at EACH threshold in the sequence
    if(!is.null(analysis_results$persistence_results[[method]][[group]]$persistence_data)) {
      pers_data <- analysis_results$persistence_results[[method]][[group]]$persistence_data
      threshold_vals <- as.numeric(names(pers_data))
      cor_mat <- abs(analysis_results$correlation_methods_raw[[method]][[group]])

      result$persistence[[method]] <- list()
      for(thresh in threshold_vals) {
        thresh_mat <- cor_mat
        thresh_mat[thresh_mat < thresh] <- 0
        result$persistence[[method]][[as.character(thresh)]] <- thresh_mat
      }
    }
  }

  return(result)
}


#' Compute Jaccard for a Single Approach (Weighted or Percolation)
#'
#' Averages Jaccard similarity across methods for a single approach type.
#'
#' @param networks_g1 List of network matrices for group 1 (keyed by method)
#' @param networks_g2 List of network matrices for group 2 (keyed by method)
#' @param exclude_nodes Optional vector of node names to exclude
#' @return Average Jaccard across methods
compute_approach_jaccard <- function(networks_g1, networks_g2, exclude_nodes = NULL) {
  methods <- intersect(names(networks_g1), names(networks_g2))

  if(length(methods) == 0) return(NA)

  jaccards <- c()

  for(method in methods) {
    mat1 <- networks_g1[[method]]
    mat2 <- networks_g2[[method]]

    if(is.null(mat1) || is.null(mat2)) next

    # Apply node exclusion
    if(!is.null(exclude_nodes) && length(exclude_nodes) > 0) {
      node_names <- rownames(mat1)
      if(is.null(node_names)) node_names <- colnames(mat1)
      if(!is.null(node_names)) {
        keep_idx <- which(!(node_names %in% exclude_nodes))
        if(length(keep_idx) >= 3) {
          mat1 <- mat1[keep_idx, keep_idx, drop = FALSE]
          mat2 <- mat2[keep_idx, keep_idx, drop = FALSE]
        } else {
          next
        }
      }
    }

    j <- compute_jaccard_for_contribution(mat1, mat2)
    if(!is.na(j)) jaccards <- c(jaccards, j)
  }

  if(length(jaccards) == 0) return(NA)
  return(mean(jaccards, na.rm = TRUE))
}


#' Compute Jaccard for Persistence Approach (Multi-Threshold)
#'
#' Computes Jaccard at EACH threshold for each method, then averages.
#' This properly captures the multi-threshold nature of persistence analysis.
#'
#' @param pers_g1 Persistence networks for group 1 (method -> threshold -> matrix)
#' @param pers_g2 Persistence networks for group 2
#' @param exclude_nodes Optional vector of node names to exclude
#' @return Average Jaccard across all method-threshold combinations
compute_persistence_jaccard <- function(pers_g1, pers_g2, exclude_nodes = NULL) {
  methods <- intersect(names(pers_g1), names(pers_g2))

  if(length(methods) == 0) return(NA)

  all_jaccards <- c()

  for(method in methods) {
    if(is.null(pers_g1[[method]]) || is.null(pers_g2[[method]])) next

    thresholds <- intersect(names(pers_g1[[method]]), names(pers_g2[[method]]))

    for(thresh in thresholds) {
      mat1 <- pers_g1[[method]][[thresh]]
      mat2 <- pers_g2[[method]][[thresh]]

      if(is.null(mat1) || is.null(mat2)) next

      # Apply node exclusion
      if(!is.null(exclude_nodes) && length(exclude_nodes) > 0) {
        node_names <- rownames(mat1)
        if(is.null(node_names)) node_names <- colnames(mat1)
        if(!is.null(node_names)) {
          keep_idx <- which(!(node_names %in% exclude_nodes))
          if(length(keep_idx) >= 3) {
            mat1 <- mat1[keep_idx, keep_idx, drop = FALSE]
            mat2 <- mat2[keep_idx, keep_idx, drop = FALSE]
          } else {
            next
          }
        }
      }

      j <- compute_jaccard_for_contribution(mat1, mat2)
      if(!is.na(j)) all_jaccards <- c(all_jaccards, j)
    }
  }

  if(length(all_jaccards) == 0) return(NA)
  return(mean(all_jaccards, na.rm = TRUE))
}


#' Brute Force Artificial Brain Area Discovery
#'
#' Exhaustively tests ALL possible combinations of 1, 2, 3, and 4 regions
#' to find which combinations most strongly affect group similarity when removed.
#' No heuristics, no path-dependency - pure exhaustive search.
#'
#' Uses approach-separated analysis:
#' - Weighted: Full correlation matrices (5 methods)
#' - Percolation: Adjacency at optimal threshold (5 methods)
#' - Persistence: Matrices at each threshold (5 methods × 17 thresholds)
#'
#' @param analysis_results The analysis_results reactive containing all data
#' @param group1 Name of group 1
#' @param group2 Name of group 2
#' @param max_combo_size Maximum combination size to test (1-4, default: 4)
#' @param n_permutations Number of permutations for significance testing (default: 500)
#' @param candidate_filter Filter method for selecting candidates to test:
#'   "all" = test all candidates (recommended, let FDR handle multiple testing)
#'   "sd_1", "sd_1.5", "sd_2" = filter by effect size > X standard deviations
#'   "pct_10", "pct_25" = filter to top/bottom X percent
#' @param progress_callback Optional function to call with progress updates (0-1)
#' @return List containing all results with approach-specific contributions
brute_force_discovery <- function(analysis_results,
                                   group1,
                                   group2,
                                   max_combo_size = 4,
                                   n_permutations = 500,
                                   candidate_filter = "all",
                                   correction_method = "fdr",
                                   progress_callback = NULL,
                                   seed = 42,
                                   methods = NULL,
                                   use_parallel = FALSE,
                                   n_workers = NULL,
                                   calibration = NULL,
                                   verbose = FALSE) {

  # Set seed for reproducibility (permutation tests use random shuffling)
  if(!is.null(seed)) {
    set.seed(seed)
  }

  if(verbose) {
    message("[DEBUG brute_force] Starting brute_force_discovery")
    message("[DEBUG brute_force] use_parallel = ", use_parallel)
    message("[DEBUG brute_force] PARALLEL_AVAILABLE = ", PARALLEL_AVAILABLE)
    message("[DEBUG brute_force] n_workers requested = ", n_workers)
    if(!is.null(calibration)) {
      message("[DEBUG brute_force] Calibration data available: optimal_workers = ", calibration$optimal_workers)
    }
  }

  # Adaptive parallel setup flag - will be determined after we know problem size
  parallel_setup_deferred <- use_parallel && PARALLEL_AVAILABLE
  actual_use_parallel <- FALSE
  actual_n_workers <- 1

  # SYMMETRIC: Sort groups alphabetically for internal processing
  # This ensures the same seed produces identical results regardless of input order
  groups_sorted <- sort(c(group1, group2))
  internal_g1 <- groups_sorted[1]
  internal_g2 <- groups_sorted[2]

  # ========== Extract networks organized by approach ==========
  networks_g1 <- extract_networks_by_approach(internal_g1, analysis_results, methods)
  networks_g2 <- extract_networks_by_approach(internal_g2, analysis_results, methods)

  # Check that we have at least some networks
  n_weighted <- length(networks_g1$weighted)
  n_percolation <- length(networks_g1$percolation)
  n_persistence <- length(networks_g1$persistence)

  if(n_weighted == 0 && n_percolation == 0 && n_persistence == 0) {
    stop("Could not extract network matrices for one or both groups")
  }

  # Get node names from first available matrix
  first_mat <- NULL
  if(n_weighted > 0) {
    first_mat <- networks_g1$weighted[[1]]
  } else if(n_percolation > 0) {
    first_mat <- networks_g1$percolation[[1]]
  } else if(n_persistence > 0) {
    first_mat <- networks_g1$persistence[[1]][[1]]
  }

  node_names <- rownames(first_mat)
  if(is.null(node_names)) node_names <- colnames(first_mat)
  if(is.null(node_names)) {
    node_names <- paste0("Node", 1:nrow(first_mat))
  }
  n_nodes <- length(node_names)

  # Cap max_combo_size at reasonable values (no upper limit - synergy handles size bias)
  if(max_combo_size < 1) max_combo_size <- 1
  if(max_combo_size > n_nodes - 2) max_combo_size <- n_nodes - 2

  # ========== Compute baseline Jaccard for each approach ==========
  baseline_weighted <- compute_approach_jaccard(networks_g1$weighted, networks_g2$weighted)
  baseline_percolation <- compute_approach_jaccard(networks_g1$percolation, networks_g2$percolation)
  baseline_persistence <- compute_persistence_jaccard(networks_g1$persistence, networks_g2$persistence)

  # Count how many approaches have valid baselines
  valid_baselines <- c(
    weighted = !is.na(baseline_weighted),
    percolation = !is.na(baseline_percolation),
    persistence = !is.na(baseline_persistence)
  )

  if(sum(valid_baselines) == 0) {
    stop("Could not compute baseline Jaccard for any approach")
  }

  # Count thresholds for persistence
  n_thresholds <- 0
  if(n_persistence > 0 && length(networks_g1$persistence[[1]]) > 0) {
    n_thresholds <- length(networks_g1$persistence[[1]])
  }

  # ========== PHASE 1: Enumerate all combinations ==========
  if(verbose) message("[DEBUG brute_force] Starting PHASE 1: Enumerate all combinations")
  all_combos_list <- list()
  combo_id <- 0

  # Calculate total combinations for progress reporting
  total_combos <- sum(sapply(1:max_combo_size, function(k) choose(n_nodes, k)))
  if(verbose) {
    message("[DEBUG brute_force] n_nodes = ", n_nodes, ", max_combo_size = ", max_combo_size)
    message("[DEBUG brute_force] Total combinations to enumerate: ", total_combos)
    message("[DEBUG brute_force] Time: ", Sys.time())
  }

  for(size in 1:max_combo_size) {
    if(verbose) message("[DEBUG brute_force] Enumerating size ", size, " combinations...")
    combos <- combn(node_names, size, simplify = FALSE)

    for(combo in combos) {
      combo_id <- combo_id + 1

      # Progress callback
      if(!is.null(progress_callback)) {
        progress_callback(combo_id / total_combos * 0.5)  # First 50% for enumeration
      }

      # Compute Jaccard without these regions for EACH approach
      J_weighted <- compute_approach_jaccard(networks_g1$weighted, networks_g2$weighted,
                                              exclude_nodes = combo)
      J_percolation <- compute_approach_jaccard(networks_g1$percolation, networks_g2$percolation,
                                                 exclude_nodes = combo)
      J_persistence <- compute_persistence_jaccard(networks_g1$persistence, networks_g2$persistence,
                                                    exclude_nodes = combo)

      # Compute contributions (J_without - baseline) for each approach
      contrib_weighted <- if(!is.na(J_weighted) && !is.na(baseline_weighted)) {
        J_weighted - baseline_weighted
      } else { NA }

      contrib_percolation <- if(!is.na(J_percolation) && !is.na(baseline_percolation)) {
        J_percolation - baseline_percolation
      } else { NA }

      contrib_persistence <- if(!is.na(J_persistence) && !is.na(baseline_persistence)) {
        J_persistence - baseline_persistence
      } else { NA }

      # Combined contribution (average across valid approaches)
      valid_contribs <- c(contrib_weighted, contrib_percolation, contrib_persistence)
      valid_contribs <- valid_contribs[!is.na(valid_contribs)]

      if(length(valid_contribs) == 0) next

      contrib_combined <- mean(valid_contribs)
      direction <- ifelse(contrib_combined > 0, "dissimilarity", "similarity")

      all_combos_list[[combo_id]] <- data.frame(
        combo_id = combo_id,
        nodes = paste(combo, collapse = ", "),
        nodes_list = I(list(combo)),
        size = length(combo),
        # Approach-specific contributions
        contrib_weighted = contrib_weighted,
        contrib_percolation = contrib_percolation,
        contrib_persistence = contrib_persistence,
        # Combined
        contribution = contrib_combined,
        direction = direction,
        stringsAsFactors = FALSE
      )
    }
  }

  # Combine into single data frame
  if(verbose) {
    message("[DEBUG brute_force] Phase 1 enumeration complete. Combining results...")
    message("[DEBUG brute_force] Time: ", Sys.time())
  }
  all_results <- do.call(rbind, all_combos_list)

  if(is.null(all_results) || nrow(all_results) == 0) {
    stop("No valid combinations could be computed")
  }
  if(verbose) message("[DEBUG brute_force] Total results: ", nrow(all_results), " combinations")

  # Add absolute contribution for ranking
  all_results$abs_contribution <- abs(all_results$contribution)

  # ========== PHASE 1.5: Compute Synergy ==========
  if(verbose) message("[DEBUG brute_force] Computing synergy...")
  # Create lookup table for singles contributions
  singles <- all_results[all_results$size == 1, ]
  singles_lookup <- setNames(singles$contribution, singles$nodes)

  # Initialize synergy columns
  all_results$expected_additive <- NA
  all_results$synergy <- NA

  # Compute synergy for combinations with size > 1
  for(i in 1:nrow(all_results)) {
    combo <- all_results$nodes_list[[i]]

    if(length(combo) > 1) {
      # Sum of individual contributions from singles
      individual_contribs <- singles_lookup[combo]
      if(all(!is.na(individual_contribs))) {
        individual_sum <- sum(individual_contribs)
        all_results$expected_additive[i] <- individual_sum
        all_results$synergy[i] <- all_results$contribution[i] - individual_sum
      }
    }
  }

  # Create rank_score: singles by abs_contribution, multi-region by RELATIVE synergy
  # Relative synergy = synergy / abs(expected_additive) - controls for combination size
  # This ensures a 4-region combo must show proportionally MORE excess than a pair to rank higher
  all_results$synergy_ratio <- NA

  for(i in 1:nrow(all_results)) {
    if(all_results$size[i] > 1 && !is.na(all_results$synergy[i]) &&
       !is.na(all_results$expected_additive[i]) && abs(all_results$expected_additive[i]) > 1e-10) {
      all_results$synergy_ratio[i] <- all_results$synergy[i] / abs(all_results$expected_additive[i])
    }
  }

  all_results$rank_score <- ifelse(
    all_results$size == 1,
    all_results$abs_contribution,
    abs(all_results$synergy_ratio)  # Use relative synergy for multi-region
  )
  # Handle NA synergy_ratio (use abs_contribution as fallback, but penalized)
  na_idx <- is.na(all_results$rank_score)
  all_results$rank_score[na_idx] <- all_results$abs_contribution[na_idx] * 0.01  # Penalize fallback cases

  # ========== PHASE 2: Rank by rank_score (synergy for multi-region) ==========
  if(verbose) message("[DEBUG brute_force] PHASE 2: Ranking results...")
  all_results <- all_results[order(-all_results$rank_score), ]
  rownames(all_results) <- NULL

  # Separate by direction
  dissim_results <- all_results[all_results$direction == "dissimilarity", ]
  sim_results <- all_results[all_results$direction == "similarity", ]
  if(verbose) {
    message("[DEBUG brute_force] Dissimilarity results: ", nrow(dissim_results))
    message("[DEBUG brute_force] Similarity results: ", nrow(sim_results))
  }

  # ========== PHASE 3: Synergy column retained (no filtering) ==========
  # Previously filtered for synergistic combinations only, but this introduced selection bias
  # before permutation testing. Now we test ALL combinations and let users filter results
  # by synergy status post-hoc. The synergy column is retained for interpretation.
  if(verbose) message("[DEBUG brute_force] PHASE 3: Synergy computed (no pre-filtering - all combinations tested)")
  if(verbose) message("[DEBUG brute_force] Combinations with synergy>0 - Dissim: ",
                      sum(dissim_results$synergy > 0, na.rm = TRUE), "/", nrow(dissim_results),
                      ", Sim: ", sum(sim_results$synergy > 0, na.rm = TRUE), "/", nrow(sim_results))

  # ========== PHASE 4: Filter candidates for permutation testing ==========
  if(verbose) message("[DEBUG brute_force] PHASE 4: Applying candidate filter '", candidate_filter, "'...")
  # Apply candidate_filter to select which candidates to test

  apply_candidate_filter <- function(df, filter_type) {
    if(nrow(df) == 0) return(df)

    if(filter_type == "all") {
      # Test all candidates - let FDR handle multiple testing
      return(df)

    } else if(grepl("^sd_", filter_type)) {
      # Effect size threshold: filter by > X standard deviations
      sd_threshold <- as.numeric(sub("sd_", "", filter_type))
      contrib_mean <- mean(df$contribution, na.rm = TRUE)
      contrib_sd <- sd(df$contribution, na.rm = TRUE)

      if(is.na(contrib_sd) || contrib_sd == 0) return(df)

      # Keep candidates where |contribution - mean| > threshold * SD
      z_scores <- abs(df$contribution - contrib_mean) / contrib_sd
      return(df[z_scores > sd_threshold, ])

    } else if(grepl("^pct_", filter_type)) {
      # Percentile filter: keep top/bottom X%
      pct <- as.numeric(sub("pct_", "", filter_type)) / 100
      n_keep <- max(1, ceiling(nrow(df) * pct))
      return(head(df, n_keep))

    } else {
      # Unknown filter, return all
      return(df)
    }
  }

  top_dissim <- apply_candidate_filter(dissim_results, candidate_filter)
  top_sim <- apply_candidate_filter(sim_results, candidate_filter)
  if(verbose) {
    message("[DEBUG brute_force] After candidate filter - Dissim: ", nrow(top_dissim), ", Sim: ", nrow(top_sim))
    message("[DEBUG brute_force] Time: ", Sys.time())
  }

  # For permutation testing, we need to use the old extract function
  # (permutation test uses all approaches combined)
  # Use internal_g1/g2 (alphabetically sorted) for symmetric results
  if(verbose) message("[DEBUG brute_force] Extracting flat network matrices for permutation testing...")
  networks_flat_g1 <- extract_all_network_matrices(internal_g1, analysis_results, methods)
  networks_flat_g2 <- extract_all_network_matrices(internal_g2, analysis_results, methods)
  if(verbose) message("[DEBUG brute_force] Extracted ", length(networks_flat_g1), " matrices for G1, ", length(networks_flat_g2), " for G2")

  # ========== CHECK FOR C++ BACKEND ==========
  # Use C++ with OpenMP for true high-performance parallelization
  use_cpp_backend <- exists("CPP_BACKEND_AVAILABLE") && CPP_BACKEND_AVAILABLE
  if(verbose) {
    if(use_cpp_backend) {
      message("[DEBUG brute_force] C++ backend AVAILABLE with ", CPP_OPENMP_THREADS, " OpenMP threads")
    } else {
      message("[DEBUG brute_force] C++ backend not available, using R implementation")
    }
  }

  # Helper function to test a single candidate
  # Uses C++ if available, otherwise R
  test_single_candidate <- function(regions_list) {
    if(use_cpp_backend) {
      # Use fast C++ implementation with OpenMP
      perm_result <- test_combined_contribution_fast(
        networks_flat_g1, networks_flat_g2,
        node_names, regions_list, n_permutations,
        n_threads = 0  # Auto-detect (will use all cores)
      )
    } else {
      # Fallback to R implementation
      perm_result <- test_combined_contribution(
        networks_flat_g1, networks_flat_g2,
        node_names, regions_list, n_permutations
      )
    }
    perm_result$p_value
  }

  # Test dissimilarity candidates
  if(verbose) {
    message("[DEBUG brute_force] Starting permutation testing phase")
    message("[DEBUG brute_force] Dissimilarity candidates: ", nrow(top_dissim))
    message("[DEBUG brute_force] Similarity candidates: ", nrow(top_sim))
  }

  # ========== DEFERRED PARALLEL SETUP ==========
  # Now that we know the problem size, determine optimal parallel configuration
  # NOTE: If using C++ backend, we don't need R's parallel package at all!
  total_candidates_to_test <- nrow(top_dissim) + nrow(top_sim)
  n_matrices_flat <- length(networks_flat_g1)

  if(parallel_setup_deferred && total_candidates_to_test >= 3) {
    # Use calibration data or heuristic to determine optimal workers
    optimal_config <- determine_optimal_workers(
      n_candidates = total_candidates_to_test,
      n_permutations = n_permutations,
      n_nodes = n_nodes,
      n_matrices = n_matrices_flat,
      calibration = calibration
    )

    if(verbose) {
      message("[DEBUG brute_force] Optimal config: ", optimal_config$recommendation)
      message("[DEBUG brute_force] Serial time estimate: ", round(optimal_config$serial_time_ms/1000, 1), " seconds")
      message("[DEBUG brute_force] Parallel time estimate: ", round(optimal_config$parallel_time_ms/1000, 1), " seconds")
    }

    # Override n_workers with optimal value if not explicitly set, or if user requested "auto"
    if(is.null(n_workers) || n_workers == "auto" || n_workers == 0) {
      actual_n_workers <- optimal_config$optimal_workers
    } else {
      actual_n_workers <- as.numeric(n_workers)
    }

    # Only use parallel if it's actually beneficial
    actual_use_parallel <- optimal_config$should_use_parallel && actual_n_workers > 1

    if(actual_use_parallel) {
      if(verbose) {
        message("[DEBUG brute_force] Setting up ADAPTIVE parallel with ", actual_n_workers, " workers")
        message("[DEBUG brute_force] Expected speedup: ", round(optimal_config$expected_speedup, 1), "x")
      }
      old_plan <- future::plan()
      future::plan(future::multisession, workers = actual_n_workers)
      on.exit(future::plan(old_plan), add = TRUE)
    } else {
      if(verbose) {
        message("[DEBUG brute_force] Parallel not beneficial - running SERIAL")
        message("[DEBUG brute_force] Reason: ", optimal_config$recommendation)
      }
    }
  } else if(parallel_setup_deferred) {
    if(verbose) message("[DEBUG brute_force] Too few candidates (", total_candidates_to_test, ") - running SERIAL")
  }

  if(nrow(top_dissim) > 0) {
    top_dissim$p_value <- NA
    top_dissim$significant <- NA

    # ========== C++ BATCH PROCESSING (FASTEST) ==========
    # If C++ backend is available, use it for ALL candidates at once
    # This is the fastest option - uses OpenMP with all cores
    if(use_cpp_backend && exists("batch_test_candidates_fast")) {
      if(verbose) {
        message("[DEBUG brute_force] Using C++ BATCH PROCESSING for dissimilarity (", nrow(top_dissim), " candidates)")
        message("[DEBUG brute_force] n_permutations = ", n_permutations)
        message("[DEBUG brute_force] Total permutation tests: ", nrow(top_dissim) * n_permutations)
        message("[DEBUG brute_force] OpenMP threads: ", CPP_OPENMP_THREADS)
        message("[DEBUG brute_force] Time: ", Sys.time())
      }

      batch_start_time <- Sys.time()

      batch_results <- batch_test_candidates_fast(
        networks_g1 = networks_flat_g1,
        networks_g2 = networks_flat_g2,
        node_names = node_names,
        candidates_list = top_dissim$nodes_list,
        n_permutations = n_permutations,
        n_threads = 0,  # Auto-detect
        progress_callback = function(p) {
          if(!is.null(progress_callback)) {
            progress_callback(0.5 + (p * 0.25))  # 50-75% for dissimilarity
          }
        }
      )

      top_dissim$p_value <- batch_results$p_value

      batch_elapsed <- as.numeric(difftime(Sys.time(), batch_start_time, units = "secs"))
      if(verbose) {
        message("[DEBUG brute_force] C++ batch processing COMPLETE in ", round(batch_elapsed, 1), " seconds")
        message("[DEBUG brute_force] Method used: ", batch_results$method[1])
        message("[DEBUG brute_force] Time: ", Sys.time())
      }

    } else if(actual_use_parallel && nrow(top_dissim) >= 3) {
      if(verbose) {
        message("[DEBUG brute_force] Using PARALLEL for dissimilarity (", nrow(top_dissim), " candidates)")
        message("[DEBUG brute_force] n_permutations = ", n_permutations)
        message("[DEBUG brute_force] Total permutation tests: ", nrow(top_dissim) * n_permutations)
        message("[DEBUG brute_force] Using ", actual_n_workers, " workers (adaptive selection)")
        message("[DEBUG brute_force] Starting future_map_dbl for dissimilarity...")
        message("[DEBUG brute_force] Time: ", Sys.time())
      }

      # Calculate progress parameters
      total_candidates <- nrow(top_dissim) + nrow(top_sim)

      # Use chunk-based processing for progress updates
      # Process in chunks so we can update Shiny progress between chunks
      chunk_size <- max(actual_n_workers * 2, 10)  # Process 2x workers at a time for efficiency
      n_chunks <- ceiling(nrow(top_dissim) / chunk_size)

      p_values <- tryCatch({
        all_p_values <- numeric(nrow(top_dissim))

        for(chunk_idx in 1:n_chunks) {
          # Calculate chunk indices
          start_idx <- (chunk_idx - 1) * chunk_size + 1
          end_idx <- min(chunk_idx * chunk_size, nrow(top_dissim))
          chunk_indices <- start_idx:end_idx

          # Process this chunk in parallel
          chunk_results <- furrr::future_map_dbl(
            top_dissim$nodes_list[chunk_indices],
            test_single_candidate,
            .options = furrr::furrr_options(
              seed = TRUE,
              globals = list(
                test_combined_contribution = test_combined_contribution,
                compute_averaged_jaccard = compute_averaged_jaccard,
                compute_jaccard_for_contribution = compute_jaccard_for_contribution,
                networks_flat_g1 = networks_flat_g1,
                networks_flat_g2 = networks_flat_g2,
                node_names = node_names,
                n_permutations = n_permutations
              )
            )
          )

          # Store results
          all_p_values[chunk_indices] <- chunk_results

          # Update progress (50% base + progress within permutation phase)
          if(!is.null(progress_callback)) {
            progress <- 0.5 + (end_idx / total_candidates) * 0.5
            progress_callback(progress)
          }

          if(verbose) message("[DEBUG brute_force] Dissimilarity chunk ", chunk_idx, "/", n_chunks,
                  " complete (", end_idx, "/", nrow(top_dissim), " candidates)")
        }

        all_p_values
      }, error = function(e) {
        if(verbose) {
          message("[DEBUG brute_force] PARALLEL ERROR: ", e$message)
          message("[DEBUG brute_force] Falling back to serial execution...")
        }
        sapply(seq_along(top_dissim$nodes_list), function(i) {
          if(verbose && i %% 20 == 1) message("[DEBUG brute_force] Fallback serial: ", i, "/", length(top_dissim$nodes_list))
          if(!is.null(progress_callback)) {
            progress_callback(0.5 + (i / total_candidates) * 0.5)
          }
          test_single_candidate(top_dissim$nodes_list[[i]])
        })
      })
      if(verbose) {
        message("[DEBUG brute_force] future_map_dbl for dissimilarity COMPLETED")
        message("[DEBUG brute_force] Time: ", Sys.time())
      }
      top_dissim$p_value <- p_values
    } else {
      if(verbose) message("[DEBUG brute_force] Using SERIAL for dissimilarity (", nrow(top_dissim), " candidates)")
      # Serial candidate testing
      for(i in 1:nrow(top_dissim)) {
        if(verbose && i %% 10 == 1) message("[DEBUG brute_force] Serial dissimilarity candidate ", i, "/", nrow(top_dissim))
        if(!is.null(progress_callback)) {
          progress_callback(0.5 + (i / (nrow(top_dissim) + nrow(top_sim))) * 0.5)
        }
        top_dissim$p_value[i] <- test_single_candidate(top_dissim$nodes_list[[i]])
      }
      if(verbose) message("[DEBUG brute_force] Serial dissimilarity COMPLETED")
    }
  } else {
    if(verbose) message("[DEBUG brute_force] No dissimilarity candidates to test")
  }

  # Test similarity candidates
  if(nrow(top_sim) > 0) {
    top_sim$p_value <- NA
    top_sim$significant <- NA

    # ========== C++ BATCH PROCESSING (FASTEST) ==========
    if(use_cpp_backend && exists("batch_test_candidates_fast")) {
      if(verbose) {
        message("[DEBUG brute_force] Using C++ BATCH PROCESSING for similarity (", nrow(top_sim), " candidates)")
        message("[DEBUG brute_force] OpenMP threads: ", CPP_OPENMP_THREADS)
        message("[DEBUG brute_force] Time: ", Sys.time())
      }

      batch_start_time <- Sys.time()

      batch_results <- batch_test_candidates_fast(
        networks_g1 = networks_flat_g1,
        networks_g2 = networks_flat_g2,
        node_names = node_names,
        candidates_list = top_sim$nodes_list,
        n_permutations = n_permutations,
        n_threads = 0,  # Auto-detect
        progress_callback = function(p) {
          if(!is.null(progress_callback)) {
            progress_callback(0.75 + (p * 0.25))  # 75-100% for similarity
          }
        }
      )

      top_sim$p_value <- batch_results$p_value

      batch_elapsed <- as.numeric(difftime(Sys.time(), batch_start_time, units = "secs"))
      if(verbose) {
        message("[DEBUG brute_force] C++ batch processing (similarity) COMPLETE in ", round(batch_elapsed, 1), " seconds")
        message("[DEBUG brute_force] Time: ", Sys.time())
      }

    } else if(actual_use_parallel && nrow(top_sim) >= 3) {
      if(verbose) {
        message("[DEBUG brute_force] Using PARALLEL for similarity (", nrow(top_sim), " candidates)")
        message("[DEBUG brute_force] Using ", actual_n_workers, " workers (adaptive selection)")
        message("[DEBUG brute_force] Starting future_map_dbl for similarity...")
        message("[DEBUG brute_force] Time: ", Sys.time())
      }

      # Calculate progress parameters (continuing from dissimilarity)
      total_candidates <- nrow(top_dissim) + nrow(top_sim)
      dissim_done <- nrow(top_dissim)

      # Use chunk-based processing for progress updates
      chunk_size <- max(actual_n_workers * 2, 10)
      n_chunks <- ceiling(nrow(top_sim) / chunk_size)

      p_values <- tryCatch({
        all_p_values <- numeric(nrow(top_sim))

        for(chunk_idx in 1:n_chunks) {
          # Calculate chunk indices
          start_idx <- (chunk_idx - 1) * chunk_size + 1
          end_idx <- min(chunk_idx * chunk_size, nrow(top_sim))
          chunk_indices <- start_idx:end_idx

          # Process this chunk in parallel
          chunk_results <- furrr::future_map_dbl(
            top_sim$nodes_list[chunk_indices],
            test_single_candidate,
            .options = furrr::furrr_options(
              seed = TRUE,
              globals = list(
                test_combined_contribution = test_combined_contribution,
                compute_averaged_jaccard = compute_averaged_jaccard,
                compute_jaccard_for_contribution = compute_jaccard_for_contribution,
                networks_flat_g1 = networks_flat_g1,
                networks_flat_g2 = networks_flat_g2,
                node_names = node_names,
                n_permutations = n_permutations
              )
            )
          )

          # Store results
          all_p_values[chunk_indices] <- chunk_results

          # Update progress (continuing from dissimilarity progress)
          if(!is.null(progress_callback)) {
            progress <- 0.5 + ((dissim_done + end_idx) / total_candidates) * 0.5
            progress_callback(progress)
          }

          if(verbose) message("[DEBUG brute_force] Similarity chunk ", chunk_idx, "/", n_chunks,
                  " complete (", end_idx, "/", nrow(top_sim), " candidates)")
        }

        all_p_values
      }, error = function(e) {
        if(verbose) {
          message("[DEBUG brute_force] PARALLEL ERROR: ", e$message)
          message("[DEBUG brute_force] Falling back to serial execution...")
        }
        sapply(seq_along(top_sim$nodes_list), function(i) {
          if(verbose && i %% 20 == 1) message("[DEBUG brute_force] Fallback serial: ", i, "/", length(top_sim$nodes_list))
          if(!is.null(progress_callback)) {
            progress_callback(0.5 + ((dissim_done + i) / total_candidates) * 0.5)
          }
          test_single_candidate(top_sim$nodes_list[[i]])
        })
      })
      if(verbose) {
        message("[DEBUG brute_force] future_map_dbl for similarity COMPLETED")
        message("[DEBUG brute_force] Time: ", Sys.time())
      }
      top_sim$p_value <- p_values
    } else {
      if(verbose) message("[DEBUG brute_force] Using SERIAL for similarity (", nrow(top_sim), " candidates)")
      # Serial candidate testing
      for(i in 1:nrow(top_sim)) {
        if(verbose && i %% 10 == 1) message("[DEBUG brute_force] Serial similarity candidate ", i, "/", nrow(top_sim))
        if(!is.null(progress_callback)) {
          progress_callback(0.5 + ((nrow(top_dissim) + i) / (nrow(top_dissim) + nrow(top_sim))) * 0.5)
        }
        top_sim$p_value[i] <- test_single_candidate(top_sim$nodes_list[[i]])
      }
      if(verbose) message("[DEBUG brute_force] Serial similarity COMPLETED")
    }
  } else {
    if(verbose) message("[DEBUG brute_force] No similarity candidates to test")
  }

  # ========== PHASE 5: Apply multiple comparison correction ==========
  if(verbose) message("[DEBUG brute_force] PHASE 5: Applying multiple comparison correction (", correction_method, ")...")
  # Collect all p-values
  all_pvals <- c(
    if(nrow(top_dissim) > 0) top_dissim$p_value else NULL,
    if(nrow(top_sim) > 0) top_sim$p_value else NULL
  )

  # Apply correction if method is not "none"
  if(correction_method != "none" && length(all_pvals) > 0) {
    adjusted_pvals <- p.adjust(all_pvals, method = correction_method)

    # Split back to dissim and sim
    n_dissim <- if(nrow(top_dissim) > 0) nrow(top_dissim) else 0
    n_sim <- if(nrow(top_sim) > 0) nrow(top_sim) else 0

    if(n_dissim > 0) {
      top_dissim$p_adjusted <- adjusted_pvals[1:n_dissim]
      top_dissim$significant <- top_dissim$p_adjusted < 0.05
    }
    if(n_sim > 0) {
      top_sim$p_adjusted <- adjusted_pvals[(n_dissim + 1):(n_dissim + n_sim)]
      top_sim$significant <- top_sim$p_adjusted < 0.05
    }
  } else {
    # No correction - use raw p-values
    if(nrow(top_dissim) > 0) {
      top_dissim$p_adjusted <- top_dissim$p_value
      top_dissim$significant <- top_dissim$p_value < 0.05
    }
    if(nrow(top_sim) > 0) {
      top_sim$p_adjusted <- top_sim$p_value
      top_sim$significant <- top_sim$p_value < 0.05
    }
  }

  # Summary statistics
  n_sig_dissim <- if(nrow(top_dissim) > 0) sum(top_dissim$significant, na.rm = TRUE) else 0
  n_sig_sim <- if(nrow(top_sim) > 0) sum(top_sim$significant, na.rm = TRUE) else 0

  if(verbose) {
    message("[DEBUG brute_force] COMPLETE! Significant: ", n_sig_dissim, " dissim, ", n_sig_sim, " sim")
    message("[DEBUG brute_force] Total time finished: ", Sys.time())
  }

  return(list(
    method = "brute_force",
    # Separate baselines by approach
    baseline_weighted = baseline_weighted,
    baseline_percolation = baseline_percolation,
    baseline_persistence = baseline_persistence,
    # Results
    total_combinations = nrow(all_results),
    all_results = all_results,
    top_dissimilarity = top_dissim,
    top_similarity = top_sim,
    n_significant_dissim = n_sig_dissim,
    n_significant_sim = n_sig_sim,
    # Metadata
    max_combo_size = max_combo_size,
    n_permutations = n_permutations,
    correction_method = correction_method,
    candidate_filter = candidate_filter,
    n_tested_dissim = nrow(top_dissim),
    n_tested_sim = nrow(top_sim),
    node_names = node_names,
    n_nodes = n_nodes,
    n_methods_weighted = n_weighted,
    n_methods_percolation = n_percolation,
    n_methods_persistence = n_persistence,
    n_thresholds = n_thresholds,
    approaches_used = names(valid_baselines[valid_baselines]),
    group1_name = group1,
    group2_name = group2,
    seed = seed
  ))
}


#' ============================================================================
#' LASSO REGRESSION FOR REGIONAL CONTRIBUTION ANALYSIS
#' ============================================================================
#'
#' Uses penalized logistic regression to identify which regions' values
#' best predict group membership. Complements brute force by providing
#' a different perspective: which regions differ in VALUE (not just network topology).
#'

#' Run Lasso Regional Contribution Analysis
#'
#' @param raw_data Data frame with subject data (rows = subjects, columns include regions)
#' @param group1_name Name of first group
#' @param group2_name Name of second group
#' @param group_col Column name containing group labels
#' @param regions Vector of region column names to use as features
#' @param alpha Elastic net mixing parameter (1 = Lasso, 0 = Ridge, 0.5 = Elastic Net)
#' @param nfolds Number of cross-validation folds
#' @param lambda_choice "min" for optimal lambda, "1se" for conservative (1 SE rule)
#' @param seed Random seed for reproducibility (default: 42). Set to NULL for no seed.
#' @return List containing Lasso results, coefficients, and model fit
run_lasso_regional_contribution <- function(raw_data,
                                             group1_name,
                                             group2_name,
                                             group_col,
                                             regions,
                                             alpha = 1,
                                             nfolds = 10,
                                             lambda_choice = "min",
                                             seed = 42) {

  # Set seed for reproducibility (cv.glmnet uses random fold assignments)
  if(!is.null(seed)) {
    set.seed(seed)
  }

  # Check if glmnet is available
  if(!GLMNET_AVAILABLE) {
    return(list(
      success = FALSE,
      error = "glmnet package not installed. Install with: install.packages('glmnet')"
    ))
  }

  # Validate inputs
  if(is.null(group_col) || length(group_col) == 0 || group_col == "") {
    return(list(
      success = FALSE,
      error = "Group column not specified. Please ensure data has a group column defined."
    ))
  }

  if(!(group_col %in% colnames(raw_data))) {
    return(list(
      success = FALSE,
      error = paste0("Group column '", group_col, "' not found in data. Available columns: ",
                     paste(colnames(raw_data), collapse = ", "))
    ))
  }

  # Check if group names exist in the data
  available_groups <- unique(raw_data[[group_col]])
  if(!(group1_name %in% available_groups)) {
    return(list(
      success = FALSE,
      error = paste0("Group '", group1_name, "' not found. Available groups: ",
                     paste(available_groups, collapse = ", "))
    ))
  }
  if(!(group2_name %in% available_groups)) {
    return(list(
      success = FALSE,
      error = paste0("Group '", group2_name, "' not found. Available groups: ",
                     paste(available_groups, collapse = ", "))
    ))
  }

  # Filter to just the two groups being compared
  data_subset <- raw_data[raw_data[[group_col]] %in% c(group1_name, group2_name), ]

  if(nrow(data_subset) < 10) {
    return(list(
      success = FALSE,
      error = paste0("Insufficient samples (", nrow(data_subset), "). Need at least 10 subjects.")
    ))
  }

  # Check for valid regions
  valid_regions <- regions[regions %in% colnames(data_subset)]
  if(length(valid_regions) == 0) {
    return(list(
      success = FALSE,
      error = "No valid region columns found in data."
    ))
  }

  # Prepare feature matrix - handle missing values
  X_raw <- as.matrix(data_subset[, valid_regions, drop = FALSE])

  # Check for columns with all NA
  valid_cols <- apply(X_raw, 2, function(x) !all(is.na(x)))
  if(sum(valid_cols) < 2) {
    return(list(
      success = FALSE,
      error = "Not enough valid regions with data (need at least 2)."
    ))
  }

  X_raw <- X_raw[, valid_cols, drop = FALSE]
  valid_regions <- valid_regions[valid_cols]

  # Impute missing values with column means (simple imputation)
  for(j in 1:ncol(X_raw)) {
    na_idx <- is.na(X_raw[, j])
    if(any(na_idx)) {
      X_raw[na_idx, j] <- mean(X_raw[!na_idx, j], na.rm = TRUE)
    }
  }

  # Standardize features (required for Lasso)
  X_scaled <- scale(X_raw)

  # Binary outcome: 0 = group1, 1 = group2
  y <- ifelse(data_subset[[group_col]] == group2_name, 1, 0)

  # Check class balance
  n_g1 <- sum(y == 0)
  n_g2 <- sum(y == 1)

  if(n_g1 < 3 || n_g2 < 3) {
    return(list(
      success = FALSE,
      error = paste0("Insufficient samples per group (Group1: ", n_g1, ", Group2: ", n_g2, "). Need at least 3 per group.")
    ))
  }

  # Adjust nfolds if necessary
  nfolds <- min(nfolds, min(n_g1, n_g2))
  if(nfolds < 3) nfolds <- 3

  # Run cross-validated Lasso
  tryCatch({
    cv_fit <- cv.glmnet(X_scaled, y, family = "binomial",
                        alpha = alpha, nfolds = nfolds,
                        type.measure = "deviance")

    # Choose lambda based on user preference
    if(lambda_choice == "1se") {
      best_lambda <- cv_fit$lambda.1se
    } else {
      best_lambda <- cv_fit$lambda.min
    }

    # Extract coefficients (exclude intercept)
    coefs <- as.vector(coef(cv_fit, s = best_lambda))[-1]
    names(coefs) <- valid_regions

    # Build results table
    results <- data.frame(
      Region = valid_regions,
      Coefficient = coefs,
      Abs_Coefficient = abs(coefs),
      Direction = ifelse(coefs > 0, paste0("Higher in ", group2_name),
                         ifelse(coefs < 0, paste0("Higher in ", group1_name), "Not Selected")),
      Is_Selected = coefs != 0,
      stringsAsFactors = FALSE
    )

    # Sort by absolute coefficient (most important first)
    results <- results[order(-results$Abs_Coefficient), ]
    rownames(results) <- NULL

    # Calculate model performance metrics
    pred_probs <- predict(cv_fit, newx = X_scaled, s = best_lambda, type = "response")
    pred_class <- ifelse(pred_probs > 0.5, 1, 0)
    accuracy <- mean(pred_class == y)

    # Confusion matrix
    confusion <- table(Actual = y, Predicted = pred_class)

    # Get mean values for each group for interpretation
    group_means <- data.frame(
      Region = valid_regions,
      Mean_Group1 = colMeans(X_raw[y == 0, , drop = FALSE], na.rm = TRUE),
      Mean_Group2 = colMeans(X_raw[y == 1, , drop = FALSE], na.rm = TRUE)
    )
    group_means$Difference <- group_means$Mean_Group2 - group_means$Mean_Group1

    # Merge with results
    results <- merge(results, group_means, by = "Region", all.x = TRUE)
    results <- results[order(-results$Abs_Coefficient), ]

    return(list(
      success = TRUE,
      results = results,
      cv_fit = cv_fit,
      best_lambda = best_lambda,
      lambda_min = cv_fit$lambda.min,
      lambda_1se = cv_fit$lambda.1se,
      n_selected = sum(coefs != 0),
      n_total_regions = length(valid_regions),
      accuracy = accuracy,
      confusion = confusion,
      alpha = alpha,
      nfolds = nfolds,
      lambda_choice = lambda_choice,
      n_subjects = nrow(data_subset),
      n_group1 = n_g1,
      n_group2 = n_g2,
      group1_name = group1_name,
      group2_name = group2_name,
      seed = seed,
      scaling_center = attr(X_scaled, "scaled:center"),
      scaling_scale = attr(X_scaled, "scaled:scale")
    ))

  }, error = function(e) {
    return(list(
      success = FALSE,
      error = paste("Lasso fitting error:", e$message)
    ))
  })
}


#' Compare Lasso Results with Brute Force Results
#'
#' Creates a comparison table showing which regions are flagged by both methods
#'
#' @param lasso_result Output from run_lasso_regional_contribution
#' @param brute_force_result Output from brute_force_discovery
#' @return Data frame comparing both methods
compare_lasso_brute_force <- function(lasso_result, brute_force_result) {

  if(!lasso_result$success) {
    return(NULL)
  }

  # Get Lasso results
  lasso_df <- lasso_result$results[, c("Region", "Coefficient", "Is_Selected", "Direction")]

  # Get brute force singles results
  if(!is.null(brute_force_result$all_results)) {
    bf_singles <- brute_force_result$all_results[brute_force_result$all_results$size == 1, ]
    bf_singles$Region <- bf_singles$nodes

    # Get p-values from top_dissimilarity and top_similarity
    top_dissim <- brute_force_result$top_dissimilarity
    top_sim <- brute_force_result$top_similarity

    # Filter for singles only
    if(!is.null(top_dissim) && nrow(top_dissim) > 0) {
      top_dissim_singles <- top_dissim[top_dissim$size == 1, ]
    } else {
      top_dissim_singles <- data.frame()
    }

    if(!is.null(top_sim) && nrow(top_sim) > 0) {
      top_sim_singles <- top_sim[top_sim$size == 1, ]
    } else {
      top_sim_singles <- data.frame()
    }

    # Combine
    bf_tested <- rbind(
      if(nrow(top_dissim_singles) > 0) top_dissim_singles[, c("nodes", "contribution", "p_value", "p_adjusted", "significant")] else NULL,
      if(nrow(top_sim_singles) > 0) top_sim_singles[, c("nodes", "contribution", "p_value", "p_adjusted", "significant")] else NULL
    )

    if(!is.null(bf_tested) && nrow(bf_tested) > 0) {
      names(bf_tested)[1] <- "Region"
    }
  } else {
    bf_tested <- data.frame()
  }

  # Merge
  comparison <- merge(lasso_df, bf_tested, by = "Region", all = TRUE)

  # Add combined score
  comparison$Lasso_Selected <- comparison$Is_Selected
  comparison$BruteForce_Significant <- !is.na(comparison$significant) & comparison$significant

  # Both methods agree
  comparison$Both_Methods <- comparison$Lasso_Selected & comparison$BruteForce_Significant

  # Sort by agreement then by Lasso coefficient
  comparison <- comparison[order(-comparison$Both_Methods, -abs(comparison$Coefficient)), ]

  return(comparison)
}
