// permutation_core.cpp
// High-performance permutation testing with OpenMP parallelization
// For ConsensusConnectR - Artificial Brain Area Discovery
//
// This replaces the pure R permutation loop with compiled C++ code
// that uses OpenMP for true shared-memory parallelization on Windows.
// No process spawning overhead = can actually use all 96 cores.

// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::plugins(openmp)]]

#include <RcppArmadillo.h>
#include <vector>
#include <algorithm>
#include <random>

#ifdef _OPENMP
#include <omp.h>
#endif

using namespace Rcpp;
using namespace arma;

// =============================================================================
// CORE JACCARD COMPUTATION (C++ VERSION)
// =============================================================================

// Compute weighted Jaccard similarity between two matrices
// This is the C++ equivalent of compute_jaccard_for_contribution()
// [[Rcpp::export]]
double compute_jaccard_cpp(const arma::mat& mat1, const arma::mat& mat2) {
  if (mat1.n_rows != mat2.n_rows || mat1.n_cols != mat2.n_cols) {
    return NA_REAL;
  }

  int n = mat1.n_rows;
  double numerator = 0.0;
  double denominator = 0.0;

  // Process upper triangle only (symmetric matrices)
  for (int i = 0; i < n; i++) {
    for (int j = i + 1; j < n; j++) {
      double w1 = std::abs(mat1(i, j));
      double w2 = std::abs(mat2(i, j));

      numerator += std::min(w1, w2);
      denominator += std::max(w1, w2);
    }
  }

  if (denominator == 0.0) return 0.0;
  return numerator / denominator;
}

// Compute Jaccard with node exclusion
// [[Rcpp::export]]
double compute_jaccard_exclude_cpp(const arma::mat& mat1, const arma::mat& mat2,
                                    const arma::uvec& exclude_indices) {
  if (mat1.n_rows != mat2.n_rows || mat1.n_cols != mat2.n_cols) {
    return NA_REAL;
  }

  int n = mat1.n_rows;

  // Create inclusion mask (true = include this node)
  std::vector<bool> include(n, true);
  for (size_t k = 0; k < exclude_indices.n_elem; k++) {
    int idx = exclude_indices(k);
    if (idx >= 0 && idx < n) {
      include[idx] = false;
    }
  }

  double numerator = 0.0;
  double denominator = 0.0;

  // Process upper triangle, skipping excluded nodes
  for (int i = 0; i < n; i++) {
    if (!include[i]) continue;
    for (int j = i + 1; j < n; j++) {
      if (!include[j]) continue;

      double w1 = std::abs(mat1(i, j));
      double w2 = std::abs(mat2(i, j));

      numerator += std::min(w1, w2);
      denominator += std::max(w1, w2);
    }
  }

  if (denominator == 0.0) return 0.0;
  return numerator / denominator;
}

// =============================================================================
// AVERAGED JACCARD ACROSS MULTIPLE MATRICES
// =============================================================================

// Compute average Jaccard across a list of matrix pairs
// [[Rcpp::export]]
double compute_averaged_jaccard_cpp(const Rcpp::List& matrices_g1,
                                     const Rcpp::List& matrices_g2,
                                     const arma::uvec& exclude_indices) {
  int n_matrices = matrices_g1.size();
  if (n_matrices == 0 || n_matrices != matrices_g2.size()) {
    return NA_REAL;
  }

  double sum_jaccard = 0.0;
  int valid_count = 0;

  for (int i = 0; i < n_matrices; i++) {
    arma::mat m1 = Rcpp::as<arma::mat>(matrices_g1[i]);
    arma::mat m2 = Rcpp::as<arma::mat>(matrices_g2[i]);

    double j;
    if (exclude_indices.n_elem > 0) {
      j = compute_jaccard_exclude_cpp(m1, m2, exclude_indices);
    } else {
      j = compute_jaccard_cpp(m1, m2);
    }

    if (!ISNA(j)) {
      sum_jaccard += j;
      valid_count++;
    }
  }

  if (valid_count == 0) return NA_REAL;
  return sum_jaccard / valid_count;
}

// =============================================================================
// PARALLEL PERMUTATION TEST (THE KEY FUNCTION)
// =============================================================================

// Single permutation iteration - uses pre-converted arma matrices (thread-safe)
double run_single_permutation_safe(
    const std::vector<arma::mat>& arma_g1,
    const std::vector<arma::mat>& arma_g2,
    const arma::uvec& exclude_indices,
    int n_nodes,
    std::mt19937& rng) {

  // Generate independent permutations for each group
  std::vector<int> perm_g1(n_nodes), perm_g2(n_nodes);
  for (int i = 0; i < n_nodes; i++) {
    perm_g1[i] = i;
    perm_g2[i] = i;
  }
  std::shuffle(perm_g1.begin(), perm_g1.end(), rng);
  std::shuffle(perm_g2.begin(), perm_g2.end(), rng);

  int n_matrices = arma_g1.size();
  double sum_baseline = 0.0;
  double sum_without = 0.0;
  int valid_count = 0;

  for (int m = 0; m < n_matrices; m++) {
    const arma::mat& m1_orig = arma_g1[m];
    const arma::mat& m2_orig = arma_g2[m];

    // Apply permutation to matrices
    arma::mat m1_perm(n_nodes, n_nodes);
    arma::mat m2_perm(n_nodes, n_nodes);

    for (int i = 0; i < n_nodes; i++) {
      for (int j = 0; j < n_nodes; j++) {
        m1_perm(i, j) = m1_orig(perm_g1[i], perm_g1[j]);
        m2_perm(i, j) = m2_orig(perm_g2[i], perm_g2[j]);
      }
    }

    // Compute Jaccard baseline
    double j_baseline = compute_jaccard_cpp(m1_perm, m2_perm);

    // Compute Jaccard without excluded nodes
    double j_without;
    if (exclude_indices.n_elem > 0) {
      j_without = compute_jaccard_exclude_cpp(m1_perm, m2_perm, exclude_indices);
    } else {
      j_without = j_baseline;
    }

    if (!ISNA(j_baseline) && !ISNA(j_without)) {
      sum_baseline += j_baseline;
      sum_without += j_without;
      valid_count++;
    }
  }

  if (valid_count == 0) return NA_REAL;

  double avg_baseline = sum_baseline / valid_count;
  double avg_without = sum_without / valid_count;

  return avg_without - avg_baseline;
}

// Main parallel permutation test function
// This is called from R to replace the slow R loop
// [[Rcpp::export]]
Rcpp::List parallel_permutation_test_cpp(
    const Rcpp::List& matrices_g1,
    const Rcpp::List& matrices_g2,
    const arma::uvec& exclude_indices,
    int n_permutations,
    int n_threads = 0,
    int seed = 42) {

  int n_matrices = matrices_g1.size();
  if (n_matrices == 0) {
    return Rcpp::List::create(
      Rcpp::Named("observed") = NA_REAL,
      Rcpp::Named("null_distribution") = Rcpp::NumericVector(0),
      Rcpp::Named("p_value") = NA_REAL,
      Rcpp::Named("n_permutations") = 0,
      Rcpp::Named("n_threads_used") = 0,
      Rcpp::Named("error") = "Empty matrix list"
    );
  }

  // CRITICAL: Convert all Rcpp matrices to arma::mat BEFORE parallel region
  // Rcpp objects are NOT thread-safe and will crash if accessed from multiple threads
  std::vector<arma::mat> arma_g1(n_matrices);
  std::vector<arma::mat> arma_g2(n_matrices);

  for (int i = 0; i < n_matrices; i++) {
    arma_g1[i] = Rcpp::as<arma::mat>(matrices_g1[i]);
    arma_g2[i] = Rcpp::as<arma::mat>(matrices_g2[i]);
  }

  // Get dimensions from first matrix
  int n_nodes = arma_g1[0].n_rows;

  // Determine number of threads
  int max_threads = 1;
  #ifdef _OPENMP
  max_threads = omp_get_max_threads();
  #endif

  if (n_threads <= 0) {
    n_threads = max_threads;
  } else {
    n_threads = std::min(n_threads, max_threads);
  }

  // Compute observed statistic (single-threaded, before parallel region)
  arma::uvec empty_exclude;
  double baseline_J = compute_averaged_jaccard_cpp(matrices_g1, matrices_g2, empty_exclude);
  double J_without = compute_averaged_jaccard_cpp(matrices_g1, matrices_g2, exclude_indices);
  double observed_contribution = J_without - baseline_J;

  // Allocate null distribution
  std::vector<double> null_distribution(n_permutations);

  // Parallel permutation loop - now uses thread-safe arma vectors
  #ifdef _OPENMP
  #pragma omp parallel num_threads(n_threads)
  {
    // Each thread gets its own RNG with unique seed
    int thread_id = omp_get_thread_num();
    std::mt19937 thread_rng(seed + thread_id * 1000);

    #pragma omp for schedule(dynamic)
    for (int perm = 0; perm < n_permutations; perm++) {
      null_distribution[perm] = run_single_permutation_safe(
        arma_g1, arma_g2, exclude_indices, n_nodes, thread_rng
      );
    }
  }
  #else
  // Fallback to serial if OpenMP not available
  std::mt19937 rng(seed);
  for (int perm = 0; perm < n_permutations; perm++) {
    null_distribution[perm] = run_single_permutation_safe(
      arma_g1, arma_g2, exclude_indices, n_nodes, rng
    );
  }
  #endif

  // Compute two-tailed p-value
  int count_extreme = 0;
  for (int i = 0; i < n_permutations; i++) {
    if (std::abs(null_distribution[i]) >= std::abs(observed_contribution)) {
      count_extreme++;
    }
  }
  double p_value = (double)(count_extreme + 1) / (double)(n_permutations + 1);

  // Return results
  return Rcpp::List::create(
    Rcpp::Named("observed") = observed_contribution,
    Rcpp::Named("null_distribution") = Rcpp::wrap(null_distribution),
    Rcpp::Named("p_value") = p_value,
    Rcpp::Named("n_permutations") = n_permutations,
    Rcpp::Named("n_threads_used") = n_threads,
    Rcpp::Named("baseline_jaccard") = baseline_J,
    Rcpp::Named("jaccard_without") = J_without
  );
}

// =============================================================================
// BATCH PROCESSING FOR MULTIPLE CANDIDATES
// =============================================================================

// Helper: run batch permutation for one candidate using pre-converted data
double run_candidate_permutation_test(
    const std::vector<arma::mat>& arma_g1,
    const std::vector<arma::mat>& arma_g2,
    const arma::uvec& exclude_indices,
    int n_nodes,
    int n_permutations,
    int seed) {

  // Compute observed statistic
  int n_matrices = arma_g1.size();
  double sum_baseline = 0.0, sum_without = 0.0;
  int valid_count = 0;

  for (int m = 0; m < n_matrices; m++) {
    double j_baseline = compute_jaccard_cpp(arma_g1[m], arma_g2[m]);
    double j_without;
    if (exclude_indices.n_elem > 0) {
      j_without = compute_jaccard_exclude_cpp(arma_g1[m], arma_g2[m], exclude_indices);
    } else {
      j_without = j_baseline;
    }
    if (!ISNA(j_baseline) && !ISNA(j_without)) {
      sum_baseline += j_baseline;
      sum_without += j_without;
      valid_count++;
    }
  }

  if (valid_count == 0) return NA_REAL;
  double observed = (sum_without / valid_count) - (sum_baseline / valid_count);

  // Run permutations
  std::vector<double> null_dist(n_permutations);
  std::mt19937 rng(seed);

  for (int perm = 0; perm < n_permutations; perm++) {
    null_dist[perm] = run_single_permutation_safe(
      arma_g1, arma_g2, exclude_indices, n_nodes, rng
    );
  }

  // Compute p-value
  int count_extreme = 0;
  for (int i = 0; i < n_permutations; i++) {
    if (std::abs(null_dist[i]) >= std::abs(observed)) {
      count_extreme++;
    }
  }

  return (double)(count_extreme + 1) / (double)(n_permutations + 1);
}

// Test multiple candidates in parallel (outer parallelization)
// [[Rcpp::export]]
Rcpp::List batch_permutation_test_cpp(
    const Rcpp::List& matrices_g1,
    const Rcpp::List& matrices_g2,
    const Rcpp::List& candidates_list,  // List of integer vectors (exclude indices)
    int n_permutations,
    int n_threads = 0,
    int seed = 42) {

  int n_candidates = candidates_list.size();
  int n_matrices = matrices_g1.size();

  if (n_matrices == 0 || n_candidates == 0) {
    return Rcpp::List::create(
      Rcpp::Named("p_values") = Rcpp::NumericVector(0),
      Rcpp::Named("observed_values") = Rcpp::NumericVector(0),
      Rcpp::Named("n_candidates") = 0,
      Rcpp::Named("n_permutations") = n_permutations,
      Rcpp::Named("n_threads") = 0
    );
  }

  // CRITICAL: Pre-convert ALL Rcpp data to C++ structures BEFORE parallel region
  // Rcpp objects are NOT thread-safe!

  // Convert matrices
  std::vector<arma::mat> arma_g1(n_matrices);
  std::vector<arma::mat> arma_g2(n_matrices);
  for (int i = 0; i < n_matrices; i++) {
    arma_g1[i] = Rcpp::as<arma::mat>(matrices_g1[i]);
    arma_g2[i] = Rcpp::as<arma::mat>(matrices_g2[i]);
  }

  int n_nodes = arma_g1[0].n_rows;

  // Convert all candidate exclude lists
  std::vector<arma::uvec> all_excludes(n_candidates);
  for (int c = 0; c < n_candidates; c++) {
    all_excludes[c] = Rcpp::as<arma::uvec>(candidates_list[c]);
  }

  // Allocate results
  std::vector<double> p_values(n_candidates);
  std::vector<double> observed_values(n_candidates);
  std::vector<double> ci_lower(n_candidates);
  std::vector<double> ci_upper(n_candidates);

  // Determine threads
  int max_threads = 1;
  #ifdef _OPENMP
  max_threads = omp_get_max_threads();
  #endif

  if (n_threads <= 0) {
    n_threads = max_threads;
  }

  // Parallel loop over candidates - now using only C++ data structures
  #ifdef _OPENMP
  #pragma omp parallel for num_threads(n_threads) schedule(dynamic)
  #endif
  for (int c = 0; c < n_candidates; c++) {
    // Compute observed for this candidate
    int n_mat = arma_g1.size();
    double sum_baseline = 0.0, sum_without = 0.0;
    int valid_count = 0;

    for (int m = 0; m < n_mat; m++) {
      double j_baseline = compute_jaccard_cpp(arma_g1[m], arma_g2[m]);
      double j_without;
      if (all_excludes[c].n_elem > 0) {
        j_without = compute_jaccard_exclude_cpp(arma_g1[m], arma_g2[m], all_excludes[c]);
      } else {
        j_without = j_baseline;
      }
      if (!ISNA(j_baseline) && !ISNA(j_without)) {
        sum_baseline += j_baseline;
        sum_without += j_without;
        valid_count++;
      }
    }

    double observed = (valid_count > 0) ?
      ((sum_without / valid_count) - (sum_baseline / valid_count)) : NA_REAL;
    observed_values[c] = observed;

    // Run permutations for this candidate
    std::vector<double> null_dist(n_permutations);
    std::mt19937 rng(seed + c * 1000);  // Unique seed per candidate

    for (int perm = 0; perm < n_permutations; perm++) {
      null_dist[perm] = run_single_permutation_safe(
        arma_g1, arma_g2, all_excludes[c], n_nodes, rng
      );
    }

    // Compute p-value
    int count_extreme = 0;
    for (int i = 0; i < n_permutations; i++) {
      if (std::abs(null_dist[i]) >= std::abs(observed)) {
        count_extreme++;
      }
    }
    p_values[c] = (double)(count_extreme + 1) / (double)(n_permutations + 1);

    // Compute 95% CI (2.5% and 97.5% quantiles) from null distribution
    // Sort a copy of null_dist to find quantiles
    std::vector<double> sorted_null = null_dist;
    std::sort(sorted_null.begin(), sorted_null.end());

    int idx_lower = (int)(0.025 * n_permutations);
    int idx_upper = (int)(0.975 * n_permutations);
    if (idx_lower < 0) idx_lower = 0;
    if (idx_upper >= n_permutations) idx_upper = n_permutations - 1;

    ci_lower[c] = sorted_null[idx_lower];
    ci_upper[c] = sorted_null[idx_upper];
  }

  return Rcpp::List::create(
    Rcpp::Named("p_values") = Rcpp::wrap(p_values),
    Rcpp::Named("observed_values") = Rcpp::wrap(observed_values),
    Rcpp::Named("ci_lower") = Rcpp::wrap(ci_lower),
    Rcpp::Named("ci_upper") = Rcpp::wrap(ci_upper),
    Rcpp::Named("n_candidates") = n_candidates,
    Rcpp::Named("n_permutations") = n_permutations,
    Rcpp::Named("n_threads") = n_threads
  );
}

// =============================================================================
// UTILITY FUNCTIONS
// =============================================================================

// Get OpenMP thread count info
// [[Rcpp::export]]
Rcpp::List get_openmp_info() {
  int max_threads = 1;
  bool openmp_available = false;

  #ifdef _OPENMP
  max_threads = omp_get_max_threads();
  openmp_available = true;
  #endif

  return Rcpp::List::create(
    Rcpp::Named("openmp_available") = openmp_available,
    Rcpp::Named("max_threads") = max_threads
  );
}

// Simple test function to verify compilation
// [[Rcpp::export]]
int test_cpp_compilation() {
  return 42;
}
