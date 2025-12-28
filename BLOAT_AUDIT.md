# Bloat Code Audit Report
## ConsensusConnectR/Persistence5

**Audit Date:** 2025-12-24
**Total Identified Bloat:** ~3,700 lines across 6 files

---

## Executive Summary

| Category | Count | Est. Lines |
|----------|-------|------------|
| Orphaned render outputs | 84 | ~1,500 |
| Dead code sections | 3 | ~200 |
| Unused module functions | 43 | ~2,000 |
| **Total** | **130+** | **~3,700** |

---

## 1. Orphaned Render Outputs in app.R

These outputs are defined but have **no corresponding UI element** to display them.

### 1.1 Orphaned renderPlot (60 items)

| Line | Output Name | Notes |
|------|-------------|-------|
| 5156 | `conservationStatsPlot` | |
| 5161 | `networkSimilarityMatrixPlot` | |
| 5187 | `summaryDashboardPlot` | |
| 5253 | `crossMethodEigenvectorPlot` | |
| 5263 | `crossMethodNodeStrengthPlot` | |
| 5274 | `hubRankingComparisonPlot` | |
| 5285 | `pipelineOverviewPlot` | |
| 5291 | `advancedMstMetricsPlot` | |
| 5299 | `mstCentralNodesPlot` | |
| 5307 | `advancedMstNetworksPlot` | |
| 5316 | `pcaAnalysisPlot` | |
| 5324 | `pcaLoadingsPlot` | |
| 5332 | `pcaVariancePlot` | |
| 5500 | `conservationCorrelationPlot` | |
| 5538 | `conservationInsightsPlot` | |
| 5568 | `persistenceVsPercolationHubsPlot` | |
| 5682 | `summaryAgreementDonut` | |
| 5730 | `summaryConfidenceHist` | |
| 5782 | `summaryApproachWeights` | |
| 5995 | `consensusOverviewTopHubsPlot` | Legacy |
| 6041 | `consensusOverviewAgreementPlot` | Legacy |
| 6103 | `comprehensiveConsensusHeatmapPlot` | |
| 6125 | `threeWayHubVennPlot` | |
| 6145 | `agreementMatrixPlot` | |
| 6339 | `disagreementHeatmapPlot` | |
| 6426 | `divergenceScatterPlot` | |
| 6651 | `zThresholdSensitivityPlot` | |
| 6749 | `agreementSensitivityPlot` | |
| 7026 | `summaryMethodAgreement` | |
| 7614 | `granularConsensusHeatmap` | |
| 7727 | `methodAgreementDistribution` | |
| 7884 | `approachVsMethodConsistency` | |
| 11132 | `validationNetworkSimilarity` | |
| 11273 | `consensusQualityPlot` | |
| 11362 | `calibrationValidationPlot` | |
| 11459 | `consensusHubRankingPlot` | |
| 11483 | `methodVarianceBarplot` | |
| 11503 | `consensusRegionalThreeWayPlot` | |
| 11578 | `weightedVsPercolationEigenvectorPlot` | |
| 11599 | `weightedVsPercolationNodeStrengthPlot` | |
| 11682 | `consensusThreeWayEigenvectorPlot` | |
| 11786 | `avgNodeStrengthByRegionPlot` | |
| 11796 | `rankBasedNodeStrengthPlot` | |
| 11815 | `avgEigenvectorByRegionPlot` | |
| 11825 | `rankBasedEigenvectorPlot` | |
| 11846 | `nodeStrengthSubregionsPlot` | |
| 11865 | `nodeStrengthRankSubregionsPlot` | |
| 11885 | `eigenvectorSubregionsPlot` | |
| 11904 | `eigenvectorRankSubregionsPlot` | |
| 12960 | `consensus_hub_agreement_matrix` | |
| 12998 | `consensus_overview_plot` | |
| 13539 | `persistenceDiagramPlot` | |
| 13545 | `bettiCurvesPlot` | |
| 13551 | `wassersteinDistancePlot` | |
| 13564 | `omstNetworkPlot` | |
| 13582 | `disparityNetworkPlot` | |
| 13600 | `spectralEmbeddingPlot` | |
| 13606 | `spectralClusteringPlot` | |
| 13625 | `nbsNetworkPlot` | |
| 13639 | `metaConsensusSummaryPlot` | |

### 1.2 Orphaned renderDataTable (10 items)

| Line | Output Name |
|------|-------------|
| 5589 | `persistenceStabilityTable` |
| 5629 | `summaryTopHubsTable` |
| 6503 | `methodSpecificHubsTable` |
| 6709 | `stableHubsTable` |
| 7801 | `superConsensusHubsTable` |
| 11209 | `methodConsistencyTable` |
| 11347 | `groupComparisonTests` |
| 11407 | `calibrationSummaryTable` |
| 12971 | `consensus_hubs_table` |
| 13657 | `robustnessScoresTable` |

### 1.3 Orphaned renderText (7 items)

| Line | Output Name |
|------|-------------|
| 5841 | `thresholdRecommendation` |
| 5874 | `thresholdQualityMetrics` |
| 5908 | `summaryKeyFindings` |
| 6250 | `agreementStatsSummary` |
| 6558 | `disagreementStatsSummary` |
| 11317 | `qualitySummaryText` |
| 11357 | `methodAgreementTests` |

### 1.4 Orphaned renderPrint (7 items)

| Line | Output Name |
|------|-------------|
| 12926 | `method_percolation_threshold` |
| 13004 | `consensus_summary_text` |
| 13570 | `omstMetricsSummary` |
| 13588 | `disparityStatsSummary` |
| 13612 | `spectralSummary` |
| 13631 | `nbsComponentsSummary` |
| 13647 | `metaConsensusSummaryText` |

---

## 2. Dead Code Sections in app.R

### 2.1 Legacy Overview Tab (DELETE)
- **Location:** Lines 5992-6085 (~94 lines)
- **Marker:** Comment states "LEGACY OVERVIEW TAB (To be deprecated)"
- **Contains:** `consensusOverviewTopHubsPlot`, `consensusOverviewAgreementPlot`
- **Action:** Delete entire section

### 2.2 Duplicate hubConservationPlot (DELETE 2 of 3)
Three definitions exist - only the last one executes:
- Line 5166-5185: First definition - **DELETE**
- Line 5401-5469: Second definition - **DELETE**
- Line 7199: Third definition - **KEEP**

### 2.3 Commented-out Code (DELETE)
- **Location:** Lines 11442-11456 (~15 lines)
- **Content:** Commented `networkSimilarityHeatmapPlot`
- **Action:** Delete

---

## 3. Unused Functions in Modules

### 3.1 visualization_functions.R (14 functions)

| Line | Function Name |
|------|---------------|
| 367 | `render_persistence_heatmap` |
| 1906 | `render_regional_summary_stats` |
| 2045 | `render_combined_eigenvector_by_region_multibar` |
| 2143 | `render_avg_eigenvector_by_region_subplot` |
| 2326 | `render_combined_node_strength_by_region_multibar` |
| 2425 | `render_avg_node_strength_by_region_subplot` |
| 2632 | `render_combined_eigenvector_rank_by_region_multibar` |
| 2938 | `render_combined_rank_by_region_node_strength` |
| 3109 | `render_single_bar_per_region_eigenvector` |
| 3865 | `render_conservation_comparison_plots` |
| 4106 | `render_node_importance_heatmap` |
| 4180 | `render_rank_trajectories` |
| 4242 | `render_stability_vs_importance` |
| 4310 | `render_persistent_nodes_summary` |

### 3.2 statistical_tests.R (13 functions)

| Line | Function Name |
|------|---------------|
| 574 | `compute_network_based_statistics` |
| 780 | `compute_group_comparison_statistics` |
| 879 | `compute_multiple_comparison_correction` |
| 899 | `compute_effect_size_matrix` |
| 1727 | `compute_multimethod_leave_one_out_contribution` |
| 2139 | `find_elbow_second_derivative` |
| 2264 | `greedy_forward_selection` |
| 2474 | `discover_both_directions` |
| 2528 | `compute_connectivity_difference_matrix` |
| 2579 | `find_optimal_clusters` |
| 2640 | `cluster_based_discovery` |
| 3565 | `run_lasso_regional_contribution` |
| 3776 | `compare_lasso_brute_force` |

### 3.3 analysis_functions.R (11 functions)

| Line | Function Name |
|------|---------------|
| 147 | `compute_correlations` |
| 871 | `assign_nodes_to_brain_areas` |
| 910 | `compute_brain_area_metrics` |
| 985 | `create_full_weighted_network` |
| 1220 | `compute_node_strength` |
| 1576 | `identify_conserved_edges` |
| 1724 | `compute_weighted_network_similarity` |
| 1782 | `compute_weighted_hub_conservation` |
| 1852 | `analyze_weighted_edge_statistics` |
| 2269 | `compute_persistence_auc_metrics` |
| 2354 | `compute_rank_based_consensus` |

### 3.4 consensus_analytics.R (3 functions)

| Line | Function Name |
|------|---------------|
| 94 | `compute_bayesian_consensus` |
| 281 | `compute_uncertainty_metrics` |
| 328 | `calibrate_dynamic_thresholds` |

### 3.5 download_simple.R (2 functions)

| Line | Function Name |
|------|---------------|
| 15 | `create_summary_plot_registry` |
| 151 | `organize_plots_by_category` |

---

## 4. Configuration Issues

### 4.1 Missing selectInput Definitions

These inputs are dynamically updated via `updateSelectInput()` but have no initial UI definition:

| Input Name | Used By |
|------------|---------|
| `consensus_group_heatmap` | `comprehensiveConsensusHeatmapPlot` |
| `consensus_group_venn` | `threeWayHubVennPlot`, `agreementMatrixPlot` |
| `disagreement_group` | Disagreement analysis plots |
| `granular_consensus_group` | Granular consensus plots |

**Fix:** Add placeholder `selectInput()` with `choices = NULL` in UI

---

## Recommended Cleanup Order

1. **Phase 1:** Remove dead code sections (safest)
   - Legacy overview tab
   - Duplicate definitions
   - Commented code

2. **Phase 2:** Remove orphaned outputs
   - Test app after each batch removal

3. **Phase 3:** Remove unused module functions
   - One file at a time
   - Test after each file

4. **Phase 4:** Fix configuration issues
   - Add missing selectInputs

---

## Files to Modify

| File | Changes |
|------|---------|
| `app.R` | Remove 84 outputs + dead sections |
| `modules/visualization_functions.R` | Remove 14 functions |
| `modules/statistical_tests.R` | Remove 13 functions |
| `modules/analysis_functions.R` | Remove 11 functions |
| `modules/consensus_analytics.R` | Remove 3 functions |
| `modules/download_simple.R` | Remove 2 functions |

---

## Notes

- Line numbers are approximate and may shift as edits are made
- Some unused functions may be intended for future features - verify before deletion
- Always test the application after each cleanup phase
- Consider creating a git branch for cleanup work
