# Persistence5 Cleanup Plan

## Overview
This plan removes ~8,300 lines of dead code, unused functions, and debug statements across 8 files.
The cleanup is divided into 5 phases with verification checkpoints.

**Estimated reduction: 32% of codebase**

---

## Phase 1: ui_components.R Cleanup (Lowest Risk)
**Target: Remove 413 lines (65% of file)**

### Changes:
1. Delete lines 224-637 containing 5 unused UI functions:
   - `citation_modal_ui()` (lines 225-254)
   - `data_import_ui()` (lines 257-338)
   - `analysis_preferences_ui()` (lines 341-437)
   - `results_ui()` (lines 440-560)
   - `create_dashboard_ui()` (lines 563-637)

2. Remove commented code block (lines 568-572):
   ```r
   # About button removed - defined in main app.R instead
   # tags$li(...)
   ```

### Verification:
- [ ] These functions have zero calls in app.R (verified by grep)
- [ ] File will go from 636 lines to ~223 lines

---

## CHECKPOINT 1: Run App and Verify
**User Action Required:**
```r
shiny::runApp()
```
- [ ] App launches without errors
- [ ] All tabs load correctly
- [ ] Data import works
- [ ] Basic analysis runs

---

## Phase 2: analysis_functions.R Cleanup
**Target: Remove ~690 lines (22.5% of file)**

### 2A. Remove 10 Unused Functions:

| Function | Lines | Reason |
|----------|-------|--------|
| `compute_method_agreement` | 632-704 | Never called |
| `get_region_colors` | 1071-1081 | Never called |
| `get_experimental_group_colors` | 1083-1106 | Duplicate of version in visualization_functions.R |
| `compute_mean_connectivity` | 1395-1425 | Never called |
| `compute_weighted_clustering_coefficient` | 1428-1502 | Never called |
| `compute_network_strength` | 1505-1532 | Never called |
| `compare_threshold_free_metrics` | 1569-1597 | Never called |
| `identify_conserved_edges` | 1895-1950 | Only called internally by unused function |
| `compute_weighted_network_similarity` | 2043-2098 | Only called internally by unused function |
| `compute_weighted_hub_conservation` | 2101-2168 | Only called internally by unused function |
| `analyze_weighted_edge_statistics` | 2171-2205 | Only called internally by unused function |
| `compute_auc_group_difference` | 2591-2647 | Never called |

### 2B. Remove 8 DEBUG Statements:
Lines containing `[DEBUG]` in `compute_comprehensive_rank_consensus`:
- Line 2952
- Lines 2955-2957
- Line 2964
- Line 2985
- Line 2991
- Line 2997

### 2C. Remove Commented Alternative Code:
- Lines 1303-1305 (commented alternative implementations)

---

## CHECKPOINT 2: Run App and Verify
**User Action Required:**
```r
shiny::runApp()
```
- [ ] App launches without errors
- [ ] Correlation analysis works
- [ ] Percolation analysis works
- [ ] Weighted eigenvector analysis works
- [ ] Conservation analysis works
- [ ] Download functionality works

---

## Phase 3: visualization_functions.R Cleanup
**Target: Remove ~735 lines (12% of file)**

### 3A. Remove Commented-Out Code Blocks:
- Lines 1097-1157 (61 lines) - Duplicate `render_avg_eigenvector_by_region`
- Lines 3373-3411 (39 lines) - Duplicate `render_cross_method_eigenvector_comparison`
- Comment markers at lines 3306-3307, 4141-4144, 4351-4352

### 3B. Remove Unused Network Functions:
| Function | Lines | Reason |
|----------|-------|--------|
| `apply_brain_area_colors` | 14-37 | Only used by unused functions |
| `get_experimental_group_color` | 40-57 | Duplicate; plural version is used |
| `create_network_legend` | 60-87 | Only used by unused functions |
| `render_network_plots` | 159-212 | Never called in app.R |
| `render_network_gallery` | 845-897 | Never called in app.R |

### 3C. Remove Unused Percolation/Threshold Functions:
| Function | Lines | Reason |
|----------|-------|--------|
| `render_global_percolation_analysis` | 1479-1571 | Never called |
| `render_percolation_analysis` | 1574-1631 | Never called |
| `render_threshold_analysis` | 1632-1669 | Never called |
| `render_threshold_comparison` | 2100-2133 | Never called |

### 3D. Remove Unused MST Functions:
| Function | Lines | Reason |
|----------|-------|--------|
| `render_mst_metrics` | 2220-2255 | Never called |
| `render_mst_properties` | 2258-2294 | Never called |
| `render_mst_networks` | 2670-2739 | Never called |

### 3E. Remove Duplicate Method Analysis Functions:
| Function | Lines | Reason |
|----------|-------|--------|
| `render_method_correlation_analysis` (copy 1) | 2439-2463 | Never called |
| `render_method_agreement_analysis` (copy 1) | 2464-2481 | Never called |
| `render_method_correlation_analysis` (copy 2) | 4353-4377 | Never called |
| `render_method_agreement_analysis` (copy 2) | 4379-4396 | Never called |

---

## CHECKPOINT 3: Run App and Verify
**User Action Required:**
```r
shiny::runApp()
```
- [ ] App launches without errors
- [ ] All visualization tabs work
- [ ] Network visualizations render correctly
- [ ] Regional analysis plots work
- [ ] Consensus visualizations work
- [ ] Download with plots works

---

## Phase 4: consensus_analytics.R Cleanup
**Target: Remove ~15 lines + fix unused parameter**

### 4A. Remove Unused Variable:
- Line 728: Remove `all_nodes <- unique(c(unlist(weighted_hubs), ...))` assignment

### 4B. Remove Unused Parameter:
- Line 632: Remove `top_percentile = 0.10` from function signature
- Update call site in app.R (line ~4231) to remove the parameter

### 4C. Clean Up Excessive Whitespace:
- Consolidate multiple consecutive blank lines into single blank lines

---

## Phase 5: complete_plot_system.R & download_simple.R Review
**Target: Remove debug/verbose output**

### 5A. complete_plot_system.R:
- Review lines 52-61 for debug cat() statements
- Remove any development-only logging

### 5B. download_simple.R:
- Review for any DEBUG or verbose output statements
- Ensure production-ready logging only

---

## CHECKPOINT 4: Final Verification
**User Action Required:**
```r
shiny::runApp()
```

### Full Test Checklist:
- [ ] App launches without errors
- [ ] Data import (CSV upload) works
- [ ] All correlation methods run
- [ ] Percolation analysis completes
- [ ] Weighted analysis works
- [ ] Consensus analysis works
- [ ] All plot tabs render correctly:
  - [ ] Correlation matrices
  - [ ] Network graphs
  - [ ] Regional bar plots
  - [ ] Consensus heatmaps
  - [ ] Hub comparisons
- [ ] Download functionality:
  - [ ] ZIP download works
  - [ ] All plots included
  - [ ] Data exports correctly
- [ ] No console errors or warnings (except expected package warnings)

---

## Summary of Changes

| Phase | File | Lines Removed | % Reduction |
|-------|------|---------------|-------------|
| 1 | ui_components.R | ~413 | 65% |
| 2 | analysis_functions.R | ~690 | 22.5% |
| 3 | visualization_functions.R | ~735 | 12% |
| 4 | consensus_analytics.R | ~15 | 1.5% |
| 5 | complete_plot_system.R | ~10 | 1.4% |
| 5 | download_simple.R | ~10 | 0.4% |
| **Total** | **6 files** | **~1,873** | **~12%** |

**Note:** app.R cleanup (embedded HTML, repetitive patterns) is deferred to a future phase as it requires more careful refactoring and has higher risk.

---

## Rollback Plan

If any phase causes issues:
1. The original files exist in Persistence4
2. Git can restore any deleted code
3. Each phase is independent - can skip problematic phases

---

## Files Changed (in order):
1. `modules/ui_components.R`
2. `modules/analysis_functions.R`
3. `modules/visualization_functions.R`
4. `modules/consensus_analytics.R`
5. `modules/complete_plot_system.R`
6. `modules/download_simple.R`
7. `app.R` (minor: remove top_percentile parameter from one call)

---

## Ready to Begin?

Respond with "proceed with phase 1" to start the cleanup.
After each checkpoint, report back with test results before continuing.
