# Implementation Plan: UI/UX Improvements and Code Cleanup

## Overview
This plan addresses multiple improvements requested for ConsensusConnectR Persistence5.

---

## Phase 1: Progress Banner & Navigation Fixes

### 1.1 Fix Progress Banner - Tick "Analysis Settings"
**File:** `app.R`
**Location:** Around line 2507 (progress_settings)
**Issue:** Analysis Settings never gets marked as complete
**Solution:** Add observer to mark progress_settings as complete when user configures settings (e.g., selects methods, sets parameters)

### 1.2 Auto-Navigate to Summary Dashboard After Analysis
**File:** `app.R`
**Location:** After line 4505 (end of analysis pipeline)
**Issue:** After "Run Comprehensive Network Analysis" completes, user stays on current tab
**Solution:** Add `updateTabItems(session, "sidebarMenu", "summary")` after analysis completion

---

## Phase 2: Notification Updates

### 2.1 Fix Step Numbering and Remove Deprecated References
**File:** `app.R`
**Lines to update:**
- Line 4052: "Step 1/7: Imputing missing data..." → Keep
- Line 4057: "Step 2/9: Computing per-method correlations..." → "Step 2/6: Computing correlations..."
- Line 4082: "Step 3a/9: Topology Analysis..." → "Step 3/6: Network topology analysis..."
- Line 4213: "Step 3b/9: Persistence analysis..." → "Step 4/6: Persistence analysis..."
- Line 4330: "Step 4/9: Comprehensive consensus..." → "Step 5/6: Consensus analysis..."
- Line 4415: "Step 5/9: Network conservation..." → Remove or consolidate
- Line 4464: "Step 5/7: Weighted Network Analysis..." → Remove (duplicate)
- Line 4483: "Step 5/7: Computing MST metrics..." → Remove (deprecated)
- Line 4488: "Step 5/7: Computing PCA analysis..." → Remove (deprecated)
- Line 4493: "Step 6/7: Cross Method Comparison..." → Remove (deprecated)
- Line 4505: "Step 7/7: Conservation Analysis..." → "Step 6/6: Finalizing..."

**New numbering scheme:**
1. Step 1/6: Imputing missing data
2. Step 2/6: Computing correlations
3. Step 3/6: Network topology analysis
4. Step 4/6: Persistence analysis
5. Step 5/6: Consensus analysis
6. Step 6/6: Finalizing results

---

## Phase 3: Progress Updates Granularity

### 3.1 Regional Contribution Analysis - More Frequent Updates
**File:** `app.R`
**Location:** Lines 8768-8812 (regional contribution analysis)
**Issue:** Progress updates are too chunky
**Solution:** Modify progress_callback to update more frequently (every 5% instead of larger chunks)

**Also in:** `modules/statistical_tests.R`
- `compute_multimethod_contribution_permutation_test()` - update progress per region
- `brute_force_discovery()` - update progress per candidate

---

## Phase 4: Deprecated igraph Functions

### 4.1 Replace erdos.renyi.game() with sample_gnm()
**File:** `modules/analysis_functions.R`
**Line 785:**
```r
# OLD:
random_g <- erdos.renyi.game(n_nodes, n_edges, type = "gnm")
# NEW:
random_g <- sample_gnm(n_nodes, n_edges, directed = FALSE)
```

### 4.2 Replace clusters() with components()
**File:** `modules/analysis_functions.R`
**Line 79:**
```r
# OLD:
components <- clusters(g)
# NEW:
comp <- components(g)
```

### 4.3 Remove scale=TRUE from eigen_centrality()
**File:** `modules/analysis_functions.R`
**Lines 814, 1041, 1166, 2142:**
```r
# OLD:
eigen_centrality(g, weights = E(g)$weight, scale = TRUE)$vector
# NEW:
eigen_centrality(g, weights = E(g)$weight)$vector
```

---

## Phase 5: Remove Debug Output

### 5.1 analysis_functions.R Debug Statements to Remove/Quiet
**File:** `modules/analysis_functions.R`
**Lines to modify (wrap in verbose flag or remove):**
- Lines 59, 110-111, 127-128, 144, 148, 155: Percolation curve messages
- Lines 168-169, 182-184: Correlation computation messages
- Lines 231, 240, 256, 258, 266, 286, 289: Method-specific warnings (keep as warnings, not cat)

### 5.2 statistical_tests.R Debug Statements
**File:** `modules/statistical_tests.R`
**Lines 2854-3382:** All [DEBUG brute_force] messages
**Solution:** Add `verbose = FALSE` parameter to brute_force_discovery() and wrap all message() calls

### 5.3 download_simple.R Debug Statements
**File:** `modules/download_simple.R`
**Keep:** Progress messages for downloads (user feedback)
**Remove:** Verbose debugging output

---

## Phase 6: Correlation Method Recommendations

### 6.1 Add Data-Based Correlation Recommendations
**File:** `app.R`
**New function:** `recommend_correlation_methods(n_subjects, n_variables)`
**Logic:**
- Pearson: Always available (n >= 3)
- Spearman: Always available (n >= 3)
- Biweight: Recommended when n >= 10 (robust to outliers)
- Shrinkage: Recommended when n < p or n is small relative to p
- Partial: Requires n > p + 2 (more subjects than variables + 2)

**UI Addition:** Display recommendations near method selection with explanations

### 6.2 Additional Correlation Methods to Consider
Potential additions (future enhancement):
- Kendall's Tau (ordinal data, small samples)
- Distance correlation (non-linear relationships)
- Mutual Information (non-parametric)

---

## Phase 7: Update Summary Dashboard Descriptions

### 7.1 Tab A - Consensus Node Metrics
**Update "What it measures" and "Method" to reflect current implementation:**
- Clarify rank-based consensus approach
- Specify weighted vs binary edge handling per approach

### 7.2 Tab B - Consensus Networks
**Update descriptions for percolation network visualization**

### 7.3 Tab C - Regional Consensus
**Update regional aggregation description**

### 7.4 Tab D - Group Similarity
**Update Jaccard calculation descriptions per approach:**
- Weighted: Raw absolute correlations
- Percolation: Binary adjacency × correlation weights
- Persistence: Average Jaccard across threshold range

### 7.5 Tab E - Regional Contribution
**Update to reflect:**
- Auto-calculated permutations
- Hypothesis testing option
- Artificial brain area discovery

### 7.6 Methods Tab
**Complete rewrite to match current implementation**

---

## Phase 8: Methods Tab Update

### 8.1 Update Formulas and Descriptions
- Update percolation threshold description
- Update persistence threshold interval (user-configurable)
- Update eigenvector weighting (weighted for percolation, binary for persistence)
- Add permutation testing methodology
- Add Jaccard calculation details per approach

---

## Implementation Order

1. **Phase 4** - Fix deprecated igraph functions (prevents warnings)
2. **Phase 5** - Remove debug output (cleaner console)
3. **Phase 1** - Progress banner fixes
4. **Phase 2** - Notification updates
5. **Phase 3** - Progress granularity
6. **Phase 6** - Correlation recommendations
7. **Phase 7 & 8** - Documentation updates

---

## Estimated Changes

| File | Lines Changed | Type |
|------|--------------|------|
| analysis_functions.R | ~50 | Debug removal, igraph fixes |
| statistical_tests.R | ~60 | Debug removal, verbose flag |
| app.R | ~150 | Notifications, navigation, recommendations, UI text |
| download_simple.R | ~20 | Debug cleanup |

---

## Testing Checklist

- [ ] Run analysis with no console debug output
- [ ] Verify no igraph deprecation warnings
- [ ] Confirm progress banner ticks all stages
- [ ] Verify auto-navigation to Summary after analysis
- [ ] Check notification numbering is sequential
- [ ] Test correlation recommendations with various n/p ratios
- [ ] Verify all Summary tab descriptions are accurate
