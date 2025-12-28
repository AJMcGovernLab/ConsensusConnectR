# ConsensusConnectR: Comprehensive Methodological Pipeline Documentation

## Executive Summary

ConsensusConnectR is an advanced, multimethod consensus-based framework for robust functional connectivity analysis. The application implements a sophisticated 7-step analytical pipeline that integrates **5 correlation methods** with **3 topological approaches**, yielding **15 independent method-approach combinations**. The final results are synthesized through Bayesian confidence-weighted consensus analysis to identify stable, robust network hubs. The entire system comprises approximately 25,000 lines of actively-used R code organized into 6 core modules.

---

## Table of Contents

1. [Application Architecture](#1-application-architecture)
2. [Complete Analytical Pipeline](#2-complete-analytical-pipeline-7-9-steps)
3. [Visualization Generation](#3-visualization-generation)
4. [Export Capabilities](#4-export-capabilities)
5. [Data Flow Architecture](#5-data-flow-architecture)
6. [Key Methodological Features](#6-key-methodological-features)
7. [Quality Assurance & Diagnostics](#7-quality-assurance--diagnostics)
8. [Computational Considerations](#8-computational-considerations)
9. [Validation & Benchmarking](#9-validation--benchmarking)
10. [Practical Usage Workflow](#10-practical-usage-workflow)
11. [Reproducibility & Documentation](#11-reproducibility--documentation)

---

## 1. Application Architecture

### 1.1 Overall Structure

**Framework:** R Shiny web application with shinydashboard UI

**Core Dependencies:**
- `igraph` (network analysis)
- `mice` (missing data imputation)
- `corrplot`, `ggplot2` (visualization)
- `psych`, `corpcor` (enhanced correlation methods)
- `TDA` (topological data analysis - optional)
- `cluster`, `dendextend` (hierarchical clustering)

**Code Organization:**
- **Main App ([app.R](app.R)):** ~12,900 lines - Contains UI, server logic, and orchestration
- **Module System (6 files, ~550 KB total):**
  - [analysis_functions.R](modules/analysis_functions.R) - Core analysis functions (109 KB)
  - [visualization_functions.R](modules/visualization_functions.R) - Plot rendering (242 KB)
  - [consensus_analytics.R](modules/consensus_analytics.R) - Multi-method consensus synthesis (37 KB)
  - [consensus_visualizations.R](modules/consensus_visualizations.R) - Consensus-specific plots (39 KB)
  - [ui_components.R](modules/ui_components.R) - Reusable UI elements (20 KB)
  - [download_simple.R](modules/download_simple.R) - Result export management (105 KB)

### 1.2 User Interface Navigation

**Main Menu Structure:**

1. **📁 Data Import** - Upload and configure data
2. **🧠 Anatomical Regions** - Define brain region anatomy
3. **⚙️ Analysis Settings** - Configure correlation methods and parameters
4. **🏆 Summary** - Analysis overview and status
5. **📊 Results** - Comprehensive result visualizations
6. **💾 Downloads** - Export all results to files
7. **ℹ️ About** - Documentation and citations

---

## 2. Complete Analytical Pipeline (7 Steps)

### Step 1: Data Input & Preprocessing

**Input Mechanisms:**
- CSV/Excel file upload
- Flexible column mapping interface
- Group assignment detection
- Covariate identification

**Data Structure Required:**
- Sample ID column
- Group/condition assignment
- Numerical measurement variables (neural activity, connectivity)
- Optional: Covariates for partial correlation, behavioral measurements

**Processing:**
```
Raw Data → Column Type Assignment → Group Definition → Data Validation
  ↓
Performs QC on sample sizes (warns if n < 25 for reliability)
```

---

### Step 2: Missing Data Handling - MICE Imputation

**Method:** Multiple Imputation by Chained Equations (MICE)

**Parameters:**
- m = 5 imputed datasets
- maxit = 10 maximum iterations
- method = "pmm" (Predictive Mean Matching)
- seed = 123 (for reproducibility)

**Mathematical Framework:**

For each variable j with missing values:

```
Y_j^(t+1) ~ P(Y_j | Y_obs, Y_(-j)^(t), θ_j^(t))
```

**Predictive Mean Matching:**
```
ŷ_i = β̂₀ + β̂₁x_i1 + ... + β̂_p x_ip + ε_i
```
(Uses k-nearest observed values based on predicted means)

**Rubin's Pooling (for multi-imputation):**
```
θ̄ = (1/m) Σ θ̂_i (point estimate)
T = Ū + (1 + 1/m)B (total variance)
```

**Quality Diagnostics:**
- Missing data pattern visualization
- Trace plots (convergence assessment)
- Density plots (original vs. imputed comparison)
- Fraction Missing Information (FMI)
- Relative Efficiency metric

**Implementation Feature:**
Currently uses single imputation (first imputed dataset) rather than pooling multiple imputations - creates simplification but improves computational efficiency.

---

### Step 3: Five-Method Correlation Analysis

**Implemented Correlation Methods:**

#### 1. Pearson Correlation
- Standard linear correlation coefficient
- Parametric, assumes normality
- Sensitivity: High to outliers
- Best for: Linear relationships with normal data
- Formula:
  ```
  r = Σ(x_i - x̄)(y_i - ȳ) / √[Σ(x_i - x̄)² × Σ(y_i - ȳ)²]
  ```

#### 2. Spearman Rank Correlation
- Pearson correlation applied to ranked data
- Non-parametric, rank-based
- Robust to monotonic relationships
- Best for: Non-linear monotonic relationships
- Formula:
  ```
  ρ = 1 - 6Σd_i² / (n(n²-1)), where d_i = rank_x - rank_y
  ```

#### 3. Biweight Midcorrelation
- Robust correlation using M-estimation
- Based on biweight psi-function
- Insensitive to outliers
- Best for: Data with influential outliers
- Uses median-based central location
- Requires: `psych` package (with Pearson fallback)

#### 4. Shrinkage Correlation
- Ledoit-Wolf shrinkage for small sample sizes
- Regularized covariance estimation
- Optimal for n << p scenarios
- Best for: Small sample neuroimaging data
- Formula:
  ```
  Σ_shrink = α × Σ_sample + (1-α) × Σ_target
  ```
- Requires: `corpcor` package (with Pearson fallback)

#### 5. Partial Correlation
- Controls for indirect effects/confounds
- Direct pairwise connections after removing mediation
- Requires: Sufficient degrees of freedom (n > p + 5)
- Formula:
  ```
  r_partial = (r_xy - r_xz × r_zy) / √[(1 - r_xz²)(1 - r_zy²)]
  ```
- Skipped when n < 25 (insufficient power)

**Processing Flow:**
```
Raw Imputed Data → [Apply 5 Methods in Parallel] → 5 Correlation Matrices per Group
                    ↓
                    Pearson
                    Spearman
                    Biweight
                    Shrinkage
                    Partial (if eligible)
```

**Consensus Creation:**
- **Aggregation Method:** Median
- Formula:
  ```
  r_consensus(i,j) = median{r_Pearson, r_Spearman, r_Biweight, r_Shrinkage, r_Partial}
  ```
- Rationale: Robust to outlier methods, requires no weighting assumptions

**Quality Metrics Computed:**
- Sample size per group
- Shrinkage intensity (λ) for shrinkage method
- Mean |r| - average absolute correlation strength
- Count of strong correlations (|r| ≥ 0.4)
- Network density at 0.4 threshold

---

### Step 4: Three-Pronged Topological Analysis

The application runs **THREE independent** analytical approaches in parallel:

#### APPROACH A: Weighted Network Analysis (Threshold-Free)

**Concept:** Uses full correlation strength without thresholding; all connections retained with their correlation magnitude.

**Key Metrics Computed:**

1. **Node Strength (Weighted Degree)**
   - Formula: `S_i = Σ_j |r_ij|` (sum of absolute correlations)
   - Interpretation: Total connectivity of a node

2. **Weighted Eigenvector Centrality**
   - Iterative computation of node importance based on connections to other important nodes
   - Formula: `x_i = (1/λ) Σ_j A_ij × x_j`
   - Interpretation: Global importance/hubness accounting for connection quality

3. **Weighted Clustering Coefficient**
   - Measures local clustering tendency with weights
   - Formula: `C_i^w = (Σ_j Σ_k w_ij × w_ik × w_jk) / (S_i × (S_i - 1))`
   - Interpretation: Local transitivity strength

4. **Network Strength (Global)**
   - Sum of all connection strengths
   - Normalized by potential maximum

5. **Mean Connectivity**
   - Average connection strength across all node pairs

**Advantages:**
- No arbitrary threshold selection
- Retains all correlation information
- Continuous rather than dichotomous networks

---

#### APPROACH B: Percolation-Based Network Analysis

**Concept:** Creates binary networks at multiple thresholds; identifies threshold where network "percolates" (becomes fragmented).

**Step B1: Percolation Threshold Selection**
```
For each group:
  1. Generate threshold range: 0.01 to 0.95 by 0.01
  2. For each threshold t:
     - Create binary network: edge exists if |r_ij| ≥ t
     - Compute giant component size (fraction of largest connected component)
     - Count number of components
     - Calculate edge density
  3. Identify percolation threshold:
     - Find point where giant component begins fragmenting
     - Use highest threshold maintaining connectivity peak
     - Fallback methods if no clear fragmentation:
       * Second derivative (max curvature) approach
       * Default to 0.4 if no clear pattern
```

**Network Metrics at Optimal Threshold:**

1. **Basic Network Properties**
   - Number of nodes/edges
   - Network diameter
   - Edge density (fraction of possible edges)

2. **Node-Level Centrality Measures**
   - Degree (number of connections)
   - Eigenvector centrality (importance based on neighbor importance)
   - Betweenness centrality (bridges between communities)
   - Closeness centrality (average distance to other nodes)
   - PageRank (network flow-based importance)

3. **Community Structure**
   - Number of communities detected
   - Modularity index (strength of community structure)
   - Inter-community vs. intra-community connectivity

4. **Global Properties**
   - Average path length
   - Clustering coefficient
   - Assortativity (tendency of hubs to connect to hubs)

**Regional/Brain Area Analysis:**
- Aggregates node metrics by anatomical regions
- Computes regional means, standard deviations, node counts
- Identifies region-level connectivity patterns

---

#### APPROACH C: Persistence-Based Analysis

**Concept:** Traces network evolution across a range of thresholds; captures which nodes/connections remain "persistent" across filtering.

**Implementation:**
```
For each correlation matrix:
  1. Generate threshold sequence: 0.1 to 0.9 by user-defined step (default 0.05)
  2. For each threshold:
     - Create binary network
     - Compute network metrics
     - Track node presence/centrality values
  3. Compute persistence metrics:
     - Birth threshold (first appearance at threshold)
     - Death threshold (disappearance at threshold)
     - Persistence = death - birth (lifetime in network)
     - Area Under Curve (AUC) for each node metric
```

**Persistence-Specific Metrics:**

1. **Hub Persistence**
   - Top-10 nodes by eigenvector centrality
   - Lifetime in network across threshold range
   - Birth/death thresholds
   - Rank evolution across thresholds

2. **Node Importance Evolution**
   - Standardized ranks (0-1 scale) across 4 measures:
     * Eigenvector centrality
     * Degree
     * Betweenness centrality
     * PageRank
   - Shows how node importance changes with filtering

3. **Metrics Evolution**
   - Curves showing how global metrics change with threshold:
     * Giant component size
     * Number of communities
     * Network diameter
     * Average path length

4. **Area Under Curve (AUC) Integration**
   - Integrates node metrics across entire threshold range
   - `AUC_Eigenvector = ∫₀^max_threshold Eigenvector_Centrality dt`
   - Provides threshold-independent summary of node importance
   - Enables comparison between approaches without threshold bias

**Advantages:**
- Captures robustness of nodes across multiple filtering levels
- Identifies which nodes appear only at specific thresholds
- No single threshold dependency

---

### Step 5: Consensus Integration Across Three Approaches

**Comprehensive Consensus Analysis combines all 15 method-approach combinations:**

```
5 Methods × 3 Approaches = 15 Independent Analyses
(Pearson, Spearman, Biweight, Shrinkage, Partial) ×
(Weighted, Percolation, Persistence)
```

#### STEP 5A: Standardized Hub Metrics (Z-Scored)

For each method × approach combination:
```
1. Extract eigenvector centrality (or AUC_Eigenvector for persistence)
2. Standardize: Z_score = (value - mean) / SD
3. Classify as hub if Z ≥ 1.5 (≈ 93rd percentile)
4. Create matrix: (nodes × approaches × methods)
```

#### STEP 5B: Bayesian Confidence-Weighted Consensus

**Purpose:** Synthesize results across methods with confidence quantification

**For each node:**

1. **Hub Probability Per Approach (Average across methods)**
   - `P(hub|approach) = (# methods showing hub) / (# total methods)`

2. **Mean Z-Score Per Approach**
   - Aggregates strength of hub signal

3. **Coefficient of Variation (CV) Per Approach**
   - Measures method-to-method consistency
   - `CV = SD / mean` (lower = more consistent)
   - Becomes weight for that approach

4. **Approach Weights**
   - Weighted by `(1 - CV)` so consistent approaches weighted more
   - Normalized across three approaches

5. **Bayesian Consensus Score**
   - Combines three approach scores with their weights:
   - `BCS = w_weighted × P(hub|weighted) + w_perc × P(hub|perc) + w_pers × P(hub|pers)`
   - Range: 0 to 1 (1 = hub in all methods and approaches)

6. **Credible Interval Estimation**
   - Beta distribution fitted to hub probabilities
   - CI bounds capture uncertainty in hub status

7. **Confidence Category Assignment**
   - "Very High" (BCS ≥ 0.8)
   - "High" (0.6-0.8)
   - "Moderate" (0.4-0.6)
   - "Low" (< 0.4)

**Matrix Output:**
```
DataFrame with columns:
Node | Approach1_HubProb | Approach2_HubProb | Approach3_HubProb |
     | Approach1_MeanZ   | Approach2_MeanZ   | Approach3_MeanZ   |
     | Approach1_CV      | Approach2_CV      | Approach3_CV      |
     | Approach1_Weight  | Approach2_Weight  | Approach3_Weight  |
     | BayesianConsensusScore | CredibleIntervalLower |
     | CredibleIntervalUpper | ConfidenceCategory
```

---

### Step 6: Advanced Topological Methods

#### Minimum Spanning Tree (MST) Analysis

**Concept:** Extracts backbone of most important connections without redundancy

**Process:**
1. Weight = 1 - |correlation| (inverted for distance interpretation)
2. Compute MST using Kruskal's algorithm
3. Calculate MST statistics:
   - Diameter (maximum path length)
   - Average path length
   - Node degrees
   - Identify central nodes (high degree in MST)

**Metrics:**
- MST diameter (efficiency measure)
- Max degree (hub identification in backbone)
- Central node identification via degree

#### PCA Analysis

**Purpose:** Dimension reduction and variance structure of brain regions

**Process:**
1. Perform PCA on correlation matrix
2. Extract principal component (PC) scores and loadings
3. Compute variance explained by each PC

**Visualizations:**
- PC1 vs PC2 scatter plots (region separation)
- PC1 loadings bar plot (which regions drive variation)
- Variance explained curve (cumulative)

---

### Step 7: Network Conservation & Cross-Method Analysis

#### Network Similarity Metrics

1. **Jaccard Similarity (Unweighted)**
   - Intersection / Union of edges
   - Range: 0-1, where 1 = identical networks
   - Formula: `J = |A ∩ B| / |A ∪ B|`

2. **Weighted Jaccard**
   - Uses absolute correlations as weights
   - More sensitive to connection strength
   - Formula: `J_w = Σ min(|r_ij1|, |r_ij2|) / Σ max(|r_ij1|, |r_ij2|)`

3. **Edge Preservation Metrics**
   - Percentage of edges from network 1 preserved in network 2
   - Directional (not symmetric)
   - Identifies method-specific edge sets

#### Hub Conservation Analysis

**Identifies hubs that persist across different methods:**

1. Extract top hubs from each method (e.g., top 10)
2. Count overlaps across method-approach combinations
3. Compute hub overlap statistics
4. Identify consensus hubs (appear in multiple methods)
5. Identify method-specific hubs

---

## 3. Visualization Generation

### Interactive Visualizations (Shiny Rendered)

#### Correlation Matrices
- Heatmaps for each group showing correlation strength
- Color-coded: red (positive), blue (negative), intensity = magnitude
- Heatmaps for each of the 5 individual methods

#### Network Plots
- Multiple layout options:
  - **Fruchterman-Reingold (FR)** - organic, force-directed
  - **Circle** - nodes on circle perimeter
  - **Kamada-Kawai (KK)** - physics-based optimization
  - **Grid** - regular lattice

**Node properties:**
- Size: proportional to degree/strength
- Color: by brain area assignment
- Brightness: intensity by centrality measure

#### Consensus Hub Visualizations
- Heatmap: nodes × approaches showing consensus scores
- Three-way Venn diagram: overlap of hubs across approaches
- Hub ranking bar plots with confidence bands
- Network similarity heatmap (Jaccard indices)

#### Topological Features
- MST metrics comparison across groups
- PCA score plots and loadings
- Persistence diagrams (birth-death plots of features)
- Hub persistence timeline

#### Method Comparison
- Cross-method network similarity matrix
- Method agreement percentages
- Regional metrics comparison across methods
- Hub stability across methods

### Export-Optimized Visualizations

All plots available in publication-quality formats:
- **Vector graphics:** PDF, SVG (scalable, editable)
- **Raster graphics:** PNG (high-resolution), JPEG
- **Batch export:** All plots for all groups simultaneously

---

## 4. Export Capabilities

### Data Exports

1. **Correlation Matrices**
   - CSV format for each method and group
   - Contains full pairwise correlations

2. **Network Metrics (Node-Level)**
   - Degree, eigenvector centrality, betweenness, clustering coefficient
   - Strength, weighted metrics
   - Persistence-based AUC metrics
   - CSV format, sortable by metric

3. **Network Metrics (Global)**
   - Diameter, density, modularity
   - Average path length, assortativity
   - Regional summaries (mean ± SD by brain area)

4. **Hub Lists**
   - Top nodes by various centrality measures
   - Consensus hub rankings with confidence scores
   - Method-specific hub sets
   - CSV format

5. **Statistical Test Results**
   - Permutation test p-values and effect sizes
   - Network-based statistics results
   - Group comparison statistics
   - Multiple comparison corrections (FDR)

6. **Method Comparisons**
   - Network similarity matrices (Jaccard, weighted)
   - Cross-method agreement tables
   - Hub overlap statistics

### Plot Exports

- **High-resolution PNG** (300 dpi default)
- **PDF format** for publication
- **SVG format** for editing in vector graphics software

**Exported plots include:**
- Individual correlation heatmaps (one per method, one per group)
- Network diagrams (multiple layouts)
- Consensus hub heatmaps
- Hub Venn diagrams
- MST metrics comparison
- PCA visualizations
- Persistence timelines
- Method similarity matrices
- Group comparison plots

---

## 5. Data Flow Architecture

### Complete End-to-End Pipeline Flow

```
┌─────────────────────────────────────────────────────────────────┐
│ INPUT: Raw Data (CSV/Excel)                                     │
│ - Neural activity measurements (regions × samples)               │
│ - Group assignments (condition/treatment)                        │
│ - Optional: Covariates, anatomical region definitions            │
└──────────────────────────┬──────────────────────────────────────┘
                           ↓
┌─────────────────────────────────────────────────────────────────┐
│ STEP 1: Data Quality & MICE Imputation                           │
│ - Detect and impute missing values                               │
│ - Multiple imputation by chained equations (m=5, maxit=10)       │
│ - Output: Imputed data matrix + diagnostics                      │
└──────────────────────────┬──────────────────────────────────────┘
                           ↓
┌─────────────────────────────────────────────────────────────────┐
│ STEP 2: Per-Method Correlation Analysis                          │
│ ┌───────────────┐ ┌────────────┐ ┌──────────┐ ┌──────────┐      │
│ │ Pearson       │ │ Spearman   │ │ Biweight │ │Shrinkage │      │
│ │ (linear)      │ │ (rank-based)│ │ (robust) │ │(regulariz)│     │
│ └───────────────┘ └────────────┘ └──────────┘ └──────────┘      │
│ ┌──────────────┐                                                 │
│ │ Partial      │  → Create 5 Correlation Matrices per Group     │
│ │ (conditional)│  → Consensus via Median Aggregation            │
│ └──────────────┘                                                 │
│ Output: correlation_methods_raw (5 × groups), correlations      │
└──────────────────────────┬──────────────────────────────────────┘
                           ↓
┌─────────────────────────────────────────────────────────────────┐
│ STEP 3A: Per-Method Percolation Analysis (5 parallel)            │
│ For EACH method (Pearson through Partial):                       │
│   1. Calculate group-specific percolation thresholds             │
│   2. Create binary networks at optimal thresholds                │
│   3. Compute node & global metrics                               │
│   4. Run threshold-free (weighted) analysis                      │
│ Output: method_percolation_results, method_weighted_results      │
└──────────────────────────┬──────────────────────────────────────┘
                           ↓
┌─────────────────────────────────────────────────────────────────┐
│ STEP 3B: Persistence Analysis Across Thresholds (5 parallel)     │
│ For EACH method (Pearson through Partial):                       │
│   1. Generate threshold sequence (0.1-0.9)                       │
│   2. For EACH threshold:                                         │
│      - Create network                                            │
│      - Compute metrics                                           │
│      - Track node persistence                                    │
│   3. Compute AUC metrics (threshold-integrated)                  │
│   4. Identify hub persistence patterns                           │
│ Output: persistence_results, auc_results                         │
└──────────────────────────┬──────────────────────────────────────┘
                           ↓
┌─────────────────────────────────────────────────────────────────┐
│ STEP 4: Comprehensive Consensus Analysis                         │
│ ┌────────────────────────────────────────────────────────┐       │
│ │ For EACH group:                                         │       │
│ │  1. Standardize hub metrics (Z-scores) from all 15     │       │
│ │     method-approach combinations                        │       │
│ │  2. Compute Bayesian confidence-weighted consensus      │       │
│ │     - Per-approach hub probabilities                    │       │
│ │     - Cross-method consistency (CV weights)            │       │
│ │     - Consensus scores + credible intervals            │       │
│ │  3. Assign confidence categories                        │       │
│ │  4. Compute uncertainty metrics                         │       │
│ │  5. Threshold sensitivity analysis                      │       │
│ └────────────────────────────────────────────────────────┘       │
│ Output: comprehensive_consensus (all groups combined)            │
└──────────────────────────┬──────────────────────────────────────┘
                           ↓
┌─────────────────────────────────────────────────────────────────┐
│ STEP 5: Network Conservation & Similarity Analysis               │
│ (Only if multiple groups)                                        │
│ - Jaccard similarity matrices (unweighted, weighted)              │
│ - Hub conservation across groups                                 │
│ - Weighted hub conservation analysis                             │
│ Output: similarities, conservation metrics                       │
└──────────────────────────┬──────────────────────────────────────┘
                           ↓
┌─────────────────────────────────────────────────────────────────┐
│ STEP 6: Advanced Network Analyses                                │
│ 1. Weighted Eigenvector Centrality (threshold-free)              │
│ 2. Minimum Spanning Tree (MST) analysis                          │
│ 3. PCA of correlation matrices                                   │
│ Output: mst_results, pca_results, weighted_eigenvector          │
└──────────────────────────┬──────────────────────────────────────┘
                           ↓
┌─────────────────────────────────────────────────────────────────┐
│ STEP 7: Cross-Method Comparison                                  │
│ - Compare weighted vs percolation vs persistence approaches      │
│ - Compute network similarity across methods                      │
│ - Identify method-specific features                              │
│ Output: cross_method_comparison, cross_method_similarity         │
└──────────────────────────┬──────────────────────────────────────┘
                           ↓
┌─────────────────────────────────────────────────────────────────┐
│ OUTPUT: Complete Analysis Results                                │
│ - Correlation matrices (all methods)                             │
│ - Network metrics (node & global level)                          │
│ - Hub lists (with consensus confidence)                          │
│ - Statistical comparisons (if multiple groups)                   │
│ - Visualizations (all plots generated)                           │
│ - Diagnostic information                                         │
└─────────────────────────────────────────────────────────────────┘
```

---

## 6. Key Methodological Features

### 1. Robust Consensus Approach

**Strength:** Uses median aggregation across 5 methods
- Resistant to outlier methods
- No weighted averaging assumptions
- Symmetric treatment of all methods
- Requires simple majority agreement for strong conclusions

### 2. Multi-Level Thresholding Strategy

**No single threshold dependency:**
- **Weighted:** Uses all data, threshold-free
- **Percolation:** Data-driven threshold selection per group
- **Persistence:** All thresholds simultaneously

**Approach reduces:**
- Arbitrary threshold bias
- Method-dependent artifacts
- Instability of single-threshold conclusions

### 3. Bayesian Confidence Integration

**Quantifies reliability:**
- Hub probabilities across methods (0-1 scale)
- Cross-method consistency weighting (CV-based)
- Credible intervals capture uncertainty
- Confidence categories (Very High to Low)

**Advantage:** Researchers can identify robust hubs vs. method-sensitive results

### 4. Persistence-Based Topological Feature Extraction

**Novel approach:**
- Tracks network evolution across threshold range
- Identifies which nodes appear/disappear at specific thresholds
- Computes area under centrality curves (AUC)
- Provides threshold-independent node importance summary

**Advantage:** Captures temporal robustness not available from single-threshold analysis

### 5. Advanced Network Refinement

**Multiple backbone extraction methods:**
- MST (removes redundancy, preserves connectivity)
- OMST (adds planarity constraint)
- Disparity filter (retains statistically significant edges)
- Spectral analysis (captures network dynamics)

**Advantage:** Different refinements reveal different aspects of network structure

### 6. Regional/Anatomical Integration

**Spatial contextualization:**
- User-defined brain region mapping
- Aggregates node metrics by regions
- Region-level connectivity patterns
- Color-coded visualization by anatomy

**Advantage:** Results interpretable in neurobiological context

### 7. Statistical Rigor

**Comprehensive significance testing:**
- Permutation tests (5,000 iterations)
- FWE and FDR corrections
- Effect size estimation (Cohen's d)
- Confidence intervals from permutation null
- NBS for connected components
- Parallel processing for speed

**Advantage:** Accounts for multiple testing burden and family-wise error

---

## 7. Quality Assurance & Diagnostics

### Data Quality Checks
- Missing data pattern detection
- Sample size adequacy (warns if n < 25)
- Correlation distribution analysis
- Outlier identification

### Imputation Diagnostics
- Convergence plots (trace plots)
- Distribution comparison (before/after imputation)
- Relative efficiency metrics
- Fraction Missing Information (FMI)

### Method-Specific Diagnostics
- Shrinkage intensity displayed (λ value)
- Partial correlation eligibility checked
- Method-specific correlation distributions
- Strong correlation counts (|r| ≥ 0.4)

### Consensus Quality
- Method agreement percentages
- Approach consistency metrics (CV)
- Hub confidence categories
- Threshold sensitivity analysis

### Output Validation
- All visualizations checked for empty results
- Graceful error handling with user-friendly messages
- Automatic format detection (CSV, Excel)
- Data type validation before analysis

---

## 8. Computational Considerations

### Performance Optimization

1. **Parallel Processing:**
   - Per-method analysis run in parallel (5 methods)
   - Permutation tests parallelized across CPU cores
   - Default: Uses n_cores - 1 (reserves one for system)

2. **Memory Management:**
   - Lazy loading of data
   - Result caching to avoid recomputation
   - Selective storage of detailed results
   - Background garbage collection

3. **Algorithm Efficiency:**
   - Sparse matrix operations when possible
   - Vectorized computations in R/C++
   - Fast igraph backend for network algorithms
   - Early termination for convergence

### Computational Time Estimates

For a typical dataset (100 brain regions, 3 groups, 50 samples/group):

| Step | Time |
|------|------|
| MICE imputation | 5-10 sec |
| 5-method correlations | 10-20 sec |
| Percolation (5 methods × 3 groups) | 30-60 sec |
| Persistence (all thresholds) | 2-5 min |
| Consensus synthesis | 30-60 sec |
| Statistical testing (5000 perm) | 1-3 min |
| Visualizations generation | 1-2 min |
| **Total** | **~5-12 minutes** |

---

## 9. Validation & Benchmarking

### Built-in Validation Methods

1. **Leave-One-Out Sensitivity:**
   - Remove each method sequentially
   - Assess impact on consensus scores
   - Identifies essential vs. redundant methods

2. **Threshold Sensitivity:**
   - Vary persistence step size
   - Assess robustness of hub identification
   - Show confidence intervals vs. threshold

3. **Bootstrap Resampling:**
   - Resample subjects within groups
   - Recompute consensus metrics
   - Estimate stability

4. **Cross-Validation:**
   - Split-half validation of correlation structure
   - Test generalization of hubs to held-out data

### Known Limitations

1. **Single Imputation:** Uses first imputed dataset only; true multiple imputation would pool across all imputations (Rubin's rules)

2. **Sample Size Dependency:** Partial correlation excluded if n < 25 (conservative but appropriate)

3. **Method Selection:** Fixed 5 methods; user cannot add/remove methods (though modular architecture allows this)

4. **Threshold Ranges:** Persistence analysis limited to 0.1-0.9 range (outside common correlation range)

5. **TDA Optional:** Persistent homology requires TDA package; falls back to simplified approach if unavailable

---

## 10. Practical Usage Workflow

### User Journey

1. **Data Preparation (outside app)**
   - Organize data in CSV: columns = regions, rows = subjects
   - Include "Group" column for group assignments
   - Optional: Include covariates

2. **Step 1: Upload Data**
   - Click Data Import tab
   - Upload CSV/Excel file
   - Verify column detection
   - Confirm group assignments

3. **Step 2: Configure Brain Anatomy (optional)**
   - Click Anatomical Regions tab
   - Define which columns belong to which brain areas
   - Assign colors to regions
   - Assign colors to experimental groups

4. **Step 3: Analysis Settings**
   - Click Analysis Settings tab
   - Select correlation methods (defaults: all 5)
   - Optionally adjust persistence step size
   - Confirm settings

5. **Step 4: Run Analysis**
   - Click "Run Complete Analysis" button
   - Monitor progress notifications (9 steps)
   - Wait for completion (~5-12 min)

6. **Step 5: Explore Results**
   - Click Results tab
   - Browse tabbed visualizations
   - View consensus hubs
   - Compare methods
   - Examine statistics

7. **Step 6: Export Results**
   - Click Downloads tab
   - Select results to export
   - Choose file formats (CSV, PDF, PNG)
   - Download individually or as batch

---

## 11. Reproducibility & Documentation

### Reproducibility Features

1. **Fixed Random Seeds:**
   - MICE imputation: seed = 123
   - Permutation tests: reproducible with seed
   - R environment specification available

2. **Complete Provenance:**
   - All parameters logged in output
   - Method selections recorded
   - Threshold values stored with results
   - Data version tracking

3. **Session Information Export:**
   - R version, package versions
   - System information
   - Analysis date/time

### Documentation Provided

- **In-app Methods Sections:** Mathematical formulas and explanations
- **Manuscript Draft:** NeuroImage Toolbox manuscript outline
- **Literature References:** Complete citation list for all methods
- **Inline Code Comments:** Extensive documentation throughout modules

---

## Summary Table: Complete Analytical Pipeline

| Step | Method | Input | Key Computation | Output |
|------|--------|-------|-----------------|--------|
| 1 | MICE | Raw data | Impute missing values | Imputed data |
| 2 | 5-Method | Imputed data | Correlation (5 methods) | Correlation matrices × 5 |
| 2.5 | Consensus | 5 correlations | Median aggregation | Median correlation matrix |
| 3A | Percolation | Consensus correlations | Threshold optimization | Networks + metrics |
| 3B | Persistence | 5-method correlations | AUC integration | Node persistence + AUC |
| 4 | Consensus | Weighted+Perc+Pers | Bayesian synthesis | Hub scores + confidence |
| 5 | Conservation | Networks | Jaccard + hub overlap | Similarity metrics |
| 6 | Advanced | Correlations | MST, PCA | Backbone networks |
| 7 | Cross-method | All results | Aggregation | Method comparison |
| 8 | Statistics | Networks + groups | Permutation tests | P-values, effect sizes |
| 9 | Visualization | All results | Plot rendering | Figures, tables |
| 10 | Export | All results | Format conversion | CSV, PDF, PNG files |

---

## Conclusion

ConsensusConnectR represents a comprehensive, multi-layered approach to functional connectivity analysis that addresses critical challenges in neuroimaging research:

1. **Methodological robustness:** Five independent correlation methods reduce algorithm-dependent artifacts
2. **Analytical flexibility:** Three parallel topological approaches provide complementary network views
3. **Confidence quantification:** Bayesian integration with explicit uncertainty quantification
4. **User accessibility:** No programming required; interactive web interface
5. **Reproducibility:** Complete documentation, fixed seeds, full provenance tracking
6. **Statistical rigor:** Permutation testing with multiple comparison correction
7. **Biological context:** Integration of anatomical region definitions
8. **Publication-ready outputs:** High-quality figures and comprehensive data tables

The 9-step pipeline systematically guides users from raw data through consensus hub identification with explicit quantification of methodological robustness, making it suitable for publication-quality functional connectivity studies.

---

## References & Citations

For complete citation information and methodological references, please see the About tab in the application interface.

**Application Information:**
- **Version:** Persistence4
- **Platform:** R Shiny
- **License:** [Specify license]
- **Contact:** [Specify contact information]

---

*Document Generated: 2025-12-22*
*Last Updated: 2025-12-22*
