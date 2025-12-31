# Correlation Method Decision Tree

This document describes the automated recommendation system for selecting correlation methods in ConsensusConnectR based on data characteristics.

## Input Parameters

- **n**: Sample size (minimum per group if multiple groups)
- **p**: Number of variables (brain regions)
- **Tie ratio**: Proportion of unique values in the data

---

## Decision Tree

### 1. Pearson Correlation

| Condition | Status |
|-----------|--------|
| Always | **RECOMMENDED** |

**Rationale**: Standard baseline for detecting linear relationships. Always included as a reference point.

**Mathematical basis**: `r = cov(X,Y) / (sd(X) * sd(Y))`

---

### 2. Spearman vs Kendall (Rank-based Methods)

These are mutually recommended based on data characteristics:

```
Is n < 20 OR tie_ratio < 0.8 (many tied values)?
    |
    +-- YES --> KENDALL: Recommended
    |           SPEARMAN: Available (alternative)
    |
    +-- NO  --> SPEARMAN: Recommended
                KENDALL: Available (alternative)
```

| Condition | Kendall Status | Spearman Status |
|-----------|---------------|-----------------|
| n < 20 OR >20% ties | **RECOMMENDED** | Available |
| n >= 20 AND <20% ties | Available | **RECOMMENDED** |

**Rationale**:
- **Kendall**: More robust for small samples, handles tied values better, has better theoretical properties
- **Spearman**: Computationally faster for larger samples, widely used

---

### 3. Biweight Midcorrelation

```
Is n >= 10?
    |
    +-- YES --> RECOMMENDED (Robust to outliers)
    |
    +-- NO  --> CAUTION (Sample may be too small)
```

| Condition | Status | Note |
|-----------|--------|------|
| n >= 10 | **RECOMMENDED** | Robust to outliers |
| n < 10 | CAUTION | Sample size may be too small |

**Rationale**: Biweight midcorrelation downweights outliers, but requires sufficient sample size for reliable estimation.

**Mathematical basis**: Uses iteratively reweighted least squares with bisquare weights.

---

### 4. Shrinkage Correlation (Ledoit-Wolf Estimator)

```
Is n < p?
    |
    +-- YES --> RECOMMENDED (Sample covariance is singular)
    |
    +-- Is n < 2p?
        |
        +-- YES --> AVAILABLE (Helps with estimation stability)
        |
        +-- NO  --> OPTIONAL (Not needed when n >> p)
```

| Condition | Status | Note |
|-----------|--------|------|
| n < p | **RECOMMENDED** | Sample covariance matrix is singular; shrinkage regularizes it |
| p <= n < 2p | AVAILABLE | Improves estimation stability |
| n >= 2p | OPTIONAL | Standard methods are sufficient |

**Rationale**: When sample size is smaller than the number of variables, the sample covariance matrix is singular and cannot be inverted. Shrinkage towards a structured target (identity matrix) regularizes the estimate.

**Mathematical basis**:
```
S_shrink = (1 - lambda) * S_sample + lambda * Target
```
Where lambda is the optimal shrinkage intensity estimated from the data.

**R Package**: `corpcor::cor.shrink()`

---

### 5. Partial Correlation

```
Is n > p + 2?
    |
    +-- YES --> AVAILABLE (Sufficient degrees of freedom)
    |
    +-- NO  --> UNAVAILABLE (Cannot compute)
```

| Condition | Status | Note |
|-----------|--------|------|
| n > p + 2 | AVAILABLE | Sufficient sample size for matrix inversion |
| n <= p + 2 | UNAVAILABLE | Requires n > p + 2 |

**Rationale**: Partial correlation requires inverting the covariance matrix, which needs n > p. The additional buffer (+2) ensures numerical stability.

**Mathematical basis**: Partial correlation between X and Y controlling for Z:
```
r_XY.Z = (r_XY - r_XZ * r_YZ) / sqrt((1 - r_XZ^2)(1 - r_YZ^2))
```

**R Package**: `psych::partial.r()`

---

## Status Legend

| Status | Icon | Meaning |
|--------|------|---------|
| RECOMMENDED | Green check | Optimal choice for your data |
| AVAILABLE | Blue circle | Valid option, can be selected |
| CAUTION | Yellow warning | May have limitations with your data |
| OPTIONAL | Gray circle | Not necessary but won't hurt |
| UNAVAILABLE | Red X | Cannot be computed with your data |

---

## Consensus Calculation

After computing correlations using selected methods, the consensus is calculated as:

```
R_consensus[i,j] = median(r_method1[i,j], r_method2[i,j], ..., r_methodN[i,j])
```

Using the median provides robustness against any single method producing outlier estimates.

---

## References

1. **Pearson**: Pearson, K. (1895). Notes on regression and inheritance in the case of two parents.
2. **Spearman**: Spearman, C. (1904). The proof and measurement of association between two things.
3. **Kendall**: Kendall, M. G. (1938). A new measure of rank correlation.
4. **Biweight**: Wilcox, R. R. (2011). Introduction to robust estimation and hypothesis testing.
5. **Shrinkage**: Ledoit, O., & Wolf, M. (2004). A well-conditioned estimator for large-dimensional covariance matrices.
6. **Partial**: Fisher, R. A. (1924). The distribution of the partial correlation coefficient.
