# tf_ggplot Analysis: Feasibility and Implementation Strategy

## Executive Summary

Based on deep analysis of ggplot2 internals, the tf class structure, and current implementation approaches, the proposed `tf_ggplot()` wrapper idea shows **strong feasibility** with some important caveats. The approach can work as a pragmatic solution but comes with architectural trade-offs.

## Background Context

### The Core Problem
The fundamental issue with tf-vectors in ggplot2 stems from a type mismatch in the rendering pipeline:

1. **Scale Training Phase**: ggplot2 trains scales early in `ggplot_build()` (line 70-77 in plot-build.R) before statistical transformations
2. **Type Expectations**: The scales package expects `typeof()` to return simple types like "double", but tf objects return "list"
3. **Custom Vector Classes**: tf objects are complex S3 classes built on vctrs that store functional data as lists of (arg, value) tuples

### Current Implementation Limitations
The existing approach in tidyfun uses:
- Custom `StatTf` that calls `tf_unnest()` to transform tf → data.frame
- Custom geoms like `geom_spaghetti()` that rely on this transformation
- Manual handling in functions like `gglasagna()`

This works but is limited because:
- Only works with specific custom geoms/stats
- Cannot leverage the full ggplot2 ecosystem (standard geoms, faceting, scales, etc.)
- Requires users to learn new geom functions instead of standard ggplot2 syntax

## Analysis of tf_ggplot Wrapper Approach

### Core Implementation Strategy

The proposed approach would work as follows:

```r
# User writes:
tf_ggplot(data) + geom_line(aes(tf = some_func, color = group))

# tf_ggplot() internally transforms to:
ggplot(transformed_data) + geom_line(aes(x = some_func.arg, y = some_func.value, 
                                       group = some_func.id, color = group))
```

### ✅ **FEASIBLE ASPECTS**

#### 1. Data Transformation is Well-Understood
- The `tf_unnest()` function already handles tf → long data.frame conversion reliably
- The transformation logic is mature and handles edge cases (irregular grids, NA functions, etc.)
- Performance is acceptable for typical functional data sizes

#### 2. Aesthetic Mapping Translation is Straightforward
- Can parse aesthetic mappings and detect tf-specific aesthetics
- Can programmatically generate new aesthetic mappings using standard ggplot2 names
- Can handle mixed aesthetics (some tf, some regular variables)

#### 3. ggplot2's Extensibility Supports This Pattern
- The `+.ggplot` method and `update_ggplot()` generic provide clear extension points
- Can create a custom ggplot-like object that pre-processes before calling real `ggplot()`
- Can leverage all existing ggplot2 infrastructure (themes, scales, facets, etc.)

### ⚠️ **CHALLENGING ASPECTS**

#### 1. Complex Aesthetic Combinations
**Issue**: When users mix tf and scalar aesthetics in complex ways:

```r
# Complex case: tf aesthetic + scalar summary
tf_ggplot(data) + 
  geom_point(aes(tf = func, color = tf_depth(func), size = mean_value))
```

**Solution**: Need sophisticated parsing to:
- Detect which aesthetics reference tf objects
- Evaluate scalar tf functions (like `tf_depth()`) before transformation
- Preserve non-tf aesthetics in the transformed data

#### 2. Faceting with Mixed Variables
**Issue**: When faceting involves both tf and non-tf variables:

```r
tf_ggplot(data) + 
  geom_line(aes(tf = func)) + 
  facet_grid(group ~ condition)
```

**Solution**: Must ensure grouping variables are properly replicated in the unnested data.

#### 3. Statistical Layers Interaction
**Issue**: What happens when users add stats that expect specific data structures:

```r
tf_ggplot(data) + 
  geom_line(aes(tf = func)) + 
  stat_smooth()  # How does this work with tf data?
```

**Partial Solution**: Some stats will work naturally (like `stat_smooth` on the unnested x,y data), others may not.

### 🚨 **CRITICAL LIMITATIONS**

#### 1. Scale Training with Mixed Data Types
**Major Issue**: If users have both tf and non-tf columns in aesthetics that get mapped to the same scale:

```r
tf_ggplot(data) + 
  geom_line(aes(tf = func)) +           # y-scale sees function values  
  geom_point(aes(x = time, y = scalar)) # same y-scale sees scalar values
```

**Problem**: The scales package will receive mixed data types during training, potentially causing the same issues we're trying to avoid.

**Mitigation**: Could detect such conflicts and warn users, or use separate scale training.

#### 2. Multi-tf Aesthetics
**Complex Case**: When multiple tf objects are used in different aesthetics:

```r
tf_ggplot(data) + 
  geom_segment(aes(tf_x = func1, tf_y = func2))  # Two tf aesthetics
```

**Challenge**: Need to handle the combinatorial explosion of evaluation grids and ensure proper alignment.

#### 3. User Mental Model Mismatch
**Philosophical Issue**: Users expect `aes(tf = func)` to behave like `aes(y = numeric_vector)`, but the underlying transformation changes the data structure significantly.

**Example Problem**:
```r
# User expectation: one row per function
tf_ggplot(data) + geom_point(aes(tf = func, color = id))

# Reality: multiple rows per function after unnesting
# Could cause unexpected color assignments or point multiplicities
```

## Complex Use Case Analysis

### Scenario 1: Multiple Functions with Grouping
```r
data <- tibble(
  subject = 1:100,
  treatment = sample(c("A", "B"), 100, replace = TRUE),
  func = tf_rgp(100),
  baseline_score = rnorm(100)
)

tf_ggplot(data) + 
  geom_line(aes(tf = func, color = treatment)) +
  facet_wrap(~treatment) +
  geom_hline(aes(yintercept = baseline_score))  # Scalar aesthetic
```

**Analysis**: 
- ✅ Feasible: tf → (x,y,group) transformation works
- ✅ Grouping by treatment is preserved
- ✅ Faceting works because treatment is in both original and transformed data
- ⚠️ Challenge: `baseline_score` needs to be replicated for each evaluation point
- ✅ Solution: Carry forward non-tf columns during unnesting

### Scenario 2: Functional Regression
```r
tf_ggplot(data) + 
  geom_line(aes(tf = observed_func), alpha = 0.3) +
  geom_line(aes(tf = predicted_func), color = "red") +
  geom_ribbon(aes(tf_ymin = lower_ci, tf_ymax = upper_ci), alpha = 0.2)
```

**Analysis**:
- ⚠️ Complex: Multiple tf aesthetics in different layers
- ⚠️ Challenge: Ensuring all tf objects are evaluated on compatible grids
- ✅ Solution: Use common evaluation grid for all tf objects in the same plot
- ⚠️ Challenge: Custom aesthetic names like `tf_ymin` need special handling

### Scenario 3: Mixed Plotting Types
```r
tf_ggplot(data) + 
  geom_line(aes(tf = func)) +
  geom_point(aes(x = visit_time, y = summary_score))  # Non-tf layer
```

**Analysis**:
- 🚨 **CRITICAL ISSUE**: Scale conflict between tf-derived y values and scalar y values
- ❌ **LIKELY FAILURE**: ggplot2 will try to train the same y-scale on both functional values and scalar values
- 🔧 **Possible Solution**: Use different y-scales or transform scalar data to match tf scale

## Technical Implementation Challenges

### 1. Evaluation Grid Management
**Problem**: Different tf objects may have different natural evaluation grids.

**Solution Strategy**:
```r
# Option A: Use union of all grids (expensive, potentially huge)
# Option B: Use common grid specified by user
# Option C: Use coarsest common grid that covers all domains
```

### 2. Performance Considerations
**Issue**: `tf_unnest()` can create large data.frames for dense functional data.

**Example**: 100 functions × 1000 evaluation points = 100,000 rows
- Memory impact for large datasets
- Rendering performance for many functions

**Mitigation**: 
- Provide options for evaluation grid coarsening
- Warning systems for large expansions
- Lazy evaluation where possible

### 3. Error Handling and User Feedback
**Challenge**: When transformation fails or produces unexpected results, users need clear error messages that relate back to their tf_ggplot code, not the internal ggplot calls.

**Solution**: Wrap all ggplot2 errors and provide tf_ggplot-specific diagnostic information.

## Verdict: FEASIBLE with Important Caveats

### ✅ **RECOMMENDED**: The tf_ggplot wrapper approach is feasible and would provide significant value.

**Reasons**:
1. **Solves the main problem**: Allows standard ggplot2 geoms to work with tf data
2. **Leverages existing infrastructure**: Uses mature tf_unnest() logic
3. **Provides familiar interface**: Users can use familiar geom_line(), geom_point(), etc.
4. **Extensible**: Can be enhanced over time with more sophisticated features

### ⚠️ **IMPLEMENTATION REQUIREMENTS**:

1. **Clear Documentation**: Users must understand the data transformation implications
2. **Sensible Defaults**: Default evaluation grids and transformation behavior
3. **Good Error Messages**: Clear feedback when things go wrong
4. **Performance Safeguards**: Warnings for large data expansions
5. **Subset Support Initially**: Start with most common use cases, expand over time

### 🚫 **KNOWN LIMITATIONS TO DOCUMENT**:

1. **No mixed scale types**: tf and non-tf aesthetics on same scale will conflict
2. **Grid alignment**: Multiple tf aesthetics must use compatible evaluation grids  
3. **Memory overhead**: Large functional datasets may become unwieldy after unnesting
4. **Statistical layers**: Some stats may not work intuitively with transformed data

## Next Steps if Implementation Proceeds

1. **Prototype core transformation logic**
2. **Handle single tf aesthetic in simple geoms**
3. **Add support for multiple tf aesthetics**
4. **Implement error handling and user warnings**
5. **Add performance optimizations**
6. **Comprehensive testing with edge cases**

---

# Implementation Plan for tf_ggplot

## Phase 1: Core Infrastructure (Week 1-2)

### 1.1 Create tf_ggplot Constructor Function
```r
tf_ggplot <- function(data = NULL, mapping = aes(), ..., 
                      arg = NULL, interpolate = TRUE) {
  # Validate inputs
  # Store original data and mapping
  # Return tf_ggplot object
}
```

**Key Design Decisions**:
- `tf_ggplot` returns a custom S3 class that inherits from `ggplot`
- Store original data and tf-specific parameters as attributes
- Defer actual transformation until `+` method is called

### 1.2 Aesthetic Mapping Parser
```r
parse_tf_aesthetics <- function(mapping) {
  # Identify tf-specific aesthetics (tf, tf_x, tf_y, tf_ymin, tf_ymax, etc.)
  # Separate tf and non-tf aesthetics
  # Return list(tf_aes = list(...), regular_aes = aes(...))
}
```

**Implementation Strategy**:
- Scan aesthetic mapping for tf-prefixed names
- Support `tf` as shorthand for `tf_y` 
- Handle special cases like `tf_ymin`, `tf_ymax` for ribbons

### 1.3 Data Transformation Engine
```r
transform_tf_data <- function(data, tf_aesthetics, arg = NULL, interpolate = TRUE) {
  # For each tf aesthetic, unnest the corresponding tf column
  # Align evaluation grids across multiple tf aesthetics
  # Replicate non-tf columns appropriately
  # Return transformed data.frame with new aesthetic mappings
}
```

**Core Logic**:
1. Extract unique tf objects from all tf aesthetics
2. Determine common evaluation grid (union, intersection, or user-specified)
3. Evaluate all tf objects on common grid using `tf_evaluate()`
4. Create long-format data.frame with arg/value columns
5. Generate group identifiers for each original function

## Phase 2: Basic geom Integration (Week 3-4)

### 2.1 Override `+.tf_ggplot` Method
```r
`+.tf_ggplot` <- function(e1, e2) {
  # If e2 is a layer (geom/stat), transform data and aesthetics
  # Convert tf_ggplot to regular ggplot with transformed data
  # Apply the layer to the transformed plot
  # Return regular ggplot (user can continue with standard ggplot2)
}
```

**Transformation Process**:
1. Detect if incoming layer uses tf aesthetics
2. Transform data using `transform_tf_data()`
3. Rewrite aesthetic mappings (`tf` → `y`, add `group` mappings, etc.)
4. Create regular ggplot with transformed data
5. Apply the layer with rewritten aesthetics

### 2.2 Support Core Geoms
**Priority Order**:
1. `geom_line()` - most common for functional data
2. `geom_point()` - for discrete evaluations  
3. `geom_ribbon()` - for confidence bands (requires tf_ymin/tf_ymax)
4. `geom_area()` - filled functional curves

**Example Transformation**:
```r
# Input:
tf_ggplot(data) + geom_line(aes(tf = func, color = group))

# Transformed to:
ggplot(transformed_data) + 
  geom_line(aes(x = func.arg, y = func.value, group = interaction(func.id, group), 
                color = group))
```

### 2.3 Error Handling and Validation
```r
validate_tf_plot <- function(data, mapping, layer) {
  # Check for scale conflicts (tf and non-tf on same aesthetic)
  # Validate tf objects exist in data
  # Check evaluation grid compatibility
  # Provide informative error messages
}
```

## Phase 3: Advanced Features (Week 5-6)

### 3.1 Multiple tf Aesthetics Support
**Challenge**: Handle cases like `aes(tf_x = func1, tf_y = func2)`

**Solution Strategy**:
1. Evaluate all tf objects on compatible grids
2. Handle different grid sizes via interpolation or restriction to common domain
3. Create Cartesian product of evaluations for multi-tf aesthetics

### 3.2 Ribbon/Band Geoms
**Special Case**: `geom_ribbon(aes(tf_ymin = lower, tf_ymax = upper))`

**Implementation**:
- Support tf_ymin, tf_ymax aesthetic names
- Ensure both tf objects use identical evaluation grids
- Transform to standard ymin/ymax aesthetics

### 3.3 Faceting Support
**Key Requirement**: Ensure grouping variables are preserved during transformation

**Implementation**:
- Detect faceting variables in the plot
- Replicate faceting variables for each evaluation point
- Test with `facet_wrap()` and `facet_grid()`

## Phase 4: Performance and User Experience (Week 7-8)

### 4.1 Performance Optimizations
```r
# Smart evaluation grid selection
choose_evaluation_grid <- function(tf_objects, max_points = 1000) {
  # Use sparsest grid that captures key features
  # Warn user about large expansions
  # Provide options for custom grids
}

# Lazy evaluation for large datasets
lazy_transform <- function(data, aesthetics) {
  # Only transform when plot is actually rendered
  # Cache transformations for repeated use
}
```

### 4.2 User Feedback and Diagnostics
```r
# Informative warnings
tf_ggplot_warn <- function(message, data_size, expansion_factor) {
  # Context-aware warning messages
  # Show before/after data dimensions
  # Suggest optimizations
}

# Diagnostic tools
diagnose_tf_plot <- function(tf_ggplot_obj) {
  # Show evaluation grids being used
  # Memory usage estimates
  # Performance recommendations
}
```

### 4.3 Documentation and Examples
- Comprehensive vignette with real-world examples
- Best practices guide for performance
- Migration guide from current geom_spaghetti() approach
- Troubleshooting common issues

## Phase 5: Testing and Edge Cases (Week 9-10)

### 5.1 Comprehensive Test Suite
```r
# Core functionality tests
test_that("tf_ggplot basic line plots work", { ... })
test_that("multiple tf aesthetics align correctly", { ... })
test_that("faceting preserves grouping", { ... })

# Edge case tests  
test_that("irregular tf objects handled correctly", { ... })
test_that("empty tf objects don't break plots", { ... })
test_that("mixed tf and scalar aesthetics produce sensible errors", { ... })

# Performance tests
test_that("large datasets produce appropriate warnings", { ... })
test_that("evaluation grid selection is reasonable", { ... })
```

### 5.2 Real-World Data Testing
- Test with FDA datasets (growth curves, pharmacokinetics)
- Test with irregular/sparse functional data
- Test with high-dimensional functional data
- Performance benchmarking vs current approaches

## Implementation Timeline

| Phase | Duration | Key Deliverables |
|-------|----------|------------------|
| 1 | 2 weeks | Core infrastructure, aesthetic parsing |
| 2 | 2 weeks | Basic geom integration, +.tf_ggplot method |
| 3 | 2 weeks | Advanced features, ribbon support |
| 4 | 2 weeks | Performance optimization, user experience |
| 5 | 2 weeks | Testing, documentation, edge cases |

**Total Estimated Time**: 10 weeks for full implementation

## Key Files to Create/Modify

### New Files
- `R/tf-ggplot.R` - Core tf_ggplot functionality
- `R/tf-ggplot-transform.R` - Data transformation logic  
- `R/tf-ggplot-aesthetics.R` - Aesthetic mapping utilities
- `tests/testthat/test-tf-ggplot.R` - Test suite
- `vignettes/tf-ggplot-guide.Rmd` - User guide

### Modified Files
- `NAMESPACE` - Export new functions
- `DESCRIPTION` - Add any new dependencies
- `tidyfun-package.R` - Document new functionality

## Risk Mitigation

### High Risk: Scale Conflicts
**Risk**: Users mixing tf and non-tf aesthetics on same scale
**Mitigation**: 
- Clear documentation of limitations
- Runtime detection and informative error messages
- Possible future enhancement with scale isolation

### Medium Risk: Performance Issues  
**Risk**: Large functional datasets causing memory/speed problems
**Mitigation**:
- Evaluation grid optimization
- Warning systems for large expansions
- Lazy evaluation strategies

### Low Risk: Edge Case Behaviors
**Risk**: Unexpected behavior in complex aesthetic combinations
**Mitigation**:
- Comprehensive testing
- Gradual feature rollout
- Clear documentation of supported use cases

---

*Analysis and Implementation Plan by Claude Code on 2025-07-30*
*Based on ggplot2 v3.5.0+ source code analysis and tf/tidyfun package investigation*
