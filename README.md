# NumericalMethods

An R package implementing classical numerical algorithms for root-finding and optimization, with interactive visualizations and a Shiny dashboard for algorithm comparison.

## Overview

Root-finding and optimization are fundamental problems in computational statistics and scientific computing. Whether estimating maximum likelihood parameters, solving nonlinear equations in simulations, or finding optimal model configurations, these numerical methods form the backbone of many statistical applications.

This package provides implementations of classical numerical algorithms with a focus on education and visualization. Unlike base R's `uniroot()` or `optimize()` which only return final answers, this package tracks the entire iteration history, classifies convergence rates, and provides animated visualizations showing exactly how each algorithm approaches the solution. This makes it useful for:

- **Learning**: Understanding how algorithms like Newton-Raphson or Brent's method actually work step-by-step
- **Teaching**: Demonstrating algorithm behavior with interactive visualizations
- **Comparison**: Seeing how different methods perform on the same problem
- **Debugging**: Diagnosing convergence issues by examining iteration history

**Key Features:**
- 4 root-finding algorithms (Bisection, Newton-Raphson, Secant, Fixed-Point)
- 3 optimization algorithms (Golden Section, Parabolic Interpolation, Brent's Method)
- Automatic derivative computation using `numDeriv` (no manual derivatives needed)
- Iteration history tracking for all algorithms
- Convergence rate classification (Linear, Superlinear, Quadratic)
- Animated visualizations showing algorithm progression
- Interactive Shiny dashboard for side-by-side comparison

## Automatic Derivatives

This package uses the `numDeriv` package to compute derivatives automatically. You never need to provide analytical derivatives—just pass your function and the algorithms handle the rest.

For example, Newton-Raphson requires f'(x), and Fixed-Point convergence analysis requires g'(x*) and g''(x*). These are all computed numerically via `numDeriv::grad()` behind the scenes.

```r
# No derivative needed - just pass f
f <- function(x) x^3 - 2*x - 5
result <- newton_raphson(f, x0 = 2)  # derivative computed automatically
```

## Installation

### Prerequisites

Ensure you have R (≥ 4.0) installed. The package depends on:

```r
install.packages(c("shiny", "shinydashboard", "ggplot2", "plotly", 
                   "dplyr", "DT", "gridExtra", "numDeriv"))
```

### Install from Local Source

```r
# Install directly from GitHub
# When prompted to update packages, press Enter to skip updates
devtools::install_github("ghosh12123/numerical-methods-r')
```

### Load the Package

```r
library(NumericalMethods)
```

## Quick Start Example

A complete workflow showing root-finding analysis:

```r
library(NumericalMethods)

# Define function: find where x^3 - 2x - 5 = 0
f <- function(x) x^3 - 2*x - 5

# Run all algorithms and compare
results <- solve_root(f, initial = c(1, 3))
comparison <- compare_all_methods(f, "root", c(1, 3))
print(comparison)

# Plot convergence comparison
plot_comparison(results)

# Analyze best performer (Newton-Raphson)
newton_result <- results$newton
print(newton_result$history)
plot_convergence(newton_result)

# View animated visualization
plot_newton_animation(newton_result, f)

# Launch interactive dashboard for exploration
run_shiny_app()
```

## Example Usage

### Root-Finding

Find where f(x) = 0.

```r
# Define function
f <- function(x) x + cos(x)

# Run all root-finding algorithms
results <- solve_root(f, initial = c(-2, 2))

# View comparison table
comparison <- compare_all_methods(f, "root", c(-2, 2))
print(comparison)
```

**Run individual algorithms:**

```r
# Bisection Method - requires interval [a, b] where f(a) and f(b) have opposite signs
result_bisection <- bisection_method(f, c(-2, 2), tol = 1e-10, max_iter = 500)

# Newton-Raphson - requires initial guess (derivative computed automatically)
result_newton <- newton_raphson(f, x0 = 0, tol = 1e-10, max_iter = 500)

# Secant Method - requires two initial guesses
result_secant <- secant_method(f, x0 = -2, x1 = 2, tol = 1e-10, max_iter = 500)

# Fixed-Point Iteration - uses g(x) = x - f(x) by default
result_fp <- fixed_point_iteration(f, x0 = 0, tol = 1e-10, max_iter = 500)
```

### Optimization

Find the minimum of f(x).

```r
# Define function
f <- function(x) x^4 - 3*x^2 + 5

# Run all optimization algorithms
results <- solve_minimize(f, interval = c(-2, 2))

# View comparison table
comparison <- compare_all_methods(f, "minimize", c(-2, 2))
print(comparison)
```

**Run individual algorithms:**

```r
# Golden Section Search
result_golden <- golden_section_search(f, c(-2, 2), tol = 1e-10, max_iter = 500)

# Parabolic Interpolation
result_parabolic <- parabolic_interpolation(f, c(-2, 2), tol = 1e-10, max_iter = 500)

# Brent's Method (hybrid golden + parabolic)
result_brent <- brent_method(f, c(-2, 2), tol = 1e-10, max_iter = 500)
```

## Launching the Shiny App

```r
library(NumericalMethods)
run_shiny_app()
```

## Algorithms

### Root-Finding Algorithms

| Algorithm | Convergence Rate | Requirements | Description |
|-----------|------------------|--------------|-------------|
| **Bisection** | Linear | Interval [a,b] with sign change | Repeatedly halves interval containing root. Guaranteed convergence but slow. |
| **Newton-Raphson** | Quadratic | Initial guess x₀ | Uses tangent line to find next approximation. Fast but may diverge with bad initial guess. |
| **Secant** | Superlinear (~1.618) | Two initial guesses x₀, x₁ | Approximates derivative using secant line. Nearly as fast as Newton without needing derivatives. |
| **Fixed-Point** | Linear* | Initial guess x₀ | Iterates x_{n+1} = g(x_n). Convergence depends on |g'(x*)| < 1. |

*Fixed-Point can achieve Quadratic convergence when g'(x*) = 0.

### Optimization Algorithms

| Algorithm | Convergence Rate | Requirements | Description |
|-----------|------------------|--------------|-------------|
| **Golden Section** | Linear | Interval [a,b] | Shrinks interval by golden ratio (≈0.618) each iteration. Robust but slow. |
| **Parabolic Interpolation** | Superlinear | Interval [a,b] | Fits parabola through 3 points to estimate minimum. Fast but can fail on non-smooth functions. |
| **Brent's Method** | Superlinear | Interval [a,b] | Hybrid of golden section and parabolic. Uses parabolic when safe, golden as fallback. Best of both worlds. |

## Analyzing Individual Algorithms

Each algorithm returns a result object containing:

```r
result <- newton_raphson(f, x0 = 0)

# Access results
result$algorithm        # Algorithm name
result$converged        # TRUE/FALSE
result$x_final          # Final x value
result$f_final          # f(x_final)
result$iterations       # Number of iterations
result$convergence_rate # "Linear", "Superlinear", or "Quadratic"
result$message          # Status message
result$history          # Data frame of all iterations
```

### Viewing Iteration History

```r
# See step-by-step progress
print(result$history)
```

### Plotting Convergence

```r
# Single algorithm convergence plot (iteration vs |f(x)|)
plot_convergence(result)

# Compare multiple algorithms
results <- solve_root(f, c(-2, 2))
plot_comparison(results)
```

### Animated Visualizations

```r
# Algorithm-specific animations (returns plotly object)
plot_algorithm_animation(result, f)

# Or call specific animation functions directly
plot_bisection_animation(result, f)    # Shows interval halving
plot_newton_animation(result, f)       # Shows tangent lines
plot_secant_animation(result, f)       # Shows secant lines
plot_fixedpoint_animation(result, f)   # Shows cobweb diagram
plot_golden_animation(result, f)       # Shows golden ratio points
plot_parabolic_animation(result, f)    # Shows fitted parabolas
plot_brent_animation(result, f)        # Shows hybrid approach
```

## Shiny App Features

The interactive dashboard provides:

### Input Panel
- **Problem Type:** Toggle between Root-Finding and Minimization
- **Custom Function:** Enter any function of x (e.g., `x^4 - 3*x^2 + 5`, `cos(x) + x`)
- **Interval:** Set search bounds [a, b] / initial gusses (x0,x1)
- **Tolerance:** Convergence tolerance (default: 1e-10)
- **Max Iterations:** Iteration limit (default: 500)

### Results Panel
- **Summary Table:** Side-by-side comparison of all algorithms showing status, final values, iterations, and convergence rate
- **Recommendation:** Suggests best algorithm based on performance

### Visualization Panel
- **Convergence Plot:** Log-scale plot of |f(x)| vs iteration for all algorithms
- **Iterations Bar Chart:** Visual comparison of iteration counts
- **Algorithm Animation:** Select any algorithm to see step-by-step animated visualization

### Export
- **Download CSV:** Export results table for further analysis

## Function Reference

### Solvers
| Function | Description |
|----------|-------------|
| `solve_root(f, initial, ...)` | Run all root-finding algorithms |
| `solve_minimize(f, interval, ...)` | Run all optimization algorithms |
| `compare_all_methods(f, problem_type, initial, ...)` | Get comparison data frame |

### Root-Finding
| Function | Description |
|----------|-------------|
| `bisection_method(f, interval, tol, max_iter)` | Bisection method |
| `newton_raphson(f, x0, tol, max_iter)` | Newton-Raphson method |
| `secant_method(f, x0, x1, tol, max_iter)` | Secant method |
| `fixed_point_iteration(f, x0, tol, max_iter)` | Fixed-point iteration |

### Optimization
| Function | Description |
|----------|-------------|
| `golden_section_search(f, interval, tol, max_iter)` | Golden section search |
| `parabolic_interpolation(f, interval, tol, max_iter)` | Parabolic interpolation |
| `brent_method(f, interval, tol, max_iter)` | Brent's method |

### Visualization
| Function | Description |
|----------|-------------|
| `plot_convergence(result)` | Single algorithm convergence plot |
| `plot_comparison(results)` | Multi-algorithm comparison plot |
| `plot_algorithm_animation(result, f)` | Auto-dispatch to correct animation |
| `run_shiny_app()` | Launch interactive dashboard |

## Tips for Best Results

1. **Bisection:** Ensure f(a) and f(b) have opposite signs, otherwise it will fail.

2. **Newton-Raphson:** Start with a guess close to the root. Bad initial guesses can cause divergence or convergence to wrong root.

3. **Fixed-Point:** This method often diverges. It works best when |g'(x*)| < 1 near the root.

4. **Golden Section:** Always converges but is slow. Use when function is noisy or non-smooth.

5. **Brent's Method:** Generally the best choice for optimization - robust and fast.

## Testing

Run package tests:

```r
# Run all tests
devtools::test()
```

## License

MIT License

## Author

Ishan Ghosh

## Acknowledgments

Developed as part of Biostat 615 coursework on numerical methods and computational statistics.



