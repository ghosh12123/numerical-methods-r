# AI Usage Documentation

## Overview
This document describes the use of AI assistance in developing the NumericalMethods R package for Biostat 615. It includes my workflow, specific examples, and lessons learned.

## AI Tool Used
- **Tool**: Claude (Anthropic)
- **Version**: Claude Sonnet 3.5 / 4
---
  
## AI Workflow
  
### 1. Initial Project Setup
**My Approach**: I started by asking Claude to help structure an R package from scratch.

**Example Prompt**:
```
Create an R package called NumericalMethods that implements bisection, 
Newton-Raphson, secant, fixed-point iteration, golden section search, 
parabolic interpolation, and Brent's method. Include proper documentation 
and package structure.
```

**What Worked**: Claude generated the basic package structure (DESCRIPTION, NAMESPACE, R/ folder) and skeleton functions.

**What Didn't Work**: The initial algorithms were too simplistic and didn't track iteration history properly for visualization.

### 2. Iterative Development Process
My typical workflow with Claude:

1. **Describe the problem** clearly with specific requirements
2. **Upload existing code** for Claude to analyze
3. **Request specific changes** (e.g., "add iteration tracking to this function")
4. **Test the changes** locally
5. **Report errors** back to Claude with error messages
6. **Repeat** until working

This cycle often took 5-10 iterations per feature.

### 3. Debugging Strategy
When something broke, I would:
- Copy the **exact error message**
- Upload the **relevant code file**
- Explain **what I expected vs. what happened**
- Ask Claude to **identify the issue and fix it**

---

## Prominent Examples

### Example 1: Parabolic Interpolation Formula Debugging

**Initial Problem**: Parabolic interpolation was converging to wrong values for `x^4 - 3*x^2 + 5`.

**My Working Code** (that I provided to Claude):
```r
parabolic_interpolation_step <- function(a, b, c, fa, fb, fc) {
  numerator <- (a - b)^2 * (fb - fc) - (a - c)^2 * (fb - fa) 
  denominator <- 2 * ((a - b) * (fa - fc) - (a - c) * (fa - fb))
  
  if (abs(denominator) < 1e-12) {
    return(b)
  }
  
  x_new <- b - 0.5 * numerator / denominator
  return(x_new)
}
```

**Conversation with Claude**:
```
Me: "The parabolic interpolation is getting the wrong answer for x^4 - 3*x^2 + 5. 
Here's my working code that gets it right. Can you update the package to use this formula?"

Claude: [Analyzed the formula and updated the package implementation]

Me: "Now the formula works but the visualization broke - I'm seeing 4 points 
per iteration instead of 3."

Claude: [Identified duplicate point storage in history tracking]
```

**Iterations**: ~8 back-and-forth exchanges
**Outcome**: Working parabolic interpolation with correct visualization

### Example 2: Visualization Consistency

**Problem**: Each algorithm had different colors, point sizes, and styling.

**My Request**:
```
Make the parabolic graph styling consistent with golden section and Brent's method. 
The function curve looks different - use steelblue like the others. Also make the 
dots and X smaller, and extend the fitted parabola past the bracket boundaries.
```

**Claude's Response**: Updated multiple parameters:
- Changed `color = "blue"` to `color = "steelblue"`
- Changed point `size = 5` to `size = 3`
- Changed parabola extension from `0.3 * bracket_span` to `0.5 * bracket_span`

**Lesson Learned**: Be very specific about what you want changed. Vague requests like "make it look better" don't work well.

### Example 3: Shiny App - Handling Multiple Algorithms

**Problem**: The Shiny app needed to call different visualization functions based on which algorithm the user selected.

**Initial Attempt**: I tried using a large if-else chain in server.R:
```r
output$plot <- renderPlotly({
  if (input$algorithm == "Bisection") {
    plot_bisection_animation(result, f)
  } else if (input$algorithm == "Newton") {
    plot_newton_animation(result, f, df)
  } else if ...
  # This got very long and repetitive
})
```

**My Request**:
```
The Shiny app needs to display different animations based on the selected algorithm. 
Right now I have a huge if-else chain. Is there a cleaner way to dispatch to the 
right visualization function?
```

**Claude's Solution**: Create a dispatcher function:
```r
plot_algorithm_animation <- function(result, f, df = NULL) {
  algorithm <- result$algorithm
  
  if (grepl("Bisection", algorithm)) {
    return(plot_bisection_animation(result, f))
  } else if (grepl("Newton", algorithm)) {
    return(plot_newton_animation(result, f, df))
  } else if (grepl("Secant", algorithm)) {
    return(plot_secant_animation(result, f))
  }
  # ... etc
}
```

Then in server.R:
```r
output$plot <- renderPlotly({
  plot_algorithm_animation(result, f, df)
})
```

**Outcome**: Much cleaner server.R code, easier to add new algorithms

### Example 4: Creating Algorithm-Specific Visualizations

**Context**: Each of the 7 algorithms needed custom animated visualizations showing how they converge. This was the most time-intensive part of the package.

**Initial Approach for Bisection**:
```
Me: "Create an animated visualization for bisection method that shows:
- The function curve
- The interval boundaries [a, b] as vertical lines
- The midpoint as a red dot
- How the interval shrinks each iteration
Use ggplot2 and plotly for interactivity."
```

**Claude's First Attempt**: Created a basic plot but had issues:
- All frames showed at once (no animation)
- Points appeared at wrong locations
- Interval boundaries didn't update properly

**Debugging Process** (took ~6 iterations):
```
Me: "The animation isn't working. All frames display simultaneously instead of 
animating through iterations."

Claude: "You need to add the 'frame' aesthetic to ggplot. Here's the fix..."

Me: [Tests] "Better, but the interval boundaries are in the wrong place. The 
left boundary should be at history$interval_a, not history$x"

Claude: [Updates the geom_line data mapping]

Me: "Now the boundaries work, but they're covering the function curve. Make 
the function curve thicker (size=1.2) and boundaries thinner (size=0.8)"

Claude: [Adjusts sizes]
```

**Parabolic Interpolation Visualization** (Most Complex):

This one required the most back-and-forth because I wanted to show:
1. The 3 bracket points (a, b, c)
2. The fitted parabola through those points
3. The trial point (x_new) on the parabola

**Initial Request**:
```
Me: "For parabolic interpolation, I need to show:
- 3 black dots for the current bracket points
- A red dashed curve showing the fitted parabola
- A red X showing where the algorithm will evaluate next
The parabola should be fitted using the matrix solve method."
```

**First Problem**: Claude created the parabola but it didn't fit the points correctly.

```
Me: "The parabola doesn't go through the 3 bracket points. Here's how to fit it:
[Provided the matrix solve formula]
Make sure to use this exact formula."

Claude: [Updated to use correct formula]
```

**Second Problem**: Parabola disappeared when scrolling through iterations.

```
Me: "The fitted parabola goes away at some point, and when I try to scroll 
through iterations, it disappears entirely. Also, sometimes a 4th point appears."

Claude: [After analyzing] "You're storing both x_new (1 point) AND the triplet 
(3 points) in history, giving you 4 points total. The visualization expects 
exactly 3 points per iteration."

[Fixed by removing duplicate storage]
```

**Third Problem**: Styling inconsistency across algorithms.

```
Me: "The parabolic graph looks different than golden section and brent. The 
function curve color is wrong. Make the dots and X smaller, and the fitted 
parabola should extend past the bracket boundaries."

Claude: [Changed]:
- color = "blue" → color = "steelblue" (to match other algorithms)
- Point size = 5 → size = 3
- Trial point size = 6 → size = 4  
- Parabola extension: 0.3 * bracket_span → 0.5 * bracket_span
```

**Common Pattern Across All Visualizations**:

1. **First Pass**: I'd describe what to show
2. **Second Pass**: Fix data structure issues (points in wrong place, missing frames, etc.)
3. **Third Pass**: Adjust styling to match other algorithms
4. **Fourth Pass**: Handle edge cases (what if parabola is degenerate? what if derivative is zero?)

**Key Lesson**: Visualization debugging took 60-70% of my AI conversation time. Each algorithm went through 4-8 iterations before looking right.

---
  
## Specific Prompting Strategies That Worked
  
### 1. Being Specific About Requirements
Instead of describing code, be explicit about what you need:
- For example, avoid statements like "Make the visualization better"
- Instead, specify concrete requirements such as: "Make the parabolic graph match golden section styling: steelblue curve (size=1), smaller points (size=3), extend parabola 50% beyond brackets"

### 2. Showing Examples
When requesting changes, reference existing working code:
```
The plot for the golden section search algorithm looks good. Make the parabolic interpolation plot match that exact style.
```
### 3. Incremental Changes
Favor incremental, testable changes rather than large rewrites.
- For example, instead of requesting "Rewrite the entire package to fix X, Y, and Z"
- Break the work into measureable steps: "First fix X. [Test]. Now fix Y. [Test]. Now fix Z. [test]"

### 4. Asking for Explanations
When Claude suggested a fix I didn't understand:
```
Me: "Why are you using rep(iter, 3) here instead of just iter?"

Claude: "Because we're storing 3 bracket points per iteration, so all 3 need 
the same iteration number for the visualization to group them correctly."
```

---

## What AI Could Not Do

### 1. Understand Cascading Effects
- Claude couldn't predict that fixing the parabolic formula would break visualization
- I had to catch these dependencies through testing

### 2. Validate Mathematical Correctness
- Clade could generate mathematically plausible formulas, but could not guarantee they were correct for my specific implementation

### 3. Maintain Overall Project Consistency
- Claude responded effectively to localized requests, but it did not maintain a global view of the project as it evolved
- Ensuring consistency across algorithms, visualizations, and documentation required ongoing oversight

---

## Verification Process

All AI-generated code was verified through:

### 1. Unit Tests
Running `devtools::test()` to ensure algorithms produce correct results:
```r
# Example test that caught formula issues
test_that("parabolic finds minimum of x^4 - 3*x^2 + 5", {
  f <- function(x) x^4 - 3 * x^2 + 5
  result <- parabolic_interpolation(f, c(-2, 2))
  expect_true(abs(result$x_final - 1.2247) < 0.01)
})
```

### 2. Visual Inspection
Running the Shiny app and manually clicking through each algorithm to verify:
- Correct animation progression
- Proper point placement
- Consistent styling

### 3. Manual Calculations
For simple cases, I traced through iterations by hand:
```r
# Bisection on x^2 - 4 with [0, 3]
# Iteration 1: midpoint = 1.5, f(1.5) = -1.75 (negative)
# Iteration 2: midpoint = 2.25, f(2.25) = 1.0625 (positive)
# ... converges to x = 2
```

---

## Key Lessons Learned

### 1. AI as a Starting Point, Not the Finish Line
- AI gave me 70% working code quickly
- I spent 30% of my time fixing edge cases AI missed
- **But this was still faster than coding from scratch**

### 2. Specificity Matters
Bad prompt:
```
"The graph doesn't look right"
```

Good prompt:
```
"The parabolic fitted curve should extend 50% beyond the bracket points, 
not just 30%. Also change the curve color from blue to steelblue to match 
the other algorithms."
```

### 3. Test Immediately
- Don't accept 10 changes at once
- Test each change individually
- Makes debugging much easier

### 4. Keep Conversation Context
- I uploaded files multiple times as they evolved
- Claude's suggestions improved when it had current code
- Long conversations (100+ messages) maintained good context

---

## Code Ownership

The final code represents **my understanding** of the numerical methods. AI assistance was used as a tool for:
- Rapid prototyping
- Syntax help (especially ggplot2/plotly)
- Debugging complex issues

All code was:
- Reviewed line-by-line
- Tested with multiple test cases
- Modified when AI suggestions were wrong
- Understood before inclusion in final package

---

## Conclusion

AI was invaluable for this project, particularly for:
1. **Boilerplate implementation** (package structure, function templates)
2. **Debugging cryptic errors** (especially R package installation issues)
3. **Rapid iteration** (trying different visualization approaches)

However, AI **required constant supervision**:
- Fixes often broke other things
- Suggestions sometimes contradicted earlier advice
- Design decisions still needed human judgment

**Overall**: AI was a powerful assistant that accelerated development, but it wasn't autopilot - it required careful direction, testing, and verification at every step.