#' Visualization Functions
#'
#' @description Create publication-quality plots for algorithm analysis
#'
#' @name visualization
#' @import ggplot2
#' @import dplyr
NULL

#' Plot Convergence History
#'
#' @description Create detailed convergence analysis plot for single algorithm
#'
#' @param result Result object from any algorithm
#' @param true_solution Known solution for error calculation (optional)
#'
#' @return ggplot object
#'
#' @export
plot_convergence <- function(result) {
  
  if (is.null(result$history) || nrow(result$history) == 0) {
    stop("No history available for plotting")
  }
  
  library(ggplot2)
  
  history <- result$history
  
  # Single plot: Iteration vs |f(x)|
  p <- ggplot(history, aes(x = iteration, y = abs(f_x))) +
    geom_point(color = "steelblue", size = 3) +
    theme_minimal() +
    labs(
      title = paste(result$algorithm, "- Convergence Plot"),
      subtitle = sprintf("Status: %s | Iterations: %d | Rate: %s",
                         ifelse(result$converged, "Converged", result$message),
                         result$iterations,
                         result$convergence_rate),
      x = "Iteration",
      y = "|f(x)|"
    )
  
  # Only add line if more than 1 point
  if (nrow(history) > 1) {
    p <- p + geom_line(color = "steelblue", size = 1)
  }
  
  # Only use log scale if we have positive values and more than 1 unique value
  if (all(abs(history$f_x) > 0) && length(unique(abs(history$f_x))) > 1) {
    p <- p + scale_y_log10()
  }
  
  p
}


#' Plot Algorithm Comparison
#'
#' @description Compare multiple algorithms on same plot
#'
#' @param results List of result objects
#' @param metric Which metric to plot: "function_value", "x_value", or "step_size"
#'
#' @return ggplot object
#'
#' @export
plot_comparison <- function(results) {
  
  library(ggplot2)
  library(dplyr)
  
  # Combine histories - only use common columns (iteration, x, f_x)
  history_list <- lapply(names(results), function(name) {
    r <- results[[name]]
    if (!is.null(r$history) && nrow(r$history) > 0) {
      # Extract only common columns
      h <- r$history[, c("iteration", "x", "f_x")]
      h$algorithm <- r$algorithm
      return(h)
    }
    NULL
  })
  
  history_combined <- do.call(rbind, Filter(Negate(is.null), history_list))
  
  if (is.null(history_combined) || nrow(history_combined) == 0) {
    stop("No history data available for comparison")
  }
  
  # Create convergence plot: iteration vs |f(x)|
  p <- ggplot(history_combined, aes(x = iteration, y = abs(f_x), 
                                    color = algorithm, group = algorithm)) +
    geom_point(size = 2) +
    theme_minimal() +
    labs(
      title = "Convergence Plot",
      x = "Iteration",
      y = "|f(x)|",
      color = "Algorithm"
    ) +
    theme(legend.position = "bottom")
  
  # Only add lines for algorithms with more than 1 point
  algorithms_with_lines <- history_combined %>%
    group_by(algorithm) %>%
    filter(n() > 1) %>%
    ungroup()
  
  if (nrow(algorithms_with_lines) > 0) {
    p <- p + geom_line(data = algorithms_with_lines, size = 1)
  }
  
  # Only use log scale if we have positive values
  if (all(abs(history_combined$f_x) > 0)) {
    p <- p + scale_y_log10()
  }
  
  p
}



#' Create Summary Dashboard
#'
#' @description Create comprehensive dashboard with all metrics
#'
#' @param results List of result objects
#' @param problem_description Description of the problem
#'
#' @return Combined grid of plots
#'
#' @export
create_dashboard <- function(results, problem_description = "") {
  
  library(ggplot2)
  library(dplyr)
  library(gridExtra)
  
  # Extract summary data
  summary_data <- lapply(names(results), function(name) {
    r <- results[[name]]
    data.frame(
      algorithm = r$algorithm,
      mode = ifelse(is.null(r$mode), "Direct", r$mode),
      iterations = r$iterations,
      converged = r$converged,
      x_final = r$x_final,
      f_final = r$f_final,
      stringsAsFactors = FALSE
    )
  })
  
  summary_df <- do.call(rbind, summary_data)
  
  # Plot 1: Iterations comparison (bar chart)
  summary_df$color_group <- ifelse(grepl("Direct", summary_df$mode), "Direct", "Converted")
  
  p1 <- ggplot(summary_df, aes(x = reorder(algorithm, -iterations), y = iterations, fill = color_group)) +
    geom_col(alpha = 0.7) +
    coord_flip() +
    scale_fill_manual(values = c("Direct" = "forestgreen", "Converted" = "orange")) +
    theme_minimal() +
    labs(
      title = "Iteration Count Comparison",
      x = "Algorithm",
      y = "Iterations",
      fill = "Mode"
    ) +
    theme(legend.position = "top")
  
  # Plot 2: Convergence comparison
  p2 <- plot_comparison(results, "function_value") +
    theme(legend.text = element_text(size = 7))
  
  # Plot 3: Final accuracy
  converged_df <- summary_df %>% filter(converged)
  
  if (nrow(converged_df) > 0) {
    p3 <- ggplot(converged_df, aes(x = reorder(algorithm, abs(f_final)), 
                                   y = abs(f_final), fill = color_group)) +
      geom_col(alpha = 0.7) +
      scale_y_log10() +
      coord_flip() +
      scale_fill_manual(values = c("Direct" = "forestgreen", "Converted" = "orange")) +
      theme_minimal() +
      labs(
        title = "Final Accuracy",
        x = "Algorithm",
        y = "|f(x_final)|"
      ) +
      theme(legend.position = "none")
  } else {
    p3 <- ggplot() + theme_void()
  }
  
  # Plot 4: Summary statistics
  stats_text <- sprintf(
    "Problem: %s\n\nTotal Algorithms: %d\nConverged: %d\nFailed: %d\n\nFastest: %s\n(%d iterations)",
    problem_description,
    nrow(summary_df),
    sum(summary_df$converged),
    sum(!summary_df$converged),
    summary_df$algorithm[which.min(summary_df$iterations)],
    min(summary_df$iterations)
  )
  
  p4 <- ggplot() +
    annotate("text", x = 0.1, y = 0.5, label = stats_text, 
             size = 4, hjust = 0, vjust = 0.5, family = "mono") +
    theme_void()
  
  # Combine plots
  grid.arrange(p1, p2, p3, p4, nrow = 2, ncol = 2,
               top = textGrob(paste("Numerical Methods Dashboard\n", problem_description),
                              gp = gpar(fontsize = 16, fontface = "bold")))
}


#' ==============================================================================
#' ALGORITHM-SPECIFIC ANIMATED VISUALIZATIONS
#' ==============================================================================

#' Plot Bisection Method - Interval Halving Animation
#'
#' @description Shows how bisection method halves the interval at each iteration
#' @param result Result object from bisection_method
#' @param f Function being solved
#' @return plotly animation object
#' @export
plot_bisection_animation <- function(result, f) {
  
  if (is.null(result$history) || nrow(result$history) == 0) {
    stop("No history available for plotting")
  }
  
  library(ggplot2)
  library(plotly)
  
  history <- result$history
  
  # Get convergence area
  all_x <- c()
  for (i in 1:nrow(history)) {
    all_x <- c(all_x, history$x[i])
  }
  
  # Get the FULL interval from user input (use history bounds if available)
  if ("interval_a" %in% names(history) && "interval_b" %in% names(history)) {
    user_interval_min <- min(history$interval_a[1], history$interval_b[1])
    user_interval_max <- max(history$interval_a[1], history$interval_b[1])
  } else {
    user_interval_min <- min(all_x)
    user_interval_max <- max(all_x)
  }
  
  # Plot function over WIDE RANGE (but not crazy wide - 5x extension)
  interval_span <- user_interval_max - user_interval_min
  if (interval_span == 0) interval_span <- 1
  
  # Extend 5x beyond user interval on each side
  full_x_min <- user_interval_min - 5 * interval_span
  full_x_max <- user_interval_max + 5 * interval_span
  
  # Create FULL function curve 
  x_grid <- seq(full_x_min, full_x_max, length.out = 1000)
  y_grid <- sapply(x_grid, f)
  
  # Determine VIEWING window - show USER'S FULL INTERVAL
  view_x_min <- user_interval_min
  view_x_max <- user_interval_max
  
  # Calculate y-range from VALUES IN USER'S INTERVAL
  x_in_view <- x_grid[x_grid >= view_x_min & x_grid <= view_x_max]
  y_in_view <- sapply(x_in_view, f)
  
  # Also include iteration points
  all_y_values <- c(y_in_view, history$f_x)
  y_finite <- all_y_values[is.finite(all_y_values)]
  
  if (length(y_finite) > 0) {
    y_range <- range(y_finite, na.rm = TRUE)
    y_padding <- diff(y_range) * 0.15
    if (y_padding == 0 || !is.finite(y_padding)) y_padding <- 1
    y_min <- y_range[1] - y_padding
    y_max <- y_range[2] + y_padding
  } else {
    y_min <- -10
    y_max <- 10
  }
  
  # Prepare animation data
  anim_data <- data.frame()
  
  # Create y=0 reference line spanning FULL x-range (all iterations)
  zero_line_df <- data.frame(
    x = c(full_x_min, full_x_max),
    y = c(0, 0),
    iteration = rep(1:nrow(history), each = 2),
    element = "zero_line",
    stringsAsFactors = FALSE
  )
  
  for (iter in 1:nrow(history)) {
    # Function curve
    curve_df <- data.frame(
      x = x_grid,
      y = y_grid,
      iteration = iter,
      element = "curve",
      stringsAsFactors = FALSE
    )
    
    # Current midpoint
    mid_x <- history$x[iter]
    mid_y <- history$f_x[iter]
    
    # Midpoint (red dot, size = 2)
    midpoint_df <- data.frame(
      x = mid_x,
      y = mid_y,
      iteration = iter,
      element = "midpoint",
      stringsAsFactors = FALSE
    )
    
    # Get interval bounds if available
    if ("interval_a" %in% names(history) && "interval_b" %in% names(history)) {
      a_val <- history$interval_a[iter]
      b_val <- history$interval_b[iter]
      fa_val <- f(a_val)
      fb_val <- f(b_val)
      
      # Vertical lines at interval bounds (full height, like golden/brent)
      left_line_df <- data.frame(
        x = c(a_val, a_val),
        y = c(y_min, y_max),
        iteration = iter,
        element = "left_line",
        stringsAsFactors = FALSE
      )
      
      right_line_df <- data.frame(
        x = c(b_val, b_val),
        y = c(y_min, y_max),
        iteration = iter,
        element = "right_line",
        stringsAsFactors = FALSE
      )
      
      # Vertical dashed red line at midpoint (full height)
      midpoint_line_df <- data.frame(
        x = c(mid_x, mid_x),
        y = c(y_min, y_max),
        iteration = iter,
        element = "midpoint_line",
        stringsAsFactors = FALSE
      )
      
      # Current interval shading (light blue) - full height
      interval_shade_df <- data.frame(
        x = c(a_val, b_val, b_val, a_val),
        y = c(y_min, y_min, y_max, y_max),
        iteration = iter,
        element = "interval",
        stringsAsFactors = FALSE
      )
      
      # Determine which subinterval will be eliminated
      # If f(a) * f(mid) > 0, left side is eliminated (a moves to mid)
      # Otherwise, right side is eliminated (b moves to mid)
      if (fa_val * mid_y > 0) {
        # Left interval [a, mid] will be eliminated
        elim_shade_df <- data.frame(
          x = c(a_val, mid_x, mid_x, a_val),
          y = c(y_min, y_min, y_max, y_max),
          iteration = iter,
          element = "eliminated",
          stringsAsFactors = FALSE
        )
      } else {
        # Right interval [mid, b] will be eliminated
        elim_shade_df <- data.frame(
          x = c(mid_x, b_val, b_val, mid_x),
          y = c(y_min, y_min, y_max, y_max),
          iteration = iter,
          element = "eliminated",
          stringsAsFactors = FALSE
        )
      }
      
      # Get zero line for this iteration
      zero_line_iter <- zero_line_df[zero_line_df$iteration == iter, ]
      
      frame_data <- rbind(curve_df, zero_line_iter, interval_shade_df, elim_shade_df,
                          left_line_df, right_line_df, midpoint_line_df, midpoint_df)
    } else {
      # Fallback: just show midpoint
      zero_line_iter <- zero_line_df[zero_line_df$iteration == iter, ]
      frame_data <- rbind(curve_df, zero_line_iter, midpoint_df)
    }
    
    anim_data <- rbind(anim_data, frame_data)
  }
  
  # Create plot with legend
  p <- ggplot() +
    geom_polygon(data = subset(anim_data, element == "interval"),
                 aes(x = x, y = y, frame = iteration, fill = "Current Interval"),
                 alpha = 0.2) +
    geom_polygon(data = subset(anim_data, element == "eliminated"),
                 aes(x = x, y = y, frame = iteration, fill = "Eliminated Region"),
                 alpha = 0.3) +
    geom_line(data = subset(anim_data, element == "curve"),
              aes(x = x, y = y, frame = iteration),
              color = "steelblue", size = 1) +
    geom_line(data = subset(anim_data, element == "zero_line"),
              aes(x = x, y = y, frame = iteration),
              linetype = "dashed", color = "gray50", size = 0.5) +
    geom_line(data = subset(anim_data, element == "left_line"),
              aes(x = x, y = y, frame = iteration, color = "Interval Bounds"),
              size = 1.5, linetype = "solid") +
    geom_line(data = subset(anim_data, element == "right_line"),
              aes(x = x, y = y, frame = iteration, color = "Interval Bounds"),
              size = 1.5, linetype = "solid", show.legend = FALSE) +
    geom_line(data = subset(anim_data, element == "midpoint_line"),
              aes(x = x, y = y, frame = iteration, color = "Midpoint"),
              size = 1, linetype = "dashed") +
    geom_point(data = subset(anim_data, element == "midpoint"),
               aes(x = x, y = y, frame = iteration, color = "Midpoint"),
               size = 2) +
    scale_color_manual(
      values = c(
        "Midpoint" = "red",
        "Interval Bounds" = "darkgray"
      )
    ) +
    scale_fill_manual(
      values = c(
        "Current Interval" = "lightblue",
        "Eliminated Region" = "red"
      ),
      guide = "none"
    ) +
    guides(color = guide_legend(title = NULL)) +
    coord_cartesian(xlim = c(view_x_min, view_x_max), ylim = c(y_min, y_max)) +
    theme_minimal() +
    theme(legend.position = "right") +
    labs(
      title = "Bisection Method: Interval Halving",
      subtitle = "Blue = current interval | Red shading = region to be eliminated",
      x = "x",
      y = "f(x)"
    )
  
  plotly_obj <- ggplotly(p) %>%
    animation_opts(frame = 1000, transition = 500, redraw = FALSE) %>%
    config(modeBarButtonsToRemove = c('select2d', 'lasso2d'))
  
  # Fix legend: remove ",1" everywhere including inside parentheses
  for (i in seq_along(plotly_obj$x$data)) {
    trace <- plotly_obj$x$data[[i]]
    if (!is.null(trace$name)) {
      plotly_obj$x$data[[i]]$name <- gsub(",\\s*1\\)", ")", trace$name)
      plotly_obj$x$data[[i]]$name <- gsub(",\\s*1$", "", plotly_obj$x$data[[i]]$name)
      
      # Hide duplicate "Interval Bounds"
      if (grepl("Interval Bounds", plotly_obj$x$data[[i]]$name) && i > 1) {
        for (j in 1:(i-1)) {
          if (!is.null(plotly_obj$x$data[[j]]$name) && 
              grepl("Interval Bounds", plotly_obj$x$data[[j]]$name)) {
            plotly_obj$x$data[[i]]$showlegend <- FALSE
            break
          }
        }
      }
    }
  }
  
  # Add legend title
  plotly_obj <- plotly_obj %>%
    layout(legend = list(title = list(text = "Legend")))
  
  plotly_obj
}


#' Plot Newton-Raphson Method - Tangent Lines Animation
#'
#' @description Shows tangent lines at each iteration leading to next guess
#' @param result Result object from newton_raphson
#' @param f Function being solved
#' @param df Derivative function (will compute numerically if NULL)
#' @return plotly animation object
#' @export
plot_newton_animation <- function(result, f, df = NULL) {
  
  if (is.null(result$history) || nrow(result$history) == 0) {
    stop("No history available for plotting")
  }
  
  library(ggplot2)
  library(plotly)
  library(numDeriv)
  
  # Calculate numerical derivative via numDeriv package
  df <- function(x) grad(f, x)
  
  history <- result$history
  
  # Determine convergence range
  x_range <- range(history$x)
  x_span <- diff(x_range)
  if (x_span == 0) x_span <- 1
  
  # Create FULL function curve (5x wider than convergence path)
  full_x_min <- x_range[1] - 5 * x_span
  full_x_max <- x_range[2] + 5 * x_span
  x_grid <- seq(full_x_min, full_x_max, length.out = 1000)
  y_grid <- sapply(x_grid, f)
  
  # VIEWING window: show convergence path plus padding
  view_x_padding <- x_span * 0.5
  view_x_min <- x_range[1] - view_x_padding
  view_x_max <- x_range[2] + view_x_padding
  
  # Calculate y-range from viewing window
  x_in_view <- x_grid[x_grid >= view_x_min & x_grid <= view_x_max]
  y_in_view <- sapply(x_in_view, f)
  all_y_values <- c(y_in_view, history$f_x)
  y_finite <- all_y_values[is.finite(all_y_values)]
  
  if (length(y_finite) > 0) {
    y_range <- range(y_finite, na.rm = TRUE)
    y_padding <- diff(y_range) * 0.15
    if (y_padding == 0 || !is.finite(y_padding)) y_padding <- 1
    y_min <- y_range[1] - y_padding
    y_max <- y_range[2] + y_padding
  } else {
    y_min <- -10
    y_max <- 10
  }
  
  # Prepare animation data
  anim_data <- data.frame()
  
  # Create y=0 reference line spanning FULL x-range
  zero_line_df <- data.frame(
    x = c(full_x_min, full_x_max),
    y = c(0, 0),
    iteration = rep(1:nrow(history), each = 2),
    element = "zero_line",
    stringsAsFactors = FALSE
  )
  
  for (iter in 1:nrow(history)) {
    # Function curve
    curve_df <- data.frame(
      x = x_grid,
      y = y_grid,
      iteration = iter,
      element = "curve"
    )
    
    # Current point ON THE CURVE
    x_curr <- history$x[iter]
    y_curr <- history$f_x[iter]
    
    point_df <- data.frame(
      x = x_curr,
      y = y_curr,
      iteration = iter,
      element = "point"
    )
    
    # Tangent line
    slope <- df(x_curr)
    # Tangent: y - y_curr = slope * (x - x_curr)
    # y = slope * (x - x_curr) + y_curr
    # Use FULL x range for tangent line
    tangent_x <- seq(full_x_min, full_x_max, length.out = 100)
    tangent_y <- slope * (tangent_x - x_curr) + y_curr
    
    tangent_df <- data.frame(
      x = tangent_x,
      y = tangent_y,
      iteration = iter,
      element = "tangent"
    )
    
    # Zero line for this iteration
    zero_line_iter <- zero_line_df[zero_line_df$iteration == iter, ]
    
    if (iter < nrow(history)) {
      x_next <- history$x[iter + 1]
      y_next <- history$f_x[iter + 1]
      
      # X-intercept point (where tangent crosses x-axis)
      x_intercept_df <- data.frame(
        x = x_next,
        y = 0,
        iteration = iter,
        element = "x_intercept"
      )
      
      # Vertical line from x-intercept up to curve (showing the "move up")
      vert_line_df <- data.frame(
        x = c(x_next, x_next),
        y = c(0, y_next),
        iteration = iter,
        element = "vert_line"
      )
      
      # Next point ON THE CURVE (where current will be next iteration)
      next_on_curve_df <- data.frame(
        x = x_next,
        y = y_next,
        iteration = iter,
        element = "next_on_curve"
      )
      
      frame_data <- rbind(curve_df, zero_line_iter, point_df, tangent_df, 
                          x_intercept_df, vert_line_df, next_on_curve_df)
    } else {
      frame_data <- rbind(curve_df, zero_line_iter, point_df, tangent_df)
    }
    
    anim_data <- rbind(anim_data, frame_data)
  }
  
  # Create plot with legend
  p <- ggplot() +
    geom_line(data = subset(anim_data, element == "curve"),
              aes(x = x, y = y, frame = iteration),
              color = "steelblue", size = 1) +
    geom_line(data = subset(anim_data, element == "zero_line"),
              aes(x = x, y = y, frame = iteration),
              linetype = "dashed", color = "gray50", size = 0.5) +
    geom_line(data = subset(anim_data, element == "tangent"),
              aes(x = x, y = y, frame = iteration, color = "Tangent Line"),
              size = 1, linetype = "dashed") +
    geom_line(data = subset(anim_data, element == "vert_line"),
              aes(x = x, y = y, frame = iteration, color = "Move to Curve"),
              size = 0.8, linetype = "dotted") +
    geom_point(data = subset(anim_data, element == "point"),
               aes(x = x, y = y, frame = iteration, color = "Current Point"),
               size = 2) +
    geom_point(data = subset(anim_data, element == "x_intercept"),
               aes(x = x, y = y, frame = iteration, color = "X-Intercept"),
               size = 2) +
    geom_point(data = subset(anim_data, element == "next_on_curve"),
               aes(x = x, y = y, frame = iteration, color = "Next Point"),
               size = 2.5, shape = 4, stroke = 1.2) +
    scale_color_manual(
      values = c(
        "Current Point" = "darkgreen",
        "Tangent Line" = "orange",
        "X-Intercept" = "red",
        "Move to Curve" = "red",
        "Next Point" = "red"
      )
    ) +
    guides(color = guide_legend(title = NULL)) +
    coord_cartesian(xlim = c(view_x_min, view_x_max), ylim = c(y_min, y_max)) +
    theme_minimal() +
    theme(legend.position = "right") +
    labs(
      title = "Newton-Raphson Method: Tangent Line Approach",
      subtitle = "Tangent intercepts x-axis, then move up to curve",
      x = "x",
      y = "f(x)"
    )
  
  plotly_obj <- ggplotly(p) %>%
    animation_opts(frame = 1000, transition = 500, redraw = FALSE) %>%
    config(modeBarButtonsToRemove = c('select2d', 'lasso2d'))
  
  # Remove ",1" everywhere including inside parentheses
  for (i in seq_along(plotly_obj$x$data)) {
    if (!is.null(plotly_obj$x$data[[i]]$name)) {
      plotly_obj$x$data[[i]]$name <- gsub(",\\s*1\\)", ")", plotly_obj$x$data[[i]]$name)
      plotly_obj$x$data[[i]]$name <- gsub(",\\s*1$", "", plotly_obj$x$data[[i]]$name)
    }
  }
  
  # Add legend title
  plotly_obj <- plotly_obj %>%
    layout(legend = list(title = list(text = "Legend")))
  
  plotly_obj
}


#' Plot Secant Method - Secant Lines Animation
#'
#' @description Shows secant lines connecting consecutive points
#' @param result Result object from secant_method
#' @param f Function being solved
#' @return plotly animation object
#' @export
plot_secant_animation <- function(result, f) {
  
  if (is.null(result$history) || nrow(result$history) == 0) {
    stop("No history available for plotting")
  }
  
  library(ggplot2)
  library(plotly)
  
  history <- result$history
  
  # Determine convergence range
  x_range <- range(history$x)
  x_span <- diff(x_range)
  if (x_span == 0) x_span <- 1
  
  # Create FULL function curve (5x wider)
  full_x_min <- x_range[1] - 5 * x_span
  full_x_max <- x_range[2] + 5 * x_span
  x_grid <- seq(full_x_min, full_x_max, length.out = 1000)
  y_grid <- sapply(x_grid, f)
  
  # VIEWING window
  view_x_padding <- x_span * 0.5
  view_x_min <- x_range[1] - view_x_padding
  view_x_max <- x_range[2] + view_x_padding
  
  # Calculate y-range from viewing window
  x_in_view <- x_grid[x_grid >= view_x_min & x_grid <= view_x_max]
  y_in_view <- sapply(x_in_view, f)
  all_y_values <- c(y_in_view, history$f_x)
  y_finite <- all_y_values[is.finite(all_y_values)]
  
  if (length(y_finite) > 0) {
    y_range <- range(y_finite, na.rm = TRUE)
    y_padding <- diff(y_range) * 0.15
    if (y_padding == 0 || !is.finite(y_padding)) y_padding <- 1
    y_min <- y_range[1] - y_padding
    y_max <- y_range[2] + y_padding
  } else {
    y_min <- -10
    y_max <- 10
  }
  
  # Prepare animation data
  anim_data <- data.frame()
  
  # Create y=0 reference line
  zero_line_df <- data.frame(
    x = c(full_x_min, full_x_max),
    y = c(0, 0),
    iteration = rep(2:nrow(history), each = 2),
    element = "zero_line",
    stringsAsFactors = FALSE
  )
  
  for (iter in 2:nrow(history)) {  # Start at 2 since secant needs 2 points
    # Function curve
    curve_df <- data.frame(
      x = x_grid,
      y = y_grid,
      iteration = iter,
      element = "curve",
      stringsAsFactors = FALSE
    )
    
    # Previous and current points
    x_prev <- history$x[iter - 1]
    y_prev <- history$f_x[iter - 1]
    x_curr <- history$x[iter]
    y_curr <- history$f_x[iter]
    
    # Previous point
    prev_point_df <- data.frame(
      x = x_prev,
      y = y_prev,
      iteration = iter,
      element = "prev_point",
      stringsAsFactors = FALSE
    )
    
    # Current point
    curr_point_df <- data.frame(
      x = x_curr,
      y = y_curr,
      iteration = iter,
      element = "curr_point",
      stringsAsFactors = FALSE
    )
    
    # Secant line through these two points
    if (abs(x_curr - x_prev) > 1e-10) {  # Avoid division by zero
      slope <- (y_curr - y_prev) / (x_curr - x_prev)
      # y - y_curr = slope * (x - x_curr)
      secant_x <- seq(full_x_min, full_x_max, length.out = 100)
      secant_y <- slope * (secant_x - x_curr) + y_curr
      
      secant_df <- data.frame(
        x = secant_x,
        y = secant_y,
        iteration = iter,
        element = "secant",
        stringsAsFactors = FALSE
      )
    } else {
      secant_df <- data.frame()
    }
    
    # Next point (where secant crosses x-axis)
    zero_line_iter <- zero_line_df[zero_line_df$iteration == iter, ]
    
    if (iter < nrow(history)) {
      x_next <- history$x[iter + 1]
      y_next <- history$f_x[iter + 1]
      
      # X-intercept point
      x_intercept_df <- data.frame(
        x = x_next,
        y = 0,
        iteration = iter,
        element = "x_intercept",
        stringsAsFactors = FALSE
      )
      
      # Vertical line from x-intercept up to curve
      vert_line_df <- data.frame(
        x = c(x_next, x_next),
        y = c(0, y_next),
        iteration = iter,
        element = "vert_line",
        stringsAsFactors = FALSE
      )
      
      # Next point ON THE CURVE
      next_on_curve_df <- data.frame(
        x = x_next,
        y = y_next,
        iteration = iter,
        element = "next_on_curve",
        stringsAsFactors = FALSE
      )
      
      if (nrow(secant_df) > 0) {
        frame_data <- rbind(curve_df, zero_line_iter, prev_point_df, curr_point_df, 
                            secant_df, x_intercept_df, vert_line_df, next_on_curve_df)
      } else {
        frame_data <- rbind(curve_df, zero_line_iter, prev_point_df, curr_point_df, 
                            x_intercept_df, vert_line_df, next_on_curve_df)
      }
    } else {
      if (nrow(secant_df) > 0) {
        frame_data <- rbind(curve_df, zero_line_iter, prev_point_df, curr_point_df, secant_df)
      } else {
        frame_data <- rbind(curve_df, zero_line_iter, prev_point_df, curr_point_df)
      }
    }
    
    anim_data <- rbind(anim_data, frame_data)
  }
  
  # Create plot with legend
  p <- ggplot() +
    geom_line(data = subset(anim_data, element == "curve"),
              aes(x = x, y = y, frame = iteration),
              color = "steelblue", size = 1) +
    geom_line(data = subset(anim_data, element == "secant"),
              aes(x = x, y = y, frame = iteration, color = "Secant Line"),
              size = 1, linetype = "dashed") +
    geom_line(data = subset(anim_data, element == "zero_line"),
              aes(x = x, y = y, frame = iteration),
              linetype = "dashed", color = "gray50", size = 0.5) +
    geom_line(data = subset(anim_data, element == "vert_line"),
              aes(x = x, y = y, frame = iteration, color = "Move to Curve"),
              size = 0.8, linetype = "dotted") +
    geom_point(data = subset(anim_data, element == "prev_point"),
               aes(x = x, y = y, frame = iteration, color = "Previous Point"),
               size = 2) +
    geom_point(data = subset(anim_data, element == "curr_point"),
               aes(x = x, y = y, frame = iteration, color = "Current Point"),
               size = 2) +
    geom_point(data = subset(anim_data, element == "x_intercept"),
               aes(x = x, y = y, frame = iteration, color = "X-Intercept"),
               size = 2) +
    geom_point(data = subset(anim_data, element == "next_on_curve"),
               aes(x = x, y = y, frame = iteration, color = "Next Point"),
               size = 2.5, shape = 4, stroke = 1.2) +
    scale_color_manual(
      values = c(
        "Previous Point" = "orange",
        "Current Point" = "darkgreen",
        "Secant Line" = "purple",
        "X-Intercept" = "red",
        "Move to Curve" = "red",
        "Next Point" = "red"
      )
    ) +
    guides(color = guide_legend(title = NULL)) +
    coord_cartesian(xlim = c(view_x_min, view_x_max), ylim = c(y_min, y_max)) +
    theme_minimal() +
    theme(legend.position = "right") +
    labs(
      title = "Secant Method: Secant Line Approach",
      subtitle = "Secant intercepts x-axis, then move up to curve",
      x = "x",
      y = "f(x)"
    )
  
  plotly_obj <- ggplotly(p) %>%
    animation_opts(frame = 1000, transition = 500, redraw = FALSE) %>%
    config(modeBarButtonsToRemove = c('select2d', 'lasso2d'))
  
  # Remove ",1" everywhere including inside parentheses
  for (i in seq_along(plotly_obj$x$data)) {
    if (!is.null(plotly_obj$x$data[[i]]$name)) {
      plotly_obj$x$data[[i]]$name <- gsub(",\\s*1\\)", ")", plotly_obj$x$data[[i]]$name)
      plotly_obj$x$data[[i]]$name <- gsub(",\\s*1$", "", plotly_obj$x$data[[i]]$name)
    }
  }
  
  # Add legend title
  plotly_obj <- plotly_obj %>%
    layout(legend = list(title = list(text = "Legend")))
  
  plotly_obj
}


#' Plot Fixed-Point Iteration - Cobweb Diagram Animation
#'
#' @description Shows cobweb plot with y=g(x) and y=x
#' @param result Result object from fixed_point_iteration
#' @param f Function being solved (will create g(x) = x - f(x))
#' @return plotly animation object
#' @export
plot_fixedpoint_animation <- function(result, f) {
  
  if (is.null(result$history) || nrow(result$history) == 0) {
    stop("No history available for plotting")
  }
  
  library(ggplot2)
  library(plotly)
  
  history <- result$history
  
  # g(x) = x - f(x) by default
  g <- function(x) x - f(x)
  
  # Determine convergence range
  x_range <- range(history$x)
  x_span <- diff(x_range)
  if (x_span == 0) x_span <- 1
  
  # Create FULL function curve (5x wider)
  full_x_min <- x_range[1] - 5 * x_span
  full_x_max <- x_range[2] + 5 * x_span
  x_grid <- seq(full_x_min, full_x_max, length.out = 1000)
  g_grid <- sapply(x_grid, g)
  
  # VIEWING window
  view_x_padding <- x_span * 0.5
  view_x_min <- x_range[1] - view_x_padding
  view_x_max <- x_range[2] + view_x_padding
  
  # Calculate y-range
  all_y_values <- c(g_grid, history$x, sapply(history$x, g))
  y_finite <- all_y_values[is.finite(all_y_values)]
  
  if (length(y_finite) > 0) {
    y_range <- range(y_finite, na.rm = TRUE)
    y_padding <- diff(y_range) * 0.15
    if (y_padding == 0 || !is.finite(y_padding)) y_padding <- 1
    y_min <- y_range[1] - y_padding
    y_max <- y_range[2] + y_padding
  } else {
    y_min <- view_x_min
    y_max <- view_x_max
  }
  
  # Handle 1-iteration convergence case - show static plot
  if (nrow(history) == 1) {
    x_final <- history$x[1]
    
    p <- ggplot() +
      geom_line(aes(x = x_grid, y = g_grid), color = "steelblue", size = 1) +
      geom_line(aes(x = x_grid, y = x_grid), color = "gray50", size = 1, linetype = "dashed") +
      geom_point(aes(x = x_final, y = x_final), color = "red", size = 4) +
      coord_cartesian(xlim = c(view_x_min, view_x_max), ylim = c(y_min, y_max)) +
      theme_minimal() +
      labs(
        title = "Fixed-Point Iteration: Converged in 1 Iteration",
        subtitle = paste("Solution: x =", round(x_final, 6)),
        x = "x",
        y = "y"
      )
    
    return(ggplotly(p) %>% config(modeBarButtonsToRemove = c('select2d', 'lasso2d')))
  }
  
  # Prepare animation data
  anim_data <- data.frame()
  
  # Show only current step's cobweb (no accumulation)
  max_iters <- min(nrow(history) - 1, 20)
  for (iter in 1:max_iters) {
    # g(x) curve
    g_curve_df <- data.frame(
      x = x_grid,
      y = g_grid,
      iteration = iter,
      element = "g_curve",
      stringsAsFactors = FALSE
    )
    
    # y = x line (spans FULL range)
    identity_df <- data.frame(
      x = x_grid,
      y = x_grid,
      iteration = iter,
      element = "identity",
      stringsAsFactors = FALSE
    )
    
    # Current iteration's cobweb ONLY (not accumulated)
    x_curr <- history$x[iter]
    x_next <- history$x[iter + 1]
    g_curr <- g(x_curr)
    
    # Step 1: Current point on y=x line (x_curr, x_curr)
    current_point_df <- data.frame(
      x = x_curr,
      y = x_curr,
      iteration = iter,
      element = "current_point",
      stringsAsFactors = FALSE
    )
    
    # Step 2: Vertical line from (x_curr, x_curr) up to (x_curr, g(x_curr))
    vert_line_df <- data.frame(
      x = c(x_curr, x_curr),
      y = c(x_curr, g_curr),
      iteration = iter,
      element = "vert_line",
      stringsAsFactors = FALSE
    )
    
    # Step 3: Point on g(x) curve at (x_curr, g(x_curr))
    g_point_df <- data.frame(
      x = x_curr,
      y = g_curr,
      iteration = iter,
      element = "g_point",
      stringsAsFactors = FALSE
    )
    
    # Step 4: Horizontal line from (x_curr, g(x_curr)) to (g(x_curr), g(x_curr)) on y=x
    horiz_line_df <- data.frame(
      x = c(x_curr, g_curr),
      y = c(g_curr, g_curr),
      iteration = iter,
      element = "horiz_line",
      stringsAsFactors = FALSE
    )
    
    # Step 5: Next point on y=x line (g(x_curr), g(x_curr)) = (x_next, x_next)
    next_point_df <- data.frame(
      x = g_curr,
      y = g_curr,
      iteration = iter,
      element = "next_point",
      stringsAsFactors = FALSE
    )
    
    frame_data <- rbind(g_curve_df, identity_df, current_point_df, vert_line_df, 
                        g_point_df, horiz_line_df, next_point_df)
    
    anim_data <- rbind(anim_data, frame_data)
  }
  
  # Create plot with legend - show only current step
  p <- ggplot() +
    geom_line(data = subset(anim_data, element == "g_curve"),
              aes(x = x, y = y, frame = iteration, color = "g(x) curve"),
              size = 1) +
    geom_line(data = subset(anim_data, element == "identity"),
              aes(x = x, y = y, frame = iteration, color = "y = x line"),
              size = 1, linetype = "dashed") +
    # Vertical line (current x up to g(x))
    geom_line(data = subset(anim_data, element == "vert_line"),
              aes(x = x, y = y, frame = iteration, color = "Cobweb Step"),
              size = 0.8) +
    # Horizontal line (g(x) back to y=x)
    geom_line(data = subset(anim_data, element == "horiz_line"),
              aes(x = x, y = y, frame = iteration, color = "Cobweb Step"),
              size = 0.8, show.legend = FALSE) +
    # Current point on y=x
    geom_point(data = subset(anim_data, element == "current_point"),
               aes(x = x, y = y, frame = iteration, color = "Current Point"),
               size = 2) +
    # Point on g(x) curve
    geom_point(data = subset(anim_data, element == "g_point"),
               aes(x = x, y = y, frame = iteration, color = "g(x) evaluation"),
               size = 2) +
    # Next point (where we land on y=x)
    geom_point(data = subset(anim_data, element == "next_point"),
               aes(x = x, y = y, frame = iteration, color = "Next Point"),
               size = 2.5, shape = 4, stroke = 1.2) +
    scale_color_manual(
      values = c(
        "g(x) curve" = "steelblue",
        "y = x line" = "gray50",
        "Cobweb Step" = "orange",
        "Current Point" = "darkgreen",
        "g(x) evaluation" = "purple",
        "Next Point" = "red"
      )
    ) +
    guides(color = guide_legend(title = NULL)) +
    coord_cartesian(xlim = c(view_x_min, view_x_max), ylim = c(y_min, y_max)) +
    theme_minimal() +
    theme(legend.position = "right") +
    labs(
      title = "Fixed-Point Iteration: Single Step Cobweb",
      subtitle = "Green = current | Purple = g(x) | Red X = next point",
      x = "x",
      y = "y"
    )
  
  plotly_obj <- ggplotly(p) %>%
    animation_opts(frame = 1000, transition = 500, redraw = FALSE) %>%
    config(modeBarButtonsToRemove = c('select2d', 'lasso2d'))
  
  # Remove ",1" everywhere including inside parentheses
  for (i in seq_along(plotly_obj$x$data)) {
    if (!is.null(plotly_obj$x$data[[i]]$name)) {
      plotly_obj$x$data[[i]]$name <- gsub(",\\s*1\\)", ")", plotly_obj$x$data[[i]]$name)
      plotly_obj$x$data[[i]]$name <- gsub(",\\s*1$", "", plotly_obj$x$data[[i]]$name)
    }
  }
  
  # Add legend title
  plotly_obj <- plotly_obj %>%
    layout(legend = list(title = list(text = "Legend")))
  
  plotly_obj
}


#' Plot Golden Section Search - Interval Reduction Animation
#'
#' @description Shows golden ratio points and interval shrinking
#' @param result Result object from golden_section_search
#' @param f Function being minimized
#' @return plotly animation object
#' @export
plot_golden_animation <- function(result, f) {
  
  if (is.null(result$history) || nrow(result$history) == 0) {
    stop("No history available for plotting")
  }
  
  library(ggplot2)
  library(plotly)
  
  history <- result$history
  
  # Get user's input interval from first iteration
  if ("interval_a" %in% names(history) && "interval_b" %in% names(history)) {
    user_interval_min <- min(history$interval_a[1], history$interval_b[1])
    user_interval_max <- max(history$interval_a[1], history$interval_b[1])
  } else {
    user_interval_min <- min(history$x)
    user_interval_max <- max(history$x)
  }
  
  interval_span <- user_interval_max - user_interval_min
  if (interval_span == 0) interval_span <- 1
  
  # Create FULL function curve (5x wider)
  full_x_min <- user_interval_min - 5 * interval_span
  full_x_max <- user_interval_max + 5 * interval_span
  x_grid <- seq(full_x_min, full_x_max, length.out = 1000)
  y_grid <- sapply(x_grid, f)
  
  # VIEWING window shows USER'S FULL INTERVAL
  view_x_min <- user_interval_min
  view_x_max <- user_interval_max
  
  # Calculate y-range from values in user's interval
  x_in_view <- x_grid[x_grid >= view_x_min & x_grid <= view_x_max]
  y_in_view <- sapply(x_in_view, f)
  all_y_values <- c(y_in_view, history$f_x)
  y_finite <- all_y_values[is.finite(all_y_values)]
  
  if (length(y_finite) > 0) {
    y_range <- range(y_finite, na.rm = TRUE)
    y_padding <- diff(y_range) * 0.15
    if (y_padding == 0 || !is.finite(y_padding)) y_padding <- 1
    y_min <- y_range[1] - y_padding
    y_max <- y_range[2] + y_padding
  } else {
    y_min <- -10
    y_max <- 10
  }
  
  # Prepare animation data
  anim_data <- data.frame()
  
  # Get unique iterations - start from 1 (skip iteration 0)
  unique_iters <- unique(history$iteration)
  unique_iters <- unique_iters[unique_iters >= 1]
  
  # Check if we have current_x stored
  has_current <- "current_x" %in% names(history)
  
  for (iter_num in unique_iters) {
    # Get all rows for this iteration
    iter_rows <- history[history$iteration == iter_num, ]
    
    # Function curve
    curve_df <- data.frame(
      x = x_grid,
      y = y_grid,
      iteration = iter_num,
      element = "curve",
      stringsAsFactors = FALSE
    )
    
    # Get ACTUAL interval bounds
    if ("interval_a" %in% names(history) && "interval_b" %in% names(history)) {
      interval_a <- iter_rows$interval_a[1]
      interval_b <- iter_rows$interval_b[1]
    } else {
      interval_a <- user_interval_min
      interval_b <- user_interval_max
    }
    
    # Interval shading
    interval_shade <- data.frame(
      x = c(interval_a, interval_b, interval_b, interval_a),
      y = c(y_min, y_min, y_max, y_max),
      iteration = iter_num,
      element = "interval",
      stringsAsFactors = FALSE
    )
    
    # Interval boundary markers (vertical lines)
    left_bound <- data.frame(
      x = c(interval_a, interval_a),
      y = c(y_min, y_max),
      iteration = iter_num,
      element = "left_bound",
      stringsAsFactors = FALSE
    )
    
    right_bound <- data.frame(
      x = c(interval_b, interval_b),
      y = c(y_min, y_max),
      iteration = iter_num,
      element = "right_bound",
      stringsAsFactors = FALSE
    )
    
    # Trial point (the new x being evaluated this iteration)
    trial_x <- iter_rows$x[1]
    trial_fx <- iter_rows$f_x[1]
    
    # Current point = the point we had BEFORE this iteration (b0 in the algorithm)
    if (has_current) {
      current_x <- iter_rows$current_x[1]
      current_fx <- iter_rows$current_fx[1]
    } else {
      # Fallback: get from previous iteration
      prev_iter <- iter_num - 1
      prev_rows <- history[history$iteration == prev_iter, ]
      if (nrow(prev_rows) > 0) {
        current_x <- prev_rows$x[1]
        current_fx <- prev_rows$f_x[1]
      } else {
        current_x <- trial_x
        current_fx <- trial_fx
      }
    }
    
    # Current point (green dot) - the point BEFORE this iteration
    current_df <- data.frame(
      x = current_x,
      y = current_fx,
      iteration = iter_num,
      element = "current_point",
      stringsAsFactors = FALSE
    )
    
    # Trial point (red X) - the new point being evaluated
    trial_df <- data.frame(
      x = trial_x,
      y = trial_fx,
      iteration = iter_num,
      element = "trial_point",
      stringsAsFactors = FALSE
    )
    
    frame_data <- rbind(curve_df, interval_shade, left_bound, right_bound, current_df, trial_df)
    anim_data <- rbind(anim_data, frame_data)
  }
  
  # Create plot with legend
  p <- ggplot() +
    geom_polygon(data = subset(anim_data, element == "interval"),
                 aes(x = x, y = y, frame = iteration, fill = "Search Interval"),
                 alpha = 0.2) +
    geom_line(data = subset(anim_data, element == "curve"),
              aes(x = x, y = y, frame = iteration),
              color = "steelblue", size = 1) +
    geom_line(data = subset(anim_data, element == "left_bound"),
              aes(x = x, y = y, frame = iteration, color = "Interval Bounds"),
              size = 1.5, linetype = "solid") +
    geom_line(data = subset(anim_data, element == "right_bound"),
              aes(x = x, y = y, frame = iteration, color = "Interval Bounds"),
              size = 1.5, linetype = "solid", show.legend = FALSE) +
    geom_point(data = subset(anim_data, element == "current_point"),
               aes(x = x, y = y, frame = iteration, color = "Current Point"),
               size = 2) +
    geom_point(data = subset(anim_data, element == "trial_point"),
               aes(x = x, y = y, frame = iteration, color = "Trial Point"),
               size = 2.5, shape = 4, stroke = 1.2) +
    scale_color_manual(
      values = c(
        "Current Point" = "darkgreen",
        "Trial Point" = "red",
        "Interval Bounds" = "darkgray"
      )
    ) +
    scale_fill_manual(
      values = c("Search Interval" = "yellow"),
      guide = "none"
    ) +
    guides(color = guide_legend(title = NULL)) +
    coord_cartesian(xlim = c(view_x_min, view_x_max), ylim = c(y_min, y_max)) +
    theme_minimal() +
    theme(legend.position = "right") +
    labs(
      title = "Golden Section Search: Interval Reduction",
      subtitle = "Compare green (current) vs red X (trial) to see which becomes new current",
      x = "x",
      y = "f(x)"
    )
  
  plotly_obj <- ggplotly(p) %>%
    animation_opts(frame = 1000, transition = 500, redraw = FALSE) %>%
    config(modeBarButtonsToRemove = c('select2d', 'lasso2d'))
  
  # Fix legend: remove ",1" everywhere including inside parentheses
  for (i in seq_along(plotly_obj$x$data)) {
    trace <- plotly_obj$x$data[[i]]
    if (!is.null(trace$name)) {
      plotly_obj$x$data[[i]]$name <- gsub(",\\s*1\\)", ")", trace$name)
      plotly_obj$x$data[[i]]$name <- gsub(",\\s*1$", "", plotly_obj$x$data[[i]]$name)
      
      # Hide duplicate "Interval Bounds"
      if (grepl("Interval Bounds", plotly_obj$x$data[[i]]$name) && i > 1) {
        for (j in 1:(i-1)) {
          if (!is.null(plotly_obj$x$data[[j]]$name) && 
              grepl("Interval Bounds", plotly_obj$x$data[[j]]$name)) {
            plotly_obj$x$data[[i]]$showlegend <- FALSE
            break
          }
        }
      }
    }
  }
  
  # Add legend title
  plotly_obj <- plotly_obj %>%
    layout(legend = list(title = list(text = "Legend")))
  
  plotly_obj
}


#' Plot Parabolic Interpolation - Parabola Fitting Animation
#'
#' @description Shows parabola fitted through points
#' @param result Result object from parabolic_interpolation
#' @param f Function being minimized
#' @return plotly animation object
#' @export
plot_parabolic_animation <- function(result, f) {
  
  if (is.null(result$history) || nrow(result$history) == 0) {
    stop("No history available for plotting")
  }
  
  library(ggplot2)
  library(plotly)
  
  history <- result$history
  
  # Get user's input interval from first iteration (if available)
  if ("interval_a" %in% names(history) && "interval_b" %in% names(history)) {
    user_interval_min <- min(history$interval_a[1], history$interval_b[1])
    user_interval_max <- max(history$interval_a[1], history$interval_b[1])
  } else {
    # Fallback: use convergence range
    user_interval_min <- min(history$x)
    user_interval_max <- max(history$x)
  }
  
  interval_span <- user_interval_max - user_interval_min
  if (interval_span == 0) interval_span <- 1
  
  # Create FULL function curve (5x wider like all other algorithms)
  full_x_min <- user_interval_min - 5 * interval_span
  full_x_max <- user_interval_max + 5 * interval_span
  x_grid <- seq(full_x_min, full_x_max, length.out = 1000)
  y_grid <- sapply(x_grid, f)
  
  # VIEWING window shows USER'S FULL INTERVAL
  view_x_min <- user_interval_min
  view_x_max <- user_interval_max
  
  # Calculate y-range from values in user's interval
  x_in_view <- x_grid[x_grid >= view_x_min & x_grid <= view_x_max]
  y_in_view <- sapply(x_in_view, f)
  all_y_values <- c(y_in_view, history$f_x)
  y_finite <- all_y_values[is.finite(all_y_values)]
  
  if (length(y_finite) > 0) {
    y_range <- range(y_finite, na.rm = TRUE)
    y_padding <- diff(y_range) * 0.15
    if (y_padding == 0 || !is.finite(y_padding)) y_padding <- 1
    y_min <- y_range[1] - y_padding
    y_max <- y_range[2] + y_padding
  } else {
    y_min <- -10
    y_max <- 10
  }
  
  # Prepare animation data
  anim_data <- data.frame()
  
  # Check if iteration column exists
  if (!"iteration" %in% names(history)) {
    history$iteration <- rep(0:(ceiling(nrow(history)/3)-1), each = 3, length.out = nrow(history))
  }
  
  unique_iters <- unique(history$iteration)
  
  # Check if we have trial point stored in history
  has_trial <- "x_trial" %in% names(history)
  
  # Start from iteration 0 (initial bracket)
  for (iter_idx in seq_along(unique_iters)) {
    iter_num <- unique_iters[iter_idx]
    
    # Get this iteration's bracket
    iter_rows <- history[history$iteration == iter_num, ]
    
    if (nrow(iter_rows) < 3) {
      next
    }
    
    # Sort bracket points by x position: left, middle, right
    sorted_idx <- order(iter_rows$x)
    x1 <- iter_rows$x[sorted_idx[1]]  # left
    x2 <- iter_rows$x[sorted_idx[2]]  # middle  
    x3 <- iter_rows$x[sorted_idx[3]]  # right
    y1 <- iter_rows$f_x[sorted_idx[1]]
    y2 <- iter_rows$f_x[sorted_idx[2]]
    y3 <- iter_rows$f_x[sorted_idx[3]]
    
    # Function curve
    curve_df <- data.frame(
      x = x_grid,
      y = y_grid,
      iteration = iter_idx,
      element = "curve",
      stringsAsFactors = FALSE
    )
    
    # Interval shading
    interval_shade <- data.frame(
      x = c(x1, x3, x3, x1),
      y = c(y_min, y_min, y_max, y_max),
      iteration = iter_idx,
      element = "interval",
      stringsAsFactors = FALSE
    )
    
    # Interval bounds
    left_bound <- data.frame(
      x = c(x1, x1),
      y = c(y_min, y_max),
      iteration = iter_idx,
      element = "left_bound",
      stringsAsFactors = FALSE
    )
    
    right_bound <- data.frame(
      x = c(x3, x3),
      y = c(y_min, y_max),
      iteration = iter_idx,
      element = "right_bound",
      stringsAsFactors = FALSE
    )
    
    # 3 bracket points
    points_df <- data.frame(
      x = c(x1, x2, x3),
      y = c(y1, y2, y3),
      iteration = iter_idx,
      element = "bracket_point",
      stringsAsFactors = FALSE
    )
    
    # Fit parabola through these 3 bracket points for DISPLAY
    A <- matrix(c(x1^2, x1, 1,
                  x2^2, x2, 1,
                  x3^2, x3, 1), nrow = 3, byrow = TRUE)
    B <- c(y1, y2, y3)
    
    coeffs <- tryCatch(solve(A, B), error = function(e) NULL)
    
    frame_data <- rbind(curve_df, interval_shade, left_bound, right_bound, points_df)
    
    if (!is.null(coeffs) && all(is.finite(coeffs))) {
      a_coef <- coeffs[1]
      b_coef <- coeffs[2]
      c_coef <- coeffs[3]
      
      # Parabola curve for display
      x_parabola_min <- view_x_min - 2 * interval_span
      x_parabola_max <- view_x_max + 2 * interval_span
      x_parabola <- seq(x_parabola_min, x_parabola_max, length.out = 300)
      y_parabola <- a_coef * x_parabola^2 + b_coef * x_parabola + c_coef
      
      parabola_df <- data.frame(
        x = x_parabola,
        y = y_parabola,
        iteration = iter_idx,
        element = "parabola",
        stringsAsFactors = FALSE
      )
      
      frame_data <- rbind(frame_data, parabola_df)
    }
    
    # Get trial point - use stored value if available, otherwise calculate
    # Look at NEXT iteration to get the trial point that was computed FROM this bracket
    if (iter_idx < length(unique_iters)) {
      next_iter_num <- unique_iters[iter_idx + 1]
      next_rows <- history[history$iteration == next_iter_num, ]
      
      if (has_trial && !is.na(next_rows$x_trial[1])) {
        # Use the stored trial point
        x_trial <- next_rows$x_trial[1]
        y_trial <- next_rows$f_trial[1]
        
        if (x_trial > x1 && x_trial < x3) {
          trial_df <- data.frame(
            x = x_trial,
            y = y_trial,
            iteration = iter_idx,
            element = "trial_point",
            stringsAsFactors = FALSE
          )
          frame_data <- rbind(frame_data, trial_df)
        }
      } else {
        # Calculate trial point using same formula as algorithm
        numerator <- (x1 - x2)^2 * (y2 - y3) - (x1 - x3)^2 * (y2 - y1)
        denominator <- 2 * ((x1 - x2) * (y1 - y3) - (x1 - x3) * (y1 - y2))
        
        if (abs(denominator) > 1e-12) {
          x_trial <- x2 - 0.5 * numerator / denominator
          
          if (x_trial > x1 && x_trial < x3) {
            y_trial <- f(x_trial)
            
            trial_df <- data.frame(
              x = x_trial,
              y = y_trial,
              iteration = iter_idx,
              element = "trial_point",
              stringsAsFactors = FALSE
            )
            frame_data <- rbind(frame_data, trial_df)
          }
        }
      }
    }
    
    anim_data <- rbind(anim_data, frame_data)
  }
  
  # Create plot
  p <- ggplot(anim_data) +
    geom_polygon(data = subset(anim_data, element == "interval"),
                 aes(x = x, y = y, frame = iteration, fill = "Bracket"),
                 alpha = 0.2) +
    geom_line(data = subset(anim_data, element == "curve"),
              aes(x = x, y = y, frame = iteration),
              color = "steelblue", size = 1) +
    geom_line(data = subset(anim_data, element == "parabola"),
              aes(x = x, y = y, frame = iteration, colour = "Fitted Parabola"),
              size = 0.8, linetype = "dashed") +
    geom_line(data = subset(anim_data, element == "left_bound"),
              aes(x = x, y = y, frame = iteration, colour = "Interval Bounds"),
              size = 1.5) +
    geom_line(data = subset(anim_data, element == "right_bound"),
              aes(x = x, y = y, frame = iteration, colour = "Interval Bounds"),
              size = 1.5, show.legend = FALSE) +
    geom_point(data = subset(anim_data, element == "bracket_point"),
               aes(x = x, y = y, frame = iteration, colour = "Bracket Points"),
               size = 2) +
    geom_point(data = subset(anim_data, element == "trial_point"),
               aes(x = x, y = y, frame = iteration, colour = "Trial Point"),
               size = 2.5, shape = 4, stroke = 1.2) +
    scale_colour_manual(
      values = c(
        "Bracket Points" = "black",
        "Fitted Parabola" = "purple",
        "Trial Point" = "red",
        "Interval Bounds" = "darkgray"
      )
    ) +
    scale_fill_manual(
      values = c("Bracket" = "lightyellow"),
      guide = "none"
    ) +
    guides(colour = guide_legend(title = NULL)) +
    coord_cartesian(xlim = c(view_x_min, view_x_max), ylim = c(y_min, y_max)) +
    theme_minimal() +
    theme(legend.position = "right") +
    labs(
      title = "Parabolic Interpolation",
      subtitle = "Black = bracket (x1 < x2 < x3) | Purple = fitted parabola | Red X = trial point",
      x = "x",
      y = "f(x)"
    )
  
  plotly_obj <- ggplotly(p) %>%
    animation_opts(frame = 1000, transition = 500, redraw = FALSE) %>%
    config(modeBarButtonsToRemove = c('select2d', 'lasso2d'))
  
  # Fix legend
  for (i in seq_along(plotly_obj$x$data)) {
    trace <- plotly_obj$x$data[[i]]
    if (!is.null(trace$name)) {
      plotly_obj$x$data[[i]]$name <- gsub(",\\s*1\\)", ")", trace$name)
      plotly_obj$x$data[[i]]$name <- gsub(",\\s*1$", "", plotly_obj$x$data[[i]]$name)
      
      if (grepl("Interval Bounds", plotly_obj$x$data[[i]]$name) && i > 1) {
        for (j in 1:(i-1)) {
          if (!is.null(plotly_obj$x$data[[j]]$name) && 
              grepl("Interval Bounds", plotly_obj$x$data[[j]]$name)) {
            plotly_obj$x$data[[i]]$showlegend <- FALSE
            break
          }
        }
      }
    }
  }
  
  plotly_obj %>%
    layout(legend = list(title = list(text = "Legend")))
}

#' Plot Brent's Method - Hybrid Approach Animation
#'
#' @description Shows Brent's combination of methods
#' @param result Result object from brent_method
#' @param f Function being minimized
#' @return plotly animation object
#' @export
plot_brent_animation <- function(result, f) {
  
  if (is.null(result$history) || nrow(result$history) == 0) {
    stop("No history available for plotting")
  }
  
  library(ggplot2)
  library(plotly)
  
  history <- result$history
  
  # Get user's input interval from first iteration
  if ("interval_a" %in% names(history) && "interval_b" %in% names(history)) {
    user_interval_min <- min(history$interval_a[1], history$interval_b[1])
    user_interval_max <- max(history$interval_a[1], history$interval_b[1])
  } else {
    user_interval_min <- min(history$x)
    user_interval_max <- max(history$x)
  }
  
  interval_span <- user_interval_max - user_interval_min
  if (interval_span == 0) interval_span <- 1
  
  # Create FULL function curve (5x wider)
  full_x_min <- user_interval_min - 5 * interval_span
  full_x_max <- user_interval_max + 5 * interval_span
  x_grid <- seq(full_x_min, full_x_max, length.out = 1000)
  y_grid <- sapply(x_grid, f)
  
  # VIEWING window shows USER'S FULL INTERVAL
  view_x_min <- user_interval_min
  view_x_max <- user_interval_max
  
  # Calculate y-range from values in user's interval
  x_in_view <- x_grid[x_grid >= view_x_min & x_grid <= view_x_max]
  y_in_view <- sapply(x_in_view, f)
  all_y_values <- c(y_in_view, history$f_x)
  y_finite <- all_y_values[is.finite(all_y_values)]
  
  if (length(y_finite) > 0) {
    y_range <- range(y_finite, na.rm = TRUE)
    y_padding <- diff(y_range) * 0.15
    if (y_padding == 0 || !is.finite(y_padding)) y_padding <- 1
    y_min <- y_range[1] - y_padding
    y_max <- y_range[2] + y_padding
  } else {
    y_min <- -10
    y_max <- 10
  }
  
  # Prepare animation data
  anim_data <- data.frame()
  
  # Check if we have step_type info
  has_step_type <- "step_type" %in% names(history)
  has_interval <- "interval_a" %in% names(history) && "interval_b" %in% names(history)
  has_wv <- "w" %in% names(history) && "v" %in% names(history)
  
  # Start from iteration 1 (skip initial)
  unique_iters <- unique(history$iteration)
  unique_iters <- unique_iters[unique_iters >= 1]
  
  # Track best minimum
  best_x <- history$x[1]
  best_fx <- history$f_x[1]
  
  for (iter_num in unique_iters) {
    iter_row <- history[history$iteration == iter_num, ]
    
    # Function curve
    curve_df <- data.frame(
      x = x_grid,
      y = y_grid,
      iteration = iter_num,
      element = "curve",
      stringsAsFactors = FALSE
    )
    
    # Get interval bounds
    if (has_interval) {
      interval_a <- iter_row$interval_a[1]
      interval_b <- iter_row$interval_b[1]
    } else {
      interval_a <- user_interval_min
      interval_b <- user_interval_max
    }
    
    # Get step type
    if (has_step_type) {
      step_type <- iter_row$step_type[1]
    } else {
      step_type <- "unknown"
    }
    
    # Determine interval shading element based on step type
    if (step_type == "golden") {
      shade_element <- "interval_golden"
    } else if (step_type == "parabolic") {
      shade_element <- "interval_parabolic"
    } else {
      shade_element <- "interval_golden"  # Default
    }
    
    # Interval shading
    interval_shade <- data.frame(
      x = c(interval_a, interval_b, interval_b, interval_a),
      y = c(y_min, y_min, y_max, y_max),
      iteration = iter_num,
      element = shade_element,
      stringsAsFactors = FALSE
    )
    
    # Interval boundary markers (vertical lines)
    left_bound <- data.frame(
      x = c(interval_a, interval_a),
      y = c(y_min, y_max),
      iteration = iter_num,
      element = "left_bound",
      stringsAsFactors = FALSE
    )
    
    right_bound <- data.frame(
      x = c(interval_b, interval_b),
      y = c(y_min, y_max),
      iteration = iter_num,
      element = "right_bound",
      stringsAsFactors = FALSE
    )
    
    # Trial point = the new x being evaluated THIS iteration
    trial_x <- iter_row$x[1]
    trial_fx <- iter_row$f_x[1]
    
    # Current point = x_best (the best point BEFORE this iteration)
    has_x_best <- "x_best" %in% names(history)
    if (has_x_best) {
      current_x <- iter_row$x_best[1]
      current_fx <- iter_row$fx_best[1]
    } else {
      # Fallback
      current_x <- best_x
      current_fx <- best_fx
    }
    
    # Update best for fallback tracking
    if (trial_fx < best_fx) {
      best_fx <- trial_fx
      best_x <- trial_x
    }
    
    # Current point (green dot) - the best point BEFORE this iteration
    current_df <- data.frame(
      x = current_x,
      y = current_fx,
      iteration = iter_num,
      element = "best_point",
      stringsAsFactors = FALSE
    )
    
    # Trial point marker (red X) - the point being compared this iteration
    trial_df <- data.frame(
      x = trial_x,
      y = trial_fx,
      iteration = iter_num,
      element = "trial_point",
      stringsAsFactors = FALSE
    )
    
    frame_data <- rbind(curve_df, interval_shade, left_bound, right_bound, current_df, trial_df)
    
    # If parabolic step and we have w, v points, draw the fitted parabola
    # Brent uses (x_best, w, v) for parabolic interpolation
    if (step_type == "parabolic" && has_wv) {
      
      # Check if we have x_best stored (the current best that was used for parabolic fit)
      has_x_best <- "x_best" %in% names(history)
      
      if (has_x_best) {
        # Use the stored x_best (the best point used for this parabolic step)
        x1 <- iter_row$x_best[1]
        y1 <- iter_row$fx_best[1]
      } else {
        # Fallback: get x from previous iteration
        prev_iter <- iter_num - 1
        prev_rows <- history[history$iteration == prev_iter, ]
        if (nrow(prev_rows) > 0) {
          x1 <- prev_rows$x[1]
          y1 <- prev_rows$f_x[1]
        } else {
          x1 <- best_x
          y1 <- best_fx
        }
      }
      
      w_val <- iter_row$w[1]
      v_val <- iter_row$v[1]
      fw_val <- iter_row$fw[1]
      fv_val <- iter_row$fv[1]
      
      # The 3 points used for parabolic interpolation in Brent: (x_best, w, v)
      x2 <- w_val
      y2 <- fw_val
      x3 <- v_val
      y3 <- fv_val
      
      # Only fit if points are distinct
      if (length(unique(c(x1, x2, x3))) == 3) {
        # Fit parabola through 3 points for DISPLAY
        A <- matrix(c(x1^2, x1, 1,
                      x2^2, x2, 1,
                      x3^2, x3, 1), nrow = 3, byrow = TRUE)
        B <- c(y1, y2, y3)
        
        coeffs <- tryCatch(solve(A, B), error = function(e) NULL)
        
        if (!is.null(coeffs) && all(is.finite(coeffs))) {
          a_coef <- coeffs[1]
          b_coef <- coeffs[2]
          c_coef <- coeffs[3]
          
          # Extended parabola for display
          x_parabola_min <- view_x_min - 2 * interval_span
          x_parabola_max <- view_x_max + 2 * interval_span
          x_parabola <- seq(x_parabola_min, x_parabola_max, length.out = 300)
          y_parabola <- a_coef * x_parabola^2 + b_coef * x_parabola + c_coef
          
          parabola_df <- data.frame(
            x = x_parabola,
            y = y_parabola,
            iteration = iter_num,
            element = "parabola",
            stringsAsFactors = FALSE
          )
          
          # The 3 points used for fitting (show them as bracket points)
          fit_points_df <- data.frame(
            x = c(x1, x2, x3),
            y = c(y1, y2, y3),
            iteration = iter_num,
            element = "bracket_points",
            stringsAsFactors = FALSE
          )
          
          frame_data <- rbind(frame_data, parabola_df, fit_points_df)
        }
      }
    }
    
    # For golden steps or when parabola couldn't be fit, add NA placeholders
    if (!("parabola" %in% frame_data$element)) {
      parabola_df <- data.frame(
        x = NA_real_,
        y = NA_real_,
        iteration = iter_num,
        element = "parabola",
        stringsAsFactors = FALSE
      )
      
      fit_points_df <- data.frame(
        x = NA_real_,
        y = NA_real_,
        iteration = iter_num,
        element = "bracket_points",
        stringsAsFactors = FALSE
      )
      
      frame_data <- rbind(frame_data, parabola_df, fit_points_df)
    }
    
    anim_data <- rbind(anim_data, frame_data)
  }
  
  # Create plot with legend
  p <- ggplot() +
    # Golden section interval (yellow)
    geom_polygon(data = subset(anim_data, element == "interval_golden"),
                 aes(x = x, y = y, frame = iteration, group = iteration),
                 fill = "gold", alpha = 0.2) +
    # Parabolic interval (light purple)
    geom_polygon(data = subset(anim_data, element == "interval_parabolic"),
                 aes(x = x, y = y, frame = iteration, group = iteration),
                 fill = "mediumpurple", alpha = 0.2) +
    # Function curve
    geom_line(data = subset(anim_data, element == "curve"),
              aes(x = x, y = y, frame = iteration, group = iteration),
              color = "steelblue", size = 1) +
    # Fitted parabola (when parabolic step)
    geom_line(data = subset(anim_data, element == "parabola"),
              aes(x = x, y = y, frame = iteration, group = iteration, color = "Fitted Parabola"),
              size = 0.8, linetype = "dashed", na.rm = TRUE) +
    # Interval bounds
    geom_line(data = subset(anim_data, element == "left_bound"),
              aes(x = x, y = y, frame = iteration, group = iteration, color = "Interval Bounds"),
              size = 1.5, linetype = "solid") +
    geom_line(data = subset(anim_data, element == "right_bound"),
              aes(x = x, y = y, frame = iteration, group = iteration, color = "Interval Bounds"),
              size = 1.5, linetype = "solid", show.legend = FALSE) +
    # Bracket points used for parabola fitting
    geom_point(data = subset(anim_data, element == "bracket_points"),
               aes(x = x, y = y, frame = iteration, color = "Bracket Points"),
               size = 2, na.rm = TRUE) +
    # Current point being compared against
    geom_point(data = subset(anim_data, element == "best_point"),
               aes(x = x, y = y, frame = iteration, color = "Current Point"),
               size = 2) +
    # Trial point (X marker) - the point being compared
    geom_point(data = subset(anim_data, element == "trial_point"),
               aes(x = x, y = y, frame = iteration, color = "Trial Point"),
               size = 2.5, shape = 4, stroke = 1.2) +
    scale_color_manual(
      values = c(
        "Current Point" = "darkgreen",
        "Trial Point" = "red",
        "Fitted Parabola" = "purple",
        "Bracket Points" = "black",
        "Interval Bounds" = "darkgray"
      ),
      drop = FALSE
    ) +
    guides(color = guide_legend(title = NULL)) +
    coord_cartesian(xlim = c(view_x_min, view_x_max), ylim = c(y_min, y_max)) +
    theme_minimal() +
    theme(legend.position = "right") +
    labs(
      title = "Brent's Method: Hybrid Optimization",
      subtitle = "Yellow = Golden | Purple = Parabolic | Compare green vs red X",
      x = "x",
      y = "f(x)"
    )
  
  plotly_obj <- ggplotly(p) %>%
    animation_opts(frame = 1000, transition = 500, redraw = FALSE) %>%
    config(modeBarButtonsToRemove = c('select2d', 'lasso2d'))
  
  # Fix legend: remove ",1" everywhere including inside parentheses
  for (i in seq_along(plotly_obj$x$data)) {
    trace <- plotly_obj$x$data[[i]]
    if (!is.null(trace$name)) {
      plotly_obj$x$data[[i]]$name <- gsub(",\\s*1\\)", ")", trace$name)
      plotly_obj$x$data[[i]]$name <- gsub(",\\s*1$", "", plotly_obj$x$data[[i]]$name)
      
      # Hide duplicate entries
      if (grepl("Interval Bounds", plotly_obj$x$data[[i]]$name) && i > 1) {
        for (j in 1:(i-1)) {
          if (!is.null(plotly_obj$x$data[[j]]$name) && 
              grepl("Interval Bounds", plotly_obj$x$data[[j]]$name)) {
            plotly_obj$x$data[[i]]$showlegend <- FALSE
            break
          }
        }
      }
    }
  }
  
  plotly_obj <- plotly_obj %>%
    layout(legend = list(title = list(text = "Legend")))
  
  plotly_obj
}


#' Dispatch Function - Create Algorithm-Specific Animation
#'
#' @description Automatically creates appropriate animation based on algorithm
#' @param result Result object from any algorithm
#' @param f Original function
#' @param df Derivative function (for Newton, optional)
#' @return plotly animation object
#' @export
plot_algorithm_animation <- function(result, f, df = NULL) {
  
  algorithm <- result$algorithm
  
  if (grepl("Bisection", algorithm)) {
    return(plot_bisection_animation(result, f))
  } else if (grepl("Newton", algorithm)) {
    return(plot_newton_animation(result, f, df))
  } else if (grepl("Secant", algorithm)) {
    return(plot_secant_animation(result, f))
  } else if (grepl("Fixed", algorithm)) {
    return(plot_fixedpoint_animation(result, f))
  } else if (grepl("Golden", algorithm)) {
    return(plot_golden_animation(result, f))
  } else if (grepl("Parabolic", algorithm)) {
    return(plot_parabolic_animation(result, f))
  } else if (grepl("Brent", algorithm)) {
    return(plot_brent_animation(result, f))
  } else {
    stop(paste("No animation defined for algorithm:", algorithm))
  }
}
