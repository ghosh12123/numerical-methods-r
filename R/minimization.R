#' Minimization Algorithms
#'
#' @description Collection of optimization algorithms for minimizing f(x)
#'
#' @name minimization
NULL

#' Golden Section Search
#'
#' @description Minimize function using golden section search. Robust interval reduction method.
#'
#' @param f Function to minimize
#' @param interval Numeric vector of length 2: c(a, b)
#' @param tol Tolerance for convergence (default: 1e-6)
#' @param max_iter Maximum iterations (default: 100)
#'
#' @return List containing convergence results and iteration history
#'
#' @export
golden_section_search <- function(f, interval, tol = 1e-6, max_iter = 100) {
  
  # Golden ratio constant
  gold <- 0.38196
  
  # Initialize bounds
  a0 <- min(interval[1], interval[2])
  c0 <- max(interval[1], interval[2])
  b0 <- (a0 + c0) * 0.5
  
  # Initialize history
  history <- data.frame(
    iteration = integer(),
    x = numeric(),
    f_x = numeric(),
    interval_width = numeric(),
    interval_a = numeric(),
    interval_b = numeric(),
    current_x = numeric(),
    current_fx = numeric()
  )
  
  # Evaluate initial point
  fb <- f(b0)
  iter <- 0
  convergence <- 1
  
  # Record initial state
  history <- rbind(history, data.frame(
    iteration = 0,
    x = b0,
    f_x = fb,
    interval_width = c0 - a0,
    interval_a = a0,
    interval_b = c0,
    current_x = b0,
    current_fx = fb
  ))
  
  # Main iteration loop
  while (iter < max_iter) {
    # Compute trial point using golden ratio
    mid <- (a0 + c0) * 0.5
    if (b0 > mid) {
      x <- b0 + gold * (a0 - b0)
    } else {
      x <- b0 + gold * (c0 - b0)
    }
    
    fx <- f(x)
    
    # Record history before updating
    iter <- iter + 1
    history <- rbind(history, data.frame(
      iteration = iter,
      x = x,
      f_x = fx,
      interval_width = c0 - a0,
      interval_a = a0,
      interval_b = c0,
      current_x = b0,
      current_fx = fb
    ))
    
    # Update bracket based on function comparison
    if (fx < fb) {
      if (x > b0) {
        a0 <- b0
      } else {
        c0 <- b0
      }
      b0 <- x
      fb <- fx
    } else {
      if (x < b0) {
        a0 <- x
      } else {
        c0 <- x
      }
    }
    
    # Check convergence
    if (abs(c0 - a0) < abs(b0) * tol) {
      convergence <- 0
      break
    }
  }
  
  # Return results
  list(
    algorithm = "Golden Section Search",
    x_final = b0,
    f_final = fb,
    iterations = iter,
    converged = (convergence == 0),
    message = if(convergence == 0) "Converged" else "Maximum iterations reached",
    history = history,
    convergence_rate = "Linear"
  )
}


#' Parabolic Interpolation
#'
#' @description Minimize function by fitting parabola through three points.
#'
#' @param f Function to minimize
#' @param interval Numeric vector of length 2: c(a, b)
#' @param tol Tolerance for convergence (default: 1e-6)
#' @param max_iter Maximum iterations (default: 100)
#'
#' @return List containing convergence results and iteration history
#'
#' @export
parabolic_interpolation <- function(f, interval, tol = 1e-6, max_iter = 100) {
  
  # Golden ratio for fallback
  phi <- (1 + sqrt(5)) / 2
  resphi <- 2 - phi
  
  # Initialize bounds
  a <- min(interval[1], interval[2])
  b <- max(interval[1], interval[2])
  
  # Initialize three bracket points: left, middle, right
  x1 <- a
  x2 <- (a + b) / 2
  x3 <- b
  
  f1 <- f(x1)
  f2 <- f(x2)
  f3 <- f(x3)
  
  # Initialize history
  history <- data.frame(
    iteration = c(0, 0, 0),
    x = c(x1, x2, x3),
    f_x = c(f1, f2, f3),
    point_type = c("left", "middle", "right"),
    x_trial = c(NA, NA, NA),
    f_trial = c(NA, NA, NA)
  )
  
  # Main iteration loop
  for (iter in 1:max_iter) {
    
    # Evaluate functions at bracket points
    f1 <- f(x1)
    f2 <- f(x2)
    f3 <- f(x3)
    
    # Compute parabola minimum - standard formula
    num <- (x2 - x1)^2 * (f2 - f3) - (x2 - x3)^2 * (f2 - f1)
    den <- (x2 - x1) * (f2 - f3) - (x2 - x3) * (f2 - f1)
    
    # Compute trial point
    if (abs(den) < 1e-12) {
      # Fallback to golden section if parabola is degenerate
      if ((x3 - x2) > (x2 - x1)) {
        x_new <- x2 + resphi * (x3 - x2)
      } else {
        x_new <- x2 - resphi * (x2 - x1)
      }
    } else {
      x_new <- x2 - 0.5 * num / den
      
      # Fallback if trial point is outside bracket
      if (x_new <= x1 || x_new >= x3) {
        if ((x3 - x2) > (x2 - x1)) {
          x_new <- x2 + resphi * (x3 - x2)
        } else {
          x_new <- x2 - resphi * (x2 - x1)
        }
      }
    }
    
    f_new <- f(x_new)
    
    # Check convergence - interval width
    if ((x3 - x1) < tol) {
      x_final <- x2
      f_final <- f2
      
      return(list(
        algorithm = "Parabolic Interpolation",
        x_final = x_final,
        f_final = f_final,
        iterations = iter,
        converged = TRUE,
        message = "Converged",
        history = history,
        convergence_rate = "Superlinear"
      ))
    }
    
    # Update bracket using 4-case logic based on trial position and value
    if (x_new < x2) {
      if (f_new < f2) {
        # Case 1: trial left of middle, trial is better
        x3 <- x2; f3 <- f2
        x2 <- x_new; f2 <- f_new
      } else {
        # Case 2: trial left of middle, trial is worse
        x1 <- x_new; f1 <- f_new
      }
    } else {
      if (f_new < f2) {
        # Case 3: trial right of middle, trial is better
        x1 <- x2; f1 <- f2
        x2 <- x_new; f2 <- f_new
      } else {
        # Case 4: trial right of middle, trial is worse
        x3 <- x_new; f3 <- f_new
      }
    }
    
    # Record history with bracket points and trial point
    history <- rbind(history, data.frame(
      iteration = rep(iter, 3),
      x = c(x1, x2, x3),
      f_x = c(f1, f2, f3),
      point_type = c("left", "middle", "right"),
      x_trial = rep(x_new, 3),
      f_trial = rep(f_new, 3)
    ))
  }
  
  # Return results
  list(
    algorithm = "Parabolic Interpolation",
    x_final = x2,
    f_final = f2,
    iterations = max_iter,
    converged = FALSE,
    message = "Maximum iterations reached",
    history = history,
    convergence_rate = "Superlinear"
  )
}


#' Brent's Method
#'
#' @description Minimize function using Brent's method. Combines golden section and parabolic interpolation.
#'
#' @param f Function to minimize
#' @param interval Numeric vector of length 2: c(a, b)
#' @param tol Tolerance for convergence (default: 1e-6)
#' @param max_iter Maximum iterations (default: 100)
#'
#' @return List containing convergence results and iteration history
#'
#' @export
brent_method <- function(f, interval, tol = 1e-6, max_iter = 100) {
  
  # Golden ratio constant
  phi <- (1 + sqrt(5)) / 2
  resphi <- 2 - phi
  
  # Initialize bounds
  a <- min(interval[1], interval[2])
  b <- max(interval[1], interval[2])
  
  # Initialize: x = best, w = second best, v = previous w
  x <- w <- v <- a + resphi * (b - a)
  fx <- fw <- fv <- f(x)
  
  # Initialize history
  history <- data.frame(
    iteration = 0,
    x = x,
    f_x = fx,
    interval_a = a,
    interval_b = b,
    x_best = x,
    fx_best = fx,
    w = w,
    v = v,
    fw = fw,
    fv = fv,
    step_type = "initial",
    stringsAsFactors = FALSE
  )
  
  # Step tracking for parabolic acceptance
  e <- 0
  d <- 0
  
  # Main iteration loop
  for (iter in 1:max_iter) {
    xm <- 0.5 * (a + b)
    tol1 <- tol * abs(x) + 1e-10
    tol2 <- 2.0 * tol1
    
    # Check convergence
    if (abs(x - xm) <= (tol2 - 0.5 * (b - a))) {
      return(list(
        algorithm = "Brent's Method",
        x_final = x,
        f_final = fx,
        iterations = iter,
        converged = TRUE,
        message = "Converged",
        history = history,
        convergence_rate = "Superlinear"
      ))
    }
    
    step_type <- "golden"
    
    # Try parabolic interpolation if previous step was large enough
    if (abs(e) > tol1) {
      # Compute parabolic step
      r <- (x - w) * (fx - fv)
      q <- (x - v) * (fx - fw)
      p <- (x - v) * q - (x - w) * r
      q <- 2.0 * (q - r)
      
      if (q > 0) p <- -p
      q <- abs(q)
      
      etemp <- e
      e <- d
      
      # Accept parabolic step only if it's safe
      if (abs(p) >= abs(0.5 * q * etemp) ||
          p <= q * (a - x) ||
          p >= q * (b - x)) {
        # Reject: use golden section
        e <- ifelse(x >= xm, a - x, b - x)
        d <- resphi * e
        step_type <- "golden"
      } else {
        # Accept parabolic step
        d <- p / q
        u <- x + d
        
        if ((u - a) < tol2 || (b - u) < tol2) {
          d <- ifelse(xm > x, tol1, -tol1)
        }
        step_type <- "parabolic"
      }
    } else {
      # Use golden section
      e <- ifelse(x >= xm, a - x, b - x)
      d <- resphi * e
      step_type <- "golden"
    }
    
    # Compute trial point
    u <- x + ifelse(abs(d) >= tol1, d, ifelse(d > 0, tol1, -tol1))
    fu <- f(u)
    
    # Record history before updating
    history <- rbind(history, data.frame(
      iteration = iter,
      x = u,
      f_x = fu,
      interval_a = a,
      interval_b = b,
      x_best = x,
      fx_best = fx,
      w = w,
      v = v,
      fw = fw,
      fv = fv,
      step_type = step_type,
      stringsAsFactors = FALSE
    ))
    
    # Update bracket and best points
    if (fu <= fx) {
      if (u >= x) {
        a <- x
      } else {
        b <- x
      }
      v <- w; fv <- fw
      w <- x; fw <- fx
      x <- u; fx <- fu
    } else {
      if (u < x) {
        a <- u
      } else {
        b <- u
      }
      
      if (fu <= fw || w == x) {
        v <- w; fv <- fw
        w <- u; fw <- fu
      } else if (fu <= fv || v == x || v == w) {
        v <- u; fv <- fu
      }
    }
  }
  
  # Return results
  list(
    algorithm = "Brent's Method",
    x_final = x,
    f_final = fx,
    iterations = max_iter,
    converged = FALSE,
    message = "Maximum iterations reached",
    history = history,
    convergence_rate = "Superlinear"
  )

}
