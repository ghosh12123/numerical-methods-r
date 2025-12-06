#' Root-Finding Algorithms
#'
#' @description Collection of root-finding algorithms for solving f(x) = 0
#'
#' @name root_finding
NULL

#' Bisection Method
#'
#' @description Find root using bisection method. Requires sign change in interval.
#'
#' @param f Function to find root of
#' @param interval Numeric vector of length 2: c(a, b)
#' @param tol Tolerance for convergence (default: 1e-6)
#' @param max_iter Maximum iterations (default: 100)
#'
#' @return List containing convergence results and iteration history
#'
#' @export
bisection_method <- function(f, interval, tol = 1e-6, max_iter = 100) {
  
  # Validate interval
  if (length(interval) != 2) {
    stop("interval must be a vector of length 2")
  }
  
  # Initialize bounds
  a <- min(interval[1], interval[2])
  b <- max(interval[1], interval[2])
  
  fa <- f(a)
  fb <- f(b)
  
  # Initialize history
  history <- data.frame(
    iteration = integer(),
    x = numeric(),
    f_x = numeric(),
    interval_width = numeric(),
    interval_a = numeric(),
    interval_b = numeric()
  )
  
  # Check for root at endpoints
  if (abs(fa) < tol) {
    return(list(
      algorithm = "Bisection",
      x_final = a,
      f_final = fa,
      iterations = 0,
      converged = TRUE,
      message = "Root found at left endpoint",
      history = history,
      convergence_rate = "Linear"
    ))
  }
  
  if (abs(fb) < tol) {
    return(list(
      algorithm = "Bisection",
      x_final = b,
      f_final = fb,
      iterations = 0,
      converged = TRUE,
      message = "Root found at right endpoint",
      history = history,
      convergence_rate = "Linear"
    ))
  }
  
  # Check for sign change
  if (fa * fb > 0) {
    return(list(
      algorithm = "Bisection",
      x_final = NA,
      f_final = NA,
      iterations = 0,
      converged = FALSE,
      message = "No sign change in interval [a, b]",
      history = history,
      convergence_rate = "N/A"
    ))
  }
  
  # Main iteration loop
  iter <- 0
  f.a <- fa
  f.b <- fb
  convergence <- 0
  
  while (abs(b - a) > tol) {
    iter <- iter + 1
    if (iter > max_iter) {
      convergence <- 1
      break
    }
    
    # Compute midpoint
    xmid <- (a + b) / 2
    ymid <- f(xmid)
    
    # Record history before updating
    history <- rbind(history, data.frame(
      iteration = iter,
      x = xmid,
      f_x = ymid,
      interval_width = b - a,
      interval_a = a,
      interval_b = b
    ))
    
    # Update interval based on sign
    if (f.a * ymid > 0) {
      a <- xmid
      f.a <- ymid
    } else {
      b <- xmid
    }
  }
  
  # Compute final root estimate
  root <- (a + b) / 2
  f_root <- f(root)
  
  # Return results
  list(
    algorithm = "Bisection",
    x_final = root,
    f_final = f_root,
    iterations = iter,
    converged = (convergence == 0),
    message = if(convergence == 0) "Converged" else "Maximum iterations reached",
    history = history,
    convergence_rate = "Linear"
  )
}


#' Newton-Raphson Method
#'
#' @description Find root using Newton-Raphson method. Uses derivative for quadratic convergence.
#'
#' @param f Function to find root of
#' @param x0 Initial guess
#' @param tol Tolerance for convergence (default: 1e-6)
#' @param max_iter Maximum iterations (default: 100)
#' @param df Derivative function (optional - uses numerical derivative if not provided)
#'
#' @return List containing convergence results and iteration history
#'
#' @export
newton_raphson <- function(f, x0, tol = 1e-6, max_iter = 100, df = NULL) {
  
  # Calculate numerical derivative via numDeriv package
  df <- function(x) numDeriv::grad(f, x)
  
  # Initialize history
  history <- data.frame(
    iteration = integer(),
    x = numeric(),
    f_x = numeric(),
    df_x = numeric()
  )
  
  # Main iteration loop
  convergence <- 1
  x1 <- x0
  
  for (iter in 1:max_iter) {
    fp0 <- df(x0)
    f0 <- f(x0)
    
    # Record history
    history <- rbind(history, data.frame(
      iteration = iter,
      x = x0,
      f_x = f0,
      df_x = fp0
    ))
    
    # Check for zero derivative
    if (abs(fp0) < tol) {
      convergence <- 1
      break
    }
    
    # Newton update: x_{n+1} = x_n - f(x_n) / f'(x_n)
    x1 <- x0 - f0 / fp0
    
    # Check convergence
    if (abs(x1 - x0) < tol) {
      convergence <- 0
      history <- rbind(history, data.frame(
        iteration = iter + 1,
        x = x1,
        f_x = f(x1),
        df_x = df(x1)
      ))
      break
    }
    
    x0 <- x1
  }
  
  # Return results
  list(
    algorithm = "Newton-Raphson",
    x_final = x1,
    f_final = f(x1),
    iterations = iter,
    converged = (convergence == 0),
    message = if(convergence == 0) "Converged" else if(abs(df(x1)) < tol) "Derivative too close to zero" else "Maximum iterations reached",
    history = history,
    convergence_rate = "Quadratic"
  )
}


#' Secant Method
#'
#' @description Find root using secant method. Derivative-free variant of Newton's method.
#'
#' @param f Function to find root of
#' @param x0 First initial guess
#' @param x1 Second initial guess (default: x0 + 0.1)
#' @param tol Tolerance for convergence (default: 1e-6)
#' @param max_iter Maximum iterations (default: 100)
#'
#' @return List containing convergence results and iteration history
#'
#' @export
secant_method <- function(f, x0, x1 = NULL, tol = 1e-6, max_iter = 100) {
  
  # Default second point
  if (is.null(x1)) {
    x1 <- x0 + 0.1
  }
  
  # Initialize history with starting points
  history <- data.frame(
    iteration = c(0, 1),
    x = c(x0, x1),
    f_x = c(f(x0), f(x1))
  )
  
  # Main iteration loop
  convergence <- 1
  f0 <- f(x0)
  f1 <- f(x1)
  
  # Check if initial points are too close
  if (abs(f0 - f1) < tol) {
    return(list(
      algorithm = "Secant",
      x_final = x1,
      f_final = f1,
      iterations = 1,
      converged = FALSE,
      message = "Initial points too close",
      history = history,
      convergence_rate = "Superlinear"
    ))
  }
  
  # Compute first secant step
  x12 <- -f1 / (f1 - f0) * (x1 - x0)
  x2 <- x1 + x12
  
  for (iter in 1:max_iter) {
    # Check convergence
    if (abs(x12) < tol) {
      convergence <- 0
      break
    }
    
    # Update points
    f0 <- f1
    x1 <- x2
    f1 <- f(x2)
    
    # Record history
    history <- rbind(history, data.frame(
      iteration = iter + 1,
      x = x2,
      f_x = f1
    ))
    
    # Compute secant step
    f01 <- f1 - f0
    if (abs(f01) < tol) {
      break
    }
    
    x12 <- -f1 / f01 * x12
    x2 <- x1 + x12
  }
  
  # Return results
  list(
    algorithm = "Secant",
    x_final = x2,
    f_final = f(x2),
    iterations = iter + 1,
    converged = (convergence == 0),
    message = if(convergence == 0) "Converged" else "Maximum iterations reached",
    history = history,
    convergence_rate = "Superlinear"
  )
}


#' Fixed-Point Iteration
#'
#' @description Find root using fixed-point iteration x_{n+1} = g(x_n).
#'
#' @param f Function to find root of
#' @param x0 Initial guess
#' @param tol Tolerance for convergence (default: 1e-6)
#' @param max_iter Maximum iterations (default: 100)
#' @param g Fixed-point function (default: g(x) = x - f(x))
#'
#' @return List containing convergence results and iteration history
#'
#' @export
fixed_point_iteration <- function(f, x0, tol = 1e-6, max_iter = 100, g = NULL) {
  
  # Default fixed-point function
  if (is.null(g)) {
    g <- function(x) x - f(x)
  }
  
  # Initialize history
  history <- data.frame(
    iteration = integer(),
    x = numeric(),
    f_x = numeric()
  )
  
  # Main iteration loop
  convergence <- 1
  x1 <- x0
  
  for (iter in 1:max_iter) {
    # Fixed-point update: x_{n+1} = g(x_n)
    x1 <- g(x0)
    
    # Record history
    history <- rbind(history, data.frame(
      iteration = iter,
      x = x1,
      f_x = x1 - g(x1)
    ))
    
    # Check for divergence
    if (!is.finite(x1) || abs(x1) > 1e10) {
      return(list(
        algorithm = "Fixed-Point Iteration",
        x_final = x1,
        f_final = if(is.finite(x1)) x1 - g(x1) else NA,
        iterations = iter,
        converged = FALSE,
        message = "Diverging",
        history = history,
        convergence_rate = "N/A"
      ))
    }
    
    # Check convergence
    if (abs(x1 - x0) < tol) {
      convergence <- 0
      break
    }
    
    x0 <- x1
  }
  
  # Determine convergence rate from g'(x*) and g''(x*)
  convergence_rate <- "Linear"
  
  if (convergence == 0 && is.finite(x1)) {
    h <- 1e-7
    
    # Compute g'(x*) numerically
    g_prime <- tryCatch({
      (g(x1 + h) - g(x1 - h)) / (2 * h)
    }, error = function(e) NA)
    
    # Compute g''(x*) numerically
    g_double_prime <- tryCatch({
      (g(x1 + h) - 2 * g(x1) + g(x1 - h)) / (h^2)
    }, error = function(e) NA)
    
    # Determine rate based on derivatives
    if (!is.na(g_prime) && is.finite(g_prime)) {
      if (abs(g_prime) < 1e-6) {
        if (!is.na(g_double_prime) && is.finite(g_double_prime)) {
          convergence_rate <- "Quadratic"
        } else {
          convergence_rate <- "Superlinear"
        }
      } else if (abs(g_prime) < 1) {
        convergence_rate <- "Linear"
      } else {
        convergence_rate <- "Linear"
      }
    }
  }
  
  # Return results
  list(
    algorithm = "Fixed-Point Iteration",
    x_final = x1,
    f_final = x1 - g(x1),
    iterations = iter,
    converged = (convergence == 0),
    message = if(convergence == 0) "Converged" else "Maximum iterations reached",
    history = history,
    convergence_rate = convergence_rate
  )
}