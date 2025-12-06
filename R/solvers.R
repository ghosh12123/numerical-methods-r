#' Solver Functions
#'
#' @description Unified interfaces for running multiple algorithms
#'
#' @name solvers
NULL

#' Solve Root-Finding Problem
#'
#' @description Run all root-finding algorithms on a function
#'
#' @param f Function to find root of
#' @param initial Initial guess or interval c(a, b)
#' @param methods Vector of methods to try (default: "all")
#' @param tol Tolerance (default: 1e-6)
#' @param max_iter Maximum iterations (default: 100)
#'
#' @return List of results from all methods
#'
#' @export
solve_root <- function(f, initial, methods = "all", tol = 1e-6, max_iter = 100) {
  
  results <- list()
  
  # Bisection Method
  if ("all" %in% methods || "bisection" %in% methods) {
    if (length(initial) == 2) {
      tryCatch({
        results$bisection <- bisection_method(f, initial, tol, max_iter)
      }, error = function(e) {
        warning("Bisection failed: ", e$message)
      })
    }
  }
  
  # Newton-Raphson Method
  if ("all" %in% methods || "newton" %in% methods) {
    tryCatch({
      x0 <- if (length(initial) == 2) mean(initial) else initial
      results$newton <- newton_raphson(f, x0, tol, max_iter)
    }, error = function(e) {
      warning("Newton-Raphson failed: ", e$message)
    })
  }
  
  # Secant Method
  if ("all" %in% methods || "secant" %in% methods) {
    tryCatch({
      if (length(initial) == 2) {
        results$secant <- secant_method(f, initial[1], initial[2], tol, max_iter)
      } else {
        results$secant <- secant_method(f, initial, initial + 0.1, tol, max_iter)
      }
    }, error = function(e) {
      warning("Secant failed: ", e$message)
    })
  }
  
  # Fixed-Point Iteration
  if ("all" %in% methods || "fixed_point" %in% methods) {
    tryCatch({
      x0 <- if (length(initial) == 2) mean(initial) else initial
      results$fixed_point <- fixed_point_iteration(f, x0, tol, max_iter)
    }, error = function(e) {
      warning("Fixed-Point failed: ", e$message)
    })
  }
  
  results
}


#' Solve Minimization Problem
#'
#' @description Run all optimization algorithms on a function
#'
#' @param f Function to minimize
#' @param interval Search interval c(a, b)
#' @param methods Vector of methods to try (default: "all")
#' @param tol Tolerance (default: 1e-6)
#' @param max_iter Maximum iterations (default: 100)
#'
#' @return List of results from all methods
#'
#' @export
solve_minimize <- function(f, interval, methods = "all", tol = 1e-6, max_iter = 100) {
  
  results <- list()
  
  # Golden Section Search
  if ("all" %in% methods || "golden" %in% methods) {
    tryCatch({
      results$golden <- golden_section_search(f, interval, tol, max_iter)
    }, error = function(e) {
      warning("Golden Section failed: ", e$message)
    })
  }
  
  # Parabolic Interpolation
  if ("all" %in% methods || "parabolic" %in% methods) {
    tryCatch({
      results$parabolic <- parabolic_interpolation(f, interval, tol, max_iter)
    }, error = function(e) {
      warning("Parabolic Interpolation failed: ", e$message)
    })
  }
  
  # Brent's Method
  if ("all" %in% methods || "brent" %in% methods) {
    tryCatch({
      results$brent <- brent_method(f, interval, tol, max_iter)
    }, error = function(e) {
      warning("Brent's Method failed: ", e$message)
    })
  }
  
  results
}


#' Compare All Methods
#'
#' @description Run all available methods and create comparison table
#'
#' @param f Function
#' @param problem_type "root" or "minimize"
#' @param initial Initial guess or interval
#' @param tol Tolerance (default: 1e-6)
#' @param max_iter Maximum iterations (default: 100)
#'
#' @return Data frame with comparison results
#'
#' @export
compare_all_methods <- function(f, problem_type = "root", initial, tol = 1e-6, max_iter = 100) {
  
  # Run appropriate solver
  results <- if (problem_type == "root") {
    solve_root(f, initial, "all", tol, max_iter)
  } else {
    solve_minimize(f, initial, "all", tol, max_iter)
  }
  
  # Convert to data frame
  df_list <- lapply(names(results), function(name) {
    r <- results[[name]]
    data.frame(
      algorithm = r$algorithm,
      status = ifelse(r$converged, "Converged", r$message),
      x_final = ifelse(is.na(r$x_final), NA, r$x_final),
      f_final = ifelse(is.na(r$f_final), NA, r$f_final),
      iterations = r$iterations,
      convergence_rate = r$convergence_rate,
      stringsAsFactors = FALSE
    )
  })
  
  do.call(rbind, df_list)
}
