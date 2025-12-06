library(testthat)

# Test data
test_functions <- list(
  simple_quadratic = list(
    f = function(x) x^2 - 4,
    df = function(x) 2*x,
    roots = c(-2, 2),
    interval = c(1, 3)
  ),
  
  cubic = list(
    f = function(x) x^3 - 2*x - 5,
    df = function(x) 3*x^2 - 2,
    roots = c(2.0945514815),
    interval = c(1, 3)
  ),
  
  transcendental = list(
    f = function(x) cos(x) - x,
    df = function(x) -sin(x) - 1,
    roots = c(0.7390851332),
    interval = c(0, 1)
  ),
  
  no_root = list(
    f = function(x) x^2 + 1,
    df = function(x) 2*x,
    roots = numeric(0),
    interval = c(-2, 2)
  )
)

# Test Bisection Method
test_that("Bisection method finds roots correctly", {
  
  # Test simple quadratic
  result <- bisection_method(test_functions$simple_quadratic$f, c(1, 3))
  expect_true(result$converged)
  expect_equal(result$x_final, 2, tolerance = 1e-5)
  
  # Test cubic
  result <- bisection_method(test_functions$cubic$f, c(1, 3))
  expect_true(result$converged)
  expect_equal(result$x_final, test_functions$cubic$roots[1], tolerance = 1e-5)
  
  # Test no root (should fail)
  result <- bisection_method(test_functions$no_root$f, c(-2, 2))
  expect_false(result$converged)
  expect_match(result$message, "sign change")
})

# Test Newton-Raphson
test_that("Newton-Raphson converges quickly", {
  
  # Test simple quadratic
  result <- newton_raphson(
    test_functions$simple_quadratic$f,
    x0 = 1.5,
    df = test_functions$simple_quadratic$df
  )
  
  expect_true(result$converged)
  expect_equal(result$x_final, 2, tolerance = 1e-6)
  expect_lt(result$iterations, 10)  # Should be fast
  
  # Test cubic
  result <- newton_raphson(
    test_functions$cubic$f,
    x0 = 2,
    df = test_functions$cubic$df
  )
  
  expect_true(result$converged)
  expect_equal(result$x_final, test_functions$cubic$roots[1], tolerance = 1e-6)
})

# Test Newton without derivative (numerical)
test_that("Newton-Raphson works with numerical derivative", {
  
  result <- newton_raphson(
    test_functions$cubic$f,
    x0 = 2,
    df = NULL  # Will use numerical derivative
  )
  
  expect_true(result$converged)
  expect_equal(result$x_final, test_functions$cubic$roots[1], tolerance = 1e-5)
})

# Test Secant Method
test_that("Secant method finds roots", {
  
  result <- secant_method(
    test_functions$simple_quadratic$f,
    x0 = 1,
    x1 = 3
  )
  
  expect_true(result$converged)
  expect_equal(result$x_final, 2, tolerance = 1e-5)
})

# Test convergence history tracking
test_that("Convergence history is recorded", {
  
  result <- newton_raphson(
    test_functions$cubic$f,
    x0 = 2,
    df = test_functions$cubic$df
  )
  
  expect_true(!is.null(result$history))
  expect_gt(nrow(result$history), 0)
  expect_true("iteration" %in% names(result$history))
  expect_true("x" %in% names(result$history))
  expect_true("f_x" %in% names(result$history))
})

# Test error handling
test_that("Algorithms handle edge cases", {
  
  # Zero derivative
  f <- function(x) x^3
  df <- function(x) 3*x^2
  result <- newton_raphson(f, x0 = 0, df = df, max_iter = 5)
  
  # Should detect zero derivative
  expect_false(result$converged)
})
