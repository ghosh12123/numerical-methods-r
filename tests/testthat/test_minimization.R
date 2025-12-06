library(testthat)

# Test data
test_functions <- list(
  convex_quadratic = list(
    f = function(x) (x - 3)^2 + 2,
    minimum = 3,
    interval = c(0, 5)
  ),
  
  quartic = list(
    f = function(x) x^4 - 3*x^3 + 2*x,
    interval = c(1, 3)  # Has local minimum around 2.14
  ),
  
  flat_valley = list(
    f = function(x) (x - 5)^4,
    minimum = 5,
    interval = c(3, 7)
  )
)

# Test Golden Section Search
test_that("Golden section search finds minima", {
  
  # Test convex quadratic
  result <- golden_section_search(
    test_functions$convex_quadratic$f,
    test_functions$convex_quadratic$interval
  )
  
  expect_true(result$converged)
  expect_equal(result$x_final, 3, tolerance = 1e-4)
  
  # Test flat valley
  result <- golden_section_search(
    test_functions$flat_valley$f,
    test_functions$flat_valley$interval
  )
  
  expect_true(result$converged)
  expect_equal(result$x_final, 5, tolerance = 1e-4)
})

# Test Parabolic Interpolation
test_that("Parabolic interpolation finds minima", {
  
  result <- parabolic_interpolation(
    test_functions$convex_quadratic$f,
    test_functions$convex_quadratic$interval
  )
  
  expect_true(result$converged)
  expect_equal(result$x_final, 3, tolerance = 1e-5)
})

# Test Brent's Method
test_that("Brent's method finds minima efficiently", {
  
  result <- brent_method(
    test_functions$convex_quadratic$f,
    test_functions$convex_quadratic$interval
  )
  
  expect_true(result$converged)
  expect_equal(result$x_final, 3, tolerance = 1e-6)
  
  # Should be faster than golden section
  golden_result <- golden_section_search(
    test_functions$convex_quadratic$f,
    test_functions$convex_quadratic$interval
  )
  
  expect_lt(result$iterations, golden_result$iterations)
})

# Test convergence history
test_that("Minimization algorithms track history", {
  
  result <- brent_method(
    test_functions$convex_quadratic$f,
    test_functions$convex_quadratic$interval
  )
  
  expect_true(!is.null(result$history))
  expect_gt(nrow(result$history), 0)
  expect_true("iteration" %in% names(result$history))
  expect_true("x" %in% names(result$history))
  expect_true("f_x" %in% names(result$history))
})
