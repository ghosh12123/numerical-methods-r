library(shiny)
library(DT)
library(plotly)
library(ggplot2)

function(input, output, session) {
  
  # Reactive values to store results
  results_data <- reactiveVal(NULL)
  
  # Get function based on selection
  get_function <- reactive({
    
    f_expr <- input$custom_function
    
    # Validate function expression
    validation <- validate_function(f_expr)
    if (!validation$valid) {
      return(list(valid = FALSE, error = validation$error))
    }
    
    # Create function
    f <- function(x) eval(parse(text = f_expr))
    
    list(valid = TRUE, f = f, expr = f_expr)
  })
  
  # Validation function - returns ONLY "ERROR: Not valid input"
  validate_function <- function(expr) {
    # Check if empty
    if (is.null(expr) || expr == "") {
      return(list(valid = FALSE, error = "ERROR: Not valid input"))
    }
    
    # Check if 'x' is present
    if (!grepl("x", expr)) {
      return(list(valid = FALSE, error = "ERROR: Not valid input"))
    }
    
    # Check for invalid single-letter variables (a-z except x)
    invalid_pattern <- "\\b[a-wyz]\\b"
    if (grepl(invalid_pattern, expr, perl = TRUE)) {
      return(list(valid = FALSE, error = "ERROR: Not valid input"))
    }
    
    # Try to parse and evaluate
    tryCatch({
      parse(text = expr)
      
      # Try to evaluate with x = 1 (just to check syntax works)
      x <- 1
      test_result <- eval(parse(text = expr))
      
      if (!is.numeric(test_result)) {
        return(list(valid = FALSE, error = "ERROR: Not valid input"))
      }
      
      # Don't check is.finite - let algorithms handle Inf/large values
      
      list(valid = TRUE)
      
    }, error = function(e) {
      list(valid = FALSE, error = "ERROR: Not valid input")
    })
  }
  
  # Warning function - checks if function might have issues in interval
  check_function_warning <- function(f, interval) {
    tryCatch({
      # Test at several points in interval
      test_points <- seq(interval[1], interval[2], length.out = 10)
      test_values <- sapply(test_points, f)
      
      # Check for NaN, Inf
      if (any(is.nan(test_values))) {
        return("⚠️ Warning: Function produces NaN values in interval (e.g., sqrt of negative)")
      }
      if (all(is.infinite(test_values))) {
        return("⚠️ Warning: Function produces only infinite values in interval")
      }
      
      return(NULL)
    }, error = function(e) {
      return("⚠️ Warning: Function may have evaluation issues in interval")
    })
  }
  
  # Run algorithms when button clicked
  observeEvent(input$run_algorithms, {
    
    # Show "Running..." with spinner
    output$status_message <- renderText({
      '<span class="status-running"><i class="fa fa-spinner spinner"></i> Running...</span>'
    })
    
    tryCatch({
      
      func_data <- get_function()
      
      # Check if function is valid
      if (!func_data$valid) {
        output$status_message <- renderText({
          '<span class="status-error">ERROR: Not valid input</span>'
        })
        return()
      }
      
      f <- func_data$f
      
      interval <- c(input$interval_a, input$interval_b)
      
      # Check for function warnings
      warning_msg <- check_function_warning(f, interval)
      if (!is.null(warning_msg)) {
        showNotification(warning_msg, type = "warning", duration = 6)
      }
      
      # Run appropriate solver
      if (input$problem_type == "root") {
        tryCatch({
          results <- solve_root(
            f = f,
            initial = interval,
            methods = "all",
            tol = input$tolerance,
            max_iter = input$max_iter
          )
        }, error = function(e) {
          cat("ERROR in solve_root:", e$message, "\n")
          stop(e)
        })
      } else {
        results <- solve_minimize(
          f = f,
          interval = interval,
          methods = "all",
          tol = input$tolerance,
          max_iter = input$max_iter
        )
      }
      
      results_data(results)
      
      # Update algorithm selector
      updateSelectInput(session, "selected_algorithm",
                       choices = names(results),
                       selected = names(results)[1])
      
      # Show "Complete" with checkmark
      output$status_message <- renderText({
        '<span class="status-complete"><i class="fa fa-check-circle"></i> Complete</span>'
      })
      
    }, error = function(e) {
      output$status_message <- renderText({
        '<span class="status-error">ERROR: Not valid input</span>'
      })
    })
  })
  
  # Results table
  output$results_table <- renderDT({
    
    req(results_data())
    results <- results_data()
    
    # Create summary table with error handling
    df_list <- lapply(names(results), function(name) {
      r <- results[[name]]
      
      # Handle malformed results
      tryCatch({
        data.frame(
          Algorithm = if(!is.null(r$algorithm)) r$algorithm else name,
          Status = if(!is.null(r$converged)) {
            ifelse(r$converged, "✓ Converged", paste("✗", r$message))
          } else {
            "✗ Error"
          },
          Iterations = if(!is.null(r$iterations)) r$iterations else 0,
          x_final = if(!is.null(r$x_final) && is.finite(r$x_final)) {
            sprintf("%.6f", r$x_final)
          } else {
            "N/A"
          },
          f_final = if(!is.null(r$f_final) && is.finite(r$f_final)) {
            sprintf("%.2e", r$f_final)
          } else {
            "N/A"
          },
          Rate = if(!is.null(r$convergence_rate)) r$convergence_rate else "N/A",
          Warning = if(!is.null(r$warning)) r$warning else "",
          stringsAsFactors = FALSE
        )
      }, error = function(e) {
        # Fallback for completely broken result
        data.frame(
          Algorithm = name,
          Status = "✗ Fatal Error",
          Iterations = 0,
          x_final = "N/A",
          f_final = "N/A",
          Rate = "N/A",
          Warning = as.character(e$message),
          stringsAsFactors = FALSE
        )
      })
    })
    
    df <- do.call(rbind, df_list)
    
    datatable(df, 
              options = list(pageLength = 15, scrollX = TRUE),
              rownames = FALSE) %>%
      formatStyle('Status',
                  backgroundColor = styleEqual(
                    c('✓ Converged'),
                    c('lightgreen')
                  ))
  })
  
  # Summary statistics
  output$summary_stats <- renderText({
    
    req(results_data())
    results <- results_data()
    
    # Safely count converged (handle NULL/missing converged field)
    converged <- sum(sapply(results, function(r) {
      if (!is.null(r$converged) && is.logical(r$converged)) {
        return(r$converged)
      } else {
        return(FALSE)
      }
    }))
    
    total <- length(results)
    
    # Safely get iterations (handle NULL/missing/non-numeric)
    iterations_vec <- sapply(results, function(r) {
      if (!is.null(r$iterations) && is.numeric(r$iterations) && is.finite(r$iterations)) {
        return(r$iterations)
      } else {
        return(Inf)  # Failed algorithms get Inf so they're not "fastest"
      }
    })
    
    # Check if we have any valid results
    if (all(is.infinite(iterations_vec))) {
      return("All algorithms failed.\nFunction may not be valid in the given interval.")
    }
    
    fastest_idx <- which.min(iterations_vec)
    fastest <- results[[fastest_idx]]
    
    # Format f_final safely
    f_final_str <- if (!is.null(fastest$f_final) && is.finite(fastest$f_final)) {
      sprintf("%.2e", abs(fastest$f_final))
    } else {
      "N/A"
    }
    
    sprintf(
      "Total Algorithms: %d\n✓ Converged: %d\n✗ Failed: %d\n\nFastest Algorithm:\n  %s\n  %d iterations\n  %s residual |f(x)|",
      total,
      converged,
      total - converged,
      if(!is.null(fastest$algorithm)) fastest$algorithm else "Unknown",
      if(!is.null(fastest$iterations)) fastest$iterations else 0,
      f_final_str
    )
  })
  
  # Recommendation
  output$recommendation <- renderText({
    
    req(results_data())
    results <- results_data()
    
    # Only consider DIRECT methods (no converted)
    direct_results <- Filter(function(r) {
      is.null(r$mode) || r$mode == "Direct"
    }, results)
    
    converged_results <- Filter(function(r) r$converged, direct_results)
    
    if (length(converged_results) == 0) {
      return("No algorithms converged.\nTry adjusting parameters or initial guess.")
    }
    
    # Determine if this is root-finding or optimization
    is_optimization <- any(sapply(converged_results, function(r) {
      grepl("Golden|Parabolic|Brent", r$algorithm)
    }))
    
    if (is_optimization) {
      # ========================================
      # OPTIMIZATION RECOMMENDATIONS
      # ========================================
      
      # Check if Parabolic did exceptionally well (< 5 iterations)
      parabolic_results <- Filter(function(r) grepl("Parabolic", r$algorithm), converged_results)
      if (length(parabolic_results) > 0 && parabolic_results[[1]]$iterations < 5) {
        r <- parabolic_results[[1]]
        return(sprintf(
          "RECOMMENDED: %s\n\nReason: Excellent for smooth functions\nIterations: %d\nResidual: %.2e\n\nNote: Very fast convergence for quadratic-like objectives",
          r$algorithm,
          r$iterations,
          abs(r$f_final)
        ))
      }
      
      # Otherwise check for Brent (most robust)
      brent_results <- Filter(function(r) grepl("Brent", r$algorithm), converged_results)
      if (length(brent_results) > 0) {
        r <- brent_results[[1]]
        return(sprintf(
          "RECOMMENDED: %s\n\nReason: Most robust optimization method\nIterations: %d\nResidual: %.2e\n\nNote: Combines parabolic + golden section for reliability",
          r$algorithm,
          r$iterations,
          abs(r$f_final)
        ))
      }
      
      # Fallback for optimization: recommend fastest
      iterations_vec <- sapply(converged_results, function(r) r$iterations)
      fastest <- converged_results[[which.min(iterations_vec)]]
      
      return(sprintf(
        "RECOMMENDED: %s\n\nReason: Fastest convergence for this problem\nIterations: %d\nResidual: %.2e",
        fastest$algorithm,
        fastest$iterations,
        abs(fastest$f_final)
      ))
      
    } else {
      # ========================================
      # ROOT-FINDING RECOMMENDATIONS
      # ========================================
      
      # Check for Newton (best if available and converged)
      newton_results <- Filter(function(r) grepl("Newton", r$algorithm), converged_results)
      if (length(newton_results) > 0) {
        r <- newton_results[[1]]
        return(sprintf(
          "RECOMMENDED: %s\n\nReason: Quadratic convergence\nIterations: %d\nResidual: %.2e\n\nNote: Fastest for smooth functions with known derivative",
          r$algorithm,
          r$iterations,
          abs(r$f_final)
        ))
      }
      
      # Check for Secant (good derivative-free option)
      secant_results <- Filter(function(r) grepl("Secant", r$algorithm), converged_results)
      if (length(secant_results) > 0) {
        r <- secant_results[[1]]
        return(sprintf(
          "RECOMMENDED: %s\n\nReason: Superlinear convergence without derivatives\nIterations: %d\nResidual: %.2e\n\nNote: Good balance of speed and simplicity",
          r$algorithm,
          r$iterations,
          abs(r$f_final)
        ))
      }
      
      # Fallback for root-finding: recommend fastest
      iterations_vec <- sapply(converged_results, function(r) r$iterations)
      fastest <- converged_results[[which.min(iterations_vec)]]
      
      return(sprintf(
        "RECOMMENDED: %s\n\nReason: Fastest convergence among direct methods\nIterations: %d\nResidual: %.2e",
        fastest$algorithm,
        fastest$iterations,
        abs(fastest$f_final)
      ))
    }
  })
  
  # Convergence plot
  output$convergence_plot <- renderPlotly({
    
    req(results_data())
    results <- results_data()
    
    # Combine histories - only use common columns (iteration, x, f_x)
    history_list <- lapply(names(results), function(name) {
      r <- results[[name]]
      if (!is.null(r$history) && nrow(r$history) > 0) {
        tryCatch({
          # Extract only common columns (all algorithms should have these)
          hist_subset <- data.frame(
            iteration = r$history$iteration,
            x = r$history$x,
            f_x = r$history$f_x,
            algorithm = r$algorithm,
            stringsAsFactors = FALSE
          )
          return(hist_subset)
        }, error = function(e) {
          # If columns don't exist, skip this algorithm
          return(NULL)
        })
      }
      NULL
    })
    
    history_combined <- do.call(rbind, Filter(Negate(is.null), history_list))
    
    if (is.null(history_combined) || nrow(history_combined) == 0) {
      return(NULL)
    }
    
    p <- ggplot(history_combined, aes(x = iteration, y = abs(f_x), 
                                      color = algorithm, group = algorithm)) +
      geom_line(size = 1) +
      geom_point(size = 2) +
      scale_y_log10() +
      facet_wrap(~ algorithm, scales = "free_x", ncol = 2) +
      theme_minimal() +
      theme(legend.position = "none") +  # Remove legend since facet labels show algorithm names
      labs(
        title = "Function Value Convergence (Each Algorithm on Own Scale)",
        x = "Iteration",
        y = "|f(x)| (log scale)"
      )
    
    ggplotly(p)
  })
  
  # Iterations bar plot
  output$iterations_barplot <- renderPlotly({
    
    req(results_data())
    results <- results_data()
    
    df <- data.frame(
      algorithm = sapply(results, function(r) r$algorithm),
      iterations = sapply(results, function(r) r$iterations),
      mode = sapply(results, function(r) ifelse(is.null(r$mode), "Direct", r$mode))
    )
    
    df$color_group <- ifelse(grepl("Direct", df$mode), "Direct", "Converted")
    
    p <- ggplot(df, aes(x = reorder(algorithm, -iterations), y = iterations, fill = color_group)) +
      geom_col(alpha = 0.7) +
      coord_flip() +
      scale_fill_manual(values = c("Direct" = "forestgreen", "Converted" = "orange")) +
      theme_minimal() +
      labs(
        title = "Iteration Count",
        x = "",
        y = "Iterations",
        fill = "Mode"
      )
    
    ggplotly(p)
  })
  
  # Algorithm details
  output$algorithm_details <- renderText({
    
    req(results_data(), input$selected_algorithm)
    results <- results_data()
    r <- results[[input$selected_algorithm]]
    
    sprintf(
      "Algorithm: %s\nStatus: %s\n\nResults:\n  x_final = %.8f\n  f(x_final) = %.2e\n  Iterations = %d\n  Convergence Rate = %s\n\n%s",
      r$algorithm,
      ifelse(r$converged, "✓ Converged", paste("✗", r$message)),
      r$x_final,
      r$f_final,
      r$iterations,
      r$convergence_rate,
      ifelse(is.null(r$warning), "", paste("Warning:", r$warning))
    )
  })
  
  # Iteration history table
  output$iteration_history <- renderDT({
    
    req(results_data(), input$selected_algorithm)
    results <- results_data()
    r <- results[[input$selected_algorithm]]
    
    if (is.null(r$history) || nrow(r$history) == 0) {
      return(NULL)
    }
    
    datatable(r$history, options = list(
      pageLength = 10,
      searching = FALSE
    ))
  })
  
  # Detailed convergence plot - Algorithm-Specific Animation
  output$detailed_convergence <- renderPlotly({
    
    req(results_data(), input$selected_algorithm)
    results <- results_data()
    r <- results[[input$selected_algorithm]]
    
    if (is.null(r$history) || nrow(r$history) == 0) {
      return(NULL)
    }
    
    # Get the function from get_function()
    func_data <- get_function()
    if (!func_data$valid) {
      return(NULL)
    }
    
    f <- func_data$f
    
    # Create algorithm-specific animation
    plot_algorithm_animation(r, f, df = NULL)
  })
  
  # Download CSV
  output$download_csv <- downloadHandler(
    filename = function() {
      paste0("numerical_methods_results_", Sys.Date(), ".csv")
    },
    content = function(file) {
      req(results_data())
      results <- results_data()
      
      df_list <- lapply(names(results), function(name) {
        r <- results[[name]]
        data.frame(
          Algorithm = r$algorithm,
          Mode = ifelse(is.null(r$mode), "Direct", r$mode),
          Status = ifelse(r$converged, "Converged", r$message),
          x_final = r$x_final,
          f_final = r$f_final,
          Iterations = r$iterations,
          Convergence_Rate = r$convergence_rate,
          stringsAsFactors = FALSE
        )
      })
      
      df <- do.call(rbind, df_list)
      write.csv(df, file, row.names = FALSE)
    }
  )
}
