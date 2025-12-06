library(shiny)
library(shinydashboard)
library(plotly)
library(DT)

dashboardPage(
  
  # Header
  dashboardHeader(title = "Numerical Methods Explorer"),
  
  # Sidebar
  dashboardSidebar(
    sidebarMenu(
      menuItem("Problem Setup", tabName = "setup", icon = icon("sliders")),
      menuItem("Results Table", tabName = "results", icon = icon("table")),
      menuItem("Convergence Plots", tabName = "plots", icon = icon("chart-line")),
      menuItem("Algorithm Deep Dive", tabName = "detail", icon = icon("search")),
      menuItem("About", tabName = "about", icon = icon("info-circle"))
    )
  ),
  
  # Body
  dashboardBody(
    
    # Custom CSS for spinner and status
    tags$head(
      tags$style(HTML("
        .status-running {
          color: #f39c12;
          font-weight: bold;
          font-size: 16px;
        }
        .status-complete {
          color: #00a65a;
          font-weight: bold;
          font-size: 16px;
        }
        .status-error {
          color: #dd4b39;
          font-weight: bold;
          font-size: 16px;
        }
        @keyframes spin {
          0% { transform: rotate(0deg); }
          100% { transform: rotate(360deg); }
        }
        .spinner {
          display: inline-block;
          animation: spin 1s linear infinite;
        }
      "))
    ),
    
    tabItems(
      
      # Tab 1: Problem Setup
      tabItem(
        tabName = "setup",
        fluidRow(
          box(
            title = "Function Definition",
            width = 6,
            solidHeader = TRUE,
            status = "primary",
            
            radioButtons("problem_type", "Problem Type:",
                        choices = c("Root-Finding" = "root", 
                                  "Minimization" = "minimize"),
                        selected = "root"),
            
            textInput("custom_function", "Function f(x):", 
                     value = "x^2 - 4",
                     placeholder = "e.g., x^2 - 4")
          ),
          
          box(
            title = "Algorithm Parameters",
            width = 6,
            solidHeader = TRUE,
            status = "info",
            
            numericInput("interval_a", "Interval/Guess a:",
                        value = 1, step = 0.1),
            
            numericInput("interval_b", "Interval/Guess b:",
                        value = 3, step = 0.1),
            
            sliderInput("tolerance", "Tolerance:",
                       min = 1e-10, max = 1e-3, value = 1e-6),
            
            sliderInput("max_iter", "Maximum Iterations:",
                       min = 10, max = 2000, value = 500, step = 10)
          )
        ),
        
        fluidRow(
          box(
            width = 12,
            actionButton("run_algorithms", "Run All Algorithms", 
                        icon = icon("play"),
                        class = "btn-success btn-lg",
                        width = "100%")
          )
        ),
        
        fluidRow(
          box(
            title = "Status",
            width = 12,
            solidHeader = TRUE,
            status = "warning",
            htmlOutput("status_message")
          )
        )
      ),
      
      # Tab 2: Results Table
      tabItem(
        tabName = "results",
        fluidRow(
          box(
            title = "Algorithm Comparison",
            width = 12,
            solidHeader = TRUE,
            status = "primary",
            DTOutput("results_table")
          )
        ),
        
        fluidRow(
          box(
            title = "Summary Statistics",
            width = 6,
            solidHeader = TRUE,
            status = "info",
            verbatimTextOutput("summary_stats")
          ),
          
          box(
            title = "Recommendation",
            width = 6,
            solidHeader = TRUE,
            status = "success",
            verbatimTextOutput("recommendation")
          )
        ),
        
        fluidRow(
          box(
            title = "Export Data",
            width = 12,
            downloadButton("download_csv", "Download Results as CSV"),
            downloadButton("download_report", "Download Full Report")
          )
        )
      ),
      
      # Tab 3: Convergence Plots
      tabItem(
        tabName = "plots",
        fluidRow(
          box(
            title = "Convergence Comparison",
            width = 12,
            solidHeader = TRUE,
            status = "primary",
            plotlyOutput("convergence_plot", height = "500px")
          )
        ),
        
        fluidRow(
          box(
            title = "Iteration Count Comparison",
            width = 12,
            solidHeader = TRUE,
            status = "info",
            plotlyOutput("iterations_barplot", height = "400px")
          )
        )
      ),
      
      # Tab 4: Algorithm Detail
      tabItem(
        tabName = "detail",
        fluidRow(
          box(
            title = "Select Algorithm for Deep Dive",
            width = 12,
            solidHeader = TRUE,
            status = "primary",
            selectInput("selected_algorithm", "Algorithm:",
                       choices = NULL)
          )
        ),
        
        fluidRow(
          box(
            title = "Algorithm Details",
            width = 6,
            solidHeader = TRUE,
            status = "info",
            verbatimTextOutput("algorithm_details")
          ),
          
          box(
            title = "Iteration History",
            width = 6,
            solidHeader = TRUE,
            status = "info",
            DTOutput("iteration_history")
          )
        ),
        
        fluidRow(
          box(
            title = "Algorithm-Specific Animation",
            width = 12,
            solidHeader = TRUE,
            status = "primary",
            plotlyOutput("detailed_convergence", height = "600px")
          )
        )
      ),
      
      # Tab 5: About
      tabItem(
        tabName = "about",
        fluidRow(
          box(
            title = "About This App",
            width = 12,
            solidHeader = TRUE,
            status = "primary",
            
            h3("Numerical Methods Comparison Framework"),
            p("An interactive tool for exploring and comparing root-finding and 
              optimization algorithms."),
            
            h4("Implemented Algorithms:"),
            tags$ul(
              tags$li(strong("Root-Finding:"), 
                     "Bisection, Newton-Raphson, Secant, Fixed-Point Iteration"),
              tags$li(strong("Optimization:"), 
                     "Golden Section Search, Parabolic Interpolation, Brent's Method")
            ),
            
            h4("Features:"),
            tags$ul(
              tags$li("Compare all algorithms side-by-side"),
              tags$li("Interactive parameter adjustment with sliders"),
              tags$li("Real-time convergence visualization"),
              tags$li("Detailed analysis of individual algorithms"),
              tags$li("Export results to CSV"),
              tags$li("Algorithm recommendations based on performance")
            ),
            
            h4("How to Use:"),
            tags$ol(
              tags$li("Select problem type (Root-Finding or Minimization)"),
              tags$li("Enter your function using 'x' as the variable"),
              tags$li("Adjust interval and parameters with sliders"),
              tags$li("Click 'Run All Algorithms'"),
              tags$li("Explore results in other tabs")
            ),
            
            h4("Key Insights:"),
            tags$ul(
              tags$li("Newton-Raphson provides quadratic convergence when derivatives are available"),
              tags$li("Brent's method is the best general-purpose derivative-free method"),
              tags$li("Bisection is slowest but most reliable for guaranteed sign changes"),
              tags$li("Always provide analytical derivatives when available for faster convergence")
            ),
            
            hr(),
            p(em("Created with R, Shiny, and ggplot2"))
          )
        )
      )
    )
  )
)