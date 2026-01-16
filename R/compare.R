# Saqrmisc Package: Comparison Analysis Functions
#
# This file contains functions for group comparison analysis using ggbetweenstats.
# Functions: compare_groups

#' @importFrom dplyr group_by summarise mutate filter count select bind_rows any_of case_when ungroup first
#' @importFrom rlang sym .data
#' @importFrom ggplot2 ggplot aes scale_color_manual
#' @importFrom stats t.test aov sd var p.adjust
#' @importFrom grDevices colorRampPalette
NULL

# =============================================================================
# HELPER FUNCTIONS
# =============================================================================

#' Interpret Bayes Factor
#'
#' @param bf Bayes factor value
#' @return Character interpretation
#' @noRd
interpret_bf <- function(bf) {
  if (is.na(bf) || is.null(bf)) return("NA")
  if (!is.finite(bf)) return(if (bf > 0) "Extreme evidence for H1" else "Extreme evidence for H0")

  if (bf > 100) return("Extreme evidence for H1")
  if (bf > 30) return("Very strong evidence for H1")
  if (bf > 10) return("Strong evidence for H1")
  if (bf > 3) return("Moderate evidence for H1")
  if (bf > 1) return("Anecdotal evidence for H1")
  if (bf == 1) return("No evidence")
  if (bf > 1/3) return("Anecdotal evidence for H0")
  if (bf > 1/10) return("Moderate evidence for H0")
  if (bf > 1/30) return("Strong evidence for H0")
  if (bf > 1/100) return("Very strong evidence for H0")
  return("Extreme evidence for H0")
}

#' Calculate pooled standard deviation
#'
#' @param x Numeric vector for group 1
#' @param y Numeric vector for group 2
#' @return Pooled SD value
#' @noRd
pooled_sd <- function(x, y) {
  n1 <- length(x)
  n2 <- length(y)
  sqrt(((n1 - 1) * stats::var(x) + (n2 - 1) * stats::var(y)) / (n1 + n2 - 2))
}

#' Reshape post-hoc p-value matrix to data frame
#'
#' @param p_matrix Matrix of p-values from pairwise tests
#' @param variable Variable name
#' @param method Adjustment method used
#' @return Data frame with comparisons
#' @noRd
reshape_posthoc_matrix <- function(p_matrix, variable, method) {
  comparisons <- data.frame(
    variable = character(),
    comparison = character(),
    p_adj = numeric(),
    method = character(),
    stringsAsFactors = FALSE
  )

  row_names <- rownames(p_matrix)
  col_names <- colnames(p_matrix)

  for (i in seq_along(row_names)) {
    for (j in seq_along(col_names)) {
      if (!is.na(p_matrix[i, j])) {
        comparisons <- rbind(comparisons, data.frame(
          variable = variable,
          comparison = paste(row_names[i], "vs", col_names[j]),
          p_adj = p_matrix[i, j],
          method = method,
          stringsAsFactors = FALSE
        ))
      }
    }
  }
  return(comparisons)
}

#' Generate ANOVA text report
#'
#' @param variable Variable name
#' @param category Grouping variable name
#' @param data Data subset
#' @param anova_details List with ANOVA statistics
#' @param posthoc_results Data frame with post-hoc results
#' @return Character string with formatted report
#' @noRd
generate_anova_text_report <- function(variable, category, data, anova_details, posthoc_results) {

  # Descriptive statistics by group
  desc_stats <- data %>%
    dplyr::group_by(.data[[category]]) %>%
    dplyr::summarise(
      M = mean(.data[[variable]], na.rm = TRUE),
      SD = stats::sd(.data[[variable]], na.rm = TRUE),
      n = dplyr::n(),
      .groups = 'drop'
    )

  # Format p-value
  format_p <- function(p) {
    if (p < 0.001) "< .001"
    else sprintf("= %.3f", p)
  }

  # Build report
  report <- paste0(
    "\n",
    paste(rep("=", 70), collapse = ""), "\n",
    "ANOVA RESULTS: ", variable, " by ", category, "\n",
    paste(rep("=", 70), collapse = ""), "\n\n"
  )

  # Descriptive statistics
  report <- paste0(report, "DESCRIPTIVE STATISTICS:\n")
  for (i in 1:nrow(desc_stats)) {
    report <- paste0(report, sprintf("  %-15s M = %6.2f, SD = %5.2f, n = %d\n",
                                     paste0(desc_stats[[category]][i], ":"),
                                     desc_stats$M[i],
                                     desc_stats$SD[i],
                                     desc_stats$n[i]))
  }

  # Omnibus test
  report <- paste0(report, "\nOMNIBUS TEST:\n")
  if (anova_details$test == "One-way ANOVA") {
    report <- paste0(report, sprintf("  F(%d, %d) = %.2f, p %s, eta-sq = %.3f (%s effect)\n",
                                     anova_details$df1,
                                     anova_details$df2,
                                     anova_details$statistic,
                                     format_p(anova_details$p_value),
                                     anova_details$effect_size,
                                     anova_details$effect_interp))

    # APA-style sentence
    sig_text <- if (anova_details$p_value < 0.05) "a statistically significant" else "no statistically significant"
    report <- paste0(report, sprintf("\n  A one-way ANOVA revealed %s difference in %s\n",
                                     sig_text, variable))
    report <- paste0(report, sprintf("  between %s groups, F(%d, %d) = %.2f, p %s, eta-sq = %.3f.\n",
                                     category,
                                     anova_details$df1,
                                     anova_details$df2,
                                     anova_details$statistic,
                                     format_p(anova_details$p_value),
                                     anova_details$effect_size))
  } else {
    report <- paste0(report, sprintf("  H(%d) = %.2f, p %s, epsilon-sq = %.3f\n",
                                     anova_details$df,
                                     anova_details$statistic,
                                     format_p(anova_details$p_value),
                                     anova_details$effect_size))
  }

  # Post-hoc comparisons
  if (!is.null(posthoc_results) && nrow(posthoc_results) > 0) {
    method_name <- if ("method" %in% names(posthoc_results)) posthoc_results$method[1] else "Bonferroni"
    report <- paste0(report, sprintf("\nPOST-HOC COMPARISONS (%s):\n", method_name))

    for (i in 1:nrow(posthoc_results)) {
      diff_str <- if ("diff" %in% names(posthoc_results) && !is.na(posthoc_results$diff[i])) {
        sprintf("diff = %6.2f, ", posthoc_results$diff[i])
      } else ""

      sig_star <- if ("significance" %in% names(posthoc_results)) posthoc_results$significance[i] else ""

      report <- paste0(report, sprintf("  %-25s %sp %s %s\n",
                                       paste0(posthoc_results$comparison[i], ":"),
                                       diff_str,
                                       format_p(posthoc_results$p_adj[i]),
                                       sig_star))
    }

    # Conclusion
    sig_comparisons <- posthoc_results[posthoc_results$p_adj < 0.05, ]
    if (nrow(sig_comparisons) > 0) {
      report <- paste0(report, "\nCONCLUSION:\n")
      report <- paste0(report, "  Significant pairwise differences were found between:\n")
      for (i in 1:nrow(sig_comparisons)) {
        report <- paste0(report, sprintf("  - %s (p %s)\n",
                                         sig_comparisons$comparison[i],
                                         format_p(sig_comparisons$p_adj[i])))
      }
    } else {
      report <- paste0(report, "\nCONCLUSION:\n")
      report <- paste0(report, "  No significant pairwise differences were found.\n")
    }
  }

  report <- paste0(report, paste(rep("=", 70), collapse = ""), "\n")

  return(report)
}

# =============================================================================
# MAIN FUNCTION
# =============================================================================

#' Generate Group Comparison Plots and Statistics
#'
#' @description
#' A comprehensive function for comparing groups using publication-ready
#' visualizations and statistical tests. Creates `ggbetweenstats` plots from
#' the ggstatsplot package with automatic test selection based on the number
#' of groups:
#'
#' \itemize{
#'   \item **2 groups**: t-test (parametric) or Mann-Whitney U (nonparametric)
#'   \item **3+ groups**: ANOVA (parametric) or Kruskal-Wallis (nonparametric)
#'     with post-hoc pairwise comparisons
#' }
#'
#' The function also supports Bayesian analysis, equivalence testing (TOST),
#' and stratified analyses using `repeat_category`.
#'
#' @param data A data frame containing the variables to analyze.
#' @param category Character. Name of the grouping variable (e.g., `"gender"`,
#'   `"treatment"`). This variable defines the groups to compare.
#' @param Vars A character vector of numeric variable names to compare across
#'   groups. Example: `c("score1", "score2", "reaction_time")`.
#' @param repeat_category Optional character. Name of a stratification variable.
#'   When provided, separate analyses are performed for each level (e.g., analyze
#'   gender differences separately for each country).
#' @param plots Logical. Generate ggstatsplot visualizations? Default: `TRUE`.
#' @param table Logical. Generate summary statistics table? Default: `TRUE`.
#' @param type Character. Type of statistical test to use. Options:
#'   \describe{
#'     \item{`"auto"` (default)}{Automatic selection: parametric tests}
#'     \item{`"parametric"`, `"p"`}{Parametric tests (t-test for 2 groups, ANOVA for 3+)}
#'     \item{`"nonparametric"`, `"np"`}{Nonparametric tests (Mann-Whitney/Kruskal-Wallis)}
#'     \item{`"kw"`}{Kruskal-Wallis (same as "np", for 3+ groups)}
#'     \item{`"mw"`}{Mann-Whitney (same as "np", for 2 groups)}
#'     \item{`"bayes"`, `"bf"`}{Bayesian tests with Bayes Factors}
#'   }
#' @param bayesian Logical. Compute Bayesian t-tests with Bayes Factors? Default: `FALSE`.
#'   Requires the BayesFactor package. Can also use `type = "bayes"`.
#' @param equivalence Logical. Perform equivalence testing (TOST)? Default: `FALSE`.
#'   Requires the TOSTER package.
#' @param equivalence_bounds Numeric vector of length 2. Equivalence bounds in
#'   Cohen's d units for TOST. Default: `c(-0.5, 0.5)`.
#' @param nonparametric Logical. Use nonparametric tests? Default: `FALSE`.
#'   When TRUE, uses Mann-Whitney U (2 groups) or Kruskal-Wallis (3+ groups).
#' @param p_adjust_method Method for multiple comparison correction. Options:
#'   `"none"`, `"bonferroni"`, `"holm"`, `"hochberg"`, `"BH"`, `"BY"`, `"fdr"`.
#'   Default: `"none"`. Applied to both tables and post-hoc tests.
#' @param posthoc Logical. Compute post-hoc pairwise comparisons for 3+ groups?
#'   Default: `TRUE`. Results include a comparison table and text report.
#' @param posthoc_method Method for post-hoc comparisons. Options:
#'   \itemize{
#'     \item `"games-howell"` (default): Does not assume equal variances
#'     \item `"tukey"`: Tukey's HSD, assumes equal variances
#'   }
#' @param pairwise_display Which pairwise comparisons to show on plots? Options:
#'   `"significant"` (default), `"all"`, `"none"`.
#' @param min_threshold Numeric. Minimum proportion (0-1) of total sample required
#'   to include a repeat_category level. Default: `0.05` (5%).
#' @param min_subcategory Integer. Minimum observations per group within
#'   repeat_category levels. Default: `5`.
#' @param colors Character vector of colors for groups. Default: `NULL` uses
#'   a built-in palette.
#' @param verbose Logical. Print progress messages? Default: `TRUE`.
#' @param combined_table Logical. When `repeat_category` is used, combine all
#'   results into a single table? Default: `TRUE`. The combined table shows
#'   all repeat_category levels together with bold highest means and red
#'   significant p-values.
#'
#' @return A list with class "comparison_results" containing:
#' \itemize{
#'   \item plots: Named list of ggplot objects, one per variable
#'   \item grid_plot: Combined plot grid (if multiple variables)
#'   \item summary_table: Formatted gt table with statistics
#'   \item summary_data: Data frame with raw statistics
#' }
#' For stratified analysis (with repeat_category), returns a named list
#' where each element corresponds to a level of repeat_category.
#'
#' @examples
#' \dontrun{
#' # ============================================================
#' # EXAMPLE 1: Basic Two-Group Comparison (t-test)
#' # ============================================================
#' data <- data.frame(
#'   gender = rep(c("Male", "Female"), each = 50),
#'   score = c(rnorm(50, mean = 100, sd = 15),
#'             rnorm(50, mean = 105, sd = 15))
#' )
#'
#' results <- compare_groups(
#'   data = data,
#'   category = "gender",
#'   Vars = c("score")
#' )
#'
#' # View the plot
#' results$plots$gender_vs_score
#'
#' # View summary statistics
#' results$summary_data
#'
#' # ============================================================
#' # EXAMPLE 2: Three-Group Comparison (ANOVA with post-hoc)
#' # ============================================================
#' data <- data.frame(
#'   country = rep(c("USA", "UK", "Germany"), each = 40),
#'   score1 = c(rnorm(40, 75, 10), rnorm(40, 70, 11), rnorm(40, 85, 9)),
#'   score2 = c(rnorm(40, 50, 8), rnorm(40, 55, 9), rnorm(40, 48, 7))
#' )
#'
#' results <- compare_groups(
#'   data = data,
#'   category = "country",
#'   Vars = c("score1", "score2"),
#'   posthoc = TRUE,
#'   posthoc_method = "tukey",
#'   pairwise_display = "all"
#' )
#'
#' # View post-hoc pairwise comparisons
#' results$summary_data$posthoc_results[[1]]
#'
#' # Print full ANOVA report
#' cat(results$summary_data$anova_report[1])
#'
#' # ============================================================
#' # EXAMPLE 3: Bayesian Analysis
#' # ============================================================
#' results <- compare_groups(
#'   data = data,
#'   category = "country",
#'   Vars = c("score1"),
#'   bayesian = TRUE
#' )
#'
#' # View Bayes Factors
#' results$summary_data[, c("country", "mean", "bf10", "bf_interpretation")]
#'
#' # ============================================================
#' # EXAMPLE 4: Nonparametric Tests
#' # ============================================================
#' results <- compare_groups(
#'   data = data,
#'   category = "country",
#'   Vars = c("score1"),
#'   nonparametric = TRUE  # Uses Kruskal-Wallis + pairwise Wilcoxon
#' )
#'
#' # ============================================================
#' # EXAMPLE 5: Stratified Analysis (separate tests per subgroup)
#' # ============================================================
#' data <- data.frame(
#'   country = rep(c("USA", "UK", "Germany"), each = 60),
#'   gender = rep(c("Male", "Female"), times = 90),
#'   score = rnorm(180, mean = 100, sd = 15)
#' )
#'
#' # Compare gender WITHIN each country
#' results <- compare_groups(
#'   data = data,
#'   category = "gender",
#'   Vars = c("score"),
#'   repeat_category = "country"
#' )
#'
#' # Access results by country
#' results$USA$summary_data
#' results$UK$plots$gender_vs_score
#' results$Germany$summary_table
#'
#' # ============================================================
#' # EXAMPLE 6: Equivalence Testing (TOST)
#' # ============================================================
#' results <- compare_groups(
#'   data = data,
#'   category = "gender",
#'   Vars = c("score"),
#'   equivalence = TRUE,
#'   equivalence_bounds = c(-0.3, 0.3)  # Small effect bounds
#' )
#'
#' # Check equivalence conclusions
#' results$summary_data[, c("gender", "mean", "equivalence_conclusion")]
#' }
#'
#' @seealso
#' \code{\link[ggstatsplot]{ggbetweenstats}} for the underlying plot function
#'
#' @importFrom dplyr group_by summarise mutate filter count select bind_rows any_of case_when ungroup first
#' @importFrom rlang sym .data
#' @importFrom ggplot2 ggplot aes scale_color_manual
#' @importFrom stats t.test aov sd var p.adjust pairwise.t.test pairwise.wilcox.test TukeyHSD as.formula kruskal.test wilcox.test
#'
#' @export
compare_groups <- function(data, category, Vars, repeat_category = NULL,
                                      repeat_levels = NULL,
                                      plots = TRUE, table = TRUE,
                                      type = "auto",
                                      bayesian = FALSE,
                                      equivalence = FALSE, equivalence_bounds = c(-0.5, 0.5),
                                      nonparametric = FALSE, p_adjust_method = "none",
                                      posthoc = TRUE, posthoc_method = "games-howell",
                                      posthoc_table = FALSE,
                                      pairwise_display = "significant",
                                      min_threshold = 0.05, min_subcategory = 5,
                                      colors = NULL, verbose = TRUE,
                                      combined_table = TRUE) {

  # ===========================================================================
  # Input Validation
  # ===========================================================================


  # Check for ggstatsplot (optional dependency)
  has_ggstatsplot <- requireNamespace("ggstatsplot", quietly = TRUE)
  if (plots && !has_ggstatsplot) {
    warning("Package 'ggstatsplot' is not installed. Plots will be disabled.\n",
            "Install with: install.packages('ggstatsplot') (requires R >= 4.3.0)",
            call. = FALSE)
    plots <- FALSE
  }

  if (!is.data.frame(data)) {
    stop("data must be a data frame")
  }

  if (missing(Vars) || is.null(Vars) || length(Vars) == 0) {
    stop("Vars parameter is required and must be a non-empty character vector")
  }

  if (!is.character(Vars)) {
    stop("Vars must be a character vector of variable names")
  }

  # ===========================================================================
  # Process 'type' parameter
  # ===========================================================================
  # Normalize type parameter
  type <- tolower(type)

  # Map shorthand to full names
  type_mapping <- list(
    # Parametric
    "parametric" = "parametric",
    "p" = "parametric",
    "param" = "parametric",
    "ttest" = "parametric",
    "t" = "parametric",
    "t-test" = "parametric",
    "anova" = "parametric",
    # Nonparametric
    "nonparametric" = "nonparametric",
    "np" = "nonparametric",
    "nonparam" = "nonparametric",
    "mann-whitney" = "nonparametric",
    "mw" = "nonparametric",
    "wilcox" = "nonparametric",
    "wilcoxon" = "nonparametric",
    "kruskal-wallis" = "nonparametric",
    "kruskal" = "nonparametric",
    "kw" = "nonparametric",
    # Bayesian
    "bayes" = "bayes",
    "bayesian" = "bayes",
    "bf" = "bayes",
    # Auto
    "auto" = "auto",
    "automatic" = "auto"
  )

  if (!type %in% names(type_mapping)) {
    stop("type must be one of: 'auto', 'parametric'/'p', 'nonparametric'/'np'/'kw'/'mw', 'bayes'")
  }

  resolved_type <- type_mapping[[type]]

  # Set flags based on resolved type
  if (resolved_type == "nonparametric") {
    nonparametric <- TRUE
  } else if (resolved_type == "bayes") {
    bayesian <- TRUE
    nonparametric <- FALSE
  } else if (resolved_type == "parametric") {
    nonparametric <- FALSE
  }
  # If "auto", use the nonparametric parameter as-is (default FALSE)

  # Validate p_adjust_method
  valid_methods <- c("none", "bonferroni", "holm", "hochberg", "BH", "BY", "fdr")
  if (!p_adjust_method %in% valid_methods) {
    stop("p_adjust_method must be one of: ", paste(valid_methods, collapse = ", "))
  }

  # Validate equivalence bounds
  if (equivalence) {
    if (length(equivalence_bounds) != 2 || equivalence_bounds[1] >= equivalence_bounds[2]) {
      stop("equivalence_bounds must be a numeric vector of length 2 with lower < upper")
    }
  }

  # Check variables exist
  missing_vars <- setdiff(Vars, names(data))
  if (length(missing_vars) > 0) {
    stop("Variables not found in data: ", paste(missing_vars, collapse = ", "))
  }

  # Handle category variable (expects quoted string)
  if (missing(category) || is.null(category)) {
    stop("'category' must be specified as a character string")
  }
  if (!is.character(category) || length(category) != 1) {
    stop("'category' must be a single character string (variable name)")
  }
  category_name_str <- category
  category_sym <- rlang::sym(category_name_str)

  if (!category_name_str %in% names(data)) {
    stop("Category variable '", category_name_str, "' not found in data")
  }

  # Handle repeat_category variable(s) (expects character string or vector)
  repeat_category_vars <- NULL
  if (!is.null(repeat_category)) {
    if (!is.character(repeat_category)) {
      stop("'repeat_category' must be a character string or vector of variable names")
    }
    repeat_category_vars <- repeat_category
  }

  # Validate all repeat_category variables exist
  if (!is.null(repeat_category_vars)) {
    missing_vars <- setdiff(repeat_category_vars, names(data))
    if (length(missing_vars) > 0) {
      stop("Repeat category variable(s) not found in data: ", paste(missing_vars, collapse = ", "))
    }

    # Create combined grouping variable if multiple
    if (length(repeat_category_vars) > 1) {
      data$.repeat_group <- apply(data[, repeat_category_vars, drop = FALSE], 1, paste, collapse = " | ")
      repeat_category_name_str <- ".repeat_group"
      # Store original variable names for labeling
      repeat_category_orig_vars <- repeat_category_vars
    } else {
      repeat_category_name_str <- repeat_category_vars
      repeat_category_orig_vars <- repeat_category_vars
    }
  } else {
    repeat_category_name_str <- NULL
    repeat_category_orig_vars <- NULL
  }

  # Set default colors
  if (is.null(colors)) {
    colors <- c("#EF476F", "#26547C", "#FFD166", "#06D6A0", "#118AB2", "#073B4C")
  }

  comparison_categories <- Vars

  # ===========================================================================
  # Check Required Packages
  # ===========================================================================
  required_pkgs <- c("ggstatsplot", "gt", "gridExtra")
  if (bayesian) required_pkgs <- c(required_pkgs, "BayesFactor")
  if (equivalence) required_pkgs <- c(required_pkgs, "TOSTER")
  if (!nonparametric) required_pkgs <- c(required_pkgs, "effsize", "effectsize")

  for (pkg in required_pkgs) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop("Package '", pkg, "' is required. Install with: install.packages('", pkg, "')")
    }
  }

  # ===========================================================================
  # Single Analysis Function
  # ===========================================================================
  run_single_analysis <- function(data_subset, group_label = NULL, silent = FALSE) {
    # When silent = TRUE, suppress all printing (for combined mode)
    result <- list()

    # -------------------------------------------------------------------------
    # Filter subcategories if needed
    # -------------------------------------------------------------------------
    if (!is.null(repeat_category_name_str) && min_subcategory > 0) {
      subcategory_counts <- data_subset %>%
        dplyr::count(.data[[category_name_str]]) %>%
        dplyr::filter(n >= min_subcategory)

      original_subcats <- unique(data_subset[[category_name_str]])
      kept_subcats <- subcategory_counts[[category_name_str]]
      excluded_subcats <- setdiff(original_subcats, kept_subcats)

      if (verbose && !silent && length(excluded_subcats) > 0) {
        cat("  Excluded subcategories (<", min_subcategory, "obs):",
            paste(excluded_subcats, collapse = ", "), "\n")
      }

      data_subset <- data_subset %>%
        dplyr::filter(.data[[category_name_str]] %in% kept_subcats)

      if (nrow(data_subset) < 2 || length(unique(data_subset[[category_name_str]])) < 2) {
        if (verbose && !silent) cat("  Warning: Insufficient data after filtering\n")
        return(list(
          plots = NULL, grid_plot = NULL, summary_table = NULL,
          summary_data = NULL, filtered_out = TRUE,
          reason = "Insufficient subcategory data"
        ))
      }
    }

    # -------------------------------------------------------------------------
    # Initialize storage
    # -------------------------------------------------------------------------
    all_plots <- list()
    summary_list <- list()

    # -------------------------------------------------------------------------
    # Process each variable
    # -------------------------------------------------------------------------
    for (comp_cat_name in comparison_categories) {
      plot_title <- if (is.null(group_label)) {
        paste0(category_name_str, " vs ", comp_cat_name)
      } else {
        paste0(category_name_str, " vs ", comp_cat_name, " (", group_label, ")")
      }

      comp_cat_sym <- rlang::sym(comp_cat_name)

      # -----------------------------------------------------------------------
      # Create Plot
      # -----------------------------------------------------------------------
      if (plots) {
        tryCatch({
          if (nrow(data_subset) < 2) {
            warning("Insufficient data for plot: ", plot_title)
            next
          }

          n_groups <- length(unique(data_subset[[category_name_str]]))
          if (n_groups < 2) {
            warning("Need at least 2 groups for: ", plot_title)
            next
          }

          if (all(is.na(data_subset[[comp_cat_name]]))) {
            warning("All NA values for: ", comp_cat_name)
            next
          }

          # Determine test type
          if (nonparametric) {
            test_type <- "nonparametric"
          } else if (bayesian) {
            test_type <- "bayes"
          } else {
            test_type <- "parametric"
          }

          p <- ggstatsplot::ggbetweenstats(
            data = data_subset,
            x = !!category_sym,
            y = !!comp_cat_sym,
            type = test_type,
            bf.message = bayesian,
            pairwise.display = if (n_groups > 2 && posthoc) pairwise_display else "none",
            p.adjust.method = if (p_adjust_method == "none") "bonferroni" else p_adjust_method,
            xlab = "",
            title = "",
            conf.level = 0.95
          ) +
            ggplot2::scale_color_manual(values = colors[1:n_groups])

          plot_name <- paste0(category_name_str, "_vs_", comp_cat_name)
          all_plots[[plot_name]] <- p
        }, error = function(e) {
          warning("Error creating plot for ", plot_title, ": ", e$message)
        })
      }

      # -----------------------------------------------------------------------
      # Calculate Statistics
      # -----------------------------------------------------------------------
      if (table) {
        tryCatch({
          if (nrow(data_subset) < 2 || all(is.na(data_subset[[comp_cat_name]]))) {
            next
          }

          # Basic summary statistics
          summary_stats <- data_subset %>%
            dplyr::group_by(!!category_sym) %>%
            dplyr::summarise(
              mean = mean(!!comp_cat_sym, na.rm = TRUE),
              sd = stats::sd(!!comp_cat_sym, na.rm = TRUE),
              n = dplyr::n(),
              .groups = 'drop'
            ) %>%
            dplyr::mutate(
              variable = comp_cat_name,
              category = category_name_str
            )

          # Initialize test results
          p_value <- NA
          efsz <- NA
          ef_type <- NA
          t_statistic <- NA
          t_df <- NA
          f_statistic <- NA
          df1 <- NA
          df2 <- NA
          test_type <- NA
          bf10 <- NA
          bf_interpretation <- NA
          tost_p <- NA
          equivalence_conclusion <- NA

          n_groups <- length(unique(data_subset[[category_name_str]]))

          # ===================================================================
          # Two-group comparisons
          # ===================================================================
          if (n_groups == 2 && nrow(data_subset) > 2) {
            groups <- unique(data_subset[[category_name_str]])
            group1_data <- data_subset[data_subset[[category_name_str]] == groups[1], comp_cat_name]
            group2_data <- data_subset[data_subset[[category_name_str]] == groups[2], comp_cat_name]

            group1_data <- group1_data[!is.na(group1_data)]
            group2_data <- group2_data[!is.na(group2_data)]

            if (length(group1_data) > 1 && length(group2_data) > 1) {

              # Parametric/Nonparametric test
              if (nonparametric) {
                tryCatch({
                  test_result <- stats::wilcox.test(group1_data, group2_data)
                  p_value <- test_result$p.value
                  t_statistic <- as.numeric(test_result$statistic)  # W statistic
                  test_type <- "Mann-Whitney U"

                  # Rank-biserial correlation as effect size
                  n1 <- length(group1_data)
                  n2 <- length(group2_data)
                  U <- test_result$statistic
                  efsz <- 1 - (2 * U) / (n1 * n2)  # Rank-biserial
                  ef_type <- "r_rb"
                }, error = function(e) {
                  warning("Error in Mann-Whitney test: ", e$message)
                })
              } else {
                tryCatch({
                  test_result <- stats::t.test(group1_data, group2_data)
                  p_value <- test_result$p.value
                  t_statistic <- as.numeric(test_result$statistic)
                  t_df <- as.numeric(test_result$parameter)
                  test_type <- "t-test"
                  efsz <- effsize::cohen.d(group1_data, group2_data)$estimate
                  ef_type <- "Cohen's d"
                }, error = function(e) {
                  warning("Error in t-test: ", e$message)
                })
              }

              # Bayesian t-test
              if (bayesian && !nonparametric) {
                tryCatch({
                  bf_result <- BayesFactor::ttestBF(group1_data, group2_data)
                  # Extract BF from the S4 object properly
                  bf_extracted <- BayesFactor::extractBF(bf_result)
                  bf10 <- bf_extracted$bf[1]
                  bf_interpretation <- interpret_bf(bf10)
                }, error = function(e) {
                  warning("Error in Bayesian t-test: ", e$message)
                })
              }

              # Equivalence test (TOST)
              if (equivalence && !nonparametric) {
                tryCatch({
                  psd <- pooled_sd(group1_data, group2_data)
                  low_bound <- equivalence_bounds[1] * psd
                  high_bound <- equivalence_bounds[2] * psd

                  # Use dataTOSTtwo if available, otherwise tsum_TOST
                  tost_result <- TOSTER::tsum_TOST(
                    m1 = mean(group1_data),
                    m2 = mean(group2_data),
                    sd1 = stats::sd(group1_data),
                    sd2 = stats::sd(group2_data),
                    n1 = length(group1_data),
                    n2 = length(group2_data),
                    low_eqbound = low_bound,
                    high_eqbound = high_bound,
                    alpha = 0.05,
                    eqbound_type = "raw"
                  )

                  # Get the larger TOST p-value
                  tost_p <- max(tost_result$TOST$p.value, na.rm = TRUE)

                  if (tost_p < 0.05) {
                    equivalence_conclusion <- "Equivalent"
                  } else if (!is.na(p_value) && p_value < 0.05) {
                    equivalence_conclusion <- "Different"
                  } else {
                    equivalence_conclusion <- "Inconclusive"
                  }
                }, error = function(e) {
                  warning("Error in equivalence test: ", e$message)
                })
              }
            }
          }

          # ===================================================================
          # Multi-group comparisons (>2 groups)
          # ===================================================================
          else if (n_groups > 2 && nrow(data_subset) > 2) {
            # Storage for ANOVA details
            anova_details <- list()
            posthoc_results <- NULL
            anova_text_report <- NULL

            if (nonparametric) {
              tryCatch({
                test_result <- stats::kruskal.test(
                  data_subset[[comp_cat_name]] ~ data_subset[[category_name_str]]
                )
                p_value <- test_result$p.value
                test_type <- "Kruskal-Wallis"

                # Epsilon squared effect size
                H <- test_result$statistic
                f_statistic <- as.numeric(H)  # H statistic
                df1 <- n_groups - 1
                n <- nrow(data_subset)
                efsz <- H / (n - 1)  # Epsilon squared
                ef_type <- "epsilon_sq"

                # Store ANOVA details
                anova_details <- list(
                  test = "Kruskal-Wallis",
                  statistic = H,
                  df = n_groups - 1,
                  p_value = p_value,
                  effect_size = efsz,
                  effect_type = "epsilon_sq"
                )

                # Post-hoc: Pairwise Wilcoxon tests
                if (posthoc) {
                  ph_method <- if (p_adjust_method == "none") "bonferroni" else p_adjust_method
                  posthoc_raw <- stats::pairwise.wilcox.test(
                    data_subset[[comp_cat_name]],
                    data_subset[[category_name_str]],
                    p.adjust.method = ph_method
                  )

                  # Convert to data frame
                  posthoc_results <- reshape_posthoc_matrix(posthoc_raw$p.value, comp_cat_name, ph_method)
                }
              }, error = function(e) {
                warning("Error in Kruskal-Wallis test: ", e$message)
              })
            } else {
              tryCatch({
                # Fit ANOVA
                formula_aov <- stats::as.formula(paste(comp_cat_name, "~", category_name_str))
                aov_result <- stats::aov(formula_aov, data = data_subset)
                aov_summary <- summary(aov_result)[[1]]

                p_value <- aov_summary[["Pr(>F)"]][1]
                f_statistic <- aov_summary[["F value"]][1]
                df1 <- aov_summary[["Df"]][1]
                df2 <- aov_summary[["Df"]][2]
                test_type <- "ANOVA"

                # Extract eta squared properly
                es_result <- effectsize::eta_squared(aov_result)
                efsz <- es_result$Eta2[1]
                ef_type <- "eta_sq"

                # Effect size interpretation
                ef_interp <- if (efsz >= 0.14) "large" else if (efsz >= 0.06) "medium" else "small"

                # Store ANOVA details
                anova_details <- list(
                  test = "One-way ANOVA",
                  statistic = f_statistic,
                  df1 = df1,
                  df2 = df2,
                  p_value = p_value,
                  effect_size = efsz,
                  effect_type = "eta_sq",
                  effect_interp = ef_interp
                )

                # Post-hoc comparisons
                if (posthoc) {
                  ph_method <- if (p_adjust_method == "none") "bonferroni" else p_adjust_method

                  if (posthoc_method == "tukey") {
                    # Tukey HSD
                    tukey_result <- stats::TukeyHSD(aov_result)
                    tukey_df <- as.data.frame(tukey_result[[1]])
                    tukey_df$comparison <- rownames(tukey_df)

                    posthoc_results <- data.frame(
                      variable = comp_cat_name,
                      comparison = tukey_df$comparison,
                      diff = tukey_df$diff,
                      ci_lower = tukey_df$lwr,
                      ci_upper = tukey_df$upr,
                      p_adj = tukey_df$`p adj`,
                      method = "Tukey HSD",
                      stringsAsFactors = FALSE
                    )
                  } else {
                    # Games-Howell (using pairwise t-test as approximation with pooled=FALSE)
                    posthoc_raw <- stats::pairwise.t.test(
                      data_subset[[comp_cat_name]],
                      data_subset[[category_name_str]],
                      p.adjust.method = ph_method,
                      pool.sd = FALSE  # Games-Howell doesn't assume equal variances
                    )
                    posthoc_results <- reshape_posthoc_matrix(posthoc_raw$p.value, comp_cat_name, ph_method)

                    # Add mean differences
                    group_means <- tapply(data_subset[[comp_cat_name]],
                                         data_subset[[category_name_str]], mean, na.rm = TRUE)
                    posthoc_results$diff <- sapply(1:nrow(posthoc_results), function(i) {
                      groups <- strsplit(posthoc_results$comparison[i], " vs ")[[1]]
                      if (length(groups) == 2 && all(groups %in% names(group_means))) {
                        group_means[groups[1]] - group_means[groups[2]]
                      } else NA
                    })
                  }

                  # Add significance stars
                  posthoc_results$significance <- sapply(posthoc_results$p_adj, function(p) {
                    if (is.na(p)) ""
                    else if (p < 0.001) "***"
                    else if (p < 0.01) "**"
                    else if (p < 0.05) "*"
                    else ""
                  })
                }

                # Generate text report
                if (posthoc && !is.null(posthoc_results)) {
                  anova_text_report <- generate_anova_text_report(
                    variable = comp_cat_name,
                    category = category_name_str,
                    data = data_subset,
                    anova_details = anova_details,
                    posthoc_results = posthoc_results
                  )
                }

              }, error = function(e) {
                warning("Error in ANOVA: ", e$message)
              })
            }

            # Store extra results for this variable
            if (exists("anova_details") && length(anova_details) > 0) {
              summary_stats$anova_details <- list(anova_details)
            }
            if (!is.null(posthoc_results)) {
              summary_stats$posthoc_results <- list(posthoc_results)
            }
            if (!is.null(anova_text_report)) {
              summary_stats$anova_report <- anova_text_report
            }
          }

          # Add results to summary
          summary_stats$p_value <- p_value
          summary_stats$efsz <- efsz
          summary_stats$ef_type <- ef_type
          summary_stats$test_type <- test_type
          summary_stats$t_statistic <- t_statistic
          summary_stats$t_df <- t_df
          summary_stats$f_statistic <- f_statistic
          summary_stats$df1 <- df1
          summary_stats$df2 <- df2
          summary_stats$bf10 <- bf10
          summary_stats$bf_interpretation <- bf_interpretation
          summary_stats$tost_p <- tost_p
          summary_stats$equivalence_conclusion <- equivalence_conclusion

          summary_list[[comp_cat_name]] <- summary_stats
        }, error = function(e) {
          warning("Error calculating statistics for ", comp_cat_name, ": ", e$message)
        })
      }
    }

    # -------------------------------------------------------------------------
    # Combine summary data (more efficient than rbind in loop)
    # -------------------------------------------------------------------------
    summary_data <- if (length(summary_list) > 0) {
      dplyr::bind_rows(summary_list)
    } else {
      data.frame()
    }

    # -------------------------------------------------------------------------
    # Apply p-value adjustment
    # -------------------------------------------------------------------------
    if (p_adjust_method != "none" && nrow(summary_data) > 0) {
      # Get unique p-values (one per variable)
      p_values_by_var <- summary_data %>%
        dplyr::group_by(variable) %>%
        dplyr::summarise(p_value = dplyr::first(p_value), .groups = 'drop')

      adjusted_p <- stats::p.adjust(p_values_by_var$p_value, method = p_adjust_method)

      # Merge back
      p_values_by_var$p_adjusted <- adjusted_p
      summary_data <- merge(summary_data, p_values_by_var[, c("variable", "p_adjusted")],
                           by = "variable", all.x = TRUE)
    }

    # -------------------------------------------------------------------------
    # Create Plots Grid (only when not silent)
    # -------------------------------------------------------------------------
    if (plots && length(all_plots) > 0) {
      result$plots <- all_plots

      # Create grid plot (use arrangeGrob to avoid immediate display)
      n_plots <- length(all_plots)
      n_cols <- if (n_plots == 1) 1 else if (n_plots <= 4) 2 else if (n_plots <= 6) 3 else 4

      tryCatch({
        valid_plots <- all_plots[sapply(all_plots, function(x) {
          !is.null(x) && inherits(x, "ggplot")
        })]

        if (length(valid_plots) > 0) {
          # Use arrangeGrob (doesn't display) instead of grid.arrange
          result$grid_plot <- do.call(
            gridExtra::arrangeGrob,
            c(valid_plots, ncol = n_cols, top = group_label)
          )
          # Only display when not silent
          if (!silent && verbose) {
            gridExtra::grid.arrange(result$grid_plot)
          }
        }
      }, error = function(e) {
        warning("Error creating grid plot: ", e$message)
        result$grid_plot <- NULL
      })
    }

    # -------------------------------------------------------------------------
    # Create Summary Table
    # -------------------------------------------------------------------------
    if (table && nrow(summary_data) > 0) {
      result$summary_data <- summary_data

      tryCatch({
        formatted_table <- summary_data %>%
          dplyr::group_by(variable) %>%
          dplyr::mutate(
            mean_formatted = sprintf("%.2f", mean),
            sd_formatted = sprintf("%.2f", sd),
            ES_formatted = ifelse(is.na(efsz), "NA", sprintf("%.2f", efsz)),
            is_highest = mean == max(mean, na.rm = TRUE),
            p_formatted = dplyr::case_when(
              is.na(dplyr::first(p_value[!is.na(p_value)])) ~ "NA",
              dplyr::first(p_value[!is.na(p_value)]) < 0.001 ~ "< 0.001",
              TRUE ~ sprintf("%.3f", dplyr::first(p_value[!is.na(p_value)]))
            )
          ) %>%
          dplyr::ungroup()

        # Add adjusted p-values if applicable
        if ("p_adjusted" %in% names(summary_data)) {
          formatted_table <- formatted_table %>%
            dplyr::group_by(variable) %>%
            dplyr::mutate(
              p_adj_formatted = dplyr::case_when(
                is.na(dplyr::first(p_adjusted[!is.na(p_adjusted)])) ~ "NA",
                dplyr::first(p_adjusted[!is.na(p_adjusted)]) < 0.001 ~ "< 0.001",
                TRUE ~ sprintf("%.3f", dplyr::first(p_adjusted[!is.na(p_adjusted)]))
              )
            ) %>%
            dplyr::ungroup()
        }

        # Add Bayesian columns
        if (bayesian && "bf10" %in% names(summary_data)) {
          formatted_table <- formatted_table %>%
            dplyr::group_by(variable) %>%
            dplyr::mutate(
              bf_formatted = dplyr::case_when(
                is.na(dplyr::first(bf10[!is.na(bf10)])) ~ "NA",
                dplyr::first(bf10[!is.na(bf10)]) > 1000 ~ "> 1000",
                dplyr::first(bf10[!is.na(bf10)]) < 0.001 ~ "< 0.001",
                TRUE ~ sprintf("%.2f", dplyr::first(bf10[!is.na(bf10)]))
              ),
              bf_interp_formatted = dplyr::first(bf_interpretation[!is.na(bf_interpretation)])
            ) %>%
            dplyr::ungroup()
        }

        # Add equivalence columns
        if (equivalence && "tost_p" %in% names(summary_data)) {
          formatted_table <- formatted_table %>%
            dplyr::group_by(variable) %>%
            dplyr::mutate(
              tost_formatted = dplyr::case_when(
                is.na(dplyr::first(tost_p[!is.na(tost_p)])) ~ "NA",
                dplyr::first(tost_p[!is.na(tost_p)]) < 0.001 ~ "< 0.001",
                TRUE ~ sprintf("%.3f", dplyr::first(tost_p[!is.na(tost_p)]))
              ),
              equiv_concl_formatted = dplyr::first(equivalence_conclusion[!is.na(equivalence_conclusion)])
            ) %>%
            dplyr::ungroup()
        }

        # Build display columns
        display_cols <- c("Variable" = "variable")
        display_cols[category_name_str] <- category_name_str
        display_cols <- c(display_cols,
                         "Mean" = "mean_formatted",
                         "SD" = "sd_formatted",
                         "N" = "n",
                         "ES" = "ES_formatted",
                         "P-value" = "p_formatted")

        if ("p_adj_formatted" %in% names(formatted_table)) {
          display_cols <- c(display_cols, "P-adj" = "p_adj_formatted")
        }

        if (bayesian && "bf_formatted" %in% names(formatted_table)) {
          display_cols <- c(display_cols,
                           "BF10" = "bf_formatted",
                           "BF Interpretation" = "bf_interp_formatted")
        }

        if (equivalence && "tost_formatted" %in% names(formatted_table)) {
          display_cols <- c(display_cols,
                           "TOST p" = "tost_formatted",
                           "Equivalence" = "equiv_concl_formatted")
        }

        # Create final table - select by OLD column names (values of display_cols)
        final_table <- formatted_table %>%
          dplyr::select(dplyr::any_of(c(unname(display_cols), "is_highest")))

        # Rename columns
        col_names <- names(final_table)
        for (i in seq_along(display_cols)) {
          col_names[col_names == display_cols[i]] <- names(display_cols)[i]
        }
        names(final_table) <- col_names

        # Build subtitle
        test_info <- if (nonparametric) "Nonparametric tests" else "Parametric tests"
        subtitle_parts <- c(test_info)
        if (bayesian) subtitle_parts <- c(subtitle_parts, "Bayesian analysis")
        if (equivalence) subtitle_parts <- c(subtitle_parts,
                                             paste0("TOST (", equivalence_bounds[1],
                                                    " to ", equivalence_bounds[2], ")"))
        if (p_adjust_method != "none") subtitle_parts <- c(subtitle_parts,
                                                           paste0("p-adjust: ", p_adjust_method))

        table_title <- if (is.null(group_label)) {
          paste0("Summary: ", category_name_str, " Comparisons")
        } else {
          paste0("Summary: ", category_name_str, " (", group_label, ")")
        }

        # Create gt table
        gt_table <- final_table %>%
          dplyr::select(-is_highest) %>%
          gt::gt() %>%
          gt::tab_header(
            title = table_title,
            subtitle = paste(subtitle_parts, collapse = " | ")
          ) %>%
          gt::tab_style(
            style = gt::cell_text(weight = "bold"),
            locations = gt::cells_column_labels()
          ) %>%
          gt::tab_style(
            style = gt::cell_text(weight = "bold"),
            locations = gt::cells_body(columns = "Variable")
          ) %>%
          gt::cols_align(align = "center") %>%
          gt::tab_options(
            table.font.size = gt::px(12),
            heading.title.font.size = gt::px(14),
            heading.subtitle.font.size = gt::px(11),
            column_labels.font.weight = "bold"
          )

        # Highlight significant p-values
        if ("P-value" %in% names(final_table)) {
          sig_rows <- which(
            final_table$`P-value` == "< 0.001" |
              (suppressWarnings(as.numeric(final_table$`P-value`)) < 0.05 &
                 !is.na(suppressWarnings(as.numeric(final_table$`P-value`))))
          )
          if (length(sig_rows) > 0) {
            gt_table <- gt_table %>%
              gt::tab_style(
                style = gt::cell_text(color = "red"),
                locations = gt::cells_body(columns = "P-value", rows = sig_rows)
              )
          }
        }

        # Highlight highest means
        highest_rows <- which(final_table$is_highest)
        if (length(highest_rows) > 0) {
          # Get columns that exist in the table for highlighting
          highlight_cols <- intersect(c(category_name_str, "Mean", "SD", "N"),
                                      names(final_table))
          if (length(highlight_cols) > 0) {
            gt_table <- gt_table %>%
              gt::tab_style(
                style = gt::cell_text(weight = "bold"),
                locations = gt::cells_body(
                  columns = highlight_cols,
                  rows = highest_rows
                )
              )
          }
        }

        result$summary_table <- gt_table

        if (verbose && !silent) {
          cat("\n")
          print(result$summary_table)
          cat("\n")
        }
      }, error = function(e) {
        warning("Error creating table: ", e$message)
        result$summary_table <- summary_data
        if (verbose && !silent) print(summary_data)
      })
    }

    return(result)
  }

  # ===========================================================================
  # Main Logic
  # ===========================================================================
  if (is.null(repeat_category_name_str)) {
    # No repeat category - single analysis
    return(run_single_analysis(data))
  } else {
    # Stratified analysis
    total_n <- nrow(data)
    category_counts <- data %>%
      dplyr::count(.data[[repeat_category_name_str]]) %>%
      dplyr::mutate(
        percentage = n / total_n,
        above_threshold = percentage >= min_threshold
      ) %>%
      dplyr::arrange(dplyr::desc(percentage))

    # If repeat_levels specified, use those; otherwise use threshold filtering
    if (!is.null(repeat_levels)) {
      # Validate that specified levels exist in data
      available_levels <- unique(data[[repeat_category_name_str]])
      invalid_levels <- setdiff(repeat_levels, available_levels)
      if (length(invalid_levels) > 0) {
        warning("repeat_levels not found in data: ", paste(invalid_levels, collapse = ", "))
      }
      categories_to_include <- intersect(repeat_levels, available_levels)
      excluded_categories <- setdiff(available_levels, categories_to_include)
    } else {
      categories_to_include <- category_counts %>%
        dplyr::filter(above_threshold) %>%
        dplyr::pull(.data[[repeat_category_name_str]])

      excluded_categories <- category_counts %>%
        dplyr::filter(!above_threshold) %>%
        dplyr::pull(.data[[repeat_category_name_str]])
    }

    if (length(categories_to_include) == 0) {
      warning("No categories to analyze. Check repeat_levels or min_threshold.")
      return(list())
    }

    # Run analysis for each category (suppress individual output when combined_table = TRUE)
    results_by_group <- list()
    successful <- 0
    all_summary_data <- list()
    all_plots <- list()
    all_stats <- list()

    for (repeat_val in categories_to_include) {
      filtered_data <- data %>%
        dplyr::filter(.data[[repeat_category_name_str]] == repeat_val)

      # Run analysis silently when combined_table = TRUE
      analysis_result <- run_single_analysis(filtered_data, repeat_val, silent = combined_table)

      if (is.null(analysis_result$filtered_out) || !analysis_result$filtered_out) {
        successful <- successful + 1
        # Collect summary data for combined table
        if (!is.null(analysis_result$summary_data) && nrow(analysis_result$summary_data) > 0) {
          analysis_result$summary_data[[repeat_category_name_str]] <- repeat_val
          all_summary_data[[as.character(repeat_val)]] <- analysis_result$summary_data

          # Collect statistics for clean output
          for (var in comparison_categories) {
            var_data <- analysis_result$summary_data[analysis_result$summary_data$variable == var, ]
            if (nrow(var_data) > 0) {
              p_val <- var_data$p_value[1]
              ef <- var_data$efsz[1]
              ef_type <- var_data$ef_type[1]
              n_total <- sum(var_data$n)
              ttype <- var_data$test_type[1]
              t_stat <- var_data$t_statistic[1]
              t_df_val <- var_data$t_df[1]
              f_stat <- var_data$f_statistic[1]
              df1_val <- var_data$df1[1]
              df2_val <- var_data$df2[1]

              # Get post-hoc results if available
              posthoc_res <- NULL
              if (!is.null(var_data$posthoc_results) && length(var_data$posthoc_results) > 0) {
                posthoc_res <- var_data$posthoc_results[[1]]
              }

              all_stats[[length(all_stats) + 1]] <- list(
                group = repeat_val,
                variable = var,
                n = n_total,
                test_type = ttype,
                t_statistic = t_stat,
                t_df = t_df_val,
                f_statistic = f_stat,
                df1 = df1_val,
                df2 = df2_val,
                p_value = p_val,
                effect_size = ef,
                effect_type = ef_type,
                posthoc = posthoc_res
              )
            }
          }
        }

        # Collect plots for combined grid
        if (!is.null(analysis_result$plots)) {
          for (plot_name in names(analysis_result$plots)) {
            new_name <- paste0(repeat_val, "_", plot_name)
            all_plots[[new_name]] <- analysis_result$plots[[plot_name]]
          }
        }
      }

      results_by_group[[as.character(repeat_val)]] <- analysis_result
    }

    # Create combined table if requested
    if (combined_table && length(all_summary_data) > 0) {
      combined_data <- dplyr::bind_rows(all_summary_data)

      # Create combined formatted table
      tryCatch({
        formatted_combined <- combined_data %>%
          dplyr::group_by(.data[[repeat_category_name_str]], variable) %>%
          dplyr::mutate(
            mean_formatted = sprintf("%.2f", mean),
            sd_formatted = sprintf("%.2f", sd),
            ES_formatted = ifelse(is.na(efsz), "NA", sprintf("%.2f", efsz)),
            is_highest = mean == max(mean, na.rm = TRUE),
            p_formatted = dplyr::case_when(
              is.na(dplyr::first(p_value[!is.na(p_value)])) ~ "NA",
              dplyr::first(p_value[!is.na(p_value)]) < 0.001 ~ "< 0.001",
              TRUE ~ sprintf("%.3f", dplyr::first(p_value[!is.na(p_value)]))
            )
          ) %>%
          dplyr::ungroup()

        # Determine display name for repeat_category column
        repeat_col_display <- if (length(repeat_category_orig_vars) > 1) {
          paste(repeat_category_orig_vars, collapse = " x ")
        } else {
          repeat_category_name_str
        }

        # Build display table
        display_combined <- formatted_combined %>%
          dplyr::select(
            dplyr::all_of(c(repeat_category_name_str, "variable", category_name_str)),
            Mean = mean_formatted,
            SD = sd_formatted,
            N = n,
            ES = ES_formatted,
            `P-value` = p_formatted,
            is_highest
          )

        # Rename columns properly
        names(display_combined)[1] <- repeat_col_display
        names(display_combined)[2] <- "Variable"
        names(display_combined)[3] <- category_name_str

        # Identify rows with highest mean for bold formatting
        highest_rows <- which(display_combined$is_highest)

        # Identify significant p-values for red coloring
        sig_rows <- which(
          display_combined$`P-value` == "< 0.001" |
            (suppressWarnings(as.numeric(display_combined$`P-value`)) < 0.05 &
               !is.na(suppressWarnings(as.numeric(display_combined$`P-value`))))
        )

        # Build subtitle
        test_info <- if (nonparametric) "Nonparametric tests" else "Parametric tests"
        subtitle_parts <- c(test_info)
        if (bayesian) subtitle_parts <- c(subtitle_parts, "Bayesian analysis")
        if (p_adjust_method != "none") subtitle_parts <- c(subtitle_parts, paste0("p-adjust: ", p_adjust_method))

        # Create gt table
        gt_combined <- display_combined %>%
          dplyr::select(-is_highest) %>%
          gt::gt() %>%
          gt::tab_header(
            title = paste0("Comparison: ", category_name_str, " by ", repeat_col_display),
            subtitle = paste(subtitle_parts, collapse = " | ")
          ) %>%
          gt::tab_style(
            style = list(
              gt::cell_text(weight = "bold"),
              gt::cell_borders(sides = "top", color = "black", weight = gt::px(2)),
              gt::cell_borders(sides = "bottom", color = "black", weight = gt::px(1))
            ),
            locations = gt::cells_column_labels()
          ) %>%
          gt::tab_style(
            style = gt::cell_text(weight = "bold"),
            locations = gt::cells_title(groups = "title")
          ) %>%
          gt::cols_align(align = "center") %>%
          gt::cols_align(align = "left", columns = c(repeat_col_display, "Variable")) %>%
          gt::tab_options(
            table.border.top.width = gt::px(2),
            table.border.top.color = "black",
            table.border.bottom.width = gt::px(2),
            table.border.bottom.color = "black",
            column_labels.border.bottom.color = "black",
            table_body.border.bottom.color = "black"
          )

        # Bold highest mean rows
        if (length(highest_rows) > 0) {
          gt_combined <- gt_combined %>%
            gt::tab_style(
              style = gt::cell_text(weight = "bold"),
              locations = gt::cells_body(rows = highest_rows)
            )
        }

        # Red significant p-values
        if (length(sig_rows) > 0) {
          gt_combined <- gt_combined %>%
            gt::tab_style(
              style = gt::cell_text(color = "red"),
              locations = gt::cells_body(columns = "P-value", rows = sig_rows)
            )
        }

        results_by_group$combined_table <- gt_combined
        results_by_group$combined_data <- combined_data
      }, error = function(e) {
        warning("Error creating combined table: ", e$message)
      })
    }

    # Create combined plot grid
    if (plots && length(all_plots) > 0) {
      results_by_group$all_plots <- all_plots

      tryCatch({
        valid_plots <- all_plots[sapply(all_plots, function(x) {
          !is.null(x) && inherits(x, "ggplot")
        })]

        if (length(valid_plots) > 0) {
          n_plots <- length(valid_plots)
          n_cols <- if (n_plots == 1) 1 else if (n_plots <= 4) 2 else if (n_plots <= 6) 3 else 4

          # Use arrangeGrob to create without displaying
          results_by_group$combined_plot <- do.call(
            gridExtra::arrangeGrob,
            c(valid_plots, ncol = n_cols)
          )
        }
      }, error = function(e) {
        warning("Error creating combined plot: ", e$message)
      })
    }

    # Create post-hoc table if requested (before verbose block so it's available for printing)
    if (posthoc_table && posthoc) {
      # Collect all post-hoc results
      all_posthoc <- data.frame()
      for (stat in all_stats) {
        if (!is.null(stat$posthoc) && nrow(stat$posthoc) > 0) {
          ph_data <- stat$posthoc
          ph_data$Group <- stat$group
          ph_data$Variable <- stat$variable
          all_posthoc <- rbind(all_posthoc, ph_data)
        }
      }

      if (nrow(all_posthoc) > 0) {
        # Format the post-hoc table
        posthoc_display <- all_posthoc %>%
          dplyr::select(
            Group,
            Variable = variable,
            Comparison = comparison,
            Difference = diff,
            `P-adj` = p_adj
          ) %>%
          dplyr::mutate(
            Difference = ifelse(is.na(Difference), "-", sprintf("%.2f", Difference)),
            `P-adj` = dplyr::case_when(
              is.na(`P-adj`) ~ "NA",
              `P-adj` < 0.001 ~ "< .001",
              TRUE ~ sprintf("%.3f", `P-adj`)
            ),
            Sig = dplyr::case_when(
              `P-adj` == "NA" ~ "",
              `P-adj` == "< .001" ~ "***",
              suppressWarnings(as.numeric(`P-adj`)) < 0.01 ~ "**",
              suppressWarnings(as.numeric(`P-adj`)) < 0.05 ~ "*",
              TRUE ~ ""
            )
          )

        # Identify significant rows
        sig_rows <- which(posthoc_display$Sig != "")

        # Create gt table
        gt_posthoc <- posthoc_display %>%
          gt::gt() %>%
          gt::tab_header(
            title = "Post-hoc Pairwise Comparisons",
            subtitle = paste0("Method: ", posthoc_method, " | Adjustment: ",
                             if (p_adjust_method == "none") "bonferroni" else p_adjust_method)
          ) %>%
          gt::tab_style(
            style = list(
              gt::cell_text(weight = "bold"),
              gt::cell_borders(sides = "bottom", color = "black", weight = gt::px(2))
            ),
            locations = gt::cells_column_labels()
          ) %>%
          gt::cols_align(align = "center") %>%
          gt::cols_align(align = "left", columns = c("Group", "Variable", "Comparison")) %>%
          gt::tab_options(
            table.border.top.width = gt::px(2),
            table.border.top.color = "black",
            table.border.bottom.width = gt::px(2),
            table.border.bottom.color = "black"
          )

        # Highlight significant rows
        if (length(sig_rows) > 0) {
          gt_posthoc <- gt_posthoc %>%
            gt::tab_style(
              style = gt::cell_text(weight = "bold"),
              locations = gt::cells_body(rows = sig_rows)
            ) %>%
            gt::tab_style(
              style = gt::cell_text(color = "red"),
              locations = gt::cells_body(columns = "P-adj", rows = sig_rows)
            )
        }

        results_by_group$posthoc_table <- gt_posthoc
      }
    }

    # Print clean output
    if (verbose) {
      # Print header
      cat("\n")
      cat(paste(rep("=", 60), collapse = ""), "\n")
      # Use original variable names for display if multiple repeat_category vars
      repeat_display_name <- if (length(repeat_category_orig_vars) > 1) {
        paste(repeat_category_orig_vars, collapse = " x ")
      } else {
        repeat_category_name_str
      }
      cat("  COMPARISON ANALYSIS: ", category_name_str, " by ", repeat_display_name, "\n", sep = "")
      cat(paste(rep("=", 60), collapse = ""), "\n\n")

      # Print sample info
      cat("Sample: N =", total_n, "\n")
      cat("Groups:", paste(categories_to_include, collapse = ", "), "\n")
      cat("Variables:", paste(comparison_categories, collapse = ", "), "\n")
      cat("Test:", if (nonparametric) "Nonparametric (Mann-Whitney/Kruskal-Wallis)" else "Parametric (t-test/ANOVA)", "\n\n")

      # Print statistical results
      cat(paste(rep("-", 70), collapse = ""), "\n")
      cat("  STATISTICAL RESULTS\n")
      cat(paste(rep("-", 70), collapse = ""), "\n\n")

      for (stat in all_stats) {
        sig_marker <- if (!is.na(stat$p_value) && stat$p_value < 0.05) "*" else ""
        p_str <- if (is.na(stat$p_value)) "NA" else if (stat$p_value < 0.001) "< .001" else sprintf("%.3f", stat$p_value)
        ef_str <- if (is.na(stat$effect_size)) "NA" else sprintf("%.3f", stat$effect_size)

        # Format test statistic based on test type
        if (!is.na(stat$test_type) && stat$test_type == "t-test") {
          test_str <- sprintf("t(%.1f) = %.2f", stat$t_df, stat$t_statistic)
        } else if (!is.na(stat$test_type) && stat$test_type == "Mann-Whitney U") {
          test_str <- sprintf("W = %.0f", stat$t_statistic)
        } else if (!is.na(stat$test_type) && stat$test_type == "ANOVA") {
          test_str <- sprintf("F(%d, %d) = %.2f", as.integer(stat$df1), as.integer(stat$df2), stat$f_statistic)
        } else if (!is.na(stat$test_type) && stat$test_type == "Kruskal-Wallis") {
          test_str <- sprintf("H(%d) = %.2f", as.integer(stat$df1), stat$f_statistic)
        } else {
          test_str <- "NA"
        }

        cat(sprintf("  %s: %s\n", stat$group, stat$variable))
        cat(sprintf("    %s, p = %s, %s = %s %s\n",
                    test_str, p_str, stat$effect_type, ef_str, sig_marker))

        # Print significant post-hoc comparisons if available
        if (!is.null(stat$posthoc) && nrow(stat$posthoc) > 0) {
          sig_posthoc <- stat$posthoc[stat$posthoc$p_adj < 0.05, , drop = FALSE]
          if (nrow(sig_posthoc) > 0) {
            # Format each significant comparison
            ph_strs <- sapply(1:nrow(sig_posthoc), function(i) {
              comp <- sig_posthoc$comparison[i]
              p_adj <- sig_posthoc$p_adj[i]
              diff_val <- if ("diff" %in% names(sig_posthoc)) sig_posthoc$diff[i] else NA

              # Determine direction from diff or comparison
              if (!is.na(diff_val) && diff_val != 0) {
                groups <- strsplit(comp, " vs ")[[1]]
                if (length(groups) == 2) {
                  if (diff_val > 0) {
                    comp_str <- paste0(groups[1], " > ", groups[2])
                  } else {
                    comp_str <- paste0(groups[2], " > ", groups[1])
                  }
                } else {
                  comp_str <- comp
                }
              } else {
                comp_str <- comp
              }

              p_str_ph <- if (p_adj < 0.001) "< .001" else sprintf("%.3f", p_adj)
              sprintf("%s (p = %s)", comp_str, p_str_ph)
            })
            cat(sprintf("    Post-hoc: %s\n", paste(ph_strs, collapse = ", ")))
          }
        }
        cat("\n")
      }

      cat("  * p < .05\n")
      cat(paste(rep("=", 70), collapse = ""), "\n\n")

      # Print combined table
      if (!is.null(results_by_group$combined_table)) {
        print(results_by_group$combined_table)
        cat("\n")
      }

      # Print post-hoc table if it exists
      if (!is.null(results_by_group$posthoc_table)) {
        cat("\n")
        print(results_by_group$posthoc_table)
        cat("\n*** p < .001, ** p < .01, * p < .05\n")
      }

      # Display combined plot
      if (!is.null(results_by_group$combined_plot)) {
        gridExtra::grid.arrange(results_by_group$combined_plot)
      }
    }

    # Add metadata
    results_by_group$metadata <- list(
      total_observations = total_n,
      min_threshold = min_threshold,
      category_counts = category_counts,
      included_categories = categories_to_include,
      excluded_categories = excluded_categories,
      variables_analyzed = comparison_categories,
      test_type = if (nonparametric) "nonparametric" else "parametric",
      bayesian_analysis = bayesian,
      equivalence_testing = equivalence,
      equivalence_bounds = if (equivalence) equivalence_bounds else NULL,
      p_adjust_method = p_adjust_method
    )

    return(results_by_group)
  }
}
