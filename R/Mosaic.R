# Saqrmisc Package: Mosaic Analysis Functions
#
# This file contains functions for categorical variable analysis using mosaic plots.
# Functions: mosaic_analysis

#' @importFrom vcd mosaic labeling_values
#' @importFrom grid gpar
#' @importFrom dplyr select bind_rows mutate everything
#' @importFrom tibble tibble
#' @importFrom stats chisq.test fisher.test as.formula
NULL

#' Create consolidated frequency table (internal helper)
#'
#' @param observed Observed frequency table
#' @param expected Expected frequency table
#' @param show_percentages Logical for showing percentages
#' @param percentage_base Base for percentage calculation
#' @return A tibble with consolidated results
#' @noRd
create_consolidated_table <- function(observed, expected, show_percentages = TRUE, percentage_base = "total") {


  # Convert to data frames
  obs_df <- as.data.frame.matrix(observed)
  exp_df <- as.data.frame.matrix(expected)

  # Calculate percentages if requested
  pct_df <- NULL
  if (show_percentages) {
    if (percentage_base == "total") {
      pct_df <- as.data.frame.matrix(round(observed / sum(observed) * 100, 1))
    } else if (percentage_base == "row") {
      pct_df <- as.data.frame.matrix(round(observed / rowSums(observed) * 100, 1))
    } else if (percentage_base == "column") {
      pct_df <- as.data.frame.matrix(round(t(t(observed) / colSums(observed)) * 100, 1))
    }
  }

  # Create consolidated table
  consolidated <- tibble::tibble()

  for (i in 1:nrow(obs_df)) {
    row_name <- rownames(obs_df)[i]

    # Observed row
    obs_row <- tibble::tibble(
      Variable = row_name,
      Type = "Observed",
      !!!obs_df[i, ],
      Total = sum(obs_df[i, ])
    )

    # Expected row
    exp_row <- tibble::tibble(
      Variable = "",
      Type = "Expected",
      !!!round(exp_df[i, ], 1),
      Total = round(sum(exp_df[i, ]), 1)
    )

    # Percentage row (if requested)
    if (show_percentages && !is.null(pct_df)) {
      pct_row <- tibble::tibble(
        Variable = "",
        Type = paste0("% (", percentage_base, ")"),
        !!!pct_df[i, ],
        Total = if (percentage_base == "row") 100.0 else round(sum(pct_df[i, ]), 1)
      )
      consolidated <- dplyr::bind_rows(consolidated, obs_row, exp_row, pct_row)
    } else {
      consolidated <- dplyr::bind_rows(consolidated, obs_row, exp_row)
    }
  }

  # Add totals row
  total_obs <- tibble::tibble(
    Variable = "Total",
    Type = "Observed",
    !!!colSums(obs_df),
    Total = sum(obs_df)
  )

  total_exp <- tibble::tibble(
    Variable = "",
    Type = "Expected",
    !!!round(colSums(exp_df), 1),
    Total = round(sum(exp_df), 1)
  )

  if (show_percentages && !is.null(pct_df)) {
    if (percentage_base == "total") {
      total_pct <- tibble::tibble(
        Variable = "",
        Type = paste0("% (", percentage_base, ")"),
        !!!round(colSums(observed) / sum(observed) * 100, 1),
        Total = 100.0
      )
    } else if (percentage_base == "column") {
      total_pct <- tibble::tibble(
        Variable = "",
        Type = paste0("% (", percentage_base, ")"),
        !!!stats::setNames(rep(100.0, ncol(observed)), colnames(observed)),
        Total = 100.0
      )
    } else {
      total_pct <- tibble::tibble(
        Variable = "",
        Type = paste0("% (", percentage_base, ")"),
        !!!round(colSums(pct_df), 1),
        Total = round(sum(pct_df), 1)
      )
    }
    consolidated <- dplyr::bind_rows(consolidated, total_obs, total_exp, total_pct)
  } else {
    consolidated <- dplyr::bind_rows(consolidated, total_obs, total_exp)
  }

  return(consolidated)
}

#' Interpret Cramer's V effect size
#'
#' @param v Cramer's V value
#' @param df Degrees of freedom (min of rows-1, cols-1)
#' @return Character string with interpretation
#' @noRd
interpret_cramers_v <- function(v, df) {
  # Cohen's guidelines adjusted for df
  # For df=1: small=0.10, medium=0.30, large=0.50
  # For df=2: small=0.07, medium=0.21, large=0.35
  # For df=3: small=0.06, medium=0.17, large=0.29
  # For df>=4: small=0.05, medium=0.15, large=0.25

  if (df == 1) {
    thresholds <- c(small = 0.10, medium = 0.30, large = 0.50)
  } else if (df == 2) {
    thresholds <- c(small = 0.07, medium = 0.21, large = 0.35)
  } else if (df == 3) {
    thresholds <- c(small = 0.06, medium = 0.17, large = 0.29)
  } else {
    thresholds <- c(small = 0.05, medium = 0.15, large = 0.25)
  }

  if (is.na(v)) return("NA")
  if (v < thresholds["small"]) return("negligible")
  if (v < thresholds["medium"]) return("small")
  if (v < thresholds["large"]) return("medium")
  return("large")
}

#' Perform a comprehensive mosaic plot analysis
#'
#' @description
#' This function conducts a complete mosaic plot analysis for two categorical variables.
#' It includes filtering by minimum count, chi-square testing (with optional Fisher's exact test),
#' calculation of Cramer's V with effect size interpretation, and generation of a detailed
#' summary table with observed/expected counts and residuals.
#'
#' @param data A data frame containing the variables.
#' @param var1 Character. Name of the first categorical variable.
#' @param var2 Character. Name of the second categorical variable.
#' @param by Character. Optional name of a third categorical variable to
#'   stratify (facet) on. When supplied, the \code{var1}-\code{var2} table is
#'   fitted separately within each level of \code{by} and the plot becomes a
#'   panel of mosaics, one per stratum. Category filtering (\code{min_count})
#'   is applied to the pooled table \emph{before} splitting, so every panel
#'   shows the same rows and columns; the residual colour scale is likewise
#'   shared across panels. Defaults to NULL (a single, un-stratified mosaic).
#' @param min_count The minimum number of observations required for a category to be
#'   included in the analysis. Defaults to 10.
#' @param fontsize The font size for the mosaic plot labels. Defaults to 8.
#' @param title The title for the mosaic plot. Defaults to "".
#' @param var1_label The label for the first variable in the plot. If NULL, uses the
#'   variable name.
#' @param var2_label The label for the second variable in the plot. If NULL, uses the
#'   variable name.
#' @param show_varnames Logical. If TRUE, draws the variable-name titles along the
#'   plot axes (e.g. the row and column variable names). Defaults to FALSE because
#'   these titles often overprint the category labels, especially when a thin
#'   category sits next to them. The category labels and column headers remain
#'   visible either way.
#' @param plot_style The mosaic plot style: "flat" (a modern flat ggplot2 mosaic
#'   shaded by standardized residuals; the default) or "classic" (the \pkg{vcd}
#'   shaded mosaic).
#' @param tile_label (flat style) What to print inside each tile: "count" (the
#'   default actual counts), "percent" (using \code{percentage_base}),
#'   "count_percent" (the count with its percentage on a second line),
#'   "residual" (standardized residual), "category" (the second-variable level
#'   name), or "none".
#' @param col_label_side (flat style) Placement of the first-variable labels:
#'   "top" (default), "bottom", "both", or "none".
#' @param row_label_side (flat style) Placement of the second-variable labels:
#'   "left" (default), "right", "both", or "none".
#' @param col_label_angle,row_label_angle (flat style) Rotation in degrees for
#'   the column and row category labels (0 = horizontal, 90 = vertical).
#' @param show_legend Logical. Show the residual colour legend. Defaults to TRUE.
#' @param legend_position Legend placement for the flat style: one of "right"
#'   (default), "left", "top", "bottom", or "none".
#' @param legend_size Numeric multiplier (> 0) scaling the legend key and text in
#'   the flat style. Smaller is more compact. Defaults to 0.7.
#' @param legend_title Legend title for the flat style. Defaults to
#'   "Std.\\nresidual".
#' @param label_size Tile-label text size for the flat style. Defaults to 3.5.
#' @param show_percentages Logical. If TRUE, includes percentages in the summary table.
#'   Defaults to TRUE.
#' @param percentage_base The base for calculating percentages ("total", "row", or "column").
#'   Defaults to "total".
#' @param use_fisher Logical. If TRUE, uses Fisher's exact test instead of chi-square
#'   (recommended for small expected cell counts). Defaults to FALSE.
#' @param by_label Label for the stratifying variable. If NULL, uses \code{by}.
#' @param min_stratum_n Minimum number of observations for a stratum to be
#'   fitted. Strata below this, or no longer spanning at least two levels of
#'   each variable, are dropped with a warning naming them. Defaults to 30.
#' @param p_adjust Multiplicity correction applied to the per-stratum p-values,
#'   passed to \code{\link[stats]{p.adjust}}. Defaults to "BH". Fitting one
#'   test per stratum is a multiple-testing problem, so the corrected value is
#'   reported alongside the raw one.
#' @param facet_ncol Number of facet columns in a stratified plot. NULL (the
#'   default) lets \pkg{ggplot2} choose.
#' @param facet_show_n Logical. Append "(n = ...)" to each panel strip so equal
#'   panel widths never imply equal sample sizes. Defaults to TRUE.
#' @param seed Optional integer. Seeds the Monte-Carlo Fisher p-value so a fit
#'   is reproducible; the caller's RNG stream is restored on exit. Defaults to
#'   NULL (no seeding).
#' @param verbose Logical. If TRUE, prints results to console. Defaults to TRUE.
#' @param save_plot Optional file path to save the mosaic plot. Supports .png, .pdf, .svg.
#'   Defaults to NULL (no saving).
#' @param interpret Logical. Pass results to AI for automatic interpretation?
#'   Default FALSE. When TRUE, generates clean Methods and Results text using
#'   AI. Includes chi-square/Fisher test results and Cramer's V effect size.
#'   Requires API key setup (see \code{\link{set_api_key}}).
#' @param ... Additional arguments passed to \code{\link{pass}} when
#'   interpret = TRUE (e.g., provider, model, context, append_prompt).
#'
#' @return A list of class "mosaic_analysis" containing:
#' \itemize{
#'   \item{\code{plot}}: The mosaic plot object
#'   \item{\code{consolidated_table}}: Tibble with observed, expected counts and percentages
#'   \item{\code{residuals}}: Data frame of standardized Pearson residuals
#'   \item{\code{chi_test}}: Chi-square test results (or Fisher's test if use_fisher=TRUE)
#'   \item{\code{cramers_v}}: Cramer's V effect size value
#'   \item{\code{cramers_v_interpretation}}: Effect size interpretation (negligible/small/medium/large)
#'   \item{\code{stats_summary}}: Tibble summarizing all statistical results
#'   \item{\code{filtered_data}}: The filtered data used for analysis
#'   \item{\code{original_n}}: Original sample size before filtering
#'   \item{\code{filtered_n}}: Sample size after filtering
#'   \item{\code{removed_categories}}: List of categories removed due to min_count
#' }
#'
#' When \code{by} is supplied the object additionally gains class
#' "mosaic_stratified" and the fields \code{strata_summary} (one row per
#' stratum: n, test, statistic, df, raw and adjusted p, Cramer's V, effect
#' size, and the number of cells beyond |2|), \code{strata_residuals} (one row
#' per stratum x cell), \code{strata_table}, and \code{overall_summary} (the
#' pooled test beside the Cochran-Mantel-Haenszel test of association
#' conditional on the stratifier). Reach these with
#' \code{\link[=as.data.frame.mosaic_analysis]{as.data.frame}}, e.g.
#' \code{as.data.frame(fit, what = "strata")}.
#'
#' @examples
#' # Create example data
#' set.seed(123)
#' example_data <- data.frame(
#'   gender = sample(c("Male", "Female"), 200, replace = TRUE),
#'   education = sample(c("High School", "Bachelor", "Master", "PhD"), 200,
#'                      replace = TRUE, prob = c(0.3, 0.4, 0.2, 0.1))
#' )
#'
#' # Basic usage with quoted variable names
#' results <- mosaic_analysis(example_data, "gender", "education")
#'
#' # Access results
#' results$cramers_v
#' results$cramers_v_interpretation
#' results$stats_summary
#'
#' \donttest{
#' # With row percentages and custom labels
#' results <- mosaic_analysis(
#'   example_data, "gender", "education",
#'   min_count = 5,
#'   var1_label = "Gender",
#'   var2_label = "Education Level",
#'   percentage_base = "row"
#' )
#'
#' # Using Fisher's exact test for small samples
#' results <- mosaic_analysis(
#'   example_data, "gender", "education",
#'   use_fisher = TRUE,
#'   verbose = FALSE
#' )
#'
#' # Stratify (facet) on a third variable: one mosaic and one test per region,
#' # with p-values corrected across strata.
#' example_data$region <- sample(c("North", "South"), 200, replace = TRUE)
#' by_region <- mosaic_analysis(
#'   example_data, "gender", "education",
#'   by = "region", min_count = 5, min_stratum_n = 20, verbose = FALSE
#' )
#' as.data.frame(by_region, what = "strata")
#' as.data.frame(by_region, what = "overall")
#' }
#'
#' @export
mosaic_analysis <- function(data, var1, var2, by = NULL, min_count = 10,
                            fontsize = 8, title = "",
                            var1_label = NULL, var2_label = NULL,
                            show_varnames = FALSE,
                            plot_style = c("flat", "classic"),
                            tile_label = c("count", "percent", "count_percent",
                                           "residual", "category", "none"),
                            col_label_side = c("top", "bottom", "both", "none"),
                            row_label_side = c("left", "right", "both", "none"),
                            col_label_angle = 0,
                            row_label_angle = 0,
                            show_legend = TRUE,
                            legend_position = "right",
                            legend_size = 0.7,
                            legend_title = "Std.\nresidual",
                            label_size = 3.5,
                            show_percentages = TRUE,
                            percentage_base = "total",
                            use_fisher = FALSE,
                            by_label = NULL,
                            min_stratum_n = 30,
                            p_adjust = "BH",
                            facet_ncol = NULL,
                            facet_show_n = TRUE,
                            seed = NULL,
                            verbose = TRUE,
                            save_plot = NULL,
                            interpret = FALSE,
                            ...) {


  # Input validation

if (!is.data.frame(data)) {
    stop("'data' must be a data frame")
  }

  if (nrow(data) == 0) {
    stop("'data' contains no observations")
  }

  if (!percentage_base %in% c("total", "row", "column")) {
    stop("'percentage_base' must be one of: 'total', 'row', 'column'")
  }

  if (!is.logical(show_varnames) || length(show_varnames) != 1 || is.na(show_varnames)) {
    stop("'show_varnames' must be a single logical value (TRUE or FALSE)")
  }

  if (!is.null(by) && (!is.character(by) || length(by) != 1)) {
    stop("'by' must be a single character string (the stratifying variable name)")
  }

  if (!is.numeric(min_stratum_n) || length(min_stratum_n) != 1 || min_stratum_n < 0) {
    stop("'min_stratum_n' must be a single non-negative number")
  }

  # A Monte-Carlo Fisher p-value is stochastic; seeding locally makes a fit
  # reproducible without leaving the caller's RNG stream disturbed.
  if (!is.null(seed)) {
    if (exists(".Random.seed", envir = globalenv(), inherits = FALSE)) {
      .old_seed <- get(".Random.seed", envir = globalenv(), inherits = FALSE)
      on.exit(assign(".Random.seed", .old_seed, envir = globalenv()),
              add = TRUE, after = FALSE)
    } else {
      on.exit(suppressWarnings(rm(".Random.seed", envir = globalenv())),
              add = TRUE, after = FALSE)
    }
    set.seed(seed)
  }

  plot_style     <- match.arg(plot_style)
  tile_label     <- match.arg(tile_label)
  col_label_side <- match.arg(col_label_side)
  row_label_side <- match.arg(row_label_side)

  if (!is.numeric(col_label_angle) || length(col_label_angle) != 1 ||
      !is.numeric(row_label_angle) || length(row_label_angle) != 1) {
    stop("'col_label_angle' and 'row_label_angle' must each be a single number")
  }

  if (!is.logical(show_legend) || length(show_legend) != 1 || is.na(show_legend)) {
    stop("'show_legend' must be a single logical value (TRUE or FALSE)")
  }

  legend_position <- match.arg(legend_position,
                               c("right", "left", "top", "bottom", "none"))

  if (!is.numeric(legend_size) || length(legend_size) != 1 || legend_size <= 0) {
    stop("'legend_size' must be a single positive number")
  }

  # Handle variable names (expects quoted strings)
  if (missing(var1) || is.null(var1)) {
    stop("'var1' must be specified as a character string")
  }
  if (!is.character(var1) || length(var1) != 1) {
    stop("'var1' must be a single character string (variable name)")
  }
  var1_name <- var1

  if (missing(var2) || is.null(var2)) {
    stop("'var2' must be specified as a character string")
  }
  if (!is.character(var2) || length(var2) != 1) {
    stop("'var2' must be a single character string (variable name)")
  }
  var2_name <- var2

  # Validate variable names exist in data
  if (!var1_name %in% names(data)) {
    stop(paste0("Variable '", var1_name, "' not found in data"))
  }

  if (!var2_name %in% names(data)) {
    stop(paste0("Variable '", var2_name, "' not found in data"))
  }

  if (!is.null(by)) {
    if (!by %in% names(data)) {
      stop(paste0("Stratifying variable '", by, "' not found in data"))
    }
    if (by %in% c(var1_name, var2_name)) {
      stop("'by' must differ from 'var1' and 'var2'")
    }
  }
  by_name <- by
  if (is.null(by_label)) by_label <- by_name

  # Set default labels if NULL
  if (is.null(var1_label)) {
    var1_label <- var1_name
  }

  if (is.null(var2_label)) {
    var2_label <- var2_name
  }

  # Remove missing values. The stratifier joins the complete-case rule so the
  # pooled table and every panel are built from exactly the same rows.
  keep_rows <- !is.na(data[[var1_name]]) & !is.na(data[[var2_name]])
  if (!is.null(by_name)) keep_rows <- keep_rows & !is.na(data[[by_name]])
  clean_data <- data[keep_rows, ]

  if (nrow(clean_data) == 0) {
    stop("No complete cases found after removing missing values")
  }

  # Convert to factors and drop unused levels
  clean_data[[var1_name]] <- factor(clean_data[[var1_name]])
  clean_data[[var2_name]] <- factor(clean_data[[var2_name]])

  # Create contingency table
  orig_table <- table(clean_data[[var1_name]], clean_data[[var2_name]])

  # Filter categories below min_count
  var1_counts <- rowSums(orig_table)
  var2_counts <- colSums(orig_table)

  # Keep only categories with sufficient counts
  keep_var1 <- names(var1_counts)[var1_counts >= min_count]
  keep_var2 <- names(var2_counts)[var2_counts >= min_count]

  # Track removed categories
  removed_var1 <- setdiff(names(var1_counts), keep_var1)
  removed_var2 <- setdiff(names(var2_counts), keep_var2)

  # Filter data
  filtered_data <- clean_data[clean_data[[var1_name]] %in% keep_var1 &
                                clean_data[[var2_name]] %in% keep_var2, ]

  # Drop unused factor levels
  filtered_data[[var1_name]] <- droplevels(filtered_data[[var1_name]])
  filtered_data[[var2_name]] <- droplevels(filtered_data[[var2_name]])

  # Create filtered contingency table
  filtered_table <- table(filtered_data[[var1_name]], filtered_data[[var2_name]])

  # Check if we have enough data
  if (nrow(filtered_table) < 2 || ncol(filtered_table) < 2) {
    stop("Not enough categories remaining after filtering. Try reducing min_count.")
  }

  # ---- stratified fit -------------------------------------------------------
  # Deliberately placed AFTER the pooled category filter so `min_count` acts on
  # the pooled margins: every panel then shows the same rows and columns, and a
  # missing combination reads as a real zero rather than a filtering artefact.
  strata <- if (!is.null(by_name)) {
    .mosaic_strata(filtered_data, var1_name, var2_name, by_name,
                   use_fisher = use_fisher, min_stratum_n = min_stratum_n,
                   p_adjust = p_adjust, show_percentages = show_percentages,
                   percentage_base = percentage_base)
  } else NULL

  # Perform statistical test
  if (use_fisher) {
    # Fisher's exact test (better for small expected counts)
    stat_test <- stats::fisher.test(filtered_table, simulate.p.value = TRUE, B = 10000)
    test_type <- "Fisher's exact test"
    test_statistic <- NA
    test_df <- NA
    p_value <- stat_test$p.value
  } else {
    # Chi-square test
    stat_test <- stats::chisq.test(filtered_table)
    test_type <- "Chi-square test"
    test_statistic <- stat_test$statistic
    test_df <- stat_test$parameter
    p_value <- stat_test$p.value

    # Warn if expected counts are low
    if (any(stat_test$expected < 5)) {
      warning("Some expected cell counts are < 5. Consider using use_fisher = TRUE for more accurate results.")
    }
  }

  # Calculate Cramer's V (effect size)
  n <- sum(filtered_table)
  k <- min(nrow(filtered_table), ncol(filtered_table))

  if (use_fisher) {
    # For Fisher's test, calculate chi-square statistic for Cramer's V
    chi_for_v <- stats::chisq.test(filtered_table)$statistic
    cramers_v <- as.numeric(sqrt(chi_for_v / (n * (k - 1))))
  } else {
    cramers_v <- as.numeric(sqrt(stat_test$statistic / (n * (k - 1))))
  }

  # Interpret effect size
  df_for_interpretation <- k - 1
  cramers_v_interpretation <- interpret_cramers_v(cramers_v, df_for_interpretation)

  # Standardized Pearson residuals — computed once and reused by both the plot
  # and the residuals table below.
  if (use_fisher) {
    stdres_mat <- stats::chisq.test(filtered_table)$stdres
  } else {
    stdres_mat <- stat_test$stdres
  }

  # Bundle everything the plot needs so it can be re-rendered later (e.g. by
  # plot.mosaic_analysis) without recomputing the test.
  # Under stratification the renderer receives one table per panel; otherwise
  # the single pooled table, which is the K = 1 case of the same code path.
  plot_parts <- list(
    table     = if (is.null(strata)) filtered_table else strata$tables,
    residuals = if (is.null(strata)) stdres_mat     else strata$residuals,
    strata_n  = if (is.null(strata)) NULL           else strata$strata_n,
    data = filtered_data,
    var1_name = var1_name, var2_name = var2_name,
    var1_label = var1_label, var2_label = var2_label,
    by_name = by_name, by_label = by_label
  )
  plot_style_args <- list(
    plot_style = plot_style, title = title, tile_label = tile_label,
    percentage_base = percentage_base,
    col_label_side = col_label_side, row_label_side = row_label_side,
    col_label_angle = col_label_angle, row_label_angle = row_label_angle,
    show_varnames = show_varnames, fontsize = fontsize,
    show_legend = show_legend, legend_position = legend_position,
    legend_size = legend_size, legend_title = legend_title,
    label_size = label_size,
    facet_ncol = facet_ncol, facet_show_n = facet_show_n
  )

  # Build the mosaic plot. flat returns a ggplot (drawn via print); classic
  # draws as a side effect and returns a structable.
  mosaic_plot <- build_mosaic_plot(plot_parts, plot_style_args)
  draw_mosaic_plot(mosaic_plot, plot_style_args)

  # Save plot if requested
  if (!is.null(save_plot)) {
    ext <- tolower(tools::file_ext(save_plot))
    if (ext == "png") {
      grDevices::png(save_plot, width = 800, height = 600)
    } else if (ext == "pdf") {
      grDevices::pdf(save_plot, width = 10, height = 8)
    } else if (ext == "svg") {
      grDevices::svg(save_plot, width = 10, height = 8)
    } else {
      warning("Unsupported file format. Use .png, .pdf, or .svg")
    }

    if (ext %in% c("png", "pdf", "svg")) {
      draw_mosaic_plot(build_mosaic_plot(plot_parts, plot_style_args), plot_style_args)
      grDevices::dev.off()
      if (verbose) cat("Plot saved to:", save_plot, "\n")
    }
  }

  # Create the consolidated table
  # For Fisher's test, we still need expected values from chi-square
  if (use_fisher) {
    expected_values <- stats::chisq.test(filtered_table)$expected
  } else {
    expected_values <- stat_test$expected
  }

  consolidated_table <- create_consolidated_table(filtered_table, expected_values,
                                                  show_percentages, percentage_base)

  # Create standardized residuals table (reusing the residuals computed above)
  residuals_df <- as.data.frame.matrix(round(stdres_mat, 2))
  residuals_df$Variable <- rownames(residuals_df)
  residuals_df <- residuals_df %>% dplyr::select(Variable, dplyr::everything())

  # Build removed categories text
  removed_text <- paste0(
    if(length(removed_var1) > 0) paste0(var1_name, ": ", paste(removed_var1, collapse = ", ")) else "",
    if(length(removed_var1) > 0 && length(removed_var2) > 0) "; " else "",
    if(length(removed_var2) > 0) paste0(var2_name, ": ", paste(removed_var2, collapse = ", ")) else ""
  )
  if(removed_text == "") removed_text <- "None"

  # Statistical summary
  stats_summary <- tibble::tibble(
    Statistic = c("Test type", "Test statistic", "Degrees of freedom", "p-value",
                  "Cramer's V", "Effect size", "Sample size", "Categories removed"),
    Value = c(
      test_type,
      ifelse(is.na(test_statistic), "N/A", round(test_statistic, 3)),
      ifelse(is.na(test_df), "N/A", test_df),
      ifelse(p_value < 0.001, "< 0.001", round(p_value, 3)),
      round(cramers_v, 3),
      cramers_v_interpretation,
      sum(filtered_table),
      removed_text
    )
  )

  # Pooled vs. conditional association, side by side. A pooled effect that the
  # per-stratum panels do not reproduce is the signature of Simpson's paradox.
  overall_summary <- if (is.null(strata)) NULL else {
    pooled <- data.frame(
      scope = "pooled", test = test_type,
      statistic = if (is.na(test_statistic)) NA_real_ else round(as.numeric(test_statistic), 3),
      df = if (is.na(test_df)) NA_real_ else as.numeric(test_df),
      p_value = p_value, cramers_v = round(cramers_v, 3),
      common_or = NA_real_, or_ci_low = NA_real_, or_ci_high = NA_real_,
      row.names = NULL, stringsAsFactors = FALSE)
    if (is.null(strata$cmh)) pooled else {
      cmh <- strata$cmh
      rbind(pooled, data.frame(
        scope = "conditional", test = cmh$test,
        statistic = round(cmh$statistic, 3), df = cmh$df,
        p_value = cmh$p_value, cramers_v = NA_real_,
        common_or = round(cmh$common_or, 3),
        or_ci_low = round(cmh$or_ci_low, 3), or_ci_high = round(cmh$or_ci_high, 3),
        row.names = NULL, stringsAsFactors = FALSE))
    }
  }

  # Print results if verbose
  if (verbose) {
    cat("\n=== MOSAIC ANALYSIS RESULTS ===\n")
    cat("Variables:", var1_name, "\u00d7", var2_name, "\n")
    if (!is.null(by_name)) cat("Stratified by:", by_name,
                               "(", strata$n_strata, "strata )\n")
    cat("Minimum count threshold:", min_count, "\n")
    cat("Test used:", test_type, "\n")
    if (show_percentages) {
      cat("Percentages based on:", percentage_base, "\n")
    }
    cat("\n")

    # Print consolidated table
    cat("CONSOLIDATED FREQUENCY TABLE\n")
    cat("===========================\n")
    print(tibble::as_tibble(consolidated_table), n = Inf)
    cat("\n")

    # Print standardized residuals
    cat("STANDARDIZED RESIDUALS\n")
    cat("=====================\n")
    cat("(Values > |2| indicate significant deviation from expected)\n")
    print(tibble::as_tibble(residuals_df), n = Inf)
    cat("\n")

    # Print statistical summary
    cat("STATISTICAL SUMMARY\n")
    cat("==================\n")
    print(tibble::as_tibble(stats_summary), n = Inf)
    cat("\n")

    if (!is.null(strata)) {
      cat("PER-STRATUM TESTS\n")
      cat("=================\n")
      cat("(p_adjusted corrects for the", strata$n_strata, "tests, method:",
          p_adjust, ")\n")
      print(tibble::as_tibble(strata$strata_summary), n = Inf)
      cat("\n")
      cat("POOLED VS CONDITIONAL\n")
      cat("=====================\n")
      print(tibble::as_tibble(overall_summary), n = Inf)
      cat("\n")
    }
  }

  # Return results as a list with class
  results <- list(
    mosaic_plot = mosaic_plot,
    summary_table = consolidated_table,
    residuals = residuals_df,
    chi_square_test = stat_test,
    cramers_v = cramers_v,
    cramers_v_interpretation = cramers_v_interpretation,
    stats_summary = stats_summary,
    filtered_data = filtered_data,
    original_n = nrow(clean_data),
    filtered_n = nrow(filtered_data),
    removed_categories = list(
      var1 = removed_var1,
      var2 = removed_var2
    ),
    # Stored so the plot can be re-rendered with new styling (see
    # plot.mosaic_analysis) without re-running the statistical test.
    plot_parts = plot_parts,
    plot_args = plot_style_args,
    var1_name = var1_name,
    var2_name = var2_name,
    by_name = by_name,
    by_label = by_label,
    call = match.call()
  )

  # A stratified fit carries the per-stratum tables in addition to everything a
  # plain fit returns, so it inherits from "mosaic_analysis" rather than
  # replacing it: existing accessors and the plot method keep working.
  if (!is.null(strata)) {
    results$strata_summary   <- strata$strata_summary
    results$strata_residuals <- strata$strata_residuals
    results$strata_table     <- strata$strata_table
    results$overall_summary  <- overall_summary
    results$strata_tables    <- strata$tables
    results$strata_n         <- strata$strata_n
    results$n_strata         <- strata$n_strata
    results$dropped_strata   <- strata$dropped_strata
  }

  class(results) <- if (is.null(strata)) c("mosaic_analysis", "list")
                    else c("mosaic_stratified", "mosaic_analysis", "list")

  # ===========================================================================
  # AI Interpretation (if requested)
  # ===========================================================================
  if (interpret) {
    # Build comprehensive metadata including chi-square results
    metadata <- list(
      var1 = var1_name,
      var2 = var2_name,
      var1_label = var1_label,
      var2_label = var2_label,
      test_type = test_type,
      total_n = sum(filtered_table),
      original_n = nrow(clean_data),
      filtered_n = nrow(filtered_data)
    )

    # Add chi-square or Fisher test results
    if (use_fisher) {
      metadata$fisher_p <- p_value
    } else {
      metadata$chi_sq <- as.numeric(test_statistic)
      metadata$df <- as.numeric(test_df)
      metadata$p_value <- p_value
    }

    # Add effect size
    metadata$cramers_v <- cramers_v
    metadata$effect_interpretation <- cramers_v_interpretation

    # Add contingency table info
    metadata$n_rows <- nrow(filtered_table)
    metadata$n_cols <- ncol(filtered_table)
    metadata$row_categories <- rownames(filtered_table)
    metadata$col_categories <- colnames(filtered_table)

    # Call the interpretation function
    interpret_with_ai(results, analysis_type = "categorical_analysis", metadata = metadata, ...)
  }

  return(invisible(results))
}

#' Plot method for mosaic_analysis objects
#'
#' @description
#' Re-renders the mosaic plot from a fitted \code{mosaic_analysis} object using
#' the stored contingency table and residuals, so styling can be changed without
#' re-running the statistical test. Any styling argument accepted by
#' \code{\link{mosaic_analysis}} (e.g. \code{plot_style}, \code{tile_label},
#' \code{col_label_side}, \code{legend_size}) can be overridden via \code{...};
#' unspecified arguments keep the values from the original call.
#'
#' @param x A \code{mosaic_analysis} object.
#' @param ... Styling overrides (see \code{\link{mosaic_analysis}}).
#' @return Invisibly, the plot object (a ggplot for the flat style). Called for
#'   its side effect of drawing the plot.
#' @examples
#' set.seed(1)
#' d <- data.frame(
#'   a = sample(c("X", "Y", "Z"), 200, replace = TRUE),
#'   b = sample(c("P", "Q"), 200, replace = TRUE)
#' )
#' res <- mosaic_analysis(d, "a", "b", min_count = 5, verbose = FALSE)
#' \donttest{
#' plot(res, tile_label = "percent", legend_size = 0.4)
#' plot(res, plot_style = "classic")
#' }
#' @export
plot.mosaic_analysis <- function(x, ...) {
  overrides <- list(...)
  unknown <- setdiff(names(overrides), names(x$plot_args))
  if (length(unknown)) {
    stop("Unknown styling argument(s): ", paste(unknown, collapse = ", "),
         call. = FALSE)
  }
  style <- utils::modifyList(x$plot_args, overrides)
  style$plot_style <- match.arg(style$plot_style, c("flat", "classic"))
  plot_obj <- build_mosaic_plot(x$plot_parts, style)
  draw_mosaic_plot(plot_obj, style)
}

#' Print method for mosaic_analysis objects
#'
#' @param x A mosaic_analysis object
#' @param ... Additional arguments (ignored)
#' @export
print.mosaic_analysis <- function(x, ...) {
  cat("Mosaic Analysis Results\n")
  cat("=======================\n")
  cat("Original N:", x$original_n, "\n")
  cat("Filtered N:", x$filtered_n, "\n")
  cat("Cramer's V:", round(x$cramers_v, 3), "(", x$cramers_v_interpretation, ")\n")
  cat("\nUse summary() for detailed statistics\n")
  invisible(x)
}

#' Summary method for mosaic_analysis objects
#'
#' @param object A mosaic_analysis object
#' @param ... Additional arguments (ignored)
#' @export
summary.mosaic_analysis <- function(object, ...) {
  cat("\n=== MOSAIC ANALYSIS SUMMARY ===\n\n")
  print(object$stats_summary, n = Inf)
  cat("\n")
  invisible(object$stats_summary)
}
