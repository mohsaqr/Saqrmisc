# Saqrmisc Package: stratified (faceted) mosaic analysis
#
# Splitting a var1 x var2 table by a third variable and fitting each stratum
# separately is the standard check for effect modification and for Simpson's
# paradox: an association present in the pooled table can weaken, vanish, or
# reverse inside every subgroup.
#
# Two conventions make the panels comparable and are relied on throughout:
#   1. Category filtering (`min_count`) is applied to the POOLED table before
#      splitting, so every panel shows the same rows and columns.
#   2. The residual colour scale is shared across panels (see flat_mosaic()).

#' Fit the statistics for one stratum's contingency table (internal)
#'
#' @param tab A two-way contingency \code{table}.
#' @param use_fisher Logical; use a Monte-Carlo Fisher exact test instead of
#'   the chi-square test.
#' @return A list with the test object, test type, statistic, degrees of
#'   freedom, p-value, Cramer's V and its interpretation, the standardized
#'   Pearson residual matrix and expected counts (both on the full category
#'   grid, \code{NA} in structurally empty rows/columns), \code{empty}
#'   (names of the dropped rows/columns), and \code{low_expected} (logical;
#'   any expected count below 5).
#' @noRd
.mosaic_stratum_fit <- function(tab, use_fisher = FALSE) {
  # Every panel is built on the pooled category grid, so a stratum can contain
  # a structurally empty row or column. Testing that padded table would divide
  # by a zero expected count (NaN residuals) and overstate the degrees of
  # freedom, so the test runs on the non-empty core while the residuals are
  # padded back to the full grid to keep the panels aligned.
  keep_r <- rowSums(tab) > 0
  keep_c <- colSums(tab) > 0
  core <- tab[keep_r, keep_c, drop = FALSE]
  empty <- c(rownames(tab)[!keep_r], colnames(tab)[!keep_c])

  # Cramer's V and the residuals always come from the chi-square decomposition,
  # even under Fisher, so the two tests stay on one effect-size footing.
  chi <- suppressWarnings(stats::chisq.test(core))

  if (use_fisher) {
    test <- stats::fisher.test(core, simulate.p.value = TRUE, B = 10000)
    test_type <- "Fisher's exact test"
    statistic <- NA_real_
    df        <- NA_real_
  } else {
    test <- chi
    test_type <- "Chi-square test"
    statistic <- as.numeric(chi$statistic)
    df        <- as.numeric(chi$parameter)
  }

  n <- sum(core)
  k <- min(nrow(core), ncol(core))
  cramers_v <- as.numeric(sqrt(as.numeric(chi$statistic) / (n * (k - 1))))

  # pad a core-shaped matrix back onto the full category grid
  pad <- function(m) {
    out <- matrix(NA_real_, nrow(tab), ncol(tab), dimnames = dimnames(tab))
    out[keep_r, keep_c] <- m
    out
  }

  list(
    test = test, test_type = test_type,
    statistic = statistic, df = df, p_value = test$p.value,
    cramers_v = cramers_v,
    cramers_v_interpretation = interpret_cramers_v(cramers_v, k - 1),
    residuals = pad(chi$stdres), expected = pad(chi$expected),
    empty = empty,
    low_expected = any(chi$expected < 5)
  )
}

#' Generalized Cochran-Mantel-Haenszel test across strata (internal)
#'
#' Tests var1-var2 association conditional on the stratifier. Returns a tidy
#' one-row data frame, or \code{NULL} with a warning when the test cannot be
#' computed (e.g. a stratum with an empty margin).
#'
#' @param arr A three-way contingency \code{table} (var1 x var2 x stratum).
#' @return A one-row \code{data.frame}, or \code{NULL}.
#' @noRd
.mosaic_cmh <- function(arr) {
  out <- tryCatch(
    stats::mantelhaen.test(arr),
    error = function(e) {
      warning(warningCondition(
        paste0("Cochran-Mantel-Haenszel test could not be computed: ",
               conditionMessage(e)),
        class = "saqrmisc_cmh_failed", call = NULL))
      NULL
    })
  if (is.null(out)) return(NULL)

  # `estimate` / `conf.int` are returned only for 2 x 2 x K tables; the
  # generalized I x J x K form reports the statistic alone.
  has_or <- !is.null(out$estimate)
  data.frame(
    test          = "Cochran-Mantel-Haenszel",
    statistic     = as.numeric(out$statistic),
    df            = as.numeric(out$parameter),
    p_value       = as.numeric(out$p.value),
    common_or     = if (has_or) as.numeric(out$estimate) else NA_real_,
    or_ci_low     = if (has_or) out$conf.int[1] else NA_real_,
    or_ci_high    = if (has_or) out$conf.int[2] else NA_real_,
    stringsAsFactors = FALSE
  )
}

#' Fit a mosaic analysis within each level of a stratifying variable (internal)
#'
#' @param filtered_data Data already cleaned and category-filtered on the
#'   pooled table, carrying the stratifier column.
#' @param var1_name,var2_name,by_name Column names.
#' @param use_fisher Logical; Monte-Carlo Fisher instead of chi-square.
#' @param min_stratum_n Minimum observations for a stratum to be fitted.
#' @param p_adjust Multiplicity correction passed to \code{stats::p.adjust}.
#' @param show_percentages,percentage_base Passed to the consolidated table.
#' @return A list with per-stratum \code{tables} and \code{residuals} (named
#'   lists), \code{strata_n}, the tidy \code{strata_summary},
#'   \code{strata_residuals} and \code{strata_table} frames, the \code{cmh}
#'   row, and \code{dropped_strata}.
#' @noRd
.mosaic_strata <- function(filtered_data, var1_name, var2_name, by_name,
                           use_fisher = FALSE, min_stratum_n = 30,
                           p_adjust = "BH", show_percentages = TRUE,
                           percentage_base = "total") {

  by_f <- droplevels(factor(filtered_data[[by_name]]))
  filtered_data[[by_name]] <- by_f
  pieces <- split(filtered_data, by_f)

  # A stratum is fittable only if it is large enough AND still spans at least
  # two levels of each variable — otherwise no association can be estimated.
  fittable <- vapply(pieces, function(p) {
    nrow(p) >= min_stratum_n &&
      length(unique(p[[var1_name]])) >= 2 && length(unique(p[[var2_name]])) >= 2
  }, logical(1))

  dropped <- names(pieces)[!fittable]
  if (length(dropped)) {
    warning(warningCondition(
      paste0(length(dropped), " stratum/strata dropped (fewer than ",
             min_stratum_n, " observations, or not spanning 2x2): ",
             paste(dropped, collapse = ", ")),
      class = "saqrmisc_strata_dropped", call = NULL))
  }
  pieces <- pieces[fittable]

  if (length(pieces) < 2) {
    stop(errorCondition(
      paste0("Fewer than 2 usable strata in '", by_name,
             "'. Lower 'min_stratum_n' or choose a different stratifier."),
      class = "saqrmisc_too_few_strata", call = NULL))
  }

  # Every panel keeps the full pooled category set, so tiles line up across
  # panels and an absent combination reads as a genuine zero.
  lv1 <- levels(droplevels(factor(filtered_data[[var1_name]])))
  lv2 <- levels(droplevels(factor(filtered_data[[var2_name]])))
  tables <- lapply(pieces, function(p) table(
    factor(p[[var1_name]], levels = lv1),
    factor(p[[var2_name]], levels = lv2)))

  fits <- lapply(tables, .mosaic_stratum_fit, use_fisher = use_fisher)
  strata_n <- vapply(tables, sum, numeric(1))

  has_empty <- names(fits)[vapply(fits, function(f) length(f$empty) > 0, logical(1))]
  if (length(has_empty)) {
    warning(warningCondition(
      paste0("Some categories are unobserved within stratum/strata: ",
             paste(has_empty, collapse = ", "),
             ". Those cells are drawn empty and excluded from that stratum's ",
             "test, which therefore has fewer degrees of freedom."),
      class = "saqrmisc_empty_cells", call = NULL))
  }

  low_exp <- names(fits)[vapply(fits, function(f) f$low_expected, logical(1))]
  if (length(low_exp) && !use_fisher) {
    warning(warningCondition(
      paste0("Expected counts < 5 in stratum/strata: ",
             paste(low_exp, collapse = ", "),
             ". Consider use_fisher = TRUE."),
      class = "saqrmisc_low_expected", call = NULL))
  }

  # ---- tidy per-stratum summary; p-values corrected across strata ----
  p_raw <- vapply(fits, function(f) f$p_value, numeric(1))
  strata_summary <- data.frame(
    stratum        = names(fits),
    n              = as.integer(strata_n),
    test           = vapply(fits, function(f) f$test_type, character(1)),
    statistic      = round(vapply(fits, function(f) f$statistic, numeric(1)), 3),
    df             = vapply(fits, function(f) f$df, numeric(1)),
    p_value        = p_raw,
    p_adjusted     = stats::p.adjust(p_raw, method = p_adjust),
    cramers_v      = round(vapply(fits, function(f) f$cramers_v, numeric(1)), 3),
    effect_size    = vapply(fits, function(f) f$cramers_v_interpretation, character(1)),
    # NA residuals mark structurally absent cells (see .mosaic_stratum_fit);
    # a category with no observations cannot deviate from expectation, so it is
    # excluded from the count rather than making the whole count unknown.
    n_cells_beyond_2 = as.integer(vapply(
      fits, function(f) sum(abs(f$residuals) > 2, na.rm = TRUE), numeric(1))),
    row.names = NULL, stringsAsFactors = FALSE
  )

  # ---- tidy long residuals: one row per stratum x cell ----
  strata_residuals <- do.call(rbind, Map(function(nm, f, tb) {
    grid <- expand.grid(var1 = rownames(tb), var2 = colnames(tb),
                        KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
    data.frame(
      stratum       = nm,
      var1          = grid$var1,
      var2          = grid$var2,
      observed      = as.integer(tb[cbind(grid$var1, grid$var2)]),
      expected      = round(f$expected[cbind(grid$var1, grid$var2)], 2),
      std_residual  = round(f$residuals[cbind(grid$var1, grid$var2)], 2),
      row.names = NULL, stringsAsFactors = FALSE)
  }, names(fits), fits, tables))
  names(strata_residuals)[2:3] <- c(var1_name, var2_name)
  strata_residuals$beyond_2 <- abs(strata_residuals$std_residual) > 2

  # ---- consolidated observed/expected table, stacked over strata ----
  strata_table <- do.call(rbind, Map(function(nm, f, tb) {
    ct <- create_consolidated_table(tb, f$expected, show_percentages, percentage_base)
    cbind(stratum = nm, as.data.frame(ct), stringsAsFactors = FALSE)
  }, names(fits), fits, tables))
  rownames(strata_table) <- NULL

  arr <- table(
    factor(filtered_data[[var1_name]], levels = lv1),
    factor(filtered_data[[var2_name]], levels = lv2),
    factor(filtered_data[[by_name]], levels = names(pieces)))
  cmh <- .mosaic_cmh(arr)

  list(
    tables = tables, residuals = lapply(fits, function(f) f$residuals),
    strata_n = strata_n, strata_summary = strata_summary,
    strata_residuals = strata_residuals, strata_table = strata_table,
    cmh = cmh, dropped_strata = dropped, n_strata = length(pieces)
  )
}

#' Tidy accessor for a mosaic_analysis result
#'
#' @description
#' Returns one of the result's tables as a plain \code{data.frame}, so callers
#' never have to reach into the object. For a stratified fit (one produced with
#' \code{by =}), \code{what = "strata"} gives one row per stratum with its own
#' test, effect size and multiplicity-corrected p-value.
#'
#' @param x A \code{mosaic_analysis} object.
#' @param row.names,optional Ignored; present for S3 consistency.
#' @param what Which table to return: \code{"summary"} (the statistical
#'   summary, default), \code{"table"} (consolidated observed/expected counts),
#'   \code{"residuals"} (standardized Pearson residuals), \code{"strata"} (one
#'   row per stratum), or \code{"overall"} (the pooled test plus the
#'   Cochran-Mantel-Haenszel test). The last two require a stratified fit.
#' @param ... Ignored.
#' @return A \code{data.frame}. For \code{what = "strata"}, one row per stratum
#'   with columns \code{stratum}, \code{n}, \code{test}, \code{statistic},
#'   \code{df}, \code{p_value}, \code{p_adjusted}, \code{cramers_v},
#'   \code{effect_size} and \code{n_cells_beyond_2}. For
#'   \code{what = "residuals"} on a stratified fit, one row per stratum x cell.
#' @examples
#' set.seed(1)
#' d <- data.frame(
#'   a = sample(c("X", "Y", "Z"), 400, replace = TRUE),
#'   b = sample(c("P", "Q"), 400, replace = TRUE),
#'   g = sample(c("g1", "g2"), 400, replace = TRUE)
#' )
#' fit <- mosaic_analysis(d, "a", "b", by = "g", min_count = 5, verbose = FALSE)
#' as.data.frame(fit, what = "strata")
#' as.data.frame(fit, what = "overall")
#' @export
as.data.frame.mosaic_analysis <- function(x, row.names = NULL, optional = FALSE,
                                          what = c("summary", "table", "residuals",
                                                   "strata", "overall"), ...) {
  what <- match.arg(what)
  stratified <- inherits(x, "mosaic_stratified")

  if (what %in% c("strata", "overall") && !stratified) {
    stop(errorCondition(
      paste0("what = \"", what, "\" needs a stratified fit; ",
             "re-run mosaic_analysis() with a `by` variable."),
      class = "saqrmisc_not_stratified", call = NULL))
  }

  switch(what,
    summary   = as.data.frame(x$stats_summary, stringsAsFactors = FALSE),
    table     = if (stratified) x$strata_table else
                  as.data.frame(x$summary_table, stringsAsFactors = FALSE),
    residuals = if (stratified) x$strata_residuals else x$residuals,
    strata    = x$strata_summary,
    overall   = x$overall_summary)
}

#' Print method for stratified mosaic_analysis objects
#'
#' @param x A \code{mosaic_analysis} object fitted with \code{by =}.
#' @param ... Additional arguments (ignored).
#' @return Invisibly, \code{x}. Called for its side effect of printing.
#' @export
print.mosaic_stratified <- function(x, ...) {
  cat("Stratified Mosaic Analysis\n")
  cat("==========================\n")
  # Plain ASCII: a multiplication sign prints as <U+00D7> under a C locale,
  # which the deploy server uses.
  cat("Association:", x$var1_name, "x", x$var2_name,
      "  stratified by:", x$by_name, "\n")
  cat("Strata fitted:", x$n_strata, " Total N:", x$filtered_n, "\n")
  if (length(x$dropped_strata))
    cat("Strata dropped:", paste(x$dropped_strata, collapse = ", "), "\n")
  cat("Pooled Cramer's V:", round(x$cramers_v, 3),
      "(", x$cramers_v_interpretation, ")\n")
  cat("\nUse as.data.frame(x, what = \"strata\") for the per-stratum table\n")
  cat("and as.data.frame(x, what = \"overall\") for the pooled and CMH tests.\n")
  invisible(x)
}

#' Summary method for stratified mosaic_analysis objects
#'
#' @param object A \code{mosaic_analysis} object fitted with \code{by =}.
#' @param ... Additional arguments (ignored).
#' @return Invisibly, the per-stratum summary \code{data.frame}.
#' @export
summary.mosaic_stratified <- function(object, ...) {
  cat("\n=== STRATIFIED MOSAIC ANALYSIS ===\n\n")
  cat("Per-stratum tests (p adjusted across strata):\n")
  print(tibble::as_tibble(object$strata_summary), n = Inf)
  cat("\nPooled and conditional tests:\n")
  print(tibble::as_tibble(object$overall_summary), n = Inf)
  cat("\n")
  invisible(object$strata_summary)
}
