# Saqrmisc Package: AI Interpretation Functions
#
# Functions for passing R output to AI for interpretation.

# Null coalescing operator (if not from rlang)
`%||%` <- function(x, y) if (is.null(x)) y else x

#' Convert data frame to clean markdown table
#' @param df Data frame to convert
#' @param digits Number of decimal places for numeric columns
#' @param max_rows Maximum rows to include (NULL for all)
#' @return Character string with markdown table
#' @noRd
df_to_markdown <- function(df, digits = 3, max_rows = NULL) {
  if (is.null(df) || !inherits(df, "data.frame") || nrow(df) == 0) {
    return(NULL)
  }

  # Convert tibble to data frame

  df <- as.data.frame(df)

 # Limit rows if specified
  if (!is.null(max_rows) && nrow(df) > max_rows) {
    df <- df[1:max_rows, , drop = FALSE]
  }

  # Round numeric columns
  for (col in names(df)) {
    if (is.numeric(df[[col]])) {
      df[[col]] <- round(df[[col]], digits)
    }
  }

  # Create header
  header <- paste0("| ", paste(names(df), collapse = " | "), " |")
  separator <- paste0("|", paste(rep("---", ncol(df)), collapse = "|"), "|
")

  # Create rows
  rows <- apply(df, 1, function(row) {
    paste0("| ", paste(row, collapse = " | "), " |")
  })

  paste(c(header, separator, rows), collapse = "\n")
}

#' Create a saqr_result object
#'
#' @description
#' Creates a standardized result object for Saqrmisc functions that includes
#' both the original data and a clean markdown representation for AI interpretation.
#'
#' @param data The main data (data frame or tibble with results)
#' @param table Optional gt table or formatted table
#' @param type Character describing the analysis type (e.g., "group_comparison", "correlation", "descriptive")
#' @param markdown Optional pre-built markdown string. If NULL, auto-generated from data.
#' @param ... Additional components to include in the result
#'
#' @return A list with class "saqr_result" containing data, table, markdown, and other components
#' @export
saqr_result <- function(data = NULL, table = NULL, type = "analysis", markdown = NULL, ...) {
  result <- list(
    data = data,
    table = table,
    type = type,
    ...
  )

  # Auto-generate markdown if not provided
  if (is.null(markdown)) {
    result$markdown <- generate_saqr_markdown(result)
  } else {
    result$markdown <- markdown
  }

  class(result) <- c("saqr_result", "list")
  result
}

#' Generate markdown summary for saqr_result
#' @noRd
generate_saqr_markdown <- function(x) {
  parts <- c()

  # Add type header
  if (!is.null(x$type)) {
    type_label <- switch(x$type,
      "group_comparison" = "## Group Comparison Results",
      "correlation" = "## Correlation Analysis",
      "descriptive" = "## Descriptive Statistics",
      "categorical" = "## Categorical Analysis",
      paste0("## ", tools::toTitleCase(gsub("_", " ", x$type)))
    )
    parts <- c(parts, type_label, "")
  }

  # Add main data table
  if (!is.null(x$data) && inherits(x$data, "data.frame") && nrow(x$data) > 0) {
    # Select key columns for cleaner output
    df <- as.data.frame(x$data)

    # Try to identify and keep only important columns
    key_cols <- intersect(names(df), c(
      # Grouping
      "variable", "Variable", "group", "Group", "category", "LLM",
      # Descriptives
      "n", "N", "mean", "Mean", "sd", "SD", "se", "SE", "median", "Median",
      # Test statistics
      "statistic", "t_statistic", "f_statistic", "chi_square",
      "p_value", "p", "P-value", "p_adjusted",
      # Effect sizes
      "effect_size", "efsz", "ES", "cohens_d", "eta_sq", "cramers_v", "ef_type",
      # Other
      "test_type", "df", "t_df", "ci_lower", "ci_upper"
    ))

    if (length(key_cols) > 0) {
      df_clean <- df[, key_cols, drop = FALSE]
    } else {
      # If no key columns found, use first 8 columns
      df_clean <- df[, 1:min(8, ncol(df)), drop = FALSE]
    }

    md_table <- df_to_markdown(df_clean)
    if (!is.null(md_table)) {
      parts <- c(parts, "### Results Table", "", md_table, "")
    }
  }

  # Add correlation matrix if present
  if (!is.null(x$correlation_matrix) && is.matrix(x$correlation_matrix)) {
    parts <- c(parts, "### Correlation Matrix", "")
    cor_df <- as.data.frame(round(x$correlation_matrix, 3))
    cor_df <- cbind(Variable = rownames(cor_df), cor_df)
    md_table <- df_to_markdown(cor_df)
    if (!is.null(md_table)) {
      parts <- c(parts, md_table, "")
    }
  }

  # Add p-values matrix if present
  if (!is.null(x$p_matrix) && is.matrix(x$p_matrix)) {
    parts <- c(parts, "### P-values Matrix", "")
    p_df <- as.data.frame(round(x$p_matrix, 4))
    p_df <- cbind(Variable = rownames(p_df), p_df)
    md_table <- df_to_markdown(p_df, digits = 4)
    if (!is.null(md_table)) {
      parts <- c(parts, md_table, "")
    }
  }

  # Add test statistics summary if present
  if (!is.null(x$test_summary) && is.character(x$test_summary)) {
    parts <- c(parts, "### Statistical Tests", "", x$test_summary, "")
  }

  paste(parts, collapse = "\n")
}

#' Print method for saqr_result
#' @export
print.saqr_result <- function(x, ...) {
  cat("Saqr Result:", x$type, "\n")
  cat(strrep("-", 40), "\n")
  if (!is.null(x$table) && inherits(x$table, "gt_tbl")) {
    print(x$table)
  } else if (!is.null(x$data)) {
    print(x$data)
  }
  invisible(x)
}

#' Get R and package citations in BibTeX format
#' @noRd
get_citations_bibtex <- function(packages = NULL) {

  citations <- c()


  # Get R citation
  r_cite <- utils::citation()
  r_bibtex <- paste(utils::capture.output(print(r_cite, bibtex = TRUE)), collapse = "\n")
  citations <- c(citations, "### R Citation", "", "```bibtex", r_bibtex, "```", "")

  # Get package citations if specified
  if (!is.null(packages) && length(packages) > 0) {
    for (pkg in packages) {
      tryCatch({
        pkg_cite <- utils::citation(pkg)
        pkg_bibtex <- paste(utils::capture.output(print(pkg_cite, bibtex = TRUE)), collapse = "\n")
        citations <- c(citations, paste0("### ", pkg, " Citation"), "", "```bibtex", pkg_bibtex, "```", "")
      }, error = function(e) {
        # Skip if package citation not available
      })
    }
  }

  paste(citations, collapse = "\n")
}

#' Generate methods and results markdown for AI interpretation
#'
#' @description
#' Creates a clean, structured markdown output emphasizing statistical methods
#' and results. Designed for AI interpretation - no junk, just the essentials.
#'
#' @param results Results object from Saqrmisc function
#' @param analysis_type Type of analysis (e.g., "group_comparison", "correlation")
#' @param metadata List with analysis metadata (test_type, variables, groups, etc.)
#'
#' @return Character string with clean markdown
#' @noRd
generate_methods_results_md <- function(results, analysis_type = "analysis", metadata = list()) {
  parts <- c()

  # === METHODS SECTION ===
  parts <- c(parts, "## Methods", "")

  # Build methods description based on analysis type and metadata
  methods_text <- switch(analysis_type,
    "group_comparison" = {
      test_type <- metadata$test_type %||% "statistical test"
      n_groups <- metadata$n_groups %||% 2
      variables <- metadata$variables %||% "outcome variables"
      grouping_var <- metadata$grouping_var %||% "grouping variable"

      if (n_groups == 2) {
        if (grepl("Mann-Whitney|Wilcox", test_type, ignore.case = TRUE)) {
          paste0("A Mann-Whitney U test (nonparametric) was used to compare groups on ",
                 if (is.character(variables)) paste(variables, collapse = ", ") else "the outcome variables",
                 ". Effect sizes were calculated using rank-biserial correlation. ",
                 "All analyses were conducted using R (R Core Team, 2024) with the Saqrmisc package.")
        } else if (grepl("t-test|t test", test_type, ignore.case = TRUE)) {
          paste0("Independent samples t-tests were conducted to compare groups on ",
                 if (is.character(variables)) paste(variables, collapse = ", ") else "the outcome variables",
                 ". Cohen's d was calculated as the effect size measure. ",
                 "All analyses were conducted using R (R Core Team, 2024) with the Saqrmisc package.")
        } else {
          paste0("A ", test_type, " was used to compare two groups. ",
                 "All analyses were conducted using R (R Core Team, 2024) with the Saqrmisc package.")
        }
      } else {
        if (grepl("Kruskal|nonparametric", test_type, ignore.case = TRUE)) {
          paste0("Kruskal-Wallis tests (nonparametric) were conducted to compare ",
                 n_groups, " groups on ",
                 if (is.character(variables)) paste(variables, collapse = ", ") else "the outcome variables",
                 ". Epsilon-squared was calculated as the effect size. Post-hoc pairwise comparisons used Wilcoxon tests with Bonferroni correction. ",
                 "All analyses were conducted using R (R Core Team, 2024) with the Saqrmisc package.")
        } else {
          paste0("One-way ANOVAs were conducted to compare ",
                 n_groups, " groups on ",
                 if (is.character(variables)) paste(variables, collapse = ", ") else "the outcome variables",
                 ". Eta-squared was calculated as the effect size. Post-hoc comparisons used ",
                 metadata$posthoc_method %||% "Games-Howell",
                 " tests. All analyses were conducted using R (R Core Team, 2024) with the Saqrmisc package.")
        }
      }
    },
    "correlation" = {
      paste0("Correlation analysis was performed using ",
             metadata$method %||% "Pearson",
             " correlation coefficients. All analyses were conducted using R (R Core Team, 2024) with the Saqrmisc package.")
    },
    "categorical" = {
      paste0("Chi-square test of independence was used to examine the association ",
             "between categorical variables. Cramer's V was calculated as the effect size. ",
             "All analyses were conducted using R (R Core Team, 2024) with the Saqrmisc package.")
    },
    "descriptive" = {
      paste0("Descriptive statistics including means, standard deviations, and sample sizes were calculated. ",
             "All analyses were conducted using R (R Core Team, 2024) with the Saqrmisc package.")
    },
    # Default
    paste0("Statistical analysis was conducted using R (R Core Team, 2024) with the Saqrmisc package.")
  )

  parts <- c(parts, methods_text, "")

  # === RESULTS SECTION ===
  parts <- c(parts, "## Results", "")

  # Add sample size info if available
  if (!is.null(metadata$total_n)) {
    parts <- c(parts, paste0("**Sample:** N = ", metadata$total_n), "")
  }

  # Add groups info if available
  if (!is.null(metadata$groups) && length(metadata$groups) > 0) {
    parts <- c(parts, paste0("**Groups:** ", paste(metadata$groups, collapse = ", ")), "")
  }

  # Generate results table - prioritize clean data
  if (!is.null(results$summary_data) && inherits(results$summary_data, "data.frame")) {
    df <- as.data.frame(results$summary_data)

    # Select only key statistical columns for clean output
    key_cols <- c(
      "variable", "Variable",
      "category", "group", "Group", "LLM",
      "n", "N",
      "mean", "Mean",
      "sd", "SD",
      "p_value", "p",
      "efsz", "ES", "effect_size",
      "ef_type", "test_type"
    )

    keep_cols <- intersect(key_cols, names(df))
    if (length(keep_cols) > 0) {
      df_clean <- df[, keep_cols, drop = FALSE]
      md_table <- df_to_markdown(df_clean, digits = 3)
      if (!is.null(md_table) && nchar(md_table) > 0) {
        parts <- c(parts, "### Summary Statistics", "", md_table, "")
      }
    }
  }

  # Add significant findings summary if available
  if (!is.null(results$summary_data) && "p_value" %in% names(results$summary_data)) {
    df <- as.data.frame(results$summary_data)
    sig_results <- df[!is.na(df$p_value) & df$p_value < 0.05, , drop = FALSE]

    if (nrow(sig_results) > 0) {
      # Get unique significant variables
      if ("variable" %in% names(sig_results)) {
        sig_vars <- unique(sig_results$variable)
        parts <- c(parts, "### Significant Findings", "")

        for (var in sig_vars) {
          var_data <- sig_results[sig_results$variable == var, , drop = FALSE]
          p_val <- min(var_data$p_value, na.rm = TRUE)
          ef_val <- var_data$efsz[1]
          ef_type <- var_data$ef_type[1]

          p_str <- if (p_val < 0.001) "p < .001" else sprintf("p = %.3f", p_val)
          ef_str <- if (!is.na(ef_val)) sprintf(", %s = %.3f", ef_type %||% "effect size", ef_val) else ""

          parts <- c(parts, paste0("- **", var, "**: ", p_str, ef_str))
        }
        parts <- c(parts, "")
      }
    }

    # Note non-significant findings
    nonsig_results <- df[is.na(df$p_value) | df$p_value >= 0.05, , drop = FALSE]
    if (nrow(nonsig_results) > 0 && "variable" %in% names(nonsig_results)) {
      nonsig_vars <- unique(nonsig_results$variable)
      if (length(nonsig_vars) > 0) {
        parts <- c(parts, "### Non-significant Findings", "")
        parts <- c(parts, paste0("No significant differences were found for: ",
                                 paste(nonsig_vars, collapse = ", "), "."), "")
      }
    }
  }

  # === REFERENCES SECTION WITH BIBTEX ===
  parts <- c(parts, "## References (BibTeX)", "")

  # Add R citation
  citations <- get_citations_bibtex(packages = c("effectsize", "BayesFactor"))
  parts <- c(parts, citations)

  paste(parts, collapse = "\n")
}

#' Internal function to interpret results using AI
#'
#' @description
#' Takes analysis results and passes them to AI for interpretation.
#' Used internally by Saqrmisc functions when pass = TRUE.
#'
#' @param results Results object from analysis function
#' @param analysis_type Type of analysis
#' @param metadata List with analysis metadata
#' @param ... Additional arguments passed to pass()
#'
#' @return The AI interpretation (invisibly)
#' @noRd
interpret_with_ai <- function(results, analysis_type = "analysis", metadata = list(), ...) {
  # Generate clean methods/results markdown

  md_content <- generate_methods_results_md(results, analysis_type, metadata)

  # Create a minimal object for pass()
  pass_obj <- list(
    markdown = md_content,
    type = analysis_type
  )
  class(pass_obj) <- c("saqr_result", "list")

  # Call pass with the clean markdown
  interpretation <- pass(
    pass_obj,
    action = "write",
    style = "scientific",
    ...
  )

  invisible(interpretation)
}

#' Smart output capture for different object types
#' @noRd
capture_smart_output <- function(x) {
  obj_class <- class(x)

  # Handle saqr_result objects - use markdown if available
 if (inherits(x, "saqr_result")) {
    if (!is.null(x$markdown) && nchar(trimws(x$markdown)) > 0) {
      return(x$markdown)
    }
    # Fallback: generate markdown
    return(generate_saqr_markdown(x))
  }

  # Helper to check if object is data frame or tibble
  is_df_like <- function(obj) {
    inherits(obj, "data.frame") || inherits(obj, "tbl_df") || inherits(obj, "tbl")
  }

  # Helper to convert df to markdown with smart column selection
  df_to_md <- function(obj, label) {
    if (is.null(obj) || !is_df_like(obj) || nrow(obj) == 0) return(NULL)

    df <- as.data.frame(obj)

    # Select only key numeric/character columns, exclude complex list columns
    keep_cols <- c()
    for (col in names(df)) {
      val <- df[[col]]
      # Keep if it's numeric, character, factor, or logical (not list)
      if (is.numeric(val) || is.character(val) || is.factor(val) || is.logical(val)) {
        # Skip columns that look like they contain long text or reports
        if (is.character(val) && any(nchar(val) > 200, na.rm = TRUE)) next
        keep_cols <- c(keep_cols, col)
      }
    }

    # Prioritize key statistical columns
    priority_cols <- c(
      "variable", "Variable", "group", "Group", "category", "LLM",
      "n", "N", "mean", "Mean", "sd", "SD", "se", "SE", "median",
      "p_value", "p", "p_adjusted", "efsz", "ES", "effect_size",
      "ef_type", "test_type", "statistic", "t_statistic", "f_statistic",
      "df", "t_df", "df1", "df2"
    )

    # Reorder: priority columns first, then others
    priority_present <- intersect(priority_cols, keep_cols)
    other_cols <- setdiff(keep_cols, priority_cols)
    keep_cols <- c(priority_present, other_cols)

    # Limit to reasonable number of columns
    if (length(keep_cols) > 12) {
      keep_cols <- keep_cols[1:12]
    }

    if (length(keep_cols) == 0) return(NULL)

    df_clean <- df[, keep_cols, drop = FALSE]
    md <- df_to_markdown(df_clean)
    if (!is.null(md) && nchar(trimws(md)) > 0) {
      return(paste0(label, "\n\n", md))
    }
    NULL
  }

  # Helper to safely print data frame/tibble (fallback)
  safe_print_df <- function(obj, label) {
    if (is.null(obj)) return(NULL)
    if (!is_df_like(obj)) return(NULL)
    if (nrow(obj) == 0) return(NULL)

    if (inherits(obj, "tbl_df")) {
      output <- paste(utils::capture.output(print(as.data.frame(obj))), collapse = "\n")
    } else {
      output <- paste(utils::capture.output(print(obj)), collapse = "\n")
    }

    if (nchar(trimws(output)) > 0) {
      return(paste0(label, "\n", output))
    }
    NULL
  }

  # Handle Saqrmisc results (lists with $data, $display, or $table components)
  if (is.list(x) && !is.data.frame(x) && !inherits(x, "tbl_df")) {
    parts <- c()

    # Check for summary_data first (from compare_groups) - use markdown format
    captured <- df_to_md(x$summary_data, "## Summary Data")
    if (!is.null(captured)) parts <- c(parts, captured)

    # Check for summary_table (gt table data)
    if (!is.null(x$summary_table)) {
      if (inherits(x$summary_table, "gt_tbl") && !is.null(x$summary_table$`_data`)) {
        captured <- df_to_md(x$summary_table$`_data`, "## Summary Table")
        if (!is.null(captured)) parts <- c(parts, captured)
      } else if (is_df_like(x$summary_table)) {
        captured <- safe_print_df(x$summary_table, "Summary Table:")
        if (!is.null(captured)) parts <- c(parts, captured)
      }
    }

    # Check for display data frame (formatted for viewing)
    captured <- df_to_md(x$display, "## Display Table")
    if (!is.null(captured)) parts <- c(parts, captured)

    # Check for raw data
    captured <- df_to_md(x$data, "## Data")
    if (!is.null(captured)) parts <- c(parts, captured)

    # Check for correlation matrix - convert to markdown table
    if (!is.null(x$correlation_matrix) && is.matrix(x$correlation_matrix)) {
      cor_df <- as.data.frame(round(x$correlation_matrix, 3))
      cor_df <- cbind(Variable = rownames(cor_df), cor_df)
      md_table <- df_to_markdown(cor_df)
      if (!is.null(md_table)) {
        parts <- c(parts, paste0("## Correlation Matrix\n\n", md_table))
      }
    }

    # Check for p-values matrix - convert to markdown table
    if (!is.null(x$p_matrix) && is.matrix(x$p_matrix)) {
      p_df <- as.data.frame(round(x$p_matrix, 4))
      p_df <- cbind(Variable = rownames(p_df), p_df)
      md_table <- df_to_markdown(p_df, digits = 4)
      if (!is.null(md_table)) {
        parts <- c(parts, paste0("## P-values\n\n", md_table))
      }
    }

    # Check for n_matrix (sample sizes) - convert to markdown table
    if (!is.null(x$n_matrix) && is.matrix(x$n_matrix)) {
      n_df <- as.data.frame(x$n_matrix)
      n_df <- cbind(Variable = rownames(n_df), n_df)
      md_table <- df_to_markdown(n_df, digits = 0)
      if (!is.null(md_table)) {
        parts <- c(parts, paste0("## Sample Sizes\n\n", md_table))
      }
    }

    # Check for test results (chi-square, etc.)
    if (!is.null(x$test_results)) {
      if (is.list(x$test_results)) {
        parts <- c(parts, paste0("Test Results:\n", paste(utils::capture.output(print(x$test_results)), collapse = "\n")))
      } else if (is.character(x$test_results)) {
        parts <- c(parts, paste0("Test Results: ", x$test_results))
      }
    }

    # Check for chi_square results
    if (!is.null(x$chi_square)) {
      parts <- c(parts, paste0("Chi-Square Test:\n", paste(utils::capture.output(print(x$chi_square)), collapse = "\n")))
    }

    # Check for fisher results
    if (!is.null(x$fisher)) {
      parts <- c(parts, paste0("Fisher's Exact Test:\n", paste(utils::capture.output(print(x$fisher)), collapse = "\n")))
    }

    # Check for effect size (Cramer's V, etc.)
    if (!is.null(x$cramers_v)) {
      parts <- c(parts, paste0("Cramer's V: ", round(x$cramers_v, 3)))
    }

    # Check for footnotes/notes (often contain test results)
    if (!is.null(x$footnote) && is.character(x$footnote)) {
      parts <- c(parts, paste0("Note: ", x$footnote))
    }
    if (!is.null(x$notes) && is.character(x$notes)) {
      parts <- c(parts, paste0("Notes: ", paste(x$notes, collapse = "\n")))
    }

    # If we captured something useful, return it
    if (length(parts) > 0) {
      return(paste(parts, collapse = "\n\n"))
    }

    # Fallback: try to print all data frame elements in the list
    for (nm in names(x)) {
      if (is_df_like(x[[nm]]) && nrow(x[[nm]]) > 0) {
        captured <- safe_print_df(x[[nm]], paste0(nm, ":"))
        if (!is.null(captured)) parts <- c(parts, captured)
      }
    }
    if (length(parts) > 0) {
      return(paste(parts, collapse = "\n\n"))
    }
  }

  # Handle gt tables - convert to data frame if possible
  if ("gt_tbl" %in% obj_class) {
    # Try to extract the underlying data
    if (!is.null(x$`_data`)) {
      return(paste(
        "GT Table Contents:",
        paste(utils::capture.output(print(as.data.frame(x$`_data`))), collapse = "\n"),
        sep = "\n"
      ))
    }
  }

  # Handle markdown_table class (from Saqrmisc)
  if ("markdown_table" %in% obj_class) {
    return(as.character(x))
  }

  # Handle data frames nicely
  if (is.data.frame(x)) {
    return(paste(utils::capture.output(print(x)), collapse = "\n"))
  }

  # Default: capture print output
  paste(utils::capture.output(print(x)), collapse = "\n")
}

#' Pass R Output to AI for Interpretation
#'
#' @description
#' Pipes any R output (test results, model summaries, tables) to an AI model
#' for scientific interpretation. Perfect for getting publication-ready
#' interpretations of statistical results.
#'
#' @param x Any R object to interpret (test result, model, data frame, etc.)
#' @param prompt Custom prompt to use. If NULL, builds from action and style.
#'   Note: This REPLACES the default prompt entirely.
#' @param add_prompt Additional instructions to ADD to the default prompt.
#'   Unlike `prompt`, this appends to (not replaces) the auto-generated prompt.
#' @param action What to do with the output:
#'   \itemize{
#'     \item `"write"` (default): Write publication-ready text (methods/results)
#'     \item `"interpret"`: Interpret the statistical results
#'     \item `"explain"`: Explain what the analysis does and means
#'     \item `"summarize"`: Brief summary of key findings
#'     \item `"critique"`: Critical evaluation with limitations
#'     \item `"suggest"`: Suggest follow-up analyses
#'   }
#' @param style Interpretation style:
#'   \itemize{
#'     \item `"scientific"` (default): APA-style for academic papers
#'     \item `"simple"`: Plain language, no jargon
#'     \item `"detailed"`: Comprehensive with assumptions, limitations, caveats
#'     \item `"brief"`: Just the key takeaway
#'   }
#' @param output Output format:
#'   \itemize{
#'     \item `"text"` (default): Plain text
#'     \item `"markdown"` or `"md"`: Markdown formatted
#'     \item `"latex"`: LaTeX formatted for papers
#'     \item `"html"`: HTML formatted
#'   }
#' @param provider AI provider: `"openai"` (default), `"anthropic"`, `"gemini"`, or `"openrouter"`.
#' @param model Model to use. Defaults: `"gpt-4.1-nano"` (OpenAI), `"claude-sonnet-4-20250514"` (Anthropic),
#'   `"gemini-2.5-flash"` (Gemini), `"anthropic/claude-sonnet-4"` (OpenRouter).
#' @param base_url Custom API base URL for OpenAI-compatible servers (e.g., LM Studio,
#'   Ollama, vLLM). Example: `"http://127.0.0.1:1234"` for LM Studio. When set,
#'   uses OpenAI-compatible format regardless of provider setting.
#' @param api_key API key. If NULL, checks environment variables
#'   (`OPENAI_API_KEY`, `ANTHROPIC_API_KEY`, `GEMINI_API_KEY`, or `OPENROUTER_API_KEY`),
#'   then prompts interactively.
#'   For local servers like LM Studio, use `api_key = "none"` or any string.
#' @param context Optional context about your study (e.g., "This is a study on
#'   student learning outcomes with N=500 participants")
#' @param system_message Optional custom instructions for the AI (e.g., "Focus on
#'   clinical implications", "Be more concise", "Emphasize effect sizes")
#' @param auto_local Logical. Automatically detect and use local AI servers
#'   (LM Studio on port 1234, Ollama on port 11434)? Default: TRUE.
#'   Set to FALSE to force using cloud providers.
#' @param copy Logical. Copy result to clipboard? Default: FALSE
#' @param quiet Logical. Suppress messages? Default: FALSE
#'
#' @return Character string with the AI interpretation (invisibly).
#'   Also prints the interpretation.
#'
#' @details
#' On first use, you'll be prompted to enter your API key. The key is stored
#' in your R environment for the session. To persist it, add to your .Renviron:
#' ```
#' ANTHROPIC_API_KEY=your-key-here
#' # or
#' OPENAI_API_KEY=your-key-here
#' ```
#'
#' @examples
#' \dontrun{
#' # Basic usage - pipe test results
#' t.test(mpg ~ am, data = mtcars) |> pass()
#'
#' # With context
#' cor.test(mtcars$mpg, mtcars$hp) |>
#'   pass(context = "Studying fuel efficiency in 1974 automobiles")
#'
#' # Different actions
#' lm(mpg ~ wt + hp, data = mtcars) |> summary() |> pass(action = "write")
#' chisq.test(mtcars$cyl, mtcars$am) |> pass(action = "explain", style = "simple")
#'
#' # Get LaTeX output for paper
#' aov(mpg ~ factor(cyl), data = mtcars) |> summary() |>
#'   pass(action = "write", output = "latex")
#'
#' # Critique an analysis
#' lm(mpg ~ ., data = mtcars) |> summary() |> pass(action = "critique")
#'
#' # Custom prompt (REPLACES default - specific request to the AI)
#' my_results |> pass(prompt = "Focus only on the interaction effects")
#'
#' # Add to default prompt (keeps "write methods/results" + your addition)
#' my_results |> pass(add_prompt = "Also mention limitations of the sample size")
#' my_results |> pass(action = "write", add_prompt = "Include a brief discussion section")
#'
#' # Custom context (about your study)
#' t.test(score ~ group, data = mydata) |>
#'   pass(context = "RCT comparing drug vs placebo, N=200 patients with diabetes")
#'
#' # Custom system message (instructions for the AI)
#' my_results |> pass(system_message = "Focus on clinical implications and effect sizes")
#'
#' # Combine all customizations
#' lm(outcome ~ treatment * age, data = mydata) |> summary() |>
#'   pass(
#'     action = "write",
#'     context = "Phase 3 clinical trial for hypertension medication",
#'     system_message = "Emphasize clinical significance over statistical significance",
#'     prompt = "Pay special attention to the treatment-age interaction"
#'   )
#' }
#'
#' @export
pass <- function(x,
                 prompt = NULL,
                 add_prompt = NULL,
                 action = c("write", "interpret", "explain", "summarize", "critique", "suggest"),
                 style = c("scientific", "simple", "detailed", "brief"),
                 output = c("text", "markdown", "md", "latex", "html"),
                 provider = c("openai", "anthropic", "gemini", "openrouter"),
                 model = NULL,
                 base_url = NULL,
                 api_key = NULL,
                 context = NULL,
                 system_message = NULL,
                 auto_local = TRUE,
                 copy = FALSE,
                 quiet = FALSE) {

  action <- match.arg(action)
  style <- match.arg(style)
  output <- match.arg(output)
  if (output == "md") output <- "markdown"  # Alias
  provider <- match.arg(provider)

  # Auto-detect local servers if base_url not provided and auto_local is TRUE
  if (is.null(base_url) && auto_local) {
    available_servers <- detect_local_servers()
    if (length(available_servers) > 0) {
      selected <- select_local_server(available_servers, quiet)
      if (!is.null(selected) && selected != "USE_CLOUD") {
        base_url <- selected
      }
      # If "USE_CLOUD" or NULL, base_url stays NULL and cloud provider will be used
    }
  }

  # Check if using local server
  use_local <- !is.null(base_url)

  # Smart capture of R output based on object type
  output_text <- capture_smart_output(x)

  # Also try to get the class/type info
  obj_class <- paste(class(x), collapse = ", ")
  obj_info <- paste0("Object class: ", obj_class)

  # Get API key (skip for local servers if not provided)
  if (use_local && is.null(api_key)) {
    api_key <- "local"  # Placeholder for local servers
  } else if (!use_local) {
    api_key <- get_api_key(provider, api_key, quiet)
  }

  # Set default model (check stored options first)
  if (is.null(model)) {
    if (use_local) {
      model <- "local-model"  # Placeholder, LM Studio uses loaded model
    } else {
      # Check for stored model option
      stored_model <- getOption(paste0("saqrmisc.", provider, "_model"))
      if (!is.null(stored_model)) {
        model <- stored_model
      } else {
        model <- switch(provider,
          openai = "gpt-4.1-nano",
          anthropic = "claude-sonnet-4-20250514",
          gemini = "gemini-2.5-flash",
          openrouter = "anthropic/claude-sonnet-4"
        )
      }
    }
  }

  # Check for stored base_url option (if not already set)
  if (is.null(base_url) && !use_local) {
    stored_base_url <- getOption(paste0("saqrmisc.", provider, "_base_url"))
    if (!is.null(stored_base_url)) {
      base_url <- stored_base_url
      use_local <- TRUE  # Treat custom base_url as local-style call
    }
  }

  # Build the prompt
  system_prompt <- build_system_prompt(action, style, output, system_message)
  user_prompt <- build_user_prompt(output_text, obj_info, prompt, add_prompt, context, action)

  if (!quiet) {
    if (use_local) {
      message("Sending to local server (", base_url, ")...")
    } else {
      message("Sending to ", provider, " (", model, ")...")
    }
  }

  # Call the API
  if (use_local) {
    response <- call_local_openai(base_url, model, api_key, system_prompt, user_prompt)
  } else {
    response <- call_ai_api(
      provider = provider,
      model = model,
      api_key = api_key,
      system_prompt = system_prompt,
      user_prompt = user_prompt
    )
  }

  # Print the response
  cat("\n", strrep("=", 60), "\n", sep = "")
  cat("AI ", tools::toTitleCase(action), " (", style, " style, ", output, " format)\n", sep = "")
  cat(strrep("=", 60), "\n\n", sep = "")
  cat(response, "\n")

  # Add disclaimer
  cat("\n", strrep("-", 60), "\n", sep = "")
  cat("IMPORTANT: AI-generated interpretations may contain errors.\n")
  cat("Always verify statistics against the original output below.\n")
  cat(strrep("-", 60), "\n", sep = "")

 # Print original R output for verification
  cat("\nORIGINAL R OUTPUT (for verification):\n")
  cat(strrep("-", 40), "\n", sep = "")
  cat(output_text, "\n")
  cat(strrep("=", 60), "\n", sep = "")

  # Copy to clipboard if requested
  if (copy) {
    if (requireNamespace("clipr", quietly = TRUE)) {
      clipr::write_clip(response)
      if (!quiet) message("Copied to clipboard!")
    } else {
      warning("Install 'clipr' package to enable clipboard support")
    }
  }

  invisible(response)
}

#' Check if a local server is running
#' @noRd
check_local_server <- function(url) {
  if (!requireNamespace("httr2", quietly = TRUE)) {
    return(FALSE)
  }
  tryCatch({
    req <- httr2::request(paste0(url, "/v1/models")) |>
      httr2::req_timeout(2)
    resp <- httr2::req_perform(req)
    httr2::resp_status(resp) == 200
  }, error = function(e) FALSE)
}

#' Detect available local servers (LM Studio, Ollama)
#' @noRd
detect_local_servers <- function() {
  servers <- list(
    lmstudio = list(name = "LM Studio", url = "http://127.0.0.1:1234"),
    ollama = list(name = "Ollama", url = "http://127.0.0.1:11434")
  )

  available <- list()
  for (id in names(servers)) {
    if (check_local_server(servers[[id]]$url)) {
      available[[id]] <- servers[[id]]
    }
  }
  available
}

#' Prompt user to select a local server
#' @noRd
select_local_server <- function(available, quiet = FALSE) {
  if (length(available) == 0) {
    return(NULL)
  }

  if (length(available) == 1) {
    server <- available[[1]]
    if (!quiet) {
      message("Found ", server$name, " running locally. Using it.")
      message("(To use a cloud provider instead, set provider explicitly or use base_url = NULL)")
    }
    return(server$url)
  }

  # Multiple servers found - ask user
  if (!interactive()) {
    # Non-interactive: use first available
    server <- available[[1]]
    if (!quiet) message("Using ", server$name, " (first available local server)")
    return(server$url)
  }

  # Interactive: let user choose
  if (!quiet) {
    message("\n", strrep("=", 50))
    message("Multiple local AI servers detected!")
    message(strrep("=", 50))
    for (i in seq_along(available)) {
      message(i, ". ", available[[i]]$name, " (", available[[i]]$url, ")")
    }
    message(length(available) + 1, ". Use cloud provider (enter API key)")
    message(strrep("=", 50))
  }

  choice <- readline(prompt = "Select server [1]: ")
  choice <- if (nzchar(choice)) as.integer(choice) else 1L

  if (is.na(choice) || choice < 1 || choice > length(available) + 1) {
    choice <- 1L
  }

  if (choice > length(available)) {
    # User chose cloud provider - return special marker
    return("USE_CLOUD")
  }

  available[[choice]]$url
}

#' Get or prompt for API key
#' @noRd
get_api_key <- function(provider, api_key = NULL, quiet = FALSE) {
  if (!is.null(api_key)) {
    return(api_key)
  }

  # Check environment variable
  env_var <- switch(provider,
    openai = "OPENAI_API_KEY",
    anthropic = "ANTHROPIC_API_KEY",
    gemini = "GEMINI_API_KEY",
    openrouter = "OPENROUTER_API_KEY"
  )
  key <- Sys.getenv(env_var)

  if (nzchar(key)) {
    return(key)
  }

  # Check package options
  opt_name <- paste0("saqrmisc.", tolower(provider), "_key")
  key <- getOption(opt_name)

  if (!is.null(key) && nzchar(key)) {
    return(key)
  }

  # Interactive prompt
  if (!interactive()) {
    stop("API key required. Set ", env_var, " environment variable or pass api_key parameter.")
  }

  if (!quiet) {
    message("\n", strrep("=", 50))
    message("API Key Setup")
    message(strrep("=", 50))
    message("No API key found for ", provider)
    message("\nTo avoid this prompt, add to your .Renviron file:")
    message("  ", env_var, "=your-key-here")
    message("\nOr set in R: Sys.setenv(", env_var, "='your-key')")
    message(strrep("=", 50), "\n")
  }

  key <- readline(prompt = paste0("Enter your ", provider, " API key: "))

  if (!nzchar(key)) {
    stop("API key is required")
  }

  # Store for session
  options(setNames(list(key), opt_name))

  return(key)
}

#' Build system prompt based on action, style, output, and custom message
#' @noRd
build_system_prompt <- function(action, style, output, system_message = NULL) {
  base <- paste0(
    "You are an expert statistician helping researchers interpret R output. ",
    "You can interpret ANY type of R output including: descriptive statistics tables, ",
    "correlation matrices, frequency tables, regression summaries, t-tests, ANOVAs, ",
    "chi-square tests, mixed models, data frames, and any other statistical output. ",
    "Even if there is no formal hypothesis test, you should describe and interpret ",
    "what the numbers mean. Always identify the type of analysis first. "
  )

  # Core scientific writing guidelines (applies to all actions)
  scientific_guidelines <- paste0(
    "IMPORTANT GUIDELINES: ",
    "1. Report means (M), standard deviations (SD), sample sizes (n/N), effect sizes, confidence intervals, and p-values when available, using APA conventions. ",
    "2. Emphasize direction, magnitude, and practical relevance of effects, not only statistical significance. ",
    "3. If effect sizes are small or results are limited, explicitly note this. ",
    "4. Do NOT speculate beyond the data provided. ",
    "5. Do NOT invent values, tests, or assumptions - treat content exactly as reported. ",
    "6. Write in past tense with neutral, formal tone. "
  )

  # Action-specific instructions
  action_prompts <- list(
    interpret = paste0(
      "Identify the type of analysis (descriptive statistics, group comparison, correlation, regression, etc.). ",
      "Describe the main findings in formal scientific language. ",
      "For descriptive tables: report central tendency (M), variability (SD), and sample sizes. ",
      "For correlations: report r values, direction, strength (weak/moderate/strong), and significance. ",
      "For group comparisons: report group means, effect sizes (Cohen's d, eta-squared), and test statistics. ",
      "For frequency tables: report counts, percentages, and any association tests. "
    ),
    explain = paste0(
      "Explain what this analysis shows and what the output means. ",
      "Help the reader understand what was computed and why it matters. ",
      "Define any technical terms used. "
    ),
    write = paste0(
      "Write publication-ready text with clearly labeled **Methods** and **Results** sections. ",
      "In **Methods**: describe the statistical approach used, cite R (R Core Team, 2024) and relevant packages. ",
      "In **Results**: write polished paragraphs (not bullet points) in past tense. ",
      "Report all statistics in APA format: M = X.XX, SD = X.XX, t(df) = X.XX, p = .XXX, d = X.XX. ",
      "For correlations: r(df) = .XX, p = .XXX. For ANOVA: F(df1, df2) = X.XX, p = .XXX, eta-sq = .XX. ",
      "End with a **References** section citing R and packages used. "
    ),
    summarize = paste0(
      "Provide a brief summary (2-4 sentences) of the key findings. ",
      "Focus on the most important takeaways. ",
      "Mention effect sizes and practical significance, not just p-values. "
    ),
    critique = paste0(
      "Provide a critical evaluation including: ",
      "1. Potential limitations of the analysis. ",
      "2. Assumptions that may be violated. ",
      "3. Alternative approaches that could be considered. ",
      "4. What the results do NOT tell us. "
    ),
    suggest = paste0(
      "Based on these results, suggest appropriate follow-up analyses. ",
      "Consider: additional variables to examine, alternative statistical approaches, ",
      "replication needs, or theoretical implications. "
    )
  )

  # Style-specific instructions
  style_prompts <- list(
    scientific = "Use APA style. Be precise about effect sizes, confidence intervals, and statistical significance. Mention practical significance, not just statistical significance.",
    simple = "Use plain language that anyone can understand. Avoid jargon. Use analogies if helpful. Focus on what the results MEAN, not the numbers.",
    detailed = "Be comprehensive. Include: what the test does, key assumptions, interpretation of all statistics, effect sizes, limitations, and caveats.",
    brief = "Be concise. Just the key takeaway in 2-3 sentences maximum."
  )

  # Output format instructions
  output_prompts <- list(
    text = "Format your response as plain text.",
    markdown = "Format your response using Markdown (headers, bold, lists, etc.).",
    latex = "Format your response using LaTeX. Use proper statistical notation (e.g., $p < .05$, $\\beta$, etc.). Format tables with tabular environment if needed.",
    html = "Format your response using HTML tags for structure and emphasis."
  )

  # Build the prompt: base + guidelines + action + style + output
  result <- paste0(
    base,
    scientific_guidelines,
    action_prompts[[action]], " ",
    style_prompts[[style]], " ",
    output_prompts[[output]]
  )

  # Add custom system message if provided
  if (!is.null(system_message) && nzchar(system_message)) {
    result <- paste0(result, " IMPORTANT: ", system_message)
  }

  result
}

#' Build user prompt
#' @noRd
build_user_prompt <- function(output_text, obj_info, custom_prompt, add_prompt, context, action) {
  parts <- c()

  if (!is.null(context)) {
    parts <- c(parts, paste0("Study context: ", context, "\n"))
  }

  parts <- c(parts, paste0(obj_info, "\n"))
  parts <- c(parts, paste0("R Output:\n```\n", output_text, "\n```\n"))

  if (!is.null(custom_prompt)) {
    # Custom prompt replaces the default entirely
    parts <- c(parts, paste0("\nSpecific request: ", custom_prompt))
  } else {
    # Default request based on action
    action_requests <- list(
      interpret = "Please interpret these results.",
      explain = "Please explain what this analysis does and what the results mean.",
      write = "Please write publication-ready Methods and Results text for these findings.",
      summarize = "Please summarize the key findings.",
      critique = "Please provide a critical evaluation of this analysis.",
      suggest = "Please suggest follow-up analyses based on these results."
    )
    parts <- c(parts, paste0("\n", action_requests[[action]]))
  }

  # Add additional prompt (appends to default or custom prompt)
  if (!is.null(add_prompt) && nzchar(add_prompt)) {
    parts <- c(parts, paste0("\nAdditional instructions: ", add_prompt))
  }

  paste(parts, collapse = "\n")
}

#' Call AI API
#' @noRd
call_ai_api <- function(provider, model, api_key, system_prompt, user_prompt) {
  switch(provider,
    openai = call_openai(model, api_key, system_prompt, user_prompt),
    anthropic = call_anthropic(model, api_key, system_prompt, user_prompt),
    gemini = call_gemini(model, api_key, system_prompt, user_prompt),
    openrouter = call_openrouter(model, api_key, system_prompt, user_prompt)
  )
}

#' Call Anthropic API
#' @noRd
call_anthropic <- function(model, api_key, system_prompt, user_prompt) {
  if (!requireNamespace("httr2", quietly = TRUE)) {
    stop("Package 'httr2' is required. Install with: install.packages('httr2')")
  }

  body <- list(
    model = model,
    max_tokens = 2048,
    system = system_prompt,
    messages = list(
      list(role = "user", content = user_prompt)
    )
  )

  req <- httr2::request("https://api.anthropic.com/v1/messages") |>
    httr2::req_headers(
      `x-api-key` = api_key,
      `anthropic-version` = "2023-06-01",
      `content-type` = "application/json"
    ) |>
    httr2::req_body_json(body) |>
    httr2::req_error(is_error = function(resp) FALSE)

  resp <- httr2::req_perform(req)

  if (httr2::resp_status(resp) != 200) {
    error_body <- httr2::resp_body_json(resp)
    stop("API error: ", error_body$error$message %||% httr2::resp_status_desc(resp))
  }

  result <- httr2::resp_body_json(resp)
  result$content[[1]]$text
}

#' Call OpenAI API
#' @noRd
call_openai <- function(model, api_key, system_prompt, user_prompt) {
  if (!requireNamespace("httr2", quietly = TRUE)) {
    stop("Package 'httr2' is required. Install with: install.packages('httr2')")
  }

  body <- list(
    model = model,
    messages = list(
      list(role = "system", content = system_prompt),
      list(role = "user", content = user_prompt)
    )
  )

  req <- httr2::request("https://api.openai.com/v1/chat/completions") |>
    httr2::req_headers(
      Authorization = paste("Bearer", api_key),
      `Content-Type` = "application/json"
    ) |>
    httr2::req_body_json(body) |>
    httr2::req_error(is_error = function(resp) FALSE)

  resp <- httr2::req_perform(req)

  if (httr2::resp_status(resp) != 200) {
    error_body <- httr2::resp_body_json(resp)
    stop("API error: ", error_body$error$message %||% httr2::resp_status_desc(resp))
  }

  result <- httr2::resp_body_json(resp)
  result$choices[[1]]$message$content
}

#' Call Local OpenAI-compatible API (LM Studio, Ollama, vLLM, etc.)
#' @noRd
call_local_openai <- function(base_url, model, api_key, system_prompt, user_prompt) {
  if (!requireNamespace("httr2", quietly = TRUE)) {
    stop("Package 'httr2' is required. Install with: install.packages('httr2')")
  }

  # Ensure base_url ends without trailing slash
  base_url <- sub("/$", "", base_url)
  url <- paste0(base_url, "/v1/chat/completions")

  # Build request body - model is optional for local servers
  body <- list(
    messages = list(
      list(role = "system", content = system_prompt),
      list(role = "user", content = user_prompt)
    )
  )
  # Only add model if it's not the placeholder
  if (model != "local-model") {
    body$model <- model
  }

  req <- httr2::request(url) |>
    httr2::req_headers(
      Authorization = paste("Bearer", api_key),
      `Content-Type` = "application/json"
    ) |>
    httr2::req_body_json(body) |>
    httr2::req_timeout(300) |>  # 5 min timeout for local models
    httr2::req_error(is_error = function(resp) FALSE)

  resp <- httr2::req_perform(req)

  if (httr2::resp_status(resp) != 200) {
    error_body <- tryCatch(
      httr2::resp_body_json(resp),
      error = function(e) list(error = list(message = httr2::resp_status_desc(resp)))
    )
    stop("API error: ", error_body$error$message %||% httr2::resp_status_desc(resp))
  }

  result <- httr2::resp_body_json(resp)
  result$choices[[1]]$message$content
}

#' Call Gemini API
#' @noRd
call_gemini <- function(model, api_key, system_prompt, user_prompt) {
  if (!requireNamespace("httr2", quietly = TRUE)) {
    stop("Package 'httr2' is required. Install with: install.packages('httr2')")
  }

  # Gemini API endpoint
  url <- paste0(
    "https://generativelanguage.googleapis.com/v1beta/models/",
    model,
    ":generateContent?key=",
    api_key
  )

  body <- list(
    system_instruction = list(
      parts = list(list(text = system_prompt))
    ),
    contents = list(
      list(
        parts = list(list(text = user_prompt))
      )
    ),
    generationConfig = list(
      maxOutputTokens = 2048
    )
  )

  req <- httr2::request(url) |>
    httr2::req_headers(`Content-Type` = "application/json") |>
    httr2::req_body_json(body) |>
    httr2::req_error(is_error = function(resp) FALSE)

  resp <- httr2::req_perform(req)

  if (httr2::resp_status(resp) != 200) {
    error_body <- httr2::resp_body_json(resp)
    error_msg <- error_body$error$message %||% httr2::resp_status_desc(resp)
    stop("API error: ", error_msg)
  }

  result <- httr2::resp_body_json(resp)
  result$candidates[[1]]$content$parts[[1]]$text
}

#' Call OpenRouter API
#' @noRd
call_openrouter <- function(model, api_key, system_prompt, user_prompt) {
  if (!requireNamespace("httr2", quietly = TRUE)) {
    stop("Package 'httr2' is required. Install with: install.packages('httr2')")
  }

  body <- list(
    model = model,
    messages = list(
      list(role = "system", content = system_prompt),
      list(role = "user", content = user_prompt)
    )
  )

  req <- httr2::request("https://openrouter.ai/api/v1/chat/completions") |>
    httr2::req_headers(
      Authorization = paste("Bearer", api_key),
      `Content-Type` = "application/json",
      `HTTP-Referer` = "https://github.com/mohsaqr/Saqrmisc",
      `X-Title` = "Saqrmisc R Package"
    ) |>
    httr2::req_body_json(body) |>
    httr2::req_error(is_error = function(resp) FALSE)

  resp <- httr2::req_perform(req)

  if (httr2::resp_status(resp) != 200) {
    error_body <- httr2::resp_body_json(resp)
    stop("API error: ", error_body$error$message %||% httr2::resp_status_desc(resp))
  }

  result <- httr2::resp_body_json(resp)
  result$choices[[1]]$message$content
}

#' Set API Key and Options for Session
#'
#' @description
#' Set your API key and optional default settings for the current R session.
#' Settings are stored as R options and used as defaults by `pass()`.
#'
#' @param key Your API key
#' @param provider Provider name: "openai" (default), "anthropic", "gemini", or "openrouter"
#' @param model Optional. Default model to use for this provider.
#' @param ... Additional options to store (e.g., `base_url`, `timeout`).
#'
#' @return Invisibly returns TRUE.
#'
#' @examples
#' \dontrun{
#' # Just set the key
#' set_api_key("sk-...", "openai")
#'
#' # Set key and default model
#' set_api_key("sk-...", "openai", model = "gpt-4o")
#'
#' # Set key with custom base URL (for Azure OpenAI, etc.)
#' set_api_key("sk-...", "openai", model = "gpt-4", base_url = "https://my-azure.openai.azure.com")
#' }
#'
#' @export
set_api_key <- function(key, provider = c("openai", "anthropic", "gemini", "openrouter"),
                        model = NULL, ...) {
  provider <- match.arg(provider)


  # Set the API key as environment variable
  env_var <- switch(provider,
    openai = "OPENAI_API_KEY",
    anthropic = "ANTHROPIC_API_KEY",
    gemini = "GEMINI_API_KEY",
    openrouter = "OPENROUTER_API_KEY"
  )
  do.call(Sys.setenv, setNames(list(key), env_var))

  # Store additional options
  opts <- list(...)
  if (!is.null(model)) {
    opts$model <- model
  }

  if (length(opts) > 0) {
    # Store options with provider-specific names
    for (opt_name in names(opts)) {
      full_name <- paste0("saqrmisc.", provider, "_", opt_name)
      do.call(options, setNames(list(opts[[opt_name]]), full_name))
    }
    message("API key and settings set for ", provider, " (this session only)")
    message("  Options: ", paste(names(opts), "=", opts, collapse = ", "))
  } else {
    message("API key set for ", provider, " (this session only)")
  }

  invisible(TRUE)
}

#' Set OpenAI API Key
#'
#' @description
#' Convenience alias for `set_api_key(key, "openai", ...)`.
#'
#' @param key Your OpenAI API key
#' @param model Optional. Default model (e.g., "gpt-4o", "gpt-4.1-nano").
#' @param ... Additional options (e.g., `base_url` for Azure OpenAI).
#'
#' @examples
#' \dontrun{
#' set_openai_key("sk-...")
#' set_openai_key("sk-...", model = "gpt-4o")
#' set_openai_key("sk-...", model = "gpt-4", base_url = "https://my-azure.openai.azure.com")
#' }
#'
#' @export
set_openai_key <- function(key, model = NULL, ...) {
  set_api_key(key, "openai", model = model, ...)
}

#' Set Anthropic (Claude) API Key
#'
#' @description
#' Convenience alias for `set_api_key(key, "anthropic", ...)`.
#'
#' @param key Your Anthropic API key
#' @param model Optional. Default model (e.g., "claude-sonnet-4-20250514").
#' @param ... Additional options.
#'
#' @examples
#' \dontrun{
#' set_claude_key("sk-ant-...")
#' set_claude_key("sk-ant-...", model = "claude-sonnet-4-20250514")
#' }
#'
#' @export
set_claude_key <- function(key, model = NULL, ...) {
  set_api_key(key, "anthropic", model = model, ...)
}

#' Set Google Gemini API Key
#'
#' @description
#' Convenience alias for `set_api_key(key, "gemini", ...)`.
#'
#' @param key Your Gemini API key
#' @param model Optional. Default model (e.g., "gemini-2.5-flash").
#' @param ... Additional options.
#'
#' @examples
#' \dontrun{
#' set_gemini_key("AIza...")
#' set_gemini_key("AIza...", model = "gemini-2.5-pro")
#' }
#'
#' @export
set_gemini_key <- function(key, model = NULL, ...) {
  set_api_key(key, "gemini", model = model, ...)
}

#' Set OpenRouter API Key
#'
#' @description
#' Convenience alias for `set_api_key(key, "openrouter", ...)`.
#'
#' @param key Your OpenRouter API key
#' @param model Optional. Default model (e.g., "anthropic/claude-sonnet-4", "openai/gpt-4o").
#' @param ... Additional options.
#'
#' @examples
#' \dontrun{
#' set_openrouter_key("sk-or-...")
#' set_openrouter_key("sk-or-...", model = "openai/gpt-4o")
#' }
#'
#' @export
set_openrouter_key <- function(key, model = NULL, ...) {
  set_api_key(key, "openrouter", model = model, ...)
}
