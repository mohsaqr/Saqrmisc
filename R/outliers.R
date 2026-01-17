#' Check for Outliers
#'
#' @description
#' Detects univariate and multivariate outliers using various methods including
#' z-score (2SD, 3SD, etc.), IQR, percentile, or Mahalanobis distance.
#'
#' @param data A data frame containing the variables.
#' @param Vars Character vector of numeric variable names to check.
#' @param method Detection method: "zscore", "iqr", "percentile", or "mahalanobis".
#' @param threshold Numeric threshold for outlier detection.
#'   For zscore: number of SDs (default 3). Common values: 2, 2.5, 3, 3.29.
#'   For iqr: IQR multiplier (default 1.5). Common values: 1.5, 3.
#'   For percentile: percentile cutoff (default 0.01 for 1st/99th). Values like 0.05 for 5th/95th.
#'   For mahalanobis: chi-sq p-value threshold (default 0.001).
#' @param flag Logical. Add outlier flag column to returned data? Default TRUE.
#' @param plot Logical. Create outlier visualization? Default TRUE.
#'
#' @return A list containing:
#' \itemize{
#'   \item \code{summary}: gt table with outlier summary
#'   \item \code{data}: Data frame with outlier flags (if flag = TRUE)
#'   \item \code{outlier_indices}: Row indices of outliers
#'   \item \code{plot}: Visualization (if requested)
#' }
#'
#' @examples
#' \dontrun{
#' # Z-score with 3 SD threshold
#' outlier_check(mtcars, Vars = c("mpg", "hp"), method = "zscore", threshold = 3)
#'
#' # Z-score with 2 SD threshold (more conservative)
#' outlier_check(mtcars, Vars = c("mpg", "hp"), method = "zscore", threshold = 2)
#'
#' # IQR method
#' outlier_check(mtcars, Vars = c("mpg", "hp"), method = "iqr", threshold = 1.5)
#'
#' # Percentile method (1st and 99th percentile)
#' outlier_check(mtcars, Vars = c("mpg", "hp"), method = "percentile", threshold = 0.01)
#'
#' # Percentile method (5th and 95th percentile)
#' outlier_check(mtcars, Vars = c("mpg", "hp"), method = "percentile", threshold = 0.05)
#'
#' # Mahalanobis distance (multivariate)
#' outlier_check(mtcars, Vars = c("mpg", "hp", "wt"), method = "mahalanobis")
#' }
#'
#' @importFrom stats mahalanobis cov pchisq qchisq IQR quantile sd na.omit
#' @importFrom gt gt tab_header cols_align tab_style cell_text cells_body
#' @importFrom ggplot2 ggplot aes geom_point geom_hline geom_boxplot theme_minimal labs facet_wrap scale_color_manual
#' @export
outlier_check <- function(data,
                          Vars,
                          method = c("zscore", "iqr", "percentile", "mahalanobis"),
                          threshold = NULL,
                          flag = TRUE,
                          plot = TRUE) {

  if (!is.data.frame(data)) {
    stop("'data' must be a data frame")
  }

  method <- match.arg(method)

  if (missing(Vars) || length(Vars) == 0) {
    stop("'Vars' must be specified")
  }

  # Check variables exist and are numeric
  missing_vars <- setdiff(Vars, names(data))
  if (length(missing_vars) > 0) {
    stop("Variables not found: ", paste(missing_vars, collapse = ", "))
  }

  non_numeric <- Vars[!sapply(data[Vars], is.numeric)]
  if (length(non_numeric) > 0) {
    stop("All variables must be numeric: ", paste(non_numeric, collapse = ", "))
  }

  # Set default thresholds
  if (is.null(threshold)) {
    threshold <- switch(method,
                        "zscore" = 3,           # 3 SD
                        "iqr" = 1.5,            # Standard IQR multiplier
                        "percentile" = 0.01,    # 1st and 99th percentile
                        "mahalanobis" = 0.001   # p < .001
    )
  }

  n_total <- nrow(data)
  data_subset <- data[, Vars, drop = FALSE]

  # Initialize results
  outlier_flags <- rep(FALSE, n_total)
  outlier_details <- list()

  if (method == "zscore") {
    # Z-score method (univariate)
    summary_list <- list()

    for (var_name in Vars) {
      x <- data[[var_name]]
      x_mean <- mean(x, na.rm = TRUE)
      x_sd <- sd(x, na.rm = TRUE)

      if (x_sd == 0) {
        z_scores <- rep(0, length(x))
      } else {
        z_scores <- (x - x_mean) / x_sd
      }

      is_outlier <- abs(z_scores) > threshold & !is.na(z_scores)
      outlier_flags <- outlier_flags | is_outlier

      n_outliers <- sum(is_outlier, na.rm = TRUE)
      outlier_indices <- which(is_outlier)

      # Calculate bounds
      lower_bound <- x_mean - threshold * x_sd
      upper_bound <- x_mean + threshold * x_sd

      summary_list[[var_name]] <- data.frame(
        Variable = var_name,
        Method = paste0("Z-score > ", threshold, " SD"),
        N_Outliers = n_outliers,
        Pct_Outliers = round(n_outliers / sum(!is.na(x)) * 100, 2),
        Lower_Bound = round(lower_bound, 2),
        Upper_Bound = round(upper_bound, 2),
        stringsAsFactors = FALSE
      )

      outlier_details[[var_name]] <- list(
        indices = outlier_indices,
        z_scores = z_scores,
        lower = lower_bound,
        upper = upper_bound
      )
    }

    summary_df <- do.call(rbind, summary_list)

  } else if (method == "iqr") {
    # IQR method (univariate)
    summary_list <- list()

    for (var_name in Vars) {
      x <- data[[var_name]]
      q1 <- quantile(x, 0.25, na.rm = TRUE)
      q3 <- quantile(x, 0.75, na.rm = TRUE)
      iqr_val <- IQR(x, na.rm = TRUE)

      lower_bound <- q1 - threshold * iqr_val
      upper_bound <- q3 + threshold * iqr_val

      is_outlier <- (x < lower_bound | x > upper_bound) & !is.na(x)
      outlier_flags <- outlier_flags | is_outlier

      n_outliers <- sum(is_outlier, na.rm = TRUE)
      outlier_indices <- which(is_outlier)

      summary_list[[var_name]] <- data.frame(
        Variable = var_name,
        Method = paste0("IQR x ", threshold),
        N_Outliers = n_outliers,
        Pct_Outliers = round(n_outliers / sum(!is.na(x)) * 100, 2),
        Lower_Bound = round(lower_bound, 2),
        Upper_Bound = round(upper_bound, 2),
        stringsAsFactors = FALSE
      )

      outlier_details[[var_name]] <- list(
        indices = outlier_indices,
        lower = lower_bound,
        upper = upper_bound
      )
    }

    summary_df <- do.call(rbind, summary_list)

  } else if (method == "percentile") {
    # Percentile method (univariate)
    summary_list <- list()

    # Threshold is the percentile (e.g., 0.01 means 1st and 99th percentile)
    lower_pct <- threshold
    upper_pct <- 1 - threshold

    for (var_name in Vars) {
      x <- data[[var_name]]
      lower_bound <- quantile(x, lower_pct, na.rm = TRUE)
      upper_bound <- quantile(x, upper_pct, na.rm = TRUE)

      is_outlier <- (x < lower_bound | x > upper_bound) & !is.na(x)
      outlier_flags <- outlier_flags | is_outlier

      n_outliers <- sum(is_outlier, na.rm = TRUE)
      outlier_indices <- which(is_outlier)

      # Format percentile labels
      lower_label <- paste0(round(lower_pct * 100, 1), "th")
      upper_label <- paste0(round(upper_pct * 100, 1), "th")

      summary_list[[var_name]] <- data.frame(
        Variable = var_name,
        Method = paste0("Percentile <", lower_label, " or >", upper_label),
        N_Outliers = n_outliers,
        Pct_Outliers = round(n_outliers / sum(!is.na(x)) * 100, 2),
        Lower_Bound = round(lower_bound, 2),
        Upper_Bound = round(upper_bound, 2),
        stringsAsFactors = FALSE
      )

      outlier_details[[var_name]] <- list(
        indices = outlier_indices,
        lower = lower_bound,
        upper = upper_bound
      )
    }

    summary_df <- do.call(rbind, summary_list)

  } else if (method == "mahalanobis") {
    # Mahalanobis distance (multivariate)
    if (length(Vars) < 2) {
      stop("Mahalanobis method requires at least 2 variables")
    }

    # Use complete cases only
    complete_mask <- complete.cases(data_subset)
    data_complete <- data_subset[complete_mask, ]

    if (nrow(data_complete) < ncol(data_complete) + 1) {
      stop("Insufficient complete cases for Mahalanobis distance")
    }

    # Calculate Mahalanobis distance
    center <- colMeans(data_complete)
    cov_matrix <- cov(data_complete)

    mahal_dist <- rep(NA, n_total)
    mahal_dist[complete_mask] <- mahalanobis(data_complete, center, cov_matrix)

    # Chi-square threshold based on p-value
    df <- length(Vars)
    chi_sq_threshold <- qchisq(1 - threshold, df)

    is_outlier <- mahal_dist > chi_sq_threshold & !is.na(mahal_dist)
    outlier_flags <- is_outlier

    n_outliers <- sum(is_outlier, na.rm = TRUE)
    outlier_indices <- which(is_outlier)

    # P-values for outliers
    p_values <- 1 - pchisq(mahal_dist, df)

    summary_df <- data.frame(
      Variables = paste(Vars, collapse = ", "),
      Method = paste0("Mahalanobis (p < ", threshold, ")"),
      N_Outliers = n_outliers,
      Pct_Outliers = round(n_outliers / sum(complete_mask) * 100, 2),
      Chi_Sq_Threshold = round(chi_sq_threshold, 2),
      Max_Distance = round(max(mahal_dist, na.rm = TRUE), 2),
      stringsAsFactors = FALSE
    )

    outlier_details[["mahalanobis"]] <- list(
      indices = outlier_indices,
      distances = mahal_dist,
      p_values = p_values,
      threshold = chi_sq_threshold
    )
  }

  # Total outliers
  total_outliers <- sum(outlier_flags, na.rm = TRUE)

  # Create gt table
  gt_table <- gt::gt(summary_df) |>
    gt::tab_header(
      title = "Outlier Detection Summary",
      subtitle = paste0("Total flagged observations: ", total_outliers, " (",
                        round(total_outliers / n_total * 100, 2), "%)")
    ) |>
    gt::cols_align(align = "left", columns = 1) |>
    gt::cols_align(align = "center", columns = -1)

  print(gt_table)

  # Add flag to data if requested
  result_data <- data
  if (flag) {
    result_data$.outlier <- outlier_flags
    cat(sprintf("\nAdded '.outlier' column to data (%d flagged)\n", total_outliers))
  }

  # Create plot if requested
  plot_obj <- NULL
  if (plot) {
    if (method %in% c("zscore", "iqr", "percentile")) {
      # Boxplot for univariate methods
      plot_data <- do.call(rbind, lapply(Vars, function(var_name) {
        data.frame(
          Variable = var_name,
          Value = data[[var_name]],
          Outlier = if (var_name %in% names(outlier_details)) {
            1:n_total %in% outlier_details[[var_name]]$indices
          } else {
            FALSE
          },
          stringsAsFactors = FALSE
        )
      }))

      plot_obj <- ggplot2::ggplot(plot_data, ggplot2::aes(x = Variable, y = Value)) +
        ggplot2::geom_boxplot(outlier.shape = NA, fill = "#E8E8E8") +
        ggplot2::geom_point(
          data = plot_data[plot_data$Outlier, ],
          ggplot2::aes(color = "Outlier"),
          size = 2, alpha = 0.7
        ) +
        ggplot2::scale_color_manual(values = c("Outlier" = "#E74C3C"), name = "") +
        ggplot2::theme_minimal() +
        ggplot2::labs(
          title = paste0("Outlier Detection (", method, " method)"),
          x = "", y = "Value"
        ) +
        ggplot2::theme(legend.position = "bottom")

    } else if (method == "mahalanobis") {
      # Scatter plot of Mahalanobis distances
      plot_data <- data.frame(
        Index = 1:n_total,
        Distance = outlier_details[["mahalanobis"]]$distances,
        Outlier = outlier_flags
      )
      plot_data <- plot_data[!is.na(plot_data$Distance), ]

      plot_obj <- ggplot2::ggplot(plot_data, ggplot2::aes(x = Index, y = Distance, color = Outlier)) +
        ggplot2::geom_point(alpha = 0.6) +
        ggplot2::geom_hline(
          yintercept = outlier_details[["mahalanobis"]]$threshold,
          linetype = "dashed", color = "#E74C3C", linewidth = 0.8
        ) +
        ggplot2::scale_color_manual(
          values = c("FALSE" = "#3498DB", "TRUE" = "#E74C3C"),
          labels = c("Normal", "Outlier"),
          name = ""
        ) +
        ggplot2::theme_minimal() +
        ggplot2::labs(
          title = "Mahalanobis Distance",
          subtitle = paste0("Threshold (p < ", threshold, "): ", round(outlier_details[["mahalanobis"]]$threshold, 2)),
          x = "Observation", y = "Mahalanobis Distance"
        ) +
        ggplot2::theme(legend.position = "bottom")
    }

    print(plot_obj)
  }

  invisible(list(
    summary = gt_table,
    summary_data = summary_df,
    data = result_data,
    outlier_indices = which(outlier_flags),
    outlier_flags = outlier_flags,
    details = outlier_details,
    plot = plot_obj,
    n_outliers = total_outliers,
    method = method,
    threshold = threshold
  ))
}


#' Replace Outliers
#'
#' @description
#' Replaces detected outliers with specified values such as NA, mean,
#' median, or winsorized values. Supports multiple detection methods.
#'
#' @param data A data frame.
#' @param Vars Character vector of numeric variable names to process.
#' @param detect Detection method: "zscore", "iqr", or "percentile".
#' @param threshold Numeric threshold for detection.
#'   For zscore: number of SDs (default 3). Use 2 for 2SD, 2.5 for 2.5SD, etc.
#'   For iqr: IQR multiplier (default 1.5).
#'   For percentile: percentile cutoff (default 0.01 for 1st/99th).
#' @param replace_with Replacement method: "NA", "mean", "median", "winsorize", or "boundary".
#' @param suffix Character. Suffix for new columns. If NULL (default), replaces in place.
#'
#' @return Data frame with outliers replaced.
#'
#' @details
#' Detection methods:
#' \itemize{
#'   \item \code{"zscore"}: Values beyond threshold SDs from mean. Common thresholds: 2, 2.5, 3, 3.29
#'   \item \code{"iqr"}: Values beyond Q1/Q3 +/- threshold*IQR. Common thresholds: 1.5, 3
#'   \item \code{"percentile"}: Values below or above percentile cutoffs. E.g., 0.01 = 1st/99th, 0.05 = 5th/95th
#' }
#'
#' Replacement methods:
#' \itemize{
#'   \item \code{"NA"}: Set outliers to NA
#'   \item \code{"mean"}: Replace with mean of non-outliers
#'   \item \code{"median"}: Replace with median of non-outliers
#'   \item \code{"winsorize"}: Cap at threshold boundary
#'   \item \code{"boundary"}: Replace with nearest non-outlier value
#' }
#'
#' @examples
#' \dontrun{
#' df <- mtcars
#'
#' # Replace outliers beyond 2 SD with NA
#' df_clean <- replace_outliers(df, Vars = "hp",
#'                              detect = "zscore", threshold = 2,
#'                              replace_with = "NA")
#'
#' # Winsorize at 3 SD
#' df_clean <- replace_outliers(df, Vars = "hp",
#'                              detect = "zscore", threshold = 3,
#'                              replace_with = "winsorize")
#'
#' # Replace outliers at 5th/95th percentile with median
#' df_clean <- replace_outliers(df, Vars = "hp",
#'                              detect = "percentile", threshold = 0.05,
#'                              replace_with = "median")
#'
#' # IQR-based winsorization
#' df_clean <- replace_outliers(df, Vars = c("mpg", "hp"),
#'                              detect = "iqr", replace_with = "winsorize")
#' }
#'
#' @importFrom stats sd IQR quantile median
#' @export
replace_outliers <- function(data,
                             Vars,
                             detect = c("zscore", "iqr", "percentile"),
                             threshold = NULL,
                             replace_with = c("NA", "mean", "median", "winsorize", "boundary"),
                             suffix = NULL) {

  if (!is.data.frame(data)) {
    stop("'data' must be a data frame")
  }

  detect <- match.arg(detect)
  replace_with <- match.arg(replace_with)

  if (missing(Vars) || length(Vars) == 0) {
    stop("'Vars' must be specified")
  }

  # Check variables exist and are numeric
  missing_vars <- setdiff(Vars, names(data))
  if (length(missing_vars) > 0) {
    stop("Variables not found: ", paste(missing_vars, collapse = ", "))
  }

  non_numeric <- Vars[!sapply(data[Vars], is.numeric)]
  if (length(non_numeric) > 0) {
    stop("All variables must be numeric: ", paste(non_numeric, collapse = ", "))
  }

  # Set default thresholds
  if (is.null(threshold)) {
    threshold <- switch(detect,
                        "zscore" = 3,
                        "iqr" = 1.5,
                        "percentile" = 0.01
    )
  }

  result <- data
  total_replaced <- 0

  for (var_name in Vars) {
    x <- data[[var_name]]
    new_col_name <- if (is.null(suffix)) var_name else paste0(var_name, suffix)

    # Detect outliers based on method
    if (detect == "zscore") {
      x_mean <- mean(x, na.rm = TRUE)
      x_sd <- sd(x, na.rm = TRUE)

      if (x_sd == 0) {
        z_scores <- rep(0, length(x))
        lower_bound <- x_mean
        upper_bound <- x_mean
      } else {
        z_scores <- (x - x_mean) / x_sd
        lower_bound <- x_mean - threshold * x_sd
        upper_bound <- x_mean + threshold * x_sd
      }

      is_outlier <- abs(z_scores) > threshold & !is.na(x)
      method_label <- paste0(threshold, " SD")

    } else if (detect == "iqr") {
      q1 <- quantile(x, 0.25, na.rm = TRUE)
      q3 <- quantile(x, 0.75, na.rm = TRUE)
      iqr_val <- IQR(x, na.rm = TRUE)

      lower_bound <- q1 - threshold * iqr_val
      upper_bound <- q3 + threshold * iqr_val

      is_outlier <- (x < lower_bound | x > upper_bound) & !is.na(x)
      method_label <- paste0("IQR x ", threshold)

    } else if (detect == "percentile") {
      lower_pct <- threshold
      upper_pct <- 1 - threshold

      lower_bound <- quantile(x, lower_pct, na.rm = TRUE)
      upper_bound <- quantile(x, upper_pct, na.rm = TRUE)

      is_outlier <- (x < lower_bound | x > upper_bound) & !is.na(x)
      method_label <- paste0(round(lower_pct * 100, 1), "th/", round(upper_pct * 100, 1), "th percentile")
    }

    n_outliers <- sum(is_outlier, na.rm = TRUE)

    if (n_outliers == 0) {
      result[[new_col_name]] <- x
      cat(sprintf("No outliers detected in '%s' (method: %s)\n", var_name, method_label))
      next
    }

    # Replace outliers
    new_vals <- x
    non_outlier_vals <- x[!is_outlier & !is.na(x)]

    if (replace_with == "NA") {
      new_vals[is_outlier] <- NA

    } else if (replace_with == "mean") {
      replacement_val <- mean(non_outlier_vals)
      new_vals[is_outlier] <- replacement_val

    } else if (replace_with == "median") {
      replacement_val <- median(non_outlier_vals)
      new_vals[is_outlier] <- replacement_val

    } else if (replace_with == "winsorize") {
      # Cap at boundaries
      new_vals[x < lower_bound & !is.na(x)] <- lower_bound
      new_vals[x > upper_bound & !is.na(x)] <- upper_bound

    } else if (replace_with == "boundary") {
      # Replace with nearest non-outlier value
      sorted_vals <- sort(non_outlier_vals)
      new_vals[x < lower_bound & !is.na(x)] <- min(sorted_vals)
      new_vals[x > upper_bound & !is.na(x)] <- max(sorted_vals)
    }

    result[[new_col_name]] <- new_vals
    total_replaced <- total_replaced + n_outliers

    cat(sprintf("Replaced %d outliers in '%s' using %s (detect: %s)\n",
                n_outliers, var_name, replace_with, method_label))
  }

  cat(sprintf("\nTotal outliers replaced: %d\n", total_replaced))

  invisible(result)
}


#' Check if Value is Outlier (vectorized for mutate/across)
#'
#' @description
#' Vectorized function to detect outliers for use with dplyr.
#' For group-wise detection, use dplyr::group_by() before mutate().
#'
#' @param x Numeric vector.
#' @param method Detection method: "zscore", "iqr", or "percentile".
#' @param threshold Threshold value.
#' @param na.rm Logical. Remove NA values? Default TRUE.
#'
#' @return Logical vector indicating outliers.
#'
#' @examples
#' \dontrun{
#' library(dplyr)
#'
#' # Flag outliers beyond 2 SD
#' mtcars %>% mutate(hp_outlier = is_outlier(hp, method = "zscore", threshold = 2))
#'
#' # Flag outliers at 5th/95th percentile
#' mtcars %>% mutate(hp_outlier = is_outlier(hp, method = "percentile", threshold = 0.05))
#'
#' # Group-wise outlier detection
#' mtcars %>% group_by(cyl) %>% mutate(hp_outlier = is_outlier(hp))
#' }
#'
#' @export
is_outlier <- function(x, method = c("zscore", "iqr", "percentile"), threshold = NULL, na.rm = TRUE) {
  method <- match.arg(method)

  if (is.null(threshold)) {
    threshold <- switch(method,
                        "zscore" = 3,
                        "iqr" = 1.5,
                        "percentile" = 0.01)
  }

  if (method == "zscore") {
    x_mean <- mean(x, na.rm = na.rm)
    x_sd <- sd(x, na.rm = na.rm)
    if (is.na(x_sd) || x_sd == 0) return(rep(FALSE, length(x)))
    z_scores <- abs((x - x_mean) / x_sd)
    return(z_scores > threshold & !is.na(x))

  } else if (method == "iqr") {
    q1 <- quantile(x, 0.25, na.rm = na.rm)
    q3 <- quantile(x, 0.75, na.rm = na.rm)
    iqr_val <- IQR(x, na.rm = na.rm)
    lower <- q1 - threshold * iqr_val
    upper <- q3 + threshold * iqr_val
    return((x < lower | x > upper) & !is.na(x))

  } else if (method == "percentile") {
    lower <- quantile(x, threshold, na.rm = na.rm)
    upper <- quantile(x, 1 - threshold, na.rm = na.rm)
    return((x < lower | x > upper) & !is.na(x))
  }
}


#' Winsorize a Vector (vectorized for mutate/across)
#'
#' @description
#' Vectorized winsorization for use with dplyr.
#' For group-wise winsorization, use dplyr::group_by() before mutate().
#'
#' @param x Numeric vector.
#' @param method Detection method: "zscore", "iqr", or "percentile".
#' @param threshold Threshold value.
#' @param na.rm Logical. Remove NA values? Default TRUE.
#'
#' @return Winsorized numeric vector.
#'
#' @examples
#' \dontrun{
#' library(dplyr)
#'
#' # Winsorize at 3 SD
#' mtcars %>% mutate(hp_w = winsorize_vec(hp, method = "zscore", threshold = 3))
#'
#' # Winsorize at 5th/95th percentile
#' mtcars %>% mutate(hp_w = winsorize_vec(hp, method = "percentile", threshold = 0.05))
#'
#' # Group-wise winsorization
#' mtcars %>% group_by(cyl) %>% mutate(hp_w = winsorize_vec(hp))
#' }
#'
#' @export
winsorize_vec <- function(x, method = c("zscore", "iqr", "percentile"), threshold = NULL, na.rm = TRUE) {
  method <- match.arg(method)

  if (is.null(threshold)) {
    threshold <- switch(method,
                        "zscore" = 3,
                        "iqr" = 1.5,
                        "percentile" = 0.05)
  }

  if (method == "zscore") {
    x_mean <- mean(x, na.rm = na.rm)
    x_sd <- sd(x, na.rm = na.rm)
    if (is.na(x_sd) || x_sd == 0) return(x)
    lower <- x_mean - threshold * x_sd
    upper <- x_mean + threshold * x_sd

  } else if (method == "iqr") {
    q1 <- quantile(x, 0.25, na.rm = na.rm)
    q3 <- quantile(x, 0.75, na.rm = na.rm)
    iqr_val <- IQR(x, na.rm = na.rm)
    lower <- q1 - threshold * iqr_val
    upper <- q3 + threshold * iqr_val

  } else if (method == "percentile") {
    lower <- quantile(x, threshold, na.rm = na.rm)
    upper <- quantile(x, 1 - threshold, na.rm = na.rm)
  }

  result <- x
  result[x < lower & !is.na(x)] <- lower
  result[x > upper & !is.na(x)] <- upper
  return(result)
}
