#' Center Variables (Mean-Centering)
#'
#' @description
#' Centers numeric variables by subtracting the mean. Supports group-wise
#' centering for multilevel/nested data.
#'
#' @param data A data frame.
#' @param Vars Character vector of numeric variable names to center.
#' @param suffix Character. Suffix for new column names. Default "_c".
#' @param group_by Unquoted variable name for group-wise centering. Optional.
#'
#' @return Data frame with centered variables added.
#'
#' @examples
#' \dontrun{
#' # Simple mean centering
#' df <- center(mtcars, Vars = c("mpg", "hp"))
#'
#' # Group-mean centering
#' df <- center(mtcars, Vars = "mpg", group_by = cyl)
#' }
#'
#' @export
center <- function(data,
                   Vars,
                   suffix = "_c",
                   group_by = NULL) {

  if (!is.data.frame(data)) {
    stop("'data' must be a data frame")
  }

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

  # Capture group_by
  group_var <- NULL
  if (!is.null(substitute(group_by))) {
    group_var <- deparse(substitute(group_by))
    if (group_var == "NULL") group_var <- NULL
  }

  result <- data

  for (var_name in Vars) {
    new_col_name <- paste0(var_name, suffix)
    x <- data[[var_name]]

    if (is.null(group_var)) {
      # Grand-mean centering
      grand_mean <- mean(x, na.rm = TRUE)
      result[[new_col_name]] <- x - grand_mean
      cat(sprintf("Centered '%s' (grand mean = %.3f)\n", var_name, grand_mean))
    } else {
      # Group-mean centering
      if (!group_var %in% names(data)) {
        stop("Group variable not found: ", group_var)
      }

      groups <- unique(data[[group_var]])
      new_vals <- numeric(nrow(data))

      for (g in groups) {
        if (is.na(g)) next
        group_mask <- data[[group_var]] == g & !is.na(data[[group_var]])
        group_mean <- mean(x[group_mask], na.rm = TRUE)
        new_vals[group_mask] <- x[group_mask] - group_mean
      }

      # Handle NA group
      na_mask <- is.na(data[[group_var]])
      if (any(na_mask)) {
        new_vals[na_mask] <- NA
      }

      result[[new_col_name]] <- new_vals
      cat(sprintf("Centered '%s' by group '%s' (%d groups)\n",
                  var_name, group_var, length(unique(na.omit(data[[group_var]])))))
    }
  }

  invisible(result)
}


#' Scale Variables
#'
#' @description
#' Scales numeric variables by dividing by standard deviation or
#' rescaling to a specified range.
#'
#' @param data A data frame.
#' @param Vars Character vector of numeric variable names to scale.
#' @param method Scaling method: "sd" (divide by SD) or "range" (min-max scaling).
#' @param range Numeric vector of length 2 specifying target range for "range" method.
#'   Default c(0, 1). Use c(1, 10) for 1-10 scaling.
#' @param suffix Character. Suffix for new column names. Default "_s".
#'
#' @return Data frame with scaled variables added.
#'
#' @examples
#' \dontrun{
#' # Scale by SD
#' df <- scale_vars(mtcars, Vars = c("mpg", "hp"), method = "sd")
#'
#' # Min-max scaling (0-1)
#' df <- scale_vars(mtcars, Vars = "mpg", method = "range")
#'
#' # Scale to 1-10 range
#' df <- scale_vars(mtcars, Vars = "mpg", method = "range", range = c(1, 10))
#'
#' # Scale to 0-100 range
#' df <- scale_vars(mtcars, Vars = "mpg", method = "range", range = c(0, 100))
#' }
#'
#' @importFrom stats sd
#' @export
scale_vars <- function(data,
                       Vars,
                       method = c("sd", "range"),
                       range = c(0, 1),
                       suffix = "_s") {

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

  # Validate range
  if (method == "range") {
    if (length(range) != 2 || !is.numeric(range)) {
      stop("'range' must be a numeric vector of length 2")
    }
    if (range[1] >= range[2]) {
      stop("range[1] must be less than range[2]")
    }
  }

  result <- data

  for (var_name in Vars) {
    new_col_name <- paste0(var_name, suffix)
    x <- data[[var_name]]

    if (method == "sd") {
      # Scale by standard deviation
      x_sd <- sd(x, na.rm = TRUE)
      if (x_sd == 0) {
        warning(sprintf("Variable '%s' has zero variance, cannot scale by SD", var_name))
        result[[new_col_name]] <- x
      } else {
        result[[new_col_name]] <- x / x_sd
        cat(sprintf("Scaled '%s' by SD (SD = %.3f)\n", var_name, x_sd))
      }

    } else if (method == "range") {
      # Min-max scaling to specified range
      x_min <- min(x, na.rm = TRUE)
      x_max <- max(x, na.rm = TRUE)

      if (x_min == x_max) {
        warning(sprintf("Variable '%s' has no range, setting to midpoint", var_name))
        result[[new_col_name]] <- rep((range[1] + range[2]) / 2, length(x))
      } else {
        # Normalize to 0-1 first
        x_normalized <- (x - x_min) / (x_max - x_min)
        # Then scale to target range
        result[[new_col_name]] <- x_normalized * (range[2] - range[1]) + range[1]
        cat(sprintf("Scaled '%s' to range [%g, %g] (original: [%.2f, %.2f])\n",
                    var_name, range[1], range[2], x_min, x_max))
      }
    }
  }

  invisible(result)
}


#' Standardize Variables (Z-Score)
#'
#' @description
#' Standardizes numeric variables to z-scores (mean = 0, SD = 1).
#' Supports group-wise standardization for multilevel/nested data.
#'
#' @param data A data frame.
#' @param Vars Character vector of numeric variable names to standardize.
#' @param suffix Character. Suffix for new column names. Default "_z".
#' @param group_by Unquoted variable name for group-wise standardization. Optional.
#'
#' @return Data frame with standardized variables added.
#'
#' @examples
#' \dontrun{
#' # Simple standardization
#' df <- standardize(mtcars, Vars = c("mpg", "hp"))
#'
#' # Group-wise standardization
#' df <- standardize(mtcars, Vars = "mpg", group_by = cyl)
#' }
#'
#' @importFrom stats sd
#' @export
standardize <- function(data,
                        Vars,
                        suffix = "_z",
                        group_by = NULL) {

  if (!is.data.frame(data)) {
    stop("'data' must be a data frame")
  }

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

  # Capture group_by
  group_var <- NULL
  if (!is.null(substitute(group_by))) {
    group_var <- deparse(substitute(group_by))
    if (group_var == "NULL") group_var <- NULL
  }

  result <- data

  for (var_name in Vars) {
    new_col_name <- paste0(var_name, suffix)
    x <- data[[var_name]]

    if (is.null(group_var)) {
      # Grand standardization
      grand_mean <- mean(x, na.rm = TRUE)
      grand_sd <- sd(x, na.rm = TRUE)

      if (grand_sd == 0) {
        warning(sprintf("Variable '%s' has zero variance, cannot standardize", var_name))
        result[[new_col_name]] <- rep(0, length(x))
      } else {
        result[[new_col_name]] <- (x - grand_mean) / grand_sd
        cat(sprintf("Standardized '%s' (M = %.3f, SD = %.3f)\n",
                    var_name, grand_mean, grand_sd))
      }
    } else {
      # Group-wise standardization
      if (!group_var %in% names(data)) {
        stop("Group variable not found: ", group_var)
      }

      groups <- unique(data[[group_var]])
      new_vals <- numeric(nrow(data))

      for (g in groups) {
        if (is.na(g)) next
        group_mask <- data[[group_var]] == g & !is.na(data[[group_var]])
        group_vals <- x[group_mask]
        group_mean <- mean(group_vals, na.rm = TRUE)
        group_sd <- sd(group_vals, na.rm = TRUE)

        if (is.na(group_sd) || group_sd == 0) {
          new_vals[group_mask] <- 0
        } else {
          new_vals[group_mask] <- (group_vals - group_mean) / group_sd
        }
      }

      # Handle NA group
      na_mask <- is.na(data[[group_var]])
      if (any(na_mask)) {
        new_vals[na_mask] <- NA
      }

      result[[new_col_name]] <- new_vals
      cat(sprintf("Standardized '%s' by group '%s' (%d groups)\n",
                  var_name, group_var, length(unique(na.omit(data[[group_var]])))))
    }
  }

  invisible(result)
}
