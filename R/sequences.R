# Saqrmisc Package: Sequence Data Functions
#
# Functions for converting and analyzing sequence data.

#' Convert Sequence Data to Various Formats
#'
#' @description
#' Converts long-form or wide-form sequence data into useful formats for analysis,
#' such as frequency tables, one-hot encodings, or edge lists (for network analysis).
#'
#' @param data A data frame containing sequence data.
#' @param seq_cols Character vector of sequence column names. If NULL (default),
#'   auto-detects all columns except the ID column.
#' @param id_col Character. Name of the ID column. If NULL (default), uses the
#'   first column.
#' @param format Character. Output format:
#'   \describe{
#'     \item{`"frequency"`}{Counts of each action per sequence (default)}
#'     \item{`"onehot"`}{Binary (1/0) presence of each action per sequence}
#'     \item{`"edgelist"`}{From -> To transition pairs (for network analysis)}
#'     \item{`"follows"`}{Action -> Previous Action pairs}
#'   }
#' @param na_values Character vector of values to treat as missing/NA.
#'   Default includes: NA, NaN, "", "*", "%", "NA", "-", "void", "missing".
#'   TraMineR typically uses "*" for missing states.
#'
#' @return A data frame structured according to the requested format:
#'   \itemize{
#'     \item `frequency`: Wide format with action counts per ID
#'     \item `onehot`: Wide format with binary indicators per ID
#'     \item `edgelist`: Long format with columns: id, from, to
#'     \item `follows`: Long format with columns: id, act, follows
#'   }
#'
#' @examples
#' \dontrun{
#' # Example sequence data
#' seq_data <- data.frame(
#'   id = 1:3,
#'   t1 = c("A", "B", "A"),
#'   t2 = c("B", "A", "C"),
#'   t3 = c("C", "C", "A"),
#'   t4 = c("A", "*", "B")  # "*" is TraMineR missing
#' )
#'
#' # Frequency table
#' convert_sequences(seq_data, format = "frequency")
#'
#' # One-hot encoding
#' convert_sequences(seq_data, format = "onehot")
#'
#' # Edge list for network analysis
#' convert_sequences(seq_data, format = "edgelist")
#'
#' # What follows what
#' convert_sequences(seq_data, format = "follows")
#' }
#'
#' @importFrom dplyr mutate filter select count distinct group_by lead lag row_number n
#' @importFrom tidyr pivot_longer pivot_wider
#' @importFrom rlang sym
#' @export
convert_sequences <- function(data,
                              seq_cols = NULL,
                              id_col = NULL,
                              format = c("frequency", "onehot", "edgelist", "follows"),
                              na_values = NULL) {


  # Match format argument
  format <- match.arg(format)

  # Validate data

if (!is.data.frame(data)) {
    stop("'data' must be a data frame")
  }

  if (nrow(data) == 0) {
    stop("'data' has no rows")
  }

  # Auto-detect ID column (first column)
  if (is.null(id_col)) {
    id_col <- names(data)[1]
    message("Using '", id_col, "' as ID column")
  }

  if (!id_col %in% names(data)) {
    stop("ID column '", id_col, "' not found in data")
  }

  # Auto-detect sequence columns (all except ID)
  if (is.null(seq_cols)) {
    seq_cols <- setdiff(names(data), id_col)
    if (length(seq_cols) == 0) {
      stop("No sequence columns found (only ID column present)")
    }
    message("Using sequence columns: ", paste(seq_cols, collapse = ", "))
  }

  missing_cols <- setdiff(seq_cols, names(data))
  if (length(missing_cols) > 0) {
    stop("Sequence columns not found: ", paste(missing_cols, collapse = ", "))
  }

  # Default NA values including TraMineR conventions
  if (is.null(na_values)) {
    na_values <- c("*", "%", "-", "", "NA", "na", "N/A", "n/a",
                   "void", "VOID", "missing", "MISSING", ".", "?")
  }

  # Convert to long format and handle missing values
  long <- data %>%
    dplyr::mutate(.seq_id = dplyr::row_number()) %>%
    tidyr::pivot_longer(
      cols = dplyr::all_of(seq_cols),
      names_to = ".time",
      values_to = ".act",
      values_transform = as.character
    ) %>%
    # Filter out missing values
    dplyr::filter(
      !is.na(.data$.act),
      !(.data$.act %in% na_values),
      nchar(trimws(.data$.act)) > 0
    )

  if (nrow(long) == 0) {
    warning("No valid (non-missing) sequence data after filtering")
    return(data.frame())
  }

  # Generate output based on format
  result <- switch(format,
    "frequency" = {
      long %>%
        dplyr::count(.data[[id_col]], .data$.seq_id, .data$.act) %>%
        tidyr::pivot_wider(
          names_from = .data$.act,
          values_from = n,
          values_fill = 0
        ) %>%
        dplyr::select(-".seq_id")
    },

    "onehot" = {
      long %>%
        dplyr::distinct(.data[[id_col]], .data$.seq_id, .data$.act) %>%
        dplyr::mutate(.present = 1L) %>%
        tidyr::pivot_wider(
          names_from = .data$.act,
          values_from = .data$.present,
          values_fill = 0L
        ) %>%
        dplyr::select(-".seq_id")
    },

    "edgelist" = {
      long %>%
        dplyr::group_by(.data$.seq_id) %>%
        dplyr::mutate(to = dplyr::lead(.data$.act)) %>%
        dplyr::filter(!is.na(.data$to)) %>%
        dplyr::ungroup() %>%
        dplyr::select(dplyr::all_of(id_col), from = ".act", to)
    },

    "follows" = {
      long %>%
        dplyr::group_by(.data$.seq_id) %>%
        dplyr::mutate(follows = dplyr::lag(.data$.act)) %>%
        dplyr::filter(!is.na(.data$follows)) %>%
        dplyr::ungroup() %>%
        dplyr::select(dplyr::all_of(id_col), act = ".act", follows)
    }
  )

  return(as.data.frame(result))
}
