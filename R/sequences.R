# Saqrmisc Package: Sequence Data Functions
#
# Functions for converting and analyzing sequence data.

#' Count Events per ID
#'
#' @description
#' Converts long-format event data to a frequency table with counts per ID.
#' Useful for transforming event logs, clickstream data, or any repeated
#' measures into a wide format suitable for analysis.
#'
#' @param data A data frame containing event data in long format.
#' @param id Character vector. Name(s) of ID column(s). If multiple IDs are
#'   provided, they are combined. If NULL (default), uses the first column.
#' @param event Character. Name of the event/action column to count.
#'   If NULL (default), uses the second column.
#' @param na_values Character vector of values to treat as missing.
#'   Default includes common missing indicators.
#'
#' @return A data frame with one row per unique ID combination and columns
#'   for each unique event value containing counts.
#'
#' @examples
#' \dontrun{
#' # Event log data
#' events <- data.frame(
#'   student = c(1, 1, 1, 2, 2, 2, 3, 3),
#'   action = c("login", "view", "submit", "login", "view", "view", "login", "submit")
#' )
#'
#' # Count events per student
#' count_events(events, id = "student", event = "action")
#'
#' # Multiple ID columns
#' events2 <- data.frame(
#'   student = c(1, 1, 1, 1, 2, 2),
#'   course = c("A", "A", "B", "B", "A", "A"),
#'   action = c("view", "submit", "view", "view", "view", "submit")
#' )
#'
#' count_events(events2, id = c("student", "course"), event = "action")
#' }
#'
#' @importFrom dplyr count all_of
#' @importFrom tidyr pivot_wider
#' @export
count_events <- function(data,
                         id = NULL,
                         event = NULL,
                         na_values = NULL) {

  if (!is.data.frame(data)) {
    stop("'data' must be a data frame")
  }

  if (nrow(data) == 0) {
    stop("'data' has no rows")
  }

  # Auto-detect ID column (first column)
  if (is.null(id)) {
    id <- names(data)[1]
    message("Using '", id, "' as ID column")
  }

  # Validate ID columns exist
  missing_id <- setdiff(id, names(data))
  if (length(missing_id) > 0) {
    stop("ID column(s) not found: ", paste(missing_id, collapse = ", "))
  }

  # Auto-detect event column (second column, or first non-ID)
 if (is.null(event)) {
    remaining <- setdiff(names(data), id)
    if (length(remaining) == 0) {
      stop("No event column found (only ID columns present)")
    }
    event <- remaining[1]
    message("Using '", event, "' as event column")
  }

  if (!event %in% names(data)) {
    stop("Event column '", event, "' not found in data")
  }

  # Default NA values
  if (is.null(na_values)) {
    na_values <- c("*", "%", "-", "", "NA", "na", "N/A", "n/a",
                   "void", "VOID", "missing", "MISSING", ".", "?")
  }

  # Filter out missing values from event column
  data_clean <- data
  data_clean[[event]] <- as.character(data_clean[[event]])
  data_clean <- data_clean[
    !is.na(data_clean[[event]]) &
    !(data_clean[[event]] %in% na_values) &
    nchar(trimws(data_clean[[event]])) > 0,
  ]

  if (nrow(data_clean) == 0) {
    warning("No valid events after filtering missing values")
    return(data.frame())
  }

  # Count events per ID
  result <- data_clean %>%
    dplyr::count(dplyr::across(dplyr::all_of(id)), .data[[event]]) %>%
    tidyr::pivot_wider(
      names_from = dplyr::all_of(event),
      values_from = n,
      values_fill = 0
    )

  return(as.data.frame(result))
}

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
