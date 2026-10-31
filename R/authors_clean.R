# Author disambiguation for bibliometric data by Scopus author ID, then ORCID.
# A Scopus author ID is one person; Scopus IDs whose papers carry one ORCID are
# one person. Names never merge anyone: they only flag a merge for a check.

#' Clean author names with Scopus author IDs and ORCID
#'
#' @description
#' Rewrites the author field of a bibliometric data frame so that each person
#' has one spelling and different people never share one. Identity comes from
#' IDs only, never from names:
#'
#' * all spellings of one Scopus ID become one spelling (the most frequent,
#'   then the most recent, then the longest), with the accents kept;
#' * with `openalex = TRUE`, OpenAlex is asked by DOI for each author's ORCID,
#'   and Scopus IDs whose papers carry the same ORCID are joined into one
#'   person. An author without a usable Scopus ID takes its OpenAlex author ID;
#' * different people who share a spelling stay apart, labelled `"KIM J"`,
#'   `"KIM J (2)"`, ... by number of papers;
#' * an author identified by neither source keeps its spelling, and a paper
#'   with no identified author is left untouched.
#'
#' A merge whose spellings do not look like one name ("ENDRASS B" and
#' "LUGRIN B" under one Scopus ID) is kept but flagged `check = TRUE`. Lost
#' accents ("KRMER N"), initials against given names ("MOORE RL", "MOORE
#' ROBERT L") and a surname part dropped ("LOPEZ SS", "SAFFON LOPEZ S") count
#' as one name. Write the merges to a file with [review_sheet()], mark the
#' ones that are wrong `split`, and pass the file back as `review`.
#'
#' Scopus writes a generational suffix as its own name ("Huff E.W., Jr."), and
#' names may hold HTML entities (`L&#x00F3;pez`); both are repaired.
#'
#' @param data A data frame with one row per paper, such as a bibliometrix
#'   data frame.
#'   The result of another `clean_*()` function works too, so the cleaners
#'   chain: `M |> authors_clean(scopus = "Dlds") |> keywords_clean()`.
#' @param scopus Paths to the raw Scopus CSV exports, or a folder holding them.
#'   They need the columns `Authors`, `Author(s) ID`, `DOI` and `EID`. Leave
#'   it out when `data` itself holds the Scopus author IDs (column
#'   `author_ids`): bibliometrix drops them, so a bibliometrix data frame
#'   needs `scopus`. Given both, `scopus` is used, since its names keep their
#'   accents.
#' @param author_ids Name of the column in `data` holding the Scopus author
#'   IDs (`;`-separated, in the order of the authors), read when `scopus` is
#'   left out. Default `"Author(s) ID"`, Scopus's own name.
#' @param eid Name of the column in `data` holding the Scopus EID
#'   (`"2-s2.0-..."`), which links each paper to its Scopus row.
#' @param authors,doi,year Names of the author (`;`-separated), DOI and
#'   publication-year columns in `data`. The defaults are bibliometrix's.
#' @param openalex Logical. Consult OpenAlex for the gaps described above?
#'   Needs the httr2 package and an internet connection. Default `TRUE`.
#' @param mailto Optional e-mail address sent to OpenAlex to use its polite
#'   pool. Default `NULL` sends none.
#' @param review Optional reviewed merges: the `.csv` or `.xlsx` file from
#'   [review_sheet()], or a data frame with its columns `person_key` and
#'   `decision`. A person marked `split` is taken apart again: into its Scopus
#'   IDs when ORCID joined several, otherwise into its Scopus spellings. An
#'   empty `decision` or `keep` keeps the merge. Default `NULL`.
#'
#' @return A `saqr_authors` object:
#' \itemize{
#'   \item `as.data.frame(x)`: `data` with the author column rewritten and a
#'     new `AU_ID` column holding the matching person keys in the same order
#'     (`-` for an unidentified author; `NA` for an untouched paper).
#'   \item [authors_changes()]: one row per person whose
#'     spelling changed, who was joined across Scopus IDs, or who shares a
#'     spelling, most papers first: `person_key`, `name`, `spellings`,
#'     `papers`, `scopus_ids`, `merged` (one Scopus ID, several spellings),
#'     `check` (a merged or joined person whose spellings do not look like
#'     one name), `homonym`, `orcid_conflict` (the person's papers show more
#'     than one ORCID; flagged, not split) and `split` (a part of a person
#'     `review` took apart; a part split by spelling has the key
#'     `<key>|<spelling>`). [authors_check()] gives its rows with
#'     `check = TRUE`.
#'   \item `as.data.frame(x, what = "authors")`: one row per author per paper
#'     (for tracing a single decision):
#'     `eid`, `doi`, `year`, `position`, `name_before`, `name_scopus`,
#'     `scopus_id`, `openalex_id`, `orcid`, `person_key` (`scopus:<id>`,
#'     `orcid:<id>`, `openalex:<id>`, or `NA`), `split_from` (the key `review`
#'     split, else `NA`), `source` and `name`.
#' }
#' `summary()` returns the counts as a data frame.
#'
#' @section Errors:
#' Raises `saqrmisc_bad_input` when `data` lacks a named column, has repeated
#' EIDs, `scopus` names no readable Scopus export, or `review` is not a review
#' sheet or holds a decision other than `keep` or `split`;
#' `saqrmisc_missing_package` when `openalex = TRUE` without httr2, or for an
#' `.xlsx` `review` without readxl. A `saqrmisc_review_unmatched` warning
#' names reviewed people this run does not have. A
#' `saqrmisc_openalex_failed` warning reports OpenAlex requests that failed;
#' the authors involved keep their Scopus identity.
#'
#' @references
#' Priem, J., Piwowar, H., & Orr, R. (2022). OpenAlex: A fully-open index of
#' scholarly works, authors, venues, institutions, and concepts.
#' arXiv:2205.01833.
#'
#' Baas, J., Schotten, M., Plume, A., Côté, G., & Karimi, R. (2020). Scopus as
#' a curated, high-quality bibliometric data source for academic research in
#' quantitative science studies. *Quantitative Science Studies*, 1(1), 377-386.
#' \doi{10.1162/qss_a_00019}
#'
#' @examples
#' scopus_file <- tempfile(fileext = ".csv")
#' utils::write.csv(data.frame(
#'   Authors = c("Pérez-Marín D., Kim J.", "Perez-Marin D., Kim J."),
#'   `Author(s) ID` = c("111;201;", "111;202;"),
#'   DOI = c("10.1/a", "10.1/b"),
#'   EID = c("2-s2.0-1", "2-s2.0-2"),
#'   check.names = FALSE
#' ), scopus_file, row.names = FALSE, fileEncoding = "UTF-8")
#' papers <- data.frame(
#'   eid = c("2-s2.0-1", "2-s2.0-2"),
#'   AU = c("PREZ-MARN D;KIM J", "PEREZ-MARIN D;KIM J"),
#'   DI = c("10.1/a", "10.1/b"),
#'   PY = c(2019, 2020)
#' )
#' cleaned <- authors_clean(papers, scopus = scopus_file, openalex = FALSE)
#' cleaned
#' as.data.frame(cleaned)
#' authors_changes(cleaned)
#'
#' # Merges to check go to a file; mark a wrong one `split` and pass it back.
#' sheet <- tempfile(fileext = ".csv")
#' review_sheet(cleaned, sheet)
#' undone <- authors_clean(papers, scopus = scopus_file, openalex = FALSE,
#'                         review = data.frame(person_key = "scopus:111", decision = "split"))
#' as.data.frame(undone)
#'
#' @export
authors_clean <- function(data,
                          scopus = NULL,
                          author_ids = "Author(s) ID",
                          eid = "eid",
                          authors = "AU",
                          doi = "DI",
                          year = "PY",
                          openalex = TRUE,
                          mailto = NULL,
                          review = NULL) {
  data <- .papers_of(data)
  if (!is.data.frame(data)) .bad_input("`data` must be a data frame")
  columns <- c(eid = eid, authors = authors, doi = doi, year = year)
  if (!all(vapply(columns, \(x) is.character(x) && length(x) == 1L, logical(1)))) {
    .bad_input("`eid`, `authors`, `doi` and `year` must each be one column name")
  }
  absent <- setdiff(columns, names(data))
  if (length(absent) > 0L) {
    .bad_input(sprintf("`data` has no column(s) %s", paste(absent, collapse = ", ")))
  }
  if (nrow(data) == 0L) .bad_input("`data` has no papers")
  if (anyNA(data[[eid]]) || !all(nzchar(trimws(data[[eid]])))) {
    .bad_input(sprintf("`data$%s` has a missing EID; every paper needs its Scopus EID", eid))
  }
  if (anyDuplicated(data[[eid]]) > 0L) {
    .bad_input(sprintf("`data$%s` repeats an EID; each paper must appear once", eid))
  }
  if (!all(is.na(data[[year]]) | grepl("^\\s*[0-9]{1,4}\\s*$", as.character(data[[year]])))) {
    .bad_input(sprintf("`data$%s` must hold years", year))
  }
  if (!(isTRUE(openalex) || isFALSE(openalex))) .bad_input("`openalex` must be TRUE or FALSE")
  if (!is.null(mailto) && !(is.character(mailto) && length(mailto) == 1L)) {
    .bad_input("`mailto` must be NULL or one e-mail address")
  }
  if (openalex && !requireNamespace("httr2", quietly = TRUE)) {
    stop(errorCondition(
      "`openalex = TRUE` needs the httr2 package: install.packages(\"httr2\"), or pass `openalex = FALSE`",
      class = "saqrmisc_missing_package", call = NULL))
  }

  if (is.null(scopus)) {
    if (!(is.character(author_ids) && length(author_ids) == 1L)) {
      .bad_input("`author_ids` must be one column name")
    }
    if (!author_ids %in% names(data)) {
      .bad_input(sprintf(paste0("`data` has no column `%s` with the Scopus author IDs ",
                                "(bibliometrix drops it); pass the Scopus CSV exports as `scopus`"), author_ids))
    }
  }
  split_keys <- .au_review_splits(review)

  papers <- data.frame(eid = as.character(data[[eid]]), AU = as.character(data[[authors]]),
                       DI = as.character(data[[doi]]), PY = data[[year]])
  raw <- if (is.null(scopus)) {
    .au_scopus_from_data(papers, data[[author_ids]])
  } else {
    .au_read_scopus(.au_scopus_files(scopus))
  }
  rows <- .au_identify(papers, raw, openalex, mailto, split_keys)

  cleaned <- data
  rewritten <- .au_rewrite(papers, rows)
  cleaned[[authors]] <- rewritten$AU
  cleaned$AU_ID <- rewritten$AU_ID

  structure(
    list(data = cleaned, authors = rows, changes = .au_changes(rows),
         openalex = openalex, author_column = authors),
    class = "saqr_authors"
  )
}

#' Tables from an authors_clean() result
#'
#' @param x An [authors_clean()] result.
#' @param row.names,optional Ignored; present for the generic.
#' @param what Which table: `"data"` (the cleaned papers, default),
#'   `"changes"` (as [authors_changes()]) or `"authors"`
#'   (one row per author per paper). See [authors_clean()] for their columns.
#' @param ... Ignored.
#' @return A data frame.
#' @export
as.data.frame.saqr_authors <- function(x, row.names = NULL, optional = FALSE,
                                       what = c("data", "changes", "authors"), ...) {
  what <- match.arg(what)
  switch(what, data = x$data, changes = x$changes, authors = x$authors)
}

#' The changes and the merges to check
#'
#' Two verbs on an [authors_clean()] result. `authors_changes()` gives one
#' row per person whose spelling changed, who was joined across Scopus IDs,
#' or who shares a spelling. `authors_check()` gives the rows of those whose
#' spellings do not look like one name, to be checked by hand. The cleaned
#' papers are `as.data.frame()` of the result.
#'
#' @param x An [authors_clean()] result.
#' @return A data frame with one row per person, with the columns listed
#'   under Value in [authors_clean()].
#' @section Errors:
#' Raises `saqrmisc_bad_input` when `x` is not an [authors_clean()] result.
#' @examples
#' scopus_file <- tempfile(fileext = ".csv")
#' utils::write.csv(data.frame(
#'   Authors = c("Lugrin B., Kim J.", "Leclere B., Kim J."),
#'   `Author(s) ID` = c("111;201;", "111;202;"),
#'   DOI = c("10.1/a", "10.1/b"),
#'   EID = c("2-s2.0-1", "2-s2.0-2"),
#'   check.names = FALSE
#' ), scopus_file, row.names = FALSE, fileEncoding = "UTF-8")
#' papers <- data.frame(
#'   eid = c("2-s2.0-1", "2-s2.0-2"),
#'   AU = c("LUGRIN B;KIM J", "LECLERE B;KIM J"),
#'   DI = c("10.1/a", "10.1/b"),
#'   PY = c(2019, 2020)
#' )
#' authors <- authors_clean(papers, scopus = scopus_file, openalex = FALSE)
#' authors_changes(authors)
#' authors_check(authors)
#' @name authors_changes
NULL

#' @rdname authors_changes
#' @export
authors_changes <- function(x) {
  .au_need_result(x)
  x$changes
}

#' @rdname authors_changes
#' @export
authors_check <- function(x) {
  .au_need_result(x)
  flagged <- x$changes[x$changes$check, ]
  rownames(flagged) <- NULL
  flagged
}

.au_need_result <- function(x) {
  if (!inherits(x, "saqr_authors")) .bad_input("`x` must be an authors_clean() result")
}

#' Summarise an authors_clean() result
#'
#' @param object An [authors_clean()] result.
#' @param ... Ignored.
#' @return A data frame with one row per count: `measure` and `n`.
#' @export
summary.saqr_authors <- function(object, ...) {
  rows <- object$authors
  changes <- object$changes
  rewritten <- !is.na(object$data$AU_ID)
  data.frame(
    measure = c("papers", "papers rewritten", "author rows", "rows identified by Scopus",
                "rows identified by OpenAlex only", "rows unidentified", "people",
                "Scopus IDs with several spellings", "people joined across Scopus IDs by ORCID",
                "merges to check (names unlike)", "people split by review",
                "people sharing a spelling (numbered)", "people with an ORCID conflict (flagged)"),
    n = c(nrow(object$data), sum(rewritten), nrow(rows), sum(rows$source == "scopus"),
          sum(rows$source == "openalex"), sum(rows$source == "none"),
          length(unique(stats::na.omit(rows$person_key))), sum(changes$merged & changes$scopus_ids <= 1L),
          sum(changes$scopus_ids > 1L), sum(changes$check),
          length(unique(stats::na.omit(rows$split_from))),
          sum(changes$homonym), sum(changes$orcid_conflict))
  )
}

#' Print an authors_clean() result
#'
#' @param x An [authors_clean()] result.
#' @param n How many merges to check to list. Default 10.
#' @param ... Ignored.
#' @return `x`, invisibly.
#' @export
print.saqr_authors <- function(x, n = 10, ...) {
  shown <- utils::head(x$changes[x$changes$check, c("name", "spellings", "papers")], n)
  counts <- summary(x)
  n_check <- sum(x$changes$check)
  n <- stats::setNames(counts$n, counts$measure)
  cat(sprintf("Author cleaning by Scopus ID%s\n", if (x$openalex) ", then ORCID" else ""))
  cat(sprintf("  %d of %d papers rewritten; %d people\n",
              n[["papers rewritten"]], n[["papers"]], n[["people"]]))
  cat(sprintf("  %d Scopus IDs had several spellings, now one; %d people joined across Scopus IDs by ORCID\n",
              n[["Scopus IDs with several spellings"]],
              n[["people joined across Scopus IDs by ORCID"]]))
  cat(sprintf("  %d of these merges to check: their spellings do not look like one name\n",
              n[["merges to check (names unlike)"]]))
  if (n[["people split by review"]] > 0L) {
    cat(sprintf("  %d merges split by your review\n", n[["people split by review"]]))
  }
  cat(sprintf("  %d share a spelling and are numbered; %d ORCID conflicts flagged\n",
              n[["people sharing a spelling (numbered)"]],
              n[["people with an ORCID conflict (flagged)"]]))
  cat(sprintf("  %d author rows unidentified, kept as they were\n", n[["rows unidentified"]]))
  if (nrow(shown) > 0L) {
    cat(sprintf("\nMerges to check (%d of %d):\n", nrow(shown), n_check))
    width <- max(20L, getOption("width") - max(nchar(shown$name)) - 12L)
    shown$spellings <- ifelse(nchar(shown$spellings) > width,
                              paste0(substr(shown$spellings, 1L, width - 3L), "..."), shown$spellings)
    print(shown, row.names = FALSE, right = FALSE)
  }
  cat("\nas.data.frame(x) gives the cleaned papers; review_sheet(x, file) writes every merge for you to check.\n")
  invisible(x)
}

#' Write the author merges to a review sheet
#'
#' @description
#' Writes every merge [authors_clean()] made (one Scopus ID with several
#' spellings, Scopus IDs joined by ORCID, people with two ORCIDs) to a file,
#' the ones whose names do not look alike (`check = TRUE`) first. Write
#' `split` in `decision` for a merge that is wrong; leave it empty (or write
#' `keep`) for one that is right. Then pass the file back:
#' `authors_clean(..., review = file)`.
#'
#' @param x An [authors_clean()] result.
#' @param file The file to write, `.csv` (UTF-8) or `.xlsx`. An existing
#'   file is never replaced, since it may hold your decisions.
#'
#' @return The file path, invisibly. The sheet has one row per merged person:
#'   `decision` (empty), `check`, `name`, `spellings`, `papers`,
#'   `scopus_ids`, `orcid_conflict`, `merged` and `person_key`.
#'
#' @section Errors:
#' Raises `saqrmisc_bad_input` when `file` is not `.csv` or `.xlsx` or
#' already exists; `saqrmisc_missing_package` for `.xlsx` without writexl.
#'
#' @examples
#' scopus_file <- tempfile(fileext = ".csv")
#' utils::write.csv(data.frame(
#'   Authors = c("Endrass B.", "Lugrin B."), `Author(s) ID` = c("7;", "7;"),
#'   DOI = c("10.1/a", "10.1/b"), EID = c("2-s2.0-1", "2-s2.0-2"),
#'   check.names = FALSE
#' ), scopus_file, row.names = FALSE, fileEncoding = "UTF-8")
#' papers <- data.frame(eid = c("2-s2.0-1", "2-s2.0-2"), AU = c("ENDRASS B", "LUGRIN B"),
#'                      DI = c("10.1/a", "10.1/b"), PY = c(2012, 2016))
#' sheet <- tempfile(fileext = ".csv")
#' review_sheet(authors_clean(papers, scopus = scopus_file, openalex = FALSE), sheet)
#' utils::read.csv(sheet)
#' @seealso [authors_clean()]
#' @export
review_sheet.saqr_authors <- function(x, file) {
  csv <- .kr_sheet_file(file)
  changes <- x$changes
  merges <- changes[changes$merged | changes$scopus_ids > 1L | changes$orcid_conflict, ]
  sheet <- data.frame(
    decision = rep(NA_character_, nrow(merges)), check = merges$check, name = merges$name,
    spellings = merges$spellings, papers = merges$papers, scopus_ids = merges$scopus_ids,
    orcid_conflict = merges$orcid_conflict, merged = merges$merged, person_key = merges$person_key,
    stringsAsFactors = FALSE
  )
  if (csv) .kr_write_csv(sheet, file) else writexl::write_xlsx(list(review = sheet), file)
  message(sprintf("%s: %d merge(s), %d to check. Write split in decision for a wrong one, then authors_clean(review = \"%s\").",
                  basename(file), nrow(sheet), sum(sheet$check), basename(file)))
  invisible(file)
}

# ---- helpers ----------------------------------------------------------------

.au_suffixes <- c("JR", "SR", "II", "III", "IV")
.au_batch_size <- 50L
.au_timeout <- 60

.au_or_na <- function(x, y) ifelse(is.na(x), y, x)

.au_scopus_files <- function(scopus) {
  if (!is.character(scopus) || length(scopus) == 0L) {
    .bad_input("`scopus` must be paths to Scopus CSV exports or a folder holding them")
  }
  files <- if (length(scopus) == 1L && dir.exists(scopus)) {
    list.files(scopus, pattern = "\\.csv$", full.names = TRUE, ignore.case = TRUE)
  } else {
    scopus
  }
  if (length(files) == 0L || !all(file.exists(files))) {
    .bad_input("`scopus` names no existing Scopus CSV export")
  }
  files
}

.au_read_scopus <- function(files) {
  needed <- c("Authors", "Author(s) ID", "DOI", "EID")
  tables <- lapply(files, \(f) {
    x <- utils::read.csv(f, check.names = FALSE, fileEncoding = "UTF-8-BOM",
                         stringsAsFactors = FALSE)
    absent <- setdiff(needed, names(x))
    if (length(absent) > 0L) {
      .bad_input(sprintf("`%s` is not a Scopus export: no column(s) %s",
                         basename(f), paste(absent, collapse = ", ")))
    }
    x[needed]
  })
  raw <- do.call(rbind, tables)
  raw[!duplicated(raw$EID), ]
}

# The Scopus table read from `data` itself: its own names, in Scopus's
# comma-separated form, beside its own author IDs. Entities go first, since
# the `;` of L&#X00F3;PEZ is not an author separator.
.au_scopus_from_data <- function(papers, ids) {
  names_list <- strsplit(.au_decode_entities(as.character(papers$AU)), ";", fixed = TRUE)
  names_list[is.na(papers$AU)] <- list(character())
  data.frame(
    Authors = vapply(names_list, \(n) paste(trimws(n), collapse = ", "), character(1)),
    `Author(s) ID` = as.character(ids),
    DOI = papers$DI,
    EID = papers$eid,
    check.names = FALSE, stringsAsFactors = FALSE
  )
}

# Scopus and bibliometrix both leave HTML entities (L&#x00F3;pez) in names.
.au_decode_entities <- function(x) {
  # regmatches<- writes "NA" into a missing element, so those are left out.
  present <- !is.na(x)
  text <- x[present]
  hits <- gregexpr("&#[xX]?[0-9A-Fa-f]+;", text)
  regmatches(text, hits) <- lapply(regmatches(text, hits), \(entity) {
    hex <- grepl("^&#[xX]", entity)
    digits <- gsub("^&#[xX]?|;$", "", entity)
    code <- ifelse(hex, strtoi(digits, 16L), strtoi(digits, 10L))
    vapply(code, intToUtf8, character(1))
  })
  x[present] <- text
  x
}

# The bibliometrix style ("PÉREZ-MARÍN D", "CHO SB"), with the accents kept.
.au_normalise <- function(x) {
  x |>
    .au_decode_entities() |>
    gsub(pattern = ".-", replacement = ".", fixed = TRUE) |>
    gsub(pattern = ".", replacement = "", fixed = TRUE) |>
    gsub(pattern = "\\s+", replacement = " ") |>
    trimws() |>
    toupper()
}

# Accent-folded words: "PÉREZ-MARÍN" gives PEREZ, MARIN. macOS transliterates
# é to 'e, so the marks are dropped before splitting.
.au_words <- function(x) {
  ascii <- iconv(x, "UTF-8", "ASCII//TRANSLIT")
  ascii[is.na(ascii)] <- ""
  lapply(strsplit(toupper(gsub("['`^~\"]", "", ascii)), "[^A-Z]+"), \(w) w[nzchar(w)])
}

# A generational suffix is its own comma field in Scopus; glue it back on.
.au_split_names <- function(authors) {
  lapply(strsplit(authors, ", ", fixed = TRUE), \(parts) {
    is_suffix <- .au_normalise(parts) %in% .au_suffixes
    unname(vapply(split(parts, cumsum(!is_suffix)), paste, character(1), collapse = " "))
  })
}

.au_split_ids <- function(ids) {
  lapply(strsplit(sub(";\\s*$", "", ids), ";", fixed = TRUE), trimws)
}

# One row per author per paper, with the Scopus ID where names and IDs line up.
.au_scopus_rows <- function(papers, raw) {
  hit <- raw[match(papers$eid, raw$EID), ]
  in_scopus <- !is.na(hit$EID)
  # read.csv reads a column of empty fields as logical NA; keep the text typed.
  as_text <- \(v) { v <- as.character(v); v[is.na(v) | !in_scopus] <- ""; v }
  names_list <- .au_split_names(as_text(hit$Authors))
  names_list[!in_scopus] <- strsplit(.au_decode_entities(as.character(papers$AU[!in_scopus])), ";", fixed = TRUE)
  ids_list <- .au_split_ids(as_text(hit[["Author(s) ID"]]))
  aligned <- in_scopus & lengths(names_list) == lengths(ids_list)
  ids_list[!aligned] <- lapply(names_list[!aligned], \(n) rep(NA_character_, length(n)))

  before <- strsplit(papers$AU, ";", fixed = TRUE)
  same_count <- lengths(before) == lengths(names_list)
  before[!same_count] <- lapply(names_list[!same_count], \(n) rep(NA_character_, length(n)))

  n <- lengths(names_list)
  scopus_id <- unlist(ids_list)
  data.frame(
    eid = rep(papers$eid, n),
    doi = tolower(trimws(rep(papers$DI, n))),
    year = rep(as.integer(trimws(papers$PY)), n),
    position = unlist(lapply(n, seq_len)),
    name_before = trimws(unlist(before)),
    name_scopus = .au_normalise(unlist(names_list)),
    scopus_id = ifelse(grepl("^[0-9]+$", scopus_id), scopus_id, NA_character_),
    stringsAsFactors = FALSE
  )
}

.au_authorship_table <- function(authorships) {
  people <- Filter(\(a) !is.null(a$author$id), authorships)
  data.frame(
    raw_name = vapply(people, \(a) a$raw_author_name %||% a$author$display_name, character(1)),
    openalex_id = vapply(people, \(a) sub("^https://openalex.org/", "", a$author$id), character(1)),
    orcid = vapply(people, \(a) sub("^https://orcid.org/", "", a$author$orcid %||% NA_character_),
                   character(1)),
    stringsAsFactors = FALSE
  )
}

# Seconds OpenAlex asks to wait before asking again; 0 when it does not say
# (or gives a date, which it does not do).
.au_wait <- function(resp) {
  wait <- trimws(httr2::resp_header(resp, "retry-after", default = ""))
  if (grepl("^[0-9]+$", wait)) as.numeric(wait) else 0
}

# One OpenAlex request: the parsed reply, NULL for 404 (no such work), or an
# error condition for anything else.
.au_openalex_get <- function(url, query) {
  req <- do.call(httr2::req_url_query, c(list(httr2::request(url)), query)) |>
    httr2::req_user_agent("Saqrmisc (https://github.com/mohsaqr/Saqrmisc)") |>
    # A dropped connection otherwise waits forever: give up on a request after
    # a minute, and retry it like a busy OpenAlex's 500-504 answers. A 429 is
    # retried only when OpenAlex asks for a short wait; a spent daily budget
    # asks for hours, which httr2 would sleep through.
    httr2::req_timeout(.au_timeout) |>
    httr2::req_retry(max_tries = 3, retry_on_failure = TRUE,
                     is_transient = \(resp) httr2::resp_status(resp) %in% c(500L, 502L, 503L, 504L) ||
                       (httr2::resp_status(resp) == 429L && .au_wait(resp) <= .au_timeout)) |>
    httr2::req_error(is_error = \(resp) FALSE)
  tryCatch({
    resp <- httr2::req_perform(req)
    status <- httr2::resp_status(resp)
    if (status == 404L) NULL
    else if (status == 429L) simpleError(sprintf(
      "OpenAlex request limit reached, try again in %.1f hours", .au_wait(resp) / 3600))
    else if (status != 200L) simpleError(sprintf("HTTP %d", status))
    else httr2::resp_body_json(resp, simplifyVector = FALSE)
  }, error = \(e) e)
}

# Authorships for each DOI, named by DOI; a DOI OpenAlex lacks is absent.
.au_fetch_openalex <- function(dois, mailto) {
  works <- .oa_fetch_works(dois, mailto, "those authors keep their Scopus identity")
  lapply(works, \(w) .au_authorship_table(w$authorships))
}

# OpenAlex works (doi and authorships) for each DOI, named by lower-case DOI;
# a DOI OpenAlex lacks is absent. DOIs go 50 to a request; one holding a comma
# or bar would break the filter syntax, so it is asked for on its own. Failed
# requests warn `saqrmisc_openalex_failed`, ending with `consequence`.
.oa_fetch_works <- function(dois, mailto, consequence) {
  polite <- if (is.null(mailto)) list() else list(mailto = mailto)
  plain <- dois[!grepl("[,|]", dois)]
  batches <- split(plain, ceiling(seq_along(plain) / .au_batch_size))
  replies <- c(
    lapply(batches, \(batch) .au_openalex_get("https://api.openalex.org/works", c(list(
      filter = paste0("doi:", paste(batch, collapse = "|")),
      select = "doi,authorships", `per-page` = .au_batch_size), polite))),
    lapply(setdiff(dois, plain), \(d) .au_openalex_get(
      sprintf("https://api.openalex.org/works/doi:%s", utils::URLencode(d, reserved = TRUE)),
      c(list(select = "doi,authorships"), polite)))
  )
  failed <- vapply(replies, inherits, logical(1), what = "error")
  if (any(failed)) {
    reasons <- unique(vapply(replies[failed], conditionMessage, character(1)))
    warning(warningCondition(
      sprintf("%d of %d OpenAlex request(s) failed (%s); %s",
              sum(failed), length(replies), paste(reasons, collapse = "; "), consequence),
      class = "saqrmisc_openalex_failed", call = NULL))
  }
  replies <- Filter(Negate(is.null), replies[!failed])
  works <- unlist(lapply(replies, \(r) r$results %||% list(r)), recursive = FALSE) %||% list()
  work_doi <- tolower(sub("^https://doi.org/", "",
                          vapply(works, \(w) w$doi %||% NA_character_, character(1))))
  keep <- !is.na(work_doi) & !duplicated(work_doi)
  stats::setNames(works[keep], work_doi[keep])
}

# Pairs each Scopus name with the one OpenAlex author whose name contains every
# word of its surname. No match, several matches, or an OpenAlex author claimed
# by two names leaves the name unmatched.
.au_match_openalex <- function(scopus_names, authorships) {
  surnames <- .au_words(sub(" [^ ]+$", "", scopus_names))
  candidates <- .au_words(authorships$raw_name)
  pick <- vapply(surnames, \(s) {
    hit <- which(vapply(candidates, \(cand) length(s) > 0L && all(s %in% cand), logical(1)))
    if (length(hit) == 1L) hit else NA_integer_
  }, integer(1))
  pick[pick %in% pick[duplicated(pick) & !is.na(pick)]] <- NA_integer_
  data.frame(openalex_id = authorships$openalex_id[pick],
             orcid = authorships$orcid[pick],
             stringsAsFactors = FALSE)
}

# Fills openalex_id and orcid for the rows at `look_up`.
.au_fill_openalex <- function(rows, look_up, mailto) {
  look_up <- look_up[!is.na(rows$doi[look_up]) & nzchar(rows$doi[look_up])]
  if (length(look_up) == 0L) return(rows)
  by_paper <- split(look_up, rows$eid[look_up])
  paper_doi <- vapply(by_paper, \(idx) rows$doi[idx[1L]], character(1))
  works <- .au_fetch_openalex(unique(paper_doi), mailto)
  found <- paper_doi %in% names(works)
  message(sprintf("OpenAlex: %d of %d paper(s) found", sum(found), length(found)))
  matched <- do.call(rbind, Map(
    \(idx, d) cbind(row = idx, .au_match_openalex(rows$name_scopus[idx], works[[d]])),
    by_paper[found], paper_doi[found]))
  if (!is.null(matched)) {
    rows$openalex_id[matched$row] <- matched$openalex_id
    rows$orcid[matched$row] <- matched$orcid
  }
  rows
}

# Keys whose papers carry one and the same ORCID become one person,
# "orcid:<ORCID>". A key showing two ORCIDs is left alone.
.au_unify_by_orcid <- function(rows) {
  claims <- unique(rows[!is.na(rows$person_key) & !is.na(rows$orcid), c("person_key", "orcid")])
  single <- claims[!claims$person_key %in% claims$person_key[duplicated(claims$person_key)], ]
  shared <- single[single$orcid %in% single$orcid[duplicated(single$orcid)], ]
  at <- match(rows$person_key, shared$person_key)
  ifelse(is.na(at), rows$person_key, paste0("orcid:", shared$orcid[at]))
}

# The spelling a person is known by: the most frequent, then the most recent,
# then the longest, then the first alphabetically. Different people under one
# spelling are then numbered by their number of papers.
.au_canonical <- function(rows) {
  keyed <- rows[!is.na(rows$person_key), c("person_key", "name_scopus", "year")]
  pair <- paste(keyed$person_key, keyed$name_scopus, sep = "\r")
  year <- ifelse(is.na(keyed$year), -Inf, keyed$year)
  keyed$n <- stats::ave(year, pair, FUN = length)
  keyed$latest <- stats::ave(year, pair, FUN = max)
  keyed$papers <- stats::ave(year, keyed$person_key, FUN = length)
  keyed <- keyed[order(keyed$person_key, -keyed$n, -keyed$latest,
                       -nchar(keyed$name_scopus), keyed$name_scopus), ]
  best <- keyed[!duplicated(keyed$person_key), c("person_key", "name_scopus", "papers")]

  best <- best[order(best$name_scopus, -best$papers, best$person_key), ]
  rank <- stats::ave(seq_along(best$name_scopus), best$name_scopus, FUN = seq_along)
  best$label <- ifelse(rank == 1L, best$name_scopus, sprintf("%s (%d)", best$name_scopus, rank))
  best$homonym <- stats::ave(rank, best$name_scopus, FUN = length) > 1L
  best
}

.au_identify <- function(papers, raw, openalex, mailto, split_keys = character()) {
  rows <- .au_scopus_rows(papers, raw)
  rows$openalex_id <- rep(NA_character_, nrow(rows))
  rows$orcid <- rep(NA_character_, nrow(rows))
  rows$person_key <- ifelse(is.na(rows$scopus_id), NA_character_, paste0("scopus:", rows$scopus_id))

  if (openalex) {
    # Every author is asked for: an ORCID can join Scopus IDs of any spelling.
    rows <- .au_fill_openalex(rows, seq_len(nrow(rows)), mailto)
    rows$person_key <- .au_or_na(
      rows$person_key,
      ifelse(is.na(rows$openalex_id), NA_character_, paste0("openalex:", rows$openalex_id)))
    rows$person_key <- .au_unify_by_orcid(rows)
  }
  rows <- .au_split(rows, split_keys)
  rows$source <- ifelse(!is.na(rows$scopus_id), "scopus",
                 ifelse(!is.na(rows$openalex_id), "openalex", "none"))

  best <- .au_canonical(rows)
  at <- match(rows$person_key, best$person_key)
  rows$name <- ifelse(is.na(at), .au_or_na(rows$name_before, rows$name_scopus), best$label[at])
  rows$homonym <- !is.na(at) & best$homonym[at]
  rownames(rows) <- NULL
  rows
}

# The new author field and its aligned person keys, one person once per paper.
.au_rewrite <- function(papers, rows) {
  person <- .au_or_na(rows$person_key, paste0("name:", rows$name))
  kept <- rows[!duplicated(data.frame(rows$eid, person)), ]
  by_paper <- split(kept, factor(kept$eid, levels = papers$eid))
  identified <- vapply(by_paper, \(p) any(!is.na(p$person_key)), logical(1))
  new_au <- vapply(by_paper, \(p) paste(p$name, collapse = ";"), character(1))
  new_id <- vapply(by_paper, \(p) paste(.au_or_na(p$person_key, "-"), collapse = ";"), character(1))
  list(AU = unname(ifelse(identified, new_au, papers$AU)),
       AU_ID = unname(ifelse(identified, new_id, NA_character_)))
}

.au_changes <- function(rows) {
  keyed <- rows[!is.na(rows$person_key), ]
  seen <- .au_or_na(keyed$name_before, keyed$name_scopus)
  n_unique <- \(x) length(unique(x[!is.na(x)]))
  spellings <- tapply(seen, keyed$person_key, \(s) paste(sort(unique(s)), collapse = " | "))
  n_spellings <- tapply(seen, keyed$person_key, n_unique)
  papers <- tapply(keyed$eid, keyed$person_key, n_unique)
  scopus_ids <- tapply(keyed$scopus_id, keyed$person_key, n_unique)
  orcids <- tapply(keyed$orcid, keyed$person_key, n_unique)
  first <- keyed[!duplicated(keyed$person_key), c("person_key", "name", "homonym", "split_from")]

  # A merge is checked when one of its spellings does not look like its name.
  spelled <- unique(data.frame(person_key = keyed$person_key, seen = seen,
                               name = sub(" \\([0-9]+\\)$", "", keyed$name)))
  spelled <- spelled[!is.na(spelled$seen) & spelled$seen != spelled$name, ]
  unlike <- unique(spelled$person_key[!.au_similar(spelled$seen, spelled$name)])

  out <- data.frame(
    person_key = first$person_key,
    name = first$name,
    spellings = as.character(spellings[first$person_key]),
    papers = as.integer(papers[first$person_key]),
    scopus_ids = as.integer(scopus_ids[first$person_key]),
    merged = as.integer(n_spellings[first$person_key]) > 1L,
    check = rep(FALSE, nrow(first)),
    homonym = first$homonym,
    orcid_conflict = as.integer(orcids[first$person_key]) > 1L,
    split = !is.na(first$split_from)
  )
  out$check <- (out$merged | out$scopus_ids > 1L) & out$person_key %in% unlike
  out <- out[out$merged | out$homonym | out$scopus_ids > 1L | out$orcid_conflict |
               out$split | out$spellings != out$name, ]
  out <- out[order(-out$check, -out$papers, out$name, out$person_key), ]
  rownames(out) <- NULL
  out
}

# Takes the people in `split_keys` apart again: a person ORCID joined from
# several Scopus IDs into those IDs, any other person into its spellings.
.au_split <- function(rows, split_keys) {
  unmatched <- setdiff(split_keys, rows$person_key)
  if (length(unmatched) > 0L) {
    warning(warningCondition(
      sprintf("%d reviewed person(s) marked split are not in this data: %s",
              length(unmatched), paste(utils::head(unmatched, 5L), collapse = ", ")),
      class = "saqrmisc_review_unmatched", call = NULL))
  }
  hit <- !is.na(rows$person_key) & rows$person_key %in% split_keys
  rows$split_from <- ifelse(hit, rows$person_key, NA_character_)
  if (!any(hit)) return(rows)
  ids <- tapply(rows$scopus_id[hit], rows$person_key[hit], \(s) length(unique(s[!is.na(s)])))
  by_id <- rows$person_key %in% names(ids)[ids > 1L] & !is.na(rows$scopus_id)
  rows$person_key <- ifelse(by_id, paste0("scopus:", rows$scopus_id),
                     ifelse(hit, paste0(rows$person_key, "|", rows$name_scopus), rows$person_key))
  rows
}

# The `split` keys of a review: a review_sheet() file or a data frame with
# `person_key` and `decision` (empty, keep or split).
.au_review_splits <- function(review) {
  if (is.null(review)) return(character())
  if (is.character(review) && length(review) == 1L) {
    if (!file.exists(review)) .bad_input(sprintf("`review` file %s does not exist", review))
    review <- if (grepl("\\.xlsx$", review, ignore.case = TRUE)) {
      .kr_need("readxl")
      as.data.frame(readxl::read_excel(review, sheet = 1L))
    } else if (grepl("\\.csv$", review, ignore.case = TRUE)) {
      .kr_read_csv(review)
    } else {
      .bad_input("`review` must be a .csv or .xlsx file from review_sheet(), or a data frame")
    }
  }
  if (!is.data.frame(review) || !all(c("person_key", "decision") %in% names(review))) {
    .bad_input("`review` needs the columns person_key and decision, as review_sheet() writes them")
  }
  decision <- tolower(trimws(as.character(review$decision)))
  decision[is.na(decision)] <- ""
  odd <- setdiff(decision, c("", "keep", "split"))
  if (length(odd) > 0L) {
    .bad_input(sprintf("`review$decision` must be empty, keep or split, not %s",
                       paste(sprintf("\"%s\"", odd), collapse = ", ")))
  }
  keys <- trimws(as.character(review$person_key))
  unique(keys[decision == "split" & !is.na(keys) & nzchar(keys)])
}

# Do two spellings look like one name? Lost accents ("KRMER N" for KRÄMER),
# transliteration (YAROVII, YAROVYI), initials against given names ("MOORE RL",
# "MOORE ROBERT L") and a dropped surname part ("LOPEZ SS", "SAFFON LOPEZ S")
# do; a different surname ("ENDRASS B", "LUGRIN B") does not.
.au_similar <- function(a, b) {
  words_a <- lapply(.au_words(a), \(w) w[!w %in% .au_suffixes])
  words_b <- lapply(.au_words(b), \(w) w[!w %in% .au_suffixes])
  vapply(seq_along(words_a), \(i) .au_same_reading(words_a[[i]], words_b[[i]]) ||
           .au_shared_surname(words_a[[i]], words_b[[i]]), logical(1))
}

# Some reading of each name as surname + initials agrees on both.
.au_same_reading <- function(wa, wb) {
  ra <- .au_readings(wa)
  rb <- .au_readings(wb)
  pairs <- expand.grid(i = seq_len(nrow(ra)), j = seq_len(nrow(rb)))
  any(mapply(\(i, j) .au_surname_close(ra$surname[i], rb$surname[j]) &&
               .au_initials_agree(ra$initials[[i]], rb$initials[[j]]),
             pairs$i, pairs$j))
}

# Each split of the words into surname and given part. The given part is read
# three ways, since "RL" and "ROBERT L" are both initials RL: as written, by
# first letters, and with long words by their first letter.
.au_readings <- function(words) {
  if (length(words) == 0L) return(data.frame(surname = "", initials = I(list(""))))
  # A name is all surname only when it is one word: "KIMJ" is no reading of KIM J.
  k <- seq_len(max(1L, length(words) - 1L))
  given <- lapply(k, \(i) words[-seq_len(i)])
  data.frame(
    surname = vapply(k, \(i) paste(words[seq_len(i)], collapse = ""), character(1)),
    initials = I(lapply(given, \(g) unique(c(
      paste(g, collapse = ""),
      paste(substr(g, 1L, 1L), collapse = ""),
      paste(.au_shorten(g), collapse = ""))))),
    stringsAsFactors = FALSE
  )
}

.au_initials_agree <- function(x, y) {
  any(outer(x, y, \(p, q) startsWith(p, q) | startsWith(q, p)))
}

# Surnames are close when one letter or two differ (transliteration), or when
# the shorter is the longer with up to a third of its letters lost, as
# bibliometrix loses accented letters ("CEREKOVIC" -> "EREKOVI").
.au_surname_close <- function(x, y) {
  if (!nzchar(x) || !nzchar(y)) return(FALSE)
  if (utils::adist(x, y) <= if (min(nchar(x), nchar(y)) < 6L) 1L else 2L) return(TRUE)
  short <- if (nchar(x) <= nchar(y)) x else y
  long <- if (nchar(x) <= nchar(y)) y else x
  lost <- nchar(long) - nchar(short)
  nchar(short) >= 3L && lost <= min(3L, nchar(long) %/% 3L) &&
    grepl(paste(strsplit(short, "")[[1L]], collapse = ".*"), long)
}

# Both names hold one surname word of three letters or more, leading at least
# one of them, and their other words share an initial: "LOPEZ SS" and "SAFFON
# LOPEZ S", not "KIM J" and "KIM S", nor "KIM JOHN" and "PARK JOHN".
.au_shared_surname <- function(wa, wb) {
  shared <- intersect(wa[nchar(wa) >= 3L], wb[nchar(wb) >= 3L])
  shared <- shared[shared %in% c(wa[1L], wb[1L])]
  if (length(shared) == 0L) return(FALSE)
  letters_of <- \(w) unique(unlist(strsplit(.au_shorten(w), "")))
  length(intersect(letters_of(setdiff(wa, shared)), letters_of(setdiff(wb, shared)))) > 0L
}

# Given-name words by their initials: a word longer than four letters is a
# name and gives its first letter; a shorter one may be initials and is kept.
.au_shorten <- function(words) {
  vapply(words, \(w) if (nchar(w) > 4L) substr(w, 1L, 1L) else w, character(1), USE.NAMES = FALSE)
}
