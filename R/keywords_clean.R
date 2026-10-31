# Keyword cleaning for bibliometric data. Each keyword is cleaned as a whole
# term, never by pattern: text cleanup, acronyms in brackets, the user's
# replacement tables, merging of similar spellings, dropping, de-duplication.

#' Clean bibliometric keywords
#'
#' @description
#' Rewrites keyword columns (such as bibliometrix's `DE` and `ID`) so that one
#' concept has one spelling. Every keyword is handled as a whole term, so its
#' position in the list, the separator spacing, and characters such as `(`,
#' `.` or `+` make no difference. In order:
#'
#' 1. **Cleanup**: HTML entities decoded, dashes and quotes unified, `&`
#'    written `AND`, a leading `#` dropped, spacing squeezed, upper case.
#' 2. **Brackets**: a plural marker is dropped (`VOICE ASSISTANT(S)`); an
#'    acronym in brackets is dropped and remembered
#'    (`NATURAL LANGUAGE PROCESSING (NLP)`, also `B-RNN (BI-DIRECTIONAL ...)`);
#'    other brackets lose only the bracket characters.
#' 3. **Acronyms** (`expand_acronyms = TRUE`): a keyword that is exactly an
#'    acronym seen in brackets becomes its long form (`NLP` becomes
#'    `NATURAL LANGUAGE PROCESSING`), but only when no other keyword in the
#'    data spells the same initials (`HRI` stays when both
#'    `HUMAN-ROBOT INTERFACE` and `HUMAN-ROBOT INTERACTION` occur).
#' 4. **Replace**: whole-term `OLD` to `NEW` replacements from `replace`,
#'    cleaned the same way; chains (`A` to `B`, `B` to `C`) resolve to the last
#'    term, and a later row overrides an earlier one that contradicts it.
#' 5. **Merge similar** (`merge_similar = TRUE`): keywords that differ only by
#'    plural, hyphen, space or punctuation (`CHAT BOT`, `CHAT-BOT`,
#'    `CHATBOTS`) become one: a `NEW` term from `replace` if one is among
#'    them, else the form in most papers.
#' 6. **Drop** the `drop` keywords, then keep each keyword once per paper.
#'
#' @param data A data frame with one row per paper.
#'   The result of another `clean_*()` function works too, so the cleaners
#'   chain: `M |> authors_clean(scopus = "Dlds") |> keywords_clean()`.
#' @param columns Names of the keyword columns, `;`-separated. Default `"DE"`
#'   (bibliometrix's author keywords); `c("DE", "ID")` adds index keywords.
#' @param replace Your replacement table: a data frame with columns `OLD` and
#'   `NEW` (more columns are ignored), or the path to a `.csv`, `.xlsx` or
#'   `.xls` file holding one. An [ask_ai()] result also works (its pairs
#'   with `decision` `accept` are applied), and so does a review sheet from
#'   [review_sheet()] (pairs whose `decision_2`, or else `decision`, is
#'   `accept`).
#'   Several tables or paths are applied in order,
#'   so a later table corrects an earlier one; within a table a later row
#'   overrides an earlier one. Reading `.xlsx`/`.xls` needs readxl.
#' @param drop Keywords to delete, e.g. `c("SURVEY", "CASE STUDY")`. Similar
#'   spellings are deleted too (`"SURVEY"` also drops `SURVEYS`).
#' @param merge_similar Merge keywords that differ only by plural, hyphen,
#'   space or punctuation? Default `TRUE`.
#' @param expand_acronyms Expand a keyword that is an acronym into the long
#'   form found in brackets elsewhere in the data? Default `TRUE`.
#'
#' @return A `saqr_keywords` object. Use `as.data.frame()` on it:
#' \itemize{
#'   \item `as.data.frame(x)`: `data` with the keyword columns rewritten,
#'     `"; "`-separated, each keyword once per paper.
#'   \item `as.data.frame(x, what = "changes")`: one row per keyword that
#'     changed, most papers first: `column`, `from` (as written in `data`),
#'     `to` (`NA` when dropped), `reason` (`cleanup`, `brackets`, `acronym`,
#'     `replaced`, `merged`, `dropped`, joined by `+`) and `papers`.
#'   \item `as.data.frame(x, what = "terms")`: one row per cleaned keyword,
#'     most papers first: `column`, `keyword`, `papers` and `forms` (how many
#'     written forms it gathers).
#' }
#' `summary()` returns one row per column with counts.
#'
#' @section Errors:
#' Raises `saqrmisc_bad_input` for a missing or non-character keyword column,
#' a `replace` table without `OLD` and `NEW`, or a `replace` file that does not
#' exist or is not `.csv`/`.xlsx`/`.xls`; `saqrmisc_missing_package` for an
#' `.xlsx` file without readxl. Warns `saqrmisc_replace_conflict`, listing each
#' row, when rows are overridden by a later row: the same `OLD` with another
#' `NEW`, or a loop (`A` to `B` above `B` to `A`).
#'
#' @examples
#' papers <- data.frame(
#'   id = 1:3,
#'   DE = c("CHATBOT;  NATURAL LANGUAGE PROCESSING (NLP);  AI",
#'          "CHAT-BOTS;  NLP;  ARTIFICIAL INTELLIGENCE",
#'          "CHATBOTS;  E-LEARNING;  SURVEY")
#' )
#' my_table <- data.frame(OLD = "ARTIFICIAL INTELLIGENCE", NEW = "AI")
#' cleaned <- keywords_clean(papers, replace = my_table, drop = "SURVEY")
#' cleaned
#' as.data.frame(cleaned)
#'
#' @export
keywords_clean <- function(data,
                           columns = "DE",
                           replace = NULL,
                           drop = NULL,
                           merge_similar = TRUE,
                           expand_acronyms = TRUE) {
  data <- .papers_of(data)
  .kw_check_columns(data, columns)
  if (!is.null(drop) && !is.character(drop)) .bad_input("`drop` must be a character vector of keywords")
  if (!(isTRUE(merge_similar) || isFALSE(merge_similar))) {
    .bad_input("`merge_similar` must be TRUE or FALSE")
  }
  if (!(isTRUE(expand_acronyms) || isFALSE(expand_acronyms))) {
    .bad_input("`expand_acronyms` must be TRUE or FALSE")
  }

  map <- .kw_replacements(replace)
  drop_terms <- if (is.null(drop)) character() else .kw_brackets(.kw_normalise(drop))$term

  results <- lapply(columns, \(f) .kw_clean_column(data[[f]], f, map, drop_terms,
                                                   merge_similar, expand_acronyms))
  cleaned <- data
  cleaned[columns] <- lapply(results, \(r) r$values)
  structure(
    list(data = cleaned,
         changes = .kw_bind(lapply(results, \(r) r$changes)),
         terms = .kw_bind(lapply(results, \(r) r$terms)),
         columns = columns),
    class = "saqr_keywords"
  )
}

#' Tables from a keywords_clean() result
#'
#' @param x A [keywords_clean()] result.
#' @param row.names,optional Ignored; present for the generic.
#' @param what Which table: `"data"` (the cleaned papers, default),
#'   `"changes"` (as [keywords_changes()]) or `"terms"` (one row per cleaned
#'   keyword). See [keywords_clean()] for their columns.
#' @param ... Ignored.
#' @return A data frame.
#' @export
as.data.frame.saqr_keywords <- function(x, row.names = NULL, optional = FALSE,
                                        what = c("data", "changes", "terms"), ...) {
  what <- match.arg(what)
  switch(what, data = x$data, changes = x$changes, terms = x$terms)
}

#' Every keyword that changed
#'
#' One row per written keyword of a [keywords_clean()] result that changed,
#' with what it became and why.
#'
#' @param x A [keywords_clean()] result.
#' @return A data frame with one row per changed keyword, with the columns
#'   listed under Value in [keywords_clean()].
#' @section Errors:
#' Raises `saqrmisc_bad_input` when `x` is not a [keywords_clean()] result.
#' @examples
#' papers <- data.frame(DE = c("Chatbots; ChatBot", "chat-bot; LLMs"))
#' keywords <- keywords_clean(papers)
#' keywords_changes(keywords)
#' @export
keywords_changes <- function(x) {
  if (!inherits(x, "saqr_keywords")) .bad_input("`x` must be a keywords_clean() result")
  x$changes
}

#' Summarise a keywords_clean() result
#'
#' @param object A [keywords_clean()] result.
#' @param ... Ignored.
#' @return A data frame with one row per keyword column: `column`, `papers`
#'   (with at least one keyword), `forms_before` (distinct written keywords),
#'   `keywords_after`, `changed` (written keywords that changed) and one
#'   column per reason (`cleanup`, `brackets`, `acronym`, `replaced`,
#'   `merged`, `dropped`) counting the written keywords it touched.
#' @export
summary.saqr_keywords <- function(object, ...) {
  reasons <- c("cleanup", "brackets", "acronym", "replaced", "merged", "dropped")
  rows <- lapply(object$columns, \(f) {
    changes <- object$changes[object$changes$column == f, ]
    terms <- object$terms[object$terms$column == f, ]
    has_keywords <- !is.na(object$data[[f]]) & nzchar(object$data[[f]])
    by_reason <- vapply(reasons, \(r) sum(grepl(r, changes$reason, fixed = TRUE)), integer(1))
    data.frame(column = f, papers = sum(has_keywords),
               forms_before = sum(terms$forms) + sum(is.na(changes$to)),
               keywords_after = nrow(terms), changed = nrow(changes),
               t(by_reason), check.names = FALSE)
  })
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

#' Print a keywords_clean() result
#'
#' @param x A [keywords_clean()] result.
#' @param ... Ignored.
#' @return `x`, invisibly.
#' @export
print.saqr_keywords <- function(x, ...) {
  counts <- summary(x)
  cat("Keyword cleaning\n")
  lines <- sprintf("  %s: %d written forms became %d keywords in %d papers (%d replaced, %d merged, %d acronyms expanded, %d dropped)",
                   counts$column, counts$forms_before, counts$keywords_after, counts$papers,
                   counts$replaced, counts$merged, counts$acronym, counts$dropped)
  cat(lines, sep = "\n")
  cat("as.data.frame(x) gives the cleaned papers; keywords_changes(x) lists every change.\n")
  invisible(x)
}

# ---- helpers ----------------------------------------------------------------

.kw_stopwords <- c("OF", "AND", "THE", "FOR", "IN", "ON", "TO", "A", "AN", "WITH", "BY")

.kw_bind <- function(tables) {
  out <- do.call(rbind, tables)
  rownames(out) <- NULL
  out
}

# The papers a cleaner works on: a data frame, or the cleaned data of an
# earlier clean_*() result, so that the cleaners chain with |>.
.papers_of <- function(data) {
  if (inherits(data, c("saqr_authors", "saqr_keywords", "saqr_institutions"))) data$data else data
}

.kw_check_columns <- function(data, columns) {
  if (!is.data.frame(data)) .bad_input("`data` must be a data frame")
  if (!is.character(columns) || length(columns) == 0L || anyNA(columns)) {
    .bad_input("`columns` must name one or more keyword columns")
  }
  absent <- setdiff(columns, names(data))
  if (length(absent) > 0L) {
    .bad_input(sprintf("`data` has no column(s) %s", paste(absent, collapse = ", ")))
  }
  not_text <- columns[!vapply(columns, \(f) is.character(data[[f]]) || all(is.na(data[[f]])), logical(1))]
  if (length(not_text) > 0L) {
    .bad_input(sprintf("keyword column(s) %s must be character", paste(not_text, collapse = ", ")))
  }
  invisible(TRUE)
}

.kw_split <- function(values) {
  # Not ifelse(): on zero papers it returns logical(0), which strsplit refuses.
  values[is.na(values)] <- ""
  lapply(strsplit(values, ";", fixed = TRUE), \(k) {
    k <- trimws(k)
    k[nzchar(k)]
  })
}

# Text cleanup that never changes what a keyword means.
.kw_normalise <- function(x) {
  x |>
    .au_decode_entities() |>
    gsub(pattern = "[\u2010-\u2015\u2212]", replacement = "-") |>
    gsub(pattern = "[\u2018\u2019]", replacement = "'") |>
    gsub(pattern = "[\"`\u201c\u201d]", replacement = "") |>
    gsub(pattern = "^\\s*#+", replacement = "") |>
    gsub(pattern = "\\s*&\\s*", replacement = " AND ") |>
    gsub(pattern = "\\s+", replacement = " ") |>
    gsub(pattern = "[.,;:]+$", replacement = "") |>
    trimws() |>
    toupper()
}

.kw_initials <- function(phrase, split_hyphens, drop_stopwords) {
  words <- strsplit(phrase, if (split_hyphens) "[ /-]+" else "[ /]+")[[1]]
  words <- words[nzchar(words)]
  if (drop_stopwords) words <- words[!words %in% .kw_stopwords]
  paste(substr(words, 1L, 1L), collapse = "")
}

# The letters an acronym may spell: as written, or without a plural S (ECAS).
.kw_acronym_letters <- function(short) {
  letters_only <- gsub("[^A-Z0-9]", "", short)
  unique(c(letters_only, if (nchar(letters_only) > 2L) sub("S$", "", letters_only)))
}

# A phrase's initials, with and without hyphenated parts and stop words.
.kw_spellings <- function(phrase) {
  unique(unlist(lapply(c(TRUE, FALSE), \(h) vapply(c(TRUE, FALSE), \(s)
    .kw_initials(phrase, h, s), character(1)))))
}

# Is `short` an acronym of `long`?
.kw_is_acronym <- function(short, long) {
  letters_only <- gsub("[^A-Z0-9]", "", short)
  if (nchar(letters_only) < 2L || nchar(letters_only) >= nchar(gsub("[^A-Z0-9]", "", long))) {
    return(FALSE)
  }
  any(.kw_acronym_letters(short) %in% .kw_spellings(long))
}

# Brackets: "(S)" plural markers go; a trailing "(...)" is kept apart so an
# acronym in either position can be recognised.
.kw_brackets <- function(x) {
  x <- trimws(gsub("\\s+", " ", gsub("\\(S\\)", "", x)))
  parts <- regmatches(x, regexec("^(.*?)\\s*\\(([^()]+)\\)$", x))
  out <- lapply(seq_along(x), \(i) {
    p <- parts[[i]]
    if (length(p) == 0L || !nzchar(trimws(p[2L]))) {
      return(c(term = trimws(gsub("[()]", " ", x[i])), short = NA, long = NA))
    }
    outer <- trimws(p[2L])
    inner <- trimws(p[3L])
    if (.kw_is_acronym(inner, outer)) return(c(term = outer, short = inner, long = outer))
    if (.kw_is_acronym(outer, inner)) return(c(term = inner, short = outer, long = inner))
    c(term = paste(outer, inner), short = NA, long = NA)
  })
  out <- do.call(rbind, out)
  data.frame(term = gsub("\\s+", " ", out[, "term"]), short = out[, "short"],
             long = out[, "long"], stringsAsFactors = FALSE)
}

# Conservative singular: STUDIES -> STUDY, CLASSES -> CLASS, AGENTS -> AGENT;
# STATUS, ANALYSIS, CLASS and short words stay.
.kw_singular <- function(w) {
  ifelse(nchar(w) > 4L & grepl("IES$", w), sub("IES$", "Y", w),
  ifelse(grepl("(SS|X)ES$", w), sub("ES$", "", w),
  ifelse(nchar(w) > 3L & grepl("S$", w) & !grepl("(SS|US|IS)$", w), sub("S$", "", w), w)))
}

# The comparison key for spelling variants: punctuation and spaces gone, each
# word made singular, Latin accents folded. Digits stay, so WEB 2.0 is not
# WEB 3.0. A word that cannot be transliterated (e.g. Chinese) is kept as it
# is, and a keyword with no letters keys on itself, so neither collapses into
# another keyword.
.kw_key <- function(x) {
  words <- strsplit(gsub("['`\u2019]", "", toupper(x)), "[^[:alnum:]]+")
  vapply(seq_along(words), \(i) {
    w <- words[[i]]
    w <- w[nzchar(w)]
    if (length(w) == 0L) return(paste0("\r", x[i]))
    folded <- iconv(w, "UTF-8", "ASCII//TRANSLIT")
    ok <- !is.na(folded) & !grepl("?", folded, fixed = TRUE)
    w[ok] <- gsub("[^A-Z0-9]", "", toupper(folded[ok]))
    paste(.kw_singular(w), collapse = "")
  }, character(1))
}

# The replacement tables as a list of named vectors OLD -> NEW, applied in
# order. Each element of `replace` is a data frame or a file path.
.kw_replacements <- function(replace) {
  if (is.null(replace)) return(list())
  single <- is.data.frame(replace) || inherits(replace, c("saqr_review", "saqr_proposals"))
  tables <- if (single) list(replace) else as.list(replace)
  tables <- lapply(tables, \(t) if (is.character(t) && length(t) == 1L) .kw_read_table(t) else t)
  tables <- lapply(tables, .kr_review_as_table)
  if (!all(vapply(tables, \(t) is.data.frame(t) && all(c("OLD", "NEW") %in% names(t)), logical(1)))) {
    .bad_input("`replace` must be tables with columns OLD and NEW, or paths to .csv/.xlsx files holding them")
  }
  lapply(tables, .kw_table_map)
}

.kw_read_table <- function(path) {
  if (!file.exists(path)) .bad_input(sprintf("`replace` file not found: %s", path))
  type <- tolower(sub(".*\\.([^.]+)$", "\\1", basename(path)))
  if (identical(type, "csv")) return(.kr_read_csv(path))
  if (!type %in% c("xlsx", "xls")) {
    .bad_input(sprintf("`replace` file must be .csv, .xlsx or .xls: %s", path))
  }
  if (!requireNamespace("readxl", quietly = TRUE)) {
    stop(errorCondition(
      sprintf("reading %s needs the readxl package: install.packages(\"readxl\")", basename(path)),
      class = "saqrmisc_missing_package", call = NULL))
  }
  as.data.frame(readxl::read_excel(path))
}

.kw_apply_maps <- function(x, maps) {
  Reduce(\(v, m) ifelse(v %in% names(m), m[v], v), maps, x)
}

# One table as a named vector OLD -> NEW, both sides cleaned like the data,
# identity rows dropped and chains resolved (A -> B, B -> C gives A -> C).
# A later row overrides an earlier one: the table is read from the bottom, and
# a row is skipped when a later row already set its OLD term or when it would
# close a loop with later rows (A -> B above B -> A).
.kw_table_map <- function(table) {
  old <- .kw_brackets(.kw_normalise(as.character(table$OLD)))$term
  new <- .kw_brackets(.kw_normalise(as.character(table$NEW)))$term
  keep <- !is.na(old) & !is.na(new) & nzchar(old) & nzchar(new) & old != new
  old <- old[keep]
  new <- new[keep]

  state <- Reduce(\(acc, i) {
    o <- old[i]
    n <- new[i]
    set_later <- o %in% names(acc$map)
    if (set_later && identical(acc$map[[o]], n)) return(acc)
    if (set_later || identical(.kw_follow(n, acc$map), o)) {
      acc$overridden <- c(acc$overridden, sprintf("%s -> %s", o, n))
      return(acc)
    }
    acc$map <- c(acc$map, stats::setNames(n, o))
    acc
  }, rev(seq_along(old)), list(map = stats::setNames(character(), character()), overridden = character()))

  if (length(state$overridden) > 0L) {
    warning(warningCondition(
      sprintf("%d `replace` row(s) contradict a later row and are not used: %s",
              length(state$overridden), paste(rev(state$overridden), collapse = "; ")),
      class = "saqrmisc_replace_conflict", call = NULL))
  }
  map <- state$map
  stats::setNames(vapply(unname(map), .kw_follow, character(1), map = map), names(map))
}

# Follows a term through the map to its last replacement. The map has no
# loops, so each step reaches a new term.
.kw_follow <- function(term, map) {
  if (!term %in% names(map)) return(term)
  .kw_follow(map[[term]], map)
}

.kw_clean_column <- function(values, column, map, drop_terms, merge_similar, expand_acronyms) {
  lists <- .kw_split(values)
  written <- unique(unlist(lists))
  papers <- tabulate(match(unlist(lapply(lists, unique)), written), length(written))
  if (length(written) == 0L) {
    empty <- data.frame(column = character(), from = character(), to = character(),
                        reason = character(), papers = integer())
    return(list(values = values, changes = empty,
                terms = data.frame(column = character(), keyword = character(),
                                   papers = integer(), forms = integer())))
  }

  cleaned <- .kw_normalise(written)
  bracketed <- .kw_brackets(cleaned)
  stage <- bracketed$term

  expanded <- stage
  if (expand_acronyms) {
    pairs <- unique(stats::na.omit(bracketed[c("short", "long")]))
    pairs$key <- .kw_key(pairs$short)
    # Every multi-word keyword whose initials spell the acronym is a possible
    # long form, so HRI is left alone when both HUMAN-ROBOT INTERFACE and
    # HUMAN-ROBOT INTERACTION occur. Variants of one long form count once.
    phrases <- unique(c(pairs$long, stage[grepl("[ -]", stage)]))
    spelled <- lapply(phrases, .kw_spellings)
    spellings <- data.frame(spelling = unlist(spelled),
                            key = rep(.kw_key(phrases), lengths(spelled)))
    long_forms <- vapply(split(pairs$short, pairs$key), \(shorts) {
      wanted <- unique(unlist(lapply(shorts, .kw_acronym_letters)))
      length(unique(spellings$key[spellings$spelling %in% wanted]))
    }, integer(1))
    pairs <- pairs[pairs$key %in% names(long_forms)[long_forms == 1L] & !duplicated(pairs$key), ]
    hit <- match(.kw_key(stage), pairs$key)
    is_acronym <- !is.na(hit) & is.na(bracketed$short)
    expanded[is_acronym] <- pairs$long[hit[is_acronym]]
  }

  mapped <- .kw_apply_maps(expanded, map)

  merged <- mapped
  if (merge_similar) {
    key <- .kw_key(mapped)
    # A table's NEW terms, as later tables leave them, are the preferred forms.
    targets <- unique(unlist(lapply(seq_along(map), \(i)
      .kw_apply_maps(unname(map[[i]]), map[-seq_len(i)]))))
    weight <- tapply(papers, mapped, sum)
    forms <- data.frame(form = names(weight), papers = as.integer(weight),
                        key = .kw_key(names(weight)), target = names(weight) %in% targets)
    forms <- forms[order(forms$key, !forms$target, -forms$papers, nchar(forms$form), forms$form), ]
    best <- forms[!duplicated(forms$key), ]
    merged <- best$form[match(key, best$key)]
  }

  dropped <- if (length(drop_terms) == 0L) rep(FALSE, length(merged))
    else if (merge_similar) .kw_key(merged) %in% .kw_key(drop_terms)
    else merged %in% drop_terms
  final <- ifelse(dropped, NA_character_, merged)

  steps <- list(cleanup = cleaned != written, brackets = stage != cleaned,
                acronym = expanded != stage, replaced = mapped != expanded,
                merged = merged != mapped, dropped = dropped)
  reason <- vapply(seq_along(written), \(i) {
    hits <- names(steps)[vapply(steps, \(s) s[i], logical(1))]
    paste(hits, collapse = "+")
  }, character(1))

  new_values <- vapply(lists, \(k) {
    out <- unique(stats::na.omit(final[match(k, written)]))
    if (length(out) == 0L) NA_character_ else paste(out, collapse = "; ")
  }, character(1))
  new_values[is.na(values)] <- NA_character_

  changed <- nzchar(reason)
  changes <- data.frame(column = rep(column, sum(changed)), from = written[changed], to = final[changed],
                        reason = reason[changed], papers = papers[changed])
  changes <- changes[order(-changes$papers, changes$from), ]

  kept <- !is.na(final)
  final_lists <- .kw_split(new_values)
  final_terms <- unique(final[kept])
  terms <- data.frame(
    column = rep(column, length(final_terms)),
    keyword = final_terms,
    papers = tabulate(match(unlist(final_lists), final_terms), length(final_terms)),
    forms = as.integer(table(factor(final[kept], levels = final_terms)))
  )
  terms <- terms[order(-terms$papers, terms$keyword), ]

  list(values = new_values, changes = changes, terms = terms)
}
