# Keyword cleaning for bibliometric data. The work is done on the distinct
# keywords, never paper by paper: each written form gets its final form once,
# and the papers are rewritten by lookup, only where something changes.

#' Clean bibliometric keywords
#'
#' @description
#' Gives each concept one spelling in the author keywords (bibliometrix's
#' `DE`) and returns the data frame, ready for bibliometrix. Only papers
#' whose keywords change are rewritten; the others keep their text exactly.
#' Nothing is ever deleted.
#'
#' The confirmed steps, applied with no review:
#' 1. **Cleanup**: HTML entities decoded, dashes and quotes unified, `&`
#'    written `AND`, a leading `#` and trailing punctuation dropped, spacing
#'    squeezed, upper case. A keyword cleanup would leave empty (`.`) keeps
#'    its written form.
#' 2. **Brackets**: a plural marker goes (`VOICE ASSISTANT(S)`); an acronym in
#'    brackets goes (`NATURAL LANGUAGE PROCESSING (NLP)`, also
#'    `B-RNN (BI-DIRECTIONAL ...)`); other brackets lose only the brackets.
#' 3. **Your tables**: the `OLD` to `NEW` rows of `replace`; chains (`A` to
#'    `B`, `B` to `C`) resolve to the last term, and a later row overrides an
#'    earlier one that contradicts it.
#' 4. **Same keyword, written differently**: forms that differ only by case,
#'    punctuation, accents, spaces, hyphens or plural (`CHAT BOT`, `CHAT-BOT`,
#'    `CHATBOTS`) become one, in word order (`TEXT TO SPEECH` and
#'    `SPEECH TO TEXT` stay apart): a `NEW` term of `replace` if one is among
#'    them, else the form in most papers.
#' 5. Each keyword once per paper.
#'
#' `level = "all"` merges nothing: it returns `data` unchanged and writes
#' every merge to the `decisions` sheet, for a person, a model or a tool such
#' as OpenRefine to check outside R, with a short prompt for a model beside
#' it. The sheet has one group per `name` (the keyword the group becomes): its `confirmed`
#' merges (the steps above) and its `needs_revision` candidates, found in
#' six ways (`source`), none by comparing all pairs:
#' * `spelling`: character trigrams, Jaccard 0.7 or more, by MinHash;
#' * `word order`: the same words in another order (`PSYCHOLOGY, CLINICAL`);
#' * `ngram`: the same letter pairs (OpenRefine's 2-gram fingerprint);
#' * `stem`: the words share their stems (Porter; needs SnowballC);
#' * `acronym`: an acronym whose long form is written in brackets elsewhere
#'   in the data;
#' * `initials`: an acronym spelled by the initials of a keyword
#'   (`NLP`, `NATURAL LANGUAGE PROCESSING`).
#'
#' Most of these propose some wrong pairs (`APPLICATION` /
#' `APPLICABILITY` share a stem), which is why candidates wait for a `yes`. Spelled alike is not the same concept
#' (`JUSTICE` / `INJUSTICE`). Only
#' keywords in at least `min_papers` papers go to the sheet, each of them,
#' grouped or alone (`single`), so a tool such as OpenRefine can also cluster
#' the whole list itself. Bring the checked sheet back with [keywords_merge()]:
#' by its `merge` answers, or by the edits of `keyword` against the untouched
#' `keyword_backup`.
#'
#' @param data A data frame with one row per paper, such as a bibliometrix
#'   data frame. The cleaners chain: `M |> authors_clean(ids = "scopus_exports")
#'   |> keywords_clean()`.
#' @param columns Names of the keyword columns, `;`-separated. Default `"DE"`
#'   (author keywords).
#' @param level `"confirmed"` (default) or `"all"`; see above.
#' @param decisions With `level = "all"`: the sheet to write, a new path
#'   ending in `.xlsx` or `.csv` (an existing file is never replaced). Its
#'   columns: `group`, `name`, `keyword`, `keyword_backup` (a copy of
#'   `keyword` that [keywords_merge()] matches on, so an edit of `keyword`
#'   does no harm), `status` (`single` for a keyword in no group,
#'   `confirmed` or `needs_revision`),
#'   `papers`, `source` (`same spelling`,
#'   `your table`, or the ways a candidate was found, see above) and `example` (the title of a paper
#'   using the keyword). The prompt goes to `<sheet>_prompt.txt` and, in an
#'   `.xlsx`, to a second tab `prompt`.
#' @param min_papers Only keywords in at least this many papers go into the
#'   sheet. Default 5: on a 2-million-paper psychology corpus, the 10\% of
#'   author keywords in 5 or more papers carry 81\% of all keyword uses.
#' @param replace Your replacement table: a data frame with columns `OLD` and
#'   `NEW` (more columns are ignored), or the path to a `.csv`, `.xlsx` or
#'   `.xls` file holding one. An [ask_ai()] result or a [review_sheet()] file
#'   also works (its accepted pairs are applied). Several tables are applied in
#'   order, so a later table corrects an earlier one. Reading `.xlsx`/`.xls`
#'   needs readxl.
#' @param titles Name of the title column, used for the `example` column of
#'   the sheet. Default `"TI"`; skipped when `data` has no such column.
#' @param output `"minimal"` (default) returns `data` with the keyword columns
#'   rewritten and nothing added. `"all"` also adds `<column>_ORIGINAL` for
#'   each column, as it came in (kept when `data` already has it).
#'
#' @return With `level = "confirmed"`: `data`, same class, rows and columns,
#'   with the keyword columns rewritten where they change (`"; "`-separated);
#'   with `output = "all"`, plus the `_ORIGINAL` columns. With
#'   `level = "all"`: `data` unchanged, invisibly (the sheet is the result). A message reports, per column, the written
#'   forms that became one keyword and the papers changed.
#'
#' @section Errors:
#' Raises `saqrmisc_bad_input` for a missing or non-character keyword column,
#' a `replace` table without `OLD` and `NEW` or a `replace` file that does not
#' exist or is not `.csv`/`.xlsx`/`.xls`, `decisions` given with
#' `level = "confirmed"`, missing or already existing with `"all"`, and a
#' `min_papers` that is not one positive number; `saqrmisc_missing_package`
#' for `.xlsx` without readxl / writexl. Warns `saqrmisc_replace_conflict`,
#' listing each row, when `replace` has rows overridden by a later row.
#'
#' @references
#' Broder, A. Z. (1997). On the resemblance and containment of documents.
#' *Proceedings of Compression and Complexity of Sequences*, 21-29.
#' \doi{10.1109/SEQUEN.1997.666900}
#'
#' Porter, M. F. (1980). An algorithm for suffix stripping. *Program*,
#' 14(3), 130-137. \\doi{10.1108/eb046814}
#'
#' Leskovec, J., Rajaraman, A., & Ullman, J. D. (2020). *Mining of Massive
#' Datasets* (3rd ed.), chapter 3. Cambridge University Press.
#'
#' @seealso [keywords_merge()] to bring the checked sheet back.
#' @examples
#' papers <- data.frame(
#'   TI = c("Paper one", "Paper two", "Paper three"),
#'   DE = c("CHATBOT; NATURAL LANGUAGE PROCESSING (NLP); AI",
#'          "CHAT-BOTS; NLP; ARTIFICIAL INTELLIGENCE",
#'          "CHATBOTS; E-LEARNING; SURVEY")
#' )
#' keywords_clean(papers)
#' keywords_clean(papers, replace = data.frame(OLD = "AI", NEW = "ARTIFICIAL INTELLIGENCE"),
#'                output = "all")
#'
#' # Every merge to a sheet, for a person or a model to check.
#' sheet <- tempfile(fileext = ".csv")
#' cleaned <- keywords_clean(papers, level = "all", decisions = sheet, min_papers = 1)
#' utils::read.csv(sheet)
#' @export
keywords_clean <- function(data,
                           columns = "DE",
                           level = c("confirmed", "all"),
                           decisions = NULL,
                           min_papers = 5,
                           replace = NULL,
                           titles = "TI",
                           output = c("minimal", "all")) {
  level <- match.arg(level)
  output <- match.arg(output)
  .kw_check_columns(data, columns)
  if (!(is.numeric(min_papers) && length(min_papers) == 1L && is.finite(min_papers) && min_papers >= 1)) {
    .bad_input("`min_papers` must be one number, 1 or more")
  }
  if (level == "confirmed" && !is.null(decisions)) {
    .bad_input("`decisions` is written only with `level = \"all\"`")
  }
  if (level == "all") {
    if (!(is.character(decisions) && length(decisions) == 1L && grepl("\\.(csv|xlsx)$", decisions, ignore.case = TRUE))) {
      .bad_input("`level = \"all\"` needs `decisions`, a new path ending in .csv or .xlsx")
    }
    .kr_sheet_file(decisions)
  }
  maps <- .kw_replacements(replace)
  if (level == "confirmed") return(.kw_run(data, columns, maps, character(), character(), output)$data)
  # level = "all": the sheet only; nothing is merged until keywords_merge().
  parsed <- lapply(columns, \(f) .kw_parse(data[[f]]))
  vocabulary <- .kw_vocabulary(parsed, maps, character(), character())
  title_text <- if (is.character(titles) && length(titles) == 1L && titles %in% names(data)) {
    as.character(data[[titles]])
  }
  .kw_write_sheet(parsed, vocabulary, title_text, min_papers, decisions)
  invisible(data)
}

#' Bring a checked keyword sheet back into the data
#'
#' @description
#' Applies the sheet that [keywords_clean()] wrote with `level = "all"`, after
#' it was checked outside R, and returns the data frame. The confirmed steps of
#' [keywords_clean()] are applied again, then the sheet, read one of two ways:
#'
#' * `by = "merge"` (default), for a person or a model that adds a column
#'   `merge` and answers `yes` or `no` in it:
#'   * a `needs_revision` row with `yes` becomes its `name`;
#'   * a `confirmed` row is merged unless `merge` says `no`; with `no` it keeps
#'     its own spelling;
#'   * an empty `merge` keeps what the row's `status` says.
#' * `by = "edit"`, for a tool that rewrites the keywords themselves, such as
#'   OpenRefine clustering the `keyword` column: every keyword whose `keyword`
#'   no longer equals its untouched `keyword_backup` becomes the new text.
#'   An edited `confirmed` row moves only that spelling; any other edited
#'   keyword moves with all its spellings. `merge` and `name` are ignored, and
#'   an edit of case or spacing alone changes nothing.
#'
#' Rows are matched on `keyword_backup` (on `keyword` when a sheet has no
#' backup column with `by = "merge"`). Pass the same `columns` and `replace` as to
#' [keywords_clean()], and the data as it was before cleaning: a confirmed
#' merge marked `no` can only be undone from there (on cleaned data everything
#' else gives the same result).
#'
#' @param data A data frame with one row per paper.
#' @param decisions The checked sheet, `.xlsx` or `.csv`.
#' @param by `"merge"` (default) reads the `merge` column; `"edit"` reads the
#'   edits of `keyword` against `keyword_backup`. See above.
#' @param columns,replace,output As in [keywords_clean()].
#'
#' @return `data`, same class, rows and columns, with the keyword columns
#'   rewritten where they change; with `output = "all"`, plus the
#'   `_ORIGINAL` columns. A message reports the counts per column and the
#'   rows applied from the sheet.
#'
#' @section Errors:
#' Raises `saqrmisc_bad_input` for a missing sheet; with `by = "merge"`, one
#' without `name`, `status`, `merge` and `keyword_backup` (or `keyword`), or a
#' `merge` other than empty, `yes` or `no`; with `by = "edit"`, one without
#' `keyword` and `keyword_backup`; and the errors of [keywords_clean()].
#'
#' @seealso [keywords_clean()]
#' @examples
#' papers <- data.frame(
#'   TI = c("Chatbots in class", "Bots that talk", "Fairness at work", "Unfair bosses"),
#'   DE = c("CHATBOT; NATURAL LANGUAGE PROCESSING (NLP)", "CHAT-BOTS; NLP",
#'          "INTERPERSONAL JUSTICE", "INTERPERSONAL INJUSTICE; NLP")
#' )
#' sheet <- tempfile(fileext = ".csv")
#' cleaned <- keywords_clean(papers, level = "all", decisions = sheet, min_papers = 1)
#' checked <- utils::read.csv(sheet)
#' checked$merge <- ifelse(checked$keyword == "NLP", "yes", "no")
#' utils::write.csv(checked, sheet, row.names = FALSE)
#' keywords_merge(papers, decisions = sheet)
#'
#' # The same sheet edited the way OpenRefine edits it: keyword rewritten,
#' # keyword_backup untouched.
#' edited <- utils::read.csv(sheet)
#' edited$keyword[edited$keyword == "NLP"] <- "NATURAL LANGUAGE PROCESSING"
#' utils::write.csv(edited, sheet, row.names = FALSE)
#' keywords_merge(papers, decisions = sheet, by = "edit")
#' @export
keywords_merge <- function(data, decisions, by = c("merge", "edit"), columns = "DE", replace = NULL,
                           output = c("minimal", "all")) {
  by <- match.arg(by)
  output <- match.arg(output)
  .kw_check_columns(data, columns)
  if (missing(decisions) || !(is.character(decisions) && length(decisions) == 1L)) {
    .bad_input("`decisions` must be the path of the sheet keywords_clean(level = \"all\") wrote")
  }
  sheet <- if (by == "merge") .kw_read_sheet(decisions) else .kw_read_edits(decisions)
  maps <- c(.kw_replacements(replace), if (length(sheet$moved) > 0L) list(sheet$moved))
  run <- .kw_run(data, columns, maps, sheet$merge, sheet$apart, output)
  if (by == "merge") {
    message(sprintf("keywords_merge: from %s, %d merge(s) and renames applied, %d confirmed merge(s) undone",
                    basename(decisions), length(sheet$merge), length(sheet$apart)))
  } else {
    targets <- unique(c(unname(sheet$merge), unname(sheet$moved)))
    message(sprintf("keywords_merge: from %s, %d edited keyword(s) applied, %d to a spelling not in the data",
                    basename(decisions), length(sheet$merge) + length(sheet$moved),
                    sum(!targets %in% run$vocabulary$stage)))
  }
  run$data
}

# The confirmed steps, then `merge` (keyword -> name, from a sheet) and
# `apart` (forms that keep their own spelling), over all columns at once.
.kw_run <- function(data, columns, maps, merge, apart, output) {
  # One vocabulary for all columns cleaned, so they agree on every spelling.
  parsed <- lapply(columns, \(f) .kw_parse(data[[f]]))
  vocabulary <- .kw_vocabulary(parsed, maps, merge, apart)
  cleaned <- data
  rewritten <- Map(\(f, p) .kw_rewrite(data[[f]], p, vocabulary), columns, parsed)
  cleaned[columns] <- lapply(rewritten, `[[`, "values")
  if (output == "all") {
    originals <- paste0(columns, "_ORIGINAL")
    fresh <- !originals %in% names(cleaned)
    cleaned[originals[fresh]] <- data[columns[fresh]]
  }
  .kw_report(columns, parsed, vocabulary, rewritten)
  list(data = cleaned, parsed = parsed, vocabulary = vocabulary)
}

.kw_report <- function(columns, parsed, vocabulary, rewritten) {
  lines <- vapply(seq_along(columns), \(i) {
    final <- vocabulary$final[match(parsed[[i]]$written, vocabulary$written)]
    sprintf("%s: %d written forms became %d keywords; %d papers changed",
            columns[i], length(final), length(unique(final)), rewritten[[i]]$changed)
  }, character(1))
  message(paste0("keywords: ", paste(lines, collapse = "; ")))
}

# ---- helpers ----------------------------------------------------------------

.kw_stopwords <- c("OF", "AND", "THE", "FOR", "IN", "ON", "TO", "A", "AN", "WITH", "BY")

.kw_bind <- function(tables) {
  out <- do.call(rbind, tables)
  rownames(out) <- NULL
  out
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
    # Invisible characters (zero-width spaces and joiners, BOM, soft hyphen)
    # go; other spaces (no-break, thin, ideographic) become a plain space.
    gsub(pattern = "\\p{Cf}", replacement = "", perl = TRUE) |>
    gsub(pattern = "[\u00a0\u2000-\u200a\u202f\u205f\u3000]", replacement = " ") |>
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
  out <- data.frame(term = gsub("\\s+", " ", x), short = rep(NA_character_, length(x)),
                    long = rep(NA_character_, length(x)), stringsAsFactors = FALSE)
  # Only terms with a bracket need the per-term work below.
  hit <- !is.na(x) & grepl("[()]", x)
  if (any(hit)) {
    parts <- .kw_bracket_parts(x[hit])
    out$term[hit] <- parts$term
    out$short[hit] <- parts$short
    out$long[hit] <- parts$long
  }
  out
}

.kw_bracket_parts <- function(x) {
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
  x <- as.character(x)
  # Vectorised over every word of every term at once: a loop over a million
  # keywords is slow.
  words <- strsplit(gsub("['`\u2019]", "", toupper(x)), "[^[:alnum:]]+")
  flat <- unlist(words)
  term <- rep(seq_along(words), lengths(words))
  keep <- nzchar(flat)
  flat <- flat[keep]
  term <- term[keep]
  folded <- iconv(flat, "UTF-8", "ASCII//TRANSLIT")
  ok <- !is.na(folded) & !grepl("?", folded, fixed = TRUE)
  flat[ok] <- gsub("[^A-Z0-9]", "", toupper(folded[ok]))
  flat <- .kw_singular(flat)
  # paste0("\r", character()) would give "\r", one element for none.
  key <- if (length(x) > 0L) paste0("\r", x) else character()
  has_words <- unique(term)
  key[has_words] <- vapply(split(flat, factor(term, levels = has_words)), paste, character(1), collapse = "")
  key
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

.kw_read_table <- function(path, argument = "replace") {
  if (!file.exists(path)) .bad_input(sprintf("`%s` file not found: %s", argument, path))
  type <- tolower(sub(".*\\.([^.]+)$", "\\1", basename(path)))
  if (identical(type, "csv")) return(.kr_read_csv(path))
  if (!type %in% c("xlsx", "xls")) {
    .bad_input(sprintf("`%s` file must be .csv, .xlsx or .xls: %s", argument, path))
  }
  if (!requireNamespace("readxl", quietly = TRUE)) {
    stop(errorCondition(
      sprintf("reading %s needs the readxl package: install.packages(\"readxl\")", basename(path)),
      class = "saqrmisc_missing_package", call = NULL))
  }
  as.data.frame(readxl::read_excel(path, col_types = "text"))
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

# ---- vocabulary ---------------------------------------------------------------

# One column's keywords: each distinct written form once (`written`, with
# its number of `papers`), and every use as (`paper`, `w` = index into
# `written`).
.kw_parse <- function(values) {
  values <- as.character(values)
  values[is.na(values)] <- ""
  parts <- strsplit(values, ";", fixed = TRUE)
  flat <- trimws(unlist(parts))
  paper <- rep(seq_along(parts), lengths(parts))
  keep <- nzchar(flat)
  flat <- flat[keep]
  paper <- paper[keep]
  written <- unique(flat)
  w <- match(flat, written)
  once <- !duplicated(paper * (length(written) + 1) + w)
  list(written = written, papers = tabulate(w[once], nbins = length(written)), paper = paper, w = w)
}

# The written forms of every column, each with its `final` form. `merge`
# (keyword -> name, from a checked sheet) is applied after the confirmed
# merge, which is then repeated so every spelling of a keyword follows it;
# forms in `apart` keep their own spelling. `stage` (after cleanup and
# brackets) and `mapped` (after `replace`) feed the sheet, the bracket parts
# (`short`, `long`) its acronym candidates.
.kw_vocabulary <- function(parsed, maps, merge, apart) {
  written <- unique(unlist(lapply(parsed, `[[`, "written")))
  papers <- Reduce(`+`, lapply(parsed, \(p) {
    counts <- numeric(length(written))
    counts[match(p$written, written)] <- p$papers
    counts
  }), numeric(length(written)))

  bracketed <- .kw_brackets(.kw_normalise(written))
  stage <- bracketed$term
  mapped <- .kw_apply_maps(stage, maps)
  merged <- .kw_merge_by_key(mapped, papers, .kw_targets(maps))
  if (length(merge) > 0L) {
    merged$form <- .kw_apply_maps(merged$form, list(merge))
    merged <- .kw_merge_by_key(merged$form, papers, unique(unname(merge)))
  }
  form <- merged$form
  held <- stage %in% apart
  form[held] <- stage[held]
  # Nothing is deleted: a form cleanup leaves empty keeps its written text.
  final <- ifelse(nzchar(form), form, written)
  list(written = written, papers = papers, final = final, stage = stage, mapped = mapped,
       short = bracketed$short, long = bracketed$long)
}

# A table's NEW terms, as later tables leave them, are the preferred forms.
.kw_targets <- function(maps) {
  unique(unlist(lapply(seq_along(maps), \(i) .kw_apply_maps(unname(maps[[i]]), maps[-seq_len(i)]))))
}

# Forms with one key become the key's best form: a target, else the form in
# most papers, then the shortest, then the first alphabetically. Returns the
# merged form and the key of each input.
.kw_merge_by_key <- function(forms, papers, targets) {
  distinct <- unique(forms)
  at <- match(forms, distinct)
  table <- data.frame(form = distinct, key = .kw_key(distinct),
                      papers = as.vector(rowsum(papers, at, reorder = TRUE)),
                      target = distinct %in% targets, stringsAsFactors = FALSE)
  best <- table[order(table$key, !table$target, -table$papers, nchar(table$form), table$form,
                      method = "radix"), ]
  best <- best[!duplicated(best$key), ]
  key <- table$key[at]
  list(form = best$form[match(key, best$key)], key = key)
}

# The column with the changed papers rebuilt from their final keywords, each
# once, "; "-separated; papers with nothing to change keep their text.
.kw_rewrite <- function(values, parsed, vocabulary) {
  final_of_written <- vocabulary$final[match(parsed$written, vocabulary$written)]
  final <- final_of_written[parsed$w]
  paper <- parsed$paper
  term_id <- match(final, unique(final))
  repeated <- !is.na(final) & duplicated(ifelse(is.na(final), -seq_along(final),
                                                paper * (length(term_id) + 1) + term_id))
  changed_use <- is.na(final) | final != parsed$written[parsed$w] | repeated
  changed <- unique(paper[changed_use])
  if (length(changed) == 0L) return(list(values = values, changed = 0L))
  keep <- !is.na(final) & !repeated & paper %in% changed
  text <- vapply(split(final[keep], factor(paper[keep], levels = changed)), paste, character(1),
                 collapse = "; ")
  text[!nzchar(text)] <- NA_character_
  values[changed] <- unname(text)
  list(values = values, changed = length(changed))
}

# ---- decisions sheet ----------------------------------------------------------

# A checked sheet as `merge` (keyword -> name: needs_revision rows with
# `yes`, and renamed groups) and `apart` (confirmed forms marked `no`).
.kw_read_sheet <- function(path, term = "keyword") {
  sheet <- .kw_read_table(path, argument = "decisions")
  backup <- paste0(term, "_backup")
  matched_on <- if (backup %in% names(sheet)) backup else term
  absent <- setdiff(c("name", "status", "merge", matched_on), names(sheet))
  if (length(absent) > 0L) {
    .bad_input(sprintf("the sheet has no column(s) %s; use the sheet that level = \"all\" wrote",
                       paste(absent, collapse = ", ")))
  }
  answer <- tolower(trimws(as.character(sheet$merge)))
  answer[is.na(answer)] <- ""
  odd <- setdiff(answer, c("", "yes", "no"))
  if (length(odd) > 0L) {
    .bad_input(sprintf("`merge` must be empty, yes or no, not %s", paste(sprintf("\"%s\"", odd), collapse = ", ")))
  }
  keyword <- as.character(sheet[[matched_on]])
  name <- as.character(sheet$name)
  status <- tolower(trimws(as.character(sheet$status)))
  status[is.na(status)] <- ""
  take <- !is.na(keyword) & !is.na(name) &
    ((status == "needs_revision" & answer == "yes") | (status == "name" & keyword != name))
  merge <- if (any(take)) {
    .kw_table_map(data.frame(OLD = keyword[take], NEW = name[take]))
  } else {
    stats::setNames(character(), character())
  }
  undone <- keyword[status == "confirmed" & answer == "no" & !is.na(keyword)]
  list(merge = merge, apart = unique(.kw_brackets(.kw_normalise(undone))$term), moved = character())
}

# An edited sheet (keyword rewritten, keyword_backup untouched, as OpenRefine
# leaves it) as maps backup -> keyword: `moved` for confirmed rows, applied
# before the spelling merge so only that spelling moves; `merge` for every
# other row, applied after it so all spellings of the keyword move.
.kw_read_edits <- function(path, term = "keyword") {
  sheet <- .kw_read_table(path, argument = "decisions")
  backup <- paste0(term, "_backup")
  absent <- setdiff(c(term, backup), names(sheet))
  if (length(absent) > 0L) {
    .bad_input(sprintf("`by = \"edit\"` needs the column(s) %s, as level = \"all\" writes them",
                       paste(absent, collapse = ", ")))
  }
  new <- trimws(as.character(sheet[[term]]))
  old <- trimws(as.character(sheet[[backup]]))
  status <- if ("status" %in% names(sheet)) tolower(trimws(as.character(sheet$status))) else rep("", nrow(sheet))
  status[is.na(status)] <- ""
  edited <- !is.na(new) & !is.na(old) & nzchar(new) & new != old
  as_map <- \(rows) if (any(rows)) .kw_table_map(data.frame(OLD = old[rows], NEW = new[rows]))
                    else stats::setNames(character(), character())
  list(merge = as_map(edited & status != "confirmed"), moved = as_map(edited & status == "confirmed"),
       apart = character())
}

# Writes every merge to the sheet, one group per name: the name row, its
# confirmed merges and its candidates (status needs_revision), none of them
# applied yet, for keywords in `min_papers` or more papers (counted over all
# columns); and the prompt beside it.
.kw_write_sheet <- function(parsed, vocabulary, titles, min_papers, path) {
  terms <- unique(vocabulary$final)
  term_of_written <- match(vocabulary$final, terms)
  # Integer codes throughout: (paper, term) for every use in every column.
  use_term <- unlist(lapply(parsed, \(p) term_of_written[match(p$written, vocabulary$written)][p$w]))
  use_paper <- unlist(lapply(parsed, `[[`, "paper"))
  once <- !duplicated(use_paper * (length(terms) + 1) + use_term)
  papers <- tabulate(use_term[once], nbins = length(terms))
  first_paper <- use_paper[once][match(seq_along(terms), use_term[once])]

  # Candidates: keywords spelled alike, and acronyms with their long forms.
  eligible <- which(papers >= min_papers)
  candidates <- terms[eligible]
  acronym <- .kw_acronym_pairs(vocabulary, candidates, papers[eligible])
  acronym$source <- rep("acronym", nrow(acronym))
  found <- .kw_candidates(candidates, papers[eligible], acronym)
  group <- found$group
  source <- found$source
  in_group <- group %in% group[duplicated(group)]
  member <- which(in_group)
  proposed <- data.frame(keyword = candidates[member], name = candidates[group[member]],
                         status = ifelse(member == group[member], "name", "needs_revision"),
                         papers = papers[eligible][member],
                         source = ifelse(member == group[member], NA_character_, source[member]),
                         stringsAsFactors = FALSE)

  # Confirmed: each cleaned form that a confirmed step gave another name.
  moved <- vocabulary$stage != vocabulary$final & nzchar(vocabulary$stage)
  pair <- paste(vocabulary$stage, vocabulary$final, sep = "\r")[moved]
  first <- !duplicated(pair)
  done <- data.frame(keyword = vocabulary$stage[moved][first], name = vocabulary$final[moved][first],
                     status = rep("confirmed", sum(first)),
                     papers = as.vector(rowsum(vocabulary$papers[moved], match(pair, unique(pair)), reorder = TRUE)),
                     source = ifelse(vocabulary$mapped[moved][first] != vocabulary$stage[moved][first],
                                     "your table", "same spelling"),
                     stringsAsFactors = FALSE)
  done <- done[papers[match(done$name, terms)] >= min_papers, ]
  heads <- setdiff(unique(done$name), proposed$keyword[proposed$status == "name"])
  named <- data.frame(keyword = heads, name = heads, status = rep("name", length(heads)),
                      papers = papers[match(heads, terms)], source = rep(NA_character_, length(heads)),
                      stringsAsFactors = FALSE)

  # Every other keyword in min_papers or more papers, alone, so a tool such as
  # OpenRefine can cluster the whole vocabulary.
  alone <- setdiff(candidates, c(proposed$keyword, done$name, done$keyword))
  single <- data.frame(keyword = alone, name = alone, status = rep("single", length(alone)),
                       papers = papers[match(alone, terms)], source = rep(NA_character_, length(alone)),
                       stringsAsFactors = FALSE)
  # No `name` rows: the name is in the `name` column of every row of its group.
  rows <- rbind(proposed, done, named, single)
  rows <- rows[rows$status != "name", ]
  rank <- match(rows$status, c("name", "single", "confirmed", "needs_revision"))
  name_papers <- papers[match(rows$name, terms)]
  rows <- rows[order(-name_papers, rows$name, rank, -rows$papers, rows$keyword, method = "radix"), ]
  shown <- rows$status != "confirmed"
  example <- rep(NA_character_, nrow(rows))
  if (!is.null(titles)) example[shown] <- titles[first_paper[match(rows$keyword[shown], terms)]]
  sheet <- data.frame(group = match(rows$name, unique(rows$name)), name = rows$name, keyword = rows$keyword,
                      keyword_backup = rows$keyword, status = rows$status,
                      papers = rows$papers, source = rows$source, example = .kr_clip(example, 150L),
                      stringsAsFactors = FALSE)
  rownames(sheet) <- NULL
  prompt <- .kw_prompt()
  prompt_file <- sub("\\.(csv|xlsx)$", "_prompt.txt", path, ignore.case = TRUE)
  writeLines(enc2utf8(prompt), prompt_file, useBytes = TRUE)
  if (grepl("\\.csv$", path, ignore.case = TRUE)) {
    .kr_write_csv(sheet, path)
  } else {
    writexl::write_xlsx(list(decisions = sheet, prompt = data.frame(prompt = strsplit(prompt, "\n")[[1L]])), path)
  }
  message(sprintf(paste0("%s: %d groups and single keywords, %d confirmed merges and %d to check, keywords in %s or more ",
                         "papers. Nothing is merged yet: add a column `merge` (yes or no), then ",
                         "keywords_merge(data, decisions = \"%s\"). A prompt for a model is in %s."),
                  basename(path), length(unique(sheet$group)), sum(sheet$status == "confirmed"),
                  sum(sheet$status == "needs_revision"), format(min_papers), basename(path), basename(prompt_file)))
}

# The instructions for a model (or a person) checking the sheet.
.kw_prompt <- function() {
  paste(
    "Review keyword merges for a bibliometric study.",
    "",
    "Each row proposes merging `keyword` into `name`. `example` is the title of a paper using `keyword`.",
    "",
    "Add a column `merge` and write yes or no on every row:",
    "- yes: same concept (spelling variant, plural, translation, abbreviation, or acronym).",
    "- no: different, broader, narrower, opposite, or uncertain meaning.",
    "",
    "By `status`:",
    "- needs_revision: yes or no.",
    "- confirmed: yes, unless the merge is incorrect (no).",
    "- single: no (nothing to merge).",
    "",
    "Change no other column.",
    sep = "\n")
}

# ---- candidates ---------------------------------------------------------------

# Pairs of terms spelled alike: Jaccard similarity of their character
# trigrams at least `threshold`. MinHash with banding (Broder 1997; Leskovec
# et al. 2020, ch. 3) proposes the pairs, so all pairs are never compared;
# each proposed pair is then checked exactly. With 15 bands of 4 rows a pair
# at Jaccard 0.7 is proposed with probability 0.98. The hash functions come
# from a fixed seed, so the result is the same on every run.
.kw_minhash_pairs <- function(terms, threshold = 0.7, n_bands = 15L, band_rows = 4L, max_bucket = 50L) {
  none <- data.frame(a = integer(), b = integer(), jaccard = numeric())
  n <- length(terms)
  if (n < 2L) return(none)
  grams <- .kw_trigrams(terms)
  term_of <- grams$term
  code <- grams$code
  sizes <- tabulate(term_of, nbins = n)
  prime <- 2147483647
  n_hash <- n_bands * band_rows
  params <- .kr_with_seed(20261114L, list(a = sample.int(prime - 1L, n_hash), b = sample.int(prime - 1L, n_hash)))
  signature <- vapply(seq_len(n_hash), \(h) {
    hashed <- (as.numeric(params$a[h]) * code + params$b[h]) %% prime
    by_term <- order(term_of, hashed, method = "radix")
    hashed[by_term][!duplicated(term_of[by_term])]
  }, numeric(n))

  pairs <- do.call(rbind, lapply(seq_len(n_bands), \(band) {
    cols <- (band - 1L) * band_rows + seq_len(band_rows)
    bucket <- do.call(paste, c(lapply(cols, \(j) signature[, j]), sep = ","))
    members <- split(seq_len(n), bucket)
    members <- members[lengths(members) >= 2L & lengths(members) <= max_bucket]
    if (length(members) == 0L) return(NULL)
    do.call(rbind, lapply(members, \(m) t(utils::combn(m, 2L))))
  }))
  if (is.null(pairs)) return(none)
  pairs <- unique(data.frame(a = pmin(pairs[, 1L], pairs[, 2L]), b = pmax(pairs[, 1L], pairs[, 2L])))

  jaccard <- .kw_jaccard(grams, pairs$a, pairs$b)
  kept <- cbind(pairs, jaccard = jaccard)[jaccard >= threshold, ]
  rownames(kept) <- NULL
  kept
}

# Exact Jaccard similarity of the trigrams of terms a and b (indices into the
# terms of `grams`), vectorised: each trigram of a is looked up among b's.
.kw_jaccard <- function(grams, a, b) {
  if (length(a) == 0L) return(numeric())
  term_of <- grams$term
  code <- grams$code
  sizes <- tabulate(term_of, nbins = max(term_of, a, b))
  offset <- c(0L, cumsum(sizes))
  per_pair <- sizes[a]
  pair_of <- rep(seq_along(a), per_pair)
  gram_of_a <- code[rep(offset[a], per_pair) + sequence(per_pair)]
  width <- max(code) + 1
  in_b <- (b[pair_of] * width + gram_of_a) %in% (term_of * width + code)
  shared <- tabulate(pair_of[in_b], nbins = length(a))
  shared / (sizes[a] + sizes[b] - shared)
}

# Each term's distinct character trigrams as integer codes, grouped by term
# in order. A term shorter than three characters is one "trigram" itself.
.kw_trigrams <- function(terms) {
  count <- pmax(nchar(terms) - 2L, 1L)
  term <- rep(seq_along(terms), count)
  start <- sequence(count)
  gram <- substr(rep(terms, count), start, start + 2L)
  code <- match(gram, unique(gram))
  distinct <- !duplicated(term * (max(code) + 1) + code)
  list(term = term[distinct], code = code[distinct])
}

# Candidate groups among `candidates`, from every source: each keyword's
# group root (`group`, an index) and the ways it was paired (`source`).
# `acronym` holds pairs (a acronym, b long form) found beforehand. A `fixed`
# term (an institution OpenAlex identified) is never paired with another
# fixed one, and is always the name of its group.
.kw_candidates <- function(candidates, papers, acronym, fixed = rep(FALSE, length(candidates))) {
  similar <- .kw_similar_pairs(candidates, papers)
  initials <- .kw_initials_pairs(candidates, papers)
  acronym <- rbind(acronym, initials[!paste(initials$a, initials$b) %in% paste(acronym$a, acronym$b), ])
  similar <- similar[!(fixed[similar$a] & fixed[similar$b]), ]
  acronym <- acronym[!fixed[acronym$a], ]
  # Grouping points each term to one in more papers; a fixed term outranks
  # every other, so it is the root.
  rank <- papers + fixed * (max(c(papers, 0)) + 1)
  group <- .kw_groups(similar, acronym, rank)
  links <- rbind(similar[c("a", "b", "source")], acronym[c("a", "b", "source")])
  labels <- strsplit(c(links$source, links$source), "+", fixed = TRUE)
  holder <- rep(c(links$a, links$b), lengths(labels))
  source <- vapply(split(as.character(unlist(labels)), factor(holder, levels = seq_along(candidates))),
                   \(x) paste(sort(unique(x)), collapse = "+"), character(1), USE.NAMES = FALSE)
  list(group = group, source = source)
}

# Every pair of keywords alike in spelling: MinHash on trigrams (`spelling`),
# and keys two keywords share, as OpenRefine clusters (key collision): the
# words in any order (`word order`), the letter pairs (`ngram`) and the
# word stems (`stem`, Porter; needs SnowballC). A key links each keyword to the one with
# most papers sharing it. Returns a, b, the trigram Jaccard and the sources.
.kw_similar_pairs <- function(candidates, papers) {
  spelling <- .kw_minhash_pairs(candidates)
  spelling$source <- rep("spelling", nrow(spelling))
  keys <- list("word order" = .kw_key_word_set(candidates), "ngram" = .kw_key_bigrams(candidates),
               "stem" = .kw_key_by_word(candidates, .kw_stem, "SnowballC"))
  keyed <- do.call(rbind, Map(\(key, label) .kw_key_pairs(key, papers, label), Filter(Negate(is.null), keys),
                              names(Filter(Negate(is.null), keys))))
  pairs <- rbind(spelling[c("a", "b", "source")], keyed)
  if (nrow(pairs) == 0L) return(data.frame(a = integer(), b = integer(), jaccard = numeric(), source = character()))
  low <- pmin(pairs$a, pairs$b)
  high <- pmax(pairs$a, pairs$b)
  pair <- match(paste(low, high), unique(paste(low, high)))
  first <- !duplicated(pair)
  out <- data.frame(a = low[first], b = high[first], stringsAsFactors = FALSE)
  out$source <- vapply(split(pairs$source, factor(pair, levels = seq_len(nrow(out)))),
                       \(x) paste(unique(x), collapse = "+"), character(1), USE.NAMES = FALSE)
  out$jaccard <- .kw_jaccard(.kw_trigrams(candidates), out$a, out$b)
  out
}

# Star pairs from a key: each keyword with the keyword in most papers that
# shares its key. Keywords with no key (NA) are left out.
.kw_key_pairs <- function(key, papers, label) {
  by_key <- order(key, -papers, seq_along(key), method = "radix")
  heads <- by_key[!duplicated(key[by_key])]
  head_of <- heads[match(key, key[heads])]
  member <- which(!is.na(key) & head_of != seq_along(key))
  data.frame(a = member, b = head_of[member], source = rep(label, length(member)), stringsAsFactors = FALSE)
}

.kw_words <- function(terms) {
  lapply(strsplit(terms, "[^\\p{L}\\p{N}]+", perl = TRUE), \(w) w[nzchar(w)])
}

# The distinct words, sorted: PSYCHOLOGY, CLINICAL = CLINICAL PSYCHOLOGY.
.kw_key_word_set <- function(terms) {
  vapply(.kw_words(terms), \(w) paste(sort(unique(w), method = "radix"), collapse = " "), character(1))
}

# The distinct letter pairs, sorted (OpenRefine's 2-gram fingerprint), for
# keywords of five or more letters; shorter ones (acronyms) have no key.
.kw_key_bigrams <- function(terms) {
  letters_only <- gsub("[^\\p{L}\\p{N}]", "", terms, perl = TRUE)
  n <- nchar(letters_only)
  vapply(seq_along(terms), \(i) {
    if (n[i] < 5L) return(NA_character_)
    pairs <- substring(letters_only[i], seq_len(n[i] - 1L), seq_len(n[i] - 1L) + 1L)
    paste(sort(unique(pairs), method = "radix"), collapse = "")
  }, character(1))
}

# Each Latin word of four or more letters replaced by `transform` (a stem), in word order; other words stay as written. NULL, with a message,
# when the package `transform` needs is missing.
.kw_key_by_word <- function(terms, transform, package) {
  if (!requireNamespace(package, quietly = TRUE)) {
    message(sprintf("keywords: the %s package is not installed, so no `stem` candidates; install.packages(\"%s\")",
                    package, package))
    return(NULL)
  }
  words <- .kw_words(terms)
  flat <- unlist(words)
  latin <- grepl("^[A-Z]{4,}$", flat)
  distinct <- unique(flat[latin])
  flat[latin] <- transform(distinct)[match(flat[latin], distinct)]
  holder <- factor(rep(seq_along(terms), lengths(words)), levels = seq_along(terms))
  key <- vapply(split(flat, holder), paste, character(1), collapse = " ", USE.NAMES = FALSE)
  key[!nzchar(key)] <- NA_character_
  key
}

.kw_stem <- function(words) toupper(SnowballC::wordStem(tolower(words), language = "english"))

# Pairs (acronym, long form) by initials: a one-word keyword whose letters
# are the initials of a keyword of several words (NLP, NATURAL LANGUAGE
# PROCESSING), even when no paper writes them together; up to three long
# forms per acronym, most papers first.
.kw_initials_pairs <- function(candidates, papers) {
  none <- data.frame(a = integer(), b = integer(), source = character())
  letters_only <- gsub("[^A-Z0-9]", "", candidates)
  short <- which(!grepl(" ", candidates) & nchar(letters_only) >= 2L & nchar(letters_only) <= 8L &
                   grepl("[A-Z]", letters_only))
  long <- which(grepl(" ", candidates))
  if (length(short) == 0L || length(long) == 0L) return(none)
  spelled <- lapply(candidates[long], .kw_spellings)
  spellings <- data.frame(b = rep(long, lengths(spelled)), spelling = unlist(spelled))
  wanted <- lapply(candidates[short], .kw_acronym_letters)
  acronyms <- data.frame(a = rep(short, lengths(wanted)), spelling = unlist(wanted))
  pairs <- unique(merge(acronyms, spellings, by = "spelling")[c("a", "b")])
  pairs <- pairs[nchar(letters_only[pairs$a]) < nchar(letters_only[pairs$b]), ]
  if (nrow(pairs) == 0L) return(none)
  pairs <- pairs[order(pairs$a, -papers[pairs$b], pairs$b, method = "radix"), ]
  pairs <- pairs[stats::ave(pairs$a, pairs$a, FUN = seq_along) <= 3L, ]
  data.frame(a = pairs$a, b = pairs$b, source = rep("initials", nrow(pairs)))
}

# Pairs (acronym, long form) among the candidates: a keyword that is an
# acronym written in brackets after a long form elsewhere in the data
# (`NATURAL LANGUAGE PROCESSING (NLP)` makes `NLP` a candidate for
# `NATURAL LANGUAGE PROCESSING`), up to three long forms per acronym, most
# papers first.
.kw_acronym_pairs <- function(vocabulary, candidates, papers) {
  none <- data.frame(a = integer(), b = integer())
  at <- which(!is.na(vocabulary$short) & !is.na(vocabulary$final))
  if (length(at) == 0L) return(none)
  defined <- unique(data.frame(short = vocabulary$short[at], long = vocabulary$final[at], stringsAsFactors = FALSE))
  candidate_key <- .kw_key(candidates)
  a <- match(.kw_key(defined$short), candidate_key)
  b <- match(defined$long, candidates)
  pairs <- unique(data.frame(a = a, b = b)[!is.na(a) & !is.na(b) & a != b, ])
  if (nrow(pairs) == 0L) return(none)
  pairs <- pairs[order(pairs$a, -papers[pairs$b], pairs$b, method = "radix"), ]
  pairs <- pairs[stats::ave(pairs$a, pairs$a, FUN = seq_along) <= 3L, ]
  rownames(pairs) <- NULL
  pairs
}

# Groups as small stars around the most used form, never as chains: each
# keyword points to its most similar neighbour in more papers (an acronym to
# its long form, first), and a group is everything that leads to one root.
# Linking all similar pairs instead chains unrelated keywords into one group
# (REPRESENTATION ... SERIAL REACTION TIME TASK on 2 million papers).
.kw_groups <- function(spelling, acronym, papers) {
  n <- length(papers)
  # Spelling links point to a keyword in more papers (ties: the earlier one),
  # never into an acronym; an acronym points to its long form. So no path
  # returns to where it started, and every path ends at a root.
  more <- papers[spelling$b] > papers[spelling$a] |
    (papers[spelling$b] == papers[spelling$a] & spelling$b < spelling$a)
  links <- data.frame(from = c(acronym$a, ifelse(more, spelling$a, spelling$b)),
                      to = c(acronym$b, ifelse(more, spelling$b, spelling$a)),
                      score = c(rep(2, nrow(acronym)), spelling$jaccard))
  links <- links[!(links$score < 2 & links$to %in% acronym$a), ]
  best <- links[order(links$from, -links$score, -papers[links$to], links$to, method = "radix"), ]
  best <- best[!duplicated(best$from), ]
  parent <- seq_len(n)
  parent[best$from] <- best$to
  # Pointer jumping: the path to the root halves each pass, so at most
  # log2(n) + 1 passes are needed.
  for (pass in seq_len(ceiling(log2(n + 1)) + 1L)) {
    jumped <- parent[parent]
    if (identical(jumped, parent)) break
    parent <- jumped
  }
  parent
}
