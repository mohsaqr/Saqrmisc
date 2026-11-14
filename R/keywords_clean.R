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
#' 1. **Cleanup**: HTML entities decoded, invisible characters (zero-width
#'    spaces and joiners, soft hyphens) removed, dashes and quotes unified,
#'    quotes around the whole keyword dropped, `&` written `AND`, a leading
#'    `#` and trailing punctuation (also `*`) dropped, spacing squeezed, upper
#'    case. A keyword cleanup would leave empty (`.`) keeps its written form.
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
#' the `decisions` sheet, for a person, a model or a tool such as OpenRefine
#' to check outside R, with a short prompt for a model beside it. It has
#' three tables:
#' * `decisions`: one question per row, should `keyword` be merged into
#'   `merge_into`? A candidate is a keyword spelled like another (character
#'   trigrams, Jaccard 0.7 or more, found by MinHash, never by comparing all
#'   pairs) or an acronym whose long form is written in brackets elsewhere in
#'   the data. Spelled alike is not the same concept, so a pair whose first
#'   letters differ (`DEPRESSION` / `REPRESSION`) or with a word that is
#'   another plus a prefix (`ADOLESCENTS` / `PREADOLESCENTS`, `SOCIAL
#'   JUSTICE` / `SOCIAL INJUSTICE`) is never proposed, and the rest wait for
#'   a `yes`.
#' * `confirmed`: each keyword with the spellings the confirmed steps merge
#'   into it, in one cell (`DEPRESSIONS (6); DÉPRESSION (9)`), with `yes` in
#'   `merge`; change it to `no` to keep them apart.
#' * `all_keywords`: every keyword, for a tool such as OpenRefine to cluster
#'   the whole list itself.
#'
#' Only keywords in at least `min_papers` papers are in the sheet. Bring it
#' back with [keywords_merge()]: by its `merge` answers, or by the edits of
#' `keyword` against the untouched `keyword_backup`.
#'
#' @param data A data frame with one row per paper, such as a bibliometrix
#'   data frame. The cleaners chain: `M |> authors_clean(ids = "scopus_exports")
#'   |> keywords_clean()`.
#' @param columns Names of the keyword columns, `;`-separated. Default `"DE"`
#'   (author keywords).
#' @param level `"confirmed"` (default) or `"all"`; see above.
#' @param decisions With `level = "all"`: the sheet to write, a new path
#'   ending in `.xlsx` or `.csv` (an existing file is never replaced). An
#'   `.xlsx` has the tabs `decisions`, `confirmed`, `all_keywords` and
#'   `prompt`; a `.csv` is the decisions table, with
#'   `<sheet>_confirmed.csv` and `<sheet>_all_keywords.csv` beside it. The
#'   prompt also goes to `<sheet>_prompt.txt`. Columns of `decisions`:
#'   `keyword`, `keyword_backup` (a copy that [keywords_merge()] matches on,
#'   so an edit of `keyword` does no harm), `merge_into`, `merge` (empty, for
#'   `yes` or `no`), `papers`, `merge_into_papers`, `source` (`spelling`,
#'   `acronym`) and `example` (the title of a paper using the keyword). Of
#'   `confirmed`: `keyword`, `papers`, `also_written_as`, `source` (`same
#'   spelling`, `your table`) and `merge`. Of `all_keywords`: `keyword`,
#'   `keyword_backup`, `papers` and `example`.
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
#'   `level = "all"`: `data` unchanged (the sheet is the result). A message reports, per column, the written
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
  data <- .papers_of(data)
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
    taken <- Filter(file.exists, if (grepl("\\.csv$", decisions, ignore.case = TRUE)) {
      vapply(.kw_parts[-1L], \(part) .kw_part_file(decisions, part), character(1))
    })
    if (length(taken) > 0L) {
      .bad_input(sprintf("%s already exists and may hold your decisions; delete or choose another name",
                         paste(taken, collapse = " and ")))
    }
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
  data
}

#' Bring a checked keyword sheet back into the data
#'
#' @description
#' Applies the sheet that [keywords_clean()] wrote with `level = "all"`, after
#' it was checked outside R, and returns the data frame. The confirmed steps of
#' [keywords_clean()] are applied again, then the sheet, read one of two ways:
#'
#' * `by = "merge"` (default), for a person or a model that answers `yes` or
#'   `no` in `merge` (or in the column `answers` names): a `decisions` row
#'   with `yes` merges its keyword, with all its spellings, into `merge_into`
#'   (edit `merge_into` to send it elsewhere); a row left empty is not merged
#'   and is counted in the message. The `confirmed` table is written with
#'   `yes` in every row; a `no` there keeps the keyword's other spellings
#'   apart.
#' * `by = "edit"`, for a tool that rewrites the keywords themselves, such as
#'   OpenRefine clustering the `keyword` column of `all_keywords` (or of
#'   `decisions`): every keyword whose `keyword` no longer equals its
#'   untouched `keyword_backup` becomes the new text, with all its spellings.
#'   `merge` is ignored, and an edit of case or spacing alone changes nothing.
#'
#' Pass the same `columns` and `replace` as to [keywords_clean()], and the
#' data as it was before cleaning: a confirmed merge marked `no` can only be
#' undone from there (on cleaned data everything else gives the same result).
#'
#' @param data A data frame with one row per paper.
#' @param decisions The checked sheet, `.xlsx` or `.csv` (the decisions
#'   table; the other tables are read from beside it when they are there).
#' @param by `"merge"` (default) reads the `merge` column; `"edit"` reads the
#'   edits of `keyword` against `keyword_backup`. See above.
#' @param answers With `by = "merge"`: the column of the decisions table
#'   holding the yes / no answers. Default `"merge"`; name another when an
#'   application writes its answers to a column of its own.
#' @param columns,replace,output As in [keywords_clean()].
#'
#' @return `data`, same class, rows and columns, with the keyword columns
#'   rewritten where they change; with `output = "all"`, plus the
#'   `_ORIGINAL` columns. A message reports the counts per column and the
#'   rows applied from the sheet.
#'
#' @section Errors:
#' Raises `saqrmisc_bad_input` for a missing sheet; with `by = "merge"`, a
#' decisions table without `merge_into`, the `answers` column and
#' `keyword_backup` (or `keyword`), or an answer other than empty, `yes` or
#' `no`; with
#' `by = "edit"`, no table with `keyword` and `keyword_backup`; and the
#' errors of [keywords_clean()].
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
keywords_merge <- function(data, decisions, by = c("merge", "edit"), answers = "merge", columns = "DE",
                           replace = NULL, output = c("minimal", "all")) {
  data <- .papers_of(data)
  by <- match.arg(by)
  output <- match.arg(output)
  .kw_check_columns(data, columns)
  if (missing(decisions) || !(is.character(decisions) && length(decisions) == 1L)) {
    .bad_input("`decisions` must be the path of the sheet keywords_clean(level = \"all\") wrote")
  }
  if (!(is.character(answers) && length(answers) == 1L && !is.na(answers) && nzchar(answers))) {
    .bad_input("`answers` must name the column holding yes or no")
  }
  sheet <- if (by == "merge") .kw_read_sheet(decisions, answers) else .kw_read_edits(decisions)
  run <- .kw_run(data, columns, .kw_replacements(replace), sheet$merge, sheet$apart, output)
  if (by == "merge") {
    message(sprintf("keywords_merge: from %s, %d merge(s) applied, %d confirmed keyword(s) kept apart, %d row(s) unanswered (not merged)",
                    basename(decisions), length(sheet$merge), length(sheet$apart), sheet$unanswered))
  } else {
    message(sprintf("keywords_merge: from %s, %d edited keyword(s) applied, %d to a spelling not in the data",
                    basename(decisions), length(sheet$merge),
                    sum(!unique(unname(sheet$merge)) %in% run$vocabulary$stage)))
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

# The papers a cleaner works on: a data frame, or the cleaned data of an
# earlier clean_*() result, so that the cleaners chain with |>.
.papers_of <- function(data) {
  if (inherits(data, "saqr_institutions")) data$data else data
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
    # Invisible format characters (zero-width spaces and joiners, BOM, soft
    # hyphen): they make a keyword look identical to another yet differ.
    gsub(pattern = "\\p{Cf}", replacement = "", perl = TRUE) |>
    gsub(pattern = "[\u2010-\u2015\u2212]", replacement = "-") |>
    gsub(pattern = "[\u2018\u2019]", replacement = "'") |>
    gsub(pattern = "[\"`\u201c\u201d]", replacement = "") |>
    gsub(pattern = "^\\s*#+", replacement = "") |>
    gsub(pattern = "\\s*&\\s*", replacement = " AND ") |>
    gsub(pattern = "\\s+", replacement = " ") |>
    gsub(pattern = "[.,;:*]+$", replacement = "") |>
    trimws() |>
    # Quotes around the whole keyword ('DEPRESSION'); one inside stays.
    sub(pattern = "^'(.+)'$", replacement = "\\1") |>
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
# the spellings of a keyword in `apart` (a confirmed merge marked no) keep
# their own spelling. `stage` (after cleanup and
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
  held <- merged$form %in% apart
  if (length(merge) > 0L) {
    merged$form <- .kw_apply_maps(merged$form, list(merge))
    merged <- .kw_merge_by_key(merged$form, papers, unique(unname(merge)))
  }
  form <- merged$form
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

# The sheet has three tables: `decisions` (one question per row: should
# `keyword` be merged into `merge_into`?), `confirmed` (each keyword with the
# spellings the confirmed steps merge into it) and `all_keywords` (every
# keyword in `min_papers` or more papers, for a tool such as OpenRefine). An
# .xlsx holds them as tabs; a .csv sheet is the decisions table, with the
# others in <sheet>_confirmed.csv and <sheet>_all_keywords.csv beside it.
.kw_parts <- c("decisions", "confirmed", "all_keywords")

.kw_part_file <- function(path, part) {
  if (identical(part, "decisions")) return(path)
  sub("\\.csv$", sprintf("_%s.csv", part), path, ignore.case = TRUE)
}

# One table of a checked sheet, or NULL when the sheet does not have it.
.kw_read_part <- function(path, part) {
  if (!file.exists(path)) .bad_input(sprintf("`decisions` file not found: %s", path))
  if (grepl("\\.csv$", path, ignore.case = TRUE)) {
    file <- .kw_part_file(path, part)
    return(if (file.exists(file)) .kw_read_table(file, argument = "decisions"))
  }
  table <- .kw_read_table(path, argument = "decisions")
  tabs <- readxl::excel_sheets(path)
  if (identical(part, "decisions") && !"decisions" %in% tabs) return(table)
  if (part %in% tabs) as.data.frame(readxl::read_excel(path, sheet = part, col_types = "text"))
}

# `merge` answers: empty, yes or no, any case or spacing.
.kw_answers <- function(merge, column = "merge") {
  answer <- tolower(trimws(as.character(merge)))
  answer[is.na(answer)] <- ""
  odd <- setdiff(answer, c("", "yes", "no"))
  if (length(odd) > 0L) {
    .bad_input(sprintf("`%s` must be yes or no, not %s", column, paste(sprintf("\"%s\"", odd), collapse = ", ")))
  }
  answer
}

.kw_no_map <- function() stats::setNames(character(), character())

# A checked sheet as `merge` (keyword -> merge_into, the decisions marked
# yes) and `apart` (keywords whose confirmed merge is marked no).
.kw_read_sheet <- function(path, answers = "merge") {
  sheet <- .kw_read_part(path, "decisions")
  matched_on <- if ("keyword_backup" %in% names(sheet)) "keyword_backup" else "keyword"
  absent <- setdiff(c("merge_into", answers, matched_on), names(sheet))
  if (length(absent) > 0L) {
    .bad_input(sprintf("the sheet has no column(s) %s; use the sheet keywords_clean(level = \"all\") wrote",
                       paste(absent, collapse = ", ")))
  }
  keyword <- trimws(as.character(sheet[[matched_on]]))
  into <- trimws(as.character(sheet$merge_into))
  answer <- .kw_answers(sheet[[answers]], answers)
  take <- answer == "yes" & !is.na(keyword) & !is.na(into) & nzchar(into)
  merge <- if (any(take)) .kw_table_map(data.frame(OLD = keyword[take], NEW = into[take])) else .kw_no_map()
  confirmed <- .kw_read_part(path, "confirmed")
  undone <- if (!is.null(confirmed) && all(c("keyword", "merge") %in% names(confirmed))) {
    confirmed$keyword[.kw_answers(confirmed$merge) == "no" & !is.na(confirmed$keyword)]
  }
  list(merge = merge, apart = unique(.kw_brackets(.kw_normalise(as.character(undone)))$term),
       unanswered = sum(answer == ""))
}

# An edited sheet (keyword rewritten, keyword_backup untouched, as OpenRefine
# leaves it), from the decisions and all_keywords tables, as a map
# backup -> keyword applied after the spelling merge, so every spelling of
# an edited keyword follows it.
.kw_read_edits <- function(path) {
  tables <- Filter(\(t) !is.null(t) && all(c("keyword", "keyword_backup") %in% names(t)),
                   lapply(c("decisions", "all_keywords"), \(part) .kw_read_part(path, part)))
  if (length(tables) == 0L) {
    .bad_input("`by = \"edit\"` needs the columns keyword and keyword_backup, as keywords_clean(level = \"all\") writes them")
  }
  new <- trimws(unlist(lapply(tables, \(t) as.character(t$keyword))))
  old <- trimws(unlist(lapply(tables, \(t) as.character(t$keyword_backup))))
  edited <- !is.na(new) & !is.na(old) & nzchar(new) & new != old
  pairs <- unique(data.frame(OLD = old[edited], NEW = new[edited]))
  list(merge = if (nrow(pairs) > 0L) .kw_table_map(pairs) else .kw_no_map(), apart = character())
}

# Writes the sheet for keywords in `min_papers` or more papers (counted over
# all columns), nothing applied yet, and the prompt beside it.
.kw_write_sheet <- function(parsed, vocabulary, titles, min_papers, path) {
  terms <- unique(vocabulary$final)
  term_of_written <- match(vocabulary$final, terms)
  # Integer codes throughout: (paper, term) for every use in every column.
  use_term <- unlist(lapply(parsed, \(p) term_of_written[match(p$written, vocabulary$written)][p$w]))
  use_paper <- unlist(lapply(parsed, `[[`, "paper"))
  once <- !duplicated(use_paper * (length(terms) + 1) + use_term)
  papers <- tabulate(use_term[once], nbins = length(terms))
  first_paper <- use_paper[once][match(seq_along(terms), use_term[once])]
  example_of <- \(k) if (is.null(titles)) rep(NA_character_, length(k)) else .kr_clip(titles[first_paper[match(k, terms)]], 150L)

  # Candidates: keywords spelled alike, and acronyms with their long forms.
  eligible <- which(papers >= min_papers)
  candidates <- terms[eligible]
  counts <- papers[eligible]
  spelling <- .kw_minhash_pairs(candidates)
  spelling <- spelling[!.kw_unlike_words(candidates[spelling$a], candidates[spelling$b]), ]
  acronym <- .kw_acronym_pairs(vocabulary, candidates, counts)
  group <- .kw_groups(spelling, acronym, counts)
  source <- vapply(seq_along(candidates), \(i) paste(c(
    if (i %in% c(spelling$a, spelling$b)) "spelling",
    if (i %in% c(acronym$a, acronym$b)) "acronym"), collapse = "+"), character(1))
  ask <- which(group != seq_along(candidates))
  decisions <- data.frame(keyword = candidates[ask], keyword_backup = candidates[ask],
                          merge_into = candidates[group[ask]], merge = rep(NA_character_, length(ask)),
                          papers = counts[ask], merge_into_papers = counts[group[ask]], source = source[ask],
                          example = example_of(candidates[ask]), stringsAsFactors = FALSE)
  decisions <- decisions[order(-decisions$merge_into_papers, decisions$merge_into, -decisions$papers,
                               decisions$keyword, method = "radix"), ]

  # Confirmed: each keyword with the cleaned spellings a confirmed step gave
  # its name, in one cell.
  moved <- vocabulary$stage != vocabulary$final & nzchar(vocabulary$stage)
  forms <- data.frame(keyword = vocabulary$final[moved], spelling = vocabulary$stage[moved],
                      papers = vocabulary$papers[moved],
                      source = ifelse(vocabulary$mapped[moved] != vocabulary$stage[moved], "your table", "same spelling"),
                      stringsAsFactors = FALSE)
  forms <- forms[forms$keyword %in% candidates, ]
  pair <- match(paste(forms$keyword, forms$spelling, sep = "\r"), unique(paste(forms$keyword, forms$spelling, sep = "\r")))
  total <- as.vector(rowsum(forms$papers, pair, reorder = TRUE))
  forms <- forms[!duplicated(pair), ]
  forms$papers <- total
  forms <- forms[order(forms$keyword, -forms$papers, forms$spelling, method = "radix"), ]
  confirmed_names <- unique(forms$keyword)
  by_name <- factor(forms$keyword, levels = confirmed_names)
  confirmed <- data.frame(
    keyword = confirmed_names, papers = counts[match(confirmed_names, candidates)],
    also_written_as = vapply(split(sprintf("%s (%d)", forms$spelling, as.integer(forms$papers)), by_name),
                             paste, character(1), collapse = "; "),
    source = vapply(split(forms$source, by_name), \(s) paste(sort(unique(s)), collapse = "+"), character(1)),
    merge = rep("yes", length(confirmed_names)), stringsAsFactors = FALSE)
  confirmed <- confirmed[order(-confirmed$papers, confirmed$keyword, method = "radix"), ]

  # Every keyword, for a tool such as OpenRefine to cluster the whole list.
  all_keywords <- data.frame(keyword = candidates, keyword_backup = candidates, papers = counts,
                             example = example_of(candidates), stringsAsFactors = FALSE)
  all_keywords <- all_keywords[order(-all_keywords$papers, all_keywords$keyword, method = "radix"), ]

  tables <- lapply(list(decisions = decisions, confirmed = confirmed, all_keywords = all_keywords), \(t) {
    rownames(t) <- NULL
    t
  })
  prompt <- .kw_prompt()
  prompt_file <- sub("\\.(csv|xlsx)$", "_prompt.txt", path, ignore.case = TRUE)
  writeLines(enc2utf8(prompt), prompt_file, useBytes = TRUE)
  if (grepl("\\.csv$", path, ignore.case = TRUE)) {
    lapply(.kw_parts, \(part) .kr_write_csv(tables[[part]], .kw_part_file(path, part)))
  } else {
    writexl::write_xlsx(c(tables, list(prompt = data.frame(prompt = strsplit(prompt, "\n")[[1L]]))), path)
  }
  message(sprintf(paste0("%s: %d merges to decide, %d keywords with confirmed spellings, %d keywords in %s or more ",
                         "papers. Nothing is merged yet: write yes or no in `merge`, then ",
                         "keywords_merge(data, decisions = \"%s\"). A prompt for a model is in %s."),
                  basename(path), nrow(decisions), nrow(confirmed), nrow(all_keywords), format(min_papers),
                  basename(path), basename(prompt_file)))
}

# The instructions for a model (or a person) checking the sheet.
.kw_prompt <- function() {
  paste(
    "You are checking proposed keyword merges for a bibliometric study.",
    "",
    "Each row asks one question: should `keyword` be merged into `merge_into`?",
    "`example` is the title of a paper that uses `keyword`.",
    "",
    "Write yes or no in `merge` on every row; leave none empty:",
    "- yes: `keyword` means the same concept as `merge_into`: a spelling variant, a plural,",
    "  a translation, an abbreviation or an acronym of it.",
    "- no: it is broader or narrower, its opposite, a different concept that is only",
    "  spelled alike, or you are not sure.",
    "",
    "Examples:",
    "- CHATBOTS -> CHATBOT: yes (plural)",
    "- AI -> ARTIFICIAL INTELLIGENCE: yes (acronym)",
    "- PSICOLOGIA -> PSYCHOLOGY: yes (translation)",
    "- VIRTUAL REALITY THERAPY -> VIRTUAL REALITY: no (narrower)",
    "- DISHONESTY -> HONESTY: no (opposite)",
    "- WEIGHT LOSS -> WEIGHT GAIN: no (different concept)",
    "",
    "Change no other column.",
    sep = "\n")
}

# Pairs spelled alike that are different words: the first letter differs
# (DEPRESSION / REPRESSION), or, word by word, a word starts with another
# letter or is another word with a prefix (SOCIAL JUSTICE / SOCIAL
# INJUSTICE, ADOLESCENTS / PREADOLESCENTS). Hyphens are ignored.
.kw_prefixes <- c("PRE", "POST", "NON", "UN", "IN", "IM", "IR", "IL", "DIS", "MIS", "ANTI", "SUB", "SUPER",
                  "SEMI", "MULTI", "INTER", "INTRA", "RE", "CO", "OVER", "UNDER", "HYPER", "HYPO")

.kw_unlike_words <- function(a, b) {
  a <- gsub("-", "", a, fixed = TRUE)
  b <- gsub("-", "", b, fixed = TRUE)
  unlike <- substr(a, 1L, 1L) != substr(b, 1L, 1L)
  words_a <- strsplit(a, " ", fixed = TRUE)
  words_b <- strsplit(b, " ", fixed = TRUE)
  aligned <- which(lengths(words_a) == lengths(words_b))
  if (length(aligned) == 0L) return(unlike)
  x <- unlist(words_a[aligned])
  y <- unlist(words_b[aligned])
  pair <- rep(aligned, lengths(words_a[aligned]))
  prefixed <- Reduce(`|`, lapply(.kw_prefixes, \(p) x == paste0(p, y) | y == paste0(p, x)), logical(length(x)))
  clash <- x != y & (substr(x, 1L, 1L) != substr(y, 1L, 1L) | prefixed)
  unlike[unique(pair[clash])] <- TRUE
  unlike
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

  # Exact Jaccard, vectorised: each trigram of a is looked up among b's.
  offset <- c(0L, cumsum(sizes))
  per_pair <- sizes[pairs$a]
  pair_of <- rep(seq_len(nrow(pairs)), per_pair)
  gram_of_a <- code[rep(offset[pairs$a], per_pair) + sequence(per_pair)]
  width <- max(code) + 1
  in_b <- (pairs$b[pair_of] * width + gram_of_a) %in% (term_of * width + code)
  shared <- tabulate(pair_of[in_b], nbins = nrow(pairs))
  jaccard <- shared / (sizes[pairs$a] + sizes[pairs$b] - shared)
  kept <- cbind(pairs, jaccard = jaccard)[jaccard >= threshold, ]
  rownames(kept) <- NULL
  kept
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
