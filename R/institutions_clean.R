# Institution cleaning for bibliometric data. Each paper's institutions come
# from OpenAlex where it identifies them, else from the address text; the
# names left from address text are cleaned like keywords, and the merges
# that need a judgement go to a sheet checked outside R.

#' Clean institution names
#'
#' @description
#' Gives each institution one name in `AU_UN` and returns the data frame,
#' ready for bibliometrix. Institutions are identified by their OpenAlex
#' institution ID: for every paper with a DOI, OpenAlex says which
#' institutions its authors belong to.
#'
#' The confirmed steps, applied with no review:
#' 1. **OpenAlex ID**: the institutions OpenAlex links to a paper's authors,
#'    under OpenAlex's name. Two institutions OpenAlex keeps apart that share
#'    a name get their country code (`NORTHEASTERN UNIVERSITY, US`,
#'    `NORTHEASTERN UNIVERSITY, CN`).
#' 2. **Address OpenAlex linked**: an address OpenAlex linked to one
#'    institution on any paper gets that institution on every paper
#'    (`source` `openalex address`).
#' 3. **OpenAlex name**: otherwise the institution part of the address is
#'    taken (the part naming a university, college, academy or hospital, else
#'    an institute, centre, lab or company, else the first part); if it is
#'    the name of one institution OpenAlex knows, accents lost or not, it is
#'    that institution (`openalex name`).
#' 4. **Address**: otherwise that part is kept as written, cleaned as
#'    [keywords_clean()] cleans keywords, your `replace` tables applied, and
#'    names written differently (case, punctuation, accents, spaces, plural)
#'    made one. A name that becomes an identified institution's name joins
#'    it.
#'
#' Papers OpenAlex does not cover, and authors it gives only an address for,
#' go through steps 2 to 4. Two institutions OpenAlex identified are never
#' merged.
#'
#' OpenAlex's answers are kept in `cache`, so a second run, or a run that
#' stopped, asks only for the papers not asked yet. Requests run ten at a
#' time, at most ten a second (OpenAlex's limit), 100 DOIs each.
#'
#' `level = "all"` merges nothing: it returns `data` unchanged and writes
#' every merge to the `decisions` sheet, for a person or a model to check
#' outside R, with a short prompt beside it, as [keywords_clean()] does. The
#' sheet has one group per `name` (the institution the group becomes): its
#' `confirmed` merges (steps 2 to 4) and its `needs_revision` candidates,
#' found as [keywords_clean()] finds them (`source`: `spelling`,
#' `word order`, `ngram`, `stem`, `initials`). An identified institution is
#' always the `name` of its group. Only institutions in at least `min_papers`
#' papers go to the sheet, each of them, grouped or alone (`single`). Bring
#' it back with [institutions_merge()].
#'
#' @param data A data frame with one row per paper, such as a bibliometrix
#'   data frame.
#' @param affiliations The column of author addresses, `;`-separated.
#'   Default `"C1"`.
#' @param doi The DOI column. Default `"DI"`.
#' @param level `"confirmed"` (default) or `"all"`; see above.
#' @param decisions With `level = "all"`: the sheet to write, a new path
#'   ending in `.xlsx` or `.csv` (an existing file is never replaced). Its
#'   columns: `group`, `name`, `institution`, `institution_backup` (a copy of
#'   `institution` that [institutions_merge()] matches on), `status`
#'   (`single`, `confirmed`, `needs_revision`), `papers`, `openalex_id`,
#'   `source` and `example` (an address naming the institution). The model
#'   or person checking it adds a column `merge`. The prompt goes to
#'   `<sheet>_prompt.txt` and, in an `.xlsx`, to a second tab `prompt`.
#' @param min_papers Only institutions in at least this many papers go into
#'   the sheet. Default 5.
#' @param replace Your replacement table for names from address text: a data
#'   frame with columns `OLD` and `NEW`, or the path to a `.csv`, `.xlsx` or
#'   `.xls` file holding one, as in [keywords_clean()]. It never renames an
#'   identified institution; a name renamed to an identified institution's
#'   name joins it.
#' @param openalex Ask OpenAlex (`TRUE`, the default)? With `FALSE` every
#'   institution comes from the address text.
#' @param cache A folder for OpenAlex's answers, or `NULL` to keep none.
#'   Default: a folder in [tools::R_user_dir()]. Delete it to ask again.
#' @param mailto An e-mail address for OpenAlex's polite pool (faster, more
#'   reliable). Sent only to OpenAlex, and only when you give it.
#' @param output `"minimal"` (default) returns `data` with `AU_UN` written
#'   and nothing else added. `"all"` also adds `AU_UN_ID` (the OpenAlex IDs
#'   of each paper's institutions, `;`-separated, `NA` where none) and, when
#'   `data` has an `AU_UN` already, `AU_UN_ORIGINAL`.
#'
#' @return With `level = "confirmed"`: `data`, same class and rows, with
#'   `AU_UN` (each paper's institutions, `"; "`-separated, in author order,
#'   `NA` where none) added or replaced; with `output = "all"`, plus the
#'   columns above. With `level = "all"`: `data` unchanged, invisibly (the sheet is the
#'   result). A message counts the institutions found each way.
#'
#' @section Errors:
#' Raises `saqrmisc_bad_input` for a missing or non-character column, a bad
#' `openalex`, `mailto`, `cache` or `min_papers`, `decisions` given with
#' `level = "confirmed"` or missing or existing with `"all"`, and the
#' `replace` errors of [keywords_clean()]; `saqrmisc_missing_package` when
#' `openalex = TRUE` without httr2, or for `.xlsx` without readxl / writexl.
#' Failed OpenAlex requests warn `saqrmisc_openalex_failed`; those papers
#' fall back to their addresses and are asked again on the next run.
#'
#' @references
#' Priem, J., Piwowar, H., & Orr, R. (2022). OpenAlex: A fully-open index of
#' scholarly works, authors, venues, institutions, and concepts.
#' *arXiv*. \doi{10.48550/arXiv.2205.01833}
#'
#' @examples
#' papers <- data.frame(
#'   TI = c("Paper one", "Paper two", "Paper three"),
#'   DI = NA_character_,
#'   C1 = c("DEPARTMENT OF COMPUTING, UNIVERSITY OF TURKU, TURKU, FINLAND",
#'          "SCHOOL OF EDUCATION, UNIVERSITY OF EASTERN FINLAND, JOENSUU, FINLAND",
#'          "UNIVERSITY-OF-TURKU, FINLAND; UNIV EASTERN FINLAND, FINLAND")
#' )
#' institutions_clean(papers, openalex = FALSE)
#'
#' # Every merge to a sheet, for a person or a model to check.
#' sheet <- tempfile(fileext = ".csv")
#' unchanged <- institutions_clean(papers, level = "all", decisions = sheet, min_papers = 1,
#'                                 openalex = FALSE)
#' utils::read.csv(sheet)
#'
#' @seealso [institutions_merge()], [authors_clean()], [keywords_clean()]
#' @export
institutions_clean <- function(data,
                               affiliations = "C1",
                               doi = "DI",
                               level = c("confirmed", "all"),
                               decisions = NULL,
                               min_papers = 5,
                               replace = NULL,
                               openalex = TRUE,
                               cache = .in_default_cache(),
                               mailto = NULL,
                               output = c("minimal", "all")) {
  level <- match.arg(level)
  output <- match.arg(output)
  .in_check(data, affiliations, doi, openalex, cache, mailto)
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
  run <- .in_run(data, affiliations, doi, openalex, cache, mailto, maps, .in_no_map(), character())
  if (level == "all") {
    .in_write_sheet(run, min_papers, decisions)
    return(invisible(data))
  }
  .in_output(data, run, output)
}

#' Bring a checked institution sheet back into the data
#'
#' @description
#' Applies the sheet that [institutions_clean()] wrote with `level = "all"`,
#' after it was checked outside R, and returns the data frame. The confirmed
#' steps of [institutions_clean()] run again (OpenAlex's answers come from
#' the cache), then the sheet, read one of two ways:
#'
#' * `by = "merge"` (default), for a person or a model that adds a column
#'   `merge` and answers `yes` or `no` in it:
#'   * a `needs_revision` row with `yes` becomes its `name`, and joins it
#'     when the name is an identified institution;
#'   * a `confirmed` row with `no` keeps its own name, also when it came to
#'     an identified institution by its address or name;
#'   * anything else keeps what the row's `status` says.
#' * `by = "edit"`, for a tool that rewrites the names themselves, such as
#'   OpenRefine clustering the `institution` column: every institution whose
#'   `institution` no longer equals its untouched `institution_backup`
#'   becomes the new text. An edited `confirmed` row moves only that
#'   spelling. Identified institutions keep their names.
#'
#' Pass the same arguments as to [institutions_clean()].
#'
#' @param data A data frame with one row per paper.
#' @param decisions The checked sheet: a data frame or the path of the
#'   `.xlsx` or `.csv` file.
#' @param by `"merge"` (default) reads the `merge` column; `"edit"` reads the
#'   edits of `institution` against `institution_backup`.
#' @param affiliations,doi,replace,openalex,cache,mailto,output As in
#'   [institutions_clean()].
#'
#' @return `data`, same class and rows, with `AU_UN` written as by
#'   [institutions_clean()]; with `output = "all"`, plus its columns. A
#'   message counts the rows applied from the sheet.
#'
#' @section Errors:
#' Raises `saqrmisc_bad_input` for a missing sheet; with `by = "merge"`, one
#' without `name`, `status`, `merge` and `institution_backup` (or
#' `institution`), or a `merge` other than empty, `yes` or `no`; with
#' `by = "edit"`, one without `institution` and `institution_backup`; and
#' the errors of [institutions_clean()].
#'
#' @seealso [institutions_clean()]
#' @examples
#' papers <- data.frame(
#'   DI = NA_character_,
#'   C1 = c("UNIVERSITY OF TURKU, FINLAND", "UNIVERSITY OF TURKU, FINLAND", "TURKU UNIVERSITY, FINLAND",
#'          "UNIVERSITY OF OULU, FINLAND")
#' )
#' sheet <- tempfile(fileext = ".csv")
#' unchanged <- institutions_clean(papers, level = "all", decisions = sheet, min_papers = 1,
#'                                 openalex = FALSE)
#' checked <- utils::read.csv(sheet)
#' checked$merge <- ifelse(checked$institution == "TURKU UNIVERSITY", "yes", "no")
#' utils::write.csv(checked, sheet, row.names = FALSE)
#' institutions_merge(papers, decisions = sheet, openalex = FALSE)
#' @export
institutions_merge <- function(data, decisions, by = c("merge", "edit"), affiliations = "C1", doi = "DI",
                               replace = NULL, openalex = TRUE, cache = .in_default_cache(), mailto = NULL,
                               output = c("minimal", "all")) {
  by <- match.arg(by)
  output <- match.arg(output)
  .in_check(data, affiliations, doi, openalex, cache, mailto)
  given <- .kw_decisions(decisions, "institutions_clean")
  sheet <- if (by == "merge") .kw_read_sheet(given$table, term = "institution", by_status = TRUE) else
    .kw_read_edits(given$table, term = "institution")
  maps <- c(.kw_replacements(replace), if (length(sheet$moved) > 0L) list(sheet$moved))
  run <- .in_run(data, affiliations, doi, openalex, cache, mailto, maps, sheet$merge, sheet$apart)
  message(sprintf("institutions_merge: from %s, %d merge(s) applied, %d confirmed merge(s) undone",
                  given$name, length(sheet$merge) + length(sheet$moved), length(sheet$apart)))
  .in_output(data, run, output)
}

# ---- the run ------------------------------------------------------------------

.in_no_map <- function() stats::setNames(character(), character())

.in_check <- function(data, affiliations, doi, openalex, cache, mailto) {
  if (!is.data.frame(data)) .bad_input("`data` must be a data frame")
  if (nrow(data) == 0L) .bad_input("`data` has no papers")
  .kw_check_columns(data, affiliations)
  if (!is.character(doi) || length(doi) != 1L || !doi %in% names(data)) {
    .bad_input(sprintf("`doi` must name a column of `data`; there is no column %s", format(doi)))
  }
  if (!(isTRUE(openalex) || isFALSE(openalex))) .bad_input("`openalex` must be TRUE or FALSE")
  if (!is.null(cache) && !(is.character(cache) && length(cache) == 1L && !is.na(cache) && nzchar(cache))) {
    .bad_input("`cache` must be one folder path, or NULL")
  }
  if (!is.null(mailto) && !(is.character(mailto) && length(mailto) == 1L && grepl("@", mailto))) {
    .bad_input("`mailto` must be one e-mail address")
  }
  if (openalex) .kr_need("httr2")
  invisible(TRUE)
}

# One row per paper and institution: `paper`, `address` (NA when OpenAlex
# named it directly), `stage` (the cleaned institution part of the address),
# `institution`, `openalex_id`, `source`, and `mapped` (`stage` after the
# replace tables). `merge` (stage -> name, from a sheet) is applied after the
# spelling merge; stages in `apart` keep their own name.
.in_run <- function(data, affiliations, doi, openalex, cache, mailto, maps, merge, apart) {
  dois <- tolower(trimws(as.character(data[[doi]])))
  dois[!is.na(dois) & !nzchar(dois)] <- NA_character_
  asked <- unique(dois[!is.na(dois)])
  tables <- if (openalex && length(asked) > 0L) .in_fetch(asked, mailto, cache) else .in_no_tables()
  built <- .in_build(data[[affiliations]], dois, tables)
  rows <- built$rows
  known <- built$known

  # A confirmed match by address or name marked no keeps the address's name.
  undone <- rows$stage %in% apart & rows$source %in% c("openalex address", "openalex name")
  rows$openalex_id[undone] <- NA_character_
  rows$source[undone] <- "address"

  text <- is.na(rows$openalex_id)
  stage <- rows$stage[text]
  distinct <- unique(stage)
  at <- match(stage, distinct)
  once <- !duplicated(paste(rows$paper[text], at))
  counts <- tabulate(at[once], nbins = length(distinct))
  mapped <- .kw_apply_maps(distinct, maps)
  merged <- .kw_merge_by_key(mapped, counts, .kw_targets(maps))
  if (length(merge) > 0L) {
    merged$form <- .kw_apply_maps(merged$form, list(merge))
    merged <- .kw_merge_by_key(merged$form, counts, unique(c(unname(merge), known$institution)))
  }
  final <- merged$form
  held <- distinct %in% apart
  final[held] <- distinct[held]
  # A name from address text that is an identified institution's name, but
  # for case, punctuation or accents, joins that institution.
  by_key <- .in_unique_lookup(.in_key(known$institution), known$openalex_id)
  joined <- unname(by_key[.in_key(final)])
  final[!is.na(joined)] <- known$institution[match(joined[!is.na(joined)], known$openalex_id)]

  rows$mapped <- rows$stage
  rows$mapped[text] <- mapped[at]
  rows$institution <- known$institution[match(rows$openalex_id, known$openalex_id)]
  rows$institution[text] <- final[at]
  rows$openalex_id[text] <- joined[at]
  reviewed <- text & (rows$mapped != rows$stage | (length(merge) > 0L & rows$stage %in% names(merge)))
  rows$source[reviewed] <- "review"
  rows <- rows[!is.na(rows$institution) & !duplicated(rows[c("paper", "institution")]), ]
  rownames(rows) <- NULL
  list(rows = rows, known = known, papers = nrow(data))
}

# `data` with AU_UN (and, with output = "all", AU_UN_ID, AU_UN_ORIGINAL).
.in_output <- function(data, run, output) {
  rows <- run$rows
  holder <- factor(rows$paper, levels = seq_len(nrow(data)))
  join <- \(x) vapply(split(x, holder), \(v) if (length(v) == 0L) NA_character_ else paste(v, collapse = "; "),
                      character(1), USE.NAMES = FALSE)
  cleaned <- data
  if (output == "all" && "AU_UN" %in% names(data) && !"AU_UN_ORIGINAL" %in% names(data)) {
    cleaned$AU_UN_ORIGINAL <- data$AU_UN
  }
  cleaned$AU_UN <- join(rows$institution)
  if (output == "all") {
    ids <- rows$openalex_id
    ids[is.na(ids)] <- "NA"
    cleaned$AU_UN_ID <- join(ids)
  }
  .in_report(run)
  cleaned
}

.in_report <- function(run) {
  rows <- run$rows
  sources <- c("openalex", "openalex address", "openalex name", "address", "review")
  links <- vapply(sources, \(s) sum(rows$source == s), integer(1))
  message(sprintf(paste0("institutions: %d institutions in %d of %d papers; links by OpenAlex ID %d, ",
                         "by an address OpenAlex linked %d, by OpenAlex name %d, from address text %d, ",
                         "by your table or sheet %d"),
                  length(unique(rows$institution)), length(unique(rows$paper)), run$papers,
                  links[[1]], links[[2]], links[[3]], links[[4]], links[[5]]))
}

# ---- OpenAlex -----------------------------------------------------------------

.in_batch_size <- 100L
.in_round_size <- 100L

.in_default_cache <- function() file.path(tools::R_user_dir("Saqrmisc", "cache"), "openalex_institutions")

.in_no_tables <- function() {
  list(asked = character(),
       direct = data.frame(doi = character(), openalex_id = character()),
       loose = data.frame(doi = character(), raw = character()),
       linked = data.frame(doi = character(), address = character(), openalex_id = character()),
       known = data.frame(openalex_id = character(), display = character(), ror = character(),
                          country = character(), type = character()))
}

.in_bind_tables <- function(parts) {
  empty <- .in_no_tables()
  stats::setNames(lapply(names(empty), \(n) {
    pieces <- c(list(empty[[n]]), lapply(parts, `[[`, n))
    if (n == "asked") unique(unlist(pieces)) else {
      out <- do.call(rbind, pieces)
      rownames(out) <- NULL
      out
    }
  }), names(empty))
}

# What OpenAlex says about `dois`: from the cache, and asked for those not in
# it. Each round of requests is saved to the cache as one file before the
# next starts, so an interrupted run keeps what it fetched.
.in_fetch <- function(dois, mailto, cache) {
  saved <- if (!is.null(cache) && dir.exists(cache)) {
    lapply(list.files(cache, pattern = "^round-.*\\.rds$", full.names = TRUE), readRDS)
  }
  store <- .in_bind_tables(saved)
  wanted <- setdiff(dois, store$asked)
  if (length(wanted) > 0L) {
    plain <- wanted[!grepl("[,|]", wanted)]
    batches <- c(split(plain, ceiling(seq_along(plain) / .in_batch_size)), as.list(setdiff(wanted, plain)))
    rounds <- split(seq_along(batches), ceiling(seq_along(batches) / .in_round_size))
    if (!is.null(cache)) dir.create(cache, recursive = TRUE, showWarnings = FALSE)
    reasons <- character()
    failed <- 0L
    done <- 0L
    stamp <- format(Sys.time(), "%Y%m%d-%H%M%S")
    # A loop, not lapply: each round is written to the cache before the next
    # is asked, so an interrupted run loses at most one round.
    for (k in seq_along(rounds)) {
      round <- rounds[[k]]
      replies <- .in_ask_openalex(batches[round], mailto)
      bad <- vapply(replies, inherits, logical(1), what = "error")
      reasons <- unique(c(reasons, vapply(replies[bad], conditionMessage, character(1))))
      failed <- failed + sum(bad)
      works <- unlist(lapply(replies[!bad], \(r) if (is.null(r)) list() else r$results %||% list(r)),
                      recursive = FALSE)
      part <- .in_tables(works)
      part$asked <- unlist(batches[round][!bad], use.names = FALSE)
      if (!is.null(cache) && length(part$asked) > 0L) {
        saveRDS(part, file.path(cache, sprintf("round-%s-%05d.rds", stamp, k)))
      }
      store <- .in_bind_tables(list(store, part))
      done <- done + length(unlist(batches[round]))
      message(sprintf("institutions: OpenAlex asked about %d of %d papers", done, length(wanted)))
    }
    if (failed > 0L) {
      warning(warningCondition(
        sprintf("%d of %d OpenAlex request(s) failed (%s); those papers fall back to their addresses",
                failed, length(batches), paste(reasons, collapse = "; ")),
        class = "saqrmisc_openalex_failed", call = NULL))
    }
  }
  keep <- \(t) t[t$doi %in% dois, , drop = FALSE]
  tables <- list(asked = intersect(store$asked, dois), direct = keep(store$direct), loose = keep(store$loose),
                 linked = keep(store$linked))
  tables$known <- store$known[store$known$openalex_id %in% c(tables$direct$openalex_id, tables$linked$openalex_id), ]
  tables$known <- tables$known[!duplicated(tables$known$openalex_id), ]
  tables
}

# Asks OpenAlex about each batch of DOIs, ten requests at a time and at most
# ten a second. Each reply is the parsed JSON, NULL (not found) or an error.
.in_ask_openalex <- function(batches, mailto) {
  polite <- if (is.null(mailto)) list() else list(mailto = mailto)
  requests <- lapply(batches, \(batch) {
    if (length(batch) == 1L && grepl("[,|]", batch)) {
      .in_request(sprintf("https://api.openalex.org/works/doi:%s", utils::URLencode(batch, reserved = TRUE)),
                  c(list(select = "doi,authorships"), polite))
    } else {
      .in_request("https://api.openalex.org/works", c(list(
        filter = paste0("doi:", paste(batch, collapse = "|")),
        select = "doi,authorships", `per-page` = .in_batch_size), polite))
    }
  })
  responses <- httr2::req_perform_parallel(requests, on_error = "continue", progress = FALSE, max_active = 10L)
  lapply(responses, .in_reply)
}

.in_request <- function(url, query) {
  do.call(httr2::req_url_query, c(list(httr2::request(url)), query)) |>
    httr2::req_user_agent("Saqrmisc (https://github.com/mohsaqr/Saqrmisc)") |>
    httr2::req_timeout(.au_timeout) |>
    httr2::req_throttle(capacity = 10, fill_time_s = 1, realm = "api.openalex.org") |>
    httr2::req_retry(max_tries = 3, retry_on_failure = TRUE,
                     is_transient = \(resp) httr2::resp_status(resp) %in% c(500L, 502L, 503L, 504L) ||
                       (httr2::resp_status(resp) == 429L && .au_wait(resp) <= .au_timeout)) |>
    httr2::req_error(is_error = \(resp) FALSE)
}

.in_reply <- function(resp) {
  if (inherits(resp, "error")) return(resp)
  status <- httr2::resp_status(resp)
  if (status == 404L) return(NULL)
  if (status == 429L) {
    return(simpleError(sprintf("OpenAlex request limit reached, try again in %.1f hours", .au_wait(resp) / 3600)))
  }
  if (status != 200L) return(simpleError(sprintf("HTTP %d", status)))
  httr2::resp_body_json(resp, simplifyVector = FALSE)
}

# OpenAlex works as tables: `direct` (each institution named on a work, in
# author order), `loose` (the addresses of authors it names no institution
# for), `linked` (addresses it linked to exactly one institution) and
# `known` (the institutions).
.in_tables <- function(works) {
  out <- .in_no_tables()
  if (length(works) == 0L) return(out)
  doi <- tolower(sub("^https://doi.org/", "", vapply(works, \(w) w$doi %||% NA_character_, character(1))))
  works <- works[!is.na(doi)]
  doi <- doi[!is.na(doi)]
  field <- \(items, name, strip = "") {
    vapply(items, \(i) sub(strip, "", i[[name]] %||% NA_character_), character(1), USE.NAMES = FALSE)
  }
  authorships <- unlist(lapply(works, `[[`, "authorships"), recursive = FALSE)
  owner <- rep(doi, vapply(works, \(w) length(w$authorships), integer(1)))

  institutions <- lapply(authorships, `[[`, "institutions")
  flat <- unlist(institutions, recursive = FALSE)
  id <- field(flat, "id", "^https://openalex.org/")
  named <- !is.na(id)
  out$direct <- data.frame(doi = rep(owner, lengths(institutions))[named], openalex_id = id[named])
  out$known <- unique(data.frame(openalex_id = id[named], display = field(flat, "display_name")[named],
                                 ror = field(flat, "ror", "^https://ror.org/")[named],
                                 country = field(flat, "country_code")[named], type = field(flat, "type")[named]))

  bare <- lengths(institutions) == 0L
  raw <- lapply(authorships[bare], \(a) as.character(unlist(a$raw_affiliation_strings)))
  out$loose <- data.frame(doi = rep(owner[bare], lengths(raw)), raw = as.character(unlist(raw)))

  affiliations <- lapply(authorships, `[[`, "affiliations")
  flat_aff <- unlist(affiliations, recursive = FALSE)
  ids <- lapply(flat_aff, \(f) as.character(unlist(f$institution_ids)))
  one <- lengths(ids) == 1L
  out$linked <- data.frame(doi = rep(owner, lengths(affiliations))[one],
                           address = field(flat_aff[one], "raw_affiliation_string"),
                           openalex_id = sub("^https://openalex.org/", "", as.character(unlist(ids[one]))))
  out
}

# ---- from tables and addresses to rows -------------------------------------

# One row per paper and institution, from OpenAlex where it names
# institutions, else from the addresses; `known` names the institutions.
.in_build <- function(address_column, dois, tables) {
  known <- tables$known
  # OpenAlex's own disambiguation, "Amazon (United States)", goes; homonyms
  # get a country code instead, and those still alike a number, so that two
  # IDs never share a name.
  known$institution <- .in_name(sub("\\s*\\([^)]*\\)$", "", known$display))
  twin <- known$institution %in% known$institution[duplicated(known$institution)] & !is.na(known$country)
  known$institution[twin] <- paste0(known$institution[twin], ", ", known$country[twin])
  known <- known[order(known$openalex_id, method = "radix"), ]
  rank <- stats::ave(seq_along(known$institution), known$institution, FUN = seq_along)
  known$institution[rank > 1L] <- sprintf("%s (%d)", known$institution[rank > 1L], rank[rank > 1L])

  by_address <- .in_unique_lookup(.in_key(tables$linked$address), tables$linked$openalex_id)
  # Names learned from OpenAlex's linked addresses count only when the part
  # names a university: a faculty or institute name ("COLLEGE OF SOFTWARE")
  # recurs across universities.
  parsed <- .in_extract(tables$linked$address, tier = TRUE)
  learned <- parsed$tier == 1L
  by_name <- .in_unique_lookup(
    c(.in_key(known$display), .in_key(known$display, mangled = TRUE), .in_key(parsed$part[learned])),
    c(known$openalex_id, known$openalex_id, tables$linked$openalex_id[learned]))

  papers <- data.frame(paper = seq_along(dois), doi = dois)[!is.na(dois), ]
  direct <- tables$direct
  direct$order <- seq_len(nrow(direct))
  direct <- merge(papers, direct, by = "doi")
  covered <- unique(direct$paper)
  loose <- tables$loose
  loose$order <- seq_len(nrow(loose))
  loose <- merge(papers[papers$paper %in% covered, ], loose, by = "doi")
  loose <- loose[order(loose$paper, loose$order), ]
  # Web of Science writes the authors before each address, "[KIM, J; LEE, A]
  # UNIV OULU": the list goes before splitting, its ";" would split the address.
  split_addresses <- .kw_split(gsub("\\[[^]]*\\]\\s*", "", as.character(address_column)))
  uncovered <- setdiff(seq_along(split_addresses), covered)
  text <- data.frame(paper = c(loose$paper, rep(uncovered, lengths(split_addresses[uncovered]))),
                     address = c(loose$raw, as.character(unlist(split_addresses[uncovered]))))
  resolved <- .in_resolve(text$address, by_address, by_name)

  rows <- rbind(
    data.frame(paper = direct$paper, address = rep(NA_character_, nrow(direct)),
               stage = rep(NA_character_, nrow(direct)), openalex_id = direct$openalex_id,
               source = rep("openalex", nrow(direct)), kind = rep(1L, nrow(direct)), order = direct$order),
    data.frame(paper = text$paper, address = text$address, stage = resolved$stage,
               openalex_id = resolved$openalex_id, source = resolved$source,
               kind = rep(2L, nrow(text)), order = seq_len(nrow(text))))
  rows <- rows[!is.na(rows$openalex_id) | (!is.na(rows$stage) & nzchar(rows$stage)), ]
  rows <- rows[order(rows$paper, rows$kind, rows$order, method = "radix"),
               c("paper", "address", "stage", "openalex_id", "source")]
  list(rows = rows, known = known)
}

# Each address resolved once, however often it is written.
.in_resolve <- function(addresses, by_address, by_name) {
  distinct <- unique(addresses)
  part <- .in_extract(distinct)
  via_address <- unname(by_address[.in_key(distinct)])
  via_name <- unname(by_name[.in_key(part)])
  id <- ifelse(!is.na(via_address), via_address, via_name)
  source <- ifelse(!is.na(via_address), "openalex address", ifelse(!is.na(via_name), "openalex name", "address"))
  at <- match(addresses, distinct)
  data.frame(stage = .in_name(part)[at], openalex_id = id[at], source = source[at])
}

# ---- the sheet ----------------------------------------------------------------

# Writes the sheet for institutions in `min_papers` or more papers: their
# confirmed merges and candidates, none applied, and the prompt beside it.
.in_write_sheet <- function(run, min_papers, path) {
  rows <- run$rows
  terms <- unique(rows$institution)
  at <- match(rows$institution, terms)
  once <- !duplicated(paste(rows$paper, at))
  papers <- tabulate(at[once], nbins = length(terms))
  id_of <- rows$openalex_id[match(terms, rows$institution)]

  eligible <- which(papers >= min_papers)
  candidates <- terms[eligible]
  none <- data.frame(a = integer(), b = integer(), source = character())
  found <- .kw_candidates(candidates, papers[eligible], none, fixed = !is.na(id_of[eligible]))
  member <- which(found$group %in% found$group[duplicated(found$group)] & found$group != seq_along(candidates))
  proposed <- data.frame(institution = candidates[member], name = candidates[found$group[member]],
                         status = rep("needs_revision", length(member)), papers = papers[eligible][member],
                         source = found$source[member], stringsAsFactors = FALSE)

  # Confirmed: each name from address text that a confirmed step gave
  # another name.
  moved <- !is.na(rows$stage) & rows$stage != rows$institution
  pair <- paste(rows$stage, rows$institution, sep = "\r")[moved]
  first <- !duplicated(pair)
  pair_papers <- tapply(rows$paper[moved], factor(pair, levels = unique(pair)), \(p) length(unique(p)))
  how <- ifelse(rows$source %in% c("openalex address", "openalex name"), rows$source,
                ifelse(rows$mapped != rows$stage, "your table", "same spelling"))
  done <- data.frame(institution = rows$stage[moved][first], name = rows$institution[moved][first],
                     status = rep("confirmed", sum(first)), papers = as.integer(pair_papers),
                     source = how[moved][first], stringsAsFactors = FALSE)
  done <- done[papers[match(done$name, terms)] >= min_papers, ]

  alone <- setdiff(candidates, c(proposed$institution, proposed$name, done$name, done$institution))
  single <- data.frame(institution = alone, name = alone, status = rep("single", length(alone)),
                       papers = papers[match(alone, terms)], source = rep(NA_character_, length(alone)),
                       stringsAsFactors = FALSE)
  sheet <- rbind(proposed, done, single)
  rank <- match(sheet$status, c("single", "confirmed", "needs_revision"))
  name_papers <- papers[match(sheet$name, terms)]
  sheet <- sheet[order(-name_papers, sheet$name, rank, -sheet$papers, sheet$institution, method = "radix"), ]
  example <- ifelse(sheet$status == "confirmed",
                    .kr_clip(rows$address[match(sheet$institution, rows$stage)], 150L),
                    .kr_clip(rows$address[match(sheet$institution, ifelse(is.na(rows$address), NA, rows$institution))],
                             150L))
  sheet <- data.frame(group = match(sheet$name, unique(sheet$name)), name = sheet$name,
                      institution = sheet$institution, institution_backup = sheet$institution,
                      status = sheet$status, papers = sheet$papers,
                      openalex_id = id_of[match(sheet$institution, terms)], source = sheet$source,
                      example = example, stringsAsFactors = FALSE)
  rownames(sheet) <- NULL

  prompt <- .in_prompt()
  prompt_file <- sub("\\.(csv|xlsx)$", "_prompt.txt", path, ignore.case = TRUE)
  writeLines(enc2utf8(prompt), prompt_file, useBytes = TRUE)
  if (grepl("\\.csv$", path, ignore.case = TRUE)) {
    .kr_write_csv(sheet, path)
  } else {
    writexl::write_xlsx(list(decisions = sheet, prompt = data.frame(prompt = strsplit(prompt, "\n")[[1L]])), path)
  }
  message(sprintf(paste0("%s: %d groups and single institutions, %d confirmed merges and %d to check, ",
                         "institutions in %s or more papers. Nothing is merged yet: add a column `merge` ",
                         "(yes or no), then institutions_merge(data, decisions = \"%s\"). A prompt for a model is in %s."),
                  basename(path), length(unique(sheet$group)), sum(sheet$status == "confirmed"),
                  sum(sheet$status == "needs_revision"), format(min_papers), basename(path), basename(prompt_file)))
}

# The instructions for a model (or a person) checking the sheet.
.in_prompt <- function() {
  paste(
    "Review institution merges for a bibliometric study.",
    "",
    "Each row proposes merging `institution` into `name`. `example` is an author address naming `institution`.",
    "",
    "Add a column `merge` and write yes or no on every row:",
    "- yes: the same organisation (spelling variant, abbreviation, acronym, translation, or former name).",
    "- no: a different organisation, a part of it (department, faculty, school or hospital), a parent or",
    "  sister organisation, or uncertain.",
    "",
    "By `status`:",
    "- needs_revision: yes or no.",
    "- confirmed: yes, unless the merge is incorrect (no).",
    "- single: no (nothing to merge).",
    "",
    "Change no other column.",
    sep = "\n")
}

# ---- address helpers ----------------------------------------------------------

# Address parts naming an institution, strongest first: a university, then a
# college, academy or hospital (a "COLLEGE OF ..." is often a faculty inside a
# university), then an institute, centre, lab or company.
.in_university <- "UNIV|POLITEC|POLYTECH|HOCHSCHULE|ECOLE|INSTITUTE OF TECHNOLOGY"
.in_college <- "COLL|ACAD|HOSP|SCUOLA"
.in_secondary <- paste0("INST|CENT(ER|RE|RO)\\b|\\bCTR\\b|\\bLAB|RESEARCH|FOUNDATION|COUNCIL|ASSOCIATION|",
                        "ORGANI[SZ]ATION|AGENCY|MINISTRY|CORPORATION|COMPANY|\\bINC\\b|\\bLTD\\b|",
                        "GMBH|AUTHORITY|MUSEUM|LIBRAR|CLINIC")

# The institution part of each address (after Aria & Cuccurullo's
# bibliometrix, as in carm-biblio): the first comma-separated part of the
# strongest kind above, else the first part longer than three letters. Parts
# starting with a digit (postcodes) or holding only a legal form (INC, LTD)
# are skipped. `tier` says which kind matched: 1 university, 2 college, 3
# institute, 4 none.
.in_extract <- function(addresses, tier = FALSE) {
  found <- lapply(strsplit(toupper(addresses), ",", fixed = TRUE), \(parts) {
    parts <- trimws(gsub("\\s+", " ", parts))
    parts <- parts[nzchar(parts) & !grepl("^[0-9]", parts) &
                     !grepl("^(INC|LTD|LLC|GMBH|CORP|CO|PLC|SA|AG|BV)\\.?$", parts)]
    if (length(parts) == 0L) return(list(part = NA_character_, tier = 4L))
    hits <- lapply(c(.in_university, .in_college, .in_secondary), \(re) which(grepl(re, parts)))
    level <- which(lengths(hits) > 0L)[1L]
    if (is.na(level)) {
      long <- which(nchar(parts) > 3L)
      return(list(part = parts[c(long, 1L)[1L]], tier = 4L))
    }
    list(part = parts[hits[[level]][1L]], tier = level)
  })
  part <- vapply(found, \(f) f$part, character(1))
  if (tier) list(part = part, tier = vapply(found, \(f) f$tier, integer(1))) else part
}

# A name as the keyword cleaner writes it, so that replacement tables and
# review sheets match: upper case, entities and dashes fixed, brackets out.
.in_name <- function(x) .kw_brackets(.kw_normalise(x))$term

# The matching key of a name or address: brackets dropped, then the keyword
# key (punctuation, spaces and accents gone). `mangled` instead deletes
# accented letters, as bibliometrix does to Scopus addresses
# (UNIVERSITAT -> UNIVERSITT), so that such addresses still find the name.
.in_key <- function(x, mangled = FALSE) {
  x <- gsub("\\s*\\([^)]*\\)", "", x)
  if (mangled) x <- iconv(x, "UTF-8", "ASCII", sub = "")
  .kw_key(x)
}

# Keys that point to one value only; a key seen with two values is dropped.
.in_unique_lookup <- function(keys, values) {
  ok <- !is.na(keys) & nzchar(keys) & !is.na(values)
  pairs <- unique(data.frame(key = keys[ok], value = values[ok]))
  pairs <- pairs[!pairs$key %in% pairs$key[duplicated(pairs$key)], ]
  stats::setNames(pairs$value, pairs$key)
}
