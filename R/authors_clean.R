# Author disambiguation for bibliometric data by Scopus author ID. One Scopus
# ID is one person, so its spellings become one. Nothing else merges unless a
# person decides it: names never merge or split anyone.

#' Clean author names by Scopus author ID
#'
#' @description
#' Gives each person one spelling in the author column of a bibliometrix data
#' frame, and returns the data frame ready for bibliometrix.
#'
#' * `level = "confirmed"` (default): all spellings of one Scopus author ID
#'   become one. Nothing else is used, and no network is needed.
#' * `level = "needs_decision"`: the confirmed merges, plus the merges you
#'   accepted in a decisions sheet. The first run asks OpenAlex, by DOI, for
#'   each author's ORCID and writes the Scopus IDs that share one ORCID to
#'   `decisions`; nothing from it is applied yet. Write `merge` in its
#'   `decision` column for the right ones and run again: the sheet is then
#'   read, without asking OpenAlex.
#'
#' A person's spelling is the one used most often in `data`, then the most
#' recent, then the longest. Authors who were not merged keep their spelling,
#' and a paper with no merged author is left as it was. Different Scopus IDs
#' that share a spelling stay as they are: they are not numbered or split.
#'
#' The Scopus author IDs are matched to the papers by record ID (the Scopus
#' EID), then by position: the *i*-th author of a paper takes its *i*-th ID. A
#' paper whose record is not in `ids`, or whose number of authors and of IDs
#' differ, is left untouched; the message names the reasons.
#'
#' @param data A bibliometrix data frame, one row per paper. The result of
#'   another cleaner works too: `M |> authors_clean(ids = "scopus_exports") |>
#'   keywords_clean()`.
#' @param ids The Scopus author IDs: paths to raw Scopus CSV exports, a folder
#'   holding them, or a data frame. Each needs the columns `EID` and
#'   `Author(s) ID`; `Authors` is used, when present, to match names on
#'   OpenAlex. bibliometrix drops the IDs, so they must be given here.
#' @param record_id Name of the column in `data` holding the Scopus EID
#'   (`"2-s2.0-..."`). Default `"eid"`.
#' @param authors Name of the author column (`;`-separated). Default `"AU"`.
#' @param full_names Name of the full-name column, rewritten alongside
#'   `authors` on papers where it has as many names. Default `"AF"`, skipped
#'   when `data` has no such column; `NULL` leaves it alone.
#' @param year Name of the publication-year column, used to break ties between
#'   spellings. Default `"PY"`, skipped when absent.
#' @param level `"confirmed"` (default) or `"needs_decision"`; see above.
#' @param decisions With `level = "needs_decision"`: the decisions sheet, a
#'   path ending in `.csv` or `.xlsx`. Written when it does not exist, read
#'   when it does.
#' @param doi Name of the DOI column, used to ask OpenAlex. Default `"DI"`.
#' @param mailto Optional e-mail address sent to OpenAlex to use its polite
#'   pool. Default `NULL` sends none.
#' @param output `"minimal"` (default) returns `data` with `authors` and
#'   `full_names` rewritten and nothing added. `"all"` also adds
#'   `AU_ORIGINAL` (and `AF_ORIGINAL`), the columns as they came in (kept, not
#'   replaced, when `data` already has them); `AU_ID`, the Scopus IDs in the
#'   order of the authors (`NA` for an untouched paper); and `check_authors`,
#'   `"YES"` for a paper with an author whose merged spellings do not look like
#'   one name (`"ENDRASS B"` and `"LUGRIN B"` under one Scopus ID), else `"NO"`.
#'
#' @return `data`, same class, rows and columns, with the author column (and
#'   the full-name column) rewritten; with `output = "all"`, plus the columns
#'   described under `output`. A message reports the counts, and why any
#'   paper was left untouched.
#'
#' @section Errors:
#' Raises `saqrmisc_bad_input` when `data` lacks a named column or repeats a
#' record ID, `ids` names no readable export or lacks `EID` / `Author(s) ID`,
#' no record of `data` is in `ids`, `decisions` is given with
#' `level = "confirmed"` or missing with `"needs_decision"`, or the decisions
#' sheet lacks `scopus_ids` / `decision` or holds a decision other than empty,
#' `merge` or `no`; `saqrmisc_missing_package` when OpenAlex is needed without
#' httr2, or for an `.xlsx` sheet without writexl / readxl. Warns
#' `saqrmisc_review_unmatched` for decided Scopus IDs not in `data`, and
#' `saqrmisc_openalex_failed` for failed OpenAlex requests.
#'
#' @references
#' Baas, J., Schotten, M., Plume, A., Côté, G., & Karimi, R. (2020). Scopus as
#' a curated, high-quality bibliometric data source for academic research in
#' quantitative science studies. *Quantitative Science Studies*, 1(1), 377-386.
#' \doi{10.1162/qss_a_00019}
#'
#' Priem, J., Piwowar, H., & Orr, R. (2022). OpenAlex: A fully-open index of
#' scholarly works, authors, venues, institutions, and concepts.
#' arXiv:2205.01833.
#'
#' @examples
#' scopus <- data.frame(
#'   EID = c("2-s2.0-1", "2-s2.0-2", "2-s2.0-3"),
#'   `Author(s) ID` = c("111;201;", "111;202;", "111;"),
#'   check.names = FALSE
#' )
#' M <- data.frame(
#'   eid = c("2-s2.0-1", "2-s2.0-2", "2-s2.0-3"),
#'   AU = c("PREZ-MARN D;KIM J", "PEREZ-MARIN D;KIM J", "PEREZ-MARIN D"),
#'   PY = c(2019, 2020, 2021)
#' )
#' authors_clean(M, ids = scopus)
#' authors_clean(M, ids = scopus, output = "all")
#'
#' @export
authors_clean <- function(data,
                          ids,
                          record_id = "eid",
                          authors = "AU",
                          full_names = "AF",
                          year = "PY",
                          level = c("confirmed", "needs_decision"),
                          decisions = NULL,
                          doi = "DI",
                          mailto = NULL,
                          output = c("minimal", "all")) {
  data <- .papers_of(data)
  level <- match.arg(level)
  output <- match.arg(output)
  if (!is.data.frame(data)) .bad_input("`data` must be a data frame")
  if (nrow(data) == 0L) .bad_input("`data` has no papers")
  if (missing(ids)) .bad_input("`ids` is needed: the Scopus CSV exports (or a data frame) holding `EID` and `Author(s) ID`")
  .au_need_column(data, record_id, "record_id")
  .au_need_column(data, authors, "authors")
  full_names <- .au_optional_column(data, full_names, "full_names", missing(full_names))
  year <- .au_optional_column(data, year, "year", missing(year))
  record <- trimws(as.character(data[[record_id]]))
  if (anyNA(record) || !all(nzchar(record))) .bad_input(sprintf("`data$%s` has a missing record ID", record_id))
  if (anyDuplicated(record) > 0L) .bad_input(sprintf("`data$%s` repeats a record ID; each paper must appear once", record_id))
  years <- rep(NA_integer_, nrow(data))
  if (!is.null(year)) {
    year_text <- trimws(as.character(data[[year]]))
    if (!all(is.na(year_text) | grepl("^[0-9]{1,4}$", year_text))) .bad_input(sprintf("`data$%s` must hold years", year))
    years <- as.integer(year_text)
  }
  if (level == "confirmed" && !is.null(decisions)) {
    .bad_input("`decisions` is read only with `level = \"needs_decision\"`")
  }
  if (level == "needs_decision" && !(is.character(decisions) && length(decisions) == 1L &&
                                     grepl("\\.(csv|xlsx)$", decisions, ignore.case = TRUE))) {
    .bad_input("`level = \"needs_decision\"` needs `decisions`, a path ending in .csv or .xlsx")
  }

  id_table <- .au_read_ids(ids)
  matched <- .au_match_papers(data, record, authors, full_names, years, id_table)
  if (!any(matched$status != "record not in ids")) {
    .bad_input(sprintf("no record of `data$%s` is in `ids`; is `record_id` the Scopus EID?", record_id))
  }
  entries <- matched$entries

  ask_openalex <- level == "needs_decision" && !file.exists(decisions)
  groups <- if (level == "needs_decision" && !ask_openalex) .au_read_decisions(decisions, entries$scopus_id) else list()
  entries$person <- .au_group_of(entries$scopus_id, groups)
  entries$basis <- ifelse(startsWith(entries$person, "decided:"), "decided", "scopus")

  entries$new_name <- unname(.au_pick(entries$person, entries$name, entries$year)[entries$person])
  picked_full <- .au_pick(entries$person, entries$full, entries$year)
  entries$new_full <- ifelse(is.na(entries$full), NA_character_, unname(picked_full[entries$person]))
  flagged <- .au_unlike_people(entries)
  entries$check <- entries$person %in% flagged

  cleaned <- data
  au_changed <- entries$new_name != entries$name
  af_changed <- !is.na(entries$full) & entries$new_full != entries$full
  cleaned[[authors]] <- .au_rewrite(data[[authors]], entries, au_changed, "new_name")
  if (!is.null(full_names)) cleaned[[full_names]] <- .au_rewrite(data[[full_names]], entries, af_changed, "new_full")
  check_paper <- seq_len(nrow(data)) %in% entries$paper[entries$check]

  if (ask_openalex) {
    .au_write_candidates(entries, data, doi, matched$match_names(), mailto, decisions)
  }
  if (output == "all") {
    cleaned <- .au_add_extras(cleaned, data, authors, full_names, matched, check_paper)
  }
  .au_report(entries, au_changed, af_changed, matched$status, check_paper, full_names)
  cleaned
}

# ---- helpers ----------------------------------------------------------------

.au_suffixes <- c("JR", "SR", "II", "III", "IV")
.au_batch_size <- 50L
.au_timeout <- 60

.au_need_column <- function(data, column, argument) {
  if (!(is.character(column) && length(column) == 1L && !is.na(column))) {
    .bad_input(sprintf("`%s` must be one column name", argument))
  }
  if (!column %in% names(data)) .bad_input(sprintf("`data` has no column `%s` (`%s`)", column, argument))
}

# A column that may be absent: NULL leaves it out, the default is skipped when
# `data` lacks it, a name the user gave must be there.
.au_optional_column <- function(data, column, argument, is_default) {
  if (is.null(column)) return(NULL)
  if (is_default && !column %in% names(data)) return(NULL)
  .au_need_column(data, column, argument)
  column
}

.au_scopus_files <- function(ids) {
  files <- if (length(ids) == 1L && dir.exists(ids)) {
    list.files(ids, pattern = "\\.csv$", full.names = TRUE, ignore.case = TRUE)
  } else {
    ids
  }
  if (length(files) == 0L || !all(file.exists(files))) .bad_input("`ids` names no existing Scopus CSV export")
  files
}

# The ID table: one row per record, `EID`, `author_ids` and `Authors` (NA when
# the export has no names). Read as text: an ID is too long for a double.
.au_read_ids <- function(ids) {
  needed <- c("EID", "Author(s) ID")
  tables <- if (is.data.frame(ids)) {
    list(ids)
  } else if (is.character(ids) && length(ids) > 0L && !anyNA(ids)) {
    lapply(.au_scopus_files(ids), \(f) utils::read.csv(f, check.names = FALSE, fileEncoding = "UTF-8-BOM",
                                                       colClasses = "character", na.strings = ""))
  } else {
    .bad_input("`ids` must be paths to Scopus CSV exports, a folder holding them, or a data frame")
  }
  tables <- lapply(tables, \(x) {
    absent <- setdiff(needed, names(x))
    if (length(absent) > 0L) .bad_input(sprintf("`ids` has no column(s) %s", paste(absent, collapse = ", ")))
    data.frame(EID = trimws(as.character(x[["EID"]])),
               author_ids = as.character(x[["Author(s) ID"]]),
               Authors = if ("Authors" %in% names(x)) as.character(x[["Authors"]]) else NA_character_,
               stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, tables)
  out[!is.na(out$EID) & !duplicated(out$EID), ]
}

# Splits a `;`-separated name field. The `;` that ends an HTML entity
# (L&#X00F3;PEZ) is not a separator, so entities are protected first.
.au_split_field <- function(x) {
  protected <- gsub("(&#[xX]?[0-9A-Fa-f]+);", "\\1\001", as.character(x))
  parts <- strsplit(protected, ";", fixed = TRUE)
  parts[is.na(x)] <- list(character())
  .au_relist(trimws(gsub("\001", ";", unlist(parts), fixed = TRUE)), lengths(parts))
}

# A flat vector cut back into one element per record, `sizes` long each.
# Cleaning the flat vector once is far faster than a loop over millions.
.au_relist <- function(flat, sizes) {
  unname(split(as.character(flat), factor(rep(seq_along(sizes), sizes), levels = seq_along(sizes))))
}

.au_split_ids <- function(ids) {
  parts <- strsplit(sub(";\\s*$", "", ids), ";", fixed = TRUE)
  parts[is.na(ids)] <- list(character())
  .au_relist(trimws(unlist(parts)), lengths(parts))
}

# Links each paper to its Scopus record and its authors to their IDs. Returns
# the status of each paper, one row per author of the papers that line up
# (`entries`), and a function giving the names to match those authors on
# OpenAlex (built only when OpenAlex is asked: it is a per-paper loop).
.au_match_papers <- function(data, record, authors, full_names, years, id_table) {
  hit <- match(record, id_table$EID)
  au_list <- .au_split_field(data[[authors]])
  id_list <- .au_split_ids(id_table$author_ids[hit])
  # Vectorised over all IDs at once: a per-paper loop is slow on millions.
  id_paper <- rep(seq_along(id_list), lengths(id_list))
  numeric_ids <- tabulate(id_paper[!grepl("^[0-9]+$", unlist(id_list))], nbins = length(id_list)) == 0L
  status <- ifelse(is.na(hit), "record not in ids",
            ifelse(lengths(au_list) != lengths(id_list), "author and ID counts differ",
            ifelse(!numeric_ids, "an author ID is not numeric", "ok")))

  ok <- which(status == "ok")
  n <- lengths(au_list[ok])
  full <- rep(NA_character_, sum(n))
  if (!is.null(full_names)) {
    af_list <- .au_split_field(data[[full_names]])[ok]
    af_aligned <- lengths(af_list) == n
    full[rep(af_aligned, n)] <- unlist(af_list[af_aligned])
  }

  # OpenAlex matching reads the export's own names (with their accents) when
  # they line up with the IDs, else the names in `data`.
  match_names <- \() {
    scopus <- as.character(id_table$Authors[hit[ok]])
    scopus_names <- lapply(.au_split_names(ifelse(is.na(scopus), "", scopus)), .au_normalise)
    unlist(Map(\(s, a) if (length(s) == length(a)) s else a, scopus_names, au_list[ok])) %||% character()
  }

  entries <- data.frame(
    paper = rep(ok, n),
    position = sequence(n),
    name = unlist(au_list[ok]) %||% character(),
    full = full,
    scopus_id = unlist(id_list[ok]) %||% character(),
    year = rep(years[ok], n),
    stringsAsFactors = FALSE
  )
  list(status = status, entries = entries, id_list = id_list, match_names = match_names)
}

# The person of each entry: its decided group when it has one, else its ID.
.au_group_of <- function(scopus_id, groups) {
  if (length(groups) == 0L) return(scopus_id)
  member <- rep(seq_along(groups), lengths(groups))
  at <- match(scopus_id, unlist(groups))
  ifelse(is.na(at), scopus_id, paste0("decided:", vapply(groups, `[[`, character(1), 1L)[member[at]]))
}

# For each person, the value used most often, then the most recent, then the
# longest, then the first alphabetically; named by person.
.au_pick <- function(person, value, year) {
  keep <- !is.na(value) & nzchar(value)
  if (!any(keep)) return(stats::setNames(character(), character()))
  person <- person[keep]
  value <- value[keep]
  year <- ifelse(is.na(year[keep]), -Inf, year[keep])
  # One row per distinct (person, spelling): how often, and how recently.
  pair_id <- match(paste(person, value, sep = "\r"), paste(person, value, sep = "\r"))
  first <- which(pair_id == seq_along(pair_id))
  by_recency <- order(pair_id, -year, method = "radix")
  picks <- data.frame(person = person[first], value = value[first],
                      n = tabulate(pair_id, nbins = length(pair_id))[first],
                      latest = year[by_recency][!duplicated(pair_id[by_recency])],
                      stringsAsFactors = FALSE)
  # Only people with several spellings need a choice.
  several <- picks$person %in% picks$person[duplicated(picks$person)]
  choose <- picks[several, ]
  choose$lost <- .au_letters_lost(choose$person, choose$value)
  choose <- choose[order(choose$person, choose$lost, -choose$n, -choose$latest, -nchar(choose$value),
                         choose$value, method = "radix"), ]
  choose <- choose[!duplicated(choose$person), ]
  stats::setNames(c(picks$value[!several], choose$value), c(picks$person[!several], choose$person))
}

# TRUE for a spelling that is another spelling of the same person with letters
# deleted, as bibliometrix deletes accented ones (YALN N for YALCIN ON, KAI Z
# for KACIC Z), or with an initial dropped (ZHIGALOV A for ZHIGALOV AY).
.au_letters_lost <- function(person, value) {
  spelled <- unique(data.frame(person = person, value = value, stringsAsFactors = FALSE))
  several <- spelled[spelled$person %in% spelled$person[duplicated(spelled$person)], ]
  if (nrow(several) == 0L) return(rep(FALSE, length(value)))
  pairs <- merge(several, several, by = "person", suffixes = c("", "_other"))
  pairs <- pairs[nchar(pairs$value) < nchar(pairs$value_other), ]
  lost <- pairs[.au_is_subsequence(pairs$value, pairs$value_other), c("person", "value")]
  paste(person, value, sep = "\r") %in% paste(lost$person, lost$value, sep = "\r")
}

# Are the characters of `short` found in `long`, in order?
.au_is_subsequence <- function(short, long) {
  patterns <- vapply(strsplit(short, ""), \(chars) paste0("\\Q", chars, "\\E", collapse = ".*"), character(1))
  as.logical(mapply(grepl, patterns, long, MoreArgs = list(perl = TRUE), USE.NAMES = FALSE))
}

# People one of whose spellings does not look like the spelling they now get.
.au_unlike_people <- function(entries) {
  pairs <- unique(entries[entries$name != entries$new_name, c("person", "name", "new_name")])
  if (nrow(pairs) == 0L) return(character())
  # A spelling with letters deleted (KAI Z for KACIC Z) is the same name.
  alike <- .au_is_subsequence(pairs$name, pairs$new_name)
  alike[!alike] <- .au_similar(pairs$name[!alike], pairs$new_name[!alike])
  unique(pairs$person[!alike])
}

# The field with the changed papers rebuilt from their entries; other papers
# keep their text exactly.
.au_rewrite <- function(field, entries, changed, column) {
  papers <- unique(entries$paper[changed])
  if (length(papers) == 0L) return(field)
  rebuilt <- entries[entries$paper %in% papers, ]
  rebuilt <- rebuilt[order(rebuilt$paper, rebuilt$position), ]
  text <- vapply(split(rebuilt[[column]], rebuilt$paper), paste, character(1), collapse = ";")
  field <- as.character(field)
  field[as.integer(names(text))] <- unname(text)
  field
}

.au_add_extras <- function(cleaned, data, authors, full_names, matched, check_paper) {
  if (!"AU_ORIGINAL" %in% names(cleaned)) cleaned$AU_ORIGINAL <- data[[authors]]
  if (!is.null(full_names) && !"AF_ORIGINAL" %in% names(cleaned)) cleaned$AF_ORIGINAL <- data[[full_names]]
  au_id <- vapply(matched$id_list, paste, character(1), collapse = ";")
  cleaned$AU_ID <- ifelse(matched$status == "ok", au_id, NA_character_)
  cleaned$check_authors <- ifelse(check_paper, "YES", "NO")
  cleaned
}

.au_report <- function(entries, au_changed, af_changed, status, check_paper, full_names) {
  merged <- length(unique(entries$person[au_changed | af_changed]))
  decided <- length(unique(entries$person[entries$basis == "decided"]))
  skipped <- table(status[status != "ok"])
  message(sprintf(
    "authors_clean: %d people given one spelling (%d by decision); %d papers changed%s; %d papers to check; %d papers untouched%s",
    merged, decided, length(unique(entries$paper[au_changed])),
    if (is.null(full_names)) "" else sprintf(" (%d in %s)", length(unique(entries$paper[af_changed])), full_names),
    sum(check_paper), sum(skipped),
    if (length(skipped) > 0L) sprintf(" (%s)", paste(sprintf("%s: %d", names(skipped), skipped), collapse = ", ")) else ""))
}

# ---- decisions --------------------------------------------------------------

# The merged groups of a decisions sheet: the Scopus IDs of each row marked
# `merge`, rows that share an ID joined into one group.
.au_read_decisions <- function(path, known_ids) {
  sheet <- if (grepl("\\.xlsx$", path, ignore.case = TRUE)) {
    .kr_need("readxl")
    as.data.frame(readxl::read_excel(path, sheet = 1L, col_types = "text"))
  } else {
    .kr_read_csv(path)
  }
  if (!all(c("scopus_ids", "decision") %in% names(sheet))) {
    .bad_input("the decisions sheet needs the columns scopus_ids and decision, as authors_clean() writes them")
  }
  decision <- tolower(trimws(as.character(sheet$decision)))
  decision[is.na(decision)] <- ""
  odd <- setdiff(decision, c("", "merge", "no"))
  if (length(odd) > 0L) {
    .bad_input(sprintf("`decision` must be empty, merge or no, not %s", paste(sprintf("\"%s\"", odd), collapse = ", ")))
  }
  groups <- Filter(\(g) length(g) > 1L, lapply(.au_split_ids(as.character(sheet$scopus_ids)[decision == "merge"]), unique))
  absent <- setdiff(unlist(groups), known_ids)
  if (length(absent) > 0L) {
    warning(warningCondition(
      sprintf("%d decided Scopus ID(s) are not in this data: %s", length(absent),
              paste(utils::head(absent, 5L), collapse = ", ")),
      class = "saqrmisc_review_unmatched", call = NULL))
  }
  # Rows that share an ID are one person: fold each row into the groups it touches.
  merged <- Reduce(\(sets, g) {
    touches <- vapply(sets, \(s) any(g %in% s), logical(1))
    c(sets[!touches], list(unique(c(unlist(sets[touches]), g))))
  }, groups, list())
  lapply(merged, sort)
}

# Asks OpenAlex for each author's ORCID and writes the Scopus IDs that share
# one ORCID to the decisions sheet, `decision` left empty.
.au_write_candidates <- function(entries, data, doi, match_names, mailto, path) {
  csv <- .kr_sheet_file(path)
  .kr_need("httr2")
  .au_need_column(data, doi, "doi")
  entries$doi <- tolower(trimws(as.character(data[[doi]])))[entries$paper]
  entries$orcid <- .au_orcids(entries, match_names, mailto)
  claims <- unique(entries[!is.na(entries$orcid), c("scopus_id", "orcid")])
  single <- claims[!claims$scopus_id %in% claims$scopus_id[duplicated(claims$scopus_id)], ]
  shared <- single[single$orcid %in% single$orcid[duplicated(single$orcid)], ]
  by_orcid <- split(shared$scopus_id, shared$orcid)
  rows <- lapply(names(by_orcid), \(orcid) {
    ids <- sort(unique(by_orcid[[orcid]]))
    own <- entries[entries$scopus_id %in% ids, ]
    names_seen <- sort(unique(own$name))
    best <- .au_pick(rep("x", nrow(own)), own$name, own$year)[["x"]]
    data.frame(decision = NA_character_, orcid = orcid, scopus_ids = paste(ids, collapse = ";"),
               names = paste(names_seen, collapse = " | "), papers = length(unique(own$paper)),
               names_alike = if (all(.au_similar(names_seen, rep(best, length(names_seen))))) "YES" else "NO",
               stringsAsFactors = FALSE)
  })
  sheet <- do.call(rbind, rows) %||%
    data.frame(decision = character(), orcid = character(), scopus_ids = character(), names = character(),
               papers = integer(), names_alike = character())
  sheet <- sheet[order(sheet$names_alike, -sheet$papers, sheet$orcid), ]
  if (csv) .kr_write_csv(sheet, path) else writexl::write_xlsx(list(decisions = sheet), path)
  message(sprintf("%s: %d Scopus ID group(s) share an ORCID. Write merge in decision for the right ones, then run again with the same `decisions`.",
                  basename(path), nrow(sheet)))
}

# The ORCID of each entry, from OpenAlex by DOI; NA where it has none.
.au_orcids <- function(entries, match_names, mailto) {
  orcid <- rep(NA_character_, nrow(entries))
  asked <- which(!is.na(entries$doi) & nzchar(entries$doi))
  if (length(asked) == 0L) return(orcid)
  by_paper <- split(asked, entries$paper[asked])
  paper_doi <- vapply(by_paper, \(idx) entries$doi[idx[1L]], character(1))
  works <- .au_fetch_openalex(unique(paper_doi), mailto)
  found <- paper_doi %in% names(works)
  message(sprintf("OpenAlex: %d of %d paper(s) found", sum(found), length(found)))
  matched <- do.call(rbind, Map(\(idx, d) cbind(row = idx, .au_match_openalex(match_names[idx], works[[d]])),
                                by_paper[found], paper_doi[found]))
  if (!is.null(matched)) orcid[matched$row] <- matched$orcid
  orcid
}

# ---- names --------------------------------------------------------------------

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

# Scopus's comma-separated names; a generational suffix is its own comma
# field in Scopus, so it is glued back on.
.au_split_names <- function(authors) {
  lapply(strsplit(.au_decode_entities(authors), ", ", fixed = TRUE), \(parts) {
    if (length(parts) == 0L) return(character())
    is_suffix <- .au_normalise(parts) %in% .au_suffixes
    unname(vapply(split(parts, cumsum(!is_suffix)), paste, character(1), collapse = " "))
  })
}

# ---- OpenAlex -----------------------------------------------------------------

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

# ---- name likeness -----------------------------------------------------------

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
