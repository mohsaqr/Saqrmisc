# Institutions by OpenAlex institution ID, the way authors_clean() uses Scopus
# author IDs: one ID is one institution, whatever its spelling. Papers OpenAlex
# does not cover are resolved from their address text, first through the
# address-to-ID pairs OpenAlex taught on the other papers, then by the
# institution part of the address. The names left over go to
# suggest_merges() and the review, like keywords.

#' Clean institution names by OpenAlex institution ID
#'
#' @description
#' Gives each institution one name and keeps different institutions apart,
#' using OpenAlex institution IDs (with their ROR IDs where OpenAlex has them). For every paper
#' with a DOI, OpenAlex says which institutions its authors belong to.
#' Papers OpenAlex does not cover, or authors it gives only an address for,
#' are resolved from the address text:
#' \enumerate{
#'   \item an address OpenAlex has already linked to an institution on another
#'     paper gets that institution (`source = "openalex address"`);
#'   \item otherwise the institution part of the address is taken (the part
#'     naming a university, college, academy or hospital, else an institute,
#'     centre, lab or company, else the first part), and if OpenAlex knows
#'     that name, the institution behind it (`"openalex name"`);
#'   \item otherwise that part is kept as written (`"address"`).
#' }
#' Two institutions OpenAlex keeps apart that share a name get their country
#' code: `NORTHEASTERN UNIVERSITY`, `NORTHEASTERN UNIVERSITY, CN`.
#' The names from step 3 are the ones left to [suggest_merges()], which
#' never pairs two institutions OpenAlex has identified.
#'
#' @param data Your papers, one row each (a bibliometrix data frame).
#'   The result of another `clean_*()` function works too, so the cleaners
#'   chain: `M |> authors_clean(scopus = "Dlds") |> keywords_clean()`.
#' @param affiliations The column of author addresses, `;`-separated.
#'   Default `"C1"`.
#' @param doi The DOI column. Default `"DI"`.
#' @param openalex Ask OpenAlex (`TRUE`, the default)? With `FALSE` every
#'   institution comes from the address text.
#' @param replace Reviewed merges to apply: an [ask_ai()] result, a review
#'   sheet from [review_sheet()], or `OLD`/`NEW` tables, as in
#'   [keywords_clean()].
#' @param mailto An e-mail address for OpenAlex's polite pool (faster, more
#'   reliable). Sent only to OpenAlex, and only when you give it.
#'
#' @return A `saqr_institutions` object. Print it for an overview.
#'   `as.data.frame(x)` gives your data with `AU_UN` (each paper's
#'   institutions, `;`-separated, in author order) and `AU_UN_ID` (their
#'   OpenAlex IDs, `NA` where none).
#'   `what = "institutions"`: one row per institution: `institution`,
#'   `openalex_id`, `ror`, `country`, `type`, `papers`, `source`.
#'   [institutions_changes()]: one row per paper and institution: `paper`,
#'   `address` (the text it came from; empty when OpenAlex named it
#'   directly), `institution`, `openalex_id`, `source`.
#'   `summary()` counts papers and institutions per `source`; names renamed
#'   by `replace` have `source = "review"`. A review never renames an
#'   institution OpenAlex identified: a name from address text renamed to an
#'   identified institution's name joins it, ID and all.
#'
#' @section Errors:
#' Raises `saqrmisc_bad_input` for a missing or non-character column, or a
#' bad `openalex` or `mailto`; `saqrmisc_missing_package` when
#' `openalex = TRUE` without httr2. Failed OpenAlex requests warn
#' `saqrmisc_openalex_failed`; those papers fall back to their addresses.
#'
#' @references
#' Priem, J., Piwowar, H., & Orr, R. (2022). OpenAlex: A fully-open index of
#' scholarly works, authors, venues, institutions, and concepts.
#' *arXiv*. \doi{10.48550/arXiv.2205.01833}
#'
#' @examples
#' papers <- data.frame(
#'   DI = NA_character_,
#'   C1 = c("DEPARTMENT OF COMPUTING, UNIVERSITY OF TURKU, TURKU, FINLAND",
#'          "SCHOOL OF EDUCATION, UNIVERSITY OF EASTERN FINLAND, JOENSUU, FINLAND")
#' )
#' institutions <- institutions_clean(papers, openalex = FALSE)
#' institutions
#'
#' @seealso [suggest_merges()], [authors_clean()], [keywords_clean()]
#' @export
institutions_clean <- function(data,
                               affiliations = "C1",
                               doi = "DI",
                               openalex = TRUE,
                               replace = NULL,
                               mailto = NULL) {
  data <- .papers_of(data)
  if (!is.data.frame(data)) .bad_input("`data` must be a data frame")
  if (nrow(data) == 0L) .bad_input("`data` has no papers")
  .kw_check_columns(data, affiliations)
  if (!is.character(doi) || length(doi) != 1L || !doi %in% names(data)) {
    .bad_input(sprintf("`doi` must name a column of `data`; there is no column %s", format(doi)))
  }
  if (!(isTRUE(openalex) || isFALSE(openalex))) .bad_input("`openalex` must be TRUE or FALSE")
  if (!is.null(mailto) && !(is.character(mailto) && length(mailto) == 1L && grepl("@", mailto))) {
    .bad_input("`mailto` must be one e-mail address")
  }
  if (openalex) .kr_need("httr2")
  map <- .kw_replacements(replace)

  dois <- tolower(trimws(as.character(data[[doi]])))
  dois[!is.na(dois) & !nzchar(dois)] <- NA_character_
  works <- if (openalex && any(!is.na(dois))) {
    .oa_fetch_works(unique(stats::na.omit(dois)), mailto, "those papers fall back to their addresses")
  } else {
    list()
  }
  built <- .in_build(data[[affiliations]], dois, works)
  addresses <- built$addresses
  # A review renames only names from address text; one renamed to an
  # identified institution's name joins that institution.
  text <- is.na(addresses$openalex_id)
  renamed <- .kw_apply_maps(addresses$institution[text], map)
  addresses$source[text][renamed != addresses$institution[text]] <- "review"
  addresses$institution[text] <- renamed
  joins <- text & addresses$institution %in% built$known$institution
  addresses$openalex_id[joins] <- built$known$openalex_id[match(addresses$institution[joins], built$known$institution)]
  addresses <- addresses[!duplicated(addresses[c("paper", "institution")]), ]

  per_paper <- split(addresses, factor(addresses$paper, levels = seq_len(nrow(data))))
  cleaned <- data
  cleaned$AU_UN <- vapply(per_paper, \(p) .in_join(unique(p$institution)), character(1), USE.NAMES = FALSE)
  cleaned$AU_UN_ID <- vapply(per_paper, \(p) .in_join(unique(stats::na.omit(p$openalex_id))),
                             character(1), USE.NAMES = FALSE)
  rownames(addresses) <- NULL
  structure(list(data = cleaned, addresses = addresses,
                 institutions = .in_institution_table(addresses, built$known),
                 openalex = openalex, papers = nrow(data)),
            class = "saqr_institutions")
}

#' Tables from an institutions_clean() result
#'
#' @param x An [institutions_clean()] result.
#' @param row.names,optional Ignored; present for the generic.
#' @param what `"data"` (the cleaned papers, default), `"institutions"` or
#'   `"addresses"` (as [institutions_changes()]); see [institutions_clean()].
#' @param ... Ignored.
#' @return A data frame.
#' @export
as.data.frame.saqr_institutions <- function(x, row.names = NULL, optional = FALSE,
                                            what = c("data", "institutions", "addresses"), ...) {
  what <- match.arg(what)
  switch(what, data = x$data, institutions = x$institutions, addresses = x$addresses)
}

#' What each address became
#'
#' One row per paper and institution of an [institutions_clean()] result: the
#' address it came from and the institution it became.
#'
#' @param x An [institutions_clean()] result.
#' @param source Only the addresses resolved this way: `"openalex"`,
#'   `"openalex address"`, `"openalex name"`, `"review"` or `"address"`
#'   (several allowed). Default `NULL` keeps all.
#' @return A data frame with one row per paper and institution: `paper`,
#'   `address` (the text it came from; empty when OpenAlex named it
#'   directly), `institution`, `openalex_id`, `source`.
#' @section Errors:
#' Raises `saqrmisc_bad_input` when `x` is not an [institutions_clean()]
#' result or `source` is none of the above.
#' @examples
#' papers <- data.frame(
#'   C1 = c("UNIVERSITY OF OULU, OULU, FINLAND", "UNIV OULU, FINLAND"),
#'   DI = c("10.1/a", "10.1/b")
#' )
#' institutions <- institutions_clean(papers, openalex = FALSE)
#' institutions_changes(institutions)
#' @export
institutions_changes <- function(x, source = NULL) {
  if (!inherits(x, "saqr_institutions")) .bad_input("`x` must be an institutions_clean() result")
  if (is.null(source)) return(x$addresses)
  known <- c("openalex", "openalex address", "openalex name", "review", "address")
  if (!is.character(source) || !all(source %in% known)) {
    .bad_input(sprintf("`source` is one or more of %s", paste(sprintf("\"%s\"", known), collapse = ", ")))
  }
  addresses <- x$addresses[x$addresses$source %in% source, ]
  rownames(addresses) <- NULL
  addresses
}

#' Summarise an institutions_clean() result
#'
#' @param object An [institutions_clean()] result.
#' @param ... Ignored.
#' @return A data frame with one row per `source` (`openalex`,
#'   `openalex address`, `openalex name`, `review`, `address`):
#'   `institutions` (distinct names), `papers` and `links` (paper-institution
#'   pairs).
#' @export
summary.saqr_institutions <- function(object, ...) {
  a <- object$addresses
  sources <- c("openalex", "openalex address", "openalex name", "review", "address")
  out <- data.frame(
    source = sources,
    institutions = vapply(sources, \(s) length(unique(a$institution[a$source == s])), integer(1)),
    papers = vapply(sources, \(s) length(unique(a$paper[a$source == s])), integer(1)),
    links = vapply(sources, \(s) sum(a$source == s), integer(1))
  )
  rownames(out) <- NULL
  out
}

#' Print an institutions_clean() result
#'
#' @param x An [institutions_clean()] result.
#' @param ... Ignored.
#' @return `x`, invisibly.
#' @export
print.saqr_institutions <- function(x, ...) {
  s <- summary(x)
  inst <- x$institutions
  with_inst <- sum(nzchar(x$data$AU_UN) & !is.na(x$data$AU_UN))
  cat(sprintf("Institutions by OpenAlex ID%s\n", if (x$openalex) "" else " (OpenAlex not asked)"))
  cat(sprintf("  %d institutions in %d of %d papers: %d identified by OpenAlex, %d from address text only\n",
              nrow(inst), with_inst, x$papers, sum(!is.na(inst$openalex_id)), sum(is.na(inst$openalex_id))))
  cat(sprintf("  links: %s\n", paste(sprintf("%s %d", s$source, s$links), collapse = ", ")))
  cat("as.data.frame(x) gives the cleaned papers; institutions_changes(x) what each address became.\n")
  cat("suggest_merges(x) proposes merges among the names OpenAlex did not identify.\n")
  invisible(x)
}

# ---- helpers ----------------------------------------------------------------

.in_join <- function(x) if (length(x) == 0L) NA_character_ else paste(x, collapse = "; ")

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

# What OpenAlex says: every institution, and every address it linked to
# institutions, from the works' authorships.
.in_from_works <- function(works) {
  authorships <- unlist(lapply(works, \(w) w$authorships), recursive = FALSE)
  doi <- rep(names(works), vapply(works, \(w) length(w$authorships), integer(1)))
  institutions <- .kw_bind(lapply(authorships, \(a) .kw_bind(lapply(a$institutions, \(i) data.frame(
    openalex_id = sub("^https://openalex.org/", "", i$id %||% NA_character_),
    display = i$display_name %||% NA_character_,
    ror = sub("^https://ror.org/", "", i$ror %||% NA_character_),
    country = i$country_code %||% NA_character_,
    type = i$type %||% NA_character_)))))
  linked <- .kw_bind(lapply(authorships, \(a) .kw_bind(lapply(a$affiliations, \(f) {
    ids <- unlist(f$institution_ids)
    if (length(ids) != 1L) return(NULL)
    data.frame(address = f$raw_affiliation_string %||% NA_character_,
               openalex_id = sub("^https://openalex.org/", "", ids))
  }))))
  list(authorships = authorships, doi = doi,
       institutions = unique(institutions %||% data.frame(openalex_id = character(), display = character(),
                                                          ror = character(), country = character(),
                                                          type = character())),
       linked = linked %||% data.frame(address = character(), openalex_id = character()))
}

# One row per paper and institution, from OpenAlex where it names
# institutions, else from the addresses.
.in_build <- function(address_column, dois, works) {
  oa <- .in_from_works(works)
  known <- oa$institutions[!duplicated(oa$institutions$openalex_id), ]
  # OpenAlex's own disambiguation, "Amazon (United States)", goes; homonyms
  # get a country code below instead.
  known$institution <- .in_name(sub("\\s*\\([^)]*\\)$", "", known$display))
  # Homonyms OpenAlex keeps apart get their country code, and those still
  # alike a number, so that two IDs never share a name.
  twin <- known$institution %in% known$institution[duplicated(known$institution)] & !is.na(known$country)
  known$institution[twin] <- paste0(known$institution[twin], ", ", known$country[twin])
  known <- known[order(known$openalex_id), ]
  rank <- stats::ave(seq_along(known$institution), known$institution, FUN = seq_along)
  known$institution[rank > 1L] <- sprintf("%s (%d)", known$institution[rank > 1L], rank[rank > 1L])

  by_address <- .in_unique_lookup(.in_key(oa$linked$address), oa$linked$openalex_id)
  # Names learned from OpenAlex's linked addresses count only when the part
  # names a university: a faculty or institute name ("COLLEGE OF SOFTWARE")
  # recurs across universities.
  parsed <- .in_extract(oa$linked$address, tier = TRUE)
  learned <- parsed$tier == 1L
  by_name <- .in_unique_lookup(
    c(.in_key(known$display), .in_key(known$display, mangled = TRUE), .in_key(parsed$part[learned])),
    c(known$openalex_id, known$openalex_id, oa$linked$openalex_id[learned]))

  # Papers whose OpenAlex record names institutions use it; their authors
  # with only an address are resolved like the papers OpenAlex lacks.
  paper_of_doi <- match(oa$doi, dois)
  direct <- .kw_bind(lapply(seq_along(oa$authorships), \(k) {
    ids <- vapply(oa$authorships[[k]]$institutions, \(i) sub("^https://openalex.org/", "", i$id %||% NA_character_),
                  character(1))
    if (length(ids) == 0L || is.na(paper_of_doi[k])) return(NULL)
    data.frame(paper = paper_of_doi[k], address = NA_character_, openalex_id = ids, source = "openalex",
               institution = NA_character_)
  }))
  covered <- unique(direct$paper)
  loose <- unlist(lapply(seq_along(oa$authorships), \(k) {
    a <- oa$authorships[[k]]
    raw <- unlist(a$raw_affiliation_strings)
    if (length(a$institutions) > 0L || length(raw) == 0L || is.na(paper_of_doi[k]) ||
          !paper_of_doi[k] %in% covered) return(NULL)
    stats::setNames(raw, rep(paper_of_doi[k], length(raw)))
  }))
  split_addresses <- .kw_split(as.character(address_column))
  uncovered <- setdiff(seq_along(split_addresses), covered)
  # as.character() keeps the type when every paper is covered and unlist() is NULL.
  from_text <- c(loose, stats::setNames(as.character(unlist(split_addresses[uncovered])),
                                        rep(uncovered, lengths(split_addresses[uncovered]))))
  resolved <- .in_resolve(unname(from_text), by_address, by_name)
  resolved$paper <- as.integer(names(from_text))

  rows <- .kw_bind(list(direct, resolved))
  if (is.null(rows)) {
    rows <- data.frame(paper = integer(), address = character(), openalex_id = character(), source = character(),
                       institution = character())
  }
  identified <- !is.na(rows$openalex_id)
  rows$institution[identified] <- known$institution[match(rows$openalex_id[identified], known$openalex_id)]
  rows <- rows[!is.na(rows$institution) & nzchar(rows$institution), ]
  rows <- rows[!duplicated(rows[c("paper", "institution")]), ]
  rows <- rows[order(rows$paper), c("paper", "address", "institution", "openalex_id", "source")]
  list(addresses = rows, known = known)
}

.in_resolve <- function(addresses, by_address, by_name) {
  if (length(addresses) == 0L) {
    return(data.frame(address = character(), openalex_id = character(), source = character(),
                      institution = character()))
  }
  via_address <- unname(by_address[.in_key(addresses)])
  part <- .in_extract(addresses)
  via_name <- unname(by_name[.in_key(part)])
  id <- ifelse(!is.na(via_address), via_address, via_name)
  data.frame(address = addresses, openalex_id = id,
             source = ifelse(!is.na(via_address), "openalex address",
                             ifelse(!is.na(via_name), "openalex name", "address")),
             institution = ifelse(is.na(id), .in_name(part), NA_character_))
}

.in_institution_table <- function(addresses, known) {
  if (nrow(addresses) == 0L) {
    return(data.frame(institution = character(), openalex_id = character(), ror = character(),
                      country = character(), type = character(), papers = integer(), source = character()))
  }
  papers <- tapply(addresses$paper, addresses$institution, \(p) length(unique(p)))
  # A name held by identified and unidentified rows takes the identified ID.
  ids <- tapply(addresses$openalex_id, addresses$institution, \(v) c(stats::na.omit(v), NA_character_)[1L])
  name <- names(papers)
  at <- match(ids[name], known$openalex_id)
  out <- data.frame(institution = name, openalex_id = unname(ids[name]),
                    ror = known$ror[at], country = known$country[at], type = known$type[at],
                    papers = as.integer(papers[name]),
                    source = ifelse(is.na(ids[name]), "address", "openalex"))
  out <- out[order(-out$papers, out$institution), ]
  rownames(out) <- NULL
  out
}
