# Six papers covering: spelling variants of one ID (111), a homonym spelling
# under two IDs (201, 202), a "Jr." field, an HTML entity, hyphenated initials,
# a group author that breaks the name/ID alignment, and one ID listed twice.
make_authors_case <- function() {
  scopus <- data.frame(
    Authors = c("Pérez-Marín D., Kim J.", "Perez-Marin D., Kim J.",
                "Pérez-Marín D., Huff E.W., Jr.", "L&#x00F3;pez A., Cho S.-B.",
                "Group G., on behalf of the Committee", "Kim J., Kim J."),
    `Author(s) ID` = c("111;201;", "111;202;", "111;300;", "400;500;", "600;", "201;201;"),
    DOI = sprintf("10.1/%s", letters[1:6]),
    EID = sprintf("e%d", 1:6),
    check.names = FALSE
  )
  path <- tempfile(fileext = ".csv")
  utils::write.csv(scopus, path, row.names = FALSE, fileEncoding = "UTF-8")
  papers <- data.frame(
    eid = scopus$EID,
    AU = c("PREZ-MARN D;KIM J", "PEREZ-MARIN D;KIM J", "PREZ-MARN D;HUFF J",
           "L&#X00F3;PEZ A;CHO SB", "GROUP G;ON BEHALF OF THE COMMITTEE", "KIM J;KIM J"),
    DI = scopus$DOI,
    PY = c(2019, 2020, 2020, 2018, 2021, 2022),
    TI = sprintf("Paper %d", 1:6)
  )
  list(papers = papers, path = path)
}

clean_case <- function(case = make_authors_case()) {
  authors_clean(case$papers, scopus = case$path, openalex = FALSE)
}

au_of <- function(cleaned, id) {
  out <- as.data.frame(cleaned)
  out$AU[out$eid == id]
}

test_that("spelling variants of one Scopus ID become the most frequent spelling", {
  cleaned <- clean_case()
  expect_equal(au_of(cleaned, "e2"), "PÉREZ-MARÍN D;KIM J (2)")
  authors <- as.data.frame(cleaned, what = "authors")
  expect_equal(unique(authors$name[authors$scopus_id %in% "111"]), "PÉREZ-MARÍN D")
})

test_that("different IDs that share a spelling stay different people", {
  authors <- as.data.frame(clean_case(), what = "authors")
  expect_equal(unique(authors$name[authors$scopus_id %in% "201"]), "KIM J")
  expect_equal(unique(authors$name[authors$scopus_id %in% "202"]), "KIM J (2)")
  expect_true(all(authors$homonym[authors$scopus_id %in% c("201", "202")]))
})

test_that("suffixes, HTML entities and hyphenated initials are normalised", {
  cleaned <- clean_case()
  expect_equal(au_of(cleaned, "e3"), "PÉREZ-MARÍN D;HUFF EW JR")
  expect_equal(au_of(cleaned, "e4"), "LÓPEZ A;CHO SB")
})

test_that("a paper whose names and IDs do not line up is left untouched", {
  case <- make_authors_case()
  cleaned <- clean_case(case)
  out <- as.data.frame(cleaned)
  expect_equal(out$AU[out$eid == "e5"], case$papers$AU[case$papers$eid == "e5"])
  expect_true(is.na(out$AU_ID[out$eid == "e5"]))
  authors <- as.data.frame(cleaned, what = "authors")
  expect_true(all(authors$source[authors$eid == "e5"] == "none"))
})

test_that("the cleaned data keeps every row and column, one person once per paper", {
  case <- make_authors_case()
  out <- as.data.frame(clean_case(case))
  expect_equal(nrow(out), nrow(case$papers))
  expect_equal(out$TI, case$papers$TI)
  expect_equal(setdiff(names(out), names(case$papers)), "AU_ID")
  expect_equal(out$AU[out$eid == "e6"], "KIM J")
  rewritten <- !is.na(out$AU_ID)
  expect_equal(lengths(strsplit(out$AU[rewritten], ";", fixed = TRUE)),
               lengths(strsplit(out$AU_ID[rewritten], ";", fixed = TRUE)))
})

test_that("the result does not depend on the order of the papers", {
  case <- make_authors_case()
  reversed <- case
  reversed$papers <- case$papers[rev(seq_len(nrow(case$papers))), ]
  a <- as.data.frame(clean_case(case))
  b <- as.data.frame(clean_case(reversed))
  expect_equal(b$AU[match(a$eid, b$eid)], a$AU)
})

test_that("a folder of exports and renamed columns work", {
  case <- make_authors_case()
  folder <- tempfile()
  dir.create(folder)
  file.copy(case$path, file.path(folder, "export.csv"))
  renamed <- case$papers
  names(renamed) <- c("scopus_eid", "authors", "doi", "year", "title")
  cleaned <- authors_clean(renamed, scopus = folder, eid = "scopus_eid", authors = "authors",
                           doi = "doi", year = "year", openalex = FALSE)
  out <- as.data.frame(cleaned)
  expect_equal(out$authors[out$scopus_eid == "e4"], "LÓPEZ A;CHO SB")
})

test_that("the changes table lists merged and homonym people, one row each", {
  changes <- as.data.frame(clean_case(), what = "changes")
  expect_s3_class(changes, "data.frame")
  expect_named(changes, c("person_key", "name", "spellings", "papers", "scopus_ids",
                          "merged", "check", "homonym", "orcid_conflict", "split"))
  expect_true(changes$merged[changes$person_key == "scopus:111"])
  expect_equal(changes$papers[changes$person_key == "scopus:111"], 3L)
  expect_false(anyDuplicated(changes$person_key) > 0L)
})

test_that("summary counts add up and print reports them", {
  cleaned <- clean_case()
  counts <- summary(cleaned)
  expect_named(counts, c("measure", "n"))
  n <- stats::setNames(counts$n, counts$measure)
  expect_equal(n[["author rows"]], n[["rows identified by Scopus"]] +
                 n[["rows identified by OpenAlex only"]] + n[["rows unidentified"]])
  expect_equal(n[["papers rewritten"]], 5)
  expect_output(print(cleaned), "5 of 6 papers rewritten")
})

test_that("bad input raises saqrmisc_bad_input", {
  case <- make_authors_case()
  expect_error(authors_clean(list(), scopus = case$path, openalex = FALSE),
               class = "saqrmisc_bad_input")
  expect_error(authors_clean(case$papers[c("eid", "AU")], scopus = case$path, openalex = FALSE),
               class = "saqrmisc_bad_input")
  expect_error(authors_clean(case$papers, scopus = "no-such-file.csv", openalex = FALSE),
               class = "saqrmisc_bad_input")
  expect_error(authors_clean(rbind(case$papers, case$papers), scopus = case$path, openalex = FALSE),
               class = "saqrmisc_bad_input")
  bad_year <- case$papers
  bad_year$PY <- "recent"
  expect_error(authors_clean(bad_year, scopus = case$path, openalex = FALSE),
               class = "saqrmisc_bad_input")
  broken <- tempfile(fileext = ".csv")
  utils::write.csv(data.frame(Authors = "A B.", EID = "e1"), broken, row.names = FALSE)
  expect_error(authors_clean(case$papers, scopus = broken, openalex = FALSE),
               class = "saqrmisc_bad_input")
})

test_that("OpenAlex matching needs a whole, unique surname", {
  authorships <- data.frame(raw_name = c("Diana Pérez-Marín", "Alice Williams", "Bo Li", "Yan Li"),
                            openalex_id = c("A1", "A2", "A3", "A4"),
                            orcid = c("0000-0001", NA, NA, NA))
  matched <- .au_match_openalex(c("PEREZ-MARIN D", "WILLIAM A", "LI B", "NOBODY X"), authorships)
  expect_equal(matched$openalex_id, c("A1", NA, NA, NA))
  expect_equal(matched$orcid[1], "0000-0001")
})

test_that("Scopus IDs join only when their papers carry one shared ORCID", {
  rows <- data.frame(
    person_key = c("scopus:1", "scopus:2", "scopus:3", "scopus:3", "scopus:4", "scopus:5"),
    orcid = c("X", "X", "Y", "Z", "Y", NA)
  )
  # 1 and 2 share X; 3 shows two ORCIDs, so it is not joined to 4 through Y.
  expect_equal(.au_unify_by_orcid(rows),
               c("orcid:X", "orcid:X", "scopus:3", "scopus:3", "scopus:4", "scopus:5"))
})

test_that("names that look alike pass and names that do not are flagged", {
  alike <- rbind(c("MOORE RL", "MOORE ROBERT"), c("ALEXANDER JH", "ALEXANDER JAMES H"),
                 c("HUFF EW", "HUFF EW JR"), c("YAROVII A", "YAROVYI A"), c("EREKOVI A", "CEREKOVIC A"),
                 c("DEGEMMIS M", "DE GEMMIS M"), c("KRMER N", "KRÄMER NC"), c("LOPEZ SS", "SAFFON LOPEZ S"),
                 c("ROSE CP", "PENSTEIN ROSÉ C"), c("KAPOIT-DZIKIEN J", "KAPOČIUTE-DZIKIENE J"))
  unlike <- rbind(c("ENDRASS B", "LUGRIN B"), c("GAN KH", "HOON GK"), c("KIM J", "KIM S"),
                  c("MOWER E", "PROVOST EM"), c("LI J", "WU J"), c("CHUNG TD", "TRAN DC"))
  expect_true(all(.au_similar(alike[, 1], alike[, 2])))
  expect_false(any(.au_similar(unlike[, 1], unlike[, 2])))
  # Symmetric.
  expect_identical(.au_similar(alike[, 2], alike[, 1]), .au_similar(alike[, 1], alike[, 2]))
  expect_identical(.au_similar(character(), character()), logical())
})

# One Scopus ID (7) under two surnames, and Pérez-Marín (111) with lost accents.
make_unlike_case <- function() {
  scopus <- data.frame(
    Authors = c("Endrass B., Pérez-Marín D.", "Lugrin B., Perez-Marin D.", "Lugrin B."),
    `Author(s) ID` = c("7;111;", "7;111;", "7;"),
    DOI = sprintf("10.2/%s", letters[1:3]), EID = sprintf("u%d", 1:3), check.names = FALSE
  )
  path <- tempfile(fileext = ".csv")
  utils::write.csv(scopus, path, row.names = FALSE, fileEncoding = "UTF-8")
  papers <- data.frame(eid = scopus$EID, AU = c("ENDRASS B;PREZ-MARN D", "LUGRIN B;PEREZ-MARIN D", "LUGRIN B"),
                       DI = scopus$DOI, PY = c(2010, 2015, 2016))
  list(papers = papers, path = path)
}

test_that("a merge of unlike names is kept and flagged for a check", {
  case <- make_unlike_case()
  cleaned <- authors_clean(case$papers, scopus = case$path, openalex = FALSE)
  changes <- as.data.frame(cleaned, what = "changes")
  expect_true(changes$check[changes$person_key == "scopus:7"])
  expect_false(changes$check[changes$person_key == "scopus:111"])
  # The most recent spelling wins the tie between the two of 111.
  expect_equal(as.data.frame(cleaned)$AU[1], "LUGRIN B;PEREZ-MARIN D")
  # Flagged rows come first.
  expect_equal(changes$person_key[1], "scopus:7")
  expect_equal(summary(cleaned)$n[summary(cleaned)$measure == "merges to check (names unlike)"], 1L)
})

test_that("review_sheet writes the merges, check first, and refuses to overwrite", {
  case <- make_unlike_case()
  cleaned <- authors_clean(case$papers, scopus = case$path, openalex = FALSE)
  sheet <- tempfile(fileext = ".csv")
  expect_message(review_sheet(cleaned, sheet), "1 to check")
  written <- .kr_read_csv(sheet)
  expect_named(written, c("decision", "check", "name", "spellings", "papers", "scopus_ids",
                          "orcid_conflict", "merged", "person_key"))
  expect_equal(written$person_key, c("scopus:7", "scopus:111"))
  expect_true(all(is.na(written$decision)))
  expect_error(review_sheet(cleaned, sheet), class = "saqrmisc_bad_input")
  expect_error(review_sheet(cleaned, tempfile(fileext = ".txt")), class = "saqrmisc_bad_input")
  expect_error(review_sheet(list(), sheet), class = "saqrmisc_bad_input")
})

test_that("a reviewed split takes a merge apart, by spelling or by Scopus ID", {
  case <- make_unlike_case()
  sheet <- tempfile(fileext = ".csv")
  review_sheet(authors_clean(case$papers, scopus = case$path, openalex = FALSE), sheet) |>
    suppressMessages()
  edited <- .kr_read_csv(sheet)
  edited$decision <- ifelse(edited$person_key == "scopus:7", " Split", "keep")
  # Saved back as Excel does in many locales, with `;`.
  utils::write.table(edited, sheet, sep = ";", row.names = FALSE, na = "", fileEncoding = "UTF-8")
  undone <- authors_clean(case$papers, scopus = case$path, openalex = FALSE, review = sheet)
  expect_equal(as.data.frame(undone)$AU, c("ENDRASS B;PEREZ-MARIN D", "LUGRIN B;PEREZ-MARIN D", "LUGRIN B"))
  changes <- as.data.frame(undone, what = "changes")
  expect_setequal(changes$person_key[changes$split], c("scopus:7|ENDRASS B", "scopus:7|LUGRIN B"))
  expect_output(print(undone), "1 merges split by your review")

  # A person ORCID joined from two Scopus IDs splits back into those IDs.
  rows <- data.frame(person_key = c("orcid:X", "orcid:X", "scopus:9"), scopus_id = c("1", "2", "9"),
                     name_scopus = c("A B", "C B", "D E"))
  expect_equal(.au_split(rows, "orcid:X")$person_key, c("scopus:1", "scopus:2", "scopus:9"))
  expect_equal(.au_split(rows, "orcid:X")$split_from, c("orcid:X", "orcid:X", NA))
})

test_that("a review is validated and unmatched people warn", {
  case <- make_unlike_case()
  run <- \(review) authors_clean(case$papers, scopus = case$path, openalex = FALSE, review = review)
  expect_error(run(data.frame(person_key = "scopus:7", decision = "maybe")), class = "saqrmisc_bad_input")
  expect_error(run(data.frame(key = "scopus:7")), class = "saqrmisc_bad_input")
  expect_error(run("no-such-review.csv"), class = "saqrmisc_bad_input")
  expect_warning(run(data.frame(person_key = "scopus:0", decision = "split")),
                 class = "saqrmisc_review_unmatched")
  kept <- run(data.frame(person_key = "scopus:7", decision = NA))
  expect_equal(as.data.frame(kept)$AU, as.data.frame(run(NULL))$AU)
})

test_that("a live OpenAlex lookup returns ORCIDs for a known paper", {
  skip_on_cran()
  skip_if_not_installed("httr2")
  skip_if_offline("api.openalex.org")
  works <- .au_fetch_openalex(c("10.1016/j.pec.2013.05.011", "10.9999/not-a-real-doi"), mailto = NULL)
  expect_named(works, "10.1016/j.pec.2013.05.011")
  expect_equal(.au_match_openalex("BICKMORE TW", works[[1]])$orcid, "0000-0002-2676-9788")
})

test_that("authors_changes and authors_check give the changes and the merges to check", {
  case <- make_unlike_case()
  cleaned <- authors_clean(case$papers, scopus = case$path, openalex = FALSE)
    expect_identical(authors_changes(cleaned), as.data.frame(cleaned, what = "changes"))
  flagged <- authors_check(cleaned)
  expect_equal(flagged$person_key, "scopus:7")
  expect_true(all(flagged$check))
  expect_equal(nrow(flagged), sum(authors_changes(cleaned)$check))
  expect_identical(rownames(flagged), as.character(seq_len(nrow(flagged))))
  expect_true("AU_ID" %in% names(as.data.frame(cleaned)))
  expect_equal(nrow(as.data.frame(cleaned)), nrow(case$papers))
  expect_error(authors_changes(case$papers), class = "saqrmisc_bad_input")
  expect_error(authors_changes(NULL), class = "saqrmisc_bad_input")
  expect_error(authors_check(list()), class = "saqrmisc_bad_input")
})

test_that("missing EIDs, missing review keys and NA names are handled, not guessed", {
  case <- make_authors_case()
  no_eid <- case$papers
  no_eid$eid[1] <- NA
  expect_error(authors_clean(no_eid, scopus = case$path, openalex = FALSE), class = "saqrmisc_bad_input")
  expect_error(authors_clean(case$papers[0, ], scopus = case$path, openalex = FALSE), class = "saqrmisc_bad_input")
  # A blank or missing key marked split splits nobody, unidentified authors included.
  plain <- as.data.frame(clean_case(case))
  kept <- authors_clean(case$papers, scopus = case$path, openalex = FALSE,
                        review = data.frame(person_key = c(NA, " "), decision = "split"))
  expect_identical(as.data.frame(kept)$AU, plain$AU)
  expect_identical(as.data.frame(kept)$AU_ID, plain$AU_ID)
  expect_true(is.na(.au_decode_entities(NA_character_)))
  expect_identical(.au_decode_entities(c(NA, "L&#x00F3;pez")), c(NA, "López"))
})

test_that("a shared given name is no shared surname", {
  expect_false(.au_similar("KIM JOHN", "PARK JOHN"))
  expect_false(.au_similar("MOORE ROBERT L", "SMITH ROBERT L"))
  expect_true(.au_similar("LOPEZ SS", "SAFFON LOPEZ S"))
})

test_that("papers with no identified author are left untouched", {
  case <- make_authors_case()
  only_group <- case$papers[case$papers$eid == "e5", ]
  out <- as.data.frame(authors_clean(only_group, scopus = case$path, openalex = FALSE))
  expect_identical(out$AU, only_group$AU)
  expect_true(is.na(out$AU_ID))
})

test_that("the cleaners chain: each takes the result of the one before", {
  case <- make_authors_case()
  papers <- transform(case$papers, DE = "CHATBOTS; CHAT-BOT", C1 = "DEPT X, UNIVERSITY OF TURKU, FINLAND")
  chained <- papers |>
    authors_clean(scopus = case$path, openalex = FALSE) |>
    keywords_clean() |>
    institutions_clean(openalex = FALSE)
  out <- as.data.frame(chained)
  expect_identical(out$AU, as.data.frame(clean_case(case))$AU)
  expect_identical(out$DE, as.data.frame(keywords_clean(papers))$DE)
  expect_identical(unique(lengths(strsplit(out$DE, ";"))), 1L)
  expect_identical(unique(out$AU_UN), "UNIVERSITY OF TURKU")
  expect_true("AU_ID" %in% names(out))
})

test_that("a spent OpenAlex budget fails at once instead of sleeping until it resets", {
  skip_if_not_installed("httr2")
  spent <- \(req) httr2::response(status_code = 429L, headers = list(`retry-after` = "46644"))
  elapsed <- system.time(
    reply <- httr2::with_mocked_responses(spent,
      .au_openalex_get("https://api.openalex.org/works", list(filter = "doi:10.1/x")))
  )[["elapsed"]]
  expect_lt(elapsed, 5)
  expect_s3_class(reply, "error")
  expect_match(conditionMessage(reply), "limit reached, try again in 13.0 hours")
  expect_warning(
    httr2::with_mocked_responses(spent, .oa_fetch_works("10.1/x", NULL, "fallback")),
    class = "saqrmisc_openalex_failed")
})

test_that(".au_wait reads seconds and ignores what it cannot read", {
  skip_if_not_installed("httr2")
  wait_of <- \(value) .au_wait(httr2::response(429L, headers = if (is.null(value)) list() else list(`retry-after` = value)))
  expect_identical(wait_of("30"), 30)
  expect_identical(wait_of(NULL), 0)
  expect_identical(wait_of("Wed, 21 Oct 2026 07:28:00 GMT"), 0)
})

test_that("with every OpenAlex request failing, institutions_clean falls back to addresses", {
  skip_if_not_installed("httr2")
  spent <- \(req) httr2::response(status_code = 429L, headers = list(`retry-after` = "46644"))
  papers <- data.frame(DI = c("10.1/a", "10.1/b"), C1 = c("[KIM J] UNIV OULU, FINLAND", "[LEE A] UNIV TURKU, FINLAND"),
                       AU = c("KIM J", "LEE A"), TI = c("A", "B"))
  institutions <- expect_warning(
    httr2::with_mocked_responses(spent, institutions_clean(papers)),
    class = "saqrmisc_openalex_failed")
  expect_s3_class(institutions, "saqr_institutions")
})

test_that("without scopus, the author IDs are read from data and give the same people", {
  scopus_file <- tempfile(fileext = ".csv")
  utils::write.csv(data.frame(
    Authors = c("Kim J., Lee A.", "Kim J., Lee A., Ruiz M."),
    `Author(s) ID` = c("201;300;", "202;300;400;"),
    DOI = c("10.1/a", "10.1/b"), EID = c("2-s2.0-1", "2-s2.0-2"),
    check.names = FALSE), scopus_file, row.names = FALSE, fileEncoding = "UTF-8")
  papers <- data.frame(eid = c("2-s2.0-1", "2-s2.0-2"), AU = c("KIM J;LEE A", "KIM J;LEE AB;RUIZ M"),
                       DI = c("10.1/a", "10.1/b"), PY = c(2019, 2020))
  from_files <- authors_clean(papers, scopus = scopus_file, openalex = FALSE)
  with_ids <- transform(papers, `Author(s) ID` = c("201;300;", "202;300;400;"), check.names = FALSE)
  from_data <- authors_clean(with_ids, openalex = FALSE)
  expect_identical(as.data.frame(from_data)$AU_ID, as.data.frame(from_files)$AU_ID)
  expect_identical(as.data.frame(from_data)$AU, c("KIM J;LEE AB", "KIM J (2);LEE AB;RUIZ M"))
  expect_identical(authors_changes(from_data)$person_key, authors_changes(from_files)$person_key)
  # a renamed column is named with `author_ids`
  renamed <- transform(papers, ids = c("201;300;", "202;300;400;"))
  expect_identical(as.data.frame(authors_clean(renamed, author_ids = "ids", openalex = FALSE))$AU_ID,
                   as.data.frame(from_files)$AU_ID)
})

test_that("without scopus or an ID column, authors_clean says to pass scopus", {
  papers <- data.frame(eid = "2-s2.0-1", AU = "KIM J", DI = "10.1/a", PY = 2019)
  expect_error(authors_clean(papers, openalex = FALSE), class = "saqrmisc_bad_input")
  expect_error(authors_clean(papers, author_ids = c("a", "b"), openalex = FALSE), class = "saqrmisc_bad_input")
})

test_that("IDs read from data line up with names holding HTML entities", {
  papers <- data.frame(eid = "2-s2.0-1", AU = "L&#X00F3;PEZ A;CARMONA J", DI = "10.1/a", PY = 2019,
                       `Author(s) ID` = "11;12;", check.names = FALSE)
  cleaned <- authors_clean(papers, openalex = FALSE)
  expect_identical(as.data.frame(cleaned)$AU_ID, "scopus:11;scopus:12")
  expect_identical(as.data.frame(cleaned)$AU, "LÓPEZ A;CARMONA J")
})
