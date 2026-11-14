# Seven papers: Pérez-Marín (111) under three spellings, one with letters lost;
# two people spelled KIM J (201, 202); an HTML entity whose `;` is no
# separator; a group author that breaks the count; a record missing from the
# IDs; one ID (7) under two surnames.
make_case <- function() {
  ids <- data.frame(
    EID = sprintf("e%d", 1:6),
    `Author(s) ID` = c("111;201;", "111;202;", "111;300;", "400;500;", "600;", "7;111;"),
    Authors = c("Pérez-Marín D., Kim J.", "Perez-Marin D., Kim J.", "Pérez-Marín D., Huff E.W., Jr.",
                "L&#x00F3;pez A., Cho S.-B.", "Group G., on behalf of the Committee", "Endrass B., Pérez-Marín D."),
    check.names = FALSE
  )
  papers <- data.frame(
    eid = sprintf("e%d", 1:7),
    AU = c("PREZ-MARN D;KIM J", "PEREZ-MARIN D;KIM J", "PEREZ-MARIN D;HUFF EW",
           "L&#X00F3;PEZ A;CHO SB", "GROUP G;ON BEHALF OF THE COMMITTEE", "ENDRASS B;PREZ-MARN D", "NOBODY N"),
    PY = c(2019, 2020, 2021, 2018, 2021, 2010, 2022),
    TI = sprintf("Paper %d", 1:7)
  )
  list(papers = papers, ids = ids)
}

# A second paper by ID 7 under its later surname.
make_unlike_case <- function() {
  case <- make_case()
  case$ids <- rbind(case$ids, data.frame(EID = "e8", `Author(s) ID` = "7;", Authors = "Lugrin B.",
                                         check.names = FALSE))
  case$papers <- rbind(case$papers, data.frame(eid = "e8", AU = "LUGRIN B", PY = 2016, TI = "Paper 8"))
  case$ids <- rbind(case$ids, data.frame(EID = "e9", `Author(s) ID` = "7;", Authors = "Lugrin B.",
                                         check.names = FALSE))
  case$papers <- rbind(case$papers, data.frame(eid = "e9", AU = "LUGRIN B", PY = 2017, TI = "Paper 9"))
  case
}

quiet_clean <- function(...) suppressMessages(authors_clean(...))

test_that("one Scopus ID gets one spelling, and the result is the data frame", {
  case <- make_case()
  out <- quiet_clean(case$papers, ids = case$ids)
  expect_s3_class(out, "data.frame")
  expect_identical(names(out), names(case$papers))
  expect_identical(nrow(out), nrow(case$papers))
  # 111 appears as PEREZ-MARIN D twice and PREZ-MARN D twice; the spelling
  # with letters lost is never picked.
  expect_identical(out$AU[1:3], c("PEREZ-MARIN D;KIM J", "PEREZ-MARIN D;KIM J", "PEREZ-MARIN D;HUFF EW"))
  expect_identical(out$AU[6], "ENDRASS B;PEREZ-MARIN D")
  expect_identical(out$TI, case$papers$TI)
})

test_that("different IDs that share a spelling are neither numbered nor split", {
  out <- quiet_clean(make_case()$papers, ids = make_case()$ids)
  expect_false(any(grepl("\\(", out$AU)))
  expect_identical(sub(".*;", "", out$AU[1:2]), c("KIM J", "KIM J"))
})

test_that("papers without a merged author keep their text exactly", {
  case <- make_case()
  out <- quiet_clean(case$papers, ids = case$ids)
  # The entity paper, the misaligned group paper and the paper not in ids.
  expect_identical(out$AU[4:5], case$papers$AU[4:5])
  expect_identical(out$AU[7], case$papers$AU[7])
})

test_that("an HTML entity's semicolon does not split a name", {
  expect_identical(.au_split_field("L&#X00F3;PEZ A;CHO SB")[[1]], c("L&#X00F3;PEZ A", "CHO SB"))
  expect_identical(.au_split_field(NA_character_)[[1]], character())
  # Entity names are merged and written back as they were spelled.
  ids <- data.frame(EID = c("a", "b", "c"), `Author(s) ID` = c("9;5;", "9;", "9;"), check.names = FALSE)
  papers <- data.frame(eid = c("a", "b", "c"), AU = c("L&#X00F3;PEZ A;CHO SB", "L&#X00F3;PEZ A", "LPEZ A"),
                       PY = c(2020, 2020, 2021))
  out <- quiet_clean(papers, ids = ids)
  expect_identical(out$AU, c("L&#X00F3;PEZ A;CHO SB", "L&#X00F3;PEZ A", "L&#X00F3;PEZ A"))
})

test_that("ties go to the most recent spelling, then the longest", {
  expect_identical(.au_pick(c("p", "p"), c("SMITH A", "SMYTH A"), c(2010, 2020)), c(p = "SMYTH A"))
  expect_identical(.au_pick(c("p", "p"), c("SMITH A", "SMYTHE A"), c(2020, 2020)), c(p = "SMYTHE A"))
  # Frequency beats recency.
  expect_identical(.au_pick(rep("p", 3), c("SMITH A", "SMITH A", "SMYTH A"), c(2010, 2011, 2020)),
                   c(p = "SMITH A"))
  # A spelling with letters lost loses even when more frequent.
  expect_identical(.au_pick(rep("p", 3), c("YALN N", "YALN N", "YALCIN ON"), c(2020, 2020, 2019)),
                   c(p = "YALCIN ON"))
  expect_identical(.au_is_subsequence(c("KAI Z", "KIM J", "A.B"), c("KACIC Z", "KIM SJ", "AXB")),
                   c(TRUE, TRUE, FALSE))
})

test_that("the result does not depend on the order of the papers", {
  case <- make_unlike_case()
  shuffled <- do.call(rbind, rev(split(case$papers, case$papers$eid)))
  a <- quiet_clean(case$papers, ids = case$ids, output = "all")
  b <- quiet_clean(shuffled, ids = case$ids, output = "all")
  both <- merge(a, b, by = "eid")
  expect_identical(nrow(both), nrow(case$papers))
  expect_identical(both$AU.x, both$AU.y)
  expect_identical(both$check_authors.x, both$check_authors.y)
})

test_that("cleaning twice changes nothing more", {
  case <- make_unlike_case()
  once <- quiet_clean(case$papers, ids = case$ids, output = "all")
  twice <- quiet_clean(once, ids = case$ids, output = "all")
  expect_identical(twice$AU, once$AU)
  expect_identical(twice$AU_ORIGINAL, case$papers$AU)
})

test_that("output = 'all' adds the original, the aligned IDs and check_authors", {
  case <- make_unlike_case()
  out <- quiet_clean(case$papers, ids = case$ids, output = "all")
  expect_identical(setdiff(names(out), names(case$papers)), c("AU_ORIGINAL", "AU_ID", "check_authors"))
  expect_identical(out$AU_ORIGINAL, case$papers$AU)
  expect_identical(out$AU_ID[1:2], c("111;201", "111;202"))
  expect_true(all(is.na(out$AU_ID[c(5, 7)])))
  expect_identical(lengths(.au_split_ids(out$AU_ID[1:4])), lengths(.au_split_field(out$AU[1:4])))
  # ID 7 is ENDRASS B once and LUGRIN B twice: one name now, flagged.
  expect_identical(out$AU[6], "LUGRIN B;PEREZ-MARIN D")
  expect_identical(out$check_authors, c(rep("NO", 5), "YES", "NO", "YES", "YES"))
})

test_that("the full-name column is rewritten alongside, where it lines up", {
  case <- make_case()
  case$papers$AF <- c("PÉREZ-MARÍN, DIANA;KIM, JI", "PEREZ-MARIN, DIANA;KIM, JIN", "PÉREZ-MARÍN, DIANA;HUFF, EARL",
                      "LÓPEZ, ANA;CHO, SUNG", "GROUP, G;COMMITTEE", "ENDRASS, B", "NOBODY, N")
  out <- quiet_clean(case$papers, ids = case$ids, output = "all")
  expect_identical(out$AF[1:3], c("PÉREZ-MARÍN, DIANA;KIM, JI", "PÉREZ-MARÍN, DIANA;KIM, JIN",
                                  "PÉREZ-MARÍN, DIANA;HUFF, EARL"))
  # Paper 6 has one full name for two authors: left alone.
  expect_identical(out$AF[6], "ENDRASS, B")
  expect_identical(out$AF_ORIGINAL, case$papers$AF)
  expect_identical(quiet_clean(case$papers, ids = case$ids, full_names = NULL)$AF, case$papers$AF)
})

test_that("the message counts the changes and names why papers were left untouched", {
  case <- make_unlike_case()
  expect_message(authors_clean(case$papers, ids = case$ids),
                 "2 people given one spelling \\(0 by decision\\); 2 papers changed; 3 papers to check; 2 papers untouched \\(author and ID counts differ: 1, record not in ids: 1\\)")
})

test_that("IDs come from a folder of exports, files or a data frame, read as text", {
  case <- make_case()
  folder <- tempfile()
  dir.create(folder)
  ids <- case$ids
  # A single long ID without its trailing `;` would read as a rounded number.
  ids$`Author(s) ID`[5] <- "57200000000000001"
  utils::write.csv(ids[1:3, ], file.path(folder, "a.csv"), row.names = FALSE, fileEncoding = "UTF-8")
  utils::write.csv(ids[4:6, ], file.path(folder, "b.csv"), row.names = FALSE, fileEncoding = "UTF-8")
  from_folder <- quiet_clean(case$papers, ids = folder, output = "all")
  from_files <- quiet_clean(case$papers, ids = list.files(folder, full.names = TRUE), output = "all")
  from_frame <- quiet_clean(case$papers, ids = ids, output = "all")
  expect_identical(from_folder, from_files)
  expect_identical(from_folder$AU, from_frame$AU)
  expect_identical(.au_read_ids(folder)$author_ids[5], "57200000000000001")
})

test_that("record IDs match whatever their case, as bibliometrix upper-cases the EID", {
  case <- make_case()
  upper <- transform(case$papers, eid = toupper(eid))
  expect_identical(quiet_clean(upper, ids = case$ids)$AU, quiet_clean(case$papers, ids = case$ids)$AU)
  scopus_style <- transform(case$ids, EID = sprintf("2-s2.0-%d", 1:6))
  bibliometrix_style <- transform(case$papers, eid = sprintf("2-S2.0-%d", 1:7))
  expect_identical(quiet_clean(bibliometrix_style, ids = scopus_style)$AU,
                   quiet_clean(case$papers, ids = case$ids)$AU)
})

test_that("renamed columns work", {
  case <- make_case()
  papers <- stats::setNames(case$papers, c("EID", "Authors", "Year", "TI"))
  out <- quiet_clean(papers, ids = case$ids, record_id = "EID", authors = "Authors", year = "Year")
  expect_identical(out$Authors, quiet_clean(case$papers, ids = case$ids)$AU)
})

test_that("bad input raises saqrmisc_bad_input", {
  case <- make_case()
  expect_error(authors_clean(case$papers), class = "saqrmisc_bad_input")
  expect_error(authors_clean(case$papers, ids = case$ids, record_id = "nope"), class = "saqrmisc_bad_input")
  expect_error(authors_clean(case$papers, ids = case$ids, full_names = "nope"), class = "saqrmisc_bad_input")
  expect_error(authors_clean(rbind(case$papers, case$papers), ids = case$ids), class = "saqrmisc_bad_input")
  expect_error(authors_clean(transform(case$papers, eid = paste0("x", eid)), ids = case$ids),
               class = "saqrmisc_bad_input")
  expect_error(authors_clean(case$papers, ids = case$ids["EID"]), class = "saqrmisc_bad_input")
  expect_error(authors_clean(case$papers, ids = tempfile()), class = "saqrmisc_bad_input")
  expect_error(authors_clean(case$papers, ids = case$ids, decisions = "d.csv"), class = "saqrmisc_bad_input")
  expect_error(authors_clean(case$papers, ids = case$ids, level = "needs_decision"), class = "saqrmisc_bad_input")
  expect_error(authors_clean(transform(case$papers, PY = "soon"), ids = case$ids), class = "saqrmisc_bad_input")
  expect_error(authors_clean(case$papers[0, ], ids = case$ids), class = "saqrmisc_bad_input")
})

test_that("decided merges join Scopus IDs, transitively, without asking OpenAlex", {
  skip_if_not_installed("httr2")
  ids <- data.frame(EID = c("a", "b", "c", "d"), `Author(s) ID` = c("1;", "2;", "3;", "4;"), check.names = FALSE)
  papers <- data.frame(eid = c("a", "b", "c", "d"), AU = c("SMITH J", "SMITH JA", "SMITH J", "JONES K"),
                       PY = c(2020, 2021, 2022, 2020))
  sheet <- tempfile(fileext = ".csv")
  .kr_write_csv(data.frame(decision = c("merge", "MERGE ", "no"), scopus_ids = c("1;2", "2;3", "3;4")), sheet)
  # Any OpenAlex request here would fail the test.
  out <- expect_message(
    httr2::with_mocked_responses(\(req) stop("OpenAlex was asked"),
      authors_clean(papers, ids = ids, level = "needs_decision", decisions = sheet, output = "all")),
    "1 people given one spelling \\(1 by decision\\)")
  expect_identical(out$AU, c("SMITH JA", "SMITH JA", "SMITH JA", "JONES K"))
  expect_identical(out$AU_ID, c("1", "2", "3", "4"))
  expect_identical(.au_group_of(c("1", "3", "4"), list(c("1", "2", "3"))), c("decided:1", "decided:1", "4"))
})

test_that("a decisions sheet is validated and decided IDs not in the data warn", {
  case <- make_case()
  bad <- tempfile(fileext = ".csv")
  .kr_write_csv(data.frame(decision = "maybe", scopus_ids = "111;201"), bad)
  expect_error(quiet_clean(case$papers, ids = case$ids, level = "needs_decision", decisions = bad),
               class = "saqrmisc_bad_input")
  no_cols <- tempfile(fileext = ".csv")
  .kr_write_csv(data.frame(decision = "merge"), no_cols)
  expect_error(quiet_clean(case$papers, ids = case$ids, level = "needs_decision", decisions = no_cols),
               class = "saqrmisc_bad_input")
  stranger <- tempfile(fileext = ".csv")
  .kr_write_csv(data.frame(decision = "merge", scopus_ids = "111;999"), stranger)
  expect_warning(quiet_clean(case$papers, ids = case$ids, level = "needs_decision", decisions = stranger),
                 class = "saqrmisc_review_unmatched")
})

test_that("the first needs_decision run writes Scopus IDs sharing an ORCID and applies none", {
  skip_if_not_installed("httr2")
  ids <- data.frame(EID = c("a", "b", "c"), `Author(s) ID` = c("1;5;", "2;", "3;"),
                    Authors = c("Smith J., Cho S.", "Smith J.A.", "Smith J."), check.names = FALSE)
  papers <- data.frame(eid = c("a", "b", "c"), AU = c("SMITH J;CHO S", "SMITH JA", "SMITH J"),
                       DI = c("10.1/a", "10.1/b", "10.1/c"), PY = c(2020, 2021, 2022))
  work <- \(doi, orcid) sprintf(
    '{"doi":"https://doi.org/%s","authorships":[{"author":{"id":"https://openalex.org/A%s","orcid":"https://orcid.org/%s"},"raw_author_name":"John Smith"}]}',
    doi, orcid, orcid)
  body <- sprintf('{"results":[%s,%s,%s]}', work("10.1/a", "0000-1"), work("10.1/b", "0000-1"), work("10.1/c", "0000-2"))
  reply <- \(req) httr2::response(status_code = 200L, headers = list(`Content-Type` = "application/json"),
                                  body = charToRaw(body))
  sheet <- tempfile(fileext = ".csv")
  out <- suppressMessages(httr2::with_mocked_responses(reply,
    authors_clean(papers, ids = ids, level = "needs_decision", decisions = sheet)))
  expect_identical(out$AU, papers$AU)
  written <- .kr_read_csv(sheet)
  expect_identical(nrow(written), 1L)
  expect_identical(written$scopus_ids, "1;2")
  expect_identical(written$orcid, "0000-1")
  expect_true(is.na(written$decision))
  # Marked merge, the second run applies it.
  written$decision <- "merge"
  unlink(sheet)
  .kr_write_csv(written, sheet)
  again <- quiet_clean(papers, ids = ids, level = "needs_decision", decisions = sheet)
  expect_identical(again$AU, c("SMITH JA;CHO S", "SMITH JA", "SMITH J"))
})

test_that("the cleaners chain", {
  case <- make_case()
  papers <- transform(case$papers, DE = "CHATBOTS; CHAT-BOT", C1 = "DEPT X, UNIVERSITY OF TURKU, FINLAND",
                      DI = sprintf("10.1/%d", 1:7))
  chained <- papers |>
    quiet_clean(ids = case$ids) |>
    keywords_clean() |>
    institutions_clean(openalex = FALSE)
  out <- as.data.frame(chained)
  expect_identical(out$AU, quiet_clean(papers, ids = case$ids)$AU)
  expect_identical(unique(out$AU_UN), "UNIVERSITY OF TURKU")
})

test_that("OpenAlex matching needs a whole, unique surname", {
  authorships <- data.frame(raw_name = c("Diana Pérez-Marín", "Alice Williams", "Bo Li", "Yan Li"),
                            openalex_id = c("A1", "A2", "A3", "A4"),
                            orcid = c("0000-0001", NA, NA, NA))
  matched <- .au_match_openalex(c("PEREZ-MARIN D", "WILLIAM A", "LI B", "NOBODY X"), authorships)
  expect_equal(matched$openalex_id, c("A1", NA, NA, NA))
  expect_equal(matched$orcid[1], "0000-0001")
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

test_that("a shared given name is no shared surname", {
  expect_false(.au_similar("KIM JOHN", "PARK JOHN"))
  expect_false(.au_similar("MOORE ROBERT L", "SMITH ROBERT L"))
  expect_true(.au_similar("LOPEZ SS", "SAFFON LOPEZ S"))
})

test_that("a live OpenAlex lookup returns ORCIDs for a known paper", {
  skip_on_cran()
  skip_if_not_installed("httr2")
  skip_if_offline("api.openalex.org")
  works <- .au_fetch_openalex(c("10.1016/j.pec.2013.05.011", "10.9999/not-a-real-doi"), mailto = NULL)
  expect_named(works, "10.1016/j.pec.2013.05.011")
  expect_equal(.au_match_openalex("BICKMORE TW", works[[1]])$orcid, "0000-0002-2676-9788")
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

