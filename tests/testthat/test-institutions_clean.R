ic <- function(...) suppressMessages(institutions_clean(...))

# A fake OpenAlex: one work naming Göttingen (twice, through two authors) and
# two Northeastern Universities that OpenAlex keeps apart.
fake_institution <- function(id, name, country) {
  list(id = paste0("https://openalex.org/", id), display_name = name,
       ror = paste0("https://ror.org/r", id), country_code = country, type = "education")
}
fake_works <- function() {
  gottingen <- fake_institution("I1", "Universität Göttingen", "DE")
  ne_us <- fake_institution("I2", "Northeastern University", "US")
  ne_cn <- fake_institution("I3", "Northeastern University", "CN")
  list(
    list(doi = "https://doi.org/10.1/a", authorships = list(
      list(institutions = list(gottingen),
           affiliations = list(list(raw_affiliation_string = "Institute of Informatics, University of Gottingen, Germany",
                                    institution_ids = list(gottingen$id))),
           raw_affiliation_strings = list("Institute of Informatics, University of Gottingen, Germany")),
      list(institutions = list(ne_us), affiliations = list(), raw_affiliation_strings = list()),
      # An author OpenAlex gives only an address for.
      list(institutions = list(), affiliations = list(),
           raw_affiliation_strings = list("Lab of Things, Acme Robotics, Inc, Boston, USA")))),
    list(doi = "https://doi.org/10.1/b", authorships = list(
      list(institutions = list(ne_cn), affiliations = list(), raw_affiliation_strings = list())))
  )
}
institution_papers <- function() {
  data.frame(
    TI = c("Paper A", "Paper B", "Paper C", "Paper D", "Paper E"),
    DI = c("10.1/A", "10.1/b", NA, NA, ""),
    C1 = c("IGNORED, BECAUSE OPENALEX COVERS THIS PAPER",
           "IGNORED TOO",
           # Accents lost, as bibliometrix writes Scopus addresses.
           "DEPARTMENT OF PHYSICS, UNIVERSITT GTTINGEN, GERMANY",
           "INSTITUTE OF INFORMATICS, UNIVERSITY OF GOTTINGEN, GERMANY",
           "COLLEGE OF SOFTWARE, NANKAI UNIVERSITY, TIANJIN, CHINA; ACME, LTD, 12345 BERLIN, GERMANY"),
    stringsAsFactors = FALSE
  )
}
# OpenAlex replaced by the fake works; counts the requests made.
local_fake_openalex <- function(works = fake_works(), env = parent.frame()) {
  asked <- new.env()
  asked$batches <- 0L
  local_mocked_bindings(.in_ask_openalex = function(batches, mailto) {
    asked$batches <- asked$batches + length(batches)
    lapply(batches, \(b) list(results = Filter(\(w) sub("^https://doi.org/", "", w$doi) %in% b, works)))
  }, .env = env)
  asked
}

test_that("the institution part of an address is the strongest kind", {
  expect_identical(
    .in_extract(c("COLLEGE OF SOFTWARE, NANKAI UNIVERSITY, TIANJIN, CHINA",
                  "MICROSOFT, INC, REDMOND, WA, UNITED STATES",
                  "DEPT OF X, FRAUNHOFER INSTITUTE, 12345 MUNICH, GERMANY",
                  "TRINITY COLLEGE DUBLIN, IRELAND",
                  "12345, BERLIN")),
    c("NANKAI UNIVERSITY", "MICROSOFT", "FRAUNHOFER INSTITUTE", "TRINITY COLLEGE DUBLIN", "BERLIN"))
  expect_identical(.in_extract("A, UNIVERSITY OF B", tier = TRUE)$tier, 1L)
  # Lost accents still find the name.
  expect_identical(.in_key("Universität Göttingen", mangled = TRUE), .in_key("UNIVERSITT GTTINGEN"))
})

test_that("the result is the data with AU_UN; without OpenAlex every institution comes from the address", {
  papers <- institution_papers()
  out <- ic(papers, openalex = FALSE)
  expect_identical(names(out), c(names(papers), "AU_UN"))
  expect_identical(out[names(papers)], papers)
  expect_identical(out$AU_UN[5], "NANKAI UNIVERSITY; ACME")
  all <- ic(papers, openalex = FALSE, output = "all")
  expect_identical(all$AU_UN_ID[5], "NA; NA")
  expect_message(institutions_clean(papers, openalex = FALSE), "from address text 6")
})

test_that("OpenAlex identifies institutions and teaches the addresses", {
  local_fake_openalex()
  out <- ic(institution_papers(), cache = NULL, output = "all")
  # Paper A: OpenAlex directly, plus the author with only an address.
  expect_identical(out$AU_UN[1], "UNIVERSITÄT GÖTTINGEN; NORTHEASTERN UNIVERSITY, US; LAB OF THINGS")
  expect_identical(out$AU_UN_ID[1], "I1; I2; NA")
  expect_identical(out$AU_UN[2], "NORTHEASTERN UNIVERSITY, CN")
  # Paper C by name (accents lost), paper D by the address OpenAlex linked.
  expect_identical(out$AU_UN_ID[3:4], c("I1", "I1"))
  expect_message(institutions_clean(institution_papers(), cache = NULL),
                 "by OpenAlex ID 3, by an address OpenAlex linked 1, by OpenAlex name 1")
})

test_that("the cache keeps OpenAlex's answers, so a second run asks nothing", {
  asked <- local_fake_openalex()
  cache <- file.path(tempdir(), sprintf("oa-cache-%d", sample.int(1e6, 1L)))
  on.exit(unlink(cache, recursive = TRUE), add = TRUE)
  first <- ic(institution_papers(), cache = cache)
  expect_identical(asked$batches, 1L)
  expect_identical(length(list.files(cache)), 1L)
  second <- ic(institution_papers(), cache = cache)
  expect_identical(asked$batches, 1L)
  expect_identical(second, first)
  # A paper not asked yet is the only one asked.
  more <- rbind(institution_papers(), data.frame(TI = "Paper F", DI = "10.1/new", C1 = "UNIVERSITY OF OULU, FINLAND"))
  ic(more, cache = cache)
  expect_identical(asked$batches, 2L)
})

test_that("failed requests warn, fall back to addresses and are asked again later", {
  local_mocked_bindings(.in_ask_openalex = function(batches, mailto) lapply(batches, \(b) simpleError("HTTP 503")))
  cache <- file.path(tempdir(), sprintf("oa-fail-%d", sample.int(1e6, 1L)))
  on.exit(unlink(cache, recursive = TRUE), add = TRUE)
  out <- expect_warning(ic(institution_papers(), cache = cache), class = "saqrmisc_openalex_failed")
  expect_identical(out$AU_UN[1], "IGNORED")
  expect_identical(length(list.files(cache)), 0L)
})

test_that("OpenAlex works become tables, in author order", {
  tables <- .in_tables(fake_works())
  expect_identical(tables$direct$openalex_id, c("I1", "I2", "I3"))
  expect_identical(tables$loose$raw, "Lab of Things, Acme Robotics, Inc, Boston, USA")
  expect_identical(tables$linked$openalex_id, "I1")
  expect_identical(nrow(tables$known), 3L)
  expect_identical(.in_tables(list()), .in_no_tables())
})

test_that("the row order of the papers does not change any paper's institutions", {
  local_fake_openalex()
  papers <- institution_papers()
  shuffled <- papers[c(5, 3, 1, 4, 2), ]
  a <- ic(papers, cache = NULL)
  b <- ic(shuffled, cache = NULL)
  expect_identical(b$AU_UN[match(a$TI, b$TI)], a$AU_UN)
})

test_that("replace renames only address names, and joins them to identified institutions", {
  local_fake_openalex()
  papers <- institution_papers()
  papers$C1[4] <- "NORTHEASTERN UNIVERSITY, BOSTON, USA"
  table <- data.frame(OLD = c("NORTHEASTERN UNIVERSITY", "UNIVERSITÄT GÖTTINGEN", "ACME"),
                      NEW = c("NORTHEASTERN UNIVERSITY, US", "GOTTINGEN RENAMED", "ACME CORP"))
  out <- ic(papers, cache = NULL, replace = table, output = "all")
  expect_identical(out$AU_UN_ID[4], "I2")
  expect_false(any(grepl("GOTTINGEN RENAMED", out$AU_UN)))
  expect_identical(out$AU_UN[5], "NANKAI UNIVERSITY; ACME CORP")
})

test_that("names written differently become one; two OpenAlex IDs never share a name", {
  papers <- data.frame(DI = NA_character_, C1 = c("NANKAI-UNIVERSITY, CHINA", "NANKAI UNIVERSITY, CHINA",
                                                  "NANKAI UNIVERSITY, TIANJIN, CHINA"))
  expect_identical(ic(papers, openalex = FALSE)$AU_UN, rep("NANKAI UNIVERSITY", 3L))
  works <- fake_works()
  works[[2]]$authorships[[1]]$institutions <- list(fake_institution("I9", "Northeastern University", "US"))
  local_fake_openalex(works)
  out <- ic(institution_papers()[1:2, ], cache = NULL, output = "all")
  expect_identical(out$AU_UN, c("UNIVERSITÄT GÖTTINGEN; NORTHEASTERN UNIVERSITY, US; LAB OF THINGS",
                                "NORTHEASTERN UNIVERSITY, US (2)"))
})

test_that("level = 'all' merges nothing and writes the sheet; identified institutions lead their groups", {
  local_fake_openalex()
  papers <- rbind(institution_papers(),
                  data.frame(TI = c("F", "G"), DI = NA, C1 = c("NORTHEASTERN UNIVERSITI, BOSTON", "NANKAI UNIVERSITY, CHINA")))
  sheet <- tempfile(fileext = ".csv")
  out <- ic(papers, cache = NULL, level = "all", decisions = sheet, min_papers = 1)
  expect_identical(out, papers)
  written <- .kr_read_csv(sheet)
  expect_named(written, c("group", "name", "institution", "institution_backup", "status", "papers", "openalex_id",
                          "source", "example"))
  expect_false("name" %in% written$status)
  expect_identical(subset(written, institution == "UNIVERSITT GTTINGEN")$status, "confirmed")
  expect_identical(subset(written, institution == "UNIVERSITT GTTINGEN")$source, "openalex name")
  asked <- subset(written, status == "needs_revision")
  expect_true("NORTHEASTERN UNIVERSITI" %in% asked$institution)
  expect_true(all(subset(asked, institution == "NORTHEASTERN UNIVERSITI")$name %in%
                    c("NORTHEASTERN UNIVERSITY, US", "NORTHEASTERN UNIVERSITY, CN")))
  # Two identified institutions are never paired.
  expect_false(any(!is.na(asked$openalex_id) & asked$name %in% c("NORTHEASTERN UNIVERSITY, US",
                                                                  "NORTHEASTERN UNIVERSITY, CN")))
  expect_true(any(grepl("institution", readLines(sub("\\.csv$", "_prompt.txt", sheet)))))
  expect_error(ic(papers, cache = NULL, level = "all", decisions = sheet), class = "saqrmisc_bad_input")
})

test_that("institutions_merge applies yes, and undoes a confirmed match marked no", {
  local_fake_openalex()
  papers <- rbind(institution_papers(),
                  data.frame(TI = "F", DI = NA, C1 = "NORTHEASTERN UNIVERSITI, BOSTON"))
  sheet <- tempfile(fileext = ".csv")
  ic(papers, cache = NULL, level = "all", decisions = sheet, min_papers = 1)
  checked <- .kr_read_csv(sheet)
  checked$merge <- ifelse(checked$institution == "NORTHEASTERN UNIVERSITI", "yes",
                          ifelse(checked$institution == "UNIVERSITT GTTINGEN", "no", NA))
  target <- subset(checked, institution == "NORTHEASTERN UNIVERSITI")$name
  unlink(sheet)
  .kr_write_csv(checked, sheet)
  out <- suppressMessages(institutions_merge(papers, decisions = sheet, cache = NULL, output = "all"))
  expect_identical(out$AU_UN[6], target)
  expect_false(is.na(out$AU_UN_ID[6]))
  expect_identical(out$AU_UN[3], "UNIVERSITT GTTINGEN")
  expect_identical(out$AU_UN_ID[3], "NA")
  expect_message(institutions_merge(papers, decisions = sheet, cache = NULL), "1 merge\\(s\\) applied, 1 confirmed")
})

test_that("institutions_merge(by = 'edit') applies edited names, never to identified institutions", {
  papers <- data.frame(DI = NA_character_, C1 = c("UNIVERSITY OF TURKU, FINLAND", "TURKU UNIVERSITY, FINLAND",
                                                  "ABO AKADEMI, TURKU, FINLAND"))
  sheet <- tempfile(fileext = ".csv")
  ic(papers, openalex = FALSE, level = "all", decisions = sheet, min_papers = 1)
  edited <- .kr_read_csv(sheet)
  edited$institution[edited$institution_backup == "TURKU UNIVERSITY"] <- "UNIVERSITY OF TURKU"
  unlink(sheet)
  .kr_write_csv(edited, sheet)
  out <- suppressMessages(institutions_merge(papers, decisions = sheet, by = "edit", openalex = FALSE))
  expect_identical(out$AU_UN, c("UNIVERSITY OF TURKU", "UNIVERSITY OF TURKU", "ABO AKADEMI"))
})

test_that("bad arguments are refused", {
  expect_error(institutions_clean("x"), class = "saqrmisc_bad_input")
  expect_error(institutions_clean(institution_papers()[0, ]), class = "saqrmisc_bad_input")
  expect_error(institutions_clean(institution_papers(), affiliations = "AD"), class = "saqrmisc_bad_input")
  expect_error(institutions_clean(institution_papers(), doi = "DOI"), class = "saqrmisc_bad_input")
  expect_error(institutions_clean(institution_papers(), openalex = NA), class = "saqrmisc_bad_input")
  expect_error(institutions_clean(institution_papers(), mailto = "nobody"), class = "saqrmisc_bad_input")
  expect_error(institutions_clean(institution_papers(), cache = 1), class = "saqrmisc_bad_input")
  expect_error(institutions_clean(institution_papers(), min_papers = 0), class = "saqrmisc_bad_input")
  expect_error(institutions_clean(institution_papers(), decisions = "x.csv"), class = "saqrmisc_bad_input")
  expect_error(institutions_clean(institution_papers(), level = "all"), class = "saqrmisc_bad_input")
  expect_error(institutions_merge(institution_papers(), openalex = FALSE), class = "saqrmisc_bad_input")
})

test_that("Web of Science author lists before an address are dropped, semicolons and all", {
  papers <- data.frame(DI = NA_character_, C1 = "[KIM, J; LEE, A] UNIV OULU, FINLAND; [PARK, S] UNIV TURKU, FINLAND")
  expect_identical(ic(papers, openalex = FALSE)$AU_UN, "UNIV OULU; UNIV TURKU")
})
