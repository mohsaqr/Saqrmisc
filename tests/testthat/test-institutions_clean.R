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
    "10.1/a" = list(doi = "https://doi.org/10.1/a", authorships = list(
      list(institutions = list(gottingen),
           affiliations = list(list(raw_affiliation_string = "Institute of Informatics, University of Gottingen, Germany",
                                    institution_ids = list(gottingen$id))),
           raw_affiliation_strings = list("Institute of Informatics, University of Gottingen, Germany")),
      list(institutions = list(ne_us), affiliations = list(), raw_affiliation_strings = list()),
      # An author OpenAlex gives only an address for.
      list(institutions = list(), affiliations = list(),
           raw_affiliation_strings = list("Lab of Things, Acme Robotics, Inc, Boston, USA")))),
    "10.1/b" = list(doi = "https://doi.org/10.1/b", authorships = list(
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
with_fake_openalex <- function(code) {
  local_mocked_bindings(.oa_fetch_works = function(dois, mailto, consequence) fake_works())
  code
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

test_that("without OpenAlex every institution comes from the address", {
  x <- institutions_clean(institution_papers(), openalex = FALSE)
  expect_s3_class(x, "saqr_institutions")
  data <- as.data.frame(x)
  expect_identical(data$AU_UN[5], "NANKAI UNIVERSITY; ACME")
  expect_true(all(is.na(data$AU_UN_ID)))
  expect_identical(nrow(data), nrow(institution_papers()))
  s <- summary(x)
  expect_identical(s$links[s$source == "address"], sum(s$links))
  expect_output(print(x), "OpenAlex not asked")
})

test_that("OpenAlex identifies institutions and teaches the addresses", {
  x <- with_fake_openalex(institutions_clean(institution_papers()))
  data <- as.data.frame(x)
  # Paper A: OpenAlex directly, plus the author with only an address.
  # A lab outranks a company name with no marker word.
  expect_identical(data$AU_UN[1], "UNIVERSITÄT GÖTTINGEN; NORTHEASTERN UNIVERSITY, US; LAB OF THINGS")
  expect_identical(data$AU_UN_ID[1], "I1; I2")
  expect_identical(data$AU_UN[2], "NORTHEASTERN UNIVERSITY, CN")
  # Paper C by name (accents lost), paper D by the address OpenAlex linked.
  expect_identical(data$AU_UN_ID[3:4], c("I1", "I1"))
  a <- institutions_changes(x)
  expect_identical(a$source[a$paper == 3L], "openalex name")
  expect_identical(a$source[a$paper == 4L], "openalex address")
  expect_identical(a$source[a$paper == 1L], c("openalex", "openalex", "address"))
  inst <- as.data.frame(x, what = "institutions")
  expect_identical(inst$papers[inst$openalex_id %in% "I1"], 3L)
  expect_identical(inst$ror[inst$openalex_id %in% "I1"], "rI1")
  expect_false(anyDuplicated(inst$institution) > 0L)
})

test_that("the row order of the papers does not change any paper's institutions", {
  papers <- institution_papers()
  shuffled <- papers[c(5, 3, 1, 4, 2), ]
  a <- as.data.frame(with_fake_openalex(institutions_clean(papers)))
  b <- as.data.frame(with_fake_openalex(institutions_clean(shuffled)))
  expect_identical(b$AU_UN[match(a$TI, b$TI)], a$AU_UN)
})

test_that("replace applies reviewed names", {
  table <- data.frame(OLD = "ACME", NEW = "ACME CORPORATION")
  data <- as.data.frame(institutions_clean(institution_papers(), openalex = FALSE, replace = table))
  expect_identical(data$AU_UN[5], "NANKAI UNIVERSITY; ACME CORPORATION")
})

test_that("suggest_merges never pairs two identified institutions and prefers their name", {
  papers <- institution_papers()
  papers$C1[3] <- "UNIVERSITY OF GOETTINGEN, GERMANY"
  papers$C1[4] <- "NORTHEASTERN UNIVERSITY, BOSTON, USA"
  x <- with_fake_openalex(institutions_clean(papers))
  merges <- suggest_merges(x, meaning = FALSE)
  pairs <- as.data.frame(merges)
  both_known <- c("NORTHEASTERN UNIVERSITY, US", "NORTHEASTERN UNIVERSITY, CN")
  expect_false(any(pairs$keyword %in% both_known & pairs$variant %in% both_known))
  expect_true("NORTHEASTERN UNIVERSITY, US" %in% pairs$keyword | "NORTHEASTERN UNIVERSITY, CN" %in% pairs$keyword)
  expect_false("NORTHEASTERN UNIVERSITY" %in% pairs$keyword)
  expect_identical(merges$kind, "institutions")
  expect_match(.ai_system_prompt(NULL, "institutions"), "institution names")
  expect_false(grepl("author keywords", .ai_system_prompt(NULL, "institutions")))
})

test_that("bad arguments are refused", {
  expect_error(institutions_clean("x"), class = "saqrmisc_bad_input")
  expect_error(institutions_clean(institution_papers(), affiliations = "AD"), class = "saqrmisc_bad_input")
  expect_error(institutions_clean(institution_papers(), doi = "DOI"), class = "saqrmisc_bad_input")
  expect_error(institutions_clean(institution_papers(), openalex = NA), class = "saqrmisc_bad_input")
  expect_error(institutions_clean(institution_papers(), mailto = "nobody"), class = "saqrmisc_bad_input")
})

test_that("a review renames only address names, and joins them to identified institutions", {
  papers <- institution_papers()
  papers$C1[4] <- "NORTHEASTERN UNIVERSITY, BOSTON, USA"
  table <- data.frame(OLD = c("NORTHEASTERN UNIVERSITY", "UNIVERSITÄT GÖTTINGEN", "ACME"),
                      NEW = c("NORTHEASTERN UNIVERSITY, US", "GOTTINGEN RENAMED", "ACME CORP"))
  x <- with_fake_openalex(institutions_clean(papers, replace = table))
  data <- as.data.frame(x)
  expect_identical(data$AU_UN_ID[4], "I2")
  expect_false(any(grepl("GOTTINGEN RENAMED", data$AU_UN)))
  expect_identical(data$AU_UN[5], "NANKAI UNIVERSITY; ACME CORP")
  inst <- as.data.frame(x, what = "institutions")
  expect_identical(inst$papers[inst$openalex_id %in% "I2"], 2L)
  expect_identical(summary(x)$links[summary(x)$source == "review"], 2L)
})

test_that("institutions_changes(source = ) keeps the addresses resolved that way", {
  x <- with_fake_openalex(institutions_clean(institution_papers()))
  expect_identical(institutions_changes(x), as.data.frame(x, what = "addresses"))
  by_name <- institutions_changes(x, source = "openalex name")
  expect_true(nrow(by_name) > 0L && all(by_name$source == "openalex name"))
  both <- institutions_changes(x, source = c("openalex", "address"))
  expect_setequal(unique(both$source), c("openalex", "address"))
  expect_equal(nrow(both), sum(institutions_changes(x)$source %in% c("openalex", "address")))
  expect_error(institutions_changes(x, source = "elsewhere"), class = "saqrmisc_bad_input")
  expect_error(institutions_changes(x, source = 1), class = "saqrmisc_bad_input")
  expect_error(institutions_changes(as.data.frame(x)), class = "saqrmisc_bad_input")
})

test_that("two OpenAlex IDs never share a name, and fully covered papers work", {
  same <- fake_institution("I9", "Northeastern University", "US")
  works <- fake_works()
  works[["10.1/b"]]$authorships[[1]]$institutions <- list(same)
  local_mocked_bindings(.oa_fetch_works = function(dois, mailto, consequence) works)
  covered <- institutions_clean(institution_papers()[1:2, ])
  inst <- as.data.frame(covered, what = "institutions")
  expect_false(anyDuplicated(inst$institution) > 0L)
  expect_setequal(inst$institution[inst$openalex_id %in% c("I2", "I9")],
                  c("NORTHEASTERN UNIVERSITY, US", "NORTHEASTERN UNIVERSITY, US (2)"))
  expect_error(institutions_clean(institution_papers()[0, ]), class = "saqrmisc_bad_input")
})
