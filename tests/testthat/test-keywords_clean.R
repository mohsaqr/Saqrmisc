kc <- function(...) suppressMessages(keywords_clean(...))
kw_of <- function(cleaned, field = "DE") cleaned[[field]]
split_kw <- function(x) lapply(strsplit(x, ";", fixed = TRUE), \(k) sort(trimws(k)))

# ---- confirmed --------------------------------------------------------------

test_that("the result is the data frame, and papers with nothing to change keep their text", {
  papers <- data.frame(TI = c("a", "b", "c"),
                       DE = c("CHATBOT;  AGENT", "CHAT-BOTS; AGENT", "E-LEARNING ;USABILITY"))
  cleaned <- kc(papers)
  expect_s3_class(cleaned, "data.frame")
  expect_identical(names(cleaned), names(papers))
  expect_identical(cleaned$TI, papers$TI)
  # CHATBOT and CHAT-BOTS tie on papers: the shorter wins, so paper 1 is untouched.
  expect_identical(kw_of(cleaned), c("CHATBOT;  AGENT", "CHATBOT; AGENT", "E-LEARNING ;USABILITY"))
})

test_that("a replacement term is replaced wherever it sits in the list", {
  # bibliometrix writes ";  " after the first keyword; the old code then
  # missed every keyword from the third position on.
  papers <- data.frame(DE = c("CHATBOT;  AGENT;  ARTIFICIAL INTELLIGENCE",
                              "ARTIFICIAL INTELLIGENCE",
                              "ARTIFICIAL INTELLIGENCE; CHATBOT"))
  cleaned <- kc(papers, replace = data.frame(OLD = "ARTIFICIAL INTELLIGENCE", NEW = "AI"))
  expect_equal(kw_of(cleaned), c("CHATBOT; AGENT; AI", "AI", "AI; CHATBOT"))
})

test_that("case and separator spacing do not change the keywords", {
  a <- kc(data.frame(DE = "Chatbot;  E-Learning;  Usability"))
  b <- kc(data.frame(DE = "CHATBOT;E-LEARNING ; USABILITY"))
  expect_equal(kw_of(a), "CHATBOT; E-LEARNING; USABILITY")
  expect_equal(split_kw(kw_of(a)), split_kw(kw_of(b)))
  expect_equal(kw_of(b), "CHATBOT;E-LEARNING ; USABILITY")
})

test_that("regex characters in terms are taken literally", {
  papers <- data.frame(DE = c("C++; WEB 2.0", "CXX; WEB 230"))
  cleaned <- kc(papers, replace = data.frame(OLD = c("C++", "WEB 2.0"), NEW = c("CPLUSPLUS", "WEB2")))
  expect_equal(kw_of(cleaned), c("CPLUSPLUS; WEB2", "CXX; WEB 230"))
})

test_that("replacement chains resolve in any row order; a loop is broken by the later row", {
  papers <- data.frame(DE = "A TERM; B TERM")
  chain <- data.frame(OLD = c("B TERM", "A TERM"), NEW = c("C TERM", "B TERM"))
  expect_equal(kw_of(kc(papers, replace = chain)), "C TERM")
  loop <- data.frame(OLD = c("A TERM", "B TERM"), NEW = c("B TERM", "A TERM"))
  expect_warning(cleaned <- kc(papers, replace = loop), class = "saqrmisc_replace_conflict")
  expect_equal(kw_of(cleaned), "A TERM")
})

test_that("a list of thesauri applies in order, so a later table corrects an earlier one", {
  papers <- data.frame(DE = c("NATURAL LANGUAGE PROCESSING", "NLP; NATURAL LANGUAGE"))
  first <- data.frame(OLD = c("NATURAL LANGUAGE PROCESSING", "NATURAL LANGUAGE"), NEW = c("NLP", "NLP"))
  second <- data.frame(OLD = "NLP", NEW = "NATURAL LANGUAGE PROCESSING")
  cleaned <- kc(papers, replace = list(first, second))
  expect_equal(kw_of(cleaned), c("NATURAL LANGUAGE PROCESSING", "NATURAL LANGUAGE PROCESSING"))
  expect_warning(combined <- kc(papers, replace = rbind(first, second)), class = "saqrmisc_replace_conflict")
  expect_equal(kw_of(combined), kw_of(cleaned))
})

test_that("bracketed NEW terms are cleaned like the data, so keep-as-is rows stay identities", {
  papers <- data.frame(DE = "TEXT-TO-SPEECH (TTS)")
  table <- data.frame(OLD = c("TEXT-TO-SPEECH (TTS)", "TEXT-TO-SPEECH"),
                      NEW = c("TEXT-TO-SPEECH (TTS)", "TEXT TO SPEECH"))
  expect_no_warning(cleaned <- kc(papers, replace = table))
  expect_equal(kw_of(cleaned), "TEXT TO SPEECH")
})

test_that("conflicting replacement rows warn and the later row wins", {
  papers <- data.frame(DE = "VIRTUAL AGENTS")
  conflict <- data.frame(OLD = c("VIRTUAL AGENTS", "VIRTUAL AGENTS"), NEW = c("VIRTUAL ASSISTANT", "VIRTUAL AGENT"))
  expect_warning(cleaned <- kc(papers, replace = conflict), class = "saqrmisc_replace_conflict")
  expect_equal(kw_of(cleaned), "VIRTUAL AGENT")
})

test_that("spelling variants merge, preferring a replacement target, then the most used form", {
  papers <- data.frame(DE = c("CHAT BOT; ROBOTS", "CHAT-BOTS; ROBOT", "CHATBOTS; ROBOTS", "CHATBOTS"))
  expect_equal(kw_of(kc(papers)), c("CHATBOTS; ROBOTS", "CHATBOTS; ROBOTS", "CHATBOTS; ROBOTS", "CHATBOTS"))
  preferred <- kc(papers, replace = data.frame(OLD = "CHAT BOT", NEW = "CHATBOT"))
  expect_equal(kw_of(preferred), c("CHATBOT; ROBOTS", "CHATBOT; ROBOTS", "CHATBOT; ROBOTS", "CHATBOT"))
  # With every form in one paper, the shortest wins.
  tied <- kc(data.frame(DE = c("CHAT BOT", "CHATBOTS", "CHATBOT")))
  expect_equal(unique(kw_of(tied)), "CHATBOT")
})

test_that("merging keeps word order and leaves distinct terms apart", {
  papers <- data.frame(DE = c("WEB 2.0; STATUS; ANALYSIS; 聊天机器人; TEXT TO SPEECH",
                              "WEB 3.0; STATU; ANALYSI; 教育; SPEECH TO TEXT"))
  expect_identical(kw_of(kc(papers)), papers$DE)
})

test_that("brackets: plural markers and acronyms go, other text is kept; acronyms are not expanded", {
  papers <- data.frame(DE = c("VOICE ASSISTANT(S); NATURAL LANGUAGE PROCESSING(NLP)",
                              "B-RNN (BI-DIRECTIONAL RECURRENT NEURAL NETWORK); NLP",
                              "LEARNING (ARTIFICIAL INTELLIGENCE)"))
  expect_equal(kw_of(kc(papers)), c("VOICE ASSISTANT; NATURAL LANGUAGE PROCESSING",
                                    "BI-DIRECTIONAL RECURRENT NEURAL NETWORK; NLP",
                                    "LEARNING ARTIFICIAL INTELLIGENCE"))
})

test_that("nothing is ever deleted; each keyword appears once per paper", {
  papers <- data.frame(DE = c("CHATBOT; SURVEY; CHATBOTS", "SURVEYS", NA, ".; CHATBOT"))
  expect_equal(kw_of(kc(papers)), c("CHATBOT; SURVEY", "SURVEY", NA, ".; CHATBOT"))
  expect_error(kc(papers, drop = "SURVEY"))
})

test_that("several columns are cleaned and other columns are untouched", {
  papers <- data.frame(id = 1:2, DE = c("CHATBOTS", "CHATBOT"),
                       ID = c("HUMAN COMPUTER INTERACTION", "HUMAN-COMPUTER INTERACTION"), TI = c("One", "Two"))
  out <- kc(papers, columns = c("DE", "ID"))
  expect_equal(out$TI, papers$TI)
  expect_equal(length(unique(out$DE)), 1L)
  expect_equal(length(unique(out$ID)), 1L)
})

test_that("the result does not depend on the order of the papers", {
  papers <- data.frame(id = 1:5, DE = c("CHAT BOT; AI", "CHATBOTS", "CHATBOT; E-LEARNING",
                                        "ELEARNING; CHAT-BOT", "E LEARNING"))
  a <- kc(papers)
  b <- kc(do.call(rbind, rev(split(papers, papers$id))))
  both <- merge(a, b, by = "id")
  expect_identical(both$DE.x, both$DE.y)
})

test_that("cleaning twice changes nothing more", {
  papers <- data.frame(DE = c("CHAT BOT; AI", "CHATBOTS; E-LEARNING", "Chatbot; E LEARNING (EL)", NA))
  once <- kc(papers)
  expect_identical(kc(once)$DE, once$DE)
})

test_that("the message counts forms, keywords and changed papers", {
  papers <- data.frame(DE = c("CHAT BOT; AI", "CHATBOTS; SURVEY", "CHATBOT"))
  expect_message(keywords_clean(papers), "DE: 5 written forms became 3 keywords; 2 papers changed")
  expect_message(keywords_clean(data.frame(DE = "CHATBOT; AGENT")), "0 papers changed")
})

test_that("output = 'all' adds the original columns, kept on a second run", {
  papers <- data.frame(DE = c("CHAT BOT", "CHATBOTS"), ID = c("X", "Y"))
  out <- kc(papers, columns = c("DE", "ID"), output = "all")
  expect_identical(setdiff(names(out), names(papers)), c("DE_ORIGINAL", "ID_ORIGINAL"))
  expect_identical(out$DE_ORIGINAL, papers$DE)
  again <- kc(transform(out, DE = "OTHER"), output = "all")
  expect_identical(again$DE_ORIGINAL, papers$DE)
})

test_that("replace takes .csv and .xlsx files, applied in the order given", {
  papers <- data.frame(DE = c("NATURAL LANGUAGE PROCESSING", "NLP; NATURAL LANGUAGE"))
  first <- tempfile(fileext = ".csv")
  utils::write.csv(data.frame(OLD = c("NATURAL LANGUAGE PROCESSING", "NATURAL LANGUAGE"), NEW = c("NLP", "NLP")),
                   first, row.names = FALSE)
  second <- tempfile(fileext = ".csv")
  utils::write.csv(data.frame(OLD = "NLP", NEW = "NATURAL LANGUAGE PROCESSING"), second, row.names = FALSE)
  from_files <- kc(papers, replace = c(first, second))
  expect_equal(kw_of(from_files), c("NATURAL LANGUAGE PROCESSING", "NATURAL LANGUAGE PROCESSING"))
  expect_equal(kw_of(kc(papers, replace = first)), c("NLP", "NLP"))

  skip_if_not_installed("readxl")
  skip_if_not_installed("writexl")
  workbook <- tempfile(fileext = ".xlsx")
  writexl::write_xlsx(data.frame(OLD = "NLP", NEW = "NATURAL LANGUAGE PROCESSING"), workbook)
  expect_equal(kw_of(kc(papers, replace = list(first, workbook))), kw_of(from_files))
})

test_that("a bad replace file raises saqrmisc_bad_input", {
  papers <- data.frame(DE = "CHATBOT")
  expect_error(kc(papers, replace = "no-such-table.csv"), class = "saqrmisc_bad_input")
  text_file <- tempfile(fileext = ".txt")
  writeLines("OLD,NEW", text_file)
  expect_error(kc(papers, replace = text_file), class = "saqrmisc_bad_input")
  no_columns <- tempfile(fileext = ".csv")
  utils::write.csv(data.frame(from = "A", to = "B"), no_columns, row.names = FALSE)
  expect_error(kc(papers, replace = no_columns), class = "saqrmisc_bad_input")
})

test_that("bad input raises saqrmisc_bad_input", {
  papers <- data.frame(DE = "CHATBOT", N = 1)
  expect_error(kc(list()), class = "saqrmisc_bad_input")
  expect_error(kc(papers, columns = "XX"), class = "saqrmisc_bad_input")
  expect_error(kc(papers, columns = "N"), class = "saqrmisc_bad_input")
  expect_error(kc(papers, replace = data.frame(from = "A", to = "B")), class = "saqrmisc_bad_input")
  expect_error(kc(papers, min_papers = 0), class = "saqrmisc_bad_input")
  expect_error(kc(papers, min_papers = c(2, 3)), class = "saqrmisc_bad_input")
  expect_error(kc(papers, decisions = "d.csv"), class = "saqrmisc_bad_input")
  expect_error(kc(papers, level = "all"), class = "saqrmisc_bad_input")
  expect_error(kc(papers, level = "all", decisions = "d.txt"), class = "saqrmisc_bad_input")
})

# ---- the vectorised helpers -------------------------------------------------

test_that("the vectorised key equals the per-term key it replaced", {
  per_term <- function(x) {
    words <- strsplit(gsub("['`’]", "", toupper(x)), "[^[:alnum:]]+")
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
  terms <- c("CHAT-BOTS", "Self-Esteem", "DÉPRESSION", "WEB 2.0", "聊天机器人", "---", "", "STUDIES OF CLASSES",
             "CHILDREN'S", "TEXT TO SPEECH", "ANALYSIS", "NA")
  expect_identical(.kw_key(terms), per_term(terms))
  expect_identical(.kw_key(character()), character())
})

test_that("brackets are worked out only where there are brackets, with the same result", {
  x <- c("VOICE ASSISTANT(S)", "NATURAL LANGUAGE PROCESSING (NLP)", "PLAIN TERM", "LEARNING (MACHINE)")
  out <- .kw_brackets(x)
  expect_identical(out$term, c("VOICE ASSISTANT", "NATURAL LANGUAGE PROCESSING", "PLAIN TERM", "LEARNING MACHINE"))
  expect_identical(out$short, c(NA, "NLP", NA, NA))
})

test_that("MinHash finds the pairs an exact all-pairs Jaccard finds, and nothing below the threshold", {
  set.seed(3)
  words <- c("MEMORY", "ATTENTION", "ANXIETY", "DEPRESSION", "SOCIAL", "WORKING", "SELF", "ESTEEM", "COGNITIVE",
             "CONTROL", "EMOTION", "REGULATION", "PERSONALITY", "TRAIT", "STRESS", "COPING")
  base <- unique(replicate(250, paste(sample(words, sample(2:3, 1)), collapse = " ")))
  typo <- vapply(base[1:80], \(t) { i <- sample(2:(nchar(t) - 1L), 1); substr(t, i, i) <- "Q"; t }, "")
  terms <- unique(c(base, typo))
  grams <- .kw_trigrams(terms)
  sets <- split(grams$code, grams$term)
  all_pairs <- utils::combn(length(terms), 2L)
  jaccard <- apply(all_pairs, 2L, \(p) {
    a <- sets[[p[1L]]]
    b <- sets[[p[2L]]]
    length(intersect(a, b)) / length(union(a, b))
  })
  truth <- paste(all_pairs[1L, jaccard >= 0.7], all_pairs[2L, jaccard >= 0.7])
  found <- .kw_minhash_pairs(terms)
  found_key <- paste(found$a, found$b)
  expect_gt(length(truth), 50L)
  expect_gte(mean(truth %in% found_key), 0.95)
  expect_true(all(found_key %in% truth))
  expect_identical(.kw_minhash_pairs(terms), found)
  expect_identical(nrow(.kw_minhash_pairs("ONE")), 0L)
})

test_that("groups are stars around the most used form, never chains", {
  papers <- c(10, 5, 1, 20, 3)
  # 2-1 and 3-2 are spelled alike, 3-1 less so; 5 is an acronym of 4.
  spelling <- data.frame(a = c(1L, 2L, 1L), b = c(2L, 3L, 3L), jaccard = c(0.9, 0.8, 0.75))
  acronym <- data.frame(a = 5L, b = 4L)
  expect_identical(.kw_groups(spelling, acronym, papers), c(1L, 1L, 1L, 4L, 4L))
  # A keyword joins its most similar neighbour in more papers, not a chain end.
  chain <- data.frame(a = c(1L, 2L), b = c(2L, 3L), jaccard = c(0.72, 0.9))
  expect_identical(.kw_groups(chain, data.frame(a = integer(), b = integer()), c(100, 50, 10)), c(1L, 1L, 1L))
  # No spelling link points into an acronym, so an acronym and its long form never loop.
  loop <- data.frame(a = 1L, b = 2L, jaccard = 0.8)
  expect_identical(.kw_groups(loop, data.frame(a = 2L, b = 1L), c(5, 50)), c(1L, 1L))
  expect_identical(.kw_groups(spelling[0, ], acronym[0, ], c(1, 2)), 1:2)
})

# ---- level = "all" and keywords_merge() ------------------------------------

decision_papers <- function() {
  data.frame(
    TI = sprintf("Title %d", 1:8),
    DE = c("CHATBOT; NATURAL LANGUAGE PROCESSING (NLP)", "CHAT-BOTS; NLP", "CHATBOT; NLP",
           "CONVERSATIONAL AGENT; INTERPERSONAL JUSTICE", "SELF ESTEEM SCALES; NLP",
           "INTERPERSONAL JUSTICE", "SELF-ESTEEM", "SELF ESTEEM SCALE"),
    ID = c("CHATBOTS", NA, NA, NA, NA, NA, "SELF ESTEEM", NA)
  )
}

# A sheet table edited the way a person or another app edits it.
rewrite_part <- function(file, edit) {
  table <- edit(.kr_read_csv(file))
  unlink(file)
  .kr_write_csv(table, file)
}

part <- function(sheet, name) .kr_read_csv(sub("\\.csv$", sprintf("_%s.csv", name), sheet))

test_that("level = 'all' merges nothing and writes one question per row", {
  sheet <- tempfile(fileext = ".csv")
  papers <- decision_papers()
  out <- kc(papers, columns = c("DE", "ID"), level = "all", decisions = sheet, min_papers = 1)
  expect_identical(out, papers)
  written <- .kr_read_csv(sheet)
  expect_named(written, c("keyword", "keyword_backup", "merge_into", "merge", "papers", "merge_into_papers",
                          "source", "example"))
  expect_identical(written$keyword_backup, written$keyword)
  expect_true(all(is.na(written$merge)))
  expect_false(any(written$keyword == written$merge_into))
  expect_identical(subset(written, keyword == "NLP")$merge_into, "NATURAL LANGUAGE PROCESSING")
  expect_identical(subset(written, keyword == "NLP")$source, "acronym")
  expect_identical(subset(written, keyword == "NLP")$merge_into_papers, 1L)
  expect_true(all(written$example %in% papers$TI))
  prompt <- readLines(sub("\\.csv$", "_prompt.txt", sheet))
  expect_true(any(grepl("merge_into", prompt)))
  expect_error(kc(papers, level = "all", decisions = sheet), class = "saqrmisc_bad_input")
})

test_that("confirmed spellings sit on one row per keyword, in their own table", {
  sheet <- tempfile(fileext = ".csv")
  kc(decision_papers(), columns = c("DE", "ID"), level = "all", decisions = sheet, min_papers = 1)
  confirmed <- part(sheet, "confirmed")
  expect_named(confirmed, c("keyword", "papers", "also_written_as", "source", "merge"))
  expect_false(anyDuplicated(confirmed$keyword) > 0L)
  expect_identical(subset(confirmed, keyword == "CHATBOT")$also_written_as, "CHAT-BOTS (1); CHATBOTS (1)")
  expect_identical(subset(confirmed, keyword == "CHATBOT")$papers, 3L)
  expect_true(all(confirmed$merge == "yes"))
  expect_false("CHAT-BOTS" %in% .kr_read_csv(sheet)$keyword)
})

test_that("a csv sheet never replaces its companion tables", {
  sheet <- tempfile(fileext = ".csv")
  writeLines("taken", sub("\\.csv$", "_confirmed.csv", sheet))
  expect_error(kc(decision_papers(), level = "all", decisions = sheet, min_papers = 1), class = "saqrmisc_bad_input")
})

test_that("invisible characters, wrapping quotes and a trailing star are cleanup, not merges", {
  papers <- data.frame(DE = c("DEPRESSION", "DEPRESSION‌", "'DEPRESSION'", "DEPRESSION*", "DEPRE­SSION",
                              "DON'T"))
  expect_identical(kc(papers)$DE, c(rep("DEPRESSION", 5L), "DON'T"))
  sheet <- tempfile(fileext = ".csv")
  kc(papers, level = "all", decisions = sheet, min_papers = 1)
  expect_identical(nrow(part(sheet, "confirmed")), 0L)
})

test_that("pairs spelled alike but with another first letter or a prefix are not proposed", {
  expect_identical(.kw_unlike_words(c("DEPRESSION", "PREADOLESCENTS", "SOCIAL INJUSTICE", "SELF-ESTEEM SCALE",
                                      "SUBSAMPLE", "CHATBOTS ASSISTANT"),
                                    c("REPRESSION", "ADOLESCENTS", "SOCIAL JUSTICE", "SELF ESTEEM SCALES",
                                      "SAMPLE", "CHATBOT ASSISTANT")),
                   c(TRUE, TRUE, TRUE, FALSE, TRUE, FALSE))
  papers <- data.frame(TI = "t", DE = c("DEPRESSION; ADOLESCENTS", "REPRESSION; PREADOLESCENTS",
                                        "SOCIAL JUSTICE; SOCIAL INJUSTICE"))
  sheet <- tempfile(fileext = ".csv")
  kc(papers, level = "all", decisions = sheet, min_papers = 1)
  expect_identical(nrow(.kr_read_csv(sheet)), 0L)
})

test_that("min_papers keeps rare keywords out of every table", {
  sheet <- tempfile(fileext = ".csv")
  kc(decision_papers(), level = "all", decisions = sheet, min_papers = 2)
  expect_true(all(.kr_read_csv(sheet)$papers >= 2))
  expect_true(all(part(sheet, "confirmed")$papers >= 2))
  expect_true(all(part(sheet, "all_keywords")$papers >= 2))
  expect_false("CONVERSATIONAL AGENT" %in% part(sheet, "all_keywords")$keyword)
})

test_that("every keyword in min_papers or more papers is in all_keywords, once", {
  sheet <- tempfile(fileext = ".csv")
  kc(decision_papers(), level = "all", decisions = sheet, min_papers = 1)
  everything <- part(sheet, "all_keywords")
  expect_named(everything, c("keyword", "keyword_backup", "papers", "example"))
  expect_true(all(c("CONVERSATIONAL AGENT", "SELF ESTEEM SCALE", "NLP") %in% everything$keyword))
  expect_false(anyDuplicated(everything$keyword) > 0L)
})

test_that("keywords_merge applies yes, keeps no, and undoes a confirmed keyword marked no", {
  sheet <- tempfile(fileext = ".csv")
  papers <- decision_papers()
  kc(papers, columns = c("DE", "ID"), level = "all", decisions = sheet, min_papers = 1)
  rewrite_part(sheet, \(t) within(t, merge <- ifelse(keyword == "NLP", "Yes ", "no")))
  rewrite_part(sub("\\.csv$", "_confirmed.csv", sheet), \(t) within(t, merge <- ifelse(keyword == "SELF ESTEEM", "no", NA)))
  expect_message(out <- keywords_merge(papers, decisions = sheet, columns = c("DE", "ID")),
                 "1 merge\\(s\\) applied, 1 confirmed keyword\\(s\\) kept apart, 0 row\\(s\\) unanswered")
  expect_identical(out$DE[2], "CHATBOT; NATURAL LANGUAGE PROCESSING")
  expect_identical(out$DE[7], "SELF-ESTEEM")
  expect_identical(out$ID[1], "CHATBOT")
  # The data before or after cleaning gives the same result, but for the undone merge.
  cleaned <- kc(papers, columns = c("DE", "ID"))
  expect_identical(suppressMessages(keywords_merge(cleaned, decisions = sheet, columns = c("DE", "ID")))$DE[-7],
                   out$DE[-7])
})

test_that("an edited merge_into sends the keyword there; rows match on keyword_backup", {
  sheet <- tempfile(fileext = ".csv")
  papers <- decision_papers()
  kc(papers, level = "all", decisions = sheet, min_papers = 1)
  rewrite_part(sheet, \(t) within(t, {
    merge <- ifelse(keyword_backup == "NLP", "yes", NA)
    merge_into <- ifelse(keyword_backup == "NLP", "LANGUAGE TECHNOLOGY", merge_into)
    keyword <- ifelse(keyword_backup == "NLP", "edited by hand", keyword)
  }))
  out <- suppressMessages(keywords_merge(papers, decisions = sheet))
  expect_identical(out$DE[2], "CHATBOT; LANGUAGE TECHNOLOGY")
  expect_identical(out$DE[1], "CHATBOT; NATURAL LANGUAGE PROCESSING")
})

test_that("a sheet is validated", {
  bad <- tempfile(fileext = ".csv")
  .kr_write_csv(data.frame(keyword = "A", merge_into = "B", merge = "maybe"), bad)
  expect_error(keywords_merge(decision_papers(), decisions = bad), class = "saqrmisc_bad_input")
  no_cols <- tempfile(fileext = ".csv")
  .kr_write_csv(data.frame(keyword = "A", merge = "yes"), no_cols)
  expect_error(keywords_merge(decision_papers(), decisions = no_cols), class = "saqrmisc_bad_input")
  expect_error(keywords_merge(decision_papers()), class = "saqrmisc_bad_input")
  expect_error(keywords_merge(decision_papers(), decisions = tempfile(fileext = ".csv")), class = "saqrmisc_bad_input")
})

test_that("the sheet round-trips through .xlsx, one tab per table", {
  skip_if_not_installed("readxl")
  skip_if_not_installed("writexl")
  sheet <- tempfile(fileext = ".xlsx")
  kc(decision_papers(), level = "all", decisions = sheet, min_papers = 1)
  expect_identical(readxl::excel_sheets(sheet), c("decisions", "confirmed", "all_keywords", "prompt"))
  checked <- as.data.frame(readxl::read_excel(sheet, sheet = "decisions"))
  checked$merge <- ifelse(checked$keyword == "NLP", "yes", NA)
  confirmed <- as.data.frame(readxl::read_excel(sheet, sheet = "confirmed"))
  confirmed$merge <- ifelse(confirmed$keyword == "CHATBOT", "no", NA)
  unlink(sheet)
  writexl::write_xlsx(list(decisions = checked, confirmed = confirmed), sheet)
  out <- suppressMessages(keywords_merge(decision_papers(), decisions = sheet))
  expect_identical(out$DE[1:2], c("CHATBOT; NATURAL LANGUAGE PROCESSING", "CHAT-BOTS; NATURAL LANGUAGE PROCESSING"))
})

test_that("by = 'edit' maps each untouched keyword_backup to its edited keyword, from both tables", {
  sheet <- tempfile(fileext = ".csv")
  papers <- decision_papers()
  kc(papers, columns = c("DE", "ID"), level = "all", decisions = sheet, min_papers = 1)
  # OpenRefine-style on all_keywords: a keyword renamed, a case-only edit; the
  # same NLP edit in both tables counts once.
  rewrite_part(sub("\\.csv$", "_all_keywords.csv", sheet), \(t) within(t, {
    keyword[keyword_backup == "CHATBOT"] <- "CONVERSATIONAL AGENT"
    keyword[keyword_backup == "INTERPERSONAL JUSTICE"] <- "interpersonal justice"
    keyword[keyword_backup == "NLP"] <- "NATURAL LANGUAGE PROCESSING"
  }))
  rewrite_part(sheet, \(t) within(t, {
    keyword[keyword_backup == "NLP"] <- "NATURAL LANGUAGE PROCESSING"
    merge <- "no"
  }))
  expect_message(out <- keywords_merge(papers, decisions = sheet, by = "edit", columns = c("DE", "ID")),
                 "2 edited keyword\\(s\\) applied, 0 to a spelling not in the data")
  # CHAT-BOTS and CHATBOTS follow CHATBOT; NLP becomes the long form; case alone changes nothing.
  expect_identical(out$DE[1:3], rep("CONVERSATIONAL AGENT; NATURAL LANGUAGE PROCESSING", 3L))
  expect_identical(out$ID[1], "CONVERSATIONAL AGENT")
  expect_identical(out$DE[6], "INTERPERSONAL JUSTICE")
})

test_that("by = 'edit' needs keyword and keyword_backup", {
  sheet <- tempfile(fileext = ".csv")
  .kr_write_csv(data.frame(keyword = "A", merge_into = "A", merge = NA), sheet)
  expect_error(keywords_merge(decision_papers(), decisions = sheet, by = "edit"), class = "saqrmisc_bad_input")
})

test_that("answers can come from another column; empty rows are not merged and are counted", {
  sheet <- tempfile(fileext = ".csv")
  papers <- decision_papers()
  kc(papers, level = "all", decisions = sheet, min_papers = 1)
  rewrite_part(sheet, \(t) within(t, Tomerge <- ifelse(keyword == "NLP", "yes", NA)))
  out <- suppressMessages(keywords_merge(papers, decisions = sheet, answers = "Tomerge"))
  expect_identical(out$DE[2], "CHATBOT; NATURAL LANGUAGE PROCESSING")
  expect_message(keywords_merge(papers, decisions = sheet), "0 merge\\(s\\) applied, 0 confirmed keyword\\(s\\) kept apart, 1 row\\(s\\) unanswered")
  expect_error(keywords_merge(papers, decisions = sheet, answers = "absent"), class = "saqrmisc_bad_input")
  expect_error(keywords_merge(papers, decisions = sheet, answers = NA_character_), class = "saqrmisc_bad_input")
})
