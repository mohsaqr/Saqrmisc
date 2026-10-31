kw_of <- function(cleaned, field = "DE") as.data.frame(cleaned)[[field]]

test_that("a replacement term is replaced wherever it sits in the list", {
  # bibliometrix writes ";  " after the first keyword; the old code then
  # missed every keyword from the third position on.
  papers <- data.frame(DE = c("CHATBOT;  AGENT;  ARTIFICIAL INTELLIGENCE",
                              "ARTIFICIAL INTELLIGENCE",
                              "ARTIFICIAL INTELLIGENCE; CHATBOT"))
  cleaned <- keywords_clean(papers, replace = data.frame(OLD = "ARTIFICIAL INTELLIGENCE", NEW = "AI"))
  expect_equal(kw_of(cleaned), c("CHATBOT; AGENT; AI", "AI", "AI; CHATBOT"))
})

test_that("separator spacing and case do not change the result", {
  a <- keywords_clean(data.frame(DE = "Chatbot;  E-Learning;  Usability"))
  b <- keywords_clean(data.frame(DE = "CHATBOT;E-LEARNING ; USABILITY"))
  expect_equal(kw_of(a), kw_of(b))
  expect_equal(kw_of(a), "CHATBOT; E-LEARNING; USABILITY")
})

test_that("regex characters in terms are taken literally", {
  papers <- data.frame(DE = c("C++; WEB 2.0", "CXX; WEB 230"))
  cleaned <- keywords_clean(papers, replace = data.frame(OLD = c("C++", "WEB 2.0"),
                                                           NEW = c("CPLUSPLUS", "WEB2")))
  expect_equal(kw_of(cleaned), c("CPLUSPLUS; WEB2", "CXX; WEB 230"))
})

test_that("replacement chains resolve in any row order; a loop is broken by the later row", {
  papers <- data.frame(DE = "A TERM; B TERM")
  chain <- data.frame(OLD = c("B TERM", "A TERM"), NEW = c("C TERM", "B TERM"))
  expect_equal(kw_of(keywords_clean(papers, replace = chain)), "C TERM")
  loop <- data.frame(OLD = c("A TERM", "B TERM"), NEW = c("B TERM", "A TERM"))
  expect_warning(cleaned <- keywords_clean(papers, replace = loop),
                 class = "saqrmisc_replace_conflict")
  expect_equal(kw_of(cleaned), "A TERM")
})

test_that("a list of thesauri applies in order, so a later table corrects an earlier one", {
  papers <- data.frame(DE = c("NATURAL LANGUAGE PROCESSING", "NLP; NATURAL LANGUAGE"))
  first <- data.frame(OLD = c("NATURAL LANGUAGE PROCESSING", "NATURAL LANGUAGE"), NEW = c("NLP", "NLP"))
  second <- data.frame(OLD = "NLP", NEW = "NATURAL LANGUAGE PROCESSING")
  cleaned <- keywords_clean(papers, replace = list(first, second))
  expect_equal(kw_of(cleaned), c("NATURAL LANGUAGE PROCESSING", "NATURAL LANGUAGE PROCESSING"))
  expect_warning(combined <- keywords_clean(papers, replace = rbind(first, second)),
                 class = "saqrmisc_replace_conflict")
  expect_equal(kw_of(combined), kw_of(cleaned))
})

test_that("bracketed NEW terms are cleaned like the data, so keep-as-is rows stay identities", {
  papers <- data.frame(DE = "TEXT-TO-SPEECH (TTS)")
  table <- data.frame(OLD = c("TEXT-TO-SPEECH (TTS)", "TEXT-TO-SPEECH"),
                      NEW = c("TEXT-TO-SPEECH (TTS)", "TEXT TO SPEECH"))
  expect_no_warning(cleaned <- keywords_clean(papers, replace = table, merge_similar = FALSE))
  expect_equal(kw_of(cleaned), "TEXT TO SPEECH")
})

test_that("conflicting replacement rows warn and the later row wins", {
  papers <- data.frame(DE = "VIRTUAL AGENTS")
  conflict <- data.frame(OLD = c("VIRTUAL AGENTS", "VIRTUAL AGENTS"),
                         NEW = c("VIRTUAL ASSISTANT", "VIRTUAL AGENT"))
  expect_warning(cleaned <- keywords_clean(papers, replace = conflict),
                 class = "saqrmisc_replace_conflict")
  expect_equal(kw_of(cleaned), "VIRTUAL AGENT")
})

test_that("spelling variants merge, preferring a replacement target, then the most used form", {
  papers <- data.frame(DE = c("CHAT BOT; ROBOTS", "CHAT-BOTS; ROBOT", "CHATBOTS; ROBOTS", "CHATBOTS"))
  merged <- keywords_clean(papers)
  expect_equal(kw_of(merged), c("CHATBOTS; ROBOTS", "CHATBOTS; ROBOTS", "CHATBOTS; ROBOTS", "CHATBOTS"))
  preferred <- keywords_clean(papers, replace = data.frame(OLD = "CHAT BOT", NEW = "CHATBOT"))
  expect_equal(kw_of(preferred), c("CHATBOT; ROBOTS", "CHATBOT; ROBOTS", "CHATBOT; ROBOTS", "CHATBOT"))
  # With every form in one paper, the shortest wins.
  tied <- keywords_clean(data.frame(DE = c("CHAT BOT", "CHATBOTS", "CHATBOT")))
  expect_equal(unique(kw_of(tied)), "CHATBOT")
})

test_that("variant merging leaves distinct terms apart", {
  papers <- data.frame(DE = c("WEB 2.0; STATUS; ANALYSIS; 聊天机器人",
                              "WEB 3.0; STATU; ANALYSI; 教育"))
  cleaned <- keywords_clean(papers)
  expect_equal(kw_of(cleaned), c("WEB 2.0; STATUS; ANALYSIS; 聊天机器人",
                                 "WEB 3.0; STATU; ANALYSI; 教育"))
})

test_that("brackets: plural markers, acronyms in either order, other text kept", {
  papers <- data.frame(DE = c("VOICE ASSISTANT(S); NATURAL LANGUAGE PROCESSING(NLP)",
                              "B-RNN (BI-DIRECTIONAL RECURRENT NEURAL NETWORK); NLP",
                              "LEARNING (ARTIFICIAL INTELLIGENCE)"))
  cleaned <- keywords_clean(papers, merge_similar = FALSE)
  expect_equal(kw_of(cleaned), c("VOICE ASSISTANT; NATURAL LANGUAGE PROCESSING",
                                 "BI-DIRECTIONAL RECURRENT NEURAL NETWORK; NATURAL LANGUAGE PROCESSING",
                                 "LEARNING ARTIFICIAL INTELLIGENCE"))
  kept <- keywords_clean(papers, merge_similar = FALSE, expand_acronyms = FALSE)
  expect_equal(kw_of(kept)[2], "BI-DIRECTIONAL RECURRENT NEURAL NETWORK; NLP")
})

test_that("an acronym is not expanded when another keyword spells the same initials", {
  papers <- data.frame(DE = c("HUMAN-ROBOT INTERFACE (HRI)", "HUMAN ROBOT INTERACTION", "HRI",
                              "INTERNET OF THINGS (IOT)", "IOT"))
  cleaned <- kw_of(keywords_clean(papers, merge_similar = FALSE))
  expect_equal(cleaned[3], "HRI")
  expect_equal(cleaned[5], "INTERNET OF THINGS")
})

test_that("an acronym with two long forms is not expanded", {
  papers <- data.frame(DE = c("AUGMENTED REALITY (AR)", "ACTION RESEARCH (AR)", "AR"))
  expect_equal(kw_of(keywords_clean(papers))[3], "AR")
})

test_that("drop deletes terms and their similar spellings; each keyword appears once per paper", {
  papers <- data.frame(DE = c("CHATBOT; SURVEY; CHATBOTS", "SURVEYS", NA))
  cleaned <- keywords_clean(papers, drop = "survey")
  expect_equal(kw_of(cleaned), c("CHATBOT", NA, NA))
  changes <- keywords_changes(cleaned)
  expect_true(all(is.na(changes$to[grepl("dropped", changes$reason)])))
})

test_that("several fields are cleaned and other columns are untouched", {
  papers <- data.frame(id = 1:2, DE = c("CHATBOTS", "CHATBOT"),
                       ID = c("HUMAN COMPUTER INTERACTION", "HUMAN-COMPUTER INTERACTION"),
                       TI = c("One", "Two"))
  cleaned <- keywords_clean(papers, columns = c("DE", "ID"))
  out <- as.data.frame(cleaned)
  expect_equal(out$TI, papers$TI)
  expect_equal(length(unique(out$DE)), 1L)
  expect_equal(length(unique(out$ID)), 1L)
  expect_setequal(unique(as.data.frame(cleaned, what = "terms")$column), c("DE", "ID"))
})

test_that("the result does not depend on the order of the papers", {
  papers <- data.frame(id = 1:5, DE = c("CHAT BOT; AI", "CHATBOTS", "CHATBOT; E-LEARNING",
                                        "ELEARNING; CHAT-BOT", "E LEARNING"))
  a <- as.data.frame(keywords_clean(papers))
  b <- as.data.frame(keywords_clean(papers[rev(seq_len(nrow(papers))), ]))
  expect_equal(b$DE[match(a$id, b$id)], a$DE)
})

test_that("summary and terms add up", {
  papers <- data.frame(DE = c("CHAT BOT; AI", "CHATBOTS; SURVEY", "CHATBOT"))
  cleaned <- keywords_clean(papers, drop = "SURVEY")
  counts <- summary(cleaned)
  terms <- as.data.frame(cleaned, what = "terms")
  expect_named(counts, c("column", "papers", "forms_before", "keywords_after", "changed",
                         "cleanup", "brackets", "acronym", "replaced", "merged", "dropped"))
  expect_equal(counts$forms_before, 5)
  expect_equal(counts$keywords_after, nrow(terms))
  expect_equal(terms$papers[terms$keyword == "CHATBOT"], 3)
  expect_output(print(cleaned), "5 written forms became 2 keywords")
})

test_that("a field with nothing to change, or nothing left, still gives tidy tables", {
  same <- keywords_clean(data.frame(DE = c("CHATBOT", "AGENT")))
  expect_equal(nrow(keywords_changes(same)), 0L)
  gone <- keywords_clean(data.frame(DE = "CHATBOT"), drop = "CHATBOT")
  expect_equal(nrow(as.data.frame(gone, what = "terms")), 0L)
  expect_true(is.na(kw_of(gone)))
})

test_that("replace takes .csv and .xlsx files, applied in the order given", {
  papers <- data.frame(DE = c("NATURAL LANGUAGE PROCESSING", "NLP; NATURAL LANGUAGE"))
  first <- tempfile(fileext = ".csv")
  utils::write.csv(data.frame(OLD = c("NATURAL LANGUAGE PROCESSING", "NATURAL LANGUAGE"),
                              NEW = c("NLP", "NLP")), first, row.names = FALSE)
  second <- tempfile(fileext = ".csv")
  utils::write.csv(data.frame(OLD = "NLP", NEW = "NATURAL LANGUAGE PROCESSING"), second, row.names = FALSE)
  from_files <- keywords_clean(papers, replace = c(first, second))
  expect_equal(kw_of(from_files), c("NATURAL LANGUAGE PROCESSING", "NATURAL LANGUAGE PROCESSING"))
  expect_equal(kw_of(keywords_clean(papers, replace = first)), c("NLP", "NLP"))

  skip_if_not_installed("readxl")
  skip_if_not_installed("writexl")
  workbook <- tempfile(fileext = ".xlsx")
  writexl::write_xlsx(data.frame(OLD = "NLP", NEW = "NATURAL LANGUAGE PROCESSING"), workbook)
  mixed <- keywords_clean(papers, replace = list(first, workbook))
  expect_equal(kw_of(mixed), kw_of(from_files))
})

test_that("a bad replace file raises saqrmisc_bad_input", {
  papers <- data.frame(DE = "CHATBOT")
  expect_error(keywords_clean(papers, replace = "no-such-table.csv"), class = "saqrmisc_bad_input")
  text_file <- tempfile(fileext = ".txt")
  writeLines("OLD,NEW", text_file)
  expect_error(keywords_clean(papers, replace = text_file), class = "saqrmisc_bad_input")
  no_columns <- tempfile(fileext = ".csv")
  utils::write.csv(data.frame(from = "A", to = "B"), no_columns, row.names = FALSE)
  expect_error(keywords_clean(papers, replace = no_columns), class = "saqrmisc_bad_input")
})

test_that("bad input raises saqrmisc_bad_input", {
  papers <- data.frame(DE = "CHATBOT", N = 1)
  expect_error(keywords_clean(list()), class = "saqrmisc_bad_input")
  expect_error(keywords_clean(papers, columns = "XX"), class = "saqrmisc_bad_input")
  expect_error(keywords_clean(papers, columns = "N"), class = "saqrmisc_bad_input")
  expect_error(keywords_clean(papers, replace = data.frame(from = "A", to = "B")),
               class = "saqrmisc_bad_input")
  expect_error(keywords_clean(papers, drop = 1), class = "saqrmisc_bad_input")
  expect_error(keywords_clean(papers, merge_similar = NA), class = "saqrmisc_bad_input")
})

test_that("keywords_changes gives the changes and refuses anything else", {
  cleaned <- keywords_clean(data.frame(DE = c("Chatbots; ChatBot", "chat-bot; LLMs")))
  expect_identical(keywords_changes(cleaned), as.data.frame(cleaned, what = "changes"))
  expect_true(nrow(keywords_changes(cleaned)) > 0L)
  expect_error(keywords_changes(data.frame()), class = "saqrmisc_bad_input")
})
