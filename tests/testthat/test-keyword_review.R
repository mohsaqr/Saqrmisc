# Papers with one pair per kind, plus look-alikes the guards must keep apart.
review_papers <- function() {
  data.frame(
    TI = c("Coaching for behavior change", "Behaviour change at work",
           "Recurrent networks for dialogue", "Dialogue with RNN models",
           "Behavior change and mediation", "Meditation apps"),
    DE = c("BEHAVIOR CHANGE; SOCIAL NETWORK; WEB 2.0",
           "BEHAVIOUR CHANGE; SOCIAL NETWORKING; WEB 3.0",
           "RECURRENT NEURAL NETWORK; DATA ANALYSIS; AI",
           "RNN; ANALYSIS OF DATA; AR; AI INTERVIEWER",
           "BEHAVIOR CHANGE; MEDIATION",
           "MEDITATION"),
    stringsAsFactors = FALSE
  )
}

# The spelling methods alone, so that these tests need no sbert model.
spelled <- function(...) suggest_merges(..., meaning = FALSE)

pair_set <- function(merges) {
  x <- as.data.frame(merges)
  sort(paste(x$keyword, x$variant, x$found_by, sep = " | "))
}

# A fake chat server: `judge(pair)` gives the answer for one pair (NULL
# leaves it out). It replies in the request's format (Anthropic when the body
# has `system`) and records the requests in `log`.
fake_server <- function(judge, log = new.env()) {
  log$requests <- list()
  function(req) {
    log$requests[[length(log$requests) + 1L]] <- req
    body <- req$body$data
    anthropic <- !is.null(body$system)
    user <- if (anthropic) body$messages[[1]]$content else body$messages[[2]]$content
    ids <- as.integer(sub("id ", "", regmatches(user, gregexpr("(?m)^id [0-9]+", user, perl = TRUE))[[1]]))
    answers <- Filter(Negate(is.null), lapply(ids, judge))
    text <- paste0("Here you go:\n```json\n",
                   jsonlite::toJSON(answers, auto_unbox = TRUE, null = "null"), "\n```")
    if (anthropic) {
      httr2::response_json(body = list(content = list(list(type = "text", text = text))))
    } else {
      httr2::response_json(body = list(choices = list(list(message = list(content = text)))))
    }
  }
}

# ask_ai() with its progress messages captured: list(value, messages).
asked <- function(...) {
  messages <- testthat::capture_messages(value <- ask_ai(...))
  list(value = value, messages = messages)
}

all_same <- function(id) list(id = id, verdict = "same", name = "merged name", confidence = 0.9, reason = "variants")

test_that("each kind of pair is suggested, the suggested name first", {
  merges <- spelled(review_papers())
  expect_s3_class(merges, "saqr_proposals")
  expect_setequal(pair_set(merges), c(
    "BEHAVIOR CHANGE | BEHAVIOUR CHANGE | spelling",
    "MEDIATION | MEDITATION | spelling",
    "DATA ANALYSIS | ANALYSIS OF DATA | word order",
    "SOCIAL NETWORK | SOCIAL NETWORKING | word ending",
    "RECURRENT NEURAL NETWORK | RNN | acronym"
  ))
  x <- as.data.frame(merges)
  expect_named(x, c("pair", "keyword", "variant", "decision", "found_by", "keyword_papers", "variant_papers",
                    "cosine", "ai_verdict", "ai_name", "ai_confidence", "ai_reason"))
  expect_identical(x$pair, seq_len(nrow(x)))
  expect_true(all(is.na(x$ai_verdict)))
  expect_true(all(x$decision == "check"))
  expect_identical(x$keyword_papers[x$keyword == "BEHAVIOR CHANGE"], 2L)
  expect_output(print(merges), "5 possible merges in DE")
})

test_that("numbers, short words and phrases containing the acronym are not suggested", {
  x <- as.data.frame(spelled(review_papers()))
  pairs <- paste(x$keyword, x$variant)
  expect_false(any(grepl("WEB", pairs)))
  expect_false(any(grepl("\\bAR\\b", pairs)))
  expect_false(any(grepl("AI INTERVIEWER", pairs)))
})

test_that("the evidence table is counted by hand", {
  papers <- data.frame(DE = c("CHATBOT; USER TRUST; DESIGN", "CHATBOOT; USER TRUST", "CHATBOT; CHATBOOT"))
  merges <- spelled(papers)
  expect_identical(as.data.frame(merges)$keyword, "CHATBOT")
  evidence <- as.data.frame(merges, what = "evidence")
  expect_named(evidence, c("pair", "column", "together", "shared_keywords", "similarity",
                           "example_keyword", "example_variant", "ai_model"))
  expect_identical(evidence$together, 1L)
  # CHATBOT is seen with USER TRUST and DESIGN, CHATBOOT with USER TRUST.
  expect_equal(evidence$shared_keywords, 0.5)
  expect_equal(evidence$similarity, round(1 - 1 / 8, 2))
  expect_true(is.na(evidence$example_keyword))
})

test_that("the row order of the papers does not change the pairs", {
  shuffled <- review_papers()[c(6, 3, 1, 5, 2, 4), ]
  expect_identical(pair_set(spelled(shuffled)), pair_set(spelled(review_papers())))
})

test_that("keywords without pairs give an empty result, not an error", {
  merges <- spelled(data.frame(DE = c("ROBOT", "TABLE")))
  expect_identical(nrow(as.data.frame(merges)), 0L)
  expect_identical(nrow(summary(merges)), 0L)
  expect_output(print(merges), "0 possible merges in DE")
  none <- spelled(data.frame(DE = character()))
  expect_identical(nrow(as.data.frame(none)), 0L)
})

test_that("the printed table fits the console", {
  papers <- data.frame(DE = c("NATURAL LANGUAGE INTERFACE TO RELATIONAL DATABASES; BEHAVIOR CHANGE",
                              "RELATIONAL DATABASES NATURAL LANGUAGE INTERFACE TO; BEHAVIOUR CHANGE"))
  old <- options(width = 60)
  on.exit(options(old), add = TRUE)
  printed <- capture.output(print(spelled(papers)))
  expect_true(all(nchar(printed) <= 60))
  expect_false(any(grepl("meaning", printed)))
})

# sbert and its model must be on this machine; the model is never downloaded
# by a test.
skip_without_model <- function() {
  skip_if_not_installed("sbert")
  skip_if_not(all(sbert::model_status()$valid), "the sbert model is not downloaded")
}

meaning_papers <- function() {
  data.frame(DE = c("HUMAN-COMPUTER INTERFACE; CHATBOT; ONTOLOGY",
                    "HUMAN-MACHINE INTERFACE; CHATBOT SOFTWARE; ONCOLOGY"))
}

test_that("meaning finds synonyms, skips narrower terms, and orders the pairs", {
  skip_without_model()
  x <- as.data.frame(suggest_merges(meaning_papers()))
  expect_identical(paste(x$keyword, x$variant, x$found_by, sep = " | "), c(
    "HUMAN-MACHINE INTERFACE | HUMAN-COMPUTER INTERFACE | meaning",
    "ONCOLOGY | ONTOLOGY | spelling"
  ))
  # CHATBOT SOFTWARE scores above the cut but is a narrower term.
  expect_false(any(grepl("CHATBOT", x$variant)))
  expect_false(is.unsorted(-x$cosine))
  expect_identical(x$pair, seq_len(nrow(x)))
  expect_output(print(suggest_merges(meaning_papers())), "meaning 1")
})

test_that("the cosine matches one computed from raw embeddings", {
  skip_without_model()
  x <- as.data.frame(suggest_merges(meaning_papers()))
  raw <- sbert::encode(tolower(c(x$keyword, x$variant)), normalize = FALSE)
  n <- nrow(x)
  by_hand <- vapply(seq_len(n), \(i) {
    u <- raw[i, ]
    v <- raw[n + i, ]
    sum(u * v) / sqrt(sum(u^2) * sum(v^2))
  }, numeric(1))
  expect_equal(x$cosine, round(by_hand, 2), tolerance = 1e-12)
  expect_true(all(x$cosine >= -1 & x$cosine <= 1))
})

test_that("meaning does not change the spelling pairs, only adds to them", {
  skip_without_model()
  with_meaning <- as.data.frame(suggest_merges(review_papers()))
  without <- as.data.frame(spelled(review_papers()))
  expect_true(all(is.na(without$cosine)))
  expect_false(anyNA(with_meaning$cosine))
  key <- \(x) paste(x$keyword, x$variant)
  expect_true(all(key(without) %in% key(with_meaning)))
})

test_that("a missing column is refused", {
  expect_error(spelled(review_papers(), columns = "ID"), class = "saqrmisc_bad_input")
  expect_error(spelled("not a data frame"), class = "saqrmisc_bad_input")
  expect_error(suggest_merges(review_papers(), meaning = NA), class = "saqrmisc_bad_input")
  expect_error(suggest_merges(review_papers(), meaning = "yes"), class = "saqrmisc_bad_input")
  local_mocked_bindings(.kr_has_sbert = function() FALSE)
  expect_error(suggest_merges(review_papers()), class = "saqrmisc_missing_package")
})

test_that("ask_ai talks to an OpenAI-compatible server", {
  skip_if_not_installed("httr2")
  skip_if_not_installed("jsonlite")
  log <- new.env()
  httr2::local_mocked_responses(fake_server(all_same, log))
  run <- asked(spelled(review_papers()), model = "llama3.1", key = "secret",
               url = "http://localhost:11434/v1")
  expect_match(run$messages[1], "Asking llama3.1 about 5 pair")
  expect_length(log$requests, 1L)
  req <- log$requests[[1]]
  expect_identical(req$url, "http://localhost:11434/v1/chat/completions")
  expect_identical(req$body$data$model, "llama3.1")
  expect_identical(req$body$data$temperature, 0)
  expect_identical(httr2::req_get_headers(req, "reveal")$Authorization, "Bearer secret")
  x <- as.data.frame(run$value)
  expect_true(all(x$ai_verdict == "same"))
  expect_true(all(x$ai_name == "MERGED NAME"))
  expect_true(all(as.data.frame(run$value, what = "evidence")$ai_model == "llama3.1"))
  expect_false(grepl("secret", paste(deparse(run$value), collapse = ""), fixed = TRUE))
})

test_that("a claude model goes to Anthropic, and instructions reach the prompt", {
  skip_if_not_installed("httr2")
  log <- new.env()
  httr2::local_mocked_responses(fake_server(all_same, log))
  run <- asked(spelled(review_papers()), model = "claude-test", key = "k",
               instructions = "The papers are on health coaching.")
  req <- log$requests[[1]]
  expect_identical(req$url, "https://api.anthropic.com/v1/messages")
  headers <- httr2::req_get_headers(req, "reveal")
  expect_identical(headers$`x-api-key`, "k")
  expect_identical(headers$`anthropic-version`, "2023-06-01")
  expect_match(req$body$data$system, "health coaching", fixed = TRUE)
  expect_true(all(as.data.frame(run$value)$ai_verdict == "same"))
})

test_that("without instructions the prompt names no field", {
  prompt <- .ai_system_prompt(NULL)
  expect_match(prompt, "Work out the research field from the example titles", fixed = TRUE)
  expect_false(grepl("researcher adds", prompt, fixed = TRUE))
})

test_that("a model that refuses temperature is asked again without it", {
  skip_if_not_installed("httr2")
  log <- new.env()
  answer <- fake_server(all_same, log)
  httr2::local_mocked_responses(function(req) {
    if (!is.null(req$body$data$temperature)) {
      return(httr2::response_json(status_code = 400L,
                                  body = list(error = list(message = "Unsupported value: 'temperature'"))))
    }
    answer(req)
  })
  run <- asked(spelled(review_papers()), model = "gpt-5-mini", key = "k")
  expect_null(log$requests[[1]]$body$data$temperature)
  expect_true(all(as.data.frame(run$value)$ai_verdict == "same"))
})

test_that("unreadable answers are retried once, then asked again next time", {
  skip_if_not_installed("httr2")
  log <- new.env()
  httr2::local_mocked_responses(fake_server(\(id) if (id == 1L) NULL else all_same(id), log))
  expect_warning(merges <- asked(spelled(review_papers()), model = "m", key = "k")$value,
                 class = "saqrmisc_ai_failed")
  expect_length(log$requests, 2L)
  x <- as.data.frame(merges)
  expect_identical(x$ai_verdict[x$pair == 1L], "failed")
  expect_output(print(merges), "1 not answered")

  httr2::local_mocked_responses(fake_server(all_same))
  again <- asked(merges, model = "m", key = "k")
  expect_match(again$messages[1], "about 1 pair")
  expect_true(all(as.data.frame(again$value)$ai_verdict == "same"))
})

test_that("a failing first request is an error; a later failure keeps the answers", {
  skip_if_not_installed("httr2")
  many <- data.frame(DE = paste0("CONCEPT", sprintf("%02d", 1:30), "ABC; CONCEPT", sprintf("%02d", 1:30), "ABD"))
  merges <- spelled(many)
  expect_identical(nrow(as.data.frame(merges)), 30L)
  httr2::local_mocked_responses(function(req) httr2::response(status_code = 401))
  expect_error(asked(merges, model = "m", key = "bad"), class = "saqrmisc_ai_failed")

  log <- new.env()
  answer <- fake_server(all_same, log)
  httr2::local_mocked_responses(function(req) {
    if (length(log$requests) >= 1L) return(httr2::response(status_code = 500))
    answer(req)
  })
  expect_warning(partial <- asked(merges, model = "m", key = "k")$value, class = "saqrmisc_ai_failed")
  expect_identical(sum(!is.na(as.data.frame(partial)$ai_verdict)), 20L)

  httr2::local_mocked_responses(fake_server(all_same))
  done <- asked(partial, model = "m", key = "k")
  expect_match(done$messages[1], "about 10 pair")
  expect_false(anyNA(as.data.frame(done$value)$ai_verdict))
})

test_that("again asks only about the chosen verdicts", {
  skip_if_not_installed("httr2")
  httr2::local_mocked_responses(fake_server(\(id) list(id = id, verdict = if (id <= 2L) "unsure" else "different",
                                                       name = NULL, confidence = 0.5, reason = "r")))
  first <- asked(spelled(review_papers()), model = "small", key = "k")$value
  expect_message(ask_ai(first, model = "small", key = "k"), "judged every pair")
  httr2::local_mocked_responses(fake_server(all_same))
  second <- asked(first, model = "big", key = "k", again = "unsure")
  expect_match(second$messages[1], "about 2 pair")
  models <- as.data.frame(second$value, what = "evidence")$ai_model
  expect_identical(models, c("big", "big", "small", "small", "small"))
})

test_that("ask_ai checks its arguments", {
  merges <- spelled(review_papers())
  skip_if(nzchar(Sys.getenv("ANTHROPIC_API_KEY")), "an Anthropic key is set")
  expect_error(ask_ai(merges, model = "claude-x"), class = "saqrmisc_bad_input")
  expect_error(ask_ai(merges, key = "k"), class = "saqrmisc_bad_input")
  expect_error(ask_ai(data.frame(), model = "m"), class = "saqrmisc_bad_input")
  expect_error(ask_ai(merges, model = "m", key = "k", url = "localhost"), class = "saqrmisc_bad_input")
  expect_error(ask_ai(merges, model = "m", key = "k", again = "maybe"), class = "saqrmisc_bad_input")
  expect_identical(.ai_endpoint("https://openrouter.ai/api/v1/", "openai"),
                   "https://openrouter.ai/api/v1/chat/completions")
  expect_identical(.ai_endpoint("http://localhost:1234", "openai"), "http://localhost:1234/v1/chat/completions")
})

test_that("the parser reads fenced, partial and malformed answers", {
  text <- "```json\n[{\"id\": \"1\", \"verdict\": \"SAME\", \"name\": \"chat-bot (cb)\", \"confidence\": 1.5},\n {\"id\": 2, \"verdict\": \"maybe\"}]\n```"
  x <- .ai_parse(text, 1:3)
  expect_identical(x$ai_verdict, c("same", "failed", "failed"))
  expect_identical(x$ai_name[1], "CHAT-BOT")
  expect_true(is.na(x$ai_confidence[1]))
  expect_true(all(.ai_parse("no json here", 1:3)$ai_verdict == "failed"))
  expect_true(all(.ai_parse("[{broken", 1:3)$ai_verdict == "failed"))
})

# The five example pairs judged by a scripted model, written to a sheet.
# Pairs: 1 BEHAVIOR CHANGE, 2 DATA ANALYSIS, 3 MEDIATION, 4 RNN, 5 SOCIAL NETWORK.
mixed_judge <- function(id) {
  switch(id,
    list(id = id, verdict = "same", name = "BEHAVIOR CHANGE", confidence = 0.95, reason = "spelling"),
    list(id = id, verdict = "different", name = NULL, confidence = 0.9, reason = "different words"),
    list(id = id, verdict = "same", name = "MEDIATION", confidence = 0.5, reason = "low"),
    list(id = id, verdict = "unsure", name = NULL, confidence = 0.6, reason = "?"),
    list(id = id, verdict = "same", name = "SOCIAL NETWORK", confidence = 0.99, reason = "stem"))
}

judged_sheet <- function(file, judge = mixed_judge) {
  httr2::local_mocked_responses(fake_server(judge))
  merges <- asked(spelled(review_papers()), model = "m", key = "k")$value
  expect_message(review_sheet(merges, file), "pair\\(s\\) to check")
  merges
}

edit_sheet <- function(file, decisions) {
  sheets <- lapply(c(review = "review", ai = "ai"), \(s) as.data.frame(readxl::read_excel(file, sheet = s)))
  at <- match(as.integer(names(decisions)), sheets$review$pair)
  sheets$review$decision_2[at] <- unname(decisions)
  writexl::write_xlsx(sheets, file)
}

test_that("review_sheet fills decisions from the AI, rows to check first", {
  skip_if_not_installed("writexl")
  skip_if_not_installed("readxl")
  file <- tempfile(fileext = ".xlsx")
  merges <- judged_sheet(file)
  sheet <- as.data.frame(readxl::read_excel(file, sheet = "review"))
  expect_identical(names(sheet)[1:5], c("decision", "decision_2", "name", "keyword", "variant"))
  expect_true(all(is.na(sheet$decision_2)))
  # Pairs 1, 5 accepted and 2 rejected by the AI; 3 and 4 to check. One of
  # the three pre-filled pairs is drawn as the blind audit.
  expect_identical(sum(sheet$audit), 1L)
  expect_true(all(sheet$decision[seq_len(3)] == "check"))
  expect_setequal(sheet$pair[sheet$decision == "check" & !sheet$audit], c(3, 4))
  expect_true(all(is.na(sheet$ai_verdict[sheet$audit])))
  expect_identical(readxl::excel_sheets(file), c("review", "ai"))
  expect_error(review_sheet(merges, file), class = "saqrmisc_bad_input")
  expect_error(review_sheet(merges, tempfile(fileext = ".txt")), class = "saqrmisc_bad_input")
  # The audit draw is fixed, so a second sheet hides the same pair.
  second <- tempfile(fileext = ".xlsx")
  expect_message(review_sheet(merges, second))
  expect_identical(as.data.frame(readxl::read_excel(second))$pair, sheet$pair)
})

test_that("review_report gives the AI's agreement on the blind pairs", {
  skip_if_not_installed("writexl")
  skip_if_not_installed("readxl")
  file <- tempfile(fileext = ".xlsx")
  judged_sheet(file, \(id) list(id = id, verdict = "same", name = NULL, confidence = 0.9, reason = "r"))
  sheets <- lapply(c(review = "review", ai = "ai"), \(s) as.data.frame(readxl::read_excel(file, sheet = s)))
  # Make every pair blind, then decide: agree on three, reject two.
  sheets$review$audit <- TRUE
  writexl::write_xlsx(sheets, file)
  edit_sheet(file, c(`1` = "accept", `2` = "Reject", `3` = "accept", `4` = "reject", `5` = "accept"))
  report <- review_report(file)
  expect_s3_class(report, "saqr_review")
  s <- summary(report)
  blind <- s[s$pairs == "blind", ]
  expect_identical(c(blind$total, blind$ai_agreed, blind$ai_disagreed), c(5L, 3L, 2L))
  expect_equal(blind$agreement, 0.6)
  # Wilson 95% interval for 3 of 5, by hand: 0.2307 to 0.8824.
  expect_equal(c(blind$ci_low, blind$ci_high), c(0.231, 0.882), tolerance = 1e-3)
  expect_named(as.data.frame(report), c("pair", "keyword", "variant", "decision", "decision_2", "final",
                                         "name", "ai_verdict", "blind"))
  expect_output(print(report), "agreed with you on 3 of the 5")
})

test_that("the Wilson interval matches prop.test without continuity correction", {
  expect_equal(.kr_wilson(30, 40), as.numeric(stats::prop.test(30, 40, correct = FALSE)$conf.int),
               tolerance = 1e-12)
  expect_equal(.kr_wilson(0, 10)[1], 0)
  expect_true(all(is.na(.kr_wilson(0, 0))))
})

test_that("keywords_clean applies the accepted pairs only and warns about open ones", {
  skip_if_not_installed("writexl")
  skip_if_not_installed("readxl")
  file <- tempfile(fileext = ".xlsx")
  judged_sheet(file)
  # Accept 1 and 5, reject 2 and 3, leave 4 (RNN) open.
  edit_sheet(file, c(`1` = "accept", `2` = "reject", `3` = "reject", `4` = "check", `5` = "accept"))
  expect_warning(cleaned <- keywords_clean(review_papers(), replace = file),
                 class = "saqrmisc_review_pending")
  de <- as.data.frame(cleaned)$DE
  expect_identical(de[2], "BEHAVIOR CHANGE; SOCIAL NETWORK; WEB 3.0")
  expect_match(de[4], "RNN; ANALYSIS OF DATA", fixed = TRUE)
  expect_identical(de[6], "MEDITATION")
  expect_setequal(as.data.frame(review_report(file), what = "replace")$OLD,
                  c("BEHAVIOUR CHANGE", "SOCIAL NETWORKING"))
})

test_that("decided pairs are skipped next time, and bad sheets are refused", {
  skip_if_not_installed("writexl")
  skip_if_not_installed("readxl")
  file <- tempfile(fileext = ".xlsx")
  judged_sheet(file)
  edit_sheet(file, c(`1` = "accept", `2` = "reject", `3` = "check", `4` = "check", `5` = "accept"))
  later <- as.data.frame(spelled(review_papers(), skip = file))
  expect_setequal(paste(later$keyword, later$variant),
                  c("MEDIATION MEDITATION", "RECURRENT NEURAL NETWORK RNN"))

  edit_sheet(file, c(`3` = "yes"))
  expect_error(review_report(file), class = "saqrmisc_bad_input")
  expect_error(review_report(tempfile(fileext = ".xlsx")), class = "saqrmisc_bad_input")
})

test_that("ask_ai fills decision: accept or reject only when the AI is sure", {
  skip_if_not_installed("httr2")
  httr2::local_mocked_responses(fake_server(mixed_judge))
  x <- as.data.frame(asked(spelled(review_papers()), model = "m", key = "k")$value)
  # 1 same 0.95, 2 different 0.9, 3 same 0.5, 4 unsure 0.6, 5 same 0.99.
  expect_identical(x$decision, c("accept", "reject", "check", "check", "accept"))
  expect_identical(.kr_ai_decision(c("same", "same", "different", "failed", NA), c(0.8, 0.79, 0.8, 0.9, NA)),
                   c("accept", "check", "reject", "check", "check"))
})

test_that("keywords_clean applies an ask_ai result as the AI decided", {
  skip_if_not_installed("httr2")
  httr2::local_mocked_responses(fake_server(mixed_judge))
  merges <- asked(spelled(review_papers()), model = "m", key = "k")$value
  expect_warning(cleaned <- keywords_clean(review_papers(), replace = merges),
                 class = "saqrmisc_review_pending")
  de <- as.data.frame(cleaned)$DE
  # Pairs 1 and 5 accepted; 3 (MEDIATION, not sure) and 4 (RNN) left alone.
  expect_identical(de[2], "BEHAVIOR CHANGE; SOCIAL NETWORK; WEB 3.0")
  expect_identical(de[6], "MEDITATION")
  expect_match(de[4], "^RNN")
})

test_that("decision_2 overrules decision, and an empty decision_2 keeps the AI's", {
  skip_if_not_installed("writexl")
  skip_if_not_installed("readxl")
  file <- tempfile(fileext = ".xlsx")
  judged_sheet(file)
  # The AI accepted 1 and 5 and rejected 2. Overrule 5, decide 4, leave the rest.
  sheets <- lapply(c(review = "review", ai = "ai"), \(s) as.data.frame(readxl::read_excel(file, sheet = s)))
  sheets$review$audit <- FALSE
  sheets$review$decision <- sheets$ai$ai_decision[match(sheets$review$pair, sheets$ai$pair)]
  writexl::write_xlsx(sheets, file)
  edit_sheet(file, c(`3` = "reject", `4` = "accept", `5` = "reject"))
  x <- as.data.frame(review_report(file))
  expect_identical(x$final, c("accept", "reject", "reject", "accept", "reject"))
  expect_identical(x$decision_2, c(NA, NA, "reject", "accept", "reject"))
  expect_output(print(review_report(file)), "overruling the AI on 1")
  expect_setequal(as.data.frame(review_report(file), what = "replace")$OLD, c("BEHAVIOUR CHANGE", "RNN"))
  expect_no_warning(keywords_clean(review_papers(), replace = file))

  edit_sheet(file, c(`2` = "maybe"))
  expect_error(review_report(file), class = "saqrmisc_bad_input")
})

test_that("a gemini model goes to Google's OpenAI-compatible endpoint", {
  skip_if_not_installed("httr2")
  old <- Sys.getenv(c("GEMINI_API_KEY", "GOOGLE_API_KEY"), unset = NA)
  on.exit(lapply(names(old), \(k) if (is.na(old[[k]])) Sys.unsetenv(k) else do.call(Sys.setenv, as.list(old[k]))),
          add = TRUE)
  Sys.setenv(GEMINI_API_KEY = "", GOOGLE_API_KEY = "")
  expect_error(ask_ai(spelled(review_papers()), model = "gemini-3-flash"), class = "saqrmisc_bad_input")
  log <- new.env()
  httr2::local_mocked_responses(fake_server(all_same, log))
  Sys.setenv(GOOGLE_API_KEY = "google-key")
  run <- asked(spelled(review_papers()), model = "gemini-3-flash")
  req <- log$requests[[1]]
  expect_identical(req$url, "https://generativelanguage.googleapis.com/v1beta/openai/chat/completions")
  expect_identical(httr2::req_get_headers(req, "reveal")$Authorization, "Bearer google-key")
  expect_true(all(as.data.frame(run$value)$decision == "accept"))
})

test_that("a .csv review sheet round-trips, also when saved back with ;", {
  skip_if_not_installed("httr2")
  dir <- tempfile("review")
  dir.create(dir)
  file <- file.path(dir, "review.csv")
  merges <- judged_sheet(file)
  expect_setequal(list.files(dir), c("review.csv", "review_ai.csv"))
  expect_identical(readBin(file, "raw", 3L), as.raw(c(0xef, 0xbb, 0xbf)))
  expect_error(review_sheet(merges, file), class = "saqrmisc_bad_input")
  unlink(file)
  # review_ai.csv alone still blocks a new sheet, since it holds the blind answers.
  expect_error(review_sheet(merges, file), class = "saqrmisc_bad_input")
  unlink(file.path(dir, "review_ai.csv"))
  expect_message(review_sheet(merges, file))

  # Another program fills decision_2 and saves with ; as the separator.
  sheet <- utils::read.csv(file, fileEncoding = "UTF-8-BOM", stringsAsFactors = FALSE)
  sheet$decision_2 <- ifelse(sheet$pair == 5L, "reject", ifelse(sheet$audit, "accept", NA))
  utils::write.table(sheet, file, sep = ";", row.names = FALSE, na = "", fileEncoding = "UTF-8")
  x <- as.data.frame(review_report(file))
  expect_identical(x$final[x$pair == 5L], "reject")
  expect_false(anyNA(x$ai_verdict))
  expect_false("SOCIAL NETWORKING" %in% as.data.frame(review_report(file), what = "replace")$OLD)
  de <- suppressWarnings(as.data.frame(keywords_clean(review_papers(), replace = file))$DE)
  expect_identical(de[2], "BEHAVIOR CHANGE; SOCIAL NETWORKING; WEB 3.0")
})

test_that("a local server gets 5 pairs a request, a cloud one 20", {
  skip_if_not_installed("httr2")
  many <- data.frame(DE = paste0("CONCEPT", sprintf("%02d", 1:12), "ABC; CONCEPT", sprintf("%02d", 1:12), "ABD"))
  log <- new.env()
  httr2::local_mocked_responses(fake_server(all_same, log))
  asked(spelled(many), model = "m", url = "http://localhost:11434/v1")
  expect_length(log$requests, 3L)
  log2 <- new.env()
  httr2::local_mocked_responses(fake_server(all_same, log2))
  asked(spelled(many), model = "m", url = "https://example.org/v1", key = "k")
  expect_length(log2$requests, 1L)
})

test_that("head() keeps the first pairs and the class", {
  merges <- spelled(review_papers())
  kept <- head(merges, 1)
  expect_s3_class(kept, "saqr_proposals")
  expect_identical(as.data.frame(kept), utils::head(as.data.frame(merges), 1))
  expect_identical(nrow(as.data.frame(head(merges, 0))), 0L)
  expect_error(head(merges, -1), class = "saqrmisc_bad_input")
})

test_that("a sheet with no pairs is written, with every column", {
  none <- head(spelled(review_papers()), 0)
  file <- tempfile(fileext = ".csv")
  expect_message(review_sheet(none, file), "0 pair")
  expect_identical(nrow(.kr_read_csv(file)), 0L)
})
