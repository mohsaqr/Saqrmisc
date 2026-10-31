# Keyword merges in four steps: suggest_merges() finds pairs of keywords that
# may name one concept, ask_ai() has a language model judge each pair and
# fills `decision`, review_sheet() writes them to Excel where a person may
# overrule it in `decision_2`, and keywords_clean(replace = ) applies the
# accepted pairs: `decision_2` where given, else `decision`.
# review_report() says what was decided and how often the AI was right.

#' Suggest keyword merges
#'
#' @description
#' Finds pairs of keywords that may name the same concept, so that they can
#' be judged by [ask_ai()] and by you in [review_sheet()]. Nothing is merged
#' here. Use it on data that [keywords_clean()] has already cleaned.
#'
#' A pair is suggested when the two keywords differ by
#' \itemize{
#'   \item **spelling**: one or two letters (`BEHAVIOR CHANGE` /
#'     `BEHAVIOUR CHANGE`), never in numbers (`WEB 2.0` / `WEB 3.0`) or in
#'     short words (`AI` / `AR`);
#'   \item **word order** (`DATA ANALYSIS` / `ANALYSIS OF DATA`);
#'   \item **word ending** (`SOCIAL NETWORK` / `SOCIAL NETWORKING`);
#'   \item **acronym**: one is the other's initials (`RNN` /
#'     `RECURRENT NEURAL NETWORK`), at most five long forms per acronym;
#'   \item **meaning**: the two mean nearly the same (`HUMAN-COMPUTER
#'     INTERFACE` / `HUMAN-MACHINE INTERFACE`), by a `cosine` of at least
#'     0.9 between their Sentence-BERT embeddings (all-MiniLM-L6-v2, through
#'     the sbert package). A narrower term (`CHATBOT` / `SMART CHATBOT`) is
#'     not suggested this way.
#' }
#' Look-alikes such as `MEDIATION` / `MEDITATION` are suggested on purpose:
#' telling them apart is the reviewer's job. Every pair gets a `cosine`, how
#' close the two are in meaning, and the pairs closest in meaning come first:
#' look-alikes with different meanings (`ONTOLOGY` / `ONCOLOGY`, 0.26) sink to
#' the bottom. The embeddings cannot read acronyms, so acronym pairs score low
#' whether right or wrong; and they barely see word order, so `TEXT TO SPEECH`
#' / `SPEECH TO TEXT` scores 0.97. The cosine orders the pairs; [ask_ai()] and
#' you decide them.
#'
#' @param data Your papers, one row each: usually
#'   `as.data.frame(keywords_clean(...))`, or the [keywords_clean()] result.
#'   A `TI` column, if present, supplies example titles. A
#'   [institutions_clean()] result reviews institution names instead (column
#'   `AU_UN`): two institutions OpenAlex identified are never paired, and the
#'   OpenAlex name is the suggested one.
#' @param columns The keyword columns. Default `"DE"`, or `"AU_UN"` for a
#'   [institutions_clean()] result.
#' @param skip Review sheets from earlier rounds: pairs decided there are not
#'   suggested again.
#' @param meaning Compare meanings with Sentence-BERT (`TRUE`, the default):
#'   adds the `meaning` pairs and the `cosine` that orders all pairs. Needs
#'   the sbert package and its model. With `FALSE`, `cosine` is empty and the
#'   pairs touching most papers come first.
#'
#' @return A `saqr_proposals` object. Print it for an overview.
#'   `as.data.frame(x)` gives one row per pair, the closest in meaning first:
#'   `pair` (its number), `keyword` (the suggested name), `variant`,
#'   `decision` (`check` until [ask_ai()] fills it), `found_by`,
#'   `keyword_papers`, `variant_papers`, `cosine` (-1 to 1), and the AI's
#'   `ai_verdict`, `ai_name`, `ai_confidence`, `ai_reason` (empty until
#'   [ask_ai()]). `as.data.frame(x, what = "evidence")` gives, per `pair`:
#'   `column`, `together` (papers listing both: a hint that they differ),
#'   `shared_keywords` (share of co-keywords in common), `similarity` (of the
#'   spelling, 0 to 1), `example_keyword`, `example_variant` (titles) and
#'   `ai_model`. `summary()` counts the pairs per `found_by`.
#'
#' @section Errors:
#' Raises `saqrmisc_bad_input` for a missing or non-character column, or a
#' `meaning` that is not `TRUE` or `FALSE`; `saqrmisc_missing_package` when
#' `meaning = TRUE` and sbert is not installed.
#'
#' @references
#' Reimers, N., & Gurevych, I. (2019). Sentence-BERT: Sentence embeddings
#' using Siamese BERT-networks. *EMNLP-IJCNLP 2019*, 3982-3992.
#' \doi{10.18653/v1/D19-1410}
#'
#' @examples
#' papers <- data.frame(
#'   TI = c("Coaching for behavior change", "Behaviour change at work"),
#'   DE = c("BEHAVIOR CHANGE; SOCIAL NETWORK", "BEHAVIOUR CHANGE; SOCIAL NETWORKING")
#' )
#' merges <- suggest_merges(papers, meaning = requireNamespace("sbert", quietly = TRUE))
#' merges
#' as.data.frame(merges)
#'
#' @seealso [ask_ai()], [review_sheet()], [review_report()], [keywords_clean()]
#' @export
suggest_merges <- function(data, columns = "DE", skip = NULL, meaning = TRUE) {
  kind <- "keywords"
  known <- character()
  if (inherits(data, "saqr_institutions")) {
    kind <- "institutions"
    institutions <- as.data.frame(data, what = "institutions")
    known <- institutions$institution[!is.na(institutions$openalex_id)]
    if (missing(columns)) columns <- "AU_UN"
  }
  if (inherits(data, c("saqr_keywords", "saqr_institutions"))) data <- as.data.frame(data)
  .kw_check_columns(data, columns)
  if (!isTRUE(meaning) && !isFALSE(meaning)) .bad_input("`meaning` must be TRUE or FALSE")
  if (meaning && !.kr_has_sbert()) {
    stop(errorCondition(paste0("meaning = TRUE needs the sbert package: ",
                               "remotes::install_github(\"sonsoleslp/sbert\"); or use meaning = FALSE"),
                        class = "saqrmisc_missing_package", call = NULL))
  }
  titles <- if ("TI" %in% names(data) && is.character(data$TI)) data$TI else NULL
  skipped <- .kr_decided_pairs(skip)
  pairs <- .kw_bind(lapply(columns, \(f) .kr_column_pairs(data[[f]], f, titles, skipped, meaning, known)))
  pairs$pair <- seq_len(nrow(pairs))
  # Whole columns, so that a result without pairs keeps its zero rows.
  pairs[c("ai_verdict", "ai_name", "ai_reason", "ai_model")] <- rep(list(rep(NA_character_, nrow(pairs))), 4L)
  pairs$ai_confidence <- rep(NA_real_, nrow(pairs))
  pairs$decision <- rep("check", nrow(pairs))
  structure(list(pairs = pairs, columns = columns, kind = kind, known = known), class = "saqr_proposals")
}

#' Tables of suggested keyword merges
#'
#' @param x A [suggest_merges()] or [ask_ai()] result.
#' @param row.names,optional Ignored; present for the generic.
#' @param what `"pairs"` (default) or `"evidence"`; see [suggest_merges()].
#' @param ... Ignored.
#' @return A data frame with one row per pair.
#' @export
as.data.frame.saqr_proposals <- function(x, row.names = NULL, optional = FALSE,
                                         what = c("pairs", "evidence"), ...) {
  what <- match.arg(what)
  columns <- switch(what,
    pairs = c("pair", "keyword", "variant", "decision", "found_by", "keyword_papers", "variant_papers",
              "cosine", "ai_verdict", "ai_name", "ai_confidence", "ai_reason"),
    evidence = c("pair", "column", "together", "shared_keywords", "similarity",
                 "example_keyword", "example_variant", "ai_model"))
  out <- x$pairs[columns]
  rownames(out) <- NULL
  out
}

#' Count suggested keyword merges
#'
#' @param object A [suggest_merges()] or [ask_ai()] result.
#' @param ... Ignored.
#' @return A data frame with one row per `found_by`: `pairs`, and how many
#'   the AI judged `same`, `different`, `unsure`, `failed`, or has not been
#'   asked about (`not_asked`).
#' @export
summary.saqr_proposals <- function(object, ...) {
  pairs <- object$pairs
  verdict <- ifelse(is.na(pairs$ai_verdict), "not_asked", pairs$ai_verdict)
  kinds <- sort(unique(pairs$found_by))
  out <- .kw_bind(lapply(kinds, \(k) {
    v <- verdict[pairs$found_by == k]
    counts <- vapply(c("same", "different", "unsure", "failed", "not_asked"), \(s) sum(v == s), integer(1))
    data.frame(found_by = k, pairs = length(v), t(counts))
  }))
  out %||% data.frame(found_by = character(), pairs = integer(), same = integer(),
                      different = integer(), unsure = integer(), failed = integer(),
                      not_asked = integer())
}

#' The first suggested merges
#'
#' Keeps the first `n` pairs of a [suggest_merges()] or [ask_ai()] result, in
#' its order: the closest in meaning (`cosine`) first. Use it to try a model
#' on a few pairs before sending them all.
#'
#' @param x A [suggest_merges()] or [ask_ai()] result.
#' @param n How many pairs to keep. Default 20.
#' @param ... Ignored.
#' @return A result of the same class with at most `n` pairs.
#' @examples
#' papers <- data.frame(DE = c("BEHAVIOR CHANGE; SOCIAL NETWORK; E-LEARNING",
#'                             "BEHAVIOUR CHANGE; SOCIAL NETWORKS; ELEARNING"))
#' head(suggest_merges(papers, meaning = FALSE), 2)
#' @importFrom utils head
#' @export
head.saqr_proposals <- function(x, n = 20L, ...) {
  if (!is.numeric(n) || length(n) != 1L || is.na(n) || n < 0) .bad_input("`n` must be one number, 0 or more")
  x$pairs <- utils::head(x$pairs, n)
  x
}

#' Print suggested keyword merges
#'
#' @param x A [suggest_merges()] or [ask_ai()] result.
#' @param n How many pairs to show. Default 10.
#' @param ... Ignored.
#' @return `x`, invisibly.
#' @export
print.saqr_proposals <- function(x, n = 10, ...) {
  pairs <- x$pairs
  kinds <- vapply(.kr_found_by, \(k) sum(grepl(k, pairs$found_by, fixed = TRUE)), integer(1))
  kinds <- kinds[kinds > 0L]
  cat(sprintf("%d possible merges in %s%s\n", nrow(pairs), paste(x$columns, collapse = ", "),
              if (length(kinds) > 0L) sprintf(" (%s)", paste(sprintf("%s %d", .kr_found_by[names(kinds)], kinds),
                                                            collapse = ", ")) else ""))
  v <- pairs$ai_verdict
  open <- sum(is.na(v) | v %in% "failed")
  if (all(is.na(v))) {
    cat("AI: not asked yet\n")
  } else {
    cat(sprintf("AI: %d same, %d different, %d unsure%s\n", sum(v %in% "same"), sum(v %in% "different"),
                sum(v %in% "unsure"), if (open > 0L) sprintf(", %d not answered", open) else ""))
    d <- pairs$decision
    cat(sprintf("Decision: %d accept, %d reject, %d check\n",
                sum(d == "accept"), sum(d == "reject"), sum(d == "check")))
  }
  if (nrow(pairs) > 0L) {
    shown <- utils::head(pairs, n)
    table <- data.frame(pair = shown$pair, keyword = shown$keyword, variant = shown$variant,
                        found_by = shown$found_by)
    if (!all(is.na(shown$cosine))) table$cosine <- format(shown$cosine, nsmall = 2L)
    if (!all(is.na(v))) table$decision <- shown$decision
    # found_by is clipped first; keyword and variant share what the console
    # width leaves.
    table$found_by <- .kr_clip(table$found_by, 22L)
    other <- setdiff(names(table), c("keyword", "variant"))
    taken <- sum(vapply(other, \(k) max(nchar(c(k, as.character(table[[k]])))) + 1L, integer(1)))
    # One space before each column, plus the leading one.
    width <- max(12L, (getOption("width", 80L) - taken - 3L) %/% 2L - 1L)
    table$keyword <- .kr_clip(table$keyword, width)
    table$variant <- .kr_clip(table$variant, width)
    cat("\n")
    print(table, row.names = FALSE, right = FALSE)
    if (nrow(pairs) > n) cat(sprintf("... %d more: as.data.frame(x)\n", nrow(pairs) - n))
  }
  invisible(x)
}

#' Ask an AI model to judge suggested keyword merges
#'
#' @description
#' Sends the pairs from [suggest_merges()], with example titles, to a
#' language model, which says for each pair whether the two keywords are the
#' `same` concept, `different` ones, or it is `unsure`, with a name, how sure
#' it is (0 to 1) and why.
#'
#' Works with any OpenAI-compatible server (OpenAI, Google Gemini,
#' OpenRouter, Groq, Mistral, a local Ollama or LM Studio) and with
#' Anthropic. Without a `url`, a `model` starting with `claude` goes to
#' Anthropic, one starting with `gemini` to Google's Gemini API, anything
#' else to OpenAI. Gemini Nano runs only inside Chrome and Android, so R
#' cannot reach it; Gemma 3n, Google's open model built like it, runs
#' locally through Ollama: `model = "gemma3n", url = "http://localhost:11434/v1"`.
#'
#' Pairs go 20 to a request, or 5 to a local server (`localhost`): small
#' local models lose track in long lists and answer every pair alike.
#'
#' Unanswered pairs are asked again each time you call `ask_ai()` on its
#' result, so an interrupted or failed run simply continues. In an
#' interactive session it says how many requests it will send and asks
#' first.
#'
#' @param x A [suggest_merges()] result, or an earlier `ask_ai()` result.
#' @param model The model's name, e.g. `"gpt-5-mini"`, `"claude-haiku-5-5"`,
#'   `"gemini-3-flash"`, `"gemma3n"`.
#' @param key Your API key. Default: `OPENAI_API_KEY`; `ANTHROPIC_API_KEY`
#'   for Anthropic; `GEMINI_API_KEY` (or `GOOGLE_API_KEY`) for Gemini. Leave
#'   empty for a local server. It is never saved.
#' @param url The server, for anything other than OpenAI or Anthropic, e.g.
#'   `"https://openrouter.ai/api/v1"` or `"http://localhost:11434/v1"`.
#' @param instructions Anything the model should know, in plain words, e.g.
#'   `"The papers are on chatbots in education. Prefer AI to ARTIFICIAL
#'   INTELLIGENCE."` Without it the model works out the field from the
#'   titles.
#' @param again Ask again about pairs the AI already judged, e.g.
#'   `again = "unsure"` with a stronger `model`.
#'
#' @return `x` with `ai_verdict`, `ai_name`, `ai_confidence` and `ai_reason`
#'   filled in, and `decision`: `accept` when the AI said `same` and was at
#'   least 80\% sure, `reject` when it said `different` and was at least 80\%
#'   sure, `check` otherwise. `keywords_clean(replace = x)` applies the
#'   accepted pairs as they are; [review_sheet()] lets you overrule them. A
#'   pair whose answer could not be read is `failed` and is asked again next
#'   time.
#'
#' @section Errors:
#' Raises `saqrmisc_bad_input` for a missing `model` or key;
#' `saqrmisc_missing_package` without httr2 or jsonlite; `saqrmisc_ai_failed`
#' when the first request fails (wrong key, model or server). A later
#' failure, or stopping the run, warns `saqrmisc_ai_failed` and keeps the
#' answers so far.
#'
#' @examples
#' \dontrun{
#' merges <- suggest_merges(cleaned)
#' merges <- ask_ai(merges, model = "gpt-5-mini")
#' merges <- ask_ai(merges, model = "claude-haiku-5-5",
#'                  instructions = "The papers are on chatbots in education.")
#' merges <- ask_ai(merges, model = "gemini-3-flash")
#' merges <- ask_ai(merges, model = "gemma3n", url = "http://localhost:11434/v1")
#' merges <- ask_ai(merges, model = "claude-sonnet-5-5", again = "unsure")
#' merges
#' }
#' @seealso [suggest_merges()], [review_sheet()]
#' @export
ask_ai <- function(x, model, key = NULL, url = NULL, instructions = NULL, again = NULL) {
  if (!inherits(x, "saqr_proposals")) .bad_input("`x` must come from suggest_merges()")
  if (missing(model) || !is.character(model) || length(model) != 1L || !nzchar(model)) {
    .bad_input("give the model's name, e.g. model = \"gpt-5-mini\"")
  }
  if (!is.null(key) && (!is.character(key) || length(key) != 1L)) .bad_input("`key` must be one string")
  if (!is.null(url) && (!is.character(url) || length(url) != 1L || !grepl("^https?://", url))) {
    .bad_input("`url` must be a web address starting with http:// or https://")
  }
  if (!is.null(instructions) && (!is.character(instructions) || length(instructions) != 1L)) {
    .bad_input("`instructions` must be one piece of text")
  }
  if (!is.null(again) && !(is.character(again) && all(again %in% c("same", "different", "unsure")))) {
    .bad_input("`again` must be \"same\", \"different\" and/or \"unsure\"")
  }
  .kr_need("httr2")
  .kr_need("jsonlite")

  provider <- if (grepl("anthropic\\.com", url %||% "") || (is.null(url) && grepl("^claude", model))) "anthropic"
    else if (grepl("generativelanguage\\.googleapis\\.com", url %||% "") || (is.null(url) && grepl("^gemini", model))) "gemini"
    else "openai"
  # Gemini is reached through Google's OpenAI-compatible endpoint.
  api <- if (identical(provider, "anthropic")) "anthropic" else "openai"
  if (identical(provider, "gemini")) url <- url %||% .ai_gemini_url
  url <- .ai_endpoint(url, api)
  key_names <- switch(provider, anthropic = "ANTHROPIC_API_KEY", gemini = c("GEMINI_API_KEY", "GOOGLE_API_KEY"),
                      openai = "OPENAI_API_KEY")
  key <- key %||% c(Filter(nzchar, Sys.getenv(key_names)), "")[[1L]]
  if (!nzchar(key)) key <- getOption(paste0("saqrmisc.", provider, "_key"), "")
  if (!nzchar(key) && grepl("api\\.(openai|anthropic)\\.com|generativelanguage\\.googleapis\\.com", url)) {
    .bad_input(sprintf("no API key: give key = \"...\" or set %s", key_names[[1L]]))
  }

  pairs <- x$pairs
  todo <- which(is.na(pairs$ai_verdict) | pairs$ai_verdict %in% c("failed", again))
  if (length(todo) == 0L) {
    message("The AI has judged every pair. Use `again = \"unsure\"` to ask about some again.")
    return(x)
  }
  local <- grepl("^https?://(localhost|127\\.0\\.0\\.1|\\[::1\\])([:/]|$)", url)
  batch_size <- if (local) .kr_batch_size_local else .kr_batch_size
  batches <- split(todo, ceiling(seq_along(todo) / batch_size))
  system_prompt <- .ai_system_prompt(instructions, x$kind %||% "keywords")
  tokens <- ceiling((nchar(system_prompt) * length(batches) +
                       sum(vapply(batches, \(b) nchar(.ai_user_prompt(pairs[b, ])), numeric(1)))) / 4)
  message(sprintf("Asking %s about %d pair(s): %d request(s) to %s, about %s tokens.",
                  model, length(todo), length(batches), .ai_host(url), format(tokens, big.mark = ",")))
  if (interactive() && !isTRUE(utils::askYesNo("Send?"))) {
    message("Nothing was sent.")
    return(x)
  }

  ask <- \(rows) .ai_parse(.ai_ask(system_prompt, .ai_user_prompt(pairs[rows, ]), model, url, key,
                                    api, length(rows)), pairs$pair[rows])
  # Answers are kept in an environment as they arrive, so a failed request or
  # an interrupt still returns everything answered before it.
  state <- new.env()
  state$answers <- list()
  state$error <- NULL
  run_batch <- \(i) {
    if (!is.null(state$error)) return(invisible(NULL))
    message(sprintf("  request %d of %d", i, length(batches)))
    rows <- batches[[i]]
    tryCatch({
      answer <- ask(rows)
      unread <- rows[answer$ai_verdict == "failed"]
      if (length(unread) > 0L) answer <- rbind(answer[answer$ai_verdict != "failed", ], ask(unread))
      state$answers[[length(state$answers) + 1L]] <- answer
    }, httr2_error = \(e) state$error <- e)
    invisible(NULL)
  }
  interrupted <- tryCatch({
    lapply(seq_along(batches), run_batch)
    FALSE
  }, interrupt = \(e) TRUE)

  answers <- .kw_bind(state$answers)
  if (is.null(answers) && !is.null(state$error)) {
    stop(errorCondition(sprintf("the AI request to %s failed: %s", .ai_host(url),
                                conditionMessage(state$error)),
                        class = "saqrmisc_ai_failed", call = NULL))
  }
  if (!is.null(answers)) {
    at <- match(answers$pair, pairs$pair)
    pairs[at, c("ai_verdict", "ai_name", "ai_confidence", "ai_reason")] <-
      answers[c("ai_verdict", "ai_name", "ai_confidence", "ai_reason")]
    pairs$ai_model[at] <- model
  }
  # An identified institution keeps its OpenAlex name, whatever the model
  # proposes; suggest_merges() put that name in `keyword`.
  identified <- pairs$keyword %in% (x$known %||% character()) & pairs$ai_verdict %in% "same"
  pairs$ai_name[identified] <- pairs$keyword[identified]
  pairs$decision <- .kr_ai_decision(pairs$ai_verdict, pairs$ai_confidence)
  x$pairs <- pairs

  open <- sum(is.na(pairs$ai_verdict[todo]))
  if (interrupted || !is.null(state$error)) {
    why <- if (interrupted) "stopped" else sprintf("a request failed (%s)", conditionMessage(state$error))
    warning(warningCondition(
      sprintf("%s; %d pair(s) not judged yet. Run ask_ai() on this result to continue.", why, open),
      class = "saqrmisc_ai_failed", call = NULL))
  }
  failed <- sum(pairs$ai_verdict[todo] %in% "failed")
  if (failed > 0L) {
    warning(warningCondition(
      sprintf("the AI's answer for %d pair(s) could not be read; run ask_ai() again to retry them", failed),
      class = "saqrmisc_ai_failed", call = NULL))
  }
  x
}

#' Write a review sheet
#'
#' @description
#' Writes what needs a human look to a file you can check anywhere and pass
#' back later. For an [authors_clean()] result, see
#' [review_sheet.saqr_authors()]; the rest of this page is about keyword and
#' institution merges.
#'
#' Writes the pairs to an Excel file for you to check. `decision` is the
#' AI's (see [ask_ai()]); `decision_2` is yours and empty. Where you fill in
#' `decision_2` (`accept`, `reject` or `check`) it overrules `decision`;
#' where you leave it empty, the AI's `decision` stands. Correct `name` (the
#' name both keywords will get) where needed, then pass the file to
#' `keywords_clean(replace = )`.
#'
#' A `.csv` file is for working on the pairs elsewhere (another program,
#' Python, a spreadsheet): the AI's answers, including the ones hidden in the
#' blind rows, go to a second file beside it, `<name>_ai.csv`. Keep it there
#' for [review_report()]. Both are UTF-8; a sheet saved back with `;` as the
#' separator, as Excel does in many countries, is read as well.
#'
#' Pairs to `check` come first, then `reject`, then `accept`; within each,
#' the closest in meaning (`cosine`). One in ten accepted or rejected pairs,
#' drawn at random, is shown blind: its `decision` is `check` and the AI's
#' answer is hidden. Your `decision_2` on those tells [review_report()] how
#' often the AI is right.
#'
#' @param x An [ask_ai()] result (a [suggest_merges()] result also works;
#'   then every pair is `check`), or an [authors_clean()] result.
#' @param file The file to write: `.xlsx` (two sheets, `review` and `ai`) or
#'   `.csv` (two files). An existing file is never replaced, since it may hold
#'   your decisions.
#'
#' @return The file path, invisibly.
#'
#' @section Errors:
#' Raises `saqrmisc_bad_input` when `file` is not `.xlsx` or `.csv`, or it
#' (or its `_ai.csv`) already exists; `saqrmisc_missing_package` for `.xlsx`
#' without writexl.
#'
#' @examples
#' if (requireNamespace("writexl", quietly = TRUE) &&
#'     requireNamespace("readxl", quietly = TRUE)) {
#'   papers <- data.frame(DE = c("BEHAVIOR CHANGE; SOCIAL NETWORK",
#'                               "BEHAVIOUR CHANGE; SOCIAL NETWORKING"))
#'   file <- tempfile(fileext = ".xlsx")
#'   review_sheet(suggest_merges(papers, meaning = FALSE), file)
#'   review_report(file)
#' }
#' @seealso [ask_ai()], [review_report()]
#' @export
review_sheet <- function(x, file) UseMethod("review_sheet")

#' @export
review_sheet.default <- function(x, file) {
  .bad_input("`x` must come from suggest_merges(), ask_ai() or authors_clean()")
}

#' @export
review_sheet.saqr_proposals <- function(x, file) {
  csv <- .kr_sheet_file(file, ai_file = TRUE)

  pairs <- x$pairs
  ai_decision <- pairs$decision
  prefilled <- which(ai_decision != "check")
  n_audit <- if (length(prefilled) > 0L) max(1L, round(.kr_audit_share * length(prefilled))) else 0L
  audited <- .kr_with_seed(1L, prefilled[sample.int(length(prefilled), n_audit)])
  is_audit <- seq_len(nrow(pairs)) %in% audited

  decision <- ifelse(is_audit, "check", ai_decision)
  ai_name <- ifelse(pairs$ai_verdict %in% "same", pairs$ai_name, NA_character_)
  hide <- \(v) ifelse(is_audit, NA, v)
  empty <- rep(NA_character_, nrow(pairs))
  review <- data.frame(
    decision = decision, decision_2 = empty,
    name = ifelse(!is_audit & !is.na(ai_name), ai_name, pairs$keyword),
    keyword = pairs$keyword, variant = pairs$variant,
    keyword_papers = pairs$keyword_papers, variant_papers = pairs$variant_papers,
    cosine = pairs$cosine, ai_verdict = hide(pairs$ai_verdict), ai_confidence = hide(pairs$ai_confidence),
    ai_reason = hide(pairs$ai_reason), audit = is_audit, note = empty,
    found_by = pairs$found_by, together = pairs$together,
    shared_keywords = pairs$shared_keywords, example_keyword = pairs$example_keyword,
    example_variant = pairs$example_variant, column = pairs$column, pair = pairs$pair,
    stringsAsFactors = FALSE
  )
  review <- review[order(match(review$decision, c("check", "reject", "accept")), review$pair), ]
  ai <- data.frame(pair = pairs$pair, ai_decision = ai_decision, ai_verdict = pairs$ai_verdict,
                   ai_name = pairs$ai_name, ai_confidence = pairs$ai_confidence,
                   ai_reason = pairs$ai_reason, ai_model = pairs$ai_model,
                   prompt = rep(.ai_prompt_version, nrow(pairs)), stringsAsFactors = FALSE)
  if (csv) {
    .kr_write_csv(review, file)
    .kr_write_csv(ai, .kr_ai_file(file))
  } else {
    writexl::write_xlsx(list(review = review, ai = ai), file)
  }
  message(sprintf("%s: %d pair(s) to check (%d of them blind), %d accepted and %d rejected by the AI. Write yours in decision_2.",
                  basename(file), sum(decision == "check"), n_audit,
                  sum(decision == "accept"), sum(decision == "reject")))
  invisible(file)
}

#' Report on a checked keyword review sheet
#'
#' @description
#' Reads a sheet from [review_sheet()] after you have checked it: how many
#' pairs were accepted, rejected or are still to check, and how often the AI
#' agreed with you. Each pair's `final` decision is your `decision_2` where
#' you gave one, else the AI's `decision`.
#'
#' @param file The review sheet: the `.xlsx` or `.csv` file from
#'   [review_sheet()]. For `.csv`, its `_ai.csv` beside it supplies the AI's
#'   answers.
#'
#' @return A `saqr_review` object. Print it for the report.
#'   `summary()` gives a data frame with two rows, `pairs` = `"blind"` (the
#'   audit pairs: an unbiased measure of the AI) and `"all"`: `total`, and
#'   the `final` decisions `accept`, `reject`, `check`; then, over the pairs
#'   where you gave a `decision_2` and the AI said `same` or `different`,
#'   `ai_agreed`, `ai_disagreed`, `agreement` and its 95\% Wilson interval
#'   `ci_low`, `ci_high`.
#'   `as.data.frame(x)` gives one row per pair: `pair`, `keyword`, `variant`,
#'   `decision` (the AI's), `decision_2` (yours), `final`, `name`,
#'   `ai_verdict`, `blind`; `what = "replace"` gives the `OLD`/`NEW` table
#'   that [keywords_clean()] applies.
#'
#' @section Errors:
#' Raises `saqrmisc_bad_input` for a missing file, a `.xlsx` without a
#' `review` sheet, a
#' `decision` or `decision_2` other than `accept`, `reject` or `check`, or an
#' accepted pair without a `name`; `saqrmisc_missing_package` without readxl.
#'
#' @references
#' Wilson, E. B. (1927). Probable inference, the law of succession, and
#' statistical inference. *Journal of the American Statistical Association*,
#' 22(158), 209-212.
#'
#' @examples
#' if (requireNamespace("writexl", quietly = TRUE) &&
#'     requireNamespace("readxl", quietly = TRUE)) {
#'   papers <- data.frame(DE = c("BEHAVIOR CHANGE; SOCIAL NETWORK",
#'                               "BEHAVIOUR CHANGE; SOCIAL NETWORKING"))
#'   file <- tempfile(fileext = ".xlsx")
#'   review_sheet(suggest_merges(papers, meaning = FALSE), file)
#'   report <- review_report(file)
#'   report
#'   summary(report)
#'   as.data.frame(report)
#' }
#' @seealso [review_sheet()], [keywords_clean()]
#' @export
review_report <- function(file) {
  if (!is.character(file) || length(file) != 1L || !file.exists(file)) {
    .bad_input("`file` must be an existing review sheet (.xlsx)")
  }
  if (grepl("\\.csv$", file, ignore.case = TRUE)) {
    review <- .kr_read_csv(file)
    ai <- if (file.exists(.kr_ai_file(file))) .kr_read_csv(.kr_ai_file(file))
  } else {
    .kr_need("readxl")
    sheets <- readxl::excel_sheets(file)
    if (!"review" %in% sheets) .bad_input(sprintf("%s has no sheet called \"review\"", basename(file)))
    review <- as.data.frame(readxl::read_excel(file, sheet = "review"))
    ai <- if ("ai" %in% sheets) as.data.frame(readxl::read_excel(file, sheet = "ai"))
  }
  review <- .kr_check_review(review, basename(file))
  if (!is.null(ai)) {
    review <- merge(review[setdiff(names(review), setdiff(names(ai), "pair"))], ai, by = "pair", all.x = TRUE)
  }
  review <- review[order(review$pair), ]
  rownames(review) <- NULL
  structure(list(review = review, file = file), class = "saqr_review")
}

#' Tables from a keyword review report
#'
#' @param x A [review_report()] result.
#' @param row.names,optional Ignored; present for the generic.
#' @param what `"decisions"` (default) or `"replace"`; see [review_report()].
#' @param ... Ignored.
#' @return A data frame.
#' @export
as.data.frame.saqr_review <- function(x, row.names = NULL, optional = FALSE,
                                      what = c("decisions", "replace"), ...) {
  what <- match.arg(what)
  review <- x$review
  if (identical(what, "replace")) return(.kr_replace_table(review))
  data.frame(pair = review$pair, keyword = review$keyword, variant = review$variant,
             decision = review$decision, decision_2 = review$decision_2, final = review$final,
             name = review$name,
             ai_verdict = review$ai_verdict %||% rep(NA_character_, nrow(review)),
             blind = review$audit, stringsAsFactors = FALSE)
}

#' Count the decisions in a keyword review
#'
#' @param object A [review_report()] result.
#' @param ... Ignored.
#' @return A data frame; see [review_report()].
#' @export
summary.saqr_review <- function(object, ...) {
  review <- object$review
  ai <- review$ai_verdict %||% rep(NA_character_, nrow(review))
  one_row <- \(label, rows) {
    agree <- .kr_agree(review$decision_2[rows], ai[rows])
    k <- sum(agree %in% TRUE)
    n <- sum(!is.na(agree))
    ci <- .kr_wilson(k, n)
    data.frame(pairs = label, total = sum(rows),
               accept = sum(review$final[rows] == "accept"),
               reject = sum(review$final[rows] == "reject"),
               check = sum(review$final[rows] == "check"),
               ai_agreed = k, ai_disagreed = n - k,
               agreement = if (n > 0L) round(k / n, 3) else NA_real_,
               ci_low = round(ci[1L], 3), ci_high = round(ci[2L], 3))
  }
  .kw_bind(list(one_row("blind", review$audit %in% TRUE), one_row("all", rep(TRUE, nrow(review)))))
}

#' Print a keyword review report
#'
#' @param x A [review_report()] result.
#' @param ... Ignored.
#' @return `x`, invisibly.
#' @export
print.saqr_review <- function(x, ...) {
  s <- summary(x)
  all_pairs <- s[s$pairs == "all", ]
  blind <- s[s$pairs == "blind", ]
  cat(sprintf("%s: %d pairs, %d accepted, %d rejected, %d still to check\n", basename(x$file),
              all_pairs$total, all_pairs$accept, all_pairs$reject, all_pairs$check))
  review <- x$review
  yours <- !is.na(review$decision_2)
  overruled <- yours & review$decision %in% c("accept", "reject") & review$decision_2 != review$decision
  cat(sprintf("You decided %d pair(s) in decision_2, overruling the AI on %d\n", sum(yours), sum(overruled)))
  decided <- blind$ai_agreed + blind$ai_disagreed
  if (decided > 0L) {
    cat(sprintf("The AI agreed with you on %d of the %d pairs you decided blind (%.0f%%, 95%% CI %.0f-%.0f%%)\n",
                blind$ai_agreed, decided, 100 * blind$agreement, 100 * blind$ci_low, 100 * blind$ci_high))
  } else if (blind$total > 0L) {
    cat(sprintf("%d blind pair(s) not decided yet\n", blind$total))
  }
  invisible(x)
}

# ---- helpers ----------------------------------------------------------------

.ai_prompt_version <- "merge-3"
.kr_batch_size <- 20L
.kr_batch_size_local <- 5L
.ai_gemini_url <- "https://generativelanguage.googleapis.com/v1beta/openai"
.kr_sure <- 0.8
.kr_audit_share <- 0.1
.kr_max_typos <- 2L
.kr_found_by <- c(typo = "spelling", order = "word order", stem = "word ending", acronym = "acronym",
                  meaning = "meaning")
.kr_meaning_cut <- 0.9
.kr_meaning_block <- 1000L

.kr_need <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop(errorCondition(sprintf("this needs the %s package: install.packages(\"%s\")", pkg, pkg),
                        class = "saqrmisc_missing_package", call = NULL))
  }
}

.kr_clip <- function(x, width = 40L) ifelse(nchar(x) > width, paste0(substr(x, 1L, width - 3L), "..."), x)

# Runs `expr` with a fixed seed and puts the caller's random state back.
.kr_with_seed <- function(seed, expr) {
  had_seed <- exists(".Random.seed", envir = globalenv(), inherits = FALSE)
  old_seed <- if (had_seed) get(".Random.seed", envir = globalenv(), inherits = FALSE)
  on.exit(if (had_seed) assign(".Random.seed", old_seed, envir = globalenv())
          else rm(".Random.seed", envir = globalenv()), add = TRUE)
  set.seed(seed)
  expr
}

.kr_pair_key <- function(a, b) paste(pmin(a, b), pmax(a, b), sep = "\r")

# Upper-case words with apostrophes removed and each word made singular.
.kr_words <- function(terms) {
  lapply(strsplit(gsub("['`\u2019]", "", toupper(terms)), "[^[:alnum:]]+"), \(w) .kw_singular(w[nzchar(w)]))
}

.kr_suffix <- "(ATIONAL|IZATION|ISATION|ATION|ITION|NESS|MENT|ABLE|IBLE|ICAL|IVE|ITY|ION|ING|IAL|AL|ED|IC|ER|LY)$"

# Strips one common ending when at least four letters remain.
.kr_stem <- function(w) {
  stem <- sub(.kr_suffix, "", w)
  ifelse(nchar(stem) >= 4L, stem, w)
}

.kr_no_pairs <- function() data.frame(a = integer(), b = integer(), method = character())

# Pairs every member of each group with the group's member in most papers.
.kr_star_pairs <- function(groups, papers, terms, method) {
  groups <- groups[lengths(groups) > 1L]
  if (length(groups) == 0L) return(.kr_no_pairs())
  .kw_bind(lapply(groups, \(g) {
    g <- g[order(-papers[g], nchar(terms[g]), terms[g])]
    data.frame(a = g[1L], b = g[-1L], method = method)
  }))
}

.kr_typo_pairs <- function(terms) {
  key <- .kw_key(terms)
  len <- nchar(key)
  digits <- gsub("[^0-9]", "", key)
  eligible <- which(len >= 5L & !startsWith(key, "\r"))
  # Comparing every pair is quadratic; two blockings (same first letter, same
  # last letter) find any pair whose typos are not at both ends.
  in_block <- \(idx) {
    if (length(idx) < 2L) return(NULL)
    d <- utils::adist(key[idx])
    hit <- which(upper.tri(d) & d >= 1 & d <= .kr_max_typos, arr.ind = TRUE)
    data.frame(a = idx[hit[, 1L]], b = idx[hit[, 2L]], d = d[hit])
  }
  blocks <- c(split(eligible, substr(key[eligible], 1L, 1L)),
              split(eligible, substring(key[eligible], len[eligible])))
  found <- .kw_bind(lapply(blocks, in_block))
  if (is.null(found)) return(.kr_no_pairs())
  found <- unique(found)
  # One edit needs 5 letters, two need 10; numbers must match.
  keep <- pmin(len[found$a], len[found$b]) >= 5L * found$d & digits[found$a] == digits[found$b]
  data.frame(a = found$a[keep], b = found$b[keep], method = rep("typo", sum(keep)))
}

.kr_order_pairs <- function(terms, papers) {
  bag <- vapply(.kr_words(terms), \(w) paste(sort(setdiff(w, .kw_stopwords)), collapse = " "), character(1))
  keep <- nzchar(bag)
  .kr_star_pairs(split(seq_along(terms)[keep], bag[keep]), papers, terms, "order")
}

.kr_stem_pairs <- function(terms, papers) {
  stemmed <- vapply(.kr_words(terms), \(w) paste(.kr_stem(w), collapse = " "), character(1))
  keep <- nzchar(stemmed)
  .kr_star_pairs(split(seq_along(terms)[keep], stemmed[keep]), papers, terms, "stem")
}

# The AI's answers of a .csv review sheet live beside it: review.csv ->
# review_ai.csv.
.kr_ai_file <- function(file) sub("(\\.csv)$", "_ai\\1", file, ignore.case = TRUE)

# Checks a review sheet path: .xlsx or .csv, and never one that exists (it may
# hold decisions). TRUE for .csv. `ai_file`: a .csv sheet has an `_ai.csv` too.
.kr_sheet_file <- function(file, ai_file = FALSE) {
  if (missing(file) || !is.character(file) || length(file) != 1L || !grepl("\\.(xlsx|csv)$", file, ignore.case = TRUE)) {
    .bad_input("`file` must be a path ending in .xlsx or .csv")
  }
  csv <- grepl("\\.csv$", file, ignore.case = TRUE)
  taken <- Filter(file.exists, c(file, if (csv && ai_file) .kr_ai_file(file)))
  if (length(taken) > 0L) {
    .bad_input(sprintf("%s already %s and may hold your decisions; delete or choose another name",
                       paste(taken, collapse = " and "), if (length(taken) > 1L) "exist" else "exists"))
  }
  if (!csv) .kr_need("writexl")
  csv
}

# UTF-8 with a byte-order mark, so that Excel shows accents correctly.
.kr_write_csv <- function(table, file) {
  con <- file(file, open = "w", encoding = "UTF-8")
  on.exit(close(con), add = TRUE)
  writeLines("\ufeff", con, sep = "")
  utils::write.csv(table, con, row.names = FALSE, na = "")
}

# Reads a sheet saved with `,` or, as Excel does in many locales, `;`. The
# header decides: keyword columns never hold `;` inside a single keyword.
.kr_read_csv <- function(file) {
  header <- readLines(file, n = 1L, encoding = "UTF-8", warn = FALSE)
  sep <- if (lengths(regmatches(header, gregexpr(";", header))) >
             lengths(regmatches(header, gregexpr(",", header)))) ";" else ","
  utils::read.csv(file, sep = sep, check.names = FALSE, fileEncoding = "UTF-8-BOM",
                  stringsAsFactors = FALSE, na.strings = "")
}

# The AI's decision: accept or reject when it was sure, else check.
.kr_ai_decision <- function(verdict, confidence) {
  sure <- !is.na(confidence) & confidence >= .kr_sure
  ifelse(verdict %in% "same" & sure, "accept", ifelse(verdict %in% "different" & sure, "reject", "check"))
}

.kr_has_sbert <- function() requireNamespace("sbert", quietly = TRUE)

# Sentence-BERT embeddings, one unit-length row per term, so that a dot
# product is a cosine.
.kr_embed <- function(terms) sbert::encode(tolower(terms), normalize = TRUE)

.kr_cosine <- function(embedding, a, b) {
  round(rowSums(embedding[a, , drop = FALSE] * embedding[b, , drop = FALSE]), 2)
}

.kr_meaning_pairs <- function(terms, embedding) {
  # The full term-by-term matrix of a large corpus does not fit in memory, so
  # cosines are computed for a block of rows at a time.
  blocks <- split(seq_along(terms), ceiling(seq_along(terms) / .kr_meaning_block))
  found <- .kw_bind(lapply(blocks, \(rows) {
    cosine <- tcrossprod(embedding[rows, , drop = FALSE], embedding)
    hit <- which(cosine >= .kr_meaning_cut, arr.ind = TRUE)
    hit <- hit[rows[hit[, 1L]] < hit[, 2L], , drop = FALSE]
    data.frame(a = rows[hit[, 1L]], b = hit[, 2L])
  }))
  if (is.null(found) || nrow(found) == 0L) return(.kr_no_pairs())
  # A term whose words contain the other's (CHATBOT / SMART CHATBOT) is
  # narrower, not the same concept.
  words <- lapply(.kr_words(terms), \(w) unique(.kr_stem(setdiff(w, .kw_stopwords))))
  narrower <- mapply(\(i, j) {
    x <- words[[i]]
    y <- words[[j]]
    !setequal(x, y) && (all(x %in% y) || all(y %in% x))
  }, found$a, found$b)
  data.frame(a = found$a[!narrower], b = found$b[!narrower], method = rep("meaning", sum(!narrower)))
}

.kr_acronym_pairs <- function(terms, papers) {
  acronyms <- which(grepl("^[A-Z0-9]{2,6}$", terms) & grepl("[A-Z]", terms))
  phrases <- which(grepl("[ /-]", terms))
  if (length(acronyms) == 0L || length(phrases) == 0L) return(.kr_no_pairs())
  spelled <- lapply(terms[phrases], .kw_spellings)
  spellings <- data.frame(spelling = unlist(spelled), phrase = rep(phrases, lengths(spelled)))
  phrase_words <- strsplit(terms, "[^[:alnum:]]+")
  .kw_bind(lapply(acronyms, \(i) {
    long <- unique(spellings$phrase[spellings$spelling %in% .kw_acronym_letters(terms[i])])
    # A phrase that contains the acronym as a word (AI INTERVIEWER) is not
    # its long form.
    long <- long[!vapply(long, \(j) terms[i] %in% phrase_words[[j]], logical(1))]
    long <- long[vapply(long, \(j) .kw_is_acronym(terms[i], terms[j]), logical(1))]
    long <- utils::head(long[order(-papers[long], terms[long])], 5L)
    data.frame(a = long, b = rep(i, length(long)), method = rep("acronym", length(long)))
  })) %||% .kr_no_pairs()
}

# Pairs accepted or rejected in earlier review sheets, as unordered keys.
.kr_decided_pairs <- function(skip) {
  if (is.null(skip)) return(character())
  if (!is.character(skip)) .bad_input("`skip` must be paths to earlier review sheets")
  unlist(lapply(skip, \(f) {
    review <- review_report(f)$review
    decided <- review[review$final != "check", ]
    .kr_pair_key(decided$keyword, decided$variant)
  }))
}

.kr_example <- function(rows, titles) {
  if (is.null(titles)) return(NA_character_)
  shown <- titles[utils::head(rows, 2L)]
  shown <- shown[!is.na(shown) & nzchar(shown)]
  if (length(shown) == 0L) return(NA_character_)
  paste(.kr_clip(shown, 100L), collapse = " | ")
}

.kr_column_pairs <- function(values, column, titles, skipped, meaning, known = character()) {
  lists <- lapply(.kw_split(values), unique)
  keyword <- unlist(lists)
  paper <- rep(seq_along(lists), lengths(lists))
  terms <- unique(keyword)
  papers_of <- split(paper, factor(keyword, levels = terms))
  papers <- lengths(papers_of)

  embedding <- if (meaning && length(terms) >= 2L) .kr_embed(terms)
  found <- if (length(terms) < 2L) .kr_no_pairs() else
    rbind(.kr_typo_pairs(terms), .kr_order_pairs(terms, papers),
          .kr_stem_pairs(terms, papers), .kr_acronym_pairs(terms, papers),
          if (meaning) .kr_meaning_pairs(terms, embedding))
  found <- found[found$a != found$b, ]

  key <- .kr_pair_key(terms[found$a], terms[found$b])
  label <- tapply(found$method, key, \(m) paste(.kr_found_by[names(.kr_found_by) %in% m], collapse = " + "))
  first <- !duplicated(key)
  found <- found[first, ]
  found$method <- unname(label[key[first]])
  found <- found[!.kr_pair_key(terms[found$a], terms[found$b]) %in% skipped, ]
  # Two names an identifier already tells apart (OpenAlex institutions) are
  # never one.
  found <- found[!(terms[found$a] %in% known & terms[found$b] %in% known), ]

  # `keyword` is the suggested name: an identified name (OpenAlex), else the
  # long form of an acronym, else the form in more papers, then the shorter,
  # then the alphabetically first.
  a_known <- terms[found$a] %in% known
  b_known <- terms[found$b] %in% known
  swap <- (b_known & !a_known) | (a_known == b_known & !grepl("acronym", found$method, fixed = TRUE) &
    (papers[found$b] > papers[found$a] |
       (papers[found$b] == papers[found$a] &
          (nchar(terms[found$b]) < nchar(terms[found$a]) |
             (nchar(terms[found$b]) == nchar(terms[found$a]) & terms[found$b] < terms[found$a])))))
  a <- ifelse(swap, found$b, found$a)
  b <- ifelse(swap, found$a, found$b)

  if (length(a) == 0L) {
    return(data.frame(column = character(), keyword = character(), variant = character(),
                      found_by = character(), keyword_papers = integer(), variant_papers = integer(),
                      cosine = numeric(), together = integer(), shared_keywords = numeric(), similarity = numeric(),
                      example_keyword = character(), example_variant = character()))
  }
  neighbours <- vector("list", length(terms))
  used <- unique(c(a, b))
  neighbours[used] <- lapply(used, \(i) unique(unlist(lists[papers_of[[i]]])))
  shared <- mapply(\(i, j) {
    na <- setdiff(neighbours[[i]], terms[c(i, j)])
    nb <- setdiff(neighbours[[j]], terms[c(i, j)])
    both <- union(na, nb)
    if (length(both) == 0L) NA_real_ else round(length(intersect(na, nb)) / length(both), 2)
  }, a, b)

  out <- data.frame(
    column = column, keyword = terms[a], variant = terms[b], found_by = found$method,
    keyword_papers = papers[a], variant_papers = papers[b],
    cosine = if (meaning) .kr_cosine(embedding, a, b) else NA_real_,
    together = mapply(\(i, j) length(intersect(papers_of[[i]], papers_of[[j]])), a, b),
    shared_keywords = unname(shared),
    similarity = round(1 - mapply(\(s, t) utils::adist(s, t)[1L, 1L], terms[a], terms[b]) /
                         pmax(nchar(terms[a]), nchar(terms[b])), 2),
    example_keyword = vapply(a, \(i) .kr_example(papers_of[[i]], titles), character(1)),
    example_variant = vapply(b, \(i) .kr_example(papers_of[[i]], titles), character(1)),
    stringsAsFactors = FALSE
  )
  out <- out[order(-out$cosine, -(out$keyword_papers + out$variant_papers), out$keyword, out$variant), ]
  rownames(out) <- NULL
  out
}

.ai_endpoint <- function(url, api) {
  default <- if (identical(api, "anthropic")) "https://api.anthropic.com/v1/messages"
    else "https://api.openai.com/v1/chat/completions"
  url <- sub("/+$", "", url %||% default)
  # A bare server address, as pass() takes it, gets the usual /v1 prefix.
  if (grepl("^https?://[^/]+$", url)) url <- paste0(url, "/v1")
  path <- if (identical(api, "anthropic")) "/messages" else "/chat/completions"
  if (!endsWith(url, path)) url <- paste0(url, path)
  url
}

.ai_host <- function(url) sub("^(https?://[^/]+).*$", "\\1", url)

.ai_system_prompt <- function(instructions, kind = "keywords") {
  rules <- switch(kind,
    keywords = paste0(
      "You check proposed merges of author keywords from a set of research papers. ",
      "Work out the research field from the example titles. ",
      "Two keywords are SAME when they name one concept that a researcher counting topics would count once: ",
      "spelling or typing variants, British and American spelling, singular and plural, word order, ",
      "an abbreviation and its full form, hyphens or spaces. ",
      "They are DIFFERENT when one is broader, narrower, related, a part or an application of the other, ",
      "or another concept that only looks alike. ",
      "Say UNSURE when the evidence does not settle it. ",
      "For SAME pairs give the best name for both: the standard, correctly spelled form used in the field. "),
    institutions = paste0(
      "You check proposed merges of institution names (universities, companies, research centres) ",
      "taken from the author affiliations of a set of research papers. ",
      "Two names are SAME when they name one organisation: spelling or typing variants, letters lost ",
      "from accents (UNIVERSITT for UNIVERSITAT), translations (UNIVERSIDAD DE GRANADA / UNIVERSITY OF GRANADA), ",
      "an acronym and its full name, a former name, or one name with and one without its country code. ",
      "They are DIFFERENT when they are separate organisations: different universities with similar names, ",
      "different campuses of one university system, or a university and a separate institute or company. ",
      "Say UNSURE when the evidence does not settle it. ",
      "For SAME pairs give the best name for both: the organisation's official name, in capitals. "))
  paste0(
    rules,
    if (is.null(instructions)) "" else paste0("The researcher adds: ", instructions, " "),
    "Answer with a JSON array only, one object per pair: ",
    "{\"id\": <id>, \"verdict\": \"same\" | \"different\" | \"unsure\", \"name\": \"<name>\" or null, ",
    "\"confidence\": <0 to 1>, \"reason\": \"<at most 15 words>\"}."
  )
}

.ai_user_prompt <- function(rows) {
  example <- \(e) ifelse(is.na(e), "", paste0("; e.g. ", e))
  lines <- sprintf("id %d: A = \"%s\" (%d papers%s); B = \"%s\" (%d papers%s); both listed in %d papers",
                   rows$pair, rows$keyword, rows$keyword_papers, example(rows$example_keyword),
                   rows$variant, rows$variant_papers, example(rows$example_variant), rows$together)
  paste(c("Pairs:", lines), collapse = "\n")
}

# One chat request at temperature 0; returns the reply text. A model that
# refuses a temperature setting is asked again without one. Other httr2
# errors (HTTP status, connection) propagate to ask_ai(), which stops the run
# and keeps what was answered.
.ai_ask <- function(system_prompt, user_prompt, model, url, key, api, n_pairs) {
  req <- httr2::request(url) |>
    httr2::req_timeout(120) |>
    httr2::req_retry(max_tries = 3) |>
    httr2::req_error(body = .ai_error_body)
  if (identical(api, "anthropic")) {
    req <- httr2::req_headers(req, `x-api-key` = key, `anthropic-version` = "2023-06-01",
                              .redact = "x-api-key")
    body <- list(model = model, max_tokens = 300L + 120L * n_pairs, system = system_prompt,
                 messages = list(list(role = "user", content = user_prompt)), temperature = 0)
  } else {
    if (nzchar(key)) req <- httr2::req_auth_bearer_token(req, key)
    body <- list(model = model,
                 messages = list(list(role = "system", content = system_prompt),
                                 list(role = "user", content = user_prompt)),
                 temperature = 0)
  }
  send <- \(b) httr2::resp_body_json(httr2::req_perform(httr2::req_body_json(req, b)))
  reply <- tryCatch(send(body), httr2_http_400 = \(e) {
    if (!grepl("temperature", conditionMessage(e), ignore.case = TRUE)) stop(e)
    body$temperature <- NULL
    send(body)
  })
  text <- if (identical(api, "anthropic")) {
    blocks <- Filter(\(b) identical(b$type, "text"), reply$content)
    paste(vapply(blocks, \(b) b$text %||% "", character(1)), collapse = "\n")
  } else {
    reply$choices[[1L]]$message$content
  }
  text %||% ""
}

# The server's own error message, so a wrong model or key says so.
.ai_error_body <- function(resp) {
  if (!isTRUE(grepl("json", httr2::resp_content_type(resp), fixed = TRUE))) return(NULL)
  body <- httr2::resp_body_json(resp)
  body$error$message %||% body$message
}

# Reads the model's JSON answer; a pair without a readable answer is "failed".
.ai_parse <- function(text, pairs) {
  out <- data.frame(pair = pairs, ai_verdict = "failed", ai_name = NA_character_,
                    ai_confidence = NA_real_, ai_reason = "no readable answer",
                    stringsAsFactors = FALSE)
  json <- regmatches(text, regexpr("(?s)\\[.*\\]", text, perl = TRUE))
  if (length(json) == 0L) return(out)
  # An unreadable reply is an expected model failure: every pair of the batch
  # stays "failed", which ask_ai() reports and retries.
  parsed <- tryCatch(jsonlite::fromJSON(json, simplifyVector = TRUE), error = \(e) NULL)
  if (!is.data.frame(parsed) || !all(c("id", "verdict") %in% names(parsed))) return(out)
  verdict <- tolower(trimws(as.character(parsed$verdict)))
  id <- as.integer(.kr_as_number(parsed$id))
  ok <- verdict %in% c("same", "different", "unsure") & id %in% pairs & !duplicated(id)
  if (!any(ok)) return(out)
  at <- match(id[ok], pairs)
  field <- \(name) if (name %in% names(parsed)) parsed[[name]][ok] else rep(NA, sum(ok))
  name <- as.character(field("name"))
  usable <- verdict[ok] == "same" & !is.na(name) & nzchar(trimws(name))
  name[usable] <- .kw_brackets(.kw_normalise(name[usable]))$term
  name[!usable] <- NA_character_
  confidence <- .kr_as_number(field("confidence"))
  confidence[!is.na(confidence) & (confidence < 0 | confidence > 1)] <- NA_real_
  out$ai_verdict[at] <- verdict[ok]
  out$ai_name[at] <- name
  out$ai_confidence[at] <- confidence
  out$ai_reason[at] <- as.character(field("reason"))
  out
}

# A number from a model's JSON, which may arrive as a number or as text;
# anything else is NA.
.kr_as_number <- function(x) {
  if (is.numeric(x)) return(as.numeric(x))
  text <- trimws(as.character(x))
  out <- rep(NA_real_, length(text))
  ok <- !is.na(text) & grepl("^-?[0-9]+(\\.[0-9]+)?$", text)
  out[ok] <- as.numeric(text[ok])
  out
}

# Checks a review sheet and normalises decisions and names. `final` is the
# person's decision_2 where given, else the AI's decision.
.kr_check_review <- function(review, source) {
  needed <- c("pair", "decision", "name", "keyword", "variant")
  absent <- setdiff(needed, names(review))
  if (length(absent) > 0L) {
    .bad_input(sprintf("%s lacks the column(s) %s", source, paste(absent, collapse = ", ")))
  }
  read_decision <- \(values, column) {
    out <- tolower(trimws(as.character(values)))
    out[!is.na(out) & !nzchar(out)] <- NA_character_
    bad <- !is.na(out) & !out %in% c("accept", "reject", "check")
    if (any(bad)) {
      .bad_input(sprintf("%s: %s must be accept, reject or check (pair %s)",
                         source, column, paste(review$pair[bad], collapse = ", ")))
    }
    out
  }
  decision <- read_decision(review$decision, "decision")
  decision[is.na(decision)] <- "check"
  decision_2 <- read_decision(review$decision_2 %||% rep(NA_character_, nrow(review)), "decision_2")
  final <- ifelse(is.na(decision_2), decision, decision_2)
  name <- as.character(review$name)
  nameless <- final == "accept" & (is.na(name) | !nzchar(trimws(name)))
  if (any(nameless)) {
    .bad_input(sprintf("%s: accepted pairs need a name (pair %s)",
                       source, paste(review$pair[nameless], collapse = ", ")))
  }
  named <- !is.na(name) & nzchar(trimws(name))
  name[named] <- .kw_brackets(.kw_normalise(name[named]))$term
  name[!named] <- NA_character_
  review$decision <- decision
  review$decision_2 <- decision_2
  review$final <- final
  review$name <- name
  review$audit <- as.logical(review$audit %||% rep(FALSE, nrow(review)))
  review
}

.kr_replace_table <- function(review) {
  accepted <- review[review$final == "accept", ]
  table <- data.frame(OLD = c(accepted$keyword, accepted$variant),
                      NEW = rep(accepted$name, 2L), stringsAsFactors = FALSE)
  table <- table[table$OLD != table$NEW, ]
  rownames(table) <- NULL
  table
}

# Used by keywords_clean(replace = ): an ask_ai() result or a review sheet
# becomes its OLD/NEW table.
.kr_review_as_table <- function(table) {
  if (inherits(table, "saqr_proposals")) {
    pairs <- table$pairs
    named <- pairs$ai_verdict %in% "same" & !is.na(pairs$ai_name)
    review <- .kr_check_review(data.frame(pair = pairs$pair, decision = pairs$decision,
                                          name = ifelse(named, pairs$ai_name, pairs$keyword),
                                          keyword = pairs$keyword, variant = pairs$variant,
                                          stringsAsFactors = FALSE), "the ask_ai() result")
  } else if (inherits(table, "saqr_review")) {
    review <- table$review
  } else if (is.data.frame(table) && all(c("decision", "keyword", "variant", "name") %in% names(table))) {
    review <- .kr_check_review(table, "the review sheet")
  } else {
    return(table)
  }
  pending <- sum(review$final == "check")
  if (pending > 0L) {
    warning(warningCondition(
      sprintf("%d reviewed pair(s) are still marked \"check\" and were not applied", pending),
      class = "saqrmisc_review_pending", call = NULL))
  }
  .kr_replace_table(review)
}

# Does the person's decision_2 match the AI's verdict? NA when either is open.
.kr_agree <- function(decision, verdict) {
  ai <- ifelse(verdict %in% "same", "accept", ifelse(verdict %in% "different", "reject", NA))
  ifelse(decision %in% c("accept", "reject") & !is.na(ai), decision == ai, NA)
}

# Wilson (1927) score interval for k successes in n trials.
.kr_wilson <- function(k, n, level = 0.95) {
  if (n == 0L) return(c(NA_real_, NA_real_))
  z <- stats::qnorm(1 - (1 - level) / 2)
  p <- k / n
  centre <- (p + z^2 / (2 * n)) / (1 + z^2 / n)
  half <- z * sqrt(p * (1 - p) / n + z^2 / (4 * n^2)) / (1 + z^2 / n)
  c(centre - half, centre + half)
}
