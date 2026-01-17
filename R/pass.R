# Saqrmisc Package: AI Interpretation Functions
#
# Functions for passing R output to AI for interpretation.

#' Pass R Output to AI for Interpretation
#'
#' @description
#' Pipes any R output (test results, model summaries, tables) to an AI model
#' for scientific interpretation. Perfect for getting publication-ready
#' interpretations of statistical results.
#'
#' @param x Any R object to interpret (test result, model, data frame, etc.)
#' @param prompt Custom prompt to use. If NULL, builds from action and style.
#' @param action What to do with the output:
#'   \itemize{
#'     \item `"interpret"` (default): Interpret the statistical results
#'     \item `"explain"`: Explain what the analysis does and means
#'     \item `"write"`: Write publication-ready text (methods/results)
#'     \item `"summarize"`: Brief summary of key findings
#'     \item `"critique"`: Critical evaluation with limitations
#'     \item `"suggest"`: Suggest follow-up analyses
#'   }
#' @param style Interpretation style:
#'   \itemize{
#'     \item `"scientific"` (default): APA-style for academic papers
#'     \item `"simple"`: Plain language, no jargon
#'     \item `"detailed"`: Comprehensive with all caveats
#'     \item `"brief"`: Just the key takeaway
#'   }
#' @param output Output format:
#'   \itemize{
#'     \item `"text"` (default): Plain text
#'     \item `"markdown"` or `"md"`: Markdown formatted
#'     \item `"latex"`: LaTeX formatted for papers
#'     \item `"html"`: HTML formatted
#'   }
#' @param provider AI provider: `"anthropic"` (default), `"openai"`, or `"gemini"`
#' @param model Model to use. Defaults: `"claude-sonnet-4-20250514"` (Anthropic),
#'   `"gpt-4o"` (OpenAI), `"gemini-2.5-flash"` (Gemini).
#' @param api_key API key. If NULL, checks environment variables
#'   (`ANTHROPIC_API_KEY`, `OPENAI_API_KEY`, or `GEMINI_API_KEY`), then prompts interactively.
#' @param context Optional context about your study (e.g., "This is a study on
#'   student learning outcomes with N=500 participants")
#' @param copy Logical. Copy result to clipboard? Default: FALSE
#' @param quiet Logical. Suppress messages? Default: FALSE
#'
#' @return Character string with the AI interpretation (invisibly).
#'   Also prints the interpretation.
#'
#' @details
#' On first use, you'll be prompted to enter your API key. The key is stored
#' in your R environment for the session. To persist it, add to your .Renviron:
#' ```
#' ANTHROPIC_API_KEY=your-key-here
#' # or
#' OPENAI_API_KEY=your-key-here
#' ```
#'
#' @examples
#' \dontrun{
#' # Basic usage - pipe test results
#' t.test(mpg ~ am, data = mtcars) |> pass()
#'
#' # With context
#' cor.test(mtcars$mpg, mtcars$hp) |>
#'   pass(context = "Studying fuel efficiency in 1974 automobiles")
#'
#' # Different actions
#' lm(mpg ~ wt + hp, data = mtcars) |> summary() |> pass(action = "write")
#' chisq.test(mtcars$cyl, mtcars$am) |> pass(action = "explain", style = "simple")
#'
#' # Get LaTeX output for paper
#' aov(mpg ~ factor(cyl), data = mtcars) |> summary() |>
#'   pass(action = "write", output = "latex")
#'
#' # Critique an analysis
#' lm(mpg ~ ., data = mtcars) |> summary() |> pass(action = "critique")
#'
#' # Custom prompt overrides action
#' my_results |> pass(prompt = "Explain this to a non-technical audience")
#' }
#'
#' @export
pass <- function(x,
                 prompt = NULL,
                 action = c("interpret", "explain", "write", "summarize", "critique", "suggest"),
                 style = c("scientific", "simple", "detailed", "brief"),
                 output = c("text", "markdown", "md", "latex", "html"),
                 provider = c("anthropic", "openai", "gemini"),
                 model = NULL,
                 api_key = NULL,
                 context = NULL,
                 copy = FALSE,
                 quiet = FALSE) {

  action <- match.arg(action)
  style <- match.arg(style)
  output <- match.arg(output)
  if (output == "md") output <- "markdown"  # Alias
  provider <- match.arg(provider)

  # Capture the R output as text
  output_text <- paste(utils::capture.output(print(x)), collapse = "\n")

  # Also try to get the class/type info
  obj_class <- paste(class(x), collapse = ", ")
  obj_info <- paste0("Object class: ", obj_class)

  # Get API key
  api_key <- get_api_key(provider, api_key, quiet)

  # Set default model
  if (is.null(model)) {
    model <- switch(provider,
      anthropic = "claude-sonnet-4-20250514",
      openai = "gpt-4o",
      gemini = "gemini-2.5-flash"
    )
  }

  # Build the prompt
  system_prompt <- build_system_prompt(action, style, output)
  user_prompt <- build_user_prompt(output_text, obj_info, prompt, context, action)

  if (!quiet) {
    message("Sending to ", provider, " (", model, ")...")
  }

  # Call the API
  response <- call_ai_api(
    provider = provider,
    model = model,
    api_key = api_key,
    system_prompt = system_prompt,
    user_prompt = user_prompt
  )

  # Print the response
  cat("\n", strrep("=", 60), "\n", sep = "")
  cat("AI ", tools::toTitleCase(action), " (", style, " style, ", output, " format)\n", sep = "")
  cat(strrep("=", 60), "\n\n", sep = "")
  cat(response, "\n")
  cat("\n", strrep("=", 60), "\n", sep = "")

  # Copy to clipboard if requested
  if (copy) {
    if (requireNamespace("clipr", quietly = TRUE)) {
      clipr::write_clip(response)
      if (!quiet) message("Copied to clipboard!")
    } else {
      warning("Install 'clipr' package to enable clipboard support")
    }
  }

  invisible(response)
}

#' Get or prompt for API key
#' @noRd
get_api_key <- function(provider, api_key = NULL, quiet = FALSE) {
  if (!is.null(api_key)) {
    return(api_key)
  }

  # Check environment variable
  env_var <- switch(provider,
    anthropic = "ANTHROPIC_API_KEY",
    openai = "OPENAI_API_KEY",
    gemini = "GEMINI_API_KEY"
  )
  key <- Sys.getenv(env_var)

  if (nzchar(key)) {
    return(key)
  }

  # Check package options
  opt_name <- paste0("saqrmisc.", tolower(provider), "_key")
  key <- getOption(opt_name)

  if (!is.null(key) && nzchar(key)) {
    return(key)
  }

  # Interactive prompt
  if (!interactive()) {
    stop("API key required. Set ", env_var, " environment variable or pass api_key parameter.")
  }

  if (!quiet) {
    message("\n", strrep("=", 50))
    message("API Key Setup")
    message(strrep("=", 50))
    message("No API key found for ", provider)
    message("\nTo avoid this prompt, add to your .Renviron file:")
    message("  ", env_var, "=your-key-here")
    message("\nOr set in R: Sys.setenv(", env_var, "='your-key')")
    message(strrep("=", 50), "\n")
  }

  key <- readline(prompt = paste0("Enter your ", provider, " API key: "))

  if (!nzchar(key)) {
    stop("API key is required")
  }

  # Store for session
  options(setNames(list(key), opt_name))

  return(key)
}

#' Build system prompt based on action, style, and output
#' @noRd
build_system_prompt <- function(action, style, output) {
  base <- "You are an expert statistician helping researchers interpret their R output. "

  # Action-specific instructions
  action_prompts <- list(
    interpret = "Interpret the statistical results, explaining what they mean scientifically.",
    explain = "Explain what this analysis does and what the output means. Help the reader understand the methodology.",
    write = "Write publication-ready text suitable for a research paper's Methods and Results sections.",
    summarize = "Provide a brief summary of the key findings. Focus on the most important takeaways.",
    critique = "Provide a critical evaluation of the analysis, including potential limitations, assumptions that may be violated, and alternative approaches.",
    suggest = "Based on these results, suggest appropriate follow-up analyses or next steps for the research."
  )

  # Style-specific instructions
  style_prompts <- list(
    scientific = "Use APA style. Be precise about effect sizes, confidence intervals, and statistical significance. Mention practical significance, not just statistical significance.",
    simple = "Use plain language that anyone can understand. Avoid jargon. Use analogies if helpful. Focus on what the results MEAN, not the numbers.",
    detailed = "Be comprehensive. Include: what the test does, key assumptions, interpretation of all statistics, effect sizes, limitations, and caveats.",
    brief = "Be concise. Just the key takeaway in 2-3 sentences maximum."
  )

  # Output format instructions
  output_prompts <- list(
    text = "Format your response as plain text.",
    markdown = "Format your response using Markdown (headers, bold, lists, etc.).",
    latex = "Format your response using LaTeX. Use proper statistical notation (e.g., $p < .05$, $\\beta$, etc.). Format tables with tabular environment if needed.",
    html = "Format your response using HTML tags for structure and emphasis."
  )

  paste0(
    base,
    action_prompts[[action]], " ",
    style_prompts[[style]], " ",
    output_prompts[[output]]
  )
}

#' Build user prompt
#' @noRd
build_user_prompt <- function(output_text, obj_info, custom_prompt, context, action) {
  parts <- c()

  if (!is.null(context)) {
    parts <- c(parts, paste0("Study context: ", context, "\n"))
  }

  parts <- c(parts, paste0(obj_info, "\n"))
  parts <- c(parts, paste0("R Output:\n```\n", output_text, "\n```\n"))

  if (!is.null(custom_prompt)) {
    parts <- c(parts, paste0("\nSpecific request: ", custom_prompt))
  } else {
    # Default request based on action
    action_requests <- list(
      interpret = "Please interpret these results.",
      explain = "Please explain what this analysis does and what the results mean.",
      write = "Please write publication-ready Methods and Results text for these findings.",
      summarize = "Please summarize the key findings.",
      critique = "Please provide a critical evaluation of this analysis.",
      suggest = "Please suggest follow-up analyses based on these results."
    )
    parts <- c(parts, paste0("\n", action_requests[[action]]))
  }

  paste(parts, collapse = "\n")
}

#' Call AI API
#' @noRd
call_ai_api <- function(provider, model, api_key, system_prompt, user_prompt) {
  switch(provider,
    anthropic = call_anthropic(model, api_key, system_prompt, user_prompt),
    openai = call_openai(model, api_key, system_prompt, user_prompt),
    gemini = call_gemini(model, api_key, system_prompt, user_prompt)
  )
}

#' Call Anthropic API
#' @noRd
call_anthropic <- function(model, api_key, system_prompt, user_prompt) {
  if (!requireNamespace("httr2", quietly = TRUE)) {
    stop("Package 'httr2' is required. Install with: install.packages('httr2')")
  }

  body <- list(
    model = model,
    max_tokens = 2048,
    system = system_prompt,
    messages = list(
      list(role = "user", content = user_prompt)
    )
  )

  req <- httr2::request("https://api.anthropic.com/v1/messages") |>
    httr2::req_headers(
      `x-api-key` = api_key,
      `anthropic-version` = "2023-06-01",
      `content-type` = "application/json"
    ) |>
    httr2::req_body_json(body) |>
    httr2::req_error(is_error = function(resp) FALSE)

  resp <- httr2::req_perform(req)

  if (httr2::resp_status(resp) != 200) {
    error_body <- httr2::resp_body_json(resp)
    stop("API error: ", error_body$error$message %||% httr2::resp_status_desc(resp))
  }

  result <- httr2::resp_body_json(resp)
  result$content[[1]]$text
}

#' Call OpenAI API
#' @noRd
call_openai <- function(model, api_key, system_prompt, user_prompt) {
  if (!requireNamespace("httr2", quietly = TRUE)) {
    stop("Package 'httr2' is required. Install with: install.packages('httr2')")
  }

  body <- list(
    model = model,
    messages = list(
      list(role = "system", content = system_prompt),
      list(role = "user", content = user_prompt)
    ),
    max_tokens = 2048
  )

  req <- httr2::request("https://api.openai.com/v1/chat/completions") |>
    httr2::req_headers(
      Authorization = paste("Bearer", api_key),
      `Content-Type` = "application/json"
    ) |>
    httr2::req_body_json(body) |>
    httr2::req_error(is_error = function(resp) FALSE)

  resp <- httr2::req_perform(req)

  if (httr2::resp_status(resp) != 200) {
    error_body <- httr2::resp_body_json(resp)
    stop("API error: ", error_body$error$message %||% httr2::resp_status_desc(resp))
  }

  result <- httr2::resp_body_json(resp)
  result$choices[[1]]$message$content
}

#' Call Gemini API
#' @noRd
call_gemini <- function(model, api_key, system_prompt, user_prompt) {
  if (!requireNamespace("httr2", quietly = TRUE)) {
    stop("Package 'httr2' is required. Install with: install.packages('httr2')")
  }

  # Gemini API endpoint
  url <- paste0(
    "https://generativelanguage.googleapis.com/v1beta/models/",
    model,
    ":generateContent?key=",
    api_key
  )

  body <- list(
    system_instruction = list(
      parts = list(list(text = system_prompt))
    ),
    contents = list(
      list(
        parts = list(list(text = user_prompt))
      )
    ),
    generationConfig = list(
      maxOutputTokens = 2048
    )
  )

  req <- httr2::request(url) |>
    httr2::req_headers(`Content-Type` = "application/json") |>
    httr2::req_body_json(body) |>
    httr2::req_error(is_error = function(resp) FALSE)

  resp <- httr2::req_perform(req)

  if (httr2::resp_status(resp) != 200) {
    error_body <- httr2::resp_body_json(resp)
    error_msg <- error_body$error$message %||% httr2::resp_status_desc(resp)
    stop("API error: ", error_msg)
  }

  result <- httr2::resp_body_json(resp)
  result$candidates[[1]]$content$parts[[1]]$text
}

#' Set API Key for Session
#'
#' @description
#' Convenience function to set your API key for the current R session.
#'
#' @param key Your API key
#' @param provider Provider name: "anthropic", "openai", or "gemini"
#'
#' @examples
#' \dontrun{
#' set_api_key("sk-ant-...", "anthropic")
#' set_api_key("sk-...", "openai")
#' set_api_key("AIza...", "gemini")
#' }
#'
#' @export
set_api_key <- function(key, provider = c("anthropic", "openai", "gemini")) {
  provider <- match.arg(provider)
  env_var <- switch(provider,
    anthropic = "ANTHROPIC_API_KEY",
    openai = "OPENAI_API_KEY",
    gemini = "GEMINI_API_KEY"
  )
  do.call(Sys.setenv, setNames(list(key), env_var))
  message("API key set for ", provider, " (this session only)")
  invisible(TRUE)
}
