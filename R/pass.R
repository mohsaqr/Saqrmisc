# Saqrmisc Package: AI Interpretation Functions
#
# Functions for passing R output to AI for interpretation.

# Null coalescing operator (if not from rlang)
`%||%` <- function(x, y) if (is.null(x)) y else x

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
#'     \item `"detailed"`: Comprehensive with assumptions, limitations, caveats
#'       (default for `action = "write"`)
#'     \item `"brief"`: Just the key takeaway
#'   }
#' @param output Output format:
#'   \itemize{
#'     \item `"text"` (default): Plain text
#'     \item `"markdown"` or `"md"`: Markdown formatted
#'     \item `"latex"`: LaTeX formatted for papers
#'     \item `"html"`: HTML formatted
#'   }
#' @param provider AI provider: `"openai"` (default), `"anthropic"`, `"gemini"`, or `"openrouter"`.
#' @param model Model to use. Defaults: `"gpt-4.1-nano"` (OpenAI), `"claude-sonnet-4-20250514"` (Anthropic),
#'   `"gemini-2.5-flash"` (Gemini), `"anthropic/claude-sonnet-4"` (OpenRouter).
#' @param base_url Custom API base URL for OpenAI-compatible servers (e.g., LM Studio,
#'   Ollama, vLLM). Example: `"http://127.0.0.1:1234"` for LM Studio. When set,
#'   uses OpenAI-compatible format regardless of provider setting.
#' @param api_key API key. If NULL, checks environment variables
#'   (`OPENAI_API_KEY`, `ANTHROPIC_API_KEY`, `GEMINI_API_KEY`, or `OPENROUTER_API_KEY`),
#'   then prompts interactively.
#'   For local servers like LM Studio, use `api_key = "none"` or any string.
#' @param context Optional context about your study (e.g., "This is a study on
#'   student learning outcomes with N=500 participants")
#' @param system_message Optional custom instructions for the AI (e.g., "Focus on
#'   clinical implications", "Be more concise", "Emphasize effect sizes")
#' @param auto_local Logical. Automatically detect and use local AI servers
#'   (LM Studio on port 1234, Ollama on port 11434)? Default: TRUE.
#'   Set to FALSE to force using cloud providers.
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
#' # Custom prompt (specific request to the AI)
#' my_results |> pass(prompt = "Focus only on the interaction effects")
#'
#' # Custom context (about your study)
#' t.test(score ~ group, data = mydata) |>
#'   pass(context = "RCT comparing drug vs placebo, N=200 patients with diabetes")
#'
#' # Custom system message (instructions for the AI)
#' my_results |> pass(system_message = "Focus on clinical implications and effect sizes")
#'
#' # Combine all customizations
#' lm(outcome ~ treatment * age, data = mydata) |> summary() |>
#'   pass(
#'     action = "write",
#'     context = "Phase 3 clinical trial for hypertension medication",
#'     system_message = "Emphasize clinical significance over statistical significance",
#'     prompt = "Pay special attention to the treatment-age interaction"
#'   )
#' }
#'
#' @export
pass <- function(x,
                 prompt = NULL,
                 action = c("interpret", "explain", "write", "summarize", "critique", "suggest"),
                 style = c("scientific", "simple", "detailed", "brief"),
                 output = c("text", "markdown", "md", "latex", "html"),
                 provider = c("openai", "anthropic", "gemini", "openrouter"),
                 model = NULL,
                 base_url = NULL,
                 api_key = NULL,
                 context = NULL,
                 system_message = NULL,
                 auto_local = TRUE,
                 copy = FALSE,
                 quiet = FALSE) {

  # Check if style was explicitly provided before match.arg
style_missing <- missing(style)

  action <- match.arg(action)
  style <- match.arg(style)
  output <- match.arg(output)
  if (output == "md") output <- "markdown"  # Alias
  provider <- match.arg(provider)

  # Default to "detailed" style for "write" action (unless user specified otherwise)
  if (action == "write" && style_missing) {
    style <- "detailed"
  }

  # Auto-detect local servers if base_url not provided and auto_local is TRUE
  if (is.null(base_url) && auto_local) {
    available_servers <- detect_local_servers()
    if (length(available_servers) > 0) {
      selected <- select_local_server(available_servers, quiet)
      if (!is.null(selected) && selected != "USE_CLOUD") {
        base_url <- selected
      }
      # If "USE_CLOUD" or NULL, base_url stays NULL and cloud provider will be used
    }
  }

  # Check if using local server
  use_local <- !is.null(base_url)

  # Capture the R output as text
  output_text <- paste(utils::capture.output(print(x)), collapse = "\n")

  # Also try to get the class/type info
  obj_class <- paste(class(x), collapse = ", ")
  obj_info <- paste0("Object class: ", obj_class)

  # Get API key (skip for local servers if not provided)
  if (use_local && is.null(api_key)) {
    api_key <- "local"  # Placeholder for local servers
  } else if (!use_local) {
    api_key <- get_api_key(provider, api_key, quiet)
  }

  # Set default model (check stored options first)
  if (is.null(model)) {
    if (use_local) {
      model <- "local-model"  # Placeholder, LM Studio uses loaded model
    } else {
      # Check for stored model option
      stored_model <- getOption(paste0("saqrmisc.", provider, "_model"))
      if (!is.null(stored_model)) {
        model <- stored_model
      } else {
        model <- switch(provider,
          openai = "gpt-4.1-nano",
          anthropic = "claude-sonnet-4-20250514",
          gemini = "gemini-2.5-flash",
          openrouter = "anthropic/claude-sonnet-4"
        )
      }
    }
  }

  # Check for stored base_url option (if not already set)
  if (is.null(base_url) && !use_local) {
    stored_base_url <- getOption(paste0("saqrmisc.", provider, "_base_url"))
    if (!is.null(stored_base_url)) {
      base_url <- stored_base_url
      use_local <- TRUE  # Treat custom base_url as local-style call
    }
  }

  # Build the prompt
  system_prompt <- build_system_prompt(action, style, output, system_message)
  user_prompt <- build_user_prompt(output_text, obj_info, prompt, context, action)

  if (!quiet) {
    if (use_local) {
      message("Sending to local server (", base_url, ")...")
    } else {
      message("Sending to ", provider, " (", model, ")...")
    }
  }

  # Call the API
  if (use_local) {
    response <- call_local_openai(base_url, model, api_key, system_prompt, user_prompt)
  } else {
    response <- call_ai_api(
      provider = provider,
      model = model,
      api_key = api_key,
      system_prompt = system_prompt,
      user_prompt = user_prompt
    )
  }

  # Print the response
  cat("\n", strrep("=", 60), "\n", sep = "")
  cat("AI ", tools::toTitleCase(action), " (", style, " style, ", output, " format)\n", sep = "")
  cat(strrep("=", 60), "\n\n", sep = "")
  cat(response, "\n")

  # Add disclaimer
  cat("\n", strrep("-", 60), "\n", sep = "")
  cat("IMPORTANT: AI-generated interpretations may contain errors.\n")
  cat("Always verify statistics against the original output below.\n")
  cat(strrep("-", 60), "\n", sep = "")

 # Print original R output for verification
  cat("\nORIGINAL R OUTPUT (for verification):\n")
  cat(strrep("-", 40), "\n", sep = "")
  cat(output_text, "\n")
  cat(strrep("=", 60), "\n", sep = "")

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

#' Check if a local server is running
#' @noRd
check_local_server <- function(url) {
  if (!requireNamespace("httr2", quietly = TRUE)) {
    return(FALSE)
  }
  tryCatch({
    req <- httr2::request(paste0(url, "/v1/models")) |>
      httr2::req_timeout(2)
    resp <- httr2::req_perform(req)
    httr2::resp_status(resp) == 200
  }, error = function(e) FALSE)
}

#' Detect available local servers (LM Studio, Ollama)
#' @noRd
detect_local_servers <- function() {
  servers <- list(
    lmstudio = list(name = "LM Studio", url = "http://127.0.0.1:1234"),
    ollama = list(name = "Ollama", url = "http://127.0.0.1:11434")
  )

  available <- list()
  for (id in names(servers)) {
    if (check_local_server(servers[[id]]$url)) {
      available[[id]] <- servers[[id]]
    }
  }
  available
}

#' Prompt user to select a local server
#' @noRd
select_local_server <- function(available, quiet = FALSE) {
  if (length(available) == 0) {
    return(NULL)
  }

  if (length(available) == 1) {
    server <- available[[1]]
    if (!quiet) {
      message("Found ", server$name, " running locally. Using it.")
      message("(To use a cloud provider instead, set provider explicitly or use base_url = NULL)")
    }
    return(server$url)
  }

  # Multiple servers found - ask user
  if (!interactive()) {
    # Non-interactive: use first available
    server <- available[[1]]
    if (!quiet) message("Using ", server$name, " (first available local server)")
    return(server$url)
  }

  # Interactive: let user choose
  if (!quiet) {
    message("\n", strrep("=", 50))
    message("Multiple local AI servers detected!")
    message(strrep("=", 50))
    for (i in seq_along(available)) {
      message(i, ". ", available[[i]]$name, " (", available[[i]]$url, ")")
    }
    message(length(available) + 1, ". Use cloud provider (enter API key)")
    message(strrep("=", 50))
  }

  choice <- readline(prompt = "Select server [1]: ")
  choice <- if (nzchar(choice)) as.integer(choice) else 1L

  if (is.na(choice) || choice < 1 || choice > length(available) + 1) {
    choice <- 1L
  }

  if (choice > length(available)) {
    # User chose cloud provider - return special marker
    return("USE_CLOUD")
  }

  available[[choice]]$url
}

#' Get or prompt for API key
#' @noRd
get_api_key <- function(provider, api_key = NULL, quiet = FALSE) {
  if (!is.null(api_key)) {
    return(api_key)
  }

  # Check environment variable
  env_var <- switch(provider,
    openai = "OPENAI_API_KEY",
    anthropic = "ANTHROPIC_API_KEY",
    gemini = "GEMINI_API_KEY",
    openrouter = "OPENROUTER_API_KEY"
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

#' Build system prompt based on action, style, output, and custom message
#' @noRd
build_system_prompt <- function(action, style, output, system_message = NULL) {
  base <- "You are an expert statistician helping researchers interpret their R output. "

  # Action-specific instructions
  action_prompts <- list(
    interpret = "Interpret the statistical results, explaining what they mean scientifically.",
    explain = "Explain what this analysis does and what the output means. Help the reader understand the methodology.",
    write = paste0(
      "Write publication-ready text with clearly labeled **Methods** and **Results** sections. ",
      "In Methods: describe the statistical test/model used, cite R and relevant packages (e.g., R Core Team, 2024). ",
      "In Results: report all statistics in APA format, include effect sizes with interpretation. ",
      "For regression models: include the equation (e.g., Y = b0 + b1*X1 + b2*X2). ",
      "End with a **References** section citing R and any packages mentioned (use proper citation format)."
    ),
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

  # Build the prompt

  result <- paste0(
    base,
    action_prompts[[action]], " ",
    style_prompts[[style]], " ",
    output_prompts[[output]]
  )

  # Add custom system message if provided
  if (!is.null(system_message) && nzchar(system_message)) {
    result <- paste0(result, " IMPORTANT: ", system_message)
  }

  result
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
    openai = call_openai(model, api_key, system_prompt, user_prompt),
    anthropic = call_anthropic(model, api_key, system_prompt, user_prompt),
    gemini = call_gemini(model, api_key, system_prompt, user_prompt),
    openrouter = call_openrouter(model, api_key, system_prompt, user_prompt)
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

#' Call Local OpenAI-compatible API (LM Studio, Ollama, vLLM, etc.)
#' @noRd
call_local_openai <- function(base_url, model, api_key, system_prompt, user_prompt) {
  if (!requireNamespace("httr2", quietly = TRUE)) {
    stop("Package 'httr2' is required. Install with: install.packages('httr2')")
  }

  # Ensure base_url ends without trailing slash
  base_url <- sub("/$", "", base_url)
  url <- paste0(base_url, "/v1/chat/completions")

  # Build request body - model is optional for local servers
  body <- list(
    messages = list(
      list(role = "system", content = system_prompt),
      list(role = "user", content = user_prompt)
    )
  )
  # Only add model if it's not the placeholder
  if (model != "local-model") {
    body$model <- model
  }

  req <- httr2::request(url) |>
    httr2::req_headers(
      Authorization = paste("Bearer", api_key),
      `Content-Type` = "application/json"
    ) |>
    httr2::req_body_json(body) |>
    httr2::req_timeout(300) |>  # 5 min timeout for local models
    httr2::req_error(is_error = function(resp) FALSE)

  resp <- httr2::req_perform(req)

  if (httr2::resp_status(resp) != 200) {
    error_body <- tryCatch(
      httr2::resp_body_json(resp),
      error = function(e) list(error = list(message = httr2::resp_status_desc(resp)))
    )
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

#' Call OpenRouter API
#' @noRd
call_openrouter <- function(model, api_key, system_prompt, user_prompt) {
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

  req <- httr2::request("https://openrouter.ai/api/v1/chat/completions") |>
    httr2::req_headers(
      Authorization = paste("Bearer", api_key),
      `Content-Type` = "application/json",
      `HTTP-Referer` = "https://github.com/mohsaqr/Saqrmisc",
      `X-Title` = "Saqrmisc R Package"
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

#' Set API Key and Options for Session
#'
#' @description
#' Set your API key and optional default settings for the current R session.
#' Settings are stored as R options and used as defaults by `pass()`.
#'
#' @param key Your API key
#' @param provider Provider name: "openai" (default), "anthropic", "gemini", or "openrouter"
#' @param model Optional. Default model to use for this provider.
#' @param ... Additional options to store (e.g., `base_url`, `timeout`).
#'
#' @return Invisibly returns TRUE.
#'
#' @examples
#' \dontrun{
#' # Just set the key
#' set_api_key("sk-...", "openai")
#'
#' # Set key and default model
#' set_api_key("sk-...", "openai", model = "gpt-4o")
#'
#' # Set key with custom base URL (for Azure OpenAI, etc.)
#' set_api_key("sk-...", "openai", model = "gpt-4", base_url = "https://my-azure.openai.azure.com")
#' }
#'
#' @export
set_api_key <- function(key, provider = c("openai", "anthropic", "gemini", "openrouter"),
                        model = NULL, ...) {
  provider <- match.arg(provider)


  # Set the API key as environment variable
  env_var <- switch(provider,
    openai = "OPENAI_API_KEY",
    anthropic = "ANTHROPIC_API_KEY",
    gemini = "GEMINI_API_KEY",
    openrouter = "OPENROUTER_API_KEY"
  )
  do.call(Sys.setenv, setNames(list(key), env_var))

  # Store additional options
  opts <- list(...)
  if (!is.null(model)) {
    opts$model <- model
  }

  if (length(opts) > 0) {
    # Store options with provider-specific names
    for (opt_name in names(opts)) {
      full_name <- paste0("saqrmisc.", provider, "_", opt_name)
      do.call(options, setNames(list(opts[[opt_name]]), full_name))
    }
    message("API key and settings set for ", provider, " (this session only)")
    message("  Options: ", paste(names(opts), "=", opts, collapse = ", "))
  } else {
    message("API key set for ", provider, " (this session only)")
  }

  invisible(TRUE)
}

#' Set OpenAI API Key
#'
#' @description
#' Convenience alias for `set_api_key(key, "openai", ...)`.
#'
#' @param key Your OpenAI API key
#' @param model Optional. Default model (e.g., "gpt-4o", "gpt-4.1-nano").
#' @param ... Additional options (e.g., `base_url` for Azure OpenAI).
#'
#' @examples
#' \dontrun{
#' set_openai_key("sk-...")
#' set_openai_key("sk-...", model = "gpt-4o")
#' set_openai_key("sk-...", model = "gpt-4", base_url = "https://my-azure.openai.azure.com")
#' }
#'
#' @export
set_openai_key <- function(key, model = NULL, ...) {
  set_api_key(key, "openai", model = model, ...)
}

#' Set Anthropic (Claude) API Key
#'
#' @description
#' Convenience alias for `set_api_key(key, "anthropic", ...)`.
#'
#' @param key Your Anthropic API key
#' @param model Optional. Default model (e.g., "claude-sonnet-4-20250514").
#' @param ... Additional options.
#'
#' @examples
#' \dontrun{
#' set_claude_key("sk-ant-...")
#' set_claude_key("sk-ant-...", model = "claude-sonnet-4-20250514")
#' }
#'
#' @export
set_claude_key <- function(key, model = NULL, ...) {
  set_api_key(key, "anthropic", model = model, ...)
}

#' Set Google Gemini API Key
#'
#' @description
#' Convenience alias for `set_api_key(key, "gemini", ...)`.
#'
#' @param key Your Gemini API key
#' @param model Optional. Default model (e.g., "gemini-2.5-flash").
#' @param ... Additional options.
#'
#' @examples
#' \dontrun{
#' set_gemini_key("AIza...")
#' set_gemini_key("AIza...", model = "gemini-2.5-pro")
#' }
#'
#' @export
set_gemini_key <- function(key, model = NULL, ...) {
  set_api_key(key, "gemini", model = model, ...)
}

#' Set OpenRouter API Key
#'
#' @description
#' Convenience alias for `set_api_key(key, "openrouter", ...)`.
#'
#' @param key Your OpenRouter API key
#' @param model Optional. Default model (e.g., "anthropic/claude-sonnet-4", "openai/gpt-4o").
#' @param ... Additional options.
#'
#' @examples
#' \dontrun{
#' set_openrouter_key("sk-or-...")
#' set_openrouter_key("sk-or-...", model = "openai/gpt-4o")
#' }
#'
#' @export
set_openrouter_key <- function(key, model = NULL, ...) {
  set_api_key(key, "openrouter", model = model, ...)
}
