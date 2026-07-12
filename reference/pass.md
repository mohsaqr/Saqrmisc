# Pass R Output to AI for Interpretation

Pipes any R output (test results, model summaries, tables) to an AI
model for scientific interpretation. Perfect for getting
publication-ready interpretations of statistical results.

## Usage

``` r
pass(
  x,
  prompt = NULL,
  append_prompt = NULL,
  action = c("write", "interpret", "explain", "summarize", "critique", "suggest"),
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
  quiet = FALSE
)
```

## Arguments

- x:

  Any R object to interpret (test result, model, data frame, etc.)

- prompt:

  Custom prompt to use. If NULL, builds from action and style. Note:
  This REPLACES the default prompt entirely.

- append_prompt:

  Additional instructions to ADD to the default prompt. Unlike
  \`prompt\`, this appends to (not replaces) the auto-generated prompt.

- action:

  What to do with the output:

  - \`"write"\` (default): Write publication-ready text
    (methods/results)

  - \`"interpret"\`: Interpret the statistical results

  - \`"explain"\`: Explain what the analysis does and means

  - \`"summarize"\`: Brief summary of key findings

  - \`"critique"\`: Critical evaluation with limitations

  - \`"suggest"\`: Suggest follow-up analyses

- style:

  Interpretation style:

  - \`"scientific"\` (default): APA-style for academic papers

  - \`"simple"\`: Plain language, no jargon

  - \`"detailed"\`: Comprehensive with assumptions, limitations, caveats

  - \`"brief"\`: Just the key takeaway

- output:

  Output format:

  - \`"text"\` (default): Plain text

  - \`"markdown"\` or \`"md"\`: Markdown formatted

  - \`"latex"\`: LaTeX formatted for papers

  - \`"html"\`: HTML formatted

- provider:

  AI provider: \`"openai"\` (default), \`"anthropic"\`, \`"gemini"\`, or
  \`"openrouter"\`.

- model:

  Model to use. Defaults: \`"gpt-4.1-nano"\` (OpenAI),
  \`"claude-sonnet-4-20250514"\` (Anthropic), \`"gemini-2.5-flash"\`
  (Gemini), \`"anthropic/claude-sonnet-4"\` (OpenRouter).

- base_url:

  Custom API base URL for OpenAI-compatible servers (e.g., LM Studio,
  Ollama, vLLM). Example: \`"http://127.0.0.1:1234"\` for LM Studio.
  When set, uses OpenAI-compatible format regardless of provider
  setting.

- api_key:

  API key. If NULL, checks environment variables (\`OPENAI_API_KEY\`,
  \`ANTHROPIC_API_KEY\`, \`GEMINI_API_KEY\`, or \`OPENROUTER_API_KEY\`),
  then prompts interactively. For local servers like LM Studio, use
  \`api_key = "none"\` or any string.

- context:

  Optional context about your study (e.g., "This is a study on student
  learning outcomes with N=500 participants")

- system_message:

  Optional custom instructions for the AI (e.g., "Focus on clinical
  implications", "Be more concise", "Emphasize effect sizes")

- auto_local:

  Logical. Automatically detect and use local AI servers (LM Studio on
  port 1234, Ollama on port 11434)? Default: TRUE. Set to FALSE to force
  using cloud providers.

- copy:

  Logical. Copy result to clipboard? Default: FALSE

- quiet:

  Logical. Suppress messages? Default: FALSE

## Value

Character string with the AI interpretation (invisibly). Also prints the
interpretation.

## Details

On first use, you'll be prompted to enter your API key. The key is
stored in your R environment for the session. To persist it, add to your
.Renviron: “\` ANTHROPIC_API_KEY=your-key-here \# or
OPENAI_API_KEY=your-key-here “\`

## Examples

``` r
if (FALSE) { # \dontrun{
# Basic usage - pipe test results
t.test(mpg ~ am, data = mtcars) |> pass()

# With context
cor.test(mtcars$mpg, mtcars$hp) |>
  pass(context = "Studying fuel efficiency in 1974 automobiles")

# Different actions
lm(mpg ~ wt + hp, data = mtcars) |> summary() |> pass(action = "write")
chisq.test(mtcars$cyl, mtcars$am) |> pass(action = "explain", style = "simple")

# Get LaTeX output for paper
aov(mpg ~ factor(cyl), data = mtcars) |> summary() |>
  pass(action = "write", output = "latex")

# Critique an analysis
lm(mpg ~ ., data = mtcars) |> summary() |> pass(action = "critique")

# Custom prompt (REPLACES default - specific request to the AI)
my_results |> pass(prompt = "Focus only on the interaction effects")

# Add to default prompt (keeps "write methods/results" + your addition)
my_results |> pass(append_prompt = "Also mention limitations of the sample size")
my_results |> pass(action = "write", append_prompt = "Include a brief discussion section")

# Custom context (about your study)
t.test(score ~ group, data = mydata) |>
  pass(context = "RCT comparing drug vs placebo, N=200 patients with diabetes")

# Custom system message (instructions for the AI)
my_results |> pass(system_message = "Focus on clinical implications and effect sizes")

# Combine all customizations
lm(outcome ~ treatment * age, data = mydata) |> summary() |>
  pass(
    action = "write",
    context = "Phase 3 clinical trial for hypertension medication",
    system_message = "Emphasize clinical significance over statistical significance",
    prompt = "Pay special attention to the treatment-age interaction"
  )
} # }
```
