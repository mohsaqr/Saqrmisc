# Ask an AI model to judge suggested keyword merges

Sends the pairs from \[suggest_merges()\], with example titles, to a
language model, which says for each pair whether the two keywords are
the \`same\` concept, \`different\` ones, or it is \`unsure\`, with a
name, how sure it is (0 to 1) and why.

Works with any OpenAI-compatible server (OpenAI, Google Gemini,
OpenRouter, Groq, Mistral, a local Ollama or LM Studio) and with
Anthropic. Without a \`url\`, a \`model\` starting with \`claude\` goes
to Anthropic, one starting with \`gemini\` to Google's Gemini API,
anything else to OpenAI. Gemini Nano runs only inside Chrome and
Android, so R cannot reach it; Gemma 3n, Google's open model built like
it, runs locally through Ollama: \`model = "gemma3n", url =
"http://localhost:11434/v1"\`.

Pairs go 20 to a request, or 5 to a local server (\`localhost\`): small
local models lose track in long lists and answer every pair alike.

Unanswered pairs are asked again each time you call \`ask_ai()\` on its
result, so an interrupted or failed run simply continues. In an
interactive session it says how many requests it will send and asks
first.

## Usage

``` r
ask_ai(x, model, key = NULL, url = NULL, instructions = NULL, again = NULL)
```

## Arguments

- x:

  A \[suggest_merges()\] result, or an earlier \`ask_ai()\` result.

- model:

  The model's name, e.g. \`"gpt-5-mini"\`, \`"claude-haiku-5-5"\`,
  \`"gemini-3-flash"\`, \`"gemma3n"\`.

- key:

  Your API key. Default: \`OPENAI_API_KEY\`; \`ANTHROPIC_API_KEY\` for
  Anthropic; \`GEMINI_API_KEY\` (or \`GOOGLE_API_KEY\`) for Gemini.
  Leave empty for a local server. It is never saved.

- url:

  The server, for anything other than OpenAI or Anthropic, e.g.
  \`"https://openrouter.ai/api/v1"\` or \`"http://localhost:11434/v1"\`.

- instructions:

  Anything the model should know, in plain words, e.g. \`"The papers are
  on chatbots in education. Prefer AI to ARTIFICIAL INTELLIGENCE."\`
  Without it the model works out the field from the titles.

- again:

  Ask again about pairs the AI already judged, e.g. \`again = "unsure"\`
  with a stronger \`model\`.

## Value

\`x\` with \`ai_verdict\`, \`ai_name\`, \`ai_confidence\` and
\`ai_reason\` filled in, and \`decision\`: \`accept\` when the AI said
\`same\` and was at least 80% sure, \`reject\` when it said
\`different\` and was at least 80% sure, \`check\` otherwise.
\`keywords_clean(replace = x)\` applies the accepted pairs as they are;
\[review_sheet()\] lets you overrule them. A pair whose answer could not
be read is \`failed\` and is asked again next time.

## Errors

Raises \`saqrmisc_bad_input\` for a missing \`model\` or key;
\`saqrmisc_missing_package\` without httr2 or jsonlite;
\`saqrmisc_ai_failed\` when the first request fails (wrong key, model or
server). A later failure, or stopping the run, warns
\`saqrmisc_ai_failed\` and keeps the answers so far.

## See also

\[suggest_merges()\], \[review_sheet()\]

## Examples

``` r
if (FALSE) { # \dontrun{
merges <- suggest_merges(cleaned)
merges <- ask_ai(merges, model = "gpt-5-mini")
merges <- ask_ai(merges, model = "claude-haiku-5-5",
                 instructions = "The papers are on chatbots in education.")
merges <- ask_ai(merges, model = "gemini-3-flash")
merges <- ask_ai(merges, model = "gemma3n", url = "http://localhost:11434/v1")
merges <- ask_ai(merges, model = "claude-sonnet-5-5", again = "unsure")
merges
} # }
```
