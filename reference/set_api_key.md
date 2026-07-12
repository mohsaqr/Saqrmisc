# Set API Key and Options for Session

Set your API key and optional default settings for the current R
session. Settings are stored as R options and used as defaults by
\`pass()\`.

## Usage

``` r
set_api_key(
  key,
  provider = c("openai", "anthropic", "gemini", "openrouter"),
  model = NULL,
  ...
)
```

## Arguments

- key:

  Your API key

- provider:

  Provider name: "openai" (default), "anthropic", "gemini", or
  "openrouter"

- model:

  Optional. Default model to use for this provider.

- ...:

  Additional options to store (e.g., \`base_url\`, \`timeout\`).

## Value

Invisibly returns TRUE.

## Examples

``` r
if (FALSE) { # \dontrun{
# Just set the key
set_api_key("sk-...", "openai")

# Set key and default model
set_api_key("sk-...", "openai", model = "gpt-4o")

# Set key with custom base URL (for Azure OpenAI, etc.)
set_api_key("sk-...", "openai", model = "gpt-4", base_url = "https://my-azure.openai.azure.com")
} # }
```
