# Set OpenRouter API Key

Convenience alias for \`set_api_key(key, "openrouter", ...)\`.

## Usage

``` r
set_openrouter_key(key, model = NULL, ...)
```

## Arguments

- key:

  Your OpenRouter API key

- model:

  Optional. Default model (e.g., "anthropic/claude-sonnet-4",
  "openai/gpt-4o").

- ...:

  Additional options.

## Examples

``` r
if (FALSE) { # \dontrun{
set_openrouter_key("sk-or-...")
set_openrouter_key("sk-or-...", model = "openai/gpt-4o")
} # }
```
