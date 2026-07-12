# Set Anthropic (Claude) API Key

Convenience alias for \`set_api_key(key, "anthropic", ...)\`.

## Usage

``` r
set_claude_key(key, model = NULL, ...)
```

## Arguments

- key:

  Your Anthropic API key

- model:

  Optional. Default model (e.g., "claude-sonnet-4-20250514").

- ...:

  Additional options.

## Examples

``` r
if (FALSE) { # \dontrun{
set_claude_key("sk-ant-...")
set_claude_key("sk-ant-...", model = "claude-sonnet-4-20250514")
} # }
```
