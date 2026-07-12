# Set OpenAI API Key

Convenience alias for \`set_api_key(key, "openai", ...)\`.

## Usage

``` r
set_openai_key(key, model = NULL, ...)
```

## Arguments

- key:

  Your OpenAI API key

- model:

  Optional. Default model (e.g., "gpt-4o", "gpt-4.1-nano").

- ...:

  Additional options (e.g., \`base_url\` for Azure OpenAI).

## Examples

``` r
if (FALSE) { # \dontrun{
set_openai_key("sk-...")
set_openai_key("sk-...", model = "gpt-4o")
set_openai_key("sk-...", model = "gpt-4", base_url = "https://my-azure.openai.azure.com")
} # }
```
