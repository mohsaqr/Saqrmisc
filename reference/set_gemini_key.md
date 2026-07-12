# Set Google Gemini API Key

Convenience alias for \`set_api_key(key, "gemini", ...)\`.

## Usage

``` r
set_gemini_key(key, model = NULL, ...)
```

## Arguments

- key:

  Your Gemini API key

- model:

  Optional. Default model (e.g., "gemini-2.5-flash").

- ...:

  Additional options.

## Examples

``` r
if (FALSE) { # \dontrun{
set_gemini_key("AIza...")
set_gemini_key("AIza...", model = "gemini-2.5-pro")
} # }
```
