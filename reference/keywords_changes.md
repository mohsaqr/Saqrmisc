# Every keyword that changed

One row per written keyword of a \[keywords_clean()\] result that
changed, with what it became and why.

## Usage

``` r
keywords_changes(x)
```

## Arguments

- x:

  A \[keywords_clean()\] result.

## Value

A data frame with one row per changed keyword, with the columns listed
under Value in \[keywords_clean()\].

## Errors

Raises \`saqrmisc_bad_input\` when \`x\` is not a \[keywords_clean()\]
result.

## Examples

``` r
papers <- data.frame(DE = c("Chatbots; ChatBot", "chat-bot; LLMs"))
keywords <- keywords_clean(papers)
keywords_changes(keywords)
#>   column     from      to         reason papers
#> 1     DE  ChatBot CHATBOT        cleanup      1
#> 2     DE Chatbots CHATBOT cleanup+merged      1
#> 3     DE     LLMs    LLMS        cleanup      1
#> 4     DE chat-bot CHATBOT cleanup+merged      1
```
