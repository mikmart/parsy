
<!-- README.md is generated from README.Rmd. Please edit that file -->

# parsy

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

parsy implements functional-inspired monadic parser combinators in R.

## Installation

You can install the development version of parsy like so:

``` r
pak::pkg_install("mikmart/parsy")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(parsy)

# An evaluating parser for numeric operators.
parse_eval <- parser_do(
  x = parse_number,
  parse_whitespace,
  op = parse_operator,
  parse_whitespace,
  y = parse_eval,
  op |> switch(
    "+" = parser_return(x + y),
    "*" = parser_return(x * y)
  )
) %|% parse_number

# Define the required sub-parsers.
parse_number <- parse_digits() |> parser_map(as.integer)
parse_whitespace <- parse_regexp("^[[:space:]]*")
parse_operator <- parse_string("+") %|% parse_string("*")

# Run the parser on some inputs.
parse_eval |> parser_run("1 + 1")
#> ✔ Success
#>  'parser_state' chr ""
#>  - attr(*, "index")= int 6
#>  - attr(*, "value")= int 2
parse_eval |> parser_run("2 + 5 * 8")
#> ✔ Success
#>  'parser_state' chr ""
#>  - attr(*, "index")= int 10
#>  - attr(*, "value")= int 42
```

## Inspiration

- Parser Combinators From Scratch by Low Byte Productions.
  <https://youtu.be/6oQLRhw5Ah0>
- JSON Parser From Scratch in Haskell by Tsoding.
  <https://youtu.be/N9RUqGYuGfw>
- Functional Parsing by Computerphile. <https://youtu.be/dDtZLm7HIJs>
- Hutton G, Meijer E (1996). Monadic Parser Combinators.
  <https://www.cs.nott.ac.uk/~pszgmh/monparsing.pdf>
