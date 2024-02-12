
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
# FILL THIS IN! HOW CAN PEOPLE INSTALL YOUR DEV PACKAGE?
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(parsy)

p <- parser_do(
  x = parse_digits() |>
    parser_map(as.integer),
  parse_whitespace(),
  op = parser_any(
    parse_string("+"),
    parse_string("-")
  ),
  parse_whitespace(),
  y = parse_digits() |>
    parser_map(as.integer),
  op |> switch(
    "+" = parser_return(x + y),
    "-" = parser_return(x - y)
  )
)

parser_run(p, "1 + 1")
#> ✔ Success
#>  'parser_state' chr ""
#>  - attr(*, "index")= int 6
#>  - attr(*, "value")= int 2
parser_run(p, "2 - 1")
#> ✔ Success
#>  'parser_state' chr ""
#>  - attr(*, "index")= int 6
#>  - attr(*, "value")= int 1
parser_run(p, "1 + foo")
#> ✖ Failure
#>  'parser_state' chr "foo"
#>  - attr(*, "index")= int 5
#>  - attr(*, "value")= chr " "
#>  - attr(*, "error")= chr "expected some digits got 'foo' at index 5"
```

## Inspiration

- Parser Combinators From Scratch by Low Byte Productions.
  <https://youtu.be/6oQLRhw5Ah0>
- JSON Parser From Scratch in Haskell by Tsoding.
  <https://youtu.be/N9RUqGYuGfw>
- Functional Parsing by Computerphile. <https://youtu.be/dDtZLm7HIJs>
- Hutton G, Meijer E (1996). Monadic Parser Combinators.
  <https://www.cs.nott.ac.uk/~pszgmh/monparsing.pdf>
