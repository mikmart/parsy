#' Basic Parsers
#'
#' @return A parser object.
#'
#' @name parsers
NULL

#' @param string A sequence of characters to match.
#' @rdname parsers
#' @export
parse_string <- function(string) {
  n <- nchar(string)
  parser(function(x) {
    if (startsWith(x, string)) {
      success(set_value(x, string) |> consume(n))
    } else {
      failure(set_error(x, state_format(x, "expected '%s' got", string)))
    }
  })
}

#' @param pattern A [regular expression][regexp] to match.
#' @rdname parsers
#' @export
parse_regexp <- function(pattern) {
  stopifnot(startsWith(pattern, "^"))
  parser(function(x) {
    if (length(regmatches(x, regexpr(pattern, x)) -> v)) {
      success(set_value(x, v) |> consume(nchar(v)))
    } else {
      failure(set_error(x, state_format(x, "regexp '%s' did not match", pattern)))
    }
  })
}

#' @rdname parsers
#' @export
parse_digits <- function() {
  parser(function(x) {
    parse_regexp("^[[:digit:]]+") |> parser_run(x) |> failure_map(\(y) {
      set_error(y, state_format(y, "expected some digits got"))
    })
  })
}

#' @rdname parsers
#' @export
parse_letters <- function() {
  parser(function(x) {
    parse_regexp("^[[:alpha:]]+") |> parser_run(x) |> failure_map(\(y) {
      set_error(y, state_format(y, "expected some letters got"))
    })
  })
}

#' @rdname parsers
#' @export
parse_whitespace <- function() {
  parser(function(x) {
    parse_regexp("^[[:space:]]*") |> parser_run(x) |> failure_map(\(y) {
      set_error(y, state_format(y, "expected whitespace got"))
    })
  })
}
