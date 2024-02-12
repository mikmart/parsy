parser <- function(f) {
  stopifnot(is.function(f))
  # Encourage users to call via parser_run().
  structure(list(), run = f, class = "parser")
}

is_parser <- function(x) inherits(x, "parser")
run_unsafe <- function(p, x) attr(p, "run")(x)

#' Run a Parser
#'
#' @param p A parser object.
#' @param x Input to be parsed.
#'
#' @return A parser state object wrapped in a result object.
#'
#' @export
parser_run <- function(p, x) {
  if (!is_parser(p))
    stop(sprintf("`p` must be a <parser> object, not <%s>.", class(p)[1]))
  if (!is_state(x))
    x <- state(x)
  r <- run_unsafe(p, x)
  if (!is_result(r))
    stop(sprintf("Expected parser to return <result>, got <%s>.", class(r)[1]))
  r
}

#' Parser Combination
#'
#' @param p,q,... Parser objects.
#'
#' @return A parser object.
#'
#' @export
parser_many <- function(p) {
  parser(function(x) {
    values <- list()
    repeat {
      r <- parser_run(p, x)
      if (is_failure(r))
        break
      x <- from_success(r)
      values <- push(values, val(x))
    }
    success(set_value(x, values))
  })
}

push <- function(l, x) c(l, list(x))

#' @rdname parser_many
#' @export
parser_all <- function(...) {
  parser(function(x) {
    values <- list()
    for (p in list(...)) {
      r <- parser_run(p, x)
      if (is_failure(r))
        return(r)
      x <- from_success(r)
      values <- push(values, val(x))
    }
    values <- setNames(values, ...names())
    success(set_value(x, values))
  })
}

#' @rdname parser_many
#' @export
parser_any <- function(...) {
  Reduce(`%|%.parser`, list(...))
}

#' @rdname parser_many
#' @export
`%|%.parser` <- function(p, q) {
  parser(function(x) {
    parser_run(p, x) %|% parser_run(q, x)
  })
}

#' Parser Transformation
#'
#' @param p A parser object.
#' @param f A function that takes the value(s) parsed by `p` and returns a new value.
#'
#' @return A new parser object.
#'
#' @export
#' @examples
#' parse_letters() |> parser_map(toupper) |> parser_run("hello world")
parser_map <- function(p, f) {
  parser(function(x) {
    parser_run(p, x) |> result_map(\(y) map_value(y, f))
  })
}

#' @rdname parser_map
#' @export
parser_call <- function(p, f) {
  parser_map(p, \(x) do.call(f, x))
}

#' Parser Sequencing
#'
#' `parser_return()` creates a parser that will resolve to the specified `value`
#'  without consuming any input.
#'
#' @param p,q Parser objects.
#' @param f A function that takes the value parsed by `p` and returns a new parser.
#'
#' @return A new parser object.
#'
#' @export
parser_bind <- function(p, f) {
  parser(function(x) {
    parser_run(p, x) |> result_bind(\(y) f(val(y)) |> parser_run(y))
  })
}

#' @rdname parser_bind
#' @export
parser_then <- function(p, q) {
  parser_bind(p, \(x) q)
}

#' @rdname parser_bind
#' @param value The parse value that the new parser resolves to.
#' @export
parser_return <- function(value) {
  parser(function(x) {
    success(set_value(x, value))
  })
}

#' @rdname parser_bind
#' @param error The parse error that the new parser resolves to.
#' @export
parser_fail <- function(error) {
  parser(function(x) {
    failure(set_error(x, error))
  })
}

#' @rdname parser_bind
#' @param ... Parser objects.
#' @export
parser_do <- function(...) {
  dots <- match.call(expand.dots = FALSE)$...
  caller <- parent.frame()
  parser(function(x) {
    env <- new.env(parent = caller)
    for (i in seq_along(dots)) {
      p <- eval(dots[[i]], env)
      r <- parser_run(p, x)
      if (is_failure(r))
        return(r)
      x <- from_success(r)
      n <- names(dots)[[i]]
      if (!is.null(n) && nzchar(n))
        assign(n, val(x), env)
    }
    r
  })
}

# TODO: Proper print methods.

#' @export
print.parser <- function(x, ...) {
  str(x)
  x
}
