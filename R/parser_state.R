state <- function(x, index = 1L, value = NULL, error = NULL) {
  stopifnot(is.character(x) && length(x) == 1) #? Support other kinds of input?
  structure(x, index = as.integer(index), value = value, error = error, class = "parser_state")
}

is_state <- function(x) inherits(x, "parser_state")

ind <- function(x) attr(x, "index", exact = TRUE)
val <- function(x) attr(x, "value", exact = TRUE)
err <- function(x) attr(x, "error", exact = TRUE)

set_value <- function(x, value) structure(x, value = value)
set_error <- function(x, error) structure(x, error = error)

map_value <- function(x, f) set_value(x, value = f(val(x)))

consume <- function(x, n) {
  #? What if input is not a string but e.g. binary or tokens?
  if (!is.integer(n))
    n <- as.integer(n)
  if (length(n) == 0 || is.na(n) || n < 0L)
    stop("`n` must be a non-negative integer.")
  if (n == 0L)
    return(x)
  structure(substring(x, n + 1L), index = ind(x) + n)
}

# Format a message in the context of a parser state.
state_format <- function(x, message, ...) {
  sprintf(paste(message, "'%s' at index %d"), ..., toString(x, 20), ind(x))
}

#' @export
print.parser_state <- function(x, ...) {
  str(x)
  x
}
