success <- function(x) {
  structure(list(value = x), class = c("success", "result"))
}

failure <- function(x) {
  structure(list(error = x), class = c("failure", "result"))
}

is_result <- function(x) inherits(x, "result")
is_success <- function(x) inherits(x, "success")
is_failure <- function(x) inherits(x, "failure")

from_success <- function(x) {
  if (is_success(x))
    return(x$value)
  stop(sprintf("Can't unwrap <success> from <%s>.", class(x)[1]))
}

failure_map <- function(x, f) {
  if (is_failure(x)) failure(f(x$error)) else x
}

result_map <- function(x, f) {
  if (is_success(x)) success(f(x$value)) else x
}

result_bind <- function(x, f) {
  if (is_success(x)) f(x$value) else x
}

#' @export
`%|%.result` <- function(x, y) {
  if (is_failure(x)) y else x
}

#' @export
print.success <- function(x, ...) {
  if (is_installed("cli")) {
    cli::cli_alert_success("Success")
  } else {
    cat("Success\n")
  }
  print(x$value, ...)
}

#' @export
print.failure <- function(x, ...) {
  if (is_installed("cli")) {
    cli::cli_alert_danger("Failure")
  } else {
    cat("Failure\n")
  }
  print(x$error, ...)
}

is_installed <- function(package) {
  requireNamespace(package, quietly = TRUE)
}
