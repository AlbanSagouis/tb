##----------------------------------------------------------------------------##
##                               READ/WRITE FUNS                              ##
##----------------------------------------------------------------------------##

#' readlines
#'
#' Read lines of file
#'
#' Simple wrapper around \code{\link[base]{readLines}} that automates opening and
#'   closing of connection file.
#'
#' @param x Input
#' @param ... Other args passed to \code{readLines}.
#' @return Output
#' @author Michael Wayne Kearney
#' @export
#'
readlines <- function(x, ...) {
  dots <- list(...)
  dots <- add_arg_if(dots, encoding = "UTF-8", skipNul = TRUE, warn = FALSE)
  if (is_url(x)) {
    con <- url(x, encoding = dots$encoding)
  } else {
    con <- file(x, encoding = dots$encoding)
  }
  on.exit(close(con))
  dots$con <- con
  do.call("readLines", dots)
}
