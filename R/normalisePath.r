#' Replace backslashes into slashes in paths
#'
#' @param fullPath A character vector with the path to transform.
#'
#' @return Character string: transformed path.
#'
#' @export

normalisePath <- function(fullPath) {
  gsub(pattern = '\\', replacement = '/', x = fullPath, fixed = TRUE)
}
