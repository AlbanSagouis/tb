#' Execute one script in the background without loading anything in the user's current environment
#'
#' Source the given file with a .r or .R extension.
#'
#' @param fullPath A character vector with the path to the scripts to execute
#' @param echo logical. Passed to \code{\link[base]{source}}.
#' @param local logical. Passed to \code{\link[base]{source}}.
#'
#' @return NULL
#' @details If not working inside an Rproject, path has to be complete from root.
#' @import data.table
#' @author Alban Sagouis
#'
#'
#' @export

executeOne <- function(fullPath = NULL, echo = FALSE, local = TRUE) {
  source(fullPath, encoding = 'UTF-8', echo = echo, local = local)
  # sys.source(fullPath, envir = new.env(), chdir = FALSE, keep.source = FALSE, keep.parse.data = FALSE, toplevel.env = topenv())
}

#' Execute all scripts inside a folder
#'
#' Source all files with a .r or .R extension in the specified folder.
#'
#' @param fullPath A character vector with the path to the folder containing the scripts to
#' execute
#' @param echo logical. Passed to \code{\link[base]{source}}.
#' @param local logical. Passed to \code{\link[base]{source}}.
#' @param recursive logical. Passed to \code{\link[base]{list.files}}. Should the
#' listing recurse into directories?
#'
#' @return The list of executed scripts.
#' @details If not working inside an Rproject, path has to be complete from root.
#'
#' @author Alban Sagouis
#'
#'
#' @export

executeAll <- function(
  fullPath = NULL,
  echo = FALSE,
  local = TRUE,
  recursive = FALSE
) {
  listF <- list.files(
    fullPath,
    pattern = ".R|.r",
    full.names = TRUE,
    recursive = recursive
  )
  lapply(listF, executeOne, echo = echo, local = local)
  return(listF)
}
