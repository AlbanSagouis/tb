#' Replace text in one script
#'
#' Open a file with a .r or .R extension and runs a gsub. No copy made except if newPath is
#' specified, use at your own risks.
#'
#' @param fullPath A character vector with the path to the script to modify.
#' @param pattern passed to \code{\link[base:grep]{base::gsub}}.
#' @param replacement passed to \code{\link[base:grep]{base::gsub}}.
#' @param newPath Character vector. Used if a copy of the script should be created
#' following this path.
#' @param userCheck Logical, default = TRUE. Matches are shown to check the pattern `accuracy`
#' before replacement.
#' @param ignore.case logical. FALSE for case-sensitive matches. Default is TRUE.
#'
#' @return The number of matches in the file.
#'
#' @details If not working inside an R project, path has to be complete from root.
#' @seealso To search for text in any text file, see \code{search_files}.
#'
#' @examples \dontrun{gsubInOneScript('DESCRIPTION','foobar@gmail.com','foobar@protonmail.com')}
#' @examples \dontrun{gsubInOneScript('master.r','ifelse','data.table::fifelse')}
#' @author Alban Sagouis
#' @export

gsubInOneScript <- function(
  fullPath,
  pattern,
  replacement,
  newPath = NULL,
  userCheck = TRUE,
  ignore.case = FALSE
) {
  sourceF <- file(fullPath, open = "r+b")
  lines <- readLines(sourceF)
  matches <- sapply(lines, grepl, pattern = pattern)

  if (userCheck) {
    search_these_files(pattern, fullPath, ignore.case = ignore.case)
    answer <- utils::askYesNo(paste0(
      'Do you want to replace matches of "',
      pattern,
      '" with "',
      replacement,
      '"?'
    ))
    if (is.na(answer)) {
      stop
    }
    if (answer) {
      for (i in which(matches)) {
        lines[i] <- gsub(lines[i], pattern = pattern, replacement = replacement)
      }
    }
  } else {
    for (i in which(matches)) {
      lines[i] <- gsub(lines[i], pattern = pattern, replacement = replacement)
    }
  }

  if (!is.null(newPath)) {
    sourceF <- file(newPath, open = "w")
  }

  writeLines(text = lines, con = sourceF) #, sep = "\n"
  close(sourceF) # closing connection
  return(sum(matches))
}


#' Replace text in all scripts from a folder
#'
#' Opens all files with a .r or .R extension in the directory and runs a gsub.
#'
#' @param fullPath A character vector with the path to the folder containing
#' the scripts to modify.
#' @param pattern passed to \code{\link[base:grep]{base::gsub}}.
#' @param replacement passed to \code{\link[base:grep]{base::gsub}}.
#' @param recursive logical. Passed to \code{base::list.files}. Should the listing recurse into directories?
#' @param newPath description
#' @param userCheck Logical, default = TRUE. Matches are shown to check the pattern `accuracy`
#' before replacement.
#' @param ignore.case logical. FALSE for case-sensitive matches. Default is TRUE.
#'
#' @return A data.frame with the number of matches in each file.
#'
#' @details If not working inside an R project, path has to be complete from root.
#'
#' @examples \dontrun{gsubInOneFolder('./R', pattern = 'sum', replacement = 'sumNA')}
#' @examples \dontrun{gsubInOneFolder('.',  pattern = 'read.csv', replacement = 'data.table::fread')}
#'
#' @author Alban Sagouis
#' @export

gsubInOneFolder <- function(
  fullPath,
  pattern,
  replacement,
  recursive = FALSE,
  newPath = NULL,
  userCheck = TRUE,
  ignore.case = FALSE
) {
  assertthat::see_if(dir.exists(fullPath))
  assertthat::see_if(assertthat::is.writeable(fullPath))

  listF <- list.files(
    fullPath,
    pattern = "\\.R$|\\.r$",
    full.names = TRUE,
    recursive = recursive
  )
  if (userCheck) {
    search_these_files(pattern, listF, ignore.case = ignore.case)
    answer <- utils::askYesNo(paste0(
      'Do you want to replace matches of "',
      pattern,
      '" with "',
      replacement,
      '"?'
    ))
    if (is.na(answer) || !answer) stop
  }

  nmatches <- sapply(
    listF,
    gsubInOneScript,
    pattern,
    replacement,
    newPath,
    userCheck = FALSE
  )

  return(data.frame(nmatches[nmatches > 0]))
}
