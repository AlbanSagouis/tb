#' search_files
#'
#' Returns matching files and line numbers of given string pattern.
#'
#' @param x Pattern.
#' @param path Path on which to restrict search. Defaults to current working
#'   directory.
#' @param recursive logical
#' @param all.files logical. Default \code{FALSE} excludes dot files
#' @param textOnly logical. Default \code{TRUE}. Looks inside .r .R
#'   .Rmd .md .html .txt .cpp .h files.
#' @return Output from terminal - file name, line number, and preview of
#'   matching text
#' @author Michael Wayne Kearney
#' @export
#'

search_files <- function(
  x,
  path = ".",
  recursive = TRUE,
  all.files = FALSE,
  textOnly = TRUE
) {
  f <- list.files(
    path,
    all.files = all.files,
    full.names = TRUE,
    recursive = recursive
  )
  i <- file.info(f)
  i <- i[i$size < 25000, ]
  f <- row.names(i)
  if (textOnly) {
    f <- grep(
      "\\.r$|\\.R$|\\.Rmd$|\\.md$|\\.html$|\\.txt$|\\.cpp$|\\.h$",
      f,
      value = TRUE
    )
  } else {
    f <- grep(
      "\\.png$|\\.jpeg$|\\.jpg$|\\.rda$|\\.rds$|\\.tiff$|\\.pdf$",
      f,
      invert = TRUE,
      value = TRUE
    )
  }
  s <- lapply(f, extra_text_search, pat = x)
  names(s) <- f
  s <- s[lengths(s) > 0]
  nms <- names(s)
  s <- lapply(s, paste, collapse = "\n")
  for (i in seq_along(s)) {
    cat("\n# File:", nms[i], fill = TRUE)
    txt <- s[[i]]
    txt <- gsub("\n", "\n", txt)
    cat(txt, fill = TRUE)
  }
}

#' Search these files
#'
#' Look for text in a group of files
#'
#' @param x Regex pattern
#' @param f Vector of files
#' @param ignore.case logical. FALSE for case-sensitive matches. Default is TRUE.
#' @return Prints matches
#' @author Michael Wayne Kearney
#' @export
#'

search_these_files <- function(x, f, ignore.case = TRUE) {
  s <- lapply(f, extra_text_search, pat = x, ignore.case = ignore.case)
  names(s) <- f
  s <- s[lengths(s) > 0]
  nms <- names(s)
  s <- lapply(s, paste, collapse = "\n")
  for (i in seq_along(s)) {
    cat("\n# File:", nms[i], fill = TRUE)
    txt <- s[[i]]
    txt <- gsub("\n", "\n", txt)
    cat(txt, fill = TRUE)
  }
}

extra_text_search <- function(file, pat, ignore.case = TRUE) {
  x <- tryCatch(
    readlines(file),
    error = function(e) NULL,
    warning = function(w) NULL
  )
  if (is.null(x)) {
    return(invisible())
  }
  x <- sub("^  ", "", x)
  term <- pat
  pat <- paste0("\\b.{0,32}", pat, ".{0,32}\\b")
  if (!any(grepl(pat, x, ignore.case = ignore.case))) {
    return(invisible())
  }
  l <- gregexpr_(x, pat, ignore.case = ignore.case)
  l <- which(sapply(l, function(.x) .x[1] > 0))
  spaces <- max(nchar(l))
  spaces <- spaces - nchar(l)
  ss <- character(length(spaces))
  for (i in seq_along(spaces)) {
    if (spaces[i] > 0) {
      ss[i] <- paste0(rep(" ", spaces[i]), collapse = "")
    } else {
      ss[i] <- ""
    }
  }
  l <- paste0(ss, l)
  m <- regmatches_(x, pat, drop = TRUE, ignore.case = ignore.case)
  m <- paste0(paste0("[", l, "] "), m)
  m <- paste0(m, collapse = "\n")
  m <- gsub("( \\{\\{\\{(?!\\{))|((?<!\\})\\}\\}\\} )", "", m, perl = TRUE)
  term <- regmatches_(m, term, drop = TRUE, ignore.case = ignore.case)
  m <- paste0("\033[38;5;242m", m, "\033[39m")
  for (.x in term) {
    m <- gsub(
      .x,
      paste0("\033[31m", .x, "\033[38;5;242m"),
      m,
      ignore.case = ignore.case
    )
  }
  paste0("\033[38;5;242m", m, "\033[39m")
}
