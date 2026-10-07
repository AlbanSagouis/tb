#' Increment version number of a package
#'
#' Reads DESCRIPTION from installed package 'pkg' and makes a new one in the specified
#' 'folder'. Four options for 'increase' are "major", minor", "patch" and "dev"
#' respectively incrementing the first to fourth element of version 0.1.0.9012.
#' Also changes the date to system date.
#'
#' @param pkg A character vector with the name of the package to update.
#' @param folder Folder in which DESCRIPTION is saved. Can be left as '.'.
#' @param increase Character string, either 'major', 'minor', 'patch' or 'dev.'
#'
#' @return NULL
#' @details Function originally posted by alko989 here https://stackoverflow.com/q/24209336.
#' Hadley Wickham discusses when and how increment version numbers in
#' this book http://r-pkgs.had.co.nz/release.html#release-version
#'
#' @author alko989, Alban Sagouis
#'
#'
#' @export

incVer <- function(pkg, folder = ".", increase = "dev") {
  ## Read DESCRIPTION
  ### If Built is kept, package built does not work. If a vector with column names is
  ### provided to avoid reading twice, there is a risk to overlook info saved in
  ### DESCRIPTION
  dcf_colnames <- colnames(read.dcf(
    file = system.file("DESCRIPTION", package = pkg)
  ))
  dcf_colnames <- dcf_colnames[dcf_colnames != "Built"]
  f <- read.dcf(
    file = system.file("DESCRIPTION", package = pkg),
    fields = dcf_colnames
  )

  ## Increment version number
  curVer <- package_version(f[4])
  if (increase == "dev") {
    curVer[[1, 4]] <- ifelse(
      is.na(curVer[[1, 4]]) | curVer[[1, 4]] == 0,
      9001,
      curVer[[1, 4]] + 1
    )
  } else if (increase == "patch") {
    curVer[[1, 3]] <- ifelse(is.na(curVer$patchlevel), 1, curVer$patchlevel + 1)
    curVer[[1, 4]] <- 0
  } else if (increase == "minor") {
    curVer[[1, 2]] <- ifelse(is.na(curVer$minor), 1, curVer$minor + 1)
    curVer[[1, 3]] <- 0
    curVer[[1, 4]] <- 0
  } else if (increase == "major") {
    curVer[[1, 1]] <- ifelse(is.na(curVer$major), 1, curVer$major + 1)
    curVer[[1, 2]] <- 0
    curVer[[1, 3]] <- 0
    curVer[[1, 4]] <- 0
  } else {
    stop(paste("Can not identify the increase argument: ", increase))
  }

  f[4] <- toString(curVer)

  ## Update also the date
  f[5] <- format(Sys.time(), "%Y-%m-%d")

  ## Save
  write.dcf(f, file = paste(folder, "DESCRIPTION", sep = "/"))

  if (increase == 'major' | increase == 'minor' | increase == 'patch') {
    print(
      "Consider updating README.md and NEWS.md."
    )
  }
}
