context("Testing tfse")
# https://rdrr.io/cran/tfse/src/tests/testthat/test-tfse.R

test_that("tfse.search_files", {
  t <- capture.output(search_files('string_to_find', path = test_path()))
  expect_true(is.character(t))
  o <- capture.output(search_files("DESCRIPTION", path = "../.."))
  expect_true(is.character(o))
})

test_that("lines", {
  txt <- paste(
    sample(c(rep(" ", 10), letters), 100, replace = TRUE),
    collapse = ""
  )
  tmp <- tempfile()
  cat(txt, file = tmp, fill = TRUE)
  x <- readlines(tmp)
  unlink(tmp)
  expect_identical(txt, x)
})


test_that("regmatches_", {
  m <- gregexpr_(letters, "a")
  expect_equal(sum(sapply(m, function(.x) .x > 0)), 1)
  o <- regmatches_(letters, "a")
  expect_identical(as.list(c("a", rep("", 25))), o)
  o <- regmatches_(letters, "a", drop = TRUE)
  expect_identical(o, "a")
  o <- regmatches_(as.list(letters), "a", drop = TRUE)
  expect_identical(o, as.list(c("a", rep(list(character()), 25))))
  o <- regmatches_(as.list(letters), "a", drop = FALSE)
  expect_identical(o, as.list(c(list(list("a")), rep(list(list("")), 25))))
})

test_that("regmatches lists", {
  x <- lapply(as.list(letters), factor)
  expect_true(is.list(regmatches_(x, "a")))
  expect_true(is.list(regmatches_first(x, "a")))
})
