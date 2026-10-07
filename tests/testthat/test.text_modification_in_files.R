context("Testing text_manipulation_in_scripts")

txt <- lapply(1:5, function(i) {
  paste0(
    'test ',
    paste(sample(c(rep(" ", 10), letters), 100, replace = TRUE), collapse = "")
  )
})
list2env(fakeTextFile(txt = txt), globalenv())

test_that("gsubInOneScript works", {
  gsubInOneScript(fullPath = tmp_path, 'test', 'experiment', userCheck = FALSE)
  changed_object <- readLines(tmp_path)
  expect_equal(length(changed_object), 5)
  expect_equal(nchar(changed_object[1]), 112)
  expect_true(grepl('experiment', changed_object[1]))
})

# Creating 10 random text files with multiple lines
testfiles <- lapply(1:10, function(i) tempfile(fileext = '.R'))
lapply(testfiles, function(testfilepath) {
  fakeTextFile(txt = txt, file_path = testfilepath)
})

test_that("gsubInOneFolder works", {
  gsubInOneFolder(fullPath = tempdir(), 'test', 'experiment', userCheck = FALSE)
  lapply(testfiles, function(tmp_path) {
    changed_object <- readLines(tmp_path)
    expect_equal(length(changed_object), 5)
    expect_equal(nchar(changed_object[1]), 112)
    expect_true(grepl('experiment', changed_object[1]))
  })
})
