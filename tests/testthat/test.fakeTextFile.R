context("Testing fakeTextFile")

test_that("fakeTextFile default parameters work", {
  list2env(fakeTextFile(), envir = globalenv())
  created_object <- readLines(tmp_path)
  expect_equal(created_object, txt)
  expect_equal(tail(unlist(strsplit(tmp_path, '\\.')), 1), 'R')
})

test_that("fakeTextFile custom parameters work", {
  dir.create(path = paste0(tempdir(), '/test_folder'))
  list2env(
    fakeTextFile(
      txt = 'test',
      file_extension = '.txt',
      dir_path = paste0(tempdir(), '/test_folder')
    ),
    envir = globalenv()
  )
  created_object <- readLines(tmp_path)
  expect_equal(created_object, 'test')
  expect_equal(created_object, txt)
  expect_equal(tail(unlist(strsplit(tmp_path, '\\.')), 1), 'txt')
})

test_that("fakeTextFile fails well", {
  expect_error(fakeTextFile(file_extension = 0))
  expect_error(fakeTextFile(dir_path = 3))
  expect_error(fakeTextFile(txt = 3))
  expect_error(fakeTextFile(n_lines = 0))
})

# test order of importance between dir_path and file_path
