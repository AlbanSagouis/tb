context("Testing executeOne and executeAll functions")

test_that('ExecuteOne', {
  if (file.exists(paste0(test_path(), '/objects/dt_test_success.csv'))) {
    file.remove(paste0(test_path(), '/objects/dt_test_success.csv'))
  }

  expect_invisible(executeOne(
    fullPath = paste0(test_path(), '/objects/data.table_test_script.r')
  ))

  expect(
    file.exists(paste0(test_path(), '/objects/dt_test_success.csv')),
    failure_message = 'executeOne did not succeed in creating the expected file.'
  )
})


test_that('ExecuteAll', {
  if (file.exists(paste0(test_path(), '/objects/dt_test_success.csv'))) {
    file.remove(paste0(test_path(), '/objects/dt_test_success.csv'))
  }

  expect_equal(
    length(executeAll(fullPath = paste0(test_path(), '/objects/'))),
    sum(grepl(list.files(paste0(test_path(), '/objects/')), pattern = '.r|.R'))
  )

  expect(
    file.exists(paste0(test_path(), '/objects/dt_test_success.csv')),
    failure_message = 'executeAll did not succeed in creating the expected file.'
  )
})
