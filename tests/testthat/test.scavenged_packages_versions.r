context("Testing that code I took from others has not been updated on CRAN")

test_that("tfse", {
  expect_equal(rvcheck::check_cran('tfse')$latest_version, '0.5.0')
})


test_that("Dependencies packages are up to date.", {
  expect_true(rvcheck::check_cran('rvcheck')$up_to_date)
})
