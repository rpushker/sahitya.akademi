test_that("Is Sahity Akademi language", {
  expect_true(is_sahitya_akademi_language("Hindi"))
  expect_true(is_sahitya_akademi_language("English"))
  expect_false(is_sahitya_akademi_language("Spanish"))
})