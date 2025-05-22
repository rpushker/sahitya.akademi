test_that("Get Sahitya Akademi winners", {
  language_df <- expect_type(get_sahitya_akademi_winners("English"), "data.frame")
  expect_equal(names(language_df), c("Year", "Author", "Book", "Category"))
})
