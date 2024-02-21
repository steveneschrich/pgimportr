ig <- set_grant_table(iris)


test_that("is_grant_table() works", {
  expect_equal(is_grant_table(NULL), FALSE)
  expect_equal(is_grant_table(iris), FALSE)
  expect_equal(is_grant_table(iris$Sepal.Length), FALSE)
  expect_equal(is_grant_table(ig), TRUE)
})

ig <- set_publication_table(iris)

test_that("is_publication_table() works", {
  expect_equal(is_publication_table(NULL), FALSE)
  expect_equal(is_publication_table(iris), FALSE)
  expect_equal(is_publication_table(iris$Sepal.Length), FALSE)
  expect_equal(is_publication_table(ig), TRUE)
})
