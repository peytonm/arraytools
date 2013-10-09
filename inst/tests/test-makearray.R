context("Array Creation")

test_that("Arrays are created with proper dimension lengths", {
  expect_equivalent(dim(makearray(1:600, 10, 3, 20)), c(10, 3, 20))
  expect_equivalent(dim(makearray(1:600, -1, 3, 20)), c(10, 3, 20))
  expect_equivalent(dim(makearray(1:600, 10, -1, 20)), c(10, 3, 20))
  expect_equivalent(dim(makearray(1:600, 10, 3, -1)), c(10, 3, 20))
  expect_equivalent(dim(makearray(1:600, 1:10, LETTERS[1:3], 20)), c(10, 3, 20))
})

test_that("Ambiguous dimensions produce an error", {
  expect_error(makearray(1:50, 5, -1, -1))
})

test_that("Inconsistent dimensions produce an error", {
  expect_error(makearray(1:10, 3, 50, 100))
})

test_that("Arrays are created with proper dimension names", {
  x <- makearray(seq_len(length(LETTERS) * length(month.name) * 5),
                 letter=LETTERS, month=month.name, i=1:5)
  expect_equivalent(dimnames(x), list(letter=LETTERS, month=month.name, i=as.character(1:5)))
  
  y <- makearray(seq_len(length(LETTERS) * length(month.name) * 5),
                 letters=LETTERS, month=month.name, 5)
  expect_equivalent(dimnames(y), list(letter=LETTERS, month=month.name, NULL))
})