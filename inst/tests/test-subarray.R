context("Subsetting")

test_that("subarray correctly subsets", {
  x <- array(seq_len(12*26*10), dim=c(12, 26, 10),
             dimnames=list(month=month.name, letter=letters, num=1:10))
  expect_equivalent(subarray(x, month=c("January", "May"), letter=letters[2:8], num=10),
                    x[c("January", "May"), letters[2:8], 10, drop=FALSE])
})