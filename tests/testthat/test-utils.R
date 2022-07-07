context("utilities")

test_that("compact works",{
  expect_equal(
    compact(list("a",NULL,"b")),
    list("a","b")
  )
})
