

test_that("dimension extraction", {
  meta <- list(
    "title" = "test",
    "dimensions" = list(
      "variable" = c(
        "avariable",
        "bvariable"
      ),
      "column" = c(
        "a_variable",
        "b_variable"
      ),
      "label" = c(
        "A Variable",
        "B Variable"
      )
    )
  )
  expected <- list(
    "A Variable" = "a_variable",
    "B Variable" = "b_variable"
  )
  result <- transfer.server::get_dimensions(meta)
  expect_equal(expected, result)
})
