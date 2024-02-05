test_that("round2 function works correctly", {
  # Example datasets
  vec1 <- 1:3 / 3
  vec2 <- c(NA, 10, 100.001, 333.0001254, 12312.1235)
  vec3 <- c(NA, "10", "100.001", "333.0001254", "12312.1235")

  # Test cases
  test_that("rounds data correctly", {
    expect_equal(round2(data = vec1), c(0.3333333, 0.6666667, 1))
    expect_equal(round2(data = vec2), c(NA, 10, 100.001, 333.0001254, 12312.1235))
    expect_equal(round2(data = vec3), c(NA, 10, 100.001, 333.0001254, 12312.1235))
  })

  # Test argument dec.nb
  test_that("handles dec.nb argument correctly", {
    expect_equal(round2(data = vec1, dec.nb = 2), c(0.33, 0.67, 1))
    expect_equal(round2(data = vec1, dec.nb = 3), c(0.333, 0.667, 1))
    expect_equal(round2(data = vec2, dec.nb = 2), c(NA, 10, 100.00, 333.00, 12312.12))
    expect_equal(round2(data = vec2, dec.nb = 3), c(NA, 10, 100.001, 333.000, 12312.124))
    expect_equal(round2(data = vec3, dec.nb = 2), c(NA, 10, 100.00, 333.00, 12312.12))
    expect_equal(round2(data = vec3, dec.nb = 3), c(NA, 10, 100.001, 333.000, 12312.124))
  })

  # Test argument after.lead.zero
  test_that("handles after.lead.zero argument correctly", {
    expect_equal(round2(data = vec2, after.lead.zero = TRUE), c(NA, 10.00, 100.00, 333.00, 12312.12))
    expect_equal(round2(data = vec2, after.lead.zero = FALSE), c(NA, 10, 100.001, 333.0001254, 12312.1235))
    expect_equal(round2(data = vec3, after.lead.zero = TRUE), c(NA, 10.00, 100.00, 333.00, 12312.12))
    expect_equal(round2(data = vec3, after.lead.zero = FALSE), c(NA, 10, 100.001, 333.0001254, 12312.1235))
  })

  # Test all arguments
  test_that("handles all arguments correctly", {
    ini_options <- options()$digits
    options(digits = 8)
    expect_equal(round2(data = vec2), c(NA, 10, 100.001, 333.0001254, 12312.1235))
    options(digits = ini_options)

    ini_options <- options()$digits
    options(digits = 8)
    expect_equal(round2(data = vec3, dec.nb = 2, after.lead.zero = FALSE), c(NA, 10, 100.00, 333.00, 12312.12))
    options(digits = ini_options)

    ini_options <- options()$digits
    options(digits = 8)
    expect_equal(round2(data = c(NA, "10", "100.001", "333.0001254", "12312.1235"), dec.nb = 2, after.lead.zero = TRUE), c(NA, 10.00, 100.00, 333.00, 12312.12))
    options(digits = ini_options)
  })
})