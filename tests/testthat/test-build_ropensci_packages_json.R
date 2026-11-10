test_that("get_hosted_packages works", {
  pkgs <- get_hosted_packages()
  expect_true(length(pkgs) > 100)
  expect_snapshot(unique(lapply(pkgs, names)))
})

test_that("get_other_packages works", {
  pkgs <- get_other_packages()
  expect_true(length(pkgs) > 3)
  expect_equal(unique(unlist(lapply(pkgs, names))), c("package", "url", "branch", "subdir"))
})
