test_that("get_hosted_packages works", {
  pkgs <- get_hosted_packages()
  expect_true(length(pkgs) > 100)
  expect_snapshot(unique(lapply(pkgs, names)))
})

test_that("get_other_packages works", {
  pkgs <- get_other_packages()
  expect_true(length(pkgs) > 3)
  expect_equal(unique(unlist(lapply(pkgs, names))), c("package", "url", "subdir"))
})

test_that("categories.json is well-formed", {
  # read raw to catch duplicated keys, which read_json() would keep
  categories <- jsonlite::read_json(
    system.file("info", "categories.json", package = "roregistry")
  )
  expect_true(length(categories) > 100)
  expect_equal(anyDuplicated(names(categories)), 0)
  expect_all_true(purrr::map_lgl(categories, is.list))
  expect_all_true(
    purrr::map_lgl(unlist(categories, recursive = FALSE), is.character)
  )
  expect_equal(names(categories), sort(names(categories), method = "radix"))
})

test_that("add_categories writes categories as an array", {
  packages <- list(
    list(package = "one", url = "u1", metadata = list(review = list(id = 1))),
    list(package = "two", url = "u2"),
    list(package = "none", url = "u3"),
    list(package = "empty", url = "u4")
  )
  categories <- list(
    one = "stat",
    two = c("data-access", "geospatial"),
    empty = character()
  )
  out <- add_categories(packages, categories)

  expect_equal(out[[1]]$metadata$review, list(id = 1))
  expect_equal(out[[1]]$metadata$ropensci_category, "stat")
  expect_equal(out[[2]]$metadata$ropensci_category, "data-access")
  expect_null(out[[3]]$metadata)
  expect_null(out[[4]]$metadata)

  json <- jsonlite::toJSON(out, auto_unbox = TRUE)
  expect_match(json, '"ropensci_categories":["stat"]', fixed = TRUE)
  expect_match(
    json,
    '"ropensci_categories":["data-access","geospatial"]',
    fixed = TRUE
  )
})
