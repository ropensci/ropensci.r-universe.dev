# current categories information
categories <- readr::read_csv(
  here::here(
    file.path(
      "info",
      "final_categories.csv"
    )
  )
)

# current packages
pkgs <- jsonlite::read_json("packages.json")

packages <- purrr::map_chr(pkgs, "package")
pkgs <- pkgs[order(packages)]
packages <- sort(packages)
ropenscilabs <- grepl("ropenscilabs", purrr::map_chr(pkgs, "url"))

categories <- dplyr::left_join(
  tibble::tibble(name = packages, ropenscilabs = ropenscilabs),
  categories
)
categories <- dplyr::select(categories, name, ropensci_category, ropenscilabs)
# update csv by hand and save it

# only keep categories information
# for current packages
categories <- dplyr::filter(categories, name %in% packages)
readr::write_csv(
  categories,
  here::here(
    file.path(
      "info",
      "final_categories.csv"
    )
  )
)
