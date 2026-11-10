# Sync inst/info/categories.json with the current packages.json:
# drop packages that are gone, add new ones with no categories,
# then fill in the empty ones by hand.
categories_file <- file.path("inst", "info", "categories.json")

categories <- jsonlite::read_json(categories_file)
packages <- purrr::map_chr(jsonlite::read_json("packages.json"), "package")

new_packages <- setdiff(packages, names(categories))
gone_packages <- setdiff(names(categories), packages)

categories <- categories[intersect(names(categories), packages)]
categories[new_packages] <- list(list())
categories <- categories[order(names(categories), method = "radix")]

# I() keeps single categories as JSON arrays
categories <- lapply(categories, function(x) I(as.character(unlist(x))))
jsonlite::write_json(categories, categories_file, pretty = TRUE)

message("Removed: ", toString(gone_packages))
message("Added: ", toString(new_packages))
message(
  "Without categories (to fill in by hand): ",
  toString(names(categories)[lengths(categories) == 0])
)
