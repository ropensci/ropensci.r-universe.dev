is_package <- function(path, old_cm){

  # Package structure?
  structure <- all(c("DESCRIPTION", "NAMESPACE", "man", "R") %in%
        dir(path))

  if (structure) {
    return(TRUE) # package if it has the right structure
    } else {
    if (!is.null(old_cm)) {
    if (
      length(old_cm[purrr::map_chr(old_cm, "identifier") ==
      basename(path)]) > 0
    ) {
      return(TRUE) # or if it is already in the registry
      }
    }

    }
  return(FALSE) # not a package in other cases (no old registry, or no entry in it)
}
