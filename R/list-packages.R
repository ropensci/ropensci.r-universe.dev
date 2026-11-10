#' Build rOpenSci packages.json
#'
#' @param out_file where to save the JSON file
#'
#' @export
build_ropensci_packages_json <- function(out_file = "packages.json") {
  # packages from our organizations
  hosted_packages <- get_hosted_packages()

  # packages from elsewhere or with not standard repo structure
  other_packages <- get_other_packages()

  # merge all --------------------------------------------------------------------
  packages <- c(hosted_packages, other_packages)
  # radix = C-locale order, so the output does not depend on the runner's locale
  packages <- packages[
    order(purrr::map_chr(packages, "package"), method = "radix")
  ]

  # Verify new packages
  if (file.exists(out_file)) {
    previous <- jsonlite::read_json(out_file, simplifyVector = TRUE)
    message(sprintf(
      "Found %d packages (old packages.json had %d packages)",
      length(packages),
      nrow(previous)
    ))
    if (nrow(previous) - length(packages) > 15) {
      stop("This does not seem right")
    }
    verify_new_packages(previous, packages)
  }

  # Add peer-review metadata
  reviews <- get_reviewed_packages()
  packages <- lapply(packages, function(pkg) {
    review <- Find(function(x) identical(x$pkgname, pkg$package), reviews)
    if (length(review)) {
      pkg$metadata <- list(
        review = list(
          id = review$iss_no,
          status = review$status,
          version = review$version,
          organization = 'rOpenSci Software Review',
          url = sprintf(
            'https://github.com/ropensci/software-review/issues/%s',
            review$iss_no
          )
        )
      )
    }
    return(pkg)
  })

  # Add rOpenSci category
  packages <- lapply(packages, function(pkg) {
    categories <- ropensci_categories()
    category <- categories[["ropensci_category"]][
      categories[["name"]] == pkg[["package"]]
    ]
    if (length(category)) {
      if (!is.null(pkg$metadata)) {
        pkg$metadata <- c(
          pkg$metadata,
          ropensci_category = category
      )
      } else {
        pkg$metadata <- list(ropensci_category = category)
      }
    }
    return(pkg)
  })

  jsonlite::write_json(
    packages,
    out_file,
    auto_unbox = TRUE,
    pretty = TRUE
  )
}

verify_new_packages <- function(previous, packages) {
  new_packages <- Filter(
    function(x) {
      isFALSE(x$package %in% previous$package)
    },
    packages
  )
  lapply(new_packages, function(pkg) {
    message("New package: ", pkg$package)
    descurl <- paste0(sub("\\.git$", "", pkg$url), '/raw/HEAD/DESCRIPTION')
    req <- curl::curl_fetch_memory(descurl)
    if (req$status_code == 200) {
      message("Found DESCRIPTION in expected URL!")
    } else {
      stop(sprintf(
        'Failed to get DESCRIPTION (HTTP %d) %s',
        req$status_code,
        descurl
      ))
    }
  })
}

get_hosted_packages <- function() {
  github_organizations <- c("ropensci", "ropenscilabs")

  excludes <- readLines(info_file("exclude_list.txt"))

  list_organization_repos <- function(github_organization, excludes) {
    repos <- gh::gh(
      "/orgs/{org}/repos",
      org = github_organization,
      .limit = Inf
    )
    repos <- repos[!purrr::map_lgl(repos, "fork")]
    repos <- repos[!purrr::map_lgl(repos, "private")]
    repos <- repos[!purrr::map_lgl(repos, "archived")]
    repos <- repos[!(purrr::map_chr(repos, "name") %in% excludes)]

    purrr::map(
      repos,
      function(repo) {
        list(
          package = repo[["name"]],
          url = repo[["html_url"]]
        )
      }
    )
  }

  github_organizations |>
    purrr::map(list_organization_repos, excludes = excludes) |>
    unlist(recursive = FALSE)
}

get_other_packages <- function() {
  jsonlite::read_json(info_file("not_transferred.json"))
}

get_reviewed_packages <- function() {
  jsonlite::read_json('https://badges.ropensci.org/json/onboarded.json')
}

.ropensci_categories <- function() {
  readr::read_csv(
    info_file("final_categories.csv"),
    show_col_types = FALSE
  )
}

#' @importFrom memoise memoise
ropensci_categories <- memoise::memoise(.ropensci_categories)

# hand-maintained files in inst/info
info_file <- function(name) {
  system.file("info", name, package = "roregistry", mustWork = TRUE)
}
