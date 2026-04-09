rOpenSci Package Registry
=========================

## What is this

This repository contains 2 files that define the official rOpenSci package suite:
our [registry for R-universe](packages.json): the official list of rOpenSci packages, identified by the package name and git url (updated hourly).

The rOpenSci package suite consists of all R packages in the [ropensci](https://github.com/ropensci) and [ropenscilabs](https://github.com/ropenscilabs) GitHub organizations, except for packages listed in [exclude list](info/exclude_list.txt), plus some extra packages listed in [not_transferred.json](info/not_transferred.json). 

The CI automatically updates the [packages.json](packages.json) file using the [makeregistry](https://github.com/ropensci-org/makeregistry) package.


## Generating packages.json

The code to re-generate packages.json is in the [makeregistry](https://github.com/ropensci-org/makeregistry) package. The `build_ropensci_packages_json()` function works as follows:

 1. It queries the GitHub API for all repositories in `ropensci` and `ropenscilabs`.
 2. It removes entries from the [exclude list](info/exclude_list.txt) 
 3. It adds packages listed in [not_transferred.json](info/not_transferred.json)
 4. Saves the final list in `packages.json`

This function should take less then a minute to complete, be very reliable, and we run it frequently.

## More metadata on rOpenSci packages

For this we now use the [R-universe API](https://docs.r-universe.dev/browse/api.html).