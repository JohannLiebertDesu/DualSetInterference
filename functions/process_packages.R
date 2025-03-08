#' Extract Package Name from Package Identifier
#'
#' Parses a package identifier and extracts the actual package name.
#' Works with both CRAN packages and GitHub packages (with or without version).
#'
#' @param pkg_id Character string. Package identifier (e.g., "dplyr" for CRAN,
#'               "username/reponame" or "username/reponame@v0.1.1" for GitHub)
#' @return Character string. The extracted package name.
extract_package_name <- function(pkg_id) {
  if (!grepl("/", pkg_id)) {
    # CRAN package
    return(pkg_id)
  }
  
  # GitHub package - extract base name
  pkg_parts <- strsplit(pkg_id, "@")[[1]][1]
  basename(pkg_parts)
}

#' Install Package If Not Already Installed
#'
#' Checks if a package is installed and installs it if needed.
#' Handles both CRAN and GitHub packages appropriately.
#'
#' @param pkg_id Character string. Package identifier (e.g., "dplyr" for CRAN,
#'               "username/reponame" or "username/reponame@v0.1.1" for GitHub)
#' @return Logical. TRUE if package is (now) installed, FALSE otherwise.
install_if_needed <- function(pkg_id) {
  pkg_name <- extract_package_name(pkg_id)
  
  if (requireNamespace(pkg_name, quietly = TRUE)) {
    return(TRUE)  # Package already installed
  }
  
  # Install based on source
  if (grepl("/", pkg_id)) {
    # GitHub package
    if (!requireNamespace("remotes", quietly = TRUE)) {
      install.packages("remotes")
    }
    remotes::install_github(pkg_id)
  } else {
    # CRAN package
    install.packages(pkg_id)
  }
  
  return(requireNamespace(pkg_name, quietly = TRUE))
}

#' Process Multiple Packages
#'
#' Processes a vector of package identifiers - installing and loading each package.
#' Tracks the status of each operation and returns detailed results.
#'
#' @param packages Character vector. Package identifiers to process.
#' @return Tibble with columns for package information and status.
process_packages <- function(packages, quietly = FALSE) {
  # First, ensure magrittr is available for the pipe operator
  if (!requireNamespace("magrittr", quietly = TRUE)) {
    install.packages("magrittr")
  }
  
  # Load the pipe operator
  if (!exists("%>%")) {
    `%>%` <- magrittr::`%>%`
  }
  
  # Also ensure tibble is available
  if (!requireNamespace("tibble", quietly = TRUE)) {
    install.packages("tibble")
  }
  
  # Create a tibble to track package information
  pkg_status <- tibble::tibble(
    package_id = packages,
    package_name = vapply(packages, extract_package_name, character(1)),
    installed = FALSE,
    loaded = FALSE,
    message = character(length(packages))
  )
  
  # Install packages if needed (using base R instead of dplyr)
  pkg_status$installed <- vapply(pkg_status$package_id, install_if_needed, logical(1))
  
  # Load packages and generate messages
  for (i in seq_len(nrow(pkg_status))) {
    pkg_name <- pkg_status$package_name[i]
    
    tryCatch({
      library(pkg_name, character.only = TRUE)
      pkg_status$loaded[i] <- TRUE
      pkg_status$message[i] <- paste(pkg_name, "loaded successfully")
    }, error = function(e) {
      pkg_status$message[i] <- paste("Error loading", pkg_name, ":", e$message)
    })
  }
  
  if (!quietly) return(pkg_status)
}