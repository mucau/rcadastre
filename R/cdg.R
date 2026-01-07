### URL section ----
#' Get the Base Data URL for cadastre.data.gouv
#'
#' This function returns the base URL for a given cadastre.data.gouv site.
#'
#' @param site `character`. The cadastre site to use.
#'    Must be one of `"pci"` or `"etalab"`.
#'
#' @return A single character string representing the base URL for the requested site.
#'
#' @details
#' The function maps the site names "pci" and "etalab" to their corresponding
#' dataset directories on the cadastre.data.gouv portal:
#'
#' - "pci" -> "dgfip-pci-vecteur"
#' - "etalab" -> "etalab-cadastre"
#'
#' It then returns the complete URL starting with "https://cadastre.data.gouv.fr/data/".
#'
#' @examples
#' \dontrun{
#' get_base_data_url("pci")
#' # Returns: "https://cadastre.data.gouv.fr/data/dgfip-pci-vecteur"
#'
#' get_base_data_url("etalab")
#' # Returns: "https://cadastre.data.gouv.fr/data/etalab-cadastre"
#' }
#'
#' @keywords internal
get_base_data_url <- function(site) {
  site <- tryCatch(
    match.arg(site, c("pci", "etalab")),
    error = function(e) stop("site must be one of 'pci' or 'etalab'", call. = FALSE)
  )

  mapping <- c(
    pci    = "dgfip-pci-vecteur",
    etalab = "etalab-cadastre"
  )

  sprintf("https://cadastre.data.gouv.fr/data/%s", mapping[site])
}

#' Construct a commune path string
#'
#' Constructs a path string for a given commune (or sheet) code by adding
#' the department code (first two characters, or three for 97x)
#' as a prefix joined with a slash.
#'
#' @param id `character` vector. Validated INSEE code(s) of the commune(s).
#'
#' @return A `character` vector combining department and commune codes separated by a slash.
#'
#' @examples
#' \dontrun{
#' construct_commune("72187")
#' # Returns: "72/72187"
#' construct_commune(c("72187", "75056"))
#' # Returns: c("72/72187", "75/75056")
#' }
#'
#' @keywords internal
construct_commune <- function(id) {
  # Assumes id codes are already validated
  dep <- substr(id, 1, ifelse(substr(id, 1, 2) == "97", 3, 2))
  commune <- substr(id, 1, 5)
  file.path(dep, commune)
}

#' Construct the full data URL for cadastre.data.gouv
#'
#' This function builds the complete URL(s) to access cadastral data hosted on
#' \url{https://cadastre.data.gouv.fr}, for a given site and one or more INSEE
#' identifiers. It automatically detects the appropriate cadastral scale
#' (communes, feuilles, or departements) from the provided identifiers and
#' applies site-specific constraints.
#'
#' @param id `character` vector. One or more INSEE identifiers.
#'   Can be commune codes, cadastral sheet identifiers, or department codes,
#'   depending on the selected site.
#'
#' @param site `character`. The cadastre.data.gouv site to use.
#'   Must be one of `"pci"` or `"etalab"`.
#'
#' @param millesime `character`. The version of the dataset.
#'   Must be one of `get_data_millesimes(site)`.
#'   Default is `"latest"`.
#'
#' @param format `character`. Optional data format.
#'   \itemize{
#'     \item For `"pci"`, must be `"edigeo"` or `"dxf"` (default: `"edigeo"`).
#'     \item For `"etalab"`, the format is always `"geojson"` and this argument
#'     is ignored.
#'   }
#'
#' @return A `character` vector of fully qualified URLs corresponding to the
#'   requested identifiers, site, scale, and dataset version.
#'
#' @details
#' The function constructs download URLs based on the selected
#' cadastre.data.gouv pipeline and the cadastral scale inferred from the
#' provided INSEE identifiers.
#'
#' Supported pipelines and scales are:
#' \itemize{
#'   \item \strong{PCI}: commune or cadastral sheet (feuille) scale
#'   \item \strong{Etalab}: commune or department scale
#' }
#'
#' INSEE identifiers are validated, the appropriate scale is inferred,
#' site-specific constraints are applied, and the final URL(s) are
#' constructed accordingly.
#'
#' @seealso [get_data_millesimes()], [get_insee_scale()]
#'
#' @examples
#' \dontrun{
#' # PCI cadastral sheets for a commune
#' get_data_url(id = "72187", site = "pci")
#' # -> https://cadastre.data.gouv.fr/data/dgfip-pci-vecteur/latest/edigeo/feuilles/72/72187
#'
#' # Etalab cadastral data at commune scale
#' get_data_url(id = "72187", site = "etalab")
#' # -> https://cadastre.data.gouv.fr/data/etalab-cadastre/latest/geojson/communes/72/72187
#'
#' # Etalab cadastral data at department scale
#' get_data_url(id = "72", site = "etalab")
#' }
#'
#' @keywords internal
get_data_url <- function(id,
                         site,
                         millesime = "latest",
                         format = NULL) {

  # Site check
  site <- match.arg(site, c("pci", "etalab"))

  # ID check
  valid <- check_insee(id, verbose = FALSE)
  if (!all(valid)) {
    stop("Some INSEE codes are invalid or correspond to mother communes.",
         call. = FALSE)
  }

  # Data version
  millesime <- match.arg(millesime, get_data_millesimes(site))

  # Scale detection
  scale <- get_insee_scale(id)

  # Forbidden combinations
  if (site == "pci" && any(scale == "departements")) {
    stop(
      paste0(
        "frcadastre doesn't handle the departmental scale for PCI.\n",
        "Try: https://cadastre.data.gouv.fr/data/dgfip-pci-vecteur/latest/",
        "edigeo/departements/"
      ),
      call. = FALSE
    )
  }
  if (site == "etalab" && any(scale == "feuilles")) {
    stop("Sheet scale not availabe for Etalab", call. = FALSE)
  }

  # For PCI, replace "communes" by "feuilles"
  if (site == "pci") {
    scale[scale == "communes"] <- "feuilles"
  }

  # Format
  format <- switch(
    site,
    pci = {
      if (is.null(format)) format <- "edigeo"
      match.arg(format, c("edigeo", "dxf"))
    },
    etalab = "geojson"
  )

  # Data path
  data_path <- ifelse(
    scale == "departements",
    id,
    construct_commune(id)
  )

  # Base URL
  base <- get_base_data_url(site)

  # Final URL
  file.path(base, millesime, format, scale, data_path)
}

### Milesime section ----
#' Detect and Extract URLs from a Web Page
#'
#' This function scans one or more web pages for hyperlinks (`<a href=...>`)
#' and extracts their URLs, either as absolute or relative paths.
#'
#' @param url `character`. One or more base URLs to scan.
#' @param absolute `logical`. Default is `"TRUE"`.
#'    If `TRUE` (default), returned links are converted to absolute URLs.
#'    If `FALSE`, relative paths are preserved.
#'
#' @return A character vector of detected URLs.
#'
#' @details
#' All `<a href>` attributes found in the HTML content of the provided `urls`
#' are returned, excluding parent directory links (`"../"`) and empty values.
#' If `absolute = TRUE`, URLs are normalized relative to the input base.
#'
#' @examples
#' \dontrun{
#' links <- detect_urls(get_data_url("72187", "etalab"))
#' print(links)
#' }
#'
#' @importFrom httr2 request req_perform resp_body_html
#' @importFrom xml2 xml_find_all xml_attr url_absolute
#'
#' @keywords internal
detect_urls <- function(url, absolute = TRUE) {
  detect_one <- function(url) {
    page <- request(url) |>
      req_perform() |>
      resp_body_html()

    links <- xml_find_all(page, ".//a[@href]") |> xml_attr("href")
    links <- links[!is.na(links) & links != "../" & nzchar(links)]

    if (absolute) {
      # Ensure the base ends with the commune folder
      base <- paste0(url, "/")  # add trailing slash
      links <- url_absolute(links, base = base)
    } else {
      links <- sub("/$", "", links)
    }

    unique(links)
  }

  unlist(lapply(url, detect_one), use.names = FALSE)
}

#' Detect available cadastral version
#'
#' Retrieves and returns the list of available cadastral version directories
#' from a specified data site.
#'
#' @param site `character`. The cadastre site to use.
#'    Must be one of `"pci"` or `"etalab"`.
#'
#' @return A `character` vector of unique cadastral version found on the site.
#'
#' @details
#' The function queries the base data URL for the specified site and extracts
#' the list of available cadastral version directories.
#'
#' @examples
#' \dontrun{
#' years <- list(pci = get_data_millesimes("pci"), etalab = get_data_millesimes("etalab"))
#' print(years)
#' }
#'
#' @export
get_data_millesimes <- function(site) {
  site <- match.arg(site, c("pci", "etalab"))
  detect_urls(get_base_data_url(site), FALSE)
}

### INSEE code section ----
#' Detect and validate INSEE code (city or department)
#'
#' Checks if the provided INSEE code(s) are valid among communes or departments.
#' Optionally returns the administrative scale ("communes" or "departments")
#' for each code.
#'
#' @param x `character` or `numeric`. Vector of INSEE codes to validate.
#' @param verbose `logical`. If `TRUE`, prints informative messages for each code.
#'
#' @return A logical vector of the same length as `x`.
#'   Each element is `TRUE` if the corresponding INSEE code is valid (commune or department),
#'   and `FALSE` otherwise. Mother communes (Paris, Lyon, Marseille) are considered invalid.
#'
#' @details
#' The function accepts either 5-character INSEE codes for communes or 2-3
#' character codes for departments. Invalid codes trigger an error.
#'
#' @examples
#' \dontrun{
#' # Validate a single commune
#' check_insee(72187)
#' # Returns: Commune '72187' = 'Marigné-Laillé' selected
#' check_insee(72187, TRUE)
#' # Returns: "communes"
#'
#' # Validate multiple codes and return scale
#' check_insee(c(72, 72187, 72187))
#' # Returns:
#' # Department '72' = 'Sarthe' selected
#' # Commune '72187' = 'Marigné-Laillé' selected
#' # Commune '72187' = 'Marigné-Laillé' selected
#' }
#'
#' @export
check_insee <- function(x, verbose = TRUE) {
  x <- as.character(x)

  # Reference tables
  communes <- frcadastre::commune_2025$COM
  departments <- frcadastre::departement_2025$DEP

  # Mother communes
  arr_to_check <- c(paris = "75056", lyon = "69123", marseille = "13055")
  arr_prefix <- c(paris = "751", lyon = "693", marseille = "132")

  # Identify mother communes
  is_mother <- x %in% arr_to_check
  if (any(is_mother) && verbose) {
    msgs <- vapply(x[is_mother], function(code) {
      ville <- names(arr_to_check)[match(code, arr_to_check)]
      valid_arr <- frcadastre::commune_2025[
        frcadastre::commune_2025$TYPECOM == "ARM" &
          startsWith(as.character(frcadastre::commune_2025$COM), arr_prefix[ville]),
      ]
      sprintf(
        "mother commune: %s (%s). Use one of: %s",
        code, ville, paste(valid_arr$COM, collapse = ", ")
      )
    }, FUN.VALUE = character(1))
    warning(paste(msgs, collapse = "\n"), call. = FALSE)
  }

  # Generic validity check
  is_valid <- (nchar(x) == 5 & x %in% communes) |
    (nchar(x) %in% c(2,3) & x %in% departments) |
    (nchar(x) == 12 & substr(x, 1, 5) %in% communes)

  # Mother communes are considered invalid
  is_valid[is_mother] <- FALSE

  # Warn about invalid codes (excluding mother communes)
  invalid_generic <- x[!is_valid & !is_mother]
  if (length(invalid_generic) > 0 && verbose) {
    warning("Invalid INSEE code(s): ", paste(invalid_generic, collapse = ", "), call. = FALSE)
  }

  return(is_valid)
}

#' Get administrative scale of INSEE codes
#'
#' Returns administrative scale for each INSEE code.
#'
#' @param x `character` or `numeric`. Vector of INSEE codes.
#'
#' @return Character vector of length `length(x)` with values "communes",
#' "departements" or "feuilles".
#'
#' @examples
#' \dontrun{
#' get_insee_scale(72187)        # "communes"
#' get_insee_scale(c(72, 72187)) # c("departements", "communes")
#' get_insee_scale(c(72, 72187, "721870000A01")) # c("departements", "communes", "feuilles")
#' }
#'
#' @keywords internal
get_insee_scale <- function(x) {
  x <- as.character(x)
  communes <- frcadastre::commune_2025$COM
  departments <- frcadastre::departement_2025$DEP

  scales <- character(length(x))
  scales[nchar(x) == 5 & x %in% communes] <- "communes"
  scales[nchar(x) %in% c(2,3) & x %in% departments] <- "departements"
  scales[nchar(x) == 12 & substr(x, 1, 5) %in% communes] <- "feuilles"

  if (any(scales == "")) {
    stop("Cannot determine scale for code(s): ", paste(x[scales==""], collapse=", "))
  }

  scales
}
