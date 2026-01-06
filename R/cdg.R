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
#' Constructs a path string for a given commune code by adding the department
#' code (first two characters, or three for 97x) as a prefix joined with a slash.
#'
#' @param commune `character` vector. Validated INSEE code(s) of the commune(s).
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
construct_commune <- function(commune) {
  # Assumes commune codes are already validated
  dep <- substr(commune, 1, ifelse(substr(commune, 1, 2) == "97", 3, 2))
  file.path(dep, commune)
}

#' Construct the Full Data URL for cadastre.data.gouv
#'
#' This function builds the complete URL to access cadastral data for a
#' given cadastre.data.gouv site, commune, and cadastral version.
#' The function handles default formats and scales for each site.
#'
#' @param site `character`. The cadastre site to use.
#'    Must be one of `"pci"` or `"etalab"`.
#' @param commune `character` vector. The INSEE code(s) of the commune(s).
#' @param millesime `character`. The version of the dataset.
#'    Must be on of `get_data_millesimes("pci")`. Default is `"latest"`.
#' @param format `character`. Optional. The format of the data.
#'    For "pci", must be `"edigeo"` or `"dxf"`.
#'    For "etalab", the default is "geojson".
#'
#' @return A character vector of full URLs for the requested site, commune(s), and cadastral version.
#'
#' @details
#' The function validates the site and commune codes.
#' Default scales and formats are:
#'
#' - "pci": scale = "feuilles", format = "edigeo" or "dxf"
#' - "etalab": scale = "communes", format = "geojson"
#'
#' The returned URLs are constructed as:
#' \code{base_url / millesime / format / scale / commune}
#'
#' @seealso [get_data_millesimes()]
#'
#' @examples
#' \dontrun{
#' # PCI data for commune "72187"
#' construct_data_url("pci", 72187)
#' # Returns: "https://cadastre.data.gouv.fr/data/dgfip-pci-vecteur/latest/edigeo/feuilles/72/72187"
#'
#' # Etalab data for commune "72187"
#' construct_data_url("etalab", "72187")
#' # Returns: "https://cadastre.data.gouv.fr/data/etalab-cadastre/latest/geojson/communes/72/72187"
#' }
#'
#' @keywords internal
construct_data_url <- function(site,
                               commune,
                               millesime = "latest",
                               format = NULL) {

  # Validate site
  site <- match.arg(site, c("pci", "etalab"))

  # Validate commune codes (once)
  valid <- check_insee(commune, verbose = FALSE)
  if (!all(valid)) {
    stop("Some INSEE codes are invalid or correspond to mother communes.")
  }

  # Determine cadastral version
  millesime <- match.arg(millesime, get_data_millesimes("pci"))

  # Determine scale and format
  if (site == "pci") {
    scale <- "feuilles"
    if (is.null(format)) format <- "edigeo"
    format <- match.arg(format, c("edigeo", "dxf"))
  } else if (site == "etalab") {
    scale <- "communes"
    format <- "geojson"
  }

  # Base URL
  base <- get_base_data_url(site)

  # Commune path
  commune_paths <- construct_commune(commune)

  # Construct full URLs
  file.path(base, millesime, format, scale, commune_paths)
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
#' links <- detect_urls(construct_data_url("etalab", "72187"))
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
    (nchar(x) %in% c(2,3) & x %in% departments)

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
#' @return Character vector of length `length(x)` with values "communes" or "departements".
#'
#' @examples
#' \dontrun{
#' get_insee_scale(72187)        # "communes"
#' get_insee_scale(c(72, 72187)) # c("departements", "communes")
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

  if (any(scales == "")) {
    stop("Cannot determine scale for code(s): ", paste(x[scales==""], collapse=", "))
  }

  scales
}
