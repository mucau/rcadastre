### Feuilles section ----
#' Retrieve PCI cadastral sheet URLs for given city codes
#'
#' Returns the URLs of PCI sheets (feuilles) for a given commune.
#' Can return either absolute URLs or relative sheet identifiers.
#'
#' @param commune `character` vector. The INSEE code(s) of the commune(s).
#' @param millesime `character`. The cadastral version of the dataset.
#'    Must be on of `get_data_millesimes("pci")`. Default is `"latest"`.
#' @param format `character`. The format of the data.
#'    Must be `"edigeo"` or `"dxf"`. Default is `"edigeo"`.
#' @param absolute `logical`. Default is `"TRUE"`.
#'    If `TRUE` (default), returned links are converted to absolute URLs.
#'    If `FALSE`, relative paths are preserved.
#'
#' @return A character vector of URLs (or relative sheet identifiers if `absolute = FALSE`).
#'
#' @details
#' PCI (computerized cadastral plan) sheets are organized per commune. This function
#' constructs the URLs to access all available sheets for the requested communes.
#' When `absolute = FALSE`, the returned sheet identifiers are cleaned by removing
#' the format prefix (`edigeo` or `dxf`) and the `.tar.bz2` file extension.
#'
#' @seealso [get_data_millesimes()]
#'
#' @examples
#' \dontrun{
#' # Retrieve absolute URLs for a single commune
#' get_pci_feuille("72187")
#'
#' # Retrieve relative sheet names
#' get_pci_feuille("72187", absolute = FALSE)
#' }
#'
#' @export
#'
get_pci_feuille <- function(commune,
                            millesime = "latest",
                            format = "edigeo",
                            absolute = TRUE) {

  # Build URLs and detect available sheets
  links <- detect_urls(
    construct_data_url(site = "pci",
                       commune = commune,
                       millesime = millesime,
                       format = format),
    absolute
  )

  # Clean names for relative output
  if (!absolute) {
    links <- sub("^(edigeo|dxf)-(.*)\\.tar\\.bz2$", "\\2", links)
  }

  links
}

#' Generate PCI sheet URLs from Commune or Sheet codes
#'
#' This internal function generates PCI (computerized cadastral plan) URLs
#' given a vector of commune codes (5-character) or individual sheet codes
#' (12-character).
#'
#' @param x `character`. Vector of codes to retrieve URLs for.
#'    Each element must be either a 5-character commune code or a 12-character sheet code.
#' @param millesime `character`. The cadastral version of the dataset.
#'    Must be on of `get_data_millesimes("pci")`. Default is `"latest"`.
#' @param format `character`. The format of the data.
#'    Must be `"edigeo"` or `"dxf"`. Default is `"edigeo"`.
#'
#' @return A character vector of full URLs pointing to the requested PCI sheets.
#'
#' @details
#' - If an element of `x` is a 5-character code, all sheets for that commune are returned.
#' - If an element of `x` is a 12-character code, only the URL for that specific sheet is returned.
#' - Invalid codes will throw an error.
#'
#' @seealso [get_data_millesimes()]
#'
#' @examples
#' \dontrun{
#' # Generate URLs for a single commune
#' get_pci_urls("72187")
#'
#' # Generate URL for a specific sheet
#' get_pci_urls("72181000AB01")
#'
#' # Multiple codes at once
#' get_pci_urls(c("72187", "72181000AB01"))
#' }
#'
#' @keywords internal
get_pci_urls <- function(x,
                         millesime = "latest",
                         format = "edigeo") {

  millesime <- match.arg(millesime, get_data_millesimes("pci"))
  format    <- match.arg(format, c("edigeo", "dxf"))

  base  <- get_base_data_url("pci")
  scale <- "feuilles"

  # Check all codes before applying
  invalid <- x[!nchar(x) %in% c(5, 12)]
  if (length(invalid) > 0) {
    stop("Invalid code(s): ", paste(invalid, collapse = ", "),
         " (must be 5-character commune or 12-character sheet)")
  }

  urls <- lapply(x, function(code) {
    if (nchar(code) == 5) {
      commune <- as.character(code)
      valid <- check_insee(commune, verbose = TRUE)
      if (!all(valid)) {
        stop("Some INSEE codes are invalid or correspond to mother communes.")
      }

      message("")
      detect_urls(construct_data_url(site = "pci",
                                     code,
                                     millesime = millesime,
                                     format = format), absolute = TRUE)
    } else {
      # sheet
      commune <- substr(code, 1, 5)
      sheet <- sprintf("%s-%s.tar.bz2", format, code)
      file.path(base, millesime, format, scale,
                construct_commune(commune), sheet)
    }
  })

  unique(unlist(urls, use.names = FALSE))
}

### Data section ----
#' Download and read PCI raw datasets from server
#'
#' This function downloads PCI data for given commune or sheet codes, extracts
#' the files, and reads them into R as `sf` objects (for EDIGEO) or another
#' suitable format (for DXF).
#'
#' @param x `character`. Vector of codes to retrieve URLs for.
#'    Each element must be either a 5-character commune code or a 12-character sheet code.
#' @param millesime `character`. The cadastral version of the dataset.
#'    Must be on of `get_data_millesimes("pci")`. Default is `"latest"`.
#' @param format `character`. The format of the data.
#'    Must be `"edigeo"` or `"dxf"`. Default is `"edigeo"`.
#' @param extract_dir `character` or `NULL`. Directory where files will be downloaded and extracted.
#'    If `NULL`, a temporary directory is used.
#' @param verbose `logical`. If `TRUE`, prints progress messages during download and extraction.
#'
#' @return An `sf` object or an `sf` objects list depending query. Returns `NULL` if downloads fail.
#'
#' @details
#' - Downloads all relevant archives for the provided codes.
#' - Extracts files in `extract_dir`.
#' - Reads EDIGEO or DXF files.
#'
#' @seealso [get_data_millesimes()]
#'
#' @examples
#' \dontrun{
#' # Download all sheets for a commune
#' pci_data <- get_pci("72187")
#'
#' # Download a specific sheet
#' pci_data <- get_pci("72181000AB01")
#'
#' # Multiple communes and sheets
#' pci_data <- get_pci(c("72187", "72181000AB01"))
#' }
#'
#' @export
get_pci <- function(x,
                    millesime = "latest",
                    format = "edigeo",
                    extract_dir = NULL,
                    verbose = TRUE) {

  millesime <- match.arg(millesime, get_data_millesimes("pci"))
  format    <- match.arg(format, c("edigeo", "dxf"))

  # 1. Get all URLs
  urls <- get_pci_urls(x, millesime, format)

  # 2. Download and extract all files in extract_dir
  download_results <- download_archives(
    urls = urls,
    destfiles = NULL,
    extract_dir = extract_dir,
    use_subdirs = FALSE,
    verbose = verbose
  )
  extraction_path <- download_results[[1]]

  # 3. Read all extrated files in extract_dir
  sf_data <- switch(format,
                    dxf = read_dxf(extraction_path),
                    edigeo = read_edigeo(extraction_path))

  return(sf_data)
}
