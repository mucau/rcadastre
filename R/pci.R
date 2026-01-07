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
    get_data_url(commune, "pci", millesime, format),
    absolute
  )

  # Clean names for relative output
  if (!absolute) {
    links <- sub("^(edigeo|dxf)-(.*)\\.tar\\.bz2$", "\\2", links)
  }

  links
}

### Data section ----
#' Download and read PCI raw datasets from server
#'
#' This function downloads PCI data for given commune or sheet codes, extracts
#' the files, and reads them into R as `sf` objects (for EDIGEO) or another
#' suitable format (for DXF).
#'
#' @param id `character` or `numeric` vector.
#'    The INSEE code(s) of the cacadastral sheet(s) or commune(s).
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
get_pci <- function(id,
                    millesime = "latest",
                    format = "edigeo",
                    extract_dir = NULL,
                    verbose = TRUE) {

  millesime <- match.arg(millesime, get_data_millesimes("pci"))
  format    <- match.arg(format, c("edigeo", "dxf"))

  # 1. Get all URLs
  scale <- get_insee_scale(id)

  fetch_urls <- function(ids) {
    if (length(ids) == 0) return(character(0))
    urls <- detect_urls(get_data_url(ids, "pci", millesime, format),
                        absolute = TRUE)
    urls[grepl(paste(ids, collapse = "|"), urls)]
  }

  sheet_urls <- fetch_urls(id[scale == "feuilles"])
  city_urls  <- fetch_urls(id[scale == "communes"])

  urls <- unique(c(sheet_urls, city_urls))

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
