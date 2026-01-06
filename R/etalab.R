### Data section ----
#' List Etalab data layers names
#'
#' Returns the names of Etalab layers for the requested type(s).
#'
#' @param type `character`. Etalab data type.
#' Must be one or more of `"raw"` or `"proc"`.
#' `"raw"` corresponds to raw Etalab layers.
#' `"proc"` to processed layers.
#' Default is `c("raw", "proc")`.
#'
#' @return A named list of character vectors containing layer names for each requested type.
#'
#' @details
#' - `"raw"`: raw Etalab layers such as `batiment`, `commune`, `parcelle`, etc.
#' - `"proc"`: processed Etalab layers such as `batiments`, `communes`, `parcelles`, etc.
#' - Invalid `types` values will throw an error.
#'
#' @examples
#' \dontrun{
#' # Get all raw layers
#' get_etalab_layernames("raw")
#'
#' # Get all processed layers
#' get_etalab_layernames("proc")
#'
#' # Get both types at once
#' get_etalab_layernames(c("raw", "proc"))
#' }
#'
#' @export
#'
get_etalab_layernames <- function(type = c("raw", "proc")) {
  mapping <- list(
    raw  = c("batiment", "borne", "commune", "label", "lieudit", "numvoie",
             "parcelle", "ptcanv", "section", "subdfisc", "subdsect",
             "symblim", "tline", "tpoint", "tronfluv", "tronroute", "tsurf",
             "zoncommuni"),
    proc = c("batiments", "communes", "feuilles", "lieux_dits",
             "parcelles", "prefixes_sections", "sections", "subdivisions_fiscales")
  )
  if (!all(type %in% names(mapping))) stop("Invalid type(s).")
  mapping[type]
}

### Download section ----
#' Download and read Etalab processed datasets from "bundle" app
#'
#' This function downloads Etalab cadastral dataset for given communes and
#' layer, by using the Etalab "bundle" cadastre.data.gouv.
#' The results are returned as an `sf` object. If multiple files are retrieved
#' (e.g. several communes), they are combined into a single `sf` object.
#'
#' @param commune `character` or `numeric` vector.
#'    The INSEE code(s) of the commune(s).
#' @param layer `character`. Datasets raw name to download.
#'    Must be dataset names returned by [get_etalab_layernames()].
#' @param verbose `logical`. If `TRUE`, prints progress messages.
#'
#' @return An `sf` object containing the requested layer, or `NULL` if download/read fails
#'
#' @seealso [get_etalab_layernames()]
#'
#' @examples
#' \dontrun{
#' # Download parcel geometries for a single commune
#' get_etalab_proc("72187", data = "parcelles")
#'
#' # Download parcel geometries for multiple communes
#' get_etalab_proc(c("72187", "72181"), data = "parcelles")
#'
#' }
#'
#' @keywords internal
get_etalab_proc <- function(commune, layer, verbose = TRUE) {

  # Commune check
  commune <- as.character(commune)
  valid <- check_insee(commune, verbose = FALSE)
  if (!all(valid)) {
    stop("Some INSEE codes are invalid or correspond to mother communes.")
  }

  # Layer check
  if (!layer %in% get_etalab_layernames("proc")$proc) {
    stop(
      sprintf(
        "Invalid processed layer: '%s'\nValid layers are: %s",
        layer, paste(get_etalab_layernames("proc")$proc, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  # Construct URL
  scale <- get_insee_scale(commune)
  url <- sprintf(
    "https://cadastre.data.gouv.fr/bundler/cadastre-etalab/%s/%s/geojson/%s",
    scale, commune, layer
  )

  if (verbose) message("Downloading ", layer, " for ", commune)

  # Download and read GeoJSON
  tryCatch(
    read_geojson(url),
    error = function(e) {
      if (verbose) message("Failed: ", conditionMessage(e))
      NULL
    }
  )
}

#' Download and read Etalab raw datasets from server
#'
#' This function downloads Etalab cadastre dataset for given communes
#' and layer.
#' The results are returned as an `sf` object. If multiple files are retrieved
#' (e.g. several communes), they are combined into a single `sf` object.
#'
#' @param commune `character` or `numeric` vector.
#'    The INSEE code(s) of the commune(s).
#' @param layer `character`. Datasets raw name to download.
#'    Must be dataset names returned by [get_etalab_layernames()].
#' @param millesime `character`. Dataset version for raw layers.
#'    Must be on of `get_data_millesimes("etalab")`. Default is `"latest"`.
#' @param extract_dir `character` or `NULL`.
#'    Directory where files will be downloaded and extracted.
#'    If `NULL`, a temporary directory is used.
#' @param verbose `logical`. If `TRUE`, prints progress messages.
#'
#' @return An `sf` object containing the requested layer, or `NULL` if download/read fails
#'
#' @seealso [get_etalab_layernames()], [get_data_millesimes()]
#'
#' @details
#' A detailed description of the data structure is available at the following link:
#' \url{https://github.com/etalab/edigeo-parser/raw/master/resources/standard_edigeo_2013.pdf}
#'
#' @examples
#' \dontrun{
#' # Download and read parcels for one commune
#' get_etalab_raw("72187", "parcelle")
#'
#' # Download and read parcels for multiple communes
#' get_etalab_raw(c("72187", "72181"), "parcelle")
#' }
#'
#' @keywords internal
get_etalab_raw <- function(commune,
                           layer,
                           millesime = "latest",
                           extract_dir = NULL,
                           verbose = TRUE) {

  # Commune check
  commune <- as.character(commune)
  valid <- check_insee(commune, verbose = FALSE)
  if (!all(valid)) {
    stop("Some INSEE codes are invalid or correspond to mother communes.")
  }

  # Layer check
  if (!layer %in% get_etalab_layernames("raw")$raw) {
    stop(
      sprintf(
        "Invalid processed layer: '%s'\nValid layers are: %s",
        layer, paste(get_etalab_layernames("raw")$raw, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  base <- construct_data_url("etalab", commune, millesime)
  raw_base <- file.path(base, "raw")

  links <- detect_urls(raw_base, absolute = TRUE)
  pattern <- paste0("^.*/pci-[0-9]+-", layer, "\\.json\\.gz$")
  url <- links[grepl(pattern, links)]

  if (!length(url)) {
    if (verbose) message("No data found for ", layer)
    return(NULL)
  }

  if (is.null(extract_dir))
    extract_dir <- tempfile("cadastre_raw_")
  dir.create(extract_dir, showWarnings = FALSE, recursive = TRUE)

  res <- download_archives(
    urls = url,
    destfiles = file.path(extract_dir, basename(url)),
    extract_dir = extract_dir,
    verbose = verbose
  )

  if (all(vapply(res, is.null, logical(1)))) {
    if (verbose) message("All downloads failed for ", layer)
    return(NULL)
  }

  tryCatch(
    read_geojson(list.files(extract_dir, full.names = TRUE)),
    error = function(e) {
      if (verbose) message("Read failed: ", conditionMessage(e))
      NULL
    }
  )
}

#' Download Etalab layer (raw or processed) for one or multiple communes
#'
#' This function automatically selects the appropriate download method
#' based on whether the requested layer is raw or processed.
#'
#' @param commune `character` or `numeric` vector.
#'    The INSEE code(s) of the commune(s).
#' @param layer `character`. Datasets raw name to download.
#'    Must be dataset names returned by [get_etalab_layernames()].
#' @param millesime `character`. Dataset version for raw layers.
#'    Must be on of `get_data_millesimes("etalab")`. Default is `"latest"`.
#' @param extract_dir `character` or `NULL`.
#'    Directory where files will be downloaded and extracted.
#'    If `NULL`, a temporary directory is used.
#' @param verbose `logical`. If `TRUE`, prints progress messages.
#'
#' @return An `sf` object containing the requested layer, or `NULL` if download/read fails
#'
#' @examples
#' \dontrun{
#' # Download processed layer
#' batiments <- get_etalab("72187", "batiments")
#'
#' # Download raw layer
#' parcelles <- get_etalab("72187", "parcelle")
#' }
#'
#' @export
get_etalab <- function(commune,
                       layer,
                       millesime = "latest",
                       extract_dir = NULL,
                       verbose = TRUE) {

  # Determine if layer is raw or processed
  layer_type <- if (layer %in% get_etalab_layernames("proc")$proc) {
    "proc"
  } else if (layer %in% get_etalab_layernames("raw")$raw) {
    "raw"
  } else {
    stop(
      sprintf(
        "Invalid layer: '%s'\nValid layers are: %s",
        layer, paste(unlist(get_etalab_layernames(), use.names = FALSE), collapse = ", ")
      ),
      call. = FALSE
    )
  }

  if (layer_type == "proc") {
    get_etalab_proc(commune, layer, verbose = verbose)
  } else {
    get_etalab_raw(commune, layer, millesime = millesime,
                   extract_dir = extract_dir, verbose = verbose)
  }
}
