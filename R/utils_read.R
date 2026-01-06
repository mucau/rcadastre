# PCI section ----
#' Read DXF file(s) as an sf object
#'
#' This function reads a single DXF file or all DXF files in a directory
#' and attempts to aggregate them into a single `sf` object.
#'
#' @param path `character` Path to a DXF file or a directory containing DXF files.
#'
#' @return An `sf` object representing the spatial features read from the DXF file(s),
#' or a list of `sf` objects if aggregation fails.
#'
#' @importFrom sf st_read st_set_crs
#'
#' @seealso \code{\link[sf]{st_read}} for reading spatial vector data.
#'
#' @keywords internal
#'
read_dxf <- function(path) {
  target_crs <- 2154

  # If path is a single DXF file, read and return it directly
  if (file.exists(path) && grepl("\\.DXF$", path, ignore.case = TRUE)) {
    return(st_set_crs(st_read(path, quiet = TRUE), target_crs))
  }

  # Otherwise, assume path is a directory and list all DXF files inside
  files <- list.files(path, pattern = "\\.dxf$", full.names = TRUE, ignore.case = TRUE)
  if (length(files) == 0) {
    stop("No DXF files found in directory ", path)
  }

  # Read all DXF files as sf objects
  layers <- lapply(files, function(f) st_set_crs(st_read(f, quiet = TRUE), target_crs))

  # Try to aggregate all layers into a single sf object
  tryCatch({
    aggregated <- do.call(rbind, layers)
    aggregated
  }, error = function(e) {
    warning("Failed to aggregate DXF files: ", e$message)
    layers
  })
}

#' Read EDIGEO data from a directory containing .THF files
#'
#' This function reads all layers from all .THF files in the specified directory,
#' attempts to aggregate layers with matching names if their attribute structures
#' are compatible, and returns a named list of `sf` objects.
#'
#' @param edigeo_dir `character` Path to the directory containing EDIGEO .THF files.
#'
#' @return A named list of `sf` objects representing the layers read and aggregated
#' from the EDIGEO files.
#'
#' @importFrom sf st_layers st_read st_set_geometry st_geometry
#'
#' @keywords internal
#'
read_edigeo <- function(edigeo_dir) {
  if (!dir.exists(edigeo_dir)) {
    stop("The directory does not exist: ", edigeo_dir)
  }

  thf_files <- list.files(edigeo_dir, pattern = "\\.THF$", ignore.case = TRUE, full.names = TRUE)
  if (length(thf_files) == 0) {
    stop("No .THF files found in directory ", edigeo_dir)
  }

  layers_aggregated <- list()
  final_layers <- list()

  for (thf_file in thf_files) {
    layers <- st_layers(thf_file)$name

    for (layer_name in layers) {
      message("Reading layer '", layer_name, "' from ", basename(thf_file))
      layer_sf <- st_read(thf_file, layer = layer_name, quiet = TRUE)

      if (!layer_name %in% names(layers_aggregated)) {
        layers_aggregated[[layer_name]] <- layer_sf
      } else {
        existing <- layers_aggregated[[layer_name]]

        data_exist <- st_set_geometry(existing, NULL)
        data_new <- st_set_geometry(layer_sf, NULL)

        cols_exist <- names(data_exist)
        cols_new <- names(data_new)

        common_cols <- intersect(cols_exist, cols_new)
        diff_types <- sapply(common_cols, function(col) {
          !identical(class(data_exist[[col]]), class(data_new[[col]]))
        })
        diff_types_cols <- common_cols[diff_types]

        cols_only_in_exist <- setdiff(cols_exist, cols_new)
        cols_only_in_new <- setdiff(cols_new, cols_exist)

        # If attributes match exactly, rbind; otherwise, separate layers
        if (length(cols_only_in_exist) == 0 && length(cols_only_in_new) == 0 && length(diff_types_cols) == 0) {
          geom_exist <- st_geometry(existing)
          geom_new <- st_geometry(layer_sf)
          data_bind <- rbind(data_exist, data_new)
          geom_bind <- c(geom_exist, geom_new)
          layers_aggregated[[layer_name]] <- st_set_geometry(data_bind, geom_bind)
        } else {
          # If differences, move existing to final_layers (if not already), add new separately
          if (!layer_name %in% names(final_layers)) {
            final_layers[[layer_name]] <- layers_aggregated[[layer_name]]
          } else {
            layer_name <- paste0(layer_name, "_alt_", length(final_layers) + 1)
          }
          final_layers[[layer_name]] <- layer_sf
          layers_aggregated[[layer_name]] <- NULL
        }
      }
    }
  }

  # Add any remaining aggregated layers to final_layers
  for (nm in names(layers_aggregated)) {
    final_layers[[nm]] <- layers_aggregated[[nm]]
  }

  final_layers
}

# Etalab section ----
#' Read and combine a single GeoJSON layer from files or URLs
#'
#' Reads one or more GeoJSON datasets corresponding to a single layer
#' from local files or remote URLs, and combines them into a single `sf` object.
#' gzipped GeoJSON files (`.json.gz`) are supported directly and do not
#' require explicit decompression.
#'
#' @param sources `character`. A vector of local file paths or URLs pointing
#'   to GeoJSON or gzipped GeoJSON files for a single layer.
#'
#' @return A single `sf` object containing all features from the provided sources.
#'   If multiple files are provided for the same layer, they are combined using
#'   row binding (`rbind`). Columns are assumed to be consistent across files.
#'
#' @details
#' Layer names are extracted from file names or URLs using the following rules:
#' 1. Remove the `.gz` extension if present.
#' 2. Remove the `.geojson` or `.json` extension.
#' 3. Keep only the substring after the last dash character.
#'
#' When local files are provided, duplicated sources corresponding to both
#' `.json` and `.json.gz` versions of the same dataset are automatically
#' filtered so that each dataset is read only once.
#'
#' Aggregation is simplified: since all files belong to the same layer, they
#' are simply combined using `rbind`.
#'
#' @seealso [sf::st_read()]
#'
#' @examples
#' \dontrun{
#' # Read all GeoJSON files for a single layer from a directory
#' files <- list.files("path/to/geojson", full.names = TRUE)
#' parcels <- read_geojson(files)
#'
#' # Read a single layer from multiple URLs
#' urls <- c(
#'   "https://example.org/data/parcelles.geojson",
#'   "https://example.org/data/parcelles-2.geojson"
#' )
#' parcels <- read_geojson(urls)
#' }
#'
#' @keywords internal
read_geojson <- function(sources) {

  # Identify URLs
  is_url <- grepl("^https?://", sources)

  # For local files, remove duplicated .json / .json.gz
  if (any(!is_url)) {
    files <- sources[!is_url]
    base <- sub("\\.gz$", "", files)
    files <- files[!duplicated(base)]
    sources[!is_url] <- files
  }

  # Unified reader for files and URLs
  reader <- function(x) {
    # Extract a simplified layer name (not used in aggregation here)
    name <- basename(x)
    name <- sub("\\.gz$", "", name, ignore.case = TRUE)
    name <- sub("\\.(geojson|json)$", "", name, ignore.case = TRUE)
    name <- sub(".*-", "", name)

    data <- sf::st_read(x, quiet = TRUE)
    data
  }

  # Read all sources
  sf_list <- lapply(sources, reader)

  # Combine into a single sf object
  if (length(sf_list) == 1) {
    sf_list[[1]]
  } else {
    do.call(rbind, sf_list)
  }
}
