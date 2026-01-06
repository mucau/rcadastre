### Manage IDU section ----
#' Build IDUs from their components
#'
#' Constructs a standardized 14-character IDU
#' from department, commune, prefix, section, and parcel number.
#'
#' @param dep `character`. Department code (mandatory).
#' Must be 2 characters (e.g., `"01"`, `"95"`, `"2A"`, `"2B"`)
#' or 3 characters for overseas departments (`"971"`–`"978"`).
#' @param com `character`. Commune code within the department.
#' Must be 3 characters for metropolitan France, or 2 characters for overseas departments.
#' @param prefix `character`. Prefix code (3 characters, zero-padded).
#' @param section `character`. Section code (2 characters, zero-padded, uppercase).
#' @param numero `character`. Parcel number (4 characters, zero-padded).
#'
#' @return A `character` vector of 14-character IDUs.
#'
#' @details
#' - All input vectors must have the same length.
#' - The function automatically zero-pads and upper field names where required.
#' - Both `dep` and `com` are required.
#'
#' @seealso [check_insee()], [idu_check()]
#'
#' @examples
#' \dontrun{
#' idu_build(dep = "72", com = "187", prefix = "000", section = "A", numero = "1")
#' }
#'
#' @export
#'
idu_build <- function(dep, com, prefix, section, numero) {
  # Check args
  missing_args <- c(
    dep     = missing(dep),
    com     = missing(com),
    prefix  = missing(prefix),
    section = missing(section),
    numero  = missing(numero)
  )
  if (any(missing_args)) {
    stop(
      "Missing required argument(s): ",
      paste(names(missing_args)[missing_args], collapse = ", "),
      call. = FALSE
    )
  }

  # Ensure character and format
  dep     <- as.character(dep)
  com     <- as.character(com)
  prefix  <- pad0(prefix, 3)
  section <- pad0(section, 2, upper = TRUE)
  numero  <- pad0(numero, 4)

  # Check consistent lengths
  lengths <- c(length(dep), length(com), length(prefix), length(section), length(numero))
  if (length(unique(lengths)) != 1) {
    stop("All input vectors (dep, com, prefix, section, numero) must have the same length.",
         call. = FALSE)
  }

  # Validate length of codes
  dep_len <- nchar(dep)
  com_len <- nchar(com)
  if (any(!dep_len %in% c(2, 3))) stop("`dep` must have 2 or 3 characters.", call. = FALSE)
  if (any(!com_len %in% c(2, 3))) stop("`com` must have 2 or 3 characters.", call. = FALSE)

  # Build full commune code
  commune <- ifelse(
    substr(dep, 1, 2) == "97",
    paste0(pad0(dep, 3, upper = TRUE), pad0(com, 2)),
    paste0(pad0(dep, 2, upper = TRUE), pad0(com, 3))
  )

  # Commune check
  commune <- as.character(commune)
  valid <- check_insee(commune, verbose = TRUE)
  if (!all(valid)) {
    stop("Some INSEE codes are invalid or correspond to mother communes.", call. = FALSE)
  }

  # Build and validate IDUs
  idu <- paste0(commune, prefix, section, numero)
  valid <- idu_check(idu)
  if (!all(valid)) {
    stop("Invalid IDU(s) generated: ", paste(idu[!valid], collapse = ", "), call. = FALSE)
  }

  idu
}

#' Split IDUs into their components
#'
#' Splits a French cadastral parcel IDU (unique parcel identifier) into its
#' components: department, commune, prefix, section, and parcel number.
#'
#' @param idu `character`
#'   A vector of IDU codes (14 characters each). Non-character inputs will be coerced.
#'
#' @return A `data.frame` with one row per IDU and columns:
#' - `code_dep`  : Department code
#' - `code_com`  : Commune code
#' - `prefix`    : Prefix code
#' - `section`   : Section code
#' - `numero`    : Parcel number
#' - `insee`     : INSEE code of commune
#'
#' @details
#' The IDU structure is:
#' ```
#' [1-2]   : Department code (DEP)
#' [3-5]   : Commune code (COM)
#' [6-8]   : Prefix code
#' [9-10]  : Section code
#' [11-14] : Parcel number
#' ```
#'
#' @examples
#' \dontrun{
#' try(idu_split("0100200A0012"))
#' idu_split("29158000AK0001")
#' }
#'
#' @export
#'
idu_split <- function(idu) {
  # IDU check
  valid <- idu_check(idu)
  if (!all(valid)) {
    stop("Invalid IDU(s) detected: ", paste(idu[!valid], collapse = ", "), call. = FALSE)
  }

  # Extract first 2 and 3 characters (possible department codes)
  dep2 <- substr(idu, 1, 2)
  dep3 <- substr(idu, 1, 3)

  # Detect DOMs (department codes 971 to 978)
  is_dom <- dep2 == "97"

  # Department code: 2 or 3 characters depending on DOM
  code_dep <- ifelse(is_dom, dep3, dep2)

  # Commune code: adjust depending on DOM or mainland
  code_com <- ifelse(is_dom,
                     substr(idu, 4, 5),   # DOM = 2 digits for commune
                     substr(idu, 3, 5))   # Mainland = 3 digits

  # Reconstruct full INSEE code
  insee <- paste0(code_dep, code_com)

  # Commune check
  commune <- as.character(insee)
  valid <- check_insee(commune, verbose = FALSE)
  if (!all(valid)) {
    stop("Some INSEE codes are invalid or correspond to mother communes.", call. = FALSE)
  }

  # Return data.frame with all IDU components
  data.frame(
    idu      = idu,
    code_dep = code_dep,
    code_com = code_com,
    prefix   = substr(idu, 6, 8),
    section  = substr(idu, 9, 10),
    numero   = substr(idu, 11, 14),
    insee    = insee,
    stringsAsFactors = FALSE
  )
}

### Check IDU section ----
#' Check if a vector contains valid cadastral parcel identifiers (IDUs)
#'
#' This function checks whether a character vector contains valid French cadastral
#' parcel identifiers (IDUs). It returns a logical vector indicating validity for
#' each element. Optionally, a warning can be issued listing invalid entries.
#'
#' @param x A `character` vector containing IDU codes to validate.
#' @param verbose `logical` (default = `FALSE`). If `TRUE`, a warning is issued
#'   listing invalid IDUs. If `FALSE`, the function runs silently.
#'
#' @details
#' A valid IDU must satisfy all of the following:
#' \itemize{
#'   \item Non-missing, non-empty string of length 14.
#'   \item First two characters: department code — digits (0–9) or letters 'A'/'B'.
#'   \item Next three characters: commune code — digits (0–9).
#'   \item Next three characters: prefix — digits (0–9).
#'   \item Next two characters: section — digits (0–9) or uppercase letters (A–Z).
#'   \item Last four characters: parcel number — digits (0–9).
#'   \item No lowercase letters or special characters.
#' }
#'
#' @return A logical vector of the same length as `x`, where `TRUE` indicates
#'   a valid IDU and `FALSE` an invalid one.
#'   If `verbose = TRUE`, a warning lists the invalid IDUs.
#'
#' @examples
#' \dontrun{
#' # Single valid IDU
#' idu_check("01001000AA0123")
#'
#' # Multiple values, with some invalid
#' idu_check(c("01001000AA0123", "12345", NA, ""), verbose = TRUE)
#'
#' # Empty input
#' idu_check(character(0), verbose = TRUE)
#' }
#'
#' @export
idu_check <- function(x, verbose = FALSE) {
  # Ensure x is a character vector
  x <- as.character(x)

  # Handle empty vector
  if (length(x) == 0) {
    if (verbose) warning("Input vector is empty.", call. = FALSE)
    return(logical(0))
  }

  # Pattern for valid IDU
  pattern <- "^[0-9AB]{2}[0-9]{3}[0-9]{3}[0-9A-Z]{2}[0-9]{4}$"

  # Check validity
  valid <- !is.na(x) & x != "" & nchar(x) == 14 & grepl(pattern, x)

  # Warning if requested
  if (verbose && !all(valid)) {
    invalids <- x[!valid]
    msg <- if (length(invalids) == 0) {
      "No invalid IDUs detected."
    } else {
      paste0("Invalid IDU(s) detected: ", paste(invalids, collapse = ", "))
    }
    warning(msg, call. = FALSE)
  }

  return(valid)
}

### Manage IDU field in df section ----
#' Detect IDU column in a data.frame
#'
#' This function checks a data.frame for a column that contains valid cadastral
#' parcel identifiers (IDUs). It returns either the column name, position, or both.
#'
#' @param df A `data.frame` or `tibble`.
#' @param output One of `"both"`, `"name"`, or `"position"` (default `"both"`).
#'
#' @return Depending on `output`:
#'   - `"name"`: name of the first column containing valid IDUs.
#'   - `"position"`: position index of the first column containing valid IDUs.
#'   - `"both"`: a list with `name` and `position`.
#'   If no column matches, returns `NULL` and prints a message.
#'
#' @examples
#' \dontrun{
#'df <- data.frame(a = c("721870000A0001", "971020000B0002"),
#'                 b = c("abc", "def"),
#'                 stringsAsFactors = FALSE)
#'idu_detect_in_df(df, output = "both")
#'}
#'
#' @export
idu_detect_in_df <- function(df, output = c("both", "name", "position")) {
  output <- match.arg(output)
  if (!is.data.frame(df)) stop("'df' must be a data.frame or tibble", call. = FALSE)

  for (i in seq_along(df)) {
    col <- df[[i]]
    if (!is.character(col)) next

    # Check if all values in the column are valid IDUs
    valid <- idu_check(col, verbose = FALSE)
    if (all(valid)) {
      return(switch(output,
                    name = names(df)[i],
                    position = i,
                    list(name = names(df)[i], position = i)))
    }
  }

  message("No column matches the IDU pattern.")
  invisible(NULL)
}

#' Rename the IDU column in a data frame
#'
#' This function detects the column containing IDU values in a data frame
#' and renames it to the name provided by the user.
#'
#' @param df A `data.frame` or similar object to search.
#' @param new_name A `character` string specifying the new column name for the IDU column.
#'
#' @return A `data.frame` identical to `df` except the IDU column is renamed.
#' If no IDU column is detected, the original data frame is returned unchanged.
#'
#' @examples
#' \dontrun{
#' df <- data.frame(
#'   parcel_id = c("12345ABCDE6789", "54321ZZZZZ0000"),
#'   name = c("Oak", "Pine"),
#'   stringsAsFactors = FALSE
#' )
#' df <- idu_rename_in_df(df, "IDU")
#' names(df)
#' }
#'
#' @export
#'
idu_rename_in_df <- function(df, new_name) {
  idu_info <- idu_detect_in_df(df, "both")
  if (is.null(idu_info)) {
    warning("No IDU column detected. Returning original data frame.", call. = FALSE)
    return(df)
  }
  names(df)[idu_info$position] <- new_name
  df
}

### Get attribut IDU section ----
#' Retrieve parcel data for given IDUs
#'
#' This function takes one or more valid IDU codes, retrieves parcel data from
#' Etalab, and optionally enriches the parcels with their associated place
#' names and administrative names. The function returns an `sf` object containing
#' the parcels and requested attributes.
#'
#' @param idu A `character` vector of valid IDU codes.
#' @param with_feuille `logical` (default: `TRUE`).
#'   Whether to retrieve and merge sheet id associated with the parcels.
#' @param with_lieudit `logical` (default: `TRUE`).
#'   Whether to retrieve and merge place names associated with the parcels.
#' @param with_cog `logical` (default: `TRUE`).
#'   Whether to retrieve and merge administrative names
#'   (region, department, commune) associated with the parcels.
#' @param ... Additional arguments passed to [idu_get_cog()].
#'
#' @return An `sf` object containing parcel geometries, the IDU code, and optionally
#'   associated place names and administrative names.
#'
#' @details
#' - All IDU codes are validated before any data is retrieved.
#' - If `with_feuille = TRUE`, the function performs a spatial join with the
#'   Etalab "lieux-dits" dataset and merges the names into the parcel data.
#' - If `with_lieudit = TRUE`, the function performs a spatial join with the
#'   Etalab "lieux-dits" dataset and merges the names into the parcel data.
#' - If `with_cog = TRUE`, the function retrieves region, department, and commune
#'   names using [idu_get_cog()] and merges them into the parcel data.
#' - The function ensures that Etalab data are returned as `sf` objects.
#'
#' @importFrom sf st_join st_drop_geometry
#'
#' @examples
#' \dontrun{
#' # Retrieve parcels with both lieudit and names
#' idu_get_parcelle(c("721870000A0001", "721870000A0002"))
#'
#' # Retrieve parcels without lieudit
#' idu_get_parcelle("721870000A0001", with_lieudit = FALSE)
#'
#' # Retrieve parcels without administrative names
#' idu_get_parcelle("721870000A0001", with_cog = FALSE)
#' }
#'
#' @export
#'
idu_get_parcelle <- function(idu,
                             with_feuille = TRUE,
                             with_lieudit = TRUE,
                             with_cog = TRUE,
                             ...) {

  # Split IDU and extract unique INSEE codes
  idu_parts   <- idu_split(idu)
  insee_codes <- unique(idu_parts$insee)

  # Download parcels
  parcelles <- get_etalab(insee_codes, "parcelles", verbose = FALSE) |>
    idu_rename_in_df("idu") |>
    subset(idu %in% idu_parts$idu)

  # Ensure parcels are sf objects
  if (!inherits(parcelles, "sf")) {
    stop("Etalab data must be 'sf' objects.", call. = FALSE)
  }

  # Retrieve sheets
  if (with_feuille) {
    sheets <- tryCatch(
      get_etalab(insee_codes, "feuilles", verbose = FALSE),
      error = function(e) NULL
    )

    # Only process if sheets is an sf object
    if (inherits(sheets, "sf")) {
      intersections <- suppressWarnings(
        st_join(parcelles, sheets, largest = TRUE) |> st_drop_geometry()
      )

      # Warn if some sheets names are missing
      if (anyNA(intersections$id )) {
        warning("Some sheets (feuille) names are missing (NA) in the 'etalab' data.", call. = FALSE)
      }

      # Merge parcels with place names
      parcelles <- merge(parcelles, intersections[, c("idu", "id")], by = "idu")
      names(parcelles)[names(parcelles) == "id"] <- "feuille"
    }
  }

  # Retrieve lieux_dits if requested
  if (with_lieudit) {
    lieudits <- tryCatch(
      get_etalab(insee_codes, "lieux_dits", verbose = FALSE),
      error = function(e) NULL
    )

    # Only process if lieudits is an sf object
    if (inherits(lieudits, "sf")) {
      intersections <- suppressWarnings(
        st_join(parcelles, lieudits, largest = TRUE) |> st_drop_geometry()
      )

      # Warn if some place names are missing
      if (anyNA(intersections$nom)) {
        warning("Some place names (lieu-dit) are missing (NA) in the 'etalab' data.", call. = FALSE)
      }

      # Merge parcels with place names
      parcelles <- merge(parcelles, intersections[, c("idu", "nom")], by = "idu")
      names(parcelles)[names(parcelles) == "nom"] <- "lieu_dit"
    }
  }

  # Retrieve parcel names if requested
  if (with_cog) {
    names_df <- idu_get_cog(idu, ...)
    parcelles <- merge(parcelles, names_df, by = "idu")
  }

  parcelles
}

#' Get region/department/commune names from IDU codes
#'
#' This function takes one or more IDU codes, validates them, splits them into
#' their components, and merges them with reference datasets to retrieve the
#' corresponding region, department, and/or commune names.
#'
#' @param idu A `character` vector of valid IDU codes.
#' @param loc A `character` vector specifying which location levels to include.
#' One or more of `"reg"`, `"dep"`, or `"com"`. Multiple values can be
#' provided simultaneously (e.g., `loc = c("reg", "dep", "com")`).
#' @param cog_field A single `character` string specifying the field name
#' in the reference datasets to use for naming. Must be one of `"NCC"`,
#' `"NCCENR"`, or `"LIBELLE"`. Default is `"NCC"`.
#'
#' @seealso \code{\link{commune_2025}}, \code{\link{departement_2025}}, \code{\link{region_2025}}
#'
#' @return A `data.frame` with the IDU split into its components and
#' the requested location names.
#'
#' @examples
#' \dontrun{
#' idu_get_cog(c("721870000A0001", "721870000A0002"))
#' }
#'
#' @keywords internal
idu_get_cog <- function(idu, loc = c("reg", "dep", "com"), cog_field = "NCC") {
  # Match argument
  loc <- match.arg(loc, c("reg", "dep", "com"), several.ok = TRUE)
  cog_field <- match.arg(cog_field, c("NCC", "NCCENR", "LIBELLE"), several.ok = FALSE)

  # Split IDU into components
  idu_parts <- idu_split(idu)
  res <- idu_parts

  # Merge region names if requested
  if ("reg" %in% loc) {
    res <- merge(res,
                 frcadastre::departement_2025[, c("DEP", "REG")],
                 by.x = "code_dep", by.y="DEP")
    res <- merge(res,
                 frcadastre::region_2025[, c("REG", cog_field)],
                 by.x = "REG", by.y="REG")
    names(res)[names(res) %in% c("REG", cog_field)] <- c("code_reg", "reg_name")
  }

  # Merge department names if requested
  if ("dep" %in% loc) {
    res <- merge(res,
                 frcadastre::departement_2025[, c("DEP", cog_field)],
                 by.x = "code_dep", by.y="DEP")
    names(res)[names(res) %in% c(cog_field)] <- c("dep_name")
  }

  # Merge commune names if requested
  if ("com" %in% loc) {
    res <- merge(res,
                 frcadastre::commune_2025[, c("COM", cog_field)],
                 by.x = "insee", by.y="COM")
    names(res)[names(res) %in% c("insee", cog_field)] <- c("code_com", "com_name")
  }

  # Keep only idu and requested name columns
  keep_cols <- c("idu")
  if ("reg" %in% loc)   keep_cols <- c(keep_cols, "code_reg", "reg_name")
  if ("dep" %in% loc)   keep_cols <- c(keep_cols, "code_dep", "dep_name")
  if ("com" %in% loc)   keep_cols <- c(keep_cols, "code_com", "com_name")

  res[, intersect(keep_cols, names(res)), drop = FALSE]
}
