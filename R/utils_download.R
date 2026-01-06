#' Print message if verbose
#'
#' @param verbose Logical, whether to print messages
#' @param ... Passed to message()
#' @keywords internal
log_msg <- function(verbose, ...) {
  if (isTRUE(verbose)) message(...)
}

#' Print warning if verbose
#'
#' @param verbose Logical, whether to print warnings
#' @param ... Passed to warning()
#' @keywords internal
log_warn <- function(verbose, ...) {
  if (isTRUE(verbose)) warning(..., call. = FALSE)
}

#' Get file extension
#'
#' @param filename Character, file name or path
#' @return File extension in lower case
#' @keywords internal
get_extension <- function(filename) {
  # keep last element after "."
  ext <- sub(".*\\.([^.]*)$", "\\1", filename)
  tolower(ext)
}

#' Extract a GZ file
#'
#' @param src Character, path to the GZ file
#' @param dest Character, path to write the decompressed file
#' @return Path to the decompressed file
#' @keywords internal
extract_gz <- function(src, dest) {
  con_in  <- gzfile(src, "rb")
  con_out <- file(dest, "wb")
  while (length(buf <- readBin(con_in, what = raw(), n = 65536)) > 0) {
    writeBin(buf, con_out)
  }
  close(con_in)
  close(con_out)
  dest
}

#' Extract a file (GZ file or archive)
#'
#' @param filepath Character, path to the file to extract
#' @param extract_dir Character, directory where files will be extracted
#' @return Path to extraction directory
#' @importFrom archive archive_extract
#' @keywords internal
extract_file <- function(filepath, extract_dir) {
  ext <- get_extension(filepath)
  if (ext == "gz") {
    out_file <- file.path(extract_dir, sub("\\.gz$", "", basename(filepath)))
    extract_gz(filepath, out_file)
  } else {
    archive_extract(filepath, dir = extract_dir)
  }
  extract_dir
}

#' Check if a URL exists
#'
#' @param url Character, URL to check
#' @return Logical, TRUE if URL exists
#' @keywords internal
url_exists <- function(url) {
  ok <- tryCatch({
    # Build request and send HEAD
    resp <- httr2::request(url) |>
      httr2::req_method("HEAD") |>
      httr2::req_timeout(5) |>
      httr2::req_perform()

    # Check HTTP status code
    httr2::resp_status(resp) < 400
  },
  error = function(e) FALSE,
  warning = function(w) FALSE
  )
  ok
}

#' Download and extract a single archive
#'
#' @param url `character`. URL
#' @param destfile `character`. Destination file path, or `NULL`
#' @param extract_dir `character` or `NULL`. Directory where files will be downloaded and extracted. If `NULL`, a temporary directory is used.
#' @param verbose `logical`. If `TRUE`, prints progress messages during download and extraction.
#' @return Path to extraction directory, or NULL if failed
#' @keywords internal
download_archive <- function(url,
                             destfile = NULL,
                             extract_dir = NULL,
                             verbose = TRUE) {
  # 1. Check URL
  if (!url_exists(url)) {
    log_warn(verbose, sprintf("URL not reachable: %s", url))
    return(NULL)
  }

  # 2. Prepare destination
  if (is.null(destfile)) destfile <- tempfile(basename(url))
  if (is.null(extract_dir)) extract_dir <- tempdir()

  # 3. Download
  ok <- tryCatch({
    httr2::request(url) |> httr2::req_perform(path = destfile)
    TRUE
  }, error = function(e) {
    log_warn(verbose, sprintf("Download failed: %s", conditionMessage(e)))
    FALSE
  })
  if (!ok) return(NULL)
  log_msg(verbose, sprintf("Downloaded: %s", basename(destfile)))

  # 4. Extract
  ok <- tryCatch({
    extract_file(destfile, extract_dir)
    TRUE
  }, error = function(e) {
    log_warn(verbose, sprintf("Extraction failed: %s", conditionMessage(e)))
    FALSE
  })
  if (!ok) return(NULL)

  log_msg(verbose, sprintf("Extracted to: %s", extract_dir))
  extract_dir
}

#' Download and extract multiple archives
#'
#' @param urls `character`. Vector of URLs
#' @param destfiles `character`. Vector of destination file paths, or `NULL`
#' @param extract_dir `character` or `NULL`. Directory where files will be downloaded and extracted. If `NULL`, a temporary directory is used.
#' @param use_subdirs `logical`. If `TRUE`, create separate subdirectories per archive
#' @param verbose `logical`. If `TRUE`, prints progress messages during download and extraction.
#' @return List of extraction paths
#' @keywords internal
download_archives <- function(urls,
                              destfiles = NULL,
                              extract_dir = NULL,
                              use_subdirs = FALSE,
                              verbose = TRUE) {

  if (is.null(destfiles)) destfiles <- rep(NA_character_, length(urls))
  if (is.null(extract_dir)) extract_dir <- tempdir()

  lapply(seq_along(urls), \(i) {
    sub_dir <- if (use_subdirs) file.path(extract_dir, paste0("archive_", i)) else extract_dir
    if (use_subdirs && !dir.exists(sub_dir)) dir.create(sub_dir, recursive = TRUE)

    download_archive(
      url = urls[i],
      destfile = if (!is.na(destfiles[i])) destfiles[i] else NULL,
      extract_dir = sub_dir,
      verbose = verbose
    )
  })
}
