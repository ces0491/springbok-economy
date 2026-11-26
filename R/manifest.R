# One manifest for every raw file: where it came from, when, and its SHA-256.
# Both fetch scripts write through manifest_upsert(); the paper reads through
# verify_manifest(), which refuses to render from a file that has changed.

MANIFEST_PATH <- "data/raw/MANIFEST.csv"

#' Record `path` in the manifest, replacing any earlier row for it. A file
#' derived from downloads, such as the World Rugby facts table, passes the time
#' of those downloads as `retrieved_utc`.
manifest_upsert <- function(path, source,
                            retrieved_utc = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")) {
  row <- data.frame(
    file = path,
    source = source,
    retrieved_utc = retrieved_utc,
    bytes = file.size(path),
    sha256 = digest::digest(path, algo = "sha256", file = TRUE)
  )
  if (file.exists(MANIFEST_PATH)) {
    held <- utils::read.csv(MANIFEST_PATH, stringsAsFactors = FALSE)
    held <- held[held$file != path, , drop = FALSE]
    row <- rbind(held, row)
  }
  row <- row[order(row$file), , drop = FALSE]
  utils::write.csv(row, MANIFEST_PATH, row.names = FALSE)
  invisible(row)
}

#' Drop manifest rows whose file path matches `pattern`, for files the
#' repository no longer holds.
manifest_drop <- function(pattern) {
  held <- utils::read.csv(MANIFEST_PATH, stringsAsFactors = FALSE)
  utils::write.csv(held[!grepl(pattern, held$file), , drop = FALSE], MANIFEST_PATH, row.names = FALSE)
  invisible(held[grepl(pattern, held$file), , drop = FALSE])
}

verify_manifest <- function(root = ".") {
  m <- utils::read.csv(file.path(root, MANIFEST_PATH), stringsAsFactors = FALSE)
  actual <- vapply(
    file.path(root, m$file),
    function(p) if (file.exists(p)) digest::digest(p, algo = "sha256", file = TRUE) else NA_character_,
    character(1)
  )
  bad <- m$file[is.na(actual) | actual != m$sha256]
  if (length(bad) > 0) {
    stop("Raw data does not match data/raw/MANIFEST.csv: ", paste(bad, collapse = ", "))
  }
  invisible(m)
}
