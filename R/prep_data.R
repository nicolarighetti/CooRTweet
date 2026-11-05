#' prep_data
#'
#' @description
#' Standardize column names to "object_id", "account_id", "content_id", and
#' "timestamp_share" and remove missing or duplicate shares.
#'
#' @details
#' This function allows the user to specify the current names of columns in
#' their data.table that they wish to rename to a standard format. The function
#' checks for each parameter and renames the corresponding column in the data.table.
#' If the parameter is NULL, no change is made to that column. The input is copied
#' before renaming, so a supplied data.table is not changed by reference.
#' By default, rows missing a value in any available standardized column are
#' removed. When all four standardized columns are present, duplicates are
#' identified by those four columns and the first row is kept. With a partial
#' schema, only identical full rows are considered duplicates. Other columns
#' are retained and their missing values do not cause rows to be removed.
#' For the 'timestamp_share' column, the function expects the format to be either UNIX format
#' (integer representing seconds since the Unix epoch) or "%Y-%m-%d %H:%M:%S". If the 'timestamp_share'
#' is in a different format, the function attempts to convert it to UNIX format using base R functions.
#'
#' @param x A data.table or an object that can be converted into a data.table.
#' This is the dataset whose columns will be renamed.
#'
#' @param object_id The current name of the column that should be renamed to "object_id".
#' If NULL, no renaming is performed on this column.
#'
#' @param account_id The current name of the column that should be renamed to "account_id".
#' If NULL, no renaming is performed on this column.
#'
#' @param content_id The current name of the column that should be renamed to "content_id".
#' If NULL, no renaming is performed on this column.
#'
#' @param timestamp_share The current name of the column that should be renamed to "timestamp_share".
#' The data in this column should be either in UNIX format or in a "%Y-%m-%d %H:%M:%S" format.
#' If conversion is unsuccessful, the function stops with an error.
#' If NULL, no renaming or conversion is performed on this column.
#' @param drop_na Remove rows missing a value in any available standardized
#' column (default TRUE). Set to FALSE to retain them.
#' @param drop_duplicates Remove duplicate shares after renaming and timestamp
#' conversion (default TRUE). With all four standardized columns present,
#' duplicates are determined by those columns; otherwise, exact duplicate rows
#' are removed. Set to FALSE to retain duplicates.
#'
#' @return A data.table with standardized columns and, by default, missing and
#' duplicate shares removed. The input object is not modified.
#'
#' @examples
#' dt <- data.table::data.table(original_object_id = c(1, 1, NA),
#'                              original_account_id = c("a", "a", "b"))
#' prep_data(dt, object_id = "original_object_id", account_id = "original_account_id")
#' prep_data(dt, object_id = "original_object_id", account_id = "original_account_id",
#'           drop_na = FALSE, drop_duplicates = FALSE)
#'
#' @import data.table
#' @export



prep_data <- function(x, object_id = NULL, account_id = NULL, content_id = NULL,
                      timestamp_share = NULL, drop_na = TRUE,
                      drop_duplicates = TRUE) {

  for (value in list(drop_na, drop_duplicates)) {
    if (!is.logical(value) || length(value) != 1L || is.na(value)) {
      stop("drop_na and drop_duplicates must each be TRUE or FALSE")
    }
  }

  # Convert x to a data.table if it's not already
  if (!inherits(x, "data.table")) {
    x <- data.table::as.data.table(x)
  } else {
    x <- data.table::copy(x)
  }

  # Rename columns based on the provided arguments
  if (!is.null(object_id)) {
    setnames(x, old = object_id, new = "object_id")
  }
  if (!is.null(account_id)) {
    setnames(x, old = account_id, new = "account_id")
  }
  if (!is.null(content_id)) {
    setnames(x, old = content_id, new = "content_id")
  }
  if (!is.null(timestamp_share)) {
    setnames(x, old = timestamp_share, new = "timestamp_share")
  }

  standardized_cols <- c("object_id", "account_id", "content_id", "timestamp_share")
  available_cols <- intersect(standardized_cols, names(x))

  if (drop_na && length(available_cols) > 0L) {
    x <- x[stats::complete.cases(x[, available_cols, with = FALSE])]
  }

  if (!is.null(timestamp_share) && !is.numeric(x[["timestamp_share"]])) {
    # Missing timestamps have already been removed when drop_na = TRUE;
    # an invalid non-missing timestamp remains an error.
    original_missing <- is.na(x[["timestamp_share"]])
    converted <- as.numeric(as.POSIXct(x[["timestamp_share"]], tz = "UTC"))
    if (any(is.na(converted) & !original_missing)) {
      stop("timestamp_share must be in UNIX format or '%Y-%m-%d %H:%M:%S' with known timezone")
    }
    x[["timestamp_share"]] <- converted
  }

  if (drop_duplicates) {
    if (all(standardized_cols %in% names(x))) {
      x <- unique(x, by = standardized_cols)
    } else {
      x <- unique(x)
    }
  }

  return(x)
}
