#' prep_data
#'
#' @description
#' Function to rename columns of a given data.table. This function standardizes
#' column names to "object_id", "account_id", "content_id", and "timestamp_share".
#' It is useful for preparing datasets for further analysis by ensuring
#' consistent column naming.
#'
#' @details
#' This function allows the user to specify the current names of columns in
#' their data.table that they wish to rename to a standard format. The function
#' checks for each parameter and renames the corresponding column in the data.table.
#' If the parameter is NULL, no change is made to that column. The function
#' ensures the data input is a data.table; if not, it converts it before renaming.
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
#' If the data is in a different format or conversion is unsuccessful, the function stops with an error.
#' If NULL, no renaming or conversion is performed on this column.
#'
#' @return A data.table with the specified columns renamed according to the input parameters.
#' If no renaming is required, the original data.table is returned unaltered.
#'
#' @examples
#' dt <- data.table::data.table(old_object_id = 1:3, old_account_id_y = 4:6)
#' dt <- prep_data(dt, object_id = "old_object_id", account_id = "old_account_id_y")
#'
#' @import data.table
#' @export



prep_data <- function(x, object_id = NULL, account_id = NULL, content_id = NULL, timestamp_share = NULL) {

  # Convert x to a data.table if it's not already
  if (!inherits(x, "data.table")) {
    x <- data.table::as.data.table(x)
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

    # Validate and convert timestamp_share to UNIX format
    if (any(!is.numeric(x[['timestamp_share']]))) {
      # Try to convert using as.POSIXct with UTC as default timezone
      x[['timestamp_share']] <- as.numeric(as.POSIXct(x[['timestamp_share']], tz = "UTC"))
      if (any(is.na(x[['timestamp_share']]))) {
        stop("timestamp_share must be in UNIX format or '%Y-%m-%d %H:%M:%S' with known timezone")
      }
    }
  }


  return(x)
}
