#' flag_speed_share
#'
#' @description
#' Function to update result based on a narrower time_window.
#'
#' @details
#' This function identifies and marks the subset of results that match a more stringent time window.
#'
#' @param x A data table from a coordination detection function
#' @param result A data table containing the result data.
#' @param min_participation The minimum repetition threshold. Users with repetition count
#'                       greater than this threshold will be retained (default parameter equal to
#'                       the one used in the \link{detect_groups} function).
#' @param time_window The number of seconds within which shared contents are to be considered as
#'                    coordinated according to the new time_window (default parameter equal to
#'                    the one used in the \link{detect_groups} function).
#'
#' @return A results data table that includes an additional column set to 1 when the share
#'         corresponds with the new time_window, and 0 otherwise.
#'
#' @import data.table
#' @export

flag_speed_share <- function(x, result, min_participation, time_window) {
  content_id <- content_id_y <- NULL

  if (!inherits(x, "data.table")) {
    x <- data.table::as.data.table(x)
  }

  # update time window
  result_update <- result[result$time_delta <= time_window]

  # filter by minimum repetition
  result_update <-
    filter_min_participation(x, result_update, min_participation)

  # set keys for joining
  setkey(result, content_id, content_id_y)
  setkey(result_update, content_id, content_id_y)

  # create a column name based on the time_window argument
  column_name <- paste("time_window", time_window, sep = "_")

  # Initialize the new column to 0 for all rows
  result[, (column_name) := 0]

  # Update the new column to 0 for rows that find a match in result_update
  result[result_update, on = .(content_id, content_id_y), (column_name) := 1]

  return(result)
}
