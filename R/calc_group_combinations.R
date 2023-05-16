#' calc_group_combinations
#'
#' @description
#' Private function that calculates the time differences
#' between all possible combinations in the group.
#'
#' @param group A data.table group with the columns:
#' content_id, id_user, timestamp_share
#'
#' @param time_window An integer with the time window of coordination.
#' Default to 10.
#'
#' @return A data.table with coordinated contents.
#' Columns: content_id, id_user, content_id_y, id_user_y, time_delta
#'
#' @import data.table
#' @noRd

calc_group_combinations <- function(group, time_window = 10) {
  id <- object_id <- content_id <- i.content_id <- NULL
  timestamp_share <- i.timestamp_share <- id_user <- NULL
  i.id_user <- time_delta <- NULL

  group$id <- seq_along(group$content_id)

  group <- group[group,
    on = .(id < id),
    .(
      object_id = object_id,
      content_id = content_id,
      content_id_y = i.content_id,
      time_delta = abs(timestamp_share - i.timestamp_share),
      id_user = id_user,
      id_user_y = i.id_user
    ),
    allow.cartesian = TRUE
  ][time_delta <= time_window]

  return(group)
}
