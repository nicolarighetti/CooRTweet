#' calc_group_combinations
#'
#' @description
#' Private function that calculates the time differences
#' between all possible combinations in the group.
#'
#' @param group A data.table group with the columns:
#' content_id, account_id, timestamp_share
#'
#' @param time_window An integer with the time window of coordination.
#' Default to 10.
#'
#' @return A data.table with coordinated contents.
#' Columns: content_id, account_id, content_id_y, account_id_y, time_delta
#'
#' @import data.table
#' @noRd

calc_group_combinations <- function(group, time_window = 10, time_window_low = 0) {
  id <- object_id <- content_id <- i.content_id <- NULL
  timestamp_share <- i.timestamp_share <- account_id <- NULL
  i.account_id <- time_delta <- NULL

  group$id <- seq_along(group$content_id)

    group <- group[group,
                   on = .(id < id),
                   .(
                     object_id = object_id,
                     content_id = content_id,
                     content_id_y = i.content_id,
                     time_delta = timestamp_share - i.timestamp_share,
                     account_id = account_id,
                     account_id_y = i.account_id
                   ),
                   allow.cartesian = TRUE
    ][abs(time_delta) <= time_window & abs(time_delta) >= time_window_low]


  return(group)
}
