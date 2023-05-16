#' detect_coordinated_groups
#'
#' @description
#' Function to detect coordinated behaviour based on content groups.
#' See details.
#'
#' @details The function groups the data by `object_id` (uniquely identifies
#' coordinated content) and calculates the time differences between all
#' `content_id` (ids of user generated contents) within their groups.
#' It then filters out all `content_id` that are higher than the `time_window`
#' (in seconds). It returns a `data.table` with all IDs of coordinated
#' contents. The `object_id` can be for example: hashtags, IDs of tweets being
#' retweeted, or URLs being shared.
#'
#' @param x a data.table with the columns: `object_id` (uniquely identifies
#' coordinated content), `id_user` (unique ids for users), `content_id`
#' (id of user generated content), `timestamp_share` (integer)
#'
#' @param time_window the number of seconds within which shared contents
#' are to be considered as coordinated (default to 10 seconds).
#'
#' @param min_repetition the minimum number of repeated coordinated 
#' action to define two users as coordinated (defaults to 2)
#' 
#' @return a data.table with ids of coordinated contents. Columns:
#' `object_id`, `id_user`, `id_user_y`, `content_id`, `content_id_y`,
#' `timedelta`
#'
#' @import data.table
#' @export


detect_coordinated_groups <- function(x,
                                      time_window = 10,
                                      min_repetition = 2) {
  # This function is a wrapper for actual calculation
  # We validate the input data before we go ahead
  # the actual functions are do_detect_coordinated_groups and
  # calc_group_combinations
  if (!inherits(x, "data.table")) {
    x <- data.table::as.data.table(x)
  }
  required_cols <- c("object_id", "id_user", "content_id", "timestamp_share")

  for (cname in required_cols) {
    if (!cname %in% colnames(x)) {
      stop("Columns or their names are incorrect.
      Ensure your data has the columns:
      object_id, id_user, content_id, timestamp_share")
    }
  }

  # TODO: add more assertions here. E.g., content_id is unique

  x <- do_detect_coordinated_groups(x,
    time_window = time_window,
    min_repetition = min_repetition
  )

  return(x)
}

#' do_detect_coordinated_groups
#'
#' @description
#' Private function that actually performs \link{detect_coordinated_groups}.
#'
#' @param x a data.table with the columns: `object_id` (uniquely identifies
#' coordinated content), `id_user` (unique ids for users), `content_id`
#' (id of user generated content), `timestamp_share` (integer)
#'
#' @param time_window the number of seconds within which shared contents
#' are to be considered as coordinated (default to 10 seconds).
#'
#' @param min_repetition the minimum number of published coordinated
#' contents necessary for a user to be included it in the coordinated
#' network. (defaults to 2)
#'
#' @return a data.table with ids of coordinated contents. Columns:
#' `object_id`, `id_user`, `id_user_y`, `content_id`, `content_id_y`,
#' `timedelta`
#'
#' @import data.table
#' @noRd

do_detect_coordinated_groups <- function(x,
                                         time_window = 10,
                                         min_repetition = 2) {
  object_id <- id_user <- content_id <- content_id_y <- NULL

  # --------------------------
  # Pre-filter
  # pre-filter based on minimum repetitions
  # a user must have tweeted a minimum number of times
  # before they can be considered coordinated
  x <- x[, if (.N > min_repetition) .SD, by = id_user]
  
  # a object has to be shared at least the times of min_rep
  x <- x[, if (.N > min_repetition) .SD, by = object_id]

  # ---------------------------
  # calculate time differences per group

  result <- x[,
    calc_group_combinations(.SD, time_window = time_window),
    by = object_id
  ]


  # ---------------------------
  # filter by minimum repetition
  # first get all content_ids that are flagged as coordinated
  coordinated_content_ids <- unique(
    c(
      unique(result$content_id),
      unique(result$content_id_y)
    )
  )

  # group input data by id_user,
  # then filter only the rows, where content_id is flagged as coordinated
  # finally, count by groups (id_user) and
  # only return rows with more than min_repetitions
  filt <- x[content_id %in% coordinated_content_ids,
    if (.N > min_repetition) .SD,
    by = id_user
  ]

  # filter the result to only contain content_ids from above
  result <- result[(content_id %in% filt$content_id) | (content_id_y %in% filt$content_id)]

  # ---------------------------
  # remove loops
  result <- result[object_id != content_id]
  result <- result[object_id != content_id_y]
  result <- result[content_id != content_id_y]

  # TODO: remove self-coordinated loops

  return(result)
}
