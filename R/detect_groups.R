#' detect_groups
#'
#' @description
#' Function to perform the initial stage in detecting coordinated behavior, identify pairs of users
#' that share the same objects in a time_window.
#' See details.
#'
#' @details This function achieves the initial stage in detecting coordinated behavior
#' by identifying users who share identical objects within the same temporal window, and is
#' preliminary to the network analysis conducted using the `generate_network` function.
#' The function groups the data by `object_id` (uniquely identifies
#' coordinated content) and calculates the time differences between all
#' `content_id` (ids of user generated contents) within their groups.
#' It then filters out all `content_id` that are higher than the `time_window`
#' (in seconds). It returns a `data.table` with all IDs of coordinated
#' contents. The `object_id` can be for example: hashtags, IDs of tweets being
#' retweeted, or URLs being shared.
#'
#' @param x a data.table with the columns: `object_id` (uniquely identifies
#' coordinated content), `account_id` (unique ids for users), `content_id`
#' (id of user generated content), `timestamp_share` (integer)
#'
#' @param time_window the number of seconds within which shared contents
#' are to be considered as coordinated (default to 10 seconds).
#'
#' @param min_participation The minimum number of actions within a specified timeframe
#' required for a user to be included in subsequent analysis (default set at two).
#' This criterion in network analysis corresponds with the concept of degree.
#' It is important to differentiate this from the frequency of repeated interactions
#' a user has with a particular other user, which is represented by edge weight.
#' The edge weight parameter is utilized in the `generate_network` function as a
#' concluding step in identifying coordinated behavior.
#'
#' @param remove_loops Should loops (shares of the same objects made by the same user
#' within the time window) be removed? (default to TRUE).
#'
#' @param ... keyword arguments for backwards compatibility.
#'
#' @return a data.table with ids of coordinated contents. Columns:
#' `object_id`, `account_id`, `account_id_y`, `content_id`, `content_id_y`,
#' `timedelta`. The `account_id` and `content_id` represent the "older"
#' data points, `account_id_y` and `content_id_y` represent the "newer"
#' data points. For example, User A retweets from User B, then User A's
#' content is newer (i.e., `account_id_y`).
#'
#' @import data.table
#' @export


detect_groups <- function(x,
                          time_window = 10,
                          min_participation = 2,
                          remove_loops = TRUE,
                          ...) {
  # This function is a wrapper for actual calculation
  # We validate the input data before we go ahead
  # the actual functions are do_detect_groups and
  # calc_group_combinations
  if (!inherits(x, "data.table")) {
    x <- data.table::as.data.table(x)
  }
  required_cols <- c("object_id", "account_id", "content_id", "timestamp_share")

  if ("id_user" %in% colnames(x)) {
    data.table::setnames(x, "id_user", "account_id")
    warning("Your data contained the column `id_user`, this name is deprecated, renamed it to `account_id`")
  }

  for (cname in required_cols) {
    if (!cname %in% colnames(x)) {
      stop("Either the columns or their names are incorrect.
           Please ensure your data includes the columns
           'object_id', 'account_id', 'content_id', and 'timestamp_share',
           or use the 'prep_data' function to prepare the dataset.")
    }
  }

  # Check for additional unnamed arguments (compatibility with min_repetition)
  additional_args <- list(...)
  if ("min_repetition" %in% names(additional_args)) {
    warning("The argument 'min_repetition' is deprecated. Use 'min_participation' instead.")
    min_participation <- additional_args$min_repetition
  }

  # TODO: add more assertions here. E.g., content_id is unique

  x <- do_detect_groups(x,
    time_window = time_window,
    min_participation = min_participation,
    remove_loops = remove_loops
  )

  return(x)
}

#' do_detect_groups
#'
#' @description
#' Private function that actually performs \link{detect_groups}.
#'
#' @param x a data.table with the columns: `object_id` (uniquely identifies
#' coordinated content), `account_id` (unique ids for users), `content_id`
#' (id of user generated content), `timestamp_share` (integer)
#'
#' @param time_window the number of seconds within which shared contents
#' are to be considered as coordinated (default to 10 seconds).
#'
#' @param min_participation the minimum number of published coordinated
#' contents necessary for a user to be included it in the coordinated
#' network. (defaults to 2)
#'
#' @return a data.table with ids of coordinated contents. Columns:
#' `object_id`, `account_id`, `account_id_y`, `content_id`, `content_id_y`,
#' `timedelta`
#'
#' @import data.table
#' @noRd


do_detect_groups <- function(x,
                             time_window = 10,
                             min_participation = 2,
                             remove_loops = TRUE) {
  object_id <- account_id <- content_id <- content_id_y <-
    account_id_y <- time_delta <- NULL

  # --------------------------
  # Pre-filter
  # pre-filter based on minimum repetitions
  # a user must have tweeted a minimum number of times
  # before they can be considered coordinated
  x <- x[, if (.N >= min_participation) .SD, by = account_id]

  # --------------------------
  # strings to factors
  x[,
    c("object_id", "account_id", "content_id") := lapply(.SD, as.factor),
    .SDcols = c("object_id", "account_id", "content_id")
  ]


  # ---------------------------
  # calculate time differences per group

  result <- x[,
    calc_group_combinations(.SD, time_window = time_window),
    by = object_id
  ]

  # ----------------------------
  # factors back to string
  result[, c("object_id", "content_id", "content_id_y", "account_id", "account_id_y") := lapply(.SD, as.character),
    .SDcols = c("object_id", "content_id", "content_id_y", "account_id", "account_id_y")
  ]

  # ---------------------------
  # remove loops
  if (remove_loops == TRUE) {
    result <- do_remove_loops(result)
  }

  # ---------------------------
  # filter by minimum repetition
  if (min_participation >= 1) {
    result <- filter_min_participation(x, result, min_participation)
  }

  # ---------------------------
  # Sort output: content_id should be older than content_id_y
  # Therefore, we swap all values with positive time_delta
  # and return the absolute value

  result[
    time_delta > 0,
    c("content_id", "content_id_y", "account_id", "account_id_y") :=
      .(content_id_y, content_id, account_id_y, account_id)
  ]
  result[, time_delta := abs(time_delta)]

  return(result)
}

#' Remove loops from the result.
#'
#' This function is a private utility function that removes loops (i.e., users
#' sharing their own content) from the result.
#'
#' @param result The result of the previous filtering steps.
#'
#' @return The result with loops removed.
#'
#'

do_remove_loops <- function(result) {
  object_id <- content_id <- account_id <- content_id_y <- account_id_y <- NULL
  # remove loops
  if ("object_id" %in% colnames(result)) {
    result <- result[object_id != content_id]
    result <- result[object_id != content_id_y]
  }
  result <- result[content_id != content_id_y]
  result <- result[account_id != account_id_y]

  return(result)
}

#' Filter the result by minimum participation
#'
#' This private function filters the result by the minimum number of participation required.
#'
#' @param x A data table from a coordination detection function
#' @param result A data table containing the result data.
#' @param min_participation The minimum repetition threshold. Users with repetition count
#'                          greater than this threshold will be retained.
#'
#' @return A data table with filtered rows based on the specified minimum participation.
#'
#' @import data.table

filter_min_participation <- function(x, result, min_participation) {
  content_id <- account_id <- content_id_y <- NULL
  # ---------------------------
  # filter by minimum repetition
  # first get all content_ids
  content_ids <- unique(
    c(
      unique(result$content_id),
      unique(result$content_id_y)
    )
  )

  # group input data by account_id,
  # then filter only the rows in content_ids
  # finally, count by groups (account_id) and
  # only return rows with more than min_participation
  filt <- x[content_id %in% content_ids,
    if (.N >= min_participation) .SD,
    by = account_id
  ]

  # filter the result to only contain content_ids from above
  result <- result[(content_id %in% filt$content_id) | (content_id_y %in% filt$content_id)]

  return(result)
}