#' detect_groups
#'
#' @description
#' Function to perform the initial stage in detecting coordinated behavior.
#' It identifies pairs of accounts that share the same objects in a time_window.
#' See details.
#'
#' @details This function achieves the initial stage in detecting coordinated
#' behavior by identifying accounts who share identical objects within the same
#' temporal window, and is preliminary to the network analysis conducted using
#' the \link{generate_coordinated_network} function.
#' `detect_groups` groups the data by `object_id` (uniquely identifies
#' content) and calculates the time differences between all
#' `content_id` (ids of account generated contents) within their groups.
#' It then filters out all `content_id` that are higher than the `time_window`
#' (in seconds). It returns a `data.table` with all IDs of coordinated
#' contents. The `object_id` can be for example: hashtags, IDs of tweets being
#' retweeted, or URLs being shared. For twitter data, best use \link{reshape_tweets}.
#'
#' @param x a data.table with the columns: `object_id` (uniquely identifies
#' coordinated content), `account_id` (unique ids for accounts), `content_id`
#' (id of account generated content), `timestamp_share` (integer). See also
#' \link{reshape_tweets} and \link{prep_data}.
#'
#' @param time_window the number of seconds within which shared contents
#' are to be considered as coordinated (default to 10 seconds).
#'
#' @param min_participation The minimum number of actions required for a account
#' to be included in subsequent analysis (default set at 2). This ensures that
#' only accounts with a minimum level of activity in the original dataset are
#' included in subsequent analysis. It is important to distinguish this from the
#' frequency of repeated interactions an account has with another specific account,
#' as represented by edge weight. The edge weight parameter is utilized in the
#' `generate_coordinated_network` function as a concluding step in identifying
#' coordinated behavior.
#'
#' @param remove_loops Should loops (shares of the same objects made by the same
#' account within the time window) be removed? (default to TRUE).
#'
#' @param ... keyword arguments for backwards compatibility.
#'
#' @return a data.table with ids of coordinated contents. Columns:
#' `object_id`, `account_id`, `account_id_y`, `content_id`, `content_id_y`,
#' `timedelta`. The `account_id` and `content_id` represent the "older"
#' data points, `account_id_y` and `content_id_y` represent the "newer"
#' data points. For example, account A retweets from account B, then account A's
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

  if ("id_account" %in% colnames(x)) {
    data.table::setnames(x, "id_account", "account_id")
    warning("Your data contained the column `id_account`, this name is deprecated, renamed it to `account_id`")
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
#' coordinated content), `account_id` (unique ids for accounts), `content_id`
#' (id of account generated content), `timestamp_share` (integer)
#'
#' @param time_window the number of seconds within which shared contents
#' might be considered as coordinated (default to 10 seconds).
#'
#' @param min_participation the minimum number of published contents necessary
#' for a account to be included it in subsequent analysis (defaults to 2).
#'
#' @return a data.table with ids of contents. Columns:
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
  # pre-filter based on minimum participation
  # a account must have tweeted a minimum number of times
  # before they can be considered in subsequent analysis
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
  # refine the filtering by minimum participation
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
#' This function is a private utility function that removes loops (i.e., accounts
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
#' @param x The original data table where a preliminary filter is applied
#' @param result A data table containing the result data from calc_group_combinations.
#' @param min_participation The minimum activity threshold. accounts with participation count
#'                          greater than this threshold will be retained in the final 'result' table.
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
