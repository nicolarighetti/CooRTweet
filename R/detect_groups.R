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
#' (in seconds). It returns a `data.frame` with all IDs of coordinated
#' contents. The `object_id` can be for example: hashtags, IDs of tweets being
#' retweeted, or URLs being shared. For twitter data, best use \link{reshape_tweets}.
#'
#' @param x a data.frame (or comparable) with the columns: `object_id` (uniquely identifies
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
#' @param remove_loops Ignored in polars branch! (default to TRUE).
#'
#' @param ... keyword arguments for backwards compatibility.
#'
#' @return a data.table (!) with ids of coordinated contents. Columns:
#' `object_id`, `account_id`, `account_id_y`, `content_id`, `content_id_y`,
#' `timedelta`. The `account_id` and `content_id` represent the "older"
#' data points, `account_id_y` and `content_id_y` represent the "newer"
#' data points. For example, account A retweets from account B, then account A's
#' content is newer (i.e., `account_id_y`).
#'
#' @import data.table polars
#' @importFrom polars pl
#' @export


detect_groups <- function(x,
                          time_window = 10,
                          min_participation = 2,
                          remove_loops = TRUE,
                          ...) {
  # We validate the input data before we go ahead
  if (remove_loops == FALSE) {
    warning("Loops are always removed in the polars branch. Ignoring parameter")
  }
  pl <- polars::pl

  required_cols <- c("object_id", "account_id", "content_id", "timestamp_share")

  x <- pl$DataFrame(x)

  if ("id_account" %in% colnames(x)) {
    x <- x$rename(id_account="account_id")
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

  # --------------------------
  # Pre-filter
  # pre-filter based on minimum participation
  # an account must have tweeted a minimum number of times
  # before they can be considered in subsequent analysis
  # yes, this expression is more complicatated in polars

  x <- x$filter(pl$col("account_id")$is_in(
    x$select(
      pl$col("account_id")$value_counts()
    )$unnest()$filter(
      pl$col("count") >= min_participation)$get_column("account_id")
  ))

  # --------------------------
  # strings to factors
  x <- x$with_columns(
    pl$col(c("account_id", "object_id", "content_id"))$cast(pl$Categorical()),
    pl$col("timestamp_share")$cast(pl$Int64)
  )

  # ---------------------------
  # calculate time differences per group
  x <- x$with_row_index("index")$lazy() # switch to lazy evaluation of polars
  result <- x$join(
    x, 
    on = "object_id", 
    suffix = "_y"
    )$filter(pl$col("account_id") != pl$col("account_id_y")
    )$filter(pl$col("index") > pl$col("index_y")
    )$with_columns(
        (pl$col("timestamp_share") - pl$col("timestamp_share_y"))$alias("time_delta")
    )$filter(
      pl$col("time_delta")$abs() <= time_window
    )$drop("timestamp_share", "timestamp_share_y", "index", "index_y")

  # Executes the query
  result <- result$collect()

  # ---------------------------
  # refine the filtering by minimum participation
  content_ids <- pl$concat(
    result$get_column("content_id"), result$get_column("content_id_y"), how = "vertical"
    )$unique()

  account_ids <- x$filter(
      pl$col("content_id")$is_in(content_ids)
    )$select(
      pl$col("account_id")$value_counts()
    )$unnest(
    )$filter(
        pl$col("count") >= min_participation
    )$collect()$get_column("account_id")
  
  result <- result$filter(pl$col("account_id")$is_in(account_ids) | pl$col("account_id_y")$is_in(account_ids))

  # ---------------------------
  # Sort output: content_id should be older than content_id_y
  # Therefore, we swap all values with positive time_delta
  # and return the absolute value

  result <- pl$concat(
      result$filter(pl$col("time_delta") > 0)$rename(
          content_id = "content_id_y", 
          account_id = "account_id_y", 
          content_id_y = "content_id", 
          account_id_y = "account_id"
          ),
      result$filter(pl$col("time_delta") <= 0
          )$with_columns(pl$col("time_delta")$abs()),
      how = "diagonal"
  )

  # ----------------------------
  # factors back to string
  result <- result$with_columns(
    pl$col(
      c("object_id", "content_id", "content_id_y", "account_id", "account_id_y")
    )$cast(pl$String))


  return(data.table::as.data.table(result$to_data_frame()))
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