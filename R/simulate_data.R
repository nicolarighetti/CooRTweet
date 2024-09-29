#' simulate_data
#'
#' @description
#' Create a simulated input and output of
#' \code{\link{detect_groups}} function.
#'
#' @details
#' This function generates a simulated dataset with fixed
#' numbers for coordinated accounts, uncoordinated accounts, and
#' shared objects. The user can set minimum participation and time window parameters
#' and the coordinated accounts will "act" randomly within these restrictions.
#'
#' The size of the resulting dataset can be adjusted with the lambda parameters.
#' If lambda is between 0.0 and 1.0, the dataset will be much smaller than
#' choosing lambdas greater than 1.
#'
#' @param n_accounts_coord the desired number of coordinated accounts
#'
#' @param n_accounts_noncoord the desired number of non-coordinated accounts
#'
#' @param n_objects the desired number of objects.
#'
#' @param min_participation the minimum number of repeated coordinated
#' action to define two accounts as coordinated.
#'
#' @param time_window the time window of coordination.
#'
#' @param approx_size the approximate size of the desired dataset. It automatically calculates the
#' lambdas passed to `rpois()`, which is the expected rate of occurrences. It only works when
#' lambda_coord and lambda_noncoord are NULL (default).
#'
#' @param lambda_coord `lambda` parameter for coordinated accounts passed to
#' `rpois()`, which is the expected rate of occurrences
#' (higher lambda means more coordinated shares).
#'
#' @param lambda_noncoord `lambda` parameter for non-coordinated accounts passed to
#' `rpois()`, which is the expected rate of occurrences
#' (higher lambda means more non-coordinated shares).
#'
#' @return a list with two data frames: a data frame
#' with the columns required by the function detect_
#' coordinated_groups (`object_id`, `account_id`, `content_id`, `timestamp_share`)
#' and the output table of the same
#' \link{detect_groups} function and columns:
#' `object_id`, `account_id`, `account_id_y`,
#' `content_id`, `content_id_y`, `time_delta`.
#'
#' @importFrom stringi stri_pad_left
#' @importFrom stats na.omit rpois
#' @importFrom data.table setDT as.data.table setnames rbindlist setkeyv
#'
#' @examples
#' # Example usage of simulate_data
#' \dontrun{
#' set.seed(123) # For reproducibility
#' simulated_data <- simulate_data(
#'   n_accounts_coord = 100,
#'   n_accounts_noncoord = 50,
#'   n_objects = 20,
#'   min_participation = 2,
#'   time_window = 10
#' )
#'
#' # Extract input
#' input_data <- simulated_data[[1]]
#'
#' # Extract output and keep coordinated actors.
#' # This is expected correspond to CooRTweet results from `detect_group`
#' simulated_results <- simulated_data[[2]]
#' simulated_results <- simulated_results[simulated_results$coordinated == TRUE, ]
#' simulated_results$coordinated <- NULL
#'
#' # Run CooRTweet using the input_data and the parameters used for simulation
#' results <- detect_groups(
#'   x = input_data,
#'   time_window = 10,
#'   min_participation = 2
#' )
#'
#' # Sort data tables and check whether they are identical
#' data.table::setkeyv(simulated_results, names(simulated_results))
#' data.table::setkeyv(results, names(simulated_results))
#'
#' identical(results, simulated_results)
#' }
#' @export
#'

simulate_data <- function(
    n_accounts_coord = 5,
    n_accounts_noncoord = 4,
    n_objects = 5,
    min_participation = 3,
    time_window = 10,
    approx_size = 200,
    lambda_coord = NULL,
    lambda_noncoord = NULL) {
  N <- object_id <- share_time_A <- share_time_B <- time_delta <- coordinated <-
    content_id <- content_id_y <- account_id <- account_id_y <- NULL
  
  # Determine lambdas ---------------------------
  if (!is.null(lambda_coord) && !is.null(lambda_noncoord) && !is.null(approx_size)) {
    warning("lambda_coord and lambda_noncoord are specified: approx_size parameter disabled")
  }

  # assert that parameters are valid
  if (n_accounts_coord < 1 || n_accounts_noncoord < 1 || n_objects < 1 || time_window < 1) {
    stop("One input parameter is 0 or negative, please check.")
  }

  if (is.null(lambda_coord) & is.null(lambda_noncoord)) {
    pairs_coord <- (n_accounts_coord * (n_accounts_coord - 1)) / 2
    pairs_noncoord <- (n_accounts_noncoord * (n_accounts_noncoord - 1)) / 2

    lambda_coord <- (approx_size / 4) / pairs_coord
    lambda_noncoord <- (approx_size / 4) / pairs_noncoord
  }


  # Create Account IDs --------------------------
  # create sets of IDs for both coordinated and non-coordinated accounts

  # account_ids ----

  # Create a set of account_ids of length n_accounts_coord + n_account_non_coord
  # and extract coord accounts and non coord accounts
  total_accounts <- n_accounts_coord + n_accounts_noncoord
  account_ids <- seq(1, total_accounts, 1)

  account_ids_coord <- account_ids[1:n_accounts_coord]
  account_ids_noncoord <- account_ids[
    (n_accounts_coord + 1):length(account_ids)
  ]

  # object_ids ----
  # These are the IDs of the content that is shared
  # in a coordinated fashion.

  object_ids <- seq(1, n_objects, 1)

  # Coordinated Behavior - Main structure -----------------

  # symmetric matrix of size n = n_accounts_coord and names in account_ids_coord
  coord_matrix <- matrix(0,
    nrow = n_accounts_coord, ncol = n_accounts_coord,
    dimnames = list(account_ids_coord, account_ids_coord)
  )

  # Repetitions sampled with Poisson distribution
  # which minimum is 0, so we add `min_participation`
  coord_matrix[upper.tri(coord_matrix)] <- rpois(
    sum(upper.tri(coord_matrix)), lambda_coord
  ) + min_participation

  # to data.table
  coord_matrix[lower.tri(coord_matrix)] <- NA
  diag(coord_matrix) <- NA
  # 0 to NAs, because they don't have any posts in common
  coord_matrix[coord_matrix == 0] <- NA

  df_coord <- na.omit(as.data.table(as.table(coord_matrix)))
  df_coord <- df_coord[rep(seq_len(nrow(df_coord)), df_coord$N), ]
  df_coord[, N := NULL]
  # rownames(df_coord) <- NULL
  setnames(df_coord, c("V1", "V2"), c("account_id", "account_id_y"))

  # add object ID
  df_coord[, object_id := sample(
    object_ids,
    size = nrow(df_coord),
    replace = TRUE
  )]

  # Non-Coordinated Behavior - Main structure ---------------

  # Use the same object_ids as above
  # symmetric matrix of size n = n_accounts_noncoord
  # and names in account_ids_noncoord

  noncoord_matrix <- matrix(0,
    nrow = n_accounts_noncoord, ncol = n_accounts_noncoord,
    dimnames = list(account_ids_noncoord, account_ids_noncoord)
  )

  # number of repeated tweets sampled with a poisson distribution
  # non-coordinated accounts can theoretically still have a high number
  # of repeated content. But they must not share it within a short
  # time window
  noncoord_matrix[upper.tri(noncoord_matrix)] <- rpois(
    sum(upper.tri(noncoord_matrix)), lambda_noncoord
  )

  # to data.table
  noncoord_matrix[lower.tri(noncoord_matrix)] <- NA
  diag(noncoord_matrix) <- NA
  # 0 to NAs (because these accounts have no content_ids in common)
  noncoord_matrix[noncoord_matrix == 0] <- NA
  df_noncoord <- na.omit(as.data.table(as.table(noncoord_matrix)))

  df_noncoord <- df_noncoord[rep(
    seq_len(nrow(df_noncoord)), df_noncoord$N
  ), ]

  df_noncoord[, N := NULL]
  # rownames(df_noncoord) <- NULL
  setnames(df_noncoord, c("V1", "V2"), c("account_id", "account_id_y"))

  # add object ID

  df_noncoord[, object_id := sample(
    object_ids,
    size = nrow(df_noncoord),
    replace = TRUE
  )]


  # Timestamps -----------------------

  # assign coordinated timestamps using systematic sampling where the
  # sampling interval is higher than time_interval,
  # to avoid unplanned links between accounts.
  # Assing the sampled timestamp to accounts' A shares,
  # then assign to accounts' B shares a timestamps equal to
  # the A interval + noise lower than time_window

  # Choose a random starting date for the simulation
  start_date <- as.Date(
    sample(as.Date("2000-01-01"):as.Date("2020-12-31"), 1),
    origin = "1970-01-01"
  )

  # the first coordinated timestamp is at t0
  # all subsequent timestamps are counted from here
  t0 <- as.numeric(as.POSIXct(start_date))


  # the sampling interval is more than 2x time_window to avoid overlaps
  sampling_interval <- (2 * time_window) + 1

  # start at t0 and then generate timestamps in the interval set above
  df_coord[, share_time_A := seq(from = t0, by = sampling_interval, length.out = nrow(df_coord))]

  # add share time to B adding number of seconds < time_interval
  df_coord[, time_delta := sample(0:time_window, nrow(df_coord), replace = TRUE)]
  df_coord[, share_time_B := share_time_A + time_delta]

  # mark as coordinated
  df_coord[, coordinated := TRUE]

  # non-coordinated timestamps:
  # accounts could share the same content with an arbitrary time_delta
  # it has to be higher than time_window, so we roughly make it twice as large
  # minimum is time_window + 1, maximum is 2x time_window + 1
  noise <- 1 + time_window +
    sample.int(time_window, nrow(df_noncoord), replace = TRUE)

  # the sampling interval is needs to be larger than the noise
  sampling_interval <- max(noise) * 2

  # extract timestamps from coordinated timestamp set and with intervals
  # exceeding time_window.
  # Uncoordinated accounts start their time stamps at the last timestamp
  # of the coordinated accounts.

  t1 <- max(df_coord$share_time_B) + sampling_interval

  # now we sample timestamps for account A with a larger
  # interval than coordinated accounts


  df_noncoord[, share_time_A := seq(
    from = t1,
    by = sampling_interval,
    length.out = nrow(df_noncoord)
  )]
  df_noncoord[, share_time_B := share_time_A + noise]
  df_noncoord[, time_delta := abs(share_time_A - share_time_B)]

  # mark as non coordinated
  df_noncoord$coordinated <- as.logical("FALSE")

  df_noncoord[, coordinated := FALSE]
  output_table <- data.table::rbindlist(list(df_coord, df_noncoord), use.names = TRUE)


  # shares IDs -----------------

  output_table[, content_id := seq(1, nrow(output_table), 1)]
  output_table[, content_id_y := seq(nrow(output_table) + 1, by = 1, length.out = nrow(output_table))]

  # subset and rename accordingly to the detect_coordinated_groups output
  output_table <- output_table[
    ,
    c(
      "object_id", "content_id", "content_id_y",
      "time_delta", "account_id", "account_id_y", "coordinated",
      "share_time_A", "share_time_B"
    )
  ]

  output_table[,
    c("object_id", "content_id", "content_id_y", "account_id", "account_id_y") := lapply(.SD, as.character),
    .SDcols = c("object_id", "content_id", "content_id_y", "account_id", "account_id_y")
  ]

  # emergency check in case simulated data contains errors
  if (any(is.na(output_table))) {
    stop("Simulated data contains NAs!")
  }

  # Create consistent, zero padded IDs
  output_table[
    ,
    object_id := paste0("object_", stringi::stri_pad_left(
      object_id,
      width = max(nchar(n_objects)), pad = "0"
    ))
  ]

  output_table[
    ,
    account_id := paste0("account_", stringi::stri_pad_left(
      account_id,
      width = max(nchar(max(account_ids))), pad = "0"
    ))
  ]

  output_table[
    ,
    account_id_y := paste0("account_", stringi::stri_pad_left(
      account_id_y,
      width = max(nchar(max(account_ids))), pad = "0"
    ))
  ]

  output_table[
    ,
    content_id := paste0("share_", stringi::stri_pad_left(
      content_id,
      width = max(nchar(max(content_id_y))), pad = "0"
    ))
  ]

  output_table[
    ,
    content_id_y := paste0("share_", stringi::stri_pad_left(
      content_id_y,
      width = max(nchar(max(content_id_y))), pad = "0"
    ))
  ]


  # Input data  -------------------------
  # (Created by reshaping the output_table)
  input_dataset <-
    unique(data.table(
      object_id = c(
        output_table$object_id,
        output_table$object_id
      ),
      account_id = c(
        output_table$account_id,
        output_table$account_id_y
      ),
      content_id = c(
        output_table$content_id,
        output_table$content_id_y
      ),
      timestamp_share = c(
        output_table$share_time_A,
        output_table$share_time_B
      ),
      stringsAsFactors = FALSE
    ))

  # drop unecessary columns share_time_A/share_time_B
  output_table <- output_table[, -c(8:9)]

  output_list <- list(input_dataset, output_table)

  gc()

  return(output_list)
}
