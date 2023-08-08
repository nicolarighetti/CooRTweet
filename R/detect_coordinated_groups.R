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
#' `timedelta`. The `id_user` and `content_id` represent the "older"
#' data points, `id_user_y` and `content_id_y` represent the "newer"
#' data points. For example, User A retweets from User B, then User A's
#' content is newer (i.e., `id_user_y`).
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
  object_id <- id_user <- content_id <- content_id_y <-
    id_user_y <- time_delta <- NULL

  # --------------------------
  # Pre-filter
  # pre-filter based on minimum repetitions
  # a user must have tweeted a minimum number of times
  # before they can be considered coordinated
  x <- x[, if (.N > min_repetition) .SD, by = id_user]

  # ---------------------------
  # calculate time differences per group

  result <- x[,
    calc_group_combinations(.SD, time_window = time_window),
    by = object_id
  ]


  # ---------------------------
  # filter by minimum repetition
  if (min_repetition > 1) {
    result <- filter_min_repetition(x, result, min_repetition)
  }

  # ---------------------------
  # remove loops
  result <- remove_loops(result)

  # ---------------------------
  # Sort output: content_id should be older than content_id_y
  # Therefore, we swap all values with positive time_delta
  # and return the absolute value

  result[
    time_delta > 0,
    c("content_id", "content_id_y", "id_user", "id_user_y") :=
      .(content_id_y, content_id, id_user_y, id_user)
  ]
  result[, time_delta := abs(time_delta)]

  return(result)
}


filter_min_repetition <- function(x, result, min_repetition) {
  content_id <- id_user <- content_id_y <- NULL
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

  return(result)
}

remove_loops <- function(result) {
  object_id <- content_id <- id_user <- content_id_y <- id_user_y <- NULL
  # remove loops
  if ("object_id" %in% colnames(result)) {
    result <- result[object_id != content_id]
    result <- result[object_id != content_id_y]
  }
  result <- result[content_id != content_id_y]
  result <- result[id_user != id_user_y]

  return(result)
}

#' detect_similar_text
#'
#' @description
#' Private (?) function that performs coordination detection
#' based on text similarity. 
#'
#'

detect_similar_text <- function(x,
                                min_repetition = 2,
                                time_window = 10,
                                min_similarity = 0.8) {
  require("textreuse")

  # https://cran.r-project.org/web/packages/textreuse/vignettes/textreuse-minhash.html
  minhash <- textreuse::minhash_generator(n = 240, seed = 3552)


  texts <- x$object_id
  names(texts) <- x$content_id

  corpus <- textreuse::TextReuseCorpus(
    text = texts,
    tokenizer = textreuse::tokenize_ngrams,
    n = 5,
    minhash_func = minhash,
    keep_tokens = TRUE,
    progress = TRUE,
    # skip_short = FALSE,
  )


  buckets <- textreuse::lsh(corpus, bands = 80, progress = TRUE)
  candidates <- textreuse::lsh_candidates(buckets)

  result <- data.table(
    textreuse::lsh_compare(
      candidates, 
      corpus, 
      textreuse::jaccard_similarity, 
      progress = TRUE))
  result_a <- result[, .(a, score)]
  result_b <- result[, .(b, score)]


  setnames(result_a, "a", "content_id")
  setnames(result_b, "b", "content_id")


  cotweet_pairs_x <- x[result_a, .(content_id, id_user, timestamp_share, score), on = "content_id"]
  cotweet_pairs_y <- x[result_b, .(content_id, id_user, timestamp_share), on = "content_id"]


  setnames(
    cotweet_pairs_y,
    c("content_id", "id_user", "timestamp_share"),
    c("content_id_y", "id_user_y", "timestamp_share_y")
  )


  cotweet_pairs <- cbind(cotweet_pairs_x, cotweet_pairs_y)

  setnames(cotweet_pairs, "score", "similarity_score")

  coordinated_cotweets <- cotweet_pairs[
    similarity_score >= min_similarity,
    time_delta := timestamp_share - timestamp_share_y
  ][abs(time_delta) <= time_window]

  # filter by minimum repetition
  coordinated_cotweets <- filter_min_repetition(x, coordinated_cotweets, min_repetition)

  # filter out loops

  coordinated_cotweets <- remove_loops(coordinated_cotweets)


  # ---------------------------
  # Sort output: content_id should be older than content_id_y
  # Therefore, we swap all values with positive time_delta
  # and return the absolute value

  coordinated_cotweets[
    time_delta > 0,
    c("content_id", "content_id_y", "id_user", "id_user_y") :=
      .(content_id_y, content_id, id_user_y, id_user)
  ]
  coordinated_cotweets[, time_delta := abs(time_delta)]

  return(coordinated_cotweets)
}
