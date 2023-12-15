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
#' coordinated content), `id_user` (unique ids for users), `content_id`
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
#' @return a data.table with ids of coordinated contents. Columns:
#' `object_id`, `id_user`, `id_user_y`, `content_id`, `content_id_y`,
#' `timedelta`. The `id_user` and `content_id` represent the "older"
#' data points, `id_user_y` and `content_id_y` represent the "newer"
#' data points. For example, User A retweets from User B, then User A's
#' content is newer (i.e., `id_user_y`).
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
  required_cols <- c("object_id", "id_user", "content_id", "timestamp_share")

  for (cname in required_cols) {
    if (!cname %in% colnames(x)) {
      stop("Either the columns or their names are incorrect.
           Please ensure your data includes the columns
           'object_id', 'id_user', 'content_id', and 'timestamp_share',
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
#' coordinated content), `id_user` (unique ids for users), `content_id`
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
#' `object_id`, `id_user`, `id_user_y`, `content_id`, `content_id_y`,
#' `timedelta`
#'
#' @import data.table
#' @noRd


do_detect_groups <- function(x,
                             time_window = 10,
                             min_participation = 2,
                             remove_loops = TRUE) {

  object_id <- id_user <- content_id <- content_id_y <-
    id_user_y <- time_delta <- NULL

  # --------------------------
  # Pre-filter
  # pre-filter based on minimum repetitions
  # a user must have tweeted a minimum number of times
  # before they can be considered coordinated
  x <- x[, if (.N >= min_participation) .SD, by = id_user]

  # --------------------------
  # strings to factors
  x[,
     c('object_id', 'id_user', 'content_id') := lapply(.SD, as.factor),
     .SDcols = c('object_id', 'id_user', 'content_id')]


  # ---------------------------
  # calculate time differences per group

  result <- x[,
    calc_group_combinations(.SD, time_window = time_window),
    by = object_id
  ]

  # ----------------------------
  # factors back to string
  result[, c("object_id", "content_id", "content_id_y", "id_user", "id_user_y") := lapply(.SD, as.character),
         .SDcols = c("object_id", "content_id", "content_id_y", "id_user", "id_user_y")]

  # ---------------------------
  # remove loops
  if(remove_loops == TRUE){
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
    c("content_id", "content_id_y", "id_user", "id_user_y") :=
      .(content_id_y, content_id, id_user_y, id_user)
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
  content_id <- id_user <- content_id_y <- NULL
  # ---------------------------
  # filter by minimum repetition
  # first get all content_ids
  content_ids <- unique(
    c(
      unique(result$content_id),
      unique(result$content_id_y)
    )
  )

  # group input data by id_user,
  # then filter only the rows in content_ids
  # finally, count by groups (id_user) and
  # only return rows with more than min_participation
  filt <- x[content_id %in% content_ids,
    if (.N >= min_participation) .SD,
    by = id_user
  ]

  # filter the result to only contain content_ids from above
  result <- result[(content_id %in% filt$content_id) | (content_id_y %in% filt$content_id)]

  return(result)
}


#' detect_similar_text
#'
#' This function detects coordinated cotweets, i.e. pairs of social media posts that
#' are similar in terms of their text and were posted within a short time window.
#'
#' @details Uses the [textreuse](https://cran.r-project.org/package=textreuse) package
#' to compare each post with each other and determine their text similarity. Use the
#' [reshape_tweets()] function with `intent = "cotweet"` parameter to prepare your data.
#'
#' @param x A data.table with the following columns:
#'   - content_id: The ID of the content (e.g. a tweet ID)
#'   - object_id: The text of the social media post
#'   - id_user: The ID of the user who shared the content
#'   - timestamp_share: The timestamp when the content was shared
#' @param min_participation the minimum number of repeated coordinated
#'   actions a user has to perform (defaults to 2 times)
#' @param time_window The maximum time difference between two posts in order
#'   for them to be considered coordinated cotweets (defaults to 10 seconds).
#' @param min_similarity The minimum similarity score between two posts in order
#'   for them to be considered coordinated cotweets (defaults to 0.8).
#' @param similarity_function The function that is used to calculate the similarity
#'   between two tweets. The default function is Jaccard Similarity (see: \link[textreuse]{jaccard_similarity}).
#' @param tokenizer The function that is used to tokenize the text of the tweets.
#'   The default function is the \link[textreuse]{tokenize_ngrams} function.
#' @param minhash_seed The seed that is used to generate the minhash signatures.
#'   If NULL, a random seed will be used.
#' @param minhash_n The number of minhash signatures that are used (see `textreuse` package for details).
#'
#' @return A data.table with the following columns:
#'   - content_id: The ID of the first post
#'   - content_id_y: The ID of the second post
#'   - id_user: The ID of the user who shared the first post
#'   - id_user_y: The ID of the user who shared the second post
#'   - timestamp_share: The timestamp when the first post was shared
#'   - timestamp_share_y: The timestamp when the second post was shared
#'   - similarity_score: The similarity score between the two posts
#'   - time_delta: The time difference between the two posts
#'
#' @import textreuse
#'
#' @export

detect_similar_text <- function(x,
                                min_participation = 2,
                                time_window = 10,
                                min_similarity = 0.8,
                                similarity_function = textreuse::jaccard_similarity,
                                tokenizer = textreuse::tokenize_ngrams,
                                minhash_seed = NULL,
                                minhash_n = 200) {

  a <- b <- score <- content_id <- id_user <-
    timestamp_share <- similarity_score <-
    time_delta <- timestamp_share_y <-
    content_id_y <- id_user_y <- NULL

  # Check arguments

  stopifnot(is.data.table(x))
  stopifnot(min_participation >= 1)
  stopifnot(time_window >= 0)
  stopifnot(min_similarity >= 0 && min_similarity <= 1)
  stopifnot(is.function(similarity_function))
  stopifnot(is.function(tokenizer))
  if (!is.null(minhash_seed)) {
    stopifnot(is.numeric(minhash_seed))
    stopifnot(length(minhash_seed) == 1)
  }
  if (!is.null(minhash_n)) {
    stopifnot(is.numeric(minhash_n))
    stopifnot(length(minhash_n) == 1)
    stopifnot(minhash_n >= 1)
  }


  # https://cran.r-project.org/web/packages/textreuse/vignettes/textreuse-minhash.html
  minhash <- textreuse::minhash_generator(n = minhash_n, seed = minhash_seed)

  texts <- x$object_id
  names(texts) <- x$content_id

  # Create a corpus object with the TextReuse package
  # Caveats:
  # - minimum length of n-grams cannot be specified
  #   due to a bug in textreuse (see: https://github.com/ropensci/textreuse/pull/80)
  # - short documents are currently skipped, meaning that every
  #   document with less than 3 tokens is ignored
  corpus <- textreuse::TextReuseCorpus(
    text = texts,
    tokenizer = tokenizer,
    minhash_func = minhash,
    keep_tokens = TRUE,
    progress = TRUE,
  )

  buckets <- textreuse::lsh(corpus, bands = 80, progress = TRUE)
  candidates <- textreuse::lsh_candidates(buckets)

  result <- data.table(
    textreuse::lsh_compare(
      candidates,
      corpus,
      similarity_function,
      progress = TRUE
    )
  )

  result_a <- result[, .(a, score)]
  result_b <- result[, .(b, score)]

  setnames(result_a, "a", "content_id")
  setnames(result_b, "b", "content_id")

  cotweet_pairs_x <- x[result_a,
    .(content_id, id_user, timestamp_share, score),
    on = "content_id"
  ]
  cotweet_pairs_y <- x[result_b,
    .(content_id, id_user, timestamp_share),
    on = "content_id"
  ]

  setnames(
    cotweet_pairs_y,
    c("content_id", "id_user", "timestamp_share"),
    c("content_id_y", "id_user_y", "timestamp_share_y")
  )

  cotweet_pairs <- cbind(cotweet_pairs_x, cotweet_pairs_y)

  setnames(cotweet_pairs, "score", "similarity_score")

  # filter by document similarity and time_window
  coordinated_cotweets <- cotweet_pairs[
    similarity_score >= min_similarity,
    time_delta := timestamp_share - timestamp_share_y
  ][abs(time_delta) <= time_window]

  # filter by minimum repetition
  coordinated_cotweets <-
    filter_min_repetition(x, coordinated_cotweets, min_participation)

  # filter out loops

  coordinated_cotweets <- do_remove_loops(coordinated_cotweets)

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
