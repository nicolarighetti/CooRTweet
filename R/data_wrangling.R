#' data_wrangling
#'
#' Preprocessing data to create a dataframe ready for coordination analysis.
#' Specifically, this function filters and groups the original dataset keeping just the type of tweets (e.g., retweets, replies, etc.) relevant to the specific coordination function of interest.
#'
#' @param tweets the data frame returned from \link[academictwitteR]{bind_tweets}.
#' @param coord_function the coordination analysis function that will be used with the preprocessessed data.
#'
#' @return a dataframe of tweets ready for coordination analysis.
#'
#' @importFrom tidyr unnest_wider separate_rows unnest
#' @importFrom dplyr group_by rename summarize mutate case_when distinct filter
#' @importFrom stringr str_starts
#' @importFrom rlang :=

utils::globalVariables(
  c(
    "tweets",
    "author_id",
    "id",
    "created_at",
    "text",
    "public_metrics",
    "referenced_tweets",
    "in_reply_to_user_id",
    "conversation_id",
    "entities",
    "entities_hashtags",
    "tag",
    "end",
    "type",
    "entities_urls",
    "expanded_url",
    "mentions",
    "urls",
    "hashtags"
  )
)

data_wrangling <- function(tweets,
                           coord_function,
                           reply_type,
                           min_repetition) {
  # de-duplicate
  tweets <- tweets |>
    dplyr::rename(tweet_id = id) |>
    dplyr::distinct(tweet_id, .keep_all = TRUE)

  # manipulate and group data by variable of interest
  if (coord_function == "get_coretweet") {
    dset_rt <- tweets |>
      # makes data wider by expanding df-columns
      tidyr::unpack(cols = c(entities, public_metrics)) |>
      # Unnest list-columns (drop null)
      tidytable::unnest.(referenced_tweets, keep_empty = TRUE, .drop = FALSE) |>
      dplyr::rename(referenced_tweet_id = id) |>
      dplyr::filter(type == "retweeted") |>
      # Unnest list-columns into columns and rows (when tweets include multiple mentions)
      tidytable::unnest.(mentions, keep_empty = TRUE, .drop = FALSE) |>
      # keep mentions to retweeted users, drop the others
      dplyr::filter(start == 3) |>
      # convert to date-time format
      dplyr::mutate(created_at = lubridate::as_datetime(created_at, tz = "UTC")) |>
      dplyr::mutate(rt_datetime = as.numeric(created_at)) |>
      # group content
      dplyr::group_by(referenced_tweet_id) |>
      dplyr::mutate(group_id = dplyr::cur_group_id())
  }

  if (coord_function == "get_cotweet") {
    dset_rt <- tweets |>
      tidyr::unpack(cols = c(entities, public_metrics)) |>
      tidyr::hoist(.col = referenced_tweets,
                   type = "type",
                   referenced_tweet_id = "id") |>
      dplyr::filter(type == "NULL")

    if (nrow(dset_rt) == 0) {
      message("\n### No enough tweets in the dataset ####\n")
      opt <- options(show.error.messages = FALSE)
      on.exit(options(opt))
      stop()
    }

    dset_rt <- dset_rt |>
      # convert to date-time format
      dplyr::mutate(created_at = lubridate::as_datetime(created_at, tz = "UTC")) |>
      dplyr::mutate(rt_datetime = as.numeric(created_at)) |>
      dplyr::group_by(text) |>
      dplyr::mutate(group_id = dplyr::cur_group_id())
  }

  if (coord_function == "get_coreply") {
    dset_rt <- tweets |>
      tidyr::unpack(cols = c(entities, public_metrics)) |>
      # Unnest list-columns (drop null)
      tidytable::unnest.(referenced_tweets, keep_empty = TRUE, .drop = FALSE) |>
      dplyr::rename(referenced_tweet_id = id) |>
      dplyr::filter(type == "replied_to") |>
      dplyr::group_by(if (reply_type == "same_text")
        text
        else if (reply_type == "same_user")
          in_reply_to_user_id) |>
      # convert to date-time format
      dplyr::mutate(created_at = lubridate::as_datetime(created_at, tz = "UTC")) |>
      dplyr::mutate(rt_datetime = as.numeric(created_at)) |>
      # group by
      dplyr::mutate(group_id = dplyr::cur_group_id()) |>
      dplyr::rename({{reply_type}} := "if (reply_type == \"same_text\") text else if (reply_type == \"same_user\") in_reply_to_user_id")

  }

  if (coord_function == "get_clsb") {
    dset_rt <- tweets |>
      # keeps only original tweets
      tidyr::unpack(cols = c(entities, public_metrics)) |>
      tidyr::hoist(.col = referenced_tweets,
                   type = "type",
                   referenced_tweet_id = "id") |>
      dplyr::filter(type == "NULL")

      if (nrow(dset_rt) == 0) {
        message("\n### No URLs in the dataset ####\n")
        opt <- options(show.error.messages = FALSE)
        on.exit(options(opt))
        stop()
      }

    dset_rt <- dset_rt |>
      tidyr::hoist(urls,
                   url = "url",
                   expanded_url = "expanded_url",
                   start = "start") |>
      tidyr::unnest_longer(c("url", "expanded_url", "start")) |>
      dplyr::filter(!is.na(url)) |>
      # remove Twitter's internal URLs
      dplyr::filter(stringr::str_starts(expanded_url, "https://twitter.com/", negate = TRUE)) |>
      # remove duplicates
      dplyr::distinct(tweet_id, url, .keep_all = TRUE) |>
      # convert to date-time format
      dplyr::mutate(created_at = lubridate::as_datetime(created_at, tz = "UTC")) |>
      dplyr::mutate(rt_datetime = as.numeric(created_at)) |>
      # group by
      dplyr::group_by(url) |>
      dplyr::mutate(group_id = dplyr::cur_group_id())
  }

  if (coord_function == "get_cohashtag") {
    dset_rt <- tweets |>
      tidyr::unpack(cols = c(entities, public_metrics)) |>
      # Unnest list-columns (drop null)
      tidytable::unnest.(referenced_tweets, keep_empty = TRUE, .drop = FALSE) |>
      dplyr::rename(referenced_tweet_id = id) |>
      # remove retweets
      dplyr::filter(type != "retweeted") |>
      tidyr::unnest_wider(hashtags) |>
      # unnest hashtags and drop null
      tidytable::unnest.(c(start, end, tag)) |>
      ## if keeps single hashtags, remove hashtags used more than once in the same tweet
      dplyr::distinct(tweet_id, tag, .keep_all = TRUE) |>
      # convert to date-time format
      dplyr::mutate(created_at = lubridate::as_datetime(created_at, tz = "UTC")) |>
      dplyr::mutate(rt_datetime = as.numeric(created_at)) |>
      # group by
      dplyr::group_by(tag) |>
      dplyr::mutate(group_id = dplyr::cur_group_id())
  }

  # filter the data according to the minimum threshold required for coordination

  ## identify users that tweeted a number of times higher than or equal to min_repetition
  author_n <- dset_rt |>
    dplyr::group_by(author_id) |>
    dplyr::summarize(n = dplyr::n()) |>
    dplyr::filter(n >= min_repetition)

  if (nrow(author_n) == 0) {
    message("\n### No network detected ####\n")
    opt <- options(show.error.messages = FALSE)
    on.exit(options(opt))
    stop()
  }

  ## identify content tweeded by at least two users (no coordination possible for content tweeted by less than two users)
  group_n <- dset_rt |>
    dplyr::group_by(group_id) |>
    dplyr::summarize(n = dplyr::n()) |>
    dplyr::filter(n >= 2)

  if (nrow(group_n) == 0) {
    message("\n### No network detected ####\n")
    opt <- options(show.error.messages = FALSE)
    on.exit(options(opt))
    stop()
  }

  # filter data
  dset_rt <- dset_rt |>
    dplyr::filter(group_id %in% group_n$group_id & author_id %in% author_n$author_id)

  # re-identify content shared by less than two users after filtering
  group_n <- dset_rt |>
    dplyr::group_by(group_id) |>
    dplyr::summarize(n = dplyr::n()) |>
    dplyr::filter(n >= 2)

  dset_rt <- dset_rt |>
    dplyr::filter(group_id %in% group_n$group_id)

  rm("author_n", "group_n")

  return(list(dset_rt, tweets))
}
