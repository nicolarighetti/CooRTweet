#' preprocess_tweets
#'
#' @description
#' Reformat nested Twitter data (retrieved from Twitter V2 API).
#' Spreads out columns and reformats nested a `data.table` to
#' a named list of unnested data.tables.
#' All output is in long-format.
#'
#' @details
#' Restructure your nested Twitter data that you loaded with
#' \link{load_tweets_json}. The function unnests the following columns:
#' `public_metrics` (likes, retweets, quotes),
#' `referenced_tweets` (IDs of "replied to" and "retweet"),
#' `entities` (hashtags, URLs, other users).
#' Returns a named list with several `data.tables`,
#' each `data.table` represents one aspect of the nested data.
#' The function also expects that the following additional
#' columns are present in the `data.table`:
#' `created_at`, `tweet_id`, `author_id`,
#' `conversation_id`, `text`,
#' `in_reply_to_user_id`.
#' Implicitely dropped columns: `edit_history_tweet_ids`
#'
#' @param tweets a data.table to unnest. Twitter data loaded
#' with \link{load_tweets_json}`.
#' @param tweets_cols a character vector specifying the columns to keep (optional).
#'
#' @return a named `list` with 5 data.tables:
#' tweets (contains all tweets and their meta-data),
#' referenced (information on referenced tweets),
#' urls (all urls mentioned in tweets),
#' mentions (other users mentioned in tweets),
#' hashtags (hashtags mentioned in tweets)
#'
#' @import data.table
#' @importFrom tidytable unnest
#' @importFrom lubridate as_datetime
#' @importFrom stringi stri_split_fixed
#'
#' @export
#'

preprocess_tweets <- function(tweets, tweets_cols = c(
                                  "possibly_sensitive", "lang", "text",
                                  "public_metrics_retweet_count",
                                  "public_metrics_reply_count",
                                  "public_metrics_like_count",
                                  "public_metrics_quote_count"
                              )) {
    tweet_id <- author_id <- created_at <- created_timestamp <-
        referenced_tweets <- referenced_tweet_id <- entities_urls <-
        domain <- expanded_url <- entities_mentions <- username <-
        id <- entities_hashtags <- tag <- NULL
    if (!inherits(tweets, "data.table")) {
        tweets <- data.table::as.data.table(tweets)
    }

    required_cols <- c(
        "entities",
        "tweet_id",
        "created_at",
        "author_id",
        "conversation_id",
        "in_reply_to_user_id",
        "referenced_tweets"
    )

    for (cname in required_cols) {
        if (!cname %in% colnames(tweets)) {
            stop("Columns or their names are incorrect.
            Ensure your data has the columns:
            entities, tweet_id, created_at")
        }
    }

    tweets_cols <- c(required_cols, tweets_cols)

    if ("public_metrics" %in% colnames(tweets)) {
        tweets <- dt_unnest_wider(tweets, c("public_metrics"))
    } else {
        tweets_cols <- tweets_cols[!startsWith(tweets_cols, "public_metrics")]
    }

    # Construct the main data.table containing all tweets and their meta-data
    # implicitly dropped columns: "edit_history_tweet_ids", "withheld"
    tmp_keep_cols <- colnames(tweets)[colnames(tweets) %in% tweets_cols]

    Tweets <- tweets[, tmp_keep_cols, with = FALSE]
    data.table::setindex(Tweets, tweet_id, author_id)

    # reformat datetime of created_at
    Tweets[, created_at := lubridate::as_datetime(created_at, tz = "UTC")]
    Tweets[, created_timestamp := as.numeric(created_at)]

    # referenced tweets
    Referenced <- tidytable::unnest(tweets, referenced_tweets)
    Referenced_cols <- c("tweet_id", "id", "type")
    Referenced <- data.table::as.data.table(Referenced[, Referenced_cols, with = FALSE])
    data.table::setnames(
        Referenced,
        c("tweet_id", "referenced_tweet_id", "type")
    )

    data.table::setindex(Referenced, tweet_id, referenced_tweet_id)

    # unnest "entities", contains: urls, hashtags, other users
    entities <- dt_unnest_wider(tweets, c("entities"))

    # Construct data.table holding all URLs
    URLs <- tidytable::unnest(entities, entities_urls)
    URLs_cols <- c(
        "tweet_id",
        "url",
        "expanded_url",
        "display_url",
        "title",
        "description",
        "unwound_url",
        "start",
        "end"
    )

    URLs <- data.table::as.data.table(URLs[, URLs_cols, with = FALSE])

    # extract domain names
    URLs[, domain := gsub("https?://", "", expanded_url)]
    URLs[, domain := stri_split_fixed(domain, "/", n = 2, simplify = TRUE)[, 1]]

    data.table::setindex(URLs, tweet_id, domain, expanded_url)


    # Construct data.table holding all users mentioned in tweets
    Mentions <- tidytable::unnest(entities, entities_mentions)

    Mentions_cols <- c("tweet_id", "username", "id", "start", "end")

    Mentions <- data.table::as.data.table(Mentions[, Mentions_cols, with = FALSE])

    data.table::setindex(Mentions, tweet_id, username, id)

    # Construct data.table with all hashtags
    Hashtags <- tidytable::unnest(entities, entities_hashtags)

    Hashtags_cols <- c("tweet_id", "tag", "start", "end")
    Hashtags <- data.table::as.data.table(Hashtags[, Hashtags_cols, with = FALSE])

    data.table::setindex(Hashtags, tag)

    rm(entities, tweets)
    gc()

    return(list(
        tweets = Tweets,
        referenced = Referenced,
        urls = URLs,
        mentions = Mentions,
        hashtags = Hashtags
    ))
}

#' preprocess_twitter_users
#'
#' @description
#' Reformat nested twitter user data (retrieved from Twitter v2 API).
#' Spreads out columns and reformats nested `data.table` to long format.
#'
#' @details
#' Take the Twitter user data that you loaded with
#' \link{load_twitter_users_json} and unnests the
#' following columns: `public_metrics` and `entities`.
#'
#' @param users a data.table with unformatted (nested user data).
#'
#' @return a data.table with reformatted user data.
#'
#' @import data.table
#' @importFrom tidytable unnest
#'
#' @export
#'


preprocess_twitter_users <- function(users) {
    domain <- expanded_url <- NULL
    if (!inherits(users, "data.table")) {
        users <- data.table::as.data.table(users)
    }

    required_cols <- c("username", "user_id")

    for (cname in required_cols) {
        if (!cname %in% colnames(users)) {
            stop("Columns or their names are incorrect.
            Ensure your data has the columns:
            username, user_id")
        }
    }

    users <- dt_unnest_wider(users, c("public_metrics"))
    entities <- dt_unnest_wider(users, c("entities"))
    urls <- dt_unnest_wider(entities, c("entities_url"))
    urls <- data.table::as.data.table(
        tidytable::unnest(urls,
            "entities_url_urls",
            .drop = FALSE,
            keep_empty = TRUE,
            names_sep = "_"
        )
    )

    urls_cols <- c(
        "user_id",
        "entities_url_urls_expanded_url",
        "entities_url_urls_display_url",
        "entities_url_urls_start",
        "entities_url_urls_end"
    )
    urls <- urls[, urls_cols, with = FALSE]
    data.table::setnames(
        urls, urls_cols,
        c("user_id", "expanded_url", "display_url", "url_start", "url_end")
    )

    # extract domain name
    urls[, domain := gsub("https?://", "", expanded_url)]
    urls[, domain := stri_split_fixed(domain, "/", n = 2, simplify = TRUE)[, 1]]

    users <- urls[users, on = "user_id"]
    users[, entities := NULL]

    rm(entities, urls)
    gc()

    return(users)
}

#' dt_unnest_wider
#'
#' @description
#' Private utility function for `data.table`.
#' Unnest a column into wide format. Takes the first 1000 rows as reference!
#' Tries to emulate the functionality of tidyr::unnest_wider()
#' Current implementation is probably very slow.
#'
#' @param dt A data.table as input
#' @param columns the name(s) of the column(s) to be unnested (as string)
#' @param keep Defaults to FALSE. Keep the column to be unnested.
#' @param simplify Defaults to TRUE. Try to simplify resulting columns
#' @import data.table
#' @importFrom data.table ':='
#'
#' @noRd

dt_unnest_wider <- function(dt,
                            columns,
                            keep = FALSE,
                            simplify = TRUE) {
    # data.table weirdness
    data.table::alloc.col(dt)

    for (colname in columns) {
        # how many rows to scan
        max_rows <- 1000
        if (nrow(dt) < 1000) {
            max_rows <- nrow(dt)
        }
        cols <- character()
        for (i in 1:max_rows) {
            cols <- unique(c(cols, names(dt[[colname]][[i]])))
        }

        for (subcol in cols) {
            new_col_name <- paste(colname, subcol, sep = "_")
            dt[
                ,
                eval(new_col_name) := lapply(
                    dt[[colname]],
                    "[[", eval(subcol)
                )
            ]

            if (simplify && is.list(dt[[new_col_name]])) {
                # if the lists in the column only have one element each,
                # then replace the column with an unlisted vector
                if (length(unlist(dt[[new_col_name]])) ==
                    length(dt[[new_col_name]])) {
                    dt[,
                        eval(new_col_name) := unlist(.SD),
                        .SDcols = c(new_col_name)
                    ]
                }
            }
        }
        if (!keep) {
            dt[, eval(colname) := NULL]
        }
    }
    return(dt)
}
