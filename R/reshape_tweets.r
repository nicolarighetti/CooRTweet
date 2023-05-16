#' reshape_tweets
#'
#' @description
#' Reshape twitter data for coordination detection.
#'
#' @details
#' This function takes the pre-processed Twitter data
#' (output of \link{preprocess_tweets}) and reshapes it
#' for coordination detection (\link{detect_coordinated_groups}).
#' You can choose the intent for reshaping the data. Use
#' `"retweets"` to detect coordinated retweeting behaviour;
#' `"hashtags"` for coordinated usage of hashtags;
#' `"urls"` to detect coordinated link sharing behaviour;
#' `"urls_domain"` to detect coordinated link sharing behaviour
#' at the domain level.
#' The output of this function is a reshaped `data.table` that
#' can be passed to \link{detect_coordinated_groups}.
#'
#' @param tweets a named list of Twitter data
#' (output of \link{preprocess_tweets})
#'
#' @param intent the desired intent for analysis.
#'
#' @return a reshaped data.table
#'
#' @import data.table
#'
#' @export
#'


reshape_tweets <- function(
    tweets,
    intent = c("retweets", "hashtags", "urls", "urls_domains")) {
    start = tweet_id = type = referenced_tweet_id = object_id = id_user = NULL
    if (!inherits(tweets, "list")) {
        stop("Provided data probably not preprocessed yet.")
    }

    required_elements <- c(
        "tweets",
        "referenced",
        "urls",
        "mentions",
        "hashtags"
    )

    for (el in required_elements) {
        if (!el %in% names(tweets)) {
            stop(
                paste("Provided data does not have the right structure.
                Please ensure the list contains:", el)
            )
        }
    }

    output_cols <- c("object_id", "id_user", "content_id", "timestamp_share")

    if (intent == "retweets") {
        # Mapping overview
        # referenced_tweet_id --> object_id
        # author_id --> id_user
        # tweet_id --> content_id:
        # created_timestamp --> timestamp_share

        # filter only mentions that start at position 3
        # these are direct retweets:
        # "RT @username"

        candidates <- tweets$mentions[start == 3, tweet_id]
        retweets <- tweets$referenced[tweet_id %in% candidates]
        retweets <- retweets[type == "retweeted"]

        # join meta data with referenced tweets
        retweets <- tweets$tweets[retweets, on = "tweet_id"]

        # attach original tweets, we need the timestamps

        filt <- tweets$tweets$tweet_id %in% retweets$referenced_tweet_id

        original_tweets <- tweets$tweets[filt]
        original_tweets[, referenced_tweet_id := tweet_id]

        tweet_cols <- c(
            "referenced_tweet_id",
            "author_id",
            "tweet_id",
            "created_timestamp"
        )

        retweets <- rbind(
            retweets[, tweet_cols, with = FALSE],
            original_tweets[, tweet_cols, with = FALSE]
        )

        data.table::setnames(retweets, tweet_cols, output_cols)
        data.table::setindex(retweets, object_id, id_user)

        return(retweets)
    } else if (intent == "hashtags") {
        # Mapping overview
        # hashtag --> object_id
        # author_id --> id_user
        # tweet_id --> content_id:
        # created_timestamp --> timestamp_share

        # join meta data with hashtags table
        hashtags <- tweets$tweets[tweets$hashtags, on = "tweet_id"]

        tweet_cols <- c("tag", "author_id", "tweet_id", "created_timestamp")
        hashtags <- hashtags[, tweet_cols, with = FALSE]

        data.table::setnames(hashtags, tweet_cols, output_cols)
        data.table::setindex(hashtags, object_id, id_user)

        return(hashtags)
    } else if (intent == "urls") {
        # Mapping overview
        # expanded_url --> object_id
        # author_id --> id_user
        # tweet_id --> content_id:
        # created_timestamp --> timestamp_share

        # remove Twitter's internal URLs
        filt <- startsWith(tweets$urls$expanded_url, "https://twitter.com")
        urls <- tweets$urls[!filt]

        # join meta data with urls table
        urls <- tweets$tweets[urls, on = "tweet_id"]

        tweet_cols <- c(
            "expanded_url",
            "author_id",
            "tweet_id",
            "created_timestamp"
        )

        urls <- urls[, tweet_cols, with = FALSE]

        data.table::setnames(urls, tweet_cols, output_cols)
        data.table::setindex(urls, object_id, id_user)

        return(urls)
    } else if (intent == "urls_domains") {
        # Mapping overview
        # domain --> object_id
        # author_id --> id_user
        # tweet_id --> content_id:
        # created_timestamp --> timestamp_share

        # remove Twitter's internal URLs
        filt <- startsWith(tweets$urls$expanded_url, "https://twitter.com")
        domains <- tweets$urls[!filt]

        # join meta data with urls table
        domains <- tweets$tweets[domains, on = "tweet_id"]

        tweet_cols <- c("domain", "author_id", "tweet_id", "created_timestamp")
        domains <- domains[, tweet_cols, with = FALSE]

        data.table::setnames(domains, tweet_cols, output_cols)
        data.table::setindex(domains, object_id, id_user)

        return(domains)
    } else {
        .NotYetImplemented()
    }
}
