#' load_tweets_json
#'
#' @description
#' Very efficient and fast way to load tweets stored in JSON files.
#' Wrapper of the function \link[RcppSimdJson]{fload}
#'
#' @details
#' This function is optimized to load tweets that were
#' collected using the academicTwittr Package (Twitter API V2).
#' It uses RcppSimdJson to load the JSON files, which is extremely
#' fast and efficient. It returns the twitter data as is. The only changes
#' are that the function renames the `id` of tweets to `tweet_id`, and
#' it also deduplicates the data (by `tweet_id`).
#' The function expects that the individual JSON files start with `data`.
#'
#' @param data_dir string that leads to the directory containing JSON files
#' @param query (string) JSON Pointer query passed on to
#' \link[RcppSimdJson]{fload} (optional). Default: `NULL`
#' @param query_error_ok (Boolean) stop if `query` causes an error. Passed on
#' to \link[RcppSimdJson]{fload} (optional). Default: `FALSE`
#'
#' @return a data.table with all tweets loaded
#'
#' @import RcppSimdJson
#' @import data.table
#'
#' @export

load_tweets_json <- function(
    data_dir,
    query = NULL,
    query_error_ok = TRUE) {
    # TODO: normalize pathing in Win / UNIX
    if (!endsWith(data_dir, "/")) {
        data_dir <- paste0(data_dir, "/")
    }
    json_files <- Sys.glob(paste0(data_dir, "data*.json"))

    # load all json files and join them into one data.table
    twitter_dt <- data.table::rbindlist(
        RcppSimdJson::fload(json_files,
            empty_array = data.frame(),
            empty_object = data.frame(),
            query = query,
            query_error_ok = query_error_ok
        ),
        use.names = TRUE,
        fill = TRUE
    )

    # rename "id" column
    data.table::setnames(twitter_dt, "id", "tweet_id")

    # deduplicate
    twitter_dt <- unique(twitter_dt, by = "tweet_id")

    data.table::alloc.col(twitter_dt)

    return(twitter_dt)
}

#' load_twitter_users_json
#'
#' @description
#' Very efficient and fast way to load user information from JSON files.
#' Wrapper of the function \link[RcppSimdJson]{fload}
#'
#' @details
#' This function is optimized to load user data JSON files that were
#' collected using the academicTwittr Package (Twitter API V2).
#' It uses RcppSimdJson to load the JSON files, which is extremely
#' fast and efficient. It returns the user data as is. The only changes
#' are that the function renames the `id` of tweets to `user_id`, and
#' it also deduplicates the data (by `user_id`).
#' The function expects that the individual JSON files start with `user`.
#'
#' @param data_dir string that leads to the directory containing JSON files
#' @param query_error_ok (Boolean) stop if `query` causes an error. Passed on
#' to \link[RcppSimdJson]{fload} (optional). Default: `TRUE`
#'
#' @return a data.table with all users loaded
#'
#' @import RcppSimdJson
#' @import data.table
#'
#' @export

load_twitter_users_json <- function(
    data_dir,
    query_error_ok = TRUE) {
    # TODO: normalize pathing in Win / UNIX
    if (!endsWith(data_dir, "/")) {
        data_dir <- paste0(data_dir, "/")
    }
    json_files <- Sys.glob(paste0(data_dir, "user*.json"))

    # load all json files and join them into one data.table
    twitter_dt <- data.table::rbindlist(
        RcppSimdJson::fload(json_files,
            empty_array = data.frame(),
            empty_object = data.frame(),
            query = "/users",
            query_error_ok = query_error_ok,
        ), # we only need the 'users' key in the files
        use.names = TRUE, fill = TRUE
    )

    # rename "id" column
    data.table::setnames(twitter_dt, "id", "user_id")

    # deduplicate
    twitter_dt <- unique(twitter_dt, by = "user_id")

    return(twitter_dt)
}



#' load_many_tweets_json
#'
#' @description
#' EXPERIMENTAL. Batched version of \link{load_tweets_json} with control over
#' retained columns. Not as efficient as \link{load_tweets_json}
#' but requires less memory.
#' Wrapper of the function \link[RcppSimdJson]{fload}
#'
#' @details
#' Unlike \link{load_tweets_json} this function loads JSON files
#' in batches and processes each batch before loading the next batch.
#' You can specify which columns to keep, which in turn requires less memory.
#' For example, you can decide not to keep the `"text` column, which
#' requires quite a lot of memory.
#'
#' @param data_dir string that leads to the directory containing JSON files
#' @param batch_size integer specifying the number of JSON files
#' to load per batch. Default: `1000`
#' @param keep_cols character vector with the names of columns you want to
#' keep. Set it to `NULL` to only retain the required columns.
#' Default: keep_cols = c("text", "possibly_sensitive", "public_metrics",
#' "lang", "edit_history_tweet_ids", "attachments", "geo")
#'
#' @param query (string) JSON Pointer query passed on to
#' \link[RcppSimdJson]{fload} (optional). Default: `NULL`
#' @param query_error_ok (Boolean) stop if `query` causes an error. Passed on
#' to \link[RcppSimdJson]{fload} (optional). Default: `FALSE`
#'
#' @return a data.table with all tweets loaded
#'
#' @import RcppSimdJson
#' @import data.table
#'
#' @export

load_many_tweets_json <- function(
    data_dir,
    batch_size = 1000,
    keep_cols = c(
        "text", "possibly_sensitive", "public_metrics",
        "lang", "edit_history_tweet_ids", "attachments", "geo"
    ),
    query = NULL,
    query_error_ok = TRUE) {
    required_cols <- c(
        "entities",
        "tweet_id",
        "created_at",
        "author_id",
        "conversation_id",
        "in_reply_to_user_id",
        "referenced_tweets"
    )

    if (is.null(keep_cols)) {
        keep_cols <- required_cols
    } else {
        keep_cols <- c(required_cols, keep_cols)
    }

    # TODO: normalize pathing in Win / UNIX
    if (!endsWith(data_dir, "/")) {
        data_dir <- paste0(data_dir, "/")
    }
    json_files <- Sys.glob(paste0(data_dir, "data*.json"))

    # Generate Batches
    batches <- split(json_files, ceiling(seq_along(json_files) / batch_size))
    tmp_loaded <- list()

    for (i in seq_along(batches)) {
        tmp_dt <- data.table::rbindlist(
            RcppSimdJson::fload(batches[[i]],
                empty_array = data.frame(),
                empty_object = data.frame(),
                query = query,
                query_error_ok = query_error_ok
            ),
            use.names = TRUE,
            fill = TRUE
        )
        # rename "id" column
        data.table::setnames(tmp_dt, "id", "tweet_id")
        # deduplicate
        tmp_dt <- unique(tmp_dt, by = "tweet_id")
        data.table::alloc.col(tmp_dt)

        # Drop unwanted columns
        tmp_keep_cols <- colnames(tmp_dt)[colnames(tmp_dt) %in% keep_cols]
        tmp_dt <- tmp_dt[, tmp_keep_cols, with = FALSE]

        tmp_loaded[[i]] <- tmp_dt
        gc()
    }
    twitter_dt <- rbindlist(tmp_loaded, use.names = TRUE)

    return(twitter_dt)
}
