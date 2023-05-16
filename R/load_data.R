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
#'
#' @return a data.table with all tweets loaded
#'
#' @import RcppSimdJson
#' @import data.table
#'
#' @export

load_tweets_json <- function(data_dir) {
    # TODO: normalize pathing in Win / UNIX
    if (!endsWith(data_dir, "/")) {
        data_dir <- paste0(data_dir, "/")
    }
    json_files <- Sys.glob(paste0(data_dir, "data*.json"))

    # load all json files and join them into one data.table
    twitter_dt <- data.table::rbindlist(
        RcppSimdJson::fload(json_files,
            empty_array = data.frame(),
            empty_object = data.frame()
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
#'
#' @return a data.table with all users loaded
#'
#' @import RcppSimdJson
#' @import data.table
#'
#' @export

load_twitter_users_json <- function(data_dir) {
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
            query = "/users"
        ), # we only need the 'users' key in the files
        use.names = TRUE, fill = TRUE
    )

    # rename "id" column
    data.table::setnames(twitter_dt, "id", "user_id")

    # deduplicate
    twitter_dt <- unique(twitter_dt, by = "user_id")

    return(twitter_dt)
}
