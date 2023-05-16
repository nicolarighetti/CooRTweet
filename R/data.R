#' Pro-Government Russian Tweet Dataset
#'
#' A anonymized dataset of Tweets.
#' All IDs have been obscured using sha256 algorithm.
#'
#' @format ## `russian_coord_tweets`
#' A data frame with 35,125 rows and 4 columns:
#' \describe{
#'   \item{object_id}{ID of retweeted content.
#' Twitter API calls this "referenced_tweet_id".}
#'   \item{id_user}{ID of the user who tweeted. Twitter API: "author_id"}
#'   \item{content_id}{Tweet ID.}
#'   \item{timestamp_share}{Ingeger. Timestamp (posix time)}
#' }
#' @source Kulichkina (in Press).
"russian_coord_tweets"
