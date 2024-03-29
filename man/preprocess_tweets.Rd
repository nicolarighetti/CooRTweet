% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/preprocess_twitter.R
\name{preprocess_tweets}
\alias{preprocess_tweets}
\title{preprocess_tweets}
\usage{
preprocess_tweets(
  tweets,
  tweets_cols = c("possibly_sensitive", "lang", "text", "public_metrics_retweet_count",
    "public_metrics_reply_count", "public_metrics_like_count",
    "public_metrics_quote_count")
)
}
\arguments{
\item{tweets}{a data.table to unnest. Twitter data loaded
with \link{load_tweets_json}`.}

\item{tweets_cols}{a character vector specifying the columns to keep (optional).}
}
\value{
a named \code{list} with 5 data.tables:
tweets (contains all tweets and their meta-data),
referenced (information on referenced tweets),
urls (all urls mentioned in tweets),
mentions (other accounts mentioned in tweets),
hashtags (hashtags mentioned in tweets)
}
\description{
Reformat nested Twitter data (retrieved from Twitter V2 API).
Spreads out columns and reformats nested a \code{data.table} to
a named list of unnested data.tables.
All output is in long-format.
}
\details{
Restructure your nested Twitter data that you loaded with
\link{load_tweets_json}. The function unnests the following columns:
\code{public_metrics} (likes, retweets, quotes),
\code{referenced_tweets} (IDs of "replied to" and "retweet"),
\code{entities} (hashtags, URLs, other accounts).
Returns a named list with several \code{data.tables},
each \code{data.table} represents one aspect of the nested data.
The function also expects that the following additional
columns are present in the \code{data.table}:
\code{created_at}, \code{tweet_id}, \code{author_id},
\code{conversation_id}, \code{text},
\code{in_reply_to_user_id}.
Implicitely dropped columns: \code{edit_history_tweet_ids}
}
