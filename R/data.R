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
#'   \item{account_id}{ID of the user who tweeted. Twitter API: "author_id"}
#'   \item{content_id}{Tweet ID.}
#'   \item{timestamp_share}{Ingeger. Timestamp (posix time)}
#' }
#' @source Kulichkina, A., Righetti, N., & Waldherr, A. (2024).
#' Protest and repression on social media: Pro-Navalny and
#' pro-government mobilization dynamics and coordination
#' patterns on Russian Twitter. New Media & Society.
#' https://doi.org/10.1177/14614448241254126
"russian_coord_tweets"

#' German 2021 election campaign sample
#'
#' An anonymized temporal sample of Facebook and Twitter posts from the
#' German 2021 election campaign, from 15 August through 7 September 2021
#' (UTC). The sample illustrates URL, hashtag, domain, and image (pHash)
#' coordination workflows. It is a subset for examples, not the full study
#' dataset or a representative sample of the campaign. The full anonymized
#' dataset is in `data-raw/german_elections_full.rda` in the source repository.
#'
#' @format ## `german_elections_sample`
#' A data.frame with 97,413 rows and 7 columns:
#' \describe{
#'  \item{account_id}{character, with shorthand for platform}
#'  \item{post_id}{integer}
#'  \item{url_id}{integer, anonymized url contained in post}
#'  \item{hashtag_id}{integer, anonymized hashtag contained in post}
#'  \item{domain_id}{integer, anonymized domain of url}
#'  \item{phash_id}{integer, anonymized perceptual hash of shared image}
#'  \item{timestamp}{numeric, timestamp of post}
#' }
#' @source Righetti, N., Giglietto, F., Kakavand, A. E.,
#' Kulichkina, A., Marino, G., & Terenzi, M. (2022).
#' Political Advertisement and Coordinated Behavior
#' on Social Media in the Lead-Up to the 2021 German Federal
#' Elections. Düsseldorf: Media Authority of North Rhine-Westphalia.
"german_elections_sample"
