#' data_wrangling_filter
#'
#' Filter data based on minimum thresholds to avoid unnecessary computation.
#'
#' @param dset_list the list returned by \link[CooRTweet]{data_wrangling}.
#' @param min_repetition minimum number of repetition set by user.
#'
#' @return a pre-filtered dataframe of tweets ready for coordination analysis.
#'
#' @importFrom tidyr unnest_wider separate_rows unnest
#' @importFrom dplyr group_by summarize filter
#' @importFrom magrittr %>%

# filter by min threshold ####
## filter the data according to the minimum threshold required for coordination

data_wrangling_filter <- function(dset_list,
                                  min_repetition){

  dset_rt <- dset_list[[1]]
  min_repetition <- min(min_repetition) # if multiple values are specified, keep the lower one

  ## identify users that tweeted a number of times higher than or equal to min_repetition
  author_n <- dset_rt %>%
    dplyr::group_by(author_id) %>%
    dplyr::summarize(n = dplyr::n()) %>%
    dplyr::filter(n >= min_repetition)

  if (nrow(author_n) == 0) {
    message("\n### No network was detected ####\n")
    opt <- options(show.error.messages = FALSE)
    on.exit(options(opt))
    stop()
  }

  ## identify content tweeted by at least two users (no coordination possible for content tweeted by less than two users)
  group_n <- dset_rt %>%
    dplyr::group_by(group_id) %>%
    dplyr::summarize(n = dplyr::n()) %>%
    dplyr::filter(n >= 2)

  if (nrow(group_n) == 0) {
    message("\n### No network was detected ####\n")
    opt <- options(show.error.messages = FALSE)
    on.exit(options(opt))
    stop()
  }

  # filter data
  dset_rt <- dset_rt %>%
    dplyr::filter(group_id %in% group_n$group_id & author_id %in% author_n$author_id)

  # re-identify content shared by more than two users after previous filtering
  group_n <- dset_rt %>%
    dplyr::group_by(group_id) %>%
    dplyr::summarize(n = dplyr::n()) %>%
    dplyr::filter(n >= 2)

  dset_rt <- dset_rt %>%
    dplyr::filter(group_id %in% group_n$group_id)

  rm("author_n", "group_n")

  return(list(dset_rt, dset_list[[2]]))
}
