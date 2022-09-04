#' error_messages_get_coordinated_tweets
#'
#' Utility function with error messages for get_coordinated_tweets.
#'
#' @param data_path see \link[CooRTnet]{get_coordinated_tweets}.
#' @param coord_function see \link[CooRTnet]{get_coordinated_tweets}.
#' @param reply_type see \link[CooRTnet]{get_coordinated_tweets}.
#' @param time_window see \link[CooRTnet]{get_coordinated_tweets}.
#' @param min_repetition see \link[CooRTnet]{get_coordinated_tweets}.

#' @return a dataframe of tweets ready for coordination analysis.

error_messages_get_coordinated_tweets <-
  function(data_path = data_path,
           coord_function = coord_function,
           reply_type = reply_type,
           time_window = time_window,
           min_repetition = min_repetition) {

    if (is.null(data_path)) {
      stop(error = "please, indicate the data path")
    }

    if (is.null(coord_function)) {
      stop(error = "please, indicate the type of coordinated behavior to be detected (coord_function)")
    }

    if (coord_function == "get_coreply" & is.null(reply_type)) {
      stop(error = "please, indicate the type of co-reply (same_text or same_user)")
    }

    if (is.null(time_window)) {
      stop(error = "please, specify a time window")
    }

    if (is.null(min_repetition)) {
      stop(error = "please, specify a minimum number of repetition")
    }

    if (min_repetition < 1) {
      stop(error = "please, set the option min_repetition to one or more repetitions")
    }

  }
