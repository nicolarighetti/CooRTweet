#' get_coordinated_tweets
#'
#' Function to identify different types of coordinated tweet networks.
#'
#' @param data_path file path to a folder containing the JSON files returned by the function \link[academictwitteR]{get_all_tweets}.
#' @param coord_function the type of coordinated behavior to detect. Currently, one of "get_coretweet", "get_cotweet", "get_coreply", "get_clsb", "get_cohashtag". See details.
#' @param reply_type the type of co-reply behavior: one among "same_text" and "same_user". Required if the coord_function is "get_coreply". See details.
#' @param time_window the number of seconds within which tweets are to be considered as coordinated (default to 60 seconds).
#' @param min_repetition the minimum number of published coordinated tweets necessary for a user to be included it in the coordinated network.
#' @param chart if TRUE, the function returns a simple chart of the network for exploratory analysis. Default to FALSE.
#' @param parallel_cores number of cores to be used in parallel computing. Default to all available cores -1.
#' @param coord_time_distribution Default to FALSE. If TRUE, it returns the distribution of time difference between the same action performed by all the users in the original dataset.
#' @param quick Default to FALSE. If TRUE, the more restrictive but quicker and less computationally intensive algorithm (Giglietto et al, 2020) is implemented to identify coordinated users. It can be especially useful when dealing with large datasets.
#'
#'
#' @return a list of outputs including:
#' * the coordinated network as a .graphml object;
#' * a data frame including information on the coordinated users identified by the analysis;
#' * the analyzed dataset augmented with a column indicating whether the tweet is coordinated or non-coordinated.
#'
#' @details
#' Coordinated behavior has been shown to be a strategy employed for political astroturfing
#' (Keller et al., 2020) and the spread of problematic content online (Giglietto et al., 2020).
#' The functions implemented in this package enable users to perform a variety of analyses to detect
#' possible coordinated newtorks on Twitter.
#'
#' In the network, users are represented as nodes and a link between them is created when they perfom
#' the same action at least n times (with n specified by the paramenter 'min_repetition') within a
#' predefined time threshold (as specified by the parameter 'time_window').
#' The coordinated actions that can be detected are the following:
#'
#' * "get_coretweet" detects networks of accounts that repeatedly shared the same retweet;
#' * "get_cotweet" detects networks of accounts that repeatedly published the same tweet;
#' * "get_coreply" detects networks of accounts that repeatedly replied with the same text (same_text) or to the same user (same_user);
#' * "get_clsb" detects networks of accounts that repeatedly shared the same URLs. The function refers to Coordinated Link Sharing Behavior (Giglietto et al, 2020). Only original tweets are considered when searching for coordinated url sharing.
#' * "get_cohashtag" detectes networks of accounts that repeatedly shared the same hashtag.
#'
#' To identify the network, all possible k=2 combinations between users is calculated for each n
#' identical actions, and then filtered according to the parameter time_window and min_repetition.
#' The number of possible combinations increases exponentially with the n number of actions
#' (base::choose(n, k)), requiring increasing computational power.
#'
#' An alternative algorithm can be implemented by setting the option *quick = TRUE*, which cuts the
#' period of time from the first to the last action in *t* period of length equals to *time_window*,
#' and defines as coordinated the accounts that performed the same action within the same time window
#' a number of times greater than or equal to the value of *min_repetition*.
#' The algorithm has been originally implemented in [CooRnet](https://github.com/fabiogiglietto/CooRnet)
#' (Giglietto et al, 2020) to detect coordinated networks on Facebook and Instagram. Depending on the analysis,
#' the choice may be more conservative than the default but faster and can be useful when dealing
#' with large datasets on personal computers.
#'
#' A plot of the network is visualized when chart = TRUE. The network is interactive: it is possible
#' to zoom in and out and by clicking or hovering on the nodes, the description of the accounts appear.
#' Visualization can be slow with large charts. For this reason, with graphs including more than 500 nodes,
#' the user can choose to visualize only the most important nodes according to the weight of the edges
#' (i.e., the number of coordinated actions).
#'
#' @references
#'
#' Giglietto, F., Righetti, N., Rossi, L., & Marino, G. (2020). It takes a village to manipulate the media: coordinated link sharing behavior during 2018 and 2019 Italian elections. Information, Communication & Society, 23(6), 867-891.
#'
#' Keller, F. B., Schoch, D., Stier, S., & Yang, J. (2020). Political astroturfing on Twitter: How to coordinate a disinformation campaign. Political Communication, 37(2), 256-280.
#'
#' @export


get_coordinated_tweets <- function(data_path = NULL,
                                   coord_function = c("get_coretweet",
                                                      "get_cotweet",
                                                      "get_coreply",
                                                      "get_clsb",
                                                      "get_cohashtag"),
                                   reply_type = NULL,
                                   time_window = 60,
                                   min_repetition = 2,
                                   chart = FALSE,
                                   parallel_cores = NULL,
                                   coord_time_distribution = FALSE,
                                   quick = FALSE) {
  error_messages_get_coordinated_tweets(
    data_path = data_path,
    coord_function = coord_function,
    reply_type = reply_type,
    time_window = time_window,
    min_repetition = min_repetition
  )

  tweets <- load_tweets(data_path = data_path)

  # return(list(dset_rt, tweets))
  dset_list <- data_wrangling(tweets = tweets,
                              coord_function = coord_function,
                              reply_type = reply_type,
                              min_repetition = min_repetition)

  # create the coordinated network edge list
  # return(list(edge_list, dset_rt (updated), coord_graph, full_edge_list (optional)))
  out_list <- coord_network_detection(
    dset_rt = dset_list[[1]],
    time_window = time_window,
    min_repetition = min_repetition,
    parallel_cores = parallel_cores,
    coord_time_distribution = coord_time_distribution,
    quick = quick
  )

  # return(list(coord_users_info, coord_graph (updated)))
  coord_list <-
    coord_users_info(
      data_path = data_path,
      out_list = out_list,
      dset_list = dset_list,
      coord_function = coord_function,
      reply_type = reply_type,
      quick = quick
    )


  # print information
  cat("\nTweets analyzed: N =",
      nrow(tweets),
      "\n")
  cat(
    "Users that tweeted together the same",
    if (coord_function == "get_clsb") {
      "URLs"
    } else {
      sub("get_co", "", coord_function)
    },
    "at least",
    min_repetition,
    "times within",
    time_window,
    "seconds from each other: N =",
    igraph::gorder(coord_list[[2]]),
    "\nNetwork componets: N =",
    max(igraph::V(out_list[[3]])$component),
    "\n"
  )

  # chart
  if (chart == TRUE) {
    net_chart <- net_viz(coord_graph = coord_list[[2]])
    print(net_chart)
  }

  # return output
  if (coord_time_distribution == FALSE) {
    return(list(# igraph network
      coord_list[[2]],
      # users' info df
      coord_list[[1]],
      # marked df
      out_list[[2]]))
  }

  if (coord_time_distribution == TRUE) {
    return(list(# igraph network
      coord_list[[2]],
      # users' info df
      coord_list[[1]],
      # marked df
      out_list[[2]],
      # tidy full_edge_list
      out_list[[5]]))
  }

}
