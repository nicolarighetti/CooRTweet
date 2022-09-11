#' coord_users_info
#'
#' Function to create a data set, and augment the graph, with information on the coordinated coord_users.
#'
#' @param coord_graph the graph representing the coordinated network.
#' @return a dataframe including information on the coordinated actors identified by the analysis
#' (highly_connected_coordinated_entities).
#'
#' @importFrom academictwitteR bind_tweets
#' @importFrom dplyr mutate case_when filter group_by ungroup select right_join rename
#' @importFrom igraph V as_data_frame
#' @importFrom stats median
#' @importFrom magrittr %>%

utils::globalVariables(
  c(
    "coord_graph",
    "dset_rt",
    "tweets",
    "coord_function",
    "reply_type",
    "referenced_tweet_id",
    "author_id",
    "coordinated",
    "id",
    "in_reply_to_user_id",
    "username",
    "name",
    "tweet_id",
    "coretweeted_tweets_n",
    "coretweeted_users_n",
    "expanded_url",
    "name.y",
    "entities.mentions"
  )
)


coord_users_info <- function(data_path,
                             out_list,
                             dset_list,
                             tweets,
                             coord_function,
                             reply_type,
                             quick) {
  edge_list <- out_list[[1]]
  dset_rt <- out_list[[2]]
  coord_graph <- out_list[[3]]
  tweets <- dset_list[[2]]

  coord_users_info <-
    igraph::as_data_frame(coord_graph, "vertices")

  # calculate users' average coordinated actions rapidity
  if (quick == FALSE) {
    time_diff_summary <- data.frame(
      name = c(edge_list$author_id_x,
               edge_list$author_id_y),
      time_diff = c(edge_list$time_diff,
                    edge_list$time_diff)
    ) %>%
      dplyr::group_by(name) %>%
      dplyr::summarize(
        average_coord_time = mean(time_diff),
        median_coord_time = stats::median(time_diff)
      )

    coord_users_info <-  merge(coord_users_info,
                               time_diff_summary,
                               by = "name",
                               all.x = TRUE)
  }

  # load users' JSONs
  cat("\n")

  users <-
    academictwitteR::bind_tweets(data_path = data_path, user = TRUE)
  users <- dplyr::distinct(users, id, .keep_all = TRUE)

  coord_users_info <- dplyr::left_join(coord_users_info,
                                       users,
                                       by = c("name" = "id")) %>%
    dplyr::rename(screenname = name.y)

  # users' info to coord_graph
  ## usernames
  igraph::V(coord_graph)$username <-
    sapply(igraph::V(coord_graph)$name, function(x)
      unique(coord_users_info$username[coord_users_info$name == x]))
  ## description
  igraph::V(coord_graph)$description <-
    sapply(igraph::V(coord_graph)$name, function(x)
      unique(coord_users_info$description[coord_users_info$name == x]))
  ## followers
  igraph::V(coord_graph)$followers <-
    sapply(igraph::V(coord_graph)$name, function(x)
      as.numeric(coord_users_info$public_metrics$followers_count[coord_users_info$name == x]),
      simplify = TRUE)
  ## image
  igraph::V(coord_graph)$profile_image_url <-
    sapply(igraph::V(coord_graph)$name, function(x)
      unique(coord_users_info$profile_image_url[coord_users_info$name == x]))

  return(list(coord_users_info, coord_graph))
}
