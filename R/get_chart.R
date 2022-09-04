#' get_chart
#'
#' Create the coordinated network chart.
#'
#' @param coord_graph the igraph object representing the coordinated network.
#'
#' @return a plot in the 'Plots' window.
#'
#' @importFrom visNetwork toVisNetworkData visNetwork visNodes visEdges visIgraphLayout

get_chart <- function(coord_graph = coord_graph) {
  vis_net <- visNetwork::toVisNetworkData(coord_graph)

  vis_net$nodes <- vis_net$nodes
  vis_net$edges <- vis_net$edges
  vis_net$nodes$label <- vis_net$nodes$username
  vis_net$nodes$title <- vis_net$nodes$description
  vis_net$nodes$image <- vis_net$nodes$profile_image_url
  vis_net$nodes$value <- vis_net$nodes$degree

  net_chart <-
    visNetwork::visNetwork(
      main = "Twitter Coordinated Network",
      submain = list(text = "CooRTweet",
                     style = "font-family:Arial;color:#000000;font-size:15px;text-align:center;"),
      vis_net$nodes,
      vis_net$edges,
      #physics = TRUE,
      idToLabel = TRUE
    ) |>
    visNetwork::visNodes(shape = "circularImage",
                         scaling = list(min = 20, max = 50)) |>
    visNetwork::visEdges(length = 100,
                         width = 2,
                         color = "black") |>
    visNetwork::visIgraphLayout(layout = "layout_nicely",
                                #physics = TRUE,
                                smooth = TRUE)

  return(net_chart)
}
