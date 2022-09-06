#' net_viz
#'
#' Create the coordinated network chart.
#'
#' @param coord_graph the igraph object representing the coordinated network.
#'
#' @return a plot in the 'Plots' window.
#'
#' @importFrom visNetwork toVisNetworkData visNetwork visNodes visEdges visIgraphLayout

net_viz <- function(coord_graph = coord_graph) {
  vis_net <- visNetwork::toVisNetworkData(coord_graph)

  colors_10 <- c("#8750a6",
    "#97b241",
    "#677fd7",
    "#d1a137",
    "#b84c7d",
    "#5bbc6d",
    "#ba4c41",
    "#48bf9e",
    "#b97736",
    "#74893a")

  vis_net$nodes <- vis_net$nodes
  vis_net$edges <- vis_net$edges
  vis_net$nodes$label <- vis_net$nodes$username
  vis_net$nodes$title <- vis_net$nodes$description
  vis_net$nodes$image <- vis_net$nodes$profile_image_url
  vis_net$nodes$value <- vis_net$nodes$degree
  vis_net$nodes$borderWidth <- 6



  top10_clusters <- vis_net$nodes |>
    dplyr::group_by(cluster) |>
    dplyr::summarize(n = dplyr::n()) |>
    dplyr::top_n(n = 10, wt = n)

  if(nrow(top10_clusters)<10){
    colors_10 <- colors_10[1:nrow(top10_clusters)]
  }

  for(i in 1:nrow(vis_net$nodes)){
    for (j in 1:nrow(top10_clusters)){
      if (vis_net$nodes$cluster[i] == top10_clusters$cluster[j]) vis_net$nodes$color.border[i] <- colors_10[j]
    }
  }


  net_chart <-
    visNetwork::visNetwork(
      main = "Twitter Coordinated Network",
      submain = list(text = "CooRTweet",
                     style = "font-family:Arial;color:#000000;font-size:15px;text-align:center;"),
      vis_net$nodes,
      vis_net$edges,
      idToLabel = TRUE
    ) |>
    visNetwork::visNodes(shape = "circularImage",
                         scaling = list(min = 20, max = 50),
                         physics = FALSE) |>
    visNetwork::visEdges(length = 50,
                         width = 2,
                         physics = FALSE,
                         color = "black") |>
    visNetwork::visIgraphLayout(layout = "layout_nicely",
                                physics = FALSE,
                                smooth = TRUE) |>
    visNetwork::visPhysics( solver = NULL, maxVelocity = 50, minVelocity = 25,
               timestep = NULL, barnesHut = list(avoidOverlap = 0.5), forceAtlas2Based = TRUE,
               repulsion = list(nodeDistance = 50), hierarchicalRepulsion = NULL,
               stabilization = list(iterations = 100),
               adaptiveTimestep = NULL)

  return(net_chart)
}
