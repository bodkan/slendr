#' Animate the simulated population dynamics
#'
#' @param model Compiled model object
#' @param nframes Number of frames of the animation
#' @param gif Path to an output GIF (animation object returned by default)
#' #' @param nframes Number of frames of the animation
#'
#' @return If `gif = NULL`, return gganimate animation object. Otherwise a GIF
#'   file is saved and no value is returned.
#'
#' @import ggplot2
#' @export
animate <- function(model, nframes, gif = NULL) {
  locs <- read.table(file.path(model$path, "output_locations.tsv.gz"), header = TRUE)
  # label populations based on their original idenifiers from the user
  locs$pop <- factor(
    locs$pop,
    levels = sort(unique(locs$pop)),
    labels = scan(file.path(model$path, "names.txt"), what = "character", quiet = TRUE)
  )
  locs$tyears <- as.integer(locs$t * model$gen_time)

  # cut time into blocks - same as the number of frames of the final GIF
  locs$tblock <- cut(
    locs$tyears,
    breaks = seq(max(locs$tyears), min(locs$tyears), length.out = nframes + 1),
    include.lowest = TRUE,
    dig.lab = 10
  )

  # reverse the order of time in the forward direction
  ordered <- rev(levels(locs$tblock[order(locs$tyears)]))
  locs$tblock <- factor(locs$tblock, levels = ordered)

  # lower sample density for plotting
  #locs <- dplyr::sample_n(locs, 10000)

  p <- ggplot(locs, aes(x, y, color = pop)) +
    geom_point(alpha = 0.5) +
  #  coord_fixed(xlim = c(0, dim(map)[2]), ylim = c(0, dim(map)[1])) +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank())

  anim <- p +
    gganimate::transition_states(
      tblock,
      transition_length = 1,
      state_length = 0
    ) +
    ggtitle("time block: {closest_state} years ago")

  anim <- gganimate::animate(
    anim,
    nframes = nframes + 1,
    width = 800,
    height = 560
  )

  if (is.null(gif))
    return(anim)
  else
    gganimate::anim_save(gif, anim)
}


#' Plot simulated ancestry proportions
#'
#' @param model Compiled model object
#'
#' @export
ancestries <- function(model, gen_time = FALSE) {
  anc_wide <- read_ancestries(model$path)

  # thank god for tidyverse, base R reshaping is truly awful...  but
  # it's not worth dragging along a huge dependency if we can do this
  # sort of thing in base R...
  anc_long <- reshape(
    data = anc_wide,
    direction = "long",
    timevar = "ancestry",
    varying = 2:(ncol(anc_wide) - 1),
    idvar = c("gen", "pop"),
    v.names = "prop",
    times = colnames(anc_wide)[2:(ncol(anc_wide) - 1)]
  )
  rownames(anc_long) <- NULL

  # order population names based on their split order
  split_order <- split(anc_long, anc_long$pop) %>%
    sapply(function(df) max(df$gen)) %>%
    sort(decreasing = TRUE) %>%
    names
  anc_long$pop <- factor(anc_long$pop, levels = split_order)

  if (gen_time)
    anc_long$time <- anc_long$gen
  else
    anc_long$time <- anc_long$gen * model$gen_time

  anc_long %>%
  ggplot(aes(-time, prop, color = ancestry)) +
    geom_line() +
    facet_wrap(~ pop) +
    coord_cartesian(ylim = c(0, 1)) +
    ggtitle("Ancestry proportions in populations during the course of their existence") +
    theme_minimal()
}


#' Plot admixture graph based on given model configuration
#'
#' @param model Compiled model object
#'
#' @import ggplot2 ggraph
#' @export
graph <- function(model) {
  # summarize model configuration into a tabular form
  split_table <- model$splits
  admixture_table <- model$admixtures

  split_edges <- get_split_edges(split_table)
  admixture_edges <- get_admixture_edges(admixture_table)
  terminal_edges <- get_terminal_edges(split_edges, admixture_edges, split_table)
  intermediate_edges <- get_intermediate_edges(split_edges, admixture_edges)

  edges <- rbind(
    split_edges,
    admixture_edges,
    terminal_edges,
    intermediate_edges
  )
  nodes <- get_nodes(edges)

  g <- tidygraph::tbl_graph(nodes = nodes, edges = edges, directed = TRUE)

  ggraph(g, layout = "sugiyama") +

    # admixture edges along with admixture rates
    geom_edge_link(
      aes(filter = type == "admixture", label = rate,
          start_cap = label_rect(node1.name),
          end_cap = label_rect(node2.name),
          linetype = "admixture"),
      angle_calc = "along",
      label_dodge = unit(3, "mm"),
      arrow = arrow(length = unit(4, "mm"))
    ) +

    # population split/continuation edges (no rates labeled)
    geom_edge_link(
      aes(filter = type  == "split",
          start_cap = label_rect(node1.name),
          end_cap = label_rect(node2.name),
          linetype = "split"),
      label_dodge = unit(10, "mm"),
      arrow = arrow(length = unit(4, "mm"))
    ) +

    # continuation edges
    geom_edge_link(aes(filter = type == "intermediate",
                       linetype = "continuation")) +

    # population removal time stamp
    geom_edge_link(aes(filter = type == "terminal"), alpha = 0.25) +

    geom_node_label(aes(filter = type == "split", fill = pop,
                        label = sprintf("%s split at %s", pop, time))) +

    geom_node_label(aes(filter = type == "ancestral", fill = pop,
                        label = paste(pop, "(ancestor)")),
                    label.size = 1) +

    geom_node_label(aes(filter = type == "admixture", fill = pop,
                        label = sprintf("admixture at %s", time))) +

    geom_node_label(aes(filter = type == "intermediate", fill = pop,
                        label = sprintf("from %s", pop))) +

    geom_node_label(aes(filter = type == "terminal",
                        label = sprintf("removed\nat %s",
                                        ifelse(time == 0, "the end", time))),
                    fill = "white") +

    scale_edge_linetype_manual(values = c("split" = "solid",
                                          "continuation" = "solid",
                                          "admixture" = "solid")) +

    guides(fill = guide_legend("population"),
           edge_linetype = FALSE) +

    theme_void() +
    theme(legend.position = "right",
          plot.margin=unit(c(1, 1, 1, 1), "cm"))
}



#' Read ancestry proportion tables generated by the backend SLiM script
read_ancestries <- function(model_dir) {
  files <- list.files(model_dir, pattern = ".*ancestry_.*.tsv$", full.names = TRUE)
  if (!length(files))
    stop("No ancestry proportion tables found. Did you specify the correct path?", call. = FALSE)

  lapply(files, function(f) {
    anc_df <- read.table(f, header = TRUE)
    anc_df$pop <- gsub(".*_ancestry_(.*).tsv", "\\1", f)
    anc_df
  }) %>% do.call(rbind, .)
}


#' Create a table of population split edges for graph visualization
get_split_edges <- function(split_table) {
  split_edges <- split_table[split_table$parent != "ancestor",
                             c("parent", "pop", "tsplit")]
  names(split_edges) <- c("from", "to", "time")
  split_edges$type <- "split"
  split_edges$rate <- NA
  split_edges$x <- paste0(split_edges$from, "-", split_edges$time)
  split_edges$y <- paste0(split_edges$to, "-", split_edges$time)

  split_edges <- split_edges[, c("x", "y", "type", "time", "rate")]
  rownames(split_edges) <- NULL

  split_edges
}

#' Create a table of population admixture edges for graph visualization
get_admixture_edges <- function(admix_table) {
  admix_edges <- admix_table[, c("source", "target", "rate", "tstart")]
  names(admix_edges) <- c("from", "to", "rate", "time")
  admix_edges$type <- "admixture"
  admix_edges$rate <- sprintf("%.1f%%", admix_edges$rate * 100)
  admix_edges$x <- paste0(admix_edges$from, "-", admix_edges$time)
  admix_edges$y <- paste0(admix_edges$to, "-", admix_edges$time)

  admix_edges[, c("x", "y", "type", "time", "rate")]
}

#' Create a table of 'intermediate' edges for graph visualization
#'
#' For plotting the entire admixture graph, just nodes representing population
#' splits are not enough. We also need nodes (population states) which are not
#' explicitly simulated as separate population, but they represent time points
#' needed to plot admixture edges.
get_intermediate_edges <- function(split_edges, admixture_edges) {
  edges <- rbind(split_edges, admixture_edges)

  all_nodes <- c(edges$x, edges$y)

  intermediate_nodes <- lapply(
    unique(gsub("-.*$", "", all_nodes)),
    function(i) {
      # grab all nodes and their times belonging to the current population i
      nodes <- all_nodes[grepl(paste0(i, "-"), all_nodes)]
      times <- gsub(".*-", "", nodes)
      # leave out the ancestral node/time from the sorting (in case it is even
      # present as a node with a non-integer "ancestral" time)
      ancestor <- nodes[times == "ancestral"]
      nodes <- nodes[times != "ancestral"]
      times <- as.integer(times[times != "ancestral"])
      c(ancestor, unique(nodes[order(times, decreasing = TRUE)]))
    }
  )

  lapply(intermediate_nodes, function(nodes) {
    if (length(nodes) == 1) return(NULL)
    # construct data frame of consecutive, linked pairs of nodes
    pairs <- cbind(nodes[-length(nodes)], nodes[-1]) %>%
      as.data.frame(stringsAsFactors = FALSE) %>%
      setNames(c("x", "y"))
    pairs$type <- "intermediate"
    pairs$time <- as.integer(gsub(".*-", "", pairs$y))
    pairs$rate <- NA
    pairs
  }) %>% do.call(rbind, .)
}


#' Generate edges leading to the times of population removals in SLiM
get_terminal_edges <- function(split_edges, admixture_edges, split_table) {
  edges <- rbind(split_edges, admixture_edges)

  # iterate over all populations and create a data frame of edges from the
  # population split nodes, to the nodes of the population removal from the
  # simulation
  terminal_edges <- lapply(1:nrow(split_table), function(i) {
    pop <- split_table[i, ]

    prev_time <- max(edges[grepl(pop$pop, edges$x) | grepl(pop$pop, edges$y), ]$time)
    prev_node <- paste0(pop$pop, "-", prev_time)
    tremove <- if (pop$tremove == -1) 0 else pop$tremove

    data.frame(
      x = prev_node,
      y = paste0(pop$pop, "-", tremove),
      type = "terminal",
      time = tremove,
      rate = NA,
      stringsAsFactors = FALSE
    )
  }) %>% do.call(rbind, .)

  terminal_edges
}


#' Get table of node labels in the graph
get_nodes <- function(edges) {
  nodes <- unique(c(edges$x, edges$y))

  # find the first occurrence of a population in the model based
  # on the time label
  pops <- gsub("-.*$", "", nodes)
  times <- as.integer(gsub("^.*-", "", nodes))

  ordered <- order(times, decreasing = TRUE)

  # non-duplicated elements in the vector of population names
  # indicate the first position of such element
  nodes <- data.frame(
    name = nodes[ordered],
    pop = pops[ordered],
    time = times[ordered],
    first = !duplicated(pops[ordered]),
    stringsAsFactors = FALSE
  )
  # assign a type to each node based on the corresponding edge type
  nodes$type <- sapply(nodes$name, function(i) {
    type <- grep("intermediate", edges[edges$y == i, ]$type, invert = TRUE, value = TRUE)
    if (length(type) > 0)
      return(type)
    else
      return("intermediate")
  })
  nodes$type[!nodes$name %in% edges$y] <- "ancestral"
  nodes
}

