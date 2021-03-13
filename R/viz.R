#' Create an animation out of locations of simulated individuals over time
#'
#' @param locations Tab-separated table with locations of individuals throughout
#'   the simulation saved by the backend SLiM script
#' @param gif Path to an output GIF (animation object returned by default)
#' @param gen_time Generation time to translate generations into years
#' @param nframes Number of frames of the animation
#'
#' @return If `gif = NULL`, return gganimate animation object. Otherwise a GIF
#'   file is saved and no value is returned.
#'
#' @import ggplot2
#' @export
animate <- function(locations, gif = NULL, gen_time = NULL, nframes) {
  locs <- read.table(locations, header = TRUE)
  locs$pop <- factor(locs$pop)
  locs$popname <- paste0("pop", locs$pop)
  locs$tyears <- as.integer(locs$t * gen_time)

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
  locs <- dplyr::sample_n(locs, 10000)

  p <- ggplot(locs, aes(x, y, color = popname)) +
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


plot_model <- function(pops, admixtures, time = -Inf) {
  pops <- pops[tsplit >= time | is.na(tsplit)]
  admixtures <- admixtures[tstart >= time]
  ggplot(pops, aes(x + 100, y + 50)) +
    background_image(map) +
    # plot directions of population splits
    geom_segment(aes(x = xfrom, y = yfrom, xend = x, yend = y, color = name),
                 data = na.omit(pops), size = 1,
                 arrow = arrow(length = unit(0.5, "cm"), type = "closed")) +
    # plot directions of admixture events
    geom_segment(
      aes(x = xfrom, y = yfrom, xend = xto, yend = yto, color = toname),
      data = admixtures, size = 1,
      arrow = arrow(length = unit(0.5, "cm"), type = "closed")
    ) +
    geom_label(
      aes(label = sprintf("%s->%s\n%dy-%dy ago\n%.1f%%",
                          fromname, toname, tstart, tend, rate / 100),
          x = (xfrom + xto) / 2, y = (yfrom + yto) / 2, fill = fromname),
      data = admixtures, size = 3
    ) +
    geom_point(aes(x, y, color = name), size = 2) +
    geom_label(aes(label = sprintf("%s (%s)\n%sy ago\nfrom: %s",
                                   name, pop,
                                   ifelse(!is.na(tsplit), tsplit, "-"),
                                   ifelse(!is.na(parentname), parentname, "-")),
                   fill = name), size = 3) +
    coord_fixed(xlim = c(0, dim(map)[2]), ylim = c(0, dim(map)[1])) +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    scale_color_discrete(drop = FALSE) +
    scale_fill_discrete(drop = FALSE) +
    theme(legend.position = "none",
          axis.title.x = element_blank(),
          axis.title.y = element_blank())
}
