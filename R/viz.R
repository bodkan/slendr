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
