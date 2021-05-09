library(shiny)

# Define UI for application that plots random distributions 
shinyUI(pageWithSidebar(

  # Application title
  headerPanel("Spatial model population dynamics"),

  # Sidebar with a slider input for number of observations
  sidebarPanel(
    sliderInput("time_snapshot", 
                "Time snapshot:", 
                min = 0,
                max = 50000, 
                value = 50000)
  ),

  # Show a plot of the generated distribution
  mainPanel(
    plotOutput(outputId = "spannr_map")
  )
))

# Define server logic required to generate and plot a random distribution
shinyServer(function(input, output) {

  # Expression that generates a plot of the distribution. The expression
  # is wrapped in a call to renderPlot to indicate that:
  #
  #  1) It is "reactive" and therefore should be automatically 
  #     re-executed when inputs change
  #  2) Its output type is a plot 
  #
  output$spannr_map <- renderPlot({
    shiny_plot(pops = list(afr, ooa, ehg, ana, eur, yam, yam_migr), input$time_snapshot)
  })
})



#' Render the spatial dynamics as an interactive shiny app
plot_snapshot <- function(..., time_snapshot, graticules = "original", intersect = TRUE) {
  if (!graticules %in% c("internal", "original"))
    stop("Graticules can be either 'original' or 'internal'", call. = FALSE)

#  pops <- list(...)
  names(pops) <- sapply(pops, function(i) i$pop[1])

  # extract the map component underlying each population object
  # and make sure they are all the same with no conflicts
  maps <- unique(lapply(pops, function(i) attr(i, "map")))
  if (length(maps) != 1) {
    stop("Objects do not share the same map component", call. = F)
  }
  map <- maps[[1]]

  # plot the world map (if a real geographic map was specified)
  if (nrow(map)) {
    p_map <- geom_sf(data = sf::st_cast(map, "MULTIPOLYGON"),
                     aes(frame = NULL), fill = "lightgray", color = NA)
  } else {
    p_map <- NULL
  }

  if (graticules == "original" & has_crs(map)) {
    graticule_crs <- "EPSG:4326"
  } else {
    graticule_crs <- sf::st_crs(map)
  }

  if (has_crs(map)) {
    p_coord <- coord_sf(crs = sf::st_crs(map), datum = graticule_crs, expand = 0)
  } else {
    p_coord <- coord_sf(
      xlim = attr(map, "xrange"),
      ylim = attr(map, "yrange"),
      expand = 0
    )
  }

  removal_times <- sapply(names(pops), function(pop_name) {
    pop_maps <- pops[[pop_name]]
    attr(pops[[pop_name]], "remove")
  })

  # get times of all spatial maps across all populations
  all_times <- unique(sort(c(
    0,
    removal_times,
    unlist(sapply(pops, function(i) i$time))
  ))) %>% .[. != Inf & . != -1]

  all_maps <- lapply(names(pops), function(pop_name) {

    pop_maps <- pops[[pop_name]]

    # get times where the spatial map of the current population
    # needs to be filled in
    missing_times <- all_times[
      all_times >= removal_times[pop_name] &
        !all_times %in% pop_maps$time
    ]

    # generate the missing maps
    new_maps <- lapply(missing_times, function(t) {
      # get all preceding maps
      previous_maps <- pop_maps %>% .[.$time > t, ] # what's going on here with 2 maps?
      if (!nrow(previous_maps)) return(NULL)
      latest_map <- previous_maps[nrow(previous_maps), ]
      latest_map$time <- t
      latest_map
    }) %>%
      do.call(rbind, .)

    combined_maps <-
      rbind(pop_maps, new_maps) %>%
      .[order(-.$time), ] %>%
      .[.$time != Inf, ]

    #  attributes(combined_maps) <- attributes(pop_maps)

    combined_maps

  }) %>%
  do.call(rbind, .) %>%
  .[order(-.$time), ]

  snapshot <- all_maps[all_maps$time == time_snapshot, ]
  p_map <-  ggplot() + theme_bw()

if (nrow(map)) {
  p_map <- geom_sf(data = map,
                   aes(frame = NULL), fill = "lightgray", color = NA)
} else {
  p_map <- NULL
}

ggplot() +
  p_map +
  geom_sf(data = snapshot,
          aes(fill = pop, alpha = -time), color = NA) +
  scale_fill_discrete(drop = FALSE) +
  scale_alpha(range = c(1, 0.1)) +
  guides(fill = FALSE, alpha = guide_legend("time"))

}
