library(shiny)

fill_maps <- function(...) {
  pops <- list(...)

  names(pops) <- sapply(pops, function(i) i$pop[1])
  
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

    attributes(combined_maps) <- attributes(pop_maps)

    combined_maps

  })
}


plot_maps <- function(..., time = NULL, graticules = "original", intersect = TRUE) {
  if (!graticules %in% c("internal", "original"))
    stop("Graticules can be either 'original' or 'internal'", call. = FALSE)

  pops <- list(...)

  spatial_maps <- do.call(rbind, lapply(pops, intersect_features))
  
  if (!is.null(time))
    spatial_maps <- spatial_maps[spatial_maps$time == time, ]

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

  if (nrow(map)) {
    p_map <- geom_sf(data = map,
                     aes(frame = NULL), fill = "lightgray", color = NA)
  } else {
    p_map <- NULL
  }
  
  ggplot() +
    p_map +
    geom_sf(data = spatial_maps,
            aes(fill = pop), color = NA) +
    scale_fill_discrete(drop = FALSE) +
    scale_alpha(range = c(1, 0.1)) +
    guides(fill = FALSE, alpha = guide_legend("time"))
}

run_shiny <- function(...) {
  
  all_maps <- fill_maps(...)
  times <- sort(unique(unlist(lapply(all_maps, `[[`, "time"))))
  
  # Define UI for app that draws a histogram ----
  ui <- fluidPage(

    # App title ----
    titlePanel("Spatial population dynamics"),

    # Sidebar layout with input and output definitions ----
    sidebarLayout(

      # Sidebar panel for inputs ----
      sidebarPanel(

        # Input: Slider for the number of bins ----
        sliderInput(inputId = "time_point",
                    label = "Time point:",
                    min = min(times),
                    max = max(times),
                    value = min(times))

      ),

      # Main panel for displaying outputs ----
      mainPanel(

        # Output: Histogram ----
        plotOutput(outputId = "spannr_shiny")

      )
    )
  )

  
  # Define server logic required to draw a histogram ----
  server <- function(input, output) {
    # This expression that generates the figure is wrapped in a call
    # to renderPlot to indicate that:
    #
    # 1. It is "reactive" and therefore should be automatically
    #    re-executed when inputs (input$bins) change
    # 2. Its output type is a plot
    output$spannr_shiny <- renderPlot({

      # get the last time snapshot before the specified time
      previous_time <- min(times[times >= input$time_point])
      
      do.call("plot_maps", c(as.list(all_maps), time = previous_time))

    })

  }
  
  shinyApp(ui, server)
}
