#' Take a list of spannr_pop population boundary objects and
#' "interpolate" all of them at time points specified by others
#' (unless a given population is supposed to be removed at that time)
fill_maps <- function(pops) {
  
  removal_times <- sapply(pops, attr, "remove")

  # get times of all spatial maps across all populations
  all_times <- unique(sort(c(
    0,
    removal_times,
    unlist(sapply(pops, function(i) i$time))
  ))) %>% .[. != Inf & . != -1]
  
  all_maps <- lapply(seq_along(pops), function(i) {

    # get times where the spatial map of the current population
    # needs to be filled in
    missing_times <- all_times[
      all_times >= removal_times[i] &
        !all_times %in% pops[[i]]$time
    ]

    # generate the missing maps
    new_maps <- lapply(missing_times, function(t) {
      # get all preceding maps
      previous_map <- pops[[i]] %>% .[.$time > t, ]
      if (!nrow(previous_map)) return(NULL)
      latest_map <- previous_map[nrow(previous_map), ]
      latest_map$time <- t
      latest_map
    }) %>%
      do.call(rbind, .)

    if (!is.null(new_maps)) {
      combined_maps <-
        rbind(pops[[i]], new_maps) %>%
        .[order(-.$time), ] %>%
        .[.$time != Inf, ]

      attributes(combined_maps) <- attributes(pops[[i]])
    } else {
      combined_maps <- pops[[i]]
    }

    combined_maps

  })

  all_maps
}


#' Plot spatial maps
#'
#' @import ggplot2
plot_maps <- function(..., time = NULL, graticules = "original", intersect = TRUE) {
  if (!graticules %in% c("internal", "original"))
    stop("Graticules can be either 'original' or 'internal'", call. = FALSE)

  spatial_maps <- list(...)
  pop_names <- unique(unlist(sapply(pops, `[[`, "pop")))

  # extract the map component underlying each population object
  # and make sure they are all the same with no conflicts
  maps <- unique(lapply(spatial_maps, function(i) attr(i, "map")))
  if (length(maps) != 1) {
    stop("Objects do not share the same map component", call. = F)
  }
  map <- maps[[1]]
  
  # get all time points defined by the user
  all_times <- sort(unique(unlist(lapply(spatial_maps, `[[`, "time"))))

  # get split and removal times of all specified populations
  split_times <- sapply(sapply(spatial_maps, `[[`, "time"), `[`, 1)
  removal_times <- sapply(spatial_maps, attr, "remove")
  
  # if the user specified a time point, "interpolate" all maps at that time
  # and return just those that match that time point
  if (!is.null(time)) {
    spatial_maps <- fill_maps(spatial_maps)

    previous_time <- min(all_times[all_times >= time])
    # get only those populations already/still present at the
    # specified time...
    present_pops <- spatial_maps[split_times >= time & removal_times <= time]
    # ... and extract their spatial maps
    spatial_maps <- lapply(present_pops, function(pop) {
      snapshot <- pop[pop$time == previous_time, ]
      attributes(snapshot) <- attributes(pop)
      snapshot
    })
  }

  if (intersect)
    spatial_maps <- lapply(spatial_maps, intersect_features)

  spatial_maps <- do.call(rbind, spatial_maps)
  spatial_maps$pop <- factor(spatial_maps$pop, levels = pop_names)

  # plot the world map (if a real geographic map was specified)
  if (nrow(map)) {
    p_map <- geom_sf(data = sf::st_cast(map, "MULTIPOLYGON"),
                     aes(frame = NULL), fill = "lightgray", color = NA)
  } else {
    p_map <- NULL
  }

  if (graticules == "original" & has_crs(map)) {
    graticule_crs <- "EPSG:4326"
    xlab <- "degrees longitude"; ylab <- "degrees latitude"
  } else {
    graticule_crs <- sf::st_crs(map)
    xlab <- ylab <- NULL
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
    geom_sf(data = spatial_maps, aes(fill = pop), color = NA, alpha = 0.5) +
    scale_fill_discrete(drop = FALSE) +
    scale_alpha(range = c(1, 0.1)) +
    theme_bw() +
    coord_sf(crs = sf::st_crs(map), datum = graticule_crs, expand = 0) +
    labs(xlab = xlab, ylab = ylab)
}


#' Open an interactive browser of the spatial model
#'
#' @import shiny
interact <- function(...) {

  args <- list(...)

  map <- attr(args[[1]], "map")
  times <- sort(c(0, unique(unlist(lapply(args, `[[`, "time")))))

  if (has_crs(map)) {
    crs <- sf::st_crs(map)$epsg
    coord_choice <- c("original", "internal")
    names(coord_choice) <- c("original (longitude-latitude)",
                             sprintf("internal (EPSG:%s)", crs))
  } else {
    coord_choice <- list("original (abstract coordinates" = "original")
  }

  # Define UI for app that draws a histogram ----
  ui <- fluidPage(

    # App title ----
    titlePanel("Spatial population dynamics"),

    # Sidebar layout with input and output definitions ----
    sidebarLayout(

      # Sidebar panel for inputs ----
      sidebarPanel(

        shinyWidgets::sliderTextInput(
          inputId = "time_point",
          label = "Time point:",
          choices = rev(times[times < Inf]),
          selected = max(times[times < Inf]),
          width = "100%"
        ),

        selectInput(
          inputId = "coord_system",
          label = "Coordinate system:",
          choices = coord_choice
        ),

        checkboxInput(
          inputId = "intersect",
          label = "Intersect against landscape",
          value = TRUE
        )

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
      do.call("plot_maps", c(
        list(...),
        time = input$time_point,
        graticules = input$coord_system,
        intersect = input$intersect
      ))

    })

  }
  
  shinyApp(ui, server)
}

# interact(afr, ooa, ehg, eur, ana, yam, yam_migr)
