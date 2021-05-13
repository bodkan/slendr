#' Take a list of spannr_pop population boundary objects and
#' "interpolate" all of them at time points specified by others
#' (unless a given population is supposed to be removed at that time)
fill_maps <- function(pops, time = NULL) {
  
  removal_times <- sapply(pops, attr, "remove")

  # get times of all spatial maps across all populations
  all_times <- unique(sort(c(
    0,
    time,
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
plot_maps <- function(..., time = NULL, migrations = FALSE,
                      graticules = "original",
                      intersect = TRUE, show_map = TRUE) {
  if (!graticules %in% c("internal", "original"))
    stop("Graticules can be either 'original' or 'internal'", call. = FALSE)

  if (is.null(show_map)) show_map <- FALSE

  args <- list(...)

  if(!all(sapply(args, inherits, "spannr")))
    stop("Only objects of the class 'spannr' can be visualized using plot.spannr", call. = FALSE)

  classes <- grep("spannr_", unique(unlist(sapply(args, class))), value = TRUE)
  if (length(classes) > 1 & "spannr_model" %in% classes)
    stop("Either a single 'spannr_model' object or multiple objects of the type 'spannr_map', 'spannr_region', or 'spannr_pop' are allowed as arguments", call. = FALSE)

  # a single model object was provided
  if (length(args) == 1 & inherits(args[[1]], "spannr_model")) {
    model <- args[[1]]
    spatial_maps <- model$populations
    map <- model$map
  } else {
    spatial_maps <- args

    # extract the map component underlying each population object
    # and make sure they are all the same with no conflicts
    maps <- unique(lapply(spatial_maps, function(i) attr(i, "map")))
    if (length(maps) != 1) {
      stop("Objects do not share the same map component", call. = F)
    }

    map <- maps[[1]]
  }

  if (migrations & (is.null(time) | !inherits(args[[1]], "spannr_model")))
    stop("Migrations can be visualized only when a time point *and* a 'spannr_model' objects are specified", call. = FALSE)
  
  pop_names <- unique(unlist(sapply(spatial_maps, `[[`, "pop")))

  # if the user specified a time point, "interpolate" all maps at that time
  # and return just those that match that time point
  if (!is.null(time)) {
    # get all time points defined by the user
    all_times <- sort(unique(unlist(lapply(spatial_maps, `[[`, "time"))))

    # get split and removal times of all specified populations
    split_times <- sapply(sapply(spatial_maps, `[[`, "time"), `[`, 1)
    removal_times <- sapply(spatial_maps, attr, "remove")
  
    spatial_maps <- fill_maps(spatial_maps, time)

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

  if (intersect) spatial_maps <- lapply(spatial_maps, intersect_features)

  spatial_maps <- do.call(rbind, spatial_maps)
  spatial_maps$pop <- factor(spatial_maps$pop, levels = pop_names)

  if (graticules == "original" & has_crs(map)) {
    graticule_crs <- "EPSG:4326"
    xlab <- "degrees longitude"; ylab <- "degrees latitude"
  } else {
    graticule_crs <- sf::st_crs(map)
    xlab <- ylab <- NULL
  }

  if (has_crs(map)) {
    bbox <- sf::st_bbox(map)
    p_coord <- coord_sf(crs = sf::st_crs(map), datum = graticule_crs, expand = 0,
                        xlim = c(bbox["xmin"], bbox["xmax"]),
                        ylim = c(bbox["ymin"], bbox["ymax"]))
  } else {
    p_coord <- coord_sf(
      xlim = attr(map, "xrange"),
      ylim = attr(map, "yrange"),
      expand = 0
    )
  }

  if (nrow(map) & show_map)
    p_map <- geom_sf(data = map, aes(frame = NULL), fill = "lightgray", color = NA)
  else
    p_map <- NULL

  # build a base map with geographic features
  p <- ggplot() +
    p_map +
    geom_sf(data = spatial_maps, aes(fill = pop), color = NA, alpha = 0.4) +
    geom_sf(data = spatial_maps, fill = NA, color = "black", size = 0.1) +
    scale_fill_discrete(drop = FALSE, name = "") +
    theme_bw() +
    p_coord

  # add migration arrows, if requested
  if (migrations) {
    migr_df <- get_migrations(model, time)
    if (nrow(migr_df))
      p <- p +
        geom_point(data = migr_df, aes(x = from_x, y = from_y, color = from), size = 7) +
        geom_point(data = migr_df, aes(x = to_x, y = to_y, color = to), size = 7) +
        geom_curve(
          data = migr_df,
          aes(x = from_x, y = from_y, xend = to_x, yend = to_y),
          arrow = arrow(length = unit(2,"mm"), type = "closed"),
          lineend = "round", size = 0.5, arrow.fill = "black"
        ) +
        scale_color_discrete(drop = FALSE) +
        guides(color = FALSE)
  }
  
  p + labs(x = xlab, y = ylab)
}

#' Pick the next/previous value from a vector
get_time_point <- function(times, current_value, what) {
  current_index <- which(current_value <= times & times <= current_value)

  if (!length(current_index)) {
    if (what == "previous")
      return(times[current_value <= times][1])
    else
      return(tail(times[current_value >= times], 1))
  } else {
    if (what == "previous")
      new_index <- current_index + 1
    else if (what == "next")
      new_index <- current_index - 1
    else
      stop("Invalid direction for the time point selection")

    # prevent jumping out of the allowed range
    if (new_index > length(times) | new_index <= 0) new_index <- current_index

    times[new_index]
  }
}

#' Open an interactive browser of the spatial model
#'
#' @param model Compiled \code{spannr_model} model object
#'
#' @import shiny
#' @export
explore <- function(model, step = model$generation_time) {

  # generate choices for the coordinate system graticules
  if (has_crs(model$map)) {
    crs <- sf::st_crs(model$map)$epsg
    coord_choice <- c("original", "internal")
    names(coord_choice) <- c("original (longitude-latitude)",
                             sprintf("internal (EPSG:%s)", crs))
  } else {
    coord_choice <- list("original (abstract coordinates" = "original")
  }

  # generate event table for manual selection of time points
  split_events <- model$splits
  split_events$event <- with(
    split_events,
    sprintf("split of %s from %s", pop, parent)
  )
  split_events <- split_events[split_events$tsplit != Inf, c("tsplit", "event")]
  colnames(split_events) <- c("time", "event")

  admixture_starts <- model$admixtures
  admixture_starts$event <- with(
    admixture_starts,
    sprintf("migration %s → %s, %.2f%%", from, to, 100 * rate)
  )
  admixture_starts <- admixture_starts[, c("tstart", "event")]
  colnames(admixture_starts) <- c("time", "event")

  admixture_ends <- model$admixtures
  admixture_ends$event <- with(
    admixture_ends,
    sprintf("migration %s → %s ends", from, to)
  )
  admixture_ends <- admixture_ends[, c("tend", "event")]
  colnames(admixture_ends) <- c("time", "event")

  cleanup_events <- do.call(rbind, lapply(model$populations, function(pop)
    data.frame(time = attr(pop, "remove"),
               event = sprintf("%s removed", pop$pop[1]),
               stringsAsFactors = FALSE)
    ))
  cleanup_events <- cleanup_events[with(cleanup_events, time != -1), ]

  events <- do.call(rbind, list(split_events, admixture_starts,
                                admixture_ends, cleanup_events))
  events <- aggregate(event~time,data = events, FUN = paste, collapse = ", ")
  events$label <- sprintf("time %s: %s", events$time, events$event)
  events <- events[order(events$time), ]
  event_choices <- events$time
  names(event_choices) <- events$label

  # generate time points for the slider
  time_point_snapshots <-
    as.integer(c(0, event_choices, unlist(lapply(model$populations, `[[`, "time"))) %>%
    sort %>% unique %>% .[. != Inf])
  time_points <- as.integer(sort(unique(c(time_point_snapshots, seq(min(time_point_snapshots), max(time_point_snapshots), by = step)))))

  ui <- fluidPage(

    navbarPage(
      "Model explorer",

      tabPanel(
        "Spatial maps",

        sidebarLayout(

          sidebarPanel(

            htmlOutput(outputId = "time_label"),

            br(),

            fluidRow(
              column(2, actionButton("previous_time", label = "",
                                     icon = icon("angle-double-left", "fa-1x"))),

              column(8, shinyWidgets::sliderTextInput(
                inputId = "time_slider",
                label = "",
                choices = rev(time_points),
                selected = max(time_points),
                width = "100%"
              )),

              column(2, actionButton("next_time", label = "",
                                     icon = icon("angle-double-right", "fa-1x")))
            ),

            selectInput(
              inputId = "time_select",
              label = "Select event:",
              choices = event_choices,
              selected = max(time_points)
            ),

            selectInput(
              inputId = "coord_system",
              label = "Coordinate system:",
              choices = coord_choice
            ),

            fluidRow(
              column(7, checkboxInput(
                inputId = "intersect",
                label = "Intersect ranges",
                value = TRUE
              )),

              if (nrow(model$map)) {
                column(5, checkboxInput(
                  inputId = "show_map",
                  label = "Show landscape",
                  value = TRUE
                ))
              } else NULL
            ),

            checkboxInput(inputId = "show_migrations", label = "Indicate migration events", value = TRUE)

          ),

          mainPanel(

            plotOutput(outputId = "spannr_maps")

          )
        )

      ), # tabPanel

      tabPanel(
        "Admixture graph",

        sidebarLayout(

          sidebarPanel(

            checkboxInput(
              inputId = "show_cleanups",
              label = "Show removal times",
              value = TRUE
            )

          ),

          mainPanel(

            plotOutput(outputId = "spannr_graph")

          )

        )

      ) # tabPanel

    ) # navbarPage

  )

  server <- function(input, output, session) {

    observeEvent(input$time_select, {
      value <- as.numeric(input$time_select)
      shinyWidgets::updateSliderTextInput(session, "time_slider", selected = value)
    }, ignoreInit = TRUE)

    observeEvent(input$previous_time, {
      value <- get_time_point(time_point_snapshots, input$time_slider, "previous")
      shinyWidgets::updateSliderTextInput(session, "time_slider", selected = value)
    })

    observeEvent(input$next_time, {
      value <- get_time_point(time_point_snapshots, input$time_slider, "next")
      shinyWidgets::updateSliderTextInput(session, "time_slider", selected = value)
    })

    output$time_label = renderText({
      event <- events[events$time == input$time_slider, "event"]
      if (length(event))
        label <- sprintf("<i>(%s)</i>", event)
      else
        label <- ""
      sprintf("<b>Time point:</b> %s %s", input$time_slider, label)
    })

    output$spannr_maps <- renderPlot({
      
      plot_maps(
        model,
        time = input$time_slider,
        graticules = input$coord_system,
        intersect = input$intersect,
        show_map = input$show_map,
        migrations = input$show_migrations
      )

    }, height = 600)

    output$spannr_graph <- renderPlot({ graph(model, input$show_cleanups) },
                                      height = 600)

  }
  
  shinyApp(ui, server)
}
