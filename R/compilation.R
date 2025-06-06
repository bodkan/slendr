#' Compile a slendr demographic model
#'
#' First, compiles the vectorized population spatial maps into a series of
#' binary raster PNG files, which is the format that SLiM understands and uses
#' it to define population boundaries. Then extracts the demographic model
#' defined by the user (i.e. population divergences and gene flow events) into a
#' series of tables which are later used by the built-in SLiM script to program
#' the timing of simulation events.
#'
#' @param populations Object(s) of the \code{slendr_pop} class (multiple objects
#'   need to be specified in a \code{list})
#' @param generation_time Generation time (in model time units)
#' @param gene_flow Gene flow events generated by the \code{gene_flow} function
#'   (either a list of data.frame objects in the format defined by the
#'   \code{gene_flow} function, or a single data.frame)
#' @param direction Intended direction of time. Under normal circumstances this
#'   parameter is inferred from the model and does not need to be set manually.
#' @param simulation_length Total length of the simulation (required for forward time
#'   models, optional for models specified in backward time units which by
#'   default run to "the present time")
#' @param serialize Should model files be serialized to disk? If not, only an
#'   R model object will be returned but no files will be created. This speeds
#'   up simulation with msprime but prevents using the SLiM back end.
#' @param path Output directory for the model configuration files which will be
#'   loaded by the backend SLiM script. If \code{NULL}, model configuration
#'   files will be saved to a temporary directory.
#' @param overwrite Completely delete the specified directory, in case it
#'   already exists, and create a new one?
#' @param force Force a deletion of the model directory if it is already
#'   present? Useful for non-interactive uses. In an interactive mode, the user
#'   is asked to confirm the deletion manually.
#' @param description Optional short description of the model
#' @param time_units Units of time in which model event times are to be interpreted.
#'   If not specified and \code{generation_time} is set to 1, this will be set to
#'   "generations", otherwise the value is "model time units".
#' @param resolution How many distance units per pixel?
#' @param competition,mating Maximum spatial competition and mating choice
#'   distance
#' @param dispersal Standard deviation of the normal distribution of the
#'   parent-offspring distance
#' @param extension Path to a SLiM script to be used for extending slendr's
#'   built-in SLiM simulation engine. This can either be a file with the snippet
#'   of Eidos code, or a string containing the code directly. Regardless, the
#'   provided snippet will be appended after the contents of the bundled slendr
#'   SLiM script.
#'
#' @return Compiled \code{slendr_model} model object which encapsulates all
#'   information about the specified model (which populations are involved,
#'   when and how much gene flow should occur, what is the spatial resolution
#'   of a map, and what spatial dispersal and mating parameters should be used
#'   in a SLiM simulation, if applicable)
#'
#' @export
#'
#' @example man/examples/model_definition.R
compile_model <- function(
    populations, generation_time, gene_flow = list(),
    direction = NULL, simulation_length = NULL,
    serialize = TRUE, path = NULL, overwrite = FALSE, force = FALSE,
    description = "", time_units = NULL,
    resolution = NULL, competition = NULL, mating = NULL, dispersal = NULL,
    extension = NULL
) {
  if (inherits(populations, "slendr_pop"))  populations <- list(populations)

  # get values of all map attributes across populations
  maps <- lapply(populations, get_map) %>% Filter(Negate(is.null), .)

  if (length(unique(maps)) > 1)
    stop("Multiple spatial maps detected across populations but only a single\n",
         "world map is allowed for spatial models.", call. = FALSE)
  else if (length(unique(maps)) == 1)
    map <- maps[[1]]
  else
    map <- NULL

  if (!is.null(map)) check_spatial_pkgs()

  if (!is.null(map) && length(maps) != length(populations))
    warning("Model containing a mix of spatial and non-spatial populations will be compiled.\n",
            "Although this is definitely supported, make sure this is really what you want.",
            call. = FALSE)

  if (!is.null(map) && is.null(resolution) && serialize)
    stop("A map resolution must be specified for spatial models", call. = FALSE)

  if (!serialize && is.null(path) && inherits(map, "slendr_map")) {
    warning("Spatial models must be serialized to disk for SLiM to simulate data from.\n",
            "Compiled like this, your model can only be simulated with msprime.",
            call. = FALSE)
  }
  if (serialize && is.null(path))
    path <- tempfile(fileext = "_slendr_model")

  # make sure that all parents are present
  pop_names <- purrr::map_chr(populations, ~ .x$pop[1])
  parent_names <- unique(purrr::map_chr(populations, function(pop) {
    parent <- attr(pop, "parent")
    if (is.character(parent))
      return(pop$pop[1])
    else
      parent$pop[1]
  }))
  if (!all(parent_names %in% pop_names))
    stop("The following parent populations are missing: ", parent_names[!parent_names %in% pop_names], call. = FALSE)

  if (length(populations) != length(unique(sapply(populations, `[[`, "pop"))))
    stop("All populations must have unique names", call. = FALSE)

  # prepare the model output directory
  if (serialize) {
    if (dir.exists(path)) {
      if (!overwrite)
        stop("Directory '", path, "' already exists. Either delete it\nmanually ",
             "or set 'overwrite = TRUE' to delete it from R.", call. = FALSE)
      else {
        if (interactive() && !force) {
          answer <- utils::menu(c("Yes", "No"),
            title = paste0("Are you ABSOLUTELY SURE you want to delete '", path,
                           "'?\nThere will be no going back after this.")
          )
          force <- answer == 1
        }
        if (force)
          unlink(path, recursive = TRUE)
        else
          stop("Compilation aborted because the specified model path directory\ncould ",
               "not have been created. If you're running this in a non-interactive\n",
               "mode in a script and want to overwrite an already existing model\n",
               "directory, you must set `force = TRUE`.", call. = FALSE)
      }

    }
    dir.create(path)
  }

  if (is.data.frame(gene_flow)) gene_flow <- list(gene_flow)

  # gene-flow events cannot span shorter amount of time than the generation time
  valid_gf <- sapply(gene_flow, function(gf) abs(gf$tend - gf$tstart) >= generation_time)
  if (!all(valid_gf)) {
    gf_str <- paste0(
      sapply(gene_flow[!valid_gf],
             function(gf) {
               sprintf("  from %s to %s, start = %s, end = %s (implied length %d)",
                       gf$from_name, gf$to_name, gf$tstart, gf$tend, abs(gf$tend - gf$tstart))
             }),
      collapse = "\n"
    )
    stop(paste0("Gene flows cannot be shorter than the generation time of ",
                generation_time, ".\n",
               "The following gene-flow events are shorter than this:\n", gf_str), call. = FALSE)
  }

  # make sure all populations share the same direction of time
  time_dir <- setdiff(unique(sapply(populations, time_direction)), "unknown")

  if (!is.null(direction) & any(direction != time_dir))
    stop("The direction that was explicitly specified contradicts the direction implied by the model", call. = FALSE)

  if (length(time_dir) > 1)
    stop("Inconsistent direction of time among the specified populations", call. = FALSE)

  if (length(time_dir) == 0 || all(time_dir == "forward")) {
    if (!is.null(direction) && all(direction == "backward"))
      time_dir <- "backward"
    else if (is.null(simulation_length))
      stop("The specified model implies a forward direction of time. However,
forward models require that the 'simulation_length' parameter is explicitly
specified in order to know when to terminate the simulation. If you
intended to run a backward time model instead, you can state this by
setting `direction = 'backward'.`", call. = FALSE)
    else
      time_dir <- "forward"
  }

  # there's no need to specify simulation run length for backward models
  # (those stop at time 0 by default) so we find the oldest time present
  # in the model and take it as the total amount of time for the simulation
  if (time_dir == "backward" || is.null(simulation_length)) {
    end_time <- get_oldest_time(populations, time_dir)
  } else
    end_time <- simulation_length

  split_table <- compile_splits(populations, generation_time, time_dir, end_time)
  admix_table <- compile_geneflows(populations, gene_flow, split_table, generation_time, time_dir, end_time)
  resize_table <- compile_resizes(populations, generation_time, time_dir, end_time, split_table)

  if (serialize && inherits(map, "slendr_map")) {
    if (!is.null(competition)) check_resolution(map, competition)
    if (!is.null(mating)) check_resolution(map, mating)
    if (!is.null(dispersal)) check_resolution(map, dispersal)
    check_resolution(map, resolution)

    map_table <- compile_maps(populations, split_table, resolution, generation_time, time_dir, end_time, path)
    dispersal_table <- compile_dispersals(populations, generation_time, time_dir, end_time, split_table,
                                        resolution, competition, mating, dispersal)

    return_maps <-  map_table[, c("pop", "pop_id", "time_orig", "time_gen", "path")]
  } else {
    map_table <- return_maps <- dispersal_table <- NULL
  }

  simulation_length <- if (is.null(simulation_length)) end_time else simulation_length

  slim_script <- system.file("scripts", "script.slim", package = "slendr")

  if (!is.null(extension)) {
    if (length(extension) == 1 && is.character(extension)) {
      if (file.exists(extension))
        extension_contents <- readLines(extension, warn = FALSE)
      else if (any(grepl("\\{", extension)) && any(grepl("\\}", extension)))
        extension_contents <- strsplit(extension, "\n")[[1]]
      else
        stop("Extension does not appear to be a file path nor a string containing SLiM code",
             call. = FALSE)
    } else
      stop("Extension can be either a (multi-line) R string, or a path to a file",
           call. = FALSE)
    script_contents <- readLines(slim_script, warn = FALSE)

    # check whether the customization snippet contains user-defined genomic
    # initialization code
    custom_init <- check_initialization(extension_contents)

    # if it does, swap it out for the default slendr initialization code
    if (custom_init) {
      init_start <- grep("default slendr neutral initialization -- start", script_contents) + 1
      init_end <- grep("default slendr neutral initialization -- end", script_contents) - 1
      script_contents <- script_contents[-(init_start:init_end)]
    }

    slim_script <- tempfile()

    combined_script <- c(
      script_contents,
      "\n//////////////////////////////////////////////////",
      "// user extension code follows",
      "//////////////////////////////////////////////////\n",
      extension_contents
    )

    writeLines(combined_script, slim_script)

    customized <- TRUE
  } else
    customized <- FALSE

  if (is.null(time_units)) {
    if (generation_time == 1)
      time_units <- "generations"
    else
      time_units <- ""
  }

  if (serialize)
    checksums <- write_model_files(
      path, populations, admix_table, map_table, split_table, resize_table,
      dispersal_table, generation_time, resolution, simulation_length, time_dir, slim_script,
      description, time_units, map
    )
  else
    checksums <- NULL

  names(populations) <- pop_names

  if (!is.null(path)) path <- normalizePath(path, winslash = "/")

  # compile the result
  result <- list(
    path = path,
    world = map,
    populations = populations,
    splits = split_table,
    resizes = resize_table,
    geneflow = admix_table,
    maps = return_maps,
    dispersals = dispersal_table,
    generation_time = generation_time,
    resolution = resolution,
    length = round(simulation_length / generation_time),
    orig_length = simulation_length,
    direction = time_dir,
    description = description,
    time_units = time_units,
    checksums = checksums,
    customized = customized
  )
  class(result) <- set_class(result, "model")

  result
}

#' Read a previously serialized model configuration
#'
#' Reads all configuration tables and other model data from a location
#' where it was previously compiled to by the \code{compile} function.
#'
#' @param path Directory with all required configuration files
#'
#' @return Compiled \code{slendr_model} model object which encapsulates all
#'   information about the specified model (which populations are involved,
#'   when and how much gene flow should occur, what is the spatial resolution
#'   of a map, and what spatial dispersal and mating parameters should be used
#'   in a SLiM simulation, if applicable)
#'
#' @examples
#' \dontshow{check_dependencies(python = TRUE, quit = TRUE) # dependencies must be present
#' }
#' init_env()
#'
#' # load an example model with an already simulated tree sequence
#' path <- system.file("extdata/models/introgression", package = "slendr")
#' model <- read_model(path)
#'
#' plot_model(model, sizes = FALSE, log = TRUE)
#' @export
read_model <- function(path) {
  # paths to files which are saved by the compile() function and are necessary
  # for running the backend script using the run() function
  path_populations <- file.path(path, "ranges.rds")
  path_splits <- file.path(path, "populations.tsv")
  path_resizes <- file.path(path, "resizes.tsv")
  path_geneflow <- file.path(path, "geneflow.tsv")
  path_maps <- file.path(path, "maps.tsv")
  path_generation_time <- file.path(path, "generation_time.txt")
  path_resolution <- file.path(path, "resolution.txt")
  path_length <- file.path(path, "length.txt")
  path_orig_length <- file.path(path, "orig_length.txt")
  path_direction <- file.path(path, "direction.txt")
  path_description <- file.path(path, "description.txt")
  path_time_units <- file.path(path, "time_units.txt")

  if (!dir.exists(path))
    stop(sprintf("Model directory '%s' does not exist", path), call. = FALSE)

  # verify checksums of serialized model configuration files
  checksums <- utils::read.table(file.path(path, "checksums.tsv"), header = TRUE)
  verify_checksums(file.path(path, checksums$file), checksums$hash)

  generation_time <- scan(path_generation_time, what = integer(), quiet = TRUE)
  length <- as.integer(scan(path_length, what = numeric(), quiet = TRUE))
  orig_length <- as.integer(scan(path_orig_length, what = numeric(), quiet = TRUE))
  description <- readLines(path_description)
  time_units <- readLines(path_time_units)

  split_table <- utils::read.table(path_splits, header = TRUE, stringsAsFactors = FALSE)
  resize_table <- NULL
  if (file.exists(path_resizes)) {
    resize_table <- utils::read.table(path_resizes, header = TRUE, stringsAsFactors = FALSE)
  }

  admix_table <- NULL
  if (file.exists(path_geneflow)) {
    admix_table <- utils::read.table(path_geneflow, header = TRUE, stringsAsFactors = FALSE)
    admix_table$overlap <- admix_table$overlap == 1
  }

  populations <- readRDS(path_populations)
  names(populations) <- purrr::map_chr(populations, ~ .x$pop[1])

  if (file.exists(path_maps)) {
    maps <- utils::read.table(path_maps, header = TRUE, stringsAsFactors = FALSE)
    resolution <- scan(path_resolution, what = numeric(), quiet = TRUE)
    world <- attr(populations[[1]], "map")
  } else
    maps <- world <- resolution <- NULL

  direction <- scan(path_direction, what = character(), quiet = TRUE)

  customized <- readLines(file.path(path, "script.slim")) %>%
    grepl("// user extension code follows", .) %>%
    any()

  result <- list(
    path = path,
    world = world,
    populations = populations,
    splits = split_table,
    resizes = resize_table,
    geneflow = admix_table,
    maps = maps,
    generation_time = generation_time,
    resolution = resolution,
    length = length,
    orig_length = orig_length,
    direction = direction,
    description = description,
    time_units = time_units,
    checksums = checksums,
    customized = customized
  )
  class(result) <- set_class(result, "model")
  result
}

hash_file <- function(f) {
  if (grepl("(txt|tsv|slim|py)$", f)) {
    file <- FALSE
    f <- paste(readLines(f), sep = " ")
  } else
    file <- TRUE

  digest::digest(f, algo = "md5", file = file, serialize = FALSE)
}

calculate_checksums <- function(files) {
  if (!all(file.exists(files)))
    stop("Not all compiled files are present", call. = FALSE)

  data.frame(
    file = basename(files),
    hash = as.vector(vapply(files, hash_file, FUN.VALUE = character(1))
    )
  )
}

# Make sure the checksums of a given set of files matches the expectation
verify_checksums <- function(files, hashes) {
  for (i in seq_along(files)) {
    if (hash_file(files[i]) != hashes[i]) {
      warning("Checksum of '", basename(files[i]), "' does not match its compiled state",
              call. = FALSE)
    }
  }
}

# Write a compiled slendr model to disk and return a table of checksums
write_model_files <- function(path, populations, admix_table, map_table, split_table,
                        resize_table, dispersal_table,
                        generation_time, resolution, length, direction,
                        script_source, description, time_units, map) {
  saved_files <- c()

  # table of split times and initial population sizes
  saved_files["splits"] <- file.path(path, "populations.tsv")
  utils::write.table(split_table, saved_files[["splits"]],
                     sep = "\t", quote = FALSE, row.names = FALSE)

  # table of geneflow events
  if (!is.null(admix_table)) {
    saved_files["geneflow"] <- file.path(path, "geneflow.tsv")
    admix_table$overlap <- as.integer(admix_table$overlap)
    utils::write.table(admix_table, saved_files[["geneflow"]],
                       sep = "\t", quote = FALSE, row.names = FALSE)
  }

  if (!is.null(map_table)) {
    # rasterized spatial maps
    for (i in seq_len(nrow(map_table))) {
      saved_files[paste0("map", i)] <- file.path(path, sprintf("%d.png", i))
      map_row <- map_table[i, ]
      save_png(map_row$map[[1]], saved_files[paste0("map", i)])
    }

    # table of paths to raster files
    saved_files["maps"] <- file.path(path, "maps.tsv")
    utils::write.table(
      map_table[, c("pop", "pop_id", "time_orig", "time_gen", "path")],
      saved_files[["maps"]], sep = "\t", quote = FALSE, row.names = FALSE
    )

    saved_files["resolution"] <- file.path(path, "resolution.txt")
    base::write(resolution, saved_files[["resolution"]])
  }

  # table of interaction and dispersal distances
  if (!is.null(dispersal_table)) {
    saved_files["dispersal"] <- file.path(path, "dispersals.tsv")
    utils::write.table(dispersal_table, saved_files[["dispersal"]],
                       sep = "\t", quote = FALSE, row.names = FALSE)
  }

  # serialized population objects
  saved_files["populations"] <- file.path(path, "ranges.rds")
  saveRDS(populations, saved_files[["populations"]])

  # table of scheduled resize events
  if (!is.null(resize_table)) {
    saved_files["resizes"] <- file.path(path, "resizes.tsv")
    utils::write.table(resize_table, saved_files["resizes"], sep = "\t",
                       quote = FALSE, row.names = FALSE)
  }

  saved_files["generation_time"] <- file.path(path, "generation_time.txt")
  saved_files["length"] <- file.path(path, "length.txt")
  saved_files["orig_length"] <- file.path(path, "orig_length.txt")
  saved_files["direction"] <- file.path(path, "direction.txt")
  saved_files["description"] <- file.path(path, "description.txt")
  saved_files["time_units"] <- file.path(path, "time_units.txt")
  base::write(generation_time, file.path(path, "generation_time.txt"))
  base::write(round(length / generation_time), file.path(path, "length.txt"))
  base::write(length, file.path(path, "orig_length.txt"))
  base::write(direction, file.path(path, "direction.txt"))
  base::write(description, file.path(path, "description.txt"))
  base::write(time_units, file.path(path, "time_units.txt"))

  saved_files["slim_script"] <- file.path(path, "script.slim")
  saved_files["msprime_script"] <- file.path(path, "script.py")
  write_script(saved_files["slim_script"], script_source, map, resolution)
  write_script(saved_files["msprime_script"],
               system.file("scripts/script.py", package = "slendr"))

  checksums <- calculate_checksums(saved_files)
  utils::write.table(checksums, file.path(path, "checksums.tsv"), sep = "\t",
                     quote = FALSE, row.names = FALSE)

  checksums
}

write_script <- function(script_target, script_source,
                         map = NULL, resolution = NULL, description = "") {
  # copy the script to the dedicated model directory, replacing the
  # placeholders for model directory and slendr version accordingly
  script_code <- readLines(script_source) %>%
    gsub("__VERSION__", paste0("slendr_", utils::packageVersion("slendr")), .)

  if (!is.null(map)) {
    crs <- ifelse(has_crs(map), sf::st_crs(map)$epsg, "NULL")
    extent <- paste(deparse(as.vector(sf::st_bbox(map))), collapse = "")
    script_code <- script_code %>%
      gsub("__CRS__", as.character(crs), .) %>%
      gsub("__EXTENT__", extent, .) %>%
      gsub("__RESOLUTION__", as.character(resolution), .)
  }
  cat(script_code, file = script_target, sep = "\n")

  script_target
}

# Iterate over population objects and convert he information about
# population split hierarchy and split times into a data frame
compile_splits <- function(populations, generation_time, direction, end_time) {
  split_table <- lapply(populations, function(p) {
    parent <- attr(p, "parent")
    if (is.character(parent) && parent == "__pop_is_ancestor") {
      parent_name <- parent
    } else {
      parent_name <- unique(attr(p, "parent")$pop)
    }

    tremove <- attr(p, "remove")
    tsplit <- attr(p, "history")[[1]]$time

    data.frame(
      pop = unique(p$pop),
      parent = parent_name,
      tsplit = tsplit,
      N = attr(p, "history")[[1]]$N,
      tremove = ifelse(!is.null(tremove), tremove, -1),
      stringsAsFactors = FALSE
    )
  }) %>% do.call(rbind, .)

  # convert times into a forward direction
  split_table <- convert_to_forward(
    split_table,
    direction = direction,
    columns = c("tsplit", "tremove"),
    start_time = get_oldest_time(populations, direction),
    end_time = end_time,
    generation_time = generation_time
  )

  # order populations by split time and assign a numeric identifier to each
  split_table <- split_table[
    order(split_table$tsplit_gen, decreasing = FALSE, na.last = FALSE), ]
  split_table$pop_id <- seq_len(nrow(split_table)) - 1
  split_table$parent_id <- lapply(
    split_table$parent,
    function(i) {
      if (i == "__pop_is_ancestor") -1
      else split_table[split_table$pop == i, ]$pop_id
    }
  ) %>% unlist()

  # if a population is ancestral (without a parent), it should appear in the
  # simulation in generation 1 regardless of which "time of appearance" was
  # specified (to include it in the burnin)
  split_table %>%
    dplyr::mutate(tsplit_gen = ifelse(parent == "__pop_is_ancestor", 1, tsplit_gen))
}

# Process vectorized population boundaries into a table with
# rasterized map objects
compile_maps <- function(populations, split_table, resolution, generation_time,
                         direction, end_time, dir) {
  # generate rasterized maps
  maps <- render(populations, resolution)

  # convert list of rasters into data frame, adding the spatial
  # maps themselves as a list column
  map_table <- lapply(maps, function(m) {
    as.data.frame(m[c("pop", "time")], stringsAsFactors = FALSE)
  }) %>%
    do.call(rbind, .)
  # add column with a numeric population identifier (used later by SLiM)
  map_table$pop_id <- unlist(lapply(
    map_table$pop,
    function(i) split_table[split_table$pop == i, ]$pop_id
  ))
  map_table$map <- I(lapply(maps, function(m) m$map))

  map_table <- convert_to_forward(
    map_table,
    direction = direction,
    columns = "time",
    start_time = get_oldest_time(populations, direction),
    end_time = end_time,
    generation_time = generation_time
  )

  # number maps sequentially in the order SLiM will be swapping them
  # later (each map number X corresponds to X.png)
  map_table <- map_table[order(map_table$time_gen, na.last = FALSE), ]
  # in some situations, multiple maps are scheduled for a single generation
  # for one population - this removes the duplicates, but ideally this kind
  # of problem should be caught somewhere upstream
  map_table <- map_table[!duplicated(map_table[, c("pop", "time_gen")]), ]
  map_table$path <- seq_len(nrow(map_table)) %>% paste0(., ".png")

  # maps of ancestral populations (that is, those that are spatial) have to
  # be set in the first generation, regardless of the specified split time
  ancestral_pops <- split_table[split_table$parent == "__pop_is_ancestor", ]$pop
  ancestral_maps <- purrr::map(ancestral_pops, ~ which(map_table$pop == .x)) %>%
    purrr::map_int(~ .x[1])
  if (any(!is.na(ancestral_maps)))
    map_table[ancestral_maps[!is.na(ancestral_maps)], ]$time_gen <- 1

  map_table
}


compile_geneflows <- function(populations, geneflow, split_table, generation_time,
                              direction, end_time) {
  if (length(geneflow) == 0)
    return(NULL)

  admix_table <- do.call(rbind, geneflow)
  admix_table <- convert_to_forward(
    admix_table,
    direction = direction,
    columns = c("tstart", "tend"),
    start_time = get_oldest_time(populations, direction),
    end_time = end_time,
    generation_time = generation_time
  )
  names(admix_table)[1:2] <- c("from", "to")

  # convert population names and their parents' names to SLiM numbers
  admix_table$from_id <- unlist(lapply(
    admix_table$from,
    function(i) split_table[split_table$pop == i, ]$pop_id
  ))
  admix_table$to_id <- unlist(lapply(
    admix_table$to,
    function(i) split_table[split_table$pop == i, ]$pop_id
  ))

  admix_table
}


# Compile table of population resize events
compile_resizes <- function(populations, generation_time, direction,
                            end_time, split_table) {
  resize_events <- lapply(populations, function(p) {
    lapply(attr(p, "history"), function(event) {
      if (unique(event$event) == "resize") event
    }) %>% do.call(rbind, .)
  }) %>% do.call(rbind, .)

  if (is.null(resize_events))
    return(NULL)
  else
    resize_events$tend[is.na(resize_events$tend)] <- -1

  resize_table <- convert_to_forward(
    resize_events,
    direction = direction,
    columns = c("tresize", "tend"),
    start_time = get_oldest_time(populations, direction),
    end_time = end_time,
    generation_time = generation_time
  )

  resize_table$pop_id <- sapply(
    resize_table$pop,
    function(i) split_table[split_table$pop == i, ]$pop_id
  ) %>% as.numeric

  resize_table[, c("pop", "pop_id", "how", "N", "prev_N",
                   "tresize_orig", "tresize_gen", "tend_orig", "tend_gen")]
}

# Compile table of population resize events
compile_dispersals <- function(populations, generation_time, direction,
                               end_time, split_table, resolution,
                               competition, mating, dispersal) {
  dispersal_events <- lapply(populations, function(p) {
    lapply(attr(p, "history"), function(event) {
      if (unique(event$event) == "split") {
        event$N <- NULL
        names(event) <- c("pop", "event", "time", "competition",
                          "mating", "dispersal", "dispersal_fun")
        event$event <- "dispersal"
        event
      } else if (unique(event$event) == "dispersal") {
        event
      }
    }) %>% do.call(rbind, .)
  }) %>% do.call(rbind, .)

  if (is.null(dispersal_events))
    return(NULL)

  dispersal_events$tdispersal <- dispersal_events$time
  dispersal_events$time <- NULL

  dispersal_table <- convert_to_forward(
    dispersal_events,
    direction = direction,
    columns = "tdispersal",
    start_time = get_oldest_time(populations, direction),
    end_time = end_time,
    generation_time = generation_time
  )

  dispersal_table$pop_id <- sapply(
    dispersal_table$pop,
    function(i) split_table[split_table$pop == i, ]$pop_id
  ) %>% as.numeric

  # take care of missing interactions and offspring distances
  dispersal_table <- set_distances(dispersal_table, resolution, competition,
                                   mating, dispersal)

  dispersal_table <- dispersal_table[order(dispersal_table$tdispersal_gen, na.last = FALSE), ]

  # dispersals of ancestral populations have to be set in the first generation,
  # regardless of the specified split time
  ancestral_pops <- split_table[split_table$parent == "__pop_is_ancestor", ]$pop
  indices <- purrr::map(ancestral_pops, ~ which(dispersal_table$pop == .x)) %>%
    purrr::map_int(~ .x[1])
  dispersal_table[indices, ]$tdispersal_gen <- 1

  dispersal_table[, c("pop", "pop_id", "tdispersal_gen", "tdispersal_orig",
                      "competition", "mating", "dispersal",
                      "dispersal_fun")]
}


# Render population boundaries to black-and-white spatial maps
render <- function(pops, resolution) {
  # process only populations which have a map
  spatial_pops <- Filter(has_map, pops)

  raster_list <- lapply(spatial_pops, function(pop) {
    # iterate over temporal maps for the current population
    snapshots <- lapply(unique(pop$time), function(t) {
      snapshot <- pop[pop$time == t, ]
      class(snapshot) <- set_class(snapshot, "pop")

      # render the population if needed
      if (is.null(attr(pop, "intersected")))
        snapshot <- intersect_features(snapshot)

      raster_map <- rasterize(snapshot, resolution)

      # return the rendered spatial map with the population name and the
      # appropriate time stamp (unique-ing because intersecting splits
      # the spatial object into multiple disjoint features)
      list(
        pop = unique(snapshot$pop),
        time = unique(snapshot$time),
        map = raster_map
      )
    })
    snapshots
  })

  # flatten the list of ggplot objects
  rasters <- do.call(c, raster_list)

  rasters
}

on_cran_windows <- function() {
  on_cran <- !identical(Sys.getenv("NOT_CRAN"), "true")
  on_windows <- tolower(Sys.info()[["sysname"]]) == "windows"
  on_cran && on_windows
}

# Rasterize the vector form of a population spatial boundary
rasterize <- function(x, resolution) {
  # add a dummy variable for plotting the bi-color map
  x$fill <- factor(1)

  # create a template object for rasterization (i.e. size of the final raster)
  if (inherits(x, "slendr_map"))
    bbox <- sf::st_bbox(x)
  else
    bbox <- sf::st_bbox(attr(x, "map"))

  template <- stars::st_as_stars(bbox, dx = resolution, dy = resolution)

  # perform the rasterization using the dummy single-value factor column

  # Windows CI machines have started giving mysterious errors, possibly due to
  # some sf-starts-GDAL changes? The full warning is this:
  # "GDAL Message 1: The definition of geographic CRS EPSG:4258 got from GeoTIFF keys is not the
  # same as the one from the EPSG registry, which may cause issues during reprojection operations.
  # Set GTIFF_SRS_SOURCE configuration option to EPSG to use official parameters (overriding the
  # ones from GeoTIFF keys), or to GEOKEYS to use custom values from GeoTIFF keys and drop the
  # EPSG code."
  # Because spatial SLiM simulations on Windows are not yet supported, I will silence this
  # call on Windows for the time being because these warnings are turned into errors on CRAN,
  # making all tests failing -- including functionality used by users who have no intention of
  # running spatial simulations.
  # wrapper <- if (on_cran_windows()) suppressWarnings else identity
  # raster <- wrapper(stars::st_rasterize(x["fill"], template))
  raster <- stars::st_rasterize(x["fill"], template)

  if (length(table(raster$ID)) == 1) {
    stop(sprintf("
The generated raster map of %s at time %s is blank.
This would cause SLiM to crash as it would not be able to place
any individuals on the map. Please check the spatial boundary for
this population at this time point.", x$pop, x$time), call. = FALSE)
  }

  pixel_values <- unique(as.numeric(raster$fill))
  if (length(pixel_values) == 1 && pixel_values == 0)
    stop("No occupiable pixel on a rasterized map for population '",
         x$pop[1], "' at time ", x$time[1], ". Make sure that the specified",
         " population boundary has sufficient space for the population to occupy.",
         call. = FALSE)

  raster
}


# Save the rasterized stars object to a PNG file
save_png <- function(raster, path) {
  tmp_tiff <- paste0(tempfile(), ".tiff")

  # write stars raster as a TIFF format
  stars::write_stars(raster, tmp_tiff)

  # convert the stars TIFF into a PNG (the only format SLiM supports)
  img <- ijtiff::read_tif(tmp_tiff, msg = FALSE)
  unlink(tmp_tiff)

  # subset the multidimensional array only to pixel two-dimensional matrix
  img_matrix <- img[, , 1, 1]

  # binarize the matrix (st_rasterize assigns a different color to each
  # fragmented spatial feature after intersect_features() call)
  img_matrix[img_matrix > 0] <- 1

  png::writePNG(img_matrix, path)
}


# Convert times given in specified columns of a data frame into
# a SLiM forward direction given in generations
convert_to_forward <- function(df, direction, columns, start_time, end_time, generation_time) {
  for (column in columns) {
    times <- df[[column]]

    # if necessary, convert to forward direction
    if (direction == "backward")
      times[times != -1] <- end_time - times[times != -1] + generation_time
    else if (direction == "forward")
      times[times != -1] <- times[times != -1] - start_time + generation_time
    else
      stop("Invalid model direction. This is a critical error in slendr, please report it!", call. = FALSE)

    # convert to generations
    times[times != -1] <- as.integer(round(times[times != -1] / generation_time))

    df[[paste0(column, "_gen")]] <- times
    df[[paste0(column, "_orig")]] <- df[[column]]
    df[[column]] <- NULL
  }
  df
}
