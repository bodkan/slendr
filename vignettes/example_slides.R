#' ---
#' title: Full example of a spatial model definition and compilation
#' subtitle: This is a subtitle
#' author: Martin Petr
#' date: \today
#' output:
#'  binb::metropolis:
#'   toc: true
#'   slide_level: 2
#' classoption: aspectratio=169
#' vignette: >
#'  %\VignetteIndexEntry{binb Metropolis Demo}
#'  %\VignetteKeywords{spammr,vignette}
#'  %\VignettePackage{spammr}
#'  %\VignetteEngine{knitr::rmarkdown}
#' ---

#+ include = FALSE
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 7,
  fig.height = 5,
  dpi = 100
)

#+ include = FALSE
devtools::load_all("~/projects/spammr")

#' # Defining the world context and smaller geographic regions

#' ## World map and Coordinate Reference System

#' ::: {.columns}

#' :::: {.column width="30%"}

#+ results = F, message = F
world <- world_map(
  lon_range = c(-15, 60),
  lat_range = c(20, 65),
  crs = "EPSG:3035"
)

#' ::::

#' :::: {.column width="60%"}

#' \pause

#+ echo = F
plot(world)

#' ::::

#' :::

#' # Geographic regions

#' ## Africa

africa <- region(
  "Africa", world,
  coords = list(
    c(-18, 20), c(40, 20), c(30, 33),
    c(20, 32), c(10, 35), c(-8, 35)
  )
)

#' ## Europe

europe <- region(
  "Western Europe", world,
  coords = list(c(-8, 35), c(-5, 36), c(10, 38), c(20, 35), c(25, 35),
                c(28, 45), c(20, 58), c(-5, 60), c(-15, 50))
)

#' ## Anatolia

anatolia <- region(
  "Anatolia", world,
  coords = list(c(28, 35), c(40, 35), c(42, 40),
                c(30, 43), c(27, 40), c(25, 38))
)

#' ## Europe _and_ Anatolia

europe_anatolia <-region(
  "Western Europe & Anatolia", world,
  coords = list(c(-10, 35), c(-5, 35), c(10, 38), c(20, 35), c(38, 35),
                c(40, 40), c(30, 45), c(20, 58), c(-5, 60), c(-15, 50))
)

#' # Population spatial ranges

#' ## Africans

afr <- population(
  "AFR", parent = "ancestor", Ne = 1000,
  world = world, region = africa
)

#+ fig.width = 8, fig.height = 12
plot(afr)

#' # Model compilation

#' ## Compile all maps in a bitmap rasterized form

#+ eval = FALSE
compile(afr, output_dir = "model/", overwrite = TRUE)


#' ## World map and Coordinate Reference System

#' ::: {.columns}

#' :::: {.column width="30%"}

#+ results = F, message = F
ls()

#' ::::

#' :::: {.column width="60%"}

#' \pause

#+ echo = F
plot(world)

#' ::::

#' :::




#' ## World map and Coordinate Reference System take TWO

#' ::: {.columns}

#' :::: {.column width="60%"}

#+ echo = F
plot(world)

#' ::::

#' :::: {.column width="40%"}

#' \pause

#' ::: incremental
#' - one
#' - two
#' - three
#' :::

#' ::::

#' :::