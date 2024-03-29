library(sf)
library(tidyverse)
library(rnaturalearth)
library(hexSticker)
library(here)

set.seed(3141592)

ne_dir <- tempdir()
ne_file <- file.path(ne_dir, "ne_110m_land.zip")
download.file(
  url = "https://naturalearth.s3.amazonaws.com/110m_physical/ne_110m_land.zip",
  destfile = ne_file, quiet = TRUE
)
unzip(ne_file, exdir = ne_dir)
map <- rnaturalearth::ne_load(
  scale = "small", type = "land", category = "physical",
  returnclass = "sf", destdir = ne_dir
) %>% st_make_valid

st_agr(map) <- "constant"
map <- st_crop(
  map,
  st_bbox(c(xmin = -28, xmax = 80, ymin = -49, ymax = 70),
          crs = 4326)
)

points_df <- tribble(
  ~lon, ~lat, ~id,
  -5, 20, 1,
  10, 10, 2,
  20, 25, 3,
  20, -4, 4,
  25, 5, 5,
  30, 18, 5.1,
  35, 10, 6,
  35, -4, 7,
  40, 37, 8,
  25, 40, 9,
  12, 48, 10,
  27, 53, 10.1,
  45, 43, 11,
  62, 35, 12,
  45, 30, 13,
  55, 20, 14,
  45, 17, 15
) %>% mutate(id = factor(id))

edges_df <- tribble(
  ~from, ~to,
  1, 2,
  1, 3,
  2, 4,
  2, 5,
  3, 5.1,
  3, 8,
  6, 15,
  5, 6,
  8, 9,
  9, 10,
  9, 10.1,
  8, 11,
  15, 13,
  13, 12,
  15, 14,
  5, 7
) %>% mutate(
  x = sapply(from, function(i) filter(points_df, id == i)$lon),
  y = sapply(from, function(i) filter(points_df, id == i)$lat),
  xend = sapply(to, function(i) filter(points_df, id == i)$lon),
  yend = sapply(to, function(i) filter(points_df, id == i)$lat),
  from = factor(from), to = factor(to)
)

p <- ggplot() +
  geom_sf(data = map, color = NA) +
#  geom_label(data = points_df, aes(lon, lat, label = id)) +
  geom_segment(data = edges_df, aes(x = x, y = y, xend = xend, yend = yend, color = from)) +
  geom_point(data = points_df, aes(lon, lat, color = id), size = 1.5) +
  guides(color = "none", fill = "none") +
  theme_void() +
  theme_transparent() +
  geom_rect(aes(xmin = -30, xmax = 80, ymin = -22, ymax = -7), fill = "white",
            color = "black")

sticker(
  p, package = "slendr",
  p_size = 21, p_y = 0.35, p_color = "black",
  h_color = "black", h_fill = "#0077be",
  s_y = 0.9, s_x = 0.99, s_width = 2.7, s_height = 2.7,
  white_around_sticker = TRUE,
  filename = file.path(".", "logo.png")
)

usethis::use_logo("logo.png")
system("convert man/figures/logo.png -define png:include-chunk=none man/figures/logo.png")
unlink("logo.png")