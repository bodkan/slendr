library(sf)
library(tidyverse)
library(rnaturalearth)
library(hexSticker)
library(here)

map <- ne_load(
  scale = 110, type = "land", category = "physical",
  destdir = "~/Google/postdoc/data/ne_data",
  returnclass = "sf"
)
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
  20, -7, 4,
  25, 5, 5,
  30, 18, 5.1,
  35, 10, 6,
  35, -7, 7,
  40, 37, 8,
  25, 40, 9,
  12, 48, 10,
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
  guides(color = F, fill = F) +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  theme_void() +
  theme_transparent() +
  geom_rect(aes(xmin = -28, xmax = 80, ymin = -32, ymax = -10),
            fill = "lightblue", alpha = 0.8)

sticker(
  p, package = "stamp",
  p_size = 8, p_y = 0.46, p_color = "black",
  h_color = "black", h_fill = "#0077be",
  s_y = 1.05, s_x = 1.05, s_width = 2.5, s_height = 2.3,
  white_around_sticker = T,
  filename = file.path(".", "logo.png")
) %>% print()

usethis::use_logo("logo.png")
unlink("logo.png")
