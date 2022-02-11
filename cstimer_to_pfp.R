# OtterCuber
# cstimer_to_pfp
# 2/11/2022

# Load packages
library(tidyverse)
library(rjson)
library(tidyjson)
library(jsonlite)
library(viridis)
library(ggdark)
library(lubridate)
library(magick)
library(ggpubr)
library(png)

# Import cstimer .txt file
df <- jsonlite::fromJSON("cstimer_export_sample.txt") %>%
  within(rm(properties)) %>%
  gather_array %>%
  unnest_wider(..JSON, names_sep = "_") %>%
  setNames(letters[1:6]) %>%
  select(c:f) %>%
  unnest_wider(c, names_sep = "_") %>%
  select(time = c_2, date = f) %>%
  mutate(time = time / 1000,
         date = as_date(as_datetime(date)),
         day = as.integer(factor(date))) %>%
  filter(time <= 60) %>% # only look at <= 60 second times
  mutate(t = cut(time, breaks = seq(1, 60, .1), labels = F)) %>%
  select(day, t)

# Function to generate pfp
get_pfp <- function(color_scheme, dir = -1, b = 0, e = 1) {
  
  # Plot
  p <- df %>%
    ggplot(aes(x = day, y = t, fill = t)) +
    geom_tile() +
    scale_fill_viridis(option = color_scheme, 
                       direction = dir, begin = b, end = e) +
    coord_polar() +
    dark_theme_void() +
    theme(legend.position = "none")
  
  # Transform and save
  f <- paste0("pfp_", color_scheme, ".png")
  ggsave(plot = p, width = 10, height = 10, filename = f)
  im <- image_read(f)
  im1 <- image_crop(im, "1000x1000+1000+1000")
  fig <- image_draw(image_blank(1000, 1000))
  symbols(500, 500, circles = 500, bg = "black", inches = F, add = T)
  dev.off()
  im2 <- image_composite(im1, fig, operator= "copyopacity")
  im3 <- image_background(im2, "none")
  image_write(im3, path = f, format = "png")
  
  # Return ggplot
  return(ggplot() + background_image(readPNG(f)))
}

# Generate pfps
pfps <- lapply(LETTERS[1:8], get_pfp)

# Save thumbnail
images <- image_read(paste0("pfp_", LETTERS[1:8], ".png"))
thumbnail <- image_montage(images, tile = "4x2", bg = "none")
image_write(thumbnail, "thumbnail.png", format = "png")