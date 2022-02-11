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

# Plot
p <- df %>%
  ggplot(aes(x = day, y = t, fill = t)) +
  geom_tile() +
  scale_fill_viridis(option = "plasma", direction = -1) +
  coord_polar() +
  dark_theme_void() +
  theme(legend.position = "none")

# Transform and save
ggsave(plot = p, width = 10, height = 10, filename = "pfp.png")
im <- magick::image_read("pfp.png")
ii <- magick::image_info(im)
ii_min <- min(ii$width, ii$height)
im1 <- magick::image_crop(im, "2000x2000+500+500")
fig <- magick::image_draw(image_blank(2000, 2000))
symbols(1000, 1000, circles=1000, bg='black', inches=FALSE, add=TRUE)
dev.off()
im2 <- magick::image_composite(im1, fig, operator='copyopacity')
im3 <- magick::image_background(im2, 'gray20')
image_write(im3, path = "pfp.png", format = "png")