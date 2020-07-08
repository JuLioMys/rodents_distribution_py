#====== Codigo del tutorial en la pagina https://luisdva.github.io/rstats/richness/ ##


# load packages
library(sf)
library(dplyr)
library(ggplot2)
library(scico)
library(rnaturalearth)
library(purrr)
library(smoothr)
library(rgbif)

# world map
worldMap <- ne_countries(scale = "medium", type = "countries", returnclass = "sf")
# country subset
CRpoly <- worldMap %>% filter(sovereignt == "Costa Rica")

# random points tables as a named list
sp_occ <- rerun(12, st_sample(CRpoly, sample(3:20, 1)))
names(sp_occ) <- paste0("sp_", letters[1:length(sp_occ)])

# to sf object
sflisss <-
  purrr::map(sp_occ, st_sf) %>%
  map2(., names(.), ~ mutate(.x, id = .y))

sp_occ_sf <- sflisss %>% reduce(rbind)

# to multipoint
sp_occ_sf <- sp_occ_sf %>%
  group_by(id) %>%
  summarise()

# trim to study area
limsCR <- st_buffer(CRpoly, dist = 0.7) %>% st_bbox()

# neighboring countries
adjacentPolys <- st_touches(CRpoly, worldMap)
neighbours <- worldMap %>% slice(pluck(adjacentPolys, 1))

# countries
divpolPlot <-
  ggplot() +
  geom_sf(data = neighbours, color = "white") +
  geom_sf(data = CRpoly) +
  coord_sf(
    xlim = c(limsCR["xmin"], limsCR["xmax"]),
    ylim = c(limsCR["ymin"], limsCR["ymax"])
  ) +
  scale_x_continuous(breaks = c(-84)) +
  theme(
    plot.background = element_rect(fill = "#f1f2f3"),
    panel.background = element_rect(fill = "#2F4051"),
    panel.grid = element_blank(),
    line = element_blank(),
    rect = element_blank()
  )
divpolPlot

# plot points
spPointsPlot <-
  ggplot() +
  geom_sf(data = neighbours, color = "white") +
  geom_sf(data = CRpoly) +
  geom_sf(data = sp_occ_sf, aes(fill = id), pch = 21) +
  scale_fill_scico_d(palette = "davos", direction = -1, end = 0.9, guide = FALSE) +
  coord_sf(
    xlim = c(limsCR["xmin"], limsCR["xmax"]),
    ylim = c(limsCR["ymin"], limsCR["ymax"])
  ) +
  scale_x_continuous(breaks = c(-84)) +
  theme(
    plot.background = element_rect(fill = "#f1f2f3"),
    panel.background = element_rect(fill = "#2F4051"),
    panel.grid = element_blank(),
    line = element_blank(),
    rect = element_blank()
  )
spPointsPlot
