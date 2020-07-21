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

# convex hulls
spEOOs <- st_convex_hull(sp_occ_sf) %>% smooth()

# plot hulls
hullsPlot <-
  ggplot() +
  geom_sf(data = neighbours, color = "white") +
  geom_sf(data = CRpoly) +
  geom_sf(data = spEOOs, aes(fill = id), alpha = 0.7) +
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
hullsPlot


# grid
CRGrid <- CRpoly %>%
  st_make_grid(cellsize = 0.2) %>%
  st_intersection(CRpoly) %>%
  st_cast("MULTIPOLYGON") %>%
  st_sf() %>%
  mutate(cellid = row_number())

# cell richness
richness_grid <- CRGrid %>%
  st_join(sp_occ_sf) %>%
  mutate(overlap = ifelse(!is.na(id), 1, 0)) %>%
  group_by(cellid) %>%
  summarize(num_species = sum(overlap))

# richness for convex hulls
richness_gridEOO <- CRGrid %>%
  st_join(spEOOs) %>%
  mutate(overlap = ifelse(!is.na(id), 1, 0)) %>%
  group_by(cellid) %>%
  summarize(num_species = sum(overlap))

# empty grid
gridPlot <-
  ggplot() +
  geom_sf(data = neighbours, color = "white") +
  geom_sf(data = CRpoly) +
  geom_sf(data = CRGrid) +
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
gridPlot

# richness
gridRichCR <-
  ggplot(richness_grid) +
  geom_sf(data = neighbours, color = "white") +
  geom_sf(data = CRpoly, fill = "grey", size = 0.1) +
  geom_sf(aes(fill = num_species), color = NA) +
  scale_fill_scico(palette = "davos", direction = -1, end = 0.9) +
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
  ) + labs(fill = "richness")
gridRichCR

# richness for convex hulls
gridRichCR_eoo <-
  ggplot(richness_gridEOO) +
  geom_sf(data = neighbours, color = "white") +
  geom_sf(data = CRpoly, fill = "grey", size = 0.1) +
  geom_sf(aes(fill = num_species), color = NA) +
  scale_fill_scico(palette = "davos", direction = -1, end = 0.9) +
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
  ) + labs(fill = "richness")
gridRichCR_eoo


###################################################
# bat data
name_suggest(q = "chiroptera")
CRbatsout <- occ_search(
  orderKey = 734, country = "CR",
  basisOfRecord = "PRESERVED_SPECIMEN", limit = 3000
)$data
# species and lat long data
CRbatsXY <- CRbatsout %>%
  select(species, decimalLongitude, decimalLatitude) %>%
  na.omit()

# to sf object, specifying variables with coordinates and projection
CRbatsXYsf <- st_as_sf(CRbatsXY, coords = c("decimalLongitude", "decimalLatitude"), crs = 4326) %>%
  group_by(species) %>%
  summarize()

# plot points
batPointsPlot <-
  ggplot() +
  geom_sf(data = neighbours, color = "white") +
  geom_sf(data = CRpoly) +
  geom_sf(data = CRbatsXYsf, pch = 21) +
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
batPointsPlot


# bat richness
bat_richness_grid <- CRGrid %>%
  st_join(CRbatsXYsf) %>%
  mutate(overlap = ifelse(!is.na(species), 1, 0)) %>%
  group_by(cellid) %>%
  summarize(num_species = sum(overlap))

# plot
batRichCR <-
  ggplot(bat_richness_grid) +
  geom_sf(data = neighbours, color = "white") +
  geom_sf(data = CRpoly, fill = "grey", size = 0.1) +
  geom_sf(aes(fill = num_species), color = NA) +
  scale_fill_scico(palette = "davos", direction = -1, end = 0.9, name = "Bat species richness") +
  coord_sf(
    xlim = c(limsCR["xmin"], limsCR["xmax"]),
    ylim = c(limsCR["ymin"], limsCR["ymax"])
  ) +
  scale_x_continuous(breaks = c(-84)) +
  theme(
    plot.background = element_rect(fill = "#f1f2f3"),
    panel.background = element_rect(fill = "#2F4051"),
    panel.grid = element_blank(),
    legend.position = "bottom",
    line = element_blank(),
    rect = element_blank()
  ) + labs(fill = "richness")
batRichCR



