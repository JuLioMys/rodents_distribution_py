
library(rgbif)
library(dplyr)



###################

# world map
worldMap <- ne_countries(scale = "medium", type = "countries", returnclass = "sf")

# country subset
PYpoly <- worldMap %>% filter(sovereignt == "Paraguay")

# trim to study area
limsPY <- st_buffer(PYpoly, dist = 0.7) %>% st_bbox()

# neighboring countries
adjacentPolys <- st_touches(PYpoly, worldMap)
neighbours <- worldMap %>% slice(pluck(adjacentPolys, 1))

# countries
divpolPlot <-
  ggplot() +
  geom_sf(data = neighbours, color = "white") +
  geom_sf(data = PYpoly) +
  coord_sf(
    xlim = c(limsPY["xmin"], limsPY["xmax"]),
    ylim = c(limsPY["ymin"], limsPY["ymax"])
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

# grid
PYGrid <- PYpoly %>%
  st_make_grid(cellsize = 0.2) %>%
  st_intersection(PYpoly) %>%
  st_cast("MULTIPOLYGON") %>%
  st_sf() %>%
  mutate(cellid = row_number())



















# occurrences of rodents in Paraguay
name_suggest(q = "rodentia")
rodents_py <- occ_search(orderKey = 1459, 
                        country = "PY",
                        basisOfRecord = "PRESERVED_SPECIMEN",
                        limit = 10000
                        )$data # $data devuelve solo el dataframe de la lista

# filter specimens and coordinates

PYrodentsXY <- rodents_py %>% 
             select(species, decimalLongitude, decimalLatitude) %>% 
             na.omit()

#Holochilus_occ <- rodents_py %>% 
#       select(genus, species, decimalLongitude, decimalLatitude) %>% 
#       na.omit() %>% 
#       filter(genus== "Holochilus")

#H_chacarius_XY <- Holochilus_occ %>% 
#                   filter(species== "Holochilus chacarius")

# to sf object, specifying variables with coordinates and projection
PYrodentsXYsf <- st_as_sf(PYrodentsXY, coords = c("decimalLongitude", "decimalLatitude"), crs = 4326) %>%
  group_by(species) %>%
  summarize()

# plot points
RodentsPointsPlot <-
  ggplot() +
  geom_sf(data = neighbours, color = "white") +
  geom_sf(data = PYpoly) +
  geom_sf(data = PYrodentsXYsf, pch = 21) +
  coord_sf(
    xlim = c(limsPY["xmin"], limsPY["xmax"]),
    ylim = c(limsPY["ymin"], limsPY["ymax"])
  ) +
  scale_x_continuous(breaks = c(-84)) +
  theme(
    plot.background = element_rect(fill = "#f1f2f3"),
    panel.background = element_rect(fill = "#2F4051"),
    panel.grid = element_blank(),
    line = element_blank(),
    rect = element_blank()
  )
RodentsPointsPlot

# rodent richness
rodent_richness_grid <- PYGrid %>%
  st_join(PYrodentsXYsf) %>%
  mutate(overlap = ifelse(!is.na(species), 1, 0)) %>%
  group_by(cellid) %>%
  summarize(num_species = sum(overlap))

# plot
RodentsRichPY <-
  ggplot(rodent_richness_grid) +
  geom_sf(data = neighbours, color = "white") +
  geom_sf(data = PYpoly, fill = "grey", size = 0.1) +
  geom_sf(aes(fill = num_species), color = NA) +
  scale_fill_scico(palette = "davos", direction = -1, end = 0.9, name = "Rodent species richness") +
  coord_sf(
    xlim = c(limsPY["xmin"], limsPY["xmax"]),
    ylim = c(limsPY["ymin"], limsPY["ymax"])
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
RodentsRichPY



