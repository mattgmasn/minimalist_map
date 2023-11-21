#### Creating Minimalist Map ####

# Load packages
library(tidyverse)
library(sf)
library(osmdata)

#### Getting OSM Data ####

# Input the city/place to base the map on
place <- "Liverpool"

# Get OSM data for different types of road, street and paths (values selected from list of OSM highways https://wiki.openstreetmap.org/wiki/Key:highway)
streets_osm <- opq(place, timeout = 50) %>%
  add_osm_feature(
    key = "highway",
    value = c(
      "motorway",
      "trunk",
      "primary",
      "secondary",
      "tertiary",
      "unclassified",
      "residential",
      "service",
      "footway"
    )
  ) %>%
  osmdata_sf()

# Select the lines from OSM data only
streets <- streets_osm$osm_lines

#### Create Circle to Define the Borders of the Map ####

# Find and define the local coordinate reference system (crs) of the area being mapped 
crs_local <- 27700 # crs for the UK (https://epsg.io/27700)

# Define centre of map (I start from the postcode I want to use and then find the lat and long via https://postcodes.io/)
centre <- c(lat = 53.40142,
            long = -2.96511)

# Define radius of circle in metres
radius <-  5000

# Create circle given centre and radius
circle <- tibble(lat = centre["lat"], long = centre["long"]) %>% 
  st_as_sf(coords = c("long", "lat"), crs = 4326) %>%
  st_transform(crs = crs_local) %>% 
  st_buffer(dist = radius) %>% 
  st_transform(crs = 4326)

# Crop the streets to the circle
streets_cropped <- streets %>% st_intersection(circle)

#### Define Point Locations to Add to the Map ####

# Get point locations to add to map (again, I started from a postcode and found the lat and long)
points <- tibble(lat = 53.40142,
                 long = -2.96511)

# Create a spatial object for the map
points_sf <- points %>% 
  st_as_sf(coords = c("long", "lat"), crs = 4326)

#### Create and Save the Map ####

# Create map with OSM attribution
minimalist_map <- ggplot() +
  geom_sf(data = streets_cropped,
          size = .2,
          color = "grey40") +
  geom_sf(
    data = points_sf,
    aes(col = "red"),
    alpha = 0.5,
    size = 5) +
  labs(caption = paste0("Map data © OpenStreetMap contributors, ODbL. ",
                        "http://www.openstreetmap.org/copyright")) +
  theme_void() +
  theme(plot.caption = element_text(color = "grey20",
                                    face = "italic"),
        legend.position = "none")

# Print the map
minimalist_map

# Save the map as a png
ggsave("minimalist_map_liverpool.png", plot=minimalist_map, width = 297, height = 420, units = "mm", dpi = "retina")

