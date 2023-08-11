#' ----------------------------
#' Fixing map for thesis paper
#' ----------------------------

# load libraries
library(dplyr)
library(leaflet)

# download squidpop sheets
lovells_squidpops <- read.csv("data/lovells_squidpops.csv")
rainsford_squidpops <- read.csv("data/rainsford_squidpops.csv")

# join the squidpop data frames together
squidpop_map <- full_join(lovells_squidpops, rainsford_squidpops)

# download the video data sheets
rainsford_video <- read.csv("data/rainsford_video.csv")
lovells_video <- read.csv("data/lovells_video.csv")

# join the video data frames together
video_map <- full_join(lovells_video, rainsford_video)

# filter to only have rows for BRUV
video_map <- video_map |> filter(method == "BRUV")

# create a new column for Tide_Height for video_map and make all values "sub"
video_map <- video_map |> mutate(Tide_Height = "sub")

# join the squidpop and video tables together
coordinates <- full_join(squidpop_map, video_map)

# create tibble for map
coordinates <- coordinates %>% 
  select(Island, Tide_Height, Site, Latitude, Longitude) %>% 
  distinct() # creates a data frame without duplicate variables

# map with labels
leaflet(data = coordinates) %>% 
  addTiles(urlTemplate = 'https://basemap.nationalmap.gov/arcgis/rest/services/USGSImageryOnly/MapServer/tile/{z}/{y}/{x}') %>% 
  addLabelOnlyMarkers(~Longitude, 
                      ~Latitude,
                      label = ~Site,
                      labelOptions = labelOptions(noHide = T,
                                                  direction = "left",
                                                  textOnly = F,
                                                  textsize = "15px",
                                                  style = list(
                                                    "color" = "black"),
                                                  fontface = "bold"
                      ))
