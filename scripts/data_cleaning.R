#' ------------------------------------
#' Data Cleaning For Squidpop and BRIV/BRUV Analysis
#' ------------------------------------

#### Libraries for Cleaning ####

library(readxl)
library(dplyr)
library(stringr)
library(tidyr)
library(ggplot2)

#### Download Data ####

# I wrote a function for downloading the data from specified sheets in a excel doc

download_rainsford <- function(name){
  file <- read_excel("data/rainsford_squidpop_BRUV_BRIV_2022.xlsx", sheet = name, na = "NA")
}

rainsford_squidpops <- download_rainsford("squidpop_percent_cover")

rainsford_musselcover <- download_rainsford("mussel_cover")

rainsford_BRUV <- download_rainsford("BRUV")

rainsford_BRIV <- download_rainsford("BRIV")

download_lovells <- download_rainsford <- function(name){
  file <- read_excel("data/lovells_squidpop_BRUV_BRIV_2022.xlsx", sheet = name, na = "NA")
}

lovells_squidpops <- download_lovells("squidpop_percent_cover")

lovells_BRUV <- download_lovells("BRUV")

lovells_BRIV <- download_lovells("BRIV")

#### Data Cleaning for Rainsford ####

## Make sure all columns are in the format I want for rainsford
skimr::skim(rainsford_squidpops)
rainsford_squidpops$Date <- as.character(rainsford_squidpops$Date)

skimr::skim(rainsford_musselcover)
rainsford_musselcover$Date <- as.character(rainsford_musselcover$Date)
rainsford_musselcover <- rainsford_musselcover |> 
  select(-`note: gathered on a different day from squidpop deployment`)

skimr::skim(rainsford_BRUV)
rainsford_BRUV <- rainsford_BRUV |> 
  select(-Notes) 
rainsford_BRUV$Latitude <- as.numeric(rainsford_BRUV$Latitude)
rainsford_BRUV$Longitude <- as.numeric(rainsford_BRUV$Longitude)

skimr::skim(rainsford_BRIV)
rainsford_BRIV$Date <- as.character(rainsford_BRIV$Date)
rainsford_BRIV <- rainsford_BRIV |> 
  select(-Notes) 
rainsford_BRIV$Latitude <- as.numeric(rainsford_BRIV$Latitude)
rainsford_BRIV$Longitude <- as.numeric(rainsford_BRIV$Longitude)

## Reshape squidpop data
rainsford_squidpops <- left_join(x = rainsford_squidpops,
                                    y = rainsford_musselcover,
                                    by = c("Island", "Site", "Quadrat", "Latitude", "Longitude"))
# remove extra date column
rainsford_squidpops <- rainsford_squidpops |> 
  select(-Date.y)

rainsford_squidpops <- rainsford_squidpops |> 
  rename(Date = Date.x)

# make substrate type columns consistent
rainsford_squidpops$Substrate_Type <- rainsford_squidpops$Substrate_Type |> 
  str_replace_all(c("large_boulder" = "boulder",
                    "small_boulder"  = "boulder"))

# # write a .csv file for rainsford_squidpops
# write.csv(rainsford_squidpops,
#           file = "data/rainsford_squidpops.csv",
#           row.names = FALSE)

##BRUV/BRIV Data
# add a new column to BRUV/BRIV saying what the camera is
rainsford_BRIV <- rainsford_BRIV %>%
  mutate(method = "BRIV",
         .after = "Island") |> 
  mutate(Tide_Height = "mid")

rainsford_BRIV  <- rainsford_BRIV|> 
  rename(Flounder = Winter_Flounder)

rainsford_BRUV <- rainsford_BRUV %>%
  mutate(method = "BRUV",
         .after = "Island") |> 
  rename(Flounder = Winter_Flounder) |> 
  mutate(Tide_Height = "sub",
         .after = Pop_Not_Eaten)

# fix unknown_crab stuff, predation = eaten in BRIV data frame
rainsford_BRIV <- rainsford_BRIV |> 
  rename("Unknown_crab" = "Unknown_Crab",
         "Eaten" = "Predation")
rainsford_BRIV <- rainsford_BRIV %>%
  mutate(Eaten = replace(Eaten, Eaten == 1, "Y")) |> 
  mutate(Eaten = replace(Eaten, Eaten == 0, "N")) 

# create number of pops eaten/not eaten columns for each rainsford BRIV site
rain_briv_pop <- rainsford_squidpops |> 
  select(Island, Site, Pop_Eaten, Pop_Not_Eaten) |> 
  unique()

# join this to the BRIV data
rainsford_BRIV <- full_join(rainsford_BRIV, rain_briv_pop)

# create eaten column for BRUV
# this requires me to make a new DF that shows Y/N based on what I see in my current data as this wasn't recorded
rain_bruv_eaten <- tibble(Island = "Rainsford",
                          method = "BRUV",
                          Site = c("A", "B", "C", "D", "E"),
                          Eaten = c("Y", "N", NA, "Y", "Y"))

# join to rainsford BRUV
rainsford_BRUV <- full_join(rainsford_BRUV, rain_bruv_eaten)

# join camera data together
rainsford_video <- full_join(rainsford_BRUV, rainsford_BRIV)

# # write a new .csv file for rainsford video
# write.csv(rainsford_video,
#           file = "data/rainsford_video.csv",
#           row.names = FALSE)

#### Data Cleaning for Lovells ####
## Make sure all columns are in the format I want for lovells
skimr::skim(lovells_squidpops)
lovells_squidpops$Latitude <- as.numeric(lovells_squidpops$Latitude)
lovells_squidpops$Longitude <- as.numeric(lovells_squidpops$Longitude)

skimr::skim(lovells_BRUV)
lovells_BRUV <- lovells_BRUV |> 
  select(-Notes)

skimr::skim(lovells_BRIV)
lovells_BRIV$Date <- as.character(lovells_BRIV$Date)
lovells_BRIV <- lovells_BRIV |> 
  select(-Notes)
lovells_BRIV$Latitude <- as.numeric(lovells_BRIV$Latitude)
lovells_BRIV$Longitude <- as.numeric(lovells_BRIV$Longitude)
lovells_BRIV$Predation <- lovells_BRIV$Predation |> 
  str_replace_all("Y", "1")
lovells_BRIV$Predation <- as.numeric(lovells_BRIV$Predation)

## Reshape Squidpop data
# change lovells_squidpop position column to site
lovells_squidpops <- lovells_squidpops |>  rename(Site = Position)

# make substrate type columns consistent
lovells_squidpops$Substrate_Type <- lovells_squidpops$Substrate_Type |> 
  str_replace_all(c("large/med boulder" = "boulder",
                    "sm/med boulder"  = "boulder",
                    "med boulder" = "boulder",
                    "med boulder, shell" = "boulder",
                    "sm boulder" = "boulder",
                    ", shell" = "",
                    "pebbles" = "pebble")) 

# # write a .csv file
# write.csv(lovells_squidpops, file = "data/lovells_squidpops.csv",
#           row.names = FALSE)

## Reshape camera data
# change the date format on lovells_BRUV
lovells_BRIV$Date <- str_replace_all(lovells_BRIV$Date, 
                                     pattern = "/",
                                     replacement = "-")
# add a new column to BRUV/BRIV saying what the camera is
lovells_BRIV <- lovells_BRIV %>%
  mutate(method = "BRIV",
         .after = "Island")|> 
  mutate(Tide_Height = "mid")

lovells_BRUV <- lovells_BRUV %>%
  mutate(method = "BRUV",
         .after = "Island") |> 
  rename(Flounder = Winter_flounder)|> 
  mutate(Tide_Height = "sub")

lovells_BRIV <- lovells_BRIV |> 
  rename("Unknown_crab" = "Unknown_Crab",
         "Eaten" = "Predation")
lovells_BRIV <- lovells_BRIV %>%
  mutate(Eaten = replace(Eaten, Eaten == 1, "Y"))

# pop eaten/not eaten is pops for BRUV. Do the same for BRIV.
love_briv_pop <- lovells_squidpops |> 
  select(c(Island, Site, Pop_Eaten, Pop_Not_Eaten)) |> 
  unique()

lovells_BRIV <- full_join(lovells_BRIV, love_briv_pop)

# join bruv and briv together
lovells_video <- full_join(lovells_BRUV, lovells_BRIV)

# # write a .csv file
# write.csv(lovells_video, file = "data/lovells_video.csv",
#           row.names = FALSE)

#### Put the video data for the islands together in the same df ####

video <- full_join(rainsford_video, lovells_video)

# reorder the columns into something that makes sense

video <- video |> 
  select(Island:Longitude, Depth_ft, Eaten, Pop_Eaten:Tide_Height, Bait:CAMA, 
         Asian_Crab, LIER, Cancer_sp, Unknown_crab, Stripped_bass:Cunner, 
         Unknown_fish, Lobster, Cormorant, Grass_shrimp, Sand:`Other Algae`, 
         DIVE, DIVE_STCL, MEME, Unknown)

# change Other Algae to Other_Algae
video <- video |> 
  rename(Other_Algae = `Other Algae`)

# replace all NAs with 0 in columns 17-44
video <- video %>% 
  mutate_at(c(17:44), ~replace_na(.,0))

# now, change the values in columns 17-44 in rows 3, 6, 9 13, and 17 back to NA
# because these are supposed to be NA based on original data
video[c(3, 6, 9, 13, 17), c(17:44)] = NA

# time to predation needs to be NA when bait not eaten
video[c(2, 7, 9, 14, 15),17] = NA

#### For Q1: squidpop/algal cover data together for islands ####

squidpops <- full_join(rainsford_squidpops, lovells_squidpops)

# change percent cover values from NA to 0
squidpops <- squidpops %>% 
  mutate_at(c(28, 29), ~replace_na(.,0))

# # make substrate type for each quadrat a column name with either 100 or 0 for 
# # the percent cover
# squidpops <- squidpops |> 
#   mutate(Substrate_Cover = 100,
#          .after = Substrate_Type)
# 
# squidpops <- squidpops |> 
#   pivot_wider(names_from = Substrate_Type,
#               values_from = Substrate_Cover,
#               values_fill = 0)

# make the substrate type names for video the same as squidpops
video <- video |> 
  rename(sand = Sand,
         shell_debris = Shell_Debris,
         pebble = Pebbles,
         cobble = Cobble,
         boulder = Boulder)

# add a final consumer column
video$bait_consumer <- c("CAMA", NA, NA, "CAMA", "CAMA", NA, NA, "CAMA", NA, "Asian_Crab", "CAMA", "CAMA", NA, NA, NA, "CAMA", NA, "CAMA", "CAMA", "CAMA")

# # write a .csv file to be used for bait consumer analysis
# write.csv(video, "data/video_consumer.csv", row.names = FALSE)

# # write a .csv file for video data
# write.csv(video, file = "data/video.csv",
#           row.names = FALSE)

# # write a .csv file for squidpops
# write.csv(squidpops,
#           file = "data/squidpops.csv", 
#           row.names = FALSE)
