#' ---------------------------------------
#' Make a stacked bar plot showing the 
#' fraction of predation events with the
#' fill being the consumer species
#' ---------------------------------------

library(dplyr)
library(stringr)
library(ggplot2)
library(tidyr)
library(viridis)
library(patchwork)
library(scales) # to wrap text
library(gridExtra) # to combine axis labels

fract_pred_events <- read.csv("data/video_consumer.csv")

# figure out which replicates are camera fails versus 
# which ones are no predation
  # Camera fails - put in bait_consumer column [,46]: 
    # Rainsford BRUV C [3,] 
    # Rainsford BRIV A [6,] 
    # Rainsford BRIV D [9,] 
    # Lovells BRUV C [13,] 
    # Lovells BRIV B [17,] 

# MAJOR PROBLEMS
  # Row 15 missing first predator - it's because predator never approached bait
  # LOVELLS BRIV B - WHY DO I HAVE STUFF FOR ALGAL COVER BUT NOT FOR PREDATORS? It's because these were photos
  # LOVELLS BRUV C - CAMERA FAIL
  # RAINSFORD BRIV A - FIX IT SUCH AT ALL VALUES ARE NA DUE TO CAMERA FAIL - done
  # RAINSFORD BRIV D - CAMERA FAIL CHECK DATA HERE TO MAKE SURE CORRECT - done
  # RAINSFORD BRUV C - CAMERA FAIL CHECK DATA HERE TO MAKE SURE CORRECT - done

  # I HAVE VALUES FOR UNKNOWN EVEN THOUGH IT LOOKS LIKE THE CAMERA FAILED?
  # OCCURS IN ROWS 6, 17 - row 6 fixed, for row 17...I don't know?
  # ROW 17 MIGHT HAVE AN ISSUE INDICATING I MESSED UP SUBSTRATE TYPE CALCULATIONS. NEED TO DOUBLE CHECK THIS

# replace these values in the data frame

fract_pred_events[3,46] = "Camera_Fail"
fract_pred_events[6,46] = "Camera_Fail"
# fract_pred_events[9,46] = "Camera_Fail" # wait was this not originally included because the bait was not eaten? Yes! This should be an NA!
fract_pred_events[13,46] = "Camera_Fail"
fract_pred_events[17,46] = "Camera_Fail"

fract_pred_events <- fract_pred_events |> 
  mutate(across('Tide_Height', # column name
                str_replace,
                'sub', # old value
                'subtidal')) # new value

fract_pred_events <- fract_pred_events |> 
  mutate(across('Tide_Height', # column name
                str_replace,
                'mid', # old value
                'intertidal')) # new value

# change method column name to Tidal_Zone
fract_pred_events <- fract_pred_events |>
  rename(Tidal_Zone # new name
         = Tide_Height) # old name

# now make the plot for consumer id predation events
pred_events <- fract_pred_events |> 
  drop_na(bait_consumer) |> 
  ggplot(aes(fill = bait_consumer, 
             x = Tidal_Zone)) + 
  geom_bar(position="stack") +
  facet_wrap(vars(Island)) +
  theme_bw() +
  theme(legend.text = element_text(face = "italic")) +
  labs(subtitle = "B.",
       fill = "Consumer") +
  xlab("Tidal Zone")+
  scale_fill_manual(values=c("#440154", "#3b528b", "#21918c"),
                    labels=c('Hemigrapsus sanguineus', 
                             'Carcinus maenas', 
                             "Camera Fail"))
  

# now let's do it for camera fail...will need to double check I did this right later?

fract_pred_events[3,15] = "Camera_Fail"
fract_pred_events[6,15] = "Camera_Fail"
fract_pred_events[13,15] = "Camera_Fail"
fract_pred_events[17,15] = "Camera_Fail"

# bar plot in color for first approach consumers
approach_events <- fract_pred_events |> 
  drop_na(First_Predator) |> 
  ggplot(aes(fill = First_Predator, 
             x = Tidal_Zone)) + 
  geom_bar(position="stack") +
  facet_wrap(vars(Island)) +
  theme_bw() +
  labs(subtitle = "A.",
       fill = "Consumer") +
  theme(legend.text = element_text(face = "italic")) +
  xlab("Tidal Zone") +
  scale_fill_viridis(labels=c('Hemigrapsus sanguineus',
                              'Carcinus maenas',
                              "Camera Fail",
                              "Libinia spp.", # can't actually id spider crab to species level
                              "Unknown Crab"),
                     discrete = TRUE)

approach_events / pred_events
