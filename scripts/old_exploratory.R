#### data_cleaning.R old version ####

#' ------------------------------------
#' Data Cleaning For Squidpop and BRIV/BRUV Analysis
#' ------------------------------------

#### Libraries for Cleaning

library(readxl)
library(dplyr)
library(stringr)
library(tidyr)
library(ggplot2)

#### Download Data

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

#### Data Cleaning for Rainsford

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

#### Data Cleaning for Lovells 
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

#### Put the video data for the islands together in the same df 

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

#### For Q1: squidpop/algal cover data together for islands 

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

#### From .qmd final project ... might need this?
# # get rid of row with missing values for predation
# video_no_12 <- video |> 
#   filter(!row_number() %in% 12)
# 
# # make a column with the predation species
# video_no_12$bait_consumer <- c("CAMA", NA, NA, "CAMA", "CAMA", NA, NA, "CAMA", NA, "Asian_Crab", "CAMA", NA, NA, NA, "CAMA", NA, "CAMA", "CAMA", "CAMA")
# 
# # get rid of algal cover and substrate type columns Bait:Unknown
# prob_pred_df <- video_no_12 |> 
#   select(-c(Bait, Total_sec_observed, Sec_to_Approach:Unknown)) |> 
#   mutate(probability = Pop_Eaten/(Pop_Eaten+Pop_Not_Eaten),
#          weight = (Pop_Eaten + Pop_Not_Eaten))

#### the following was not used for the .csv files - can change later 
# # make the algal cover names the same for squidpops and video - reduce 
# # squidpops data to look like video
# squid_algae_red <- squidpops |> 
#   mutate(Kelp = SLJ+SL,
#             MAST = MAST,
#             CHCR = CHCR,
#             CO = CO,
#             MYED = MYED,
#             Large_Folios_Algae = ASNO + FUSP + FUVE + FUDI + FU,
#             Other_Algae = ULIN + PORS + ULLA,
#          method = "pop") |> 
#   select(-c(ASNO:FUDI, FU:PORS, SLJ, SL, ULLA)) |> 
#   # reorder the columns and get rid of extras
#   select(Island:Site, method, Tide_Height, Latitude:Pop_Not_Eaten, 
#          Kelp:Other_Algae, MAST:MYED, boulder:sand)
# 
# # reduce video data to join with squid_algae_red
# video_algae_red <- video |> 
#   select(Island, Site, method, Tide_Height, Latitude, Longitude, Pop_Eaten, 
#          Pop_Not_Eaten, sand:Unknown)
# 
# # combine algal cover and substrate type for video and squidpops together
# algal_substrate <- full_join(squid_algae_red, video_algae_red) |> 
#   group_by(Island, Site)
# 
# # make most values NA for columns ____ except for those the data tells otherwise
# algal_substrate <- algal_substrate %>% 
#   mutate_at(c(9:26), ~replace_na(.,0))
# 
# algal_substrate[c(53, 56, 59, 63, 67), c(9:26)] = NA
# 
# #### For Q2: df for algal/sub cover, predation probability, time to pred 
# 
# ## Predation probability df
# # make the average algal cover/substrate type for each site for squidpop df
# squid_average <- squidpops |> 
#   mutate(Kelp = SLJ+SL,
#          MAST = MAST,
#          CHCR = CHCR,
#          CO = CO,
#          MYED = MYED,
#          Large_Folios_Algae = ASNO + FUSP + FUVE + FUDI + FU,
#          Other_Algae = ULIN + PORS + ULLA,
#          method = "pop") |> 
#   select(-c(ASNO:FUDI, FU:PORS, SLJ, SL, ULLA)) |> 
#   # reorder into something that makes sense
#   select(c(Island:Time, method, Latitude:Relief, Kelp:Other_Algae, MAST:sand)) |> 
#   group_by(Island, Site, method, Pop_Eaten, Pop_Not_Eaten, Tide_Height) |> 
#   summarize(Kelp = mean(Kelp),
#          Large_Folios_Algae = mean(Large_Folios_Algae),
#          Other_Algae = mean(Other_Algae),
#          MAST = mean(MAST),
#          CHCR = mean(CHCR),
#          CO = mean(CO),
#          MYED = mean(MYED),
#          boulder = mean(boulder),
#          bedrock = mean(bedrock),
#          cobble = mean(cobble),
#          pebble = mean(pebble),
#          shell_debris = mean(shell_debris),
#          sand = mean(sand))
# 
# # select out columns i want from the video df
# prob_pred_algal_sub_video <- video |> 
#   select(Island, Site, method, Pop_Eaten, Pop_Not_Eaten, Tide_Height, sand:Unknown)
# 
# # join them together for a probability of predation df
# prob_pred <- full_join(squid_average, prob_pred_algal_sub_video)
# 
# # actually, wait...
# # i need to combine algal and substrate pop data with BRIV data somehow...
# # may not be able to due to how data collection was done differently
# 
# ## Time to Pred df

##### PredationByIslandAndZone.R old version ####

#' ------------------------------------------------
#' Cleaning data to make a .csv for probability 
#' of predation by island and tidal zone
#' 
#' From this .csv, I'm creaitng a figure, 
#' building a model, and running statistical tests
#' 
#' ------------------------------------------------

library(ggplot2) # create visualizations
library(dplyr) # data cleaning
library(stringr) # data cleaning for strings
library(tidyr) # pivot data
library(performance) # checking model assumptions
library(broom) # also for checking model assumptions
library(DHARMa) # for checking model assumptions
library(car) # ANOVA
library(formattable) # for a pretty statistics table
library(RColorBrewer)

#### Read in the .csv files for video to make the figure

# ### read in video data
# video <- read.csv("data/video.csv")
# 
# ## clean the data frame so I can make a new .csv to work with
# 
# # remove row with no predation data
# video_predation_prob <- video |>
#   filter(!row_number() %in% 12)
# 
# # change strings in df so that BRUV = subtidal and BRIV = intertidal
# video_predation_prob <- video_predation_prob %>%
#   mutate(across('method', # column name
#                 str_replace,
#                 'BRUV', # old value
#                 'subtidal')) # new value
# 
# video_predation_prob <- video_predation_prob %>%
#   mutate(across('method', # column name
#                 str_replace,
#                 'BRIV', # old value
#                 'intertidal')) # new value
# 
# # change method column name to Tidal_Zone
# video_predation_prob <- video_predation_prob |>
#   rename(Tidal_Zone # new name
#          = method) # old name
# 
# # select out unneeded columns and create new columns for data essential to the model
# video_predation_prob <- video_predation_prob |>
#   select(Island, Tidal_Zone, Site, Pop_Eaten, Pop_Not_Eaten, Sec_to_predation, Sec_to_Approach)|>
#   mutate(probability = Pop_Eaten/(Pop_Eaten+Pop_Not_Eaten),
#          weight = (Pop_Eaten + Pop_Not_Eaten))
# 
# # write a .csv file to have just in case
# write.csv(video_predation_prob,
#           "data/Prob_Pred_Island_Zone_Only.csv",
#           row.names = FALSE)

#### Make model using the .csv file

# read in the .csv
Prob_Pred_Island_Zone_Only <- read.csv("data/Prob_Pred_Island_Zone_Only.csv")

# build the model
Prob_Pred_Island_Zone_Model <- glm(probability ~ Tidal_Zone*Island,
                                   data = Prob_Pred_Island_Zone_Only,
                                   family = binomial(link = "logit"),
                                   weight = weight)

# statistical test
chisq_IslandZone <- Anova(Prob_Pred_Island_Zone_Model)|> tidy()

chisq_IslandZone$statistic <- format(round(chisq_IslandZone$statistic, 2), nsmall = 2)
chisq_IslandZone$p.value <- format(round(chisq_IslandZone$p.value, 2), nsmall = 3)

formattable(chisq_IslandZone |> 
              rename(`Predictor` = term,
                     `X^2 Value` = statistic,
                     `Degrees of Freedom` = df,
                     `P Value` = p.value),
            align = c("l", rep("r")))

# make the figure in black and white
ggplot(data = Prob_Pred_Island_Zone_Only, 
       mapping = aes(x = Island, 
                     y = probability,
                     shape = Tidal_Zone)) +
  geom_point(size = 3,
             alpha = 0.5,
             position = position_dodge(.3)) +
  stat_summary(fun.data = "mean_se",
               size = 1,
               position = position_dodge(.3)) +
  xlab("Island") +
  ylab("Probability of Predation") +
  labs(shape = "Tidal Zone") +
  theme_bw()

# make the figure in color
ggplot(data = Prob_Pred_Island_Zone_Only, 
       mapping = aes(x = Island, 
                     y = probability,
                     shape = Tidal_Zone,
                     color = Tidal_Zone)) +
  geom_point(size = 3,
             alpha = 0.5,
             position = position_dodge(.3)) +
  scale_color_manual(values = c("#7570b3", "#1b9e77")) +
  stat_summary(fun.data = "mean_se",
               size = 1,
               position = position_dodge(.3)) +
  xlab("Island") +
  ylab("Probability of Predation") +
  labs(shape = "Tidal Zone",
       color = "Tidal Zone") +
  theme_bw()

# make the figure facetted
ggplot(data = Prob_Pred_Island_Zone_Only, 
       mapping = aes(x = Tidal_Zone, 
                     y = probability,
                     shape = Tidal_Zone)) +
  geom_point(size = 3,
             alpha = 0.5,
             position = position_dodge(.3)) +
  stat_summary(fun.data = "mean_se",
               size = 1,
               position = position_dodge(.3)) +
  facet_wrap(vars(Island)) +
  xlab("Tidal Zone") +
  ylab("Probability of Predation") +
  labs(shape = "Tidal Zone") +
  theme_bw()

# doing another one without Island
Prob_Pred_Zone_Model <- glm(probability ~ Tidal_Zone,
                            data = Prob_Pred_Island_Zone_Only,
                            family = binomial(link = "logit"),
                            weight = weight)

# statistical test
chisq_Zone <- Anova(Prob_Pred_Zone_Model)|> tidy()

chisq_Zone$statistic <- format(round(chisq_Zone$statistic, 2), nsmall = 2)
chisq_Zone$p.value <- format(round(chisq_Zone$p.value, 2), nsmall = 3)

formattable(chisq_Zone |> 
              rename(`Predictor` = term,
                     `X^2 Value` = statistic,
                     `Degrees of Freedom` = df,
                     `P Value` = p.value),
            align = c("l", rep("r")))

# make the figure
ggplot(data = Prob_Pred_Island_Zone_Only, 
       mapping = aes(x = Island, 
                     y = probability,
                     shape = Tidal_Zone)) +
  geom_point(size = 3,
             alpha = 0.5,
             position = position_dodge(.3)) +
  stat_summary(fun.data = "mean_se",
               size = 1,
               position = position_dodge(.3)) +
  xlab("Tidal Zone") +
  ylab("Probability of Predation") +
  labs(shape = "Tidal Zone") +
  theme_bw()

# remove the lovells value with the lowest substrate and its adjacent intertidal site. see how it compares to our result.

predation_no_lovellsD <- Prob_Pred_Island_Zone_Only |> 
  filter(Island != "Lovells" | Site != "D")

ggplot(data = predation_no_lovellsD, 
       mapping = aes(x = Island, 
                     y = probability,
                     shape = Tidal_Zone)) +
  geom_point(size = 3,
             alpha = 0.5,
             position = position_dodge(.3)) +
  stat_summary(fun.data = "mean_se",
               size = 1,
               position = position_dodge(.3)) +
  xlab("Island") +
  ylab("Probability of Predation") +
  labs(shape = "Tidal Zone") +
  theme_bw()

# huh things are even more different

#### Time_to_Predation.R old version ####
#' ----------------------------------
#' Make a figure showing how time to
#' predation varies between the 
#' intertidal and subtidal.
#' 
#' Do another visualization for
#' seconds to approach
#' 
#' Do a statistical analysis as well
#' if it looks like it will be work it
#' ----------------------------------

library(ggplot2) # create visualizations
library(dplyr) # data cleaning
library(stringr) # data cleaning for strings
library(tidyr) # pivot data
library(performance) # checking model assumptions
library(broom) # also for checking model assumptions
library(DHARMa) # for checking model assumptions
library(car) # ANOVA
library(formattable) # for a pretty statistics table
library(patchwork)
library(tidyverse) # for drop_na

time_to_pred <- read.csv("data/Prob_Pred_Island_Zone_Only.csv")

time_to_pred_min <- time_to_pred |> 
  mutate(min_to_predation = Sec_to_predation / 60,
         min_to_approach = Sec_to_Approach / 60)

# shows time to predation in seconds
predation <- time_to_pred |> 
  ggplot(aes(x = Tidal_Zone,
             y = Sec_to_predation)) + 
  geom_point(alpha = 0.5) +
  stat_summary(fun.data = "mean_se",
               size = .8,
               position = position_dodge(.3)) +
  facet_wrap(vars(Island)) +
  #labs(title = "Time to Predation by Tidal Zone") +
  ylab("Seconds to Predation") +
  xlab("Tidal Zone")+
  theme_bw()

# shows time to predation in minutes
predation_min <- time_to_pred_min |> 
  ggplot(aes(x = Tidal_Zone,
             y = min_to_predation)) + 
  geom_point(alpha = 0.5) +
  stat_summary(fun.data = "mean_se",
               size = .8,
               position = position_dodge(.3)) +
  facet_wrap(vars(Island)) +
  #labs(title = "Time to Predation by Tidal Zone") +
  ylab("Minutes to Predation") +
  xlab("Tidal Zone")+
  theme_bw()+
  ylim(0,45)

# shows time to approach in seconds WITHOUT taking out the points that did not result in predation events
approach <- time_to_pred |> 
  ggplot(aes(x = Tidal_Zone,
             y = Sec_to_Approach)) + 
  geom_point(alpha = 0.5) +
  stat_summary(fun.data = "mean_se",
               size = .8,
               position = position_dodge(.3)) +
  facet_wrap(vars(Island)) +
  #labs(title = "Seconds to Approach by Tidal Zone") +
  ylab("Seconds to Approach") +
  xlab("Tidal Zone")+
  theme_bw()

# shows time to approach in minutes WITHOUT taking out the points that did not result in predation events
approach_min <- time_to_pred_min |> 
  ggplot(aes(x = Tidal_Zone,
             y = min_to_approach)) + 
  geom_point(alpha = 0.5) +
  stat_summary(fun.data = "mean_se",
               size = .8,
               position = position_dodge(.3)) +
  facet_wrap(vars(Island)) +
  #labs(title = "Time to Predation by Tidal Zone") +
  ylab("Minutes to Approach") +
  xlab("Tidal Zone")+
  theme_bw()+
  ylim(0,45)

# probability of predation faceted by island
probability <- ggplot(data = time_to_pred, 
                      mapping = aes(x = Tidal_Zone, 
                                    y = probability,
                                    color = Tidal_Zone,
                                    shape = Tidal_Zone)) +
  facet_wrap(vars(Island)) +
  geom_point(alpha = 0.5,
             position = position_dodge(.3)) +
  scale_color_manual(values = c("#7570b3", "#1b9e77")) +
  stat_summary(fun.data = "mean_se",
               size = .5,
               position = position_dodge(.3)) +
  xlab("Tidal Zone") +
  ylab("Probability of Predation") +
  labs(shape = "Tidal Zone",
       color = "Tidal Zone",
       subtitle = "A.") +
  theme_bw()

# compares predation probability and time to approach and predation WITHOUT taking out the approacch points without predation events
predation_na <- (probability/approach_min/predation_min)

# now make it without the points for predation in the minutes to approach
# want to get rid of values where predation never occurs at all

pred_points_only <- time_to_pred_min |> 
  filter(!is.na(min_to_predation))

# shows time to predation with only the predation points as a way to check to make sure i filtered correctly
pred_points_only_min_figure <- pred_points_only |> 
  ggplot(aes(x = Tidal_Zone,
             y = min_to_predation,
             color = Tidal_Zone,
             shape = Tidal_Zone)) + 
  geom_point(alpha = 0.5) +
  scale_color_manual(values = c("#7570b3", "#1b9e77")) +
  stat_summary(fun.data = "mean_se",
               size = .5,
               position = position_dodge(.3)) +
  facet_wrap(vars(Island)) +
  labs(subtitle = "C.",
       shape = "Tidal Zone",
       color = "Tidal Zone") +
  ylab("Minutes to Predation") +
  xlab("Tidal Zone")+
  theme_bw()+
  ylim(0,45)

# time to approach in minutes with only the points that result in predation events
pred_points_only_approach_figure <- pred_points_only |> 
  ggplot(aes(x = Tidal_Zone,
             y = min_to_approach,
             color = Tidal_Zone,
             shape = Tidal_Zone)) + 
  geom_point(alpha = 0.5) +
  scale_color_manual(values = c("#7570b3", "#1b9e77")) +
  stat_summary(fun.data = "mean_se",
               size = .5,
               position = position_dodge(.3)) +
  facet_wrap(vars(Island)) +
  labs(subtitle = "B.",
       shape = "Tidal Zone",
       color = "Tidal Zone") +
  ylab("Minutes to Approach") +
  xlab("Tidal Zone")+
  theme_bw()+
  ylim(0,45)

# ok, make a patchwork showing with and without predation points - is the same story being told? If approach isn't useful/isn't adding anything, just take it out

# approach and predation with only predation event points
predation_no_na <- (probability/pred_points_only_approach_figure/pred_points_only_min_figure)

# comparing to make sure everything works
predation_na | predation_no_na

# well, it looks like time to approach doesn't tell us anything special - time to approach is consistent when looking at points where only predation actually happens.
# it looks like time to predation is slower in the intertidal of Lovells even though probability of predation is high

# ok, let's just use the original predation and minutes to predation graphs and color code them. Make them all faceted then crop them for the PPT as I so choose

probability_color <- ggplot(data = time_to_pred, 
                            mapping = aes(x = Tidal_Zone, 
                                          y = probability,
                                          color = Tidal_Zone,
                                          shape = Tidal_Zone)) +
  facet_wrap(vars(Island)) +
  geom_point(alpha = 0.5,
             position = position_dodge(.3)) +
  scale_color_manual(values = c("#7570b3", "#1b9e77")) +
  stat_summary(fun.data = "mean_se",
               size = .5,
               position = position_dodge(.3)) +
  xlab("Tidal Zone") +
  ylab("Probability of Predation") +
  labs(shape = "Tidal Zone",
       color = "Tidal Zone",
       subtitle = "A.") +
  theme_bw()

predation_color <- time_to_pred_min |> 
  ggplot(aes(x = Tidal_Zone,
             y = min_to_predation,
             color = Tidal_Zone,
             shape = Tidal_Zone)) + 
  geom_point(alpha = 0.5) +
  scale_color_manual(values = c("#7570b3", "#1b9e77")) +
  stat_summary(fun.data = "mean_se",
               size = .8,
               position = position_dodge(.3)) +
  facet_wrap(vars(Island)) +
  labs(color = "Tidal Zone",
       shape = "Tidal Zone") +
  ylab("Minutes to Predation") +
  xlab("Tidal Zone")+
  theme_bw()

(probability_color / predation_color) + 
  plot_layout(guides = 'collect')

# making individual plots just in case I need them for the presentation

# rainsford predaiton probability
rainsford_prob_pred <- time_to_pred |> 
  filter(Island == "Rainsford") |> 
  ggplot(mapping = aes(x = Tidal_Zone, 
                       y = probability,
                       color = Tidal_Zone,
                       shape = Tidal_Zone)) +
  geom_point(alpha = 0.5,
             position = position_dodge(.3)) +
  scale_color_manual(values = c("#7570b3", "#1b9e77")) +
  stat_summary(fun.data = "mean_se",
               size = .8,
               position = position_dodge(.3)) +
  xlab("Tidal Zone") +
  ylab("Probability of Predation") +
  labs(shape = "Tidal Zone",
       color = "Tidal Zone",
       title = "Rainsford Island") +
  theme_bw()

# Lovells probability of predation
lovells_prob_pred <- time_to_pred |> 
  filter(Island == "Lovells") |> 
  ggplot(mapping = aes(x = Tidal_Zone, 
                       y = probability,
                       color = Tidal_Zone,
                       shape = Tidal_Zone)) +
  geom_point(alpha = 0.5,
             position = position_dodge(.3)) +
  scale_color_manual(values = c("#7570b3", "#1b9e77")) +
  stat_summary(fun.data = "mean_se",
               size = .8,
               position = position_dodge(.3)) +
  xlab("Tidal Zone") +
  ylab("Probability of Predation") +
  labs(shape = "Tidal Zone",
       color = "Tidal Zone",
       title = "Lovells Island") +
  theme_bw()

# rainsford time to predation
rainsford_time_pred <- time_to_pred_min |> 
  filter(Island == "Rainsford") |> 
  ggplot(aes(x = Tidal_Zone,
             y = min_to_predation,
             color = Tidal_Zone,
             shape = Tidal_Zone)) + 
  geom_point(alpha = 0.5) +
  scale_color_manual(values = c("#7570b3", "#1b9e77")) +
  stat_summary(fun.data = "mean_se",
               size = .8,
               position = position_dodge(.3)) +
  labs(color = "Tidal Zone",
       shape = "Tidal Zone") +
  ylab("Minutes to Predation") +
  xlab("Tidal Zone")+
  theme_bw()


# lovells time to predation
lovells_time_pred <- time_to_pred_min |> 
  filter(Island == "Lovells") |> 
  ggplot(aes(x = Tidal_Zone,
             y = min_to_predation,
             color = Tidal_Zone,
             shape = Tidal_Zone)) + 
  geom_point(alpha = 0.5) +
  scale_color_manual(values = c("#7570b3", "#1b9e77")) +
  stat_summary(fun.data = "mean_se",
               size = .8,
               position = position_dodge(.3)) +
  labs(color = "Tidal Zone",
       shape = "Tidal Zone") +
  ylab("Minutes to Predation") +
  xlab("Tidal Zone")+
  theme_bw()

# patch together rainsford
(rainsford_prob_pred / rainsford_time_pred) + 
  plot_layout(guides = 'collect')

# patch together lovells
(lovells_prob_pred / lovells_time_pred) + 
  plot_layout(guides = 'collect')

# the visual for the paper - points with only predation events
predation_no_na

# make a color version

(probability_color/pred_points_only_approach_figure/pred_points_only_min_figure) + 
  plot_layout(guides = 'collect') & theme(legend.position = "bottom")

#### Abudance_of_Species.R old ####
#' -----------------------------------------
#' Make a data frame where it has abundance
#' of species. It should be number of 
#' individuals per minute for each species
#' 
#' Also do a proprotion bar plot of species
#' to accomapny because why not?
#' -----------------------------------------

# load libraries
library(ggplot2) # create visualizations
library(dplyr) # data cleaning
library(stringr) # data cleaning for strings
library(tidyr) # pivot data
library(viridis)

# read in data frame
video_consumer <- read.csv("data/video_consumer.csv")

# reorder the columns, select out unneccesary organisms that are not going to be eating the squid
video_consumer <- video_consumer |> 
  select(c(Island:Site, Pop_Eaten:Tide_Height, bait_consumer, Total_sec_observed:Lobster, sand:boulder))

# remove rows with NA for species
video_consumer <- video_consumer[-c(3, 6, 9, 13, 17),]

# make a new column for total consumers
video_consumer <- video_consumer |> 
  rowwise() |> 
  mutate(Total_Consumers = sum(PALO:Lobster))

# make a column for minutes observed
video_consumer <- video_consumer |> 
  mutate(Total_min_observed = Total_sec_observed/60) 

# pivot longer to calculate number of individuals of a species per minute - a form of abundance
abundance_per_min <- video_consumer |> 
  pivot_longer(cols = PALO:Lobster,
               names_to = "consumer",
               values_to = "consumer_count")

# create a column for consumer count per minute
abundance_per_min <- abundance_per_min |> 
  mutate(number_per_min = consumer_count/Total_min_observed)

# change Tide_Height values and also change the column name
abundance_per_min <- abundance_per_min |>
  rename(Tidal_Zone # new name
         = Tide_Height) # old name

# change values in Tidal Zone to intertidal and subtidal
abundance_per_min <- abundance_per_min %>%
  mutate(across('Tidal_Zone', # column name
                str_replace,
                'mid', # old value
                'intertidal')) # new value

abundance_per_min <- abundance_per_min %>%
  mutate(across('Tidal_Zone', # column name
                str_replace,
                'sub', # old value
                'subtidal')) # new value

# get rid of irrelevant species
abundance_per_min <-subset(abundance_per_min, consumer != "PALO" & consumer != "Stripped_bass" & consumer != "Flounder" & consumer != "Skate" & consumer != "Rock_gunnel" & consumer != "Lobster")

# change species names
abundance_per_min <- abundance_per_min |> 
  mutate(across('consumer', # column name
                str_replace,
                'Asian_Crab', # old value
                'Hemigrapsus sanguineus')) # new value

abundance_per_min <- abundance_per_min |> 
  mutate(across('consumer', # column name
                str_replace,
                'CAMA', # old value
                'Carcinus maenas')) # new value

abundance_per_min <- abundance_per_min |> 
  mutate(across('consumer', # column name
                str_replace,
                'Cancer_sp', # old value
                'Cancer spp.')) # new value

abundance_per_min <- abundance_per_min |> 
  mutate(across('consumer', # column name
                str_replace,
                'Cunner', # old value
                'Tautogolabrus adspersus')) # new value

abundance_per_min <- abundance_per_min |> 
  mutate(across('consumer', # column name
                str_replace,
                'LIER', # old value
                'Libinia emarginata')) # new value

abundance_per_min <- abundance_per_min |> 
  mutate(across('consumer', # column name
                str_replace,
                'Unknown_crab', # old value
                'Unknown crab')) # new value

abundance_per_min <- abundance_per_min |> 
  mutate(across('consumer', # column name
                str_replace,
                'Unknown_fish', # old value
                'Unknown fish')) # new value

# create a visualization for abundance
abundance_per_min |> 
  ggplot(aes(fill = consumer, 
             y = number_per_min, 
             x = Site)) + 
  geom_bar(position="stack", stat="identity") +
  scale_fill_viridis(discrete = T) +
  facet_wrap(vars(Island, Tidal_Zone)) +
  theme_minimal() +
  labs(title = "Number of Individuals per Minute by Species")

# make a scatter plot for intertidal and subtidal by island

addline_format <- function(y,...){
  gsub('\\s','\n',y)
}

abundance_per_min |> 
  ggplot(aes(x = number_per_min, 
             y = consumer, 
             shape = Tidal_Zone,
             color = Tidal_Zone)) + 
  geom_point(alpha = 0.5,
             position = position_dodge(.8)) +
  scale_color_manual(values = c("#7570b3", "#1b9e77")) +
  stat_summary(fun.data = "mean_se",
               size = .5,
               position = position_dodge(.8)) +
  facet_wrap(vars(Island)) +
  theme_bw() +
  theme(legend.position = "bottom",
        axis.text.y = element_text(face = "italic"))+
  labs(shape = "Tidal Zone",
       color = "Tidal Zone") +
  scale_y_discrete(labels=addline_format(abundance_per_min$consumer)) +
  ylab("Predator Species") +
  xlab("Number of Individuals per Minute")

#### New visualization for benthics

# get rid of unknown crab and fish

abundance_per_min <- abundance_per_min |> 
  filter(!(consumer == "Unknown crab" | consumer == "Unknown fish"))


# make visualization
abundance_per_min |> 
  ggplot(aes(x = Island,
             y = number_per_min,
             shape = Tidal_Zone,
             color = Tidal_Zone)) +
  geom_point(alpha = 0.5,
             position = position_dodge(.8)) +
  stat_summary(fun.data = "mean_se",
               size = .5,
               position = position_dodge(.8)) +
  facet_wrap(vars(consumer)) +
  theme_bw() +
  scale_color_manual(values = c("#7570b3", "#1b9e77")) +
  labs(shape = "Tidal Zone",
       color = "Tidal Zone")+
  ylab("Number of Individuals per Minute") + 
  theme(strip.text = element_text(face = "italic"),
        legend.position = c(0.83, 0.23)) # c(0,0) bottom left, c(1,1) top-right)

# make a longer version for my poster
abundance_per_min |> 
  ggplot(aes(x = Island,
             y = number_per_min,
             shape = Tidal_Zone,
             color = Tidal_Zone)) +
  geom_point(alpha = 0.5,
             position = position_dodge(.8)) +
  stat_summary(fun.data = "mean_se",
               size = .5,
               position = position_dodge(.8)) +
  facet_wrap(vars(consumer),
             ncol = 2) +
  theme_bw() +
  scale_color_manual(values = c("#7570b3", "#1b9e77")) +
  labs(shape = "Tidal Zone",
       color = "Tidal Zone")+
  ylab("Number of Individuals per Minute") + 
  theme(strip.text = element_text(face = "italic"),
        legend.position = c(0.75, 0.15)) # c(0,0) bottom left, c(1,1) top-right)


# need to calculate dominant substrate for this dataframe. come back to later.

#### consumer_figures.R old ####

## Ultimately did not end up using this script

#' -------------------------------
#' Predator Figures:
#' Who ate what?
#' Who was first predator?
#' Stacked bar plots of predators
#' -------------------------------

# libraries
library(ggplot2) # create visualizations
library(dplyr) # data cleaning
library(stringr) # data cleaning for strings
library(tidyr) # pivot data
library(patchwork)
library(viridis)

# download .csv for analysis
video_consumer <- read.csv("data/video_consumer.csv")

# select desired columns
video_consumer <- video_consumer |> 
  select(c(Island:Time, Depth_ft, Pop_Eaten:Sec_to_Approach, bait_consumer, Sec_to_predation:boulder))

# adjust Tide_Height column
video_consumer <- video_consumer %>%
  mutate(across('Tide_Height', # column name
                str_replace,
                'sub', # old value
                'subtidal')) # new value

video_consumer <- video_consumer %>%
  mutate(across('Tide_Height', # column name
                str_replace,
                'mid', # old value
                'intertidal')) # new value

# change method column name to Tidal_Zone
video_consumer <- video_consumer |>
  rename(Tidal_Zone # new name
         = Tide_Height) # old name

# make columns for probability of predation 
video_consumer <- video_consumer |> 
  mutate(probability = Pop_Eaten/(Pop_Eaten+Pop_Not_Eaten),
         weight = (Pop_Eaten + Pop_Not_Eaten))

# reorder columns to make them make more sense
video_consumer <- video_consumer |> 
  select(c(Island:Pop_Not_Eaten, probability, weight, Tidal_Zone:boulder))

# make a figure showing ultimate predator
video_consumer |> 
  ggplot(aes(x = Site,
             y = probability,
             shape = Tidal_Zone,
             label = bait_consumer)) +
  facet_wrap(vars(Island, Tidal_Zone)) +
  geom_point(position = position_dodge(.3),
             size = 2.5) +
  geom_text_repel(show.legend = FALSE,
                  size = 3) +
  ylab("Probability of Predation") +
  labs(shape = "Tidal Zone") +
  theme_bw()

video_consumer |> 
  ggplot(aes(x = Site,
             y = probability,
             shape = bait_consumer,
             label = bait_consumer)) +
  facet_wrap(vars(Island, Tidal_Zone)) +
  geom_point(position = position_dodge(.3),
             size = 2.5) +
  geom_text_repel(show.legend = FALSE,
                  size = 3) +
  ylab("Probability of Predation") +
  labs(shape = "Consumer") +
  theme_bw()

# make a figure showing first predator

video_consumer |> 
  ggplot(aes(x = Site,
             y = probability,
             shape = Tidal_Zone,
             label = First_Predator)) +
  facet_wrap(vars(Island, Tidal_Zone)) +
  geom_point(position = position_dodge(.3),
             size = 2.5) +
  geom_text_repel(show.legend = FALSE,
                  size = 3) +
  ylab("Probability of Predation") +
  labs(shape = "Tidal_Zone") +
  theme_bw()

video_consumer |> 
  ggplot(aes(x = Site,
             y = probability,
             shape = First_Predator,
             label = First_Predator)) +
  facet_wrap(vars(Island, Tidal_Zone)) +
  geom_point(position = position_dodge(.3),
             size = 2.5) +
  geom_text_repel(show.legend = FALSE,
                  size = 3) +
  ylab("Probability of Predation") +
  labs(shape = "First Predator") +
  theme_bw()

# pivot longer so predator is one column and count is the next column
consumer_long <- video_consumer |> 
  pivot_longer(cols = PALO:Lobster,
               names_to = "Consumer",
               values_to = "Consumer_Count")

# make a stacked bar plot showing proportion of predators
consumer_long |> 
  ggplot(aes(fill = Consumer, 
             y = Consumer_Count, 
             x = Site)) + 
  geom_bar(position="stack", stat="identity") +
  scale_fill_viridis(discrete = T) +
  facet_wrap(vars(Island, Tidal_Zone)) +
  theme_minimal()

# create a table showing length of video to put next to stacked bar plot showing proportion of predators
video_lengths <- video_consumer |> 
  select(c(Island:Site, Tidal_Zone, ))

# make 

#### Fraction_Predation_Events_Barplot.R old script ####
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

# now make the plot
predation <- fract_pred_events |> 
  drop_na(bait_consumer) |> 
  ggplot(aes(fill = bait_consumer, 
             x = Tidal_Zone)) + 
  geom_bar(position="stack") +
  facet_wrap(vars(Island)) +
  theme_bw() +
  labs(title = "Species Predation Events",
       fill = "Consumer") +
  xlab("Tidal Zone") +
  scale_fill_viridis(labels=c('Hemigrapsus sanguineus', 
                              'Carcinus maenas', 
                              "Camera Fail"),
                     discrete = TRUE)

# # let's try putting a pattern in the bar plot
# fract_pred_events |> 
#   drop_na(bait_consumer) |> 
#   ggplot(aes(x = Tidal_Zone)) + 
#   geom_bar_pattern(aes(pattern = bait_consumer),
#                    position="stack",
#                    color = "black",
#                    fill = "white") +
#   facet_wrap(vars(Island)) +
#   scale_pattern_manual(values = c("none", "stripe", "circle")) +
#   theme_bw() +
#   theme(legend.key.size = unit(1, 'cm')) +
#   labs(title = "Species Predation Events")


# now let's do it for camera fail...will need to double check I did this right later?

fract_pred_events[3,15] = "Camera_Fail"
fract_pred_events[6,15] = "Camera_Fail"
fract_pred_events[13,15] = "Camera_Fail"
fract_pred_events[17,15] = "Camera_Fail"

# bar plot in color
approach <- fract_pred_events |> 
  drop_na(First_Predator) |> 
  ggplot(aes(fill = First_Predator, 
             x = Tidal_Zone)) + 
  geom_bar(position="stack") +
  facet_wrap(vars(Island)) +
  theme_bw() +
  labs(title = "Species First Approach",
       fill = "Consumer") +
  xlab("Tidal Zone") +
  scale_fill_viridis(labels=c('Hemigrapsus sanguineus',
                              'Carcinus maenas',
                              "Camera Fail",
                              "Libinia emarginata",
                              "Unknown Crab"),
                     discrete = TRUE)

# let's try putting a pattern in the bar plot
# fract_pred_events |> 
#   drop_na(First_Predator) |> 
#   ggplot(aes(x = Tidal_Zone)) + 
#   geom_bar_pattern(aes(pattern = First_Predator),
#                    position="stack",
#                    color = "black",
#                    fill = "white") +
#   facet_wrap(vars(Island)) +
#   scale_pattern_manual(values = c("none", "stripe", "circle", "crosshatch", "wave")) +
#   theme_bw() +
#   theme(legend.key.size = unit(1, 'cm')) +
#   labs(title = "First Approach Species")

approach / predation

#### New visualiztion for Benthics Talk

# let's do this only for green and asian crabs
# figure where you have numer of events, x axis with green crab and hemi, one bar for each insubtidal and intertidal
# coordinate flip and put crab next to sets of bars
# also faceted by island?

## Approach

# first remove irrelevant consumers
green_asian_approach <- fract_pred_events |> 
  filter(First_Predator == "CAMA" | First_Predator == "Asian_Crab") |> 
  mutate(across("First_Predator",
                str_replace,
                "CAMA",
                "Carcinus maenas")) |> 
  mutate(across("First_Predator",
                str_replace,
                "Asian_Crab",
                "Hemigrapsus sanguineus"))

# now use this to make the new visual
g_a_approach <- green_asian_approach |> 
  ggplot(mapping = aes(y = First_Predator,
                       fill = Tidal_Zone)) +
  geom_bar(position = "dodge") + 
  facet_wrap(vars(Island)) +
  theme_bw() +
  labs(title = "Species First Approach",
       fill = "Tidal Zone") +
  ylab("Consumer Species") +
  scale_fill_manual(values = c("#7570b3", "#1b9e77")) +
  scale_y_discrete(labels = wrap_format(10)) +
  theme(axis.title.y = element_blank()) # for patchwork

## Predation

# first remove irrelevant consumers
green_asian_predation <- fract_pred_events |> 
  filter(bait_consumer == "CAMA" | bait_consumer == "Asian_Crab") |> 
  mutate(across("bait_consumer",
                str_replace,
                "CAMA",
                "Carcinus maenas")) |> 
  mutate(across("bait_consumer",
                str_replace,
                "Asian_Crab",
                "Hemigrapsus sanguineus"))

# now use this to make the new visual
g_a_pred <- green_asian_predation |> 
  ggplot(mapping = aes(y = bait_consumer,
                       fill = Tidal_Zone)) +
  geom_bar(position = "dodge") + 
  facet_wrap(vars(Island)) +
  theme_bw() +
  labs(title = "Species Predation Events",
       fill = "Tidal Zone") +
  ylab("Consumer Species") +
  scale_fill_manual(values = c("#7570b3", "#1b9e77")) +
  scale_y_discrete(labels = wrap_format(10))+
  theme(axis.title.y = element_blank()) # for patchwork

# put them together in a patchwork
grid.arrange(patchworkGrob(g_a_approach + 
                             g_a_pred + 
                             plot_layout(guides = 'collect') & theme(legend.position = "bottom")), left = "Consumer Species")

# ok the actual one for the presentation
green_asian_predation |> 
  ggplot(mapping = aes(y = bait_consumer,
                       fill = Tidal_Zone)) +
  geom_bar(position = position_dodge2(preserve = "single")) + 
  facet_wrap(vars(Island)) +
  theme_bw() +
  labs(fill = "Tidal Zone") +
  ylab("Consumer Species") +
  scale_fill_manual(values = c("#7570b3", "#1b9e77")) +
  scale_y_discrete(labels = wrap_format(10))+
  theme(legend.position = "bottom",
        axis.text.y=element_text(face="italic"))

# for poster - make count on the y and consumer on the x
green_asian_predation |> 
  ggplot(mapping = aes(x = Island,
                       fill = Tidal_Zone)) +
  geom_bar(position = position_dodge2(preserve = "single")) + 
  facet_wrap(vars(bait_consumer)) +
  theme_bw() +
  labs(fill = "Tidal Zone") +
  xlab("Consumer Species") +
  scale_fill_manual(values = c("#7570b3", "#1b9e77")) +
  scale_x_discrete(labels = wrap_format(10))+
  theme(legend.position = "bottom",
        strip.text = element_text(face = "italic"))

#### Int_vs_Sub_Prob_Pred.R old script ####
# didn't end up using

#' --------------------------------------------------------
#' Cleaning data to make a .csv for comparing 
#' probability of predation for the intertidal 
#' versus the subtidal
#' 
#' From this .csv, I'm creaitng a figure, building a model,
#' and running statistical tests
#' --------------------------------------------------------

# load libraries
library(ggplot2) # create visualizations
library(dplyr) # data cleaning
library(stringr) # data cleaning for strings
library(tidyr) # pivot data
library(performance) # checking model assumptions
library(broom) # also for checking model assumptions
library(DHARMa) # for checking model assumptions
library(car) # ANOVA
library(formattable) # for a pretty statistics table
library(ggrepel) # for having labels


# read in the .csv
int_vs_sub <- read.csv("data/Prob_Pred_Island_Zone_Only.csv")

# let's reshape the data for the visualization
int_vs_sub <- int_vs_sub |> 
  select(-c(Pop_Eaten, Pop_Not_Eaten, weight)) |> 
  pivot_wider(names_from = Tidal_Zone,
              values_from = probability)

# code for the figure
ggplot(int_vs_sub,
       aes(x = subtidal,
           y = intertidal,
           label = Site)) + 
  geom_point() +
  geom_label_repel(fontface = "bold") +
  facet_wrap(vars(Island)) +
  theme_bw(base_size = 14) +
  ylab("Intertidal Predation Probability") +
  xlab("Subtidal Predation Probability")

#### substrate_model.R old script ####
# didn't end up using this

#' ------------------------------
#' Updated model for thesis
#' ------------------------------

# libraries used
library(ggplot2)
library(dplyr) # data cleaning
library(tidyr) # pivot data
library(performance) # checking model assumptions
library(broom) # also for checking model assumptions
library(DHARMa) # for checking model assumptions
library(car) # ANOVA
library(emmeans)
library(formattable)
library(stringr)
library(AICcmodavg)

# # read in the .csv files
# video <- read.csv("data/video.csv") # date formats wrong - need to fix; so is lat/long for rainsford BRIV
# squidpops <- read.csv("data/squidpops.csv")
# 
# # # not sure if I need these, but here is the code in case I do later
# # lovells_video <- read.csv("data/lovells_video.csv")
# # rainsford_video <- read.csv("data/rainsford_video.csv")
# # lovells_squidpops <- read.csv("data/lovells_squidpops.csv")
# # rainsford_squidpops <- read.csv("data/rainsford_squidpops.csv")
# 
# #### Data frame used for final project 
# # get rid of row with missing values for predation
# video_no_12 <- video |> 
#   filter(!row_number() %in% 12)
# 
# # make a column with the predation species
# video_no_12$bait_consumer <- c("CAMA", NA, NA, "CAMA", "CAMA", NA, NA, "CAMA", NA, "Asian_Crab", "CAMA", NA, NA, NA, "CAMA", NA, "CAMA", "CAMA", "CAMA")
# 
# # # get rid of algal cover and substrate type columns Bait:Unknown
# # prob_pred_df <- video_no_12 |>
# #   select(-c(Bait, Total_sec_observed, Sec_to_Approach:Unknown)) |>
# #   mutate(probability = Pop_Eaten/(Pop_Eaten+Pop_Not_Eaten),
# #          weight = (Pop_Eaten + Pop_Not_Eaten))
# 
# #### Make data frame for new model
# 
# # calculate prob of predation and weight for the model
# new_model_df <- video_no_12 |> 
#   select(Island, Tide_Height, Site, Pop_Eaten, Pop_Not_Eaten) |> 
#   mutate(probability = Pop_Eaten/(Pop_Eaten+Pop_Not_Eaten),
#          weight = (Pop_Eaten + Pop_Not_Eaten))
# 
# # look at the substrate types for the squidpops data frame.
# # make a new data frame with only island, site, quad, tide_height, and substrate. Order the rows by max count for each site.
# squid_substrate <- squidpops |> 
#   select(Island, Site, Tide_Height, Substrate_Type)
# 
# # look at the substrate percent cover for the video_no_12 df
# # select out columns for island, site, tide_height, and the various substrates available
# 
# video_substrate <- video_no_12 |> 
#   select(Island, Site, method, Tide_Height, sand:boulder)
# 
# # filter out rows with NAs
# video_substrate <- video_substrate |> 
#   na.omit()
# 
# # pivot longer to make substrate type one column and percent cover another column
# video_substrate <- video_substrate |> 
#   pivot_longer(cols = sand:boulder,
#                names_to = "Substrate_Type",
#                values_to = "Percent_Cover")
# 
# # group by all the columns except for substrate type and percent cover and filter for max values
# video_substrate <- video_substrate |> 
#   group_by(Island, Site, method, Tide_Height) |> 
#   filter(Percent_Cover == max(Percent_Cover)) |> 
#   ungroup()
# 
# # get rid of method column, add a count column where the value is 1 for all, remove percent cover column
# video_substrate <- video_substrate |> 
#   select(-c(method, Percent_Cover)) 
# 
# # join the two data frames together
# dominant_substrate <- full_join(squid_substrate, video_substrate)
# 
# # count all the substrate types by island, site, tide height, and substrae type
# dominant_substrate <- dominant_substrate |>
#   group_by(Island, Site, Tide_Height, Substrate_Type) |>
#   add_count(Substrate_Type) |>
#   unique() |>
#   group_by(Island, Site, Tide_Height, Substrate_Type) |>
#   arrange(desc(n),
#           .by_group = TRUE) |> 
#   rename(Count = n)
# 
# # filter for maximum count
# dominant_substrate <- dominant_substrate |> 
#   group_by(Island, Site, Tide_Height) |> 
#   filter(Count == max(Count))
# 
# # select out the count column
# dominant_substrate <- dominant_substrate |> 
#   select(-Count)
# 
# # join to new_model_df
# substrate_predation_df <- full_join(new_model_df, dominant_substrate)
# 
# # filter out rows with NA
# substrate_predation_df <- substrate_predation_df |> na.omit()
# 
# # get all values of substrate type and figure out how to group them. Explain this in my slides that I am presenting to Jarrett
# unique(substrate_predation_df$Substrate_Type)
# # bedrock + boulder + cobble = rock
# # shell_debris + pebble = coarse_sediment
# # sand = fine_sediment
# 
# # now rename these values in the Substrate_Type column
# substrate_predation_df <- substrate_predation_df %>%
#   mutate(across('Substrate_Type', # column name
#                 str_replace,
#                 'sand', # old value
#                 'fine_sediment')) # new value
# 
# substrate_predation_df <- substrate_predation_df %>%
#   mutate(across('Substrate_Type', # column name
#                 str_replace,
#                 'shell_debris|pebble', # old value
#                 'coarse_sediment')) # new value
# 
# substrate_predation_df <- substrate_predation_df %>%
#   mutate(across('Substrate_Type', # column name
#                 str_replace,
#                 'bedrock|boulder|cobble', # old value
#                 'rock')) # new value
# 
# video_predation_prob <- video_predation_prob %>%
#   mutate(across('method', # column name
#                 str_replace,
#                 'BRIV', # old value
#                 'intertidal')) # new value
# 
# # change method column name to Tidal_Zone
# substrate_predation_df <- substrate_predation_df |>
#   rename(Tidal_Zone # new name
#          = Tide_Height) # old name
# 
# # change values in Tidal Zone to intertidal and subtidal
# substrate_predation_df <- substrate_predation_df %>%
#   mutate(across('Tidal_Zone', # column name
#                 str_replace,
#                 'mid', # old value
#                 'intertidal')) # new value
# 
# substrate_predation_df <- substrate_predation_df %>%
#   mutate(across('Tidal_Zone', # column name
#                 str_replace,
#                 'sub', # old value
#                 'subtidal')) # new value
# 
# write.csv(substrate_predation_df, 
#           "data/substrate_prob_pred.csv", 
#           row.names = FALSE)

#### build the model and do the analysis 

# load in .csv file for a new df
substrate_prob_pred <- read.csv("data/substrate_prob_pred.csv")

# and now the model!
substrate_model <- glm(probability ~ Tidal_Zone*Substrate_Type + Island,
                       data = substrate_prob_pred,
                       family = binomial(link = "logit"),
                       weight = weight)

# check assumptions
res_bin <- simulateResiduals(substrate_model)
substrate_profile <- profile(substrate_model)

plotQQunif(res_bin) # looks ok

plot(substrate_profile) # looks a lot better

# statistical test
chisq_substrate <- Anova(substrate_model)|> tidy()

chisq_substrate$statistic <- format(round(chisq_substrate$statistic, 2), nsmall = 2)
chisq_substrate$p.value <- format(round(chisq_substrate$p.value, 2), nsmall = 3)

formattable(chisq_substrate |> 
              rename(`Predictor` = term,
                     `X^2 Value` = statistic,
                     `Degrees of Freedom` = df,
                     `P Value` = p.value),
            align = c("l", rep("r")))

Anova(substrate_model) # chi square ANOVA - emmeans of tide height by substrate type visualization 

summary(substrate_model) 

# make a post-hoc plot just to see

substrate_means <- emmeans(substrate_model, 
                           specs = ~Tidal_Zone | Substrate_Type | Island) 

substrate_cont <- contrast(substrate_means, method = "pairwise")

plot(substrate_cont) +
  geom_vline(xintercept = 0, lty = 2)

#### Substrate model version 2 - remove Island 

### removed island because adding island as an interaction over paramaterized the model

substrate_model_2 <- glm(probability ~ Tidal_Zone*Substrate_Type,
                         data = substrate_prob_pred,
                         family = binomial(link = "logit"),
                         weight = weight)

# check assumptions
res_bin <- simulateResiduals(substrate_model_2)
substrate_profile_2 <- profile(substrate_model_2)

plotQQunif(res_bin) # doesn't look like it fits the model

plot(substrate_profile_2) # a little iffy

# statistical test
chisq_substrate_2 <- Anova(substrate_model_2)|> tidy()

chisq_substrate_2$statistic <- format(round(chisq_substrate_2$statistic, 2), nsmall = 2)
chisq_substrate_2$p.value <- format(round(chisq_substrate_2$p.value, 2), nsmall = 3)

formattable(chisq_substrate_2 |> 
              rename(`Predictor` = term,
                     `X^2 Value` = statistic,
                     `Degrees of Freedom` = df,
                     `P Value` = p.value),
            align = c("l", rep("r")))

#### Substrate model version 3 - Move cobble to coarse sediment with Island 

# ## make a new .csv file
# 
# # read in the .csv files
# video <- read.csv("data/video.csv") # date formats wrong - need to fix; so is lat/long for rainsford BRIV
# squidpops <- read.csv("data/squidpops.csv")
# 
# #### Data frame used for final project 
# # get rid of row with missing values for predation
# video_no_12 <- video |>
#   filter(!row_number() %in% 12)
# 
# # make a column with the predation species
# video_no_12$bait_consumer <- c("CAMA", NA, NA, "CAMA", "CAMA", NA, NA, "CAMA", NA, "Asian_Crab", "CAMA", NA, NA, NA, "CAMA", NA, "CAMA", "CAMA", "CAMA")
# 
# # get rid of algal cover and substrate type columns Bait:Unknown
# prob_pred_df <- video_no_12 |>
#   select(-c(Bait, Total_sec_observed, Sec_to_Approach:Unknown)) |>
#   mutate(probability = Pop_Eaten/(Pop_Eaten+Pop_Not_Eaten),
#          weight = (Pop_Eaten + Pop_Not_Eaten))
# 
# #### Make data frame for new model 
# 
# # calculate prob of predation and weight for the model
# new_model_df <- video_no_12 |>
#   select(Island, Tide_Height, Site, Pop_Eaten, Pop_Not_Eaten) |>
#   mutate(probability = Pop_Eaten/(Pop_Eaten+Pop_Not_Eaten),
#          weight = (Pop_Eaten + Pop_Not_Eaten))
# 
# # look at the substrate types for the squidpops data frame.
# # make a new data frame with only island, site, quad, tide_height, and substrate. Order the rows by max count for each site.
# squid_substrate <- squidpops |>
#   select(Island, Site, Tide_Height, Substrate_Type)
# 
# # look at the substrate percent cover for the video_no_12 df
# # select out columns for island, site, tide_height, and the various substrates available
# 
# video_substrate <- video_no_12 |>
#   select(Island, Site, method, Tide_Height, sand:boulder)
# 
# # filter out rows with NAs
# video_substrate <- video_substrate |>
#   na.omit()
# 
# # pivot longer to make substrate type one column and percent cover another column
# video_substrate <- video_substrate |>
#   pivot_longer(cols = sand:boulder,
#                names_to = "Substrate_Type",
#                values_to = "Percent_Cover")
# 
# # group by all the columns except for substrate type and percent cover and filter for max values
# video_substrate <- video_substrate |>
#   group_by(Island, Site, method, Tide_Height) |>
#   filter(Percent_Cover == max(Percent_Cover)) |>
#   ungroup()
# 
# # get rid of method column, add a count column where the value is 1 for all, remove percent cover column
# video_substrate <- video_substrate |>
#   select(-c(method, Percent_Cover))
# 
# # join the two data frames together
# dominant_substrate <- full_join(squid_substrate, video_substrate)
# 
# # count all the substrate types by island, site, tide height, and substrae type
# dominant_substrate <- dominant_substrate |>
#   group_by(Island, Site, Tide_Height, Substrate_Type) |>
#   add_count(Substrate_Type) |>
#   unique() |>
#   group_by(Island, Site, Tide_Height, Substrate_Type) |>
#   arrange(desc(n),
#           .by_group = TRUE) |>
#   rename(Count = n)
# 
# # filter for maximum count
# dominant_substrate <- dominant_substrate |>
#   group_by(Island, Site, Tide_Height) |>
#   filter(Count == max(Count))
# 
# # select out the count column
# dominant_substrate <- dominant_substrate |>
#   select(-Count)
# 
# # join to new_model_df
# substrate_predation_df <- full_join(new_model_df, dominant_substrate)
# 
# # filter out rows with NA
# substrate_predation_df <- substrate_predation_df |> na.omit()
# 
# # get all values of substrate type and figure out how to group them. Explain this in my slides that I am presenting to Jarrett
# unique(substrate_predation_df$Substrate_Type)
# # bedrock + boulder = rock
# # shell_debris + pebble + cobble = coarse_sediment
# # sand = fine_sediment
# 
# # now rename these values in the Substrate_Type column
# substrate_predation_df <- substrate_predation_df %>%
#   mutate(across('Substrate_Type', # column name
#                 str_replace,
#                 'sand', # old value
#                 'fine_sediment')) # new value
# 
# substrate_predation_df <- substrate_predation_df %>%
#   mutate(across('Substrate_Type', # column name
#                 str_replace,
#                 'shell_debris|pebble|cobble', # old value
#                 'coarse_sediment')) # new value
# 
# substrate_predation_df <- substrate_predation_df %>%
#   mutate(across('Substrate_Type', # column name
#                 str_replace,
#                 'bedrock|boulder', # old value
#                 'rock')) # new value
# 
# # change method column name to Tidal_Zone
# substrate_predation_df <- substrate_predation_df |>
#   rename(Tidal_Zone # new name
#          = Tide_Height) # old name
# 
# # change values in Tidal Zone to intertidal and subtidal
# substrate_predation_df <- substrate_predation_df %>%
#   mutate(across('Tidal_Zone', # column name
#                 str_replace,
#                 'mid', # old value
#                 'intertidal')) # new value
# 
# substrate_predation_df <- substrate_predation_df %>%
#   mutate(across('Tidal_Zone', # column name
#                 str_replace,
#                 'sub', # old value
#                 'subtidal')) # new value
# 
# write.csv(substrate_predation_df,
#           "data/substrate_prob_pred_v2.csv",
#           row.names = FALSE)

substrate_prob_pred2 <- read.csv("data/substrate_prob_pred_v2.csv")

# and now the model!
substrate_model_3 <- glm(probability ~ Tidal_Zone*Substrate_Type + Island,
                         data = substrate_prob_pred2,
                         family = binomial(link = "logit"),
                         weight = weight)

# check assumptions
res_bin <- simulateResiduals(substrate_model_3)
substrate_profile_3 <- profile(substrate_model_3)

plotQQunif(res_bin) # looks ok

plot(substrate_profile_3) # looks a lot better

# statistical test
chisq_substrate_3 <- Anova(substrate_model_3)|> tidy()

chisq_substrate_3$statistic <- format(round(chisq_substrate_3$statistic, 2), nsmall = 2)
chisq_substrate_3$p.value <- format(round(chisq_substrate_3$p.value, 2), nsmall = 3)

formattable(chisq_substrate_3 |> 
              rename(`Predictor` = term,
                     `X^2 Value` = statistic,
                     `Degrees of Freedom` = df,
                     `P Value` = p.value),
            align = c("l", rep("r")))

# hm statistical values changed quite a bit based on moving around substrate


#### Substrate model version 4 - Move cobble to coarse sediment remove Island 
substrate_model_4 <- glm(probability ~ Tidal_Zone*Substrate_Type,
                         data = substrate_prob_pred2,
                         family = binomial(link = "logit"),
                         weight = weight)

# check assumptions
res_bin <- simulateResiduals(substrate_model_4)
substrate_profile_4 <- profile(substrate_model_4)

plotQQunif(res_bin) # looks ok

plot(substrate_profile_4) # looks a lot better

# statistical test
chisq_substrate_4 <- Anova(substrate_model_4)|> tidy()

chisq_substrate_4$statistic <- format(round(chisq_substrate_4$statistic, 2), nsmall = 2)
chisq_substrate_4$p.value <- format(round(chisq_substrate_4$p.value, 2), nsmall = 3)

formattable(chisq_substrate_4 |> 
              rename(`Predictor` = term,
                     `X^2 Value` = statistic,
                     `Degrees of Freedom` = df,
                     `P Value` = p.value),
            align = c("l", rep("r")))

#### Do an AIC comparison of the models 
# make a substrate intercept model
substrate_model_int <- glm(probability ~ 1,
                           data = substrate_prob_pred,
                           family = binomial(link = "logit"),
                           weight = weight)

model_list <- list(substrate_model_int,
                   substrate_model,
                   substrate_model_2,
                   substrate_model_3,
                   substrate_model_4)

model_names <- c("Intercept",
                 "Coarse Sediment = shell debris + pebble With Island",
                 "Coarse Sediment = shell debris + pebble No Island",
                 "Coarse Sediment = shell debris + pebble + cobble With Island",
                 "Coarse Sediment = shell debris + pebble + cobble No Island")

aicc_presentation <- aictab(cand.set = model_list,
                            modnames = model_names,
                            second.ord = TRUE)

aicc_presentation$AICc <- format(round(aicc_presentation$AICc, 2), nsmall = 2)
aicc_presentation$Delta_AICc <- format(round(aicc_presentation$Delta_AICc, 2), nsmall = 2)
aicc_presentation$AICcWt <- format(round(aicc_presentation$AICcWt, 2), nsmall = 2)

formattable(aicc_presentation %>% 
              select(Modnames, AICc, Delta_AICc, AICcWt) %>% 
              rename(`Model Name` = Modnames,
                     `Delta AICc` = Delta_AICc,
                     `AICc Weight` = AICcWt),
            align = c("l", rep("r")))

#### Intertidal Substrate 

# # bedrock + boulder + cobble = rock
# # shell_debris + pebble = coarse_sediment
# # sand = fine_sediment

substrate_intertidal <- substrate_prob_pred |> 
  filter(Tidal_Zone == "intertidal")

substrate_intertidal_mod1 <- glm(probability ~ Substrate_Type + Island,
                                 data = substrate_intertidal,
                                 family = binomial(link = "logit"),
                                 weight = weight)


res_bin <- simulateResiduals(substrate_intertidal_mod1)
substrate_intertidal_profile_1 <- profile(substrate_intertidal_mod1)

plotQQunif(res_bin) # looks ok

plot(substrate_intertidal_profile_1) # looks a lot better

Anova(substrate_intertidal_mod1)|> tidy() # "not significant"


#### new_map.R ####
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

# map without labels
leaflet(data = coordinates) %>% 
  addTiles(urlTemplate = 'https://basemap.nationalmap.gov/arcgis/rest/services/USGSImageryOnly/MapServer/tile/{z}/{y}/{x}') %>% 
  addLabelOnlyMarkers(~Longitude, 
                      ~Latitude)

