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
  select(c(Island:Site, Pop_Eaten:Tide_Height, bait_consumer, Total_sec_observed:Lobster))

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
                'Libinia spp.')) # new value - also, upon further review I cannot make out the different species of spider crabs in video. Changing it to just genus for data visualization and analysis.

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

# make a scatter plot for intertidal and subtidal by island

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
  ylab("Predator Species") +
  xlab("Number of Individuals per Minute")

