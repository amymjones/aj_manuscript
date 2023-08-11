#' ----------------------------------
#' Make a figure showing how time to
#' predation varies between the 
#' intertidal and subtidal.
#' 
#' Do another visualization for
#' seconds to approach
#' 
#' Do a statistical analysis as well
#' if it looks like it will be worth it
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

# now make it without the points for predation in the minutes to approach
# want to get rid of values where predation never occurs at all

pred_points_only <- time_to_pred_min |> 
  filter(!is.na(min_to_predation))

# time to approach in minutes with only the points that result in predation events
approach <- pred_points_only |> 
  ggplot(aes(x = Island,
             y = min_to_approach,
             color = Tidal_Zone,
             shape = Tidal_Zone)) + 
  geom_point(size = 3,
             alpha = 0.5,
             position = position_dodge(.3)) +
  scale_color_manual(values = c("#7570b3", "#1b9e77")) +
  stat_summary(fun.data = "mean_se",
               size = .8,
               position = position_dodge(.3)) +
  labs(subtitle = "A.",
       shape = "Tidal Zone",
       color = "Tidal Zone") +
  ylab("Minutes to Approach") +
  xlab("Tidal Zone")+
  theme_bw()+
  ylim(0,45)

# shows time to predation in minutes
predation <- pred_points_only |> 
  ggplot(aes(x = Island,
           y = min_to_predation,
           color = Tidal_Zone,
           shape = Tidal_Zone)) + 
  geom_point(size = 3,
             alpha = 0.5,
             position = position_dodge(.3)) +
  scale_color_manual(values = c("#7570b3", "#1b9e77")) +
  stat_summary(fun.data = "mean_se",
               size = .8,
               position = position_dodge(.3)) +
  labs(subtitle = "B.",
       color = "Tidal Zone",
       shape = "Tidal Zone") +
  ylab("Minutes to Predation") +
  xlab("Tidal Zone")+
  theme_bw()

# create a patchwork of the graphs
(approach | predation) + plot_layout(guides = 'collect') & theme(legend.position = "bottom")


