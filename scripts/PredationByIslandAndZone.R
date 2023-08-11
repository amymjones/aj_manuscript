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

#### Read in the .csv files for video to make the figure ####

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

#### Make model using the .csv file ####

# read in the .csv
Prob_Pred_Island_Zone_Only <- read.csv("data/Prob_Pred_Island_Zone_Only.csv")

# build the model
Prob_Pred_Island_Zone_Model <- glm(probability ~ Tidal_Zone*Island,
                       data = Prob_Pred_Island_Zone_Only,
                       family = binomial(link = "logit"),
                       weight = weight)

# check the model
check_model(Prob_Pred_Island_Zone_Model)

binned_residuals(Prob_Pred_Island_Zone_Model, check = "binned_residuals") |> plot()

res_bin <- simulateResiduals(Prob_Pred_Island_Zone_Model)

plotQQunif(res_bin)

plotResiduals(res_bin)

# look at the model
summary(Prob_Pred_Island_Zone_Model)


# statistical test
chisq_IslandZone <- Anova(Prob_Pred_Island_Zone_Model)|> tidy()

chisq_IslandZone$statistic <- format(round(chisq_IslandZone$statistic, 2), nsmall = 2)
chisq_IslandZone$p.value <- format(round(chisq_IslandZone$p.value, 2), nsmall = 3)

# look at statistical results
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