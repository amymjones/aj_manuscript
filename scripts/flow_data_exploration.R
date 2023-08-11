#' --------------------------------------
#' data and code sent from Jarrett 
#' on BHI water flow data
#' --------------------------------------

# extract - nearest cell for the flow issues

library(terra)
library(sf)
# library(raster)

current_velocity_crop_ag <- rast("data/current_velocity_crop_ag.grd")

plet((current_velocity_crop_ag+1) |> log10())

# a data frame that then gets reprojected into the same CRS as the raster
# BHI_points <- read.csv("data/BHI_lat_long.csv") |>
#   st_as_sf(crs = 4326, coords = c("Longitude", "Latitude"))

BHI_points <- read.csv("data/BHI_lat_long_labels.csv")

# to make the tidal zone case consistent with predation data frame
BHI_points$Tidal_Zone <- tolower(BHI_points$Tidal_Zone)

# removing lovells subtidal B due to missing data in predation data frame
BHI_points <- BHI_points[-2,]
  
BHI_points <- BHI_points |> 
  st_as_sf(crs = 4326, coords = c("Longitude", "Latitude"))

BHI_points_trans <- BHI_points |>
  st_transform(crs = st_crs(current_velocity_crop_ag))

#show that the transform worked and check the point
plot((current_velocity_crop_ag+1) |> log10())
plot(BHI_points_trans$geometry, add = TRUE, pch = 19)

# extract the value
current_vals <- terra::extract(current_velocity_crop_ag, 
                BHI_points_trans |> vect(),
                ID = FALSE)

BHI_points <- BHI_points |>
  dplyr::bind_cols(current_vals)

ext(BHI_points)

# extract current values using adjacent cells to deal with NA issues
# example from https://stackoverflow.com/questions/76330119/how-to-get-a-spatraster-of-indices-of-the-nearest-na-cell-in-r-terra

elv <- rast(system.file("ex/elev.tif", package="terra"))
riv <- vect(system.file("ex/lux.shp", package="terra"))[12] |> as.lines()
riv$val <- NA
elv <- rasterize(riv, elv, "val", update=TRUE)

# option 1 using focal
felv <- focal(elv, 3, mean, na.policy="only")

# option 2 using extract
e <- terra::extract(elv, riv, cell=TRUE) # 121 rows
a <- adjacent(x = elv, cells = e$cell) # 121 rows
a <- cbind(a, terra::extract(elv, a[,2])) |> na.omit()
b <- terra::aggregate(x = a[, "elevation", drop=F], 
                      by = a[,"from", drop=FALSE], 
                      FUN = mean)

# trying with my data
e1 <- terra::extract(x = current_velocity_crop_ag, # SpatRaster or SpatVector
                    y = BHI_points_trans |> vect(), # must be SpatVector of points/lines/polygons
                    cell = TRUE) # keep the cell number
a1 <- adjacent(x = current_velocity_crop_ag, #SpatRaster
               cells = e1$cell) # vector of cell numbers to get adjacent cells
a2 <- cbind(a1, terra::extract(x = current_velocity_crop_ag, 
                               y = a1[,2])) # |> na.omit()
current_vals2 <- aggregate(x = a2[, "band1", drop=F], # SpatVector
                           by = a2[,"from", drop=FALSE], # character; variable(s) used to group the geometries
                           FUN = mean)

# trying to zoom in on the areas of interest
library(leaflet)

BHI_points_labels <- read.csv("data/BHI_lat_long_labels.csv")

BHI_points_labels <- BHI_points_labels[-2,] # removing row with missing data

library(viridis)

plet((current_velocity_crop_ag+1) |> log10(),
     tiles=c("Esri.WorldImagery"),
     col = viridis(n = 256, direction = -1)) |> 
  addCircleMarkers(lng = BHI_points_labels$Longitude,
             lat = BHI_points_labels$Latitude,
             color = "black",
             radius = 2,
             opacity = 1,
             fillOpacity = 1) |> 
  setView(lng = -70.939552,
          lat = 42.323578,
          zoom = 14)


#### add the extracted current vector values to the model
source("scripts/PredationByIslandAndZone.R")

## join the extracted values with my current data from to add it to a new model

prob_pred_flow <- full_join(Prob_Pred_Island_Zone_Only, BHI_points,
                            by = c("Island", "Tidal_Zone", "Site"))

# Prob_Pred_Island_Zone_Model <- glm(probability ~ Tidal_Zone*Island,
#                                    data = Prob_Pred_Island_Zone_Only,
#                                    family = binomial(link = "logit"),
#                                    weight = weight)

prob_pred_flow_glm <- glm(probability ~ Tidal_Zone*Island*band1,
                          data = prob_pred_flow,
                          family = binomial(link = "logit"),
                          weight = weight)

# check the model
check_model(prob_pred_flow_glm)

# binned_residuals(prob_pred_flow, check = "binned_residuals") |> plot()
# 
# res_bin <- simulateResiduals(prob_pred_flow)
# 
# plotQQunif(res_bin)
# 
# plotResiduals(res_bin)

# look at the model
summary(prob_pred_flow_glm)

## how to interpret the model:


# statistical test
Anova(prob_pred_flow_glm)

# let's visualize it
library(ggplot2)

# version 1
qplot(x = probability, 
      y = band1, 
      color=Tidal_Zone, 
      data = prob_pred_flow) +
  stat_smooth(method="glm") +
  facet_wrap(vars(Island)) +
  ylab("Peak Flow") +
  xlab("Probability of Predation") +
  guides(color=guide_legend(title="Tidal Zone")) +
  theme_bw()

# version 2 - make flow the predictor
qplot(x = band1, 
      y = probability, 
      color= paste(Tidal_Zone, Island),
      data = prob_pred_flow) +
  stat_smooth(method="glm") +
  xlab("Peak Flow") +
  ylab("Probability of Predation") +
  guides(color=guide_legend(title="Tidal Zone")) +
  theme_bw()

#### plotting parallel lines using data_grid and predict() to create a prediction data frame that then gets overlain on the data points

### sourced from this tutorial: https://biol607.github.io/lab/many_predictor_types.html#mixing-categorical-and-continuous-variables 

# Not sure this is what I am looking for? from 3.3
pred_flow_aug <- augment(x = prob_pred_flow_glm, interval="confidence")

head(pred_flow_aug)

ggplot(data = pred_flow_aug,
       aes(x = band1, color= paste(Tidal_Zone, Island))) +
  geom_point(mapping=aes(y=probability)) +
  geom_line(mapping = aes(y=.fitted)) + 
  geom_ribbon(aes(ymin=.lower,
                  ymax=.upper,
                  group = paste(Tidal_Zone, Island)), 
              fill="lightgrey", 
              alpha=0.5,
              color = NA) +
  theme_bw()

# trying this next? 1.4 from same tutorial
library(modelr)
keeley <- read.csv("~/Desktop/UMass_Boston/BIOL 607 BioStats/many_predictors/data/10/Keeley_rawdata_select4.csv")

keeley_mlr <- lm(rich ~ firesev + cover, data=keeley)

# code from the tutorial
k_firesev_explore <- data_grid(keeley,
                               cover = seq_range(cover, 100), # this creates 100 entries for cover
                               firesev = seq_range(firesev, 4)) |> # this creates 4 entries for fire sev for each cover entry. total of 400 lines
  augment(keeley_mlr, newdata = _, interval = "confidence") |> # this creates confidence interivals for each entry
  rename(rich = .fitted)

# # attempt1 with my own data from tutotiral - didn't work
# pred_flow_explore <- data_grid(prob_pred_flow,
#                                band1 = seq_range(band1, 100),
#                                Tidal_Zone,
#                                Island) |>
#   augment(prob_pred_flow_glm, 
#           newdata = _, 
#           interval = "prediction") 


# from the internet on glms and predict:
# https://stackoverflow.com/questions/42216496/using-modelradd-predictions-for-glm
library(tidyverse)
library(modelr)

d <- as_tibble(ISLR::Default)
model <- glm(default ~ balance, data = d, family = binomial)

grid <- d %>% data_grid(balance) %>% # generate an evenly spaced grid of points from the data to visualise the model, 
  mutate(pred = predict(model, newdata = ., type = 'response')) # this predicts the values from the model

# visualize the model
ggplot(data = d, 
       aes(x = balance)) + 
  geom_point(aes(y = as.numeric(default) - 1)) + 
  geom_line(data = grid, aes(y = pred)) + 
  scale_y_continuous('default', breaks = 0:1, labels = levels(d$default))

ggplot(d, aes(x = balance)) + 
  geom_point(aes(y = as.numeric(default) - 1)) + 
  geom_line(data = grid, aes(y = pred)) + 
  scale_y_continuous('default', breaks = 0:1, labels = levels(d$default))

# use geom_smooth to calculate confidence intervals
ggplot(d, aes(balance, as.numeric(default) - 1)) + 
  geom_point() + 
  geom_smooth(method = 'glm', method.args = list(family = 'binomial')) + 
  scale_y_continuous('default', breaks = 0:1, labels = levels(d$default))

# attempt 2 with my data
pred_flow_explore2 <- prob_pred_flow %>% 
  data_grid(Island, Tidal_Zone, band1) %>% 
  mutate(probability = predict(prob_pred_flow_glm, newdata = ., type = 'response'))

ggplot(prob_pred_flow, 
       aes(x = band1, 
           y = probability,
           color = paste(Tidal_Zone, Island))) + 
  geom_point() + 
  geom_line(data = pred_flow_explore2, aes(y = probability)) + 
 # geom_smooth(method = 'glm', method.args = list(family = 'binomial')) +
  scale_y_continuous('probability', breaks = 0:1, labels = levels(prob_pred_flow$probability))

# attempt 2 with my data transformed on the y axis
library(car)

pred_flow_explore2 <- prob_pred_flow %>% 
  data_grid(Island, Tidal_Zone, band1) %>% 
  mutate(probability = predict(prob_pred_flow_glm, newdata = ., type = 'response'))

ggplot(prob_pred_flow, 
       aes(x = band1, 
           y = logit(probability),
           color = paste(Tidal_Zone, Island))) + 
  geom_point() + 
  geom_line(data = pred_flow_explore2, aes(y = logit(probability))) + 
  # geom_smooth(method = 'glm', method.args = list(family = 'binomial')) +
  scale_y_continuous('probability', breaks = 0:1, labels = levels(prob_pred_flow$probability))

# attempt 3 with type = "link" w/out points

pred_flow_explore3 <- prob_pred_flow %>% 
  data_grid(Island, Tidal_Zone, band1) %>% 
  mutate(probability = predict(prob_pred_flow_glm, newdata = ., type = 'link'))

ggplot(prob_pred_flow, 
       aes(x = band1, 
           y = logit(probability),
           color = paste(Tidal_Zone, Island))) + 
  geom_line(data = pred_flow_explore2, aes(y = logit(probability))) + 
  scale_y_continuous('probability', breaks = 0:1, labels = levels(prob_pred_flow$probability))

# attempt 4 with type = "link" w/ points logit transformed

pred_flow_explore3 <- prob_pred_flow %>% 
  data_grid(Island, Tidal_Zone, band1) %>% 
  mutate(probability = predict(prob_pred_flow_glm, newdata = ., type = 'link'))

ggplot(prob_pred_flow, 
       aes(x = band1, 
           y = logit(probability),
           color = paste(Tidal_Zone, Island))) + 
  geom_point() +
  geom_line(data = pred_flow_explore2, aes(y = logit(probability))) + 
  scale_y_continuous('probability', breaks = 0:1, labels = levels(prob_pred_flow$probability))