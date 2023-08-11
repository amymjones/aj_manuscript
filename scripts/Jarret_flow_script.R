library(terra)
library(sf)
current_velocity_crop_ag <- rast("data/current_velocity_crop_ag.grd")

plet((current_velocity_crop_ag+1) |> log10())

# a fake data frame that then gets reprojected into the same CRS as the raster
dat <- data.frame(id = 1, lat = 42.31680378001301, long = -70.95229604591574) |>
  st_as_sf(crs = 4326, coords = c("long", "lat")) 

dat_trans <- dat |>
  st_transform(crs = st_crs(current_velocity_crop_ag))

#show that the transform worked and check the point
plot((current_velocity_crop_ag+1) |> log10())
plot(dat_trans$geometry, add = TRUE, pch = 19)

# extract the value
vals <- extract(current_velocity_crop_ag, dat_trans |> vect(),
                ID = FALSE)

dat <- dat |>
  dplyr::bind_cols(vals)

dat
