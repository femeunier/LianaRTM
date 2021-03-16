rm(list = ls())

library(rrtm)  # remotes::install_github("ashiklom/rrtm")

example <- edr_r(
  # Cohort state variables, in order from tallest to shortest
  pft = c(1, 1, 2),
  lai = c(2, 1, 0.8),
  wai = c(0.01, 0.01, 0.01),
  cai = c(1, 1, 1),
  # PFT-level parameters -- indexed by `pft` above
  N = c(1.4, 1.5),
  Cab = c(40, 30),
  Car = c(8, 8),
  Cw = c(0.01, 0.01),
  Cm = c(0.008, 0.01),
  orient_factor = c(0.5, 0.5),
  clumping_factor = c(0.84, 0.735),
  # Global parameters
  soil_moisture = 0.5,
  direct_sky_frac = 0.9,
  czen = 1
)

# Inspect the `example` object:
str(example)

# Result is a list containing the top-of-canopy albedo, and additional
# information about the light levels by cohort layer. `2101` is the number of
# wavelengths -- 400-2500nm at 1 nm resolution. There are 3 cohorts in this
# simulation, so there are 4 `up` and `down` fluxes -- one for each cohort
# layer, plus the ground (soil) layer.

# Below, we do a simple plot of the albedo.
plot(400:2500, example$albedo, type = "l",
     xlab = "Wavelength (nm)",
     ylab = "Albedo [0, 1]")
