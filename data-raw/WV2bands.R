rm(list = ls())

library(purrr)
library(tidyr)
library(ggplot2)
library(dplyr)
library(stringr)
library(hsdar)

bands_WV2 <- get.sensor.characteristics("WorldView2-8", TRUE)

wavelengths  <- seq.int(attr(bands_WV2$response, "minwl"),
                        attr(bands_WV2$response, "maxwl"),
                        attr(bands_WV2$response, "stepsize"))

WV2.bands <- cbind(wavelengths,
                   bands_WV2[["response"]])

usethis::use_data(WV2.bands, overwrite = TRUE)
