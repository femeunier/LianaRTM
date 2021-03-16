rm(list = ls())

library(rrtm)  # remotes::install_github("ashiklom/rrtm")

# Leaf level
out <- prospect5(1.4, 40, 10, 0, 0.01, 0.01)
plot(c(400, 2500), c(0, 1), type = "n",
     xlab = "Wavelength (nm)",
     ylab = "Spectra")
lines(400:2500, out$reflectance, col = "red")
lines(400:2500, 1 - out$transmittance, col = "blue")
legend("top", legend = c("reflectance", "1 - transmittance"),
       lty = 1, col = c("red", "blue"))


# Canopy
sail_out <- pro4sail_5(N = 1.4, Cab = 40, Car = 10, Cbrown = 0,
                       Cw = 0.01, Cm = 0.01,
                       LAI = 3, soil_moisture = 0.5)

matplot(400:2500, do.call(cbind, sail_out),
        xlab = "Wavelength (nm)", ylab = "Reflectance (0-1)",
        type = "l", lty = "solid", col = 1:4)
legend("topright", c("BHR", "DHR", "HDR", "BDR"),
       lty = "solid", col = 1:4)
