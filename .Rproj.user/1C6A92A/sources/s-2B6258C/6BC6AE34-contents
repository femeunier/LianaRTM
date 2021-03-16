rm(list = ls())

library(ncdf4)
library(rrtm)

# Small function for better regular expressions
regex <- function(string, pattern) {
  m <- regexec(pattern, string, perl = TRUE)
  regmatches(string, m)
}

# Some sample outputs are available from here (~15M):
# https://github.com/ashiklom/rrtm/releases/download/edr-example/output.tar.gz

# List output files, and extract the years (for plotting later). Modify the
# `outdir` as appropriate.
outdir <- "./output-bg-small"
outfiles <- list.files(outdir, "\\.h5$", full.names = TRUE)
ym <- regex(outfiles, "(?<=-E-)([[:digit:]]{4})-([[:digit:]]{2})")
years <- as.numeric(sapply(ym, "[[", 2))

# Set PFT-level RTM parameters. NPFT here is because the PFT parameters are
# indexed by integer, and the largest PFT number in these simulations is 11
# (Temperate late-successional hardwood).
npft <- 11
N <- rep(1.4, npft)
N[c(6, 8)] <- 2.0
Cab <- rep(40, npft)
Car <- rep(10, npft)
Cw <- rep(0.01, npft)
Cm <- rep(0.01, npft)
Cm[c(6, 8)] <- 0.02
orient_factor <- rep(0.5, npft)
clumping_factor <- rep(0.84, npft)
clumping_factor[c(6, 8)] <- 0.735

# A few global parameters
soil_brightness <- 0.5   # Hapke model parameter
czen <- 0.85             # Approximate value for July at UMBS
direct_sky_frac <- 0.9   # Assume relatively clear sky

# Empty lists for storing results.
pft_list <- list()
lai_list <- list()
wai_list <- list()
results <- list()

pb <- txtProgressBar(style = 3)
for (i in seq_along(outfiles)) {
  f <- outfiles[i]
  nc <- nc_open(f)
  pft <- ncvar_get(nc, "PFT")
  lai <- ncvar_get(nc, "MMEAN_LAI_CO")
  wai <- ncvar_get(nc, "WAI_CO")
  nc_close(nc)
  cai <- rep(1, length(pft))
  pft_list[[f]] <- pft
  lai_list[[f]] <- lai
  wai_list[[f]] <- wai
  results[[f]] <- edr_r(
    pft = pft, lai = lai, wai = wai, cai = cai,
    N = N, Cab = Cab, Car = Car, Cw = Cw, Cm = Cm,
    orient_factor = orient_factor,
    clumping_factor = clumping_factor,
    soil_moisture = soil_brightness,
    direct_sky_frac = direct_sky_frac,
    czen = czen
  )
  setTxtProgressBar(pb, i / length(outfiles))
}
close(pb)

# Plot LAI to confirm that values are reasonable
lai_sum <- sapply(lai_list, sum)
plot(years, lai_sum, type = "l",
     ylab = "Total LAI", xlab = "Year")

# Plot evolution of hyperspectral albedo through time
cols <- hcl.colors(length(years))
results_mat <- do.call(cbind, lapply(results, "[[", "albedo"))
matplot(400:2500, results_mat, type = "l",
        xlab = "Wavelength (nm)",
        ylab = "Albedo [0,1]",
        lty = "solid", col = cols)
