rm(list = ls())

library(LianaRTM)
library(rrtm)
library(dplyr)
library(ggplot2)

head(Gigante.data)

Removal.data <- Gigante.data %>% mutate(DBH = DBH/10) # mm --> cm

# Timing
target.season <- "wet"   # dry/wet
target.year <- 2017

## PFT parameters
# Height parameters
dbh_min = c(Liana = 3.)
href = c(Liana = 61.7, Tree = 56.5)
b1Ht = c(Liana = 0.1, Tree = 0.011)
b2Ht = c(Liana = 0.87, Tree = 0.64)
Delta_H = 0.5

# LAI parameters
b1Bl = c(Liana = 0.049, Tree = 0.02)
b2Bl = c(Liana = 1.89, Tree = 1.85)
SLA = c(Liana = 1/0.064, Tree = 1/0.087)
clumping_factor = c(Liana = 0.72, Tree = 0.48)
orient_factor = c(Liana = 0.33, Tree = -0.42)

# Prospect parameters
N = c(Liana = 1.76, Tree = 2.06)
Cab = c(Liana = 45.5, Tree = 56.6)
Car = c(Liana = 15.9, Tree = 20.6)
Cw = c(Liana = 0.0152, Tree = 0.0199)
Cm = 1/(10*SLA)

# A few global parameters
soil_brightness <- 0.5   # Hapke model parameter
czen <- 0.85             # Approximate value for July at UMBS
direct_sky_frac <- 0.9   # Assume relatively clear sky

edr_r_outputs <- list()

df.albedo <- df.patch <- data.frame()

for (target.plot in sort(unique(Removal.data$plot))){

  cat("Simulating plot #",as.character(target.plot),"| progress = ",format((target.plot/length(unique(Removal.data$plot))*100)),"% \n")

  delta_season1 <- ifelse(target.season == "dry",0,4/12)
  delta_season2 <- ifelse(target.season == "dry",3/12,12/12)

  Removal.data.select <- Removal.data %>%
    filter(plot == target.plot,
           Time2 >= (target.year + delta_season1),
           Time2 <= (target.year + delta_season2),
           !is.na(DBH))

  # ggplot(data = Removal.data.select) +
  #   geom_boxplot(aes(x = as.factor(GF),y = DBH)) +
  #   theme_bw()

  Removal.data.select <- Removal.data.select %>%
    mutate(H = case_when(!(GF == "Liana") ~ (href[GF]*(1 -exp(-b1Ht[GF]*((10*DBH)**b2Ht[GF])))),
                         ((GF == "Liana") & DBH > dbh_min["Liana"]) ~ Delta_H + max(H[!(GF == "Liana")],na.rm = TRUE),
                         ((GF == "Liana") & DBH <= dbh_min["Liana"]) ~ pmin(Delta_H + max(H[GF != "Liana"],na.rm = TRUE),
                                                                            href[GF]*(1 -exp(-b1Ht[GF]*(DBH**b2Ht[GF]))))),
           Bl = b1Bl[GF]*(DBH**b2Bl[GF]),
           lai = SLA[GF]*Bl/(60*60),
           wai = 0,
           cai = 1,
           pft = case_when(GF == "Liana" ~ 1,
                           GF != "Liana" ~ 2))


  # ggplot(data = Removal.data.select) +
  #   geom_point(aes(x = DBH,y = lai, color = as.factor(pft))) +
  #   theme_bw()

  # ggplot(data = Removal.data.select) +
  #   geom_point(aes(x = DBH,y = H,color = as.factor(GF))) +
  #   theme_bw()

  Removal.data.select.arranged <- Removal.data.select %>% arrange((H)) # Tallest cohort must be last in sw_two_stream


  Removal.data.select.arranged.merged <- merge_cohorts(Removal.data.select.arranged %>% mutate(dbh = DBH))

  edr_r_outputs[[target.plot]] <-
    with(Removal.data.select.arranged.merged,
         edr_r(pft = pft, lai = lai, wai = wai, cai = cai,
               N = N, Cab = Cab, Car = Car, Cw = Cw, Cm = Cm,
               orient_factor = orient_factor,
               clumping_factor = clumping_factor,
               soil_moisture = soil_brightness,
               direct_sky_frac = direct_sky_frac,
               czen = czen))

  df.patch <- bind_rows(list(df.patch,
                             Removal.data.select.arranged %>%
                               group_by(GF) %>% summarise(N = length(ID),
                                                          DBHm = mean(DBH,na.rm = TRUE),
                                                          lai = sum(lai),
                                                          .groups = "keep") %>% mutate(plot = target.plot,
                                                                                       Treatment = Removal.data.select[1,] %>% pull(Treatment))))


  df.albedo <- bind_rows(list(df.albedo,
                               data.frame(plot = target.plot,
                                          Treatment = Removal.data.select[1,"Treatment"],
                                          albedo = edr_r_outputs[[target.plot]][["albedo"]],
                                          wavelength = eval(formals(edr_r)[["wavelengths"]]))))
}


ggplot(data = df.patch) +
  geom_boxplot(aes(x = Treatment, y = lai,fill = GF)) +
  theme_bw()


ggplot(data = df.albedo) +
  geom_line(aes(x = wavelength, y = albedo, color = as.factor(Treatment),group = interaction(plot,Treatment))) +
  theme_bw()

df.treatment <- df.albedo %>% group_by(Treatment,wavelength) %>% summarise(albedo.m = mean(albedo,na.rm = TRUE),
                                                                           albedo.sd = sd(albedo,na.rm = TRUE))

ggplot(data = df.treatment,
       aes(x = wavelength, y = albedo.m, ymin = albedo.m - albedo.sd, ymax = albedo.m + albedo.sd,
           color = as.factor(Treatment), fill = as.factor(Treatment))) +
  geom_ribbon(alpha = 0.5,color = NA) +
  # scale_x_continuous(limits = c(500,600)) +
  geom_line() +
  theme_bw()


