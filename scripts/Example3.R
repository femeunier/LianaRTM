rm(list = ls())

library(hsdar)
library(LianaRTM)
library(rrtm)
library(dplyr)
library(ggplot2)
library(GeoLight)

head(Gigante.data)

Removal.data <- Gigante.data %>% mutate(DBH = DBH/10) # mm --> cm

bands_WV2 <- get.sensor.characteristics("WorldView2-8", FALSE)
central.wl <- (bands_WV2[-c(2),"lb"] + bands_WV2[-c(2),"ub"])/2

# Timing
target.season <- "dry"   # dry/wet
target.year <- 2012
delta_season1 <- ifelse(target.season == "dry",0,4/12)
delta_season2 <- ifelse(target.season == "dry",3/12,12/12)

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
soil_brightness <- 0.1   # Hapke model parameter
direct_sky_frac <- 0.7   # Assume relatively clear sky

s <- solar(as.POSIXct("2011-01-01 12:00:00",tz = "EST"))
czen <- cos((zenith(s, lon = -79.8, lat = 9.2))*pi/180)

edr_r_outputs <- list()

df.albedo <- df.patch <- df.bands <- data.frame()

for (target.plot in sort(unique(Removal.data$plot))){

  cat("Simulating plot #",as.character(target.plot),"| progress = ",format((target.plot/length(unique(Removal.data$plot))*100)),"% \n")

  Removal.data.select <- Removal.data %>%
    dplyr::filter(plot == target.plot,
           Time2 >= (target.year + delta_season1),
           Time2 <= (target.year + delta_season2),
           !is.na(DBH))

  subplots <- unique(Removal.data.select$cuadrante)

  Removal.data.select <- Removal.data.select %>%
    mutate(H = case_when(!(GF == "Liana") ~ (href[GF]*(1 -exp(-b1Ht[GF]*((10*DBH)**b2Ht[GF])))),
                         ((GF == "Liana") & DBH > dbh_min["Liana"]) ~ Delta_H + max(H[!(GF == "Liana")],na.rm = TRUE),
                         ((GF == "Liana") & DBH <= dbh_min["Liana"]) ~ pmin(Delta_H + max(H[GF != "Liana"],na.rm = TRUE),
                                                                            href[GF]*(1 -exp(-b1Ht[GF]*(DBH**b2Ht[GF]))))),
           Bl = b1Bl[GF]*(DBH**b2Bl[GF]),
           lai = SLA[GF]*Bl/((60*60)/length(subplots)),
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

  for (isubplot in seq(1,length(subplots))){

    Removal.data.select.plot <- Removal.data.select %>% dplyr::filter(cuadrante == subplots[isubplot])

    Removal.data.select.plot.arranged <- Removal.data.select.plot %>% arrange((H)) # Tallest cohort must be last in sw_two_stream


    edr_r_outputs[[target.plot]] <-
      with(Removal.data.select.plot.arranged,
           edr_r(pft = pft, lai = lai, wai = wai, cai = cai,
                 N = N, Cab = Cab, Car = Car, Cw = Cw, Cm = Cm,
                 orient_factor = orient_factor,
                 clumping_factor = clumping_factor,
                 soil_moisture = soil_brightness,
                 direct_sky_frac = direct_sky_frac,
                 czen = czen))

     # Faster but is it correct?
     # Removal.data.select.plot.arranged.merged <- merge_cohorts(Removal.data.select.plot.arranged %>% mutate(dbh = DBH),
     #                                                           Niter = 50)
     # edr_r_outputs[[target.plot]]  <-
     #  with(Removal.data.select.plot.arranged.merged,
     #       edr_r(pft = pft, lai = lai, wai = wai, cai = cai,
     #             N = N, Cab = Cab, Car = Car, Cw = Cw, Cm = Cm,
     #             orient_factor = orient_factor,
     #             clumping_factor = clumping_factor,
     #             soil_moisture = soil_brightness,
     #             direct_sky_frac = direct_sky_frac,
     #             czen = czen))

    WLs <- eval(formals(edr_r)[["wavelengths"]])

    df.patch <- bind_rows(list(df.patch,
                               Removal.data.select.plot.arranged %>%
                                 group_by(GF) %>% summarise(N = length(ID),
                                                            DBHm = mean(DBH,na.rm = TRUE),
                                                            understorey.light = mean(edr_r_outputs[[target.plot]][["light_level"]][WLs>=400 & WLs <=700,1]),
                                                            lai = sum(lai),
                                                            .groups = "keep") %>% mutate(plot = target.plot,
                                                                                         subplot = isubplot,
                                                                                         Treatment = Removal.data.select[1,] %>% pull(Treatment))))

    albedo <- edr_r_outputs[[target.plot]][["albedo"]]
    names(albedo) <- WLs

    albedo.band <- aggregate.albedo.platform(albedo,WV2.bands[,-c(2)])

    df.bands <- bind_rows(list(df.bands,
                               data.frame(plot = target.plot,
                                          subplot = isubplot,
                                          Treatment = Removal.data.select[1,"Treatment"],
                                          albedo = as.vector(albedo.band),
                                          band = names(albedo.band),
                                          wl = central.wl)))



    df.albedo <- bind_rows(list(df.albedo,
                                data.frame(plot = target.plot,
                                           subplot = isubplot,
                                           Treatment = Removal.data.select[1,"Treatment"],
                                           albedo = albedo,
                                           wavelength = eval(formals(edr_r)[["wavelengths"]]))))
  }
}

df.patch.all <- bind_rows(list(df.patch,
                               df.patch %>%
                                 group_by(plot,subplot,Treatment) %>% summarise(lai = sum(lai),
                                                                                understorey.light = mean(understorey.light),
                                                                                GF = "all",
                                                                                DBHm = weighted.mean(DBHm,N),
                                                                                N = sum(N),
                                                                                .groups = "keep")))
ggplot(data = df.patch.all) +
  geom_boxplot(aes(x = Treatment, y = lai,fill = GF)) +
  theme_bw()

df.patch.all %>% group_by(Treatment,GF) %>% summarise(lai.m = mean(lai),
                                                      .groups = "keep")

ggplot(data = df.patch  %>%
         group_by(Treatment,plot,subplot) %>%
         summarise(understorey.light.m = mean(understorey.light, na.rm = TRUE),
                   .groups = "keep")) +
  geom_boxplot(aes(x = Treatment, y = understorey.light.m)) +
  theme_bw()


df.treatment <- df.albedo %>% group_by(Treatment,wavelength) %>% summarise(albedo.m = mean(albedo,na.rm = TRUE),
                                                                           albedo.sd = sd(albedo,na.rm = TRUE),
                                                                           .groups = "keep")

ggplot(data = df.treatment,
       aes(x = wavelength, y = albedo.m, ymin = albedo.m - albedo.sd, ymax = albedo.m + albedo.sd,
           color = as.factor(Treatment), fill = as.factor(Treatment))) +
  geom_ribbon(alpha = 0.5,color = NA) +
  geom_line() +
  theme_bw()


df.bands %>% group_by(Treatment,wl,band) %>% summarise(N = length(albedo),
                                                       albedo.m = mean(albedo),
                                                       albedo.median = median(albedo),
                                                       .groups = "keep") %>% arrange(wl)


ggplot() +
  geom_boxplot(data = df.bands,
               aes(x = (wl), y = albedo, fill = as.factor(Treatment), group = interaction(wl,Treatment))) +
  geom_ribbon(data = df.treatment,
              aes(x = wavelength, y = albedo.m, ymin = albedo.m - albedo.sd, ymax = albedo.m + albedo.sd,
                  color = as.factor(Treatment), fill = as.factor(Treatment)),alpha = 0.1,color = NA) +
  geom_line(data = df.treatment,
            aes(x = wavelength, y = albedo.m,color = as.factor(Treatment))) +
  scale_x_continuous(limits = c(400,1000)) +
  theme_bw()



