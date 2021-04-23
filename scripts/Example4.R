rm(list = ls())

library(LianaRTM)
library(rrtm)
library(dplyr)
library(ggplot2)

head(BCI.data)

BCI.data <- BCI.data %>% mutate(GF = case_when(is_liana ~ "Liana",
                                               !is_liana ~ "Tree"))

## PFT parameters
# Height parameters
dbh_min = c(Liana = 3.)
href = c(Liana = 61.7)
b1Ht = c(Liana = 0.1)
b2Ht = c(Liana = 0.87)
Delta_H = 0.5

# Tropical tree height and crown allometries for the Barro Colorado
# Nature Monument, Panama: a comparison of alternative
# hierarchical models incorporating interspecific variation
# in relation to life history traits
csvfile <- file.path("./data/","height_allometry.csv")
data.H <- read.csv(csvfile)

BCI.data <- BCI.data %>% left_join(data.H %>% dplyr::select(-c(Genus,Species)),
                                   by = "sp") %>% mutate(a = case_when(!is_liana & is.na(a) ~ 58.0,
                                                                       !is_liana ~ 1.017*a,
                                                                       TRUE ~ NA_real_),
                                                         b = case_when(!is_liana & is.na(b) ~ 0.73,
                                                                       !is_liana ~ b,
                                                                       TRUE ~ NA_real_),
                                                         k = case_when(!is_liana & is.na(k) ~ 21.8,
                                                                       !is_liana ~ k,
                                                                       TRUE ~ NA_real_)) %>% group_by(patch) %>%
  mutate(H = case_when(!is_liana ~ (a*((dbh)**b))/(k + ((dbh)**b)),
                       is_liana ~ NA_real_)) %>%
  mutate(H = case_when(((is_liana) & dbh > dbh_min["Liana"]) ~ Delta_H + max(H[!(is_liana)],na.rm = TRUE),
                       ((is_liana) & dbh <= dbh_min["Liana"]) ~ pmin(Delta_H + max(H[!(is_liana)],na.rm = TRUE),
                                                                     href[GF]*(1 -exp(-b1Ht[GF]*(dbh**b2Ht[GF])))),
                       TRUE ~ H)) %>% ungroup()

# ggplot(data = BCI.data %>% filter(patch == 100)) +
#   geom_point(aes(x = dbh, y = H, color = as.factor(is_liana))) +
#   theme_bw()

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

edr_r_outputs <- edr_r_outputs_trees <- list()

df.patch <- data.frame()

patches <- sort(unique(BCI.data$patch))

Delta_xy = sqrt(50*100*100/length(patches))

for (ipatch in seq(1,length(patches))){

  target.patch <- patches[ipatch]

  cat("\r Simulating patch #",as.character(target.patch),
      "| progress = ",format((target.patch/length(patches)*100)),"% \n")


  BCI.data.patch <- BCI.data %>%
    filter(patch == target.patch,
           !is.na(dbh))


  BCI.data.patch <- BCI.data.patch %>%
    mutate(Bl = b1Bl[GF]*(dbh**b2Bl[GF]),
           lai = SLA[GF]*Bl/(Delta_xy*Delta_xy),
           wai = 0,
           cai = 1,
           pft = case_when(is_liana ~ 1,
                           !is_liana ~ 2))

  # ggplot(data = BCI.data.patch) +
  #   geom_point(aes(x = dbh,y = lai, color = as.factor(pft))) +
  #   theme_bw()
  # ggplot(data = BCI.data.patch) +
  #   geom_point(aes(x = dbh,y = H,color = as.factor(GF))) +
  #   theme_bw()

  BCI.data.patch.arranged <- BCI.data.patch %>% arrange((H)) # Tallest cohort must be last in sw_two_stream

  BCI.data.patch.arranged.merged <- merge_cohorts(BCI.data.patch.arranged)

  edr_r_outputs[[target.patch]] <-
    with(BCI.data.patch.arranged.merged,
         edr_r(pft = pft, lai = lai, wai = wai, cai = cai,
               N = N, Cab = Cab, Car = Car, Cw = Cw, Cm = Cm,
               orient_factor = orient_factor,
               clumping_factor = clumping_factor,
               soil_moisture = soil_brightness,
               direct_sky_frac = direct_sky_frac,
               czen = czen))

  edr_r_outputs_trees[[target.patch]] <-
    with(BCI.data.patch.arranged.merged %>% mutate(pft = 2),
         edr_r(pft = pft, lai = lai, wai = wai, cai = cai,
               N = N, Cab = Cab, Car = Car, Cw = Cw, Cm = Cm,
               orient_factor = orient_factor,
               clumping_factor = clumping_factor,
               soil_moisture = soil_brightness,
               direct_sky_frac = direct_sky_frac,
               czen = czen))

  WLs <- eval(formals(edr_r)[["wavelengths"]])

  # plot(WLs,edr_r_outputs_trees[[target.patch]][["albedo"]],type = 'l')
  # lines(WLs,edr_r_outputs[[target.patch]][["albedo"]],type = 'l',col = 'red')

  df.patch <- bind_rows(list(df.patch,
                             BCI.data.patch.arranged.merged %>%
                               group_by(is_liana) %>% summarise(N = length(patch),
                                                                DBHm = mean(dbh,na.rm = TRUE),
                                                                BA = sum(pi*(dbh**2)/4)/(Delta_xy*Delta_xy),

                                                                density = nrow(BCI.data.patch.arranged %>% filter(is_liana))/(Delta_xy*Delta_xy),
                                                                large.density = nrow(BCI.data.patch.arranged %>% filter(is_liana,
                                                                                                                        dbh >= dbh_min))/(Delta_xy*Delta_xy),

                                                                albedo = mean(edr_r_outputs[[target.patch]][["albedo"]]),
                                                                albedo.par =  mean(edr_r_outputs[[target.patch]][["albedo"]][WLs>=400 & WLs <=700]),
                                                                albedo.greenpeak =  mean(edr_r_outputs[[target.patch]][["albedo"]][WLs>=525 & WLs <=575]),
                                                                albedo.NIR =  mean(edr_r_outputs[[target.patch]][["albedo"]][WLs>=750 & WLs <=1000]),

                                                                understorey.light = mean(edr_r_outputs[[target.patch]][["light_level"]][WLs>=400 & WLs <=700,1]),

                                                                albedo.tree = mean(edr_r_outputs_trees[[target.patch]][["albedo"]]),
                                                                albedo.par.tree =  mean(edr_r_outputs_trees[[target.patch]][["albedo"]][WLs>=400 & WLs <=700]),
                                                                albedo.greenpeak.tree =  mean(edr_r_outputs_trees[[target.patch]][["albedo"]][WLs>=525 & WLs <=575]),
                                                                albedo.NIR.tree =  mean(edr_r_outputs_trees[[target.patch]][["albedo"]][WLs>=750 & WLs <=1000]),

                                                                understorey.light.tree = mean(edr_r_outputs_trees[[target.patch]][["light_level"]][WLs>=400 & WLs <=700,1]),

                                                                lai = sum(lai),

                                                                .groups = "keep") %>% mutate(patch = target.patch)
  )
  )
}

# ggplot(data = df.albedo) +
#   geom_line(aes(x = wavelength, y = albedo, color = (liana.density),group = interaction(patch))) +
#   theme_bw()


df.patch.all <- df.patch %>% group_by(patch) %>% summarise(lai.tot = sum(lai),

                                                           BA.liana = BA[is_liana],

                                                           lai.liana = lai[is_liana],
                                                           liana.density = density[is_liana],
                                                           largeliana.density = large.density[is_liana],

                                                           albedo = mean(albedo),
                                                           albedo.par = mean(albedo.par),
                                                           albedo.greenpeak = mean(albedo.greenpeak),
                                                           albedo.NIR = mean(albedo.NIR),

                                                           understorey.light = mean(understorey.light),

                                                           albedo.tree = mean(albedo.tree),
                                                           albedo.par.tree = mean(albedo.par.tree),
                                                           albedo.greenpeak.tree = mean(albedo.greenpeak.tree),
                                                           albedo.NIR.tree = mean(albedo.NIR.tree),

                                                           understorey.light.tree = mean(understorey.light.tree))

ggplot(data = df.patch) +
  geom_boxplot(aes(x = as.factor(is_liana),y = lai, fill = as.factor(is_liana))) +
  theme_bw()

ggplot(data = df.patch.all) +
  geom_histogram(aes(x = 100*lai.liana/lai.tot)) +
  theme_bw()

ggplot(data = df.patch.all %>% filter(lai.tot >= 3.5),
       aes(x = lai.liana/lai.tot, y = albedo.greenpeak)) +
  geom_point() +
  theme_bw()

ggplot(data = df.patch.all %>% filter(lai.tot >= 3.5),
       aes(x = lai.liana, y = (albedo.NIR))) +
  geom_point() +
  theme_bw()

ggplot(data = df.patch.all %>% filter(lai.tot >= 3.5),
       aes(x = lai.liana, y = albedo.greenpeak - albedo.greenpeak.tree)) +
  geom_point() +
  theme_bw()

ggplot(data = df.patch.all %>% filter(lai.tot >= 3.5)) +
  geom_point(aes(x = lai.liana/lai.tot, y = (albedo.NIR - albedo.NIR.tree)/albedo.NIR.tree)) +
  theme_bw()

ggplot(data = df.patch.all %>% filter(lai.tot >= 3.5),
       aes(x = lai.liana, y = understorey.light, color = (lai.liana))) +
  geom_point() +
  geom_smooth(method = lm,formula= ((y) ~ (x)), se=TRUE) +
  theme_bw()


ggplot(data = df.patch.all) +
  geom_point(aes(x = lai.liana,
                 y = (understorey.light - understorey.light.tree)/understorey.light.tree)) +
  theme_bw()
