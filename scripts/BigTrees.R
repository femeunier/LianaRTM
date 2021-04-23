rm(list = ls())

library(LianaRTM)
library(rrtm)
library(dplyr)
library(ggplot2)

head(BCI.data)

# Heigth Parameters
dbh_min = c(Liana = 3.)
href = c(Liana = 61.7)
b1Ht = c(Liana = 0.1)
b2Ht = c(Liana = 0.87)
Delta_H = 0.5

csvfile <- file.path("./data/","height_allometry.csv")
data.H <- read.csv(csvfile)

# Tropical tree height and crown allometries for the Barro Colorado
# Nature Monument, Panama: a comparison of alternative
# hierarchical models incorporating interspecific variation
# in relation to life history traits

BCI.data <- BCI.data %>% left_join(data.H %>% dplyr::select(-c(Genus,Species)),
                                           by = "sp") %>% mutate(a = case_when(!is_liana & is.na(a) ~ 58.0,
                                                                               !is_liana ~ 1.017*a,
                                                                               TRUE ~ NA_real_),
                                                                 b = case_when(!is_liana & is.na(b) ~ 0.73,
                                                                               !is_liana ~ b,
                                                                               TRUE ~ NA_real_),
                                                                 k = case_when(!is_liana & is.na(k) ~ 21.8,
                                                                               !is_liana ~ k,
                                                                               TRUE ~ NA_real_)) %>%
  mutate(H = case_when(!is_liana ~ (a*((dbh)**b))/(k + ((dbh)**b)),
                       is_liana ~ NA_real_))

# LAI parameters
b1Bl = c(Liana = 0.049, Tree = 0.02)
b2Bl = c(Liana = 1.89, Tree = 1.85)
SLA = c(Liana = 1/0.064, Tree = 1/0.087)

patches <- sort(unique(BCI.data$patch))
Delta_xy = sqrt(50*100*100/length(patches))

BCI.data <- BCI.data %>% mutate(GF = case_when(is_liana ~ "Liana",
                                               !is_liana ~ "Tree"))

BCI.data <- BCI.data %>%
  mutate(H = case_when(((is_liana) & dbh > dbh_min["Liana"]) ~ Delta_H + max(H[!(is_liana)],na.rm = TRUE),
                       ((is_liana) & dbh <= dbh_min["Liana"]) ~ pmin(Delta_H + max(H[!(is_liana)],na.rm = TRUE),
                                                                     href[GF]*(1 -exp(-b1Ht[GF]*(dbh**b2Ht[GF])))),
                       TRUE ~ H),
         Bl = b1Bl[GF]*(dbh**b2Bl[GF]),
         lai = SLA[GF]*Bl/(Delta_xy*Delta_xy),
         wai = 0,
         cai = 1,
         pft = case_when(is_liana ~ 1,
                         !is_liana ~ 2))


ggplot(data = BCI.data %>% filter(!is_liana, H > 25),
       aes(x = gx, y = gy)) +
  geom_point() +
  scale_x_continuous(expand = c(0,0),minor_breaks = seq(0,1000,20),breaks = seq(0,1000,200)) +
  scale_y_continuous(expand = c(0,0),minor_breaks = seq(0,500,20),breaks = seq(0,500,100)) +
  labs(x = "",y = "") +
  theme_bw() +
  theme(panel.grid.minor.x = element_line(size=0.5),
        panel.grid.major.x =  element_line(size=0.5),
        panel.grid.major.y = element_line(size=0.5),
        panel.grid.minor.y = element_line(size=0.5))
