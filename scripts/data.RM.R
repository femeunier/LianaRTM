rm(list = ls())

library(purrr)
library(tidyr)
library(ggplot2)
library(dplyr)
library(stringr)
library(hsdar)

LianaRemovalPixelReflectanceWV3 <- readRDS("./data/LianaRemovalPixelReflectanceWV3.rds")

all.data <- LianaRemovalPixelReflectanceWV3 %>% pivot_longer(cols = -c("type","plot"),
                                                             names_to = "wl",
                                                             values_to = "reflectance") %>% rename(band = wl) %>%
  mutate(wl = str_sub(band,3),
         reflectance = reflectance/10000)


ggplot(data = all.data %>% filter(wl > 450)) +
  geom_boxplot(aes(x = as.factor(wl),y = reflectance,fill = as.factor(type))) +
  theme_bw()

all.data %>% group_by(wl) %>% summarise(p.val = summary(aov(reflectance ~ as.factor(type)))[[1]][1,5])
all.data %>% group_by(wl, type) %>% summarise(N = length(reflectance),
                                              r.m = mean(reflectance),
                                              r.median = median(reflectance))



