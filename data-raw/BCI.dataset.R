library(LianaRTM)

trees <- tree.BCI %>% filter(census.time == 2005)
lianas <- liana.BCI

BCI.data <- bind_rows(list(trees,
                         lianas))

usethis::use_data(BCI.data, overwrite = TRUE)
