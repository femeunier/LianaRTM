library(LianaRTM)

Removal.data <- bind_rows(Liana.data,Tree.data)

usethis::use_data(Removal.data, overwrite = TRUE)
