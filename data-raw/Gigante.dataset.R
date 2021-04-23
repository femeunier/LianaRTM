library(LianaRTM)

Gigante.data <- bind_rows(Liana.Gigante.data,Tree.Gigante.data)

usethis::use_data(Gigante.data, overwrite = TRUE)
