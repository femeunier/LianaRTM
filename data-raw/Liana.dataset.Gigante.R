## code to prepare `Gigante_control` dataset goes here

library(dplyr)
library(purrr)
library(tidyr)

file.liana <- file.path(getwd(),"data","Liana data 210119a.csv")
data.liana <- read.csv(file.liana,stringsAsFactors = FALSE) %>% mutate(ID = row_number()) %>% rename(Date.0 = Date,
                                                                                                     DBH.0 = DBH)

data.liana.formatted <- data.liana %>% group_by(ID) %>% select(c("plot","ID","cuadrante",starts_with("Date"))) %>%
  pivot_longer(cols = -c("ID","plot","cuadrante"),
               names_to = "date",
               values_to = "Time") %>% mutate(timing = as.numeric(gsub(".*Date.", "", date))) %>% left_join(data.liana %>% group_by(ID) %>% select(c("ID",starts_with("DBH"))) %>%
                                                                                                              pivot_longer(cols = -c("ID"),
                                                                                                                           names_to = "date",
                                                                                                                           values_to = "DBH") %>% mutate(timing = as.numeric(gsub(".*DBH.", "", date))),by = c("ID","timing")) %>%
  select(-c(date.x,date.y))

Liana.Gigante.data <- data.liana.formatted %>% mutate(BA = pi/4*DBH^2,
                                              AGB = exp(-0.968+2.657*log(DBH/10)),
                                              AGBC = 0.5*exp(-0.968+2.657*log(DBH/10))) %>% group_by(timing) %>% mutate(Time2 = mean(Time,na.rm = TRUE),
                                                                                                                        timing = 1 + timing,
                                                                                                                        Treatment = "C",
                                                                                                                        WD = NA,
                                                                                                                        CC = 50,
                                                                                                                        GF = "Liana")

usethis::use_data(Liana.Gigante.data, overwrite = TRUE)
