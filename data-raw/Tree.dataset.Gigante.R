library(dplyr)
library(purrr)
library(tidyr)
library(ggplot2)

file.tree <- file.path(getwd(),"data","Tree data based on dendros 210106.csv")
data.tree <- read.csv(file.tree) %>% mutate(ID = X) %>% dplyr::select(-c("Date_2008","Date_2010",
                                                                  "DBH2008","DBH2010",
                                                                  "BA2008","BA2010",
                                                                  "H2008","H2010",
                                                                  "AGB2008","AGB2010",
                                                                  "AGBC2008","AGBC2010","X"))


data.tree.formatted <- data.tree %>% group_by(ID) %>% dplyr::select(c("Treatment","Cuadrante","Parcela","WD","CC","ID",starts_with("Date"))) %>%
  pivot_longer(cols = -c("ID","Treatment","WD","CC","Parcela","Cuadrante"),
               names_to = "date",
               values_to = "Time") %>% mutate(timing = as.numeric(gsub(".*_", "", date))) %>% left_join(data.tree %>% group_by(ID) %>% select(c("ID","Cuadrante",starts_with("DBH"))) %>%
                                                                                                          pivot_longer(cols = -c("ID","Cuadrante"),
                                                                                                                       names_to = "date",
                                                                                                                       values_to = "DBH") %>% mutate(timing = as.numeric(gsub(".*DBH", "", date))),by = c("ID","timing","Cuadrante")) %>%
  left_join(data.tree %>% group_by(ID) %>% select(c("ID","Cuadrante",starts_with("AGB"))) %>%
              pivot_longer(cols = -c("ID","Cuadrante"),
                           names_to = "date",
                           values_to = "AGB") %>% mutate(timing = as.numeric(gsub(".*AGB", "", date))),by = c("ID","timing","Cuadrante")) %>%
  left_join(data.tree %>% group_by(ID) %>% select(c("ID","Cuadrante",starts_with("H"))) %>%
              pivot_longer(cols = -c("ID","Cuadrante"),
                           names_to = "date",
                           values_to = "H") %>% mutate(timing = as.numeric(gsub(".*H", "", date))),by = c("ID","timing","Cuadrante")) %>%
  left_join(data.tree %>% group_by(ID) %>% select(c("ID","Cuadrante",starts_with("BA"))) %>%
              pivot_longer(cols = -c("ID","Cuadrante"),
                           names_to = "date",
                           values_to = "BA") %>% mutate(timing = as.numeric(gsub(".*BA", "", date))),by = c("ID","timing","Cuadrante")) %>%
  left_join(data.tree %>% group_by(ID) %>% select(c("ID","Cuadrante",starts_with("AGBC"))) %>%
              pivot_longer(cols = -c("ID","Cuadrante"),
                           names_to = "date",
                           values_to = "AGBC") %>% mutate(timing = as.numeric(gsub(".*AGBC", "", date))),by = c("ID","timing","Cuadrante")) %>%
  select(-c(date.x,date.y,date.x.x,date.y.y,date.x.x.x,date.y.y.y)) %>% mutate(Time = case_when(Time < 2002 ~ Time + 10,
                                                                                                TRUE ~ Time))

Tree.Gigante.data <- data.tree.formatted %>% group_by(timing) %>% mutate(Time2 = mean(Time,na.rm = TRUE),
                                                                 GF = "Tree") %>% rename(plot = Parcela,
                                                                                         cuadrante = Cuadrante) %>%
  filter(cuadrante != 1) # remove suspicous record

usethis::use_data(Tree.Gigante.data, overwrite = TRUE)


