library(dplyr)
library(LianaRTM)

census.file <- "./data/census_BCI_all.csv"
data.BCI <- read.csv(census.file)

Delta_XY <- 25

liana.BCI <- data.BCI %>% filter(is_liana) %>% filter(gx <= 1000,
                                                      gx >= 0,
                                                      gy >= 0,
                                                      gy <= 500) %>% mutate(patch = (patchnumber_from_position(gx,gy,Delta_XY,Delta_XY))) %>%
  dplyr::select(tag,scientific,patch,gx,gy,dbh) %>% mutate(is_liana = TRUE,
                                                           DFstatus = "alive",
                                                           census.time = 2007) %>%
  rename(treeID = tag,
         sp = scientific)


usethis::use_data(liana.BCI, overwrite = TRUE)
