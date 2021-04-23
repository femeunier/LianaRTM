library(dplyr)
library(LidarED)

folder <- "./data/"

times <- seq(1980,2010,5)

census <- list()
df.BCI <- data.frame()
for (i in seq(1,7)){
  censusname <- paste0("bci.full",i)
  datafile2read <- file.path(folder,paste0(censusname,".rdata"))
  load(datafile2read)
  census[[as.character(times[i])]] <- get(censusname)

  df.BCI <- bind_rows(list(df.BCI,
                           census[[as.character(times[i])]] %>% mutate(census.time = times[i])))
}


Delta_XY <- 25

patch.num <- df.BCI%>% filter(gx <= 1000,
                              gx >= 0,
                              gy >= 0,
                              gy <= 500,census.time == 2000) %>% mutate(patch = (patchnumber_from_position(gx,gy,Delta_XY,Delta_XY))) %>% pull(patch)

df.BCI.f <- df.BCI%>% filter(gx <= 1000,
                             gx >= 0,
                             gy >= 0,
                             gy <= 500) %>% mutate(patch = rep(patch.num,length(times)))

tree.BCI <- df.BCI.f %>% dplyr::select(treeID,sp,patch,DFstatus,census.time,gx,gy,dbh,agb) %>% mutate(is_liana = FALSE,
                                                                                                      dbh = dbh/10)

usethis::use_data(tree.BCI, overwrite = TRUE)
