#' @param patch patch
#' @return
#' @author Félicien Meunier
#' @export

merge_cohorts <- function(patch){

  patch_in <- patch
  patch_out <- patch_in[1,]
  patch_out <- patch_out[-c(1),] # empty

  nrow_beg <- nrow(patch_in)
  nrow_end <- nrow(patch_out)
  iter = 1
  while((nrow_beg != nrow_end) & iter < 50){
    for (i in seq(2,nrow_beg)){
      coh1 <- patch_in[i-1,]
      coh2 <- patch_in[i,]
      if ((coh1[["pft"]] == coh2[["pft"]])){ # merge

        coh2["dbh"] <-  (coh2["dbh"] + coh1["dbh"])/2
        coh2["lai"] <-  coh1["lai"] +  coh2["lai"]
        coh2["wai"] <-  coh1["wai"] +  coh2["wai"]
        coh2["cai"] <-  (coh1["cai"] +  coh2["cai"])/2

        coh1["dbh"] <- 0
        coh1["lai"] <- 0
        coh1["wai"] <- 0
        coh1["wai"] <- 0

        patch_in[i-1,] <- coh1
        patch_in[i,] <- coh2
      }
    }

    patch_out <- patch_in[patch_in[,"lai"]>0,]

    nrow_beg <- nrow(patch_in)
    nrow_end <- nrow(patch_out)

    patch_in <- patch_out
    nrow_beg <- nrow(patch_in)
    iter <- iter +1
  }
  return(patch_in)
}
