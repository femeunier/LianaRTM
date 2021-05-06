aggregate.albedo.platform <- function(albedo,bands){

  Nbands = (ncol(bands) - 1)

  albedo.band <- rep(NA,Nbands)
  names(albedo.band) <- colnames(bands)[2:(Nbands + 1)]

  WL.albedo <- as.numeric(names(albedo))
  WL.bands <- bands[,1]

  for (iband in seq(1,Nbands)){

    weights <- WL.albedo*0
    weights[WL.albedo %in% WL.bands] <- bands[WL.albedo %in% WL.bands,1 + iband]

    albedo.band[iband] <- weighted.mean(x = albedo,
                                        w = weights)
  }
  return(albedo.band)
}
