#library(slab)

#' Title
#'
#' @param ras 
#'
#' @return
#' @export
#'
#' @examples
is.ras <- function(ras) {
  class(ras)[1] %in% c("Raster", "RasterLayer", "RasterBrick", "RasterStack")

}


#' Title
#'
#' @param cmt 
#' @param limsx 
#' @param limsy 
#' @param ppp 
#' @param simplify 
#'
#' @return
#' @export
#'
#' @examples
cmt_to_ppp <-
  function(cmt=NA,
           limsx = c(-180, 180),
           limsy = c(-90, 90), ppp=T, simplify=F) {
    if (is.ras(limsx))  {
      library(spatstat)
      extent.lim <- extent(limsx)
      print(extent.lim)
      limsy <-  extent.lim[3:4]
      limsx <- extent.lim[1:2]
    }
    if (length(limsx) >= 4) {
      limsy = limsx[3:4]
      limsx = limsx[1:2]
    }


    if (is.na(cmt) ) cmt <-read_cmtRDS()$data
    # cmt <-
    #   cmtrecord[, c(2, 5, 4, 6, 46, 47, 49, 50, 52, 53, 54, 55, 56, 57, 58, 59, 60)]
    # names(cmt) <- c(
    #   'date',
    #   'long',
    #   'lat',
    #   'depth',
    #   'tp',
    #   't',
    #   'bp',
    #   'b',
    #   'pp',
    #   'p',
    #   'scalarmoment',
    #   'np1strike',
    #   'np1dip',
    #   'np1rake',
    #   'NP2strike',
    #   'np2dip',
    #   'np2rake'
    # )
    #
    #cmt <- cmtrecord
    names(cmt) <- stringr::str_to_lower(names(cmt))
    #message(cmt %>% str())
    # cmt %>% names() %>% str_replace("eigenvalue1", "t") -> names( cmt)
    # cmt %>% names() %>% str_replace("eigenvalue2", "b") -> names( cmt)
    # cmt %>% names() %>% str_replace("eigenvalue3", "p") -> names( cmt)
    # cmt %>% names() %>% str_replace("plunge", "p") -> names( cmt)
    # cmt %>% names() %>% str_replace("azimuth", "a") -> names( cmt)
    # cmt %>% names() %>% str_replace("latitude", "lat") -> names( cmt)
    # cmt %>% names() %>% str_replace("longitude", "long") -> names( cmt)
    message("cmt records : ",stringr::str_c(names(cmt), ", "))

    cmt <-
      subset(cmt, long >= limsx[1] &
               long <= limsx[2] & lat >= limsy[1] & lat <= limsy[2])

    #cmt$date <- as.POSIXct(cmt$date)

    #see Stress derivation from earthquake focal mechanisms
    # A. Barth, J. Reinecker and O. Heidbach


    message("cmt regimes set according to wsm guidlelines as per http://www.world-stress-map.org/data/")
    cmt$sh <- cmt$pa
    if (simplify)  cmt$regime <- "U" else cmt$regime <- "U"

    cmt$quality <- "E"

    inds <- which(cmt$pp >= 52 & cmt$tp <= 35)
    cmt$sh[inds] <- cmt$ba[inds]
    cmt$regime[inds] <- "NF"
    cmt$quality[inds] <- "C"

    inds <- which(cmt$pp < 52 &  cmt$pp >= 40 & cmt$tp <= 20)
    cmt$sh[inds] <- cmt$ta[inds] + 90
    if (simplify)  cmt$regime[inds] <-"SS" else cmt$regime[inds] <- "NS"
    cmt$quality[inds] <- "C"


    inds <- which(cmt$pp < 40 &  cmt$bp >= 45 & cmt$tp <= 20)
    cmt$sh[inds] <- cmt$ta[inds] + 90
    cmt$regime[inds] <- "SS"
    cmt$quality[inds] <- "C"

    inds <- which(cmt$pp <= 20 & cmt$bp >= 45 & cmt$tp < 40)
    cmt$regime[inds] <- "SS"
    cmt$quality[inds] <- "C"

    inds <-  which(cmt$pp <= 20 &  cmt$tp >= 40 & cmt$tp < 52)  #cmt$AZI[inds] <-cmt$P[inds]
    if (simplify)  cmt$regime[inds] <- "TF" else cmt$regime[inds] <- "TS"
    cmt$quality[inds] <- "C"

    inds <-
      which(cmt$pp <= 35 & cmt$tp >= 52)  #cmt$AZI[inds] <-cmt$P[inds]
    cmt$regime[inds] <- "TF"
    cmt$quality[inds] <- "C"
    cmt$sh <- cmt$sh %% 180

    message( "setting cmt aizmuth to shmax (p-axis) by default")
    cmt$azi <- cmt$sh  #sets AZI <- SH by default

    if (ppp==TRUE)
      return( # modulu
        spatstat::ppp(cmt$long, cmt$lat, window=spatstat::owin(limsx,limsy),  marks =cmt)) else
          return(cmt) #data.frame(AZI=cmt$AZI, REGIME=cmt$REGIME, DEPTH=cmt$DEPTH, P=cmt$P, T=cmt$T, B=cmt$B))) else

  }
