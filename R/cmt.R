
#' set_cmt_sys sets a lcoal directory for the 
#'
#' @param dir defaults to "~/data/global/quakes/cmt17/" recursively genartes this if not in existence
#'
#' @return
#' @export
#'
#' @examples
set_cmt_sys <- function(dir=NA) {
  
  con <- file(system.file( file.path("bash",".cmtdir", fsep = .Platform$file.sep), package='cmt'), open="r+")
 #con <- file(paste0(system.file("", package="cmt"), "bash/.cmtdir"), open="r+")
  
 if (!is.na(dir)) {
   if( stringr::str_sub(dir, -1) !="/") dir= paste0(dir, "/")
   writeChar( dir, con , 
                             nchars= stringr::str_length(dir), eos=NULL)
 }
  #readChar("./inst/bash/.cmtdir", n=300)
  dir <- readChar(con, n=300)
  dir.ndk <- paste0(dir, "ndk")
  if (!dir.exists(dir.ndk)) dir.create(dir.ndk, recursive=T)
  message ("setting R_CMT_HOME to: ", dir)
  Sys.setenv(R_CMT_HOME = dir )  
  close(con)
}


#' Title
#'
#' @return
#' @export
#' invokes the bash script bash/ndk2fwf.bash"
#' @examples
ndk2fwf <- function(){
  system2(system.file( file.path("bash","ndk2fwf.bash", fsep = .Platform$file.sep), package='cmt'), 
          args=Sys.getenv("R_CMT_HOME"))
  
}

#' updates the R_CMT_HOME with latest files 
#'
#' @return
#' @export
#'
#' calls the bashe script "bash/update_cmt.bash"
#' 
#' @examples
update_cmt <- function() {
  if (Sys.getenv("R_CMT_HOME")=="")  set_cmt_sys()
  # system(paste0(  Sys.getenv("R_CMT_HOME"), "ndk/scripts/update_cmt.sh") )
  # sh.com <- paste("bash/update_cmt.bash "  , Sys.getenv("R_CMT_HOME"))
  system2(system.file(file.path("bash","update_cmt.bash", fsep = .Platform$file.sep), package='cmt'),
            args=Sys.getenv("R_CMT_HOME"))
 # system.file('scripts/peak_mem.sh', package='clustertools')
  ndk2fwf()
}



#' builds  the cmt tibble
#'
#' @return
#' @export
#'
#' @examples
cmt2rds <- function() {
  library(magrittr)
  if (Sys.getenv("R_CMT_HOME")=="")  set_cmt_sys()

 # system(paste0(Sys.getenv("R_CMT_HOME"), "/ndk/scripts/ndk2fwf.sh"))
  #cpos <- readr::fwf_empty("MLI  1976/01/01_01:29:39.6 -28.61 -177.64 159.0 6.2 0.0 KERMADEC_ISLANDS_REGION_ M010176A         B: 10  100 100 S: 10  100 100 M: 12  130 135 CMT: 1 BOXHD:  9.4 CENTROID:     13.8 0.2 -29.25 0.02 -176.96 0.01  47.8  0.6 FREE O-00000000000000 26 17.680 0.090 10.090 0.060 -7.770 0.070 11.390 0.160 14.520 0.160 -3.260 0.060 V10  18.940 75 283 111.260 12 119 -10.190 15 110  19.560 202 30  193 118 60  188 +")

  cpos <- readr::fwf_empty(system.file( file.path("bash","cmt_template.fwf.R", fsep = .Platform$file.sep), package='cmt') )


  cpos$col_names <-
    c(
      "referenceCatalog",
      "date",
      "lat",
      "long",
      "depth",
      "magnitude",
      "magnitude1",
      "region",
      "eventName",
      "B",
      "BStations",
      "BComponents",
      "BPeriod",
      "S",
      "SStations",
      "SComponents",
      "SPeriod",
      "M",
      "MStations",
      "MComponents",
      "MPeriod",
      "sourceType",
      "sourceInversion",
      "momentRateFunction",
      "momentRateDuration",
      "centroid",
      "centroidTime",
      "centroidTimeError",
      "centroidLatitude",
      "centroidLatitudeError",
      "centroidLongitude",
      "centroidLongitudeError",
      "centroidDepth",
      "centroidDepthError",
      "depthType",
      "timeStamp",
      "exponent",
      "Mrr",
      "MrrError",
      "Mtt",
      "MttError",
      "Mpp",
      "MppError",
      "Mrt",
      "MrtError",
      "Mrp",
      "MrpError",
      "Mtp",
      "MtpError",
      "versionCode",
      "Pv",
      "Pp",
      "Pa",
      "Bv",
      "Bp",
      "Ba",
      "Tv", # "eigenValue3",
      "Tp", #eigenValue3plunge",
      "Ta", #eigenValue3azimuth",
      "scalarMoment",
      "NP1strike",
      "NP1dip",
      "NP1rake",
      "NP2strike",
      "NP2dip",
      "NP2rake"
    )



  cmtrecord <-readr::read_fwf(paste0(Sys.getenv("R_CMT_HOME"),"tmpc.ndkj"), col_positions = cpos)    %>%
    tibble::as_tibble()
  cmtrecord$Mw<-2/3*log10(cmtrecord$scalarMoment*10^cmtrecord$exponent)-10.7
  cmtrecord$date <- lubridate::ymd_hms(paste(cmtrecord$date ))
  cmtrecord <- cmtrecord %>% dplyr::arrange(date) %>% dplyr::select( -M, -S, -B, -sourceType, -centroid,-versionCode)
  data.dir<-paste0(system.file("", package="cmt"), "data/")
  if (!dir.exists( data.dir)) dir.create(data.dir)

  saveRDS(list(data=cmtrecord, date=Sys.Date()),
          file=paste0(system.file("", package="cmt"), "data/cmt.RDS"))
 # file=paste0(Sys.getenv("R_CMT_HOME"),"cmt.RDS"))

  # save(cmt=list(data=cmtrecord, date=Sys.Date()),
  #         file=paste0(system.file("", package="cmt"), "data/cmt.RData"))


  cmtrecord <-readr::read_fwf(paste0(Sys.getenv("R_CMT_HOME"),"tmpq.ndkj"), col_positions = cpos)    %>%
    tibble::as_tibble()
  cmtrecord$Mw<-2/3*log10(cmtrecord$scalarMoment*10^cmtrecord$exponent)-10.7
  cmtrecord$date <- lubridate::ymd_hms(paste(cmtrecord$date ))
  cmtrecord <- cmtrecord %>% dplyr::arrange(date) %>% dplyr::select( -M, -S, -B, -sourceType, -centroid,-versionCode)
  saveRDS(list(data=cmtrecord, date=Sys.Date()),
          file=paste0(system.file("", package="cmt"), "data/qcmt.RDS"))
  #file=system.file("data/qcmt.RDS", packae="cmt"))
  # save(qcmt=list(data=cmtrecord, date=Sys.Date()),
  #         file=paste0(system.file("", package="cmt"), "data/qcmt.RData"))


}

read_cmtRDS<- function(quick=F){
  # if (Sys.getenv("R_CMT_HOME")=="")  set_cmt_sys()
  # if (quick ==T) readRDS( file=paste0(Sys.getenv("R_CMT_HOME"),"qcmt.RDS"))  else readRDS(file=paste0(Sys.getenv("R_CMT_HOME"),"cmt.RDS"))

  data.dir<-paste0(system.file("", package="cmt"), "data/")
  if (!dir.exists( data.dir))  cmt2rds()
    if (quick ==T) readRDS( file=paste0(data.dir,"qcmt.RDS"))  else readRDS(file=paste0(data.dir,"cmt.RDS"))



}
##--------


#'  filter by geographic region name in regex
#'
#' @param data defaults to "TIMOR|BANDA|INDONE"
#' @param f 
#' @param quick 
#'
#' @return
#' @export
#'
#' @examples
filter_region <- function(data =NA, f="TIMOR|BANDA|INDONE", quick=F) {
  if (is.na(data)) read_cmtRDS(quick=quick)$data %>% dplyr::filter(  grepl(f,region)) else
    data %>% dplyr::filter(  grepl(f,region))

}

#' filter by mw
#'
#' @param f defult f=7
#' @param data  default=NA in whcihc ase cmt.RDS of qmt.RDS is read
#' @param quick  dafault=F, set to TRUE if quick cmt is requird.
#'
#' @return filter cmt tibble
#' @export
#'
#' @examples
filter_mw <- function(f=7, data =NA, quick=F) {
  if (is.na(data)) read_cmtRDS(quick=quick)$data %>% dplyr::filter(  Mw>=f) else
    data %>% dplyr::filter(  Mw>=f)

}

#' filter by depth
#'
#' @param f default c(),300)
#' @param data  default=NA in which case cmt.RDS of qmt.RDS is read
#' @param quick  dafault=F, set to TRUE if quick cmt is requird.
#'
#' @return filter cmt tibble
#' @export
#'
#' @examples
filter_depth <- function(f=c(0,300), data = NA,  quick=F) {
  if (length(f)==1) f= c(f, 1000.)
  print(f)
  if (is.na(data)) read_cmtRDS(quick=quick)$data %>% dplyr::filter(  depth >f[1] &  depth< f[2]) else
    data %>% dplyr::filter( depth >f[1] &  depth< f[2])

}

filter_long <- function(f=c(100,160),data = NA,  quick=F) {
  if (length(f)==1) f= c(f, 180)
  print(f)
  if (is.na(data)) read_cmtRDS(quick=quick)$data %>% dplyr::filter(  long>f[1] & long< f[2]) else
    data %>% dplyr::filter( long >f[1] &  long< f[2])

}

filter_lat <- function(f=c(-25,10),data = NA,  quick=F) {
  if (length(f)==1) f= c(f, 90)
  print(f)
  if (is.na(data)) read_cmtRDS(quick=quick)$data %>% dplyr::filter(  lat>f[1] & lat < f[2]) else
    data %>% dplyr::filter( lat >f[1] &  lat< f[2])

}

minmax <- function(data,  tight=0){
  range <- c(min(data), max(data))
  range + (c(-1,1 )*diff(range)*(tight/100))
}

plot_cmt <- function(data, tight=NA, projection="mollweide") {
  library(ggplot2)
  world <- map_data("world")

  if(!is.na(tight)) {
    long.range=minmax(data$long,  tight)
    lat.range=minmax(data$lat, tight)}
  else {
    long.range=minmax(-180,180)
    lat.range=minmax(-90,90)
  }
  ggplot(data, aes(long, lat)) +
   geom_path(data=world, aes( group = group), fill="grey")+
     geom_point(aes( size=Mw,  col=depth))+
    xlim(long.range)+
    ylim(lat.range)+
    coord_map(projection=projection)
}


#' @param norm
#' @param simplify
#'
#' @return
#' @export
#'
#' @examples
#'
vec_lines<- function(wsm,   scale=1, psp=F, segment=F, norm=T, simplify=T, ax="P"){

  names(wsm) <- stringr::str_to_lower(names(wsm))
  print(names(wsm))
  print(wsm$x)
  #returns stress trajectories
  #scale=1
  if (ax=="B") {wsm$marks$dip=wsm$marks$bp
  wsm$marks$azi=wsm$marks$ba}
   else if (ax=="P")  { wsm$marks$dip=wsm$marks$tp
   wsm$marks$azi=wsm$marks$ta} else
   {wsm$marks$dip=wsm$marks$pp
   wsm$marks$azi=wsm$marks$pa}

 # if (is.null( wsm$marks$dip))   wsm$marks$dip <-0
  if (norm) scale<- cos(wsm$marks$dip*pi/180)*scale #norm means scale according dip
  sinazi<- sin(wsm$marks$azi*pi/180)*scale
  cosazi<- cos(wsm$marks$azi*pi/180)*scale
  x<- matrix(ncol=length(wsm$y),nrow=3)
  y<- matrix(ncol=length(wsm$y),nrow=3)
  x[1,]<-wsm$x+ sinazi
  x[2,]<-wsm$x- sinazi
  y[1,]<-wsm$y+ cosazi
  y[2,]<-wsm$y- cosazi
  #
    # print(x)
    # print(y)
  #
  # cols<- vector("character",length(wsm$regime))
  # cols[wsm$regime=="NF"] <-"Red"
  # cols[wsm$regime=="NS"] <-"Red"
  # cols[wsm$regime=="SS"] <-"Green"
  # cols[wsm$regime=="TF"] <-"Blue"
  # cols[wsm$regime=="TS"] <-"Blue"
  # cols[wsm$regime=="U"] <-"Black"
  # cols<- rep(cols, each=3)
  if (simplify){
    wsm$marks$regime[wsm$marks$regime=="TS"]<-"SS"
    # wsm$marks$regime[wsm$marks$regime=="U"]<-"NF"
    wsm$marks$regime[wsm$marks$regime=="NS"]<-"SS"
  }
  if (psp==T & segment==F ){
    xrange<- c(min(x[2,])-scale, max(x[1,])+scale)
    yrange<- c(min(y[2,])-scale, max(y[1,])+scale)
    print(xrange)
    print(yrange)

    return(psp(x[1,], y[1,], x[2,], y[2,], owin(xrange,yrange), marks =wsm$marks$regime))
  } else if (segment==T)  {

    data.frame(x=wsm$x, y=wsm$y,xstart=x[2,], xend=x[1,], ystart=y[2,], yend=y[1,], regime=wsm$marks$regime, plunge=wsm$marks$dip )
  } else
  {
    return(data.frame(x=as.vector(x), y=as.vector(y), regime=rep(wsm$marks$regime, each=3)))
  }
}


#
# world <- map_data("world")
# pc<-ggplot(tail(cmtrecord, last.events), aes(long, lat)) +
#   coord_map("lambert",  150,0)+
#   #coord_map("stereographic" )+
#   # coord_map("ortho", orientation = c(-21,134, 0))+
#   geom_polygon(data=world, aes( group = group), fill="grey")+
#   scale_size(range=c(.3,3))+geom_point(aes(size=mw, col=depth)) +
#   labs(title= paste("cmt : last", last.events, "events"))
# ggsave(paste0("cmt_last",last.events,"events.png"),pc, width=6, height=4)
#
# last.days=100
# pq<-ggplot(qcmtrecord %>% subset(date > Sys.Date()-last.days), aes(long, lat)) +
#   coord_map("lambert",  150,0)+
#   #coord_map("stereographic" )+
#   # coord_map("ortho", orientation = c(-21,134, 0))+
#   geom_polygon(data=world, aes( group = group), fill="grey")+
#   scale_size(range=c(.3,3))+geom_point(aes(size=mw, col=depth)) +
#   labs(title= paste("qcmt : last", last.days, "days"))
# ggsave(paste0("qcmt_last",last.days,"days.png"),pq, width=6, height=4)



# b <-dplyr::filter(cmtrecord, grepl( "TIMOR|BANDA|INDONE",region))
#
# tail(cmtrecord)
# library(ggplot2)
# ggplot(b, aes(long, lat, size=Mw)) +geom_point()
