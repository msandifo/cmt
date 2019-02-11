# list.files("~/data/global/quakes/isc-fms/", pattern="*.csv",  full.names=T)-> csv.f
# purrr::map_df(csv.f, rio::import) -> isc.fms
#
# isc.fms[,1] <- stringr::str_extract(isc.fms[,1],"[0-9]{7}")
#
# isc.fms[,2] <-stringr::str_extract(isc.fms[,2],"[A-Z,_]{3,10}")
# isc.fms<-isc.fms[1:10,1:29]


# f18="http://www.isc.ac.uk/cgi-bin/web-db-v4?request=COMPREHENSIVE&out_format=FMCSV&bot_lat=&top_lat=&left_lon=&right_lon=&ctr_lat=&ctr_lon=&radius=&max_dist_units=deg&searchshape=GLOBAL&srn=&grn=&start_year=2018&start_month=1&start_day=01&start_time=00%3A00%3A00&end_year=2018&end_month=12&end_day=01&end_time=00%3A00%3A00&min_dep=&max_dep=&min_mag=&max_mag=&req_mag_type=&req_mag_agcy=&req_fm_agcy=Any&include_links=on"
#  download.file(f18, "~/data/global/quakes/isc-fms/f1018.csv")
#
# f00="http://www.isc.ac.uk/cgi-bin/web-db-v4?request=COMPREHENSIVE&out_format=FMCSV&bot_lat=&top_lat=&left_lon=&right_lon=&ctr_lat=&ctr_lon=&radius=&max_dist_units=deg&searchshape=GLOBAL&srn=&grn=&start_year=2000&start_month=1&start_day=01&start_time=00%3A00%3A00&end_year=2009&end_month=12&end_day=01&end_time=00%3A00%3A00&min_dep=&max_dep=&min_mag=&max_mag=&req_mag_type=&req_mag_agcy=&req_fm_agcy=Any&include_links=on"
# download.file(f00, "~/data/global/quakes/isc-fms/f0009.csv")
#
# f90="http://www.isc.ac.uk/cgi-bin/web-db-v4?request=COMPREHENSIVE&out_format=FMCSV&bot_lat=&top_lat=&left_lon=&right_lon=&ctr_lat=&ctr_lon=&radius=&max_dist_units=deg&searchshape=GLOBAL&srn=&grn=&start_year=1990&start_month=1&start_day=01&start_time=00%3A00%3A00&end_year=1999&end_month=12&end_day=01&end_time=00%3A00%3A00&min_dep=&max_dep=&min_mag=&max_mag=&req_mag_type=&req_mag_agcy=&req_fm_agcy=Any&include_links=on"
# download.file(f90, "~/data/global/quakes/isc-fms/f9099.csv")
#
isc_fms <- function(download=F, update=T) {

if (download) for ( i in 1964:lubridate::year(Sys.Date())){

  f.name= paste0("~/data/global/quakes/isc-fms/fms",i,".csv" )
  f.url=paste0("http://www.isc.ac.uk/cgi-bin/web-db-v4?request=COMPREHENSIVE&out_format=FMCSV&bot_lat=&top_lat=&left_lon=&right_lon=&ctr_lat=&ctr_lon=&radius=&max_dist_units=deg&searchshape=GLOBAL&srn=&grn=&start_year=",i,"&start_month=1&start_day=01&start_time=00%3A00%3A00&end_year=",i,"&end_month=12&end_day=01&end_time=00%3A00%3A00&min_dep=&max_dep=&min_mag=&max_mag=&req_mag_type=&req_mag_agcy=&req_fm_agcy=Any&include_links=on")
  download.file(f.url, f.name)


}

  if (update) for ( i in  lubridate::year(Sys.Date())){

    f.name= paste0("~/data/global/quakes/isc-fms/fms",i,".csv" )
    f.url=paste0("http://www.isc.ac.uk/cgi-bin/web-db-v4?request=COMPREHENSIVE&out_format=FMCSV&bot_lat=&top_lat=&left_lon=&right_lon=&ctr_lat=&ctr_lon=&radius=&max_dist_units=deg&searchshape=GLOBAL&srn=&grn=&start_year=",i,"&start_month=1&start_day=01&start_time=00%3A00%3A00&end_year=",i,"&end_month=12&end_day=01&end_time=00%3A00%3A00&min_dep=&max_dep=&min_mag=&max_mag=&req_mag_type=&req_mag_agcy=&req_fm_agcy=Any&include_links=on")
    download.file(f.url, f.name)


  }

library(magrittr)
list.files("~/data/global/quakes/isc-fms/", pattern="*.csv",  full.names=T)   -> csv.f

# note taht isc-fms mechansis for < 1964 hav different formatting
csv.f<- csv.f[!(csv.f %>% stringr::str_detect(c("1963|1962|1961|1960")))]
# purrr::map_df(csv.f, rio::import,format = ",") -> isc.fms
# # purrr::map_df(csv.f, readr::read_csv, skip=31) -> isc.fms


purrr::map_df( csv.f, data.table::fread, check.names=T)  -> isc.fms
#stringr::str_extract(isc.fms[1:10,1][[1]]  ,"[0-9]{7}")


isc.fms[ ,1] <- stringr::str_extract(isc.fms[,1][[1]],"[0-9]{7}")
#grepl(isc.fms[,1] , "[0-9]{7}")
isc.fms[,2] <-stringr::str_extract(isc.fms[,2][[1]],"[A-Z,_]{3,10}")
#isc.fms1<-isc.fms[,1:34]
isc.fms$DATE <- lubridate::ymd_hms(paste(isc.fms$DATE,isc.fms$TIME))
isc.fms <- isc.fms %>% dplyr::select( -TIME,-AUTHOR.1)
names(isc.fms)<- stringr::str_to_lower(names(isc.fms)) %>% stringr::str_remove_all("_") %>%   stringr::str_remove_all(",")
names(isc.fms)[25:33] <- stringr::str_trunc(names(isc.fms)[25:33], 2, ellipsis="")

isc.fms
}
