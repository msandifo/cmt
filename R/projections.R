#' Title
#'
#' @param df 
#' @param axis 
#' @param scale 
#' @param bearing 
#' @param profile 
#' @param test 
#'
#' @return
#' @export
#'
#' @examples
project_axes <-function(df, axis="T", scale=10, bearing=18, profile="1", test=F){
  if(test){df<-pro.ppp
  axis<- "T"
  bearing=18
  scale=10
  profile="1"
  }
  if (axis=="T"){ azi= df$marks$ta; plunge=df$marks$tp;
  if (is.na(bearing)) bearing= df$marks$bearing
  }
  if (axis=="P"){ azi= df$marks$pa; plunge=df$marks$pp;  if (is.na(bearing)) bearing= df$marks$bearing }
  if (axis=="B"){ azi= df$marks$ba; plunge=df$marks$bp;  if (is.na(bearing)) bearing= df$marks$bearing }
  
  pro.bearing <- (azi - bearing)
  pro.bearing[pro.bearing>180] <-  pro.bearing[pro.bearing>180]-360
  ends.xy.plan <-  cos((pro.bearing)* pi/180)
  ends.xy <- -scale* cos(plunge* pi/180) *  ends.xy.plan
  ends.z <- scale* sin(plunge* pi/180)
  # pro.sign <- 0 * pro.bearing +1
  # pro.sign[abs(pro.bearing) < 90] <- -1
  #print(data.frame(azi, pro.bearing, azi-pro.bearing, pro.sign))
  # if (abs(pro.bearing) > 90) ends.z <- -ends.z
  data.frame(x=df$marks$distance+ ends.xy  ,
             xend= df$marks$distance-ends.xy ,
             y= df$marks$depth-(ends.z ),
             yend= df$marks$depth+(ends.z ),
             regime=df$marks$regime,
             pro.bearing=pro.bearing,
             profile=profile,
             axis= axis)
}


#' Title
#'
#' @param my.ppp 
#' @param win 
#' @param plates 
#' @param profile 
#' @param add 
#' @param test 
#'
#' @return
#' @export
#'
#' @examples
project_ppp<- function(my.ppp,
                       win=pwin(my.ppp),
                       plates=bird.plates.psp,
                       profile="p0",
                       add=F, test=F){
  
  if (test) { my.ppp=isc.ppp[p2]
  win=p2
  plates=bird.plates.psp
  profile="p0"
  add=F
  }
  #cmt.my.ppp <- cmt.my.ppp %>% subset(dplyr::distinct(cmt.my.ppp$marks$eventname, .keep_all=T))
  if (!add){
    my.ppp$marks$profile <-NA
    my.ppp$marks$bearing <-NA
    my.ppp$marks$pro.lat <-NA
    my.ppp$marks$pro.long <-NA
    my.ppp$marks$distance <-NA
  }
  my.ppp[win]$marks$profile <- profile
  my.ppp.proj <-spatstat::project2segment(my.ppp[ win], plates[win])
  my.ppp[win]$marks$pro.long <-my.ppp.proj$Xproj$x
  my.ppp[win]$marks$pro.lat <- my.ppp.proj$Xproj$y
  my.ppp[win]$marks$bearing <- geosphere::bearing( my.ppp[win]$marks[c("pro.long", "pro.lat")] %>% as.matrix, my.ppp[win]$marks[c("long", "lat")] %>% as.matrix)
  my.ppp[win]$marks$distance <-geosphere::distGeo(my.ppp[win] %>% 
                                                    as.SpatialPoints.ppp(),  my.ppp.proj$Xproj %>% 
                                                    as.SpatialPoints.ppp() )/1000
  my.ppp[win]$marks$plates <- split_ppp(my.ppp[win]$marks)
  my.ppp[win]$marks$distance[my.ppp[win]$marks$plates=="0"] <- 
    (-my.ppp[win]$marks$distance)
  
  #print(mean)
  #  plot(my.ppp[win]$marks$distance, my.ppp[win]$marks$bearing)
  
  return(my.ppp)
}



#' returns a poly as an owin (default) or as list of x (longs) and y (lats)
#'
#' @param long reference longtitude
#' @param lat  reference latitude
#' @param length  length in degrees
#' @param width  width in degrees
#' @param angle  angle of poly central axis 
#' @param owin  boolean (default= TRUE) to retuen owin or list
#' @param clockwise  boolean (default=FALSE) reverse directions  allows ? holes
#' @param left 
#' @param right 
#' @param top 
#' @param bottom 
#'
#' @return
#' @export
#'
#' @examples
get_poly <-function(long=-100, 
                    lat=15, 
                    length=5, 
                    width=2, 
                    angle=15, 
                    owin=T,
                    clockwise=F, 
                    left=1, 
                    right=1, 
                    top=1, 
                    bottom=1 #for holes i
){
  adtor <- angle*pi/180
  long1<- long + length*sin(adtor)
  lat1 <- lat + length*cos(adtor)
  longs = c( long  - right*bottom*width/2*cos(adtor), 
             long  + left*bottom*width/2*cos(adtor), 
             long1 + left*top*width/2*cos(adtor),
             long1 - right*top*width/2*cos(adtor))
  
  lats = c( lat  + right*bottom*width/2*sin(adtor), 
            lat  - left*bottom*width/2*sin(adtor), 
            lat1 - left*top*width/2*sin(adtor), 
            lat1 + right*top*width/2*sin(adtor))
  
  if (clockwise) {longs=rev(longs); lats=rev(lats)}
  if (!owin)
    return(list(x=longs, y=lats)) else return(spatstat::owin(poly=list(x=longs, y=lats)))
}
