library(magrittr)
library(ggplot2)
library(cmt)
depth.cols<-RColorBrewer::brewer.pal(11,"Spectral")[c(1,2,4, 9:11)]
plunge.cols <-   RColorBrewer::brewer.pal(11,"Spectral")[c(1,2,4,5, 9:11)]
world <- map_data("world")
tight=2
projection="mollweide"


filter_long(c(143,155)) %>%
  filter_lat(c(-15,5),.)   %>%
  filter_depth(c(75, 350),.) ->
  data;

data %>% cmt_to_ppp() %>%
  vec_lines(  scale=0.4,  segment=T, norm=T, ax="P")  ->
  dat.ll
dat<- cbind(data, dat.ll)
ldat<-length(dat$lat);
long.range=minmax(dat$long,  tight)
lat.range=minmax(dat$lat, tight)


ggplot(dat, aes(long, lat)) +
  geom_polygon(data=world, aes( group = group), fill="grey90")+
  labs(title=paste(ldat,"gCMT intermediate to deep (>75 km) events, P-axis"),x=NULL, y=NULL)+
  theme_minimal()+
  theme(legend.position = c(.8,.15),   legend.direction = "vertical", legend.box = "horizontal")+
  scale_size(range=c(.3,4) )+
  # geom_segment(data=dat.ll, aes(x=x,xend=xend ,y=y,yend=yend), colour="white", lwd=1.15 , lineend="round" ,arrow = arrow(length = unit(0.01, "npc")))+
  geom_segment(  aes(x=x,xend=xend ,y=y,yend=yend), colour="black", lwd=.75 , lineend="round" ,arrow = arrow(length = unit(0.01, "npc")))+
  geom_segment(  aes(x=x,xend=xend ,y=y,yend=yend, colour=depth), lwd=.65 , lineend="round" ,arrow = arrow(length = unit(0.01, "npc")))+
  scale_colour_gradientn(colours=plunge.cols, limits=c(0,90))+
 # ggnewscale::new_scale_color() +
  geom_point( colour="white", aes(size=Mw))+
  geom_point( aes(colour=depth,size=Mw*.85))+
  scale_colour_gradientn(colours=plunge.cols )+
  coord_map(projection=projection, xlim=long.range, ylim=lat.range)

    # geom_segment(  aes(x=x,xend=xend ,y=y,yend=yend, colour=depth), lwd=.3 ,
    #            lineend="round" ,arrow = arrow(length = unit(0.01, "npc")))


ggsave("example/newbrit.pdf", width=15, height=9)

tight=4

filter_long(c(123,147)) %>%
  filter_lat(c(25,50),.)   %>%
  filter_depth(c(75, 750),.) ->
  data;

data %>% cmt_to_ppp() %>%
  vec_lines(  scale=1.5,  segment=T, norm=T, ax="P")  ->
  dat.ll
dat<- cbind(data, dat.ll)
ldat<-length(dat$lat);
long.range=minmax(dat$long,  -tight)
lat.range=minmax(dat$lat, -tight)

save(dat, file="example/kor_int_deep.RDS")
ggplot(dat, aes(long, lat)) +
  geom_polygon(data=world, aes( group = group), fill="grey85", col="grey70")+
  labs(title=paste(ldat,"gCMT intermediate to deep (>75 km) events, P-axis"),x=NULL, y=NULL)+
  theme_minimal()+
  theme(legend.position = c(.45,.1),   legend.direction = "vertical", legend.box = "horizontal")+
  scale_size(range=c(.3,4) )+
  # geom_segment(data=dat.ll, aes(x=x,xend=xend ,y=y,yend=yend), colour="white", lwd=1.15 , lineend="round" ,arrow = arrow(length = unit(0.01, "npc")))+
  geom_segment(  aes(x=x,xend=xend ,y=y,yend=yend), colour="black", lwd=.75 , lineend="round" ,arrow = arrow(length = unit(0.01, "npc")))+
  geom_segment(  aes(x=x,xend=xend ,y=y,yend=yend, colour=depth), lwd=.65 , lineend="round" ,arrow = arrow(length = unit(0.01, "npc")))+
  scale_colour_gradientn(colours=plunge.cols )+
  # ggnewscale::new_scale_color() +
  geom_point( colour="white", aes(size=Mw))+
  geom_point( aes(colour=depth,size=Mw*.85))+
  scale_colour_gradientn(colours=plunge.cols )+
  coord_map(projection=projection, xlim=long.range, ylim=lat.range)

# theme_minimal()+
#   theme(legend.position = c(.1,.85)) +
#   scale_size(range=c(.3,4)*.1)+
#   # geom_segment(data=dat.ll, aes(x=x,xend=xend ,y=y,yend=yend), colour="white", lwd=1.15 , lineend="round" ,arrow = arrow(length = unit(0.01, "npc")))+
#   geom_segment(  aes(x=x,xend=xend ,y=y,yend=yend), colour="black", lwd=.75 , lineend="round" ,arrow = arrow(length = unit(0.01, "npc")))+
#   geom_segment(  aes(x=x,xend=xend ,y=y,yend=yend, colour=depth), lwd=.65 , lineend="round" ,arrow = arrow(length = unit(0.01, "npc")))+
#   scale_colour_gradientn(colours=plunge.cols )+
#   geom_point( aes(colour=depth))+
#   xlim(long.range)+
#   ylim(lat.range)+
#   coord_map(projection=projection)

# geom_segment(  aes(x=x,xend=xend ,y=y,yend=yend, colour=depth), lwd=.3 ,
#            lineend="round" ,arrow = arrow(length = unit(0.01, "npc")))


ggsave("example/kor.pdf", width=10, height=12.5)

tight=4
filter_long(c(123,147)) %>%
  filter_lat(c(25,50),.)   %>%
  filter_depth(c(0, 76),.) ->
  data;

data %>% cmt_to_ppp() %>%
  vec_lines(  scale=1.,  segment=T, norm=T, ax="P")  ->
  dat.ll
dat<- cbind(data, dat.ll)
ldat<-length(dat$lat);
long.range=minmax(dat$long,  -tight)
lat.range=minmax(dat$lat, -tight)
save(dat, file="example/kor_shallow.RDS")

ggplot(dat, aes(long, lat)) +
  geom_polygon(data=world, aes( group = group), fill="grey85", colour="grey70", alpha=.3)+
  labs(title=paste(ldat,"gCMT shallow (<=75 km) events, P-axis"),x=NULL, y=NULL)+
  theme_minimal()+
  theme(legend.position = c(.55,.1),   legend.direction = "vertical", legend.box = "horizontal")+
  scale_size(range=c(.3,4) )+
  # geom_segment(data=dat.ll, aes(x=x,xend=xend ,y=y,yend=yend), colour="white", lwd=1.15 , lineend="round" ,arrow = arrow(length = unit(0.01, "npc")))+
  geom_segment(  aes(x=x,xend=xend ,y=y,yend=yend), colour="black", lwd=.75 , lineend="round" ,arrow = arrow(length = unit(0.01, "npc")))+
  geom_segment(  aes(x=x,xend=xend ,y=y,yend=yend, colour=depth), lwd=.65 , lineend="round" ,arrow = arrow(length = unit(0.01, "npc")))+
  scale_colour_gradientn(colours=plunge.cols, limits=c(0,90))+
  # ggnewscale::new_scale_color() +
  geom_point( colour="white", aes(size=Mw))+
  geom_point( aes(colour=depth,size=Mw*.85))+
  scale_colour_gradientn(colours=plunge.cols )+
  coord_map(projection=projection, xlim=long.range, ylim=lat.range)
# geom_segment(  aes(x=x,xend=xend ,y=y,yend=yend, colour=depth), lwd=.3 ,
#            lineend="round" ,arrow = arrow(length = unit(0.01, "npc")))


ggsave("example/kora.pdf", width=10, height=12.5)

