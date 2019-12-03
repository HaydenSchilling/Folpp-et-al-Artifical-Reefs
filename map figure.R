### Map for Folpp et al.

### Map figure
library(rgdal) 
library(sp)
library(raster)
library(ggplot2)
#install.packages("ggsn")
library(ggsn)

#Load map data
Aus <- readOGR(dsn = "Shape files/australia",layer = "cstauscd_r")
estuaries <- readOGR(dsn = "Shape files/estuaries",layer = "Estuary_water_bodies")
estuaries <- spTransform(estuaries, CRS("+proj=longlat +datum=WGS84"))
plot(estuaries, axes=TRUE)

#summary(estuaries)

estuaries <- subset(estuaries,  Estuary == "Botany Bay" | 
  Estuary == "Lake Macquarie" |
  Estuary == "St. Georges Basin")

head(estuaries)
plot(estuaries)

#plot(Aus)
Aus_coast <- subset(Aus, FEAT_CODE != "sea" )

head(Aus_coast)

#plot(Aus_coast)

min_lon <- 149
max_lon <- 155
min_lat <- -36
max_lat <- -32

geo_bounds <- c(left = min_lon, bottom = min_lat, right = max_lon, top = max_lat)

Sites.grid <- expand.grid(lon_bound = c(geo_bounds[1], geo_bounds[3]), 
                          lat_bound = c(geo_bounds[2], geo_bounds[4]))

coordinates(Sites.grid) <- ~ lon_bound + lat_bound

Aus_crop <- crop(Aus_coast, extent(Sites.grid)) #rgeos must be installed to run



boxes <- read.csv("estuary bounds.csv", header = T)

cols = "red"
shapes = "1"

p1 <- ggplot() + theme_classic() + 
  labs(x=expression(paste("Longitude (",degree, ")", sep="")), y=expression(paste("Latitude (", degree, ")"))) +
  scale_x_continuous(expand = c(0,0), limits = c(149,153)) + scale_y_continuous(expand = c(0,0), limits = c(-36,-32)) +
  #scale_fill_distiller(palette = "Spectral", na.value=NA, trans="log10") +
  coord_quickmap() + #coord_map() + #  # this line could be very slow
  geom_polygon(data = Aus_crop, aes(x=long, y = lat, group = group), fill = "gray60", colour = "gray60")+
  geom_polygon(data = estuaries, aes(x=long, y = lat, group = group), fill = "white", colour = "white")+
  geom_rect(data = boxes, aes(xmin=x_max, xmax=x_min, ymin=y_min, ymax=y_max, group = Estuary), fill = NA, col = "black") +
  theme(axis.title.x = element_text(face="bold", colour="black", size = 18),
        axis.text  = element_text(colour="black", size = 12), 
        axis.title.y = element_text(face="bold", colour="black", size = 18),
        axis.ticks = element_line(colour="black"),
        strip.text = element_text(colour="black", face = "bold", size = 14),
        strip.background = element_rect(colour = NULL),
        legend.justification=c(1,0), legend.position=c(0.92,0.05), legend.direction = "horizontal",
        panel.border = element_rect(colour = "black", fill=NA, size = 1),
        #legend.key.size = unit(1, "cm"),
        #legend.text = element_text(size = 12, face = "bold"),
        legend.title=element_blank(),
        legend.background = element_blank())  +
  #geom_point(data = dots, aes(x = Longitude, y = Latitude*-1, col = cols), size = 2) +
  #scale_shape_manual(values = 1, label ="Larval Fish \nSample") +
  #geom_point(data = NULL, aes(x = 151.258693, y = 33.838509*-1), size = 2, col = "blue") +
  geom_text(data = boxes, aes(x = x_min, y = y_min, label = Estuary), 
            nudge_x = c(0.75,0.55,0.8))# +

p1

ggsave("plots/Figure 1.pdf")
ggsave("plots/Figure 1.png", dpi = 600)


### Online code for inset
##################################
# Creating Inset Maps in ggplot2 #
##################################

library(ggplot2)
library(raster)
library(grid)
library(gridExtra)
# 
# ph0<-getData("GADM", country="PHL", level=0) # download PHL level 0 map for ucdavis site
# phl<-getData("GADM", country="PHL", level=2) # download PHL level 2 map for ucdavis site
# mrdq<-(phl[phl$NAME_1=="Marinduque",]) # subset province of Marinduque from PHL map
# munnames<-coordinates(mrdq) # get center coordinates of municipalities of Marinduque
# munnames<-data.frame(munnames) # convert matrix format munnames object to data.frame
# munnames$label<-mrdq@data$NAME_2

# Extent rectangle for inset map
pol<-data.frame(xmin=149,xmax=153 ,ymin=-36 ,ymax=-32)


# Main Map
# p1<-ggplot()+geom_polygon(data=mrdq, aes(long+0.008,lat-0.005, group=group), fill="#9ecae1")+
#   geom_polygon(data=mrdq, aes(long,lat, group=group), colour="grey10",fill="#fff7bc")+
#   geom_text(data=munnames, aes(x=X1, y=X2,label=label), size=3, colour="grey20")+
#   coord_equal()+theme_bw()+xlab("")+ylab("")+
#   scale_x_continuous(breaks=seq(121.8,122.2, 0.1), labels=c(paste(seq(121.8,122.2, 0.1),"Â°E", sep="")))+
#   scale_y_continuous(breaks=seq(13.2,13.6, 0.1), labels=c(paste(seq(13.2,13.6, 0.1),"Â°N", sep="")))+
#   theme(axis.text.y =element_text(angle = 90, hjust=0.5))

#Inset
p2<-ggplot()+geom_polygon(data=Aus_coast, aes(x=long, y = lat, group = group), fill = "gray60", colour = "gray60") +
  coord_quickmap()+theme_bw()+labs(x=NULL,y=NULL) + 
  #scale_x_continuous(breaks=seq(117.5,125, 2.5), labels=c(paste(seq(117.5,125, 2.5),"Â°E", sep="")))+
  #scale_y_continuous(breaks=seq(5,20, 5), labels=c(paste(seq(5,20, 5),"Â°N", sep="")))+
  geom_rect(data = pol, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), alpha=0, colour="red", size = 1, linetype=1)+
  theme(axis.text.x =element_blank(),axis.text.y= element_blank(), axis.ticks=element_blank(),axis.title.x =element_blank(),
        axis.title.y= element_blank(), plot.background = element_rect(fill = "transparent", color = NA)
  ) 
p2

#ggsave("test.pdf")

png(file="Figure 1.png",w=1800,h=1800, res=300)
grid.newpage()
v1<-viewport(width = 1, height = 1, x = 0.5, y = 0.5) #plot area for the main map
v2<-viewport(width = 0.3, height = 0.35, x = 0.32, y = 0.855, clip = "off") #plot area for the inset map
print(p1,vp=v1) 
print(p2,vp=v2)
dev.off()



### Now to make the individual estuary maps
locations <- read.csv("Reef Locations.csv", header = T)


# Botany Bay
Bb <- subset(estuaries, Estuary == "Botany Bay")
Bb_reefs <- subset(locations, Estuary == "Botany Bay")

pBB <- ggplot(Bb) + theme_classic() + 
  labs(x=expression(paste("Longitude (",degree, ")", sep="")), y=expression(paste("Latitude (", degree, ")"))) +
  scale_x_continuous(expand = c(0,0), limits = c(149,153)) + scale_y_continuous(expand = c(0,0), limits = c(-36,-32)) +
  #scale_fill_distiller(palette = "Spectral", na.value=NA, trans="log10") +
  coord_quickmap(xlim = c(boxes$x_min[2], boxes$x_max[2]),
                 ylim = c(boxes$y_min[2], boxes$y_max[2])) + #coord_map() + #  # this line could be very slow
  geom_polygon(data = Aus_crop, aes(x=long, y = lat, group = group), fill = "gray60", colour = "gray60")+
  geom_polygon(aes(x=long, y = lat, group = group), fill = "white", colour = "white")+
  geom_rect(data = boxes, aes(xmin=x_max, xmax=x_min, ymin=y_min, ymax=y_max, group = Estuary), fill = NA, col = "black") +
  #facet_wrap(~Estuary)+
  theme(axis.title.x = element_text(face="bold", colour="black", size = 18),
        axis.text  = element_text(colour="black", size = 12), 
        axis.title.y = element_text(face="bold", colour="black", size = 18),
        axis.ticks = element_line(colour="black"),
        strip.text = element_text(colour="black", face = "bold", size = 14),
        strip.background = element_rect(colour = NULL),
        legend.justification=c(1,0), legend.position="right", legend.direction = "vertical",
        panel.border = element_rect(colour = "black", fill=NA, size = 1),
        #legend.key.size = unit(1, "cm"),
        #legend.text = element_text(size = 12, face = "bold"),
        legend.title=element_blank(),
        legend.background = element_blank())  +
  scalebar(NULL, x.min =boxes$x_min[3], x.max = boxes$x_max[3], y.min = boxes$y_min[3],
           y.max = boxes$y_max[3], dist = 1, dist_unit = "km",
           transform = T, model = "WGS84", st.bottom = FALSE,
           anchor = c("x" = 151.2,"y"= -34.03)) +
  #geom_point(data = dots, aes(x = Longitude, y = Latitude*-1, col = cols), size = 2) +
  #scale_shape_manual(values = 1, label ="Larval Fish \nSample") +
  geom_point(data = Bb_reefs, aes(x = Long, y = Lat, shape = Type), size = 1.5) #+
  #geom_text(data = boxes, aes(x = x_min, y = y_min, label = Estuary), 
  #          nudge_x = c(0.75,0.55,0.8))# +

pBB


# Lake Macquarie
LM <- subset(estuaries, Estuary == "Lake Macquarie")
LM_reefs <- subset(locations, Estuary == "Lake Macquarie")

pLM <- ggplot(LM) + theme_classic() + 
  labs(x=expression(paste("Longitude (",degree, ")", sep="")), y=expression(paste("Latitude (", degree, ")"))) +
  scale_x_continuous(expand = c(0,0), limits = c(149,153)) + scale_y_continuous(expand = c(0,0), limits = c(-36,-32)) +
  #scale_fill_distiller(palette = "Spectral", na.value=NA, trans="log10") +
  coord_quickmap(xlim = c(boxes$x_min[1], boxes$x_max[1]),
                 ylim = c(boxes$y_min[1], boxes$y_max[1])) + #coord_map() + #  # this line could be very slow
  geom_polygon(data = Aus_crop, aes(x=long, y = lat, group = group), fill = "gray60", colour = "gray60")+
  geom_polygon(aes(x=long, y = lat, group = group), fill = "white", colour = "white")+
  geom_rect(data = boxes, aes(xmin=x_max, xmax=x_min, ymin=y_min, ymax=y_max, group = Estuary), fill = NA, col = "black") +
  #facet_wrap(~Estuary)+
  theme(axis.title.x = element_text(face="bold", colour="black", size = 18),
        axis.text  = element_text(colour="black", size = 12), 
        axis.title.y = element_text(face="bold", colour="black", size = 18),
        axis.ticks = element_line(colour="black"),
        strip.text = element_text(colour="black", face = "bold", size = 14),
        strip.background = element_rect(colour = NULL),
        legend.justification=c(1,0), legend.position="right", legend.direction = "vertical",
        panel.border = element_rect(colour = "black", fill=NA, size = 1),
        #legend.key.size = unit(1, "cm"),
        #legend.text = element_text(size = 12, face = "bold"),
        legend.title=element_blank(),
        legend.background = element_blank())  +
  scalebar(NULL, x.min =boxes$x_min[3], x.max = boxes$x_max[3], y.min = boxes$y_min[3],
           y.max = boxes$y_max[3], dist = 1, dist_unit = "km",
           transform = T, model = "WGS84", st.bottom = FALSE,
           anchor = c("x" = 151.65,"y"= -33.14)) +
  #geom_point(data = dots, aes(x = Longitude, y = Latitude*-1, col = cols), size = 2) +
  #scale_shape_manual(values = 1, label ="Larval Fish \nSample") +
  geom_point(data = LM_reefs, aes(x = Long, y = Lat, col = Type), size = 1.5) #+
#geom_text(data = boxes, aes(x = x_min, y = y_min, label = Estuary), 
#          nudge_x = c(0.75,0.55,0.8))# +

pLM



# St Georges Basin
SBG <- subset(estuaries, Estuary == "St. Georges Basin")
SBG_reefs <- subset(locations, Estuary == "St Georges Basin")

pSGB <- ggplot(SBG) + theme_classic() + 
  labs(x=expression(paste("Longitude (",degree, ")", sep="")), y=expression(paste("Latitude (", degree, ")"))) +
  scale_x_continuous(expand = c(0,0), limits = c(149,153)) + scale_y_continuous(expand = c(0,0), limits = c(-36,-32)) +
  #scale_fill_distiller(palette = "Spectral", na.value=NA, trans="log10") +
  coord_quickmap(xlim = c(boxes$x_min[3], boxes$x_max[3]),
                 ylim = c(boxes$y_min[3], boxes$y_max[3])) + #coord_map() + #  # this line could be very slow
  geom_polygon(data = Aus_crop, aes(x=long, y = lat, group = group), fill = "gray60", colour = "gray60")+
  geom_polygon(aes(x=long, y = lat, group = group), fill = "white", colour = "white")+
  geom_rect(data = boxes, aes(xmin=x_max, xmax=x_min, ymin=y_min, ymax=y_max, group = Estuary), fill = NA, col = "black") +
  #facet_wrap(~Estuary)+
  theme(axis.title.x = element_text(face="bold", colour="black", size = 18),
        axis.text  = element_text(colour="black", size = 12), 
        axis.title.y = element_text(face="bold", colour="black", size = 18),
        axis.ticks = element_line(colour="black"),
        strip.text = element_text(colour="black", face = "bold", size = 14),
        strip.background = element_rect(colour = NULL),
        legend.justification=c(1,0), legend.position="right", legend.direction = "vertical",
        panel.border = element_rect(colour = "black", fill=NA, size = 1),
        #legend.key.size = unit(1, "cm"),
        #legend.text = element_text(size = 12, face = "bold"),
        legend.title=element_blank(),
        legend.background = element_blank())  +
  scalebar(NULL, x.min =boxes$x_min[3], x.max = boxes$x_max[3], y.min = boxes$y_min[3],
          y.max = boxes$y_max[3], dist = 1, dist_unit = "km",
          transform = T, model = "WGS84", st.bottom = FALSE,
          anchor = c("x" = 150.64,"y"= -35.18)) +
  #geom_point(data = dots, aes(x = Longitude, y = Latitude*-1, col = cols), size = 2) +
  #scale_shape_manual(values = 1, label ="Larval Fish \nSample") +
  geom_point(data = SBG_reefs, aes(x = Long, y = Lat, shape = Type), size = 1.5) #+
#geom_text(data = boxes, aes(x = x_min, y = y_min, label = Estuary), 
#          nudge_x = c(0.75,0.55,0.8))# +

pSGB
