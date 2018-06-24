####################################################################################################
####################################################################################################
## Clip available products for Tunisia
## Contact remi.dannunzio@fao.org 
## 2018/04/17
####################################################################################################
####################################################################################################
time_start  <- Sys.time()

workdir      <-  "/media/dannunzio/OSDisk/Users/dannunzio/Documents/countries/sudan/data/ifn_plots/"
setwd(workdir)

inXS <- read.csv("List_inaccessible_SUs.csv")
allp <- read.csv("plots_all.csv")

allp$LonCor <- allp$Lon

allp[allp$EW == "E2",]$LonCor <-  allp[allp$EW == "E2",]$Lon+20
allp[allp$EW == "E3",]$LonCor <-  allp[allp$EW == "E3",]$Lon+30

all(inXS$SU %in% allp$SU)

joinXS <- merge(inXS,allp,all.x=T,by.x="SU",by.y="SU")
head(joinXS)
spjoinXS <- SpatialPointsDataFrame(coords = joinXS[,c("LonCor","Lat")],data = joinXS)

proj4string(spjoinXS) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"

plot(spjoinXS)

country <- getData('GADM',path="../admin/", country= "SDN", level=2)
plot(country,add=T)

writeOGR(spjoinXS,"points.shp","points","ESRI Shapefile",overwrite_layer = T)

proj <- "+proj=aea +lat_1=20 +lat_2=-23 +lat_0=0 +lon_0=25 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"

sp <- spTransform(spjoinXS,proj)

ysize <- 1000
xsize <- 1000
sp@coords
pts <- data.frame(sp@coords)
xcoord <- "LonCor"
ycoord <- "Lat" 

pts$id <- row(pts)[,1]
names(pts)
lp<-list()

for(i in 1:nrow(sp)){
  ymin <- pts[i,ycoord]
  ymax <- pts[i,ycoord]+ysize
  xmin <- pts[i,xcoord]
  xmax <- pts[i,xcoord]+xsize
  
  p  <- Polygon(cbind(c(xmin,xmin,xmax,xmax,xmin),c(ymin,ymax,ymax,ymin,ymin)))
  ps <- Polygons(list(p), pts[i,"id"])
  lp <- append(lp,list(ps))
}

#################### Transform the list of polygons into a SPDF
spdf<-SpatialPolygonsDataFrame(
  SpatialPolygons(lp,1:nrow(pts)), 
  sp@data,#,xcoord,ycoord)], 
  match.ID = F
)

plot(spdf)

proj4string(spdf) <- proj

operators <- c("naila",
               "elyas",
               "samia",
               "salah",
               "hanadi",
               "manal",
               "safaa",
               "nouh",
               "ameer",
               "mutasim")

spdf@data$operator <- rep(operators,length.out=146)

head(spdf@data)
writeOGR(spdf,"../ifn_plots/plots_1km.shp","plots_1km","ESRI Shapefile",overwrite_layer = T)
writeOGR(spTransform(spdf,proj4string(spjoinXS)),"../ifn_plots/plots_1km_geo.shp","plots_1km_geo","ESRI Shapefile",overwrite_layer = T)
