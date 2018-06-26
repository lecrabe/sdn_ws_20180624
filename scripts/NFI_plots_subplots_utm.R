####################################################################################################
####################################################################################################
## Clip available products for Tunisia
## Contact remi.dannunzio@fao.org 
## 2018/04/17
####################################################################################################
####################################################################################################
time_start  <- Sys.time()

### Set working directory
workdir      <-  "/media/dannunzio/OSDisk/Users/dannunzio/Documents/countries/sudan/data/ifn_plots/"
setwd(workdir)

### Read data tables of existing plots
allp <- read.csv("plots_utm.csv")

proj_geo <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
proj_aea <- "+proj=aea +lat_1=20 +lat_2=-23 +lat_0=0 +lon_0=25 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"

ysize <- 1000
xsize <- 1000

table(allp$utm_n,allp$utm_z)

for(zone in 34:37){
  all <- allp[allp$utm_n == zone,]
  
  lp<-list()
  
  for(i in 1:nrow(all)){
    all_i <- all[i,]
    
    ymin <- all[i,]$y0
    ymax <- all[i,]$y0+ysize
    xmin <- all[i,]$x0
    xmax <- all[i,]$x0+xsize
    su_out  <- Polygon(cbind(c(xmin,xmin,xmax,xmax,xmin),c(ymin,ymax,ymax,ymin,ymin)))
    
    ymin <- all[i,]$y1
    ymax <- all[i,]$y1+250
    xmin <- all[i,]$x1-10
    xmax <- all[i,]$x1+10
    su_p1   <- Polygon(cbind(c(xmin,xmin,xmax,xmax,xmin),c(ymin,ymax,ymax,ymin,ymin)))
    
    
    ymin <- all[i,]$y2-10
    ymax <- all[i,]$y2+10
    xmin <- all[i,]$x2
    xmax <- all[i,]$x2+250
    su_p2   <- Polygon(cbind(c(xmin,xmin,xmax,xmax,xmin),c(ymin,ymax,ymax,ymin,ymin)))
    
    ymin <- all[i,]$y3
    ymax <- all[i,]$y3-250
    xmin <- all[i,]$x3-10
    xmax <- all[i,]$x3+10
    su_p3   <- Polygon(cbind(c(xmin,xmin,xmax,xmax,xmin),c(ymin,ymax,ymax,ymin,ymin)))
    
    ymin <- all[i,]$y4-10
    ymax <- all[i,]$y4+10
    xmin <- all[i,]$x4
    xmax <- all[i,]$x4-250
    su_p4   <- Polygon(cbind(c(xmin,xmin,xmax,xmax,xmin),c(ymin,ymax,ymax,ymin,ymin)))
    
    ps_0 <- Polygons(list(su_out,su_p1,su_p2,su_p3,su_p4),paste0(i,"_",0))
    ps_1 <- Polygons(list(su_p1), paste0(i,"_",1))
    ps_2 <- Polygons(list(su_p2), paste0(i,"_",2))
    ps_3 <- Polygons(list(su_p3), paste0(i,"_",3))
    ps_4 <- Polygons(list(su_p4), paste0(i,"_",4))
    
    lp <- append(lp,list(ps_0,ps_1,ps_2,ps_3,ps_4))
  }
  
  length(lp)
  
  df <- all[,c("su","Strata")]
  
  data<- df[rep(seq_len(nrow(df)), each=5),]
  
  data$plot <- rep(0:4,nrow(all))
  
  #################### Transform the list of polygons into a SPDF
  spdf<-SpatialPolygonsDataFrame(
    SpatialPolygons(lp,1:length(lp)), 
    data,#,xcoord,ycoord)], 
    match.ID = F
  )
  
  #plot(spdf)
  proj4string(spdf) <- CRS(paste0("+init=epsg:326",zone))
  spdf <- spTransform(spdf,proj_aea)
  assign(paste0("spdf_",zone),spdf)
}

out <- union(spdf_34,spdf_35)
out <- union(out,spdf_36)
out <- union(out,spdf_37)

writeOGR(out,"plots_subplots_utm.shp","plots_subplots_utm","ESRI Shapefile",overwrite_layer = T)


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
