####################################################################################################
####################################################################################################
## Spatialize NFI tree information
## Contact remi.dannunzio@fao.org 
## 2018/04/17
####################################################################################################
####################################################################################################
time_start  <- Sys.time()

### Set working directory
workdir      <-  "/media/dannunzio/OSDisk/Users/dannunzio/Documents/countries/sudan/data/nfi_plots/"
setwd(workdir)

### Read results from Collect
nfi <- read.csv("collect-csv-data-export-sudan_nfi_2017-2018-06-25T14_28_53/tree.csv")

### Read plot coordinates as measured in field
df <- read.csv("collect-csv-data-export-sudan_nfi_2017-2018-06-25T14_28_53/plot_corr.csv")

### See how many trees are measured per SU
(trees_measured <- table(nfi$su_su_no))
plot(trees_measured)

### Choose 3 SU with different tree numbers
sel <- c(783,952,615)

### CHECK & CLEAN
names(nfi)
summary(nfi$tree_axis_distance)
summary(nfi$tree_right_axis)
summary(nfi$tree_left_axis)

table(nfi$tree_axis_distance)

nfi[!is.na(nfi$tree_right_axis) & nfi$tree_right_axis > 10,"tree_axis_distance"] <- NA
nfi[!is.na(nfi$tree_left_axis)  & nfi$tree_left_axis  > 10,"tree_axis_distance"] <- NA

nfi[is.na(nfi$tree_axis_distance),"tree_right_axis"] <- NA
nfi[is.na(nfi$tree_axis_distance),"tree_left_axis"] <- NA

### PUT NA.VALUES OF RIGHT & LEFT COORDINATES TO ZERO
nfi[!is.na(nfi$tree_axis_distance) & is.na(nfi$tree_right_axis),"tree_right_axis"] <- 0
nfi[!is.na(nfi$tree_axis_distance) & is.na(nfi$tree_left_axis), "tree_left_axis" ] <- 0

### CHECK IF COLLECTED AND PLANNED CORRESPOND
names(df)
plot(df$plot_access_plot_location_given_x,df$plot_access_plot_starting_position_x)
plot(df$plot_access_plot_location_given_y,df$plot_access_plot_starting_position_y)

### Initialize For each tree, starting point coordinates
nfi$x_head  <- 0
nfi$y_head  <- 0

### Initialize For each tree, coordinates of tree
nfi$x_utm  <- 0
nfi$y_utm  <- 0

### Initialize For each tree, UTM zone 
nfi$srs    <- "none"
i<-1

### Loop through all trees of the NFI
for(i in 1:nrow(nfi)){
  
  ### Which SU and which subplot
  subplot <- nfi[i,"plot_plot_no"]
  su      <- nfi[i,"su_su_no"]
  
  tryCatch({
    ### Grab the corresponding specifications
    dfi     <- df[df$su_su_no == su & df$plot_no == subplot,]
    
    ### Fill in the starting point and SRS
    nfi[i,]$x_head <- dfi$plot_access_plot_starting_position_x
    nfi[i,]$y_head <- dfi$plot_access_plot_starting_position_y
    nfi[i,]$srs    <- dfi$plot_access_plot_starting_position_srs
    
    ### Compute tree coordinates for subplot 1
    if(subplot == 1){
      nfi[i,]$x_utm <- nfi[i,]$x_head + nfi[i,]$tree_right_axis - nfi[i,]$tree_left_axis
      nfi[i,]$y_utm <- nfi[i,]$y_head + nfi[i,]$tree_axis_distance 
    }
    
    ### Compute tree coordinates for subplot 2
    if(subplot == 2){
      nfi[i,]$x_utm <- nfi[i,]$x_head + nfi[i,]$tree_axis_distance 
      nfi[i,]$y_utm <- nfi[i,]$y_head - nfi[i,]$tree_right_axis + nfi[i,]$tree_left_axis
    }
    
    ### Compute tree coordinates for subplot 3
    if(subplot == 3){
      nfi[i,]$x_utm <- nfi[i,]$x_head - nfi[i,]$tree_right_axis + nfi[i,]$tree_left_axis
      nfi[i,]$y_utm <- nfi[i,]$y_head - nfi[i,]$tree_axis_distance 
    }
    
    ### Compute tree coordinates for subplot 4
    if(subplot == 4){
      nfi[i,]$x_utm <- nfi[i,]$x_head - nfi[i,]$tree_axis_distance
      nfi[i,]$y_utm <- nfi[i,]$y_head + nfi[i,]$tree_right_axis - nfi[i,]$tree_left_axis 
    }
    
  },error=function(e){print(paste0("no plot"))})
}

### Check
summary(nfi$x_utm)
table(nfi$srs)

### Restrict to trees with coordinates
nfi <- nfi[!is.na(nfi$tree_axis_distance),]

write.csv(nfi,"trees_nfi_20180626.csv",row.names = F)

nfi2 <- nfi[,c("su_su_no","plot_plot_no",
               "tree_lucs","tree_or_stump","tree_species_scientific_name",
               "diameter","tree_dbh","tree_height","tree_bole_height","crown_condition","x_utm","y_utm","srs")]

names(nfi2) <- c("SU","plot",
                 "lucs","tr_st","specie",
                 "diam","dbh","height","bolet","crown","x_utm","y_utm","srs")
### Separate by UTM zone
nfi_35 <- nfi2[nfi2$srs == "EPSG:32635",]
nfi_34 <- nfi2[nfi2$srs == "EPSG:32634",]
nfi_36 <- nfi2[nfi2$srs == "EPSG:32636",]

#################### Transform the list of points into a SPDF - UTM 34
spdf_34 <- SpatialPointsDataFrame(coords = nfi_34[,c("x_utm","y_utm")],
                                  data = nfi_34,
                                  proj4string = CRS("+init=epsg:32634")
)

#################### Transform the list of points into a SPDF - UTM 35
spdf_35 <- SpatialPointsDataFrame(coords = nfi_35[,c("x_utm","y_utm")],
                                  data = nfi_35,
                                  proj4string = CRS("+init=epsg:32635")
)

#################### Transform the list of points into a SPDF - UTM 36
spdf_36 <- SpatialPointsDataFrame(coords = nfi_36[,c("x_utm","y_utm")],
                                  data = nfi_36,
                                  proj4string = CRS("+init=epsg:32636")
)

### Reproject in a common system
proj_aea <- "+proj=aea +lat_1=20 +lat_2=-23 +lat_0=0 +lon_0=25 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"

aea_34 <- spTransform(spdf_34,proj_aea)
aea_35 <- spTransform(spdf_35,proj_aea)
aea_36 <- spTransform(spdf_36,proj_aea)

### Merge all
aea <- SpatialPointsDataFrame(coords = rbind(aea_34@coords,aea_35@coords,aea_36@coords),
                              data = rbind(aea_34@data,aea_35@data,aea_36@data),
                              proj4string = CRS(proj_aea))

### Export as Shapefile
writeOGR(aea,"trees_aea_20180625.shp","trees_aea_20180625","ESRI Shapefile",overwrite_layer = T)

nrow(nfi)