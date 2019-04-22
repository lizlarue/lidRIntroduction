####################################
###Code to open lidar .las files and create a canopy height model
#Written by Dr. Elizabeth LaRue
#email elizabethannlarue@gmail.com
#4/12/2018
####################################

#first use the packages tab to install the lidR package
library(lidR) #open the lidR package


#if you can't install the lidR package from cran then pull it from GitHub
#library(devtools)
#devtools::install_github("Jean-Romain/lidR")
#require(lidR)


#PART 1
#Creat canopy height model from Indiana aerial LiDAR

#next use the Files tab to set the working directory to the LidarLesson folder
forest <- readLAS("IN.forest.patches.laz", filter = "-drop_z_below 0 -drop_z_above 500") 

#look at the spatial extent and summary of data file
 extent(forest)
 summary(forest)
 
 #plot the 3D point
 #can zoom in or move the plot around to see the 3D point cloud
 plot(forest)
 #What does the color scale represent?
 
 #In order to measure tree height, we have to correct the point cloud for elevation 
 lasnormalize(forest, dtm=NULL, method = "delaunay")
 forest@data$Z[forest@data$Z <= 0] <- NA  #Filters out negative z_vals
  
  
  #creates a matrix of all the data points
  mat <- as.spatial(forest) #creates a SpatialPointsDataFrame w/ 14 columns
  data.xyz <- as.data.frame(mat) #transform sp into a dataframe
  data.xyz <- cbind(data.xyz[, 1], data.xyz[,10:11])
  colnames(data.xyz) <- c("z", "x", "y")
  data.xyz <- data.xyz[complete.cases(data.xyz), ]
  
  #Create a density plot of canopy heights
  plot(density(data.xyz[,1], na.rm = TRUE))
  #At height is there a large peak? Is this likey to be an error? If not what is this?
  

  #create a canopy height model to measure max canopy height in 1 x 1 m grids
  #this is a common way of getting canopy heights in a 2D plot (it can be output as a raster)
  chm <- grid_canopy(forest, res = 1, subcircle = 0, na.rm=TRUE)
  plot(chm)
  #Do you notice anything about the formatation of gaps in the forest canopy? 
  
  #calculate average mean canopy height
  sum(chm[,3],na.rm=TRUE)/nrow(chm[,3])
  

  #ACTIVITY 
  #Use the code provided here to look at the golfcourse.laz file
  #Make a CHM for the golf course and measure the average height of trees
  
  
  #PART 2
  #LiDAR from Terrestrial Laser Scanning unit
  #much higher density than aerial LIDAR
  
  #read in the .las file
  # it might take a few seconds
  tls <- readLAS("20180901_1_1x1_CNB.las")
  tls <- lasroi(tls) #allows you to manually select a section
  plot(tls)