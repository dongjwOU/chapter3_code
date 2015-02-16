####      rf_classify.R
# Preamble ----------------------------------------------------------------


####Origin and info
#This script reads training data from the CSV file created using the "percentCoverResample.R 
# script.  The script then uses the X and Y coordinates from the training data file to select
# the pixel values (predictor values) for each sample point in the input image. The predictor 
# values and the percent cover data from the training data file (response variable) are 
# combined and used as input to the random forests model. After the model is created percent 
# cover predictions are made on the input image to create an output image with percent cover 
# values ranging from 0 to 1. 
# 
# This script was written by Ned Horning [horning@amnh.org]
# Support for writing and maintaining this script comes from The John D. and 
# Catherine T. MacArthur Foundation.
#
# This script is free software; you can redistribute it and/or modify it under the 
# terms of the GNU General Public License as published by the Free Software Foundation
# either version 2 of the License, or ( at your option ) any later version.      
#
#
####  Packages 
#Load libraries
require(maptools)
require(sp)
require(randomForest)
require(raster)
require(rgdal)

# Inputs ------------------------------------------------------------------
# The CSV file containing X, Y, and percent cover point data created by the rf_csv_create.R script.
pointData <- 'D:\\Chapters\\KNP Data\\Shrub_cover_test\\outSamples2.csv'
# Name and path for the input satellite image 
inImage <-'D:\\Research\\Chapters\\KNP Data\\Shrub_cover_test\\LT51680762008258.tif'
# Name and path of the output GeoTiff image
outImage <- 'D:\\Chapters\\KNP Data\\Shrub_cover_test\\shrubcov_1.tif'


# Function ----------------------------------------------------------------
rf_classify <-function(pointData, inImage, outImage){
  
  
# Start processing
print("Set variables and start processing")
startTime <- Sys.time()
cat("Start time", format(startTime),"\n")
  
pointTable <- read.csv(pointData, header=TRUE)
xy <- SpatialPoints(pointTable[,1:2])
response <- as.numeric(pointTable[,3])

# Load the moderate resolution image
nd <- 0
satImage <- stack(inImage)
for (b in 1:nlayers(satImage)) { NAvalue(satImage@layers[[b]]) <- nd }
  
# Get pixel DNs from the input image for each sample point
print("Getting the pixel values under each point")
trainvals <- cbind(response, extract(satImage, xy)) 

# Remove NA values from trainvals
trainvals_no_na <- na.omit(trainvals)
  
# Run Random Forest
print("Starting to calculate random forest object")
randfor <- randomForest(response ~. , data=trainvals_no_na)
  
# Start predictions
print("Starting predictions")
predict(satImage, randfor, filename=outImage, progress='text', format='GTiff', datatype='FLT4S', type='response', overwrite=TRUE)
#
# Calculate processing time
timeDiff <- Sys.time() - startTime
cat("Processing time", format(timeDiff), "\n")
}  