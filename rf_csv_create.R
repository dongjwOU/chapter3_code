####    rf_csv_create.R


# Information -------------------------------------------------------------
#### PREAMBLE 
# Tool to create csv file of percentage cover values,per sensor pixel, from high(er) resolution AP 
# Input data must be in the same projection and have an overlapping extent
# Classified image should be in format of (0,1,2) 
#         0 = no data such as background, clouds and shadow, as no cloud in the SNASA AP reclassed to 2
#         1 = class for which percent cover is being calculated 
#         2 = all other land cover classes


#### TO DO
# Add defensive function to ensure that proj systems match
# Add no data for the input classified image, if nessesary 

#####  BACKGROUND AND ORIGIN 
#The script gets the pixel values in a high resolution classified image that correspond to 
# individual randomly selected moderate resolution pixels and then calculates the percent of 
# the classified image pixels that represent your cover type of interest. In other words, if 
# your high resolution image has a pixel size of 1m and your moderate resolution image has a 
# pixel size of 30m the sampling process would take a block of 900 of the 1m resolution pixels 
# that correspond to a single 30m pixel and calculate the percentage of the 1m pixels that 
# are forest. For example, if there were 600 forest pixels and 300 non-forest pixels the value 
# given for the output pixel would be 0.67 since 67% of the block of 1m pixels were forest. If 
# there are clouds or other no-data values in the high resolution image the following logic will 
# apply. If the total no-data values for a block of high resolution pixels is greater than or equal 
# to a user defined threshold (we will use 10% i.e., 90 or more pixels in our example above) then 
# it will not be included in the training data set since there is too much missing data to provide 
# a reliable cover percentage. If the cloud cover is less then 10% the no-data pixels are removed 
# from the total number of pixels when calculating the percent forest cover.
# This script was written by Ned Horning [horning@amnh.org]
# Support for writing and maintaining this script comes from The John D. and 
# Catherine T. MacArthur Foundation.
#
# This script is free software; you can redistribute it and/or modify it under the 
# terms of the GNU General Public License as published by the Free Software Foundation
# either version 2 of the License, or ( at your option ) any later version.      



# Inputs ------------------------------------------------------------------
# Number of samples to be selected
numSamps <- 4000
# Name and path for the input classified image 
inClassImage <-"E:\\PhD Data\\Shrub_cover_working\\function_test\\data\\NB_ISO.tif"
# Name and path for the input image that will be used for predictions 
inPredImage <-'E:\\PhD Data\\Shrub_cover_working\\function_test\\data\\LT51680762008258.tif'
# Output file path and name
outFolder <- 'E:\\PhD Data\\Shrub_cover_working\\function_test\\outputs\\'

# Function ----------------------------------------------------------------

rf_csv_create <-function(numSamps,ClassImage,inPredImage,outFolder){
  
print("Set variables and start processing")
startTime <- Sys.time()
cat("Start time", format(startTime),"\n")

# No data value for the prediction image
noDataPct <- 1

fromVals <- c(0,1,2)
toVals <-   c(2,1,2)

# Load the classified image
classImage <- raster(inClassImage)

# Load the first band of the image that will be used for predicitons. 
predImage <- raster(inPredImage, band=1)

# Calculate the resolution of the two images
predImageRes <- res(predImage)[1]
classImageRes <- res(classImage)[1]
# Calculate the distance from the center of a raster cell to the edge - assuming a square pixel
halfCell <- predImageRes/2

# Select the class values that will be used to create the percent cover map
percentCoverValues <- fromVals[toVals == 1]

# Calculate the common extent to ensure sample cover both images
commonExt <- intersect(predImage, classImage)

# Select random samples from the prediciton image
cat("\nSelecting", numSamps, "samples from the prediction image\n")
sampleCells <- sampleRandom(crop(predImage,commonExt), size=numSamps, xy=TRUE, ext=commonExt, na.rm=TRUE)
lenSampCells <- nrow(sampleCells)
# Create the matrix that will hold the output data and label the columns
outputMatrix <- matrix(nrow=lenSampCells, ncol=3)
colnames(outputMatrix) <- c("X", "Y", "pct_cover")

fromTo <- data.frame(from=fromVals, to=as.integer(toVals==1))

# Get the pixels values from the classified image that fit inside the selected course-resolution pixel
cat("Calculating percent cover values for the output matrix\n\n")
for (i in 1:lenSampCells) {
  # Get the x and y coordinate from the center of the predition image pixel indicated by the sample point
  centerCoords <- sampleCells[i,1:2]
  # Insert x and y coordinate into the output matrix
  outputMatrix[i,1:2] <- c(centerCoords)
  # Calculate the extent of the selected prediction image pixel by adding/subtracting half the resolution of the pixel
  ext <- extent(centerCoords[1] - halfCell, centerCoords[1] + halfCell, centerCoords[2] - halfCell, centerCoords[2] + halfCell)
  # Get the cell numbers of all the classified image pixels that fall inside the extent of the selected prediction image pixel
  classCellNumbers <- cellsFromExtent(classImage, ext)
  # Get the class number of all of the selected classified image pixels
  classCellValues <- extract(classImage, classCellNumbers)
  # Convert classCellValues no-data pixels (0 in toVals) to NA
  classCellValues[classCellValues %in% fromVals[toVals==0]] <- NA
  # If the number of no-data pixels from the classified image is less than 'noDataPct' then calculate the percent cover
  # otherwise output NA
  if (sum(is.na(classCellValues))/length(classCellValues) < noDataPct) {
    outputMatrix[i,3] <- sum(!is.na(match(classCellValues, fromVals[toVals==1])))/length(classCellNumbers)
  } else {
    outputMatrix[i,3] <- NA
  }
  if (i %% 25 == 0)    cat("Processing sample", i, "of", lenSampCells, "\r")
}


# names for the output csv

matrix_nm <- paste0(outFolder, 'Allpoints.csv')
validation_nm <- paste0(outFolder, 'validation.csv')
training_nm<- paste0(outFolder, 'training.csv')

# Write out the non-NA values in the output matrix to a CSV file

write.csv(outputMatrix[which(!is.na(outputMatrix[,3])),], matrix_nm, row.names=FALSE)

matrix_csv_2 <- read.csv(matrix_nm, header=TRUE)

outMatrix_split <- split(matrix_csv_2,rep(1:2,each=500))

validation<-data.frame(outMatrix_split[1])
training <-data.frame(outMatrix_split[2])
write.csv(training, training_nm, row.names=FALSE)
write.csv(validation, validation_nm, row.names=FALSE)


return(outputMatrix)
#
# Calculate processing time
timeDiff <- Sys.time() - startTime
cat("Processing time", format(timeDiff), "\n")}



