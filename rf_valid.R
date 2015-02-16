####      rf_valid.R
# Preamble ----------------------------------------------------------------
# Function to validate the obtained percentage cover rasters agains AP values from in the
# rf_csv_create function
# 
# 12 correlation metrics are returned, in addition to the data frame containing actual/modelled results 


#### Packages
require(sirad)
require(raster)

# Inputs ------------------------------------------------------------------

# The validation CSV file containing X, Y, and percent cover point data created by the rf_csv_create.R script.
pointData <- 'D:\\Chapters\\KNP Data\\Shrub_cover_test\\outSamples2.csv'
# Name and path for the input satellite image 
inImage <-'D:\\Research\\Chapters\\KNP Data\\Shrub_cover_test\\LT51680762008258.tif'
# Name and path of the output csv of validation metrics
outfolder <- 'E:\\PhD Data\\Shrub_cover_working\\function_test\\outputs\\'

# Function ----------------------------------------------------------------

rf_valid <-function(pointData,inImage,outfile){

#load pct_cover raster 
shrub_raster <- raster(inImage)

#load csv and convert to point file
pointCoordinates <-read.csv(pointData)
coordinates(pointCoordinates)= ~ X1.X + X1.Y

#load csv file to join with extracted raster values.
points.csv <-read.csv(pointData)

#extract raster values
raster_values =extract(shrub_raster, pointCoordinates)

#combine values in csv
validation.df=cbind(points.csv,raster_values)


# Preform model validation tests
validation.mod <-modeval(calculated = validation.df$raster_values,measured = validation.df$X1.pct_cover)


# create file names and write outputs 
matrix <- paste0(outfolder, 'validation_sample.csv')
validation_metrics <- paste0(outfolder, 'validation_metrics.csv')

write.csv(validation.df,matrix)
write.csv(validation.mod, validation_metrics)

return(validation.mod)
}





