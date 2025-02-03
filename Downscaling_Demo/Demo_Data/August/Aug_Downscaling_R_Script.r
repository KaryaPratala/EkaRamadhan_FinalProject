#DATA PREPARATION
#1.Ensure that every month the environmental variables (Rainfall, NDVI, DEM) have gone through the GVT stage.
#2.Make sure the required data (Rainfall, NDVI, DEM) have the same extent.
#3.The coordinate system must be the same

#Set working directory
setwd("D:/loremipsum/8.Agustus")

#Package
library(sp)
library(sf)
library(raster)
library(abind)
library(Metrics)
library(rlang)
library(spatialEco)
library(spData)
library(spDataLarge)
library(stars)
library(spgwr)
library(tmap)
library(tmaptools)

# Resampling NDVI 1km to 11km
srtm1km48s<-raster("SRTM_1km.tif")
chaug11km48s<-raster("Aug_CH_11km.tif")
srtm11km48s<-resample(srtm1km48s, chaug11km48s,"bilinear",filename='SRTM_11km.tif')

# Get the resampled data
srtmlowres<-raster("SRTM_11km.tif")

# Generate point predictor from lowrest image
stack_lowres<-stack(chaug11km48s,srtmlowres)
stack_lowres_pt<-rasterToPoints(stack_lowres,spatial=TRUE)
names(stack_lowres_pt)<-c("chaug11km48s","srtmlowres")
head(stack_lowres_pt) #Checking the NA value if still presist

# Generate point predictor from highrest image
stack_highres<-stack(srtm1km48s)
stack_highres_pt<-rasterToPoints(stack_highres,spatial=TRUE)
names(stack_highres_pt)<-c("srtmlowres")
head(stack_highres_pt) #Makesure not contains the NA value

# GWR Process
withband<-gwr.sel(formula=chaug11km48s~srtmlowres,
                  data=stack_lowres_pt,
                  adapt=FALSE,
                  gweight=gwr.Gauss,
                  verbose=TRUE)
thegwr<-gwr(formula=chaug11km48s~srtmlowres,
            bandwidth=withband,
            data=stack_lowres_pt,
            fit.points=stack_highres_pt,
            gweight=gwr.Gauss,
            predictions=TRUE)
View(thegwr)

# Get the spatial data frame (sdf)
sdfthegwr<-thegwr[["SDF"]]
output_path<-'D:/loremipsum/8.Agustus/Aug_Predict_Highres_PT.shp' #Set saving folder
shapefile(sdfthegwr,output_path) #Save .shp

# Rasterizing
titik<-st_read("D:/loremipsum/8.August/Aug_Predict_Highres_PT.shp") #Read the point data
rasterize(x=titik,
          y=srtm1km48s,
          field="pred",
          filename="Aug_CH_1km.tif") #Change the vector data into highRes raster

library(ggplot2)
library(lattice)
library(rasterVis)

# Data Checking
# Plot of original and prediction data
plot(stack_lowres[[1]], main = "Rain Precipitation 11km August")
plot(raster("Aug_CH_1km.tif"), main = "Rain Precipitation 1km August")

# Comparing plot between original and prediction data lowres
observed <- stack_lowres_pt$chaug11km48s
predicted <- extract(raster("Aug_CH_1km.tif"), coordinates(stack_lowres_pt))

df_comparison <- data.frame(observed, predicted)
ggplot(df_comparison, aes(x = observed, y = predicted)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, col = "blue") +
  labs(title = "Rain Precipitation 11km vs Rain Precipitation 1km August",
       x = "Rain Precipitation 11km",
       y = "Rain Precipitation 1km") +
  theme_minimal()

# Calculate Error Matrix
mae_val <- mae(df_comparison$observed, df_comparison$predicted)
rmse_val <- rmse(df_comparison$observed, df_comparison$predicted)
r_squared <- cor(df_comparison$observed, df_comparison$predicted)^2
bias_val <- mean(df_comparison$predicted - df_comparison$observed)

# Shows the matrix
cat("Mean Absolute Error (MAE): ", mae_val, "\n")
cat("Root Mean Square Error (RMSE): ", rmse_val, "\n")
cat("R-squared: ", r_squared, "\n")
cat("BIAS: ", bias_val, "\n")