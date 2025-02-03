#DATA PREPARATION
#1.Ensure that every month the environmental variables (Rainfall, NDVI, DEM) have gone through the GVT stage.
#2.Make sure the required data (Rainfall, NDVI, DEM) have the same extent.
#3.The coordinate system must be the same

#Set working directory
("D:/loremipsum/1.Januari")

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
ndvi1km48s<-raster("Jan_NDVI_1km.tif")
chjan11km48s<-raster("Jan_CH_11km.tif")
ndvi11km48s<-resample(ndvi1km48s, chjan11km48s,"bilinear",filename='Jan_NDVI_11km.tif')

# Get the resampled data 
ndvilowres<-raster("Jan_NDVI_11km.tif")

# Generate point predictor from lowrest image
stack_lowres<-stack(chjan11km48s,ndvilowres)
stack_lowres_pt<-rasterToPoints(stack_lowres,spatial=TRUE)
names(stack_lowres_pt)<-c("chjan11km48s","ndvilowres")
head(stack_lowres_pt) #Checking the NA value if still presist
# Buat point predictor citra highres
stack_highres<-stack(ndvi1km48s)
stack_highres_pt<-rasterToPoints(stack_highres,spatial=TRUE)
names(stack_highres_pt)<-c("ndvilowres")
head(stack_highres_pt) #Makesure not contains the NA value

# GWR Process
withband<-gwr.sel(formula=chjan11km48s~ndvilowres,
                  data=stack_lowres_pt,
                  adapt=FALSE,
                  gweight=gwr.Gauss,
                  verbose=TRUE)
thegwr<-gwr(formula=chjan11km48s~ndvilowres,
            bandwidth=withband,
            data=stack_lowres_pt,
            fit.points=stack_highres_pt,
            gweight=gwr.Gauss,
            predictions=TRUE)
View(thegwr)

# Get the spatial data frame (sdf)
sdfthegwr<-thegwr[["SDF"]]
output_path<-'D:/loremipsum/1.Januari/Jan_Predict_Highres_PT.shp' #Set saving folder penyimpanan
shapefile(sdfthegwr,output_path) #Save .shp

# Rasterizing
titik<-st_read("D:/loremipsum/1.Januari/Jan_Predict_Highres_PT.shp") #Read the point data
rasterize(x=titik,
          y=ndvi1km48s,
          field="pred",
          filename="Jan_CH_1km.tif") #Change the vector data into highRes raster

library(ggplot2)
library(lattice)
library(rasterVis)

# Data Checking
# Plot of original and prediction data
plot(stack_lowres[[1]], main = "Rain Precipitation 11km Januari")
plot(raster("Jan_CH_1km.tif"), main = "Rain Precipitation 1km Januari")

# Comparing plot between original and prediction data lowres
observed <- stack_lowres_pt$chjan11km48s
predicted <- extract(raster("Jan_CH_1km.tif"), coordinates(stack_lowres_pt))

df_comparison <- data.frame(observed, predicted)
ggplot(df_comparison, aes(x = observed, y = predicted)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, col = "blue") +
  labs(title = "Rain Precipitation 11km vs Rain Precipitation 1km Januari",
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
cat("Root Mean Square Error (RMSE): ", rmse_val, "mm")
cat("R-squared: ", r_squared, "\n")
cat("BIAS: ", bias_val, "\n")