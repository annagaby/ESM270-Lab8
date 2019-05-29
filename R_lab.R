# install.packages(c("raster","rgdal","rasterVis","maps","rgeos","dplyr","RColorBrewer"))

# Load the libraries into this R session
library(raster)       #Main raster library with nearly all functions used in this analysis
library(rgdal)        #Spatial library - most functions used from rgdal are for vectors (shapefiles)
library(rasterVis)    #Useful for raster visualizations
library(maps)         #Has a database of maps. I use this to add a map to my raster to visualize land boundaries
library(rgeos)        #Need this library for topology operations on geometries
library(dplyr)        #NOT spatial - this is a data wrangling library
library(RColorBrewer) #Also not spatial - used to set the spectral color scheme 

# Part 2.
# view some color palettes
# display.brewer.all()
# rainbow color scheme
cols = rev(colorRampPalette(brewer.pal(11, 'Spectral'))(255)) 

# setting smaller margins for plotting
par(mar=c(2,2,1,1))

# Change this path to the path to the lab data: "R:/Spring2019/ESM270/Week7_R_lab/Threats_data/full_modelnv.tif"
all_threats <- raster("full_modelnv.tif")

# Visualize
plot(all_threats,col=cols)

# add a landmap to your shapefile. the add=T argument tells R to add it to the existing plot.
# make sure you understand what the other arguments do
plot(all_threats,ext=extent(-130,-110,24,50),col=cols)
map('world',fill=T,add=T,col='gray')

# Extent and Zoom
# A good extent for the Santa Barbara Channel
plot(all_threats,col=cols,ext=extent(-121,-117,32,35),main="Cumulative Threats") 

# Raster data attributes
# Histogram
hist(all_threats,main="Cumulative Threats Frequency")

cellStats(all_threats,mean)

cellStats(all_threats,sd)

# Part 3. Raster calculations

# Import species data
# Make sure the pathname to the data is correct
all_spp <- raster("ca_curr_sp_rich.tif")

all_spp

plot(all_spp,col=cols)

# Two raster functions: 
# Crop
#?crop see what the crop function does

threats_crop <- crop(all_threats,all_spp) #Crop the threats layer to the same extent at species

# Resample 
# (a little artificial, converts into higher resolution)
#?resample see what the resample function does
# NOTE: the progress='text' argument is a great tool: it prints out the progress
# of a longer-running function into the console, so you can see how the operation is going

# the method='ngb' argument specifies that we want to use a nearest neighbor algorithm to resample, instead of interpolation
spp_res <- resample(all_spp,threats_crop,method='ngb',progress='text')
# need method argument, which can be bilinear or ngb
# Raster stack
spp_threat_stack <- stack(threats_crop,spp_res)
plot(spp_threat_stack,col=cols)
# Reclassify
hist(spp_res,main="Species Raster Values")
# notice that in the following, we are OVERWRITING the original spp_res object.
# This is okay in this instance since we won't be using the old version, but
# often it is better to assign any output of a function to a new variable or object
spp_res <- reclassify(spp_res,rcl=c(-Inf,0,NA))
hist(spp_res,main="Species Raster Values, Zeroes Removed") # did the function do what we were hoping?
plot(spp_res)


#?quantile what does the quantile function do?
spp_cutoff <- quantile(spp_res,0.8) # Find the value of the 80th percentile
spp_maxVal <- cellStats(spp_res,max) #find the maximum

# Our reclassification matrix. Make sure you know what this is saying
rcl_mat <- c(-Inf,spp_cutoff,0,
             spp_cutoff,spp_maxVal,1)

# Reclassify the species layer
spp_binary <- reclassify(spp_res,rcl=rcl_mat)

# Because we have binary data now, I want to change the color scheme again
binary_cols <- c("white","firebrick")
plot(spp_binary,col=binary_cols,legend=F,main="Top 20% of Species Richness")
map('world',fill=T,add=T,col='gray')

# Reclassify threats
threats_crop <- reclassify(threats_crop,rcl=c(-Inf,0,NA))
hist(threats_crop,main="Threat Raster Values, Zeroes Removed") # did the function do what we were hoping?
plot(threats_crop)
#?quantile what does the quantile function do?
threats_cutoff <- quantile(threats_crop,0.8) # Find the value of the 80th percentile
threats_maxVal <- cellStats(threats_crop,max) #find the maximum

# Our reclassification matrix. Make sure you know what this is saying
rcl_mat <- c(-Inf,threats_cutoff,0,
             threats_cutoff,threats_maxVal,1)

# Reclassify the species layer
threats_binary <- reclassify(threats_crop,rcl=rcl_mat)

# Because we have binary data now, I want to change the color scheme again
binary_cols <- c("white","firebrick")
plot(threats_binary,col=binary_cols,legend=F,main="Top 20% of Cumulative Threats")
map('world',fill=T,add=T,col='gray')

# the hotspots
hotspots <- overlay(spp_binary,threats_binary,fun=function(x,y){x+y})

# color breakpoints. We need three colors now! (cell values of 0,1,or 2)
brks_hotspots <- seq(0,3,length.out=4) 
hotspot_cols <- c("white","lightblue","firebrick") #

# plot the hotspots!
plot(hotspots,col=hotspot_cols,legend=F,main="Hotspots");map('world',fill=T,add=T,col='gray80')

