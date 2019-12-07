########################## Final Project 

### Data Preparation:
#Libraries
library(sf)
library(plyr)
library(dplyr)
library(spdep)
library(GISTools)
library(raster)
library(maptools)
library(rgdal)
library(spatstat)
library(sp)
library(tmap)
library(gstat)
library(spgwr)
library(gridExtra)
library(grid)
library(gtable)
library(e1071)
library(ggplot2)
library(mapview)


#Set working directory
dir <- "/Users/HenryOrsini/Desktop/Geog 418/Labs/Final/Working"
setwd(dir)
getwd()

#Reading in particulate matter dataset
pm25 <- read.csv("PM25.csv")
#Select only columns 1 and 2
pm25 <- pm25[,1:2]
#Change the column names 
colnames(pm25) <- c("POSTALCODE", "PM25")
#Omit na values
pm25 <- na.omit(pm25)

#Reading in postal code shapefile
postalcodes <- shapefile("./BC_PostalCodes/BC_Postal_Codes") 

#Reading in dissemination tract and income data
income <- read.csv("Income.csv")  
#Select only ID and Income columns
colnames(income) <- c("DAUID", "Income") 
#Read in dissemination tract shapefile
census.tracts <- shapefile("./BC_DA/BC_DA.shp") 
#Merge income and dissemination data
income.tracts <- merge(census.tracts,income, by = "DAUID") 
#Determine the number of columns in the dataframe
nrow(income.tracts) 
# Removing NA
income.tracts <- income.tracts[!is.na(income.tracts$Income),] 

# #Create choropleth map of income
# med.income <- income.tracts$Income
# shades <- auto.shading(med.income, n=6, cols = brewer.pal(6, 'Oranges'))
# #map the data with associated colours
# choropleth(income.tracts, med.income, shades) 
# #Adding Legend
# choro.legend(-123.8, 49.15, shades, cex = .5, title = "Yearly Income") 

map_income <- tm_shape(income.tracts) +
  tm_polygons(col = "Income",
              title = "Income",
              style = "fisher", 
              palette="Greens",
              lwd = 0.3) +
  tm_shape(spSample) + 
  tm_dots(col="PM25AGG", palette = "-RdBu", title="PM2.5 (in ppm)", size=0.2) +
  tm_compass(north = 0, type = NA, text.size = 0.8,size = NA, position = "LEFT") +
  tm_scale_bar(width = 0.15, text.size = 0.5, position = "LEFT") +
  tm_layout(legend.outside = TRUE)
map_income

#Select postal codes that fall within dissemination tracts)
postalcodes <- intersect(postalcodes,income.tracts)
#See what the data looks like spatially
# plot(postalcodes) 
#See what the data looks like in tabular form
# head(postalcodes) 

#Join PM2.5 data with postal code data
pm25.spatial <- merge(postalcodes,pm25,by = "POSTALCODE")

#Aggregate the PM2.5 values in each DA in order to have a single value per DA. Here we aggregate based on the max
pm25.aggregate <- aggregate((as.numeric(pm25.spatial$PM25)/10)~pm25.spatial$DAUID,FUN=max)

#Re-join aggregated data to the income.tracts layer.
colnames(pm25.aggregate) <- c("DAUID", "PM25AGG") #Select only ID and Income columns
income.pm25 <- merge(income.tracts,pm25.aggregate, by = "DAUID") #Merge income and dissemination data

#Re-join aggregated data to the pm25.spatial points layer.
pm25.points.aggregate <- merge(pm25.spatial, pm25.aggregate, by = "DAUID")

#Create a subsample of the datapoints provided in the PM2.5 dataset using the sample n provided on CourseSpaces ## CHANGE THIS TO 260
sampleSize=170
spSample <- pm25.points.aggregate[sample(1:length(pm25.points.aggregate),sampleSize),]

#Create a grid called grd to use in your interpolation
# Create an empty grid where n is the total number of cells
grd <- as.data.frame(spsample(spSample, "regular", n=6000))
names(grd)       <- c("X", "Y")
coordinates(grd) <- c("X", "Y")
gridded(grd)     <- TRUE  # Create SpatialPixel object
fullgrid(grd)    <- TRUE  # Create SpatialGrid object
proj4string(grd) <- proj4string(spSample)

# # Study Area
# tm_shape(income.pm25) +
#   tm_fill() +
#   tm_borders() +
#   tm_shape(spSample) + tm_dots(col="PM25AGG", palette = "-RdBu", title="PM2.5 (in ppm)", size=0.4) +
#   tm_compass(north = 0, type = NA, text.size = 0.8,size = NA,position = "LEFT") +
#   tm_scale_bar(width = 0.15, text.size = 0.5, position = "LEFT") +
#   tm_layout(legend.outside = TRUE)




###### Descriptive Statistics
#Mean
meanInc <- round(mean(pm25.points.aggregate$Income, na.rm = TRUE), digits=2)
meanPM25 <- round(mean(pm25.points.aggregate$PM25AGG, na.rm = TRUE), digits=2)

#Mode
modeInc <- round(as.numeric(names(sort(table(pm25.points.aggregate$Income), decreasing = TRUE))[1]), digits=2)
modePM25 <- round(as.numeric(names(sort(table(pm25.points.aggregate$PM25AGG), decreasing = TRUE))[1]), digits=2)

#Median
medInc <- round(median(pm25.points.aggregate$Income, na.rm = TRUE), digits=2)
medPM25 <- round(median(pm25.points.aggregate$PM25AGG, na.rm = TRUE), digits=2)

#Standard Deviation
sdInc <- round(sd(pm25.points.aggregate$Income, na.rm = TRUE), digits=2)
sdPM25 <- round(sd(pm25.points.aggregate$PM25AGG, na.rm = TRUE), digits=2)

#Skewness
skewInc <- round(skewness(pm25.points.aggregate$Income, na.rm = TRUE)[1], digits=2)
skewPm25 <- round(skewness(pm25.points.aggregate$PM25AGG, na.rm = TRUE)[1], digits=2)

#Create a table of descriptive stats
samples = c("Yearly Income", "PM2.5") # Create an object for the labels
mean = c(meanInc, meanPM25) #Create an object for the means
median = c(medInc, medPM25) #Create an object for the medians
mode = c(modeInc, modePM25) #Create an object for the modes
standard_deviation = c(sdInc, sdPM25) #Create an object for the standard deviations
skewness = c(skewInc, skewPm25) #Create an object for the skewness

# Making table
table = data.frame(samples, mean, median, mode, standard_deviation)
table <- tableGrob(table, rows = c("","")) # "Graphical Object" (GrOb)
Caption <- textGrob("Table 1: Descriptive Statistics for Yearly Income and PM2.5", gp = gpar(fontsize = 09))
padding <- unit(5, "mm")

# Table
grid.arrange(table, newpage = TRUE)
dev.off

### Histograms
hist.Inc <- hist(pm25.points.aggregate$Income, breaks = 30, main = NA, xlab = "Income")
hist.PM25 <- hist(pm25.points.aggregate$PM25AGG, breaks = 30, main = NA, xlab = "PM2.5")



########## Spatial Autocorrelation of Income

# Queens Case Network
income.nb <- poly2nb(income.pm25)
income.net <- nb2lines(income.nb,coords=coordinates(income.pm25))

# Weight Matrix
income.lw <- nb2listw(income.nb, zero.policy = TRUE, style = "W")
print.listw(income.lw, zero.policy = TRUE)

# Global Moran's I
mi <- moran.test(income.pm25$Income, income.lw, zero.policy = TRUE) # On Median
mi

# Range of Moran's I
moran.range <- function(lw) {
  wmat <- listw2mat(lw)
  return(range(eigen((wmat + t(wmat))/2)$values))
}
moran.range(income.lw)

# Z formula
mI <- mi$estimate[[1]] # Global Morans i
eI <- mi$estimate[[2]] # Expected
V <- mi$estimate[[3]] # Varience
z <- (mI-eI)/((V)^0.5)
z

# Local Moran's I
lisa.test <- localmoran(income.pm25$Income, income.lw)

# Adding columns
income.pm25$Ii <- lisa.test[,1]
income.pm25$E.Ii<- lisa.test[,2]
income.pm25$V.Ii<- lisa.test[,3]
income.pm25$Z.Ii<- lisa.test[,4]
income.pm25$P<- lisa.test[,5]

#range(income.pm25$Ii)
# tmap_mode("view") # Plot, view
# Mapping LISA
# map_LISA <- tm_shape(income.pm25) +
#   tm_polygons(col = "Ii",
#               title = "Local Moran's I",
#               style = "fisher",
#               palette = "viridis", n = 6,
#               alpha = 0.5) +
#   tm_compass(north = 0, type = NA, text.size = 0.8,size = NA, position = "LEFT") +
#   tm_scale_bar(width = 0.15, text.size = 0.5, position = "LEFT") +
#   tm_layout(legend.outside = TRUE)
# map_LISA # Neighbourhoods with significant spatial autocorrolation

#range(income.pm25$Z.Ii)
map_LISA_z <- tm_shape(income.pm25) +
  tm_polygons(col = "Z.Ii",
              title = "Local Moran's Z scores",
              style = "fixed", breaks=c(-10,-1.96,0,1.96,20),
              palette="-RdBu",
              lwd = 0.3) +
  tm_compass(north = 0, type = NA, text.size = 0.8,size = NA, position = "LEFT") +
  tm_scale_bar(width = 0.15, text.size = 0.5, position = "LEFT") +
  tm_layout(legend.outside = TRUE)
map_LISA_z

# Plot of all spatial autocorrolation
moran.plot(income.pm25$Income, income.lw, zero.policy=NULL, spChk=NULL, labels=NULL, xlab="Density", ylab="Spatially Lagged Density", quiet=NULL)


########## Spatial Interpolation 

# ## Polynomial Trends
# # Define the 2nd order polynomial equation: DEFINING AS A PERABOLA
# f.2 <- as.formula(PM25AGG ~ X + Y + I(X*X)+I(Y*Y) + I(X*Y)) 
# # Add X and Y to P
# spSample$X <- coordinates(spSample)[,1]
# spSample$Y <- coordinates(spSample)[,2]
# # Run the regression model
# lm.2 <- lm(f.2, data=spSample)
# # Use the regression model output to interpolate the surface
# dat.2nd <- SpatialGridDataFrame(grd, data.frame(PM25AGG = predict(lm.2, newdata=grd))) 
# # Clip the interpolated raster
# r.2nd   <- raster(dat.2nd)
# # r.m <- mask(r, income.pm25)
# # Plot the map
# tm_shape(r.2nd) + 
#   tm_raster(n=6, palette="-RdYlBu", title="PM2.5 (in ppm)") +
#   tm_shape(spSample) + 
#   tm_dots(size=0.2) +
#   tm_legend(legend.outside=TRUE) +
#   tm_compass(north = 0, type = NA, text.size = 0.8,size = NA,position = "LEFT") +
#   tm_scale_bar(width = 0.2, text.size = 0.5, position = "LEFT")
# 
# ### UNIVERSAL KRIGING
# f.2 <- as.formula(PM25AGG ~ X + Y + I(X*X) + I(Y*Y) + I(X*Y))
# var.smpl <- variogram(f.2, spSample, cloud = FALSE) 
# dat.fit  <- fit.variogram(var.smpl, vgm(model="Gau")) 
# plot(var.smpl, dat.fit)
# # Using the model
# #View(dat.fit)
# dat.krg <- krige(f.2, spSample, grd, dat.fit) 
# # View(dat.krg@data)
# # Convert kriged surface to a raster object for clipping
# r <- raster(dat.krg)
# r.m <- mask(r, income.pm25)
# # Plot the map
# tm_shape(r) + 
#   tm_raster(n=7, palette="-RdYlBu",title="PM 2.5") +
#   tm_shape(spSample) + 
#   tm_dots(size=0.2) +
#   tm_legend(legend.outside=TRUE) + 
#   tm_compass(north = 0, type = NA, text.size = 0.8,size = NA,position = "LEFT") +
#   tm_scale_bar(width = 0.2, text.size = 0.5, position = "LEFT")




######### Spatial Interpolation

# IDW
proj4string(grd) <- proj4string(spSample)
P.idw <- gstat::idw(PM25AGG ~ 1, spSample, newdata=grd, idp=4)
r       <- raster(P.idw)
r.m     <- mask(r, income.pm25)
# View(P.idw@data)
tm_shape(r.m) +
  tm_raster(n=7, palette = "Oranges", title="Predicted PM2.5 (in ppm)") +
  tm_shape(spSample) +
  tm_dots(size=0.2) +
  tm_legend(legend.outside=TRUE) +
  tm_compass(north = 0, type = NA, text.size = 0.8,size = NA,position = "LEFT") +
  tm_scale_bar(width = 0.2, text.size = 0.5, position = "LEFT")

# tm_compass(north = 0, type = NA, text.size = 0.8,size = NA,position = "LEFT") + tm_scale_bar(width = 0.15, text.size = 0.5, position = "LEFT")

# Leave on out
IDW.out <- vector(length = length(spSample))
for (i in 1:length(spSample)) {
  IDW.out[i] <- gstat::idw(PM25AGG ~ 1, spSample[-i,], spSample[i,], idp=4)$var1.pred
}
# Plot the differences
OP <- par(pty="s", mar=c(4,3,0,0))
plot(IDW.out ~ spSample$PM25AGG, asp=1, xlab="Observed", ylab="Predicted", pch=16, col=rgb(0,0,0,0.5))
abline(lm(IDW.out ~ spSample$PM25AGG), col="red", lw=2,lty=2)
abline(0,1)
par(OP)
sqrt(sum((IDW.out - spSample$PM25AGG)^2) / length(spSample))



###### Aggrigate Data

# Extracting the idw and income into one file
pm.income.poly <- raster::extract(r, income.tracts, fun=mean, sp=TRUE) # View(pm.income.poly@data)
# Renaming the column
names(pm.income.poly)[names(pm.income.poly) == "var1.pred"] <- "PM25"
#irrelevant line # sum(pm.income.poly$PM25, na.rm = TRUE)
# OMITS NA VALUES OF PM25 RESULTING FROM THE BOUNDS OF TH INTERPOLATED OUTPUT GRID NOT INCLUDING POINTS IN THOSE REGIONS
pm.income.poly <- pm.income.poly[!is.na(pm.income.poly$PM25),]
# View(pm.income.poly@data)
# sum(is.na(pm.income.poly$PM25))

# Get ride of polygons with negative pm2.5 values - switch certain values of the dataset to zero - no need to do this all values are positive



######Linear Regression##########
#Let's say your dataset with both PM2.5 and Income are stored in a dataset called pm.income.poly.

#Plot income and PM2.5 from the pm.income.poly dataset you created
plot(pm.income.poly$Income~pm.income.poly$PM25, )
#Notice that there are a lot of 0's in this dataset. If you decide to remove them, use the following line:
pm.income.poly <-  pm.income.poly[pm.income.poly$PM25 != 0, ]
#Now plot the data again
plot(pm.income.poly$Income~pm.income.poly$PM25, xlab="PM2.5", ylab="Income",)

#Perform a linear regression on the two variables. You should decide which one is dependent.
lm.model <- lm(pm.income.poly$Income~pm.income.poly$PM25)
#Add the regression model to the plot you created
abline(lm.model)
#Get the summary of the results
summary(lm.model)

#You want to determine if the model residuals are spatially clustered. 
#First obtain the residuals from the model
model.resids <- as.data.frame(residuals.lm(lm.model))
#Then add the residuals to your spatialpolygon dataframe
pm.income.poly$residuals <- residuals.lm(lm.model)
#Observe the result to make sure it looks correct
head(pm.income.poly)

# #Now, create choropleth map of residuals
# resids <- pm.income.poly$residuals
# range(resids)
# shades.res <- shading(breaks=c(-8500,-4500,0,4500,8500), cols = brewer.pal(6, 'PRGn')) #shades <- auto.shading(resids, n=6, cols = brewer.pal(6, 'PuOr'))
# choropleth(income.tracts, resids, shades.res) 
# choro.legend(-123.8, 49.2, shades.res, cex = .5, title = "Residuals") 


map_resids <- tm_shape(pm.income.poly) +
  tm_polygons(col = "residuals",
              title = "Residuals",
              style = "fixed", breaks=c(-25000,-8500,-4500,0,4500,8500,25000),
              palette="-RdBu", 
              n = 6,
              lwd = 0.3) + #  style = "-RdBu",palette = "", n = 6,
  tm_compass(north = 0, type = NA, text.size = 0.8,size = NA, position = "LEFT") +
  tm_scale_bar(width = 0.15, text.size = 0.5, position = "LEFT") +
  tm_layout(legend.outside = TRUE) 
map_resids


########## Moran's I from assignment 3 ##########

# Queens Case
income.nb_r <- poly2nb(pm.income.poly)
income.net_r <- nb2lines(income.nb_r,coords=coordinates(pm.income.poly))

# Weight Matrix
income.lw_r <- nb2listw(income.nb_r, zero.policy = TRUE, style = "W")
print.listw(income.lw_r, zero.policy = TRUE)


### Global Moran's I test
mi_r <- moran.test(pm.income.poly$residuals, income.lw_r, zero.policy = TRUE) # On Median 
mi_r # Gives us statistics to calculate a z-score

# Range of Moran's I
moran.range <- function(lw_r) {
  wmat <- listw2mat(lw_r)
  return(range(eigen((wmat + t(wmat))/2)$values))
}
moran.range(income.lw_r)

# Z formula
mI_r <- mi_r$estimate[[1]] # Grabs morans i stat
eI_r <- mi_r$estimate[[2]] # Grabs expected 
V_r <- mi_r$estimate[[3]] # Grabs varience

z_r <- (mI_r-eI_r)/((V_r)^0.5) 
z_r 


### Local Moran's I
lisa.test_r <- localmoran(pm.income.poly$residuals, income.lw_r)

# Adding columns 
pm.income.poly$Ii_r <- lisa.test_r[,1]
pm.income.poly$E.Ii_r <- lisa.test_r[,2]
pm.income.poly$V.Ii_r <- lisa.test_r[,3]
pm.income.poly$Z.Ii_r <- lisa.test_r[,4]
pm.income.poly$P_r <- lisa.test_r[,5]

# tmap_mode("view") # Plot, view
# Mapping LISA
# map_LISA_r <- tm_shape(pm.income.poly) + 
#   tm_polygons(col = "Ii_r", 
#               title = "Local Moran's I", 
#               style = "fisher", # "fixed", breaks=c(-5, -1, 0, 1, 3, 5, 10), 
#               palette = "viridis", n = 6, #8   palette="PRGn", n = 8, # 
#               alpha = 0.5) +
#   tm_compass(north = 0, type = NA, text.size = 0.8,size = NA, position = "LEFT") +
#   tm_scale_bar(width = 0.15, text.size = 0.5, position = "LEFT") +
#   tm_layout(legend.outside = TRUE) 
# map_LISA_r # Neighbourhoods with significant spatial autocorrolation 
# range(pm.income.poly$Z.Ii_r)
map_LISA_r_z <- tm_shape(pm.income.poly) +
  tm_polygons(col = "Z.Ii_r",
              title = "Local Moran's Z scores",
              style = "fixed", breaks=c(-10,-1.96,0,1.96,20),
              palette="-RdBu",
              lwd = 0.3) +
  tm_compass(north = 0, type = NA, text.size = 0.8,size = NA, position = "LEFT") +
  tm_scale_bar(width = 0.15, text.size = 0.5, position = "LEFT") +
  tm_layout(legend.outside = TRUE)
map_LISA_r_z


# Plot of all spatial autocorrolation 
moran.plot(pm.income.poly$residuals, income.lw_r, zero.policy=NULL, spChk=NULL, labels=NULL, xlab="Density", ylab="Spatially Lagged Density", quiet=NULL)




####Geographically Weighted Regression #######

#Let's say you are continuing with your data from the regression analysis. 

#The first thing you need to do is to add the polygon coordinates to the spatialpolygondataframe.
#You can obtain the coordinates using the "coordinates" function from the sp library
pm.income.poly.coords <- sp::coordinates(pm.income.poly)
#Observe the result
head(pm.income.poly.coords)
#Now add the coordinates back to the spatialpolygondataframe
pm.income.poly$X <- pm.income.poly.coords[,1]
pm.income.poly$Y <- pm.income.poly.coords[,2]
head(pm.income.poly)

###Determine the bandwidth for GWR: this will take a while
# GWRbandwidth <- gwr.sel(pm.income.poly$Income~pm.income.poly$PM25, data=pm.income.poly, coords=cbind(pm.income.poly$X,pm.income.poly$Y),adapt=T) 

###Perform GWR on the two variables with the bandwidth determined above
###This will take a looooooong while
# gwr.model = gwr(pm.income.poly$Income~pm.income.poly$PM25, data=pm.income.poly, coords=cbind(pm.income.poly$X,pm.income.poly$Y), adapt=GWRbandwidth, hatmatrix=TRUE, se.fit=TRUE) 

#Print the results of the model
gwr.model

#Look at the results in detail
results<-as.data.frame(gwr.model$SDF)
head(results)

#Now for the magic. Let's add our local r-square values to the map
pm.income.poly$localr <- results$localR2

# #Create choropleth map of r-square values
# local.r.square <- pm.income.poly$localr
# shades.r2 <- auto.shading(local.r.square, n=6, cols = brewer.pal(6, 'Oranges'))
# choropleth(income.tracts, local.r.square, shades.r2) #map the data with associated colours
# choro.legend(-123.7, 49.19, shades.r2, cex = .5, title = "R-Square Value") #add a legend (you might need to change the location)
# tmap_mode('plot')
map_r2 <-  tm_shape(pm.income.poly) +
  tm_polygons(col = "localr",
              title = "Local R-Square",
              style = "fixed", breaks=c(0,0.2,0.4,0.6,0.8,1),
              palette="Oranges", 
              n = 7,
              lwd = 0.3) + #  style = "-RdBu",palette = "", n = 6,
  tm_compass(north = 0, type = NA, text.size = 0.8,size = NA, position = "LEFT") +
  tm_scale_bar(width = 0.15, text.size = 0.5, position = "LEFT") +
  tm_layout(legend.outside = TRUE) 
map_r2


# #Time for more magic. Let's map the coefficients
# pm.income.poly$coeff <- results$pm.income.poly.PM25
# range(pm.income.poly$coeff)
# #Create choropleth map of the coefficients
# local.coefficient <- pm.income.poly$coeff
# shades.coef <- shading(breaks=c(-12000,-4000,0,4000,12000), cols = brewer.pal(6, 'PRGn'))# auto.shading(local.coefficient, n=6, cols = brewer.pal(6, 'Oranges'))
# choropleth(income.tracts, local.coefficient, shades.coef) #map the data with associated colours
# choro.legend(-123.75, 49.19, shades.coef, cex = .5, title = "Coefficient Value")

map_coeff <- tm_shape(pm.income.poly) +
  tm_polygons(col = "coeff",
              title = "Local Coefficient",
              style = "fixed", breaks=c(-50000,-13000,-4000,0,4000, 13000, 30000),
              palette="-RdBu", 
              n = 7,
              lwd = 0.3) + #  style = "-RdBu",palette = "", n = 6,
  tm_compass(north = 0, type = NA, text.size = 0.8,size = NA, position = "LEFT") +
  tm_scale_bar(width = 0.15, text.size = 0.5, position = "LEFT") +
  tm_layout(legend.outside = TRUE) 
map_coeff 




########## Point Pattern Analysis from assignment 2######### 
# spSample.trans <- spTransform(spSample, "+proj=utm +zone=10 ellps=32610") # st_crs(spSample.trans)
# proj4string(grd) <- proj4string(spSample) CRS arguments:+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0

# Transforming the coordinate system
spSample.trans <- spTransform(spSample, CRS("+init=epsg:32610"))
spSample.trans

# Adding x and y
spSample.trans$x <- coordinates(spSample.trans)[,1]
spSample.trans$y <- coordinates(spSample.trans)[,2]

#finds zero distance among points
zd <- zerodist(spSample.trans)
zd
#remove duplicates
spSample.trans <- remove.duplicates(spSample.trans)

#create an "extent" object which can be used to create the observation window for spatstat
spSample.trans.ext <- as.matrix(extent(spSample.trans)) 

#observation window
window <- as.owin(list(xrange = spSample.trans.ext[1,], yrange = spSample.trans.ext[2,]))

#create ppp oject from spatstat
spSample.trans.ppp <- ppp(x = spSample.trans$x, y = spSample.trans$y, window = window)

#####
##Nearest Neighbour Distance
nearestNeighbour <- nndist(spSample.trans.ppp)

##Convert the nearestNeighbor object into a dataframe.
nearestNeighbour=as.data.frame(as.numeric(nearestNeighbour))
##Change the column name to "Distance"
colnames(nearestNeighbour) = "Distance"

nnd = (sum(nearestNeighbour$Distance))/sampleSize 

# Find the area from the shape file 
studyArea <- 2877360000 # gArea(income.tracts) 115000000 2700000000 
pointDensity <- sampleSize/studyArea

r.nnd = 1/(2*(pointDensity)^(1/2))

# Value for a perfectly disersed sample
d.nnd = 1.07453/((pointDensity)^(0.5))

# Comparable ratio:
R = nnd/r.nnd

# Checking significance  
se.nnd <- 0.26136/((sampleSize*pointDensity)^0.5) # Standard diviation

z = (nnd-r.nnd)/se.nnd # Z-value


### K Function
k.fun <- Kest(spSample.trans.ppp, correction = "Ripley")
#use simulation to test the point pattern against CSR
K_Function <- envelope(spSample.trans.ppp, Kest, nsim = 95, correction = "Ripley")
plot(K_Function) 


###KERNEL DENSITY ESTIMATION
#use cross-validation to get the bandwidth that minimizes MSE
bw.d <- bw.diggle(spSample.trans.ppp)
plot(bw.d, ylim=c(-10, 10))

#density using the cross-validation bandwidth
kde.bwo <- density(spSample.trans.ppp, sigma = bw.d, at = "pixels", eps = c(100, 100))
kde.bwo.m <- mask(kde.bwo, income.pm25)

plot(kde.bwo)
# plot(kde.bwo, main = "KDE", "  N = ", nrow(spSample@data), sep = "")