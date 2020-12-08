#Libraries


library(spgwr)
library(spatstat)
library(tmap)
library(gstat)
library(sf)
library(raster)
library(rgdal)
library(e1071)
library(spdep)

#Set working directory
dir <- "/Users/hannahandtrevor/Desktop/GEOG518/Lab5"
setwd(dir)
getwd()

#Reading in elevation dataset
elev <- readOGR(dsn = ".", layer = "ElevSample") #Read in data
elev <- spTransform(elev, CRS("+init=epsg:26910"))

#Reading in VRI data
VRI <- readOGR(dsn = ".", layer = "WatershedVRI") #Read in shapefile
VRI <- spTransform(VRI, CRS("+init=epsg:26910"))
head(VRI@data)

##--------------Clean up VRI data as we did in Lab 3:----------------##

#Check data columns
View(VRI@data)
#VRI$REFERENCE1 <- as.numeric(VRI$REFERENCE1)
#range_date <- range(VRI$REFERENCE1)
#range_date
VRI$PROJECTED_

nrow(VRI)

#View data fields in seperate window
#View(VRI@data)

citation("spdep")

#Select only the existing columns that are of interest
vriCleanCols <- c("FID_VEG_CO", "POLYGON_ID", "PROJ_AGE_1",
                  "SITE_INDEX", "SPECIES__4", "SPECIES__5",
                  "PROJ_HEI_1", "SPECIES_PC", "SPECIES__6",
                  "VRI_LIVE_S", "BASAL_AREA", "WHOLE_STEM",
                  "CROWN_CL_1")

#Clean dataset to only include desired columns:
vriClean <- VRI[,vriCleanCols]

##-------------Metadata and links----------------##
#Meta Data (https://www2.gov.bc.ca/assets/gov/farming-natural-resources-and-industry/forestry/stewardship/forest-analysis-inventory/data-management/standards/vegcomp_poly_rank1_data_dictionaryv5_2019.pdf)
# VRI = Vegetation Resource Inventory
# FID = Field ID
# PolyID = VRI Polygon ID
# Stand_Age = Estimated stand age projected to 2020 from estimated establishment date
# Site_Index = A value to estimate site quality. This describes the height that the stand could grow to by age 50 in meters.
# CoDom_Sp = The species code for the co-dominant tree species. Full list of codes: https://www.for.gov.bc.ca/hfp/publications/00026/fs708-14-appendix_d.htm
# Dom_Sp = The species code for the dominant tree species. Full list of codes: https://www.for.gov.bc.ca/hfp/publications/00026/fs708-14-appendix_d.htm
# Stand_HT = The estimated height for the stand (in meters)
# DomSP_Perc = The estimated percentage of the dominant species
# CDomSP_Perc = The estimated percentage of the co-dominant species
# Stand_Dens = Estimated density of stand (Stems per hectare)
# Stand_BA = Estimated Basal area of the stand (square meters)
# Stand_StemBio = Estimated stand level stem biomass (tonnes per hectare)
# Stand_CrownCl = The percentage of ground area covered by tree crowns

#Rename the columns in the clean dataset to better reflect what they represent and the names used in the metadata definitions (above)
newNames <- c("FID", "PolyID", "Stand_Age", "Site_Index",
              "CoDom_Sp", "Dom_Sp", "Stand_HT", "DomSP_Perc", 
              "CDomSP_Perc", "Stand_Dens", "Stand_BA", "Stand_StemBio", "Stand_CrownCl")

#View new column names in the dataset
colnames(vriClean@data) <- newNames
head(vriClean@data)

#Choose a variable for the analysis and remove unknowns or 'NA' values:
#First, I choose to examine Tree Stand Height (i.e. "Stand_HT")
vriClean <- vriClean[!is.na(vriClean@data$Stand_StemBio), ]


#Notice that there are a lot of 0's in this dataset. If you decide to remove them, use the following line:
VRI.no0 <-  vriClean[which(vriClean$Stand_StemBio > 0), ]

head(VRI.no0$Stand_StemBio)

#clipped elevation to watershed boundary
clipped_elev <- elev[vriClean,]


########################
#Use this tool to select a palette for figure outputs
#BrBG is a colour-blind friendly palette, fits with the theme of our study (forests) and makes the figures easy to interpret
tmaptools::palette_explorer()

tmap_mode("view")
tmap_mode("plot")


#Create choropleth map of height
map_Bio <- tm_shape(VRI.no0) +
  tm_polygons(col = "Stand_StemBio",
              title = "Tree Stand Biomass (tonnes/ha)",
              style = "jenks",
              palette = "Greens", n = 6) +
  tm_legend(legend.position = c("LEFT", "BOTTOM")) + tm_compass(type="arrow", position=c("LEFT","TOP"), show.labels = 1) +
  tm_scale_bar(position=c("RIGHT","BOTTOM")) 

map_Bio

##---------------End of Code from Lab 3-----------------##



##------------- MAP Elevation data in the Study Areas---------------##
head(elev)
# Elevation point data plotted on study area. Colours vary by elevation
m1 <- tm_shape(VRI.no0) +
  tm_polygons() +
  tm_shape(elev) +
  tm_dots(col="grid_code", palette = "OrRd", 
          title="Elevation (metres)", size=0.4) + 
  tm_legend(legend.outside=FALSE) + 
  tm_compass(type="arrow", position=c("LEFT", "TOP"), show.labels = 1) +
  tm_scale_bar(position =c("LEFT", "BOTTOM"))
m1


#Elevation point data and Biomass chloropleth together

m2 <- tm_shape(VRI.no0) +
  tm_polygons(col = "Stand_StemBio",
              title = "Tree Stand Biomass \n (tonnes/ha)",
              style = "fisher",
              palette = "-OrRd", n = 8) +
  tm_shape(clipped_elev) +
  tm_dots(col="grid_code", palette = "OrRd", 
          title="Elevation (metres)", size=0.1) + 
  tm_legend(legend.outside=FALSE) + 
  tm_compass(type="arrow", position=c("LEFT", "TOP"), show.labels = 1) +
  tm_scale_bar(position =c("LEFT", "BOTTOM"))
m2


#Same map, different colours
m3 <- tm_shape(VRI.no0) +
  tm_polygons(col = "Stand_StemBio",
              title = "Tree Stand \n Biomass \n (tonnes/ha)",
              style = "fisher",
              palette = "Greens", n = 8) +
  tm_shape(clipped_elev) +
  tm_dots(col="grid_code", palette = "-RdBu", 
          title="Elevation (metres)", size=0.05) + 
  tm_legend(legend.outside=TRUE) + 
  tm_compass(type="arrow", position=c("LEFT", "TOP"), show.labels = 1) +
  tm_scale_bar(position =c("LEFT", "BOTTOM"))
m3

#Same map, different colours
m4 <- tm_shape(vriClean) +
  tm_polygons(col = "Stand_Age",
              title = "Tree Stand \n Age (Years)",
              style = "fisher",
              palette = "Greens", n = 8) +
  tm_legend(legend.outside=TRUE) + 
  tm_compass(type="arrow", position=c("LEFT", "TOP"), show.labels = 1) +
  tm_scale_bar(position =c("LEFT", "BOTTOM"))
m4



#################################################################

##--------------Lab 1 Descriptive Stats----------------##

#################################################################

library("rgdal")
library("lubridate")
library("e1071")
library("gtable")
library("gridExtra")
library("grid")
library("ggplot2")
library("dplyr")
library("bcmaps")
library('bcmapsdata', repos='https://bcgov.github.io/drat/')
library("raster")
library("maps")
library("tmap")
library("rgeos")

##### ELEVATION DESCRIPTIVE STATS ##########

#make elev numeric:
clipped_elev$grid_code <- as.numeric(clipped_elev$grid_code)

# Range of Elevation measurements total (some outside of watershed boundaries)
elev_range_all <- range(elev$grid_code)
elev_range_all
# 53.85 838.49 meters

nrow(clipped_elev)
#378
nrow(elev)
#487

# Range of Elevation measurements total (some outside of watershed boundaries)
clipped_elev_range_all <- range(clipped_elev$grid_code)
clipped_elev_range_all
# 154.45 838.49 meters

# Sample size - elevation points
clipped_elev$grid_code <- as.numeric(clipped_elev$grid_code)
n_elev <- nrow(clipped_elev)
n_elev

### I CANT FIGURE OUT HOW TO CLIP BOUNDARY SO ELEVATION DESCRIPTIVE STATS ONLY CALCULATED ON VALUES WITHIN THE WATERSHED.... THESE STATS WILL CHANGE WHEN I DO.

# Range of Elevation measurements total (some outside of watershed boundaries)
elev_range_all <- range(clipped_elev$grid_code)
# 53.85 to 838.49 meters

# Mean
elev_mean <- mean(clipped_elev$grid_code)
elev_mean 

#Median
elev_median <- median(clipped_elev$grid_code)
elev_median

#Mode
elev_mode <- as.numeric(names(sort(table(clipped_elev$grid_code), decreasing = TRUE))[1])
elev_mode 

#Standard Deviation
elev_sd <- sd(clipped_elev$grid_code, na.rm = TRUE) #Calculate the SD, ignoring NA values
elev_sd 

#Skewness
elev_skew <- skewness(clipped_elev$grid_code, na.rm = TRUE)[1]
#elev_skew <-formatC( round(elev_skew, 3 ), format='f', digits=2 )
elev_skew

#Kurtosis
elev_kurt <- kurtosis(clipped_elev$grid_code, na.rm = TRUE)[1]
#kurtPop <-formatC( round(kurtPop, 3 ), format='f', digits=2 )
elev_kurt

#CoV
elev_CoV <- (elev_sd / elev_mean) * 100
#CoVPop <-formatC( round(CoVPop, 3 ), format='f', digits=2 )
elev_CoV 

#Normal distribution test

#Once I remove the zeros this should run
elev_norm_PVAL <- shapiro.test(clipped_elev$grid_code)$p.value
#normPop_PVAL <- formatC(normPop_PVAL, format = "e", digits = 2) 
elev_norm_PVAL

#Histogram

elev1 <- as.data.frame(elev)

#have a look elev as a regular dataframe 

elev1

#GGplot histograms
hist_elev <- ggplot(elev1, aes(x = grid_code)) + 
  geom_histogram(bins = 10, color = "black", fill = "blue") + labs(title = "Frequency of Elevations (metres)", x = "Elevation (metres)", y = "Frequency of occurrence") + #label plot, x axis, y axis
  theme_classic() + #set the theme to classic (removes background and borders etc.)
  theme(plot.title = element_text(face = "bold", hjust = 0.5)) #set title to center 
#scale_x_continuous(breaks = seq(0, 8500, by = 100)))
hist_elev


######### BIOMASS DESCRIPTIVE STATS ############

View(VRI.no0)

#make Biomass estimates numeric:
#VRI.no0$Stand_StemBio <- (as.numeric(unlist(VRI.no0$Stand_StemBio))

# Sample size - polygons with Biomass measurements

VRI.no0$Stand_StemBio <- as.numeric(VRI.no0$Stand_StemBio)
n_Biomass <- nrow(VRI.no0)
n_Biomass

#With Zeros, to see how many polygons cleaned out
n_Biomass_0 <- nrow(vriClean)

# Range of Biomass estimates
Biomass_range_all <- range(VRI.no0$Stand_StemBio)
Biomass_range_all

# Mean
Biomass_mean <- mean(VRI.no0$Stand_StemBio)
Biomass_mean 

#Median
Biomass_median <- median(VRI.no0$Stand_StemBio)
Biomass_median

#Mode
Biomass_mode <- as.numeric(names(sort(table(VRI.no0$Stand_StemBio), decreasing = TRUE))[1])
Biomass_mode 

#Standard Deviation
Biomass_sd <- sd(VRI.no0$Stand_StemBio, na.rm = TRUE) #Calculate the SD, ignoring NA values
Biomass_sd 

#Skewness
Biomass_skew <- skewness(VRI.no0$Stand_StemBio, na.rm = TRUE)[1]
#elev_skew <-formatC( round(elev_skew, 3 ), format='f', digits=2 )
Biomass_skew

#Kurtosis
Biomass_kurt <- kurtosis(VRI.no0$Stand_StemBio, na.rm = TRUE)[1]
#kurtPop <-formatC( round(kurtPop, 3 ), format='f', digits=2 )
Biomass_kurt

#CoV
Biomass_CoV <- (Biomass_sd / Biomass_mean) * 100
#CoVPop <-formatC( round(CoVPop, 3 ), format='f', digits=2 )
Biomass_CoV 

#Normal distribution test
Biomass_norm_PVAL <- shapiro.test(VRI.no0$Stand_StemBio)$p.value
#normPop_PVAL <- formatC(normPop_PVAL, format = "e", digits = 2) 
Biomass_norm_PVAL


#before zeros removed this is definately not normal
qqnorm(vriClean$Stand_StemBio, pch = 1, frame = FALSE)
qqline(vriClean$Stand_StemBio, col = "steelblue", lwd = 2)

#before zeros removed this is definately not normal
qqnorm(VRI.no0$Stand_StemBio, pch = 1, frame = FALSE)
qqline(VRI.no0$Stand_StemBio, col = "red", lwd = 2)



#GGplot histograms: VRI

View(VRI.no0)

vriClean2 <- as.data.frame(VRI.no0)

View(vriClean2)

hist_VRI <- ggplot(vriClean2, aes(x = Stand_StemBio)) +  
  geom_histogram(bins = 10, color = "black", fill = "orange", binwidth = 10) + labs(title = "Frequency of Biomass Tonnage (Tonnes)", x = "Stand Stem Biomass (Tonnes)", y = "Frequency of occurrence") + #label plot, x axis, y axis
  theme_classic() + #set the theme to classic (removes background and borders etc.)
  theme(plot.title = element_text(face = "bold", hjust = 0.5)) + scale_x_continuous(breaks = seq(0, 8500, by = 100))
hist_VRI



######### TABLE FOR DESCRIPTIVE STATS ############

#Create a table of descriptive stats

Samples = c("Tree Biomass \n (tonnes/ha)", "Elevation \n (metres)") #Create an object for the labels
Samples
Mean = c(Biomass_mean, elev_mean) #Create an object for the means
Mean <- formatC( round(Mean, 3 ), format = 'f', digits = 1 )
SD = c(Biomass_sd, elev_sd) #Create an object for the standard deviations
SD <-formatC( round(SD, 3 ), format='f', digits=1 )
Median = c(Biomass_median, elev_median) #Create an object for the medians
Median <-formatC( round(Median, 3 ), format='f', digits=1)
Mode <- c(Biomass_mode, elev_mode) #Create an object for the modes
Mode <-formatC( round(Mode, 3 ), format='f', digits=1)
Skewness <- c(Biomass_skew, elev_skew) #Create an object for the skewness
Skewness  <-formatC( round(Skewness, 3 ), format='f', digits=1)
Kurtosis <- c(Biomass_kurt, elev_kurt) #Create an object for the kurtosis
Kurtosis  <-formatC( round(Kurtosis, 3 ), format='f', digits=1)
CoV<- c(Biomass_CoV, elev_CoV) #Create an object for the CoV
CoV <-formatC( round(CoV, 3 ), format='f', digits=1)
Normality <- c("< 0.0001", "< 0.0001") #Create an object for the normality
Normality
Range <- c(Biomass_range_all , elev_range_all)
Range <-formatC( round(Range, 3 ), format='f', digits=1)
SampleSize <- c(n_Biomass, n_elev)
SampleSize

##Check table values for sigfigs?

data.for.table1 = data.frame(Samples, SampleSize, Median, Mode, Mean, SD)
data.for.table1
data.for.table2 = data.frame(Samples, Skewness, Kurtosis, CoV, Normality)
data.for.table2


colnames(data.for.table1) 
colnames(data.for.table2)

names(data.for.table1)[names(data.for.table1) == "Samples"] <- "Datasets"
names(data.for.table1)[names(data.for.table1) == "SD"] <- "Standard Deviation"
names(data.for.table1)[names(data.for.table1) == "SampleSize"] <- " Sample Size"

names(data.for.table2)[names(data.for.table2) == "Samples"] <- "Datasets"
names(data.for.table2)[names(data.for.table2) == "CoV"] <- "Coefficient \n of Variation"
names(data.for.table2)[names(data.for.table2) == "Normality"] <- "Shapiro Normality \n Test (p-values)"

colnames(data.for.table1) 
colnames(data.for.table2)
#Make table 1
table1 <- tableGrob(data.for.table1, rows = c("","")) #make a table "Graphical Object" (GrOb) 
table1
#add a blank row to our table and place our figure caption
t1Caption <- textGrob("Table 1: Descriptive Statistics for VRI Biomass and Elevation Data", gp = gpar(fontsize = 12))
padding <- unit(5, "mm")


table1 <- gtable_add_rows(table1, 
                          heights = grobHeight(t1Caption) + padding, 
                          pos = 0)

table1 <- gtable_add_grob(table1,
                          t1Caption, t = 1, l = 1, r = ncol(data.for.table1) + 1)


table2 <- tableGrob(data.for.table2, rows = c("",""))
t2Caption <- textGrob("Table 2: Measures of Dispersion and Relative Position for \n VRI Biomass and Elevation Data", gp = gpar(fontsize = 12))
padding <- unit(5, "mm")

table2 <- gtable_add_rows(table2, 
                          heights = grobHeight(t2Caption) + padding, 
                          pos = 0)

table2 <- gtable_add_grob(table2,
                          t2Caption, t = 1, l = 1, r = ncol(data.for.table2) + 1)

grid.arrange(table1, newpage = TRUE)
grid.arrange(table2, newpage = TRUE)


#Printing a table (You can use the same setup for printing other types of objects (see ?png))
png("Output_Table1.png") #Create an object to print the table to
grid.arrange(table1, newpage = TRUE)
#dev.off closes a file
dev.off() #Print table
#check working directory area to find this png file that you can put in your final assignment
#if you ever open your png and see blank then you forgot dev.off

png("Output_Table2.png") #Create an object to print the table to
grid.arrange(table2, newpage = TRUE) #Create table
dev.off()


####################################################################

##--------------EXAMINE OBJECTIVE 1---------------------##
##-------------- Lab 3 Spatial Autocorrelation --------------##

####################################################################

## NEIGHBOURHOOD WEIGHTS MATRIX CODE:
# First we will identify which polygons 'neighbour' each other

#Queens weight - Forest Height
# Identify polygons that share edges (i.e. define the neighbourhoods)
vri.nb <- poly2nb(VRI.no0)

# Convert the neighbourhood lists from above into a 'line graph' or 'network graph.'
vri.net <- nb2lines(vri.nb, coords=coordinates(VRI.no0))
crs(vri.net) <- crs(VRI.no0)

#Visualize the network graph
tm_shape(VRI.no0) + tm_borders(col='grey') +
  tm_layout(main.title = "Tree Stand Biomass \n Using Queen's Weight Matrix",
            main.title.position = "center",
            main.title.color = "black") + 
  tm_shape(vri.net) + tm_lines(col='black', lwd = 1) +
  tm_compass(type="arrow", position=c("LEFT", "top"), show.labels = 1) + #this adds a north arrow
  tm_scale_bar(position =c("left", "bottom")) # this adds a scale bar
#NOT SURE IF I NEED TO RUN THE ROOKS SEPERATELY AND OVERLAY... Probably not as I am not comparing them, just picking one and going for it. 
# Rooks weight on top of Queen's weight connections for Forest Stand Height

########################

#Make the weights matrix
?nb2listw

vri.lw <- nb2listw(vri.nb, zero.policy = TRUE, style = "W")
print.listw(vri.lw, zero.policy = TRUE)


########################

##Global MORANS I CODE

########################
#Calculate a Global Moran's I statistic which is the most common way for testing spatial autocorrelation. 
# i = an object in a landscape
# j = the object i's neighbours
# but who are your neighbours?

mi <- moran.test(VRI.no0$Stand_StemBio, vri.lw, zero.policy = TRUE)
mi

moran.range <- function(lw) {
  wmat <- listw2mat(lw)
  return(range(eigen((wmat + t(wmat))/2)$values))
}
moran.range(vri.lw)
#well that took a long time but ran in about 15 - 30 mins/


mI <- mi$estimate[[1]]
eI <- mi$estimate[[2]]
var <- mi$estimate[[3]]


# Moran I statistic standard deviate = 41.939, p-value < 2.2e-16

#calculate z score
z <- ((mI - eI)/(sqrt((var))))

mI
eI
var
z

########################  

##LOCAL MORANS I CODE

########################  
#Run's the local moran's I on our data and gives the local weights matrix we've calculated.

VRI.no0_clean <- na.omit(VRI.no0)
lisa.test <- localmoran(VRI.no0$Stand_StemBio, vri.lw)

#This extracts the relevant information
VRI.no0$Ii <- lisa.test[,1]
VRI.no0$E.Ii<- lisa.test[,2]
VRI.no0$Var.Ii<- lisa.test[,3]
#look up if this is a 1 or 2 tailed test, figure out how to get the test you want & make sure you know how to interpret it. 
VRI.no0$Z.Ii<- lisa.test[,4]
VRI.no0$P<- lisa.test[,5]

########################

#Map Local Moran's I

map_LISA2 <- tm_shape(VRI.no0) + 
  tm_polygons(col = "P", 
              title = " LMI Z test P values", 
              breaks = c(0.000, 0.05, 1.000),
              style = "fixed", 
              palette = "RdBu", n = 10, stretch = FALSE) +
  tm_layout(main.title = "LISA Test using Tree Stand Biomass",
            main.title.position = "center",
            main.title.color = "black") +   
  tm_compass(type="arrow", position=c("right", "top"), show.labels = 1) +
  tm_scale_bar(position = c("left", "top")) 
map_LISA2

# Moran plot:
moran.plot(VRI.no0$Stand_StemBio, vri.lw, zero.policy=TRUE, spChk=NULL, labels=NULL, xlab="Tree Stand Biomass Value", ylab="Neighbour's Value", quiet=NULL)
?moran.plot

####################################################################

##---------------Lab 2 Point Pattern Analysis---------------##

##################################################################

library(spatstat)
library(rgdal)
library(maptools)
library(raster)      
library(sp)           
library(plyr)                                                        
library(lubridate)

#make sure coordinates are correct
Coords <- clipped_elev[,c("grid_code")]
crs <- CRS("+init=epsg:26910") 
checkCRS <- proj4string(VRI.no0)
checkCRS
library("sf")

checkCRS <- st_crs(VRI.no0)
checkCRS 

# check clipped_elev and VRI.no0 are both class: spatial points data frame
class(clipped_elev)
class(VRI.no0)


#this plots the polygons and points as blk & white and difficult to look at.... need to apply more to it 

plot(VRI.no0)
plot(clipped_elev, col = "red", add = TRUE)

#this just shows the number of occurances; which in our case is mostly 1 because they are unique elevation data points.... 
#convert the elevation data type to factor
clipped_elev@data$grid_code <- as.factor(clipped_elev@data$grid_code)
levels(clipped_elev$grid_code)

kma <- clipped_elev
kma$x <- coordinates(kma)[,1]
kma$y <- coordinates(kma)[,2]


kma

#check for and remove duplicated points
#first, finds zero distance among points to see if there are any duplicates

#This removes crimes that happened at the same spot- messes up data -
#investigate a way to keep duplicates 

zd <- zerodist(kma)
zd

#if there are duplicates, remove them
kma <- remove.duplicates(kma)

#crude method to create study area-> We should find a better way 
#create an "extent" object which can be used to create the observation window for spatstat

kma.ext <- as.matrix(extent(clipped_elev)) 

##################################################################


#create ppp object from spatstat
kma.ppp <- ppp(x = kma$x, y = kma$y, window = window)


#observation window
window <- as.owin(list(xrange = kma.ext[1,], yrange = kma.ext[2,]))


#Making this obj to run stats on 
#create ppp oject from spatstat
kma.ppp <- ppp(x = kma$x, y = kma$y, window = window)



#####
##K-FUNCTION 
#basic k-function (for a visual of how things change at different scales of view)
k.fun <- Kest(kma.ppp, correction = "Ripley")
plot(k.fun)

#use simulation to test the point pattern against CSR
k.fun.e <- envelope(kma.ppp, Kest, nsim = 99, correction = "Ripley")
plot(k.fun.e)

#####

##Nearest Neighbour Distance

#calculate the distances between each point
nearestNeighbour <- nndist(kma.ppp)

##Convert the nearestNeighbor object into a dataframe.
nearestNeighbour=as.data.frame(as.numeric(nearestNeighbour))

##Change the column name to "Distance"
colnames(nearestNeighbour) = "Distance"

##Calculate the nearest neighbor statistic to test for a random spatial distribution.
N.nnd <- sum(kma.ppp$n) #n = number of points in the dataset
N.nnd #378

#double check nrow is the same as N.nnd
nrow(clipped_elev)

#Mean nnd #double check if / 1000 is right
nnd = (((sum(nearestNeighbour$Distance))/N.nnd)/1000)
nnd  #0.423662
#SIMILAR TO quadrat analysis this will be for us to calculate
#mean nearest neighbour for random spatial distribution

#Define study area as VRI.no0 boundary
library(raster)

crs(VRI.no0)
studyArea <- area(VRI.no0) / 1000000
#View(studyArea)
studyArea.df <- as.data.frame(studyArea)
colnames(studyArea.df) <- c("area")
studyArea.df <- as.data.frame(studyArea.df)
studyArea.km2 <- sum(studyArea.df$area)
studyArea.km2 #207.8425 km^2

mean.neighborhoodArea <- mean(studyArea.df$area)
mean.neighborhoodArea #0.04252968 km^2
#is that the mean area per polygon???? how did I get this number and what does it mean?

pointDensity <- N.nnd / (studyArea.km2) #number of points / area
pointDensity
#1.818685

r.nnd = ((1)/(2*(sqrt(pointDensity))))
r.nnd

d.nnd = ((1.07453)/(sqrt(pointDensity)))
d.nnd

R = (nnd / r.nnd)
R

SE.NND <- ((0.26136)/(sqrt((N.nnd)*(pointDensity))))
SE.NND

z = (((nnd) - r.nnd)/ (SE.NND))
z

#Now use your values to compare to NNDc and NNDd to see if we are more clustered, random, or dispsered
#interpret our results to the standards for comparisons and decide the distribution of our points. 
#c.nnd = 0,  and d.nnd = 0.43569 (r.nnd = 0.202737)
#then nnd = 243.26 is a very high mean nearest neighbor distance meaning the points are dispersed. 






####################################################################

##-------------- Lab 4 Spatial Interpolation ----------------##

####################################################################


##--Code from Lab 5 prepping for interpolation--##

#Create a grid called grd to use in your interpolation
# Create an empty grid where n is the total number of cells
grd <- as.data.frame(spsample(elev, "regular", n=50000))
names(grd)       <- c("X", "Y")
coordinates(grd) <- c("X", "Y")
gridded(grd)     <- TRUE  # Create SpatialPixel object
fullgrid(grd)    <- TRUE  # Create SpatialGrid object
proj4string(grd) <- proj4string(elev)


##################################################

library(rgdal)
library(spatstat)  # Used for the dirichlet tessellation function
library(maptools)  # Used for conversion from SPDF to ppp
library(tmap)
#install.packages("gstat")
library(gstat)
library(raster)    # Used to clip out thiessen polygons
library(sp)

##################################################

##KRIGING CODE (or maybe IDW... look at literature & decide)

###### Spatial Interpolation with Kriging ########

#similar to what we had to do with the IDW, we will have to create a base formula that the variogram will run off of, 'value ~ 1'
#then we build a base semivariogram with our data and then we use cloud = FALSE so R doesnt crash
#this will be the basic template of a variogram, then we will fit the variogram and then we will use to the vgm to start specifying things like the sill, range and nugget. 
#to know the different types of models we can specify, do ?vgm
# we will have to start playing around with the numbers to fit the model.
#jason switches range to 10000 and 30000 
#what we want is to have the line to best fit the distribution of points in the variogram. capture the data as a whole as best we can. 
#this is the critical part of kriging. for sure try Sph Exp and Gau

#once we fit the semivariogram we can use this to then interpolate our surface. We give it the base formula, the points to interpolate from, then the grd which is the same empty grid we used in IDW and then we can provide our fitted semivarigram. 
#then we will convert it into a raster, clip the raster then map it. 

# Define the trend model
f.0 <- as.formula(grid_code ~ 1) 

#Create variogram
var.smpl1 <- variogram(f.0, elev, cloud = FALSE) 
dat.fit1  <- fit.variogram(var.smpl1, fit.ranges = FALSE, fit.sills = FALSE, vgm(psill=44500, model="Exp", range=6300, nugget=0))
plot(var.smpl1, dat.fit1)#changing the range to 10,000 starts to look at lot better


# Perform the krige interpolation (note the use of the variogram model
# created in the earlier step)

#Perform kriging using exponential model
dat.krg1 <- krige(f.0, elev, grd, dat.fit1)

# Convert kriged surface to a raster object for clipping
r <- raster(dat.krg1)
r.m <- mask(r, VRI.no0)

# Plot the map of kriged surface
Krig_plot_exp <- tm_shape(r.m) + 
  tm_raster(n=10, palette="-RdBu",  
            title="Predicted \n Elevation (metres)") +
  tm_shape(elev) + tm_dots(col="grid_code", palette = "black", title="Elevation (metres)", size=0.05) +
  tm_legend(legend.outside=FALSE, position = c("LEFT", "BOTTOM")) + 
  tm_compass(type="arrow", position=c("LEFT", "TOP"), show.labels = 1) +
  tm_scale_bar(position =c("LEFT", "BOTTOM"))

Krig_plot_exp

## I can change the colours of the elevation points above (maybe black is best).
## How can I add a border to the watershed polygons?



# Plot the Variance of kriged surface

r   <- raster(dat.krg1, layer="var1.var")
r.m <- mask(r, VRI.no0)

krig_var_exp <- tm_shape(r.m) + 
  tm_raster(n=7, palette ="-RdBu",
            title="Variance Elevation map \n(metres)") +tm_shape(elev) + tm_dots(size=0.05) +
  tm_legend(legend.outside=FALSE, position = c("LEFT", "BOTTOM")) +
  tm_compass(type="arrow", position=c("LEFT", "TOP"), show.labels = 1) +
  tm_scale_bar(position =c("LEFT", "BOTTOM"))

krig_var_exp

#The variance seems very high.....2,000-6,000 meters of variance in most areas... 
#Try manual breaks?
#does this make sense that the variance would be in meters? 
#Check the original code they gave us.


#Plot 95% CI map for kriged elevation surface

r   <- sqrt(raster(dat.krg1, layer="var1.var")) * 1.96
r.m <- mask(r, VRI.no0)

Exp_Elev_CI <- tm_shape(r.m) + 
  tm_raster(n=7, palette ="-RdBu",
            title="95% CI \n Elevation map (metres)") +tm_shape(elev) + tm_dots(size=0.05) +
  tm_legend(legend.outside=FALSE, position = c("LEFT", "BOTTOM")) +
  tm_compass(type="arrow", position=c("LEFT", "TOP"), show.labels = 1) +
  tm_scale_bar(position =c("LEFT", "BOTTOM"))

Exp_Elev_CI



##################################################

##---------Combine Datasets--------------########

##################################################


#see minutes 35 or 38 where this part of lab video starts.

#These steps will help you combine the outputs 
#from your spatial interpolation with your income data.
#Convert your interpolation into a raster and map it:
r <- raster(dat.krg1)
r.m <- mask(r, VRI.no0)

surfaceMap <- tm_shape(r.m) + 
  tm_raster(n=5,palette = "-plasma",
            title="Elev (m)") +
  tm_shape(elev) + tm_dots(size=0.03) +
  tm_compass(type="arrow", position=c("RIGHT", "TOP"), show.labels = 1) +
  tm_scale_bar(position =c("LEFT", "BOTTOM"))
surfaceMap

#factor is the number of cells it collapases into one. look into it a bit more. 
#If you have too many cells, 
#you can reduce the number by aggregating values
#agg <- aggregate(yourRasterFromKriging, fact=??, fun=mean)
#For instance if you have a nice surface with high resolution, but it takes forever to combine, then aggregating can help change it so that you dont have to redo your interpolation method but can get a quicker output for raster....

#Extract average elev for each polygon
vriClean$Elev <- extract(r, vriClean, fun = mean)[,1]

##------------ADD REGRESSION CODE-----------##

######Linear Regression##########
#Let's say your dataset with both Elev and Height are stored in a dataset called VRI.
#Plot Height and Elev from the VRI dataset you created
plot(vriClean$Stand_StemBio ~ vriClean$Elev)

##IS THIS OKAY? (ABOVE) I HAD TO CHANGE TO VRI$Elev to get it to run....######

#Notice that there are a lot of 0's in this dataset. If you decide to remove them, use the following line:
VRI.no0 <-  vriClean[which(vriClean$Stand_StemBio > 0), ]
VRI.no0 <-  VRI.no0[which(VRI.no0$Elev > 0), ]


##------------ADD REGRESSION CODE-----------##

######Linear Regression##########
#Let's say your dataset with both Elev and Height are stored in a dataset called VRI.
#Plot Height and Elev from the VRI dataset you created
plot(VRI.no0$Stand_StemBio ~ VRI.no0$Elev)

#Perform a linear regression on the two variables. You should decide which one is dependent.
lm.model <- lm(VRI.no0$Stand_StemBio ~ VRI.no0$Elev)

#Add the regression model to the plot you created
plot(VRI.no0$Stand_StemBio ~ VRI.no0$Elev)
abline(lm.model, col = "red")

#Get the summary of the results
summary(lm.model)

#add the fitted values to your spatialpolygon dataframe
VRI.no0$predictlm <- lm.model$fitted.values

#You want to determine if the model residuals are spatially clustered. 
#add the residuals to your spatialpolygon dataframe
VRI.no0$residuals <- residuals.lm(lm.model)

#Observe the result to make sure it looks correct
head(VRI.no0@data)

#Now, create choropleth map of residuals
map_resid <- tm_shape(VRI.no0) +
  tm_polygons(col = "residuals",
              title = "Tree Stand Biomass Residuals",
              breaks=c(-Inf, -250, -200, -150, -100, - 50, 0.0, 50, 100, 150, 200, 400, 600, Inf),
              palette = "-RdBu") +
  tm_compass(type="arrow", position=c("RIGHT", "TOP"), show.labels = 1) +
  tm_scale_bar(position =c("LEFT", "BOTTOM"))

map_resid



########################

##Global MORANS I OF SLR RESIDUALS

########################
#Calculate a Global Moran's I statistic which is the most common way for testing spatial autocorrelation. 
# i = an object in a landscape
# j = the object i's neighbours
# but who are your neighbours?


mi <- moran.test(VRI.no0$residuals, vri.lw, zero.policy = TRUE)
mi

moran.range <- function(lw) {
  wmat <- listw2mat(lw)
  return(range(eigen((wmat + t(wmat))/2)$values))
}
moran.range(vri.lw)
#well that took a long time but ran in about 15 - 30 mins/


mI <- mi$estimate[[1]]
eI <- mi$estimate[[2]]
var <- mi$estimate[[3]]


# Moran I statistic standard deviate = 41.939, p-value < 2.2e-16

#calculate z score
z <- ((mI - eI)/(sqrt((var))))

mI
eI
var
z

##################################################


##------------- GWR -----------------------##

####Geographically Weighted Regression
#Let's say you are continuing with 
#your data from the regression analysis. 
#The first thing you need to do is to add the 
#polygon coordinates to the spatialpolygondataframe.
#You can obtain the coordinates using the 
#"coordinates" function from the sp library
VRI.no0.coords <- sp::coordinates(VRI.no0)
#Observe the result:
head(VRI.no0.coords)
#Now add the coordinates back to the spatialpolygondataframe
VRI.no0$X <- VRI.no0.coords[,1]
VRI.no0$Y <- VRI.no0.coords[,2]




###############################################################################


###Determine the bandwidth for GWR: this will take a while
GWRbandwidth <- gwr.sel(VRI.no0$Stand_StemBio ~ VRI.no0$Elev, 
                        data=VRI.no0, coords=cbind(VRI.no0$X,VRI.no0$Y),adapt=T) 

GWRbandwidth

###Perform GWR on the two variables with the bandwidth determined above
###This will take a looooooong while
gwr.model = gwr(VRI.no0$Stand_StemBio ~ VRI.no0$Elev, 
                data=VRI.no0, coords=cbind(VRI.no0$X,VRI.no0$Y), 
                adapt=GWRbandwidth, hatmatrix=TRUE, se.fit=TRUE) 

#Print the results of the model
gwr.model

#Look at the results in detail
results<-as.data.frame(gwr.model$SDF)
head(results)

#Now for the magic. Let's add our local r-square values to the map
VRI.no0$localr <- results$localR2

#Create choropleth map of r-square values
map_r2 <- tm_shape(VRI.no0) +
  tm_polygons(col = "localr",
              title = "R2 values",
              breaks = c(-Inf, 0, 0.25, 0.50, 0.75, 1.00),
              palette = "-RdBu", n = 5)  +
  tm_compass(type="arrow", position=c("RIGHT", "TOP"), show.labels = 1) +
  tm_scale_bar(position =c("LEFT", "BOTTOM"))
map_r2

map_r2 <- tm_shape(VRI.no0) +
  tm_polygons(col = "localr",
              title = "R2 values",
              breaks=c(-Inf, 0.00, 0.05, 0.1, 0.2, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1),
              palette = "-RdBu", n = 10)  +
  tm_compass(type="arrow", position=c("RIGHT", "TOP"), show.labels = 1) +
  tm_scale_bar(position =c("LEFT", "BOTTOM"))
map_r2

#Time for more magic. Let's map the coefficients
VRI.no0$coeff <- results$VRI.no0.Elev
#Create choropleth map of the coefficients


map_coef <- tm_shape(VRI.no0) +
  tm_polygons(col = "coeff",
              title = "Coefficients",
              breaks=c(-Inf, -50, -20, -10, -5, -2.5, 0, 2.5, 5,  10, 20, 50),
              #style = "fisher",
              palette = "-RdBu") +
  tm_compass(type="arrow", position=c("right", "top"), show.labels = 1) +
  tm_scale_bar(position =c("left", "bottom"))
map_coef


map_coef <- tm_shape(VRI.no0) +
  tm_polygons(col = "coeff",
              title = "Coefficients",
              breaks=c( -Inf, -100.0, -50.0, -25.0,-5.0, 0, 5.0, 25.0, 50.0, 100.0),
              palette = "-RdBu", n = 6)  +
  tm_compass(type="arrow", position=c("RIGHT", "TOP"), show.labels = 1) +
  tm_scale_bar(position =c("LEFT", "BOTTOM"))
map_coef


