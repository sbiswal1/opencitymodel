library(sf)
library(dplyr)
library(readr)
library(tigris)
library(sp)
library(leaflet)
library(colorspace)
#first step is downloading the counties & metro areas of USA using the 
#Tigris library

USCounties<-counties(state=NULL, cb = FALSE, year = 2016)
metro <-core_based_statistical_areas(cb = FALSE, resolution = "500k", year = 2016)
#class(metro
#### Load Treatment facilities csv
Opioids<-read.csv('C:/Opioids/TreatmentCenterDetails-V5.csv')
head(Opioids,10)


Opioids$lat<-as.numeric(as.character(Opioids$lat))
Opioids$lng<-as.numeric(as.character(Opioids$long))
Opioids<-Opioids[complete.cases(Opioids), ]

Opioids
#we can explore how the actual shapefile looks by using the base R plot command
plot(metro)

#how do we transform this information into data insights?
#we have two options either transform into a large spatial dataframe


Opioid_SF<-SpatialPointsDataFrame(coords = cbind(Opioids[c("lng",
                                                           "lat")]), 
                                  data = dplyr::select(Opioids, 
                                                       -c(lng,
                                                          lat)),
                                  proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))


#convert our original crime data and transform it into sf dataframe


Opioids_sf<-st_as_sf(Opioids, coords = c("lng", "lat"), crs = 4326, agr = "constant")
head(Opioids_sf)


#we now transform the recently downloaded file to SF

USCounties_sf<-st_as_sf(USCounties, crs = 4326)
metro_sf<-st_as_sf(metro, crs = 4326)



USCounties_sf<-USCounties_sf%>%st_transform(USCounties_sf,crs = 4326)

metro_sf<-metro_sf%>%st_transform(metro_sf,crs = 4326)

st_crs(metro_sf)
### plotting
plot(USCounties_sf)
plot(Opioids_sf)

#now perform an intersection
inter_county<-st_intersection(Opioids_sf,metro_sf)
inter<-st_intersection(Opioids_sf,USCounties_sf)
st_crs(Opioids_sf)
st_crs(USCounties_sf)

st_write(inter_county, "C:/Opioids/TreatmentFacilities_V5_200418_new.csv", layer_options = "GEOMETRY=AS_XY")

st_write(inter, "C:/Opioids/TreatmentFacilities_V5_200418_new.csv", layer_options = "GEOMETRY=AS_XY")


st_write(USCounties_sf, "C:/Opioids/counties.csv", layer_options = "GEOMETRY=AS_XY")
