library(maps)
library(ggmap)
library(stringr)
library(rgdal)
library(sp)       # for spatial objects
library(leaflet)  # for interactive maps (NOT leafletR here)
library(dplyr)    # for working with data frames
library(ggplot2)  # for plotting
library(maptools)
library(broom)
require("maptools")
gpclibPermit()

setwd("~/Desktop/Brown_Data_Science/data2020/Midterm") # set working dir
census_df_full = read.csv("Midterm Project/nyc_census.csv") # read in file
census_df_naomit <- na.omit(census_df_full)
colnames(census_df_naomit)[1] <- "id"
census_df_naomit$id <- as.character(census_df_naomit$id)

# Files available: https://www.census.gov/geo/maps-data/data/cbf/cbf_tracts.html
tract <- readOGR(dsn="cb_2016_36_tract_500k", layer = "cb_2016_36_tract_500k")
tract@data$GEOID<-as.character(tract@data$GEOID)
ggtract<-tidy(tract, region = "GEOID") 

# join tabular data
ggtract<-left_join(ggtract, census_df_naomit, by=c("id")) 

# here we limit to non-NA for speed
ggtract_na_omit <- na.omit(ggtract)
#ggtract <- ggtract[grep("Kings|Bronx|New York County|Queens|Richmond", ggtract$geography),]

ggplot() +
    geom_polygon(data = ggtract_na_omit , aes(x=long, y=lat, group = group, fill=Income), color="grey50") +
    scale_fill_gradientn(colours = c("red", "white", "cadetblue"),
                         values = c(1,0.75, .3, .2, .1, 0))+
    coord_map(xlim = c(-74.26, -73.71), ylim = c(40.49,40.92))

ggplot() +
    geom_polygon(data = ggtract_na_omit , aes(x=long, y=lat, group = group, fill=Poverty), color="grey50") +
    scale_fill_gradientn(colours = c("red", "white", "cadetblue"),
                         values = c(1,0.75, .3, .2, .1, 0))+
    coord_map(xlim = c(-74.26, -73.71), ylim = c(40.49,40.92))


#function for creating a Polygons object for 
# input tractname
polyFunc<-function(groupname, dat){
    poly<-filter(dat, id==groupname) %>% 
        select(long, lat)
    return(Polygons(list(Polygon(poly)), groupname))
}



#####################################################
# MEDIAN INCOME
#####################################################

tracts <- distinct(ggtract_na_omit, id, Income)
tractname <- tracts$id
polygons<-lapply(tractname, function(x) polyFunc(x, dat=ggtract_na_omit)) 
sp.polygon<-SpatialPolygons(polygons)
df.polygon<-SpatialPolygonsDataFrame(sp.polygon, 
                                     data=data.frame(row.names=tractname, tracts))
df.polygon <- df.polygon[order(df.polygon$Income),]

popup <- paste0("GEOID: ", df.polygon$id, "<br>", "Median Household Income (USD): ", 
                paste0("$", formatC(as.numeric(df.polygon$Income), format="f", digits=2, big.mark=",")))
pal <- colorNumeric(
    palette = "YlGnBu",
    domain = df.polygon$Income
)

map1<-leaflet() %>%
    addProviderTiles("CartoDB.Positron") %>%
    addPolygons(data = df.polygon, 
                fillColor = ~pal(Income), 
                color = "#b2aeae", # you need to use hex colors
                fillOpacity = 0.7, 
                weight = 0.3, 
                smoothFactor = 0.2,
                popup = popup) %>%
    addLegend(pal = pal, 
              values = df.polygon$Income, 
              position = "bottomright", 
              #title = "Median Household Income",
              labFormat = labelFormat(prefix = "$"))
map1



#####################################################
# POVERTY
#####################################################

tracts <- distinct(ggtract_na_omit, id, Poverty)
tractname <- tracts$id
polygons<-lapply(tractname, function(x) polyFunc(x, dat=ggtract_na_omit)) 
sp.polygon<-SpatialPolygons(polygons)
df.polygon<-SpatialPolygonsDataFrame(sp.polygon, 
                                     data=data.frame(row.names=tractname, tracts))
df.polygon <- df.polygon[order(df.polygon$Poverty),]

popup <- paste0("GEOID: ", df.polygon$id, "<br>", "Poverty Rate: ", 
                paste0(df.polygon$Poverty, "%"))
pal <- colorNumeric(
    palette = "Reds",
    domain = df.polygon$Poverty
)

map2<-leaflet() %>%
    addProviderTiles("CartoDB.Positron") %>%
    addPolygons(data = df.polygon, 
                fillColor = ~pal(Poverty), 
                color = "#b2aeae", # you need to use hex colors
                fillOpacity = 0.7, 
                weight = 0.3, 
                smoothFactor = 0.2,
                popup = popup) %>%
    addLegend(pal = pal, 
              values = df.polygon$Poverty, 
              position = "bottomright", 
              #title = "Poverty Rate",
              labFormat = labelFormat(suffix = "%"))
map2


#####################################################
# PublicWork
#####################################################

tracts <- distinct(ggtract_na_omit, id, PublicWork)
tractname <- tracts$id
polygons<-lapply(tractname, function(x) polyFunc(x, dat=ggtract_na_omit)) 
sp.polygon<-SpatialPolygons(polygons)
df.polygon<-SpatialPolygonsDataFrame(sp.polygon, 
                                     data=data.frame(row.names=tractname, tracts))
df.polygon <- df.polygon[order(df.polygon$PublicWork),]

popup <- paste0("GEOID: ", df.polygon$id, "<br>", "Population employed in public jobs: ", 
                paste0(df.polygon$PublicWork, "%"))
pal <- colorNumeric(
    palette = "Oranges",
    domain = df.polygon$PublicWork
)

map3<-leaflet() %>%
    addProviderTiles("CartoDB.Positron") %>%
    addPolygons(data = df.polygon, 
                fillColor = ~pal(PublicWork), 
                color = "#b2aeae", # you need to use hex colors
                fillOpacity = 0.7, 
                weight = 0.3, 
                smoothFactor = 0.2,
                popup = popup) %>%
    addLegend(pal = pal, 
              values = df.polygon$PublicWork, 
              position = "bottomright", 
              #title =  "Population employed in public jobs:",
              labFormat = labelFormat(suffix = "%"))
map3


#####################################################
# OtherTransp
#####################################################

tracts <- distinct(ggtract_na_omit, id, OtherTransp)
tractname <- tracts$id
polygons<-lapply(tractname, function(x) polyFunc(x, dat=ggtract_na_omit)) 
sp.polygon<-SpatialPolygons(polygons)
df.polygon<-SpatialPolygonsDataFrame(sp.polygon, 
                                     data=data.frame(row.names=tractname, tracts))
df.polygon <- df.polygon[order(df.polygon$OtherTransp),]

popup <- paste0("GEOID: ", df.polygon$id, "<br>", "Population commuting via other means: ", 
                paste0(df.polygon$OtherTransp, "%"))
pal <- colorNumeric(
    palette = "Greens",
    domain = df.polygon$OtherTransp
)

map4<-leaflet() %>%
    addProviderTiles("CartoDB.Positron") %>%
    addPolygons(data = df.polygon, 
                fillColor = ~pal(OtherTransp), 
                color = "#b2aeae", # you need to use hex colors
                fillOpacity = 0.7, 
                weight = 0.3, 
                smoothFactor = 0.2,
                popup = popup) %>%
    addLegend(pal = pal, 
              values = df.polygon$OtherTransp, 
              position = "bottomright", 
              #title =  "Commuting via other means:",
              labFormat = labelFormat(suffix = "%"))
map4


#####################################################
# WorkAtHome
#####################################################

tracts <- distinct(ggtract_na_omit, id, WorkAtHome)
tractname <- tracts$id
polygons<-lapply(tractname, function(x) polyFunc(x, dat=ggtract_na_omit)) 
sp.polygon<-SpatialPolygons(polygons)
df.polygon<-SpatialPolygonsDataFrame(sp.polygon, 
                                     data=data.frame(row.names=tractname, tracts))
df.polygon <- df.polygon[order(df.polygon$WorkAtHome),]

popup <- paste0("GEOID: ", df.polygon$id, "<br>", "Population working at home: ", 
                paste0(df.polygon$WorkAtHome, "%"))
pal <- colorNumeric(
    palette = "Blues",
    domain = df.polygon$WorkAtHome
)

map5<-leaflet() %>%
    addProviderTiles("CartoDB.Positron") %>%
    addPolygons(data = df.polygon, 
                fillColor = ~pal(WorkAtHome), 
                color = "#b2aeae", # you need to use hex colors
                fillOpacity = 0.7, 
                weight = 0.3, 
                smoothFactor = 0.2,
                popup = popup) %>%
    addLegend(pal = pal, 
              values = df.polygon$WorkAtHome, 
              position = "bottomright", 
              #title =  "Population working at home:",
              labFormat = labelFormat(suffix = "%"))
map5


#####################################################
# Professional
#####################################################

tracts <- distinct(ggtract_na_omit, id, Professional)
tractname <- tracts$id
polygons<-lapply(tractname, function(x) polyFunc(x, dat=ggtract_na_omit)) 
sp.polygon<-SpatialPolygons(polygons)
df.polygon<-SpatialPolygonsDataFrame(sp.polygon, 
                                     data=data.frame(row.names=tractname, tracts))
df.polygon <- df.polygon[order(df.polygon$Professional),]

popup <- paste0("GEOID: ", df.polygon$id, "<br>", "Population employed in management, business, science, and arts: ", 
                paste0(df.polygon$Professional, "%"))
pal <- colorNumeric(
    palette = "Purples",
    domain = df.polygon$Professional
)

map6<-leaflet() %>%
    addProviderTiles("CartoDB.Positron") %>%
    addPolygons(data = df.polygon, 
                fillColor = ~pal(Professional), 
                color = "#b2aeae", # you need to use hex colors
                fillOpacity = 0.7, 
                weight = 0.3, 
                smoothFactor = 0.2,
                popup = popup) %>%
    addLegend(pal = pal, 
              values = df.polygon$Professional, 
              position = "bottomright", 
              #title =  "Pop. employed in management, business, science, and arts:",
              labFormat = labelFormat(suffix = "%"))
map6


## OLD ##

#census_df <- census_df_full[,-which(names(census_df_full) %in% c("Borough", 
#                                                                 "IncomeErr",
#                                                                 "IncomePerCap",
#                                                                 "IncomePerCapErr"))]

# ## Census Tract Information No Longer Used
# location_information = read.csv("Midterm Project/census_tract_loc.csv") # read in file
# location_information_df = data.frame(location_information)
# location_information$CensusTract <- substr(location_information$BlockCode, 1,11)
# location_aggregation <- aggregate(cbind(Latitude, Longitude)~CensusTract,location_information,mean)
# 
# census_df_rd2_with_location <- merge(x = census_df_rd2, y = location_aggregation, all.x = TRUE)
# 
# # Testing some other mapping options.
# test <- subset(location_aggregation)
# qmplot(Longitude, Latitude, data = census_df_rd2_with_location,geom = "point")
