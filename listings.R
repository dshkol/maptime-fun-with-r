# Required packages for today's workshop
library(ggplot2)
library(dplyr)
library(sp)
library(rgdal)
library(leaflet)


#big file
listings_small <- "http://data.insideairbnb.com/canada/bc/vancouver/2015-12-03/visualisations/listings.csv"

listings_big <- "http://data.insideairbnb.com/canada/bc/vancouver/2015-12-03/data/listings.csv.gz"

# download small file

download.file(listings_small,"listings_small.csv")
#unzip("listings.csv")

# Read in data

listings <- read.csv("listings_small.csv", header = TRUE, stringsAsFactors = FALSE)

# Take a look at the data structure

# A few of these strings are categorical factors. Let's change the type so they're easier to work with and take less memory.

listings$neighbourhood <- as.factor(listings$neighbourhood)
listings$room_type <- as.factor(listings$room_type)

# Before we get into the mapping part, let's take a look at some of the data using the ubiquitous ggplot2 graphics library.

ggplot(listings, aes(factor(room_type))) + geom_bar

ggplot(listings, aes(factor(neighbourhood))) + geom_bar() + coord_flip()

ggplot(listings, aes(factor(neighbourhood), fill = room_type)) + geom_bar() + coord_flip()

# Take a look at some group statistics for neighbourhoods and room types. Group statistics are very well handled using another Hadley-verse package 'dplyr'.


summarise(group_by(listings, neighbourhood), neighbourhood_median = median(price))

# or... using the piping syntax

listings %>% 
  group_by(neighbourhood) %>%
  summarise(neighbourhood_median = median(price))

neighbourhood_prices <- listings %>% 
  group_by(neighbourhood, room_type) %>%
  summarise(median_price = median(price),
            observations = n())

neighbourhood_prices[neighbourhood_prices$observations < 5,] <- NA

ggplot(na.omit(neighbourhood_prices), aes (x = median_price, y = neighbourhood, colour = room_type, size = observations)) + geom_point()

# Pretty-fy the chart
# Reorder the factor levels, so A is at the top of our charts. 
# neighbourhood_prices$neighbourhood <- factor(neighbourhood_prices$neighbourhood, levels=rev(levels(neighbourhood_prices$neighbourhood)))
# Fix labels and legend, add titles.
# ggplot(na.omit(neighbourhood_prices), aes (x = median_price, y = neighbourhood, colour = room_type, size = observations)) + geom_point() + xlab("Median price per night") + ylab("Neighbourhood") + scale_size_continuous(name = "Observations", breaks = c(50,250,500,750)) + scale_colour_discrete(name="Room Type") + ggtitle("AirBnB Nightly Prices by Neighbourhood and Room Type") +theme(plot.title = element_text(face = "bold"))
# ggsave("nightly_prices_breakdown.png")

# Lets make some maps, finally. 

# Using ggmap...

# Make a bounding box for our data

bbox <- make_bbox(longitude, latitude, listings, f = 0.1)
vanMap <- get_map(location = bbox, zoom = 12, maptype = "toner")
ggmap(vanMap) + geom_point(aes(x = longitude, y = latitude, colour = room_type), data = listings)

# Thats messy...

ggmap(vanMap) + geom_point(aes(x = longitude, y = latitude, colour = room_type), alpha = 0.75, size = 1, data = listings) + ggtitle("Map of AirBnB Listings by Room Type") + theme(legend.position = "bottom", axis.text = element_blank(), axis.title = element_blank(), axis.ticks = element_blank(), plot.title = element_text(face = "bold")) + scale_colour_discrete(name = "Room Type")
ggsave("ggmap_example_1.png")

# Still hard to look at. One of the nice things in R is how easy it is to make faceted graphics.

ggmap(vanMap) + geom_point(aes(x = longitude, y = latitude, colour = room_type), alpha = 0.5, size = 0.5, data = listings) + facet_wrap(~room_type) + ggtitle("AirBnB Listings by Room Type") + theme(legend.position = "none", axis.text = element_blank(), axis.title = element_blank(), axis.ticks = element_blank(), plot.title = element_text(face = "bold") ) 
ggsave("ggmap_example_2.png")

# Lets take a look at prices for whole apartments

listings_apt <- listings[listings$room_type == "Entire home/apt",]
ggmap(vanMap) + geom_point(aes(x = longitude, y = latitude, colour = price), alpha = 0.95, size = 1, data = listings_apt) + scale_colour_gradient(low = "lightgrey", high = "darkred", limits = c(100,750)) + ggtitle("AirBnB Listings (Entire Home/Apartment)") + theme(legend.position = "bottom", axis.text = element_blank(), axis.title = element_blank(), axis.ticks = element_blank(), plot.title = element_text(face = "bold") ) 

ggmap(vanMap) + geom_density2d(aes(x = longitude, y = latitude), data = listings_apt) +  stat_density2d(aes(x = longitude, y = latitude, colour = price, alpha = ..level..), geom = "polygon", data = listings_apt) + ggtitle("title") + scale_colour_gradient(low = "lightgrey", high = "darkred")  + theme(legend.position = "bottom", axis.text = element_blank(), axis.title = element_blank(), axis.ticks = element_blank(), plot.title = element_text(face = "bold") ) 

ggsave("ggmap_example_3.png")




# Load neighbourhood shapes

localArea <- readOGR(dsn = "localAreaBoundary", layer = "csg_neighborhood_areas")
localArea <- spTransform(localArea,CRS("+proj=longlat +datum=WGS84"))



# Turn the listings into R Spatial Format
listings.coordinates <- cbind.data.frame(listings$longitude,listings$latitude)
listings.sp <- SpatialPointsDataFrame(listings.coordinates, data = listings)
listings.sp@proj4string <- localArea@proj4string

