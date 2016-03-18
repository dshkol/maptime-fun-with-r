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

str(listings)

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



ggplot(listings, aes)