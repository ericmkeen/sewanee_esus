################################################################################
# R tutorial 1 (2022)
################################################################################
################################################################################
# Running R code

# calculator (add, subtract, multiply, divide, exponent)

# incomplete commands: 45 +

# getting errors

# use parentheses
2*7 - 2*5 / 2
(2*7 - 2*5) / 2

# unresolved parentheses cause errors

################################################################################
# Setting up scripts

# Open file
# Write stuff in scripts
# Save file
# Put library at the top
# Comments
# Why scripts are awesome

################################################################################
# More basics

# testing equality
1 == 1
1 == 2
1 != 1
1 != 2

# sets of numbers (vectors)
1:10
10:1
c(1,3,5,6)

# built-in functions
sqrt(16)
log(4)
round(43.44)
round(43.44, 1)

mean(1:10)
sd(1:10)

rnorm(1000)
?rnorm
rnorm(n = 1000, mean = 0, sd = 1)
rnorm(n = 1000, mean = 0, sd = 3)

################################################################################
# Packages - think of them like SHOES

install.packages('dplyr')
install.packages('readr')
install.packages('ggplot2')
install.packages('gsheet')
install.packages("wesanderson")
install.packages("leaflet")

library(dplyr)
library(readr)
library(ggplot2)
library(gsheet)
library(leaflet)
library(wesanderson)

wes_palette("Royal1")
wes_palette("Zissou1")
?wes_palette

################################################################################
# The pipe

1:10 %>% mean
16 %>% sqrt
1:100 %>% mean %>% sqrt %>% round
rnorm(10000) %>% hist
rnorm(n = 10000, mean = 0, sd = 3) %>% hist

# PAUSE & TEST: Find errors
# 5 * 6 +
# sqrt(16
# round(100/3,digits+3)

################################################################################
# Variables

3 + 5

# Declaring variables
x <- 3 + 5

# Calling variables

# Using parentheses to print

# Naming variables

# Adding together

# Numeric v character v logical
# can't add them together

#  PAUSE: Finding errors
# my_var < 5

# my_var == 5

#x <- 5
#y <- 1
#X + y

################################################################################
# Read in some data

library(gsheet)

url <- 'https://docs.google.com/spreadsheets/d/1xoecVY2roNzS2gpt8UnvhGhCxrocXjJMpji9eUgiDMw/edit?usp=sharing'

gsheet2tbl(url)

df <- gsheet2tbl(url)

# this is a dataframe

################################################################################
# Make some cool maps

library(leaflet)

leaflet()

leaflet() %>% addTiles()
# zoom in / zoom out

leaflet() %>% addTiles() %>%  addMarkers(data=df)

# Stylize your Tiles
providers

# Satellite
leaflet() %>% addProviderTiles(providers$Esri.WorldImagery) %>%  addMarkers(data=df)

# Topo
leaflet() %>% addProviderTiles(providers$Esri.WorldTopoMap) %>%  addMarkers(data=df)

# Stylize marker
leaflet() %>%
  addProviderTiles(providers$Esri.WorldTopoMap) %>%
  addCircleMarkers(data=df,
                   color = "firebrick",
                   radius = 15)

# Add pop up
leaflet() %>%
  addProviderTiles(providers$Esri.WorldTopoMap) %>%
  addCircleMarkers(data=df,
                   color = "firebrick",
                   radius = 15,
                   popup = 'YSR, yall')


# Mapping specific area, without markers
leaflet() %>%
  addProviderTiles(providers$Esri.OceanBasemap) %>%
  fitBounds(lng1 = -165,
            lng2 = -100,
            lat1 = 10,
            lat2 = 50)

################################################################################
# Geocoding

install.packages('tidygeocoder')
library(tidygeocoder)

# Create a dataframe with address(es)
addresses <- data.frame(name= 'White House',
                        addr = "1600 Pennsylvania Ave NW, Washington, DC")

# Geocode
lat_longs <-
  addresses %>%
  geocode(addr,
          method = 'osm',
          lat = latitude ,
          long = longitude)

# Check it out
lat_longs

# Map it
leaflet(data=lat_longs) %>%
  addProviderTiles(providers$Stamen.Watercolor) %>%
  addMarkers()


################################################################################
# Homework



