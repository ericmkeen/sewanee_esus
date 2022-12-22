################################################################################
################################################################################
# (4) Carbon sequestration
################################################################################
################################################################################

library(dplyr)
library(ggplot2)
library(readr)
library(tidyr)
library(gsheet)
library(leaflet)
library(sf)

################################################################################
# Sample locations

urban_sewanee <- read.csv('nelson_polygon.csv')
sites <- read.csv('nelson-sample-locations.csv')
sites %>% head

leaflet() %>%
  addTiles() %>%
  addPolygons(lng=urban_sewanee$lon, lat=urban_sewanee$lat, col='purple') %>%
  addCircleMarkers(lng=sites$lon, lat=sites$lat)

################################################################################
# Load data

# Tree measurements ============================================================

url <- 'https://docs.google.com/spreadsheets/d/1AWggN3uj1lMIZc061DGonr4GHudSTtjQ3pKTLiskEwM/edit?usp=sharing'
df <- gsheet2tbl(url)

trees <-
  df %>%
  mutate(year = 2022) %>%
  select(year, plot,
         species = `species (generalized)`,
         dbh = `dbh(cm)`)

write.csv(trees, file='nelson_trees.csv', quote=FALSE, row.names=FALSE)

# Biomass conversion coefficients  =============================================

url <- 'https://docs.google.com/spreadsheets/d/1mx-4kfU4j966ORpzuFO0DG_VMmuQ08NR_uIxddJGc1E/edit?usp=sharing'
coefficients <- gsheet2tbl(url)
coefficients

################################################################################

# Biomass regressions
# https://www.fs.usda.gov/research/treesearch/7058

# bm = exp(b0 + b1 ln dbh)



