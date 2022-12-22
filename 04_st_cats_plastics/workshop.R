################################################################################
################################################################################
# (5) Beach plastics
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
# Don't run again

if(FALSE){
  load('plastics.RData')

  transects <-
    transects %>%
    select(beach, master_id, latitude, longitude, length_m, width_m = depth, area_m2 = area)

  beaches <-
    beaches %>%
    select(name = beach, beach = code, length_km = km)

  debris <-
    debris %>%
    filter(event == 'Debris') %>%
    select(beach, master_id, material, structure, cm_from_tape, size_cm, quantity,
           m_from_base, frac_from_base)

  write.csv(transects, 'transects.csv',quote=FALSE, row.names=FALSE)
  write.csv(beaches, 'beaches.csv',quote=FALSE, row.names=FALSE)
  write.csv(debris, 'debris.csv',quote=FALSE, row.names=FALSE)
}

################################################################################
# Read in data

transects <- read.csv('transects.csv')
beaches <- read.csv('beaches.csv')
debris <- read.csv('debris.csv')

################################################################################
# Map

leaflet() %>%  addTiles() %>%
  addCircleMarkers(lat=transects$latitude, lng=transects$longitude,
                   radius=.1)

################################################################################
# Explore data

# Total length of transects
# Total items found
# Total length of items found
# Total items for each material etc
# Beach position?
# Item freq ~ distance histogram (implications)

################################################################################
# Estimate mean width of each beach
# save in an object named beach_widhts

beach_widths <-
  transects %>%
  group_by(beach) %>%
  summarize(width_mean = mean(width_m),
            width_sd = sd(width_m))

beach_widths

################################################################################
# Estimate the total area of each beach
# save in an object named beach_table

beach_table <-
  left_join(beaches, beach_widths) %>%
  mutate(area_km2 = length_km*(width_mean/1000))

beach_table

# What is the total area of beach on St Cats?
beach_table$area_km2 %>% sum

# What fraction of the beach was surveyed in this effort?
sum(transects$area_m2/1000000) / (beach_table$area_km2 %>% sum)

################################################################################
# Calculate density of each transect
# save in an object named densities

items <-
  debris %>%
  group_by(master_id) %>%
  summarize(items = n())

densities <-
  left_join(transects, items) %>%
  mutate(items = items %>% replace_na(0)) %>%
  mutate(D = items / area_m2)

densities

# Display a histogram of transect debris densities
densities$D %>% hist

# Compare densities across beaches in a plot
ggplot(densities,
       aes(x=beach,
           y=D)) +
  geom_violin(fill='navyblue', alpha=.3) +
  geom_point(alpha=.6)

################################################################################
# Create a table showing mean + SD density for each beach
# save in an object named density_table

density_table <-
  densities %>%
  group_by(beach) %>%
  summarize(Dmean = mean(D),
            Dsd = sd(D))

density_table

# For every 100 sq meters you survey, how many pieces of plastic are you likely to find?

# Which beach has the highest density of anthropogenic debris?
# Any ideas why this might be the case?

# Which beach has the lowest? Any ideas why?

################################################################################
# Estimate the total abundance of debris on each beach
# save in an object called final_table

final_table <-
  left_join(beach_table, density_table) %>%
  mutate(items = area_km2*1000000*(Dmean))

final_table

# Total items on each beach
final_table$items %>% sum

# Plot it
ggplot(final_table,
       aes(x=name, y=items)) +
  geom_col()

# How can South Beach have more debris? I thought Top Beach had higher densities!


################################################################################
# Repeat for linear debris length



