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

# Explain context; Nelson's study and rationale

################################################################################
# Sample locations

url <- 'https://raw.githubusercontent.com/ericmkeen/sewanee_esus/master/03_sequestration/nelson_polygon.csv'
urban_sewanee <- read_csv(url)
url <- 'https://raw.githubusercontent.com/ericmkeen/sewanee_esus/master/03_sequestration/nelson-sample-locations.csv'
sites <- read_csv(url)
sites %>% head

leaflet() %>%
  addTiles() %>%
  addPolygons(lng=urban_sewanee$lon, lat=urban_sewanee$lat, col='purple') %>%
  addCircleMarkers(lng=sites$lon, lat=sites$lat)

################################################################################
# Load data

# Tree measurements ============================================================
url <- 'https://raw.githubusercontent.com/ericmkeen/sewanee_esus/master/03_sequestration/nelson_trees.csv'
trees <- read_csv(url)
trees

# Biomass conversion coefficients  =============================================
url <- 'https://docs.google.com/spreadsheets/d/1mx-4kfU4j966ORpzuFO0DG_VMmuQ08NR_uIxddJGc1E/edit?usp=sharing'
coefficients <- gsheet2tbl(url)
coefficients

################################################################################
# Data explore

# How many plots?
# How many trees measured?
# Average number of trees measured per plot? Min? max?

# How many of each species was seen?
ggplot(trees %>% group_by(species) %>% tally(),
       aes(x=n, y=species)) +
  geom_col()

# Create a table with the sample size, the mean and SD dbh of each species,
# and rank by sample size in descending order
tree_table <-
  trees %>%
  group_by(species) %>%
  summarize(n=n(),
            dbh_mean = mean(dbh),
            dbh_sd = sd(dbh)) %>%
  arrange(desc(n))

tree_table

# Which tree is, on average, the largest?
# For the top 10 sample species, compare their dhb distributions across species
ggplot(trees %>% filter(species %in% tree_table$species[1:10]),
       aes(x=dbh, y=species)) +
  geom_violin()

################################################################################
# Biomass regressions
# https://www.fs.usda.gov/research/treesearch/7058

b0 = -2.54
b1 = 2.43
dbh <- 0:200
biomass_kg = exp(b0 + b1*log(dbh))
plot(biomass_kg~dbh)

# You measure a tree with 0.79cm dbh. Would it weigh more if it were a pine tree or an oak tree?

# Join the datasets
trees <- left_join(trees, coefficients)
trees <-
  trees %>%
  mutate(biomass_kg = exp(b0 + b1*log(dbh)))

# For the top 10 sampled species, compare their biomass across species
ggplot(trees %>% filter(species %in% tree_table$species[1:10]),
       aes(x=biomass_kg, y=species)) +
  geom_violin()

################################################################################
# Convert to carbon storage then to C02 (Jenkins et al 2004)

trees <-
  trees %>%
  mutate(carbon_kg = biomass_kg*0.50,
         co2_kg = carbon_kg*3.663)

trees

################################################################################
# Total co2 biomass per plot
# save as plot_table

plot_table <- trees %>%
  group_by(plot) %>%
  summarize(co2_kg = sum(co2_kg))

# How many total kg of co2 were found in the plots?

# What was the average kg of co2 found per plot?

################################################################################
# co2 per square meter

# add a column "co2_kg_m2" that provides the kg of co2 per square meter for each plot
# Each plot is 25-meter radius

plot_table <-
  plot_table %>%
  mutate(co2_kg_m2 = co2_kg / (pi*25^2))

# What was the average kg of co2 per square meter found per plot?


################################################################################
# Standing stock

# The total area of nelson's Urban Study Area is 1,043,121 m2.
# How many kg of co2 do you predict for urban sewanee?
# What is this in metric tons?

################################################################################
# Extrapolate sequestration

# That number was standing biomass. What about sequestration moving forward?
# Let's say the average dbh growth of a tree is 0.55 cm.
# How much co2 gets sequestered in a single year of growth in 'urban sewanee'

# There are 1630 students on campus. How many co2 offsets does urban sewanee give each student?

# What share of the average student's carbon budget is offset by this sequestration? Use references.








