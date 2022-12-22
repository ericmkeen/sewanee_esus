################################################################################
################################################################################
# (1) Early victories in R
################################################################################
################################################################################

# Calculator


# Saving objects


# Scripts

# Libraries
library(dplyr)
library(readr)
library(ggplot2)
library(gganimate)
library(gapminder)
library(scales)
library(plotly)

# Load dataset
data(gapminder)

# Look at dataset
gapminder %>% head
gapminder %>% View

# Talk about the pipe

# Look at years in dataset
gapminder$year
gapminder$year %>% table

# 2007 plot
gapminder_filtered <- gapminder %>% filter(year==2007)

# ggplot it - canvas
ggplot(gapminder_filtered,
       aes(x = gdpPercap,
           y = lifeExp))

# ggplot it - minimal
ggplot(gapminder_filtered,
       aes(x = gdpPercap,
           y = lifeExp)) +
  geom_point()

# ggplot it - final
ggplot(gapminder_filtered,
       aes(x = gdpPercap,
           y = lifeExp,
           group=country,
           size = pop,
           color = continent)) +
  geom_point(alpha=0.7) +
  xlab("GDP per capita") +
  ylab("Life expectancy at birth") +
  theme_minimal() +
  scale_size_area(guide = FALSE, max_size = 15) +
  scale_x_continuous(labels = dollar)

# plotly version
ggplotly()

# Plot all years at once
p <-
  ggplot(gapminder,
         aes(x = gdpPercap,
             y = lifeExp,
             size = pop,
             color = continent,
             group=country)) +
  geom_point(alpha=0.7) +
  xlab("GDP per capita") +
  ylab("Life expectancy at birth") +
  theme_minimal() +
  scale_size_area(guide = FALSE, max_size = 15) +
  scale_x_continuous(labels = dollar)
p

# Animate
p +
  ggtitle("{round(frame_time)}") +
  transition_time(year) +
  ease_aes("linear")


################################################################################
################################################################################
# (2) Climate impacts
################################################################################
################################################################################

# Our World in Data
# https://ourworldindata.org/explorers/climate-change

# https://github.com/owid/co2-data # Their data repository

# Their entire dataset
url <- 'https://nyc3.digitaloceanspaces.com/owid-public/data/co2/owid-co2-data.csv'

# Read in
carbon <- read_csv(url)

# Explore
carbon %>% View
carbon %>% names
# data dictionary # https://github.com/owid/co2-data/blob/master/owid-co2-codebook.csv

# Simple exploration
carbon <-carbon %>% select(country, year, population, gdp, co2, cumulative_co2, co2_per_gdp)

################################################################################
# Case study: USA
################################################################################

usa <- carbon %>% filter(country == 'United States')
usa %>% nrow

# Annual co2
ggplot(usa,
       aes(x=year, y=co2)) +
  geom_line(alpha=.7) +
  labs(title='Annual CO2 emissions by USA')

# Cumulative co2
ggplot(usa,
       aes(x=year, y=cumulative_co2)) +
  geom_line(alpha=.7) +
  labs(title='Cumulative CO2 emissions by USA')

# Per capita
ggplot(usa %>% mutate(co2_percapita = co2/population),
       aes(x=year, y=co2_percapita)) +
  geom_line(alpha=.7) +
  labs(title='Per-capita CO2 emissions by USA')


################################################################################
# Case study: USA & China
################################################################################

toptwo <- carbon %>% filter(country %in% c('United States', 'China'))
toptwo %>% nrow()

# Annual co2
ggplot(toptwo,
       aes(x=year, y=co2, color=country)) +
  geom_line(alpha=.7, lwd=1.2) +
  labs(title='Annual CO2 emissions by USA & China')

# Cumulative co2
ggplot(toptwo,
       aes(x=year, y=cumulative_co2, color=country)) +
  geom_line(alpha=.7, lwd=1.2) +
  labs(title='Cumulative CO2 emissions by USA & China')

# Per capita
ggplot(toptwo %>%
         group_by(country) %>%
         mutate(co2_percapita = co2/population),
       aes(x=year, y=co2_percapita, color=country)) +
  geom_line(alpha=.7, lwd=1.2) +
  labs(title='Per-capita CO2 emissions by USA & China')

################################################################################
# CO2 and GDP (present day)
################################################################################

# Join gapminder to co2 data
gapminder %>% names
super_data <- left_join(carbon, gapminder)
super_data %>% filter(country == 'United States') %>% View

# Starting point
ggplot(super_data %>% filter(year == 2007),
       aes(x=gdpPercap,
           y= co2 / population,
           size=population,
           color = continent,
           group=country)) +
  geom_point(alpha=.8)

# Intermediate
ggplot(super_data %>%
         filter(year == '2007'),
       aes(x=gdpPercap,
           y=(co2*1000000) / population,
           size=population,
           color = continent,
           group=country)) +
  geom_point(alpha=.8) +
  scale_x_continuous(trans='log') +
  scale_size_area(guide = FALSE, max_size = 15)

# Final
p <-
  ggplot(super_data %>%
         filter(!is.na(gdpPercap)),
         aes(x=gdpPercap,
             y=(co2*1000000) / population,
             size=population,
             color = continent,
             group=country)) +
  geom_point(alpha=.8) +
  scale_x_continuous(trans='log',
                     labels=dollar_format(),
                     breaks=c(500, 1500, 5000, 15000, 60000)) +
  scale_size_area(guide = FALSE, max_size = 15) +
  theme_light() +
  ylim(0, 30) +
  xlab('GPD per capita') +
  ylab('CO2 emissions (tons) per capita')
p

# Plotly version
ggplotly()

# Animated version
p +
  ggtitle('Relationship between CO2 and GDP: {round(frame_time)}') +
  transition_time(year) +
  ease_aes("linear")


################################################################################
################################################################################
# (3) Energy sector trends
################################################################################
################################################################################

getwd()


################################################################################
################################################################################
# (4a) Measuring forest and soil carbon
################################################################################
################################################################################

# This is a field exercise

################################################################################
################################################################################
# (4b) Measuring forest and soil carbon
################################################################################
################################################################################

# R Solo

################################################################################
################################################################################
# (5) Plastic pollution on beaches
################################################################################
################################################################################



################################################################################
################################################################################
# (6) Roadside plastic survey
################################################################################
################################################################################

# This is an experimental design exercise


################################################################################
################################################################################
################################################################################
#================= S  P  R  I  N  G    B  R  E  A  K ==========================#
################################################################################
################################################################################
################################################################################

################################################################################
################################################################################
# (7) Ecological footprint analysis
################################################################################
################################################################################



################################################################################
################################################################################
# (8) Modeling environmental injustice
################################################################################
################################################################################



################################################################################
################################################################################
# (9) Dream graphs and fake data
################################################################################
################################################################################




