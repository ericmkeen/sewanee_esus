################################################################################
################################################################################
# (1) Early victories in R
################################################################################
################################################################################

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

# Libraries
library(dplyr)
library(readr)
library(ggplot2)
library(gganimate)
library(gapminder)
library(scales)
library(plotly)

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
################################################################################
# Early victories!

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




