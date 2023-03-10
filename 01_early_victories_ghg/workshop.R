################################################################################
################################################################################
# (1) Early victories in R
################################################################################
################################################################################

# Running R code ===============================================================

# Open script
# Save your script: week2
# Write stuff in script
3 + 3
# Command enter (how to run lines of code)

# Run more stuff:
6 / 3
6 * 3
6 > 3
6 < 3
6 == 3
3 == 3
# (emphasize: R is just a fancy calculator)

# Talk about errors
# 3 +
# 3 * (4 + 3

# Assigning values to objects
x <- 3 # note how it did not print to console
y <- 6
x*y
z <- y/x
z

m <- 1:100
m
# analogy: objects are like credit cards: they are a super-portable way of accessing big collections of things.
# note: hinting at true power of R: an object named m could hold gigabytes of data

# you can name objects pretty much anything
duh <- runif(10)
duh

# use underscores instead of spaces
rob_pearigen <- 10
rob_pearigen *10

# Save script
# Re-run all of your script so far.
# Discuss difference between script & console
# Another advantage of scripts: # COMMENTS --
# go back and comment your code up to this point.

# Functions ====================================================================

x <- 1:10
x
mean(x)
sd(x)
# This is a function -- functions are like vending machines.

# Try another function:
x <- rnorm(10000)
x
hist(x)
mean(x)
sd(x)
# re-run to show how random number generation works

# Packages =====================================================================

# R comes with a lot of functions built-in. Like mean, sd, hist, rnorm, runif, etc.

# You can also download functions that others have developed using PACKAGES.
install.packages('dplyr')

# Packages are like shoes.
# Base R is like going barefoot -- you can do alot with barefeet.
# But sometimes you want to go snowshoeing, or skiing, or rockclimbing,
# and you need special shoes for those things.

# Install some more:
install.packages('readr')
install.packages('ggplot2')
install.packages('gsheet')
install.packages('leaflet')
install.packages('gganimate')
install.packages('gapminder')
install.packages('scales')
install.packages('plotly')
install.packages('av')
install.packages('gifski')

# There's a difference between buying shoes and wearing shoes.
# In R, installing a package is like buying your shoes. You get to keep them at your house.
# But to actually use your package, you need to use another line of code.
library(dplyr)

# This is like putting your shoes ON.
library(readr)
library(ggplot2)
library(gsheet)
library(leaflet)
library(gganimate)
library(gapminder)
library(scales)
library(plotly)
library(av)
library(gifski)

# Install -- Do ONCE -- use quotes. # comment out after done
# Library -- Do in EVERY SCRIPT -- no quotes.

# Re-install R
# Re-run library code

# The pipe =====================================================================

# Type this:
1:10 %>% mean

# The pipe means THEN.
16 %>% sqrt
rnorm(10000) %>% hist

# Shortcut: Command + Shift + M

# This can make your code clearer
round(sqrt(mean(1:100)))
1:100 %>% mean %>% sqrt %>% round

# The pipe will become INDISPENSABLE once we start working with datasets

# Looking at data ==============================================================

# Load dataset
data(gapminder)

# Look at dataset
gapminder %>% head
gapminder %>% View
# note columns and rows

# Look at years in dataset
gapminder$year # use $ to look at just one column
gapminder$year %>% table

# We want to plot these data for 2007 only.
gapminder %>% filter(year==2007) # First, filter the data
gapminder_filtered <- gapminder %>% filter(year==2007) # save to a new object
gapminder_filtered %>% View

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
           group=country, # add 1
           size = pop,
           color = continent)) +
  geom_point(alpha=0.7) +
  theme_minimal() + # add 2
  xlab("GDP per capita") +
  ylab("Life expectancy at birth") + # add 3
  scale_x_continuous(trans='log', # add 4
                     labels = dollar) + # add 5
  scale_size_area(guide = FALSE, max_size = 15) # add 6

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
  scale_x_continuous(trans='log', labels = dollar)
p

# Animate
p +
  ggtitle("{round(frame_time)}") +
  transition_time(year) +
  ease_aes("linear")


################################################################################
################################################################################
# R Lab #1: Climate impacts & carbon emissions
################################################################################
################################################################################

# Go to course calendar, click on workshop link.

# Read in data
url <- 'https://nyc3.digitaloceanspaces.com/owid-public/data/co2/owid-co2-data.csv'
carbon <- read_csv(url)

# Explore
carbon %>% View
carbon %>% names

# Simple exploration
carbon_simple <- carbon %>% select(country, year, population, gdp, co2, cumulative_co2, co2_per_gdp)

################################################################################
# Task # 1: Case study: USA
################################################################################

# Filter to the United States only
usa <- carbon_simple %>% filter(country == 'United States')
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
# Task #2: case study: USA & China
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
# Task #3: CO2 and GDP (present day)
################################################################################

# Join gapminder to co2 data
gapminder %>% names
super_data <- left_join(carbon, gapminder)
super_data %>% filter(country == 'United States') %>% View

# Starting point
ggplot(super_data %>%
         filter(year == 2007),
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
           y=(co2*1000000) / population, # change 1
           size=population,
           color = continent,
           group=country)) +
  geom_point(alpha=.8) +
  scale_x_continuous(trans='log') +  # change 2
  scale_size_area(guide = FALSE, max_size = 15) # change 3

# Final
ggplot(super_data %>%
         filter(year == '2007'),
       aes(x=gdpPercap,
           y=(co2*1000000) / population,
           size=population,
           color = continent,
           group=country)) +
  geom_point(alpha=.8) +
  scale_x_continuous(trans='log',
                     labels=dollar_format(), # change 1
                     breaks=c(500, 1500, 5000, 15000, 60000)) + # change 2
  scale_size_area(guide = FALSE, max_size = 15) +
  theme_light() + # change 3
  ylim(0, 30) + # change 4
  xlab('GPD per capita') + # change 5
  ylab('CO2 emissions (tons) per capita') # change 6

# Bring in ALL years with gdp data
p <-
  ggplot(super_data %>%
         filter(!is.na(gdpPercap)), # CHANGE THIS
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

