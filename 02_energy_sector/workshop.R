################################################################################
################################################################################
# (3) Energy sector trends
################################################################################
################################################################################

library(dplyr)
library(ggplot2)
library(readr)
library(tidyr)

# Data source
# https://pxweb.irena.org/pxweb/en/IRENASTAT/IRENASTAT__Finance/PUBFIN_2022_cycle2.px/

# Read in data
url <- 'https://raw.githubusercontent.com/ericmkeen/sewanee_esus/master/02_energy_sector/electricity_generation.csv'
energy <- read_csv(url, skip=2)
energy %>% head
energy %>% names
names(energy) <- c('country', 'technology', 'grid', 'year', 'electricity')
energy %>% names
energy$electricity <- as.numeric(energy$electricity)

################################################################################
################################################################################
# Energy consumption

energy %>% head

# Trends in use by country =====================================================

# Simple
by_country <-
  energy %>%
  group_by(year, country) %>%
  summarize(total = sum(electricity, na.rm=TRUE))

by_country$country %>% unique

ggplot(by_country,
       aes(x=year,
           y=total,
           color=country)) +
  geom_line()

# Top 10 emitters
top10 <-
  by_country %>%
  arrange(desc(total)) %>%
  mutate(rank = 1:n()) %>%
  filter(rank < 10)

ggplot(top10,
       aes(x=year,
           y=total,
           color=country)) +
  geom_line()


# Trends by energy type -- global ==============================================

type_by_year <-
  energy %>%
  group_by(year, technology) %>%
  summarize(total = sum(electricity, na.rm=TRUE))

type_by_year

# Basic plot - line
ggplot(type_by_year,
       aes(x=year,
           y=total,
           color=technology)) +
  geom_line()

# Basic plot - area
ggplot(type_by_year,
       aes(x=year,
           y=total,
           fill=technology)) +
  geom_area()

################################################################################
################################################################################
# Use of renewables

# Trends in renewables =========================================================

renewables <-
  type_by_year %>%
  filter(technology %in% c('Solar photovoltaic',
                           'Onshore wind energy',
                           'Offshore wind energy',
                           'Biogas'))
renewables

ggplot(renewables,
       aes(x=year,
           y=total,
           fill=technology)) +
  geom_area()

# Change in use between 2000 and 2020 ==========================================

type_by_year %>%
  group_by(technology) %>%
  summarize(percent_change = 100*(total[year == 2020] / total[year == 2000]) - 100) %>%
  arrange(percent_change)

# Share of other renewables ==========================================================
# Ignoring hydropower

(energy_sources <- energy$technology %>% unique)
(renewable_sources <- energy_sources[c(1, 3, 4, 8, 9, 10, 11, 12, 13)])

shares <-
  energy %>%
  mutate(type = ifelse(technology %in% renewable_sources, 'Renewable', 'Non-renewable')) %>%
  left_join(gapminder %>% select(country, continent)) %>%
  group_by(continent, country, year) %>%
  summarize(total = sum(electricity, na.rm=TRUE),
            renewable = sum(electricity[type=='Renewable'], na.rm=TRUE),
            share = renewable/total) %>%
  filter(!is.na(continent))

ggplot(shares,
       aes(x=year,
           y=share,
           group = country)) +
  geom_line() +
  facet_wrap(~continent)

ggplotly()


################################################################################
################################################################################
# Cost of energy

# Our World In Data
# https://ourworldindata.org/grapher/levelized-cost-of-energy

# 'levelized-cost-of-energy.csv'
url <- 'https://raw.githubusercontent.com/ericmkeen/sewanee_esus/master/02_energy_sector/levelized-cost-of-energy.csv'
costs <- read_csv(url)
costs

ggplot(costs,
       aes(x=year,
           y=cost,
           color = source)) +
  geom_point(alpha=.5) +
  geom_smooth(se=FALSE) +
  ylab('Levelized cost of energy') +
  labs(color = 'Source') +
  theme_light() +
  labs(title='Trends in renewable energy costs') +
  geom_hline(yintercept = 0.18, lty=2, size=1, alpha=.3) +
  geom_hline(yintercept = 0.05, lty=2, size=1, alpha=.3) +
  annotate('text', x=1987, y=0.06, label='Price range of fossil fuels', alpha=.4, size=3)


