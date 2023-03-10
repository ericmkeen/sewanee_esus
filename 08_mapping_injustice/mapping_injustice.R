# MAPS =========================================================================
# https://r-spatial.org/r/2018/10/25/ggplot2-sf.html

library(sf)
library(dplyr)
library(readr)
library(ggplot2)
library(ggspatial)
library(rnaturalearth)
library(rnaturalearthdata)
library(raster)

theme_set(theme_bw())
world <- ne_countries(scale = "medium", returnclass = "sf")

ggplot(data = world) +
  geom_sf()

ggplot(data = world) +
  geom_sf(color = "black",
          fill = "lightgreen")


# CHLOROPLETH
world %>% head
ggplot(data = world) +
  geom_sf(aes(fill = pop_est)) +
  scale_fill_viridis_c(option = "plasma", trans = "sqrt")

# Change range
ggplot(data = world) +
  geom_sf() +
  coord_sf(xlim = c(-90, -82), ylim = c(30, 36), expand = FALSE)

# Make fancy
ggplot(data = world) +
  geom_sf() +
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering) +
  coord_sf(xlim = c(-102.15, -74.12), ylim = c(7.65, 33.97))


# CENSUS DATA ==================================================================
# https://walker-data.com/census-r/mapping-census-data-with-r.html
# https://justinmorganwilliams.medium.com/import-nyc-census-data-into-r-with-tidycensus-c94d2d1f23fa

library(tidycensus)
library(tigris)

# Decennial census =============================================================

# American Indian & Alaska Native population by state
# from the 2020 decennial Census
aian_2020 <- get_decennial(
  geography = "state",
  variables = "P1_005N",
  year = 2020,
  sumfile = "pl"
)

aian_2020

# Decennial variables
dec_vars <- load_variables(year=2020, dataset='dpas')
dec_vars

# Select Variables
desired_vars = c(
  all = "P2_001N", # All Residents
  hisp = "P2_002N", # Hispanic
  white = "P2_005N", # White
  baa = "P2_006N", # Black or African American
  amin = "P2_007N", # Native American(American Indian in data)
  asian = "P2_008N", # Asian
  nhopi = "P2_009N", # Native Hawaiian or Pacific Islander
  other = "P2_010N", # Some Other Race
  multi = "P2_011N" # Two or More Races
)

reth_NY_20 = get_decennial(
  geography = "county",
  state = "NY",
  variables = desired_vars,
  summary_var = "P2_001N", # Same as 'All'
  year = 2020,
  geometry=TRUE
)

reth_NY_20

ggplot(reth_NY_20 %>% mutate(perc = value / summary_value),
       aes(fill=perc)) +
  geom_sf() +
  facet_wrap(~variable)

# ACS ==========================================================================

# https://www.census.gov/content/dam/Census/programs-surveys/acs/about/ACS_Information_Guide.pdf


dc_income <- get_acs(geography = "tract",
                     variables = "B19013_001",
                     state = "DC",
                     year = 2020,
                     geometry = TRUE)


plot(dc_income["estimate"])



us_median_age <- get_acs(
  geography = "state",
  variables = "B01002_001",
  year = 2019,
  survey = "acs1",
  geometry = TRUE,
  resolution = "20m"
) %>%
  shift_geometry()

ggplot(data = us_median_age, aes(fill = estimate)) +
  geom_sf()

# Look at variables
vars <- load_variables(year=2021, dataset='acs1')
cats <- vars$concept %>% unique
cats %>% View
cats[grepl('TRANSPORTATION', cats)]
cats[grepl('INCOME', cats)]
cats[grepl('POVERTY', cats)]
cats[grepl('RACE', cats)]
cats[grepl('SEX', cats)]
cats[grepl('HOUSING', cats)]
cats[grepl('AIR', cats)]



# Climate justice dataset
# https://www.ciesin.columbia.edu/data/sepher/sepher2.0_cleaned.csv
library(readr)
sepher <- read_csv('https://www.ciesin.columbia.edu/data/sepher/sepher2.0_cleaned.csv')


################################################################################
################################################################################
# FedData
# https://github.com/ropensci/FedData

library(devtools)
devtools::install_github("ropensci/FedData")
library(FedData)

# National elevation dataset
?get_ned

?get_daymet

?get_ghcn_daily

?get_nhd

?get_ssurgo

?get_nlcd

FedData::meve

# Bounding box for south cumberland
lims <- tibble(longitude = c(-86.5, -84.5, -84.5, -86.5, -86.5),
               latitude = c(34.5, 34.5, 36, 36, 34.5))
sewanee_box <-
  st_as_sf(lims, coords = c('longitude', 'latitude'), crs=4326) %>%
  summarise(geometry = st_combine(geometry)) %>%
  st_cast("POLYGON")

library(leaflet)
leaflet(sewanee_box) %>% addTiles() %>% addPolygons()


NED <- get_ned(template = sewanee_box,
        label='socu')

library(rasterVis)
gplot(NED) +
  geom_tile(aes(x=x, y=y, fill=value), alpha=0.8) +
  coord_sf()

sewanee_box %>% class
meve %>% class


# Get census information
# various variables - % white, % income, % home ownership, etc
# plot it
# df it
# get census block polygons
# get bounding box

# Get land cover
# plot it
#


# Join


################################################################################

my_api <- '513e12d29ff5d0d1f5f94028cb981be1eb12a823'
# Add key to .Renviron
Sys.setenv(CENSUS_KEY=my_api)
# Reload .Renviron
readRenviron("~/.Renviron")
# Check to see that the expected key is output in your R console
Sys.getenv("CENSUS_KEY")

library(censusapi)

apis <- listCensusApis()
apis %>% View

apis$description[grepl('enviro', tolower(apis$description))]

colnames(apis)


################################################################################
################################################################################
# SEPHER dataset

sepher <- read_csv('/Users/erickeen/Downloads/sepher2.0_cleaned.csv')
sepher

length(sepher)

(nonyearnames <- names(sepher)[which(! grepl('20', names(sepher)))])

(sepher %>% names)[1:200]
(sepher %>% names)[201:400]
(sepher %>% names)[401:600]
(sepher %>% names)[601:800]
(sepher %>% names)[801:1000]
(sepher %>% names)[1001:1200]
(sepher %>% names)[1201:1400]
(sepher %>% names)[1401:1600]
(sepher %>% names)[1601:1800]

for(i in 2000:2020){
  ni <- grep(as.character(i), names(sepher)) %>% length()
  message(i, ' : n = ', ni)
}

# Find columns to keep
(front_matter <- nonyearnames[1:17])
(n16 <- grep('2016', names(sepher)))
names(sepher)[n16]
dontkeep <- c(17, 18, 20, 21:23,
              29:35,
              44:62,
              98:116,
              154:158,
              179)
(socios <- names(sepher)[n16[-dontkeep]])
(risk_overall <- nonyearnames[18:21])
(annual_loss <- nonyearnames[c(22:25, 30)])
(sovi <- nonyearnames[31:34])
(resl <- nonyearnames[36:39])
(events <- nonyearnames[41:365][grep('EXPT', nonyearnames[41:365])])

# Make v of all names to keep
(keepnames <- c(front_matter, socios, risk_overall, annual_loss, sovi, resl, events))

# Subset to these columns
sepher <- sepher[, which(names(sepher) %in% keepnames)]
sepher %>% names

# Save to file
save(sepher, file='/Users/erickeen/Downloads/SEPHER_2016.rds')

# Check graph
ggplot(sepher, aes(y=POVERTY.RATE_2016,
                   x=PCT.WHITE_2016)) +
  geom_point(alpha=.1) +
  geom_smooth(method='lm')

################################################################################
################################################################################
# Data key

key <- readxl::read_excel('/Users/erickeen/Downloads/sepher2.0_cleaned_dataDictionary_20211123.xlsx')
key$Name
name_order <- sapply(names(sepher), function(x){which(key$Name == x)})
name_order
key <- key[name_order,]
key
save(key, file='/Users/erickeen/Downloads/key_sepher.rds')


################################################################################
################################################################################
# Shape file of tracts

fname <- '/Users/erickeen/Downloads/basis_US_tracts/basis_US_tracts.shp'
tracts <- st_read(fname)
tracts <- st_make_valid(tracts)
tracts$FIPS <- paste0(tracts$FIPS, 'GID')
tracts$STATE %>% unique

# Try saving object
save(tracts, file='/Users/erickeen/Downloads/census_tracts.rds')


################################################################################
################################################################################
# Air quality dataset

# https://www.epa.gov/enviroatlas/data-download-step-2
# https://www.epa.gov/national-air-toxics-assessment/2014-nata-assessment-results

air <- readxl::read_excel('/Users/erickeen/Downloads/nata2014v2_national_cancerrisk_by_tract_poll.xlsx')
air %>% names
air <- air[-1,1:7]
air <- air %>% dplyr::select(FIPS = Tract, cancer_risk = `Total Cancer Risk (per million)`)
air %>% head
air$FIPS <- paste0(air$FIPS, 'GID')
save(air, file='/Users/erickeen/Downloads/cancer_risk.rds')


################################################################################
################################################################################
################################################################################
################################################################################
# WORKSHOP

#===============================================================================
# SEPHER dataset

# https://www.ciesin.columbia.edu/data/sepher/
# https://www.liebertpub.com/doi/10.1089/env.2021.0059
# census tracts:
# https://www.google.com/search?q=map+of+census+tracts&rlz=1C5GCEM_enUS914US914&sxsrf=AJOqlzVuq8FlethjplKv-DWdei7KlSb9Xg:1678462715520&source=lnms&tbm=isch&sa=X&ved=2ahUKEwimirXv2NH9AhWiH0QIHfnpD0kQ_AUoAnoECAEQBA&biw=1792&bih=849&dpr=2#imgrc=IqwlVkgAfwox8M

# Load SEPHER (2016 data only)
load(url('https://github.com/ericmkeen/sewanee_esus/blob/master/08_mapping_injustice/SEPHER_2016.rds?raw=true'))
names(sepher)

# Load data key
load(url('https://github.com/ericmkeen/sewanee_esus/blob/master/08_mapping_injustice/key_sepher.rds?raw=true'))
key
key$Name

# Explore
ggplot(sepher, aes(y=POVERTY.RATE_2016,
                   x=PCT.WHITE_2016)) +
  geom_point(alpha=.1)

ggplot(sepher %>% filter(ST_ABBR %in% c('CA', 'FL')),
       aes(x=RISK_SCORE,
           fill=ST_ABBR)) +
  geom_density(alpha=.4)

# Is environmental risk exposure related to income?
ggplot(sepher, aes(y=RISK_SCORE,
                   x=MEDIAN.HOUSEHOLD.INCOME_2016)) +
  geom_point(alpha=.1)

# Devise your own justice-related question and make a plot that answers it


#===============================================================================
# Census tracts

# Load tracts
load(url('https://github.com/ericmkeen/sewanee_esus/blob/master/08_mapping_injustice/census_tracts.rds?raw=true'))

# Demo w/ california
cali <- tracts %>% filter(STATE == 'CALIFORNIA')

# Make a leaflet map
library(leaflet)

# Make a ggplot map
ggplot(cali) +
  geom_sf()


#===============================================================================
# Join the datasets together

mr <- left_join(tracts, sepher, by='FIPS')

mr %>% names %>% tail

#===============================================================================
# Chloropleths

mr_tn <- mr %>% filter(STATE == 'TENNESSEE')

# Make map
ggplot(mr_tn,
       aes(fill=E_PCI_2016)) +
  geom_sf(color=NA) +
  labs(title='Per-capita income by census tract')

mr_tn %>% names

# Other color scales
library(viridis)
# details here: https://cran.r-project.org/web/packages/viridis/vignettes/intro-to-viridis.html

ggplot(mr_tn,
       aes(fill=E_PCI_2016)) +
  geom_sf(color=NA) +
  #scale_fill_viridis()
  scale_fill_viridis(option='magma')


# DIY CHLOROPLETH of your own state

ggplot(cali, aes(fill=RISK_SCORE)) +
  geom_sf(color=NA) +
  labs(title='FEMA risk score')


#===============================================================================
# Subset to manully defined study area, e.g., South Cumberland Plateau

socu <- st_crop(mr,
                  xmin = -86.5,
                  ymin = 34.5,
                  xmax = -84.5,
                  ymax = 36.6)

ggplot(socu,
       aes(fill=POPULATION)) +
  geom_sf() +
  geom_point(aes(x=-85.92, y=35.2), color='orange', size=3)

# DIY chloropleth of your own manually defined study area


#===============================================================================
# Air quality

load(url('https://github.com/ericmkeen/sewanee_esus/blob/master/08_mapping_injustice/cancer_risk.rds?raw=true'))

mra <- left_join(mr, air, by='FIPS')
mra %>% head

ggplot(mra,
       aes(x=PCT.WHITE_2016,
           y=cancer_risk)) +
  ylim(0, 70) +
  geom_point(alpha=.1)


ggplot(mra %>%
         mutate(whiteness = ifelse(PCT.WHITE_2016 > 0.5, 'more white', 'less white')) %>%
         filter(!is.na(whiteness) == TRUE,
                cancer_risk < 100),
       aes(x=whiteness, y=cancer_risk)) +
  geom_boxplot()





#===============================================================================
# FedData

# FedData
# https://github.com/ropensci/FedData

library(devtools)
devtools::install_github("ropensci/FedData")
library(FedData)

# National elevation dataset
?get_ned

?get_daymet

?get_ghcn_daily

?get_nhd

?get_ssurgo

?get_nlcd

FedData::meve

# Bounding box for south cumberland
lims <- tibble(longitude = c(-86.5, -84.5, -84.5, -86.5, -86.5),
               latitude = c(34.5, 34.5, 36, 36, 34.5))
sewanee_box <-
  st_as_sf(lims, coords = c('longitude', 'latitude'), crs=4326) %>%
  summarise(geometry = st_combine(geometry)) %>%
  st_cast("POLYGON")

library(leaflet)
leaflet(sewanee_box) %>% addTiles() %>% addPolygons()


vt <- mr %>% filter(STATE == 'VERMONT')
ggplot(vt) + geom_sf()

NED <- get_ned(template = vt,
               label='cali')
NED %>% class
plot(NED)

library(rasterVis)
gplot(NED) +
  geom_tile(aes(x=x, y=y, fill=value), alpha=0.8) +
  coord_sf()


# Join NLCD
cover <- get_nlcd(template = vt,
         label = 'vt')
cover <- projectRaster(from=cover, crs=4326)
plot(cover)
xyz <- rasterToPoints(cover) %>% as.data.frame
names(xyz) <- c('x', 'y', 'z')
xyz <- xyz %>% mutate(cover = ifelse(z > 20 & z < 30, 'dev', 'und'))
xyz$cover %>% table



library(exactextractr)
ex <- exact_extract(x=cover,
                    y=vt)
ex <- lapply(ex, function(x){
  xi <- x %>% filter(coverage_fraction > 0.5) %>% pull(value)
  xii <- length(which(xi > 20 & xi < 30)) / length(xi)
  return(xii)})
vt$frac_dev <- ex %>% unlist

ggplot(vt) +
  geom_sf(aes(fill=frac_dev))

mr %>% names
mr$EP_POV_2016


# Subset to FLORIDA
tracts <- tracts %>% filter(STATE == 'FLORIDA')
mr <- left_join(tracts, sepher, by='FIPS')

ggplot(mr, aes(x=E_PCI_2014, y=RISK_SCORE)) +
  geom_point()

ggplot(mr) +
  geom_sf(aes(fill=E_PCI_2014), color=NA)

ggplot(mr) +
  geom_sf(aes(fill=RISK_SCORE), color=NA)

ggplot(mr) +
  geom_sf(aes(fill=PCT.HISPANIC_2012), color=NA)

whites <- apply(mr[,grep('PCT.WHITE', names(mr))] %>% as.data.frame, 1,
      function(x){
        xii <- NA
        xi <- x[!is.na(x)]
        if(length(xi)>0){
          xi %>% tail(1)
          xii <- xi
        }
        return(xii)
      })

whites %>% unlist



# https://www.epa.gov/enviroatlas/data-download-step-2


