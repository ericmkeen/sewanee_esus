################################################################################
# R tutorial 1 (2022)
################################################################################

# Part A. Running R Code =======================================================

# 1. Find the sum of the ages of everyone in your immediate family.


# 2. What is their average age?


# 3. The following lines of code have errors. Re-type the line to be error-free:

#    5 * 6 +

#    sqrt(16

#    round(100/3, digits+3)


# 4. Show that the following statements are true:

#    pi is greater than the square root of 9

#    It is FALSE that the square root of 9 is greater than pi

#    pi rounded to the nearest whole number equals the square root of 9


# 5. Write and run a line of code that asks whether
#    these two calculations return the same result:
#    2*7 - 2*5 / 2
#    (2*7 - 2*5) / 2


# ==============================================================================
# Part B. Working with variables ===============================================

# 6. Develop a calculation to estimate how many bananas you've eaten in your lifetime,
#    and store that value in a variable (choose whatever name you wish).


# 8. Now estimate how many ice cream sandwiches you've eaten in your lifetime,
#    using a similar method, and store that value in a different variable.

# 9. Now use these variables to calculate your Banana-to-ICS ratio.
#    Store your result in a third variable.

# 10. Who in your pod has the highest Banana-to-ICS ratio?
# (Write their name as a character string)


# 11. What is the average Banana-to-ICS ratio in your pod?


# 12. Let's change the subject.
#     Assign a variable `farenheit` to the numerical value of 32.


# 13. Assign a variable `celsius` to equal the conversion from Farenheit to Celsius.
#     Unless you are a meteorology nerd, you may need to Google the equation.


# 14. Now use this code to determine the Celsius equivalent of 212 degrees Farenheit.


# ==============================================================================
# Part C. Making fancy maps ====================================================

# 15. What line of code would you use to install the package 'leaflet'?


# 16. What line of code would you use to load the `leaflet` functions into
#     your current R session?


# 17. Which one of these commands do you run once,
#     and which do you run every time you sit down to work in R?
#     (provide your answer as a comment.)


# 18. Load the packages 'dplyr', 'readr', and 'tidygeocoder' to use in your R session.
#     If you don't have those packages installed yet, install them then laod them.


# 19. Run this code to "read in" a dataset into R:
whales <- read_csv('https://raw.githubusercontent.com/ericmkeen/capstone/master/fin_whales.csv')

# 20. To view this dataset, run this code:
View(whales)

# 21.  In this dataset, each row is a sighting of a fin whale in coastal British Columbia.
#      Which columns correspond to geographic coordinates?
#      Write your answer as a comment.


# 22. Make a beautiful leaflet map of these sightings.

leaflet() %>%
  addProviderTiles(providers$Esri.OceanBasemap) %>%
  addCircleMarkers(data=whales,
                   color='firebrick',
                   radius=1)

# 23. Let's change the subject.
#     Now make a beautiful leaflet map of your favorite restaurant in the world,
#     using a map tile theme you have not used yet.
#     Also, add a "pop up" message that appears when you click on the marker for your restaurant.
#     The steps involved here are provided below.
#     If you need help, refer to this website:
#     http://www.datascience.pizza/interactive-maps.html#geocoding

# a. First, use Google to get the name and address of your restaurant of choice.
#    Save that address as a dataframe:

addresses <- data.frame(name= "Friendly Khmer Satay Noodle House",
                        addr = "434 George Street, North Dunedin, Dunedin 9016, New Zealand")

# b. Use the package 'tidygeocoder' to get the latitude / longitude for this address.
lat_longs <-
  addresses %>%
  geocode(addr,
          method = 'osm',
          lat = latitude,
          long = longitude)

lat_longs # check it out

# c. Build a basic leaflet map using a new tile theme.

leaflet(data=lat_longs) %>%
  addTiles() %>%
  addMarkers()

# d. Modify your map using a new tile them.
#   Use this website to explore your options:
#   https://leaflet-extras.github.io/leaflet-providers/preview/

leaflet(data=lat_longs) %>%
  addProviderTiles(providers$Esri.WorldTopoMap) %>%
  addMarkers()



