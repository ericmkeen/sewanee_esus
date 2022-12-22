################################################################################
################################################################################
# 2022 R Tutorial 3: Working with dataframes
################################################################################
################################################################################
# Review homework

# Go to Slack channel and check out the graphs
# Check for any questions

################################################################################
# Today's focus: DATA WRANGLING using DPLYR verbs (plus more plotting practice)

# Load some packages
library(dplyr)
library(ggplot2)
library(gsheet)

################################################################################
# Reading in the survey data
url <- 'https://docs.google.com/spreadsheets/d/1sW1X7KFuOIq9fddLD_otPWj3xJD24v0JFdDPh4vc1a4/edit?usp=sharing'
df <- gsheet2tbl(url)
################################################################################
# Orientation

# How do I view this dataset in the Console?
df

# How do I view it in a separate tab?
View(df)

# Another useful one:
df %>% glimpse

# How many survey responses?
df %>% nrow

# How many questions were asked?
df %>% ncol

# What were those questions?
df %>% names

################################################################################
# DPLYR VERBS

# You already know one: filter()

df %>% filter(year == 'Junior') %>% nrow
df %>% filter(year == 'Senior') %>% nrow

df %>% filter(gender == 'Male') %>% nrow
df %>% filter(gender == 'Female') %>% nrow

# Cool -- but there are more powerful ways of summarizing groups in your data:
# We are going to learn
# filter
# summarize
# group_by
# tally
# mutate
# arrange
# select
# rename
# left_join
# (Note that there are many others we wont cover today)

################################################################################
# Creating summary tables

# Get total responses
df %>% tally()
# same as nrow

df %>% summarize(responses = n())
# same as nrow

# Get responses for each class
df %>%
  group_by(year) %>%
  summarize(responses = n())

# Add columns to determine response rate
# Build this up:
df %>%
  group_by(year) %>%
  summarize(responses = n()) %>%
  mutate(possible = c(15, 11)) %>%
  mutate(response_rate = responses / possible)

# Wait a minute! Why is the Senior response rate more than 1?
# There should only be 11 senior responses -- first evidence of SURVEY FRAUD!

# Anyways -- you just used 3 different dplyr verbs: group_by, summarize, mutate
# Note:
# group_by usually needs to be accompanied by summarize.
# mutate adds a new column.
# Note that we could have also written like this:
df %>%
  group_by(year) %>%
  summarize(responses = n()) %>%
  mutate(possible = c(15, 11),
         response_rate = responses / possible)

# What if I want to put the smallest survey group on top?
df %>%
  group_by(year) %>%
  summarize(responses = n()) %>%
  mutate(possible = c(15, 11),
         response_rate = responses / possible) %>%
  arrange(possible)

# new verb! arrange()
# what about descending order?

df %>%
  group_by(year) %>%
  summarize(responses = n()) %>%
  mutate(possible = c(15, 11),
         response_rate = responses / possible) %>%
  arrange(response_rate %>% desc)

# Okay: I want a table seeing the breakdown of responses by GENDER. How do I do it?
df %>%
  group_by(gender) %>%
  summarize(responses = n())

# What about a breakdown by both YEAR and GENDER?
df %>%
  group_by(year, gender) %>%
  summarize(responses = n())

# What about a breakdown by YEAR, GENDER, and HANDEDNESS?
df %>%
  group_by(year, gender, left_right) %>%
  summarize(responses = n())

# OK, now I want to add a column in this summary table with the mean and SD of height
df %>%
  group_by(year, gender) %>%
  summarize(responses = n(),
            height_mean = mean(height),
            height_sd = sd(height))

# Add mean and SD of shoe_size
df %>%
  group_by(year, gender) %>%
  summarize(responses = n(),
            height_mean = mean(height),
            height_sd = sd(height),
            shoe_mean = mean(shoe_size),
            shoe_sd = mean(shoe_size))

# Usually summary tables are accompanied by plots that show all the data.
# So let's recall our ggplot skills to make a plot of the relationship b/w height and shoe size

# Build this up bit by bit:
ggplot(df,
       aes(x=height, y=shoe_size)) +
  geom_point() +
  geom_smooth()

ggplot(df,
       aes(x=height, y=shoe_size, color=year, pch=gender)) +
  geom_point()
# show why geom_smooth() won't work

# Next level:
# I want a breakdown by GENDER of expected salary at age 40

df %>%
  group_by(gender) %>%
  summarize(expected_salary = mean(salary40))

# Oh no! What happened? It looks like R is not recognizing this column as numeric.
# Why not? Go to View and investigate.
# (Someone wrote No Idea -- a FEMALE. First survey fraud, now survey SNARK. Well well well.)
# No worries, let's try this:

df %>%
  mutate(salary40 = salary40 %>% as.numeric) %>%
  group_by(gender) %>%
  summarize(expected_salary = mean(salary40))

# Oh no! Why is it STILL not working?
# Discuss NA's and how to handle them.

df %>%
  mutate(salary40 = salary40 %>% as.numeric) %>%
  group_by(gender) %>%
  summarize(expected_salary = mean(salary40, na.rm=TRUE))

# Wow -- what a difference! We need a complimentary plot to make sense of this:

# First, let's update df with a numeric for salary40
df <- df %>% mutate(salary40 = salary40 %>% as.numeric)

ggplot(df,
       aes(x=gender, y=salary40)) +
  geom_col()

# Okay, but that's not very informative; essentially the same amount of information as the table.
# Let's try this:

ggplot(df,
       aes(x=gender, y=salary40)) +
  geom_violin() +
  geom_point()

# Hmmm ... okay, there are a couple very confident & ambitious men in the class.

df %>% filter(salary40 > 500000)
# The millionaires have mustaches!

# What if we removed these two outliers?
# Copy and paste your summary table, and use filter()

df %>%
  filter(salary40 < 500000) %>%
  group_by(gender) %>%
  summarize(expected_salary = mean(salary40, na.rm = TRUE))


ggplot(df %>% filter(salary40 < 500000),
       aes(x=gender, y=salary40)) +
  geom_violin() +
  geom_point()

# Females round less -- interesting!
# This y axis is kinda hard to read. What if we just presented the results in thousands?
# Also, it is confusing that the Male points all fall on the same values.
# Let's modify:

ggplot(df %>%
         filter(salary40 < 500000) %>%
         mutate(salary40 = salary40 / 1000),
       aes(x=gender, y=salary40)) +
  ylab('Salary at age 40 (thousands USD)') +
  geom_violin() +
  geom_jitter(alpha=.5, width=.03, height=1)

# How do we interpret this result? What hypotheses does this plot generate?


################################################################################
# A few more dplyr verbs

# God: good idea or bad idea?

# First, I want to be able to show my raw data (not a summary) of ONLY the relevant columns.
# use select()

df %>%
  select(year, gender, god, religious_family) %>%
  View

# What if I want to make better-formatted column names, with actual spaces?
df %>%
  select(Year = year,
         Gender = gender,
         `Position on God?` = god,
         `Raised in a Religious Family` = religious_family) %>%
  View


# Now I want a summary table (build this up piece by piece:)
df %>%
  select(Year = year,
         Gender = gender,
         `Position on God?` = god,
         `Raised in a Religious Family` = religious_family) %>%
  group_by(`Position on God?`, `Raised in a Religious Family`) %>%
  summarize(Responses = n()) %>%
  View

# Now I want an accompanying plot
# (showing a different way to pass data to ggplot())

df %>%
  select(Year = year,
         Gender = gender,
         `Position on God?` = god,
         `Raised in a Religious Family` = religious_family) %>%
  ggplot(aes(x= `Position on God?`, fill=`Raised in a Religious Family`)) +
  geom_bar(stat='count')

################################################################################
# Final verbs:

# Rename
df %>% rename(`Pants in the world` = pants_billion) %>% names

# Joins -- when you want to merge to datasets
# (just mentioning this, so they know it is out there)
coolness <- data.frame(dinos_whales = c('Whales', 'Dinosaurs'),
                       coolness = c('Super cool', 'Pretty cool'))
coolness

left_join(df, coolness, by='dinos_whales') %>%
  select(year, gender, dinos_whales, coolness)

################################################################################
################################################################################
################################################################################
# Homework:

# (1)
# Which local eatery is more beloved: McClurg or Mi Casa?
# And does this result change by your year at Sewanee?
# Also, does your year at Sewanee influence how much you love to cook?

# To answer these questions, create a summary table that shows
# the average love score for McClurg, Mi Casa, and for cooking, broken down by year.

# Then produce three beautiful plots displaying the data behind this summary table.
# Make sure the y axis ranges from 0 to 1 for all of them -- do so by adding this code to yoru ggplot call:   + ylim(0,10)
# Then provide a Results statement (as a commented line of code) summarizing your conclusion.

df %>%
  group_by(year) %>%
  summarize(McClurg = mean(love_for_mcclurg),
            MiCasa = mean(love_for_micasa),
            Cooking = mean(love_for_cooking))

ggplot(df, aes(x=year, y=love_for_mcclurg)) + ylim(0,10) +
  geom_violin() + geom_jitter(width=.05,height=.1, alpha=.5) # plus fanciness

ggplot(df, aes(x=year, y=love_for_micasa)) + ylim(0,10) +
  geom_violin() + geom_jitter(width=.05,height=.1, alpha=.5) # plus fanciness

ggplot(df, aes(x=year, y=love_for_cooking)) + ylim(0,10) +
  geom_violin() + geom_jitter(width=.05,height=.1, alpha=.5) # plus fanciness

#===============================================================================
# (2)
# Among E&S majors at Sewanee, do family goals differ for men and women?
# Specifically, do they have different targets for the number of children they want,
# and the age at which they first start having children?
# Create 1 summary table (include mean, min, and max summary columns for each variable)
# as well as 2 beautiful summary plots to answer this question,
# Then provide a Results statement (as a commented line of code) summarizing your conclusion.

df %>%
  mutate(age_at_first_child = age_at_first_child %>% as.numeric) %>%
  group_by(gender) %>%
  summarize(n_children_mean = mean(n_children),
            n_children_max = max(n_children),
            n_children_min = min(n_children),
            age_mean = mean(age_at_first_child, na.rm=TRUE),
            age_min = min(age_at_first_child, na.rm=TRUE),
            age_max = max(age_at_first_child, na.rm=TRUE))

ggplot(df,
       aes(x=gender,
           y=n_children)) +
  geom_violin() +
  geom_jitter(width=.05, height=.1)
# plus fanciness

ggplot(df %>%
         mutate(age_at_first_child = age_at_first_child %>% as.numeric),
       aes(x=gender,
           y=age_at_first_child)) +
  geom_violin() +
  geom_jitter(width=.05, height=.1)
# plus fanciness

# Conclusion: moderate evidence for larger families and earlier starts in females.

#===============================================================================
# (3)
# EXTRA CREDIT:
# Pose a research question of your own that this survey may be able to answer.
# The more creative the better.
# Create 1 summary table and 1 beautiful summary plot to answer this question,
# Then provide a Results statement (as a caption to your plot) summarizing your conclusion.



################################################################################
################################################################################
################################################################################
# Homework:

# (1)
# Which local eatery is more beloved: McClurg or Mi Casa?
# And does this result change by your year at Sewanee?
# Also, does your year at Sewanee influence how much you love to cook?

# To answer these questions, create a summary table that shows
# the average love score for McClurg, Mi Casa, and for cooking, broken down by year.

# Then produce three beautiful plots displaying the data behind this summary table.
# Make sure the y axis ranges from 0 to 1 for all of them -- do so by adding this code to yoru ggplot call:   + ylim(0,10)
# Then provide a Results statement (as a commented line of code) summarizing your conclusion.

#===============================================================================
# (2)
# Among E&S majors at Sewanee, do family goals differ for men and women?
# Specifically, do they have different targets for the number of children they want,
# and the age at which they first start having children?
# Create 1 summary table (include mean, min, and max summary columns for each variable)
# as well as 2 beautiful summary plots to answer this question,
# Then provide a Results statement (as a commented line of code) summarizing your conclusion.

#===============================================================================
# (3)
# EXTRA CREDIT:
# Pose a research question of your own that this survey may be able to answer.
# The more creative the better.
# Create 1 summary table and 1 beautiful summary plot to answer this question,
# Then provide a Results statement (as a caption to your plot) summarizing your conclusion.

