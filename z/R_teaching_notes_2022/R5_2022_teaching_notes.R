# Tutorial 5 teaching notes

#install.packages('gapminder')

# Load the gapminder into your R sessions
library(gapminder)
data(gapminder)
head(gapminder)

# Other packages
library(ggplot2)
library(dplyr)


ggplot(gapminder,
       aes(x=continent, y=lifeExp, color=year)) +
  geom_jitter() +
  ylab('Life expectancy') +
  xlab('Continent') +
  labs(title='The geography of life expectancy')



ggplot(gapminder %>% filter(continent == 'Europe'),
       aes(x=pop,
           y=country)) +
  geom_col(fill='blue') +
  labs(title = 'Population sizes of countries in Europe') +
  xlab('Population size') +
  ylab(NULL)



# Write code that displays the unique years included in this dataset
gapminder$year %>% unique


ggplot(gapminder %>% filter(year %in% c(1952, 1972, 1992, 2007)),
       aes(x=lifeExp,
           y=gdpPercap,
           size=pop,
           color=continent)) +
  geom_point() +
  scale_y_continuous(trans='log') +
  facet_wrap(~year)


library(plotly)


ggplotly()


x <- 1:100
y <- 2*x + rnorm(length(x),0,40)
df <- data.frame(x,y)
ggplot(df,aes(x=x, y=y)) +
  geom_point() +
  geom_smooth(method='lm')


