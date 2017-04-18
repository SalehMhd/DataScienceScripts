#install.packages("tidyverse")
#install.packages(c("nycflights13", "gapminder", "Lahman"))

library("tidyverse")

mtcars <- mtcars
ncol(mpg)
nrow(mpg)

?mpg
?geom_point

#Scatter Plots == Point Geom

ggplot(data = mpg) + geom_point(mapping = aes(x = displ, y = hwy, shape = class)) #size, color
ggplot(data = mpg) + geom_point(mapping = aes(x = displ, y = hwy, color = fl == "p", shape = factor(cyl))) #size, color

#Factes : categorial variables
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) +
  facet_wrap(~ class, nrow = 2)

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) +
  facet_grid(drv ~ cyl)

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) +
  facet_grid(. ~ cyl) #facet_grid(drv ~ .)


#Bar charts == Bar geom
#line charts == line geom (smooth)
#box plot == box geom

ggplot(data = mpg) + 
  geom_smooth(mapping = aes(x = displ, y = hwy, linetype = drv))