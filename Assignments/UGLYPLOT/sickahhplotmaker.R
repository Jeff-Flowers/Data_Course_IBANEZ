install.packages("ggplot2")
library(ggplot2)
install.packages("showtext")
library(showtext)
install.packages("ggpattern")
library(ggpattern)
install.packages("magick")
library(magick)
install.packages("ggimage")
library(ggimage)

showtext_auto()
font_add(family = "Covid", regular = "./zai_COVID-19VaCcine.ttf")

mtcars2 <- mtcars
mtcars2$model <- rownames(mtcars2)

ggplot(mtcars2, aes(x = mpg, y = hp, label = model)) +
  geom_rect_pattern(inherit.aes = FALSE, data = data.frame(1), xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, pattern = "image", pattern_type = "expand", pattern_filename = "./yoda.jpg") +
  geom_image(aes(image = "./fox.png"), size = .1, by = "width") +
  geom_text(family = "Covid", color = "red") +
  labs(title = "Coolness vs lame factor", subtitle = "(Horsepower vs mpg)",
       x = "Miles per Gallon of Gasoline i Assume, or perhaps diesel",
       y = "Equivalent Number of Horses") +
  theme(plot.title = element_text(hjust = .5, size = 20, family = "Covid", color = "red"),
        plot.subtitle = element_text(hjust = .5, family = "Covid", color = "white"),
        plot.background = element_rect(fill = "yellow"))
