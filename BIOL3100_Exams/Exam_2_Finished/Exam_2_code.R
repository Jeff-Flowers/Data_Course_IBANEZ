### Prepare to have your socks knocked off. I've been training offscreen.

# 1.
death <- read.csv("unicef-u5mr.csv")

# 2.
install.packages("tidyverse")
library(tidyverse)
tidydeath <- death |>
  pivot_longer(
    cols = starts_with("U5MR."),
    names_to = "Year",
    names_prefix = "U5MR.",
    names_transform = list(Year = as.numeric),
    values_to = "U5MR"
  )

# 3.
install.packages("ggplot2")
library(ggplot2)
ggplot(tidydeath, aes(x = Year, y = U5MR, group = CountryName)) +
  geom_line(size = .9) +
  facet_wrap(~ Continent)

# 4. See IBANEZ_Plot_1.png in folder.

# 5.
tidydeath |>
  group_by(Continent, Year) |>
  summarize(meanie = mean(U5MR, na.rm = T)) |>
  ggplot(aes(x = Year, y = meanie, color = Continent)) +
  geom_line(size = 2)

# 6. See IBANEZ_Plot_2.png in folder.

# 7.
mod1 <- lm(U5MR ~ Year, data = tidydeath)
mod2 <- lm(U5MR ~ Year + Continent, data = tidydeath)
mod3 <- lm(U5MR ~ Year * Continent, data = tidydeath)

# 8.
AIC(mod1, mod2, mod3)
### Model 3 is the best with the lowest AIC score to no surprise. It allows the most freedom in expressing different aspects of the data, fist by splitting the world mean
### into continents, then allowing the continents to have different slopes, representing how the U5MR starts in different places and changes at different rates.

#9.
pred1 <- predict(mod1, newdata = tidydeath)
pred2 <- predict(mod2, newdata = tidydeath)
pred3 <- predict(mod3, newdata = tidydeath)

plot_data <- tidydeath
plot_data$mod1 <- pred1
plot_data$mod2 <- pred2
plot_data$mod3 <- pred3
plot_data <- plot_data |>
  pivot_longer(cols = c(mod1, mod2, mod3), names_to = "Model", values_to = "Prediction")

ggplot(plot_data, aes(x = Year, y = Prediction, group = interaction(CountryName, Continent), color = Continent)) +
  geom_line(size = 2) +
  facet_wrap(~ Model)

# 10.
# ehh im good
