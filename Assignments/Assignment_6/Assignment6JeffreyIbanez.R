Biolog <- read.csv("C:/Users/jeyba/Desktop/Data_Course_IBANEZ/Data/BioLog_Plate_Data.csv")

Biolog
unique(Biolog$Sample.ID)

install.packages("tidyverse")
library(tidyverse)

TidyBiolog <- Biolog |> # Makes time data long form, and a number
  pivot_longer(
    cols = c("Hr_24", "Hr_48", "Hr_144"),
    names_to = "Time",
    values_to = "Absorbance"
  ) |>
  mutate(
    Time = parse_number(Time)
  )

TidyBiolog # Juuuust checking

TidyBiolog <- TidyBiolog |> # Adding sample type column
  mutate(Type = case_when(
    str_detect(Sample.ID, "Soil_1") ~ "Soil",
    str_detect(Sample.ID, "Soil_2") ~ "Soil",
    str_detect(Sample.ID, "Clear_Creek") ~ "Water",
    str_detect(Sample.ID, "Waste_Water") ~ "Water"
  ))

TidyBiolog[TidyBiolog$Type == "Soil",] # Juuuuuuuust checking

plot_data_smooth <- TidyBiolog |> # Mean per type data
  filter(Dilution == 0.1) |>
  group_by(Substrate, Type, Time) |>
  summarize(Mean_Absorbance = mean(Absorbance), .groups = "drop")

ggplot(plot_data_smooth, aes(x = Time, y = Mean_Absorbance, color = Type, group = Type)) +
  geom_line(linewidth = 1) +
  facet_wrap(~ Substrate) +
  labs(
    title = "Absorbance over Time (Dilution = 0.1)",
    x = "Time (Hours)",
    y = "Mean Absorbance",
    color = "Type"
  )

install.packages("gganimate")
library(gganimate)
install.packages("gifski")
library(gifski)

itaconic_dat <- TidyBiolog |>
  filter(Substrate == "Itaconic Acid") |>
  group_by(Sample.ID, Dilution, Time, Type) |>
  summarize(Mean_Absorbance = mean(Absorbance), .groups = "drop")

p <- ggplot(itaconic_dat, aes(x = Time, y = Mean_Absorbance, color = Sample.ID, group = Sample.ID)) +
  geom_line(linewidth = 1) +
  facet_wrap(~ Dilution, labeller = label_both) + 
  labs(
    title = "Substrate: Itaconic Acid",
    x = "Time",
    y = "Mean_absorbance",
    color = "Sample ID"
  ) +
  transition_reveal(Time)

anim <- animate(p, nframes = 100, fps = 10, duration = 5, renderer = gifski_renderer())

anim_save("itaconic_animation.gif", animation = anim)

# This was was a doozy and its 17 minutes late sorry
