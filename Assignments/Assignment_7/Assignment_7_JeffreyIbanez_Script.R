library(tidyverse)

# Importing the datasets
airlines <- read.csv("airlines.csv")
airports <- read.csv("airports.csv")
janflight <- read.csv("jan_flights.csv")
jansnow <- read.csv("Jan_snowfall.csv")

# Just reading them to look at them.
airlines
airports
janflight
jansnow

# Making a column of actual dates.
janflight$DATE <- make_date(year = janflight$YEAR, month = janflight$MONTH, day = janflight$DAY)

### Randomly gets 20 entries to make check if stuff worked.
janflight[sample.int(nrow(janflight),20),]
jansnow[sample.int(nrow(jansnow),20),]


jansnow$Date <- ymd(jansnow$Date)

### Used an inner join because theres no point to including rows without precip data.
jandata <- inner_join(x = janflight, y = jansnow, by = join_by(DATE == Date, ORIGIN_AIRPORT == iata))
jandata

### Also gonna join the state and airline data, and fix a name overlap
jandata <- inner_join(x = jandata, y = airports, by = join_by(ORIGIN_AIRPORT == IATA_CODE))
jandata <- inner_join(x = jandata, y = airlines, by = join_by(AIRLINE == IATA_CODE))
jandata <- jandata |> rename(AIRLINE_NAME = AIRLINE.y)


### Time for answers
### Is departure delay correlated with snowfall?
cor(x = jandata$snow_precip_cm, y = jandata$DEPARTURE_DELAY, use = "complete.obs")
# output = 0.00465418, but I think the data is being confounded by the randomness of flight delays, so I'm going to take the average flight delay per airport per day
daily_data <- jandata |>
  group_by(ORIGIN_AIRPORT, DATE) |>
  summarize(mean_delay = mean(DEPARTURE_DELAY, na.rm = T), snow_precip_cm = mean(snow_precip_cm))
daily_data
cor(x = daily_data$snow_precip_cm, y = daily_data$mean_delay, use = "complete.obs")
# output = 0.01330067, a much stronger positive relationship (meaning higher snowfall means longer delays)
# ggplot for fun
ggplot(daily_data, aes(x = snow_precip_cm, y = mean_delay))+
  geom_point()
# Ok now that I'm looking at the graph, the relationship seems pretty weak to nonexistent.

### Average departure delays by state over time
state_data <- jandata |>
  group_by(STATE, DATE) |>
  summarize(mean_delay = mean(DEPARTURE_DELAY, na.rm = T))
state_data  

ggplot(state_data, aes(x = DATE, y = mean_delay, color = STATE)) +
  geom_line() +
  facet_wrap(~ STATE)

### Average departure delays by airline over time
airline_data <- jandata |>
  group_by(AIRLINE_NAME, DATE) |>
  summarize(mean_delay = mean(DEPARTURE_DELAY, na.rm = T))
airline_data  

ggplot(airline_data, aes(x = DATE, y = mean_delay, color = AIRLINE_NAME)) +
  geom_line(size = 1) +
  facet_wrap(~ AIRLINE_NAME)

### Effect of snowfall on departure AND arrival delays
daily_data2 <- jandata |>
  group_by(ORIGIN_AIRPORT, DATE) |>
  summarize(mean_dep_delay = mean(DEPARTURE_DELAY, na.rm = T), mean_arr_delay = mean(ARRIVAL_DELAY, na.rm = T), snow_precip_cm = mean(snow_precip_cm))
daily_data2

daily_data2 <- daily_data2 |>
  pivot_longer(
    cols = c(mean_dep_delay, mean_arr_delay),
    names_to = "delay_type",
    values_to = "minutes")

ggplot(daily_data2, aes(x = snow_precip_cm, y = minutes, color = delay_type)) +
  geom_point() +
  facet_wrap(~ delay_type)

### WHOOOOOO

