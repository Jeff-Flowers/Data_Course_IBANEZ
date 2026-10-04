# Jeffrey Ibanez - Data Anylysis for Biologists - Exam 1
# I. Read the cleaned_covid_data.csv file into an R data frame.
# Easiest points ever

covid <- read.csv("cleaned_covid_data.csv")

# II. Subset the data set to just show states that begin with “A” and save this as an object called A_states.
# The set of states starting with A is small enough that I can just match them by name.

library(tidyverse)
unique(covid$Province_State)
A_States <- c("Alabama", "Alaska", "Arizona", "Arkansas")

ACovid <- covid |> filter(Province_State %in% A_States)
ACovid

# III. Create a plot of that subset showing Deaths over time, with a separate facet for each state.

ggplot(ACovid, aes(x = Last_Update, y = Deaths)) +
  geom_point() +
  geom_smooth(method = "loess", se = FALSE) +
  facet_wrap(~ Province_State, scales = "free")

# IV.Find the “peak” of Case_Fatality_Ratio for each state and save this as a new data frame object called state_max_fatality_rate.
# I did this in a stupid way
states <- unique(covid$Province_State)

state_max_fatality_rate <- data.frame(Province_State = character(), Maximum_Fatality_Ratio = numeric())

# for loop that filters by each unique state, gets the max, then adds it to a dataframe
for (i in states){
  single_state <- covid |> filter(Province_State == i)
  max_val <- max(single_state$Case_Fatality_Ratio, na.rm = TRUE)
  max_row <- data.frame(Province_State = i, Maximum_Fatality_Ratio = max_val)
  state_max_fatality_rate <- rbind(state_max_fatality_rate, max_row)
}
state_max_fatality_rate <- arrange(state_max_fatality_rate, desc(Maximum_Fatality_Ratio))

state_max_fatality_rate

# V.Use that new data frame from task IV to create another plot.

ggplot(state_max_fatality_rate, aes(x = reorder(Province_State,-Maximum_Fatality_Ratio), y = Maximum_Fatality_Ratio)) +
  geom_col() +
  labs(x = "Region", y = "Maximum Fatality Ratio") +
  theme(axis.text.x = element_text(angle = 90))
  
# VI. BONuS
# No
