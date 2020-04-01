require(ggplot)
require(dplyr)
require(tidyr)
require(ggplotly)
require(gganimate)
require(readr)
require(scales)

alco_covid19 <- tibble(
  Date = c("2020-03-14", "2020-03-15", "2020-03-16", "2020-03-17", "2020-03-18", "2020-03-19", "2020-03-20", "2020-03-21", "2020-03-22", "2020-03-23", "2020-03-24", "2020-03-25", "2020-03-26", "2020-03-27", "2020-03-28", "2020-03-29", "2020-03-30", "2020-03-31", "2020-04-01"),
  Cases = c(2, 4, 6, 7, 12, 18, 28, 31, 40, 48, 58, 88, 133, 158, 219, 265, 290, 325, 356),
  Deaths = c(0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2),
  Hospitalizations = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 13, 20, 25, 31, 35, 38, 51, 61)
) %>%
  mutate(Date = as.POSIXct(Date))

pivot <- alco_covid19 %>%
  pivot_longer(-Date)

plot <- ggplot(pivot, aes(x = Date, y = value, color = name, group = name)) +
  geom_point() +
  geom_line() +
  theme_bw() +
  labs(x = "",
       y = "", 
       title = "Allegheny County Covid-19 Cases",
       color = "")

plot

plot +
  transition_reveal(Date) +
  view_follow()

animate(plot, end_pause = 10)

anim_save("alco.gif", animation = last_animation())

covid_States <- read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv")

pa_covid <- covid_States %>%
  filter(state == "Pennsylvania", cases > 0)

pa_plot <- ggplot(pa_covid, aes(x = date, y = cases, color = county, group = county)) +
  geom_point() +
  geom_line() +
  geom_text(aes(label = county)) +
  theme_bw() +
  labs(x = "",
       y = "", 
       title = "PA Covid-19 Cases by County",
       color = "") +
  theme(legend.position = "none") +
  scale_y_continuous(labels = comma) +
  transition_reveal(date) +
  view_follow()

animate(pa_plot, end_pause = 10)

anim_save("pa_counties.gif", animation = last_animation())

states_agg <- covid_States %>%
  group_by(date, state) %>%
  summarise(cases = sum(cases), deaths = sum(deaths)) %>%
  filter(cases > 0)

states_plot <- ggplot(states_agg, aes(x = date, y = cases, color = state, group = state)) +
  geom_point() +
  geom_line() +
  geom_text(aes(label = state)) +
  theme_bw() +
  labs(x = "",
       y = "", 
       title = "US Covid-19 Cases by State",
       color = "") +
  theme(legend.position = "none") +
  scale_y_continuous(labels = comma) +
  transition_reveal(date) +
  view_follow()

animate(states_plot, end_pause = 10)

anim_save("us_states.gif", animation = last_animation())
