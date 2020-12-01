require(ggplot)
require(dplyr)
require(tidyr)
require(ggplotly)
require(gganimate)
require(readr)
require(scales)
require(gifski)

alco_covid19 <- tibble(
  Date = c("2020-03-13", "2020-03-14", "2020-03-15", "2020-03-16", "2020-03-17", "2020-03-18", "2020-03-19", "2020-03-20", "2020-03-21", "2020-03-22", "2020-03-23", "2020-03-24", "2020-03-25", "2020-03-26", "2020-03-27", "2020-03-28", "2020-03-29", "2020-03-30", "2020-03-31", "2020-04-01", "2020-04-02", "2020-4-03", "2020-04-04", "2020-04-05", "2020-04-06", "2020-04-07", "2020-04-08", "2020-04-09", "2020-04-10", "2020-04-11", "2020-04-12", "2020-04-13", "2020-04-14", "2020-04-15"),
  Cases = c(0, 2, 4, 6, 7, 12, 18, 28, 31, 40, 48, 58, 88, 133, 158, 219, 265, 290, 325, 356, 419, 476, 552, 605,  642, 689, 720, 759, 788, 836, 857, 876, 893, 904),
  Deaths = c(0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 3, 4, 4, 6, 10, 12, 18, 19, 19, 21, 24, 26),
  Hospitalizations = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 13, 20, 25, 31, 35, 38, 51, 61, 70, 78, 86, 87, 92, 101, 113, 117, 124, 130, 130, 134, 142, 146)
) %>%
  mutate(Date = as.Date(Date),
         `New Cases` = Cases - lag(Cases))

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

ani_plt <- plot +
  labs(subtitle = "Date: {frame_along}") +
  transition_reveal(Date) +
  view_follow()

animate(ani_plt, end_pause = 10, duration = 20, fps = 20, width = 400, height = 400, renderer = gifski_renderer())
anim_save("alco.gif", animation = last_animation())

png("alco.png")
print(plot)
dev.off()

covid_states <- read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv")

pa_covid <- covid_states %>%
  filter(state == "Pennsylvania", cases > 0)

pa_plot <- ggplot(pa_covid, aes(x = date, y = cases, color = county, group = county)) +
  geom_point() +
  geom_line() +
  geom_text(aes(label = county)) +
  theme_bw() +
  labs(x = "",
       y = "",
       title = "PA Covid-19 Cases by County",
       subtitle = "Date: {frame_along}",
       color = "") +
  theme(legend.position = "none") +
  scale_y_continuous(labels = comma) +
  transition_reveal(date) +
  view_follow()

animate(pa_plot, end_pause = 10, duration = 20, fps = 20, width = 400, height = 400, renderer = gifski_renderer())
anim_save("pa_counties.gif", animation = last_animation())

states_agg <- covid_states %>%
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
       subtitle = "Date: {frame_along}",
       color = "") +
  theme(legend.position = "none") +
  scale_y_continuous(labels = comma) +
  transition_reveal(date) +
  view_follow()

animate(states_plot, end_pause = 10, duration = 20, fps = 20, width = 400, height = 400, renderer = gifski_renderer())
anim_save("us_states.gif", animation = last_animation())

# Deaths
states_deaths <- states_agg %>%
  filter(deaths > 0)

state_deaths <- ggplot(states_deaths, aes(x = date, y = deaths, color = state, group = state)) +
  geom_point() +
  geom_line() +
  geom_text(aes(label = state)) +
  theme_bw() +
  labs(x = "",
       y = "", 
       title = "US Covid-19 Deaths by State",
       subtitle = "Date: {frame_along}",
       color = "") +
  theme(legend.position = "none") +
  scale_y_continuous(labels = comma) +
  transition_reveal(date) +
  view_follow()

animate(state_deaths, end_pause = 10, duration = 20, fps = 20, width = 400, height = 400, renderer = gifski_renderer())
anim_save("us_states_deaths.gif", animation = last_animation())

world <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")

coords <- world %>%
  distinct(`Province/State`, `Country/Region`, Lat, Long)

p_world <- world %>%
  select(-c(Lat, Long)) %>%
  pivot_longer(-c(`Province/State`, `Country/Region`), names_to = "date", values_to = "cases") %>%
  filter(cases > 0) %>%
  mutate(date = as.Date(date, format = "%m/%d/%y")) %>%
  group_by(`Country/Region`, date) %>%
  summarise(cases = sum(cases))

world_cases <- ggplot(p_world, aes(x = date, y = cases, color = `Country/Region`, group = `Country/Region`)) +
  geom_point() +
  geom_line() +
  geom_text(aes(label = `Country/Region`)) +
  theme_bw() +
  labs(x = "",
       y = "", 
       title = "Global Covid-19 Cases by Country/Region",
       subtitle = "Date: {frame_along}",
       color = "") +
  theme(legend.position = "none") +
  scale_y_continuous(labels = comma) +
  transition_reveal(date) +
  view_follow()

animate(world_cases, end_pause = 10, duration = 45, fps = 20, width = 400, height = 400, renderer = gifski_renderer())
anim_save("world_cases.gif", animation = last_animation())