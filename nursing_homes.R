require(httr)
require(readxl)
require(dplyr)
require(tidyr)
require(readr)

GET("https://www.health.pa.gov/topics/Documents/Diseases%20and%20Conditions/COVID-19%20LTCF%20Data_5-19-20.xlsx", write_disk(tf <- tempfile(fileext = ".xlsx")))
nursing <- read_excel(tf) %>%
  mutate_at(vars(starts_with("Number")),  as.numeric)

totals <- nursing %>%
  pivot_longer(cols = starts_with("Number")) %>%
  group_by(COUNTY, name) %>%
  summarise(value = sum(value, na.rm = TRUE)) %>%
  pivot_wider()
  

covid_states <- read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv")

pa_covid <- covid_states %>%
  filter(state == "Pennsylvania", cases > 0, date == as.Date("2020-05-19")) %>%
  mutate(county = toupper(county))

perc <- left_join(totals, pa_covid, by = c("COUNTY" = "county")) %>%
  mutate(case_share = round(`Number Of Resident Cases` / cases * 100, 2),
         death_share = round(`Number Of Deaths` / deaths * 100, 2))

state <- nursing %>%
  pivot_longer(cols = starts_with("Number")) %>%
  group_by(name) %>%
  summarise(value = sum(value, na.rm = T)) %>%
  pivot_wider() %>%
  bind_cols(summarise(.data = pa_covid, cases = sum(cases), deaths = sum(deaths))) %>%
  mutate(case_share = round(`Number Of Resident Cases` / cases * 100, 2),
         death_share = round(`Number Of Deaths` / deaths * 100, 2))
            