require(jsonlite)
require(dplyr)
require(tidyr)
require(ggplot2)
require(ggrepel)

hospital <- fromJSON( "https://data.pa.gov/resource/kayn-sjhx.json?County=Allegheny") %>%
  mutate(date = as.Date(date)) %>%
  mutate_at(vars(ends_with("avail")), as.numeric) %>%
  mutate_at(vars(ends_with("total")), as.numeric) %>%
  mutate_at(vars(ends_with("mean")), as.numeric) %>%
  mutate_at(vars(ends_with("percent")), as.numeric) %>%
  mutate_at(vars(starts_with("covid")), as.numeric) %>%
  mutate_at(vars(starts_with("vents")), as.numeric) %>%
  arrange(date) %>%
  select(-county)

long <- hospital %>%
  pivot_longer(-date)

graph_data <- long %>%
  filter(name %in% c('icu_avail', 'covid_vents', 'covid_patients', 'ped_avail')) %>%
  mutate(value = case_when(name == 'ped_avail' & date == as.Date('2020-10-01') ~ 0,
                           TRUE ~ value),
         name = case_when(name == 'icu_avail' ~ "Avail. Adult ICU Beds",
                          name == 'covid_patients' ~ 'Covid-19 Patients',
                          name == 'covid_vents' ~ 'Covid-19 Ventilators',
                          name == 'ped_avail' ~ 'Avail. Pediatric ICU Beds'),
         name = as.factor(name),
         lab = case_when(date == max(date) ~ prettyNum(value, big.mark = ","),
                           TRUE ~ "")) %>%
  filter(value > 0)

graph_data %>%
  filter(grepl('Covid-19',name)) %>%
  ggplot(aes(x = date, y = value, color = name, group = name)) +
    scale_color_brewer(palette = 'Set2') +
    geom_point() +
    geom_line() +
    geom_text_repel(aes(label = lab), 
                    size = 2.5, 
                    hjust = 1, 
                    vjust = -.5, 
                    show.legend = F, 
                    segment.color = 'transparent') +
    theme_minimal() +
    theme(legend.position = 'bottom') +
    labs(title = "Hospital Utilization in Allegheny County",
         x = "Date",
         y = "Count",
         caption = "Source: PA Department of Health",
         color = "")

graph_data %>%
  filter(!grepl('Covid-19',name)) %>%
  ggplot(aes(x = date, y = value, color = name, group = name)) +
  scale_color_manual(values = c("#e7298a", "#7570b3")) +
  geom_point() +
  geom_line() +
  geom_text_repel(aes(label = lab), 
                  size = 2.5, 
                  hjust = 1, 
                  vjust = -.5, 
                  show.legend = F, 
                  segment.color = 'transparent') +
  theme_minimal() +
  theme(legend.position = 'bottom') +
  labs(title = "ICU Utilization in Allegheny County",
       x = "Date",
       y = "Count",
       caption = "Source: PA Department of Health",
       color = "")
