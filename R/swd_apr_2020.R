library(tidyverse)
library(ggpmthemes)

theme_set(theme_light_modified(base_family = "Roboto Condensed"))

url <-
  pins::pin("https://datahub.io/core/covid-19/r/worldwide-aggregated.csv")

df <- read_csv(url) %>%
  janitor::clean_names()

df

# Relative numbers --------------------------------------------------------

lab <- tibble(
  date = as.Date(
    c(
      "2020-02-11",
      "2020-01-30",
      "2020-03-11",
      "2020-04-02",
      "2020-04-15"
    )
  ),
  desc = c(
    "WHO announced a name for the new coronavirus disease: COVID-19",
    "The outbreak was declared a Public Health Emergency of International Concern",
    "Coronavirus outbreak officially declared a pandemic",
    "One milion cases reported",
    "Two milions cases reported"
  )
)

df %>%
  select(-increase_rate) %>%
  mutate(total = confirmed + recovered + deaths) %>%
  mutate(across(c("confirmed", "recovered", "deaths"), ~ . / total)) %>%
  select(-total) %>%
  pivot_longer(-date, names_to = "type", values_to = "n") %>%
  mutate(type = str_to_upper(type)) %>%
  ggplot(aes(x = date, y = n, fill = type)) +
  geom_area() +
  geom_segment(
    data = lab,
    aes(x = date, xend = date, y = 0, yend = 0.75),
    lty = 2,
    size = 0.5,
    inherit.aes = FALSE,
    color = "#3c3c3c"
  ) +
  geom_point(
    data = lab,
    aes(x = date, y = 0.75),
    inherit.aes = FALSE,
    color = "#3c3c3c",
    size = 4
  ) +
  geom_text(
    data = lab,
    aes(x = date, y = 0.95, label = date),
    inherit.aes = FALSE,
    color = "#3c3c3c",
    fontface = "bold",
    size = 4
  ) +
  geom_text(
    data = lab,
    aes(x = date, y = 0.90, label = str_wrap(desc, 25)),
    inherit.aes = FALSE,
    color = "#3c3c3c",
    size = 2.5,
    vjust = 1
  ) +
  scale_y_continuous(labels = scales::label_percent(), expand = c(0, 0)) +
  scale_x_date(expand = expansion(mult = c(0, 0))) +
  labs(
    y = "Proportion of cases",
    x = NULL,
    title = "COVID-19 relative cases in the World",
    caption = "SWD challenge: April 2020 (area graphs)\nData Source: https://datahub.io\nVisualization: @philmassicotte"
  ) +
  scale_fill_manual(
    values = c(
      "CONFIRMED" = "#FFAE00",
      "DEATHS" = "#582000",
      "RECOVERED" = "#669242"
    ),
    guide = guide_legend(
      title = NULL,
      label.position = "top",
      label.theme = element_text(color = "white", family = "Paytone One"),
      keyheight = unit(0.3, "cm"),
      keywidth = unit(3, "cm")
    )
  ) +
  theme(
    panel.border = element_blank(),
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "#36405C"),
    plot.background = element_rect(fill = "#36405C"),
    plot.title = element_text(
      color = "white",
      size = 28,
      family = "Baloo Chettan 2"
    ),
    plot.caption = element_text(color = "gray75", size = 8),
    legend.background = element_rect(fill = "#36405C"),
    legend.key = element_blank(),
    legend.position = "bottom",
    axis.ticks = element_blank(),
    axis.text = element_text(color = "gray75"),
    axis.title = element_text(color = "gray85")
  )

ggsave(
  here::here("graphs", "swd_apr_2020.png"),
  dpi = 600,
  type = "cairo",
  width = 12, height = 6
)
