library(tidyverse)
library(httr)
library(readxl)
library(tidytext)
library(ggpmthemes)
library(glue)

theme_set(theme_poppins())

GET(
  "https://query.data.world/s/grfxn7os5xwnxmr2n43tagk2vnroie",
  write_disk(tf <- tempfile(fileext = ".xlsx"))
)
df <- read_excel(tf)

df <- df %>%
  pivot_longer(
    -c("Continent", "Country", "City", "Year"),
    names_to = "month",
    values_to = "sunshine_duration"
  ) %>%
  janitor::clean_names() %>%
  select(-year) %>%
  distinct()

df

df <- df %>%
  group_by(continent, country, city) %>%
  summarise(yearly_sunshine_duration = sum(sunshine_duration)) %>%
  ungroup() %>%
  group_by(continent) %>%
  top_n(15, yearly_sunshine_duration) %>%
  ungroup() %>%
  mutate(city = fct_reorder2(city, yearly_sunshine_duration, continent))

df <- df %>%
  group_nest(continent) %>%
  mutate(data2 = map2(continent, data, function(continent, data) {
    data %>%
      mutate(continent = continent) %>%
      add_row(continent = continent) %>%
      add_row(continent = continent) %>%
      add_row(continent = continent)
  })) %>%
  select(-continent, -data) %>%
  unnest(data2) %>%
  rowid_to_column() %>%
  mutate(color = vapoRwave:::floralShoppe_palette[2:8][factor(continent)]) %>%
  mutate(continent = fct_inorder(continent))

df

label_data <- df
number_of_bar <- nrow(label_data)
angle <- 90 - 360 * (label_data$rowid - 0.5) / number_of_bar
label_data$hjust <- ifelse(angle < -90, 0, 1)
label_data$angle <- ifelse(angle < -90, angle + 180, angle)

colors <- df %>%
  distinct(continent, color)

# Plot --------------------------------------------------------------------

df %>%
  ggplot(aes(x = factor(rowid), y = yearly_sunshine_duration, fill = color)) +
  geom_col() +
  geom_text(
    data = label_data,
    aes(
      x = factor(rowid),
      y = 0.95 * yearly_sunshine_duration,
      label = glue("{yearly_sunshine_duration} hours"),
      hjust = hjust
    ),
    color = "#3c3c3c",
    fontface = "plain",
    size = 2.5,
    angle = label_data$angle,
    inherit.aes = FALSE,
    family = "Source Sans Pro"
  ) +
  geom_text(
    data = label_data,
    aes(
      x = factor(rowid),
      y = -100,
      label = city,
      hjust = hjust,
      color = color
    ),
    # color = "white",
    fontface = "plain",
    size = 2.5,
    angle = label_data$angle,
    inherit.aes = FALSE,
    family = "Source Sans Pro"
  ) +
  scale_y_continuous(expand = c(0, 0), limits = c(-5000, NA)) +
  coord_polar() +
  scale_color_identity(
    breaks = colors$color,
    labels = colors$continent,
  ) +
  scale_fill_identity(
    breaks = colors$color,
    labels = colors$continent,
    guide = guide_legend(
      label.position = "top",
      title = NULL,
      keywidth = unit(3, "cm"),
      nrow = 2
    )
  ) +
  labs(
    title = "Sunniest cities in the world",
    subtitle = str_wrap(
      "Do you have winter blues? These are the 15 sunniest cities by continent in terms of annual sunshine hours.",
      75
    ),
    caption = "SWD challenge: FEB 2020 (barplot)\n Data Source: Wikipedia\nVisualization: @philmassicotte"
  ) +
  theme(
    legend.position = "bottom",
    axis.text = element_blank(),
    panel.grid = element_blank(),
    panel.border = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    plot.background = element_rect(fill = "#3c3c3c"),
    panel.background = element_rect(fill = "#3c3c3c"),
    legend.background = element_rect(fill = "#3c3c3c"),
    legend.key = element_rect(color = "#3c3c3c"),
    legend.text = element_text(color = "white", face = "bold"),
    legend.margin = margin(t = unit(-30, "cm")),
    plot.title = element_text(
      color = "white",
      family = "Sofadi One",
      margin = margin(1, 0, 0, 0, unit = "cm"),
      hjust = 0.5,
      size = 24
    ),
    plot.subtitle = element_text(
      color = "white",
      family = "Economica",
      margin = margin(1, 0, 0, 0, unit = "cm"),
      hjust = 0.5,
      size = 20
    ),
    plot.caption = element_text(color = "gray75", size = 8)
  )

pdf_file <- here::here("graphs", "swd_feb_2020.pdf")
png_file <- here::here("graphs", "swd_feb_2020.png")

ggsave(pdf_file,
  device = cairo_pdf,
  width = 8,
  height = 10
)

knitr::plot_crop(pdf_file)

bitmap <- pdftools::pdf_render_page(pdf_file, dpi = 600)
png::writePNG(bitmap, png_file)
