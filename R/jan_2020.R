library(tidyverse)
library(data.table)
library(sf)
library(ggpmthemes)

theme_set(theme_poppins())

files <- fs::dir_ls("data/raw/jan_2020/namesbystate/", glob = "*.TXT")

top_names <- function(file) {
  df <-
    fread(file, col.names = c("state", "sex", "year", "name", "count")) %>%
    as_tibble()

  df <- df %>%
    group_by(year, sex) %>%
    top_n(1, count) %>%
    ungroup()


  return(df)
}

df <- map_df(files, top_names)

us_hex <- st_read("data/raw/jan_2020/us_states_hexgrid.geojson") %>%
  rename(state = iso3166_2) %>%
  mutate_if(is.factor, as.character)

us_hex %>%
  ggplot() +
  geom_sf()

df <- inner_join(us_hex, df)

# Plot --------------------------------------------------------------------

df_viz <- df %>%
  filter(year %% 25 == 0) %>%
  mutate(sex = case_when(
    sex == "F" ~ "Female",
    sex == "M" ~ "Male"
  ))

# Sometimes there is a tie, just keep the first one
df_viz <- df_viz %>%
  distinct(sex, year, state, .keep_all = TRUE)

p <- df_viz %>%
  ggplot() +
  geom_sf(aes(fill = count), size = 0.1, color = "white") +
  geom_sf_text(
    aes(label = glue::glue("{name}\n{count}")),
    size = 1.75,
    fontface = "plain",
    family = "Open Sans"
  ) +
  facet_grid(year ~ sex) +
  coord_sf() +
  rcartocolor::scale_fill_carto_c(palette = "SunsetDark") +
  guides(
    fill = guide_colorbar(
      barheight = unit(2.5, units = "mm"),
      barwidth = unit(90, units = "mm"),
      direction = "horizontal",
      ticks.colour = "#3c3c3c",
      title.position = "top",
      title.hjust = 0.5,
      label.theme = element_text(color = "white"),
      title.theme = element_text(color = "white"),
      title = "Number of births"
    )
  ) +
  labs(
    title = "Most popular names in USA over time",
    caption = "SWD challenge: JAN 2020 (small multiples)\n Data: https://www.ssa.gov/OACT/babynames/limits.html\nVisualization: @philmassicotte"
  ) +
  theme(
    axis.title = element_blank(),
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.border = element_blank(),
    legend.position = "bottom",
    panel.background = element_rect(fill = "#3c3c3c"),
    plot.background = element_rect(fill = "#3c3c3c"),
    legend.background = element_rect(fill = "#3c3c3c"),
    strip.background = element_rect(fill = "#3c3c3c"),
    strip.text = element_text(
      color = "gray75",
      size = 24,
      face = "bold",
      family = "Quicksand"
    ),
    plot.title = element_text(
      color = "white",
      hjust = 0.5,
      size = 28,
      family = "IBM Plex"
    ),
    plot.caption = element_text(color = "gray75", size = 8)
  )

pdf_file <- here::here("graphs", "jan_2020.pdf")
png_file <- here::here("graphs", "jan_2020.png")

ggsave(pdf_file,
  device = cairo_pdf,
  width = 10,
  height = 12
)

knitr::plot_crop(pdf_file)

bitmap <- pdftools::pdf_render_page(pdf_file, dpi = 600)
png::writePNG(bitmap, png_file)
