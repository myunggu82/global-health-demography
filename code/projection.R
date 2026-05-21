install.packages(c("gganimate", "gifski"))

library(tidyverse)
library(wpp2024)
library(gganimate)
library(gifski)
data(popAge5dt)
data(package = "wpp2024")


country_name <- "Republic of Korea"

target_years <- c(
  1950, 1955, 1960, 1965, 1970,
  1975, 1980, 1985, 1990, 1995,
  2000, 2005, 2010, 2015, 2020
)

pyramid_data <- popAge5dt %>%
  filter(name == country_name, year %in% target_years) %>%
  select(year, age, popM, popF) %>%
  pivot_longer(
    cols = c(popM, popF),
    names_to = "sex",
    values_to = "population"
  ) %>%
  mutate(
    sex = case_when(
      sex == "popM" ~ "남",
      sex == "popF" ~ "여"
    ),
    age = factor(age, levels = unique(age)),
    plot_pop = ifelse(sex == "남", -population, population)
  )

total_pop_by_year <- pyramid_data %>%
  group_by(year) %>%
  summarise(
    total_pop = sum(population) * 1000,
    .groups = "drop"
  ) %>%
  mutate(
    frame_label = paste0(
      format(round(total_pop), big.mark = ","),
      "명 (",
      year,
      "년)"
    )
  )

pyramid_data <- pyramid_data %>%
  left_join(total_pop_by_year, by = "year") %>%
  mutate(
    frame_label = factor(frame_label, levels = total_pop_by_year$frame_label)
  )

p <- ggplot(
  pyramid_data,
  aes(x = age, y = plot_pop, fill = sex)
) +
  geom_col(width = 0.9) +
  coord_flip() +
  scale_y_continuous(
    labels = function(x) format(abs(x), big.mark = ",")
  ) +
  scale_fill_manual(
    name = "성별",
    values = c(
      "남" = "#00AFC4",
      "여" = "#006D6F"
    )
  ) +
  labs(
    title = "대한민국 인구추계",
    subtitle = "총인구: {current_frame}",
    x = NULL,
    y = "인구수(천명)"
  ) +
  theme_bw(base_size = 12) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, face = "bold"),
    legend.title = element_text(face = "bold"),
    legend.position = "right"
  ) +
  transition_manual(frame_label)

animate(
  p,
  nframes = 120,
  fps = 30,
  width = 800,
  height = 600,
  end_pause = 30,
  renderer = gifski_renderer("figure/korea_population_pyramid.gif")
)
