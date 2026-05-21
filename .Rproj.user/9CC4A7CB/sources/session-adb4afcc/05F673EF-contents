library(tidyverse)
library(wpp2024)

data(popAge1dt)

country_name <- "Republic of Korea"
target_year <- 2020

pyramid_data <- popAge5dt %>%
  filter(name == country_name, year == target_year) %>%
  select(age, popM, popF) %>%
  pivot_longer(
    cols = c(popM, popF),
    names_to = "sex",
    values_to = "population"
  ) %>%
  mutate(
    sex = recode(sex, popM = "남", popF = "여"),
    age = factor(age, levels = unique(age))
  )

total_pop <- sum(pyramid_data$population) * 1000


male_plot <- pyramid_data %>%
  filter(sex == "남") %>%
  ggplot(aes(x = population, y = age)) +
  geom_col(fill = "#00AFC4", width = 0.9) +
  scale_x_reverse(labels = function(x) format(x, big.mark = ",")) +
  labs(title = "남", x = NULL, y = NULL) +
  theme_bw(base_size = 12) +
  theme(
    panel.grid = element_blank(),
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.7),
    plot.title = element_text(
      hjust = 0.5,
      face = "bold",
      margin = margin(b = 6)
    ),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  )

age_plot <- pyramid_data %>%
  distinct(age) %>%
  ggplot(aes(x = 1, y = age, label = age)) +
  geom_text(size = 3.5) +
  labs(x = NULL, y = NULL) +
  theme_void(base_size = 12)

female_plot <- pyramid_data %>%
  filter(sex == "여") %>%
  ggplot(aes(x = population, y = age)) +
  geom_col(fill = "#006D6F", width = 0.9) +
  scale_x_continuous(labels = function(x) format(x, big.mark = ",")) +
  labs(title = "여", x = NULL, y = NULL) +
  theme_bw(base_size = 12) +
  theme(
    panel.grid = element_blank(),
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.7),
    plot.title = element_text(
      hjust = 0.5,
      face = "bold",
      margin = margin(b = 6)
    ),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  )

(male_plot + age_plot + female_plot) +
  plot_layout(widths = c(7, 1.2, 7)) +
  plot_annotation(
    title = "대한민국 인구추계",
    subtitle = paste0(
      target_year, "년 : ",
      format(round(total_pop), big.mark = ","),
      "명"
    ),
    caption = "인구수(천명)"
  ) &
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, face = "bold"),
    plot.caption = element_text(hjust = 0.5)
  )
