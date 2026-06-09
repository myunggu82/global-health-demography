
# install packages --------------------------------------------------------

remotes::install_github("timriffe/DemoTools")
library(DemoTools)


library(wpp2024)
library(DemoTools)
library(tidyverse)

data(mxM)
data(mxF)

names(mxM)
head(mxM)


data(mxM)


kor_mx <- mxM %>%
  filter(name == "Republic of Korea") %>%
  select(age, `2020-2025`) %>%
  rename(Age = age, nMx = `2020-2025`) %>%
  mutate(
    Age = as.numeric(gsub("100\\+", "100", Age)),
    AgeInt = c(1, 4, rep(5, n() - 3), NA)
  )

lt_abridged(
  nMx = kor_mx$nMx,
  Age = kor_mx$Age,
  radix = 100000
)

lt_kor <- lt_abridged(
  nMx = kor_mx$nMx,
  Age = kor_mx$Age,
  radix = 100000
)

ggplot(lt_kor, aes(x = Age, y = lx)) +
  geom_line(linewidth = 1.2) +
  labs(
    title = "Survivorship Curve: Republic of Korea, Male, 2020-2025",
    x = "Age",
    y = "Survivors, lx"
  ) +
  theme_minimal()


ggplot(lt_kor, aes(x = Age, y = lx)) +
  geom_line(linewidth = 1.2) +
  scale_y_log10() +
  labs(
    title = "Survivorship Curve, log scale",
    x = "Age",
    y = "Survivors, lx"
  ) +
  theme_minimal()


lt_kor <- lt_single_mx(
  nMx = kor_mx$nMx,
  Age = kor_mx$Age,
  n = kor_mx$AgeInt,
  radix = 100000
)

lt_kor
lt_kor

lt_kor <- lt_single_mx(
  nMx = kor_mx$nMx,
  Age = kor_mx$Age,
  AgeInt = kor_mx$AgeInt,
  radix = 100000
)

lt_kor <- lt_single_mx(
  nMx = kor_mx$nMx,
  Age = kor_mx$Age,
  AgeInt = kor_mx$AgeInt,
  radix = 100000
)

lt_kor
ggplot(lt_kor, aes(x = Age, y = lx)) +
  geom_line(linewidth = 1.2) +
  labs(
    title = "Survivorship Curve: Republic of Korea, Male, 2020-2025",
    x = "Age",
    y = "Survivors, lx"
  )

head(lt_kor)


kor_mx <- mxM %>%
  filter(name == "Republic of Korea", time == 2020)

install.packages(
  c(
    "tidyverse",
    "gganimate",
    "gifski",
    "remotes"
  )
)

remotes::install_github("PPgp/wpp2024")


# open libraries ----------------------------------------------------------
library(tidyverse)
library(wpp2024)
library(gganimate)
library(gifski)


# wpp 2024 data -----------------------------------------------------------
data(package = "wpp2024")


# single year pyramid -----------------------------------------------------
data(popAge5dt)
head(popAge5dt)
tail(popAge5dt)

unique(popAge5dt$name)
table(popAge5dt$year)


country_name <- "Eastern and South-Eastern Asia"
target_year <- 2020


pyramid_data <- popAge5dt %>%
  filter(name == country_name, year == target_year) %>%
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

head(pyramid_data)

total_pop <- sum(pyramid_data$population) * 1000


ggplot(
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
    title = "Eastern and South-Eastern Asia",
    subtitle = paste0(
      "총인구: ",
      format(round(total_pop), big.mark = ","),
      "명 (",
      target_year,
      "년)"
    ),
    x = NULL,
    y = "인구수(천명)"
  ) +
  theme_bw(base_size = 12, base_family = "AppleGothic") +
  theme(
    text = element_text(family = "AppleGothic"),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, face = "bold"),
    legend.title = element_text(face = "bold"),
    legend.position = "right",
    panel.grid.minor = element_blank()
  )


target_years <- seq(1950, 2020, by = 5)

historical_data <- popAge5dt %>%
  filter(
    name == country_name,
    year %in% target_years
  ) %>%
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
    plot_pop = ifelse(
      sex == "남",
      -population,
      population
    )
  )

head(historical_data)


total_pop_by_year <- historical_data %>%
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

historical_data <- historical_data %>%
  left_join(total_pop_by_year, by = "year") %>%
  mutate(
    frame_label = factor(
      frame_label,
      levels = total_pop_by_year$frame_label
    )
  )

head(total_pop_by_year)


p_historical <- ggplot(
  historical_data,
  aes(
    x = age,
    y = plot_pop,
    fill = sex
  )
) +
  geom_col(width = 0.9) +
  coord_flip() +
  scale_y_continuous(
    labels = function(x)
      format(abs(x), big.mark = ",")
  ) +
  scale_fill_manual(
    name = "Sex",
    values = c(
      "남" = "#00AFC4",
      "여" = "#006D6F"
    )
  ) +
  labs(
    title = "Republic of Korea",
    subtitle = "Total population: {current_frame}",
    x = NULL,
    y = "Population (thousands)"
  ) +
  theme_bw(base_size = 12) +
  theme(
    text = element_text(family = "AppleGothic"),
    plot.title = element_text(
      hjust = 0.5,
      face = "bold"
    ),
    plot.subtitle = element_text(
      hjust = 0.5,
      face = "bold"
    ),
    legend.title = element_text(face = "bold"),
    legend.position = "right",
    panel.grid.minor = element_blank()
  ) +
  transition_manual(frame_label)


animate(
  p_historical,
  nframes = 120,
  fps = 20,
  width = 900,
  height = 650,
  end_pause = 30,
  renderer = gifski_renderer(
    "../figure/Asia_population_pyramid.gif"
  )
)


getwd()
