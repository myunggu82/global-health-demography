library(dplyr)
library(ggplot2)
library(DemoTools)
library(wpp2024)

# =========================================================
# 1. Load WPP data
# =========================================================

data(mx1dt)
data(popAge5dt)
data(mx5dt)


# =========================================================
# 2. Korea and Uganda, 2018: age-specific mortality + population
# =========================================================

jap_mx <- mx1dt %>%
  filter(name == "Japan", year == 2018) %>%
  select(age, mxB)

uga_mx <- mx1dt %>%
  filter(name == "Uganda", year == 2018) %>%
  select(age, mxB)

jap_pop <- popAge1dt %>%
  filter(name == "Japan", year == 2018) %>%
  select(age, pop)

uga_pop <- popAge1dt %>%
  filter(name == "Uganda", year == 2018) %>%
  select(age, pop)


# =========================================================
# 3. Crude Death Rate (CDR)
# =========================================================

jap <- jap_pop %>%
  left_join(jap_mx, by = "age") %>%
  mutate(deaths = pop * mxB)

jap_cdr <- sum(jap$deaths) / sum(jap$pop) * 1000


uga <- uga_pop %>%
  left_join(uga_mx, by = "age") %>%
  mutate(deaths = pop * mxB)

uga_cdr <- sum(uga$deaths) / sum(uga$pop) * 1000


# =========================================================
# 4. Direct age standardization
#    Standard population = average of Korea and Uganda population
# =========================================================

std_pop <- jap_pop %>%
  rename(pop_jap = pop) %>%
  left_join(
    uga_pop %>% rename(pop_uga = pop),
    by = "age"
  ) %>%
  mutate(std_pop = (pop_jap + pop_uga) / 2) %>%
  select(age, std_pop)


jap_std <- std_pop %>%
  left_join(jap_mx, by = "age") %>%
  mutate(expected_deaths = std_pop * mxB)

jap_asmr <- sum(jap_std$expected_deaths) / sum(jap_std$std_pop) * 1000
jap_asmr

uga_std <- std_pop %>%
  left_join(uga_mx, by = "age") %>%
  mutate(expected_deaths = std_pop * mxB)

uga_asmr <- sum(uga_std$expected_deaths) / sum(uga_std$std_pop) * 1000


# =========================================================
# 5. Summary table
# =========================================================

mortality_result <- tibble(
  Country = c("Japan", "Uganda"),
  CDR = c(jap_cdr, uga_cdr),
  ASMR = c(jap_asmr, uga_asmr)
)

mortality_result




# =========================================================
# 6. Korea survivorship curves: 1953 vs 2018
# =========================================================

kor_mx_1953 <- mx5dt %>%
  filter(name == "Republic of Korea", year == 1953) %>%
  transmute(
    Age = as.numeric(gsub("100\\+", "100", age)),
    nMx = mxB
  ) %>%
  arrange(Age)

kor_mx_2018 <- mx5dt %>%
  filter(name == "Republic of Korea", year == 2018) %>%
  transmute(
    Age = as.numeric(gsub("100\\+", "100", age)),
    nMx = mxB
  ) %>%
  arrange(Age)


lt_kor_1953 <- lt_abridged(
  nMx = kor_mx_1953$nMx,
  Age = kor_mx_1953$Age,
  radix = 100000
) %>%
  mutate(year = "1953")

lt_kor_2018 <- lt_abridged(
  nMx = kor_mx_2018$nMx,
  Age = kor_mx_2018$Age,
  radix = 100000
) %>%
  mutate(year = "2018")

lt_kor_both <- bind_rows(
  lt_kor_1953,
  lt_kor_2018
)


# =========================================================
# 7. Plot survivorship curves
# =========================================================

ggplot(
  lt_kor_both,
  aes(x = Age, y = lx, color = year, linetype = year)
) +
  geom_line(linewidth = 1.5) +
  scale_color_manual(
    values = c(
      "1953" = "red",
      "2018" = "blue"
    )
  ) +
  labs(
    title = "Survivorship Curves, Republic of Korea",
    x = "Age",
    y = "Survivors, lx",
    color = "Year",
    linetype = "Year"
  ) +
  theme_classic()
